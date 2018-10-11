# This CMakeLists file will generate a hierarchical build for Open-FVS.
#
# Note that every effort has been made to be platform independent, but surely
# some things will be overlooked. Please let us know of any bugs or quirks.
# Some support is included for Visual Studio and Intel compilers, but most
# work supports GCC/GFortran under MinGW and Linux.
#
# This CMakeLists.txt supports out-of-source builds. This will be useful 
# keeping debug, release, etc. builds seperate.
#
# Example: A project folder for a debug build and a seperate folder for release
#   mkdir <FVS bin>/debug
#   cd <FVS bin>/debug
#   cmake .. -G"MinGW Makefiles" ^
#       -DFVS_VARIANTS="pnc;wcc" ^
#       -DCMAKE_BUILD_TYPE=DEBUG
#
#   cmake --build .
#
# Example: Linux 64 bit
#   mkdir debug
#   cd debug
#   cmake .. -G"Unix Makefiles" \
#       -DFVS_VARIANTS="pnc;wcc" \
#       -DCMAKE_BUILD_TYPE=Debug \
#       -DNATIVE_ARCH=ON -DSTATIC_LINK=ON
#   cmake --build .
#
# CMake will create a subfolder for each FVS variant and copy the cmake list
# and source list files.  A sub-project is then specified for each variant. Files
# common to all variants are compiled in the top level folder for use by each
# variant sub-project.
#
# The result is a make target for each executable, library, Python module. These
# targets can then be built individually or as a group with the "all" target.
# For instance "cmake --build . --target FVSpnc" would generate the PNC shared 
# library and the PNC executable.  "cmake --build . --target pyfvspnc" would 
# result in the common libraries, PNC shared library, and the Python
# module, but not the PNC executable.  "cmake --build . --target FVS_pnc" would 
# result in common libraries and the PNC shared library, but no executable or 
# Python module.
#
# Generated binaries are placed in the Open-FVS subfolder.
#
# Additional arguments:
#   -DSTATIC_LINK=ON/OFF - Statically link FFE & SQL with the FVS binaries.
#   -DNO_PYTHON=OFF/ON - Do not build Python module targets, default is OFF
#   -DFVS_VARIANTS=<variant list> - Explicitly list the variants to include in
#           the build as a semicolon seperated list of variant abbreviations,
#           eg. pnc;wcc;ncc;soc, the default is to include all variants.
#   -DPROFILING=ON/OFF - Include profiling info with compiled binaries.
#   -DENABLE_LTO=ON/OFF - Enable link time optimizations if BUILD_TYPE=Release.
#   -DNATIVE_ARCH=ON/OFF - Target the local machine architecture. Default=ON
#   -D32BIT_TARGET=ON/OFF - Set the target architecture to 32 bit. Default=OFF
#   -DUNIX_TARGET=ON/OFF - Set the target OS to be a UNIX flavor. Default=OFF
#
# CMake Variables:
#   -DCMAKE_BUILD_TYPE=<build type> - Adjust compiler & linker flags. Debug;Release

cmake_minimum_required (VERSION 3.12 FATAL_ERROR)

project(OPEN_FVS C CXX Fortran)
enable_language(Fortran)

set(version "Open-FVS")

# List known variants to compile by default
set(all_variants "pnc;wcc;soc;cac;ecc;op;oc")

# Set search path for CMake include files
set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake_include")

# Include the configuration scripts
include("BuildType")

#Assumes the FVS source is one level up from the parent CmakeLists.txt
#Used to set the prefix of all files in *sourceList.txt
get_filename_component(source_root "${CMAKE_CURRENT_LIST_DIR}/.." ABSOLUTE)
set(source_list_dir ${CMAKE_CURRENT_LIST_DIR})

string(TOLOWER ${FVS_VARIANTS} FVS_VARIANTS)
if (FVS_VARIANTS STREQUAL "all" OR NOT FVS_VARIANTS)
    set(FVS_VARIANTS ${all_variants})
endif ()

#TODO: Clean up cache variable to enable cmake-gui usage
# Define default values for build configuration options
set(FVS_VARIANTS "all" CACHE STRING "FVS variants to configure")
set(NO_PYTHON OFF CACHE BOOL "Do not build the Python modules.")
set(NATIVE_ARCH ON CACHE BOOL "Target the local machine architecture.")
set(32BIT_TARGET OFF CACHE BOOL "Set the target architecture to 32 bit.")
set(UNIX_TARGET OFF CACHE BOOL "Set the target OS to be a UNIX flavor.")
set(STATIC_LINK ON CACHE BOOL "Statically link FFE & SQL.")
set(ENABLE_LTO OFF CACHE BOOL "Enable link time optimizations.")
set(PROFILING OFF CACHE BOOL "Compile with profiling information.")

message(STATUS "Build Type:" ${CMAKE_BUILD_TYPE})

# Install relative to the build folder if not specified.
# NOTE: There is no expectation that CMake will be used as the install medium
if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    set (CMAKE_INSTALL_PREFIX "Open-FVS" CACHE PATH "Default install path" FORCE)
endif()
message(STATUS "Install Prefix: ${CMAKE_INSTALL_PREFIX}")

# If targetting the native architecture adjust the bit and OS flags
if (NATIVE_ARCH)
    message(STATUS "Configure for native architecture target.")

    # Determine native architecture bit depth
    if (CMAKE_SIZEOF_VOID_P EQUAL 8)
        message(STATUS "Native target architecture is 64 bit")
        set(32BIT_TARGET OFF)
    else ()
        message(STATUS "Native target architecture is 32 bit")
        set(32BIT_TARGET ON)
    endif ()

    # Determine the native OS type
    if (CMAKE_HOST_UNIX EQUAL 1)
        message(STATUS "Native target OS is *NIX")
        set(UNIX_TARGET ON)
    else ()
        message(STATUS "Native target OS is NOT *NIX")
        set(UNIX_TARGET OFF)
    endif ()

endif ()

##-----COMPILER FLAGS-----##
if (CMAKE_GENERATOR MATCHES "Visual Studio 10")
    message(STATUS "Configure FVS build for ${CMAKE_GENERATOR}")
    set(flags "/D_WINDOWS /W3 /Zm100")
    set (CMAKE_C_FLAGS "${flags}" CACHE STRING "VS10 mod A" FORCE)
    set (CMAKE_CXX_FLAGS "${flags}" CACHE STRING "VS10 mod B" FORCE)
endif ()

# TODO: Check for target OS and compiler instead of relying on the CMake generator
# TODO: Add Cygwin GNU toolchains

if (CMAKE_COMPILER_IS_GNUCC OR APPLE)
    message(STATUS "Add GCC compiler flags.")

    # TODO: These may be clobbering CMAKE FLAGS

    # CMAKE_BUILD_TYPE = Debug
    set(debug_flags "-g3 -mfpmath=sse -msse")
    set(CMAKE_Fortran_FLAGS_DEBUG "${debug_flags} -fbacktrace -ffpe-trap=invalid,zero,overflow,underflow")
    set(CMAKE_CXX_FLAGS_DEBUG "${debug_flags}")
    set(CMAKE_C_FLAGS_DEBUG "${debug_flags}")

    # CMAKE_BUILD_TYPE = Release
    set(release_flags "-Ofast -mfpmath=sse -msse")
    # set(release_flags "-O2 -mfpmath=387")
    if (ENABLE_LTO)
        set(release_flags "${release_flags} -funroll-loops -flto -fwhole-program")
    else ()
        set(CMAKE_Fortran_FLAGS_RELEASE "${release_flags} -funroll-loops")
    endif ()
    set(CMAKE_CXX_FLAGS_RELEASE "${release_flags}")
    set(CMAKE_C_FLAGS_RELEASE "${release_flags}")

    if (PROFILING)
        add_definitions(-DPROFILING)
        set(profile_flags "-pg")
        set(CMAKE_CXX_FLAGS "${profile_flags} ${CMAKE_CXX_FLAGS}")
        set(CMAKE_C_FLAGS "${profile_flags} ${CMAKE_C_FLAGS}")
        set(CMAKE_Fortran_FLAGS "${profile_flags} -fprofile-arcs -ftest-coverage ${CMAKE_Fortran_FLAGS}")
        set(CMAKE_EXE_LINKER_FLAGS "${profile_flags} ${CMAKE_EXE_LINKER_FLAGS}")
        set(CMAKE_SHARED_LINKER_FLAGS "${profile_flags} ${CMAKE_SHARED_LINKER_FLAGS}")
    endif ()

    # Flags for all build types
    # TODO: Add additional architecture and optimization flags -mtune=, -mfpmath=sse, etc.
    # TODO: Need to specify target architectures.
    if (NATIVE_ARCH)
        set(flags "-march=native")
        if (APPLE)
            # TODO: Catch the Clang compiler explicitly
            # Add -mno-avx per: http://stackoverflow.com/a/10894096/673590
            set(flags "${flags} -mno-avx")
        endif()

    endif()

    # TODO: Is pthread necessary
    # set(flags "-pipe -pthread")
    if (32BIT_TARGET)
        set(flags "${flags} -m32 -pipe")
    else ()
        if (WIN32)
            add_definitions(-DMS_WIN64)
        endif ()
        set(flags "${flags} -m64 -pipe")
    endif ()

    # Specify the target OS; perhaps useful with MinGW.
    if (UNIX_TARGET)
        message(STATUS "Add *NIX GCC flags and defs.")
        set(flags "${flags} -fPIC")
        add_definitions(-DANSI)
    endif()

   # Enable link time optimizations
    if (ENABLE_LTO)
        message(STATUS "Enable link time optimizations.")
        set(flags "${flags} -flto")
    endif()

    set(CMAKE_CXX_FLAGS "-cpp ${flags} ${CMAKE_CXX_FLAGS}")
    set(CMAKE_C_FLAGS "-cpp ${flags} ${CMAKE_C_FLAGS}")
    set(CMAKE_Fortran_FLAGS "-cpp ${flags} ${CMAKE_Fortran_FLAGS}")

    # Linker Flags
    set(CMAKE_EXE_LINKER_FLAGS "${flags} ${CMAKE_EXE_LINKER_FLAGS}")
    set(CMAKE_SHARED_LINKER_FLAGS "${flags} ${CMAKE_SHARED_LINKER_FLAGS}")
    set(flags "")

    if (NOT UNIX_TARGET)
        set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -static")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
        add_definitions(-DANSI -DWINDOWS -DCMPgcc)

    elseif (UNIX_TARGET)
        set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS}")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
        add_definitions(-DANSI -DCMPgcc)

    endif ()

endif ()

# Get the source list files for each variant
set(source_lists "")
foreach(var ${FVS_VARIANTS})
    set(fn "${source_list_dir}/FVS${var}_sourceList.txt")
    if (NOT EXISTS "${fn}")
        message(FATAL_ERROR "No source list for variant ${var}: ${fn}")
    else()
        message(STATUS "Variant source list: ${var} - ${fn}")
        list(APPEND source_lists ${fn})
    endif()
endforeach(var)

list(LENGTH FVS_VARIANTS num_variants)
message(STATUS "FVS Variants = (${num_variants}) ${FVS_VARIANTS}")
message(STATUS "Source Root = ${source_root}")

#---Top Level Objects
list(GET source_lists 0 source_list)

# Read the source file names as a list as strip whitespace
file(STRINGS ${source_list} x)
set(source_file_list)
foreach(y ${x})
    string(STRIP ${y} z)
    list(APPEND source_file_list ${z})
endforeach()

# Localize the paths
string(REPLACE "../" "${source_root}/" source_file_list "${source_file_list}")

#---Include Dirs
# Loop through the source files to gather the C header and include file paths
set(include_dirs)
foreach (fn ${source_file_list})
    get_filename_component(pathname ${fn} PATH)
    get_filename_component(filename ${fn} NAME)
    if (${fn} MATCHES "(.*\\.h$)|(.*\\.F77$)")
        list(APPEND include_dirs ${pathname})
    endif()
endforeach(fn)

# Include the root folder to catch the DBSTYPEDEFS.F77 location
list(APPEND include_dirs ${CMAKE_CURRENT_BINARY_DIR})
list(REMOVE_DUPLICATES include_dirs)
include_directories(BEFORE ${include_dirs})

#---SQL
# Setup targets for the mkdbsTypeDefs generator, and SQL library
# Get the SQL source code file
file(STRINGS ${source_list} sql_source REGEX "fvsSQL.c")
string(REPLACE "../" "${source_root}/" sql_source "${sql_source}")

# Add the command to generate the SQL type defs
file(STRINGS ${source_list} typedefs_src REGEX "mkdbsTypeDefs.c")
string(REPLACE "../" "${source_root}/" typedefs_src "${typedefs_src}")
add_executable(mkdbsTypeDefs ${typedefs_src})

set(dbstypedefs_f77 "${CMAKE_CURRENT_BINARY_DIR}/DBSTYPEDEFS.F77")

# Execute the DBSTYPEDEFS.F77 generator.
add_custom_command(
        OUTPUT ${dbstypedefs_f77}
        COMMAND $<TARGET_FILE:mkdbsTypeDefs>
        DEPENDS mkdbsTypeDefs)

# Create a dummy target so the DBSTYPEDEFS.F77 generator will be called.
add_custom_target(gen_dbstypedefs ALL DEPENDS ${dbstypedefs_f77})

# TODO: use find_library to locate the correct odbc library

# Compile the SQL object library
if (NOT STATIC_LINK)
    message(STATUS "Build shared SQL library.")
    add_library(sql_lib SHARED ${sql_source})

    if (WIN32)
        target_link_libraries(sql_lib odbc32)
    else ()
        target_link_libraries(sql_lib odbc)
    endif ()

    set_target_properties(sql_lib PROPERTIES
            LINKER_LANGUAGE CXX
            OUTPUT_NAME FVSsql
            )

else ()
    message(STATUS "Build static SQL objects.")
    add_library(sql_lib OBJECT ${sql_source})

endif ()

# Ensure that DBSTYPEDEFS.F77 is generated
add_dependencies(sql_lib gen_dbstypedefs)

#---FFE
# Get the list of FFE C/C++ source files
set(ffe_source)
foreach (fn ${source_file_list})
    # FFE Source files are C/C++, excluding the SQL code
    if((${fn} MATCHES "(.*\\.c$)|(.*\\.cpp$)")
            AND (NOT ${fn} MATCHES "(.*mkdbsTypeDefs.c$)|(.*fvsSQL.c$)"))
        list (APPEND ffe_source ${fn})
    endif()
endforeach(fn)
list(REMOVE_DUPLICATES ffe_source)

# Compile the FFE object library
if (NOT STATIC_LINK)
    message(STATUS "Build shared FFE library.")
    add_library(ffe_lib SHARED ${ffe_source})
    set_target_properties(ffe_lib PROPERTIES
            LINKER_LANGUAGE CXX
            OUTPUT_NAME FVSffe
            #TODO: is stdcall alias necessary
            #LINK_FLAGS -Wl,--add-stdcall-alias
            )
else ()
    message(STATUS "Build static FFE objects.")
    add_library(ffe_lib OBJECT ${ffe_source})
endif ()

add_custom_target(dev COMMENT "Copy files for development")

#---Configure each requested variant.
message(STATUS "Configure variants: ${FVS_VARIANTS}")
math(EXPR num_variants "${num_variants}-1")
foreach(v RANGE ${num_variants})
    list(GET FVS_VARIANTS ${v} variant)
    get_filename_component(variant_dir ${CMAKE_BINARY_DIR}/${variant} ABSOLUTE)
    message(STATUS "Variant Dir = ${variant_dir}")

    file(MAKE_DIRECTORY ${variant_dir})

    # Write out the modified source list file to the variant sub directory
    list(GET source_lists ${v} srclst_fn)
    message(STATUS "Source List File: ${srclst_fn}")
    file(STRINGS ${srclst_fn} source_list NEWLINE_CONSUME)
    string(REPLACE "../" "${source_root}/" source_list "${source_list}")
    file(WRITE ${variant_dir}/sourcelist.txt ${source_list})

    # Write out the new variant CMakeLists file
    # TODO: Move the variant template to a seperate file.
    file(STRINGS FVSVariant.cmake.in cmakelist NEWLINE_CONSUME)
    # string(REGEX REPLACE ".*##-----DO NOT REMOVE THIS TAG-----##" ""
           # cmakelist ${cmakelist})

    # Replace template strings
    # TODO: use string(configure ...) instead of this mess
    string(REGEX REPLACE "<<variant>>" "${variant}" cmakelist ${cmakelist})
    string(REGEX REPLACE "<<source_list>>" "${variant_dir}/sourcelist.txt" cmakelist ${cmakelist})
    string(REGEX REPLACE "<<source_root>>" "${source_root}" cmakelist ${cmakelist})
    string(REGEX REPLACE "<<no_python>>" ${NO_PYTHON} cmakelist ${cmakelist})
    file(WRITE ${variant_dir}/CMakeLists.txt ${cmakelist})

    # Add the the FVS variant CMakeLists.txt as a sub-project
    add_subdirectory(${variant_dir} ${variant_dir})
    
endforeach()
