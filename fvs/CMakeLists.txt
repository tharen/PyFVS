cmake_minimum_required (VERSION 2.8.8 FATAL_ERROR)

project(OPEN_FVS C CXX Fortran)
enable_language(Fortran)

set(version "open-fvs")

#Used to set the prefix of all files in *sourceList.txt
get_filename_component(
        src_root "${CMAKE_CURRENT_LIST_DIR}" ABSOLUTE)
set(source_list_dir "${CMAKE_CURRENT_LIST_DIR}/bin")

#TODO: Clean up cache variable to enable cmake-gui usage
# Define default values for build configuration options
set(FVS_VARIANTS "all" CACHE STRING "FVS variants")
set(NATIVE_ARCH ON CACHE BOOL "Target local architecture.")
set(32BIT_TARGET OFF CACHE BOOL "Target is 32 bit.")
set(UNIX_TARGET OFF CACHE BOOL "Set the target OS to UNIX.")
set(STATIC_LINK ON CACHE BOOL "Statically link FFE & SQL.")
set(ENABLE_LTO OFF CACHE BOOL "Link time optimizations.")
set(PROFILING OFF CACHE BOOL "Compile for profiling.")

message(STATUS "Build Type:" ${CMAKE_BUILD_TYPE})

#FIXME: There may be other destinations for the binaries
#      Perhaps this should be handled with  packaging.
set(INSTALL_DIR "${CMAKE_BINARY_DIR}/${version}" CACHE PATH
        "Install location for compiled binaries.")
set(CMAKE_INSTALL_PREFIX ${INSTALL_DIR})
set(EXECUTABLE_OUTPUT_PATH ${INSTALL_DIR}/bin)
set(LIBRARY_OUTPUT_PATH ${INSTALL_DIR}/bin)
set(INCLUDE_INSTALL_PATH ${INSTALL_DIR}/lib)

# If targetting native arch  adjust the bit and OS flags
if (NATIVE_ARCH)
    message(STATUS "Configure for native target.")

    # Determine native architecture bit depth
    if (CMAKE_SIZEOF_VOID_P EQUAL 8)
        message(STATUS "Native target is 64 bit")
        set(32BIT_TARGET OFF)
    else ()
        message(STATUS "Native target is 32 bit")
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
    message(STATUS
            "Configure FVS build for ${CMAKE_GENERATOR}")
    set(flags "/D_WINDOWS /W3 /Zm100")
    set (CMAKE_C_FLAGS
            "${flags}" CACHE STRING "VS10 mod A" FORCE)
    set (CMAKE_CXX_FLAGS
            "${flags}" CACHE STRING "VS10 mod B" FORCE)
endif ()

# TODO: Add Cygwin GNU toolchains

if (CMAKE_COMPILER_IS_GNUCC)
    message(STATUS "Add GCC compiler flags.")

    # CMAKE_BUILD_TYPE = Debug
    set(debug_flags "-Og -g")
    set(CMAKE_Fortran_FLAGS_DEBUG "${debug_flags}")
    set(CMAKE_CXX_FLAGS_DEBUG "${debug_flags}")
    set(CMAKE_C_FLAGS_DEBUG "${debug_flags}")
    
    # CMAKE_BUILD_TYPE = Release
    set(release_flags
            "-Ofast -funroll-loops")
    if (ENABLE_LTO)
        set(release_flags
                "${release_flags} -flto -fwhole-program")
    endif ()
    set(CMAKE_Fortran_FLAGS_RELEASE "${release_flags}")
    set(CMAKE_CXX_FLAGS_RELEASE "${release_flags}")
    set(CMAKE_C_FLAGS_RELEASE "${release_flags}")
    
    if (PROFILING)
        add_definitions(-DPROFILING)
        set(profile_flags "-pg -fprofile-arcs -ftest-coverage")
        set(CMAKE_CXX_FLAGS "${profile_flags} ${CMAKE_CXX_FLAGS}")
        set(CMAKE_C_FLAGS "${profile_flags} ${CMAKE_C_FLAGS}")
        set(CMAKE_Fortran_FLAGS "${profile_flags} ${CMAKE_Fortran_FLAGS}")
        set(CMAKE_EXE_LINKER_FLAGS "${profile_flags} ${CMAKE_EXE_LINKER_FLAGS}")
        set(CMAKE_SHARED_LINKER_FLAGS "${profile_flags} ${CMAKE_SHARED_LINKER_FLAGS}")
    endif ()
    
    if (NATIVE_ARCH)
        set(flags "-march=native -mtune=native")
    endif()

    if (32BIT_TARGET)
        set(flags "${flags} -m32 -pipe")
    else()
        set(flags "${flags} -m64 -pipe")
    endif()

    # Specify the target OS; perhaps useful for use with MinGW.
    if (UNIX_TARGET)
        message(STATUS "Add -fPIC for UNIX targets.")
        set(flags "${flags} -fPIC")
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

#Process the variant list command line argument and make a list of source lists
#  eg. cmake <...> -DFVS_VARIANTS=pnc;wcc;iec
string(TOLOWER ${FVS_VARIANTS} FVS_VARIANTS)
if (FVS_VARIANTS STREQUAL "all")
    file(GLOB source_lists "${source_list_dir}/FVS*_sourceList.txt")
    set(FVS_VARIANTS "")
    foreach (fn ${source_lists})
        string(REGEX REPLACE ".*FVS(.+)_sourceList.txt" "\\1" var ${fn})
        string(TOLOWER ${var} var)
        list(APPEND FVS_VARIANTS ${var})
    endforeach(fn)

else()
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

endif()

list(LENGTH FVS_VARIANTS num_variants)
message(STATUS "FVS Variants = (${num_variants}) ${FVS_VARIANTS}")
message(STATUS "Source Root = ${src_root}")

#---Top Level Objects
list(GET source_lists 0 source_list)

# Read the source file names as a list
file(STRINGS ${source_list} source_file_list)
# Localize the paths
string(REPLACE "../" "${src_root}/" source_file_list "${source_file_list}")

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
string(REPLACE "../" "${src_root}/" sql_source "${sql_source}")

# Add the command to generate the SQL type defs
file(STRINGS ${source_list} typedefs_src REGEX "mkdbsTypeDefs.c")
string(REPLACE "../" "${src_root}/" typedefs_src "${typedefs_src}")
add_executable(mkdbsTypeDefs ${typedefs_src})

set(dbstypedefs_f77 "${CMAKE_CURRENT_BINARY_DIR}/DBSTYPEDEFS.F77")

# Execute the DBSTYPEDEFS.F77 generator.
add_custom_command(
        OUTPUT ${dbstypedefs_f77}
        COMMAND mkdbsTypeDefs
        DEPENDS mkdbsTypeDefs)

# Create a dummy target so the DBSTYPEDEFS.F77 generator will be called.
add_custom_target(gen_dbstypedefs ALL DEPENDS ${dbstypedefs_f77})

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

#---Common Objects
# Generate a list of fortran source code files common to all variants
set(common_source)
list(GET source_lists 0 sl)
file(STRINGS ${sl} common_source REGEX "(.*f$)|(.*f90$)")
string(REPLACE "../" "${src_root}/" common_source "${common_source}")

set(temp_src)
foreach(fn ${common_source})
    file(READ ${fn} foo)
    if ((NOT ${fn} MATCHES ".*main.f$")
        # Variant specific code includes the prgprm and esparm files
        AND (NOT "${foo}" MATCHES ".*PRGPRM.F77.*")
        AND (NOT "${foo}" MATCHES ".*ESPARM.F77.*"))
        list(APPEND temp_src ${fn})
    endif()
endforeach()
set(common_source ${temp_src})

# Parse each configured variant source file list keeping files common to all
foreach (sl ${source_lists})
    set(temp_src)
    # Read the list of source files for this variant
    file(STRINGS ${sl} fl)
    string(REPLACE "../" "${src_root}/" fl "${fl}")

    # Compare each file currently in the common list with the variant file list
    foreach(fn ${common_source})
        list(FIND fl ${fn} m)
        # FIND returns -1 for files not in the variant file list
        if (NOT m EQUAL -1)
            list(APPEND temp_src ${fn})
        endif()
    endforeach()
    set(common_source ${temp_src})
endforeach()
list(REMOVE_DUPLICATES common_source)

# Compile the  object libraries
add_library(common_objs OBJECT ${common_source})

# Ensure the SQL and common objects are built in sequence
add_dependencies(common_objs sql_lib)

#---Configure each requested variant.
message(STATUS "Configure variants: ${FVS_VARIANTS}")
math(EXPR num_variants "${num_variants}-1")
foreach(v RANGE ${num_variants})
    list(GET FVS_VARIANTS ${v} fvs_variant)
    get_filename_component(variant_dir ${CMAKE_BINARY_DIR}/build/${fvs_variant} ABSOLUTE)
    message(STATUS "Variant Dir = ${variant_dir}")

    file(MAKE_DIRECTORY ${variant_dir})

    # Write out the modified source list file to the variant sub directory
    list(GET source_lists ${v} srclst_fn)
    message(STATUS "Source List File: ${srclst_fn}")
    file(STRINGS ${srclst_fn} source_list NEWLINE_CONSUME)
    string(REPLACE "../" "${src_root}/" source_list "${source_list}")
    set(src_list_path "${variant_dir}/sourcelist.txt")
    file(WRITE ${src_list_path} ${source_list})

    # Write out the new variant CMakeLists file
    configure_file("./CMakeLists.txt.in" 
           ${variant_dir}/CMakeLists.txt @ONLY)

    # Add the FVS variant CMakeLists.txt as a sub-project
    add_subdirectory(${variant_dir} ${variant_dir})

endforeach()

return()

