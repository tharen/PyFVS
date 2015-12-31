## $Id$

## Towards using cmake! NCrookston July 24 2012

cmake_minimum_required (VERSION 2.8)

# Default variable values which can be set on the command line
set(FVS_VARIANTS "all" CACHE STRING "FVS variants to configure, eg. pnc,wcc,oc,op")
set(CMAKE_SYSTEM_NAME "Windows" CACHE STRING "Target system name, Windows, Linux, etc.")
set(MAKE_JOBS 8 CACHE STRING "Number of parallel make jobs to execute, eg. make -j8")

# The list of variants can be passed as a comma or semicolon separated list
# If semicolon is used it translates directly to a CMake list, but must be 
#   properly escaped or quoted in a bash terminal, eg. pnc\;wcc or "pnc;wcc"
string(REPLACE "," ";" FVS_VARIANTS "${FVS_VARIANTS}")

if (CMAKE_GENERATOR MATCHES "Visual Studio 10")
  set (CMAKE_C_FLAGS   "/D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod A" FORCE)
  set (CMAKE_CXX_FLAGS "/D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod B" FORCE)
  message(STATUS "Building FVS with VS10/Intel")
elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
  message(STATUS "Building FVS with MinGW")
elseif (CMAKE_GENERATOR MATCHES "MSYS Makefiles")
  message(STATUS "Building FVS with MSYS")
elseif (CMAKE_GENERATOR MATCHES "Unix Makefiles")
  message(STATUS "Building FVS with Unix")
else ()
  message(STATUS "Unknown FVS toolchain; exiting")
  return()
endif()

message(STATUS "Target system: ${CMAKE_SYSTEM_NAME}")

## example 1: Single-variant GLOB: file(GLOB tobuild FVSiec_sourceList.txt)
## example 2: All-variant GLOB: file(GLOB tobuild FVS*_sourceList.txt)

#file(GLOB tobuild FVSiec_sourceList.txt)

# Enumerate variant source list files to configure.
if (FVS_VARIANTS STREQUAL "all")
    # Setup all variants
    file(GLOB tobuild "FVS*_sourceList.txt")
else ()
    # User specified variants, eg "pnc,wcc"
    set(tobuild "")
    foreach(var ${FVS_VARIANTS})
        set(fn "FVS${var}_sourceList.txt")
        if (NOT EXISTS "${fn}")
            message(FATAL_ERROR "No source list for variant ${var}: ${fn}")
        else()
            message(STATUS "Adding variant to build: ${var} - ${fn}")
            list(APPEND tobuild ${fn})
        endif()
    endforeach ()
endif ()

list(SORT tobuild)
message(STATUS "tobuild = ${tobuild}")

set(dirsToProcess)

# Configure each variant subdirectory
foreach (sourceList ${tobuild})

  get_filename_component (slfn ${sourceList} NAME CACHE)
  string(REPLACE "_sourceList.txt" "" prgName ${slfn})
  string(REPLACE "FVS" "" varName ${prgName})

  file(STRINGS ${sourceList}  sourceListStrings  NEWLINE_CONSUME)
  string(REPLACE "../" "../../" newsourceList ${sourceListStrings})

  file(STRINGS CMakeLists.txt cmakelistin NEWLINE_CONSUME)

  string(REGEX REPLACE ".*##DO NOT REMOVE THIS TAG##" ""
      cmakelistout ${cmakelistin})

  file(MAKE_DIRECTORY ${prgName}_CmakeDir)
  file(REMOVE ${prgName}_CmakeDir/CMakeLists.txt)
  file(REMOVE ${prgName}_CmakeDir/CMakeCache.txt)
  file(REMOVE ${prgName}_CmakeDir/${prgName}_sourceList.txt)
  file(REMOVE_RECURSE ${prgName}_CmakeDir/CMakeFiles)
  file(WRITE ${prgName}_CmakeDir/CMakeLists.txt ${cmakelistout})
  file(WRITE ${prgName}_CmakeDir/${prgName}_sourceList.txt ${newsourceList})
  list(APPEND dirsToProcess ${prgName}_CmakeDir)
  
  # Perform variable substitution on the variant CMakeLists.txt
  configure_file(
      ${prgName}_CmakeDir/CMakeLists.txt
      ${prgName}_CmakeDir/CMakeLists.txt
      @ONLY
      )
      
  unset(prgName CACHE)
  unset(slfn CACHE)

  message(STATUS "${prgName} - ${sourceList}")
  
endforeach(sourceList)

foreach(toProcess ${dirsToProcess})
  execute_process(COMMAND cmake -G ${CMAKE_GENERATOR} CMakeLists.txt WORKING_DIRECTORY ${toProcess})
endforeach(toProcess)

# TODO: Use add_subdirectory to configure a hierarchical build
#     Once the CMakeLists.txt files are configured CMake can handle the rest.
#     Note that CMake generates a Makefile, but this would only be valid
#     if we were using the CMake add_subdirectory command.
#
# Generate a top level makefile
if (${CMAKE_GENERATOR} MATCHES "MinGW Makefiles")
  set(make "mingw32-make")
else ()
  set(make "make")
endif ()

set(top_make "makefile_all") #makefile name
file(WRITE ${top_make} "# Build all configured variants\n")
file(APPEND ${top_make} "MAKE = ${make}\n")
file(APPEND ${top_make} "BUNDLE_DIR = Open-FVS\n")
if (CMAKE_SYSTEM_NAME MATCHES "Windows")
  file(APPEND ${top_make} "EXE_SUFFIX = .exe\n")
  file(APPEND ${top_make} "SO_SUFFIX = .dll\n")
else ()
  file(APPEND ${top_make} "EXE_SUFFIX = \n")
  file(APPEND ${top_make} "SO_SUFFIX = .so\n")
endif ()

# List all variants to be built
set(all_vars "")
foreach (source_list ${tobuild})
  string(REGEX REPLACE ".*FVS(.*)_sourceList.txt" "${CMAKE_MATCH_1}" var ${source_list})
  list(APPEND all_vars ${CMAKE_MATCH_1})
endforeach ()

string(REPLACE ";" " " _all_vars "${all_vars}")
file(APPEND ${top_make} "ALL_VARIANTS = ${_all_vars}\n")

# Phony targets
file(APPEND ${top_make} "\n")
file(APPEND ${top_make} ".PHONY: all\n")
file(APPEND ${top_make} ".PHONY: bundle\n")

file(APPEND ${top_make} "\nall: \$(ALL_VARIANTS)\n")

# Target to bundle (package,install,etc.) all libraries and executables
file(APPEND ${top_make} "\nbundle: \$(ALL_VARIANTS)\n\tmkdir -p $(BUNDLE_DIR)\n")
file(APPEND ${top_make} "\tfor var in \$(ALL_VARIANTS); do \\\n")
file(APPEND ${top_make} "\t\tcp FVS\$\${var}_CmakeDir/FVS\$\${var}\$(EXE_SUFFIX) \$(BUNDLE_DIR); \\\n")
file(APPEND ${top_make} "\t\tcp FVS\$\${var}_CmakeDir/libFVS_\$\${var}\$(SO_SUFFIX) \$(BUNDLE_DIR); \\\n")
file(APPEND ${top_make} "\t\tcp FVS\$\${var}_CmakeDir/libFVSfofem\$(SO_SUFFIX) \$(BUNDLE_DIR); \\\n")
file(APPEND ${top_make} "\t\tcp FVS\$\${var}_CmakeDir/libFVSsql\$(SO_SUFFIX) \$(BUNDLE_DIR); \\\n")
file(APPEND ${top_make} "\tdone\n")

# Configure targets for each variant
foreach (var ${all_vars})
  file(APPEND ${top_make} "\n${var}:\n\t\$(MAKE) -j${MAKE_JOBS} -C FVS${var}_CmakeDir\n")
endforeach ()

return()

##The text below this tag becomes the CMakeLists.txt file in the subdirectory
##DO NOT REMOVE THIS TAG##

##If edits are made, make them in the CMakeList.txt file in the parent directory.
cmake_minimum_required (VERSION 2.8)

set (CMAKE_SYSTEM_NAME @CMAKE_SYSTEM_NAME@)
set (MAKE_JOBS @MAKE_JOBS@)

if (CMAKE_GENERATOR MATCHES "Visual Studio 10")
  set (CMAKE_C_FLAGS   "/D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod A" FORCE)
  set (CMAKE_CXX_FLAGS "/D_WINDOWS /W3 /Zm100" CACHE STRING "VS10 mod B" FORCE)
  message(STATUS "Building FVS variant with VS10/Intel")
elseif (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
  message(STATUS "Building FVS variant with MinGW")
elseif (CMAKE_GENERATOR MATCHES "MSYS Makefiles")
  message(STATUS "Building FVS variant with MSYS")
elseif (CMAKE_GENERATOR MATCHES "Unix Makefiles")
  message(STATUS "Building FVS variant with Unix")
else ()
  message(STATUS "Unknown toolchain; exiting")
  return()
endif()

message(STATUS "Target system: ${CMAKE_SYSTEM_NAME}")

project (FVS C CXX Fortran)

set (CMAKE_Fortran_Format FIXED)
set (CMAKE_VERBOSE_MAKEFILE OFF)

file(GLOB sourceList FVS*_sourceList.txt)
get_filename_component (slfn ${sourceList} NAME CACHE)
string(REPLACE "_sourceList.txt" "" prgName ${slfn})
string(REPLACE "FVS" "" varName ${prgName})
message(STATUS "slfn = ${slfn} prgName= ${prgName}")

add_definitions(-DANSI)

if (CMAKE_SYSTEM_NAME MATCHES "Windows")
  link_libraries(odbc32)
  add_definitions(-DWINDOWS -D_WINDLL)
else ()
  link_libraries(odbc)
endif ()

if (CMAKE_COMPILER_IS_GNUCC)
  add_definitions(-DCMPgcc)
endif ()

set (OUTPUT_NAME ${prgName})
set (includeDirs)
set (CsourceSQL)
set (CsourceFFE)
##set (CsourceCFIM)
set (FsourceFVS)
set (FsourceMAIN)

file(STRINGS ${prgName}_sourceList.txt fileList)

foreach (fn ${fileList})
  
  # White space mucks with STREQUAL
  string(STRIP ${fn} fn)
  
  get_filename_component (fname   ${fn} NAME CACHE)
  get_filename_component (pname   ${fn} PATH CACHE)
  get_filename_component (extname ${fn} EXT  CACHE)

  if (${extname} STREQUAL ".h" OR ${extname} STREQUAL ".F77")
    list (APPEND includeDirs  ${pname})
  endif()

  if (${extname} STREQUAL ".f")
    if (${fname} STREQUAL "main.f")
      list (APPEND FsourceMAIN ${fn})
    else()
      list (APPEND FsourceFVS ${fn})
    endif()
  elseif(${extname} STREQUAL ".c" OR ${extname} STREQUAL ".cpp")
    if (fname STREQUAL mkdbsTypeDefs.c)
      add_executable(mkdbsTypeDefs ${fn})
      set(dbstypedefs_f77 "DBSTYPEDEFS.F77")
      add_custom_command(OUTPUT ${dbstypedefs_f77}
                         COMMAND $<TARGET_FILE:mkdbsTypeDefs>
                         DEPENDS mkdbsTypeDefs)
      add_custom_target(dbstypedefs ALL DEPENDS ${dbstypedefs_f77})
    elseif(fname STREQUAL fvsSQL.c)
      list (APPEND CsourceSQL ${fn})
    else()
      list (APPEND CsourceFFE ${fn})
    endif()
  endif()
  unset (fname   CACHE)
  unset (pname   CACHE)
  unset (extname CACHE)
endforeach(fn)

list (REMOVE_DUPLICATES includeDirs)
list (APPEND includeDirs ${CMAKE_CURRENT_BINARY_DIR})

include_directories(BEFORE ${includeDirs})

#message(STATUS "includeDirs= ${includeDirs}")
#message(STATUS "CsourceSQL= ${CsourceSQL}")
#message(STATUS "CsourceFFE= ${CsourceFFE}")
#message(STATUS "FsourceFVS= ${FsourceFVS}")
#message(STATUS "FsourceMAIN= ${FsourceMAIN}")

message(STATUS "FVSvariant = FVS${varName}")

add_library(FVSsql SHARED ${CsourceSQL})
add_library(FVSfofem SHARED ${CsourceFFE})
##add_library(FVScfim SHARED ${CsourceCFIM})
add_library(FVS_${varName} SHARED ${FsourceFVS})
add_executable (${prgName} ${FsourceMAIN})

target_link_libraries(${prgName} FVS_${varName} FVSsql FVSfofem)
target_link_libraries(FVS_${varName} FVSfofem FVSsql)

add_dependencies(FVS_${varName} dbstypedefs)

set_target_properties(${prgName} PROPERTIES LINKER_LANGUAGE Fortran)

# set up function alias for DLL containing CPP code

if (CMAKE_GENERATOR MATCHES "MinGW Makefiles")
  set_target_properties(FVSfofem PROPERTIES LINK_FLAGS -Wl,--add-stdcall-alias)
endif()

#INSTALL(TARGETS ${prgName} FVSsql FVSfofem FVS_${varName}
#	RUNTIME DESTINATION ../../myBin
#)
