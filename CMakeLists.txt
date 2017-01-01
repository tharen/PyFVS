cmake_minimum_required(VERSION 2.8.8 FATAL_ERROR)

project(PyFVS C CXX Fortran)
enable_language(Fortran)

add_subdirectory(fvs)

include(./fvs_api/CMakeLists.txt)

foreach (variant ${FVS_VARIANTS})
    message(STATUS "Configure wrapper for: ${variant}")
    set(fvslib "fvs${variant}-wrap")

    # Link the wrapper with the FVS variant library
    # Link the cmake object library
    set(fvs_objs "fvs${variant}_objs")
    add_library(${fvslib}
        SHARED ${api_src}
        $<TARGET_OBJECTS:${fvs_objs}>
        $<TARGET_OBJECTS:ffe_lib>
        $<TARGET_OBJECTS:sql_lib>
        $<TARGET_OBJECTS:common_objs>
        )
    target_link_libraries(${fvslib} odbc ltdl)

    ## Optionally link to the shared/static library
    #set(fvs_objs "fvs${variant}_static")
    #add_library(${fvslib} SHARED ${wrap_src})
    #target_link_libraries(${fvslib} ${fvs_objs})

    set_target_properties(${fvslib}
        PROPERTIES
        OUTPUT_NAME "fvs${variant}"
        LINKER_LANGUAGE Fortran
        )
    
    include_directories("./fvs/pn/common")

endforeach ()

