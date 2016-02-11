cmake_minimum_required (VERSION 2.8.8 FATAL_ERROR)

project(OPEN_FVS C CXX Fortran)
enable_language(Fortran)

set(version "Open-FVS")

get_filename_component(build_root "${CMAKE_CURRENT_LIST_DIR}" ABSOLUTE)

function(setup_variant variant)
    message(STATUS "Setup build for ${variant}")
    
    get_filename_component(variant_dir ${CMAKE_CURRENT_LIST_DIR}/${variant} ABSOLUTE)
    file(MAKE_DIRECTORY ${variant_dir})
    include(${variant}/../${variant}_sources.cmake)
  
endfunction(setup_variant)

set(fvs_variants pnc wcc soc)

foreach(variant ${fvs_variants})
    setup_variant(${variant})
endforeach()

