# Basic info about the build
cmake_minimum_required(VERSION 2.8)
project(nobarrier_omp Fortran)

option(BUILD_SHARED_LIBS
  "Build a shared rather than a static library." ON)

include(CTest)

list(APPEND CMAKE_MODULE_PATH ${CESM_CMAKE_MODULE_DIRECTORY})
include(CESM_utils)

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)

  # GNU compiler flags
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fopenmp")

  set(CMAKE_Fortran_FLAGS_DEBUG
    "${CMAKE_Fortran_FLAGS_DEBUG} -fcheck=all")

endif()

# Fortran-specific options.
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/include")

include_directories("${CMAKE_CURRENT_BINARY_DIR}/include")

add_subdirectory("src")

if(BUILD_TESTING)
  add_subdirectory("tests")
endif()
