################################################################################
# GSIbec
################################################################################

cmake_minimum_required( VERSION 3.12 )
project( gsibec VERSION 1.1.2 LANGUAGES Fortran )

## Ecbuild integration
set( ECBUILD_DEFAULT_BUILD_TYPE Release )
find_package( ecbuild 3.3.2 REQUIRED )
include( ecbuild_system NO_POLICY_SCOPE )
ecbuild_declare_project()
list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
set( CMAKE_DIRECTORY_LABELS ${PROJECT_NAME} )

################################################################################
# Standard compiler flags
################################################################################

option( ENABLE_MKL "Use MKL for LAPACK implementation (if available)" ON )
include( gsibec_compiler_flags )
include( gsibec_extra_macros )

################################################################################
# Dependencies
################################################################################
if( ENABLE_MKL )
    find_package( MKL )
endif()
if( MKL_FOUND )
    set( LAPACK_LIBRARIES ${MKL_LIBRARIES} )
else()
    find_package( LAPACK REQUIRED )
endif()

find_package( jedicmake QUIET )  # Prefer find modules from jedi-cmake
find_package( MPI REQUIRED COMPONENTS Fortran )
find_package( NetCDF REQUIRED COMPONENTS Fortran )

# NCEP spectral library (optional)
find_package( sp QUIET )

################################################################################
# Sources
################################################################################

add_subdirectory( src/gsibec )
add_subdirectory( test )

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME ${PROJECT_NAME} )

# print the summary of the configuration
ecbuild_print_summary()
