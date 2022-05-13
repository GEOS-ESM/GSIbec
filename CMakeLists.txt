# (C) Copyright 2019 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# GSIBCLIM
################################################################################

cmake_minimum_required( VERSION 3.12 )
project( gsibclim VERSION 0.0.0 LANGUAGES Fortran )

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

include( gsibclim_compiler_flags )

################################################################################
# Dependencies
################################################################################

find_package( jedicmake QUIET )  # Prefer find modules from jedi-cmake
find_package( MPI REQUIRED COMPONENTS Fortran )
find_package( NetCDF REQUIRED COMPONENTS Fortran )

# NCEP spectral library (optional)
find_package( sp QUIET )

################################################################################
# Sources
################################################################################

add_subdirectory( src )
add_subdirectory( test )

################################################################################
# Finalise configuration
################################################################################

# prepares a tar.gz of the sources and/or binaries
ecbuild_install_project( NAME ${PROJECT_NAME} )

# print the summary of the configuration
ecbuild_print_summary()
