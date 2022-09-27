# Include macros for creating links and symlinks
#include( gsibec_functions )

# Create Data directory for test input config and symlink all files
list( APPEND gsibec_test_input
   testinput/
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput/unit_tests)
#CREATE_SYMLINK( ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR} ${gsibec_test_input} )


# Add files to cmake resources
ecbuild_add_resources( TARGET gsibec_test_scripts
                       SOURCES_PACK
                       ${gsibec_test_input}
                     )

# Create Data directory for test data and symlink files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)


ecbuild_add_executable( TARGET  eta_echo.x
                        SOURCES mains/eta_echo.f90
                        LIBS    gsibec
                       )

ecbuild_add_executable( TARGET  test_gsi_bkerror.x
                        SOURCES mains/test_gsi_bkerror.F90
                        LIBS    gsibec
                       )

ecbuild_add_executable( TARGET  test_geos_getens.x
                        SOURCES mains/test_geos_getens.F90
                        LIBS    gsibec
                       )

ecbuild_add_executable( TARGET  write_berror_global.x
                        SOURCES mains/write_berror_global.F90
                        LIBS    gsibec
                       )

list( APPEND gsibec_testinput
  testinput/geos_saberror.nml
  testinput/gfs_saberror.nml
  testinput/gsi-coeffs-geos-global-l72x72y46.nc4
  testinput/gsi-coeffs-gfs-global-l127x192y96.nc4
)

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
LINK_FILES( "${gsibec_testinput}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})

# Test GMAO and NCEP B-Error-Cov:

ecbuild_add_test( TARGET   gsibec_test_tier1_geos
                  MPI      6
                  ARGS    "testinput/geos_saberror.nml" "testinput/gsi-coeffs-geos-global-l72x72y46.nc4" 
                  COMMAND  test_gsi_bkerror.x )

ecbuild_add_test( TARGET   gsibec_test_tier1_gfs
                  MPI      6
                  ARGS    "testinput/gfs_saberror.nml" "testinput/gsi-coeffs-gfs-global-l127x192y96.nc4"
                  COMMAND  test_gsi_bkerror.x )
