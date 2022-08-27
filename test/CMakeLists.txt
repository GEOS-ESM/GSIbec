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


ecbuild_add_executable( TARGET  test_gsi_bkerror.x
                        SOURCES mains/test_gsi_bkerror.F90
                        LIBS    gsibec
                       )

ecbuild_add_executable( TARGET  test_geos_getens.x
                        SOURCES mains/test_geos_getens.F90
                        LIBS    gsibec
                       )

# Test GMAO and NCEP B-Error-Cov:

#ecbuild_add_test( TARGET  test_gsibec_gmao
#                  SOURCES mains/test_gsi_bkerror.F90
#                  ARGS    "testinput/gmao_saberror.nml"
#                  ENVIRONMENT OOPS_TRAPFPE=1
#                  LIBS    gsibec )

#ecbuild_add_test( TARGET  test_gsibec_ncep
#                  SOURCES mains/test_gsi_bkerror.F90
#                  ARGS    "testinput/ncep_saberror.nml"
#                  ENVIRONMENT OOPS_TRAPFPE=1
#                  LIBS    gsibec )
