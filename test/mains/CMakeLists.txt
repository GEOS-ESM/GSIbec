ecbuild_add_executable( TARGET  eta_echo.x
                        SOURCES eta_echo.f90
                        LIBS    gsibec
                       )
ecbuild_add_executable( TARGET  test_gsi_bkerror.x
                        SOURCES test_gsi_bkerror.F90
                        LIBS    gsibec
                       )
ecbuild_add_executable( TARGET  test_geos_getens.x
                        SOURCES test_geos_getens.F90
                        LIBS    gsibec
                       )
