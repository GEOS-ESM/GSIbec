ecbuild_add_executable( TARGET  test_bkerror_clim.x
                        SOURCES test_bkerror_clim.F90
                        LIBS    gsibclim
                       )
ecbuild_add_executable( TARGET  test_geos_getens.x
                        SOURCES test_geos_getens.F90
                        LIBS    gsibclim
                       )
