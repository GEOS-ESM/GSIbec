# (C) Copyright 2017-2018 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

ecbuild_add_executable( TARGET  test_bkerror_clim.x
                        SOURCES test_bkerror_clim.F90
                        LIBS    gsibclim
                       )
ecbuild_add_executable( TARGET  test_geos_getens.x
                        SOURCES test_geos_getens.F90
                        LIBS    gsibclim
                       )
