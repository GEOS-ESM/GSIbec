# (C) Copyright 2022 United States Government as represented by the Administrator of the National
#     Aeronautics and Space Administration
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# GSI-GEOS

list(APPEND gsigeos_src_files_list
m_nc_GEOSens.f90
geos_StateIO.F90
gsi_fixture_GEOS.F90
cplr_ensemble.F90
)

set( gsigeos_src_files

${gsigeos_src_files_list}

PARENT_SCOPE
)
