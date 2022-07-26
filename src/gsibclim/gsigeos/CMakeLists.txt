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
