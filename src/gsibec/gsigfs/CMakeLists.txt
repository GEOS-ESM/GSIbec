# GSI-GFS

list(APPEND gsigfs_src_files_list
gsi_fixture_GFS.F90
m_nc_GFSens.f90
m_read_gfsens.F90
gfs_StateIO.F90
cplr_gfs_ensemble.F90
)

set( gsigfs_src_files

${gsigfs_src_files_list}

PARENT_SCOPE
)
