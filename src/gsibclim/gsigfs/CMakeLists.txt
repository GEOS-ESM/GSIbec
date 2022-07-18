# (C) Copyright 2022 United States Government as represented by the Administrator of the National
#     Aeronautics and Space Administration
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

# GSI-GEOS

list(APPEND gsigfs_src_files_list
ncepnems_io.f90
)

set( gsigfs_src_files

${gsigfs_src_files_list}

PARENT_SCOPE
)
