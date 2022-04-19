# (C) Copyright 2019 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

####################################################################
# FLAGS COMMON TO ALL BUILD TYPES
####################################################################

####################################################################
# RELEASE FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_RELEASE "-r8 -O3 -qopt-report0 -ftz -align all -fno-alias -traceback -assume realloc_lhs -convert big_endian -assume byterecl -fPIC -fp-model strict -align dcommons " )

####################################################################
# DEBUG FLAGS
####################################################################

set( CMAKE_Fortran_FLAGS_DEBUG "-r8 -O0 -g -qopt-report0 -ftz -align all -fno-alias -traceback -assume realloc_lhs -convert big_endian -assume byterecl -fPIC -fp-model strict -align dcommons " )

####################################################################
# BIT REPRODUCIBLE FLAGS
####################################################################

#set( CMAKE_Fortran_FLAGS_BIT     "-O2 -ip -ipo -unroll -inline -no-heap-arrays" )

####################################################################
# LINK FLAGS
####################################################################

set( CMAKE_Fortran_LINK_FLAGS    "" )

####################################################################

# Meaning of flags
# ----------------
# todo

