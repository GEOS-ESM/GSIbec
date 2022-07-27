# Compiler definitions
# --------------------
add_definitions( -D_REAL8_ -DLINUX -Dfunder -DFortranByte=char -DFortranInt=int -I/usr/local/intel/2020/compilers_and_libraries_2020.0.166/linux/mpi/intel64/include )

if( NOT CMAKE_BUILD_TYPE MATCHES "Debug" )
  add_definitions( -DNDEBUG )
endif( )


# Compiler flags
# --------------
if( CMAKE_Fortran_COMPILER_ID MATCHES "Intel" )
  include( compiler_flags_Intel_Fortran )
elseif( CMAKE_Fortran_COMPILER_ID MATCHES "GNU" )
  include( compiler_flags_GNU_Fortran )
else()
  message( STATUS "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()
