# Common flags
# ------------
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -r8")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qopt-report0")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ftz ")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -align all")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-alias")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -assume realloc_lhs")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -convert big_endian")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -assume byterecl")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fp-model strict")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -align dcommons")
