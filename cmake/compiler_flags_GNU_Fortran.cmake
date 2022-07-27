# Definitions
# -----------
add_definitions(-D__GFORTRAN__)


# Common flags
# ------------
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-double-8")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fPIC")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -falign-commons")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffixed-line-length-132")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fcray-pointer")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -frecord-marker=4")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-range-check")


# GNU version 10 or greater
# -------------------------
if (CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-invalid-boz")
endif ()
