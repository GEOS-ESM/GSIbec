# GSIbec
GSIbec: Extracts the background error covariance (BEC) model capabilities from 
 the Gridpoint Statistical Interpolation (GSI) atmospheric analysis system into a 
 library of its own.

The main objective of this library is to aid JEDI and make the GSI BEC available
for applications of interest in that context.

Strong dependencies: MPI
Weak dependecies: SP (spectral lib from NCEP)

This package should be renamed lower-case gsibec after cloning.

At this stage, two c-tests serve as demo for the use of the code. These 
can be executed from the build/gsibec/test as follows:

ctest -V -R gsibec_test_tier1_geos
ctest -V -R gsibec_test_tier1_gfs

as the names indicate they tests provide examples on use of B in the grid 
of GEOS (lat-lon) or GFS (Gaussian). The namelists driving both tests are
found in the testinput directory of gsibec/test. Familiarity with GSI tells 
that the driving namelist is much similar to some of the nameslists in GSI,
as well as the so-called anavinfo file. However, let the user not be fooled,
not all variables from GSI are available (or meanningful) in the context of 
GSIbec and to a large extent are not available here.
