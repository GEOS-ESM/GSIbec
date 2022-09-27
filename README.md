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
not all variables from GSI are meanningful in the context of GSIbec, and to 
a large extent are not available here.

The c-tests above are not quite the typical JEDI c-test. Instead of comparing
numbers and text files, the tests produce grads files (and corresponding 
control tables) with the result of the test. The test is set to apply B to 
a vector field set as multiple Dirac-like functions placed randomly on the 
globe. The default c-test uses on 6 processors to run; the user can re-run
the examples by hand with other choices of number of processors. Notice that
by construction, and for simplicity, a changes  in the number of PEs changes
the number of Dirac-functions placed on the globe.

