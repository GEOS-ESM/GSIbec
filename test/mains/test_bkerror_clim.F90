program test_bkerror_clim

use m_gsibclim, only: gsibclim_init
use m_gsibclim, only: gsibclim_cv_space
use m_gsibclim, only: gsibclim_sv_space
use m_gsibclim, only: gsibclim_final

implicit none

character(len=*), parameter :: myname ="SABerror"
logical :: cv

call gsibclim_init(cv)

if (cv) then
   call gsibclim_cv_space()
else
   call gsibclim_sv_space()
endif

call gsibclim_final()

end program test_bkerror_clim
