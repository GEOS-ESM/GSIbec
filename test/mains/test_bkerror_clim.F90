program test_bkerror_clim

use m_gsibclim, only: gsibclim_init
use m_gsibclim, only: gsibclim_init_guess
!use m_gsibclim, only: gsibclim_set_guess
use m_gsibclim, only: gsibclim_cv_space
use m_gsibclim, only: gsibclim_sv_space
use m_gsibclim, only: gsibclim_final_guess
use m_gsibclim, only: gsibclim_final

use guess_grids, only: gsiguess_bkgcov_init
!use guess_grids, only: gsiguess_bkgcov_final

implicit none

character(len=*), parameter :: myname ="SABerror"
logical :: cv
integer :: lat2,lon2

call gsibclim_init(cv,lat2,lon2)
call gsibclim_init_guess()
!call gsibclim_set_guess()
call gsiguess_bkgcov_init()

if (cv) then
   call gsibclim_cv_space()
else
   call gsibclim_sv_space()
endif

!call gsiguess_bkgcov_final()
call gsibclim_final_guess()
call gsibclim_final(.true.)

end program test_bkerror_clim
