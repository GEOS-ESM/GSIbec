program test_gsi_bkerror

use m_gsibec, only: gsibec_init
use m_gsibec, only: gsibec_init_guess
!use m_gsibec, only: gsibec_set_guess
use m_gsibec, only: gsibec_cv_space
use m_gsibec, only: gsibec_sv_space
use m_gsibec, only: gsibec_final_guess
use m_gsibec, only: gsibec_final

use guess_grids, only: gsiguess_bkgcov_init
!use guess_grids, only: gsiguess_bkgcov_final

implicit none

character(len=*), parameter :: myname ="SABerror"
logical :: cv
integer :: lat2,lon2

call gsibec_init(cv,lat2,lon2)
call gsibec_init_guess()
!call gsibec_set_guess()
call gsiguess_bkgcov_init()

if (cv) then
   call gsibec_cv_space()
else
   call gsibec_sv_space()
endif

!call gsiguess_bkgcov_final()
call gsibec_final_guess()
call gsibec_final(.true.)

end program test_gsi_bkerror
