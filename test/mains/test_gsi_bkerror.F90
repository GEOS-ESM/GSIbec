program test_gsi_bkerror

use m_gsibec, only: gsibec_init
use m_gsibec, only: gsibec_init_guess
use m_gsibec, only: gsibec_cv_space
use m_gsibec, only: gsibec_sv_space
use m_gsibec, only: gsibec_final_guess
use m_gsibec, only: gsibec_final

use guess_grids, only: gsiguess_bkgcov_init

implicit none

character(len=*), parameter :: myname ="SABerror"
logical :: cv

call gsibec_init(cv,vgrid=.true.)
call gsibec_init_guess()
call gsiguess_bkgcov_init()

if (cv) then
   call gsibec_cv_space()
else
   call gsibec_sv_space()
endif

call gsibec_final_guess()
call gsibec_final(.true.)

end program test_gsi_bkerror
