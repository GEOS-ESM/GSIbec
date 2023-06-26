program test_gsi_bkerror

use m_gsibec, only: gsibec_init
use m_gsibec, only: gsibec_init_guess
use m_gsibec, only: gsibec_cv_space
use m_gsibec, only: gsibec_sv_space
use m_gsibec, only: gsibec_final_guess
use m_gsibec, only: gsibec_final

use guess_grids, only: gsiguess_bkgcov_init
use hybrid_ensemble_parameters, only: gsi_enperts

use mpeu_util, only: die
use m_mpimod, only: gsi_mpi_comm_world
use m_mpimod, only: setworld
use mpeu_mpif, only: mpi_character

implicit none

integer, parameter :: MAXSTR=255
character(len=*), parameter :: myname = "test_gsi_berror"
character(len=MAXSTR) :: nml, bef
integer :: mype,ier
logical :: cv
type(gsi_enperts) :: epts

call mpi_init(ier)
call setworld()
call mpi_comm_rank(gsi_mpi_comm_world,mype,ier)

call init_(ier)
if(ier/=0) then
  call die(trim(myname),'fail reading cmd line, try again',ier)
endif

call gsibec_init(cv,epts,nmlfile=nml,befile=bef,vgrid=.true.)
call gsibec_init_guess()
call gsiguess_bkgcov_init()

if (cv) then
   call gsibec_cv_space()
else
   call gsibec_sv_space()
endif

call gsibec_final_guess()
call gsibec_final(.true.)

CONTAINS

subroutine init_(rc)
  integer,intent(out) :: rc
  integer argc, iarg
  integer iargc
  character(len=255) :: argv
  rc=0
  if(mype==0) then
     argc = iargc()
     if ( argc < 1 ) then
        print *, "   "
        print *, "Usage: ",trim(myname),".x nml befile"
        print *, "   "
        print *, " Apply GSI B error to arbitrary (internally"
        print *, " defined) Dirac-like vector"
        print *, "   "
        rc=1
        return
     end if
     iarg = 1
     do while (iarg<=argc)
        call GetArg ( iarg, argv )
        nml=trim(argv)
        iarg = iarg+1
        call GetArg ( iarg, argv )
        bef=trim(argv)
        iarg = iarg+1
     end do
  endif
  call mpi_bcast(nml,MAXSTR,mpi_character,0,gsi_mpi_comm_world,ier)
  call mpi_bcast(bef,MAXSTR,mpi_character,0,gsi_mpi_comm_world,ier)
end subroutine init_

end program test_gsi_bkerror
