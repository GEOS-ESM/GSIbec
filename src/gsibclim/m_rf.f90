module m_rf
use m_kinds, only: r_kind, i_kind
use constants, only: zero
use berror, only: create_berror_vars
use balmod, only: create_balance_vars
use balmod, only: prebal
use gridmod, only: nlon,nlat,lon2,lat2,lon2,nsig
use jfunc, only: qoption,cwoption
use mpeu_util, only: getindex
use control_vectors, only: cvars3d
use berror, only: fut2ps,cwcoveqqcov
use m_berror_stats, only: berror_get_dims
use mpeu_util, only: die
implicit none
public :: varq
public :: varcw
public :: rf_set
public :: rf_unset

real(r_kind),allocatable,dimension(:,:):: varq
real(r_kind),allocatable,dimension(:,:):: varcw
interface rf_set; module procedure set_; end interface
interface rf_unset; module procedure unset_; end interface

character(len=*), parameter :: myname = 'm_rf'
contains
  subroutine init_
  if (getindex(cvars3d,'q')>0) then
    allocate(varq(nlat,nsig))
  endif
  allocate(varcw(nlat,nsig))
  varcw=zero
  end subroutine init_
  subroutine set_ (mype)
  integer(i_kind), intent(in) :: mype
  character(len=*), parameter :: mynmae_ = myname//'*set_'
  integer(i_kind) msig,mlat,mlon
  logical good
! Load background error arrays used by recursive filters
  call init_
  call berror_get_dims(msig,mlat,mlon)
  good=nlat==mlat.and.nlon==mlon.and.nsig==msig
  if (.not. good) then
    print *, nlat, mlat, nlon, mlon, nsig, msig
    call die(myname,': bad dims',99)
  endif
  call create_balance_vars
  call create_berror_vars
  call prebal(fut2ps,cwcoveqqcov)
  call prewgt(varcw,cwoption,varq,qoption,mype)
  end subroutine set_
  subroutine unset_
  call final_
  end subroutine unset_
  subroutine final_
  if(allocated(varcw)) deallocate(varcw)
  if(allocated(varq )) deallocate(varq)
  end subroutine final_
end module m_rf
