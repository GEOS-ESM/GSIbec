module m_rf
use m_kinds, only: r_kind, i_kind
use constants, only: zero
use berror, only: create_berror_vars
use berror, only: destroy_berror_vars
use berror, only: final_rftable
use balmod, only: create_balance_vars
use balmod, only: destroy_balance_vars
use balmod, only: prebal
use smooth_polcarf, only: destroy_smooth_polcas
use gridmod, only: nlon,nlat,lon2,lat2,lon2,nsig
use mpeu_util, only: getindex
use control_vectors, only: cvars3d
use berror, only: fut2ps,cwcoveqqcov
use m_berror_stats, only: berror_get_dims
use mpeu_util, only: die
implicit none
public :: rf_set_default
public :: rf_set
public :: rf_unset

public :: varq
public :: varcw
public :: qoption
public :: cwoption
public :: pseudo_q2
public :: clip_supersaturation

integer(i_kind) :: qoption
integer(i_kind) :: cwoption
logical :: pseudo_q2
logical :: clip_supersaturation

real(r_kind),allocatable,dimension(:,:):: varq
real(r_kind),allocatable,dimension(:,:):: varcw
interface rf_set_default; module procedure set_def_; end interface
interface rf_set; module procedure set_; end interface
interface rf_unset; module procedure unset_; end interface

character(len=*), parameter :: myname = 'm_rf'
contains
  subroutine set_def_
  qoption=1
  cwoption=0
  pseudo_q2=.false. 
  clip_supersaturation=.false.
  end subroutine set_def_
  subroutine init_(mlat,msig)
  integer,intent(in) :: mlat,msig
  if(.not.allocated(varq)) then 
    if (getindex(cvars3d,'q')>0) then
       allocate(varq(mlat,msig))
       varq=zero
    endif
  endif
  if(.not.allocated(varcw)) then
     allocate(varcw(mlat,msig))
     varcw=zero
  endif
  end subroutine init_
  subroutine set_ (mype)
  integer(i_kind), intent(in) :: mype
  character(len=*), parameter :: mynmae_ = myname//'*set_'
  integer(i_kind) msig,mlat,mlon
  logical good
! Load background error arrays used by recursive filters
  call berror_get_dims(msig,mlat,mlon)
  good=nlat==mlat.and.nlon==mlon.and.nsig==msig
  if (.not. good) then
    print *, nlat, mlat, nlon, mlon, nsig, msig
    call die(myname,': bad dims',99)
  endif
  call init_(mlat,msig)
  call create_balance_vars
  call create_berror_vars
  call prebal(fut2ps,cwcoveqqcov)
  call prewgt(cwoption,qoption,mype)
  end subroutine set_
  subroutine unset_
  call destroy_smooth_polcas ! the set is called in prewgt - gsi typically has inconsistent set/unset
  call destroy_berror_vars
  call destroy_balance_vars
  call final_rftable         ! out of place as consequence of gsi typically has inconsistent set/unset
  call final_
  end subroutine unset_
  subroutine final_
  if(allocated(varcw)) deallocate(varcw)
  if(allocated(varq )) deallocate(varq)
  end subroutine final_
end module m_rf
