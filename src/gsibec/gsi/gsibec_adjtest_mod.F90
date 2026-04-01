module gsibec_adjtest_mod

!$$$ module documentation block
!           .      .    .                                       .
! module:   adjtest
!  prgmmr: tremolet
!
! abstract: Routines and data to perform adjoint test
!
! program history log:
!   2007-05-09  tremolet - initial code
!   2009-08-14  lueken   - update documentation
!   2010-01-07  todling  - add test for state-vector
!                        - bias-component only contributes to dotp on pe=0
!   2010-08-19  lueken   - add only to module use
!
! subroutines included:
!   sub adtest_cv
!
! variable definition:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_kinds, only: r_kind,i_kind
use gsi_4dvar, only: nsubwin
use constants, only: zero, two
use m_mpimod, only: mype
use control_vectors, only: control_vector,allocate_cv,random_cv, &
    deallocate_cv,dot_product,assignment(=)
use state_vectors, only: allocate_state,deallocate_state,dot_product
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
use bias_predictors, only: predictors,allocate_preds,deallocate_preds, &
    assignment(=)
use compact_diffs, only: stvp2uv, tstvp2uv
!use jfunc, only: nrclen

implicit none
private
public adtest_cv1,adtest_cv2
public adtest_bkgerr
public adtest_stvp2uv
public iadtest
public iadtest_maxiter

interface adtest_bkgerr
  module procedure adtest_bkgerr_
end interface adtest_bkgerr

type(control_vector),save :: xtest1,xtest2

! iadtest           - adjoint tests:
!                     (1) test CV1 
!                     (2) test CV2 - another flavor
!                     (3) test B - full cov
!                     (4) test stvp2uv

logical, save :: iadtest(4)=.false.

integer :: iadtest_maxiter=0
integer :: nrclen=0
integer, save :: iadtest_cnt(4)=0
! ----------------------------------------------------------------------
contains
! ----------------------------------------------------------------------
subroutine adtest_cv1(xhat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    adtest
!   prgmmr: tremolet
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!
!   input argument list:
!    xhat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(control_vector), optional, intent(in   ) :: xhat

! Declare local variables  
type(gsi_bundle) :: stest1(nsubwin),stest2(nsubwin)
type(predictors) :: sbias1,sbias2
integer(i_kind) :: ii,idig
real(r_kind) :: zz1,zz2,zz3

if (iadtest_cnt(1)>iadtest_maxiter) return

if (mype==0) write(6,*)'ADTEST_CV1 starting'

! ----------------------------------------------------------------------
! Allocate local variables
call allocate_cv(xtest1)
call allocate_cv(xtest2)
do ii=1,nsubwin
   call allocate_state(stest1(ii))
   call allocate_state(stest2(ii))
end do
call allocate_preds(sbias1)
call allocate_preds(sbias2)

! Initialize control space vectors
if (present(xhat)) then
   xtest1=xhat
   if (mype==0) write(6,*)'ADTEST_CV1 use input xhat'
else
   call random_cv(xtest1)
   if (mype==0) write(6,*)'ADTEST_CV1 use random_cv(xhat)'
endif
xtest2=zero

! Initialize state vectors
do ii=1,nsubwin
   stest1(ii)=zero
   stest2(ii)=zero
enddo
sbias1=zero
sbias2=zero

! Run test
call control2state(xtest1,stest1,sbias1)
do ii=1,nsubwin
   stest2(ii)=stest1(ii)
enddo
sbias2=sbias1
call control2state_ad(stest2,sbias2,xtest2)

! Diagnostics
zz1=dot_product(xtest1,xtest2)

zz2=zero
do ii=1,nsubwin
   zz2=zz2+dot_product(stest1(ii),stest1(ii))
enddo
DO ii=1,nrclen
   zz2=zz2+sbias1%values(ii)*sbias1%values(ii)
ENDDO

if ( abs(zz1+zz2) > sqrt(tiny(zz3)) ) then
   zz3=two*abs(zz1-zz2)/(zz1+zz2)
else
   zz3=abs(zz1-zz2)
endif
idig= int(-log(abs(zz3)+tiny(zz3))/log(10.0_r_kind))

if (mype==0) then
   write(6,'(A)')' ADTEST_CV1              0.123456789012345678'
   write(6,'(A,ES25.18)')' ADTEST_CV1 <F''*F.Y,X>= ',zz1
   write(6,'(A,ES25.18)')' ADTEST_CV1 < F.Y,F.X>= ',zz2
   write(6,'(A,i3,   A)')' ADTEST_CV1 ',idig,' digits are identical'
   write(6,'(A,ES25.18)')' ADTEST_CV1 rel. err.= ',zz3
   write(6,'(A,ES25.18)')' ADTEST_CV1 mach.eps = ',epsilon(zz3)
endif

! Release local variables
call deallocate_cv(xtest1)
call deallocate_cv(xtest2)
do ii=1,nsubwin
   call deallocate_state(stest1(ii))
   call deallocate_state(stest2(ii))
enddo
call deallocate_preds(sbias1)
call deallocate_preds(sbias2)
! ----------------------------------------------------------------------

if (mype==0) write(6,*)'ADTEST_CV1 finished'

iadtest_cnt(1) = iadtest_cnt(1) + 1

return
end subroutine adtest_cv1
! ----------------------------------------------------------------------
subroutine adtest_cv2(xhat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    adtest
!   prgmmr: tremolet
!
! abstract:
!
! program history log:
!   2009-08-14  lueken - added subprogram doc block
!   2025-05-19  todling - this implements a different flavor of the
!                         original test.
!
!   input argument list:
!    xhat
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(control_vector), optional, intent(in   ) :: xhat

! Declare local variables  
type(gsi_bundle) :: stest1(nsubwin),stest2(nsubwin)
type(predictors) :: sbias1,sbias2
integer(i_kind) :: ii,idig
real(r_kind) :: zz1,zz2,zz3

if (iadtest_cnt(2)>iadtest_maxiter) return

if (mype==0) write(6,*)'ADTEST_CV2 starting'

! ----------------------------------------------------------------------
! Allocate local variables
call allocate_cv(xtest1)
call allocate_cv(xtest2)
do ii=1,nsubwin
   call allocate_state(stest1(ii))
   call allocate_state(stest2(ii))
end do
call allocate_preds(sbias1)
call allocate_preds(sbias2)

! Initialize control space vectors
if (present(xhat)) then
   xtest1=xhat
   if (mype==0) write(6,*)'ADTEST_CV2 use input xhat'
else
   call random_cv(xtest1)
   if (mype==0) write(6,*)'ADTEST_CV2 use random_cv(xhat)'
endif
call random_cv(xtest2,kseed=1999)

! Initialize state vectors
do ii=1,nsubwin
   stest1(ii)=zero
   stest2(ii)=zero
enddo
sbias1=zero
sbias2=zero

! Run test
call control2state(xtest1,stest1,sbias1) ! F*X
call control2state(xtest2,stest2,sbias2) ! F*Y

! Calculated: <FY,FX>
zz2=zero
do ii=1,nsubwin
   zz2=zz2+dot_product(stest1(ii),stest2(ii))
enddo
DO ii=1,nrclen
   zz2=zz2+sbias1%values(ii)*sbias2%values(ii)
ENDDO

xtest2=zero
call control2state_ad(stest2,sbias2,xtest2) ! F''*(F*Y)

! Calculated: <F''FY,X>
zz1=dot_product(xtest1,xtest2)

! Diagnostics
if ( abs(zz1+zz2) > sqrt(tiny(zz3)) ) then
   zz3=two*abs(zz1-zz2)/(zz1+zz2)
else
   zz3=abs(zz1-zz2)
endif
idig= int(-log(abs(zz3)+tiny(zz3))/log(10.0_r_kind))

if (mype==0) then
   write(6,'(A)')' ADTEST_CV2                0.123456789012345678'
   write(6,'(A,ES25.18)')' ADTEST_CV2 <F''*F.Y,  X>= ',zz1
   write(6,'(A,ES25.18)')' ADTEST_CV2 <   F.Y,F.X>= ',zz2
   write(6,'(A,i3,   A)')' ADTEST_CV2 ',idig,' digits are identical'
   write(6,'(A,ES25.18)')' ADTEST_CV2 rel. err.= ',zz3
   write(6,'(A,ES25.18)')' ADTEST_CV2 mach.eps = ',epsilon(zz3)
endif

! Release local variables
call deallocate_cv(xtest1)
call deallocate_cv(xtest2)
do ii=1,nsubwin
   call deallocate_state(stest1(ii))
   call deallocate_state(stest2(ii))
enddo
call deallocate_preds(sbias1)
call deallocate_preds(sbias2)
! ----------------------------------------------------------------------

if (mype==0) write(6,*)'ADTEST_CV2 finished'

iadtest_cnt(2) = iadtest_cnt(2) + 1

return
end subroutine adtest_cv2
! ----------------------------------------------------------------------
subroutine adtest_bkgerr_ (g1,g2)
use control_vectors, only: control_vector
use control_vectors, only: allocate_cv
use control_vectors, only: deallocate_cv
use control_vectors, only: allocate_cv,deallocate_cv
use control_vectors, only: assignment(=)
use control_vectors, only: random_cv
use control_vectors, only: prt_control_norms
use jfunc, only: nsclen,npclen,ntclen
use hybrid_ensemble_parameters,only : l_hyb_ens
use hybrid_ensemble_isotropic, only: bkerror_a_en
implicit none
type(control_vector), optional :: g1,g2
type(control_vector) :: gx
type(control_vector) :: gy
type(control_vector) :: gz
integer(i_kind) :: idig, istatus
real(r_kind) :: zz1,zz2,zz3

if (iadtest_cnt(3)>2*iadtest_maxiter+1) return

call allocate_cv(gx)
call allocate_cv(gy)
call allocate_cv(gz)

if (present(g1).and.present(g2) ) then
  gx = g1
  gy = g2
else
  call random_cv(gx,kseed=8765)
  call random_cv(gy,kseed=54321)
endif
call prt_control_norms(gx,'ADTEST_BKGERR: gx')
call prt_control_norms(gy,'ADTEST_BKGERR: gy')

! z=By
gz=zero
call bkerror(gy,gz, &
             1,nsclen,npclen,ntclen)
if (l_hyb_ens) then
   call bkerror_a_en(gy,gz)
endif
call prt_control_norms(gz,'ADTEST_BKGERR: By')
zz1 = dot_product(gz,gx) ! (By)''*x

! z=Bx
gz=zero
call bkerror(gx,gz, &
             1,nsclen,npclen,ntclen)
if (l_hyb_ens) then
   call bkerror_a_en(gx,gz)
endif
call prt_control_norms(gz,'ADTEST_BKGERR: Bx')
zz2 = dot_product(gy,gz) !  y''(Bx)

if ( abs(zz1+zz2) > sqrt(tiny(zz3)) ) then
   zz3=two*abs(zz1-zz2)/(zz1+zz2)
else
   zz3=abs(zz1-zz2)
endif
idig= int(-log(abs(zz3)+tiny(zz3))/log(10.0_r_kind))

if (mype==0) then
   write(6,'(A)')' ADTEST_BKGERR             0.123456789012345678'
   write(6,'(A,ES25.18)')' ADTEST_BKGERR <B.Y,X>  = ',zz1
   write(6,'(A,ES25.18)')' ADTEST_BKGERR <Y,B.X>  = ',zz2
   write(6,'(A,i3,   A)')' ADTEST_BKGERR ',idig,' digits are identical'
   write(6,'(A,ES25.18)')' ADTEST_BKGERR rel. err.= ',zz3
   write(6,'(A,ES25.18)')' ADTEST_BKGERR mach.eps = ',epsilon(zz3)
endif

call deallocate_cv (gz)
call deallocate_cv (gy)
call deallocate_cv (gx)

iadtest_cnt(3) = iadtest_cnt(3) + 1

end subroutine adtest_bkgerr_
! ----------------------------------------------------------------------

subroutine adtest_stvp2uv(idx,mype)
  use m_rerank, only: rerank
  use gridmod, only: nlon,nlat
  implicit none
  integer,intent(in):: idx
  integer,intent(in):: mype
  real(r_kind),pointer,dimension(:,:,:) :: x,y,z
  real(r_kind),pointer,dimension(:) :: xptr,yptr
  integer, allocatable :: nseed(:)
  real(r_kind) zz1, zz2, zz3
  integer(i_kind) :: idig,i,j,k,idim

  if (iadtest_cnt(4)>iadtest_maxiter) return

  idim=2
  allocate(x(idim,nlat,nlon))
  allocate(y(idim,nlat,nlon))
  allocate(z(idim,nlat,nlon))
  call random_seed(size=j)
  allocate(nseed(j))
  nseed(1:j)=idx
! The following because we don't want all procs to get
! exactly the same sequence (which would be repeated in
! the then not so random vector) but it makes the test
! not reproducible if the number of procs is changed.
  nseed(1)=idx+mype
  call random_seed(put=nseed)
  deallocate(nseed)

  call random_number(x)
  call random_number(y)

  z = y
  call  stvp2uv(z,idim) ! z=Ay
  call tstvp2uv(z,idim) ! z:=A^tz = A^t Ay
  zz1 = my_dot_product(reshape(x,(/2*nlat*nlon/)),reshape(z,(/2*nlat*nlon/)))

  call stvp2uv(y,idim) ! y:=Ay
  call stvp2uv(x,idim) ! x:=Ax
  zz2 = my_dot_product(reshape(x,(/2*nlat*nlon/)),reshape(y,(/2*nlat*nlon/)))

  if ( abs(zz1+zz2) > sqrt(tiny(zz3)) ) then
     zz3=two*abs(zz1-zz2)/(zz1+zz2)
  else
     zz3=abs(zz1-zz2)
  endif
  idig= int(-log(abs(zz3)+tiny(zz3))/log(10.0_r_kind))

  if (mype==0) then
    write(6,'(A)')' ADTEST_STVP2UV            0.123456789012345678'
    write(6,'(A,ES25.18)')' ADTEST_STVP2UV <F''*F.Y,  X>= ',zz1
    write(6,'(A,ES25.18)')' ADTEST_STVP2UV <   F.Y,F.X>= ',zz2
    write(6,'(A,i3,   A)')' ADTEST_STVP2UV ',idig,' digits are identical'
    write(6,'(A,ES25.18)')' ADTEST_STVP2UV rel. err.= ',zz3
    write(6,'(A,ES25.18)')' ADTEST_STVP2UV mach.eps = ',epsilon(zz3)
  endif
  deallocate(x,y,z)
  iadtest_cnt(4) = iadtest_cnt(4) + 1
end subroutine adtest_stvp2uv
 
real(r_kind) function my_dot_product(x,y)
  use mpl_allreducemod, only: mpl_allreduce
  use m_kinds, only: r_quad
  implicit none
  real(r_kind),intent(in) :: x(:),y(:)
  integer i
  real(r_quad) :: accm(1)
  accm(1) = 0._r_quad
  do i=1,size(x)
    accm(1) = accm(1) + x(i)*y(i)
  enddo
  call mpl_allreduce(1,qpvals=accm)
  my_dot_product = accm(1)
end function my_dot_product
! ----------------------------------------------------------------------
end module gsibec_adjtest_mod
