module m_fitcons
!$$$ module documentation block
!           .      .    .                                       .
! module:  module_fitcons
!  prgmmr: purser
!
! abstract:
!
! program history log:
!   1994-  -    purser
!   2008-04-28  safford - add stander module documentation block
!
! subroutines included:
!   setq
!   lagw
!   infit
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

!============================================================================
use m_kinds, only: i_kind
use m_kinds, only: r_kind => r_double
use constants, only: zero,one,two,three,five
implicit none

! set default to private
  private
! set subroutines to public
  public :: setq
  public :: lagw
  public :: infit
! set passed variables to public
  public :: qco,dco,ldsig4,ldsig,nohp,sigc,nohm,no,sigb,noh,ico,wt,dwt
  public :: nop,hunit2,nom,nnit,rcrit,q,hunit,dwt1,wt1,q1,hunit1

integer(i_kind),parameter         :: noh=3,    nohm=noh-1,   nohp=noh+1,&
                                     no=noh*2, nom=no-1,     nop=no+1,   nnit=7
real(r_kind),parameter            :: sigc=three,  sigb=two
real(r_kind),dimension(no)        :: hunit,q,wt,dwt
real(r_kind),dimension(nom)       :: hunit1,hunit2,q1,wt1,dwt1
real(r_kind),dimension(-noh:noh)  :: qco
real(r_kind),dimension(-1-noh:noh):: ico,dco
real(r_kind)                      :: rcrit,ldsig,ldsig4
!============================================================================

contains


!============================================================================
SUBROUTINE setq(q,x,n) 
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setq
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:      Precompute the N constant denominator factors of the 
!                N-point Lagrange polynomial interpolation formula.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     X -   The N abscissae.
!     N -   The number of points involved.
!
!   output argument list:
!     Q -   The N denominator constants.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_kind),          INTENT(in   ) :: n
  REAL(r_kind),DIMENSION(n),INTENT(in   ) :: x
  REAL(r_kind),DIMENSION(n),INTENT(  out) :: q
!-----------------------------------------------------------------------------
  INTEGER(i_kind)                         :: i,j
!=============================================================================
DO i=1,n
   q(i)=one
   DO j=1,n
      IF(j /= i)q(i)=q(i)/(x(i)-x(j))
   ENDDO
ENDDO
END SUBROUTINE setq 


!============================================================================
SUBROUTINE lagw(x,xt,q,w,dw,n) 
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    lagw
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:      Construct the Lagrange weights and their derivatives when 
!                target abscissa is known and denominators Q have already 
!                been precomputed
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     X   - Grid abscissae
!     XT  - Target abscissa
!     Q   - Q factors (denominators of the Lagrange weight formula)
!     N   - Number of grid points involved in the interpolation
!
!   output argument list:
!     W   - Lagrange weights
!     DW  - Derivatives, dW/dX, of Lagrange weights W
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_kind),          INTENT(in   ) :: n
  REAL(r_kind),             INTENT(in   ) :: xt
  REAL(r_kind),DIMENSION(n),INTENT(in   ) :: x,q
  REAL(r_kind),DIMENSION(n),INTENT(  out) :: w,dw
!-----------------------------------------------------------------------------
  REAL(r_kind),DIMENSION(n)               :: sdit,d,di
  INTEGER(i_kind)                         :: i,j
  REAL(r_kind)                            :: p,s,sdil,sdir
!============================================================================
p=one      ! ...will become product of all the d(i)=xt-x(i)
DO i=1,n
   d(i)=xt-x(i)
   p=p*d(i)
ENDDO

!   test p to reveal whether any of the d(i) vanish:
IF(p == zero)THEN   ! xt coincides with a grid point - use special code:
   p=one            ! p will become the product of the nonzero d(i),
   s=zero           ! s will become the corresponding sum of q(i)/d(i)
   DO i=1,n
      IF(d(i) == zero)THEN
         j=i            ! identify the grid index corresponding to present xt
         w(j)=one       ! interpolation weighted entirely to this one.
      ELSE
         w(i)=zero
         p=p*d(i)
         dw(i)=q(i)/d(i)
         s=s+dw(i)
      ENDIF
   ENDDO
   dw(j)=-s*p
   DO i=1,n
      IF(i /= j)dw(i)=dw(i)*p
   ENDDO
ELSE             ! xt is not a grid point - use generic code:
   sdil=zero            ! will become the sum of terms to the left.
   sdir=zero            ! will become the sum of terms to the right.
   DO i=1,n
      di(i)=one/d(i)
      sdit(i)=sdil
      sdil=sdil+di(i)
      w(i)=q(i)*p*di(i)
   ENDDO
   DO i=n,1,-1
      sdit(i)=sdit(i)+sdir
      sdir=sdir+di(i)
      dw(i)=w(i)*sdit(i)
   ENDDO
ENDIF
END SUBROUTINE lagw 



!============================================================================
subroutine infit
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    infit
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

implicit none

integer(i_kind) :: i
real(r_kind)    :: divq,divd
!============================================================================
! Initialize quantities that relate to interpolations:
do i=1,no; hunit(i)=i-noh; enddo
hunit1=hunit(:nom)    ; hunit2=hunit(2:)
call setq(q,hunit,no) ; call setq(q1,hunit1,nom)
rcrit=SQRT(EPSILON(one))
!------------------------------------
! Initialize coefficients for quadrature, differencing and mdpt interpolation:
divq=967680_r_kind        ; divd=1024_r_kind
qco(0)=862564_r_kind/divq ; dco(0)=1225_r_kind/divd     ; ico(0)=1225_r_kind/(2*divd)
qco(1)= 57249_r_kind/divq ; dco(1)=-245_r_kind/(3*divd) ; ico(1)=-245_r_kind/(2*divd)
qco(2)= -5058_r_kind/divq ; dco(2)=  49_r_kind/(5*divd) ; ico(2)=  49_r_kind/(2*divd)
qco(3)=   367_r_kind/divq ; dco(3)=      -five/(7*divd) ; ico(3)=      -five/(2*divd)
qco(-1:-noh:-1)  = qco(1:noh) ! complete the stencil of quadrature coeffs.
dco(-1:-nohp:-1) =-dco(0:noh) ! complete the stencil of difference coeffs
ico(-1:-nohp:-1) = ico(0:noh) ! complete the stencil of interpolation coeffs.
!------------------------------------
! Initial coefficients related to control of working grid resolution:
ldsig =log(sigc/sigb)
ldsig4=ldsig**4
end subroutine infit
end module m_fitcons
