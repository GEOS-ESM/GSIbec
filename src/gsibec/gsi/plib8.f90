!=============================================================================
subroutine coefrf(sig,nu,n,m,bnf,lnf)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    coefrf
!
!   prgrmmr:     R.J.Purser, NCEP 2001
!
! abstract:    
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     n, m    -
!     sig, nu - 
!
!   output argument list:
!     bnf     -
!     lnf     -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use m_plib8mat2
use m_kinds, only: i_kind
use m_kinds, only: r_kind => r_double
use constants, only: zero,half,one
implicit none

integer(i_kind),              intent(IN   ) :: n,m
real(r_kind), dimension(n),   intent(IN   ) :: sig,nu
real(r_kind), dimension(n),   intent(  OUT) :: bnf
real(r_kind), dimension(m,n), intent(  OUT) :: lnf
!-------------------------------------------------------------------------- 
integer(i_kind), parameter                  :: irmax=6
real(r_kind), dimension(n,-m:m)             :: s
real(r_kind), dimension(n,-m:0)             :: sl
real(r_kind), dimension(n,-m:m,m)           :: k,l
real(r_kind), dimension(n)                  :: eta
real(r_kind), dimension(irmax)              :: bcofi,bcofh
integer(i_kind)                             :: i,i1,il,ir,ik
!--------------------------------------------------------------------------
! The coefficients bcofi are the reciprocals of the i=1 entries of TABLE 1
! of NCEP O.N. 431:
data bcofi/one, 12._r_kind, 90._r_kind, 560._r_kind, 3150._r_kind, 16632._r_kind/
!=============================================================================
bcofh=half/bcofi
do i=1,n
   eta(i)=sig(i)*sqrt(nu(i))
enddo
k=zero
!-------------------------------------------------------------------------
! Set k(:, -1:1, 1) to be the K-matrix of (4.8)--(4.10) of NCEP O.N. 431: 
!--------------------------------------------------------------------------
do i=1,n-1
   k(i  , 0,1)=k(i  ,0,1)+eta(i+1)/eta(i  )
   k(i+1, 0,1)=k(i+1,0,1)+eta(i  )/eta(i+1)
   k(i  , 1,1)=-one
   k(i+1,-1,1)=-one
enddo

!-------------------------------------------------------------------------
! Set k(:, : , ir) to be the original K-matrix raised to the power of (ir):
!--------------------------------------------------------------------------
do ir=2,m
   il=ir-1
   call mulbb(k(:,-1:1,1),k(:,-il:il,il),k(:,-ir:ir,ir),n,n,1,1,il,il,ir,ir)
enddo

!-------------------------------------------------------------------------
! Pre- and post-multiply each of the m powers of K by the diagonal matrix,
! sigma, of NCEP O.N. 431, where the elements of sigma measure the smoothing
! scale of the quasi-Gaussian filter in grid-space units.
! Also, multiply each of the resulting banded matrices by .5*b_{1,ir} for
! the appropriate index, ir, corresponding to the power by which the original
! K was raised.
!--------------------------------------------------------------------------
do ir=1,m
   call mulbd(k(:,-ir:ir,ir),sig,k(:,-ir:ir,ir),n,n,ir,ir)
   call muldb(sig,k(:,-ir:ir,ir),k(:,-ir:ir,ir),n,n,ir,ir)
   k(:,-ir:ir,ir)=k(:,-ir:ir,ir)*bcofh(ir)
enddo


s=zero
s(:,0)=one

do ir=1,m
   l(:,-ir:ir,ir)=k(:,-ir:ir,ir)
   s(:,-ir:ir)=s(:,-ir:ir)+l(:,-ir:ir,ir)
enddo
do i1=2,m
   do ir=m,i1,-1
      l(:,-ir:ir,ir)=zero
      do ik=1,ir-i1+1
         il=ir-ik
         call madbb(k(:,-ik:ik,ik),l(:,-il:il,il),l(:,-ir:ir,ir), &
              n,n,ik,ik,il,il,ir,ir)
      enddo
      l(:,-ir:ir,ir)=l(:,-ir:ir,ir)/i1
      s(:,-ir:ir)=s(:,-ir:ir)+l(:,-ir:ir,ir)
   enddo
enddo
call ldlb(s,sl,bnf,n,m)
do i1=1,m
   do i=1,n
      lnf(i1,i)=sl(i,-i1)
   enddo
enddo
end subroutine coefrf


!============================================================================
subroutine ldlb1i(nol,lnf,bnf,                                              &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb1i
!
!   prgrmmr:     
!
! abstract:    
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol      -
!     ims, ime -
!     its, ite -
!     bnf      -
!     lnf      -
!
!   output argument list:
!     bnf      -
!     lnf      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime
  INTEGER(i_kind), INTENT(IN   ) :: its,ite

  REAL(r_kind), DIMENSION(ims:ime),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do i=its,ite
   nola=min(nol,i-its)
   do l=nola,1,-1
      s=lnf(l,i)
      do m=l+1,nola
         s=s-lnf(m,i)*bnf(i-m)*lnf(m-l,i-l)
      enddo
      lnf(l,i)=s/bnf(i-l)
   enddo
   s=bnf(i)
   do l=1,nola
      s=s-lnf(l,i)**2*bnf(i-l)
   enddo
   bnf(i)=s
enddo
end subroutine ldlb1i

   
!============================================================================
subroutine ldlb2i(nol,lnf,bnf,                                              &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb2i
!
!   prgrmmr:     
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol                -
!     ims, ime, jms, jme -
!     its, ite, jts, jte -
!     bnf                -
!     lnf                -
!
!   output argument list:
!     bnf                -
!     lnf                -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do j=jts,jte
   do i=its,ite
      nola=min(nol,i-its)
      do l=nola,1,-1
         s=lnf(l,i,j)
         do m=l+1,nola
            s=s-lnf(m,i,j)*bnf(i-m,j)*lnf(m-l,i-l,j)
         enddo
         lnf(l,i,j)=s/bnf(i-l,j)
      enddo
      s=bnf(i,j)
      do l=1,nola
         s=s-lnf(l,i,j)**2*bnf(i-l,j)
      enddo
      bnf(i,j)=s
   enddo
enddo
end subroutine ldlb2i

   
!============================================================================
subroutine ldlb2j(nol,lnf,bnf,                                              &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb2
!
!   prgrmmr:     
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     bnf              -
!     lnf              -
! 
!   output argument list:
!     bnf              -
!     lnf              -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do j=jts,jte
   nola=min(nol,j-jts)
   do i=its,ite
      do l=nola,1,-1
         s=lnf(l,i,j)
         do m=l+1,nola
            s=s-lnf(m,i,j)*bnf(i,j-m)*lnf(m-l,i,j-l)
         enddo
         lnf(l,i,j)=s/bnf(i,j-l)
      enddo
      s=bnf(i,j)
      do l=1,nola
         s=s-lnf(l,i,j)**2*bnf(i,j-l)
      enddo
      bnf(i,j)=s
   enddo
enddo
end subroutine ldlb2j

   
!============================================================================
subroutine ldlb3i(nol,lnf,bnf,                                              &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb3i
!
!   prgrmmr:    
!
! abstract:    
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do j=jts,jte
   do k=kts,kte
      do i=its,ite
         nola=min(nol,i-its)
         do l=nola,1,-1
            s=lnf(l,i,k,j)
            do m=l+1,nola
               s=s-lnf(m,i,k,j)*bnf(i-m,k,j)*lnf(m-l,i-l,k,j)
            enddo
            lnf(l,i,k,j)=s/bnf(i-l,k,j)
         enddo
         s=bnf(i,k,j)
         do l=1,nola
            s=s-lnf(l,i,k,j)**2*bnf(i-l,k,j)
         enddo
         bnf(i,k,j)=s
      enddo
   enddo
enddo
end subroutine ldlb3i

   
!============================================================================
subroutine ldlb3j(nol,lnf,bnf,                                              &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb3j
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do j=jts,jte
   nola=min(nol,j-jts)
   do k=kts,kte
      do i=its,ite
         do l=nola,1,-1
            s=lnf(l,i,k,j)
            do m=l+1,nola
               s=s-lnf(m,i,k,j)*bnf(i,k,j-m)*lnf(m-l,i,k,j-l)
            enddo
            lnf(l,i,k,j)=s/bnf(i,k,j-l)
         enddo
         s=bnf(i,k,j)
         do l=1,nola
            s=s-lnf(l,i,k,j)**2*bnf(i,k,j-l)
         enddo
         bnf(i,k,j)=s
      enddo
   enddo
enddo
end subroutine ldlb3j

   
SUBROUTINE hbnrf1i(a,nol,lnf,bnf,                                           &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf1i
!
!   prgrmmr:    
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                1-dimensional, active index i
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol     -
!     ims,ime -
!     its,ite -
!     a       -
!     bnf     -
!     lnf     -
!
!   output argument list:
!     a       -
!     bnf     -
!     lnf     -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime
  INTEGER(i_kind), INTENT(IN   ) :: its,ite

  REAL(r_kind), DIMENSION(ims:ime),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,l,nola
!============================================================================
DO i=its+1,ite
   nola=MIN(nol,i-its)
   DO l=1,nola
      a(i)=a(i)-lnf(l,i)*a(i-l)
   ENDDO
ENDDO
DO i=its,ite
   a(i)=bnf(i)*a(i)
ENDDO
DO i=ite-1,its,-1
   nola=MIN(nol,ite-i)
   DO l=1,nola
      a(i)=a(i)-lnf(l,i+l)*a(i+l)
   ENDDO
ENDDO
END SUBROUTINE hbnrf1i


SUBROUTINE hbnrf2i(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf2i
!
!   prgrmmr:   
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                2-dimensional, active index i
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     a                -
!     bnf              -
!     lnf              -
!
!   output argument list:
!     a                -
!     bnf              -
!     lnf              -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,l,nola
!============================================================================
DO j=jts,jte
   DO i=its+1,ite
      nola=MIN(nol,i-its)
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j)*a(i-l,j)
      ENDDO
   ENDDO
   DO i=its,ite
      a(i,j)=bnf(i,j)*a(i,j)
   ENDDO
   DO i=ite-1,its,-1
      nola=MIN(nol,ite-i)
      DO l=1,nol
         a(i,j)=a(i,j)-lnf(l,i+l,j)*a(i+l,j)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf2i


SUBROUTINE hbnrf2j(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf1i
!
!   prgrmmr:   
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                2-dimensional, active index j
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     a                -
!     bnf              -
!     lnf              -
!
!   output argument list:
!     a                -
!     bnf              -
!     lnf              -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,l,nola
!============================================================================
DO j=jts+1,jte
   nola=MIN(nol,j-jts)
   DO i=its,ite
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j)*a(i,j-l)
      ENDDO
   ENDDO
ENDDO
DO j=jts,jte
   DO i=its,ite
      a(i,j)=bnf(i,j)*a(i,j)
   ENDDO
ENDDO
DO j=jte-1,jts,-1
   nola=MIN(nol,jte-j)
   DO i=its,ite
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j+l)*a(i,j+l)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf2j


SUBROUTINE hbnrf3i(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf3i
!
!   prgrmmr:   
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                3-dimensional, active index i
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     a                         -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     a                         -
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,nola
!============================================================================
DO j=jts,jte
   DO k=kts,kte
      DO i=its+1,ite
         nola=MIN(nol,i-its)
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i-l,k,j)
         ENDDO
      ENDDO
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
      DO i=ite-1,its,-1
         nola=MIN(nol,ite-i)
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i+l,k,j)*a(i+l,k,j)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf3i


SUBROUTINE hbnrf3j(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf3j
!
!   prgrmmr:   
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                3-dimensional, active index j
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     a                         -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     a                         -
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,nola
!============================================================================
DO j=jts+1,jte
   nola=MIN(nol,j-jts)
   DO k=kts,kte
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i,k,j-l)
         ENDDO
      ENDDO
   ENDDO
ENDDO
DO j=jts,jte
   DO k=kts,kte
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
   ENDDO
ENDDO
DO j=jte-1,jts,-1
   nola=MIN(nol,jte-j)
   DO k=kts,kte
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j+l)*a(i,k,j+l)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf3j


SUBROUTINE vbnrf1k(a,nol,lnf,bnf,                                           &
       kms,kme,                                                             &
       kts,kte                                                              )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vbnrf1k
!
!   prgrmmr:   
!
! abstract:      Vertical bounded grid inhomogeneous recursive filter, 
!                1-dimensional, active index k
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol     -
!     kms,kme -
!     kts,kte -
!     a       -
!     bnf     -
!     lnf     -
!
!   output argument list:
!     a       -
!     bnf     -
!     lnf     -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: kts,kte

  REAL(r_kind), DIMENSION(kms:kme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(kms:kme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, kms:kme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: k,l,nola
!============================================================================
DO k=kts+1,kte
   nola=MIN(nol,k-kts)
   DO l=1,nola
      a(k)=a(k)-lnf(l,k)*a(k-l)
   ENDDO
ENDDO
DO k=kts,kte
   a(k)=bnf(k)*a(k)
ENDDO
DO k=kte-1,kts,-1
   nola=MIN(nol,kte-k)
   DO l=1,nola
      a(k)=a(k)-lnf(l,k+l)*a(k+l)
   ENDDO
ENDDO
END SUBROUTINE vbnrf1k


SUBROUTINE vbnrf2k(a,nol,lnf,bnf,                                           &
       ims,ime, kms,kme,                                                    &
       its,ite, kts,kte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vbnrf2k
!
!   prgrmmr:   
!
! abstract:      Vertical bounded grid inhomogeneous recursive filter, 
!                2-dimensional, active index k
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, kms,kme -
!     its,ite, kts,kte -
!     a                -
!     bnf              -
!     lnf              -
!
!   output argument list:
!     a                -
!     bnf              -
!     lnf              -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,k,l,nola
!============================================================================
DO k=kts+1,kte
   nola=MIN(nol,k-kts)
   DO i=its,ite
      DO l=1,nola
         a(i,k)=a(i,k)-lnf(l,i,k)*a(i,k-l)
      ENDDO
   ENDDO
ENDDO
DO k=kts,kte
   DO i=its,ite
      a(i,k)=bnf(i,k)*a(i,k)
   ENDDO
ENDDO
DO k=kte-1,kts,-1
   nola=MIN(nol,kte-k)
   DO i=its,ite
      DO l=1,nola
         a(i,k)=a(i,k)-lnf(l,i,k+l)*a(i,k+l)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE vbnrf2k


SUBROUTINE vbnrf3k(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vbnrf3k
!
!   prgrmmr:   
!
! abstract:      Vertical bounded grid inhomogeneous recursive filter, 
!                3-dimensional, active index k
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     a                         -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     a                         -
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,nola
!============================================================================
DO j=jts,jte
   DO k=kts+1,kte
      nola=MIN(nol,k-kts)
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i,k-l,j)
         ENDDO
      ENDDO
   ENDDO
   DO k=kts,kte
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
   ENDDO
   DO k=kte-1,kts,-1
      nola=MIN(nol,kte-k)
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k+l,j)*a(i,k+l,j)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE vbnrf3k


SUBROUTINE hbncij(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,                           &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbncij 
!
!   prgrmmr:   
!
! abstract:      
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     hamp,bnfi,bnfj   -
!     lnfi,lnfj        -
!     a                -
!
!   output argument list:
!     a                -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(IN   ) :: hamp,bnfi,bnfj
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(IN   ) :: lnfi,lnfj
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j
!============================================================================
DO j=jts,jte
   DO i=its,ite
      a(i,j)=hamp(i,j)*a(i,j)
   ENDDO
ENDDO
!---------------
CALL hbnrf2i(a,nol,lnfi,bnfi,             &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
CALL hbnrf2j(a,nol,lnfj,bnfj,             &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
END SUBROUTINE hbncij


SUBROUTINE hbncji(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,                           &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbncji
!
!   prgrmmr:   
!
! abstract:     
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     hamp,bnfi,bnfj   -
!     lnfi,lnfj        -
!     a                -
!
!   output argument list:
!     a                -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(IN   ) :: hamp,bnfi,bnfj
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(IN   ) :: lnfi,lnfj
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j
!============================================================================
CALL hbnrf2j(a,nol,lnfj,bnfj,             &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
CALL hbnrf2i(a,nol,lnfi,bnfi,             &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!---------------
DO j=jts,jte
   DO i=its,ite
      a(i,j)=hamp(i,j)*a(i,j)
   ENDDO
ENDDO
!---------------
END SUBROUTINE hbncji


SUBROUTINE hbncijk(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,lnfk,bnfk,                &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbncijk
!
!   prgrmmr:   
!
! abstract: 
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     hamp,bnfi,bnfj,bnfk       -
!     lnfi,lnfj,lnfk            -
!     a                         -
!
!   output argument list:
!     a                         -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: hamp,bnfi,bnfj,bnfk
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnfi,lnfj,lnfk
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k
!============================================================================
DO j=jts,jte
   do k=kts,kte
      DO i=its,ite
         a(i,k,j)=hamp(i,k,j)*a(i,k,j)
      ENDDO
   enddo
ENDDO
!---------------
CALL hbnrf3i(a,nol,lnfi,bnfi,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3j(a,nol,lnfj,bnfj,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
call vbnrf3k(a,nol,lnfk,bnfk,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
END SUBROUTINE hbncijk


SUBROUTINE hbnckji(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,lnfk,bnfk,                &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnckji
!
!   prgrmmr:   
!
! abstract:
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     hamp,bnfi,bnfj,bnfk       -
!     lnfi,lnfj,lnfk            -
!     a                         -
!
!   output argument list:
!     a                         -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use m_kinds, only: i_kind
  use m_kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: hamp,bnfi,bnfj,bnfk
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnfi,lnfj,lnfk
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k
!============================================================================
call vbnrf3k(a,nol,lnfk,bnfk,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3j(a,nol,lnfj,bnfj,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3i(a,nol,lnfi,bnfi,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!---------------
DO j=jts,jte
   do k=kts,kte
      DO i=its,ite
         a(i,k,j)=hamp(i,k,j)*a(i,k,j)
      ENDDO
   enddo
ENDDO
!---------------
END SUBROUTINE hbnckji


!============================================================================
subroutine rfit(ng,sig,nu, ns,nw,ssig,snu,ins1,wts)  
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    rfit
!
!   prgrmmr:     R. J. Purser, NCEP 2001
!
! abstract:     
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     ng
!     sig,nu
!     ins1
!     wts
!
!   output argument list:
!     ins1
!     wts
!     ns,nw
!     ssig,snu
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use m_kinds, only: i_kind
use m_kinds, only: r_kind => r_double
use constants, only: zero,one
use m_fitcons
implicit none

integer(i_kind),                  intent(IN   ) :: ng
real(r_kind)   , dimension(ng),   intent(IN   ) :: sig,nu
integer(i_kind),                  intent(  OUT) :: ns,nw
real(r_kind)   , dimension(ng),   intent(  OUT) :: ssig,snu
integer(i_kind), dimension(ng),   intent(INOUT) :: ins1
real(r_kind)   , dimension(no,ng),intent(INOUT) :: wts
!----------------------------------------------------------------------------
integer(i_kind)                                :: i,i1,im,k,l,is
real(r_kind)                                   :: t
real(r_kind)   , dimension(-nohm:ng+noh)       :: dcdg
real(r_kind)   , dimension(-noh:ng+noh)        :: cofg,cofs
real(r_kind)   , dimension(ng)                 :: dsdg,dhdg
!============================================================================
nw=0
do i=1,ng
   dcdg(i)=one/sig(i)
   if(sig(i) <= sigb)then
!----------------------------------------------------------------------------
! sig(i) below threshold; cleave to original grid spacing with ds/dg and 
! dh/dg set accordingly:
!----------------------------------------------------------------------------
      dsdg(i)=one     ;      dhdg(i)=zero
   else
!----------------------------------------------------------------------------
! sig(i) exceeds basic threshold sigb, allowing working grid with coordinate
! s to differ from original grid with coordinate g. The formula for ds/dg
! is now <1 but tends smoothly to 1 again at the threshold value, sig=sigb.
! [The function for log(ds/dg) is based on the "hyper-hyperbola":
!    y= (1+x**4)**(-4)-1, which rises very gradually from its base at x=y=0]
! Likewise, the perturbative component, dh/dg, is now < 0, but tends
! smoothly to 0 again at the threshold.
!----------------------------------------------------------------------------
      t=ldsig-sqrt(sqrt(ldsig4+(ldsig-log(sigc*dcdg(i)))**4))
      dsdg(i)=exp(t) ;      dhdg(i)=dsdg(i)*t
   endif
enddo

!----------------------------------------------------------------------------
! Apply mirror-symmetry to extrapolate beyond ends:
!----------------------------------------------------------------------------
do l=1,noh
   dcdg(1-l)=dcdg(l); dcdg(ng+l)=dcdg(ng+1-l)
enddo

!----------------------------------------------------------------------------
! Integrate dc/dg wrt g to get c(g) at each of the points of the g-grid
! which is NOT staggered relative to the boundary
!----------------------------------------------------------------------------
cofg(0)=zero
do i=1,ng
   cofg(i)=cofg(i-1)+dot_product(qco,dcdg(i-noh:i+noh))
enddo
do l=1,noh
   cofg(  -l)=-cofg(   l)
   cofg(ng+l)=-cofg(ng-l)+2*cofg(ng)
enddo

im=0
ns=0
!----------------------------------------------------------------------------
! loop over noncontiguous segments where it is numerically beneficial
! to employ a grid of relatively coarse resolution. The adoption of each
! alternative grid segment is subject to some conditions:
! 1) Each coarse-grid segment must span at least 5 points of the original grid
! 2) Each segment must shorten the tally of working grid points by at least 3.
!     Subject to the above conditions, the coarse grid is blended smoothly
! with the original grid at internal thresholds and is designed to provide
! a resolution such that the smoothing scale, sigma, never exceeds the 
! product, sigc*dg/ds, where sigc is a dimensionless parameter (e.g. sigc=3.)
! and dg/ds is the local working grid (s) spacing in units of the original
! grid (g) spacing. 
!
! Each segment found is defined by its end points in the original grid,
! i1 and im. k is the counter for segments along this line.
! ns keeps count of the number of working grid (s-grid) points found so far.
!----------------------------------------------------------------------------
cofs(0)=zero
do k=1,ng 
   do i1=im+1,ng
      if(i1< ng-3 .and. dhdg(i1) /= zero)exit
!----------------------------------------------------------------------------
! working s-grid continues to track the original g-grid; Set indices and 
! weight for the trivial "interpolation" between these coincident grids:
!----------------------------------------------------------------------------
      ns=ns+1
      ins1(i1)=-ns
      cofs(ns)=cofg(i1)
   enddo
   if(i1 > ng)exit
!----------------------------------------------------------------------------
! Having met the basic conditions for the start of a new segment in which
! the s-grid and g-grids may part company, seek the other end, im, of this
! possible segment:
!----------------------------------------------------------------------------
   do im=i1+1,ng
      if(dhdg(im) == zero)exit
   enddo
   im=im-1
   if(im < i1+4)then
!----------------------------------------------------------------------------
! Segment too short to be viable; keep s-grid and g-grids synchronized:
!----------------------------------------------------------------------------
      do i=i1,im
         ns=ns+1
         ins1(i)=-ns
         cofs(ns)=cofg(i)
      enddo
   else
!----------------------------------------------------------------------------
! Segment long enough to be potentially viable. Call jfit to determine if 
! the final condition is met, namely that the number of s-grid points 
! in this segment is smaller than the g-grid tally by at least 3. If so,
! Fit an exact integer number of s-points into this segment and compute
! the indices and weights for the associated nontrivial interpolation
! from these (and neighboring) s-points back to the g-points of the segment:
!----------------------------------------------------------------------------
      call jfit(ng,i1,im,ns,nw,cofg,dsdg,dhdg,cofs,ins1,wts)
      if(ns<0) return
   endif
enddo
if(ns<no .and. nw>0)then ! <- s-grid too short; use copy of g-grid instead
   wts(:,1:nw)=zero
   nw=0
   do i=1,ng
      ins1(i)=-i
      cofs(i)=cofg(i)
   enddo
   ns=ng
endif

do l=1,noh
   cofs(  -l)=-cofs(   l)
   cofs(ns+l)=-cofs(ns-l)+2*cofs(ns)
enddo
do is=1,ns
   ssig(is)=one/dot_product(dco,cofs(is-nohp:is+noh))
enddo

!----------------------------------------------------------------------------
! By applying adjoint-interpolation to the g-grid metric terms, obtain
! the corresponding metric terms for the new s-grid:
!----------------------------------------------------------------------------
call stogt(ns,ng,ins1,wts, snu,nu)

end subroutine rfit


!============================================================================
subroutine jfit(ng,ig1,igm,ns,iw,cofg,dsdg,dhdg,cofs,ins1,wts)
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    jfit
!
!   prgrmmr:     R. J. Purser, NCEP 2001
!
! abstract: 
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     ng,ig1,igm
!     ns,iw
!     dsdg,dhdg
!     cofg
!     cofs
!     ins1
!     wts
!
!   output argument list:
!     ns,iw
!     cofs
!     ins1
!     wts
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use m_kinds, only: i_kind
use m_kinds, only: r_kind => r_double
use constants, only: zero,half,one,two
use m_fitcons
implicit none

integer(i_kind),                         intent(IN   ) :: ng,ig1,igm
integer(i_kind),                         intent(INOUT) :: ns,iw
real(r_kind)   , dimension(ng),          intent(IN   ) :: dsdg,dhdg
real(r_kind)   , dimension(-noh:ng+noh), intent(IN   ) :: cofg
real(r_kind)   , dimension(-noh:ng+noh), intent(INOUT) :: cofs
integer(i_kind), dimension(ng),          intent(INOUT) :: ins1
real(r_kind)   , dimension(no,ng),       intent(INOUT) :: wts
!----------------------------------------------------------------------------
real(r_kind)   , dimension(-noh:ng+noh)               :: sofg,dsdgt
real(r_kind)                                          :: et,estar,destar,r,dr,sm
integer(i_kind)                                       :: i,l,ie,iep,ie1,ien,ig0,is0,ism,init
!============================================================================

!----------------------------------------------------------------------------
! Form the definite integral sm, of ds/dg, within this segment:
!----------------------------------------------------------------------------
sm=sum(dsdg(ig1:igm)) 

!---------------------------------------------------------------------------
! Test whether it is worthwhile to allow s-grid to deviate from the original
! g-grid within this segment on the basis of the number of grid points that
! could potentially be eliminated (we require a saving > 3 per segment):
!---------------------------------------------------------------------------
if(sm > igm-ig1-2)then
!----------------------------------------------------------------------------
! This putative s-grid segment reduces the total number of grid points by an
! insufficient amount to justify the additional interpolations. Therefore,
! keep the original g-grid instead for this segment, and return:
!---------------------------------------------------------------------------
   do i=ig1,igm
      ns=ns+1
      ins1(i)=-ns
      cofs(ns)=cofg(i)
   enddo
   return
endif
!----------------------------------------------------------------------------
! s-grid segment achieves a worthwhile reduction of the number of points
! of the working grid. The tasks of the rest of this routine are to:
! (1) adjust the segment length in the s-metric to make it an integer;
! (2) find the s-coordinates of each g-grid points in this segment
!     and hence the nontrivial interpolation indices and weights required 
!     to go from the s-grid to the g-grid (or adjoints going the other way);
! (3) use Newton iterations to find the accurate interpolation formulae
!     that enable c(s) to be interpolated from the given c(g).
!----------------------------------------------------------------------------
ig0=ig1-1
is0=ns; ism=sm
!----------------------------------------------------------------------------
! Fractional remainder of sm, divided by the definite integral of dh/dg
! provides the adjustment factor that scales the perturbative component,
! dhdg, by exactly the amount that will make the segment integral of the 
! perturbed grid-to-grid jacobian, dsdgt, the exact integer, ism:
!----------------------------------------------------------------------------
r=(sm-ism)/sum(dhdg(ig1:igm))
do i=ig1,igm
   dsdgt(i)=dsdg(i)-r*dhdg(i)
enddo
!----------------------------------------------------------------------------
! Mirror-extrapolate adjusted ds/dg as an even-symmetry function at the 
! ends of this segment. Note that the grid on which derivatives such as
! ds/dg reside is the one staggered wrt domain boundaries and segment
! end points. The indices of this grid go from ig1 to igm inside the
! segment. (The convention for the companion grid, NOT staggered wrt 
! boundaries, is such that the two segment ends are denoted by indices,
! ig0=ig1-1 and igm.)
!----------------------------------------------------------------------------
do l=1,noh
   dsdgt(ig1-l)=dsdgt(ig0  +l)
   dsdgt(igm+l)=dsdgt(igm+1-l)
enddo
ism=is0+ism ! This integer also becomes (within round-off) the value, sofg(igm)
!----------------------------------------------------------------------------
! Set s(g) at both ends of the segment to be the appropriate integers:
!----------------------------------------------------------------------------
sofg(ig0)=is0; sofg(igm)=ism
!----------------------------------------------------------------------------
! Get s(g) inside the segment by performing a numerical quadrature of dsdgt:
!----------------------------------------------------------------------------
do i=ig1,igm
   sofg(i)=sofg(i-1)+dot_product(qco,dsdgt(i-noh:i+noh))
enddo
!----------------------------------------------------------------------------
! Mirror-extrapolate s(g) as an odd-symmetry function at segment end points.
! Note that, being an inegral, s(g) resides on the grid NOT staggered wrt
! boundaries and segment end points.
!----------------------------------------------------------------------------
do l=1,noh
   sofg(ig0-l)=two*is0-sofg(ig0+l)
   sofg(igm+l)=two*ism-sofg(igm-l)
enddo
do i=ig1,igm
   iw=iw+1 ; wts(:,iw)=zero
   r=dot_product(ico,sofg(i-nohp:i+noh))+half
   ie=r            ! Take integer part...
   ins1(i)=ie-nohm ! ...hence the index of the first point in the stencil...
   r=r-ie          ! ...then the fractional part to find interpolation weights:
   call lagw(hunit1,r,q1,wt1,dwt1,nom)   ! weights for left-biased stencil
   wts(:nom,iw) =            (one-r)*wt1 !   bias weight, 1-r
   call lagw(hunit2,r,q1,wt1,dwt1,nom)   ! weights for right-biased stencil
   wts(2:   ,iw) = wts(2:   ,iw)  +r*wt1 !   bias weight, r.
!----------------------------------------------------------------------------
! Exploit the mirror symmetries to confine the weight stencil to the 
! domain interior, even though this may entail padding innermost end of
! the stencil with useless zeroes:
!----------------------------------------------------------------------------
   L=1-INS1(I)
   IF(L > 0)THEN ! FOLD LEFT OVERLAP OF L ELEMENTS BACK INSIDE: 
      WTS(1:L,IW)      =WTS(L:1:-1,IW)+WTS(L+1:L*2,IW) ! FOLD INTO 1ST L
      WTS(L+1:NO-L,IW) =WTS(L*2+1:NO,IW)            ! SHIFT THE REST LEFT
      WTS(NOP-L:NO,IW)=zero ! SET TRAILING L ELEMENTS TO ZERO
      INS1(I)=1          ! RESET INDEX OF FIRST POINT OF STENCIL
   ENDIF
   l=ins1(i)+nom-ism
   if(l > 0)then ! Fold right overlap of L elements back inside:
      wts(nop-l:no,iw)=wts(no:nop-l:-1,iw)+wts(nop-l*2:no-l,iw) ! Fold last L
      wts(l+1:no-l,iw)=wts(1:no-l*2,iw)                         ! Shift right
      wts(1:l,iw)=zero   ! Set first L elements to zero
      ins1(i)=ism-nom    ! reset index of first point of stencil
   endif
enddo
ns=ism

!----------------------------------------------------------------------------
! Use Newton-Raphson iterations to locate the g-coordinates of all this
! segment's s-grid points. Then interpolate the function c to each of
! these s-grid points. (Note that, in the present context, the
! s- and g-grids are the ones NOT staggered wrt the domain boundaries.)
!----------------------------------------------------------------------------
ie=ig0
iloop: do i=is0+1,ism-1 ! Loop over s-grid target points interior to this segment
   et=i
!----------------------------------------------------------------------------
! Find the g-grid interval containing this target: 
!----------------------------------------------------------------------------
   do iep=ie+1,igm-1;  if(sofg(iep) > et)exit; enddo
   do ie=iep-1,ig1,-1; if(sofg(ie) <= et)exit; enddo

   ie1=ie-nohm;   ien=ie+noh   ! <-- Set the interpolation stencil range:

   r=(et-sofg(ie))/(sofg(ie+1)-sofg(ie)) ! Linearly estimate interval fraction

!----------------------------------------------------------------------------
! Perform Newton-Raphson iterations to refine interval fraction, r:
!----------------------------------------------------------------------------
   do init=1,nnit
      call lagw(hunit,r,q,wt,dwt,no) ! Get Lagrange weights, wt and d(wt)/dg
      estar =dot_product(wt, sofg(ie1:ien))-et ! <- Residual error, estar.
      destar=dot_product(dwt,sofg(ie1:ien))    ! <- d(estar)/dg.
      dr=-estar/destar                         ! <- Newton correction to r
      r=r+dr                                   ! <- Refined estimate, r
      if(abs(dr) <= rcrit)then                 ! <- Converged enough yet?
        wt=wt+dr*dwt                           ! <- Final refinement to wt
        cofs(i)=dot_product(wt, cofg(ie1:ien)) ! <- Interpolate c(s)
        cycle iloop
      end if
   enddo
 ! stop 'Too many Newton iterations'           ! <- It never convergenced! 
   write(6,*)' Too many Newton iterations'           ! <- It never convergenced! 
   ns=-1
   return
enddo iloop
cofs(ism)=cofg(igm)                            ! <- End value directly
return
end subroutine jfit


!============================================================================
subroutine stog(ns,ng,ins1,wts, as,ag) 
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    stog
!
!   prgrmmr:     R. J. Purser NCEP 2001
!
! abstract:      Forward interpolation from s-grid to g-grid
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     ns,ng - sizes of s and g grids
!     ins1  - array of 1st stencil indices (s-grid) for each target (g) point.
!     wts   - interpolation weights for each target (g-grid point).
!     as    - s-grid array of source data.
!
!   output argument list:
!     ag    - g-grid array of interpolated target data.
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use m_kinds, only: i_kind
use m_kinds, only: r_kind => r_double
use constants, only: zero
implicit none

integer(i_kind), parameter                      :: noh=3,no=noh*2,nom=no-1
integer(i_kind),                  intent(IN   ) :: ns,ng
integer(i_kind), dimension(ng),   intent(IN   ) :: ins1
real(r_kind)   , dimension(no,ng),intent(IN   ) :: wts
real(r_kind)   , dimension(ns),   intent(IN   ) :: as
real(r_kind)   , dimension(ng),   intent(  OUT) :: ag
!----------------------------------------------------------------------------
integer(i_kind)                                 :: i,is,iw
!============================================================================
iw=0
ag=zero
do i=1,ng
   is=ins1(i)
   if(is>0)then
      iw=iw+1
      ag(i)=dot_product(wts(:,iw),as(is:is+nom))
   else
      ag(i)=as(-is)
   endif
enddo
end subroutine stog


!============================================================================
subroutine stogt(ns,ng,ins1,wts, as,ag) 
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    stogt
!
!   prgrmmr:     R. J. Purser NCEP 2001
!
! abstract:      Perform the transpose of the operation defined by stog
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     ns,ng
!     ins1
!     wts
!     ag
!
!   output argument list:
!     as
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use m_kinds, only: i_kind
use m_kinds, only: r_kind => r_double
use constants, only: zero
implicit none

integer(i_kind), parameter                      :: noh=3,no=noh*2,nom=no-1
integer(i_kind),                  intent(IN   ) :: ns,ng
integer(i_kind), dimension(ng),   intent(IN   ) :: ins1
real(r_kind)   , dimension(no,ng),intent(IN   ) :: wts
real(r_kind)   , dimension(ns),   intent(  OUT) :: as
real(r_kind)   , dimension(ng),   intent(IN   ) :: ag
!----------------------------------------------------------------------------
integer(i_kind)                                 :: i,is,iw
!============================================================================
iw=0
as=zero
do i=1,ng
   is=ins1(i)
   if(is>0)then
      iw=iw+1
      as(is:is+nom)=as(is:is+nom)+wts(:,iw)*ag(i)
   else
      as(-is)=as(-is)+ag(i)
   endif
enddo
end subroutine stogt


