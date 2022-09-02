MODULE m_plib8mat2
!$$$   module documentation block
!                .      .    .                                     .
! module:  m_plib8mat2
!  prgmmr: purser
!
! abstract:
!
! program history log:
!   1994-  -    purser
!   2008-04-25  safford - add documentation block
!
! subroutines included:
!   avco
!   davco
!   dfco
!   ddfco
!   dfco2
!   ddfco2
!   clib
!   dclib
!   cad1b
!   csb1b
!   cad2b
!   csb2b
!   copbt
!   conbt
!   copmb
!   conmb
!   copbm
!   conbm
!   mulbb
!   madbb
!   msbbb
!   ldub
!   dldub
!   l1ubb
!   dl1ubb
!   l1ueb
!   dl1ueb
!   l1lb
!   ldlb
!   dldlb
!   udub
!   dudub
!   mulbv
!   madbv
!   msbbv
!   mulbx
!   madbx
!   msbbx
!   mulby
!   madby
!   msbby
!   mulvb
!   madvb
!   msbvb
!   mulxb
!   madxb
!   msbxb
!   mulyb
!   madyb
!   msbyb
!   mulbd
!   madbd
!   msbbd
!   muldb
!   maddb
!   msbdb
!   udlbv
!   udlbx
!   udlby
!   udlvb
!   udlxb
!   udlyb
!   u1lbv
!   u1lbx
!   u1lby
!   u1lvb
!   u1lxb
!   u1lyb
!   linbv
!   wrtb
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

USE m_plib8mat1
use m_kinds, only: i_kind
use m_kinds, only: my_kind => r_double
use constants, only: zero,one,two
IMPLICIT NONE

! set default to private
  private
! set subroutines/interfaces to public
  public :: avco
  public :: avco_d
  public :: dfco
  public :: dfco_d
  public :: dfco2
  public :: dfco2_d
  public :: clib
  public :: clib_d
  public :: cad1b
  public :: csb1b
  public :: cad2b
  public :: csb2b
  public :: copbt
  public :: conbt
  public :: copmb
  public :: conmb
  public :: mulbb
  public :: madbb
  public :: msbbb
  public :: ldub
  public :: ldub_d
  public :: l1ubb
  public :: l1ubb_d
  public :: l1ueb
  public :: l1ueb_d
  public :: l1lb
  public :: ldlb
  public :: ldlb_d
  public :: udub
  public :: udub_d
  public :: mulbv
  public :: madbv
  public :: msbbv
  public :: mulbx
  public :: madbx
  public :: msbbx
  public :: mulby
  public :: madby
  public :: msbby
  public :: mulvb
  public :: madvb
  public :: msbvb
  public :: mulxb
  public :: madxb
  public :: msbxb
  public :: mulyb
  public :: madyb
  public :: msbyb
  public :: mulbd
  public :: madbd
  public :: msbbd
  public :: muldb
  public :: maddb
  public :: msbdb
  public :: udlbv
  public :: udlbx
  public :: udlby
  public :: udlvb
  public :: udlxb
  public :: udlyb
  public :: u1lbv
  public :: u1lbx
  public :: u1lby
  public :: u1lvb
  public :: u1lxb
  public :: u1lyb
  public :: linbv
  public :: wrtb

INTERFACE avco;   MODULE PROCEDURE avco;           END INTERFACE
INTERFACE avco_d; MODULE PROCEDURE davco;          END INTERFACE
INTERFACE dfco;   MODULE PROCEDURE dfco;           END INTERFACE
INTERFACE dfco_d; MODULE PROCEDURE ddfco;          END INTERFACE
INTERFACE dfco2;  MODULE PROCEDURE dfco2;          END INTERFACE
INTERFACE dfco2_d;MODULE PROCEDURE ddfco2;         END INTERFACE
INTERFACE clib;   MODULE PROCEDURE clib;           END INTERFACE
INTERFACE clib_d; MODULE PROCEDURE dclib;          END INTERFACE
INTERFACE cad1b;  MODULE PROCEDURE cad1b;          END INTERFACE
INTERFACE csb1b;  MODULE PROCEDURE csb1b;          END INTERFACE
INTERFACE cad2b;  MODULE PROCEDURE cad2b;          END INTERFACE
INTERFACE csb2b;  MODULE PROCEDURE csb2b;          END INTERFACE
INTERFACE copbt;  MODULE PROCEDURE copbt;          END INTERFACE
INTERFACE conbt;  MODULE PROCEDURE conbt;          END INTERFACE
INTERFACE copmb;  MODULE PROCEDURE copmb;          END INTERFACE
INTERFACE conmb;  MODULE PROCEDURE conmb;          END INTERFACE
INTERFACE copbm;  MODULE PROCEDURE copbm;          END INTERFACE
INTERFACE conbm;  MODULE PROCEDURE conbm;          END INTERFACE
INTERFACE mulbb;  MODULE PROCEDURE mulbb;          END INTERFACE
INTERFACE madbb;  MODULE PROCEDURE madbb;          END INTERFACE
INTERFACE msbbb;  MODULE PROCEDURE msbbb;          END INTERFACE
INTERFACE ldub;   MODULE PROCEDURE ldub;           END INTERFACE
INTERFACE ldub_d; MODULE PROCEDURE dldub;          END INTERFACE
INTERFACE l1ubb;  MODULE PROCEDURE l1ubb;          END INTERFACE
INTERFACE l1ubb_d;MODULE PROCEDURE dl1ubb;         END INTERFACE
INTERFACE l1ueb;  MODULE PROCEDURE l1ueb;          END INTERFACE
INTERFACE l1ueb_d;MODULE PROCEDURE dl1ueb;         END INTERFACE
INTERFACE l1lb;   MODULE PROCEDURE l1lb;           END INTERFACE
INTERFACE ldlb;   MODULE PROCEDURE ldlb;           END INTERFACE
INTERFACE ldlb_d; MODULE PROCEDURE dldlb;          END INTERFACE
INTERFACE udub;   MODULE PROCEDURE udub;           END INTERFACE
INTERFACE udub_d; MODULE PROCEDURE dudub;          END INTERFACE
INTERFACE mulbv;  MODULE PROCEDURE mulbv;          END INTERFACE
INTERFACE madbv;  MODULE PROCEDURE madbv;          END INTERFACE
INTERFACE msbbv;  MODULE PROCEDURE msbbv;          END INTERFACE
INTERFACE mulbx;  MODULE PROCEDURE mulbx;          END INTERFACE
INTERFACE madbx;  MODULE PROCEDURE madbx;          END INTERFACE
INTERFACE msbbx;  MODULE PROCEDURE msbbx;          END INTERFACE
INTERFACE mulby;  MODULE PROCEDURE mulby;          END INTERFACE
INTERFACE madby;  MODULE PROCEDURE madby;          END INTERFACE
INTERFACE msbby;  MODULE PROCEDURE msbby;          END INTERFACE
INTERFACE mulvb;  MODULE PROCEDURE mulvb;          END INTERFACE
INTERFACE madvb;  MODULE PROCEDURE madvb;          END INTERFACE
INTERFACE msbvb;  MODULE PROCEDURE msbvb;          END INTERFACE
INTERFACE mulxb;  MODULE PROCEDURE mulxb;          END INTERFACE
INTERFACE madxb;  MODULE PROCEDURE madxb;          END INTERFACE
INTERFACE msbxb;  MODULE PROCEDURE msbxb;          END INTERFACE
INTERFACE mulyb;  MODULE PROCEDURE mulyb;          END INTERFACE
INTERFACE madyb;  MODULE PROCEDURE madyb;          END INTERFACE
INTERFACE msbyb;  MODULE PROCEDURE msbyb;          END INTERFACE
INTERFACE mulbd;  MODULE PROCEDURE mulbd;          END INTERFACE
INTERFACE madbd;  MODULE PROCEDURE madbd;          END INTERFACE
INTERFACE msbbd;  MODULE PROCEDURE msbbd;          END INTERFACE
INTERFACE muldb;  MODULE PROCEDURE muldb;          END INTERFACE
INTERFACE maddb;  MODULE PROCEDURE maddb;          END INTERFACE
INTERFACE msbdb;  MODULE PROCEDURE msbdb;          END INTERFACE
INTERFACE udlbv;  MODULE PROCEDURE udlbv;          END INTERFACE
INTERFACE udlbx;  MODULE PROCEDURE udlbx;          END INTERFACE
INTERFACE udlby;  MODULE PROCEDURE udlby;          END INTERFACE
INTERFACE udlvb;  MODULE PROCEDURE udlvb;          END INTERFACE
INTERFACE udlxb;  MODULE PROCEDURE udlxb;          END INTERFACE
INTERFACE udlyb;  MODULE PROCEDURE udlyb;          END INTERFACE
INTERFACE u1lbv;  MODULE PROCEDURE u1lbv;          END INTERFACE
INTERFACE u1lbx;  MODULE PROCEDURE u1lbx;          END INTERFACE
INTERFACE u1lby;  MODULE PROCEDURE u1lby;          END INTERFACE
INTERFACE u1lvb;  MODULE PROCEDURE u1lvb;          END INTERFACE
INTERFACE u1lxb;  MODULE PROCEDURE u1lxb;          END INTERFACE
INTERFACE u1lyb;  MODULE PROCEDURE u1lyb;          END INTERFACE
INTERFACE linbv;  MODULE PROCEDURE linbv;          END INTERFACE
INTERFACE wrtb;   MODULE PROCEDURE wrtb;           END INTERFACE


CONTAINS


!=============================================================================
SUBROUTINE davco(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    davco
!
!   prgrmmr:    purser					      1999
!
! abstract:  Compute one row of the coefficients for the compact mid-interval
!            interpolation scheme characterized by matrix equation of the form,
!			 A.t = B.s			       (*)
!            Where s is the vector of "source" values, t the staggered "target" 
!            values.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA  - number of t-points operated on by this row of the A of (*)
!     NB  - number of s-points operated on by this row of the B of (*)
!     ZA  - coordinates of t-points used in this row of (*)
!     ZB  - coordinates of s-points used in this row of (*)
!     Z0  - nominal point of application of this row of (*)
!
!   output argument list:
!     A   - the NA coefficients A for this scheme
!     B   - the NB coefficients B for this scheme
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(my_kind)   , INTENT(IN   ) :: za(na),zb(nb),z0
REAL(my_kind)   , INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                    :: na1,nab,i
REAL(my_kind),DIMENSION(na+nb,na+nb):: w
REAL(my_kind),DIMENSION(na)         :: za0,pa
REAL(my_kind),DIMENSION(nb)         :: zb0,pb
REAL(my_kind),DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=2,nab; w(i,1:na)=pa;    pa=pa*za0; w(i,na1:nab)=pb; pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE davco


!=============================================================================
SUBROUTINE avco(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    avco
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for the compact mid-interval
!            interpolation scheme characterized by matrix equation of the form,
!                        A.t = B.s                             (*)
!            Where s is the vector of "source" values, t the staggered "target"
!            values.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA  - number of t-points operated on by this row of the A of (*)
!     NB  - number of s-points operated on by this row of the B of (*)
!     ZA  - coordinates of t-points used in this row of (*)
!     ZB  - coordinates of s-points used in this row of (*)
!     Z0  - nominal point of application of this row of (*)
!
!   output argument list:
!     A   - the NA coefficients A for this scheme
!     B   - the NB coefficients B for this scheme
!   
! attributes:  
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(my_kind),    INTENT(IN   ) :: za(na),zb(nb),z0
REAL(my_kind),    INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(my_kind), DIMENSION(na+nb,na+nb):: w
REAL(my_kind), DIMENSION(na)         :: za0,pa
REAL(my_kind), DIMENSION(nb)         :: zb0,pb
REAL(my_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=2,nab; w(i,1:na)=pa;    pa=pa*za0; w(i,na1:nab)=pb; pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE avco 


!=============================================================================
SUBROUTINE ddfco(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ddfco
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for either the compact 
!            differencing or quadrature scheme characterized by matrix 
!            equation of the form,
!			 A.d = B.c			       (*)
!            In either case, d is the derivative of c.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA  - number of d-points operated on by this row of the A of (*)
!     NB  - number of c-points operated on by this row of the B of (*)
!     ZA  - coordinates of d-points used in this row of (*)
!     ZB  - coordinates of c-points used in this row of (*)
!     Z0  - nominal point of application of this row of (*)
!
!   output argument list:
!     A   - the A-coefficients for this scheme
!     B   - the B-coefficients for this scheme
!   
! attributes:  
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(my_kind)   , INTENT(IN   ) :: za(na),zb(nb),z0
REAL(my_kind)   , INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(my_kind), DIMENSION(na+nb,na+nb):: w
REAL(my_kind), DIMENSION(na)         :: za0,pa
REAL(my_kind), DIMENSION(nb)         :: zb0,pb
REAL(my_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=3,nab; w(i,1:na)   =pa*(i-2); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;              pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE ddfco 


!=============================================================================
SUBROUTINE dfco(na,nb,za,zb,z0,a,b)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dfco
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for either the compact
!            differencing or quadrature scheme characterized by matrix
!            equation of the form,
!                        A.d = B.c                             (*)
!            In either case, d is the derivative of c.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA  - number of d-points operated on by this row of the A of (*)
!     NB  - number of c-points operated on by this row of the B of (*)
!     ZA  - coordinates of d-points used in this row of (*)
!     ZB  - coordinates of c-points used in this row of (*)
!     Z0  - nominal point of application of this row of (*)
!
!   output argument list:
!     A   - the A-coefficients for this scheme
!     B   - the B-coefficients for this scheme
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(my_kind),    INTENT(IN   ) :: za(na),zb(nb),z0
REAL(my_kind),    INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(my_kind), DIMENSION(na+nb,na+nb):: w
REAL(my_kind), DIMENSION(na)         :: za0,pa
REAL(my_kind), DIMENSION(nb)         :: zb0,pb
REAL(my_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=3,nab; w(i,1:na)   =pa*(i-2); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;              pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE dfco 


!=============================================================================
SUBROUTINE ddfco2(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ddfco2
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for either the compact second-
!            differencing scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!            Where d is the second-derivative of c.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA   - number of d-points operated on by this row of the A of (*)
!     NB   - number of c-points operated on by this row of the B of (*)
!     ZA   - coordinates of d-points used in this row of (*)
!     ZB   - coordinates of c-points used in this row of (*)
!     Z0   - nominal point of application of this row of (*)
!
!   output argument list:
!     A    - the NA coefficients A for this scheme
!     B    - the NB coefficients B for this scheme
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(my_kind)   , INTENT(IN   ) :: za(na),zb(nb),z0
REAL(my_kind)   , INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(my_kind), DIMENSION(na+nb,na+nb):: w
REAL(my_kind), DIMENSION(na)         :: za0,pa
REAL(my_kind), DIMENSION(nb)         :: zb0,pb
REAL(my_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=4,nab; w(i,1:na)   =pa*(i-2)*(i-3); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;                           pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE ddfco2 


!=============================================================================
SUBROUTINE dfco2(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dfco2
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for either the compact second-
!            differencing scheme characterized by matrix equation of the form,
!                        A.d = B.c                             (*)
!            Where d is the second-derivative of c.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA   - number of d-points operated on by this row of the A of (*)
!     NB   - number of c-points operated on by this row of the B of (*)
!     ZA   - coordinates of d-points used in this row of (*)
!     ZB   - coordinates of c-points used in this row of (*)
!     Z0   - nominal point of application of this row of (*)
!
!   output argument list:
!     A    - the NA coefficients A for this scheme
!     B    - the NB coefficients B for this scheme
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(my_kind),    INTENT(IN   ) :: za(na),zb(nb),z0
REAL(my_kind),    INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(my_kind), DIMENSION(na+nb,na+nb):: w
REAL(my_kind), DIMENSION(na)         :: za0,pa
REAL(my_kind), DIMENSION(nb)         :: zb0,pb
REAL(my_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=4,nab; w(i,1:na)   =pa*(i-2)*(i-3); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;                           pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE dfco2 


!=============================================================================
SUBROUTINE clib(a,m1,m2,mah1,mah2) ! Clip the dead space of the band matrix, a
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    clib
!
!   prgrmmr:    
!
! abstract:  Clip the dead space of the band matrix 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2  - 
!
!   output argument list:
!     a                   -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind)   , INTENT(INOUT) :: a(m1,-mah1:mah2)

INTEGER(i_kind)                :: j

IF(m2-m1+mah1 < 0)STOP 'In CLIB, form of band matrix implies redundant rows'
DO j=1,mah1; a(1:min(m1,j),-j)=zero; ENDDO; DO j=m2-m1+1,mah2; a(max(1,m2-j+1):m1,j)=zero; ENDDO
END SUBROUTINE clib


!=============================================================================
SUBROUTINE dclib(a,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dclib
!
!   prgrmmr:    
!
! abstract:  Clip the dead space of the band matrix
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2  - 
!
!   output argument list:
!     a                   - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind)   , INTENT(INOUT) :: a(m1,-mah1:mah2)

INTEGER(i_kind)                :: j

IF(m2-m1+mah1 < 0)STOP 'In CLIB_d, form of band matrix implies redundant rows'
DO j=1,mah1; a(1:min(m1,j),-j)=zero; ENDDO; DO j=m2-m1+1,mah2; a(max(1,m2-j+1):m1,j)=zero; ENDDO
END SUBROUTINE dclib


!=============================================================================
SUBROUTINE cad1b(a,m1,mah1,mah2,mirror2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    cad1b
!
!   prgrmmr:    
!
! abstract:  Incorporate operand symmetry near end-1 of a band matrix operator
!
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A          -  Input as unclipped operator, output as symmetrized and clipped.
!     m1         - Sizes of implied full matrix
!     mah1, mah2 - Left and right semi-bandwidths of A.
!     mirror2    - 2*location of symmetry axis relative to end-1 operand element.
!
!   output argument list:
!     A          -  Input as unclipped operator, output as symmetrized and clipped.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1,mah1,mah2,mirror2
REAL(my_kind),     INTENT(INOUT) :: a(0:m1-1,-mah1:mah2)

INTEGER(i_kind)                :: i,i2,jm,jp,jpmax

IF(mirror2+mah1 > mah2)STOP 'In cad1b, mah2 insufficient'
DO i=0,m1-1; i2=i*2; jpmax=mirror2+mah1-i2; IF(jpmax <= -mah1)EXIT
   DO jm=-mah1,mah2; jp=mirror2-jm-i2; IF(jp <= jm)EXIT
      a(i,jp)=a(i,jp)+a(i,jm) ! Reflect and add
      a(i,jm)=zero            ! zero the exterior part
   ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY     csb1b(a,m1,mah1,mah2,mirror2)
!=============================================================================
! Like cad1b, but for antisymmetric operand
IF(mirror2+mah1 > mah2)STOP 'In csb1b, mah2 insufficient'
DO i=0,m1-1; i2=i*2; jpmax=mirror2+mah1-i2; IF(jpmax < -mah1)EXIT
   DO jm=-mah1,mah2; jp=mirror2-jm-i2; IF(jp < jm)EXIT
      a(i,jp)=a(i,jp)-a(i,jm) ! Reflect and subtract
      a(i,jm)=zero            ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE cad1b


!=============================================================================
SUBROUTINE cad2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    cad2b
!
!   prgrmmr:    
!
! abstract:  Incorporate operand symmetry near end-2 of a band matrix operator
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A          - Input as unclipped operator, output as symmetrized and clipped.
!     m1, m2     - Sizes of implied full matrix
!     mah1, mah2 - Left and right semi-bandwidths of A.
!     mirror2    - 2*location of symmetry axis relative to end-2 operand element.
!
!   output argument list:
!     A         - Input as unclipped operator, output as symmetrized and clipped.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1,m2,mah1,mah2,mirror2
REAL(my_kind),     INTENT(INOUT) :: a(1-m1:0,m1-m2-mah1:m1-m2+mah2)

INTEGER(i_kind)                :: i,i2,jm,jp,jmmin,nah1,nah2,mirror,j0

nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In cad2b, mah1 insufficient'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin >= nah2)EXIT
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm >= jp)EXIT
      a(i,jm)=a(i,jm)+a(i,jp) ! Reflect and add
      a(i,jp)=zero            ! zero the exterior part
   ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY    csb2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In csb2b, mah1 insufficient'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin > nah2)EXIT
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm > jp)EXIT
      a(i,jm)=a(i,jm)-a(i,jp) ! Reflect and subtract
      a(i,jp)=zero            ! zero the exterior part
   ENDDO
ENDDO
!=============================================================================
ENTRY    cex2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In cex2b, mah1 insufficient'
mirror=mirror2/2
IF(mirror*2 /= mirror2)STOP 'In cex2b, mirror2 is not even'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin >= nah2)EXIT
   j0=mirror-i
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm >= jp)EXIT
      a(i,jm)=a(i,jm)-a(i,jp)    ! Reflect and subtract
      a(i,j0)=a(i,j0)+two*a(i,jp)! Apply double the coefficient to end
      a(i,jp)=zero               ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE cad2b


!=============================================================================
SUBROUTINE copbt(a,b,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copbt
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:  Copy transpose of rectangular banded matrix A to B
!
!     Note:  This routine expects A and B always to occupy separate storage.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A      - input matrix in banded format
!     M1     - number of rows of A, columns of B
!     M2     - number of columns of A, rows of B
!     MAH1   - left-half-bandwidth of A, right-half-bandwidth of B
!     MAH2   - right-half-bandwidth of A, left-half-bandwidth of B
!
!   output argument list:
!     B      - output matrix in banded format
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2)
REAL(my_kind),     INTENT(  OUT) :: b(m2,-mah2:mah1)

INTEGER(i_kind)                 :: j, i

CALL clib(b,mah2,mah1,m2,m1)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); b(j+i,-j)=a(i,j); ENDDO
ENDDO
RETURN
ENTRY	 conbt(a,b,m1,m2,mah1,mah2)
CALL clib(b,mah2,mah1,m2,m1)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); b(j+i,-j)=-a(i,j); ENDDO
ENDDO
END SUBROUTINE copbt


!=============================================================================
SUBROUTINE copmb(afull,aband,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copmb
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2  - 
!     afull               -
!
!   output argument list:
!     aband               - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),                           INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind),     DIMENSION(m1,m2),        INTENT(IN   ) :: afull
REAL(my_kind),     DIMENSION(m1,-mah1:mah2),INTENT(  OUT) :: aband

INTEGER(i_kind)                                          :: i1,i2, i, j

CALL clib(aband,m1,m2,mah1,mah2)
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; aband(i,j)= afull(i,j+i); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY      conmb(afull,aband,m1,m2,mah1,mah2)
!=============================================================================
CALL clib(aband,m1,m2,mah1,mah2)
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; aband(i,j)=-afull(i,j+i); ENDDO
ENDDO
END SUBROUTINE copmb


!=============================================================================
SUBROUTINE copbm(aband,afull,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copbm
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2  - 
!     aband               -
!
!   output argument list:
!     afull               - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),                           INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind),     DIMENSION(m1,-mah1:mah2),INTENT(IN   ) :: aband
REAL(my_kind),     DIMENSION(m1,m2),        INTENT(  OUT) :: afull

INTEGER(i_kind)                                          :: i1,i2, i, j

afull=zero
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; afull(i,j+i)= aband(i,j); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY      conbm(aband,afull,m1,m2,mah1,mah2)
!=============================================================================
afull=zero
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; afull(i,j+i)=-aband(i,j); ENDDO
ENDDO
END SUBROUTINE copbm

 
!=============================================================================
SUBROUTINE mulbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulbb
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2     - 
!     mbh1, mbh2, mch1, mch2 - 
!     a, b                   -
!     c                      -
!
!   output argument list:
!     c                   - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL(my_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL(my_kind),     INTENT(INOUT) :: c(m1,-mch1:mch2)

INTEGER(i_kind)                :: nch1, nch2, j, k, jpk, i1,i2

c=zero
ENTRY      madbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
nch1=mah1+mbh1; nch2=mah2+mbh2
IF(nch1 /= mch1 .OR. nch2 /= mch2)STOP 'In MULBB, dimensions inconsistent'
DO j=-mah1,mah2
   DO k=-mbh1,mbh2; jpk=j+k; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
      c(i1:i2,jpk)=c(i1:i2,jpk)+a(i1:i2,j)*b(j+i1:j+i2,k)
   ENDDO
ENDDO
END SUBROUTINE mulbb


!=============================================================================
SUBROUTINE msbbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    msbbb
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2     - 
!     mbh1, mbh2, mch1, mch2 -
!     a, b                   -
!
!   output argument list:
!     c                      - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL(my_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL(my_kind),     INTENT(  OUT) :: c(m1,-mch1:mch2)

INTEGER(i_kind)                 :: nch1, nch2, j, k, jpk, i1,i2

nch1=mah1+mbh1; nch2=mah2+mbh2
IF(nch1 /= mch1 .OR. nch2 /= mch2)STOP 'In MSBBB, dimensions inconsistent'
DO j=-mah1,mah2
   DO k=-mbh1,mbh2; jpk=j+k; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
      c(i1:i2,jpk)=c(i1:i2,jpk)-a(i1:i2,j)*b(j+i1:j+i2,k)
   ENDDO
ENDDO
END SUBROUTINE msbbb


!=============================================================================
SUBROUTINE LDUB(a,m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldub
!
!   prgrmmr:    R.J.Purser, 1994
!
! abstract:  Compute [L]*[D**-1]*[U] decomposition of asymmetric band-matrix
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A       - input as the asymmetric band matrix. On output, it contains
!               the [L]*[D**-1]*[U] factorization of the input matrix, where
!               [L] is lower triangular with unit main diagonal
!               [D] is a diagonal matrix
!               [U] is upper triangular with unit main diagonal
!     M       - The number of rows of array A
!     MAH1    - The left half-bandwidth of fortran array A
!     MAH2    - The right half-bandwidth of fortran array A
!
!   output argument list:
!     A       - input as the asymmetric band matrix. On output, it contains
!               the [L]*[D**-1]*[U] factorization of the input matrix, where
!               [L] is lower triangular with unit main diagonal
!               [D] is a diagonal matrix
!               [U] is upper triangular with unit main diagonal
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m,mah1, mah2 
REAL(my_kind),    INTENT(INOUT) :: a(m,-mah1:mah2) 

INTEGER(i_kind)               :: j, imost, jmost, jp, i
REAL(my_kind)                  :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)THEN
      PRINT '(" Failure in LDUB:"/" Matrix requires pivoting or is singular")'
      STOP
   ENDIF
   ajji=one/ajj
   a(j,0)=ajji
   DO i=jp,imost
      aij=ajji*a(i,j-i)
      a(i,j-i)=aij
      a(i,jp-i:jmost-i)=a(i,jp-i:jmost-i)-aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,jp-j:jmost-j)=ajji*a(j,jp-j:jmost-j)
ENDDO
END SUBROUTINE LDUB


!=============================================================================
SUBROUTINE DLDUB(a,m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dldub
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m, mah1, mah2  - 
!     a              - 
!
!   output argument list:
!     a              - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

INTEGER(i_kind),  INTENT(IN   ) :: m,mah1, mah2 
REAL(my_kind)   ,  INTENT(INOUT) :: a(m,-mah1:mah2) 

INTEGER(i_kind)                :: j, imost, jmost, jp, i
REAL(my_kind)                   :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)THEN
      PRINT '(" Fails in LDUB_d:"/" Matrix requires pivoting or is singular")'
      STOP
   ENDIF
   ajji=one/ajj
   a(j,0)=ajji
   DO i=jp,imost
      aij=ajji*a(i,j-i)
      a(i,j-i)=aij
      a(i,jp-i:jmost-i)=a(i,jp-i:jmost-i)-aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,jp-j:jmost-j)=ajji*a(j,jp-j:jmost-j)
ENDDO
END SUBROUTINE DLDUB

!=============================================================================
SUBROUTINE L1UBB(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    l1ubb
!
!   prgrmmr:    R.J.Purser, 1996
!
! abstract:  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix  
!            [A] replace lower triangular elements of [A] by [D**-1]*[L]*[D], 
!            the upper by [U], replace matrix [B] by [D**-1]*[B].
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!     M     - Number of rows of A and B
!     MAH1  - left half-width of fortran array A
!     MAH2  - right half-width of fortran array A
!     MBH1  - left half-width of fortran array B
!     MBH2  - right half-width of fortran array B
!
!   output argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) ::  m,mah1, mah2, mbh1, mbh2 
REAL(my_kind)   , INTENT(INOUT) :: a(m,-mah1:mah2), b(m,-mbh1:mbh2)

INTEGER(i_kind)                :: j, imost, jmost, jleast, jp, i
REAL(my_kind)                   :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jleast=MAX(1,j-mah1)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)STOP 'failure in L1UBB'
   ajji=one/ajj
   a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
   DO i=jp,imost
      aij=a(i,j-i)
      a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,0)=one
   b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE L1UBB


!=============================================================================
SUBROUTINE DL1UBB(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dl1ubb
!
!   prgrmmr:    R.J.Purser, 1996
!
! abstract:  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix
!            [A] replace lower triangular elements of [A] by [D**-1]*[L]*[D],
!            the upper by [U], replace matrix [B] by [D**-1]*[B].
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!     M     - Number of rows of A and B
!     MAH1  - left half-width of fortran array A
!     MAH2  - right half-width of fortran array A
!     MBH1  - left half-width of fortran array B
!     MBH2  - right half-width of fortran array B
!
!   output argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) ::  mah1, mah2, mbh1, mbh2 
REAL(my_kind)   ,  INTENT(INOUT) :: a(m,-mah1:mah2), b(m,-mbh1:mbh2)

INTEGER(i_kind)                :: m,j, imost, jmost, jleast, jp, i
REAL(my_kind)                   :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jleast=MAX(1,j-mah1)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)STOP 'failure in DL1UBB'
   AJJI=one/AJJ
   a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
   DO I=JP,IMOST
      AIJ=A(I,J-I)
      a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
   ENDDO
   A(J,0)=one
   b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE DL1UBB


!=============================================================================
SUBROUTINE l1ueb(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    l1ueb
!
!   prgrmmr:    R.J.Purser, 1998
!
! abstract:  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix
!            [A] replace all but row zero of the lower triangular
!            elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!            replace matrix [B] by [D**-1]*[B].
!            This is a special adaptation of L1UBB used to process quadarature weights
!            for QEDBV etc in which the initial quadrature value is provided as input
!            instead of being implicitly assumed zero (which is the case for QZDBV etc).
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!     M     - number of rows of B, one less than the rows of A (which has "row 0")
!     MAH1  - left half-width of fortran array A
!     MAH2  - right half-width of fortran array A
!     MBH1  - left half-width of fortran array B
!     MBH2  - right half-width of fortran array B
!
!   output argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m,mah1, mah2, mbh1, mbh2 
REAL(my_kind)   , INTENT(INOUT) :: a(0:m,-mah1:mah2), b(m,-mbh1:mbh2)

INTEGER(i_kind) :: j, imost, jmost, jleast, jp, i
REAL(my_kind)    :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jleast=MAX(0,j-mah1)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)STOP 'failure in L1UEB'
   ajji=one/ajj
   a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
   DO i=jp,imost
      aij=a(i,j-i)
      a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,0)=one
   b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE l1ueb


!=============================================================================
SUBROUTINE dl1ueb(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dl1ueb
!
!   prgrmmr:    R.J.Purser, 1998
!
! abstract:  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix
!            [A] replace all but row zero of the lower triangular
!            elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!            replace matrix [B] by [D**-1]*[B].
!            This is a special adaptation of L1UBB used to process quadarature weights
!            for QEDBV etc in which the initial quadrature value is provided as input
!            instead of being implicitly assumed zero (which is the case for QZDBV etc).
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!     M     - number of rows of B, one less than the rows of A (which has "row 0")
!     MAH1  - left half-width of fortran array A
!     MAH2  - right half-width of fortran array A
!     MBH1  - left half-width of fortran array B
!     MBH2  - right half-width of fortran array B
!
!   output argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m,mah1, mah2, mbh1, mbh2 
REAL(my_kind)   ,  INTENT(INOUT) :: a(0:,-mah1:), b(:,-mbh1:)

INTEGER(i_kind)                   :: j, imost, jmost, jleast, jp, i
REAL(my_kind)                      :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jleast=MAX(0,j-mah1)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)STOP 'failure in L1UEB_d'
   ajji=one/ajj
   a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
   DO i=jp,imost
      aij=a(i,j-i)
      a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,0)=one
   b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE dl1ueb


!=============================================================================
SUBROUTINE L1LB(a,b,m,mah)   ! Cholesky LU decomposition of Banded.
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    l1lb
!
!   prgrmmr: 
!
! abstract:  Cholesky LU decomposition of Banded.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - 
!     M     - 
!     MAH   - 
!
!   output argument list:
!     B     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah
REAL(my_kind),     INTENT(IN   ) :: a(m,-mah:mah)
REAL(my_kind),     INTENT(  OUT) :: b(m,-mah:0)

INTEGER(i_kind)                 :: i, j,jmi
REAL(my_kind)                    :: s

CALL clib(b,m,m,mah,0)
DO j=1,m
   s=a(j,0)-DOT_PRODUCT(b(j,-mah:-1),b(j,-mah:-1))
   IF(s <= zero)THEN
      PRINT '(" L1LB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   s=SQRT(s); b(j,0)=s; s=one/s
   DO i=j+1,MIN(m,j+mah); jmi=j-i
      b(i,jmi)=s*(a(i,jmi)-DOT_PRODUCT(b(i,-mah:jmi-1),b(j,-mah-jmi:-1)))
   ENDDO
ENDDO
END SUBROUTINE L1LB

!=============================================================================
SUBROUTINE LDLB(a,b,d,m,mah) ! Modified Cholesky [L(D**-1)U, without sqrt]
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb
!
!   prgrmmr:    
!
! abstract:   Modified Cholesky [L(D**-1)U, without sqrt]
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a     - 
!     m     - 
!     mah   - 
!
!   output argument list:
!     b     - 
!     d     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah
REAL(my_kind),     INTENT(IN   ) :: a(m,-mah:mah)
REAL(my_kind),     INTENT(  OUT) :: b(m,-mah:0)
REAL(my_kind),     INTENT(  OUT) :: d(m) 

INTEGER(i_kind)                 :: i, j,k,jmi,lj,li
REAL(my_kind)                    :: s,t

CALL clib(b,m,m,mah,0); b(:,0)=one
DO j=1,m; lj=MAX(-mah,1-j)
   s=a(j,0)
   do k=lj,-1
      s=s-b(j,k)**2*d(k+j)
   enddo
   IF(s <= zero)THEN
      PRINT '(" LDLB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   d(j)=s; s=one/s
   DO i=j+1,MIN(m,j+mah); jmi=j-i; li=MAX(-mah,1-i); lj=li-jmi
      t=a(i,jmi)
      do k=li,jmi-1
         t=t-b(i,k)*b(j,k-jmi)*d(i+k)
      enddo
      b(i,jmi)=s*t
   ENDDO
ENDDO
d=one/d
END SUBROUTINE LDLB

!=============================================================================
SUBROUTINE DLDLB(a,b,d,m,mah) ! Modified Cholesky [L(D**-1)U, without sqrt]
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dl1lb
!
!   prgrmmr:    
!
! abstract:  Modified Cholesky [L(D**-1)U, without sqrt]
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a     - 
!     m     - 
!     mah   - 
!
!   output argument list:
!     b     - 
!     d     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah
REAL(my_kind)   ,  INTENT(IN   ) :: a(m,-mah:mah)
REAL(my_kind)   ,  INTENT(  OUT) :: b(m,-mah:0)
REAL(my_kind)   ,  INTENT(  OUT) :: d(m) 

INTEGER(i_kind)                 :: i, j,k,jmi,lj,li
REAL(my_kind)                    :: s,t

CALL clib_d(b,m,m,mah,0); b(:,0)=one
DO j=1,m; lj=MAX(-mah,1-j)
   s=a(j,0)
   do k=lj,-1
      s=s-b(j,k)**2*d(k+j)
   enddo
   IF(s <= zero)THEN
      PRINT '(" DLDLB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   d(j)=s; s=one/s
   DO i=j+1,MIN(m,j+mah); jmi=j-i;  
      li=MAX(-mah,1-i); 
      lj=li-jmi; 
      t=a(i,jmi)
      do k=li,jmi-1
         t=t-b(i,k)*b(j,k-jmi)*d(i+k)
      enddo
      b(i,jmi)=s*t
   ENDDO
ENDDO
d=one/d
END SUBROUTINE DLDLB

!=============================================================================
SUBROUTINE UDUB(a,b,d,m,mah) ! Modified reverse Cholesky [U(D**-1)U^t],
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udub
!
!   prgrmmr:    
!
! abstract:   Modified reverse Cholesky [U(D**-1)U^t],
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a     - 
!     m     - 
!     mah   - 
!
!   output argument list:
!     b     - 
!     d     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),        INTENT(IN   ) :: m, mah
REAL(my_kind),           INTENT(IN   ) :: a(m,-mah:mah)
REAL(my_kind),           INTENT(  OUT) :: b(m,0:mah)
REAL(my_kind),           INTENT(  OUT) :: d(m) 

REAL(my_kind), DIMENSION(m,-mah:mah  ) :: at
REAL(my_kind), DIMENSION(m,-mah:0)     :: bt
REAL(my_kind), DIMENSION(m)            :: dt

at=a(m:1:-1,mah:-mah:-1); CALL ldlb(at,bt,dt,m,mah);
b=bt(m:1:-1,0:-mah:-1); d=dt(m:1:-1)
END SUBROUTINE UDUB


!=============================================================================
SUBROUTINE DUDUB(a,b,d,m,mah) ! Modified reverse Cholesky [U(D**-1)U^t],
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dudub
!
!   prgrmmr:       
!
! abstract:   Modified reverse Cholesky [U(D**-1)U^t],
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a     - 
!     m     - 
!     mah   - 
!
!   output argument list:
!     b     -
!     d     -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),        INTENT(IN   ) :: m, mah
REAL(my_kind),           INTENT(IN   ) :: a(m,-mah:mah)
REAL(my_kind),           INTENT(  OUT) :: b(m,0:mah)
REAL(my_kind),           INTENT(  OUT) :: d(m) 

REAL(my_kind), DIMENSION(m,-mah:mah  ) :: at
REAL(my_kind), DIMENSION(m,-mah:0)     :: bt
REAL(my_kind), DIMENSION(m)            :: dt

at=a(m:1:-1,mah:-mah:-1); CALL ldlb_d(at,bt,dt,m,mah);
b=bt(m:1:-1,0:-mah:-1);   d=dt(m:1:-1)
END SUBROUTINE DUDUB


!=============================================================================
SUBROUTINE mulbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulbv
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times a Vector.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the matrix
!     V1   - the input vector
!     M1   - the number of rows assumed for A and for V2
!     M2   - the number of columns assumed for A and rows for V1
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V2   - the output vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2)
REAL(my_kind),     INTENT(  OUT) :: v2(m1)

INTEGER(i_kind)                 :: j, i1,i2 

v2 = zero
!=============================================================================
ENTRY	 madbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(i1:i2) = v2(i1:i2) + a(i1:i2,j)*v1(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(i1:i2) = v2(i1:i2) - a(i1:i2,j)*v1(j+i1:j+i2)
ENDDO
END SUBROUTINE mulbv


!=============================================================================
SUBROUTINE mulbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulbx
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times parallel X-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the matrix
!     V1   - the array of input vectors
!     M1   - the number of rows assumed for A and for V2
!     M2   - the number of columns assumed for A and rows for V1
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - the number of parallel X-vectors
!
!   output argument list:
!     V2   - the array of output vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL(my_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2,my)
REAL(my_kind),     INTENT(  OUT) :: v2(m1,my)

INTEGER(i_kind)                 :: i,j

v2=zero
!=============================================================================
ENTRY	 madbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(i,:)=v2(i,:)+a(i,j)*v1(i+j,:); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(i,:)=v2(i,:)-a(i,j)*v1(i+j,:); ENDDO
ENDDO
END SUBROUTINE mulbx


!=============================================================================
SUBROUTINE mulby(a,v1,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulby
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times parallel Y-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the matrix
!     V1   - the array of input vectors
!     M1   - the number of rows assumed for A and for V2
!     M2   - the number of columns assumed for A and rows for V1
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - the length of each of the of parallel Y-vectors
!
!   output argument list:
!     V2   - the array of output vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL(my_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(mx,m2)
REAL(my_kind),     INTENT(  OUT) :: v2(mx,m1)

INTEGER(i_kind)                 :: i,j

v2(1:mx,1:m1) = zero
ENTRY	 madby(a,v1,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,i)=v2(:,i)+a(i,j)*v1(:,i+j); ENDDO
ENDDO
RETURN
ENTRY	 msbby(a,v1,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,i)=v2(:,i)-a(i,j)*v1(:,i+j); ENDDO
ENDDO
END SUBROUTINE mulby


!=============================================================================
SUBROUTINE MULVB(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulvb
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Vector times a Banded matrix.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V1   - the input row-vector
!     A    - is the matrix
!     M1   - the number of rows assumed for A and columns for V1
!     M2   - the number of columns assumed for A and for V2
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V2   - the output vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: v1(m1), a(m1,-mah1:mah2)
REAL(my_kind),     INTENT(  OUT) :: v2(m2)

INTEGER(i_kind)                 :: j, i1,i2

v2=zero
!=============================================================================
ENTRY	 madvb(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)+v1(i1:i2)*a(i1:i2,j)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbvb(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)-v1(i1:i2)*a(i1:i2,j)
ENDDO
END SUBROUTINE mulvb


!=============================================================================
SUBROUTINE mulxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulxb
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of X-Vectors times Banded matrix.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V1   - the array of input row-vectors
!     A    - is the matrix
!     M1   - the number of rows assumed for A and columns for V1
!     M2   - the number of columns assumed for A and V2
!     MAH1 -  the left half-bandwidth of fortran array A
!     MAH2 -  the right half-bandwidth of fortran array A
!     MY   - the number of parallel X-vectors
!
!   output argument list:
!     V2   - the array of output vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL(my_kind),     INTENT(IN   ) :: v1(m1,my), a(m1,-mah1:mah2)
REAL(my_kind),     INTENT(  OUT) :: v2(m2,my)

INTEGER(i_kind)                 :: i,j

v2=zero
!=============================================================================
ENTRY	 madxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)+v1(i,:)*a(i,j); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY	 msbxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)-v1(i,:)*a(i,j); ENDDO
ENDDO
END SUBROUTINE mulxb


!=============================================================================
SUBROUTINE mulyb(v1,a,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulyb
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of Y-Vectors times a Banded matrix.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the matrix
!     V1   - the array of input row-vectors
!     M1   - the number of rows assumed for A and colums for V1
!     M2   - the number of columns assumed for A and V2
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - the length of each of the parallel Y-vectors
!
!   output argument list:
!     V2   - the array of output vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL(my_kind),     INTENT(IN   ) :: v1(mx,m1), a(m1,-mah1:mah2)
REAL(my_kind),     INTENT(  OUT) :: v2(mx,m2)

INTEGER(i_kind)                 :: i,j

v2=zero
ENTRY	 madyb(v1,a,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,j+i)=v2(:,j+i)+v1(:,i)*a(i,j); ENDDO
ENDDO
RETURN
ENTRY	 msbyb(v1,a,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,j+i)=v2(:,j+i)-v1(:,i)*a(i,j); ENDDO
ENDDO
END SUBROUTINE mulyb


!=============================================================================
SUBROUTINE mulbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulbd
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times a Diagonal
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the input banded-matrix
!     D    - the diagonal matrix
!     M1   - the number of rows assumed for A and for B
!     M2   - number of columns assumed for A and B, number of elements of D
!     MAH1 - the left half-bandwidth of arrays A and B
!     MAH2 - the right half-bandwidth of arrays A and B
!
!   output argument list:
!     B    - the output matrix
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: d(m2)
REAL(my_kind),     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)

INTEGER(i_kind)                :: j, i1,i2

CALL clib(b,m1,m2,mah1,mah2)
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j)=a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 madbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j) = b(i1:i2,j)+a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j) = b(i1:i2,j)-a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
END SUBROUTINE mulbd


!=============================================================================
SUBROUTINE muldb(d,a,b,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    muldb
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times a Diagonal
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     D    - the diagonal matrix
!     A    - is the input banded-matrix ! <->  if A and B are actually
!     M1   - the number of rows assumed for A and for B
!     M2   - number of columns assumed for A and B, number of elements of D
!     MAH1 - the left half-bandwidth of arrays A and B
!     MAH2 - the right half-bandwidth of arrays A and B
!
!   output argument list:
!     B    - the output matrix          ! <->  equivalent arrays.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: d(m1)
REAL(my_kind),     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)

INTEGER(i_kind)                 :: j

CALL clib(b,m1,m2,mah1,mah2)
DO j=-mah1,mah2; b(:,j)=d(:)*a(:,j); ENDDO
END SUBROUTINE muldb


!=============================================================================
SUBROUTINE maddb(d,a,b,m1,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    maddb
!
!   prgrmmr:     
!
! abstract:      
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d    - 
!     M1   - 
!     MAH1 - 
!     MAH2 -
!     a    -
!     b    -
!
!   output argument list:
!     a    -
!     b    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: d(m1)
REAL(my_kind),     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)

INTEGER(i_kind)                 :: j

DO j=-mah1,mah2; b(:,j)=b(:,j)+d(:)*a(:,j); ENDDO
END SUBROUTINE maddb


!=============================================================================
SUBROUTINE msbdb(d,a,b,m1,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    msbdb
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d    - 
!     M1   - 
!     MAH1 - 
!     MAH2 - 
!     a    -
!     b    -
!
!   output argument list:
!     a    -
!     b    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: d(m1) 
REAL(my_kind),     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)

INTEGER(i_kind)                 :: j

DO j=-mah1,mah2; b(:,j)=b(:,j)-d(:)*a(:,j); ENDDO
END SUBROUTINE msbdb


!=============================================================================
SUBROUTINE udlbv(a,v, m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlbv
!
!   prgrmmr:     R.J.Purser, 1994
!
! abstract:      BACk-substitution step of linear inversion involving
!                Banded matrix and Vector.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB
!     V    - input as right-hand-side vector, output as solution vector
!     M    - the number of rows assumed for A and for V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V    - input as right-hand-side vector, output as solution vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ):: m, mah1, mah2
REAL(my_kind),     INTENT(IN   ):: a(m,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT):: v(m)

INTEGER(i_kind)                :: i, j
REAL(my_kind)                   :: vj

DO j=1,m
   vj=v(j)
   DO i=j+1,MIN(m,j+mah1); v(i)=v(i)-a(i,j-i)*vj; ENDDO; v(j)=a(j,0)*vj
ENDDO
DO j=m,2,-1
   vj=v(j)
   DO i=MAX(1,j-mah2),j-1; v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
END SUBROUTINE udlbv


!=============================================================================
SUBROUTINE udlbx(a,v, mx,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlbx
!
!   prgrmmr:     R.J.Purser, 1994
!
! abstract:      BACk-substitution step of parallel linear inversion involving
!                Banded matrix and X-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB or, if N=NA, by LDUB
!     V    - input as right-hand-side vectors, output as solution vectors
!     MX   - the number of rows assumed for A and length of
!            X-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - number of parallel X-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: mx, mah1, mah2, my
REAL(my_kind),     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: jx, ix

DO jx=1,mx
   DO ix=jx+1,MIN(mx,jx+mah1); v(ix,:) = v(ix,:) - a(ix,jx-ix)*v(jx,:); ENDDO
   v(jx,:) = a(jx,0) * v(jx,:)
ENDDO
DO jx=mx,2,-1
   DO ix=MAX(1,jx-mah2),jx-1; v(ix,:) = v(ix,:) - a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
END SUBROUTINE udlbx


!=============================================================================
SUBROUTINE udlby(a,v, my,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlby
!
!   prgrmmr:     R.J.Purser, 1994
!
! abstract:      BACk-substitution step of parallel linear inversion involving
!                Banded matrix and Y-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB or, if N=NA, by LDUB
!     V    - input as right-hand-side vectors, output as solution vectors
!     MY   - the number of rows assumed for A and length of
!            Y-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - number of parallel Y-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: my, mah1, mah2, mx
REAL(my_kind),     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: iy, jy

DO jy=1,my
   DO iy=jy+1,MIN(my,jy+mah1); v(:,iy) = v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
   v(:,jy)=a(jy,0)*v(:,jy)
ENDDO
DO jy=my,2,-1
   DO iy=MAX(1,jy-mah2),jy-1; v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
END SUBROUTINE udlby


!=============================================================================
SUBROUTINE udlvb(v,a, m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlvb
!
!   prgrmmr:     R.J.Purser, 1994
!
! abstract:      BACk-substitution step of linear inversion involving
!                row-Vector and Banded matrix.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB
!     M    - the number of rows assumed for A and columns for V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V    - input as right-hand-side row-vector, output as solution vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: a(m,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(m)

INTEGER(i_kind)                :: i, j
REAL(my_kind)                   :: vi

DO i=1,m
   vi=v(i)
   DO j=i+1,MIN(m,i+mah2); v(j)=v(j)-vi*a(i,j-i); ENDDO
   v(i)=vi*a(i,0)
ENDDO
DO i=m,2,-1
   vi=v(i)
   DO j=MAX(1,i-mah1),i-1; v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
END SUBROUTINE udlvb


!=============================================================================
SUBROUTINE udlxb(v,a, mx,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlxb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:     BACk-substitution step of parallel linear inversion involving
!               Banded matrix and row-X-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB
!     MX   - the number of rows assumed for A and length of
!            X-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - number of parallel X-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: mx, mah1, mah2, my
REAL(my_kind),     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: ix, jx

DO ix=1,mx
   DO jx=ix+1,MIN(mx,ix+mah2); v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
   v(ix,:)=v(ix,:)*a(ix,0)
ENDDO
DO ix=mx,2,-1
   DO jx=MAX(1,ix-mah1),ix-1; v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
END SUBROUTINE udlxb

!=============================================================================
SUBROUTINE udlyb(v,a, my,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlyb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:      BACk-substitution step of parallel linear inversion involving
!                Banded matrix and row-Y-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB
!     MY   - the number of rows assumed for A and length of
!            Y-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - number of parallel Y-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: my, mah1, mah2, mx
REAL(my_kind),     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: iy, jy

DO iy=1,my
   DO jy=iy+1,MIN(my,iy+mah2); v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
   v(:,iy)=v(:,iy)*a(iy,0)
ENDDO
DO iy=my,2,-1
   DO jy=MAX(1,iy-mah1),iy-1; v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
END SUBROUTINE udlyb


!=============================================================================
SUBROUTINE u1lbv(a,v, m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lbv
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      BACk-substitution step ((U**-1)*(L**-1)) of linear inversion 
!                involving special Banded matrix and right-Vector.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     V    - input as right-hand-side vector, output as solution vector
!     M    - the number of rows assumed for A and for V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V    - input as right-hand-side vector, output as solution vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: a(m,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(m)

INTEGER(i_kind)                :: i, j
REAL(my_kind)                   :: vj

DO j=1,m
   vj=v(j)
   DO i=j+1,MIN(m,j+mah1); v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
DO j=m,2,-1
   vj=v(j)
   DO i=MAX(1,j-mah2),j-1; v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
END SUBROUTINE u1lbv


!=============================================================================
SUBROUTINE u1lbx(a,v, mx,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lbx
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of parallel linear inversion 
!                involving Banded matrix and X-right-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     V    - input as right-hand-side vectors, output as solution vectors
!     MX   - the number of rows assumed for A and length of
!            X-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - number of parallel X-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: mx, mah1, mah2, my
REAL(my_kind),     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: ix, jx

DO jx=1,mx
   DO ix=jx+1,MIN(mx,jx+mah1); v(ix,:)=v(ix,:)-a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
DO jx=mx,2,-1
   DO ix=MAX(1,jx-mah2),jx-1; v(ix,:)=v(ix,:)-a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
END SUBROUTINE u1lbx


!=============================================================================
SUBROUTINE u1lby(a,v, my,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lby
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of parallel linear inversion 
!                involving Banded matrix and Y-right-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     V    - input as right-hand-side vectors, output as solution vectors
!     MY   - the number of rows assumed for A and length of
!            Y-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - number of parallel Y-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: my, mah1, mah2, mx
REAL(my_kind),     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: iy, jy

DO jy=1,my
   DO iy=jy+1,MIN(my,jy+mah1); v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
DO jy=my,2,-1
   DO iy=MAX(1,jy-mah2),jy-1; v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
END SUBROUTINE u1lby


!=============================================================================
SUBROUTINE u1lvb(v,a, m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lvb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of linear inversion involving
!                left-Vector and Banded matrix.
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side row-vector, output as solution vector
!     A    - encodes the special [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     M    - the number of rows assumed for A and columns for V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V    - input as right-hand-side row-vector, output as solution vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m, mah1, mah2
REAL(my_kind),    INTENT(IN   ) :: a(m,-mah1:mah2)
REAL(my_kind),    INTENT(INOUT) :: v(m)

INTEGER(i_kind)               :: i, j
REAL(my_kind)                  :: vi

DO i=1,m
   vi=v(i)
   DO j=i+1,MIN(m,i+mah2); v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
DO i=m,2,-1
   vi=v(i)
   DO j=MAX(1,i-mah1),i-1; v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
END SUBROUTINE u1lvb


!=============================================================================
SUBROUTINE u1lxb(v,a, mx,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lxb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of parallel linear inversion 
!                involving Banded matrix and X-left-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!     A    - encodes the special [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     MX   - the number of rows assumed for A and length of
!            X-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - number of parallel X-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: mx, mah1, mah2, my
REAL(my_kind),     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: ix, jx

DO ix=1,mx
   DO jx=ix+1,MIN(mx,ix+mah2); v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
DO ix=mx,2,-1
   DO jx=MAX(1,ix-mah1),ix-1;  v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
END SUBROUTINE u1lxb


!=============================================================================
SUBROUTINE u1lyb(v,a, my,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lyb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of parallel linear inversion 
!                involving special Banded matrix and Y-left-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!     A    - encodes the [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     MY   - the number of rows assumed for A and length of
!            Y-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - number of parallel Y-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: my, mah1, mah2, mx
REAL(my_kind),     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL(my_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: iy, jy

DO iy=1,my
   DO jy=iy+1,MIN(my,iy+mah2); v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
DO iy=my,2,-1
   DO jy=MAX(1,iy-mah1),iy-1;  v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
END SUBROUTINE u1lyb


!=============================================================================
SUBROUTINE linbv(a,v,m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linbv
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:      Solve LINear system with square Banded-matrix and vector V
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - system matrix on input, its [L]*[D**-1]*[U] factorization on exit
!     V    - vector of right-hand-sides on input, solution vector on exit
!     M    - order of matrix A
!     MAH1 - left half-bandwidth of A
!     MAH2 - right half-bandwidth of A
!
!   output argument list:
!     A    - system matrix on input, its [L]*[D**-1]*[U] factorization on exit
!     V    - vector of right-hand-sides on input, solution vector on exit
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m, mah1, mah2
REAL(my_kind),    INTENT(INOUT) :: a(m,-mah1:mah2), v(m)

CALL ldub(a,m,mah1,mah2)
CALL udlbv(a,v,m,mah1,mah2)
END SUBROUTINE linbv


!=============================================================================
SUBROUTINE wrtb(a,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    wrtb
!
!   prgrmmr:     
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1   -
!     m2   -
!     mah1 - 
!     mah2 -
!     a    -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(my_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2)

INTEGER(i_kind)              :: i1, i2, i, j1, j2, j, nj1

DO i1=1,m1,20
   i2=MIN(i1+19,m1)
   PRINT '(7x,6(i2,10x))',(j,j=-mah1,mah2)
   DO i=i1,i2
      j1=MAX(-mah1,1-i)
      j2=MIN(mah2,m2-i)
      nj1=j1+mah1
      IF(nj1==0)PRINT '(1x,i3,6(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==1)PRINT '(1x,i3,12x,5(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==2)PRINT '(1x,i3,24x,4(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==3)PRINT '(1x,i3,36x,3(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==4)PRINT '(1x,i3,48x,2(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==5)PRINT '(1x,i3,60x,1(1x,e12.5))',i,(a(i,j),j=j1,j2)
   ENDDO
   READ(*,*)
ENDDO
END SUBROUTINE wrtb

END MODULE m_plib8mat2
