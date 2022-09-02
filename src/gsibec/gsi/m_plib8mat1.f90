MODULE m_plib8mat1
!$$$ module documentation block
!              .      .    .                                       .
! module:  m_plib8mat1
!
! abstract:  Routines for basic algebraic operations on general matrices 
!             and vectors
!
! additional notes:
!  These routines, perform basic algebraic operations on real vectors and
!  matrices. The task performed by each routine is, as far as possible,
!  encoded in each routine's name; three letters describe the
!  operation, the remainder defining the type of operand and, if needed to
!  resolve an ambiguity, the type of result.
!
! program history log:
!   2011-07-04 todling - set to double precision to allow running GSI in
!                        in either single or double precision
!
! remarks:
!   1. routines here must work under REAL*8 (double precision)
!
!  OPERATIONS:
!   DET     evaluate log-determinant
!   DIF     differentiate
!   INT     integrate
!   INV     invert the matrix, or linear system involving the matrix operand
!   L1L     Cholesky LU decomposition, where U is just L-transpose
!   L1U     L-U decomposition of first arg, with 1's along diagonal of L and U
!   LDL     Cholesky LDU decomposition, where U is just L-transpose and D diag.
!   LDU     LDU decomposition
!   NOR     evaluate norm of operand
!   POL     polynomial (first argument) of second argument
!   POW     raise operand to some integer power
!   SWP     swap first two operands
!   TRC     evaluate trace of operand
!   U1L     back substitution with matrix decomposed into LU form, 1's on diag.
!   UDL     back substitution with matrix decomposed into LDU form
!   WRT     write out
!   ZER     set operand to zero
!
!  OPERAND TYPES:
!   B	    banded matrix
!   C	    circulant matrix
!   D	    diagonal matrix
!   H	    symmetric or hermitian matrix
!   L	    lower triangular matrix
!   M	    matrix (rectangular, in general)
!   P	    polynomial or power-series coefficient vector
!   Q	    sQuare matrix with Fortran dimension same as logical dimension
!   R	    row of a matrix
!   S	    scalar
!   T	    transpose of the matrix
!   U	    upper triangular matrix
!   V	    vector, or column of a matrix
!   X	    field of parallel X-vectors (aligned like "columns" of a matrix)
!   Y	    field of parallel Y-vectors (aligned like "rows" of a matrix)
!
! program history:
!   1994-  -    R.J.Purser - initial coding
!   2008-04-25  safford    - add standard documentation blocks
!
! subroutines included:
!   pro333
!   dpro333
!   cro33
!   dcro33
!   norv
!   dnorv
!   norq
!   dnorq
!   swpvv
!   dswpvv
!   mulmd
!   dmulmd
!   multd
!   dmultd
!   muldm
!   dmuldm
!   muldt
!   dmuldt
!   mulpp
!   dmulpp
!   madpp
!   dmadpp
!   msbpp
!   dmsbpp
!   difp
!   ddifp
!   intp
!   dintp
!   invp
!   dinvp
!   prgv
!   dprgv
!   mulcc
!   dmulcc
!   madcc
!   dmadcc
!   msbcc
!   dmsbcc
!   zerl
!   dzerl
!   zeru
!   dzeru
!   ldum
!   dldum
!   udlmm, udlmv
!   dudlmm,dudlmv
!   linvan
!   dlinvan
!   copdm
!   dcopdm
!   condm
!   dcondm
!   copsm
!   dcopsm
!   consm
!   dconsm
!   addmd
!   daddmd
!   submd
!   dsubmd
!   addms
!   daddms
!   subms
!   dsubms
!   l1lm
!   dl1lm
!   ldlm
!   dldlm
!   invh
!   dinvh
!   invl
!   dinvl
!   linlv
!   dlinlv
!   linuv
!   dlinuv
!   powp
!   dpowp
!   polps
!   dpolps
!   polpp
!   dpolpp
!   trcm
!   dtrcm
!   invmt, linmmt, linmvt
!   dinvmt,dlinmmt,dlinmvt
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use m_kinds, only: i_kind
use m_kinds, only: r_kind => r_double
use constants,only: zero,one
IMPLICIT NONE

! set default to private
  private
! set subroutines/interfaces to public
  public :: pro333
  public :: pro333_d
  public :: cro33
  public :: cro33_d
  public :: norv
  public :: norv_d
  public :: norq
  public :: norq_d
  public :: swpvv
  public :: swpvv_d
  public :: mulmd
  public :: mulmd_d
  public :: multd
  public :: multd_d
  public :: muldm
  public :: muldm_d
  public :: muldt
  public :: muldt_d
  public :: mulpp
  public :: mulpp_d
  public :: madpp
  public :: madpp_d
  public :: msbpp
  public :: msbpp_d
  public :: difp
  public :: difp_d
  public :: intp
  public :: intp_d
  public :: invp
  public :: invp_d
  public :: prgv
  public :: prgv_d
  public :: mulcc
  public :: mulcc_d
  public :: madcc
  public :: madcc_d
  public :: msbcc
  public :: msbcc_d
  public :: zerl
  public :: zerl_d
  public :: zeru
  public :: zeru_d
  public :: ldum
  public :: ldum_d
  public :: udlmm
  public :: udlmm_d
  public :: linvan
  public :: linvan_d
  public :: copdm
  public :: copdm_d
  public :: condm
  public :: condm_d
  public :: copsm
  public :: copsm_d
  public :: consm
  public :: consm_d
  public :: addmd
  public :: addmd_d
  public :: submd
  public :: submd_d
  public :: addms
  public :: addms_d
  public :: subms
  public :: subms_d
  public :: l1lm
  public :: l1lm_d
  public :: ldlm
  public :: ldlm_d
  public :: invh
  public :: invh_d
  public :: invl
  public :: invl_d
  public :: linlv
  public :: linlv_d
  public :: linuv
  public :: linuv_d
  public :: powp
  public :: powp_d
  public :: polps
  public :: polps_d
  public :: polpp
  public :: polpp_d
  public :: trcm
  public :: trcm_d
  public :: inv
  public :: inv_d

INTERFACE pro333  ; MODULE PROCEDURE pro333;                 END INTERFACE
INTERFACE pro333_d; MODULE PROCEDURE dpro333;                END INTERFACE
INTERFACE cro33   ; MODULE PROCEDURE cro33;                  END INTERFACE
INTERFACE cro33_d;  MODULE PROCEDURE dcro33;                 END INTERFACE
INTERFACE norv;     MODULE PROCEDURE norv;                   END INTERFACE
INTERFACE norv_d;   MODULE PROCEDURE dnorv;                  END INTERFACE
INTERFACE norq;     MODULE PROCEDURE norq;                   END INTERFACE
INTERFACE norq_d;   MODULE PROCEDURE dnorq;                  END INTERFACE
INTERFACE swpvv;    MODULE PROCEDURE swpvv;                  END INTERFACE
INTERFACE swpvv_d;  MODULE PROCEDURE dswpvv;                 END INTERFACE
INTERFACE mulmd;    MODULE PROCEDURE mulmd;                  END INTERFACE
INTERFACE mulmd_d;  MODULE PROCEDURE dmulmd;                 END INTERFACE
INTERFACE multd;    MODULE PROCEDURE multd;                  END INTERFACE
INTERFACE multd_d;  MODULE PROCEDURE dmultd;                 END INTERFACE
INTERFACE muldm;    MODULE PROCEDURE muldm;                  END INTERFACE
INTERFACE muldm_d;  MODULE PROCEDURE dmuldm;                 END INTERFACE
INTERFACE muldt;    MODULE PROCEDURE muldt;                  END INTERFACE
INTERFACE muldt_d;  MODULE PROCEDURE dmuldt;                 END INTERFACE
INTERFACE mulpp;    MODULE PROCEDURE mulpp;                  END INTERFACE
INTERFACE mulpp_d;  MODULE PROCEDURE dmulpp;                 END INTERFACE
INTERFACE madpp;    MODULE PROCEDURE madpp;                  END INTERFACE
INTERFACE madpp_d;  MODULE PROCEDURE dmadpp;                 END INTERFACE
INTERFACE msbpp;    MODULE PROCEDURE msbpp;                  END INTERFACE
INTERFACE msbpp_d;  MODULE PROCEDURE dmsbpp;                 END INTERFACE
INTERFACE difp;     MODULE PROCEDURE difp;                   END INTERFACE
INTERFACE difp_d;   MODULE PROCEDURE ddifp;                  END INTERFACE
INTERFACE intp;     MODULE PROCEDURE intp;                   END INTERFACE
INTERFACE intp_d;   MODULE PROCEDURE dintp;                  END INTERFACE
INTERFACE invp;     MODULE PROCEDURE invp;                   END INTERFACE
INTERFACE invp_d;   MODULE PROCEDURE dinvp;                  END INTERFACE
INTERFACE prgv;     MODULE PROCEDURE prgv;                   END INTERFACE
INTERFACE prgv_d;   MODULE PROCEDURE dprgv;                  END INTERFACE
INTERFACE mulcc;    MODULE PROCEDURE mulcc;                  END INTERFACE
INTERFACE mulcc_d;  MODULE PROCEDURE dmulcc;                 END INTERFACE
INTERFACE madcc;    MODULE PROCEDURE madcc;                  END INTERFACE
INTERFACE madcc_d;  MODULE PROCEDURE dmadcc;                 END INTERFACE
INTERFACE msbcc;    MODULE PROCEDURE msbcc;                  END INTERFACE
INTERFACE msbcc_d;  MODULE PROCEDURE dmsbcc;                 END INTERFACE
INTERFACE zerl;     MODULE PROCEDURE zerl;                   END INTERFACE
INTERFACE zerl_d;   MODULE PROCEDURE dzerl;                  END INTERFACE
INTERFACE zeru;     MODULE PROCEDURE zeru;                   END INTERFACE
INTERFACE zeru_d;   MODULE PROCEDURE dzeru;                  END INTERFACE
INTERFACE ldum;     MODULE PROCEDURE ldum;                   END INTERFACE
INTERFACE ldum_d;   MODULE PROCEDURE dldum;                  END INTERFACE
INTERFACE udlmm;    MODULE PROCEDURE udlmm, udlmv;           END INTERFACE
INTERFACE udlmm_d;  MODULE PROCEDURE dudlmm,dudlmv;          END INTERFACE
INTERFACE linvan;   MODULE PROCEDURE linvan;                 END INTERFACE
INTERFACE linvan_d; MODULE PROCEDURE dlinvan;                END INTERFACE
INTERFACE copdm;    MODULE PROCEDURE copdm;                  END INTERFACE
INTERFACE copdm_d;  MODULE PROCEDURE dcopdm;                 END INTERFACE
INTERFACE condm;    MODULE PROCEDURE condm;                  END INTERFACE
INTERFACE condm_d;  MODULE PROCEDURE dcondm;                 END INTERFACE
INTERFACE copsm;    MODULE PROCEDURE copsm;                  END INTERFACE
INTERFACE copsm_d;  MODULE PROCEDURE dcopsm;                 END INTERFACE
INTERFACE consm;    MODULE PROCEDURE consm;                  END INTERFACE
INTERFACE consm_d;  MODULE PROCEDURE dconsm;                 END INTERFACE
INTERFACE addmd;    MODULE PROCEDURE addmd;                  END INTERFACE
INTERFACE addmd_d;  MODULE PROCEDURE daddmd;                 END INTERFACE
INTERFACE submd;    MODULE PROCEDURE submd;                  END INTERFACE
INTERFACE submd_d;  MODULE PROCEDURE dsubmd;                 END INTERFACE
INTERFACE addms;    MODULE PROCEDURE addms;                  END INTERFACE
INTERFACE addms_d;  MODULE PROCEDURE daddms;                 END INTERFACE
INTERFACE subms;    MODULE PROCEDURE subms;                  END INTERFACE
INTERFACE subms_d;  MODULE PROCEDURE dsubms;                 END INTERFACE
INTERFACE l1lm;     MODULE PROCEDURE l1lm;                   END INTERFACE
INTERFACE l1lm_d;   MODULE PROCEDURE dl1lm;                  END INTERFACE
INTERFACE ldlm;     MODULE PROCEDURE ldlm;                   END INTERFACE
INTERFACE ldlm_d;   MODULE PROCEDURE dldlm;                  END INTERFACE
INTERFACE invh;     MODULE PROCEDURE invh;                   END INTERFACE
INTERFACE invh_d;   MODULE PROCEDURE dinvh;                  END INTERFACE
INTERFACE invl;     MODULE PROCEDURE invl;                   END INTERFACE
INTERFACE invl_d;   MODULE PROCEDURE dinvl;                  END INTERFACE
INTERFACE linlv;    MODULE PROCEDURE linlv;                  END INTERFACE
INTERFACE linlv_d;  MODULE PROCEDURE dlinlv;                 END INTERFACE
INTERFACE linuv;    MODULE PROCEDURE linuv;                  END INTERFACE
INTERFACE linuv_d;  MODULE PROCEDURE dlinuv;                 END INTERFACE
INTERFACE powp;     MODULE PROCEDURE powp;                   END INTERFACE
INTERFACE powp_d;   MODULE PROCEDURE dpowp;                  END INTERFACE
INTERFACE polps;    MODULE PROCEDURE polps;                  END INTERFACE
INTERFACE polps_d;  MODULE PROCEDURE dpolps;                 END INTERFACE
INTERFACE polpp;    MODULE PROCEDURE polpp;                  END INTERFACE
INTERFACE polpp_d;  MODULE PROCEDURE dpolpp;                 END INTERFACE
INTERFACE trcm;     MODULE PROCEDURE trcm;                   END INTERFACE
INTERFACE trcm_d;   MODULE PROCEDURE dtrcm;                  END INTERFACE
INTERFACE inv;      MODULE PROCEDURE invmt, linmmt, linmvt;  END INTERFACE
INTERFACE inv_d;    MODULE PROCEDURE dinvmt,dlinmmt,dlinmvt; END INTERFACE

CONTAINS


FUNCTION pro333(d,e,f) RESULT(pro_res)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    pro333
!
!   prgrmmr:
!
! abstract:  triple product of 3 3-vectors
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d(3), e(3), f(3) - input vectors
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(IN   ) :: d(3), e(3), f(3)

REAL(r_kind)                :: pro_res
REAL(r_kind)                :: g(3)

CALL CRO33(E,F,G)
pro_res=DOT_PRODUCT(d,g)
END FUNCTION pro333


FUNCTION dpro333(d,e,f) RESULT(pro_res)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dpro333
!
!   prgrmmr:
!
! abstract:  triple product of 3 3-vectors
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d(3), e(3), f(3) - input vectors
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: d(3), e(3), f(3)

REAL(r_kind)             :: pro_res
REAL(r_kind)             :: g(3)
CALL CRO33_d(E,F,G)
pro_res=DOT_PRODUCT(d,g)
END FUNCTION dpro333


SUBROUTINE cro33(a,b,c) 
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    cro33
!
!   prgrmmr:
!
! abstract:  special case of 3-dimensions:  cross-product
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a(3), b(3)       - input vectors
!
!   output argument list:
!     c(3)             - resulting cross-product
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block$
implicit none

REAL(r_kind),    INTENT(IN   ) :: a(3), b(3)
REAL(r_kind),    INTENT(  OUT) :: c(3)

c(1)=a(2)*b(3)-a(3)*b(2)
c(2)=a(3)*b(1)-a(1)*b(3)
c(3)=a(1)*b(2)-a(2)*b(1)
END SUBROUTINE cro33


SUBROUTINE dcro33(a,b,c)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dcro33
!
!   prgrmmr:
!
! abstract:  special case of 3-dimensions:  cross-product
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a(3), b(3)       - input vectors
!
!   output argument list:
!     c(3)             - resulting cross-product
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(3), b(3)
REAL(r_kind), INTENT(  OUT) :: c(3)

c(1)=a(2)*b(3)-a(3)*b(2)
c(2)=a(3)*b(1)-a(1)*b(3)
c(3)=a(1)*b(2)-a(2)*b(1)
END SUBROUTINE dcro33


FUNCTION norv(d) RESULT(norv_res)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    norv
!
!   prgrmmr:
!
! abstract:  norm of vector
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d                - input vector
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(IN   ) :: d(:)

REAL(r_kind)                :: norv_res

norv_res=SQRT(DOT_PRODUCT(D,D))
END FUNCTION norv


FUNCTION dnorv(d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dnorv
!
!   prgrmmr:
!
! abstract:  norm of vector
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d                - input vector
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(IN   ) :: d(:)

REAL(r_kind):: dnorv

dnorv=SQRT(DOT_PRODUCT(d,d))
END FUNCTION dnorv


FUNCTION norq(d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    norq
!
!   prgrmmr:
!
! abstract:  norm of a matrix
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d                - input matrix
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN   ) :: d(:,:)

REAL(r_kind):: norq
INTEGER(i_kind) m2,i2

m2=SIZE(d,2)
norq=zero; DO i2=1,m2; norq=norq+dot_PRODUCT(d(:,i2),d(:,i2)); ENDDO
norq=SQRT(norq)
END FUNCTION norq


FUNCTION dnorq(d) ! norm of a matrix
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dnorq
!
!   prgrmmr:
!
! abstract:  norm of a matrix
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d                - input matrix
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN   ) :: d(:,:)

REAL(r_kind):: dnorq
INTEGER(i_kind) m2,i2

m2=SIZE(d,2)
dnorq=zero; DO i2=1,m2; dnorq=dnorq+dot_PRODUCT(d(:,i2),d(:,i2)); ENDDO
dnorq=SQRT(dnorq)
END FUNCTION dnorq


SUBROUTINE swpvv(d,e)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    swpvv 
!
!   prgrmmr:
!
! abstract:  swap first two operands of input vectors
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d, e       - 
!
!   output argument list:
!     d, e       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: d(:), e(:)

REAL(r_kind) :: t(SIZE(d))

t = d; d = e; e = t
END SUBROUTINE swpvv


SUBROUTINE dswpvv(d,e)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dswpvv
!
!   prgrmmr:
!
! abstract:  swap first two operads of input vectors
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d, e       - 
!
!   output argument list:
!     d, e       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: d(:), e(:)

REAL(r_kind) :: t(SIZE(d))

t = d; d = e; e = t
END SUBROUTINE dswpvv


SUBROUTINE mulmd(a,d,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulmd
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       - 
!     d          -
!   output argument list:
!     a, b       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m2,j

m2=SIZE(a,2)
DO j=1,m2; b(:,j)=a(:,j)*d(j); ENDDO
END SUBROUTINE mulmd


SUBROUTINE dmulmd(a,d,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmulmd
!
!   prgrmmr:
!
! abstract:  special case of 3-dimensions:  cross-product
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       - 
!     d          - 
!
!   output argument list:
!     a, b       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:)
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m2,j

m2=SIZE(a,2)
DO j=1,m2; b(:,j)=a(:,j)*d(j); ENDDO
END SUBROUTINE dmulmd


SUBROUTINE multd(a,d,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    multd
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       - 
!     d          - 
!
!   output argument list:
!     a, b       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m2,j

m2=SIZE(a,1)
DO j=1,m2; b(:,j) = a(j,:) * d(j); ENDDO
END SUBROUTINE multd


SUBROUTINE dmultd(a,d,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmultd
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m2,j

m2=SIZE(a,1)
DO j=1,m2; b(:,j) = a(j,:) * d(j); ENDDO
END SUBROUTINE dmultd


SUBROUTINE muldm(d,a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    muldm
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind)                :: m1,i

m1=SIZE(a,1)
DO i=1,m1; b(i,:) = d(i)*a(i,:); ENDDO
END SUBROUTINE muldm


SUBROUTINE dmuldm(d,a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmuldm
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind)             :: m1,i

m1=SIZE(a,1)
DO i=1,m1; b(i,:) = d(i)*a(i,:); ENDDO
END SUBROUTINE dmuldm


SUBROUTINE muldt(d,a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    muldt
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind)                :: m1,i

m1=SIZE(a,2)
DO i=1,m1; b(i,:) = d(i)*a(:,i); ENDDO
END SUBROUTINE muldt


SUBROUTINE dmuldt(d,a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmuldt
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m1,i

m1=SIZE(a,2)
DO i=1,m1; b(i,:) = d(i)*a(:,i); ENDDO
END SUBROUTINE dmuldt


SUBROUTINE mulpp(a,b,c)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulpp
!
!   prgrmmr:
!
! abstract:  multiply polynomials, possibly in place
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a, b       -
!     c          -
!
!   output argument list:
!     c          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(IN   ) :: a(0:), b(0:)
REAL(r_kind),    INTENT(INOUT) :: c(0:)

INTEGER(i_kind)                :: m,mcp, j
REAL(r_kind)                   :: s

m=SIZE(a)-1
mcp=mcmax(a,b,m)
c(mcp:m) = zero
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=s
ENDDO
RETURN
ENTRY madpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=c(j-1)+s
ENDDO
RETURN
ENTRY msbpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=c(j-1)-s
ENDDO
RETURN
CONTAINS
FUNCTION mcmax(a,b,m) RESULT(mmx_res) ! This fn can be contained in mulpp().
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    mcmax
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-26  lueken - added subprogram doc block
!
!   input argument list:
!    m
!    a,b
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m
REAL(r_kind),    INTENT(IN   ) :: a(0:m), b(0:m)

INTEGER(i_kind)             :: mmx_res
INTEGER(i_kind)             :: ma, mb

mmx_res=0                     ! default for when ALL elements of c are zero
DO ma=m,0,-1                      ! seek last nonzero coefficient of polynomial a
   IF(a(ma) /= zero)THEN
      DO mb=m,0,-1                  ! seek last nonzero coefficient of polynomial b
         IF(b(mb) /= zero)THEN
            mmx_res=MIN(m,ma+mb)+1 ! hence, 1+last non-0 element of their product
            RETURN
         ENDIF
      ENDDO
      RETURN
   ENDIF
ENDDO
END FUNCTION mcmax
END SUBROUTINE mulpp


SUBROUTINE difp(a,b) ! Symbolically differentiate polynomial
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    difp
!
!   prgrmmr:
!
! abstract:  Symbolically differentiate polynomial
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a          -
!
!   output argument list:
!     b          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(0:)
REAL(r_kind), INTENT(  OUT) :: b(0:)

INTEGER(i_kind)           :: m, i
REAL(r_kind)              :: s, b0

m=SIZE(a)-1
DO i=1,m        ! possibly with coincident storage for a and b
   b(i-1)=i*a(i)
ENDDO
b(m)=zero
RETURN
ENTRY intp(a,b) ! Symbolically integrate polynomial
m=SIZE(a)-1
DO i=m,1,-1     ! possibly with coincident storage for a and b
   b(i)=a(i-1)/i
ENDDO
b(0)=zero
RETURN
ENTRY invp(a,b) ! Invert polynomial or power-series
m=SIZE(a)-1
b0=one/a(0)     ! storage of a and b must not be the same
b(0)=b0
DO i=1,m
   s = SUM(b(i-1:0:-1)*a(1:i))
   b(i)=-b0*s
ENDDO
END SUBROUTINE difp


SUBROUTINE dmulpp(a,b,c) !  multiply polynomials, possibly in place
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmulpp
!
!   prgrmmr:
!
! abstract:  multiply polynomials, possibly in place
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a, b       -
!     c          -
!
!   output argument list:
!     c          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(0:), b(0:)
REAL(r_kind), INTENT(INOUT) :: c(0:)

INTEGER(i_kind)            :: m,mcp, j
REAL(r_kind)               :: s

m=SIZE(a)-1
mcp=mcmax(a,b,m)
c(mcp:m) = zero
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=s
ENDDO
RETURN
ENTRY dmadpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=c(j-1)+s
ENDDO
RETURN
ENTRY dmsbpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=c(j-1)-s
ENDDO
RETURN
CONTAINS

FUNCTION mcmax(a,b,m) RESULT(mmx_res)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mcmax
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-26  lueken - added subprogram doc block
!
!   input argument list:
!    m
!    a,b
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m
REAL(r_kind)   ,  INTENT(IN   ) :: a(0:m), b(0:m)

INTEGER(i_kind)              :: mmx_res
INTEGER(i_kind)              :: ma, mb

mmx_res=0                     ! default for when all elements of c are zero
DO ma=m,0,-1                      ! seek last nonzero coefficient of polynomial a
   IF(a(ma) /= zero)THEN
      DO mb=m,0,-1                  ! seek last nonzero coefficient of polynomial b
         IF(b(mb) /= zero)THEN
            mmx_res=MIN(m,ma+mb)+1 ! hence, 1+last non-0 element of their product
            RETURN
         ENDIF
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN
END FUNCTION mcmax

END SUBROUTINE dmulpp


SUBROUTINE ddifp(a,b) ! Symbolically differentiate polynomial
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ddifp
!
!   prgrmmr:
!
! abstract:  Symbolically differentiate polynomial
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a          -
!     b          -
!
!   output argument list:
!     b          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(0:)
REAL(r_kind), INTENT(INOUT) :: b(0:)

INTEGER(i_kind)            :: m, i
REAL(r_kind)               :: s, b0

m=SIZE(a)-1
DO i=1,m         ! possibly with coincident storage for a and b
   b(i-1)=i*a(i)
ENDDO
b(m)=zero
RETURN
ENTRY dintp(a,b) ! Symbolically integrate polynomial
m=SIZE(a)-1
DO i=m,1,-1      ! possibly with coincident storage for a and b
   b(i)=a(i-1)/i
ENDDO
b(0)=zero
RETURN
ENTRY dinvp(a,b) ! Invert polynomial or power-series
m=SIZE(a)-1
b0=one/a(0)      ! storage of a and b must not be the same
b(0)=b0
DO i=1,m
   s = SUM(b(i-1:0:-1)*a(1:i))
   b(i)=-b0*s
ENDDO
END SUBROUTINE ddifp


SUBROUTINE prgv(d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    prgv
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d          -
!
!   output argument list:
!     d          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: d(:)

REAL(r_kind), PARAMETER        :: crit=1.E-30_r_kind
INTEGER(i_kind)                :: i,m

m=SIZE(d)
DO i=1,m; IF(ABS(d(i)) <= crit)d(i)=zero; ENDDO
END SUBROUTINE prgv


SUBROUTINE dprgv(d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dprgv
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d          -
!
!   output argument list:
!     d          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: d(:)

REAL(r_kind), PARAMETER     :: crit=1.E-30_r_kind
INTEGER(i_kind)             :: i,m

m=SIZE(d)
DO i=1,m; IF(ABS(d(i)) <= crit)d(i)=zero; ENDDO
END SUBROUTINE dprgv


SUBROUTINE mulcc(a,b,c,m)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulcc
!
!   prgrmmr:
!
! abstract:  Multiply circulant matrices of period M
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b, c    -
!     m          -
!
!   output argument list:
!     a, b, c    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m
REAL(r_kind)   , INTENT(INOUT) :: a(0:m-1), b(0:m-1), c(0:m-1)

INTEGER(i_kind)                :: mm, j

c(0:m-1) = zero
ENTRY madcc(a,b,c,m)
mm=m-1
DO j=0,mm
   c(j:m-1) = c(j:m-1) + a(0:m-j-1)*b(j)
   c(0:j-1) = c(0:j-1) + a(m-j:m-1)*b(j)
ENDDO
RETURN
ENTRY msbcc(a,b,c,m)
mm=m-1
DO j=0,mm
   c(j:m-1) = c(j:m-1) - a(0:m-j-1)*b(j)
   c(0:j-1) = c(0:j-1) - a(m-j:m-1)*b(j)
ENDDO
END SUBROUTINE mulcc


SUBROUTINE dmulcc(a,b,c,m)  ! Multiply circulant matrices of period M
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmulcc
!
!   prgrmmr:
!
! abstract:  Multiply circulant matrices of period M
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b, c    -
!     m          -
!
!   output argument list:
!     a, b, c    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m
REAL(r_kind)   , INTENT(INOUT) :: a(0:m-1), b(0:m-1), c(0:m-1)

INTEGER(i_kind)                :: mm, j

c(0:m-1) = zero
ENTRY dmadcc(a,b,c,m)
mm=m-1
DO j=0,mm
   c(j:m-1) = c(j:m-1) + a(0:m-j-1)*b(j)
   c(0:j-1) = c(0:j-1) + a(m-j:m-1)*b(j)
ENDDO
RETURN
ENTRY dmsbcc(a,b,c,m)
mm=m-1
DO j=0,mm
   c(j:m-1) = c(j:m-1) - a(0:m-j-1)*b(j)
   c(0:j-1) = c(0:j-1) - a(m-j:m-1)*b(j)
ENDDO
END SUBROUTINE dmulcc


SUBROUTINE zerl(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zerl
!
!   prgrmmr:
!
! abstract:  Zero out the strictly lower triangle of elements
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(INOUT) :: a(:,:)

INTEGER(i_kind)           :: m,j

m=SIZE(a,1); DO j=1,m; a(j+1:m,j) = zero; ENDDO; RETURN

ENTRY zeru(a)       ! Zero out the strictly upper triangle of elements
m=SIZE(a,1); DO j=1,m; a(1:j-1,j) = zero; ENDDO
END SUBROUTINE zerl


SUBROUTINE dzerl(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmuldm
!
!   prgrmmr:
!
! abstract:  Zero out the strictly lower triangle of elements
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(INOUT) :: a(:,:)

INTEGER(i_kind)           :: m,j

m=SIZE(a,1); DO j=1,m; a(j+1:m,j) = zero; ENDDO; RETURN

ENTRY dzeru(a)      ! Zero out the strictly upper triangle of elements
m=SIZE(a,1); DO j=1,m; a(1:j-1,j) = zero; ENDDO
END SUBROUTINE dzerl


SUBROUTINE ldum(a,ipiv,d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldum
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.	1996
!
! abstract:  perform l-d-u decomposition of square matrix a in place with
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - square matrix to be factorized
!
!   output argument list:
!     a          - square matrix to be factorized
!     ipiv       - ipiv array encoding the pivoting sequence
!     d          - indicator for possible sign change of determinant
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(INOUT) :: a(:,:) 
REAL(r_kind),    INTENT(OUT  ) :: d
INTEGER(i_kind), INTENT(OUT  ) :: ipiv(:)

INTEGER(i_kind)                :: m,i, j, jp, ibig, jm
REAL(r_kind)                   :: s(SIZE(a,1)),  aam, aa, abig,  ajj, ajji, aij

m=SIZE(a,1)
DO i=1,m
   aam=zero
   DO j=1,m
      aa=ABS(a(i,j))
      IF(aa > aam)aam=aa
   ENDDO
   IF(aam == zero)THEN
      PRINT '(" row ",i3," of matrix in ldum vanishes")',i
      STOP
   ENDIF
   s(i)=one/aam
ENDDO
d=one
ipiv(m)=m
DO j=1,m-1
   jp=j+1
   abig=s(j)*ABS(a(j,j))
   ibig=j
   DO i=jp,m
      aa=s(i)*ABS(a(i,j))
      IF(aa > abig)THEN
         ibig=i
         abig=aa
      ENDIF
   ENDDO
!  swap rows, recording changed sign of determinant
   ipiv(j)=ibig
   IF(ibig /= j)THEN
      d=-d
      CALL swpvv(a(j,:),a(ibig,:))
      s(ibig)=s(j)
   ENDIF
   ajj=a(j,j)
   IF(ajj == zero)THEN
      jm=j-1
      PRINT '(" failure in ldum:"/" matrix singular, rank=",i3)',jm
      STOP
   ENDIF
   ajji=one/ajj
   DO i=jp,m
      aij=ajji*a(i,j)
      a(i,j)=aij
      a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
   ENDDO
ENDDO
END SUBROUTINE ldum


SUBROUTINE DLDUM(A,IPIV,D)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dldum
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1996
!
! abstract:  perform l-d-u decomposition of square matrix a in place with
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - square matrix to be factorized
!
!   output argument list:
!     a          - square matrix to be factorized
!     ipiv       - ipiv array encoding the pivoting sequence
!     d          - indicator for possible sign change of determinant
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind)   , INTENT(INOUT) :: a(:,:) 
REAL(r_kind)   , INTENT(  OUT) :: d
INTEGER(i_kind), INTENT(  OUT) :: ipiv(:)

INTEGER(i_kind)                :: m,i, j, jp, ibig, jm
REAL(r_kind)                   :: s(SIZE(a,1)),  aam, aa, abig,  ajj, ajji, aij

m=SIZE(a,1)
DO i=1,m
   aam=zero
   DO j=1,m
      aa=ABS(a(i,j))
      IF(aa > aam)aam=aa
   ENDDO
   IF(aam == zero)THEN
      PRINT '(" row ",i3," of matrix in dldum vanishes")',i
      STOP
   ENDIF
   s(i)=one/aam
ENDDO
d=one
ipiv(m)=m
DO j=1,m-1
   jp=j+1
   abig=s(j)*ABS(a(j,j))
   ibig=j
   DO i=jp,m
      aa=s(i)*ABS(a(i,j))
      IF(aa > abig)THEN
         ibig=i
         abig=aa
      ENDIF
   ENDDO
!  swap rows, recording changed sign of determinant
   ipiv(j)=ibig
   IF(ibig /= j)THEN
      d=-d
      CALL swpvv_d(a(j,:),a(ibig,:))
      s(ibig)=s(j)
   ENDIF
   ajj=a(j,j)
   IF(ajj == zero)THEN
      jm=j-1
      PRINT '(" Failure in dldum:"/" matrix singular, rank=",i3)',jm
      STOP
   ENDIF
   ajji=one/ajj
   DO i=jp,m
      aij=ajji*a(i,j)
      a(i,j)=aij
      a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
   ENDDO
ENDDO
END SUBROUTINE dldum


SUBROUTINE udlmm(a,b,ipiv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlmm
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  use l-u factors in A to back-substitute for mm rhs in B, 
!            using ipiv to define the pivoting permutation used in the l-u 
!            decomposition.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - L-D-U factorization of linear system matrux
!     b          - right-hand-sides on entry, corresponding matrix of solution
!	           vectors on return
!     ipiv       - ipiv array encoding the pivoting sequence
!
!   output argument list:
!     b          - right-hand-sides on entry, corresponding matrix of solution
!	           vectors on return
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: ipiv(:) 
REAL(r_kind),    INTENT(IN   ) :: a(:,:) 
REAL(r_kind),    INTENT(INOUT) :: b(:,:) 

INTEGER(i_kind)                :: m,mm,i, k, l
REAL(r_kind)                   :: s,aiii

m=SIZE(a,1); mm=SIZE(b,2)
DO k=1,mm !loop over columns of b
   DO i=1,m
      l=ipiv(i)
      s=b(l,k)
      b(l,k)=b(i,k)
      s = s - SUM(b(1:i-1,k)*a(i,1:i-1))
      b(i,k)=s
   ENDDO
   b(m,k)=b(m,k)/a(m,m)
   DO i=m-1,1,-1
      aiii=one/a(i,i)
      b(i,k) = b(i,k) - SUM(b(i+1:m,k)*a(i,i+1:m))
      b(i,k)=b(i,k)*aiii
   ENDDO
ENDDO
END SUBROUTINE udlmm


SUBROUTINE dudlmm(a,b,ipiv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dudlmm
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  use l-u factors in A to back-substitute for mm rhs in B,
!            using ipiv to define the pivoting permutation used in the l-u
!            decomposition.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - L-D-U factorization of linear system matrux
!     b          - right-hand-sides on entry, corresponding matrix of solution
!                  vectors on return
!     ipiv       - ipiv array encoding the pivoting sequence
!
!   output argument list:
!     b          - right-hand-sides on entry, corresponding matrix of solution
!                  vectors on return
!     
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: ipiv(:) 
REAL(r_kind)   , INTENT(IN   ) :: a(:,:) 
REAL(r_kind)   , INTENT(INOUT) :: b(:,:) 

INTEGER(i_kind)                :: m,mm,i, k, l
REAL(r_kind)                   :: s,aiii

m=SIZE(a,1); mm=SIZE(b,2)
DO k=1,mm !loop over columns of b
   DO i=1,m
      l=ipiv(i)
      s=b(l,k)
      b(l,k)=b(i,k)
      s = s - SUM(b(1:i-1,k)*a(i,1:i-1))
      b(i,k)=s
   ENDDO
   b(m,k)=b(m,k)/a(m,m)
   DO i=m-1,1,-1
      aiii=one/a(i,i)
      b(i,k) = b(i,k) - SUM(b(i+1:m,k)*a(i,i+1:m))
      b(i,k)=b(i,k)*aiii
   ENDDO
ENDDO
END SUBROUTINE dudlmm


SUBROUTINE udlmv(a,b,ipiv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlmv
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  use l-u factors in A to back-substitute for mm rhs in B, using 
!            ipiv to define the pivoting permutation used in the l-u 
!            decomposition.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - L-D-U factorization of linear system matrux
!     b          - right-hand-side on entry, corresponding vector solution
!                  on return
!     ipiv       - ipiv array encoding the pivoting sequence
!
!   output argument list:
!     b          - right-hand-side on entry, corresponding vector solution
!                  on return
!     
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: ipiv(:) 
REAL(r_kind),    INTENT(IN   ) :: a(:,:) 
REAL(r_kind),    INTENT(INOUT) :: b(:) 

INTEGER(i_kind)                :: m,i, l
REAL(r_kind)                   :: s,aiii

m=SIZE(a,1)
DO i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - SUM(b(1:i-1)*a(i,1:i-1))
   b(i)=s
ENDDO
b(m)=b(m)/a(m,m)
DO i=m-1,1,-1
   aiii=one/a(i,i)
   b(i) = b(i) - SUM(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
ENDDO
END SUBROUTINE udlmv


SUBROUTINE dudlmv(a,b,ipiv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dudlmv
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  use l-u factors in A to back-substitute for mm rhs in B, using
!            ipiv to define the pivoting permutation used in the l-u
!            decomposition.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - L-D-U factorization of linear system matrux
!     b          - right-hand-side on entry, corresponding vector solution
!                  on return
!     ipiv       - ipiv array encoding the pivoting sequence
!
!   output argument list:
!     b          - right-hand-side on entry, corresponding vector solution
!                  on return
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: ipiv(:) 
REAL(r_kind)   ,  INTENT(IN   ) :: a(:,:) 
REAL(r_kind)   ,  INTENT(INOUT) :: b(:) 

INTEGER(i_kind)                 :: m,i, l
REAL(r_kind)                    :: s,aiii

m=SIZE(a,1)
DO i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - SUM(b(1:i-1)*a(i,1:i-1))
   b(i)=s
ENDDO
b(m)=b(m)/a(m,m)
DO i=m-1,1,-1
   aiii=one/a(i,i)
   b(i) = b(i) - SUM(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
ENDDO
END SUBROUTINE dudlmv


SUBROUTINE linvan(w,ab)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linvan
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     W       - Generalized Vandermonde matrix in, conditioning indicator out.
!     AB      - R.h.s. vector in, solution vector of numerical coefficients out.
!
!   output argument list:
!     W       - Generalized Vandermonde matrix in, conditioning indicator out.
!     AB      - R.h.s. vector in, solution vector of numerical coefficients out.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: w(:,:), ab(:)

INTEGER(i_kind), PARAMETER  :: nit=20
REAL(r_kind)                :: d1(SIZE(w,1)), d2(SIZE(w,1)), &
                               w2(SIZE(w,1),SIZE(w,1)),v(SIZE(w,1))
INTEGER(i_kind)             :: i, j, it, jt, ipiv(SIZE(w,1)), nc
REAL(r_kind)                :: p, e, dw, c, d, d2j
REAL(r_kind),ALLOCATABLE    :: wv(:,:) ! work variable for ab(nc) and v(nn)

nc = SIZE(w,DIM=1)
ALLOCATE(wv(nc,1))

w2=w                ! Preserve original W and AB for use
v = ab(1:nc)        ! in later "clean-up" operation.

d1 = one            ! Row scaling factors set to default
d2 = one            ! Column scaling factors set to default

C=1.E-16_r_kind	    ! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
DO i=1,nc
   p=zero
   e=zero
   DO j=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
         e=e+one
         p=p+LOG(dw)
      ENDIF
   ENDDO
   IF(E == zero)STOP 'W effectively singular in LINVAN'
   d1(i)=EXP(-p/e)
ENDDO
CALL muldm(d1,w2,w)

DO j=1,nc
   p=zero
   e=zero
   DO i=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
         e=e+one
         p=p+LOG(dw)
      ENDIF
   ENDDO
   IF(E == zero)STOP 'W effectively singular in LINVAN'
   d2(j)=EXP(-p/e)
ENDDO
CALL mulmd(w,d2,w)

c=1.e-8_r_kind  ! reset the criterion for "negligible" elements

! revert to iterations of the more efficient method without logarithms:
DO jt=1,2
   DO it=1,nit    !	perform nit relaxation iterations
      DO i=1,nc    !	do rows:
         p=one
         e=zero
         DO j=1,nc
            dw=ABS(w(i,j))
            IF(dw > c)THEN
               e=e+one
               p=p*dw
            ENDIF
         ENDDO
         p=one/(p**(one/e))
         w(i,:) = w(i,:) * p            ! rescale this row of w..
         d1(i)=d1(i)*p     ! ..and update d1 consistently
      ENDDO
      DO j=1,nc    !	do columns:
         p=one
         e=zero
         d2j=d2(j)
         DO i=1,nc
            dw=ABS(w(i,j))
            IF(dw > c)THEN
               e=e+one
               p=p*dw
            ENDIF
         ENDDO
         p=one/(p**(one/e))
         w(:,j) = w(:,j) * p        ! rescale this column of w..
         d2(j)=d2(j)*p       ! ..and update d2 consistently
      ENDDO
   ENDDO
   c=1.e-3_r_kind    ! final setting for criterion for "negligible" elements
ENDDO
ab(1:nc) = d1(1:nc) * ab(1:nc) ! rescale r.h.s vector by d1
p=one     ! p becomes product of row-lengths:
DO i=1,nc
   p=p*SQRT(dot_PRODUCT(w(i,:),w(i,:)))
ENDDO
CALL ldum(w,ipiv,d)
DO i=1,nc
   d=d*w(i,i)      ! d becomes the determinant of w
ENDDO
wv(:,1) = ab ! convert shape of array
CALL udlmm(w,wv(:,1:1),ipiv)
ab = d2 * wv(:,1) ! rescale solution vector by d2
!     ab(1:nc) = d2(1:nc) * ab(1:nc) ! rescale solution vector by d2
!  note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of w. we invoke original matrix elements w2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  begin "clean-up" process. substitute solution vector in original
!  equation and leave the residual difference in v
v=v-MATMUL(w2,ab)
v = d1 * v    ! rescale the residual vector by d1
wv(:,1) = v ! convert shape of array
CALL udlmm(w,wv(:,1:1),ipiv) ! solve linear system with this rhs.
ab=ab+wv(:,1)*d2 ! add residual solution vector, 
                                      ! scaled, to ab

DEALLOCATE(wv)
w(1,1)=d/p  ! this ratio is an indicator of the overall conditioning
            ! when d/p is very small, treat the results with suspicion!

END SUBROUTINE linvan


SUBROUTINE dlinvan(w,ab)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dlinvan
!                .      .    .
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1996
!
! abstract: 
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     W   -  Generalized Vandermonde matrix in, conditioning indicator out.
!     AB  -  R.h.s. vector in, solution vector of numerical coefficients out.
!
!   output argument list:
!     W   -  Generalized Vandermonde matrix in, conditioning indicator out.
!     AB  -  R.h.s. vector in, solution vector of numerical coefficients out.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: w(:,:), ab(:)

INTEGER(i_kind), PARAMETER     :: nit=20
REAL(r_kind)                   :: d1(SIZE(w,1)), d2(SIZE(w,1)), &
                                  w2(SIZE(w,1),SIZE(w,1)),v(SIZE(w,1))
INTEGER(i_kind)                :: i, j, it, jt, ipiv(SIZE(w,1)), nc
REAL(r_kind)                   :: p, e, dw, c, d, d2j
REAL(r_kind),ALLOCATABLE       :: wv(:,:) ! work variable for ab(nc) and v(nn)

nc = SIZE(w,DIM=1)
ALLOCATE(wv(nc,1))

w2=w                ! Preserve original W and AB for use
v = ab(1:nc)        ! in later "clean-up" operation.

d1 = one            ! Row scaling factors set to default
d2 = one            ! Column scaling factors set to default

C=1.E-16_r_kind     ! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
DO i=1,nc
   p=zero
   e=zero
   DO j=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
         e=e+one
         p=p+LOG(dw)
      ENDIF
   ENDDO
   IF(e == zero)STOP 'w effectively singular in linvan'
   d1(i)=EXP(-p/e)
ENDDO
CALL muldm_d(d1,w2,w)

DO j=1,nc
   p=zero
   e=zero
   DO i=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
         e=e+one
         p=p+LOG(dw)
      ENDIF
   ENDDO
   IF(e == zero)STOP 'w effectively singular in linvan'
   d2(j)=EXP(-p/e)
ENDDO
CALL mulmd_d(w,d2,w)
 
c=1.e-8_r_kind  ! reset the criterion for "negligible" elements

! revert to iterations of the more efficient method without logarithms:
DO jt=1,2
   DO it=1,nit    !	perform nit relaxation iterations
      DO i=1,nc    !	do rows:
         p=one
         e=zero
         DO j=1,nc
            dw=ABS(w(i,j))
            IF(dw > c)THEN
               e=e+one
               p=p*dw
            ENDIF
         ENDDO
         p=one/(p**(one/e))
         w(i,:) = w(i,:) * p            ! rescale this row of w..
         d1(i)=d1(i)*p     ! ..and update d1 consistently
     ENDDO
    DO j=1,nc    ! do columns:
      p=one
      e=zero
      d2j=d2(j)
      DO i=1,nc
        dw=ABS(w(i,j))
        IF(dw > c)THEN
          e=e+one
          p=p*dw
        ENDIF
      ENDDO
      p=one/(p**(one/e))
      w(:,j) = w(:,j) * p        ! rescale this column of w..
      d2(j)=d2(j)*p       ! ..and update d2 consistently
    ENDDO
  ENDDO
  c=1.e-3_r_kind    ! final setting for criterion for "negligible" elements
ENDDO
ab(1:nc) = d1(1:nc) * ab(1:nc) ! rescale r.h.s vector by d1
p=one     ! p becomes product of row-lengths:
DO i=1,nc
   p=p*SQRT(dot_PRODUCT(w(i,:),w(i,:)))
ENDDO
CALL ldum_d(w,ipiv,d)
DO i=1,nc
  d=d*w(i,i)      ! d becomes the determinant of w
ENDDO
wv(:,1) = ab ! convert shape of array
CALL udlmm_d(w,wv(:,1:1),ipiv)
ab = d2 * wv(:,1) ! rescale solution vector by d2
!     ab(1:nc) = d2(1:nc) * ab(1:nc) ! Rescale solution vector by D2
!  Note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of W. We invoke original matrix elements W2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  Begin "clean-up" process. Substitute solution vector in original
!  equation and leave the residual difference in V
v=v-MATMUL(w2,ab)
v = d1 * v    ! Rescale the residual vector by D1
wv(:,1) = v ! Convert shape of array
CALL UDLMM_d(w,wv(:,1:1),ipiv) ! Solve linear system with THIS rhs.
ab=ab+wv(:,1)*d2 ! Add residual solution vector, 
                                      ! scaled, to AB

DEALLOCATE(wv)
w(1,1)=d/p  ! this ratio is an indicator of the overall conditioning
            ! When D/P is very small, treat the results with suspicion!

END SUBROUTINE dlinvan


SUBROUTINE copdm(d,a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copdm
!
!   prgrmmr:     
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:),INTENT(IN)::d; REAL(r_kind),DIMENSION(:,:),INTENT(OUT)::a; INTEGER(i_kind) i
                  a=zero; DO i=1,SIZE(a,1); a(i,i)= d(i); ENDDO; RETURN
ENTRY condm(d,a); a=zero; DO i=1,SIZE(a,1); a(i,i)=-d(i); ENDDO
END SUBROUTINE copdm


SUBROUTINE dcopdm(d,a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dcopdm
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:),INTENT(IN)::d; REAL(r_kind),DIMENSION(:,:),INTENT(OUT)::a
INTEGER(i_kind) i
                   a=zero; DO i=1,SIZE(a,1); a(i,i)= d(i); ENDDO; RETURN
ENTRY dcondm(d,a); a=zero; DO i=1,SIZE(a,1); a(i,i)=-d(i); ENDDO
END SUBROUTINE dcopdm


SUBROUTINE copsm(s,a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copsm
!
!   prgrmmr:    
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     s          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN) :: s; REAL(r_kind),DIMENSION(:,:),INTENT(OUT):: a; INTEGER(i_kind) i
                  a=zero; DO i=1,SIZE(a,1); a(i,i)= s; ENDDO; RETURN
ENTRY consm(s,a); a=zero; DO i=1,SIZE(a,1); a(i,i)=-s; ENDDO
END SUBROUTINE copsm


SUBROUTINE dcopsm(s,a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dcopsm
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     s          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN) :: s; REAL(r_kind),DIMENSION(:,:),INTENT(OUT):: a; INTEGER(i_kind) i
                   a=zero; DO i=1,SIZE(a,1); a(i,i)= s; ENDDO; RETURN
ENTRY dconsm(s,a); a=zero; DO i=1,SIZE(a,1); a(i,i)=-s; ENDDO
END SUBROUTINE dcopsm


SUBROUTINE addmd(a,b,d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    addmd
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          - 
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT):: a,b; REAL(r_kind),DIMENSION(:),INTENT(IN):: d
REAL(r_kind) s;  INTEGER(i_kind) i
                   b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+d(i); ENDDO; RETURN
ENTRY submd(a,b,d);b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-d(i); ENDDO; RETURN
ENTRY addms(a,b,s);b=a; DO I=1,SIZE(a,1); b(i,i)=b(i,i)+s;    ENDDO; RETURN
ENTRY SUBMS(A,B,S);b=a; DO I=1,SIZE(a,1); B(I,I)=B(I,I)-S;    ENDDO;
END SUBROUTINE addmd


SUBROUTINE daddmd(a,b,d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    daddmd
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       - 
!     d          -
!
!   output argument list:
!     a, b       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT)::A,B;REAL(r_kind),DIMENSION(:),INTENT(IN)::D
REAL(r_kind) s; INTEGER(i_kind) i
                     b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+d(i); ENDDO; RETURN
ENTRY DSUBMD(A,B,D); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-d(i); ENDDO; RETURN
ENTRY DADDMS(A,B,S); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+s;    ENDDO; RETURN
ENTRY DSUBMS(A,B,S); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-s;    ENDDO;
END SUBROUTINE daddmd


SUBROUTINE l1lm(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    l1lm
!
!   prgrmmr:    
!
! abstract:  Cholesky, M -> L*U, U(i,j)=L(j,i)
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - 
!     b          - 
!
!   output argument list:
!     b          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)
REAL(r_kind), INTENT(INOUT) :: b(:,:)
INTEGER(i_kind)             :: m,j, jm, jp, i
REAL(r_kind)                :: s, bjji
m=SIZE(a,1)
DO j=1,m
  jm=j-1
  jp=j+1
  s = a(j,j) - SUM(b(j,1:jm)*b(j,1:jm))
  IF(S <= zero)THEN
    PRINT '(" L1LM detects non-positivity at diagonal index",i2)',J
    STOP
  ENDIF
  b(j,j)=SQRT(s)
  bjji=one/b(j,j)
  DO i=jp,m
    s = a(i,j) - SUM(b(i,1:jm)*b(j,1:jm))
    b(i,j)=s*bjji
  ENDDO
  b(1:jm,j) = zero
ENDDO
END SUBROUTINE l1lm


SUBROUTINE DL1LM(A,B)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dl1lm
!
!   prgrmmr:    
!
! abstract:  Cholesky, M -> L*U, U(i,j)=L(j,i)
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
! 
!   input argument list:
!     a          -
!     b          -
!
!   output argument list:
!     b          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:) 
REAL(r_kind), INTENT(INOUT) :: b(:,:) 

INTEGER(i_kind) :: m,j, jm, jp, i
REAL(r_kind) :: s, bjji

m=SIZE(a,1)
DO j=1,m
   jm=j-1
   jp=j+1
   s = a(j,j) - SUM(b(j,1:jm)*b(j,1:jm))
   IF(s <= zero)THEN
      PRINT '(" L1LM detects non-positivity at diagonal index",i2)',J
      STOP
   ENDIF
   b(j,j)=SQRT(s)
   bjji=one/b(j,j)
   DO i=jp,m
      s = a(i,j) - SUM(b(i,1:jm)*b(j,1:jm))
      b(i,j)=s*bjji
   ENDDO
   b(1:jm,j) = zero
ENDDO
RETURN
END SUBROUTINE dl1lm

SUBROUTINE ldlm(a,b,d) ! Modified Cholesky decompose Q --> L*D*U, U(i,j)=L(j,i)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlm
!
!   prgrmmr:    
!
! abstract:  Modified Cholesky decompose Q --> L*D*U, U(i,j)=L(j,i)
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
! 
!   input argument list:
!     a          -
!     b          -
!
!   output argument list:
!     b          -
!     d          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)
REAL(r_kind), INTENT(INOUT) :: b(:,:)
REAL(r_kind), INTENT(  OUT) :: d(:)

INTEGER(i_kind) :: m,j, jm, jp, i
REAL(r_kind) :: bjji

m=SIZE(a,1)
DO j=1,m
   jm=j-1
   jp=j+1
   d(j)=a(j,j) - SUM(b(1:jm,j)*b(j,1:jm))
  
   b(j,j) = one
   IF(d(j) == zero)THEN
      PRINT '(" LDLM detects singularity at diagonal index",i2)',J
      STOP
   ENDIF
   bjji=one/d(j)
   DO i=jp,m
      b(j,i)= a(i,j) - dot_PRODUCT(b(1:jm,j),b(i,1:jm))
      b(i,j)=b(j,i)*bjji
   ENDDO
ENDDO
CALL zeru(b)
RETURN
END SUBROUTINE ldlm


SUBROUTINE dldlm(a,b,d) ! Modified Cholesky  Q --> L*D*U, U(i,j)=L(j,i)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dldlm
!
!   prgrmmr:    
!
! abstract:  Modified Cholesky  Q --> L*D*U, U(i,j)=L(j,i)
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
! 
!   input argument list:
!     a          -
!     b          -
!
!   output argument list:
!     b          -
!     d          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)
REAL(r_kind), INTENT(INOUT) :: b(:,:)
REAL(r_kind), INTENT(  OUT) :: d(:)

INTEGER(i_kind)             :: m,j, jm, jp, i
REAL(r_kind)                :: bjji

m=SIZE(a,1)
DO j=1,m; jm=j-1; jp=j+1
   d(j)=a(j,j) - SUM(b(1:jm,j)*b(j,1:jm))
   b(j,j) = one
   IF(d(j) == zero)THEN
      PRINT '(" DLDLM detects singularity at diagonal index",i2)',J
      STOP
   ENDIF
   bjji=one/d(j)
   DO i=jp,m
      b(j,i)= a(i,j) - dot_PRODUCT(b(1:jm,j),b(i,1:jm))
      b(i,j)=b(j,i)*bjji
   ENDDO
ENDDO
CALL zeru_d(b)
RETURN
END SUBROUTINE dldlm


SUBROUTINE invh(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    invh
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1993
!
! abstract:   Inver,t in place, a symmetric matrix
!
! limitation:  This routine incorporates no pivoting - it is intended for matrices
!              that are already diagonally dominant
!  
! program history log:
!   2008-04-25  safford -- add subprogram doc block
! 
!   input argument list:
!     A          - symmetric square matrix, output as inverse of input
!
!   output argument list:
!     A          - symmetric square matrix, output as inverse of input
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:) 

INTEGER(i_kind)                  :: m,k, kp, i, ip, j
REAL(r_kind),DIMENSION(SIZE(a,1)):: d

m=SIZE(a,1)
!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
CALL ldlm(a,a,d)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
DO k=1,m; kp=k+1
   a(k,k)=one/d(k)
   DO i=kp,m
      a(i,k) = a(i,k) + SUM(a(kp:i-1,k)*a(i,kp:i-1)) ! really??
      a(i,k) =-a(i,k)
   ENDDO
ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
DO i=2,m
   a(1:i-1,i) = a(i,1:i-1) * a(i,i) ! Really?
ENDDO
DO i=1,m
   ip=i+1
   DO j=1,i-1
      a(j,i) = a(j,i) + SUM(a(ip:ip+m-i-1,i)*a(j,ip:ip+m-i-1))
      a(i,j) = a(j,i)
   ENDDO
   a(i,i) = a(i,i) + SUM(a(ip:ip+m-i-1,i)*a(i,ip:ip+m-i-1))
ENDDO
END SUBROUTINE invh


SUBROUTINE dinvh(a)
!$$$  subprogram documentation block
!                .      .    .    
! subprogram:    dinvh 
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1993 
!
! abstract:   Inver,t in place, a symmetric matrix
!
! limitation:  This routine incorporates no pivoting - it is intended for matrices
!              that are already diagonally dominant
!  
! program history log: 
!   2008-04-25  safford -- add subprogram doc block
! 
!   input argument list:
!     A          - symmetric square matrix, output as inverse of input
!
!   output argument list:
!     A          - symmetric square matrix, output as inverse of input
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:) 

INTEGER(i_kind)                  :: m,k, kp, i, ip, j
REAL(r_kind),DIMENSION(SIZE(a,1)):: d

m=SIZE(a,1)
!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
CALL ldlm_d(a,a,d)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
DO k=1,m
   kp=k+1
   a(k,k)=one/d(k)
   DO i=kp,m
      a(i,k) = a(i,k) + SUM(a(kp:i-1,k)*a(i,kp:i-1)) ! really??
      a(i,k) =-a(i,k)
   ENDDO
ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
DO i=2,m
   a(1:i-1,i) = a(i,1:i-1) * a(i,i) ! really?
ENDDO
DO i=1,m
   ip=i+1
   DO j=1,i-1
      a(j,i) = a(j,i) + SUM(a(ip:ip+m-i-1,i)*a(j,ip:ip+m-i-1))
      a(i,j) = a(j,i)
   ENDDO
   a(i,i) = a(i,i) + SUM(a(ip:ip+m-i-1,i)*a(i,ip:ip+m-i-1))
ENDDO
END SUBROUTINE dinvh


SUBROUTINE invl(a)
!$$$  subprogram documentation block
!                .      .    .    
! subprogram:    invl 
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:   Invert lower triangular matrix in place if A are same
!
! program history log: 
!   2008-04-25  safford -- add subprogram doc block
! 
!   input argument list:
!     a          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:) 

INTEGER(i_kind)             :: m,j, i
REAL(r_kind)                :: s

m=SIZE(a,1)
DO j=m,1,-1
   a(1:j-1,j) = zero
   a(j,j)=one/a(j,j)
   DO i=j+1,m
      s = SUM(a(j:i-1,j)*a(i,j:i-1))
      a(i,j)=-a(i,i)*s
   ENDDO
ENDDO
END SUBROUTINE invl


SUBROUTINE dinvl(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dinvl
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:   Invert lower triangular matrix in place if A are same
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:) 
INTEGER(i_kind)             :: m,j, i
REAL(r_kind)                :: s
m=SIZE(a,1)
DO j=m,1,-1
   a(1:j-1,j) = zero
   a(j,j)=one/a(j,j)
   DO i=j+1,m
      s = SUM(a(j:i-1,j)*a(i,j:i-1))
      a(i,j)=-a(i,i)*s
   ENDDO
ENDDO
END SUBROUTINE dinvl


SUBROUTINE linlv(a,u)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linvlv
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:   Solve linear system involving lower triangular (LINLV) or upper
!             triangular (LINUV) matrix. u is input as right-hand-side, output
!             as the solution vector.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - 
!     u          - 
!
!   output argument list:
!     u          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ):: a(:,:)
REAL(r_kind), INTENT(INOUT):: u(:)

INTEGER(i_kind)            :: m,i, j, jp

DO i=1,SIZE(a,1);    u(i)=(u(i) - SUM(u(1:i-1)*a(i,1:i-1)))/a(i,i); ENDDO
RETURN
ENTRY linuv(a,u); m=SIZE(a,1)
DO j=m,1,-1; jp=j+1; u(j)=(u(j) - SUM(a(jp:m,j)*u(jp:m)))  /a(j,j); ENDDO
END SUBROUTINE linlv


SUBROUTINE dlinlv(a,u)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dlinvlv
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:   Invert lower triangular matrix in place if A are same
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - 
!     u          - 
!
!   output argument list:
!     u          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)
REAL(r_kind), INTENT(INOUT) :: u(:)

INTEGER(i_kind) :: m,i, j, jp

DO i=1,SIZE(a,1); u(i)= (u(i) - SUM(u(1:i-1)*a(i,1:i-1)))/a(i,i); ENDDO
RETURN
ENTRY dlinuv(a,u); m=SIZE(a,1)
DO j=m,1,-1; jp=j+1; u(j) = (u(j) - SUM(a(jp:m,j)*u(jp:m)))/a(j,j); ENDDO
END SUBROUTINE dlinlv


SUBROUTINE powp(a,b,n) 
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    powp
!
!   prgrmmr: 
!
! abstract:  Raise power series A to the power 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -  power series
!     n          -  power to raise to
!
!   output argument list:
!     b          -  output power series
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: n       ! of N and output as B
REAL(r_kind),    INTENT(IN   ) :: a(0:)
REAL(r_kind),    INTENT(  OUT) :: b(0:)

REAL(r_kind),DIMENSION(0:SIZE(a)-1):: t; INTEGER(i_kind) :: k

b(0)=one; b(1:) = zero; DO k=1,n; CALL mulpp(a,b,t); b=t; ENDDO
END SUBROUTINE powp


SUBROUTINE DPOWP(A,B,N)        ! Raise power series A to the power
!$$$  subprogram documentation block
!                .      .    . 
! subprogram:    dpowp
!
!   prgrmmr:  
!
! abstract:  Raise power series A to the power
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -  power series
!     n          -  power to raise to
!
!   output argument list:
!     b          -  output power series
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: n      ! of N and output as B
REAL(r_kind)   , INTENT(IN   ) :: a(0:)
REAL(r_kind)   , INTENT(  OUT) :: b(0:)

REAL(r_kind),DIMENSION(0:SIZE(a)-1):: t; INTEGER(i_kind) :: k

B(0)=one; b(1:) = zero; DO k=1,n; CALL mulpp_d(a,b,t); b=t; ENDDO
END SUBROUTINE dpowp


SUBROUTINE polps(a,s1,s2) ! Apply series A to scalar S1 to obtain S2
!$$$  subprogram documentation block
!                .      .    . 
! subprogram:    polps
!
!   prgrmmr:  
!
! abstract:  Apply series A to scalar S1 to obtain S2
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -  
!     s1         - 
!
!   output argument list:
!     s2         -  
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN   ) :: a(0:)
REAL(r_kind),INTENT(IN   ) :: s1
REAL(r_kind),INTENT(  OUT) :: s2

INTEGER(i_kind) m,k

m=SIZE(a)-1; s2=a(m); DO k=m-1,0,-1; s2=s2*s1+a(k); ENDDO
END SUBROUTINE polps


SUBROUTINE dpolps(a,s1,s2) ! Apply series A to scalar S1 to obtain S2
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dpolps
!
!   prgrmmr:
!
! abstract:  Apply series A to scalar S1 to obtain S2
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -  
!     s1         -  
!
!   output argument list:
!     s2         -  
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN   ) :: a(0:)
REAL(r_kind),INTENT(IN   ) :: s1
REAL(r_kind),INTENT(  OUT) :: s2

INTEGER(i_kind) m,k

m=SIZE(a)-1; s2=a(m); DO k=m-1,0,-1; s2=s2*s1+a(k); ENDDO
END SUBROUTINE dpolps


SUBROUTINE polpp(a,b,c)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    polpp
!
!   prgrmmr:
!
! abstract:  Apply power series A to power series B and put
!            the result out as power-series C.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b,c      -  
!
!   output argument list:
!     a,b,c      -  
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(INOUT) :: a(0:),b(0:),c(0:)

REAL(r_kind),DIMENSION(0:SIZE(a)-1):: t
INTEGER(i_kind) m,k

m=SIZE(a)-1; c(0)=a(m); c(1:m) = zero
DO k=m-1,0,-1; CALL mulpp(b,c,t); c=t; c(0)=c(0)+a(k); ENDDO
END SUBROUTINE polpp


SUBROUTINE dpolpp(a,b,c)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dpolpp
!
!   prgrmmr:
!
! abstract:  Apply power series A to power series B and put
!            the result out as power-series C.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b,c      -
!
!   output argument list:
!     a,b,c      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(INOUT) :: a(0:),b(0:),c(0:)

REAL(r_kind),DIMENSION(0:SIZE(a)-1):: t
INTEGER(i_kind) m,k

m=SIZE(a)-1
c(0)=a(m); c(1:m) = zero
DO k=m-1,0,-1; CALL mulpp_d(b,c,t); c=t; c(0)=c(0)+a(k); ENDDO
END SUBROUTINE dpolpp


FUNCTION trcm(a) RESULT(trc_res)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    trcm
!
!   prgrmmr:
!
! abstract:  Trace of square matrix A
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a          -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)

REAL(r_kind)             :: trc_res
INTEGER(i_kind)          :: i

trc_res=zero; DO i=1,SIZE(a,1); trc_res=trc_res+a(i,i); ENDDO
END FUNCTION trcm

FUNCTION dtrcm(a) RESULT(trc_res)	    ! Trace of square matrix A
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dtrcm
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-26  lueken - added subprogram doc block
!
!   input argument list:
!    a
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)

REAL(r_kind)             :: trc_res
INTEGER(i_kind)          :: i

trc_res=zero; DO i=1,SIZE(a,1); trc_res=trc_res+a(i,i); ENDDO
END FUNCTION dtrcm


SUBROUTINE invmt(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    invmt
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a

INTEGER(i_kind) m,i,j,jp,l
REAL(r_kind) d
INTEGER(i_kind),DIMENSION(SIZE(a,1)):: ipiv

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to invmt is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
CALL ldum(a,ipiv,d)

! Invert upper triangular portion U in place:
DO i=1,m; a(i,i)=one/a(i,i); ENDDO
DO i=1,m-1
   DO j=i+1,m; a(i,j)=-a(j,j)*DOT_PRODUCT(a(i:j-1,j),a(i,i:j-1)); ENDDO
ENDDO

! Invert lower triangular portion L in place:
DO j=1,m-1; jp=j+1
   DO i=jp,m; a(i,j)=-a(i,j)-DOT_PRODUCT(a(jp:i-1,j),a(i,jp:i-1)); ENDDO
ENDDO

!  Form the product of U**-1 and L**-1 in place
DO j=1,m-1; jp=j+1
   DO i=1,j; a(i,j)=a(i,j)+DOT_PRODUCT(a(jp:m,j),a(i,jp:m)); ENDDO
   DO i=jp,m; a(i,j)=DOT_PRODUCT(a(i:m,j),a(i,i:m));         ENDDO
ENDDO

!  Permute columns according to ipiv
DO j=m-1,1,-1; l=ipiv(j); CALL swpvv(a(:,j),a(:,l)); ENDDO
END SUBROUTINE invmt


SUBROUTINE dinvmt(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dinvmt
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a

INTEGER(i_kind)                          :: m,i,j,jp,l
REAL(r_kind)                             :: d
INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to dinvmt is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
CALL ldum_d(a,ipiv,d)

! Invert upper triangular portion U in place:
DO i=1,m; a(i,i)=one/a(i,i); ENDDO
DO i=1,m-1
   DO j=i+1,m; a(i,j)=-a(j,j)*DOT_PRODUCT(a(i:j-1,j),a(i,i:j-1)); ENDDO
ENDDO

! Invert lower triangular portion L in place:
DO j=1,m-1; jp=j+1
   DO i=jp,m; a(i,j)=-a(i,j)-DOT_PRODUCT(a(jp:i-1,j),a(i,jp:i-1)); ENDDO
ENDDO

!  Form the product of U**-1 and L**-1 in place
DO j=1,m-1; jp=j+1
   DO i=1,j; a(i,j)=a(i,j)+DOT_PRODUCT(a(jp:m,j),a(i,jp:m)); ENDDO
   DO i=jp,m; a(i,j)=DOT_PRODUCT(a(i:m,j),a(i,i:m));         ENDDO
ENDDO

!  Permute columns according to ipiv
DO j=m-1,1,-1; l=ipiv(j); CALL swpvv_d(a(:,j),a(:,l)); ENDDO
END SUBROUTINE dinvmt


SUBROUTINE linmmt(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linmmt
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b        -
!
!   output argument list:
!     a,b        -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a,b

INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv
INTEGER(i_kind)                          :: m
REAL(r_kind)                             :: d

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmmt is not square'
IF(m /= SIZE(b,1))STOP 'matrix and vectors in linmmt have unmatched sizes'
CALL ldum(a,ipiv,d); CALL udlmm(a,b,ipiv)
END SUBROUTINE linmmt


SUBROUTINE dlinmmt(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dlinmmt
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b        -
!
!   output argument list:
!     a,b        -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a,b

INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv
INTEGER(i_kind)                          :: m 
REAL(r_kind)                             :: d

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmmt_d is not square'
IF(m /= SIZE(b,1))STOP 'matrix and vectors in linmmt_d have unmatched sizes'
CALL ldum_d(a,ipiv,d); CALL udlmm_d(a,b,ipiv)
END SUBROUTINE dlinmmt


SUBROUTINE linmvt(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linmvt
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b        -
!
!   output argument list:
!     a,b        -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a
REAL(r_kind),DIMENSION(:),  INTENT(INOUT) :: b

INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv
INTEGER(i_kind)                          :: m
REAL(r_kind)                             :: d

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmvt is not square'
IF(m /= SIZE(b))STOP 'matrix and vectors in linmvt have unmatched sizes'
CALL ldum(a,ipiv,d); CALL udlmm(a,b,ipiv)
END SUBROUTINE linmvt


SUBROUTINE dlinmvt(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dlinmvt
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b        -
!
!   output argument list:
!     a,b        -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a
REAL(r_kind),DIMENSION(:),  INTENT(INOUT) :: b

INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv
INTEGER(i_kind) m; REAL(r_kind) d

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmvt_d is not square'
IF(m /= SIZE(b))STOP 'matrix and vectors in linmvt_d have unmatched sizes'
CALL ldum_d(a,ipiv,d); CALL udlmm_d(a,b,ipiv)
END SUBROUTINE dlinmvt

end module m_plib8mat1
