module jfunc
use m_kinds, only: i_kind
implicit none
public :: jfunc_init

public :: mockbkg

public :: jiter
public :: nsclen
public :: npclen
public :: ntclen
public :: nval_lenz
public :: ljc4tlevs
public :: iadate

integer(i_kind),dimension(5):: iadate
integer(i_kind) :: jiter
integer(i_kind) :: nsclen
integer(i_kind) :: npclen
integer(i_kind) :: ntclen
integer(i_kind) :: nval_lenz
logical :: mockbkg
logical :: ljc4tlevs
contains
subroutine jfunc_init
 mockbkg=.true. ! fake background state (internally generated)
 jiter=1        ! used as index for output spread - wired for now
 nsclen=0
 npclen=0
 ntclen=0
 nval_lenz=0
 ljc4tlevs=.false.  ! used to be in jcmod
 iadate=0           ! used to be in obsmod
end subroutine jfunc_init
end module jfunc
