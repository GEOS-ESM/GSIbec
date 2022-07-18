module jfunc
use m_kinds, only: i_kind
implicit none
public :: jfunc_init

public :: jiter
public :: nsclen
public :: npclen
public :: ntclen
public :: nval_lenz
public :: qoption
public :: cwoption
public :: clip_supersaturation
public :: ljc4tlevs
public :: iadate

integer(i_kind),dimension(5):: iadate
integer(i_kind) :: jiter
integer(i_kind) :: nsclen
integer(i_kind) :: npclen
integer(i_kind) :: ntclen
integer(i_kind) :: nval_lenz
integer(i_kind) :: qoption
integer(i_kind) :: cwoption
logical :: pseudo_q2
logical :: clip_supersaturation
logical :: ljc4tlevs
contains
subroutine jfunc_init
 jiter=1  ! used as index for output spread - wired for now
 nsclen=0
 npclen=0
 ntclen=0
 nval_lenz=0
 qoption=1
 cwoption=0
 pseudo_q2=.false.
 clip_supersaturation=.false.
 ljc4tlevs=.false.  ! used to be in jcmod
 iadate=0           ! used to be in obsmod
end subroutine jfunc_init
end module jfunc
