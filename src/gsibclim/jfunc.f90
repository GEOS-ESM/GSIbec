module jfunc
use m_kinds, only: i_kind
implicit none
public :: jfunc_init

public :: nsclen
public :: npclen
public :: ntclen
public :: nsubwin
public :: qoption
public :: cwoption
public :: clip_supersaturation

integer(i_kind) :: nsclen
integer(i_kind) :: npclen
integer(i_kind) :: ntclen
integer(i_kind) :: nsubwin
integer(i_kind) :: qoption
integer(i_kind) :: cwoption
logical :: pseudo_q2
logical :: clip_supersaturation
contains
subroutine jfunc_init
 nsubwin=1
 nsclen=0
 npclen=0
 ntclen=0
 qoption=1
 cwoption=0
 pseudo_q2=.false.
 clip_supersaturation=.false.
end subroutine jfunc_init
end module jfunc
