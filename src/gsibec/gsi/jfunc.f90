module jfunc
use m_kinds, only: i_kind
implicit none
public :: jfunc_init

public :: mockbkg

public :: jiter
public :: jiterstart
public :: npred
public :: npredp
public :: npcptype
public :: nsclen
public :: npclen
public :: ntclen
public :: nval_lenz
public :: nclen
public :: nclenz
public :: jpch_rad
public :: ljc4tlevs
public :: iadate
public :: qoption
public :: cwoption
public :: pseudo_q2
public :: switch_on_derivatives
public :: tendsflag
public :: clip_supersaturation

integer(i_kind),dimension(5):: iadate
integer(i_kind) :: jiter
integer(i_kind) :: jiterstart
integer(i_kind) :: npred
integer(i_kind) :: npredp
integer(i_kind) :: npcptype
integer(i_kind) :: nsclen
integer(i_kind) :: npclen
integer(i_kind) :: ntclen
integer(i_kind) :: nval_lenz
integer(i_kind) :: nclen
integer(i_kind) :: nclenz
integer(i_kind) :: jpch_rad
integer(i_kind) :: qoption
integer(i_kind) :: cwoption
logical :: mockbkg
logical :: ljc4tlevs
logical :: pseudo_q2
logical :: switch_on_derivatives
logical :: tendsflag
logical :: clip_supersaturation
contains
subroutine jfunc_init
 mockbkg=.true. ! fake background state (internally generated)
 jiter=1        ! used as index for output spread - wired for now
 jiterstart=1   ! used as index for output spread - wired for now
 npred=0
 npredp=0
 npcptype=0
 nsclen=0
 npclen=0
 ntclen=0
 nval_lenz=0
 nclen=0
 nclenz=0
 jpch_rad=0
 ljc4tlevs=.false.  ! used to be in jcmod
 iadate(1) = 1776   ! year, default: should be replaced in each pass
 iadate(2) = 07     ! month default: should be replaced in each pass
 iadate(3) = 04     ! day,  default: should be replaced in each pass
 iadate(4) =  0     ! hour, default: should be replaced in each pass
 iadate(5) =  0     ! sec,  default: should be replaced in each pass
 switch_on_derivatives=.false.
 tendsflag=.false.

 qoption=1
 cwoption=0
 pseudo_q2=.false.
 clip_supersaturation = .false.

end subroutine jfunc_init
end module jfunc
