module jfunc
use m_kinds, only: i_kind
use guess_grids, only:  switch_on_derivatives,tendsflag,clip_supersaturation
implicit none
public :: jfunc_init

public :: mockbkg

public :: jiter
public :: jiterstart
public :: nsclen
public :: npclen
public :: ntclen
public :: nval_lenz
public :: ljc4tlevs
public :: iadate
public :: qoption
public :: cwoption
public :: pseudo_q2

integer(i_kind),dimension(5):: iadate
integer(i_kind) :: jiter
integer(i_kind) :: jiterstart
integer(i_kind) :: nsclen
integer(i_kind) :: npclen
integer(i_kind) :: ntclen
integer(i_kind) :: nval_lenz
integer(i_kind) :: qoption
integer(i_kind) :: cwoption
logical :: mockbkg
logical :: ljc4tlevs
logical :: pseudo_q2
contains
subroutine jfunc_init
 mockbkg=.true. ! fake background state (internally generated)
 jiter=1        ! used as index for output spread - wired for now
 jiterstart=1   ! used as index for output spread - wired for now
 nsclen=0
 npclen=0
 ntclen=0
 nval_lenz=0
 ljc4tlevs=.false.  ! used to be in jcmod
 iadate=0           ! used to be in obsmod
 switch_on_derivatives=.false.
 tendsflag=.false.

 qoption=1
 cwoption=0
 pseudo_q2=.false.
 clip_supersaturation = .false.

end subroutine jfunc_init
end module jfunc
