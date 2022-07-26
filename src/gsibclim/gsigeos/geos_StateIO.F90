module geos_StateIO
use m_kinds, only: i_kind
implicit none
private
public :: state_get
public :: state_put

interface state_get
   module procedure get_1state_
   module procedure get_Nstate_
end interface
interface state_put
   module procedure state_put_
end interface
contains
subroutine get_1state_(xx,sgrid,nymd,nhms,iwhat,tau)
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
use general_sub2grid_mod, only: sub2grid_info
implicit none
type(gsi_bundle),   intent(inout) :: xx
type(sub2grid_info),intent(in   ) :: sgrid ! internal subdomain grid
integer(i_kind),    intent(in   ) :: nymd,nhms
integer(i_kind),    intent(in   ) :: iwhat
integer(i_kind),optional,intent(in ) :: tau   ! time interval in hours
end subroutine get_1state_
subroutine get_Nstate_(xx,sgrid,nymd,nhms,tau)
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
use general_sub2grid_mod, only: sub2grid_info
implicit none
type(gsi_bundle),   intent(inout) :: xx(:)
type(sub2grid_info),intent(in   ) :: sgrid ! internal subdomain grid
integer(i_kind),    intent(in   ) :: nymd,nhms
integer(i_kind),optional,intent(in ) :: tau   ! time interval in hours
end subroutine get_Nstate_
subroutine state_put_(xx,sgrid,nymd,nhms,member)
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
use general_sub2grid_mod, only: sub2grid_info
implicit none
type(gsi_bundle),   intent(inout) :: xx
type(sub2grid_info),intent(in   ) :: sgrid ! internal subdomain grid
integer(i_kind),    intent(in   ) :: nymd,nhms
integer(i_kind),    intent(in   ) :: member
end subroutine state_put_
end module geos_StateIO
