module gfs_StateIO
!$$$ module documentation block
!                .      .    .                                       .
! module:   gfs_StateIO
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: State I/O wrapper for GFS gaussian grid ensemble members.
!           Analogous to geos_StateIO.F90.
!           Constructs filenames from the ensemble filename template
!           (ens_fname_tmpl) or ensemble_path, then calls m_read_gfsens
!           to read and scatter data.
!
! program history log:
!   2024-01-01  initial version following geos_StateIO.F90 structure
!
! subroutines included:
!   sub state_get  - read one or N ensemble members
!   sub state_put  - write one ensemble member (stub)
!
! attributes:
!   language: Fortran 90 and/or above
!
!$$$

use m_kinds, only: i_kind
use m_mpimod, only: mype, npe
use hybrid_ensemble_parameters, only: ens_fname_tmpl
use hybrid_ensemble_parameters, only: ensemble_path
use mpeu_util, only: strTemplate

use m_read_gfsens, only: gfs_readens

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

character(len=*), parameter :: myname = 'gfs_StateIO'

! Default fallback pattern when ens_fname_tmpl is not set.
! Use the standard GFS ensemble naming: sigf{FF}_ens_mem{NNN}.nc4
! where FF is zero-padded 2-digit forecast lead time and NNN is 3-digit member index.
character(len=*), parameter :: GFS_ENS_FALLBACK_FMT = '(a,a,i2.2,a,i3.3,a)'
character(len=*), parameter :: GFS_ENS_FALLBACK_MID = 'sigf'
character(len=*), parameter :: GFS_ENS_FALLBACK_SUF = '_ens_mem'
character(len=*), parameter :: GFS_ENS_FALLBACK_EXT = '.nc4'

contains

subroutine get_1state_(xx, sgrid, nymd, nhms, iwhat, tau)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_1state_   read a single GFS ensemble state (stub)
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Stub for single-member reading.  Ensemble members are read
!           via get_user_Nens which calls get_Nstate_.
!
!$$$
use gsi_bundlemod, only: gsi_bundle
use general_sub2grid_mod, only: sub2grid_info
implicit none
type(sub2grid_info), intent(in   ) :: sgrid
type(gsi_bundle),    intent(inout) :: xx
integer(i_kind),     intent(in   ) :: nymd, nhms
integer(i_kind),     intent(in   ) :: iwhat
integer(i_kind), optional, intent(in) :: tau

end subroutine get_1state_

subroutine get_Nstate_(xx, sgrid, nymd, nhms, tau)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_Nstate_   read all GFS ensemble members
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Read all GFS gaussian grid ensemble members and scatter to
!           all MPI tasks.  Constructs filenames from ens_fname_tmpl or
!           ensemble_path and calls gfs_readens.
!
!   input argument list:
!     sgrid  - sub2grid communication information
!     nymd   - date (YYYYMMDD)
!     nhms   - time (HHMMSS)
!     tau    - (optional) forecast lead time in hours
!
!   output argument list:
!     xx     - array of gsi_bundle (one per ensemble member)
!
!$$$
use gsi_bundlemod, only: gsi_bundle
use general_sub2grid_mod, only: sub2grid_info
implicit none
type(sub2grid_info), intent(in   ) :: sgrid
type(gsi_bundle),    intent(inout) :: xx(:)
integer(i_kind),     intent(in   ) :: nymd, nhms
integer(i_kind), optional, intent(in) :: tau

character(len=*), parameter :: myname_ = myname//'::get_Nstate_'
integer :: ii, istatus, tau_
character(len=255), allocatable :: fnames(:)

! tau_ is the forecast lead time in hours (default 0 if not provided).
! Used in the fallback filename as the forecast hour (e.g., sigf06_ens_mem001.nc4).
tau_ = 0
if (present(tau)) tau_ = tau

allocate(fnames(size(xx)))
do ii = 1, size(xx)
   if (len_trim(ens_fname_tmpl) > 0 .and. trim(ens_fname_tmpl) /= 'NULL') then
      call strTemplate(fnames(ii), ens_fname_tmpl, nymd=nymd, nhms=nhms, ens=ii, stat=istatus)
   else
   ! Fall back to ensemble_path with standard GFS ensemble naming convention:
   ! {ensemble_path}sigf{FF}_ens_mem{NNN}.nc4
   ! where FF is the 2-digit forecast lead time (tau, in hours) and NNN is the
   ! 3-digit member index.  Set ens_fname_tmpl in the HYBRID_ENSEMBLE namelist
   ! to override this default.
   write(fnames(ii), GFS_ENS_FALLBACK_FMT) trim(adjustl(ensemble_path)), &
        GFS_ENS_FALLBACK_MID, tau_, GFS_ENS_FALLBACK_SUF, ii, GFS_ENS_FALLBACK_EXT
   endif
enddo

call gfs_readens(sgrid, xx, fnames, npe, mype, 0)
deallocate(fnames)

end subroutine get_Nstate_

subroutine state_put_(sgrid, xx, nymd, nhms, member)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    state_put_   write a GFS ensemble state (stub)
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Stub for writing a GFS ensemble member.
!           Currently not implemented.
!
!$$$
use gsi_bundlemod, only: gsi_bundle
use general_sub2grid_mod, only: sub2grid_info
implicit none
type(sub2grid_info), intent(in   ) :: sgrid
type(gsi_bundle),    intent(inout) :: xx
integer(i_kind),     intent(in   ) :: nymd, nhms
integer(i_kind),     intent(in   ) :: member

character(len=*), parameter :: myname_ = myname//'::state_put_'
if (mype == 0) write(6,'(a)') myname_//': GFS ensemble write not yet implemented'

end subroutine state_put_

end module gfs_StateIO
