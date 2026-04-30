module cplr_gfs_ensemble
!$$$ module documentation block
!                .      .    .                                       .
! module:   cplr_gfs_ensemble
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: GFS gaussian grid ensemble coupler for GSIbec.
!           Analogous to cplr_ensemble.F90 (for GEOS).
!           Extends stub_ensmod::ensemble and overrides the get/put methods
!           to read GFS gaussian grid ensemble members from NetCDF4 files
!           via gfs_StateIO.
!
! program history log:
!   2024-01-01  initial version following cplr_ensemble.F90 structure
!
! attributes:
!   language: Fortran 90 and/or above
!
!$$$

    use stub_ensmod, only: stubEnsemble => ensemble

    implicit none
    private
    public :: ensemble
    public :: ensemble_typemold

    type, extends(stubEnsemble) :: ensemble
      private
      contains
      procedure :: get_user_ens  => get_gfs_ens
      procedure :: get_user_Nens => get_gfs_Nens
      procedure :: put_user_ens  => put_gfs_ens
      procedure, nopass :: mytype => typename
    end type ensemble

    character(len=*), parameter :: myname = 'gfs_ensmod'

    type(ensemble), target :: mold_

contains

function ensemble_typemold()
  implicit none
  type(ensemble), pointer :: ensemble_typemold
  ensemble_typemold => mold_
end function ensemble_typemold

function typename()
  implicit none
  character(len=:), allocatable :: typename
  typename = '['//myname//'::ensemble]'
end function typename

subroutine get_gfs_ens(this, grd, member, nymd, nhms, tau, atm_bundle, iret)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gfs_ens   read one GFS ensemble member
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Read a single GFS gaussian grid ensemble member from a NetCDF4
!           file and place the fields into atm_bundle.
!           Based on get_geos_ens from cplr_ensemble.F90.
!
!   input argument list:
!     grd    - sub2grid communication information
!     member - ensemble member index
!     nymd   - date (YYYYMMDD)
!     nhms   - time (HHMMSS)
!     tau    - forecast lead time (hours)
!
!   output argument list:
!     atm_bundle - gsi_bundle with ensemble member fields
!     iret       - return code (0 = success)
!
!$$$
use m_mpimod, only: mype
use m_kinds, only: i_kind, r_kind
use general_sub2grid_mod, only: sub2grid_info
use gsi_bundlemod, only: gsi_grid
use gsi_bundlemod, only: gsi_gridcreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: gsi_bundlegetpointer
use gfs_StateIO, only: State_get
use hybrid_ensemble_parameters, only: uv_hyb_ens
use control_vectors, only: cvars2d, cvars3d, nc2d, nc3d
use gridmod, only: nlat, nlon
implicit none

class(ensemble),     intent(inout) :: this
type(sub2grid_info), intent(in   ) :: grd
integer(i_kind),     intent(in   ) :: member
integer(i_kind),     intent(in   ) :: nymd, nhms
integer(i_kind),     intent(in   ) :: tau
integer(i_kind),     intent(  out) :: iret
type(gsi_bundle),    intent(inout) :: atm_bundle

character(len=*), parameter :: myname_ = myname//'::get_gfs_ens'
character(len=40) :: evar
integer(i_kind) :: istatus, ii, ier
logical, save :: first = .true.
real(r_kind), pointer, dimension(:,:  ) :: iptr2d
real(r_kind), pointer, dimension(:,:,:) :: iptr3d
real(r_kind), pointer, dimension(:,:  ) :: optr2d
real(r_kind), pointer, dimension(:,:,:) :: optr3d
type(gsi_grid)   :: grid
type(gsi_bundle) :: flds

associate( this => this ) ! eliminates warning for unused dummy argument
end associate

! Only works for non-dual resolution
if (grd%nlat /= nlat .or. grd%nlon /= nlon) then
   if (first) then
      if (mype == 0) then
         write(6,'(a,a)') myname_, ': grd%(nlat,nlon) = '
         write(6,'(a,2i6)') myname_//': ', grd%nlat, grd%nlon
         write(6,'(a,a)') myname_, ': ges%(nlat,nlon) = '
         write(6,'(a,2i6)') myname_//': ', nlat, nlon
         write(6,'(a)') myname_//': dual resolution hybrid analysis in use'
      endif
      first = .false.
   endif
endif

! Ensure u,v are part of the ensemble
if (.not. uv_hyb_ens) then
   if (mype == 0) then
      write(6,'(a)') myname_//': must have uv as part of ensemble members'
      write(6,'(a)') myname_//': set uv_hyb_ens to true'
   endif
   call stop2(999)
endif

! Create temporary bundle to hold input fields
call gsi_gridcreate(grid, grd%lat2, grd%lon2, grd%nsig)
call gsi_bundlecreate(flds, grid, 'gfs ensemble member', istatus, &
                      names2d=cvars2d, names3d=cvars3d)
if (istatus /= 0) then
   write(6,'(a)') myname_//': trouble creating temporary bundle'
   call stop2(999)
endif

! Read one ensemble member from file
call state_get(flds, grd, nymd, nhms, member, tau=tau)

! Copy 2d fields from flds to atm_bundle
do ii = 1, nc2d
   evar = trim(cvars2d(ii))
   call gsi_bundlegetpointer(flds,       evar, iptr2d, istatus)
   call gsi_bundlegetpointer(atm_bundle, evar, optr2d, ier)
   if (istatus == 0 .and. ier == 0) then
      optr2d = iptr2d
   else
      if (ier /= 0 .and. mype == 0) &
         write(6,'(a,a,a)') myname_, ': 2d field ', trim(evar)//' not in ens CV, skipping'
      if (istatus /= 0 .and. mype == 0) &
         write(6,'(a,a,a)') myname_, ': 2d field ', trim(evar)//' not in member file, skipping'
   endif
enddo

! Copy 3d fields from flds to atm_bundle
do ii = 1, nc3d
   evar = trim(cvars3d(ii))
   call gsi_bundlegetpointer(atm_bundle, evar, optr3d, ier)
   call gsi_bundlegetpointer(flds,       evar, iptr3d, istatus)
   if (istatus == 0 .and. ier == 0) then
      optr3d = iptr3d
   else
      if (ier /= 0 .and. mype == 0) &
         write(6,'(a,a,a)') myname_, ': 3d field ', trim(evar)//' not in ens CV, skipping'
      if (istatus /= 0 .and. mype == 0) &
         write(6,'(a,a,a)') myname_, ': 3d field ', trim(evar)//' not in member file, skipping'
   endif
enddo
iret = 0  ! Fields not found in ens CV or member file are skipped (warned above);
          ! follow GEOS pattern of not treating missing optional fields as fatal.

! Clean up
call gsi_bundledestroy(flds)

end subroutine get_gfs_ens

subroutine get_gfs_Nens(this, grd, members, nymd, nhms, tau, atm_bundle, iret)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_gfs_Nens   read all GFS ensemble members
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Read all GFS gaussian grid ensemble members at once from
!           NetCDF4 files and scatter fields to all MPI tasks.
!           Based on get_geos_Nens from cplr_ensemble.F90.
!
!   input argument list:
!     grd     - sub2grid communication information
!     members - number of ensemble members
!     nymd    - date (YYYYMMDD)
!     nhms    - time (HHMMSS)
!     tau     - forecast lead time (hours)
!
!   output argument list:
!     atm_bundle - array of gsi_bundle (one per member)
!     iret       - return code (0 = success)
!
!$$$
use m_mpimod, only: mype
use m_kinds, only: i_kind, r_kind
use general_sub2grid_mod, only: sub2grid_info
use gsi_bundlemod, only: gsi_grid
use gsi_bundlemod, only: gsi_gridcreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: gsi_bundlegetpointer
use gfs_StateIO, only: State_get
use hybrid_ensemble_parameters, only: uv_hyb_ens
use control_vectors, only: cvars2d, cvars3d, nc2d, nc3d
use gridmod, only: nlat, nlon
implicit none

class(ensemble),     intent(inout) :: this
type(sub2grid_info), intent(in   ) :: grd
integer(i_kind),     intent(in   ) :: members
integer(i_kind),     intent(in   ) :: nymd, nhms
integer(i_kind),     intent(in   ) :: tau
integer(i_kind),     intent(  out) :: iret
type(gsi_bundle),    intent(inout) :: atm_bundle(:)

character(len=*), parameter :: myname_ = myname//'::get_gfs_Nens'
character(len=40) :: evar
integer(i_kind) :: istatus, ii, ier, mm
logical, save :: first = .true.
real(r_kind), pointer, dimension(:,:  ) :: iptr2d
real(r_kind), pointer, dimension(:,:,:) :: iptr3d
real(r_kind), pointer, dimension(:,:  ) :: optr2d
real(r_kind), pointer, dimension(:,:,:) :: optr3d
type(gsi_grid)                  :: grid
type(gsi_bundle), allocatable   :: flds(:)

associate( this => this ) ! eliminates warning for unused dummy argument
end associate

! Only works for non-dual resolution
if (grd%nlat /= nlat .or. grd%nlon /= nlon) then
   if (first) then
      if (mype == 0) then
         write(6,'(a,2i6)') myname_//': grd%(nlat,nlon) = ', grd%nlat, grd%nlon
         write(6,'(a,2i6)') myname_//': ges%(nlat,nlon) = ', nlat, nlon
         write(6,'(a)') myname_//': dual resolution hybrid analysis in use'
      endif
      first = .false.
   endif
endif

! Ensure u,v are part of the ensemble
if (.not. uv_hyb_ens) then
   if (mype == 0) then
      write(6,'(a)') myname_//': must have uv as part of ensemble members'
      write(6,'(a)') myname_//': set uv_hyb_ens to true'
   endif
   call stop2(999)
endif

! Create temporary bundles
call gsi_gridcreate(grid, grd%lat2, grd%lon2, grd%nsig)
allocate(flds(members))
do mm = 1, members
   call gsi_bundlecreate(flds(mm), grid, 'gfs ensemble member', istatus, &
                         names2d=cvars2d, names3d=cvars3d)
   if (istatus /= 0) then
      if (mype == 0) write(6,'(a)') myname_//': trouble creating temporary bundle'
      call stop2(999)
   endif
enddo

! Read all members at once
call state_get(flds, grd, nymd, nhms, tau=tau)

! Copy fields from flds to atm_bundle for each member
do mm = 1, members
   do ii = 1, nc2d
      evar = trim(cvars2d(ii))
      call gsi_bundlegetpointer(flds(mm),       evar, iptr2d, istatus)
      call gsi_bundlegetpointer(atm_bundle(mm), evar, optr2d, ier)
      if (istatus == 0 .and. ier == 0) then
         optr2d = iptr2d
      else
         if (ier /= 0 .and. mype == 0) &
            write(6,'(a,a)') myname_//': 2d field not in ens CV: ', trim(evar)
         if (istatus /= 0 .and. mype == 0) &
            write(6,'(a,a)') myname_//': 2d field not in member file: ', trim(evar)
      endif
   enddo
   do ii = 1, nc3d
      evar = trim(cvars3d(ii))
      call gsi_bundlegetpointer(atm_bundle(mm), evar, optr3d, ier)
      call gsi_bundlegetpointer(flds(mm),       evar, iptr3d, istatus)
      if (istatus == 0 .and. ier == 0) then
         optr3d = iptr3d
      else
         if (ier /= 0 .and. mype == 0) &
            write(6,'(a,a)') myname_//': 3d field not in ens CV: ', trim(evar)
         if (istatus /= 0 .and. mype == 0) &
            write(6,'(a,a)') myname_//': 3d field not in member file: ', trim(evar)
      endif
   enddo
enddo
iret = 0  ! Fields not found in ens CV or member file are skipped (warned above);
          ! follow GEOS pattern of not treating missing optional fields as fatal.

! Clean up
do mm = members, 1, -1
   call gsi_bundledestroy(flds(mm))
enddo
deallocate(flds)

end subroutine get_gfs_Nens

subroutine put_gfs_ens(this, grd, member, ntindex, pert, iret)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    put_gfs_ens   write a GFS ensemble perturbation (stub)
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Stub for writing a GFS ensemble perturbation.
!           Currently not implemented; returns iret=0.
!
!$$$
use m_mpimod, only: mype
use m_kinds, only: i_kind
use general_sub2grid_mod, only: sub2grid_info
use gsi_bundlemod, only: gsi_bundle
implicit none

class(ensemble),     intent(inout) :: this
type(sub2grid_info), intent(in   ) :: grd
integer(i_kind),     intent(in   ) :: member
integer(i_kind),     intent(in   ) :: ntindex
type(gsi_bundle),    intent(inout) :: pert
integer(i_kind),     intent(  out) :: iret

associate( this => this ) ! eliminates warning for unused dummy argument
end associate
if (mype == 0) write(6,'(a)') &
     myname//'::put_gfs_ens: GFS ensemble write not yet implemented'
iret = 0

end subroutine put_gfs_ens

end module cplr_gfs_ensemble
