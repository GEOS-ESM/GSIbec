module m_nc_GFSens
!$$$ module documentation block
!                .      .    .                                       .
! module:   m_nc_GFSens
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: NetCDF4 I/O routines for GFS gaussian grid ensemble files.
!           Analogous to m_nc_GEOSens.f90 but adapted for GFS conventions.
!
! program history log:
!   2024-01-01  initial version following m_nc_GEOSens.f90 structure
!
! subroutines included:
!   sub nc_GFSens_dims       - read file dimensions
!   sub nc_GFSens_read       - read GFS ensemble member from NetCDF4 file
!   sub nc_GFSens_vars_set   - initialize nc_GFSens_vars type from CV names
!   sub nc_GFSens_vars_init  - allocate arrays in nc_GFSens_vars
!   sub nc_GFSens_vars_final - deallocate arrays in nc_GFSens_vars
!   sub nc_GFSens_summary    - print summary of nc_GFSens_vars
!   sub nc_GFSens_gfs2gsi    - convert from GFS to GSI units/orientation
!
! attributes:
!   language: Fortran 90 and/or above
!
!$$$

use netcdf
use mpeu_util, only: getindex
implicit none
private

public :: nc_GFSens_vars_set
public :: nc_GFSens_vars_init
public :: nc_GFSens_vars_final
public :: nc_GFSens_vars_comp
public :: nc_GFSens_vars
public :: nc_GFSens_dims
public :: nc_GFSens_read
public :: nc_GFSens_summary
public :: nc_GFSens_gfs2gsi
public :: nc_GFSens_getpointer

! This type stores GFS ensemble data for one member.
! Arrays are stored in GSI orientation (nlat, nlon, [nsig,] nvar)
! when gsiset=.true., else (nlon, nlat, [nsig,] nvar).
type nc_GFSens_vars
   logical :: initialized = .false.
   integer :: nlon = 0, nlat = 0, nsig = 0
   logical :: gsiset = .false.
   real(4), pointer, dimension(:,:,:,:) :: ptr3d => null()  ! (nlat,nlon,nsig,nv3d) if gsiset
   real(4), pointer, dimension(:,:,:)   :: ptr2d => null()  ! (nlat,nlon,nv2d) if gsiset
   integer :: nv2d = -1
   integer :: nv3d = -1
   character(len=8), allocatable :: gsi_vnames2d(:)  ! GSI control variable names (2d)
   character(len=8), allocatable :: gsi_vnames3d(:)  ! GSI control variable names (3d)
end type nc_GFSens_vars

character(len=*), parameter :: myname = 'm_nc_GFSens'

! Unit conversion: surface pressure Pa -> centibars (cb)
! 1 centibar = 0.01 bar = 1000 Pa = 1 kPa = 10 hPa
! (not to be confused with millibar: 1 mb = 100 Pa = 1 hPa)
! GSI uses centibars for surface pressure; GFS NetCDF4 files use Pa.
real, parameter :: Pa_to_cb = 1.0e-3  ! Pa to centibars: 1 cb = 1000 Pa

interface nc_GFSens_dims;      module procedure read_dims_;         end interface
interface nc_GFSens_read;      module procedure read_GFSens_;       end interface
interface nc_GFSens_vars_set;  module procedure set_vars_;          end interface
interface nc_GFSens_vars_init; module procedure init_GFSens_vars_;  end interface
interface nc_GFSens_vars_final;module procedure final_GFSens_vars_; end interface
interface nc_GFSens_vars_comp; module procedure comp_GFSens_vars_;  end interface
interface nc_GFSens_summary;   module procedure summary_;           end interface
interface nc_GFSens_gfs2gsi;   module procedure gfs2gsi_;           end interface
interface nc_GFSens_getpointer
   module procedure get_pointer_2d_
   module procedure get_pointer_3d_
end interface

contains

!---------------------------------------------------------------------------
! Return the GFS file variable name corresponding to a GSI control variable name.
! GFS NetCDF4 gaussian grid ensemble files use standard NCEP variable names.
function gfs_varname_(gsiname) result(gfsname)
   character(len=*), intent(in) :: gsiname
   character(len=32) :: gfsname
   select case(trim(adjustl(gsiname)))
   case('sf')   ; gfsname = 'ugrd'    ! zonal wind -> stream function slot (uv_hyb_ens)
   case('vp')   ; gfsname = 'vgrd'    ! meridional wind -> vel. potential slot (uv_hyb_ens)
   case('u')    ; gfsname = 'ugrd'
   case('v')    ; gfsname = 'vgrd'
   case('t')    ; gfsname = 'tmp'     ! temperature
   case('tv')   ; gfsname = 'tmp'     ! virtual temperature (file has T, GSI converts)
   case('q')    ; gfsname = 'spfh'    ! specific humidity
   case('oz')   ; gfsname = 'o3mr'    ! ozone mixing ratio
   case('cw')   ; gfsname = 'clwmr'  ! cloud liquid water
   case('ql')   ; gfsname = 'clwmr'  ! cloud liquid water
   case('qi')   ; gfsname = 'icmr'   ! cloud ice
   case('qr')   ; gfsname = 'rwmr'   ! rain water
   case('qs')   ; gfsname = 'snmr'   ! snow
   case('qg')   ; gfsname = 'grle'   ! graupel
   case('ps')   ; gfsname = 'pressfc' ! surface pressure
   case default ; gfsname = trim(gsiname)
   end select
end function gfs_varname_

!---------------------------------------------------------------------------
subroutine set_vars_(cvars2d, cvars3d, bvars)
   implicit none
   character(*), intent(in)  :: cvars2d(:)  ! GSI 2d control variable names
   character(*), intent(in)  :: cvars3d(:)  ! GSI 3d control variable names
   type(nc_GFSens_vars), intent(inout) :: bvars

   bvars%nv2d = size(cvars2d)
   bvars%nv3d = size(cvars3d)
   if (bvars%nv2d > 0) then
      if (.not. allocated(bvars%gsi_vnames2d)) &
           allocate(bvars%gsi_vnames2d(bvars%nv2d))
      bvars%gsi_vnames2d = cvars2d
   endif
   if (bvars%nv3d > 0) then
      if (.not. allocated(bvars%gsi_vnames3d)) &
           allocate(bvars%gsi_vnames3d(bvars%nv3d))
      bvars%gsi_vnames3d = cvars3d
   endif
end subroutine set_vars_

!---------------------------------------------------------------------------
subroutine init_GFSens_vars_(bvars, nlon, nlat, nsig, gsi)
   implicit none
   type(nc_GFSens_vars), intent(inout) :: bvars
   integer, intent(in) :: nlon, nlat, nsig
   logical, intent(in), optional :: gsi

   bvars%nlon = nlon
   bvars%nlat = nlat
   bvars%nsig = nsig
   bvars%initialized = .true.

   if (present(gsi)) then
      bvars%gsiset = gsi
   else
      bvars%gsiset = .false.
   endif

   if (bvars%nv2d > 0) then
      if (bvars%gsiset) then
         allocate(bvars%ptr2d(nlat, nlon, bvars%nv2d))
      else
         allocate(bvars%ptr2d(nlon, nlat, bvars%nv2d))
      endif
   endif
   if (bvars%nv3d > 0) then
      if (bvars%gsiset) then
         allocate(bvars%ptr3d(nlat, nlon, nsig, bvars%nv3d))
      else
         allocate(bvars%ptr3d(nlon, nlat, nsig, bvars%nv3d))
      endif
   endif
end subroutine init_GFSens_vars_

!---------------------------------------------------------------------------
subroutine final_GFSens_vars_(bvars)
   implicit none
   type(nc_GFSens_vars), intent(inout) :: bvars

   ! Deallocate data arrays and reset initialized flag.
   ! NOTE: gsi_vnames2d/gsi_vnames3d are preserved for reuse on next call.
   bvars%initialized = .false.
   if (bvars%nv2d > 0 .and. associated(bvars%ptr2d)) then
      deallocate(bvars%ptr2d)
      nullify(bvars%ptr2d)
   endif
   if (bvars%nv3d > 0 .and. associated(bvars%ptr3d)) then
      deallocate(bvars%ptr3d)
      nullify(bvars%ptr3d)
   endif
end subroutine final_GFSens_vars_

!---------------------------------------------------------------------------
subroutine comp_GFSens_vars_(avars, bvars, verbose)
   implicit none
   type(nc_GFSens_vars), intent(in) :: avars, bvars
   logical, intent(in), optional :: verbose
   logical :: verbose_
   verbose_ = .false.
   if (present(verbose)) verbose_ = verbose
   if (avars%nlon /= bvars%nlon .or. avars%nlat /= bvars%nlat .or. &
       avars%nsig /= bvars%nsig) then
      if (verbose_) print *, myname, ': fields are inconsistent'
   else
      if (verbose_) print *, myname, ': fields match'
   endif
end subroutine comp_GFSens_vars_

!---------------------------------------------------------------------------
subroutine summary_(bvars)
   implicit none
   type(nc_GFSens_vars), intent(in) :: bvars
   integer :: nv
   print *, myname, ': nlon,nlat,nsig = ', bvars%nlon, bvars%nlat, bvars%nsig
   print *, myname, ': nv2d,nv3d = ', bvars%nv2d, bvars%nv3d
   print *, myname, ': gsiset = ', bvars%gsiset
   if (allocated(bvars%gsi_vnames2d)) then
      do nv = 1, bvars%nv2d
         print *, myname, ': 2d var ', nv, ' = ', trim(bvars%gsi_vnames2d(nv))
      enddo
   endif
   if (allocated(bvars%gsi_vnames3d)) then
      do nv = 1, bvars%nv3d
         print *, myname, ': 3d var ', nv, ' = ', trim(bvars%gsi_vnames3d(nv))
      enddo
   endif
end subroutine summary_

!---------------------------------------------------------------------------
subroutine read_dims_(fname, nlat, nlon, nlev, rc, myid, root)
   implicit none
   character(len=*), intent(in)  :: fname
   integer, intent(out) :: nlat, nlon, nlev, rc
   integer, intent(in), optional :: myid, root

   integer :: ncid, varid, ier
   integer :: mype_, root_
   character(len=*), parameter :: myname_ = myname//'::read_dims_'

   rc = 0; mype_ = 0; root_ = 0
   if (present(myid) .and. present(root)) then
      mype_ = myid
      root_ = root
   endif

   call check_(nf90_open(fname, NF90_NOWRITE, ncid), rc, mype_, root_)
   if (rc /= 0) return

   ! Try various dimension naming conventions
   ! Attempt 'lon' first (GEOS convention), then 'grid_xt' (FV3GFS convention)
   nlon = 0
   ier = nf90_inq_dimid(ncid, 'lon', varid)
   if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlon), rc, mype_, root_)
   if (nlon == 0) then
      ier = nf90_inq_dimid(ncid, 'grid_xt', varid)
      if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlon), rc, mype_, root_)
   endif
   if (nlon == 0) then
      ier = nf90_inq_dimid(ncid, 'longitude', varid)
      if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlon), rc, mype_, root_)
   endif

   nlat = 0
   ier = nf90_inq_dimid(ncid, 'lat', varid)
   if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlat), rc, mype_, root_)
   if (nlat == 0) then
      ier = nf90_inq_dimid(ncid, 'grid_yt', varid)
      if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlat), rc, mype_, root_)
   endif
   if (nlat == 0) then
      ier = nf90_inq_dimid(ncid, 'latitude', varid)
      if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlat), rc, mype_, root_)
   endif

   nlev = 0
   ier = nf90_inq_dimid(ncid, 'lev', varid)
   if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlev), rc, mype_, root_)
   if (nlev == 0) then
      ier = nf90_inq_dimid(ncid, 'pfull', varid)
      if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlev), rc, mype_, root_)
   endif
   if (nlev == 0) then
      ier = nf90_inq_dimid(ncid, 'level', varid)
      if (ier == NF90_NOERR) call check_(nf90_inquire_dimension(ncid, varid, len=nlev), rc, mype_, root_)
   endif

   if (nlon == 0 .or. nlat == 0 .or. nlev == 0) then
      if (mype_ == root_) then
         print *, myname_, ': could not determine dimensions, nlon,nlat,nlev = ', nlon, nlat, nlev
      endif
      rc = 99
   endif

   call check_(nf90_close(ncid), rc, mype_, root_)
end subroutine read_dims_

!---------------------------------------------------------------------------
subroutine read_GFSens_(fname, bvars, rc, myid, root, gsiset, gfspoles)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_GFSens_   read one GFS gaussian grid ensemble member
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Read GFS gaussian grid ensemble member from a NetCDF4 file.
!           When gfspoles=.true. (the normal GFS case), the file is assumed
!           to contain nlat-2 Gaussian latitude rows (no pole rows).  The
!           output bvars arrays are expanded to nlat rows by inserting the
!           south pole (index 1) and north pole (index nlat) and filling
!           them using the same algorithm as fillpoles_s_ / fillpoles_v_ in
!           cplr_gfs_ensmod.f90 -- the standard GSIbec / GSI practice.
!
!   For scalar fields:
!     pole_value = mean of the nearest Gaussian latitude row
!   For vector fields (GSI names sf/u and vp/v):
!     pole computed from average of nearest row using cos/sin of longitudes
!     (identical to fillpoles_v_ in cplr_gfs_ensmod.f90)
!
!   input argument list:
!     fname    - NetCDF4 file name
!     bvars    - nc_GFSens_vars struct (variable names must be set)
!     myid     - MPI rank of calling process (default 0)
!     root     - MPI root rank for messages (default 0)
!     gsiset   - if .true., transpose to (nlat,nlon) GSI layout (default .false.)
!     gfspoles - if .true., expand from (nlat-2) file rows to (nlat) GSI rows
!                and fill pole rows; requires gsiset=.true. (default .false.)
!
!   output argument list:
!     bvars    - populated with ensemble data
!     rc       - return code (0=success)
!
!$$$
   implicit none
   character(len=*), intent(in)    :: fname
   type(nc_GFSens_vars), intent(inout) :: bvars
   integer, intent(out) :: rc
   integer, intent(in), optional :: myid, root
   logical, intent(in), optional :: gsiset
   logical, intent(in), optional :: gfspoles

   integer :: ncid, varid, ier
   integer :: kk, nv, nlat, nlon, nlev
   integer :: nlat_, nlon_, nlev_
   integer :: nlat_file                 ! lats in file (nlat-2 for GFS, nlat for GEOS)
   integer :: mype_, root_
   real(4), allocatable :: data3d(:,:,:)
   real(4), allocatable :: data2d(:,:)
   real(4), allocatable :: clons(:), slons(:)  ! cos/sin of GFS longitudes for vector poles
   logical :: gsi_, gfspoles_, verbose, init_
   character(len=32) :: fvname
   integer :: uid, vid                  ! indices of u/v (sf/vp) in gsi_vnames3d
   integer :: i, j
   character(len=*), parameter :: myname_ = myname//'::read_GFSens_'

   rc = 0; mype_ = 0; root_ = 0
   verbose = .true.
   if (present(myid) .and. present(root)) then
      mype_ = myid
      root_ = root
      if (myid /= root) verbose = .false.
   endif

   gsi_ = .false.
   if (present(gsiset)) gsi_ = gsiset

   gfspoles_ = .false.
   if (present(gfspoles)) gfspoles_ = gfspoles

   ! Get dimensions from file
   call read_dims_(fname, nlat_file, nlon_, nlev_, rc, mype_, root_)
   if (rc /= 0) return

   ! Derive GSI nlat: for GFS gaussian grids nlat_gsi = nlat_file + 2 (pole rows added).
   ! For non-GFS files (gfspoles=.false.) nlat_gsi == nlat_file.
   if (gfspoles_) then
      nlat_ = nlat_file + 2
   else
      nlat_ = nlat_file
   endif

   init_ = bvars%initialized
   if (init_) then
      nlat = bvars%nlat
      nlon = bvars%nlon
      nlev = bvars%nsig
      if (nlon_ /= nlon .or. nlat_ /= nlat .or. nlev_ /= nlev) then
         if (mype_ == root_) then
            print *, myname_, ': nlat(file+poles)/nlat(req) = ', nlat_, nlat
            print *, myname_, ': nlon(file)/nlon(req)        = ', nlon_, nlon
            print *, myname_, ': nlev(file)/nlev(req)        = ', nlev_, nlev
            print *, myname_, ': inconsistent dimensions, aborting'
         endif
         rc = 1
         return
      endif
   else
      nlat = nlat_
      nlon = nlon_
      nlev = nlev_
      call init_GFSens_vars_(bvars, nlon, nlat, nlev, gsi=gsi_)
   endif

   call check_(nf90_open(fname, NF90_NOWRITE, ncid), rc, mype_, root_)
   if (rc /= 0) return

   ! Read 3D variables.
   ! data3d is read in file orientation (nlon, nlat_file, nlev).
   allocate(data3d(nlon, nlat_file, nlev))
   do nv = 1, bvars%nv3d
      fvname = gfs_varname_(bvars%gsi_vnames3d(nv))
      ier = nf90_inq_varid(ncid, trim(fvname), varid)
      if (ier /= NF90_NOERR) then
         if (verbose) print *, myname_, ': variable not found in file: ', trim(fvname), &
              ' (GSI name: ', trim(bvars%gsi_vnames3d(nv)), ')'
         data3d = 0.0
      else
         call check_(nf90_get_var(ncid, varid, data3d), rc, mype_, root_)
      endif
      if (gsi_) then
         if (gfspoles_) then
            ! GFS pole expansion: poles (rows 1 and nlat) initialised to zero;
            ! file rows 1..nlat_file -> GSI rows 2..nlat-1 (S->N CF ordering preserved).
            bvars%ptr3d(:,:,:,nv) = 0.0
            do kk = 1, nlev
               do j=1,nlat_file
                  do i=1,nlon
                     bvars%ptr3d(j+1,i,kk,nv) = data3d(i,j,kk)
                  end do
               end do
            enddo
         else
            do kk = 1, nlev
               bvars%ptr3d(:,:,kk,nv) = transpose(data3d(:,:,kk))
            enddo
         endif
      else
         bvars%ptr3d(:,:,:,nv) = data3d
      endif
   enddo
   deallocate(data3d)

   ! Read 2D variables.
   allocate(data2d(nlon, nlat_file))
   do nv = 1, bvars%nv2d
      fvname = gfs_varname_(bvars%gsi_vnames2d(nv))
      ier = nf90_inq_varid(ncid, trim(fvname), varid)
      if (ier /= NF90_NOERR) then
         if (verbose) print *, myname_, ': variable not found in file: ', trim(fvname), &
              ' (GSI name: ', trim(bvars%gsi_vnames2d(nv)), ')'
         data2d = 0.0
      else
         call check_(nf90_get_var(ncid, varid, data2d), rc, mype_, root_)
      endif
      if (gsi_) then
         if (gfspoles_) then
            bvars%ptr2d(:,:,nv) = 0.0
            do j=1,nlat_file
               do i=1,nlon
                  bvars%ptr2d(j+1,i,nv) = data2d(i,j)
               end do
            end do
         else
            bvars%ptr2d(:,:,nv) = transpose(data2d)
         endif
      else
         bvars%ptr2d(:,:,nv) = data2d
      endif
   enddo
   deallocate(data2d)

   call check_(nf90_close(ncid), rc, mype_, root_)
   if (rc /= 0) return

   if (verbose) print *, myname_, ': finished reading file: ', trim(fname)

   ! Convert from GFS file units to GSI units (e.g. ps Pa -> centibars).
   call gfs2gsi_(bvars)

   ! Fill pole rows for GFS gaussian grid (gsiset=.true. required).
   ! Step 1: scalar fill for all fields (mean of the nearest Gaussian row).
   ! Step 2: override u/v poles with the proper vector formula (fillpoles_v_).
   ! This matches the fillpoles_s_ / fillpoles_v_ sequence in cplr_gfs_ensmod.f90.
   if (gfspoles_ .and. gsi_) then

      ! Scalar pole fill for all 3D fields
      do nv = 1, bvars%nv3d
         do kk = 1, nlev
            call fillpoles_s_nc_(bvars%ptr3d(:,:,kk,nv), nlon, nlat)
         end do
      enddo
      ! Scalar pole fill for all 2D fields
      do nv = 1, bvars%nv2d
         call fillpoles_s_nc_(bvars%ptr2d(:,:,nv), nlon, nlat)
      enddo

      ! Vector pole override for u (sf or u) and v (vp or v) 3D pair.
      ! GFS longitudes are uniformly spaced 0 to 360: lon(j) = (j-1)*360/nlon deg.
      allocate(clons(nlon), slons(nlon))
      call compute_loncs_nc_(nlon, clons, slons)

      uid = getindex(bvars%gsi_vnames3d, 'sf')
      if (uid <= 0) uid = getindex(bvars%gsi_vnames3d, 'u')
      vid = getindex(bvars%gsi_vnames3d, 'vp')
      if (vid <= 0) vid = getindex(bvars%gsi_vnames3d, 'v')

      if (uid > 0 .and. vid > 0) then
         do kk = 1, nlev
            call fillpoles_v_nc_(bvars%ptr3d(:,:,kk,uid), &
                                 bvars%ptr3d(:,:,kk,vid), &
                                 nlon, nlat, clons, slons)
         enddo
      endif
      deallocate(clons, slons)

   endif

end subroutine read_GFSens_

!---------------------------------------------------------------------------
! Convert GFS file units/orientation to GSI convention.
! GFS NetCDF4 files (FV3/CF-compliant) are assumed to have:
!   - latitude south-to-north (same as GSI convention, no horizontal flip needed)
!   - longitude 0-360 eastward (same as GSI convention)
!   - vertical levels from model top to near-surface (k=1 = model top)
!
! GSI uses bottom-to-top ordering (k=1 = lowest model level, near surface).
! A vertical level flip is therefore required.
!
! Conversions applied:
!   - Vertical levels flipped (top->bottom to bottom->top) for 3D fields
!   - Surface pressure: Pa -> centibars (1 cb = 1 kPa = 1000 Pa)
!   - Temperature T -> virtual temperature Tv = T*(1 + fv*q)
!     (GFS files store actual temperature; GSI expects virtual temperature,
!      same as the move2bundle_ convention in cplr_gfs_ensmod.f90)
subroutine gfs2gsi_(x)
   use constants, only: fv
   implicit none
   type(nc_GFSens_vars), intent(inout) :: x
   integer :: id, id_t, id_q, nv

   ! need flip so localization function applies equally to EnKF and Hybrid-GSI
   call flip_(x)

   ! Surface pressure: Pa -> centibars (1 cb = 1000 Pa)
   id = getindex(x%gsi_vnames2d, 'ps')
   if (id > 0) x%ptr2d(:,:,id) = x%ptr2d(:,:,id) * Pa_to_cb

   ! Temperature: T -> virtual temperature Tv = T*(1 + fv*q)
   ! GFS files contain actual temperature ('tmp').  GSI expects virtual temperature
   ! in the 't'/'tv' bundle slot (same as move2bundle_ in cplr_gfs_ensmod.f90).
   ! fv is r_kind (double precision); cast to real(4) to match ptr3d storage.
   ! If neither 't' nor 'tv' is present (id_t<=0), the guard below skips the conversion.
   id_t = getindex(x%gsi_vnames3d, 't')
   if (id_t <= 0) id_t = getindex(x%gsi_vnames3d, 'tv')
   id_q = getindex(x%gsi_vnames3d, 'q')
   
   if (id_t > 0 .and. id_q > 0) then
      x%ptr3d(:,:,:,id_t) = x%ptr3d(:,:,:,id_t) * (1.0 + real(fv, kind=4) * x%ptr3d(:,:,:,id_q))
   endif

end subroutine gfs2gsi_

!---------------------------------------------------------------------------
! Compute cos and sin of GFS Gaussian grid longitudes (uniformly spaced).
! GFS Gaussian grids have nlon equally spaced longitudes starting at 0 deg:
!   lon(j) = (j-1) * 360 / nlon  degrees  =>  radians = (j-1) * 2*pi / nlon
! This is consistent with the clons/slons used in fillpoles_v_ (cplr_gfs_ensmod.f90).
subroutine compute_loncs_nc_(nlon, clons, slons)
   use constants, only: pi
   implicit none
   integer, intent(in)  :: nlon
   real(4), intent(out) :: clons(nlon), slons(nlon)
   integer :: j
   real(4) :: dlon
   dlon = real(2.0_8 * pi, 4) / real(nlon, 4)
   do j = 1, nlon
      clons(j) = cos(real(j-1) * dlon)
      slons(j) = sin(real(j-1) * dlon)
   enddo
end subroutine compute_loncs_nc_

!---------------------------------------------------------------------------
! Scalar pole fill: set south pole (row 1) and north pole (row nlat) to the
! average of the nearest Gaussian latitude row (row 2 and row nlat-1).
! Array is in GSI orientation: (nlat, nlon).
! Identical in logic to fillpoles_s_ in cplr_gfs_ensmod.f90.
subroutine fillpoles_s_nc_(arr, nlon, nlat)
   implicit none
   integer, intent(in)    :: nlon, nlat
   real(4), intent(inout) :: arr(nlat, nlon)
   integer :: j
   real(4) :: sums, sumn, rnlon
   rnlon = 1.0 / real(nlon)
   sums = sum(arr(2,     :)) * rnlon   ! mean of southernmost Gaussian row
   sumn = sum(arr(nlat-1,:)) * rnlon   ! mean of northernmost Gaussian row
   do j = 1, nlon
      arr(1,   j) = sums
      arr(nlat,j) = sumn
   enddo
end subroutine fillpoles_s_nc_

!---------------------------------------------------------------------------
! Vector pole fill for u and v on the GFS/GSI Gaussian grid.
! u and v are each in GSI orientation (nlat, nlon).
! clons and slons are cos/sin of the GFS longitude values.
! Identical in logic to fillpoles_v_ in cplr_gfs_ensmod.f90.
subroutine fillpoles_v_nc_(u, v, nlon, nlat, clons, slons)
   implicit none
   integer, intent(in)    :: nlon, nlat
   real(4), intent(inout) :: u(nlat, nlon), v(nlat, nlon)
   real(4), intent(in)    :: clons(nlon), slons(nlon)
   integer :: j
   real(4) :: polnu, polnv, polsu, polsv, rnlon
   rnlon = 1.0 / real(nlon, 4)
   polnu = 0.0
   polnv = 0.0
   polsu = 0.0
   polsv = 0.0
   do j = 1, nlon
      polnu = polnu + u(nlat-1,j)*clons(j) - v(nlat-1,j)*slons(j)
      polnv = polnv + u(nlat-1,j)*slons(j) + v(nlat-1,j)*clons(j)
      polsu = polsu + u(2,j     )*clons(j) + v(2,j     )*slons(j)
      polsv = polsv + u(2,j     )*slons(j) - v(2,j     )*clons(j)
   enddo
   polnu = polnu * rnlon
   polnv = polnv * rnlon
   polsu = polsu * rnlon
   polsv = polsv * rnlon
   do j = 1, nlon
      u(nlat,j) =  polnu*clons(j) + polnv*slons(j)
      v(nlat,j) = -polnu*slons(j) + polnv*clons(j)
      u(1,j)    =  polsu*clons(j) + polsv*slons(j)
      v(1,j)    =  polsu*slons(j) - polsv*clons(j)
   enddo
end subroutine fillpoles_v_nc_

!---------------------------------------------------------------------------
! flip_ combines latflip and levflip for cases where the file has both
! N->S latitude ordering AND bottom-to-top level ordering.
! For standard GFS NetCDF4 (FV3) gaussian grid files the latitude is already S->N
subroutine flip_(x)
  implicit none
  type(nc_GFSens_vars), intent(inout) :: x
  integer :: im, jm, km, nv

  im=x%nlon
  jm=x%nlat
  km=x%nsig
!
  do nv=1,x%nv2d
     call hflip2_(x%ptr2d(:,:,nv),im,jm,x%gsiset)
  enddo
!
  do nv=1,x%nv3d
     call hflip3_(x%ptr3d(:,:,:,nv),im,jm,km,x%gsiset)
     call vflip_ (x%ptr3d(:,:,:,nv),im,jm,km)
  enddo
  
end subroutine flip_

!---------------------------------------------------------------------------
! Flip latitude dimension (N->S to S->N).
! In GSI orientation (gsiset=.true.): array is (nlat,nlon[,nlev])
! In file orientation (gsiset=.false.): array is (nlon,nlat[,nlev])
subroutine hflip3_ ( q,im,jm,km, gsi )
  implicit none
  integer, intent(in) :: im,jm,km
  logical, intent(in) :: gsi
  real(4), intent(inout) :: q(:,:,:)
  integer :: i, j, k
  real(4), allocatable   :: dum(:)

  allocate ( dum(jm) )
  
  if (gsi) then
     ! Array is (nlat, nlon, nlev): flip first dimension
     do k=1,km
        do i=1,im
           dum = 0.0
           do j=1,jm
              dum(jm+1-j) = q(j,i,k)
           end do
           do j=1,jm
              q(j,i,k) = dum(j)
           end do
        enddo
     enddo
  else
     ! Array is (nlon, nlat, nlev): flip second dimension
     do k=1,km
        do i=1,im
           dum=0.0
           do j=1,jm
              dum(jm+1-j) = q(i,j,k)
           end do
           do j=1,jm
              q(i,j,k) = dum(j)
           end do
        enddo
     enddo
  endif
  
  deallocate ( dum )
  
end subroutine hflip3_

subroutine hflip2_ ( q,im,jm, gsi )
  implicit none
  integer, intent(in) :: im,jm
  logical, intent(in) :: gsi
  real(4), intent(inout) :: q(:,:)
  integer :: i, j
  real(4), allocatable   :: dum(:)
  
  allocate ( dum(jm) )
  
  if (gsi) then
     ! Array is (nlat, nlon): flip first dimension     
     do i=1,im
        dum = 0.0
        do j=1,jm
           dum(jm+1-j) = q(j,i)
        end do
        do j=1,jm
           q(j,i) = dum(j)
        end do
     enddo
  else
     ! Array is (nlon, nlat): flip second dimension
     do i=1,im
        dum = 0.0
        do j=1,jm
           dum(jm+1-j) = q(i,j)
        end do
        do j=1,jm
           q(i,j) = dum(j)
        end do
     enddo
  endif
  
  deallocate ( dum )
  
end subroutine hflip2_
  
subroutine vflip_(q,im,jm,km)
   implicit none
   integer,intent(in) :: im, jm, km
   real(4),intent(inout) :: q(im,jm,km)
   real(4), allocatable  :: dum(:)
   integer :: i, j
   
   allocate( dum(km) )
   do j=1,jm
      do i=1,im
         dum      = q(i,j,:)
         q(i,j,:) = dum(km:1:-1)
     end do
  end do
  
  deallocate( dum )
  
end subroutine vflip_


!---------------------------------------------------------------------------
subroutine get_pointer_2d_(vname, bvars, ptr, rc)
   implicit none
   character(len=*), intent(in) :: vname
   type(nc_GFSens_vars), intent(in) :: bvars
   real(4), pointer, intent(inout) :: ptr(:,:)
   integer, intent(out) :: rc
   integer :: id
   rc = -1
   id = getindex(bvars%gsi_vnames2d, trim(vname))
   if (id > 0) then
      ptr => bvars%ptr2d(:,:,id)
      rc = 0
   endif
end subroutine get_pointer_2d_

!---------------------------------------------------------------------------
subroutine get_pointer_3d_(vname, bvars, ptr, rc)
   implicit none
   character(len=*), intent(in) :: vname
   type(nc_GFSens_vars), intent(in) :: bvars
   real(4), pointer, intent(inout) :: ptr(:,:,:)
   integer, intent(out) :: rc
   integer :: id
   rc = -1
   id = getindex(bvars%gsi_vnames3d, trim(vname))
   if (id > 0) then
      ptr => bvars%ptr3d(:,:,:,id)
      rc = 0
   endif
end subroutine get_pointer_3d_

!---------------------------------------------------------------------------
subroutine check_(status, rc, myid, root)
   integer, intent(in)  :: status
   integer, intent(out) :: rc
   integer, intent(in)  :: myid, root
   rc = 0
   if (status /= nf90_noerr) then
      if (myid == root) print *, trim(nf90_strerror(status))
      rc = 999
   endif
end subroutine check_

end module m_nc_GFSens
