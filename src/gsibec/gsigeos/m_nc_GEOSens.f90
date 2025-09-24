module m_nc_GEOSens
use netcdf
implicit none
private

public :: nc_GEOSens_vars_set
public :: nc_GEOSens_vars_init
public :: nc_GEOSens_vars_final
public :: nc_GEOSens_vars_comp
public :: nc_GEOSens_vars_copy
public :: nc_GEOSens_vars
public :: nc_GEOSens_dims
public :: nc_GEOSens_read
public :: nc_GEOSens_write
public :: nc_GEOSens_summary
public :: nc_GEOSens_getpointer
public :: nc_GEOSens_geos2gsi
public :: nc_GEOSens_gsi2geos

type nc_GEOSens_vars
   logical :: initialized=.false.
   integer :: nlon,nlat,nsig
   logical :: gsiset=.false.
   real(4),pointer,dimension(:):: ak,bk
   real(4),pointer,dimension(:,:,:):: dp
   real(4),pointer,dimension(:,:,:):: tv
   real(4),pointer,dimension(:,:,:):: u,v
   real(4),pointer,dimension(:,:,:):: qv
   real(4),pointer,dimension(:,:,:):: qi,ql,qr,qs
   real(4),pointer,dimension(:,:,:):: oz
   real(4),pointer,dimension(:,:)  :: ps,ts
   real(4),pointer,dimension(:,:,:):: ext1
   real(4),pointer,dimension(:,:,:):: ext2
!
   real(4),pointer,dimension(:)    :: v1d
   real(4),pointer,dimension(:,:)  :: v2d
   real(4),pointer,dimension(:,:,:):: v3d
end type nc_GEOSens_vars

character(len=*), parameter :: myname = 'm_nc_GEOSens'
real, parameter:: PPMV2GpG = 1.6571E-6 ! from ppmv to g/g
real, parameter:: mbar_per_Pa = 0.01   ! mb to Pa
real, parameter:: Pa_per_kPa = 1000.0

! belongs to bvars ... later
integer, save :: nv2d = -1
integer, save :: nv3d = -1
character(len=5),allocatable :: cvars2d(:)
character(len=5),allocatable :: cvars3d(:)

interface nc_GEOSens_dims; module procedure    &
  read_dims_ ; end interface
interface nc_GEOSens_read; module procedure    &
  read_GEOSens_ ; end interface
interface nc_GEOSens_write; module procedure    &
  write_GEOSens_ ; end interface
interface nc_GEOSens_vars_set; module procedure    &
  set_vars_ ; end interface
interface nc_GEOSens_vars_init; module procedure    &
  init_GEOSens_vars_ ; end interface
interface nc_GEOSens_vars_final; module procedure    &
  final_GEOSens_vars_ ; end interface
interface nc_GEOSens_vars_comp; module procedure    &
  comp_GEOSens_vars_ ; end interface
interface nc_GEOSens_vars_copy; module procedure    &
  copy_ ; end interface
interface nc_GEOSens_summary; module procedure    &
  summary_ ; end interface
interface nc_GEOSens_geos2gsi; module procedure    &
  geos2gsi_ ; end interface
interface nc_GEOSens_gsi2geos; module procedure    &
  gsi2geos_ ; end interface
interface nc_GEOSens_getpointer
  module procedure get_pointer_2d_
  module procedure get_pointer_3d_
end interface

! internal only
interface stddev_
  module procedure stddev2_
  module procedure stddev3_
end interface
contains

subroutine set_vars_(fvars2d,fvars3d)
implicit none
character(*), intent(in) :: fvars2d(:), fvars3d(:)

!integer, parameter :: met_nv2d = 2
!character(len=5),parameter :: met_cvars2d(met_nv2d) = (/ 'ps   ', 'ts   ' /)
!
!integer, parameter :: met_nv3d = 10
!character(len=5),parameter :: met_cvars3d(met_nv3d) = (/ &
!                                                     'tv   ', 'u    ', 'v    ', &
!                                                     'sphu ', 'qitot', 'qltot', &
!                                                     'qrtot', 'qstot', 'ozone', &
!                                                     'delp '&
!                                                     /)
!integer, parameter :: chm_nv2d = 1
!character(len=5),parameter :: chm_cvars2d(chm_nv2d) = (/ 'ps   '/)
!
!integer, parameter :: chm_nv3d = 2
!character(len=5),parameter :: chm_cvars3d(chm_nv3d) = (/ 'ext1 ', 'ext2 ' /)
!
!if ( trim(opt) == 'chem' ) then
!  nv2d = chm_nv2d
!  nv3d = chm_nv3d
!  allocate(cvars2d(nv2d))
!  allocate(cvars3d(nv3d))
!  cvars2d = chm_cvars2d
!  cvars3d = chm_cvars3d
!else
!  nv2d = met_nv2d
!  nv3d = met_nv3d
!  allocate(cvars2d(nv2d))
!  allocate(cvars3d(nv3d))
!  cvars2d = met_cvars2d
!  cvars3d = met_cvars3d
!endif
nv2d = size(fvars2d)
nv3d = size(fvars3d)
if (nv2d>0) then
  if(.not.allocated(cvars2d)) allocate(cvars2d(nv2d))
  cvars2d = fvars2d
endif
if (nv3d>0) then
  if(.not.allocated(cvars3d)) allocate(cvars3d(nv2d))
  cvars3d = fvars3d
endif
end subroutine set_vars_

subroutine read_dims_ (fname,nlat,nlon,nlev,rc, myid,root)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  integer, intent(out) :: rc
  integer, intent(out) :: nlat,nlon,nlev
  integer, intent(in), optional :: myid, root

! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid, ier
  integer :: mype_,root_

! Local variables
  character(len=*), parameter :: myname_ = myname//"::dims_"
  logical :: verbose

! Return code (status)
  rc=0; mype_=0; root_=0
  if(present(myid) .and. present(root) ) then
     mype_ = myid
     root_ = root
  endif

! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
! the file.

  call check_( nf90_open(fname, NF90_NOWRITE, ncid), rc, mype_, root_ )
  if(rc/=0) return

! Read global attributes
  call check_( nf90_inq_dimid(ncid, "lon", varid), rc, mype_, root_)
  call check_( nf90_inquire_dimension(ncid, varid, len=nlon), rc, mype_, root_ )
  call check_( nf90_inq_dimid(ncid, "lat", varid), rc, mype_, root_ )
  call check_( nf90_inquire_dimension(ncid, varid, len=nlat), rc, mype_, root_ )
  call check_( nf90_inq_dimid(ncid, "lev", varid), rc, mype_, root_ )
  call check_( nf90_inquire_dimension(ncid, varid, len=nlev), rc, mype_, root_ )

! Close the file, freeing all resources.
  call check_( nf90_close(ncid), rc, mype_, root_ )

  return

end subroutine read_dims_

subroutine read_GEOSens_ (fname,bvars,rc, myid,root, gsiset)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  type(nc_GEOSens_vars),intent(inout) :: bvars ! background error variables
  integer, intent(out) :: rc
  integer, intent(in), optional :: myid,root ! accommodate MPI calling programs
  logical, intent(in), optional :: gsiset

! This will be the netCDF ID for the file and data variable.
  integer :: ncid, varid

! Local variables
  character(len=*), parameter :: myname_ = myname//"::read_"
  character(len=4) :: cindx
  integer :: kk,nv,nl,nlat,nlon,nlev
  integer :: ndims_, nvars_, ngatts_, unlimdimid_
  integer :: nlat_,nlon_,nlev_
  integer :: mype_,root_
  real(4), allocatable :: data_in(:,:,:)
  logical :: verbose
  logical :: init_
  logical :: gsi_

! Return code (status)
  rc=0; mype_=0; root_=0
  verbose=.true.
  init_=.false.
  if(present(myid).and.present(root) )then
    if(myid/=root) verbose=.false.
    mype_ = myid
    root_ = root
  endif

  gsi_=.false.
  if(present(gsiset)) then
     gsi_=gsiset
  endif

! Get dimensions
  call read_dims_ (fname,nlat_,nlon_,nlev_,rc, mype_,root_)

  init_ = bvars%initialized
  if ( init_ ) then
!   Set dims
    nlat=bvars%nlat
    nlon=bvars%nlon
    nlev=bvars%nsig

!   Consistency check
    if (nlon_ /= nlon .or. nlat_ /=nlat .or. nlev_/=nlev ) then
       rc=1
       if(myid==root) then
         print *, 'nlat(file) = ', nlat_, 'nlat(required) = ', nlat
         print *, 'nlon(file) = ', nlon_, 'nlon(required) = ', nlon
         print *, 'nlev(file) = ', nlev_, 'nlev(required) = ', nlev
         print *, myname_,  'Inconsistent dimensions, aborting ... '
       endif
       return
    endif
  else
!   Set dims
    nlat=nlat_
    nlon=nlon_
    nlev=nlev_
    call init_GEOSens_vars_(bvars,nlon,nlat,nlev,gsi=gsi_)
  endif

! Open the file. NF90_NOWRITE tells netCDF we want read-only access to
! the file.

  call check_( nf90_open(fname, NF90_NOWRITE, ncid), rc, mype_, root_ )
  if(rc/=0) return

! Read global attributes
! call check_( nf90_inquire(ncid, ndims_, nvars_, ngatts_, unlimdimid_), rc, mype_, root_ )
! call check_( nf90_inq_dimid(ncid, "lon", varid), rc, mype_, root_ )
! call check_( nf90_inquire_dimension(ncid, varid, len=nlon_), rc, mype_, root_ )
! call check_( nf90_inq_dimid(ncid, "lat", varid), rc, mype_, root_ )
! call check_( nf90_inquire_dimension(ncid, varid, len=nlat_), rc, mype_, root_ )
! call check_( nf90_inq_dimid(ncid, "lev", varid), rc, mype_, root_ )
! call check_( nf90_inquire_dimension(ncid, varid, len=nlev_), rc, mype_, root_ )

! Read data to file
  allocate(data_in(nlon,nlat,1))
  do nv = 1, nv2d
     call check_( nf90_inq_varid(ncid, trim(cvars2d(nv)), varid), rc, mype_, root_ )
     call check_( nf90_get_var(ncid, varid, data_in(:,:,1)), rc, mype_, root_ )
     if (bvars%gsiset) then
       if(trim(cvars2d(nv))=="ps" ) bvars%ps = transpose(data_in(:,:,1))
       if(trim(cvars2d(nv))=="ts" ) bvars%ts = transpose(data_in(:,:,1))
     else
       if(trim(cvars2d(nv))=="ps" ) bvars%ps = data_in(:,:,1)
       if(trim(cvars2d(nv))=="ts" ) bvars%ts = data_in(:,:,1)
     endif
  enddo
  deallocate(data_in)
!
  allocate(data_in(nlon,nlat,nlev))
  do nv = 1, nv3d
     call check_( nf90_inq_varid(ncid, trim(cvars3d(nv)), varid), rc, mype_, root_  )
     call check_( nf90_get_var(ncid, varid, data_in(:,:,:)), rc, mype_, root_ )

     if (bvars%gsiset) then
        if(trim(cvars3d(nv))=="delp") then
           do kk=1,bvars%nsig
              bvars%dp(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
        if(trim(cvars3d(nv))=="tv"  ) then
           do kk=1,bvars%nsig
              bvars%tv(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
        if(trim(cvars3d(nv))=="u"   ) then
           do kk=1,bvars%nsig
              bvars%u(:,:,kk)  = transpose(data_in(:,:,kk))
           enddo
        endif
        if(trim(cvars3d(nv))=="v"   ) then
           do kk=1,bvars%nsig
              bvars%v(:,:,kk)  = transpose(data_in(:,:,kk))
           enddo
        endif
!
        if(trim(cvars3d(nv))=="sphu" ) then
           do kk=1,bvars%nsig
              bvars%qv(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
        if(trim(cvars3d(nv))=="qitot") then
           do kk=1,bvars%nsig
              bvars%qi(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
        if(trim(cvars3d(nv))=="qltot") then
           do kk=1,bvars%nsig
              bvars%ql(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
        if(trim(cvars3d(nv))=="qrtot") then
           do kk=1,bvars%nsig
              bvars%qr(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
        if(trim(cvars3d(nv))=="qstot") then
           do kk=1,bvars%nsig
              bvars%qs(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
!
        if(trim(cvars3d(nv))=="ozone") then
           do kk=1,bvars%nsig
              bvars%oz(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
!
        if(trim(cvars3d(nv))=="ext1") then
           do kk=1,bvars%nsig
              bvars%ext1(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
        if(trim(cvars3d(nv))=="ext2") then
           do kk=1,bvars%nsig
              bvars%ext2(:,:,kk) = transpose(data_in(:,:,kk))
           enddo
        endif
     else
        if(trim(cvars3d(nv))=="delp") bvars%dp = data_in(:,:,:)
        if(trim(cvars3d(nv))=="tv"  ) bvars%tv = data_in(:,:,:)
        if(trim(cvars3d(nv))=="u"   ) bvars%u  = data_in(:,:,:)
        if(trim(cvars3d(nv))=="v"   ) bvars%v  = data_in(:,:,:)
!
        if(trim(cvars3d(nv))=="sphu" ) bvars%qv = data_in(:,:,:)
        if(trim(cvars3d(nv))=="qitot") bvars%qi = data_in(:,:,:)
        if(trim(cvars3d(nv))=="qltot") bvars%ql = data_in(:,:,:)
        if(trim(cvars3d(nv))=="qrtot") bvars%qr = data_in(:,:,:)
        if(trim(cvars3d(nv))=="qstot") bvars%qs = data_in(:,:,:)
!
        if(trim(cvars3d(nv))=="ozone") bvars%oz = data_in(:,:,:)
!
        if(trim(cvars3d(nv))=="ext1") bvars%ext1 = data_in(:,:,:)
        if(trim(cvars3d(nv))=="ext2") bvars%ext2 = data_in(:,:,:)
     endif
!
  enddo
  deallocate(data_in)

! Close the file, freeing all resources.
  call check_( nf90_close(ncid), rc, mype_, root_ )

  if(verbose) print *,"*** Finish reading file: ", trim(fname)

! Convert to GSI units and orientation
  call geos2gsi_(bvars)

  return

end subroutine read_GEOSens_

subroutine write_GEOSens_ (fname,bvars,lats,lons,rc, myid,root,plevs)
  implicit none
  character(len=*), intent(in)    :: fname ! input filename
  type(nc_GEOSens_vars),intent(in)    :: bvars ! background error variables
  real(4), intent(in) :: lats(:)           ! latitudes per GSI: increase index from South to North Pole
  real(4), intent(in) :: lons(:)           ! longitude per GSI: increase index from East to West
  integer, intent(out) :: rc
  real(4), intent(in), optional :: plevs(:)
  integer, intent(in), optional :: myid,root        ! accommodate MPI calling programs

  character(len=*), parameter :: myname_ = myname//"::read_"
  integer, parameter :: NDIMS = 3

! When we create netCDF files, variables and dimensions, we get back
! an ID for each one.
  character(len=4) :: cindx
  integer :: ncid, dimids(NDIMS)
  integer :: x_dimid, y_dimid, z_dimid
  integer :: lon_varid, lat_varid, lev_varid
  integer :: ii,jj,nl,nv,nn,nlat,nlon,nlev
  integer :: mype_,root_
  integer, allocatable :: varid2d(:), varid3d(:)
  logical :: verbose

! This is the data array we will write. It will just be filled with
! a progression of integers for this example.
  real(4), allocatable :: data_out(:,:,:)
  real(4), allocatable :: idlevs(:)

! Consistency check
  if (bvars%gsiset) then
     print *,myname,'write must be in GEOS orientation'
     rc=99
     return
  endif

! Convert to GSI units and orientation
  call gsi2geos_(bvars)

! Return code (status)
  rc=0; mype_=0; root_=0
  verbose=.true.
  if(present(myid).and.present(root) )then
    if(myid/=root) verbose=.false.
    mype_ = myid
    root_ = root
  endif

! Set dims
  nlat=bvars%nlat
  nlon=bvars%nlon
  nlev=bvars%nsig

! Always check the return code of every netCDF function call. In
! this example program, wrapping netCDF calls with "call check()"
! makes sure that any return which is not equal to nf90_noerr (0)
! will print a netCDF error message and exit.

! Create the netCDF file. The nf90_clobber parameter tells netCDF to
! overwrite this file, if it already exists.
  call check_( nf90_create(fname, NF90_CLOBBER, ncid), rc, mype_, root_ )
  if(rc/=0) return

! Define the dimensions. NetCDF will hand back an ID for each.
  call check_( nf90_def_dim(ncid, "lon", nlon, x_dimid), rc, mype_, root_ )
  call check_( nf90_def_dim(ncid, "lat", nlat, y_dimid), rc, mype_, root_ )
  call check_( nf90_def_dim(ncid, "lev", nlev, z_dimid), rc, mype_, root_ )

  call check_( nf90_def_var(ncid, "lon", NF90_REAL, x_dimid, lon_varid), rc, mype_, root_ )
  call check_( nf90_def_var(ncid, "lat", NF90_REAL, y_dimid, lat_varid), rc, mype_, root_ )
  call check_( nf90_def_var(ncid, "lev", NF90_REAL, z_dimid, lev_varid), rc, mype_, root_ )

  call check_( nf90_put_att(ncid, lon_varid, "units", "degress"), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, lat_varid, "units", "degress"), rc, mype_, root_ )
  call check_( nf90_put_att(ncid, lev_varid, "units", "Pa"), rc, mype_, root_ )

! The dimids array is used to pass the IDs of the dimensions of
! the variables. Note that in fortran arrays are stored in
! column-major format.
  dimids =  (/ x_dimid, y_dimid, z_dimid /)

! Define variables.
  allocate(varid2d(nv2d))
  do nv = 1, nv2d
     call check_( nf90_def_var(ncid, trim(cvars2d(nv)), NF90_REAL, (/ x_dimid, y_dimid /), varid2d(nv)), rc, mype_, root_ )
  enddo
  allocate(varid3d(nv3d))
  do nv = 1, nv3d
     call check_( nf90_def_var(ncid, trim(cvars3d(nv)), NF90_REAL, (/ x_dimid, y_dimid, z_dimid /), varid3d(nv)), rc, mype_, root_ )
  enddo

! End define mode. This tells netCDF we are done defining metadata.
  call check_( nf90_enddef(ncid), rc, mype_, root_ )

! Write coordinate variables data
  call check_( nf90_put_var(ncid, lon_varid, lons ), rc, mype_, root_ )
  call check_( nf90_put_var(ncid, lat_varid, lats ), rc, mype_, root_ )
  if(present(plevs)) then
    call check_( nf90_put_var(ncid, lev_varid, plevs), rc, mype_, root_ )
  else
    allocate(idlevs(nlev))
    do ii = 1,nlev
       idlevs(ii) = ii
    enddo
    call check_( nf90_put_var(ncid, lev_varid, idlevs), rc, mype_, root_ )
    deallocate(idlevs)
  endif

! Write data to file
  allocate(data_out(nlon,nlat,1))
  do nv = 1, nv2d
     if(trim(cvars2d(nv))=="ps" ) data_out(:,:,1) = bvars%ps
     if(trim(cvars2d(nv))=="ts" ) data_out(:,:,1) = bvars%ts
     call check_( nf90_put_var(ncid, varid2d(nv), data_out(:,:,1)), rc, mype_, root_)
  enddo
  deallocate(data_out)
  allocate(data_out(nlon,nlat,nlev))
  do nv = 1, nv3d
     if(trim(cvars3d(nv))=="delp") data_out(:,:,:) = bvars%dp
     if(trim(cvars3d(nv))=="tv"  ) data_out(:,:,:) = bvars%tv
     if(trim(cvars3d(nv))=="u"   ) data_out(:,:,:) = bvars%u
     if(trim(cvars3d(nv))=="v"   ) data_out(:,:,:) = bvars%v
!
     if(trim(cvars3d(nv))=="sphu" ) data_out(:,:,:) = bvars%qv
     if(trim(cvars3d(nv))=="qitot") data_out(:,:,:) = bvars%qi
     if(trim(cvars3d(nv))=="qltot") data_out(:,:,:) = bvars%ql
     if(trim(cvars3d(nv))=="qrtot") data_out(:,:,:) = bvars%qr
     if(trim(cvars3d(nv))=="qstot") data_out(:,:,:) = bvars%qs
!
     if(trim(cvars3d(nv))=="ozone") data_out(:,:,:) = bvars%oz
!
     if(trim(cvars3d(nv))=="ext1") data_out(:,:,:) = bvars%ext1
     if(trim(cvars3d(nv))=="ext2") data_out(:,:,:) = bvars%ext2
!
     call check_( nf90_put_var(ncid, varid3d(nv), data_out(:,:,:)), rc, mype_, root_ )
  enddo
  deallocate(data_out)

! Close file
  call check_( nf90_close(ncid), rc, mype_, root_ )

  deallocate(varid3d)
  deallocate(varid2d)

  print *, "*** Finish writing file ", fname

  return

end subroutine write_GEOSens_

subroutine init_GEOSens_vars_(vr,nlon,nlat,nsig,gsi)

  integer,intent(in) :: nlon,nlat,nsig
  type(nc_GEOSens_vars) vr
  logical,intent(in),optional :: gsi

  if(vr%initialized) return

  if(present(gsi)) then
    vr%gsiset = gsi
  endif

  vr%nlon=nlon
  vr%nlat=nlat
  vr%nsig=nsig

! allocate single precision arrays
! why the if?
  if (vr%gsiset) then
     if(any(cvars3d == 'tv')) allocate(vr%tv(nlat,nlon,nsig))
     if(any(cvars3d == 'u' )) allocate(vr%u (nlat,nlon,nsig))
     if(any(cvars3d == 'v' )) allocate(vr%v (nlat,nlon,nsig))
     if(any(cvars3d == 'sphu' )) allocate(vr%qv(nlat,nlon,nsig))
     if(any(cvars3d == 'qitot') ) allocate(vr%qi(nlat,nlon,nsig))
     if(any(cvars3d == 'qltot') ) allocate(vr%ql(nlat,nlon,nsig))
     if(any(cvars3d == 'qrtot') ) allocate(vr%qr(nlat,nlon,nsig))
     if(any(cvars3d == 'qstot') ) allocate(vr%qs(nlat,nlon,nsig))
     if(any(cvars3d == 'ozone') ) allocate(vr%oz(nlat,nlon,nsig))
     if(any(cvars3d == 'delp' )) allocate(vr%dp(nlat,nlon,nsig))
     if(any(cvars3d == 'ext1' )) allocate(vr%ext1(nlat,nlon,nsig))
     if(any(cvars3d == 'ext2' )) allocate(vr%ext2(nlat,nlon,nsig))

     if(any(cvars2d == 'ps' )) allocate(vr%ps(nlat,nlon))
     if(any(cvars2d == 'ts' )) allocate(vr%ts(nlat,nlon))
  else
     if(any(cvars3d == 'tv')) allocate(vr%tv(nlat,nlon,nsig))
     if(any(cvars3d == 'u' )) allocate(vr%u (nlat,nlon,nsig))
     if(any(cvars3d == 'v' )) allocate(vr%v (nlat,nlon,nsig))
     if(any(cvars3d == 'sphu' )) allocate(vr%qv(nlat,nlon,nsig))
     if(any(cvars3d == 'qitot') ) allocate(vr%qi(nlat,nlon,nsig))
     if(any(cvars3d == 'qltot') ) allocate(vr%ql(nlat,nlon,nsig))
     if(any(cvars3d == 'qrtot') ) allocate(vr%qr(nlat,nlon,nsig))
     if(any(cvars3d == 'qstot') ) allocate(vr%qs(nlat,nlon,nsig))
     if(any(cvars3d == 'ozone') ) allocate(vr%oz(nlat,nlon,nsig))
     if(any(cvars3d == 'delp' )) allocate(vr%dp(nlat,nlon,nsig))
     if(any(cvars3d == 'ext1' )) allocate(vr%ext1(nlat,nlon,nsig))
     if(any(cvars3d == 'ext2' )) allocate(vr%ext2(nlat,nlon,nsig))

     if(any(cvars2d == 'ps' )) allocate(vr%ps(nlat,nlon))
     if(any(cvars2d == 'ts' )) allocate(vr%ts(nlat,nlon))
  endif
  vr%initialized=.true.
  end subroutine init_GEOSens_vars_

  subroutine final_GEOSens_vars_(vr)
  type(nc_GEOSens_vars) vr
! deallocate arrays
  if(.not. vr%initialized) return
  if(associated(vr%tv)) deallocate(vr%tv)
  if(associated(vr%u))  deallocate( vr%u)
  if(associated(vr%v))  deallocate( vr%v)
  if(associated(vr%v))  deallocate( vr%qv)
  if(associated(vr%v))  deallocate( vr%qi)
  if(associated(vr%v))  deallocate( vr%ql)
  if(associated(vr%v))  deallocate( vr%qr)
  if(associated(vr%v))  deallocate( vr%qs)
  if(associated(vr%v))  deallocate( vr%oz)
  if(associated(vr%dp))  deallocate( vr%dp)
  if(associated(vr%ext1))  deallocate(vr%ext1)
  if(associated(vr%ext2))  deallocate(vr%ext2)

  if(associated(vr%ps)) deallocate(vr%ps)
  if(associated(vr%ts)) deallocate(vr%ts)
  vr%initialized=.false.
end subroutine final_GEOSens_vars_

subroutine comp_GEOSens_vars_(va,vb,rc, myid,root)
  type(nc_GEOSens_vars) va
  type(nc_GEOSens_vars) vb
  integer, intent(out) :: rc
  integer, intent(in), optional :: myid,root        ! accommodate MPI calling programs
  character(len=*), parameter :: myname_ = myname//"::comp_GEOSens_vars_"
  integer :: ii,jj
  logical :: verbose, failed
  real :: tolerance = 10.e-10
  integer, allocatable :: ier(:)
!
  rc=0
  verbose=.true.
  if(present(myid).and.present(root) )then
    if(myid/=root) verbose=.false.
  endif
! Consistency check
  if (va%nlon/=vb%nlon .or. va%nlat/=vb%nlat .or. va%nsig/=vb%nsig ) then
     rc=1
     if(myid==root) then
       print *, 'nlat(va) = ', va%nlat, 'nlat(vb) = ', vb%nlat
       print *, 'nlon(va) = ', va%nlon, 'nlon(vb) = ', vb%nlon
       print *, 'nlev(va) = ', va%nsig, 'nlev(vb) = ', vb%nsig
       print *, myname_,  'Inconsistent dimensions, aborting ... '
     endif
     return
  endif

  allocate(ier(nv2d+nv3d))
  ii=0;ier=0
  if (associated(va%dp)) then
  ii=ii+1; if(abs(sum(va%dp - vb%dp)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%tv)) then
  ii=ii+1; if(abs(sum(va%tv - vb%tv)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%u)) then
  ii=ii+1; if(abs(sum(va%u  - vb%u )) >tolerance) ier(ii)=ii
  endif
  if (associated(va%v)) then
  ii=ii+1; if(abs(sum(va%v  - vb%v )) >tolerance) ier(ii)=ii
  endif
  if (associated(va%qv)) then
  ii=ii+1; if(abs(sum(va%qv - vb%qv)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%qi)) then
  ii=ii+1; if(abs(sum(va%qi - vb%qi)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%ql)) then
  ii=ii+1; if(abs(sum(va%ql - vb%ql)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%qr)) then
  ii=ii+1; if(abs(sum(va%qr - vb%qr)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%qs)) then
  ii=ii+1; if(abs(sum(va%qs - vb%qs)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%oz)) then
  ii=ii+1; if(abs(sum(va%oz - vb%oz)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%ext1)) then
  ii=ii+1; if(abs(sum(va%ext1 - vb%ext1)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%ext2)) then
  ii=ii+1; if(abs(sum(va%ext2 - vb%ext2)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%ps)) then
  ii=ii+1; if(abs(sum(va%ps - vb%ps)) >tolerance) ier(ii)=ii
  endif
  if (associated(va%ts)) then
  ii=ii+1; if(abs(sum(va%ts - vb%ts)) >tolerance) ier(ii)=ii
  endif
  failed=.false.
  do jj=1,ii
     if(ier(jj)/=0.and.verbose) then
       print *, 'Found field ', jj, ' not to match'
       failed=.true.
     endif
  enddo
  deallocate(ier)
  if (.not.failed) then
       if(verbose) print *, 'Comp finds all fields to match'
  endif
end subroutine comp_GEOSens_vars_

subroutine copy_(ivars,ovars,rc)
  type(nc_GEOSens_vars) ivars
  type(nc_GEOSens_vars) ovars
  integer, intent(out) :: rc
  integer :: kk

  rc=0
  if (ovars%nlon/=ivars%nlon .or. &
      ovars%nlat/=ivars%nlat .or. &
      ovars%nsig/=ivars%nsig ) then
      print*, 'copy_GEOSens_vars_: Trying to copy inconsistent vectors, aborting ...'
      rc=99
      return
  endif

  if (ivars%gsiset .neqv. ovars%gsiset ) then
     do kk=1,ovars%nsig
        if(associated(ovars%dp)) &
        ovars%dp(:,:,kk) = transpose(ivars%dp(:,:,kk))
        if(associated(ovars%tv)) &
        ovars%tv(:,:,kk) = transpose(ivars%tv(:,:,kk))
        if(associated(ovars%u)) &
        ovars%u (:,:,kk) = transpose(ivars%u (:,:,kk))
        if(associated(ovars%v)) &
        ovars%v (:,:,kk) = transpose(ivars%v (:,:,kk))
        if(associated(ovars%qv)) &
        ovars%qv(:,:,kk) = transpose(ivars%qv(:,:,kk))
        if(associated(ovars%qi)) &
        ovars%qi(:,:,kk) = transpose(ivars%qi(:,:,kk))
        if(associated(ovars%ql)) &
        ovars%ql(:,:,kk) = transpose(ivars%ql(:,:,kk))
        if(associated(ovars%qr)) &
        ovars%qr(:,:,kk) = transpose(ivars%qr(:,:,kk))
        if(associated(ovars%qs)) &
        ovars%qs(:,:,kk) = transpose(ivars%qs(:,:,kk))
        if(associated(ovars%oz)) &
        ovars%oz(:,:,kk) = transpose(ivars%oz(:,:,kk))
        if(associated(ovars%ext1)) &
        ovars%ext1(:,:,kk) = transpose(ivars%ext1(:,:,kk))
        if(associated(ovars%ext2)) &
        ovars%ext2(:,:,kk) = transpose(ivars%ext2(:,:,kk))
     enddo
     if(associated(ovars%ps)) &
     ovars%ps = transpose(ivars%ps)
     if(associated(ovars%ts)) &
     ovars%ts = transpose(ivars%ts)
  else
     if(associated(ovars%dp)) &
     ovars%dp = ivars%dp
     if(associated(ovars%tv)) &
     ovars%tv = ivars%tv
     if(associated(ovars%u)) &
     ovars%u  = ivars%u
     if(associated(ovars%v)) &
     ovars%v  = ivars%v
     if(associated(ovars%qv)) &
     ovars%qv = ivars%qv
     if(associated(ovars%qi)) &
     ovars%qi = ivars%qi
     if(associated(ovars%ql)) &
     ovars%ql = ivars%ql
     if(associated(ovars%qr)) &
     ovars%qr = ivars%qr
     if(associated(ovars%qs)) &
     ovars%qs = ivars%qs
     if(associated(ovars%oz)) &
     ovars%oz = ivars%oz
     if(associated(ovars%ext1)) &
     ovars%ext1 = ivars%ext1
     if(associated(ovars%ext2)) &
     ovars%ext2 = ivars%ext2

     if(associated(ovars%ps)) &
     ovars%ps = ivars%ps
     if(associated(ovars%ts)) &
     ovars%ts = ivars%ts
  endif

end subroutine copy_

subroutine get_pointer_2d_ (vname, bvars, ptr, rc )
implicit none
character(len=*), intent(in) :: vname
type(nc_GEOSens_vars) bvars
real(4),pointer,intent(inout) :: ptr(:,:)
integer,intent(out) :: rc
rc=-1
if(trim(vname)=='ps') then
  ptr => bvars%ps
  rc=0
endif
if(trim(vname)=='ts') then
  ptr => bvars%ts
  rc=0
endif
end subroutine get_pointer_2d_

subroutine get_pointer_3d_ (vname, bvars, ptr, rc )
implicit none
character(len=*), intent(in) :: vname
type(nc_GEOSens_vars) bvars
real(4),pointer,intent(inout) :: ptr(:,:,:)
integer,intent(out) :: rc
character(len=5) :: var
rc=-1
!
var='delp'
if(trim(vname)==trim(var)) then
  ptr => bvars%dp
  rc=0
  return
endif
!
var='tv'
if(trim(vname)==trim(var)) then
  ptr => bvars%tv
  rc=0
  return
endif
!
var='u'
if(trim(vname)==trim(var)) then
  ptr => bvars%u
  rc=0
  return
endif
!
var='v'
if(trim(vname)==trim(var)) then
  ptr => bvars%v
  rc=0
  return
endif
!
var='sphu'
if(trim(vname)==trim(var)) then
  ptr => bvars%qv
  rc=0
  return
endif
!
var='qitot'
if(trim(vname)==trim(var)) then
  ptr => bvars%qi
  rc=0
  return
endif
!
var='qltot'
if(trim(vname)==trim(var)) then
  ptr => bvars%ql
  rc=0
  return
endif
!
var='qrtot'
if(trim(vname)==trim(var)) then
  ptr => bvars%qr
  rc=0
  return
endif
!
var='qstot'
if(trim(vname)==trim(var)) then
  ptr => bvars%qs
  rc=0
  return
endif
!
var='ozone'
if(trim(vname)==trim(var)) then
  ptr => bvars%oz
  rc=0
  return
endif
!
var='ext1'
if(trim(vname)==trim(var)) then
  ptr => bvars%ext1
  rc=0
  return
endif
!
var='ext2'
if(trim(vname)==trim(var)) then
  ptr => bvars%ext2
  rc=0
  return
endif
end subroutine get_pointer_3d_

subroutine check_(status,rc, myid, root)
    integer, intent ( in) :: status
    integer, intent (out) :: rc
    integer, intent ( in) :: myid, root
    rc=0
    if(status /= nf90_noerr) then
      if(myid==root) print *, trim(nf90_strerror(status))
      rc=999
    end if
end subroutine check_

subroutine geos2gsi_ (x)
  implicit none
  type(nc_GEOSens_vars) x

  !==> input ps in Pa - convert to hPa(mb)
! x%grid%ak = x%grid%ak / Pa_per_kPa
  if (associated(x%ps)) &
  x%ps       = x%ps / Pa_per_kPa
  if (associated(x%dp)) &
  x%dp       = x%dp / Pa_per_kPa
  if (associated(x%oz)) &
  x%oz       = x%oz / PPMV2GpG
  ! need flip so localization function applies equaly to EnKF and Hybrid-GSI
  call flip_(x)
  ! still need a transpose
end subroutine geos2gsi_

subroutine gsi2geos_ (x)
  implicit none
  type(nc_GEOSens_vars) x

  !==> input ps in mbar - convert to Pa
! x%grid%ak    = x%grid%ak * Pa_per_kPa
  if (associated(x%ps) )&
  x%ps = x%ps * Pa_per_kPa
  if (associated(x%dp) )&
  x%dp = x%dp * Pa_per_kPa
  if (associated(x%oz) )&
  x%oz = x%oz * PPMV2GpG
  ! need flip so localization function applies equaly to EnKF and Hybrid-GSI
  call flip_(x)
  ! still need a transpose
end subroutine gsi2geos_

subroutine flip_(x)
  implicit none
  type(nc_GEOSens_vars) x
  integer im,jm,km
  im=x%nlon
  jm=x%nlat
  km=x%nsig
!
  if (associated(x%ps) ) then
  call hflip2_(x%ps,im,jm,x%gsiset)
  endif
  if (associated(x%ts) ) then
  call hflip2_(x%ts,im,jm,x%gsiset)
  endif
!
  if (associated(x%dp) ) then
  call hflip3_(x%dp,im,jm,km,x%gsiset)
  call vflip_ (x%dp,im,jm,km)
  endif

  if (associated(x%tv) ) then
  call hflip3_(x%tv,im,jm,km,x%gsiset)
  call vflip_ (x%tv,im,jm,km)
  endif

  if (associated(x%u) ) then
  call hflip3_(x%u ,im,jm,km,x%gsiset)
  call vflip_ (x%u ,im,jm,km)
  endif

  if (associated(x%v) ) then
  call hflip3_(x%v ,im,jm,km,x%gsiset)
  call vflip_ (x%v ,im,jm,km)
  endif

  if (associated(x%qv) ) then
  call hflip3_(x%qv,im,jm,km,x%gsiset)
  call vflip_ (x%qv,im,jm,km)
  endif

  if (associated(x%qi) ) then
  call hflip3_(x%qi,im,jm,km,x%gsiset)
  call vflip_ (x%qi,im,jm,km)
  endif

  if (associated(x%ql) ) then
  call hflip3_(x%ql,im,jm,km,x%gsiset)
  call vflip_ (x%ql,im,jm,km)
  endif

  if (associated(x%qr) ) then
  call hflip3_(x%qr,im,jm,km,x%gsiset)
  call vflip_ (x%qr,im,jm,km)
  endif

  if (associated(x%qs) ) then
  call hflip3_(x%qs,im,jm,km,x%gsiset)
  call vflip_ (x%qs,im,jm,km)
  endif

  if (associated(x%oz) ) then
  call hflip3_(x%oz,im,jm,km,x%gsiset)
  call vflip_ (x%oz,im,jm,km)
  endif

  if (associated(x%ext1) ) then
  call hflip3_(x%ext1,im,jm,km,x%gsiset)
  call vflip_ (x%ext1,im,jm,km)
  endif

  if (associated(x%ext2) ) then
  call hflip3_(x%ext2,im,jm,km,x%gsiset)
  call vflip_ (x%ext2,im,jm,km)
  endif
end subroutine flip_

subroutine hflip3_ ( q,im,jm,km, gsi )
    implicit none
    integer  im,jm,km,i,j,k
    logical  gsi
    real(4), intent(inout) :: q(:,:,:)
    real(4), allocatable   :: dum(:)
    allocate ( dum(im) )
    if (gsi) then
       do k=1,km
       do j=1,jm
       do i=1,im/2
          dum(i) = q(j,i+im/2,k)
          dum(i+im/2) = q(j,i,k)
       enddo
          q(j,:,k) = dum(:)
       enddo
       enddo
    else
       do k=1,km
       do j=1,jm
       do i=1,im/2
          dum(i) = q(i+im/2,j,k)
          dum(i+im/2) = q(i,j,k)
       enddo
          q(:,j,k) = dum(:)
       enddo
       enddo
    endif
    deallocate ( dum )
end subroutine hflip3_

subroutine hflip2_ ( q,im,jm, gsi )
    implicit none
    integer  im,jm,i,j
    logical  gsi
    real(4), intent(inout) :: q(:,:)
    real(4), allocatable   :: dum(:)
    allocate ( dum(im) )
    if (gsi) then
       do j=1,jm
       do i=1,im/2
          dum(i) = q(j,i+im/2)
          dum(i+im/2) = q(j,i)
       enddo
          q(j,:) = dum(:)
       enddo
    else
       do j=1,jm
       do i=1,im/2
          dum(i) = q(i+im/2,j)
          dum(i+im/2) = q(i,j)
       enddo
          q(:,j) = dum(:)
       enddo
    endif
    deallocate ( dum )
end subroutine hflip2_

subroutine vflip_(q,im,jm,km)
   implicit none
   integer,intent(in) :: im,jm,km
   real(4),intent(inout) :: q(im,jm,km)
   real(4), allocatable  :: dum(:)
   integer i,j
   allocate(dum(km))
   do j=1,jm
      do i=1,im
         dum      = q(i,j,:)
         q(i,j,:) = dum(km:1:-1)
     end do
  end do
  deallocate(dum)
end subroutine vflip_

subroutine summary_(x,myid)
implicit none
  type(nc_GEOSens_vars) x
  integer, intent(in), optional :: myid
  integer lu,nxy,nxyz
  nxy  = x%nlat*x%nlon
  nxyz = nxy*x%nsig
  lu=6
  if(present(myid)) lu=myid
  write(lu,'(a)') "================================================"
  write(lu,'(a)') "var     min        max       mean      stddev   "
  write(lu,'(a)') "================================================"
  write(lu,'(a4,1p,4(e10.3,1x))') 'ps', minval(x%ps), maxval(x%ps), sum(x%ps)/nxy,  stddev_(x%ps)
  write(lu,'(a4,1p,4(e10.3,1x))') 'ts', minval(x%ts), maxval(x%ts), sum(x%ts)/nxy,  stddev_(x%ts)
  write(lu,'(a4,1p,4(e10.3,1x))') 'dp', minval(x%dp), maxval(x%dp), sum(x%dp)/nxyz, stddev_(x%dp)
  write(lu,'(a4,1p,4(e10.3,1x))') 'tv', minval(x%tv), maxval(x%tv), sum(x%tv)/nxyz, stddev_(x%tv)
  write(lu,'(a4,1p,4(e10.3,1x))') 'qv', minval(x%qv), maxval(x%qv), sum(x%qv)/nxyz, stddev_(x%qv)
  write(lu,'(a4,1p,4(e10.3,1x))') 'qi', minval(x%qi), maxval(x%qi), sum(x%qi)/nxyz, stddev_(x%qi)
  write(lu,'(a4,1p,4(e10.3,1x))') 'ql', minval(x%ql), maxval(x%ql), sum(x%ql)/nxyz, stddev_(x%ql)
  write(lu,'(a4,1p,4(e10.3,1x))') 'qr', minval(x%qr), maxval(x%qr), sum(x%qr)/nxyz, stddev_(x%qr)
  write(lu,'(a4,1p,4(e10.3,1x))') 'qs', minval(x%qs), maxval(x%qs), sum(x%qs)/nxyz, stddev_(x%qs)
  write(lu,'(a4,1p,4(e10.3,1x))') 'oz', minval(x%oz), maxval(x%oz), sum(x%oz)/nxyz, stddev_(x%oz)
  write(lu,'(a4,1p,4(e10.3,1x))') 'ext1', minval(x%ext1), maxval(x%ext1), sum(x%ext1)/nxyz, stddev_(x%ext1)
  write(lu,'(a4,1p,4(e10.3,1x))') 'ext2', minval(x%ext2), maxval(x%ext2), sum(x%ext2)/nxyz, stddev_(x%ext2)
  write(lu,'(a)') "================================================"
end subroutine summary_

real function stddev2_(x)
 implicit none
 real(4) :: x(:,:)
 integer im,jm
 real mean
 im = size(x,1)
 jm = size(x,2)
 mean = sum(x)/(im*jm)
 stddev2_= sqrt(sum((x-mean)*(x-mean)))/(im*jm-1)
end function stddev2_

real function stddev3_(x)
 implicit none
 real(4) :: x(:,:,:)
 integer im,jm,km
 real mean
 im = size(x,1)
 jm = size(x,2)
 km = size(x,3)
 mean = sum(x)/(im*jm*km)
 stddev3_= sqrt(sum((x-mean)*(x-mean)))/(im*jm*km-1)
end function stddev3_

end module m_nc_GEOSens
