module m_nc_GEOSens
use netcdf
use mpeu_util, only: getindex
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

! The following type is essentially the CV but the varnames
! it operates on a those in file, as opposed to the GSI
! names - see anavinfo. Because this is controlled in 
! anavinfo, the ordering of the fields is that in the CV.
! See also, set_ routine below and its input fields names
! arrays.
type nc_GEOSens_vars
   logical :: initialized=.false.
   integer :: nlon,nlat,nsig
   logical :: gsiset=.false.
   real(4),pointer,dimension(:):: ak,bk
   real(4),pointer,dimension(:,:,:,:):: ptr3d
   real(4),pointer,dimension(:,:,:)  :: ptr2d
!
   integer :: nv2d = -1
   integer :: nv3d = -1
   character(len=5),allocatable :: fvars2d(:)
   character(len=5),allocatable :: fvars3d(:)
!
   real(4),pointer,dimension(:)    :: v1d
   real(4),pointer,dimension(:,:)  :: v2d
   real(4),pointer,dimension(:,:,:):: v3d
end type nc_GEOSens_vars

character(len=*), parameter :: myname = 'm_nc_GEOSens'
real, parameter:: PPMV2GpG = 1.6571E-6 ! from ppmv to g/g
real, parameter:: mbar_per_Pa = 0.01   ! mb to Pa
real, parameter:: Pa_per_kPa = 1000.0


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

subroutine set_vars_(fvars2d,fvars3d,bvars)
implicit none
character(*), intent(in) :: fvars2d(:) ! in-file-varname for 2d-fiels per anavinfo 
character(*), intent(in) :: fvars3d(:) ! in-file-varname for 3d-fiels per anavinfo
type(nc_GEOSens_vars),intent(inout) :: bvars

bvars%nv2d = size(fvars2d)
bvars%nv3d = size(fvars3d)
if (bvars%nv2d>0) then
  if(.not.allocated(bvars%fvars2d)) allocate(bvars%fvars2d(bvars%nv2d))
  bvars%fvars2d = fvars2d
endif
if (bvars%nv3d>0) then
  if(.not.allocated(bvars%fvars3d)) allocate(bvars%fvars3d(bvars%nv2d))
  bvars%fvars3d = fvars3d
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
  do nv = 1, bvars%nv2d
     call check_( nf90_inq_varid(ncid, trim(bvars%fvars2d(nv)), varid), rc, mype_, root_ )
     call check_( nf90_get_var(ncid, varid, data_in(:,:,1)), rc, mype_, root_ )
     if (bvars%gsiset) then
       bvars%ptr2d(:,:,nv) = transpose(data_in(:,:,1))
     else
       bvars%ptr2d(:,:,nv) = data_in(:,:,1)
     endif
  enddo
  deallocate(data_in)
!
  allocate(data_in(nlon,nlat,nlev))
  do nv = 1, bvars%nv3d
     call check_( nf90_inq_varid(ncid, trim(bvars%fvars3d(nv)), varid), rc, mype_, root_  )
     call check_( nf90_get_var(ncid, varid, data_in(:,:,:)), rc, mype_, root_ )

     if (bvars%gsiset) then
        do kk=1,nlev
           bvars%ptr3d(:,:,kk,nv) = transpose(data_in(:,:,kk))
        enddo
     else
        bvars%ptr3d(:,:,:,nv) = data_in(:,:,:)
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
  allocate(varid2d(bvars%nv2d))
  do nv = 1, bvars%nv2d
     call check_( nf90_def_var(ncid, trim(bvars%fvars2d(nv)), NF90_REAL, (/ x_dimid, y_dimid /), varid2d(nv)), rc, mype_, root_ )
  enddo
  allocate(varid3d(bvars%nv3d))
  do nv = 1, bvars%nv3d
     call check_( nf90_def_var(ncid, trim(bvars%fvars3d(nv)), NF90_REAL, (/ x_dimid, y_dimid, z_dimid /), varid3d(nv)), rc, mype_, root_ )
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
  do nv = 1, bvars%nv2d
     data_out(:,:,1) = bvars%ptr2d(:,:,nv)
     call check_( nf90_put_var(ncid, varid2d(nv), data_out(:,:,1)), rc, mype_, root_)
  enddo
  deallocate(data_out)
  allocate(data_out(nlon,nlat,nlev))
  do nv = 1, bvars%nv3d
     data_out(:,:,:) = bvars%ptr3d(:,:,:,nv)
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

  integer :: ii

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
     if(vr%nv3d>0) allocate(vr%ptr3d(nlat,nlon,nsig,vr%nv3d))
     if(vr%nv2d>0) allocate(vr%ptr2d(nlat,nlon,vr%nv2d))
  else
     if(vr%nv3d>0) allocate(vr%ptr3d(nlat,nlon,nsig,vr%nv3d))
     if(vr%nv2d>0) allocate(vr%ptr2d(nlat,nlon,vr%nv2d))
  endif
  vr%initialized=.true.
  end subroutine init_GEOSens_vars_

  subroutine final_GEOSens_vars_(vr)
  type(nc_GEOSens_vars) vr
! deallocate arrays
  if(.not. vr%initialized) return
  if(vr%nv2d>0) deallocate(vr%ptr2d)
  if(vr%nv3d>0) deallocate(vr%ptr3d)
  vr%initialized=.false.
end subroutine final_GEOSens_vars_

subroutine comp_GEOSens_vars_(va,vb,rc, myid,root)
  type(nc_GEOSens_vars) va
  type(nc_GEOSens_vars) vb
  integer, intent(out) :: rc
  integer, intent(in), optional :: myid,root        ! accommodate MPI calling programs
  character(len=*), parameter :: myname_ = myname//"::comp_GEOSens_vars_"
  integer :: ii,jj,nv
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

  if(va%nv2d /= vb%nv2d .or. va%nv3d /= vb%nv3d) then
     rc=1
     if(myid==root) then
       print *, 'nv2d(va) = ', va%nv2d, 'nv2d(vb) = ', vb%nv2d
       print *, 'nv3d(va) = ', va%nv3d, 'nv3d(vb) = ', vb%nv3d
       print *, myname_,  'Inconsistent dimensions, aborting ... '
     endif
  endif
  allocate(ier(va%nv2d+va%nv3d))
  ier=0
  
  ii=0
  do nv=1,va%nv3d
    ii=ii+1
    if(abs(sum(va%ptr3d(:,:,:,nv) - vb%ptr3d(:,:,:,nv))) >tolerance) ier(ii)=ii
  enddo
  do nv=1,va%nv2d
    ii=ii+1
    if(abs(sum(va%ptr2d(:,:,nv) - vb%ptr2d(:,:,nv))) >tolerance) ier(ii)=ii
  enddo
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
  integer :: kk,nv

  rc=0
  if (ovars%nlon/=ivars%nlon .or. &
      ovars%nlat/=ivars%nlat .or. &
      ovars%nsig/=ivars%nsig ) then
      print*, 'copy_GEOSens_vars_: Trying to copy inconsistent vectors, aborting ...'
      rc=99
      return
  endif

  if (ivars%gsiset .neqv. ovars%gsiset ) then
     do nv=1,ovars%nv3d
        do kk=1,ovars%nsig
           ovars%ptr3d(:,:,kk,nv) = transpose(ivars%ptr3d(:,:,kk,nv))
        enddo
     enddo
     do nv=1,ovars%nv2d
        ovars%ptr2d(:,:,nv) = transpose(ivars%ptr2d(:,:,nv))
     enddo
  else
     do nv=1,ovars%nv3d
        ovars%ptr3d(:,:,:,nv) = ivars%ptr3d(:,:,:,nv)
     enddo
     do nv=1,ovars%nv2d
        ovars%ptr2d(:,:,nv) = ivars%ptr2d(:,:,nv)
     enddo
  endif

end subroutine copy_

subroutine get_pointer_2d_ (vname, bvars, ptr, rc )
implicit none
character(len=*), intent(in) :: vname
type(nc_GEOSens_vars) bvars
real(4),pointer,intent(inout) :: ptr(:,:)
integer,intent(out) :: rc
integer :: id
rc=-1
id=getindex(bvars%fvars2d,trim(vname))
if (id>0) then
  ptr = bvars%ptr2d(:,:,id)
  rc=0
endif
end subroutine get_pointer_2d_

subroutine get_pointer_3d_ (vname, bvars, ptr, rc )
implicit none
character(len=*), intent(in) :: vname
type(nc_GEOSens_vars) bvars
real(4),pointer,intent(inout) :: ptr(:,:,:)
integer,intent(out) :: rc
integer :: id
rc=-1
id=getindex(bvars%fvars3d,trim(vname))
if (id>0) then
  ptr = bvars%ptr3d(:,:,:,id)
  rc=0
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
  integer id

  !==> input ps in Pa - convert to hPa(mb)
! x%grid%ak = x%grid%ak / Pa_per_kPa
  id=getindex(x%fvars2d,'ps')
  if (id>0) then
     x%ptr2d(:,:,id) = x%ptr2d(:,:,id) / Pa_per_kPa
  endif
  id=getindex(x%fvars3d,'delp')
  if (id>0) then
     x%ptr3d(:,:,:,id) = x%ptr3d(:,:,:,id) / Pa_per_kPa
  else
     id=getindex(x%fvars3d,'dp')
     if(id>0) x%ptr3d(:,:,:,id) = x%ptr3d(:,:,:,id) / Pa_per_kPa
  endif
  id=getindex(x%fvars3d,'ozone')
  if (id>0) then
     x%ptr3d(:,:,:,id) = x%ptr3d(:,:,:,id) / PPMV2GpG
  else
     id=getindex(x%fvars3d,'oz')
     if(id>0) x%ptr3d(:,:,:,id) = x%ptr3d(:,:,:,id) / PPMV2GpG
  endif
  ! need flip so localization function applies equaly to EnKF and Hybrid-GSI
  call flip_(x)
  ! still need a transpose
end subroutine geos2gsi_

subroutine gsi2geos_ (x)
  implicit none
  type(nc_GEOSens_vars) x
  integer :: id

  !==> input ps in mbar - convert to Pa
! x%grid%ak    = x%grid%ak * Pa_per_kPa
  id=getindex(x%fvars2d,'ps')
  if (id>0) then
     x%ptr2d(:,:,id) = x%ptr2d(:,:,id) * Pa_per_kPa
  endif
  id=getindex(x%fvars3d,'delp')
  if (id>0) then
     x%ptr3d(:,:,:,id) = x%ptr3d(:,:,:,id) * Pa_per_kPa
  else
     id=getindex(x%fvars3d,'dp')
     if (id>0) x%ptr3d(:,:,:,id) = x%ptr3d(:,:,:,id) * Pa_per_kPa 
  endif
  id=getindex(x%fvars3d,'ozone')
  if (id>0) then
     x%ptr3d(:,:,:,id) = x%ptr3d(:,:,:,id) * PPMV2GpG
  else
     id=getindex(x%fvars3d,'oz')
     if(id>0) x%ptr3d(:,:,:,id) = x%ptr3d(:,:,:,id) * PPMV2GpG
  endif
  ! need flip so localization function applies equaly to EnKF and Hybrid-GSI
  call flip_(x)
  ! still need a transpose
end subroutine gsi2geos_

subroutine flip_(x)
  implicit none
  type(nc_GEOSens_vars) x
  integer im,jm,km,nv
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
  integer lu,nxy,nxyz,nv
  nxy  = x%nlat*x%nlon
  nxyz = nxy*x%nsig
  lu=6
  if(present(myid)) lu=myid
  write(lu,'(a)') "================================================="
  write(lu,'(a)') "var      min        max       mean      stddev   "
  write(lu,'(a)') "================================================="
  do nv=1,x%nv2d
     write(lu,'(a5,1p,4(e10.3,1x))') trim(x%fvars2d(nv)), minval(x%ptr2d(:,:,nv)), &
                                                          maxval(x%ptr2d(:,:,nv)), &
                                                             sum(x%ptr2d(:,:,nv))/nxy, &
                                                         stddev_(x%ptr2d(:,:,nv))
  enddo
  do nv=1,x%nv3d
     write(lu,'(a5,1p,4(e10.3,1x))') trim(x%fvars3d(nv)), minval(x%ptr3d(:,:,:,nv)), &
                                                          maxval(x%ptr3d(:,:,:,nv)), &
                                                             sum(x%ptr3d(:,:,:,nv))/nxy, &
                                                         stddev_(x%ptr3d(:,:,:,nv))
  enddo
  write(lu,'(a)') "================================================="
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
