program test_geos_getens
use m_nc_GEOSens, only: nc_GEOSens_vars_init
use m_nc_GEOSens, only: nc_GEOSens_vars_final
use m_nc_GEOSens, only: nc_GEOSens_vars
use m_nc_GEOSens, only: nc_GEOSens_dims
use m_nc_GEOSens, only: nc_GEOSens_read
use m_nc_GEOSens, only: nc_GEOSens_write
use m_nc_GEOSens, only: nc_GEOSens_summary

use m_mpimod,  only: mpi_integer,gsi_mpi_comm_world
use m_mpimod,  only: setworld
use mpeu_util, only: die
implicit none

character(len=*), parameter :: myname='test_geos_getens'
character(len=*), parameter :: ifname='ibkgXXX.nc4'
character(len=*), parameter :: ofname='obkgXXX.nc4'
character(len=80) :: fname
logical :: already_init_mpi
integer :: nlon,nlat,nlev
integer :: ii,rc,mype,root,npe,ier
real :: dlat,dlon
real(4),allocatable :: lats(:), lons(:)

type(nc_GEOSens_vars) :: evars

root=0
call mpi_initialized(already_init_mpi,ier)
if(ier/=0) call die(myname,'mpi_initialized(), ieror =',ier)
if(.not.already_init_mpi) then
   call mpi_init(ier)
   if(ier/=0) call die(myname,'mpi_init(), ier =',ier)
endif
call setworld()
call mpi_comm_size(gsi_mpi_comm_world,npe,ier)
call mpi_comm_rank(gsi_mpi_comm_world,mype,ier)

fname = ifname
if(mype==0) then
  write(fname(5:7),'(i3.3)') 1
  call nc_GEOSens_dims (fname,nlat,nlon,nlev,rc, mype,root)
endif
call mpi_bcast(nlon,1,mpi_integer,0,gsi_mpi_comm_world,ier)
call mpi_bcast(nlat,1,mpi_integer,0,gsi_mpi_comm_world,ier)
call mpi_bcast(nlev,1,mpi_integer,0,gsi_mpi_comm_world,ier)

! for now only ...
allocate(lats(nlat),lons(nlon))
dlat=180/(nlat-1)
do ii=1,nlat
   lats(ii) = -90 - (ii-1)*dlat
enddo
dlon=360/nlon
do ii=1,nlon
   lons(ii) = -180 - ii*dlon
enddo

do ii=1,npe
  if(ii-1==mype) then
    write(fname(5:7),'(i3.3)') ii
    print *, 'pe=', mype, ' processing ', trim(fname)
    call readone_(fname,mype)
  endif
enddo

deallocate(lats,lons)

contains

subroutine readone_(fname,myid)
implicit none
character(len=*) :: fname
integer myid

call nc_GEOSens_read (fname,evars,rc, myid,root)
if(rc/=0) then
  call die('readone_',': failed reading file',99)
endif
call nc_GEOSens_summary(evars,myid+1)

end subroutine readone_

subroutine writeone_(myid)
implicit none
integer myid
!call nc_GEOSens_write (ofname,evars,lats,lons,rc, myid,root)
if(rc/=0) then
  call die('writeone_',': failed writing file',99)
endif
end subroutine writeone_

end program test_geos_getens

