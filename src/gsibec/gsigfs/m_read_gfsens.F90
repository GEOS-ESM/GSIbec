module m_read_gfsens
!$$$ module documentation block
!                .      .    .                                       .
! module:   m_read_gfsens
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Parallel reading and MPI scattering of GFS gaussian grid ensemble
!           members from NetCDF4 files.  Analogous to m_read_geosens.f90.
!
! program history log:
!   2024-01-01  initial version following m_read_geosens.f90 structure
!
! subroutines included:
!   sub gfs_readens - read and scatter all ensemble members
!
! attributes:
!   language: Fortran 90 and/or above
!
!$$$

  use m_kinds, only: i_kind, r_kind
  use m_mpimod, only: gsi_mpi_comm_world
  use mpeu_util, only: die

  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  use gridmod, only: nsig
  use control_vectors, only: nc2d, nc3d
  use control_vectors, only: cvars2d, cvars3d

  use general_sub2grid_mod, only: sub2grid_info
  use m_grid2sub1var, only: grid2sub1var

  implicit none

  character(len=*), parameter :: myname = 'm_read_gfsens'
  private
  public gfs_readens

  interface gfs_readens
     module procedure read_gfsens_
  end interface

  integer(i_kind), allocatable, dimension(:) :: spec_send
  integer(i_kind), allocatable, dimension(:) :: disp_spec
  integer(i_kind) :: nlat_g, nlon_g, iglobal, nsig1o

contains

subroutine read_gfsens_(sgrid, xx, filename, npe, mype, root, nreaders)
!$$$ subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfsens_   read GFS ensemble members from NetCDF4 files
!   prgmmr: todling          org: np22                date: 2024-01-01
!
! abstract: Read GFS gaussian grid ensemble members from NetCDF4 files and
!           scatter them to all MPI tasks.  Based on read_geosens_ from
!           m_read_geosens.f90.
!
!   input argument list:
!     sgrid    - sub2grid communication information
!     filename - array of filenames (one per ensemble member)
!     npe      - total number of MPI tasks
!     mype     - this task's MPI rank
!     root     - root MPI task
!     nreaders - (optional) number of parallel readers; default npe/2
!
!   output argument list:
!     xx       - array of gsi_bundle (one per ensemble member)
!
!$$$
  use m_nc_GFSens, only: nc_GFSens_vars_set
  use m_nc_GFSens, only: nc_GFSens_vars
  use m_nc_GFSens, only: nc_GFSens_read
  use m_nc_GFSens, only: nc_GFSens_vars_final

  implicit none

  type(sub2grid_info), intent(in)    :: sgrid
  type(gsi_bundle),    intent(inout) :: xx(:)
  character(len=*),    intent(in)    :: filename(:)
  integer,             intent(in)    :: npe, mype, root
  integer, optional,   intent(in)    :: nreaders

  character(len=*), parameter :: myname_ = myname//'::read_gfsens_'
  integer :: i, iret, numcases, nreaders_
  integer :: mm1, nsig1, ii, ns, nskip
  type(nc_GFSens_vars) :: evars

  real(r_kind), allocatable, dimension(:,:) :: z4all
  integer, allocatable :: nprocs(:)

  if (present(nreaders)) then
     nreaders_ = nreaders
  else
     nreaders_ = max(1, npe/2)
  endif
  numcases = size(filename)
  nsig1 = nsig + 1

  nlat_g = sgrid%nlat
  nlon_g = sgrid%nlon
  iglobal = sgrid%iglobal

  call nc_GFSens_vars_set(cvars2d, cvars3d, evars)
  call init_()

  mm1 = mype + 1

  allocate(z4all(iglobal, nsig1o))

  if (nreaders_ > npe) nreaders_ = 1
  if (mod(npe, nreaders_) /= 0) then
     call die(myname_, ': nreaders must divide npe evenly', 99)
  endif
  nskip = 1
  allocate(nprocs(nreaders_))
  do ii = 1, nreaders_
     nprocs(ii) = (ii-1)*npe/nreaders_
  enddo
  ii = 0; ns = 1
  do while (ns <= numcases)
     do ii = 1, nreaders_, nskip
        if (ns <= numcases) then
           call read_(ns, nprocs(ii))
        endif
        ns = ns + 1
     enddo
     call mpi_barrier(gsi_mpi_comm_world, iret)
     ns = ns - nreaders_/nskip
     do ii = 1, nreaders_, nskip
        if (ns <= numcases) then
           call scatter_(sgrid, ns, nprocs(ii))
        endif
        ns = ns + 1
     enddo
     call mpi_barrier(gsi_mpi_comm_world, iret)
  enddo
  deallocate(nprocs)
  deallocate(z4all)

  call mpi_barrier(gsi_mpi_comm_world, iret)
  call final_()

  return

contains

  subroutine init_()
    integer :: kchk, n

    allocate(spec_send(npe), disp_spec(npe))

    ! nsig1o: total number of 2D slices (nc3d vars * nsig levels + nc2d vars).
    ! The max(0,...) guards against nc3d/nc2d being uninitialized, zero, or negative.
    nsig1o = (nsig*max(0,nc3d)) + max(0,nc2d)
    if (mod(nsig1o, npe) == 0) then
       kchk = npe
    else
       kchk = mod(nsig1o, npe)
    endif

    do n = 1, npe
       if (n <= kchk) then
          spec_send(n) = iglobal*nsig1o
       else
          spec_send(n) = iglobal*(nsig1o-1)
       endif
    enddo

    disp_spec(1) = 0
    do n = 2, npe
       disp_spec(n) = disp_spec(n-1) + spec_send(n-1)
    enddo
  end subroutine init_

  subroutine read_(n, proc1)
    implicit none
    integer, intent(in) :: n, proc1

    character(len=*), parameter :: myname__ = myname_//'::read_'
    integer :: i, j, k, ii, ke, nv, istatus

    if (mype /= proc1) return

    write(6,'(2a,1x,i5,1x,a)') myname__, ': reading file on PE= ', mype, trim(filename(n))
    ! gfspoles=.true. tells the reader that the GFS gaussian grid file has nlat-2
    ! latitude rows (no poles).  The reader expands to nlat rows and fills pole
    ! rows using the same fillpoles_s_ / fillpoles_v_ logic as cplr_gfs_ensmod.f90.
    call nc_GFSens_read(filename(n), evars, istatus, myid=mype, root=proc1, &
                        gsiset=.true., gfspoles=.true.)
    if (istatus /= 0) call die(myname__, ': failed reading ensemble member', 99)
    if (evars%nlat /= nlat_g .or. evars%nlon /= nlon_g .or. evars%nsig /= nsig) then
       print *, myname__, ': inconsistent dims (code): ', nlat_g, nlon_g, nsig
       print *, myname__, ': inconsistent dims (file): ', evars%nlat, evars%nlon, evars%nsig
       call die(myname__, ': aborting due to dimension mismatch', 999)
    endif

    ke = 0
    do nv = 1, nc3d
       do k = 1, nsig
          ii = 0
          do j = 1, nlon_g
             do i = 1, nlat_g
                ii = ii + 1
                z4all(ii, ke+k) = evars%ptr3d(i, j, k, nv)
             enddo
          enddo
       enddo
       ke = ke + nsig
    enddo

    do nv = 1, nc2d
       ii = 0
       do j = 1, nlon_g
          do i = 1, nlat_g
             ii = ii + 1
             z4all(ii, ke+1) = evars%ptr2d(i, j, nv)
          enddo
       enddo
       ke = ke + 1
    enddo

    call nc_GFSens_vars_final(evars)
    write(6,'(2a,i5)') myname__, ': done reading file on PE= ', mype
  end subroutine read_

  subroutine scatter_(sgrid_in, n, proc1)
    type(sub2grid_info), intent(in) :: sgrid_in
    integer, intent(in) :: n, proc1

    call grid2subNvar_(sgrid_in, reshape(z4all, (/nlat_g, nlon_g, nsig1o/)), xx(n), mype, proc1)
  end subroutine scatter_

  subroutine final_()
    deallocate(spec_send, disp_spec)
  end subroutine final_

end subroutine read_gfsens_

!---------------------------------------------------------------------------
subroutine grid2subNvar_(sgrid, zvec, xx, mype, proc1)
  implicit none
  type(sub2grid_info), intent(in)    :: sgrid
  real(r_kind),        intent(in)    :: zvec(:,:,:)
  type(gsi_bundle),    intent(inout) :: xx
  integer,             intent(in)    :: mype
  integer,             intent(in)    :: proc1

  character(len=*), parameter :: myname_ = myname//'::grid2subNvar_'
  real(r_kind), allocatable :: aux(:,:)
  integer :: ie, is, jj, nv, istatus

  is = 0
  ie = 0
  do nv = 1, nc3d
     call gsi_bundlegetpointer(xx, cvars3d(nv), jj, istatus)
     if (istatus == 0) then
        is = ie + 1
        ie = ie + nsig
        call grid2sub1var(sgrid, zvec(:,:,is:ie), xx%r3(jj)%q, proc1, istatus)
     endif
  enddo

  ! Allocate aux once for 2D scatter; must be global dimensions (nlat_g x nlon_g)
  ! for grid2sub1var to correctly scatter from the reading PE to all subdomains.
  if (nc2d > 0) allocate(aux(size(zvec,1), size(zvec,2)))
  do nv = 1, nc2d
     call gsi_bundlegetpointer(xx, cvars2d(nv), jj, istatus)
     if (istatus == 0) then
        ie = ie + 1
        aux = zvec(:,:,ie)
        call grid2sub1var(sgrid, aux, xx%r2(jj)%q, proc1, istatus)
     endif
  enddo
  if (nc2d > 0) deallocate(aux)

end subroutine grid2subNvar_

end module m_read_gfsens
