module m_read_geosens
  use m_kinds, only: i_kind,r_kind
  use m_mpimod, only: gsi_mpi_comm_world
  use m_mpimod, only: mpi_rtype
  use mpeu_util, only: die
  use mpeu_util, only: getindex
  use constants, only: zero

  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: assignment(=)
  use gsi_bundlemod, only: gsi_bundlegetpointer

  use gridmod,only: nlat,nlon,nsig,iglobal
  use control_vectors, only: nc2d,nc3d
  use control_vectors, only: cvars2d,cvars3d

  use m_grid2sub1var, only: grid2sub1var

implicit none

character(len=*),parameter :: myname = 'm_read_geosens'
private
public geos_readens

interface geos_readens
   module procedure read_geosens_
end interface

integer(i_kind),allocatable,dimension(:):: spec_send 
integer(i_kind),allocatable,dimension(:):: disp_spec
integer(i_kind) :: nsig1o

contains
subroutine read_geosens_(xx,filename,npe,mype,root,nreaders)

  use m_nc_GEOSens, only: nc_GEOSens_vars
  use m_nc_GEOSens, only: nc_GEOSens_read
  use m_nc_GEOSens, only: nc_GEOSens_vars_final

  implicit none

  type(gsi_bundle),intent(inout) :: xx(:)
  character(len=*),intent(in) :: filename(:)
  integer, intent(in) :: npe,mype,root
  integer, optional, intent(in) :: nreaders

! local variables
  character(len=*), parameter :: myname="m_readpairs"
  integer i,iret,numcases,nreaders_
  integer :: mm1,nsig1,ii,ns,nskip
  type(nc_GEOSens_vars) :: evars

  real(r_kind), allocatable,dimension(:,:) :: z4all
  integer,allocatable::nprocs(:)

  if(present(nreaders)) then
    nreaders_ = nreaders
  else
    nreaders_ = npe/2
  endif
  numcases = size(filename)
  nsig1 = nsig+1

  call init_()

  mm1=mype+1

  allocate(z4all(iglobal,nsig1o))

  if(nreaders_>npe) then
     nreaders_ = 1
  endif
  if(mod(npe,nreaders_)/=0) then
    call die(myname,': nreaders should divide npe whole, aborting',99)
  endif
  nskip=1
  allocate(nprocs(nreaders_))
  do ii=1,nreaders_
     nprocs(ii)=(ii-1)*npe/nreaders_
  enddo
  ii=0;ns=1
  do while (ns<=numcases)
     do ii=1,nreaders_,nskip
        if(ns<=numcases) then
           call read_(ns,nprocs(ii))
        endif
        ns=ns+1
     enddo
     call mpi_barrier(gsi_mpi_comm_world,iret) ! shouldn''t need this barrier
     ns=ns-nreaders_/nskip
     do ii=1,nreaders_,nskip
        if(ns<=numcases) then
           call scatter_(ns,nprocs(ii))
        endif
        ns=ns+1
     enddo
     call mpi_barrier(gsi_mpi_comm_world,iret) ! shouldn''t need this barrier
  enddo
  deallocate(nprocs)
  deallocate(z4all)

  call mpi_barrier(gsi_mpi_comm_world,iret)

  call final_()

  return
contains

 subroutine init_

  integer kchk,n

  allocate(spec_send(npe),disp_spec(npe))   

  nsig1o=(nsig*max(0,nc3d))+max(nc2d,0)
  if (mod(nsig1o,npe)==0) then
    kchk=npe
  else
    kchk=mod(nsig1o,npe)
  end if

  do n=1,npe
    if (n.le.kchk) then
      spec_send(n) = iglobal*nsig1o
    else
      spec_send(n) = iglobal*(nsig1o-1)
    end if
  end do

  disp_spec(1)=0
  do n=1,npe
    if(n/=1) then
      disp_spec(n)=disp_spec(n-1)+spec_send(n-1)
    end if
  end do

 end subroutine init_

 subroutine read_(n,proc1)

  implicit none

  integer, intent(in) :: n, proc1

  character(len=*),parameter :: myname_ = myname//'*read_' 
  integer i,j,k,ii,ke,nv,istatus

    if(mype/=proc1) return

    write(6,'(2a,1x,i5,1x,a)') myname_, ': reading file on PE= ', mype, trim(filename(n))
    call nc_GEOSens_read(filename(n),evars,istatus, myid=mype,root=proc1,gsiset=.true.)
    if(istatus/=0) call die(myname_,' failed reading ensemble',99)
    if (evars%nlat/=nlat .or. evars%nlon/=nlon .or. evars%nsig/=nsig) then
       print *, myname_, ': inconsistent dims (code): ', nlat,nlon,nsig
       print *, myname_, ': inconsistent dims (file): ', evars%nlat,evars%nlon,evars%nsig
       call die(myname_,' aborting ...',999)
    endif
      
!   call mpi_barrier(gsi_mpi_comm_world,iret)

    ke=0
    do nv=1,nc3d
       if (cvars3d(nv)=='sf') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%u(i,j,k)
             enddo
             enddo
          enddo
          ke=ke+nsig
       endif
       if (cvars3d(nv)=='vp') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%v(i,j,k)
             enddo
             enddo
          enddo
          ke=ke+nsig
       endif
       if (cvars3d(nv)=='t') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%tv(i,j,k)
             enddo
             enddo
          enddo
          ke=ke+nsig
       endif
       if (cvars3d(nv)=='q') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%qv(i,j,k)
             enddo
             enddo
          enddo
          ke=ke+nsig
       endif
       if (cvars3d(nv)=='oz') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%oz(i,j,k)
             enddo
             enddo
          enddo
          ke=ke+nsig
       endif
       if (cvars3d(nv)=='qi') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%qi(i,j,k)
             enddo
             enddo
          enddo
          ke=ke+nsig
       endif
       if (cvars3d(nv)=='ql') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%ql(i,j,k)
             enddo
             enddo
          enddo
          ke=ke+nsig
       endif
       if (cvars3d(nv)=='qr') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%qr(i,j,k)
             enddo
             enddo
          enddo
          ke=ke+nsig
       endif
       if (cvars3d(nv)=='qs') then
          do k=1,nsig
             ii =0
             do j=1,nlon
             do i=1,nlat
                ii = ii +1
                z4all(ii,ke+k)=evars%qs(i,j,k)
             end do
             end do
          enddo
          ke=ke+nsig
       endif
    enddo

    do nv=1,nc2d
       if (cvars2d(nv)=='ps') then
          ii=0
          do j=1,nlon
          do i=1,nlat
             ii = ii+1
             z4all(ii,ke+1)=evars%ps(i,j)
          enddo
          enddo
          ke=ke+1
       endif
       if (cvars2d(nv)=='ts') then
          ii=0
          do j=1,nlon
          do i=1,nlat
             ii = ii+1
             z4all(ii,ke+1)=evars%ts(i,j)
          enddo
          enddo
          ke=ke+1
       endif
    enddo

    call nc_GEOSens_vars_final(evars)
    write(6,'(2a,i5)') myname_, ': done handling file on PE= ', mype

 end subroutine read_

 subroutine scatter_(n,proc1)
  integer, intent(in) :: n, proc1

! local variables
  character(len=*),parameter :: myname_ = myname//'*scatter_' 
  real(r_kind),allocatable,dimension(:,:)   :: z41
  integer ierror

  allocate(z41(iglobal,nsig1o))

  call grid2subNvar_ ( reshape(z4all,(/nlat,nlon,nsig1o/)), xx(n), mype, proc1 )

  deallocate(z41)

 end subroutine scatter_

 subroutine final_
   deallocate(spec_send,disp_spec)
 end subroutine final_

end subroutine read_geosens_

subroutine grid2subNvar_ ( zvec, xx, mype, proc1 )
  implicit none
  real(r_kind),intent(in) :: zvec(:,:,:)
  type(gsi_bundle),intent(inout) :: xx
  integer,intent(in) :: mype
  integer,intent(in) :: proc1

  character(len=*),parameter :: myname_ = myname//'*grid2subNvar_' 
  real(r_kind),allocatable :: aux(:,:)
  integer ie,is,jj,nv,istatus

  is=0
  ie=0
  do nv=1,nc3d
    call gsi_bundlegetpointer(xx,cvars3d(nv),  jj,  istatus)
    if(istatus==0) then
      is=ie+1
      ie=ie+nsig
      call grid2sub1var ( zvec(:,:,is:ie), xx%r3(jj)%q, proc1, istatus )
    endif
  enddo

  do nv=1,nc2d
     call gsi_bundlegetpointer(xx,cvars2d(nv), jj,  istatus)
     if(istatus==0) then
       ie=ie+1
       allocate(aux(size(xx%r2(jj)%q,1),size(xx%r2(jj)%q,2)))
       aux=zvec(:,:,ie)
       call grid2sub1var ( aux, xx%r2(jj)%q, proc1, istatus )
       deallocate(aux)
     endif
  enddo
  
end subroutine grid2subNvar_

end module m_read_geosens
