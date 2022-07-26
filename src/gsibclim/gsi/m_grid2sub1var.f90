module m_grid2sub1var
use m_kinds, only: i_kind,r_kind
use m_mpimod, only: mype,mpi_rtype,gsi_mpi_comm_world
use gridmod, only: sg=>grd_a
implicit none
public :: grid2sub1var
interface grid2sub1var; module procedure grid2sub_; end interface
integer(i_kind), parameter :: ROOT=0
character(len=*),parameter :: myname='m_grid2sub1var'
contains
      subroutine grid2sub_ ( fld, sub, stat_ )

      real(r_kind),   intent(in)  :: fld(:,:,:)
      real(r_kind),   intent(out) :: sub(:,:,:)
      integer(i_kind),intent(out) :: stat_

      character(len=*), parameter :: myname_ = myname//'*pert2gsi_'
!     type(sub2grid_info) :: sg

      real(r_kind), allocatable :: work3d(:,:,:)     ! auxiliar 3d array
      real(r_kind), allocatable :: work2d(:,:)       ! auxiliar 2d array
      real(r_kind), allocatable :: work(:)
      integer(i_kind) i,j,k,mm1,ierr
      integer(i_kind) imr,jnp,nl

      mm1 = mype+1
      stat_= 0
      imr  =sg%nlon
      jnp  =sg%nlat
      nl   =sg%nsig

      allocate ( work3d(sg%nlon,sg%nlat,nl), stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 91
            if(mype==ROOT) print*, trim(myname_), ': Alloc(work3d)'
            return
        end if

!     Gather GCM perturbations to root processor
!     ------------------------------------------
      call mygather3d_( fld, work3d )
!     if (mype == ROOT) then
!        work3d=fld
!     endif

!     Flip horizontal and vertical
!     ----------------------------
      if ( mype==ROOT ) then
!          call hflip3_ ( work3d, imr,jnp,nl )
!          call SwapV_ ( work3d )
      endif

!     Swap work memory
!     ----------------
      allocate ( work(sg%itotsub), stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 91
            if(mype==ROOT) print*, trim(myname_), ': Alloc(work)'
            return
        end if
      allocate ( work2d(sg%lat2,sg%lon2), stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 91
            if(mype==ROOT) print*, trim(myname_), ': Alloc(work2d)'
            return
        end if

!     Scatter to GSI subdomains
!     -------------------------
      do k=1,nl
         if (mype==ROOT) then
             call reorder21_(sg,work3d(:,:,k),work)
         endif
         call mpi_scatterv(work,sg%ijn_s,sg%displs_s,mpi_rtype,&
              work2d,sg%ijn_s(mm1),mpi_rtype,root,gsi_mpi_comm_world,ierr)
         do j=1,sg%lon2
            do i=1,sg%lat2
               sub(i,j,k) = work2d(i,j)
            end do
         end do
      end do

!     Release work memory
!     -------------------
      deallocate ( work2d, stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 99
            if(mype==ROOT) print*, trim(myname_), ': Dealloc(work2d)'
            return
        end if
      deallocate ( work, stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 99
            if(mype==ROOT) print*, trim(myname_), ': delloc(work)'
            return
        end if
      deallocate ( work3d, stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 99
            if(mype==ROOT) print*, trim(myname_), ': delloc(work3d)'
            return
        end if

      end subroutine grid2sub_

      subroutine reorder21_(sg,grid_in,grid_out)
      use m_kinds, only: r_kind,r_double
      use general_sub2grid_mod, only: sub2grid_info
      implicit none

      type(sub2grid_info),                    intent(in  ) :: sg        ! subdomain info
      real(r_kind),dimension(sg%nlon,sg%nlat),intent(in  ) :: grid_in   ! input grid
      real(r_kind),dimension(sg%itotsub)     ,intent( out) :: grid_out  ! output gridend subroutine reorder21_

!     Declare local variables
      integer(i_kind) i,j,k

!     Transfer input 2d array to output 1d array
      do k=1,sg%itotsub
         i=sg%ltosi_s(k)
         j=sg%ltosj_s(k)
         grid_out(k)=grid_in(j,i)
      end do

      end subroutine reorder21_

      subroutine mygather3d_ (fld,wrk)
      implicit none
      real(r_kind), intent(in)  :: fld(:,:,:)
      real(r_kind), intent(inout) :: wrk(:,:,:)
      integer k,km
      km=size(wrk,3)
      do k=1,km
        call gather_stuff2(fld(1,1,k),wrk(1,1,k),mype,0)
      end do
     end  subroutine mygather3d_
end module m_grid2sub1var
