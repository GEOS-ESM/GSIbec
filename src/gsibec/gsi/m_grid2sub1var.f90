module m_grid2sub1var
use m_kinds, only: i_kind,r_kind
use m_mpimod, only: mype,mpi_rtype,gsi_mpi_comm_world
use gridmod, only: sg=>grd_a
use mpeu_util, only: die 
implicit none
public :: grid2sub1var
interface grid2sub1var 
  module procedure grid2sub3d_
  module procedure grid2sub2d_
end interface
character(len=*),parameter :: myname='m_grid2sub1var'
contains
      subroutine grid2sub3d_ ( fld, sub, proc, stat_ )

      real(r_kind),   intent(in)   :: fld(:,:,:) ! only exists on proc
      real(r_kind),   intent(out)  :: sub(:,:,:)
      integer(i_kind),intent(in)   :: proc
      integer(i_kind),intent(out)  :: stat_

      character(len=*), parameter :: myname_ = myname//'*grid2sub3d_'

      real(r_kind), allocatable :: work2d(:,:)       ! auxiliar 2d array
      real(r_kind), allocatable :: work(:)
      integer(i_kind) i,j,k,mm1,ierr

      mm1 = mype+1 
      stat_= 0

!     Swap work memory
!     ----------------
      allocate ( work(sg%itotsub), stat=ierr )
!     allocate ( work(sg%iglobal), stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 91
            if(mype==proc) print*, trim(myname_), ': Alloc(work)'
            return
        end if
      allocate ( work2d(sg%lat2,sg%lon2), stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 91
            if(mype==proc) print*, trim(myname_), ': Alloc(work2d)'
            return
        end if

!     Scatter to GSI subdomains
!     -------------------------
      do k=1,sg%nsig
         if (mype==proc) then
             call reorder21_(sg,fld(:,:,k),work)
         endif
         call mpi_scatterv(work,sg%ijn_s,sg%displs_s,mpi_rtype,&
                           work2d,sg%ijn_s(mm1),mpi_rtype,proc,&
                           gsi_mpi_comm_world,ierr)
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
            if(mype==proc) print*, trim(myname_), ': Dealloc(work2d)'
            return
        end if
      deallocate ( work, stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 99
            if(mype==proc) print*, trim(myname_), ': delloc(work)'
            return
        end if

      end subroutine grid2sub3d_

      subroutine grid2sub2d_ ( fld, sub, proc, stat_ )

      real(r_kind),   intent(in)   :: fld(:,:) ! only exists on proc
      real(r_kind),   intent(out)  :: sub(:,:)
      integer(i_kind),intent(in)   :: proc
      integer(i_kind),intent(out)  :: stat_

      character(len=*), parameter :: myname_ = myname//'*grid2sub2d_'

      real(r_kind), allocatable :: work2d(:,:)       ! auxiliar 2d array
      real(r_kind), allocatable :: work(:)
      integer(i_kind) i,j,k,mm1,ierr

      mm1 = mype+1 
      stat_= 0

!     Swap work memory
!     ----------------
      allocate ( work(sg%itotsub), stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 91
            if(mype==proc) print*, trim(myname_), ': Alloc(work)'
            return
        end if
      allocate ( work2d(sg%lat2,sg%lon2), stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 91
            if(mype==proc) print*, trim(myname_), ': Alloc(work2d)'
            return
        end if

!     Scatter to GSI subdomains
!     -------------------------
      if (mype==proc) then
          call reorder21_(sg,fld(:,:),work)
      endif
      call mpi_scatterv(work,sg%ijn_s,sg%displs_s,mpi_rtype,&
                        work2d,sg%ijn_s(mm1),mpi_rtype,proc,&
                        gsi_mpi_comm_world,ierr)
      do j=1,sg%lon2
         do i=1,sg%lat2
            sub(i,j) = work2d(i,j)
         end do
      end do

!     Release work memory
!     -------------------
      deallocate ( work2d, stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 99
            if(mype==proc) print*, trim(myname_), ': Dealloc(work2d)'
            return
        end if
      deallocate ( work, stat=ierr )
        if ( ierr/=0 ) then
            stat_ = 99
            if(mype==proc) print*, trim(myname_), ': delloc(work)'
            return
        end if

      end subroutine grid2sub2d_

      subroutine reorder21_(sg,grid_in,grid_out)
      use m_kinds, only: r_kind
      use general_sub2grid_mod, only: sub2grid_info
      implicit none
   
      type(sub2grid_info),                    intent(in  ) :: sg        ! subdomain info
!     real(r_kind),dimension(sg%nlon,sg%nlat),intent(in  ) :: grid_in   ! input grid
!     real(r_kind),dimension(sg%nlat,sg%nlon),intent(in  ) :: grid_in   ! input grid
!     real(r_kind),dimension(sg%itotsub)     ,intent( out) :: grid_out  ! output gridend subroutine reorder21_
      real(r_kind),dimension(:,:),intent(in  ) :: grid_in   ! input grid
      real(r_kind),dimension(:)  ,intent( out) :: grid_out  ! output gridend subroutine reorder21_

!     Declare local variables
      integer(i_kind) i,j,k

!     Transfer input 2d array to output 1d array
      do k=1,sg%itotsub
         i=sg%ltosi_s(k)
         j=sg%ltosj_s(k)
         if (i>sg%nlat.or.j>sg%nlon) then
           call die('reorder21',': dims error',99)
         endif
         grid_out(k)=grid_in(i,j)
      end do

      end subroutine reorder21_

end module m_grid2sub1var
