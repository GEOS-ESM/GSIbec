module TwoPhaseScatterGather_JEDIGlobal
  ! Inspired by a post by Jonathan Dursi at
  ! https://stackoverflow.com/questions/29325513/scatter-matrix-blocks-of-different-sizes-using-mpi
  use mpi
  use, intrinsic :: iso_c_binding
  use, intrinsic :: ieee_arithmetic, only : ieee_is_nan
  use general_sub2grid_mod, only: sub2grid_info
  use m_mpimod, only: gsi_mpi_comm_world,mype,nxpe,nype
  implicit none

  private :: sub2grid_info,gsi_mpi_comm_world,mype,nxpe,nype

  ! GENERIC INTERFACES
  ! ------------------
  interface TwoPhaseScatter_Phase1
    procedure TwoPhaseScatter_Phase1_r4, TwoPhaseScatter_Phase1_r8
  end interface TwoPhaseScatter_Phase1

  interface TwoPhaseScatter_Phase2
    procedure TwoPhaseScatter_Phase2_r4, TwoPhaseScatter_Phase2_r8
  end interface TwoPhaseScatter_Phase2

  interface TwoPhaseGather_Phase1
    procedure TwoPhaseGather_Phase1_r4, TwoPhaseGather_Phase1_r8
  end interface TwoPhaseGather_Phase1

  interface TwoPhaseGather_Phase2
    procedure TwoPhaseGather_Phase2_r4, TwoPhaseGather_Phase2_r8
  end interface TwoPhaseGather_Phase2

  ! CUSTOM TYPES
  ! ------------
  type :: scatter_t
    logical :: lalloc = .false.
    integer, allocatable :: sendcounts_phase1(:), senddispls_phase1(:)
    integer, allocatable :: sendcounts_phase2(:), senddispls_phase2(:)
    integer :: vec_r4, localvec_r4
    integer :: vec_r8, localvec_r8
  endtype scatter_t

  type :: gather_t
    logical :: lalloc = .false.
    integer, allocatable :: recvcounts_phase1(:), recvdispls_phase1(:)
    integer, allocatable :: recvcounts_phase2(:), recvdispls_phase2(:)
    integer :: vec_r4, localvec_r4
    integer :: vec_r8, localvec_r8
  endtype gather_t

  ! COMMUNICATION STATE OF ONE READ OR ONE WRITE
  ! --------------------------------------------
  ! Everything the two-phase scatter/gather needs for ONE geometry.  Each read or write call
  ! owns one instance: it initializes it (TwoPhaseScatter_init or TwoPhaseGather_init), passes
  ! it to the Phase1/Phase2 routines, and frees it with the matching delete routine before
  ! returning.  Nothing is shared between calls, so a read and a write (or two grids) can no
  ! longer see each other's sizes, communicators or workspaces.
  !
  ! Declare instances TARGET, ASYNCHRONOUS.  The workspaces and the per-owner count and
  ! displacement arrays are handed to nonblocking collectives that are completed later by an
  ! MPI_Waitall in the caller.  ASYNCHRONOUS cannot be given to components, so it goes on the
  ! variable (and on every dummy argument that receives it); its subobjects inherit it.  Do not
  ! copy or modify an instance while requests are outstanding.
  type :: twophase_decomp
    ! Per-owner counts, displacements and MPI datatypes (created lazily in Phase1)
    type(scatter_t), allocatable :: TwoPhaseScatter(:)
    type(gather_t), allocatable  :: TwoPhaseGather(:)

    ! Dedicated Scatter Workspaces
    real(kind=4), allocatable :: scatter_workspace_r4(:,:,:)
    real(kind=8), allocatable :: scatter_workspace_r8(:,:,:)

    ! Dedicated Gather Workspaces
    real(kind=4), allocatable :: gather_workspace_r4(:,:,:)
    real(kind=8), allocatable :: gather_workspace_r8(:,:,:)

    ! Shared workspaces for local type conversion
    real(kind=4), allocatable :: scatter_recv_cast_workspace_r4(:,:,:)
    real(kind=4), allocatable :: gather_send_cast_workspace_r4(:,:,:)
    real(kind=4), allocatable :: gather_send_cast_workspace_r8(:,:,:)

    ! Geometrical arrays
    integer :: EWindex = -1, NSindex = -1, rowrank = -1, colrank = -1, mype = -1
    integer :: colComm = MPI_COMM_NULL, rowComm = MPI_COMM_NULL
    integer(kind=4), allocatable :: NumColsPerRank(:), NumRowsPerRank(:)
    integer(kind=4), allocatable :: MyRowGlobal(:), MyColGlobal(:)
    integer(kind=4), allocatable :: MyRankInRowComm(:), MyRankInColComm(:)
    integer, allocatable :: ibegin(:), iend(:), jbegin(:), jend(:)
    integer :: pe_layout(2) = 0, globalsizes(2) = 0, localsizes(2) = 0
    integer :: gather_hw = 0
    integer :: scatter_hw = 0
    integer :: alloc_localsizes(2) = 0
    integer :: my_gather_start(2) = 0
    integer :: my_gather_count(2) = 0

    ! Generic Contiguous vs Strided Mapping
    integer :: contig_comm = MPI_COMM_NULL
    integer :: strided_comm = MPI_COMM_NULL
    integer(kind=4), allocatable :: MyRankInContigComm(:)
    integer(kind=4), allocatable :: MyRankInStridedComm(:)
  end type twophase_decomp

contains

  ! SUBROUTINES
  ! -----------
  subroutine TwoPhaseScatter_init(decomp, sgrid, npes, pe_layout, batch_size, halo_width)
  use ifport, only : flush
    implicit none
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    type(sub2grid_info), intent(in) :: sgrid
    integer, intent(in) :: npes, pe_layout(2), batch_size
    integer, intent(in), optional :: halo_width
    integer :: k, r, hw, ierr, res
    integer :: rowSize, colSize
    integer :: myRowRank, myColRank
    integer ::  wrank, wsize
    integer :: temptype, col, row, myrow, mycol
    integer(kind=MPI_ADDRESS_KIND) :: lb, extent

    ! A decomp is initialized once and freed by the matching delete routine
    if (decomp%rowComm /= MPI_COMM_NULL .or. decomp%colComm /= MPI_COMM_NULL) then
      write(6,'("TwoPhaseScatter_init: decomp is already initialized; call TwoPhaseScatter_delete first")')
      call flush(6)
      call MPI_Abort(gsi_mpi_comm_world, 50, ierr)
    endif

    hw=0
    if(present(halo_width)) hw=halo_width
    decomp%scatter_hw = hw

    call MPI_Comm_rank(gsi_mpi_comm_world, wrank, ierr)
    call MPI_Comm_size(gsi_mpi_comm_world, wsize, ierr)
    decomp%mype=wrank

    decomp%pe_layout = pe_layout
    decomp%EWindex = modulo(wrank,decomp%pe_layout(1))
    decomp%NSindex = (wrank/decomp%pe_layout(1))

    ! rowComm spans E-W (Longitudes). Group by Y (NSindex).
    call MPI_Comm_split(gsi_mpi_comm_world, decomp%NSindex, wrank, decomp%rowComm, ierr)
    ! colComm spans N-S (Latitudes). Group by X (EWindex).
    call MPI_Comm_split(gsi_mpi_comm_world, decomp%EWindex, wrank, decomp%colComm, ierr)

    call MPI_Comm_size(decomp%rowComm, rowSize, ierr)
    call MPI_Comm_size(decomp%colComm, colSize, ierr)

    ! GSI Layout: Latitude (dim 1) is gathered over colComm, Longitude (dim 2) over rowComm
    allocate(decomp%ibegin(0:colSize-1), decomp%iend(0:colSize-1))
    call MPI_AllGather(sgrid%istart(decomp%mype+1), 1, MPI_Integer, decomp%ibegin(0:), 1, MPI_Integer, decomp%colComm, ierr)
    call MPI_AllGather(sgrid%istart(decomp%mype+1) + sgrid%ilat1(decomp%mype+1) - 1, 1, MPI_Integer, decomp%iend(0:), 1, MPI_Integer, decomp%colComm, ierr)

    allocate(decomp%jbegin(0:rowSize-1), decomp%jend(0:rowSize-1))
    call MPI_AllGather(sgrid%jstart(decomp%mype+1), 1, MPI_Integer, decomp%jbegin(0:), 1, MPI_Integer, decomp%rowComm, ierr)
    call MPI_AllGather(sgrid%jstart(decomp%mype+1) + sgrid%jlon1(decomp%mype+1) - 1, 1, MPI_Integer, decomp%jend(0:), 1, MPI_Integer, decomp%rowComm, ierr)

    ! Let other ranks know my row and column index
    allocate(decomp%MyRowGlobal(0:wsize-1), decomp%MyColGlobal(0:wsize-1))
    decomp%MyRowGlobal=-999; decomp%MyColGlobal=-999
    call MPI_AllGather(decomp%NSindex,1,MPI_Integer,decomp%MyRowGlobal,1,MPI_Integer, gsi_mpi_comm_world, ierr)
    call MPI_AllGather(decomp%EWindex,1,MPI_Integer,decomp%MyColGlobal,1,MPI_Integer, gsi_mpi_comm_world, ierr)

    ! Let other ranks know my rank in the row and column communicators
    call MPI_Comm_rank(decomp%rowComm, decomp%rowrank, ierr)
    call MPI_Comm_rank(decomp%colComm, decomp%colrank, ierr)
    allocate(decomp%MyRankInRowComm(0:wsize-1), decomp%MyRankInColComm(0:wsize-1))
    decomp%MyRankInRowComm=-999; decomp%MyRankInColComm=-999
    call MPI_AllGather(decomp%rowrank,1,MPI_Integer,decomp%MyRankInRowComm,1,MPI_Integer, gsi_mpi_comm_world, ierr)
    call MPI_AllGather(decomp%colrank,1,MPI_Integer,decomp%MyRankInColComm,1,MPI_Integer, gsi_mpi_comm_world, ierr)

    ! dimensions of my subdomain
    call MPI_Comm_rank(decomp%rowComm, myRowRank, ierr)
    call MPI_Comm_rank(decomp%colComm, myColRank, ierr)

    decomp%localsizes(1) = decomp%iend(myColRank) - decomp%ibegin(myColRank) + 1 + 2*hw  ! Latitudes
    decomp%localsizes(2) = decomp%jend(myRowRank) - decomp%jbegin(myRowRank) + 1 + 2*hw  ! Longitudes

    ! Let other ranks in my row and column know how many rows and columns I have in my subdomain
    allocate(decomp%NumColsPerRank(0:rowSize-1))
    allocate(decomp%NumRowsPerRank(0:colSize-1))
    decomp%NumColsPerRank=-999; decomp%NumRowsPerRank=-999
    call MPI_Allgather(decomp%localsizes(1), 1, MPI_Integer, decomp%NumRowsPerRank, 1, MPI_Integer, decomp%colComm, ierr)
    call MPI_Allgather(decomp%localsizes(2), 1, MPI_Integer, decomp%NumColsPerRank, 1, MPI_Integer, decomp%rowComm, ierr)

    ! Horizontal dimensions of global fields in the application space.  GSI and UFS are different
    decomp%globalsizes(1) = sgrid%nlat + 2*hw
    decomp%globalsizes(2) = sgrid%nlon + 2*hw

    ! Allocate the structure
    ! ----------------------
    if (.not. allocated(decomp%TwoPhaseScatter)) allocate(decomp%TwoPhaseScatter(0:npes-1))

    ! Initialize all MPI Datatype handles to NULL
    ! (Guarantees safe datatype creation and teardown checks)
    ! -------------------------------------------------------
    do r = 0, npes-1
      ! Scatter handles
      decomp%TwoPhaseScatter(r)%localvec_r4 = MPI_DATATYPE_NULL
      decomp%TwoPhaseScatter(r)%localvec_r8 = MPI_DATATYPE_NULL
      decomp%TwoPhaseScatter(r)%vec_r4      = MPI_DATATYPE_NULL
      decomp%TwoPhaseScatter(r)%vec_r8      = MPI_DATATYPE_NULL
      decomp%TwoPhaseScatter(r)%lalloc      = .false.
    enddo

    ! Allocate the shared workspaces.  batch_size*2 for double-buffered approach
    if (.not. allocated(decomp%scatter_workspace_r4)) then
      allocate(decomp%scatter_workspace_r4(decomp%globalsizes(1), decomp%localsizes(2), batch_size*2))
      !decomp%scatter_workspace_r4 = 0.0_4
    endif
    if (.not. allocated(decomp%scatter_workspace_r8)) then
      allocate(decomp%scatter_workspace_r8(decomp%globalsizes(1), decomp%localsizes(2), batch_size*2))
      !decomp%scatter_workspace_r8 = 0.0_8
    endif
    if (.not. allocated(decomp%scatter_recv_cast_workspace_r4)) then
      allocate(decomp%scatter_recv_cast_workspace_r4(decomp%localsizes(1), decomp%localsizes(2), batch_size*2))
      !decomp%scatter_recv_cast_workspace_r4 = 0.0_4
    endif

    ! Set up the generic contiguous/strided routing once
    if (.not. allocated(decomp%MyRankInContigComm)) allocate(decomp%MyRankInContigComm(0:wsize-1))
    if (.not. allocated(decomp%MyRankInStridedComm)) allocate(decomp%MyRankInStridedComm(0:wsize-1))
    ! GSI Layout (lat, lon):
    ! The 1st dimension (lat) is contiguous.
    ! A slice of the 2nd dimension (lon) forms a solid contiguous block of memory.
    ! Therefore, Phase 1 MUST scatter along Longitudes (rowComm).
    decomp%contig_comm  = decomp%rowComm
    decomp%strided_comm = decomp%colComm
    decomp%MyRankInContigComm  = decomp%MyRankInRowComm
    decomp%MyRankInStridedComm = decomp%MyRankInColComm

    ! -------------------------------------------------------
    ! PRE-BAKE: Setup counts, displacements, and datatypes 
    ! for every potential rank ownership mapping up-front.
    ! -------------------------------------------------------
    myrow = decomp%NSindex
    mycol = decomp%EWindex

    do r = 0, npes-1
      associate(ts => decomp%TwoPhaseScatter(r))
        ! --- Phase 1 & 2 Counts/Displacements ---
        if (myrow == decomp%MyRowGlobal(r)) then
          allocate(ts%sendcounts_phase1(0:decomp%pe_layout(1)-1))
          allocate(ts%senddispls_phase1(0:decomp%pe_layout(1)-1))
          do col = 0, decomp%pe_layout(1)-1
            ts%sendcounts_phase1(col) = decomp%globalsizes(1) * decomp%NumColsPerRank(col)
            ts%senddispls_phase1(col) = decomp%globalsizes(1) * (decomp%jbegin(col) - 1)
          enddo
        endif

        allocate(ts%sendcounts_phase2(0:decomp%pe_layout(2)-1))
        allocate(ts%senddispls_phase2(0:decomp%pe_layout(2)-1))
        do row = 0, decomp%pe_layout(2)-1
          ts%sendcounts_phase2(row) = decomp%NumRowsPerRank(row)
          ts%senddispls_phase2(row) = (decomp%ibegin(row) - 1)
        enddo
        ts%lalloc = .true.

        ! --- Datatypes ---
        if (myrow == decomp%MyRowGlobal(r)) then
          ! vec_r4
          lb=0; extent=4
          call MPI_Type_vector(decomp%NumColsPerRank(mycol), 1, decomp%globalsizes(1), MPI_REAL, temptype, ierr)
          call MPI_Type_create_resized(temptype, lb, extent, ts%vec_r4, ierr)
          call MPI_Type_commit(ts%vec_r4, ierr)
          call MPI_Type_free(temptype, ierr)
          
          ! vec_r8
          lb=0; extent=8
          call MPI_Type_vector(decomp%NumColsPerRank(mycol), 1, decomp%globalsizes(1), MPI_DOUBLE_PRECISION, temptype, ierr)
          call MPI_Type_create_resized(temptype, lb, extent, ts%vec_r8, ierr)
          call MPI_Type_commit(ts%vec_r8, ierr)
          call MPI_Type_free(temptype, ierr)
        endif

        ! localvec_r4
        lb=0; extent=4
        call MPI_Type_vector(decomp%NumColsPerRank(mycol), 1, decomp%localsizes(1), MPI_REAL, temptype, ierr)
        call MPI_Type_create_resized(temptype, lb, extent, ts%localvec_r4, ierr)
        call MPI_Type_commit(ts%localvec_r4, ierr)
        call MPI_Type_free(temptype, ierr)

        ! localvec_r8
        lb=0; extent=8
        call MPI_Type_vector(decomp%NumColsPerRank(mycol), 1, decomp%localsizes(1), MPI_DOUBLE_PRECISION, temptype, ierr)
        call MPI_Type_create_resized(temptype, lb, extent, ts%localvec_r8, ierr)
        call MPI_Type_commit(ts%localvec_r8, ierr)
        call MPI_Type_free(temptype, ierr)
      end associate
    enddo

  end subroutine TwoPhaseScatter_init


  subroutine TwoPhaseGather_init(decomp, sgrid, npes, pe_layout, batch_size, halo_width)
  use ifport, only : flush
    implicit none
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    type(sub2grid_info), intent(in) :: sgrid
    integer, intent(in) :: npes, pe_layout(2), batch_size
    integer, intent(in), optional :: halo_width
    integer :: k, r, ierr, res
    integer :: rowSize, colSize
    integer :: myRowRank, myColRank
    integer ::  wrank, wsize
    integer :: temptype, col, row, myrow, mycol
    integer(kind=MPI_ADDRESS_KIND) :: lb, extent

    ! A decomp is initialized once and freed by the matching delete routine
    if (decomp%rowComm /= MPI_COMM_NULL .or. decomp%colComm /= MPI_COMM_NULL) then
      write(6,'("TwoPhaseGather_init: decomp is already initialized; call TwoPhaseGather_delete first")')
      call flush(6)
      call MPI_Abort(gsi_mpi_comm_world, 50, ierr)
    endif

    decomp%gather_hw=0
    if(present(halo_width)) decomp%gather_hw = halo_width

    call MPI_Comm_rank(gsi_mpi_comm_world, wrank, ierr)
    call MPI_Comm_size(gsi_mpi_comm_world, wsize, ierr)
    decomp%mype = wrank

    decomp%pe_layout = pe_layout
    decomp%EWindex = modulo(wrank,pe_layout(1))
    decomp%NSindex = (wrank/pe_layout(1))

    call MPI_Comm_split(gsi_mpi_comm_world, decomp%NSindex, wrank, decomp%rowComm, ierr)
    call MPI_Comm_split(gsi_mpi_comm_world, decomp%EWindex, wrank, decomp%colComm, ierr)

    call MPI_Comm_size(decomp%rowComm, rowSize, ierr)
    call MPI_Comm_size(decomp%colComm, colSize, ierr)
    decomp%pe_layout(1) = rowSize
    decomp%pe_layout(2) = colSize

    ! Allocate based on actual comm sizes.  Guarded so that a read-after-write (or any
    ! second init without an intervening delete) re-sizes instead of aborting.
    if (allocated(decomp%ibegin)) deallocate(decomp%ibegin)
    if (allocated(decomp%iend))   deallocate(decomp%iend)
    if (allocated(decomp%jbegin)) deallocate(decomp%jbegin)
    if (allocated(decomp%jend))   deallocate(decomp%jend)

    ! GSI Layout: Latitude (dim 1) is gathered over colComm, Longitude (dim 2) over rowComm
    allocate(decomp%ibegin(0:colSize-1), decomp%iend(0:colSize-1))
    call MPI_AllGather(sgrid%istart(decomp%mype+1), 1, MPI_Integer, decomp%ibegin(0:), 1, MPI_Integer, decomp%colComm, ierr)
    call MPI_AllGather(sgrid%istart(decomp%mype+1) + sgrid%ilat1(decomp%mype+1) - 1, 1, MPI_Integer, decomp%iend(0:), 1, MPI_Integer, decomp%colComm, ierr)

    allocate(decomp%jbegin(0:rowSize-1), decomp%jend(0:rowSize-1))
    call MPI_AllGather(sgrid%jstart(decomp%mype+1), 1, MPI_Integer, decomp%jbegin(0:), 1, MPI_Integer, decomp%rowComm, ierr)
    call MPI_AllGather(sgrid%jstart(decomp%mype+1) + sgrid%jlon1(decomp%mype+1) - 1, 1, MPI_Integer, decomp%jend(0:), 1, MPI_Integer, decomp%rowComm, ierr)

    ! Let other ranks know my row and column index
    if (allocated(decomp%MyRowGlobal)) deallocate(decomp%MyRowGlobal)
    if (allocated(decomp%MyColGlobal)) deallocate(decomp%MyColGlobal)
    allocate(decomp%MyRowGlobal(0:wsize-1), decomp%MyColGlobal(0:wsize-1))
    decomp%MyRowGlobal=-999; decomp%MyColGlobal=-999
    call MPI_AllGather(decomp%NSindex,1,MPI_Integer,decomp%MyRowGlobal,1,MPI_Integer, gsi_mpi_comm_world, ierr)
    call MPI_AllGather(decomp%EWindex,1,MPI_Integer,decomp%MyColGlobal,1,MPI_Integer, gsi_mpi_comm_world, ierr)

    ! Let other ranks know my rank in the row and column communicators
    call MPI_Comm_rank(decomp%rowComm, decomp%rowrank, ierr)
    call MPI_Comm_rank(decomp%colComm, decomp%colrank, ierr)
    if (allocated(decomp%MyRankInRowComm)) deallocate(decomp%MyRankInRowComm)
    if (allocated(decomp%MyRankInColComm)) deallocate(decomp%MyRankInColComm)
    allocate(decomp%MyRankInRowComm(0:wsize-1), decomp%MyRankInColComm(0:wsize-1))
    decomp%MyRankInRowComm=-999; decomp%MyRankInColComm=-999
    call MPI_AllGather(decomp%rowrank,1,MPI_Integer,decomp%MyRankInRowComm,1,MPI_Integer, gsi_mpi_comm_world, ierr)
    call MPI_AllGather(decomp%colrank,1,MPI_Integer,decomp%MyRankInColComm,1,MPI_Integer, gsi_mpi_comm_world, ierr)

    ! Compute memory allocation sizes (Interior + all halos)
    call MPI_Comm_rank(decomp%rowComm, myRowRank, ierr)
    call MPI_Comm_rank(decomp%colComm, myColRank, ierr)
    decomp%alloc_localsizes(1) = (decomp%iend(myColRank) - decomp%ibegin(myColRank) + 1) + 2*decomp%gather_hw
    decomp%alloc_localsizes(2) = (decomp%jend(myRowRank) - decomp%jbegin(myRowRank) + 1) + 2*decomp%gather_hw

    ! Rank-aware gather extents: Unconditionally strip ALL halos (interior and perimeter)
    decomp%my_gather_start(1) = 1 + decomp%gather_hw
    decomp%my_gather_count(1) = decomp%alloc_localsizes(1) - 2 * decomp%gather_hw

    decomp%my_gather_start(2) = 1 + decomp%gather_hw
    decomp%my_gather_count(2) = decomp%alloc_localsizes(2) - 2 * decomp%gather_hw

    ! Broadcast the rank-aware counts
    if (allocated(decomp%NumColsPerRank)) deallocate(decomp%NumColsPerRank)
    if (allocated(decomp%NumRowsPerRank)) deallocate(decomp%NumRowsPerRank)
    allocate(decomp%NumColsPerRank(0:rowSize-1))
    allocate(decomp%NumRowsPerRank(0:colSize-1))
    decomp%NumColsPerRank=-999; decomp%NumRowsPerRank=-999

    call MPI_Allgather(decomp%my_gather_count(1), 1, MPI_Integer, decomp%NumRowsPerRank, 1, MPI_Integer, decomp%colComm, ierr)
    call MPI_Allgather(decomp%my_gather_count(2), 1, MPI_Integer, decomp%NumColsPerRank, 1, MPI_Integer, decomp%rowComm, ierr)

    ! Global sizes dynamically sum the preserved perimeter halos and exact interior domains
    decomp%globalsizes(1) = sum(decomp%NumRowsPerRank)
    decomp%globalsizes(2) = sum(decomp%NumColsPerRank)

    ! Allocate the structure
    if (.not. allocated(decomp%TwoPhaseGather))  allocate(decomp%TwoPhaseGather(0:npes-1))

    ! Initialize all MPI Datatype handles to NULL
    ! (Guarantees safe datatype creation and teardown checks)
    ! -------------------------------------------------------
    do r = 0, npes-1
      decomp%TwoPhaseGather(r)%localvec_r4  = MPI_DATATYPE_NULL
      decomp%TwoPhaseGather(r)%localvec_r8  = MPI_DATATYPE_NULL
      decomp%TwoPhaseGather(r)%vec_r4       = MPI_DATATYPE_NULL
      decomp%TwoPhaseGather(r)%vec_r8       = MPI_DATATYPE_NULL
      decomp%TwoPhaseGather(r)%lalloc       = .false.
    enddo

    ! Allocate the shared workspaces.  batch_size*2 for double-buffered approach
    if (.not. allocated(decomp%gather_workspace_r4)) then
      allocate(decomp%gather_workspace_r4(decomp%globalsizes(1), decomp%alloc_localsizes(2), batch_size*2))
    endif
    if (.not. allocated(decomp%gather_workspace_r8)) then
      allocate(decomp%gather_workspace_r8(decomp%globalsizes(1), decomp%alloc_localsizes(2), batch_size*2))
    endif
    if (.not. allocated(decomp%gather_send_cast_workspace_r4)) then
      allocate(decomp%gather_send_cast_workspace_r4(decomp%alloc_localsizes(1), decomp%alloc_localsizes(2), batch_size*2))
    endif
    if (.not. allocated(decomp%gather_send_cast_workspace_r8)) then
      allocate(decomp%gather_send_cast_workspace_r8(decomp%alloc_localsizes(1), decomp%alloc_localsizes(2), batch_size*2))
    endif

    ! Set up the generic contiguous/strided routing once
    if (.not. allocated(decomp%MyRankInContigComm)) allocate(decomp%MyRankInContigComm(0:wsize-1))
    if (.not. allocated(decomp%MyRankInStridedComm)) allocate(decomp%MyRankInStridedComm(0:wsize-1))
    ! GSI Layout (lat, lon):
    ! The 1st dimension (lat) is contiguous.
    ! A slice of the 2nd dimension (lon) forms a solid contiguous block of memory.
    ! Therefore, Phase 1 MUST scatter along Longitudes (rowComm).
    decomp%contig_comm  = decomp%rowComm
    decomp%strided_comm = decomp%colComm
    decomp%MyRankInContigComm  = decomp%MyRankInRowComm
    decomp%MyRankInStridedComm = decomp%MyRankInColComm

    ! -------------------------------------------------------
    ! PRE-BAKE: Setup counts, displacements, and datatypes 
    ! for every potential rank ownership mapping up-front.
    ! -------------------------------------------------------
    myrow = decomp%NSindex
    mycol = decomp%EWindex

    do r = 0, npes-1
      associate(ts => decomp%TwoPhaseGather(r))
        ! --- Phase 1 & 2 Counts/Displacements ---
        allocate(ts%recvcounts_phase1(0:decomp%pe_layout(2)-1))
        allocate(ts%recvdispls_phase1(0:decomp%pe_layout(2)-1))
        ts%recvcounts_phase1(0) = decomp%NumRowsPerRank(0)
        ts%recvdispls_phase1(0) = 0
        do row = 1, decomp%pe_layout(2)-1
          ts%recvcounts_phase1(row) = decomp%NumRowsPerRank(row)
          ts%recvdispls_phase1(row) = ts%recvdispls_phase1(row-1) + ts%recvcounts_phase1(row-1)
        enddo
          
        if (myrow == decomp%MyRowGlobal(r)) then
          allocate(ts%recvcounts_phase2(0:decomp%pe_layout(1)-1))
          allocate(ts%recvdispls_phase2(0:decomp%pe_layout(1)-1))
          ts%recvcounts_phase2(0) = decomp%globalsizes(1) * decomp%NumColsPerRank(0)
          ts%recvdispls_phase2(0) = 0
          do col = 1, decomp%pe_layout(1)-1
            ts%recvcounts_phase2(col) = decomp%globalsizes(1) * decomp%NumColsPerRank(col)
            ts%recvdispls_phase2(col) = ts%recvdispls_phase2(col-1) + ts%recvcounts_phase2(col-1)
          enddo
        endif
        ts%lalloc = .true.

        ! --- Datatypes ---
        if (myrow == decomp%MyRowGlobal(r)) then
          ! vec_r4
          lb=0; extent=4
          call MPI_Type_vector(decomp%NumColsPerRank(mycol), 1, decomp%globalsizes(1), MPI_REAL, temptype, ierr)
          call MPI_Type_create_resized(temptype, lb, extent, ts%vec_r4, ierr)
          call MPI_Type_commit(ts%vec_r4, ierr)
          call MPI_Type_free(temptype, ierr)
          
          ! vec_r8
          lb=0; extent=8
          call MPI_Type_vector(decomp%NumColsPerRank(mycol), 1, decomp%globalsizes(1), MPI_DOUBLE_PRECISION, temptype, ierr)
          call MPI_Type_create_resized(temptype, lb, extent, ts%vec_r8, ierr)
          call MPI_Type_commit(ts%vec_r8, ierr)
          call MPI_Type_free(temptype, ierr)
        endif

        ! localvec_r4
        lb=0; extent=4
        call MPI_Type_vector(decomp%NumColsPerRank(mycol), 1, decomp%alloc_localsizes(1), MPI_REAL, temptype, ierr)
        call MPI_Type_create_resized(temptype, lb, extent, ts%localvec_r4, ierr)
        call MPI_Type_commit(ts%localvec_r4, ierr)
        call MPI_Type_free(temptype, ierr)

        ! localvec_r8
        lb=0; extent=8
        call MPI_Type_vector(decomp%NumColsPerRank(mycol), 1, decomp%alloc_localsizes(1), MPI_DOUBLE_PRECISION, temptype, ierr)
        call MPI_Type_create_resized(temptype, lb, extent, ts%localvec_r8, ierr)
        call MPI_Type_commit(ts%localvec_r8, ierr)
        call MPI_Type_free(temptype, ierr)
      end associate
    enddo

  end subroutine TwoPhaseGather_init

  subroutine TwoPhaseScatter_delete(decomp)
    implicit none
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer :: r, ierr

    if (allocated(decomp%TwoPhaseScatter)) then
      do r = lbound(decomp%TwoPhaseScatter, 1), ubound(decomp%TwoPhaseScatter, 1)
        if (allocated(decomp%TwoPhaseScatter(r)%sendcounts_phase1)) deallocate(decomp%TwoPhaseScatter(r)%sendcounts_phase1)
        if (allocated(decomp%TwoPhaseScatter(r)%senddispls_phase1)) deallocate(decomp%TwoPhaseScatter(r)%senddispls_phase1)
        if (allocated(decomp%TwoPhaseScatter(r)%sendcounts_phase2)) deallocate(decomp%TwoPhaseScatter(r)%sendcounts_phase2)
        if (allocated(decomp%TwoPhaseScatter(r)%senddispls_phase2)) deallocate(decomp%TwoPhaseScatter(r)%senddispls_phase2)

        ! Free Datatypes ONLY if they were actually created
        if (decomp%TwoPhaseScatter(r)%localvec_r4 /= MPI_DATATYPE_NULL) call MPI_Type_free(decomp%TwoPhaseScatter(r)%localvec_r4, ierr)
        if (decomp%TwoPhaseScatter(r)%localvec_r8 /= MPI_DATATYPE_NULL) call MPI_Type_free(decomp%TwoPhaseScatter(r)%localvec_r8, ierr)

        if (decomp%TwoPhaseScatter(r)%vec_r4 /= MPI_DATATYPE_NULL) call MPI_Type_free(decomp%TwoPhaseScatter(r)%vec_r4, ierr)
        if (decomp%TwoPhaseScatter(r)%vec_r8 /= MPI_DATATYPE_NULL) call MPI_Type_free(decomp%TwoPhaseScatter(r)%vec_r8, ierr)
      enddo
      deallocate(decomp%TwoPhaseScatter)
    endif
    if (allocated(decomp%scatter_workspace_r4)) deallocate(decomp%scatter_workspace_r4)
    if (allocated(decomp%scatter_workspace_r8)) deallocate(decomp%scatter_workspace_r8)
    if (allocated(decomp%scatter_recv_cast_workspace_r4)) deallocate(decomp%scatter_recv_cast_workspace_r4)

    if(allocated(decomp%ibegin)) deallocate(decomp%ibegin)
    if(allocated(decomp%iend)) deallocate(decomp%iend)
    if(allocated(decomp%jbegin)) deallocate(decomp%jbegin)
    if(allocated(decomp%jend)) deallocate(decomp%jend)
    if(allocated(decomp%MyRowGlobal)) deallocate(decomp%MyRowGlobal)
    if(allocated(decomp%MyColGlobal)) deallocate(decomp%MyColGlobal)
    if(allocated(decomp%MyRankInRowComm)) deallocate(decomp%MyRankInRowComm)
    if(allocated(decomp%MyRankInColComm)) deallocate(decomp%MyRankInColComm)
    if(allocated(decomp%NumColsPerRank)) deallocate(decomp%NumColsPerRank)
    if(allocated(decomp%NumRowsPerRank)) deallocate(decomp%NumRowsPerRank)

    ! Release the row/column communicators created by TwoPhaseScatter_init()
    if (decomp%rowComm /= MPI_COMM_NULL) then
      call MPI_Comm_free(decomp%rowComm, ierr)
      decomp%rowComm = MPI_COMM_NULL
    endif
    if (decomp%colComm /= MPI_COMM_NULL) then
      call MPI_Comm_free(decomp%colComm, ierr)
      decomp%colComm = MPI_COMM_NULL
    endif

    if(allocated(decomp%MyRankInContigComm)) deallocate(decomp%MyRankInContigComm)
    if(allocated(decomp%MyRankInStridedComm)) deallocate(decomp%MyRankInStridedComm)
  end subroutine TwoPhaseScatter_delete

  subroutine TwoPhaseGather_delete(decomp)
    implicit none
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer :: r, ierr

    if (allocated(decomp%TwoPhaseGather)) then
      do r = lbound(decomp%TwoPhaseGather, 1), ubound(decomp%TwoPhaseGather, 1)
        if (allocated(decomp%TwoPhaseGather(r)%recvcounts_phase1)) deallocate(decomp%TwoPhaseGather(r)%recvcounts_phase1)
        if (allocated(decomp%TwoPhaseGather(r)%recvdispls_phase1)) deallocate(decomp%TwoPhaseGather(r)%recvdispls_phase1)
        if (allocated(decomp%TwoPhaseGather(r)%recvcounts_phase2)) deallocate(decomp%TwoPhaseGather(r)%recvcounts_phase2)
        if (allocated(decomp%TwoPhaseGather(r)%recvdispls_phase2)) deallocate(decomp%TwoPhaseGather(r)%recvdispls_phase2)

        ! Free Datatypes ONLY if they were actually created
        if (decomp%TwoPhaseGather(r)%localvec_r4 /= MPI_DATATYPE_NULL) call MPI_Type_free(decomp%TwoPhaseGather(r)%localvec_r4, ierr)
        if (decomp%TwoPhaseGather(r)%localvec_r8 /= MPI_DATATYPE_NULL) call MPI_Type_free(decomp%TwoPhaseGather(r)%localvec_r8, ierr)

        if (decomp%TwoPhaseGather(r)%vec_r4 /= MPI_DATATYPE_NULL) call MPI_Type_free(decomp%TwoPhaseGather(r)%vec_r4, ierr)
        if (decomp%TwoPhaseGather(r)%vec_r8 /= MPI_DATATYPE_NULL) call MPI_Type_free(decomp%TwoPhaseGather(r)%vec_r8, ierr)
      enddo
      deallocate(decomp%TwoPhaseGather)
    endif
    if (allocated(decomp%gather_workspace_r4))  deallocate(decomp%gather_workspace_r4)
    if (allocated(decomp%gather_workspace_r8))  deallocate(decomp%gather_workspace_r8)
    if (allocated(decomp%gather_send_cast_workspace_r4)) deallocate(decomp%gather_send_cast_workspace_r4)
    if (allocated(decomp%gather_send_cast_workspace_r8)) deallocate(decomp%gather_send_cast_workspace_r8)

    if(allocated(decomp%ibegin)) deallocate(decomp%ibegin)
    if(allocated(decomp%iend)) deallocate(decomp%iend)
    if(allocated(decomp%jbegin)) deallocate(decomp%jbegin)
    if(allocated(decomp%jend)) deallocate(decomp%jend)
    if(allocated(decomp%MyRowGlobal)) deallocate(decomp%MyRowGlobal)
    if(allocated(decomp%MyColGlobal)) deallocate(decomp%MyColGlobal)
    if(allocated(decomp%MyRankInRowComm)) deallocate(decomp%MyRankInRowComm)
    if(allocated(decomp%MyRankInColComm)) deallocate(decomp%MyRankInColComm)
    if(allocated(decomp%NumColsPerRank)) deallocate(decomp%NumColsPerRank)
    if(allocated(decomp%NumRowsPerRank)) deallocate(decomp%NumRowsPerRank)

    ! Release the row/column communicators created by TwoPhaseGather_init()
    if (decomp%rowComm /= MPI_COMM_NULL) then
      call MPI_Comm_free(decomp%rowComm, ierr)
      decomp%rowComm = MPI_COMM_NULL
    endif
    if (decomp%colComm /= MPI_COMM_NULL) then
      call MPI_Comm_free(decomp%colComm, ierr)
      decomp%colComm = MPI_COMM_NULL
    endif

    if(allocated(decomp%MyRankInContigComm)) deallocate(decomp%MyRankInContigComm)
    if(allocated(decomp%MyRankInStridedComm)) deallocate(decomp%MyRankInStridedComm)
  end subroutine TwoPhaseGather_delete

  subroutine TwoPhaseScatter_run(decomp, sgrid, xx, mype_lbegin, ntotallev, batch_size, &
                                 LevelToProcMap, LevelToVariableMap, LevelToLevelMap, &
                                 VarToVarMap, nc_vartype, bundle_is_2d, bundle_true_idx, d4r4_t, d4r8_t)
    
    use gsi_bundlemod, only: gsi_bundle
    use m_kinds, only: r_kind
    use netcdf, only: NF90_FLOAT, NF90_DOUBLE
    use, intrinsic :: iso_c_binding, only : c_float, c_double
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    type(sub2grid_info), intent(in) :: sgrid
    type(gsi_bundle), intent(inout) :: xx
    integer, intent(in) :: mype_lbegin, ntotallev, batch_size
    integer(kind=4), intent(in) :: LevelToProcMap(:), LevelToVariableMap(:)
    integer(kind=4), intent(in) :: LevelToLevelMap(:), VarToVarMap(:)
    integer(kind=4), intent(in) :: nc_vartype(:)
    integer, intent(in) :: bundle_is_2d(:), bundle_true_idx(:)
    real(4), allocatable, intent(inout), target, asynchronous :: d4r4_t(:,:,:)
    real(8), allocatable, intent(inout), target, asynchronous :: d4r8_t(:,:,:)

    ! Local Variables
    integer :: b_start_curr, b_end_curr, n_in_batch_curr
    integer :: b_start_next, b_end_next, n_in_batch_next
    integer :: b_idx, b_ind, i, j, k, l, owner, ierr
    integer :: file_var_idx, jedi_var_idx, bundle_idx
    integer :: buf_curr, buf_next, offset_curr, offset_next
    logical :: is_2d
    real(kind=4), target :: dummy_r4(1,1)
    real(kind=8), target :: dummy_r8(1,1)

    ! Automatic arrays for requests (automatically sized/freed by the compiler)
    integer :: reqs_p1(batch_size, 0:1), reqs_p2(batch_size, 0:1)

    ! Edge case: zero fields
    if (ntotallev <= 0) return

    l = mype_lbegin

    ! ------------------------------------------------------------------
    ! 1. PRIME THE PIPELINE (Batch 1 into Buffer 0)
    ! ------------------------------------------------------------------
    buf_curr = 0
    offset_curr = 0
    b_start_curr = 1
    b_end_curr = min(b_start_curr + batch_size - 1, ntotallev)
    n_in_batch_curr = b_end_curr - b_start_curr + 1

    ! [ MPI PHASE 1 - BUFFER 0 ]
    reqs_p1(:, buf_curr) = MPI_REQUEST_NULL
    do b_idx = 1, n_in_batch_curr
      k = b_start_curr + b_idx - 1
      b_ind = b_idx + offset_curr
      file_var_idx = LevelToVariableMap(k) ! NetCDF File space
      owner = LevelToProcMap(k)

      if (sgrid%mype == owner) then
        ! Compiler automatically routes to _r4 or _r8
        if (nc_vartype(file_var_idx) == NF90_FLOAT) then
          call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, d4r4_t(:,:,l), b_ind, reqs_p1(b_idx, buf_curr))
        elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_double) then
          call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, d4r8_t(:,:,l), b_ind, reqs_p1(b_idx, buf_curr))
        elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_float) then
          call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, d4r4_t(:,:,l), b_ind, reqs_p1(b_idx, buf_curr))
        endif
        l=l+1
      else
        ! Non-owners must pass the appropriately typed dummy
        if (nc_vartype(file_var_idx) == NF90_FLOAT) then
          call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, dummy_r4, b_ind, reqs_p1(b_idx, buf_curr))
        elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_double) then
          call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, dummy_r8, b_ind, reqs_p1(b_idx, buf_curr))
        elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_float) then
          call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, dummy_r4, b_ind, reqs_p1(b_idx, buf_curr))
        endif
      endif
    enddo

    ! ------------------------------------------------------------------
    ! 2. OVERLAP MAIN LOOP (Batch 2 to End)
    ! ------------------------------------------------------------------
    do b_start_next = 1 + batch_size, ntotallev, batch_size
      buf_next = 1 - buf_curr
      offset_next = buf_next * batch_size
      b_end_next = min(b_start_next + batch_size - 1, ntotallev)
      n_in_batch_next = b_end_next - b_start_next + 1

      ! [ MPI PHASE 1 - NEXT BUFFER ]
      reqs_p1(:, buf_next) = MPI_REQUEST_NULL
      do b_idx = 1, n_in_batch_next
        k = b_start_next + b_idx - 1
        b_ind = b_idx + offset_next
        owner = LevelToProcMap(k)
        file_var_idx = LevelToVariableMap(k)

        if (sgrid%mype == owner) then
          ! Compiler automatically routes to _r4 or _r8
          if (nc_vartype(file_var_idx) == NF90_FLOAT) then
            call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, d4r4_t(:,:,l), b_ind, reqs_p1(b_idx, buf_next))
          elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_double) then
            call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, d4r8_t(:,:,l), b_ind, reqs_p1(b_idx, buf_next))
          elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_float) then
            call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, d4r4_t(:,:,l), b_ind, reqs_p1(b_idx, buf_next))
          endif
          l=l+1
        else
          ! Non-owners must pass the appropriately typed dummy
          if (nc_vartype(file_var_idx) == NF90_FLOAT) then
            call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, dummy_r4, b_ind, reqs_p1(b_idx, buf_next))
          elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_double) then
            call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, dummy_r8, b_ind, reqs_p1(b_idx, buf_next))
          elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_float) then
            call TwoPhaseScatter_Phase1(decomp, owner, sgrid%mype, dummy_r4, b_ind, reqs_p1(b_idx, buf_next))
          endif
        endif
      enddo

      ! --- SYNCHRONIZE PREVIOUS BUFFER ---
      ! Wait for PREVIOUS batch Phase 1 to finish hitting the intermediate root ranks
      call MPI_Waitall(n_in_batch_curr, reqs_p1(1:n_in_batch_curr, buf_curr), MPI_STATUSES_IGNORE, ierr)

      ! [ MPI PHASE 2 - PREVIOUS BUFFER ]
      reqs_p2(:, buf_curr) = MPI_REQUEST_NULL
      do b_idx = 1, n_in_batch_curr
        k = b_start_curr + b_idx - 1
        b_ind = b_idx + offset_curr
        owner = LevelToProcMap(k)
        file_var_idx = LevelToVariableMap(k)
        jedi_var_idx = VarToVarMap(file_var_idx)
        
        is_2d = (bundle_is_2d(jedi_var_idx) == 1)
        bundle_idx = bundle_true_idx(jedi_var_idx)

        if (nc_vartype(file_var_idx) == NF90_FLOAT .and. r_kind == c_double) then
          call TwoPhaseScatter_Phase2(decomp, owner, sgrid%mype, decomp%scatter_recv_cast_workspace_r4(:,:,b_ind), b_ind, reqs_p2(b_idx, buf_curr))
        else
          if (is_2d) then
             call TwoPhaseScatter_Phase2(decomp, owner, sgrid%mype, xx%r2(bundle_idx)%q(:,:), b_ind, reqs_p2(b_idx, buf_curr))
          else
             call TwoPhaseScatter_Phase2(decomp, owner, sgrid%mype, xx%r3(bundle_idx)%q(:,:,LevelToLevelMap(k)), b_ind, reqs_p2(b_idx, buf_curr))
          endif
        endif
      end do

      call MPI_Waitall(n_in_batch_curr, reqs_p2(1:n_in_batch_curr, buf_curr), MPI_STATUSES_IGNORE, ierr)

      ! [ POST-PROCESS TYPE CONVERSIONS - PREVIOUS BUFFER ]
      do b_idx = 1, n_in_batch_curr
        k = b_start_curr + b_idx - 1
        b_ind = b_idx + offset_curr
        file_var_idx = LevelToVariableMap(k)
        jedi_var_idx = VarToVarMap(file_var_idx)

        if (nc_vartype(file_var_idx) == NF90_FLOAT .and. r_kind == c_double) then
          is_2d = (bundle_is_2d(jedi_var_idx) == 1)
          bundle_idx = bundle_true_idx(jedi_var_idx)

          if (is_2d) then
             xx%r2(bundle_idx)%q(:,:) = real(decomp%scatter_recv_cast_workspace_r4(:,:,b_ind), kind=r_kind)
          else
             xx%r3(bundle_idx)%q(:,:,LevelToLevelMap(k)) = real(decomp%scatter_recv_cast_workspace_r4(:,:,b_ind), kind=r_kind)
          endif
        endif
      end do
      ! --- OVERLAP END ---

      ! Shift state for next iteration
      buf_curr = buf_next
      offset_curr = offset_next
      b_start_curr = b_start_next
      b_end_curr = b_end_next
      n_in_batch_curr = n_in_batch_next
    end do

    ! ------------------------------------------------------------------
    ! 3. DRAIN THE PIPELINE (Finish Phase 1 and 2 for the final batch)
    ! ------------------------------------------------------------------
    call MPI_Waitall(n_in_batch_curr, reqs_p1(1:n_in_batch_curr, buf_curr), MPI_STATUSES_IGNORE, ierr)

    reqs_p2(:, buf_curr) = MPI_REQUEST_NULL
    do b_idx = 1, n_in_batch_curr
      k = b_start_curr + b_idx - 1
      b_ind = b_idx + offset_curr
      owner = LevelToProcMap(k)
      file_var_idx = LevelToVariableMap(k)
      jedi_var_idx = VarToVarMap(file_var_idx)

      is_2d = (bundle_is_2d(jedi_var_idx) == 1)
      bundle_idx = bundle_true_idx(jedi_var_idx)

      if (nc_vartype(file_var_idx) == NF90_FLOAT .and. r_kind == c_double) then
        ! Route directly into the shared module casting workspace
        call TwoPhaseScatter_Phase2(decomp, owner, sgrid%mype, decomp%scatter_recv_cast_workspace_r4(:,:,b_ind), b_ind, reqs_p2(b_idx, buf_curr))
      else
        ! Scatter directly to strictly typed application array inside the GSI bundle
        if (is_2d) then
           call TwoPhaseScatter_Phase2(decomp, owner, sgrid%mype, xx%r2(bundle_idx)%q(:,:), b_ind, reqs_p2(b_idx, buf_curr))
        else
           call TwoPhaseScatter_Phase2(decomp, owner, sgrid%mype, xx%r3(bundle_idx)%q(:,:,LevelToLevelMap(k)), b_ind, reqs_p2(b_idx, buf_curr))
        endif
      endif
    end do

    call MPI_Waitall(n_in_batch_curr, reqs_p2(1:n_in_batch_curr, buf_curr), MPI_STATUSES_IGNORE, ierr)

    do b_idx = 1, n_in_batch_curr
      k = b_start_curr + b_idx - 1
      b_ind = b_idx + offset_curr
      file_var_idx = LevelToVariableMap(k)
      jedi_var_idx = VarToVarMap(file_var_idx)

      if (nc_vartype(file_var_idx) == NF90_FLOAT .and. r_kind == c_double) then
        is_2d = (bundle_is_2d(jedi_var_idx) == 1)
        bundle_idx = bundle_true_idx(jedi_var_idx)

        if (is_2d) then
           xx%r2(bundle_idx)%q(:,:) = real(decomp%scatter_recv_cast_workspace_r4(:,:,b_ind), kind=r_kind)
        else
           xx%r3(bundle_idx)%q(:,:,LevelToLevelMap(k)) = real(decomp%scatter_recv_cast_workspace_r4(:,:,b_ind), kind=r_kind)
        endif
      endif
    end do
  end subroutine TwoPhaseScatter_run

  subroutine TwoPhaseGather_run(decomp, sgrid, xx, mype_lbegin, ntotallev, batch_size, &
                                LevelToProcMap, LevelToVariableMap, LevelToLevelMap, &
                                VarToVarMap, nc_vartype, bundle_is_2d, bundle_true_idx, d4r4_t, d4r8_t)
    
    use gsi_bundlemod, only: gsi_bundle
    use m_kinds, only: r_kind
    use netcdf, only: NF90_FLOAT, NF90_DOUBLE
    use, intrinsic :: iso_c_binding, only : c_float, c_double
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    type(sub2grid_info), intent(in) :: sgrid
    type(gsi_bundle), intent(inout) :: xx
    integer, intent(in) :: mype_lbegin, ntotallev, batch_size
    integer(kind=4), intent(in) :: LevelToProcMap(:), LevelToVariableMap(:)
    integer(kind=4), intent(in) :: LevelToLevelMap(:), VarToVarMap(:)
    integer(kind=4), intent(in) :: nc_vartype(:)
    integer, intent(in) :: bundle_is_2d(:), bundle_true_idx(:)
    real(4), allocatable, intent(inout), target, asynchronous :: d4r4_t(:,:,:)
    real(8), allocatable, intent(inout), target, asynchronous :: d4r8_t(:,:,:)

    ! Local Variables
    integer :: b_start_curr, b_end_curr, n_in_batch_curr
    integer :: b_start_next, b_end_next, n_in_batch_next
    integer :: b_idx, b_ind, k, owner, ierr
    integer :: buf_curr, buf_next, offset_curr, offset_next
    integer :: reqs_p1(batch_size, 0:1), reqs_p2(batch_size, 0:1)
    
    integer :: level, file_var_idx, jedi_var_idx, bundle_idx, l
    logical :: is_2d
    integer, allocatable :: level_to_l(:)
    real(4), target, asynchronous :: dummy_r4(1,1)
    real(8), target, asynchronous :: dummy_r8(1,1)

    if (ntotallev <= 0) return

    ! Pre-compute the 'l' index (local read buffer index) for each global level
    allocate(level_to_l(ntotallev))
    l = mype_lbegin
    do k = 1, ntotallev
      if (sgrid%mype == LevelToProcMap(k)) then
        level_to_l(k) = l
        l = l + 1
      else
        level_to_l(k) = -1
      endif
    enddo

    ! ------------------------------------------------------------------
    ! 1. PRIME THE PIPELINE (Batch 1 Phase 1 into Buffer 0)
    ! ------------------------------------------------------------------
    buf_curr = 0
    offset_curr = 0
    b_start_curr = 1
    b_end_curr = min(b_start_curr + batch_size - 1, ntotallev)
    n_in_batch_curr = b_end_curr - b_start_curr + 1

    reqs_p1(:, buf_curr) = MPI_REQUEST_NULL
    do b_idx = 1, n_in_batch_curr
      k = b_start_curr + b_idx - 1
      b_ind = b_idx + offset_curr
      owner = LevelToProcMap(k)
      file_var_idx = LevelToVariableMap(k)
      jedi_var_idx = VarToVarMap(file_var_idx)
      
      is_2d = (bundle_is_2d(jedi_var_idx) == 1)
      bundle_idx = bundle_true_idx(jedi_var_idx)

      ! CPU Pre-processing for Downcasting
      if (nc_vartype(file_var_idx) == NF90_FLOAT .and. r_kind == c_double) then
        if (is_2d) then
           decomp%gather_send_cast_workspace_r4(:,:,b_ind) = real(xx%r2(bundle_idx)%q(:,:), kind=4)
        else
           decomp%gather_send_cast_workspace_r4(:,:,b_ind) = real(xx%r3(bundle_idx)%q(:,:,LevelToLevelMap(k)), kind=4)
        endif
        call TwoPhaseGather_Phase1(decomp, owner, sgrid%mype, decomp%gather_send_cast_workspace_r4(:,:,b_ind), b_ind, reqs_p1(b_idx, buf_curr))
      else
        if (is_2d) then
           call TwoPhaseGather_Phase1(decomp, owner, sgrid%mype, xx%r2(bundle_idx)%q(:,:), b_ind, reqs_p1(b_idx, buf_curr))
        else
           call TwoPhaseGather_Phase1(decomp, owner, sgrid%mype, xx%r3(bundle_idx)%q(:,:,LevelToLevelMap(k)), b_ind, reqs_p1(b_idx, buf_curr))
        endif
      endif
    end do

    ! ------------------------------------------------------------------
    ! 2. OVERLAP MAIN LOOP (Batch 2 to End)
    ! ------------------------------------------------------------------
    do b_start_next = 1 + batch_size, ntotallev, batch_size
      buf_next = 1 - buf_curr
      offset_next = buf_next * batch_size
      b_end_next = min(b_start_next + batch_size - 1, ntotallev)
      n_in_batch_next = b_end_next - b_start_next + 1

      ! --- OVERLAP START ---
      ! [ MPI PHASE 1 - NEXT BUFFER ]
      reqs_p1(:, buf_next) = MPI_REQUEST_NULL
      do b_idx = 1, n_in_batch_next
        k = b_start_next + b_idx - 1
        b_ind = b_idx + offset_next
        owner = LevelToProcMap(k)
        file_var_idx = LevelToVariableMap(k)
        jedi_var_idx = VarToVarMap(file_var_idx)
        
        is_2d = (bundle_is_2d(jedi_var_idx) == 1)
        bundle_idx = bundle_true_idx(jedi_var_idx)

        if (nc_vartype(file_var_idx) == NF90_FLOAT .and. r_kind == c_double) then
          if (is_2d) then
             decomp%gather_send_cast_workspace_r4(:,:,b_ind) = real(xx%r2(bundle_idx)%q(:,:), kind=4)
          else
             decomp%gather_send_cast_workspace_r4(:,:,b_ind) = real(xx%r3(bundle_idx)%q(:,:,LevelToLevelMap(k)), kind=4)
          endif
          call TwoPhaseGather_Phase1(decomp, owner, sgrid%mype, decomp%gather_send_cast_workspace_r4(:,:,b_ind), b_ind, reqs_p1(b_idx, buf_next))
        else
          if (is_2d) then
             call TwoPhaseGather_Phase1(decomp, owner, sgrid%mype, xx%r2(bundle_idx)%q(:,:), b_ind, reqs_p1(b_idx, buf_next))
          else
             call TwoPhaseGather_Phase1(decomp, owner, sgrid%mype, xx%r3(bundle_idx)%q(:,:,LevelToLevelMap(k)), b_ind, reqs_p1(b_idx, buf_next))
          endif
        endif
      end do

      ! --- SYNCHRONIZE PREVIOUS BUFFER ---
      call MPI_Waitall(n_in_batch_curr, reqs_p1(1:n_in_batch_curr, buf_curr), MPI_STATUSES_IGNORE, ierr)

      ! [ MPI PHASE 2 - PREVIOUS BUFFER ]
      reqs_p2(:, buf_curr) = MPI_REQUEST_NULL
      do b_idx = 1, n_in_batch_curr
        k = b_start_curr + b_idx - 1
        b_ind = b_idx + offset_curr
        owner = LevelToProcMap(k)
        file_var_idx = LevelToVariableMap(k)

        if (sgrid%mype == owner) then
          if (nc_vartype(file_var_idx) == NF90_FLOAT) then
            call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, d4r4_t(:,:,level_to_l(k)), b_ind, reqs_p2(b_idx, buf_curr))
          elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_double) then
            call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, d4r8_t(:,:,level_to_l(k)), b_ind, reqs_p2(b_idx, buf_curr))
          elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_float) then
            call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, d4r4_t(:,:,level_to_l(k)), b_ind, reqs_p2(b_idx, buf_curr))
          endif
        else
          ! Pass conditionally typed dummies for non-owners
          if (nc_vartype(file_var_idx) == NF90_FLOAT) then
            call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, dummy_r4, b_ind, reqs_p2(b_idx, buf_curr))
          elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_double) then
            call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, dummy_r8, b_ind, reqs_p2(b_idx, buf_curr))
          elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_float) then
            call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, dummy_r4, b_ind, reqs_p2(b_idx, buf_curr))
          endif
        endif
      end do

      call MPI_Waitall(n_in_batch_curr, reqs_p2(1:n_in_batch_curr, buf_curr), MPI_STATUSES_IGNORE, ierr)
      ! --- OVERLAP END ---

      ! Shift state for next iteration
      buf_curr = buf_next
      offset_curr = offset_next
      b_start_curr = b_start_next
      b_end_curr = b_end_next
      n_in_batch_curr = n_in_batch_next
    end do

    ! ------------------------------------------------------------------
    ! 3. DRAIN THE PIPELINE (Finish Phase 1 and 2 for the final batch)
    ! ------------------------------------------------------------------
    call MPI_Waitall(n_in_batch_curr, reqs_p1(1:n_in_batch_curr, buf_curr), MPI_STATUSES_IGNORE, ierr)

    reqs_p2(:, buf_curr) = MPI_REQUEST_NULL
    do b_idx = 1, n_in_batch_curr
      k = b_start_curr + b_idx - 1
      b_ind = b_idx + offset_curr
      owner = LevelToProcMap(k)
      file_var_idx = LevelToVariableMap(k)

      if (sgrid%mype == owner) then
        if (nc_vartype(file_var_idx) == NF90_FLOAT) then
          call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, d4r4_t(:,:,level_to_l(k)), b_ind, reqs_p2(b_idx, buf_curr))
        elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_double) then
          call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, d4r8_t(:,:,level_to_l(k)), b_ind, reqs_p2(b_idx, buf_curr))
        elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_float) then
          call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, d4r4_t(:,:,level_to_l(k)), b_ind, reqs_p2(b_idx, buf_curr))
        endif
      else
        if (nc_vartype(file_var_idx) == NF90_FLOAT) then
          call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, dummy_r4, b_ind, reqs_p2(b_idx, buf_curr))
        elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_double) then
          call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, dummy_r8, b_ind, reqs_p2(b_idx, buf_curr))
        elseif (nc_vartype(file_var_idx) == NF90_DOUBLE .and. r_kind == c_float) then
          call TwoPhaseGather_Phase2(decomp, owner, sgrid%mype, dummy_r4, b_ind, reqs_p2(b_idx, buf_curr))
        endif
      endif
    end do
    call MPI_Waitall(n_in_batch_curr, reqs_p2(1:n_in_batch_curr, buf_curr), MPI_STATUSES_IGNORE, ierr)

    deallocate(level_to_l)
  end subroutine TwoPhaseGather_run

  subroutine TwoPhaseScatter_Phase1_r4(decomp, owner, rank, sendbuf, b_ind, req_p1)
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer, intent(in) :: owner, rank
    real(kind=4), contiguous, target, asynchronous, intent(inout) :: sendbuf(:,:)
    integer, intent(in)  :: b_ind
    integer, intent(out) :: req_p1

    integer :: ierr
    integer :: myrow, mycol

    myrow = decomp%NSindex
    mycol = decomp%EWindex

    associate(ts => decomp%TwoPhaseScatter(owner))
    ! Execute the scatter
    if (myrow == decomp%MyRowGlobal(owner)) then
      if (rank == owner) then
        call MPI_Iscatterv(sendbuf(1,1), ts%sendcounts_phase1, ts%senddispls_phase1, &
                           MPI_REAL, decomp%scatter_workspace_r4(1,1,b_ind), ts%sendcounts_phase1(mycol), &
                           MPI_REAL, decomp%MyRankInContigComm(owner), decomp%contig_comm, req_p1, ierr)
      else
        call MPI_Iscatterv(MPI_BOTTOM, ts%sendcounts_phase1, ts%senddispls_phase1, &
                           MPI_REAL, decomp%scatter_workspace_r4(1,1,b_ind), ts%sendcounts_phase1(mycol), &
                           MPI_REAL, decomp%MyRankInContigComm(owner), decomp%contig_comm, req_p1, ierr)
      endif
    else
      req_p1 = MPI_REQUEST_NULL
    endif
    end associate
  end subroutine TwoPhaseScatter_Phase1_r4

  subroutine TwoPhaseScatter_Phase1_r8(decomp, owner, rank, sendbuf, b_ind, req_p1)
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer, intent(in) :: owner, rank
    real(kind=8), contiguous, target, asynchronous, intent(inout) :: sendbuf(:,:)
    integer, intent(in)  :: b_ind
    integer, intent(out) :: req_p1

    integer :: ierr
    integer :: myrow, mycol

    myrow = decomp%NSindex
    mycol = decomp%EWindex

    associate(ts => decomp%TwoPhaseScatter(owner))
    ! Execute the scatter
    if (myrow == decomp%MyRowGlobal(owner)) then
      if (rank == owner) then
        call MPI_Iscatterv(sendbuf(1,1), ts%sendcounts_phase1, ts%senddispls_phase1, &
                           MPI_DOUBLE_PRECISION, decomp%scatter_workspace_r8(1,1,b_ind), ts%sendcounts_phase1(mycol), &
                           MPI_DOUBLE_PRECISION, decomp%MyRankInContigComm(owner), decomp%contig_comm, req_p1, ierr)
      else
        call MPI_Iscatterv(MPI_BOTTOM, ts%sendcounts_phase1, ts%senddispls_phase1, &
                           MPI_DOUBLE_PRECISION, decomp%scatter_workspace_r8(1,1,b_ind), ts%sendcounts_phase1(mycol), &
                           MPI_DOUBLE_PRECISION, decomp%MyRankInContigComm(owner), decomp%contig_comm, req_p1, ierr)
      endif
    else
      req_p1 = MPI_REQUEST_NULL
    endif
    end associate
  end subroutine TwoPhaseScatter_Phase1_r8

  subroutine TwoPhaseScatter_Phase2_r4(decomp, owner, rank, recvbuf, b_ind, req_p2)
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer, intent(in) :: owner, rank
    real(kind=4), contiguous, target, asynchronous, intent(inout) :: recvbuf(:,:)
    integer, intent(in)  :: b_ind
    integer, intent(out) :: req_p2

    integer :: ierr, vec
    integer(kind=MPI_ADDRESS_KIND) :: lb, extent
    integer :: myrow, mycol

    myrow = decomp%NSindex
    mycol = decomp%EWindex

    associate(ts => decomp%TwoPhaseScatter(owner))
    if (myrow == decomp%MyRowGlobal(owner)) then
      vec = ts%vec_r4
      call MPI_Iscatterv(decomp%scatter_workspace_r4(1,1,b_ind), ts%sendcounts_phase2, &
                         ts%senddispls_phase2, vec, recvbuf(1,1), &
                         ts%sendcounts_phase2(myrow), ts%localvec_r4, &
                         decomp%MyRankInStridedComm(owner), decomp%strided_comm, req_p2, ierr)
    else
      call MPI_Iscatterv(MPI_BOTTOM, ts%sendcounts_phase2, ts%senddispls_phase2, &
                         MPI_REAL, recvbuf(1,1), ts%sendcounts_phase2(myrow), &
                         ts%localvec_r4, decomp%MyRankInStridedComm(owner), decomp%strided_comm, req_p2, ierr)
    endif
    end associate
  end subroutine TwoPhaseScatter_Phase2_r4

  subroutine TwoPhaseScatter_Phase2_r8(decomp, owner, rank, recvbuf, b_ind, req_p2)
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer, intent(in) :: owner, rank
    real(kind=8), contiguous, target, asynchronous, intent(inout) :: recvbuf(:,:)
    integer, intent(in)  :: b_ind
    integer, intent(out) :: req_p2

    integer :: ierr, vec
    integer(kind=MPI_ADDRESS_KIND) :: lb, extent
    integer :: myrow, mycol

    myrow = decomp%NSindex
    mycol = decomp%EWindex

    associate(ts => decomp%TwoPhaseScatter(owner))
    if (myrow == decomp%MyRowGlobal(owner)) then
      vec = ts%vec_r8
      call MPI_Iscatterv(decomp%scatter_workspace_r8(1,1,b_ind), ts%sendcounts_phase2, &
                         ts%senddispls_phase2, vec, recvbuf(1,1), &
                         ts%sendcounts_phase2(myrow), ts%localvec_r8, &
                         decomp%MyRankInStridedComm(owner), decomp%strided_comm, req_p2, ierr)
    else
      call MPI_Iscatterv(MPI_BOTTOM, ts%sendcounts_phase2, ts%senddispls_phase2, &
                         MPI_DOUBLE_PRECISION, recvbuf(1,1), ts%sendcounts_phase2(myrow), &
                         ts%localvec_r8, decomp%MyRankInStridedComm(owner), decomp%strided_comm, req_p2, ierr)
    endif
    end associate
  end subroutine TwoPhaseScatter_Phase2_r8


  subroutine TwoPhaseGather_Phase1_r4(decomp, owner, rank, sendbuf, b_ind, req_p1)
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer, intent(in) :: owner, rank
    real(kind=4), contiguous, target, asynchronous, intent(inout) :: sendbuf(:,:)
    integer, intent(in)  :: b_ind
    integer, intent(out) :: req_p1

    integer :: ierr, vec
    integer(kind=MPI_ADDRESS_KIND) :: lb, extent
    integer :: myrow, mycol

    myrow = decomp%NSindex
    mycol = decomp%EWindex

    associate(ts => decomp%TwoPhaseGather(owner))
    ! EVERY column participates in the parallel gather
    if (myrow == decomp%MyRowGlobal(owner)) then
      vec = ts%vec_r4
      call MPI_Igatherv(sendbuf(decomp%my_gather_start(1), decomp%my_gather_start(2)), &
                        ts%recvcounts_phase1(myrow), ts%localvec_r4, &
                        decomp%gather_workspace_r4(1,1,b_ind), ts%recvcounts_phase1, &
                        ts%recvdispls_phase1, vec, decomp%MyRankInStridedComm(owner), &
                        decomp%strided_comm, req_p1, ierr)
    else
      call MPI_Igatherv(sendbuf(decomp%my_gather_start(1), decomp%my_gather_start(2)), &
                        ts%recvcounts_phase1(myrow), ts%localvec_r4, &
                        MPI_BOTTOM, ts%recvcounts_phase1, &
                        ts%recvdispls_phase1, MPI_REAL, decomp%MyRankInStridedComm(owner), &
                        decomp%strided_comm, req_p1, ierr)
    endif
    end associate
  end subroutine TwoPhaseGather_Phase1_r4

  subroutine TwoPhaseGather_Phase1_r8(decomp, owner, rank, sendbuf, b_ind, req_p1)
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer, intent(in) :: owner, rank
    real(kind=8), contiguous, target, asynchronous, intent(inout) :: sendbuf(:,:)
    integer, intent(in)  :: b_ind
    integer, intent(out) :: req_p1

    integer :: ierr, vec
    integer(kind=MPI_ADDRESS_KIND) :: lb, extent
    integer :: myrow, mycol

    myrow = decomp%NSindex
    mycol = decomp%EWindex

    associate(ts => decomp%TwoPhaseGather(owner))
    ! EVERY column participates in the parallel gather
    if (myrow == decomp%MyRowGlobal(owner)) then
      vec = ts%vec_r8
      call MPI_Igatherv(sendbuf(decomp%my_gather_start(1), decomp%my_gather_start(2)), &
                        ts%recvcounts_phase1(myrow), ts%localvec_r8, &
                        decomp%gather_workspace_r8(1,1,b_ind), ts%recvcounts_phase1, &
                        ts%recvdispls_phase1, vec, decomp%MyRankInStridedComm(owner), &
                        decomp%strided_comm, req_p1, ierr)
    else
      call MPI_Igatherv(sendbuf(decomp%my_gather_start(1), decomp%my_gather_start(2)), &
                        ts%recvcounts_phase1(myrow), ts%localvec_r8, &
                        MPI_BOTTOM, ts%recvcounts_phase1, &
                        ts%recvdispls_phase1, MPI_DOUBLE_PRECISION, decomp%MyRankInStridedComm(owner), &
                        decomp%strided_comm, req_p1, ierr)
    endif
    end associate
  end subroutine TwoPhaseGather_Phase1_r8

  subroutine TwoPhaseGather_Phase2_r4(decomp, owner, rank, recvbuf, b_ind, req_p2)
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer, intent(in) :: owner, rank
    real(kind=4), contiguous, target, asynchronous, intent(inout) :: recvbuf(:,:)
    integer, intent(in)  :: b_ind
    integer, intent(out) :: req_p2

    integer :: myrow, mycol, ierr

    myrow = decomp%NSindex
    mycol = decomp%EWindex

    associate(ts => decomp%TwoPhaseGather(owner))
    ! GSI Layout: E-W across rowComm (Contiguous)
    if (myrow == decomp%MyRowGlobal(owner)) then
      if (rank == owner) then
        call MPI_Igatherv(decomp%gather_workspace_r4(1,1,b_ind), ts%recvcounts_phase2(mycol), MPI_REAL, &
                          recvbuf(1,1), ts%recvcounts_phase2, &
                          ts%recvdispls_phase2, MPI_REAL, decomp%MyRankInContigComm(owner), &
                          decomp%contig_comm, req_p2, ierr)
      else
        call MPI_Igatherv(decomp%gather_workspace_r4(1,1,b_ind), ts%recvcounts_phase2(mycol), MPI_REAL, &
                          MPI_BOTTOM, ts%recvcounts_phase2, &
                          ts%recvdispls_phase2, MPI_REAL, decomp%MyRankInContigComm(owner), &
                          decomp%contig_comm, req_p2, ierr)
      endif
    else
      req_p2 = MPI_REQUEST_NULL
    endif
    end associate
  end subroutine TwoPhaseGather_Phase2_r4

  subroutine TwoPhaseGather_Phase2_r8(decomp, owner, rank, recvbuf, b_ind, req_p2)
    implicit none

    ! Arguments
    type(twophase_decomp), target, asynchronous, intent(inout) :: decomp
    integer, intent(in) :: owner, rank
    real(kind=8), contiguous, target, asynchronous, intent(inout) :: recvbuf(:,:)
    integer, intent(in)  :: b_ind
    integer, intent(out) :: req_p2

    integer :: myrow, mycol, ierr

    myrow = decomp%NSindex
    mycol = decomp%EWindex

    associate(ts => decomp%TwoPhaseGather(owner))
    ! GSI Layout: E-W across rowComm (Contiguous)
    if (myrow == decomp%MyRowGlobal(owner)) then
      if (rank == owner) then
        call MPI_Igatherv(decomp%gather_workspace_r8(1,1,b_ind), ts%recvcounts_phase2(mycol), MPI_DOUBLE_PRECISION, &
                          recvbuf(1,1), ts%recvcounts_phase2, &
                          ts%recvdispls_phase2, MPI_DOUBLE_PRECISION, decomp%MyRankInContigComm(owner), &
                          decomp%contig_comm, req_p2, ierr)
      else
        call MPI_Igatherv(decomp%gather_workspace_r8(1,1,b_ind), ts%recvcounts_phase2(mycol), MPI_DOUBLE_PRECISION, &
                          MPI_BOTTOM, ts%recvcounts_phase2, &
                          ts%recvdispls_phase2, MPI_DOUBLE_PRECISION, decomp%MyRankInContigComm(owner), &
                          decomp%contig_comm, req_p2, ierr)
      endif
    else
      req_p2 = MPI_REQUEST_NULL
    endif
    end associate
  end subroutine TwoPhaseGather_Phase2_r8

end module TwoPhaseScatterGather_JEDIGlobal
