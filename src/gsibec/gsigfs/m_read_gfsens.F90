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
  use m_mpimod, only: gsi_mpi_comm_world,nxpe,nype
  use mpeu_util, only: die

  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer

  use gridmod, only: nsig
  use control_vectors, only: nc2d, nc3d
  use control_vectors, only: cvars2d, cvars3d

  use general_sub2grid_mod, only: sub2grid_info
  use m_grid2sub1var, only: grid2sub1var

  use netcdf
  implicit none

  character(len=*), parameter :: myname = 'm_read_gfsens'
  private
  public gfs_readens

  interface gfs_readens
     module procedure read_gfsens_
     module procedure read_gfsens_new_
  end interface

  integer(i_kind), allocatable, dimension(:) :: spec_send
  integer(i_kind), allocatable, dimension(:) :: disp_spec
  integer(i_kind) :: nlat_g, nlon_g, iglobal, nsig1o

  integer, parameter :: numfiles = 1

  ! Subcommunicator
  integer :: read_comm, write_comm

  ! MPI_Info
  integer :: info

  ! spread_comm communicator is used to distribute reads to all nodes in the job
  integer :: node_comm, spread_comm, node_count
  integer :: local_rank, spread_mype, is_node_leader, total_nodes, batch_size
  integer, allocatable :: spread_to_world(:)
  integer, allocatable :: reqs_p1(:), reqs_p2(:)

  integer :: my_var_index
  integer :: ntotallev
  integer :: mype_lbegin,mype_lend
  integer :: mype_vartype
  integer :: mype_fileid
  integer :: cached_globalsizes(2)=0  ! Local copy of the grid dimensions needed in order to track changes (dual resolution cases)
  logical :: first_pass = .true. ! Some arrays need only be allocated once
  logical :: need_to_reallocate_ps = .false. ! Some arrays need only be allocated once
  character(len=72) :: mype_varname

  integer(kind=4), allocatable :: LevelToProcMap(:), LevelToVariableMap(:), LevelToLevelMap(:), VarToVarMap(:)
  integer(kind=4), allocatable :: nc_vartype(:), numvarfile(:), nlevpervar(:)
  character(len=72), allocatable :: varnames(:)
  character(len=NF90_MAX_NAME), allocatable :: FileNamesToProcess(:)  ! This holds all the file names for a given ensemble member

contains

subroutine read_gfsens_new_(sgrid, xx, filename, npe, mype)
  use module_ncfile_stat, only : ncfile_stat
  use TwoPhaseScatterGather_JEDIGlobal
  use netcdf
  use mpi, only : MPI_Wtime, MPI_REAL, MPI_SUM
  use constants, only: fv
  use mpeu_util, only: getindex
  use, intrinsic :: ieee_arithmetic
  use, intrinsic :: iso_c_binding

  implicit none
  real, parameter :: Pa_to_cb = 1.0e-3  ! Pa to centibars: 1 cb = 1000 Pa
  character(len=*), parameter :: myname_ = myname//'::read_gfsens_new_'
  type(sub2grid_info), intent(in)    :: sgrid
  type(gsi_bundle),    intent(inout) :: xx(:)
  character(len=*),    intent(in)    :: filename(:)
  integer,             intent(in)    :: npe, mype

  ! TwoPhase datatypes for scatter and gather
  type(twophase_decomp) :: tpds, tpdg

  integer :: num_restart_vars(numfiles)
  character(len=500)  :: tmpvarlist(numfiles)
  character(len=500), allocatable  :: varlist(:)
  integer :: i, j, k, level, l, n, indexrst, file_var_idx, jedi_var_idx
  integer :: iret,ilev,r,ncioid,var_id,loc
  integer(kind=4), allocatable :: nlev(:), numvar(:)

  real(kind=4), target :: dummy_r4(1,1)
  real(kind=8), target :: dummy_r8(1,1)

  type(ncfile_stat) :: ncfs_all
  integer, allocatable :: out_fileid(:), out_lvlbegin(:), out_lvlend(:)
  character(len=72), allocatable :: out_varname(:)
  character(len=72), allocatable :: tmp_names(:)
  character(len=72), allocatable :: bundle_vars(:)
  integer, allocatable :: tmp_d3(:), tmp_fid(:)

  ! sub communicator
  integer :: color, key
  integer :: uv_color, uv_comm
  integer :: tq_color, tq_comm

  ! MPI_Info
  integer :: info

  ! spread_comm communicator is used to distribute reads to all nodes in the job
  integer :: node_comm, spread_comm
  integer :: local_rank, spread_mype, is_node_leader, total_nodes, batch_size
  integer, allocatable :: spread_to_world(:)
  integer, allocatable :: reqs_p1(:), reqs_p2(:)

  !real(4), pointer, contiguous :: d2r4ptr(:,:) => null()
  !real(8), pointer, contiguous :: d2r8ptr(:,:) => null()

  real(4) :: sums_f,sumn_f,rnlon
  real(8) :: sums_d,sumn_d
  real(4),allocatable, target, asynchronous :: d4r4(:,:,:), d4r4_t(:,:,:)
  real(8),allocatable, target, asynchronous :: d4r8(:,:,:), d4r8_t(:,:,:)

  integer :: iens, ierr
  integer(kind=4) :: starts(4), counts(4), pe_layout(2)
  real(kind=8) :: tb1,tb2,tb3,tb4,tb5, times(5), walltime(5)
  real(kind=8) :: te1,te2,te3,te4,te5
  integer :: totalnumfiles, ens_count
  real(4), allocatable :: clons(:), slons(:)  ! cos/sin of GFS longitudes for vector poles

  ! Arrays to hold precomputed dot products for vector interpolated north and south poles
  !real(4) :: polnu,polnv,polsu,polsv
  !real(4), allocatable :: pu_n_c(:,:), pu_n_s(:,:), pu_s_c(:,:), pu_s_s(:,:)
  !real(4), allocatable :: pv_n_c(:,:), pv_n_s(:,:), pv_s_c(:,:), pv_s_s(:,:)

  ! Variables for exact bit-for-bit 32-bit vector pole interpolation
  real(4), allocatable :: u_pole_n_r4(:,:), u_pole_s_r4(:,:)
  real(4), allocatable :: v_pole_n_r4(:,:), v_pole_s_r4(:,:)
  real(4) :: polnu_r4, polnv_r4, polsu_r4, polsv_r4, rnlon_r4

  ! Variables for exact bit-for-bit 64-bit vector pole interpolation
  real(8), allocatable :: u_pole_n_r8(:,:), u_pole_s_r8(:,:)
  real(8), allocatable :: v_pole_n_r8(:,:), v_pole_s_r8(:,:)
  real(8) :: polnu_r8, polnv_r8, polsu_r8, polsv_r8, rnlon_r8

  logical :: gsi_,gfspoles_, is_2d
  !integer :: f, nz, bundle_idx
  integer :: bundle_idx
  character(len=:), allocatable :: str
  integer :: b_start, b_end, n_in_batch, b_ind, owner
  integer :: id_u, id_v, id_t, id_q, id_ps, id_sst
  integer :: global_lon_idx,local_n,local_s,k_file

  ! Added to deal with missing SST data.
  integer, allocatable :: bundle_is_2d(:), bundle_true_idx(:)

  ! Debugging
  real(4) :: checksum
  real(4) :: checksum_v(sgrid%nsig), checksum_v_global(sgrid%nsig)

  ! Used for early virtual temperature calculation
  integer :: file_idx_t, file_idx_q, l_counter, v
  integer :: partner_world_rank, partner_read_rank
  integer :: world_group, read_group, tag_q2t
  real(4), allocatable :: q_chunk_r4(:,:,:)
  real(8), allocatable :: q_chunk_r8(:,:,:)

  real(4), allocatable :: q_full_r4(:,:,:)
  real(8), allocatable :: q_full_r8(:,:,:)

  if(nxPE > 0 .AND. nyPE > 0) then
    if( npe/=nxpe*nype ) then
      call die(myname_,'NPE inconsistent from  NxPE NyPE ',npe)
    endif
  else
    call die(myname_,'MPI Layout required for Two Phase Scatter/Gather ',npe)
  endif

  gsi_ = .true.
  gfspoles_ = .true.
  ens_count = size(xx)
  !ens_count = size(filename)

  times(:)=0.0; walltime(:)=0.0

  tb5 = MPI_Wtime()
  tb1=tb5

  ! Group ranks by physical shared-memory node
  call MPI_Comm_split_type(gsi_mpi_comm_world, MPI_COMM_TYPE_SHARED, 0, MPI_INFO_NULL, node_comm, ierr)
  call MPI_Comm_rank(node_comm, local_rank, ierr)

  ! Determine the number of physical computers being used.  Will use that number as the batch size for non-blocking scatter
  is_node_leader = 0
  if (local_rank == 0) is_node_leader = 1
  call MPI_Allreduce(is_node_leader, total_nodes, 1, MPI_INTEGER, MPI_SUM, gsi_mpi_comm_world, ierr)

  batch_size=max(1,total_nodes)
  batch_size=batch_size*3
  !batch_size_gather=total_nodes
  !batch_size_scatter=2*batch_size_gather

  allocate(reqs_p1(batch_size), reqs_p2(batch_size))
  call MPI_Comm_free(node_comm, ierr)

  ! Interleave ranks across nodes using local_rank as the sort key
  call MPI_Comm_split(gsi_mpi_comm_world, 0, local_rank, spread_comm, ierr)
  call MPI_Comm_rank(spread_comm, spread_mype, ierr)

  ! Create a dictionary to map the new interleaved rank back to the true world rank
  allocate(spread_to_world(0:npe-1))
  call MPI_Allgather(mype, 1, MPI_INTEGER, spread_to_world, 1, MPI_INTEGER, spread_comm, ierr)


  allocate(bundle_vars(xx(1)%n2d+xx(1)%n3d-1))  ! not ingesting sst.  Need to set sst to 0.0 somewhere downstream
  i=1
  allocate(bundle_is_2d(xx(1)%n2d+xx(1)%n3d-1))
  allocate(bundle_true_idx(xx(1)%n2d+xx(1)%n3d-1))
  i=1
  do j=1,xx(1)%n2d
    if(trim(xx(1)%r2(j)%shortname) == 'sst') cycle
    bundle_vars(i) = trim(xx(1)%r2(j)%shortname)
    bundle_is_2d(i) = 1
    bundle_true_idx(i) = j
    i=i+1
  enddo
  do j=1,xx(1)%n3d
    bundle_vars(i) = trim(xx(1)%r3(j)%shortname)
    bundle_is_2d(i) = 0
    bundle_true_idx(i) = j
    i=i+1
  enddo


  if(allocated(nlev)) deallocate(nlev)
  allocate(nlev(0:npe-1))

  !totalnumfiles=count(rstflag(:) .eqv. .true.)
  totalnumfiles=1 ! this is set to one until I get a case that reads from more than one file.
  if(allocated(FileNamesToProcess)) deallocate(FileNamesToProcess)
  allocate(FileNamesToProcess(totalnumfiles))
  FileNamesToProcess(1) = filename(1)

  allocate(numvar(totalnumfiles))
  numvar(1)=size(bundle_vars)


  tmpvarlist=''
  allocate(varlist(totalnumfiles))
  ! Need a whitespace separated string of variable names for each file to feed into ncfs_all%init
  indexrst=1 ! this is set to one until I get a case that reads from more than one file.
  do jedi_var_idx = 1,size(bundle_vars)
    varlist(1) = trim(bundle_vars(jedi_var_idx))
    if (len_trim(tmpvarlist(indexrst)) > 0) then
      tmpvarlist(indexrst)=trim(tmpvarlist(indexrst)) //' '// gfs_varname_(trim(bundle_vars(jedi_var_idx)))
    else
      tmpvarlist(indexrst)=gfs_varname_(trim(bundle_vars(jedi_var_idx)))
    endif
  enddo

  varlist(1) = tmpvarlist(1)
  !if(mype == 0) write(6,'("read_gfsens_new_: All vars minus sst: " A)') trim(varlist(1))

!  i=1
!  do n=1, numfiles
!    if ( rstflag(n) ) then
!      !FileNamesToProcess(i) = trim(self%datapath)//'/'//trim(self%filenames(n))
!      FileNamesToProcess(i) = filename(1)
!      numvar(i) = num_restart_vars(n)
!      varlist(i) = tmpvarlist(n)
!      i=i+1
!    endif
!  enddo

  ! Total number of variables that will actually be read from files
  ! (this can be smaller than size(fields) when PS is computed from DELP)
  if(allocated(nlevpervar)) deallocate(nlevpervar)
  allocate(nlevpervar(sum(numvar)))

  if(allocated(varnames)) deallocate(varnames)
  allocate(varnames(sum(numvar)))

  if(allocated(nc_vartype)) deallocate(nc_vartype)
  allocate(nc_vartype(sum(numvar)))

  ! init on all ranks to avoid passing unallocated allocatable to MPI_Scatter
  allocate(out_fileid(npe), out_varname(npe), out_lvlbegin(npe), out_lvlend(npe))

  ! Parse files from one ensemble member
  if(spread_mype==0) then
    ! find dimension of each field
    call ncfs_all%init(totalnumfiles, FileNamesToProcess, numvar, varlist)
    !call ncfs_all%init(totalnumfiles, filename(1), numvar, varlist)
    call ncfs_all%fill_dims()

    allocate(tmp_names(ncfs_all%numvar), tmp_d3(ncfs_all%numvar), tmp_fid(ncfs_all%numvar))

    ! Extract variables from NetCDF metadata
    tmp_names(:) = ncfs_all%list_varname(1:ncfs_all%numvar)
    tmp_d3(:)    = ncfs_all%dim_3(1:ncfs_all%numvar)

    ! Build the file ID map (which variables belong to which file index)
    i = 0
    do n = 1, ncfs_all%numfiles
      do k = 1, ncfs_all%numvarfile(n)
        i = i + 1
        tmp_fid(i) = n
      enddo
    enddo

    ! distibute variables to each core
    call distribute_io_work(npe, ncfs_all%numvar, tmp_names, tmp_d3, tmp_fid, &
                            out_fileid, out_varname, out_lvlbegin, out_lvlend, nlev)
    nlev(:)=0
    do i=0,npe-1
      if( (out_lvlend(i+1) > 0) .and. (out_lvlbegin(i+1) > 0) ) then
        nlev(i) = (out_lvlend(i+1) - out_lvlbegin(i+1) + 1)
      endif
    enddo
    ntotallev = sum(ncfs_all%dim_3)

    nlevpervar(:ncfs_all%numvar) = ncfs_all%dim_3(:)
    varnames(:ncfs_all%numvar) = ncfs_all%list_varname(:)  ! Names of vcariables to be processed
    nc_vartype(:ncfs_all%numvar) = ncfs_all%vartype(:)        ! NetCDF type of each variable
    deallocate(tmp_names, tmp_d3, tmp_fid)
    call ncfs_all%close()
  endif

  deallocate(varlist)

  ! Let all ranks know what is going on
  call MPI_Scatter(out_fileid  ,  1,   mpi_integer, mype_fileid ,  1, mpi_integer  , 0, spread_comm,ierr)
  call MPI_Scatter(out_varname , 72, mpi_character, mype_varname, 72, mpi_character, 0, spread_comm,ierr)
  call MPI_Scatter(out_lvlbegin,  1,   mpi_integer, mype_lbegin ,  1, mpi_integer  , 0, spread_comm,ierr)
  call MPI_Scatter(out_lvlend  ,  1,   mpi_integer, mype_lend   ,  1, mpi_integer  , 0, spread_comm,ierr)
  deallocate(out_fileid, out_varname, out_lvlbegin, out_lvlend)

  call MPI_Bcast(ntotallev, 1, mpi_integer, 0, spread_comm,ierr)
  call MPI_Bcast(nlev, npe, mpi_integer, 0, spread_comm,ierr)
  call MPI_Bcast(nlevpervar, sum(numvar), mpi_integer, 0, spread_comm,ierr)
  call MPI_Bcast(varnames, 72*sum(numvar), mpi_character, 0, spread_comm,ierr)
  call MPI_Bcast(nc_vartype, sum(numvar), mpi_integer, 0, spread_comm,ierr)

  ! Find this rank's assigned vartype by matching the assigned varname

  mype_vartype = -1
  do i = 1, sum(numvar)
    if (trim(varnames(i)) == trim(adjustl(mype_varname))) then
      mype_vartype = nc_vartype(i)
      exit
    endif
  enddo

  ! Map field level to the process handling that level
  ! LevelToProcMap(levelIndex) gives the rank
  if(allocated(LevelToProcMap)) deallocate(LevelToProcMap)
  allocate(LevelToProcMap(ntotallev))
  LevelToProcMap(:) = -999
  l=1
  do r=0,npe-1
    do i=1,nlev(r)
      LevelToProcMap(l) = spread_to_world(r)
      l=l+1
    enddo
  enddo
  call MPI_Comm_free(spread_comm, ierr)
  deallocate(spread_to_world)

  if (any(LevelToProcMap(:) == -999)) then
    write(6,'("read_gfsens_new_: Some sigma level were not assigned")')
    call MPI_Abort(gsi_mpi_comm_world,10,ierr)
  endif

  ! Map combined level to variable
  ! LevelToVariableMap(levelIndex) gives an index into the fields array
  if(allocated(LevelToVariableMap)) deallocate(LevelToVariableMap)
  allocate(LevelToVariableMap(ntotallev))
  LevelToVariableMap(:) = -999
  l=1
  do file_var_idx=1,sum(numvar)
    do i=1,nlevpervar(file_var_idx)
      LevelToVariableMap(l) = file_var_idx
      l=l+1
    enddo
  enddo

  if (any(LevelToVariableMap(:) == -999)) then
    loc = findloc(LevelToVariableMap, value=-999, dim=1, back=.false.)
    write(6,'("read_gfsens_new_: Some variables were not assigned ",2I6)') ntotallev,loc
    call MPI_Abort(gsi_mpi_comm_world,11,ierr)
  endif

  ! Map combined level to variable level
  ! LevelToLevelMap(levelIndex) gives an index into fields(var)%array
  ! JEDI Global wants 3D arrays flipped vertically so adjust the level mapping
  ! to force the scatter process to make them land in the right place
  if(allocated(LevelToLevelMap)) deallocate(LevelToLevelMap)
  allocate(LevelToLevelMap(ntotallev))
  LevelToLevelMap(:) = -999
  l=1
  do file_var_idx=1,sum(numvar)
    do i=1,nlevpervar(file_var_idx)
      !LevelToLevelMap(l) = i
      LevelToLevelMap(l) = nlevpervar(file_var_idx) - i + 1
      l=l+1
    enddo
  enddo

  if (any(LevelToLevelMap(:) == -999)) then
    write(6,'("read_gfsens_new_: Some levels were not assigned")')
    call MPI_Abort(gsi_mpi_comm_world,12,ierr)
  endif


  ! Identify the file variable indices for Temperature and Humidity
  file_idx_t = 0; file_idx_q = 0
  do i = 1, sum(numvar)
    if (trim(varnames(i)) == 'tmp') file_idx_t = i
    if (trim(varnames(i)) == 'spfh') file_idx_q = i
  enddo

  ! Precompute the global rank of the counterpart holding my exact Z-levels
  partner_world_rank = MPI_PROC_NULL
  if (trim(mype_varname) == 'tmp' .and. file_idx_q > 0) then
     l_counter = 1
     do v = 1, file_idx_q - 1
        l_counter = l_counter + nlevpervar(v)
     enddo
     partner_world_rank = LevelToProcMap(l_counter + mype_lbegin - 1)
  else if (trim(mype_varname) == 'spfh' .and. file_idx_t > 0) then
     l_counter = 1
     do v = 1, file_idx_t - 1
        l_counter = l_counter + nlevpervar(v)
     enddo
     partner_world_rank = LevelToProcMap(l_counter + mype_lbegin - 1)
  end if

  deallocate(nlevpervar)


  if(allocated(VarToVarMap)) deallocate(VarToVarMap)
  allocate(VarToVarMap(sum(numvar)))
  VarToVarMap(:) = -999
  !do jedi_var_idx = 1,xx(1)%NumVars
  do jedi_var_idx = 1,size(bundle_vars)
    do file_var_idx = 1,sum(numvar)
      if (trim(gfs_varname_(trim(bundle_vars(jedi_var_idx)))) == trim(varnames(file_var_idx))) then
        VarToVarMap(file_var_idx) = jedi_var_idx
        exit
      endif
    enddo
  enddo
  deallocate(varnames)

  if (any(VarToVarMap(:) == -999)) then
    write(6,'("read_gfsens_new_: Some variables not mapped",21I6)') VarToVarMap(1:sum(numvar))
    call MPI_Abort(gsi_mpi_comm_world,112,ierr)
  endif
  deallocate(numvar)

  ! Create sub-communicator to handle each file
  key=mype+1
  if(mype_fileid > 0 .and. mype_fileid <= totalnumfiles) then
       color = mype_fileid
  else
       color = MPI_UNDEFINED
  endif

  call MPI_Comm_split(gsi_mpi_comm_world,color,key,read_comm,ierr)
  if ( ierr /= 0 ) then
       write(6,'(a,i5)')'***ERROR*** after mpi_comm_create with iret = ',ierr
       call mpi_abort(gsi_mpi_comm_world,101,ierr)
  endif

  ! Translate the partner's world rank into its equivalent read_comm rank
  partner_read_rank = MPI_PROC_NULL
  if (partner_world_rank /= MPI_PROC_NULL .and. read_comm /= MPI_COMM_NULL) then
     call MPI_Comm_group(gsi_mpi_comm_world, world_group, ierr)
     call MPI_Comm_group(read_comm, read_group, ierr)
     call MPI_Group_translate_ranks(world_group, 1, partner_world_rank, read_group, partner_read_rank, ierr)
     call MPI_Group_free(world_group, ierr)
     call MPI_Group_free(read_group, ierr)
  end if

  te1 = MPI_Wtime()
  times(1) = te1-tb1

! read 2D slab from each file using sub communicator
  call mpi_barrier(gsi_mpi_comm_world,ierr)
  call MPI_Info_create(info, ierr)
  call MPI_Info_set(info, "romio_cb_read", "disable", ierr)

  ! Vector pole override for u (sf or u) and v (vp or v) 3D pair.
  ! GFS longitudes are uniformly spaced 0 to 360: lon(j) = (j-1)*360/nlon deg.
  allocate(clons(sgrid%nlon), slons(sgrid%nlon))
  call compute_loncs_nc_(sgrid%nlon, clons, slons)

  uv_comm = MPI_COMM_NULL
  tq_comm = MPI_COMM_NULL

  pe_layout(1)=nxpe; pe_layout(2)=nype
  call TwoPhaseScatter_init(tpds, sgrid, npe, pe_layout, batch_size, halo_width=1)

  do iens=1,ens_count

    ! Only ranks assigned one or more levels from the combined set of levels enter here
    if (MPI_COMM_NULL /= read_comm) then

      tb2 = MPI_Wtime()

      iret=nf90_open(trim(filename(iens)),ior(nf90_nowrite,nf90_mpiio),ncioid,comm=read_comm,info=info)
      if(iret/=nf90_noerr) then
        write(6,'("read_restart_fields_reg: Error opening NetCDF file ",2A,I4,A,I4)') &
                   trim(FileNamesToProcess(mype_fileid)),' fileid=',mype_fileid,', Status =',iret
        write(6,*)  nf90_strerror(iret)
        stop 333
      endif

      if(iens == 1) then
        ! Assign a color to U and V ranks; exclude all others
        if (trim(mype_varname) == 'ugrd' .or. trim(mype_varname) == 'vgrd') then
           uv_color = 1
        else
           uv_color = MPI_UNDEFINED
        end if
        call MPI_Comm_split(read_comm, uv_color, mype, uv_comm, ierr)

        ! Assign a color to T and SPFH ranks; exclude all others
        if (trim(mype_varname) == 'tmp' .or. trim(mype_varname) == 'spfh') then
           tq_color = 1
        else
           tq_color = MPI_UNDEFINED
        end if
        call MPI_Comm_split(read_comm, tq_color, mype, tq_comm, ierr)


        ! Allocated storage for file arrays as well as padded and transposed versions needed by JEDI
        if(mype_vartype==NF90_FLOAT) then
          allocate(d4r4(sgrid%nlon,sgrid%nlat-2,mype_lbegin:mype_lend))
          allocate(d4r4_t(0:sgrid%nlat+1,0:sgrid%nlon+1,mype_lbegin:mype_lend))  ! allocated to the same size as the global fields with buffer
          if (uv_comm /= MPI_COMM_NULL) then
            allocate(u_pole_n_r4(sgrid%nlon, sgrid%nsig))
            allocate(u_pole_s_r4(sgrid%nlon, sgrid%nsig))
            allocate(v_pole_n_r4(sgrid%nlon, sgrid%nsig))
            allocate(v_pole_s_r4(sgrid%nlon, sgrid%nsig))
          endif

        elseif(mype_vartype==NF90_DOUBLE .and. r_kind == c_double) then
          ! The file provides c_double and application wants c_double, so don't convert during read
          allocate(d4r8(sgrid%nlon,sgrid%nlat-2,mype_lbegin:mype_lend))
          allocate(d4r8_t(0:sgrid%nlat+1,0:sgrid%nlon+1,mype_lbegin:mype_lend))
          allocate(u_pole_n_r8(sgrid%nlon, sgrid%nsig))
          allocate(u_pole_s_r8(sgrid%nlon, sgrid%nsig))
          allocate(v_pole_n_r8(sgrid%nlon, sgrid%nsig))
          allocate(v_pole_s_r8(sgrid%nlon, sgrid%nsig))

        elseif(mype_vartype==NF90_DOUBLE .and. r_kind == c_float) then
          ! The file provides c_double but application wants c_float,
          ! so convert to c_float during read to avoid scattering more data than needed
          allocate(d4r4(sgrid%nlon,sgrid%nlat-2,mype_lbegin:mype_lend))
          allocate(d4r4_t(0:sgrid%nlat+1,0:sgrid%nlon+1,mype_lbegin:mype_lend))
          allocate(u_pole_n_r4(sgrid%nlon, sgrid%nsig))
          allocate(u_pole_s_r4(sgrid%nlon, sgrid%nsig))
          allocate(v_pole_n_r4(sgrid%nlon, sgrid%nsig))
          allocate(v_pole_s_r4(sgrid%nlon, sgrid%nsig))
        else
          write(6,*) 'Warning, unknown datatype'
        endif
      endif ! iens == 1


      starts=(/1,1,mype_lbegin,1/)
      counts=(/sgrid%nlon,sgrid%nlat-2,(mype_lend - mype_lbegin + 1),1/)

      call check( nf90_inq_varid(ncioid, trim(adjustl(mype_varname)), var_id) )
      iret = nf90_var_par_access(ncioid, var_id, nf90_independent)
      if(mype_vartype==NF90_FLOAT) then
        call check( nf90_get_var(ncioid,var_id,d4r4(:,:,mype_lbegin:mype_lend),start=starts,count=counts) )
      elseif(mype_vartype==NF90_DOUBLE .and. r_kind == c_double) then
        ! The file provides c_double and application wants c_double, so don't convert during read
        call check( nf90_get_var(ncioid,var_id,d4r8(:,:,mype_lbegin:mype_lend),start=starts,count=counts) )
      elseif(mype_vartype==NF90_DOUBLE .and. r_kind == c_float) then
        ! The file provides c_double but application wants c_float,
        ! so allow NetCDF to convert to c_float during read to avoid
        ! scattering more data than needed.
        call check( nf90_get_var(ncioid,var_id,d4r4(:,:,mype_lbegin:mype_lend),start=starts,count=counts) )
      endif
      iret=nf90_close(ncioid)

      te2 = MPI_Wtime()
      times(2) = times(2) + (te2-tb2)
      if(mype_vartype==NF90_FLOAT) then
        ! Convert surface pressure now instead of after the scatter to maintain bit-reproducible output
        if (trim(mype_varname) == 'pressfc') then
          d4r4(:,:,mype_lbegin:mype_lend) = d4r4(:,:,mype_lbegin:mype_lend) * Pa_to_cb
        endif

        ! Compute virtual temperature now to maintain bit-reproducible output
        tag_q2t = 100
        if (partner_read_rank /= MPI_PROC_NULL) then
          if (trim(mype_varname) == 'tmp') then
            ! I need to RECEIVE Humidity data.
            allocate(q_chunk_r4(sgrid%nlon, sgrid%nlat-2, mype_lbegin:mype_lend))
          
            call MPI_Recv(q_chunk_r4, size(q_chunk_r4), MPI_REAL, partner_read_rank, tag_q2t, &
                          read_comm, MPI_STATUS_IGNORE, ierr)
             
            do k = mype_lbegin, mype_lend
                  d4r4(:,:,k) = d4r4(:,:,k) * (1.0_4 + fv * max(0.0_4, q_chunk_r4(:,:,k)))
            end do
            deallocate(q_chunk_r4)
             
          else if (trim(mype_varname) == 'spfh') then
            ! I need to SEND my data to the appropritate temperature rank.
            ! Use MPI_Send so the rank waits until the T rank is ready to pull the data
            call MPI_Send(d4r4(:,:,mype_lbegin:mype_lend), size(d4r4(:,:,mype_lbegin:mype_lend)), &
                          MPI_REAL, partner_read_rank, tag_q2t, read_comm, ierr)
          end if
        end if


        ! Add north and south poles, transpose horizontally and then invert first dimension (North/South)
        do k = mype_lbegin,mype_lend
          sums_f = 0.0
          sumn_f = 0.0
          do j=1,sgrid%nlat-2
            do i=1,sgrid%nlon
              d4r4_t(sgrid%nlat -j,i,k) = d4r4(i,j,k)
            end do
          end do

          ! Pole interpolation for scalars
          rnlon = 1.0 / real(sgrid%nlon)
          sums_f = sum(d4r4(:, sgrid%nlat-2, k)) * rnlon
          sumn_f = sum(d4r4(:,            1, k)) * rnlon
          ! Apply to the poles (rows 1 and nlat)
          do j = 1, sgrid%nlon
            d4r4_t(         1, j, k) = sums_f
            d4r4_t(sgrid%nlat, j, k) = sumn_f
          end do

          ! halo update code is lifted from reorder21_() which uses ltosi_s and ltosj_s from general_sub2grid_mod to fill halo points
          !if(s%ltosj_s(ns)==     0) s%ltosj_s(ns)=nlon
          !if(s%ltosj_s(ns)==nlon+1) s%ltosj_s(ns)=1
          ! Periodic Longitude Wraps (East/West Halos)
          d4r4_t(:,            0, k) = d4r4_t(:, sgrid%nlon, k)
          d4r4_t(:, sgrid%nlon+1, k) = d4r4_t(:,          1, k)

          !if(s%ltosi_s(ns)==     0) s%ltosi_s(ns)=1
          !if(s%ltosi_s(ns)==nlat+1) s%ltosi_s(ns)=nlat
          ! Latitude outward copies (North/South Halos)
          d4r4_t(           0, :, k) = d4r4_t(         1, :, k)
          d4r4_t(sgrid%nlat+1, :, k) = d4r4_t(sgrid%nlat, :, k)
        enddo

        ! Pole interpolation for vectors (fillpoles_v_nc_)
        ! Collect needed u and v values for the weight computation
        if (uv_comm /= MPI_COMM_NULL) then
          u_pole_n_r4(:, :) = 0.0; u_pole_s_r4(:, :) = 0.0
          v_pole_n_r4(:, :) = 0.0; v_pole_s_r4(:, :) = 0.0
        endif
        if (trim(mype_varname) == 'ugrd') then
          do k = mype_lbegin,mype_lend
            u_pole_n_r4(:, k) = d4r4(:,            1, k)
            u_pole_s_r4(:, k) = d4r4(:, sgrid%nlat-2, k)
          end do
        else if (trim(mype_varname) == 'vgrd') then
          do k = mype_lbegin,mype_lend
            v_pole_n_r4(:, k) = d4r4(:,            1, k)
            v_pole_s_r4(:, k) = d4r4(:, sgrid%nlat-2, k)
          end do
        end if

        ! Perform vector pole interpolation (fillpoles_v_nc_)
        if (uv_comm /= MPI_COMM_NULL) then
          call MPI_Allreduce(MPI_IN_PLACE, u_pole_n_r4, sgrid%nlon * sgrid%nsig, MPI_REAL, MPI_SUM, uv_comm, ierr)
          call MPI_Allreduce(MPI_IN_PLACE, u_pole_s_r4, sgrid%nlon * sgrid%nsig, MPI_REAL, MPI_SUM, uv_comm, ierr)
          call MPI_Allreduce(MPI_IN_PLACE, v_pole_n_r4, sgrid%nlon * sgrid%nsig, MPI_REAL, MPI_SUM, uv_comm, ierr)
          call MPI_Allreduce(MPI_IN_PLACE, v_pole_s_r4, sgrid%nlon * sgrid%nsig, MPI_REAL, MPI_SUM, uv_comm, ierr)

          rnlon_r4 = 1.0 / real(sgrid%nlon, 4)
        
          do k = mype_lbegin, mype_lend
            polnu_r4 = 0.0_4; polnv_r4 = 0.0_4
            polsu_r4 = 0.0_4; polsv_r4 = 0.0_4

            ! Match BASELINE accumulation order exactly
            do j = 1, sgrid%nlon
              polnu_r4 = polnu_r4 + u_pole_n_r4(j, k)*clons(j) - v_pole_n_r4(j, k)*slons(j)
              polnv_r4 = polnv_r4 + u_pole_n_r4(j, k)*slons(j) + v_pole_n_r4(j, k)*clons(j)
              polsu_r4 = polsu_r4 + u_pole_s_r4(j, k)*clons(j) + v_pole_s_r4(j, k)*slons(j)
              polsv_r4 = polsv_r4 + u_pole_s_r4(j, k)*slons(j) - v_pole_s_r4(j, k)*clons(j)
            enddo

            polnu_r4 = polnu_r4 * rnlon_r4; polnv_r4 = polnv_r4 * rnlon_r4
            polsu_r4 = polsu_r4 * rnlon_r4; polsv_r4 = polsv_r4 * rnlon_r4

            ! Overwrite the global pre-scatter buffer poles
            do j = 1, sgrid%nlon
              if (trim(mype_varname) == 'ugrd') then
                d4r4_t(sgrid%nlat, j, k) =  polnu_r4*clons(j) + polnv_r4*slons(j)
                d4r4_t(         1, j, k) =  polsu_r4*clons(j) + polsv_r4*slons(j)
              else if (trim(mype_varname) == 'vgrd') then
                d4r4_t(sgrid%nlat, j, k) = -polnu_r4*slons(j) + polnv_r4*clons(j)
                d4r4_t(         1, j, k) =  polsu_r4*slons(j) - polsv_r4*clons(j)
              end if
            enddo

            ! Re-apply East/West periodic wrap for the updated pole rows
            d4r4_t(sgrid%nlat,            0, k) = d4r4_t(sgrid%nlat, sgrid%nlon, k)
            d4r4_t(sgrid%nlat, sgrid%nlon+1, k) = d4r4_t(sgrid%nlat,          1, k)
            d4r4_t(         1,            0, k) = d4r4_t(         1, sgrid%nlon, k)
            d4r4_t(         1, sgrid%nlon+1, k) = d4r4_t(         1,          1, k)

            ! Re-apply North/South outward halo copies
            d4r4_t(sgrid%nlat+1, :, k) = d4r4_t(sgrid%nlat, :, k)
            d4r4_t(           0, :, k) = d4r4_t(         1, :, k)
          end do
        end if
      elseif(mype_vartype==NF90_DOUBLE .and. r_kind == c_double) then
        write(6,'(a)')'read_gfsens_new_: NF90_DOUBLE/c_double scenario not fully implemented'
        call mpi_abort(gsi_mpi_comm_world,121,ierr)

          ! Add north and south poles, transpose horizontally and then invert first dimension (North/South)
          do k = mype_lbegin,mype_lend
            do j=1,sgrid%nlat-2
              do i=1,sgrid%nlon
                d4r8_t(sgrid%nlat-j, i,k) = d4r8(i,j,k)
              end do
            end do

            !call fillpoles_s_nc_(d4r8_t(:,:,kk), sgrid%nlon, sgrid%nlat)
            sums_d = sum(d4r8(:, sgrid%nlat-2, k)) / real(sgrid%nlon,r_kind)
            sumn_d = sum(d4r8(:,            1, k)) / real(sgrid%nlon,r_kind)
            ! Apply to the poles (rows 1 and nlat)
            do i = 1, sgrid%nlon
              d4r8_t(         1, i, k) = sums_d
              d4r8_t(sgrid%nlat, i, k) = sumn_d
            end do

            ! Periodic Longitude Wraps (East/West Halos)
            d4r8_t(:,            0, k) = d4r8_t(:, sgrid%nlon, k)
            d4r8_t(:, sgrid%nlon+1, k) = d4r8_t(:,          1, k)
          
            ! Latitude outward copies (North/South Halos)
            d4r8_t(           0, :, k) = d4r8_t(         1, :, k)
            d4r8_t(sgrid%nlat+1, :, k) = d4r8_t(sgrid%nlat, :, k)
          enddo
      elseif(mype_vartype==NF90_DOUBLE .and. r_kind == c_float) then
        write(6,'(a)')'read_gfsens_new_: Mixed NF90_DOUBLE/c_float scenario not fully implemented'
        call mpi_abort(gsi_mpi_comm_world,121,ierr)

          ! Add north and south poles, transpose horizontally and then invert first (latitude) dimension (North/South)
          do k = mype_lbegin,mype_lend
            do j=1,sgrid%nlat-2
              do i=1,sgrid%nlon
                d4r4_t(sgrid%nlat-j, i,k) = d4r4(i,j,k)
              end do
            end do

            !call fillpoles_s_nc_(d4r4_t(:,:,kk), sgrid%nlon, sgrid%nlat)
            sums_f = sum(d4r4(:, sgrid%nlat-2, k)) / real(sgrid%nlon,r_kind)
            sumn_f = sum(d4r4(:,            1, k)) / real(sgrid%nlon,r_kind)
            ! Apply to the poles (rows 1 and nlat)
            do i = 1, sgrid%nlon
              d4r4_t(         1, i, k) = sums_f
              d4r4_t(sgrid%nlat, i, k) = sumn_f
            end do
          enddo

          ! Periodic Longitude Wraps (East/West Halos)
          d4r4_t(:,            0, k) = d4r4_t(:, sgrid%nlon, k)
          d4r4_t(:, sgrid%nlon+1, k) = d4r4_t(:,            1, k)
          
          ! Latitude outward copies (North/South Halos)
          d4r4_t(           0, :, k) = d4r4_t(         1, :, k)
          d4r4_t(sgrid%nlat+1, :, k) = d4r4_t(sgrid%nlat, :, k)
      endif
      te3 = MPI_Wtime()
      times(3) = times(3) + (te3-te2)
    endif ! read_comm
    call MPI_Barrier(gsi_mpi_comm_world,ierr)

    ! Scatter file data to all ranks and convert to application type as needed
    ! ------------------------------------------------------------------------
    tb4 = MPI_Wtime()
    if(mype==0) write(*,*) 'Scatter ensemble member =', iens
    call TwoPhaseScatter_run(tpds, sgrid, xx(iens), mype_lbegin, ntotallev, batch_size, &
                             LevelToProcMap, LevelToVariableMap, LevelToLevelMap, &
                             VarToVarMap, nc_vartype, bundle_is_2d, bundle_true_idx, &
                             d4r4_t, d4r8_t)
    te4 = MPI_Wtime()
    times(4) = times(4) + (te4-tb4)
  enddo ! iens

  ! Hack to reamin consistent with current global case
  id_sst=0
  do j = 1, xx(1)%n2d
    if (trim(xx(1)%r2(j)%shortname) == 'sst') id_sst = j
  enddo

  if (id_sst > 0) then
    do iens=1,ens_count
      xx(iens)%r2(id_sst)%q(:,:) = 0.0
    enddo
  endif

  ! Deallocate temporary arrays
  if (MPI_COMM_NULL /= read_comm) then
    if(allocated(d4r4)) deallocate(d4r4)
    if(allocated(d4r4_t)) deallocate(d4r4_t)
    if(allocated(d4r8)) deallocate(d4r8)
    if(allocated(d4r8_t)) deallocate(d4r8_t)
  endif

  if(allocated(nlev)) deallocate(nlev)

! Deallocate MPI and File Control arrays
  if (allocated(reqs_p1)) deallocate(reqs_p1, reqs_p2)
  if (allocated(FileNamesToProcess)) deallocate(FileNamesToProcess)
  if (allocated(nc_vartype)) deallocate(nc_vartype)

  ! Deallocate Level and Variable Mapping Dictionaries
  if (allocated(LevelToProcMap)) deallocate(LevelToProcMap)
  if (allocated(LevelToVariableMap)) deallocate(LevelToVariableMap)
  if (allocated(LevelToLevelMap)) deallocate(LevelToLevelMap)
  if (allocated(VarToVarMap)) deallocate(VarToVarMap)

  deallocate(bundle_vars)
  deallocate(bundle_is_2d, bundle_true_idx)

  ! Deallocate Vector Pole Math Arrays
  if (allocated(clons)) deallocate(clons, slons)

  call TwoPhaseScatter_delete(tpds)

  te5 = MPI_Wtime()
  times(5) = te5-tb5
  call MPI_Reduce(times, walltime, size(times), MPI_DOUBLE_PRECISION, MPI_MAX, 0, gsi_mpi_comm_world, ierr)
  if (mype == 0) write(*,'(A,6F12.6)') 'read_gfsens_new_: Walltime ', walltime, sum(walltime(1:4))

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

  subroutine check(status)
!    use netcdf
    integer, intent ( in) :: status
    integer :: ierr

    if(status /= nf90_noerr) then
      print *, trim(nf90_strerror(status))
      call MPI_Abort(MPI_COMM_WORLD,2,ierr)
    end if
  end subroutine check

  subroutine distribute_io_work(npes, total_vars, var_names_in, var_nz_in, var_fileid_in, &
                                out_fileid, out_varname, out_lvlbegin, out_lvlend, out_nlev)
    implicit none

    ! --- Inputs ---
    integer, intent(in) :: npes, total_vars
    character(len=*), intent(in) :: var_names_in(total_vars)
    integer, intent(in) :: var_nz_in(total_vars)
    integer, intent(in) :: var_fileid_in(total_vars)

    ! --- Outputs ---
    ! Sized exactly to the number of MPI ranks (1 to npes)
    integer, intent(out) :: out_fileid(npes)
    character(len=72), intent(out) :: out_varname(npes)
    integer, intent(out) :: out_lvlbegin(npes)
    integer, intent(out) :: out_lvlend(npes)
    integer, intent(out) :: out_nlev(0:npes-1)

    ! --- Locals ---
    integer :: nlvl2d, nlvl3d, nlvl3d_small, nlvlcore
    integer, allocatable :: nlvl3d_list(:)
    integer :: i, k, nn, n3d, nz, mynlvl3d

    ! Initialize outputs to handle ranks that receive no work
    out_fileid = 0
    out_varname = ""
    out_lvlbegin = 0
    out_lvlend = 0
    out_nlev = 0

    ! Count levels and categorize variables
    nlvl2d = 0; nlvl3d = 0; nlvl3d_small = 0
    do i = 1, total_vars
      if (var_nz_in(i) > 1) then
        if (var_nz_in(i) <= 10) then
          nlvl3d_small = nlvl3d_small + var_nz_in(i)
        else
          nlvl3d = nlvl3d + 1
        endif
      else
        nlvl2d = nlvl2d + 1
      endif
    enddo

    ! Calculate cores per large 3D field
    if (nlvl3d > 0) then
      allocate(nlvl3d_list(nlvl3d))
      nlvlcore = (npes - nlvl2d - nlvl3d_small) / nlvl3d
      nlvl3d_list = nlvlcore

      ! Intentionally do NOT distribute the remainder cores. 
      ! By leaving the remainder cores completely idle, we guarantee 
      ! that every large 3D variable chunk perfectly aligns, allowing 
      ! safe point-to-point MPI transfers between variables.
      !nlvlcore = (npes - nlvl2d - nlvl3d_small) - (nlvl3d * nlvlcore)
      !if (nlvlcore > 0) then
      !  do k = 1, nlvlcore
      !    nlvl3d_list(k) = nlvl3d_list(k) + 1
      !  enddo
      !endif
    endif

    ! Assign levels to ranks
    nn = 0; n3d = 0
    do i = 1, total_vars
      nz = var_nz_in(i)
      if (nz > 10) then
        ! Distributed large 3D field
        n3d = n3d + 1
        mynlvl3d = min(nlvl3d_list(n3d), nz)
        nlvlcore = nz / mynlvl3d

        do k = 1, mynlvl3d
          nn = nn + 1
          if (nn > npes) exit

          out_varname(nn) = trim(var_names_in(i))
          out_fileid(nn)  = var_fileid_in(i)

          if (k == 1) then
            out_lvlbegin(nn) = 1
          else
            out_lvlbegin(nn) = out_lvlend(nn-1) + 1
          endif

          out_lvlend(nn) = out_lvlbegin(nn) + nlvlcore - 1
          if (k <= (nz - nlvlcore * mynlvl3d)) out_lvlend(nn) = out_lvlend(nn) + 1
        enddo
      else
        ! 2D field or small 3D field (assigned 1 level per rank)
        do k = 1, nz
          nn = nn + 1
          if (nn > npes) exit
          out_varname(nn) = trim(var_names_in(i))
          out_fileid(nn)  = var_fileid_in(i)
          out_lvlbegin(nn) = k
          out_lvlend(nn)   = k
        enddo
      endif
    enddo

    ! Calculate final array of level counts assigned to each rank (0-indexed for JEDI standard)
    do k = 1, npes
      if (out_lvlend(k) > 0 .and. out_lvlbegin(k) > 0) then
        out_nlev(k-1) = out_lvlend(k) - out_lvlbegin(k) + 1
      endif
    enddo

    !if(mype==0) then
    !  write(6,'(2a5,2x,a45,2a10)') "core","fid","varname","lvlbegin","lvlend"
    !  do k=1,npes
    !     write(6,'(2I5,2x,a45,2I10)') k,out_fileid(k),trim(out_varname(k)),out_lvlbegin(k),out_lvlend(k)
    !  enddo
    !  write(6,*) "======================================================================="
    !endif

    if (allocated(nlvl3d_list)) deallocate(nlvl3d_list)

  end subroutine distribute_io_work

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

end subroutine read_gfsens_new_

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

  if(nxPE > 0 .AND. nyPE > 0) then
    if( npe/=nxpe*nype ) then
      call die(myname_,'NPE inconsistent from  NxPE NyPE ',npe)
    endif
    !write(6,'("read_gfsens_: MPI layout " 2I6)') nxpe,nype
  else
    write(6,'("read_gfsens_: MPI layout not set")')
  endif

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
