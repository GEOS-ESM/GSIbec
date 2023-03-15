module m_gsibec

!use mpi

use constants, only: zero,one
use constants, only: Pa_per_kPa
use constants, only: constoz
use m_kinds, only: i_kind,r_kind
use m_mpimod, only: npe,mype,mpi_character,gsi_mpi_comm_world
use m_mpimod, only: setworld

use gsi_4dvar, only: nsubwin
use hybrid_ensemble_parameters, only: ntlevs_ens
use jfunc, only: nsclen,npclen,ntclen
use jfunc, only: mockbkg
use gridmod, only: lon2,lat2,lat1,lon1,nsig

use guess_grids, only: nfldsig
use guess_grids, only: gsiguess_init
use guess_grids, only: gsiguess_final
use guess_grids, only: gsiguess_set
use guess_grids, only: gsiguess_bkgcov_final
!use guess_grids, only: gsiguess_bkgcov_init

use state_vectors, only: allocate_state,deallocate_state

use control_vectors, only: control_vector
use control_vectors, only: allocate_cv,deallocate_cv
use control_vectors, only: assignment(=)
use control_vectors, only: cvars3d
use control_vectors, only: prt_control_norms
use control_vectors, only: inquire_cv
use control_vectors, only: cvars2d, cvars3d

use bias_predictors, only: predictors,allocate_preds,deallocate_preds,assignment(=)

use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundleprint
use gsi_bundlemod, only: assignment(=)

use gsimod, only: gsimain_initialize
use gsimod, only: gsimain_finalize

use m_berror_stats,only : berror_stats
use berror, only: simcv,bkgv_write_cv,bkgv_write_sv
use hybrid_ensemble_parameters,only : l_hyb_ens
use hybrid_ensemble_isotropic, only: hybens_grid_setup
use hybrid_ensemble_isotropic, only: bkerror_a_en

use general_sub2grid_mod, only: sub2grid_info
use general_sub2grid_mod, only: general_sub2grid_create_info
use general_sub2grid_mod, only: general_sub2grid_destroy_info

use mpeu_util, only: die
use m_mpimod, only: nxpe,nype
implicit none

private
public gsibec_init
public gsibec_init_guess
public gsibec_set_guess
public gsibec_get_grid
public gsibec_set_grid
!public gsibec_set_guess_aux
public gsibec_cv_space
public gsibec_sv_space
public gsibec_befname
public gsibec_final_guess
public gsibec_final

interface gsibec_init
  module procedure init_
end interface gsibec_init

interface gsibec_init_guess
  module procedure init_guess_
end interface gsibec_init_guess

interface gsibec_get_grid
  module procedure get_hgrid_
end interface gsibec_get_grid

interface gsibec_set_grid
  module procedure set_vgrid_
end interface gsibec_set_grid

interface gsibec_set_guess
  module procedure set_guess2_
  module procedure set_guess3_
end interface gsibec_set_guess

!interface gsibec_set_guess_aux
!  module procedure set_guess_aux_
!end interface gsibec_set_guess_aux

interface gsibec_cv_space
  module procedure be_cv_space0_
  module procedure be_cv_space1_
end interface gsibec_cv_space

interface gsibec_sv_space
  module procedure be_sv_space0_
  module procedure be_sv_space1_
end interface gsibec_sv_space

interface gsibec_befname
  module procedure befname_
end interface gsibec_befname

interface gsibec_final_guess
  module procedure final_guess_
end interface gsibec_final_guess

interface gsibec_final
  module procedure final_
end interface gsibec_final

logical :: initialized_ = .false.
logical :: iamset_ = .false.

character(len=*), parameter :: myname ="m_gsibec"
contains
  subroutine init_(cv,vgrid,bkgmock,nmlfile,befile,layout,comm)

  logical, intent(out) :: cv
  logical, optional, intent(in)  :: vgrid
  logical, optional, intent(out) :: bkgmock
  character(len=*),optional,intent(in) :: nmlfile
  character(len=*),optional,intent(in) :: befile
  integer,optional,intent(in) :: layout(2) ! 1=nx, 2=ny
  integer,optional,intent(in) :: comm

  character(len=*), parameter :: myname_=myname//"init_"
  type(sub2grid_info) :: sg
  integer :: ier
  logical :: already_init_mpi

  if (initialized_) then
     call final_(.false.) ! finalize what should have been finalized
  endif

  ier=0
  call mpi_initialized(already_init_mpi,ier)
  if(ier/=0) call die(myname,'mpi_initialized(), ieror =',ier)
  if(.not.already_init_mpi) then
     call mpi_init(ier)
     if(ier/=0) call die(myname,'mpi_init(), ier =',ier)
  endif
  call setworld(comm=comm)
  call mpi_comm_size(gsi_mpi_comm_world,npe,ier)
  call mpi_comm_rank(gsi_mpi_comm_world,mype,ier)

  if (present(layout)) then
     nxpe=layout(1)
     nype=layout(2)
  endif
  if (present(befile)) then
     call befname_(befile,0)
  endif
  call gsimain_initialize(nmlfile=nmlfile)
  call set_(vgrid=vgrid)
  if(l_hyb_ens) then
    call hybens_grid_setup()
  endif
  call set_pointer_()

! create subdomain/grid indexes 
! call general_sub2grid_create_info(sg,0,nlat,nlon,nsig,1,.false.)
! istart=sg%istart
! jstart=sg%jstart
! call general_sub2grid_destroy_info(sg)

  cv = simcv
  if (present(bkgmock) ) then
    bkgmock = mockbkg
  endif
  initialized_=.true.
  end subroutine init_
!--------------------------------------------------------
  subroutine init_guess_
  call gsiguess_init(mockbkg=mockbkg)
! call gsiguess_bkgcov_init()  ! not where I want for this to be
  end subroutine init_guess_
!--------------------------------------------------------
  subroutine set_guess2_(varname,var)
  character(len=*),intent(in) :: varname
  real(r_kind),intent(in) :: var(:,:)
  call gsiguess_set(varname,var) 
  end subroutine set_guess2_
!--------------------------------------------------------
  subroutine set_guess3_(varname,var)
  character(len=*),intent(in) :: varname
  real(r_kind),intent(in) :: var(:,:,:)
  call gsiguess_set(varname,var) 
  end subroutine set_guess3_
!--------------------------------------------------------
  subroutine final_(closempi)

  logical, intent(in) :: closempi

  call gsiguess_bkgcov_final()
  call gsiguess_final()
  call unset_()
  call gsimain_finalize(closempi)
  initialized_=.false.

  end subroutine final_
!--------------------------------------------------------
  subroutine get_hgrid_ (eqspace,units,gsi_lats,gsi_lons) ! for now: redundant routine
  use constants, only: init_constants_derived
  use constants, only: pi,one,two,half,rad2deg
  use mpeu_util, only: die
  implicit none
   logical,intent(in) :: eqspace
   character(len=*), intent(in) :: units
   real(r_kind),intent(inout) :: gsi_lats(:),gsi_lons(:)
   character(len=*),parameter :: myname_=myname//"*get_hgrid_"
   real(r_kind) :: amlon,dlat,dlon,pih
   real(r_kind) :: loncir,latcir,latcen
   real(r_kind),allocatable,dimension(:) :: wlatx,slatx
   integer i,j,jb,je,idrt,mlon,mlat,jmax

!  Unfortunately, need to make sure basic constants are initialized
   call init_constants_derived

   mlat=size(gsi_lats)
   mlon=size(gsi_lons)
   if (eqspace) then ! regular Lat-Lon grid

      if(trim(units)=='degree') then
        loncir=360._r_kind
        latcir=180._r_kind
        latcen= 90._r_kind
      else
        loncir=pi+pi
        latcir=pi
        latcen=half*pi
      endif
      dlon=loncir/mlon
      dlat=latcir/(mlat-1)
      do i=1,mlon                       ! from 0 to 2pi
         gsi_lons (i)=(i-one)*dlon
      enddo
      do j=1,mlat                       ! from -pi/2 to +pi/2
         gsi_lats(j)=(j-one)*dlat - latcen
      end do

   else ! Gaussian grid

      ! Set local constants
      pih=half*pi
      amlon=float(mlon)
      dlon=two*pi/amlon
      jmax=mlat-2
      jb=1;je=(jmax+1)/2
      allocate(wlatx(jmax),slatx(jmax))

      ! Longitudes
      do i=1,mlon
         gsi_lons(i)=float(i-1)*dlon
      enddo

      ! Gaussian Latitudes
      idrt=4 ! defines Gaussian grid
      call splat(idrt,jmax,slatx,wlatx)

      gsi_lats(1)=-pih
      do j=jb,je
        i=j+1
        gsi_lats(i)=-asin(slatx(j))
        i=mlat-j
        gsi_lats(i)=asin(slatx(j))
      enddo
      gsi_lats(mlat)=pih
      deallocate(wlatx,slatx)

      if (trim(units)=='degree') then
         gsi_lons=gsi_lons*rad2deg
         gsi_lats=gsi_lats*rad2deg
      endif

   endif

  end subroutine get_hgrid_
!--------------------------------------------------------
  subroutine set_vgrid_(myid,akbk)
  use gridmod, only: gridmod_vgrid
  implicit none
  integer(i_kind),intent(in)  :: myid
  character(len=*),intent(in) :: akbk
  call gridmod_vgrid(myid,fname=akbk)
  end subroutine set_vgrid_
!--------------------------------------------------------
  subroutine set_(vgrid)

   use constants, only: pi,one,half,rearth
   use m_mpimod, only: mype
   use gridmod, only: nlon,nlat
   use gridmod, only: rlats,rlons,wgtlats
   use gridmod, only: coslon,sinlon
   use gridmod, only: rbs2
   use gridmod, only: sp_a
   use gridmod, only: create_grid_vars
   use gridmod, only: use_sp_eqspace
   use gridmod, only: gridmod_vgrid
   use compact_diffs, only: cdiff_created
   use compact_diffs, only: cdiff_initialized
   use compact_diffs, only: create_cdiff_coefs
   use compact_diffs, only: inisph
!  use mp_compact_diffs_mod1, only: init_mp_compact_diffs1
!  use compact_diffs, only: uv2vordiv
   implicit none
   logical,optional :: vgrid

   real(r_kind) :: dlat,dlon,pih
   integer i,j,i1,ifail

   if (iamset_ ) return

   call create_grid_vars()
   ifail=0
   if(.not.allocated(rlons)) ifail = 1
   if(.not.allocated(rlats)) ifail = 1
   if(ifail/=0) call die('init','dims not alloc', 99)

   if (use_sp_eqspace) then
      dlon=(pi+pi)/nlon    ! in radians
      dlat=pi/(nlat-1)

! Set grid longitude array used by GSI.
      do i=1,nlon                       ! from 0 to 2pi
         rlons (i)=(i-one)*dlon
         coslon(i)=cos(rlons(i))
         sinlon(i)=sin(rlons(i))
      end do

! Set grid latitude array used by GSI.
      pih =half*pi
      do j=1,nlat                       ! from -pi/2 to +pi/2
         rlats(j)=(j-one)*dlat - pih
      end do

! wgtlats is used by spectral code. The values are used as divisor in the
! compact_diffs::inisph() routine.  Therefore, set to TINY instead of ZERO.
!     wgtlats(:)=TINY(wgtlats)
      wgtlats=zero
      do i=sp_a%jb,sp_a%je
         i1=i+1
         wgtlats(i1)=sp_a%wlat(i) !sp_a%clat(i)
         i1=nlat-i
         wgtlats(i1)=sp_a%wlat(i) !sp_a%clat(i)
      end do

! rbs2=1/cos^2(rlats)) is used in pcp.  polar points are set to zeroes.
      rbs2(1       )=zero
      rbs2(2:nlat-1)=cos(rlats(2:nlat-1))
      rbs2(2:nlat-1)=one/(rbs2(2:nlat-1)*rbs2(2:nlat-1))
      rbs2(  nlat  )=zero
   else
      if(mype==0) print *, 'Gaussian Grid in Use'
      call gengrid_vars
   endif

   if(present(vgrid)) then
     if(vgrid) call gridmod_vgrid(mype)
   endif
   if(.not.cdiff_created()) call create_cdiff_coefs()
   if(.not.cdiff_initialized()) call inisph(rearth,rlats(2),wgtlats(2),nlon,nlat-2)
!  call init_mp_compact_diffs1(nsig+1,mype,.false.)
   iamset_ = .true.
  end subroutine set_
!--------------------------------------------------------
  subroutine unset_
   use gridmod, only: destroy_grid_vars
   use compact_diffs, only: cdiff_created
   use compact_diffs, only: destroy_cdiff_coefs
   implicit none
   if(cdiff_created()) call destroy_cdiff_coefs
   call destroy_grid_vars
   iamset_ = .false.
  end subroutine unset_
!--------------------------------------------------------
  subroutine set_pointer_
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_pointer
!   prgmmr: treadon          org: np23                date: 2004-07-28
!
! abstract: Set length of control vector and other control 
!           vector constants
!
! program history log:
!   2004-07-28  treadon
!   2006-04-21  kleist - include pointers for more time tendency arrays
!   2008-12-04  todling - increase number of 3d fields from 6 to 8 
!   2009-09-16  parrish - add hybrid_ensemble connection in call to setup_control_vectors
!   2010-03-01  zhu     - add nrf_levb and nrf_leve, generalize nval_levs
!                       - generalize vector starting points such as nvpsm, nst2, and others
!   2010-05-23  todling - remove pointers such as nvpsm, nst2, and others (intro on 10/03/01)
!                       - move nrf_levb and nrf_leve to anberror where they are needed
!   2010-05-29  todling - generalized count for number of levels in state variables
!   2013-10-22  todling - revisit level count in view of changes to bundle
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use gridmod, only: latlon11,latlon1n,nsig,lat2,lon2
    use gridmod, only: nlat,nlon
    use gridmod, only: nnnn1o
    use state_vectors, only: ns2d,levels
    use constants, only : max_varname_length
    use bias_predictors, only: setup_predictors
    use control_vectors, only: nc2d,nc3d
    use control_vectors, only: setup_control_vectors
    use state_vectors, only: setup_state_vectors
    use hybrid_ensemble_parameters, only: l_hyb_ens,n_ens,generate_ens,grd_ens
    use jfunc, only: nval_lenz
    use jfunc, only: nclenz
    use jfunc, only: npred,jpch_rad
    use jfunc, only: npredp,npcptype
    implicit none

    character(len=*),parameter :: myname_=myname//'set_pointer_'
    integer(i_kind) n_ensz,nval_lenz_tot,nval_lenz_enz

    integer(i_kind) nvals_levs,nval_len
    integer(i_kind) nvals_len,nval_levs
    integer(i_kind) nclen,nrclen,nval2d
    logical lsqrtb

    if(lsqrtb) then
      call die(myname_,': cannot handle lsqrtb=.t.',999)
    endif

    nvals_levs=max(0,ns2d)+sum(levels)
    nvals_len=nvals_levs*latlon11

    nval_levs=max(0,nc3d)*nsig+max(0,nc2d)
    nval_len=nval_levs*latlon11
    if(l_hyb_ens) then
       nval_len=nval_len+n_ens*grd_ens%nsig*grd_ens%latlon11
    end if
    nsclen=npred*jpch_rad
    npclen=npredp*npcptype
    ntclen=0
    nclen=nsubwin*nval_len+nsclen+npclen+ntclen
    nrclen=nsclen+npclen+ntclen
  
    n_ensz=0
    nval_lenz_enz=0
    if(l_hyb_ens.and.generate_ens) then
       call set_sqrt_2dsize_(nval2d)
       nval_lenz=nval2d*nnnn1o
       nval_lenz_tot=nval_lenz
       nclenz=nsubwin*nval_lenz_tot+nsclen+npclen+ntclen
    else
       nval2d=latlon11
    end if

    CALL setup_control_vectors(nsig,lat2,lon2,latlon11,latlon1n, &
                               nsclen,npclen,ntclen,nclen,nsubwin,&
                               nval_len,lsqrtb,n_ens, &
                               nval_lenz_enz)
    CALL setup_predictors(nrclen,nsclen,npclen,ntclen)
    CALL setup_state_vectors(latlon11,latlon1n,nvals_len,lat2,lon2,nsig)

  end subroutine set_pointer_
!--------------------------------------------------------

  subroutine set_sqrt_2dsize_(ndim2d)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    set_sqrt_2dsize
!   prgmmr: todling          org: np23                date: 2011-09-05
!
! abstract: Calculates size of 2d-component of control vector in sqrt-B
!           case. This being an independent call allows using ckgcov
!           within context of B-precond.
!
! program history log:
!   2011-09-05  todling - move as independent piece out of set_pointer
!
!   input argument list:
!
!   output argument list:
!     ndim2d - size of 2d component of control vector in sqrt-B case
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
  use m_kinds, only: i_kind
  use gridmod, only: nlat,nlon
  implicit none
  integer(i_kind),intent(out):: ndim2d
  integer(i_kind) nx,ny,mr,nr,nf
!           following lifted from subroutine create_berror_vars in module berror.f90
!            inserted because create_berror_vars called after this routine
     nx=nlon*3/2
     nx=nx/2*2
     ny=nlat*8/9
     ny=ny/2*2
     if(mod(nlat,2)/=0)ny=ny+1
     mr=0
     nr=nlat/4
     nf=nr
     ndim2d=(ny*nx + 2*(2*nf+1)*(2*nf+1))*3
  end subroutine set_sqrt_2dsize_
!--------------------------------------------------------

  subroutine set_silly_(bundle)
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none
  type(gsi_bundle) bundle
  character(len=*), parameter :: myname_ = myname//'*set_silly_'
  real(r_kind),pointer :: ptr3(:,:,:)=>NULL()
  real(r_kind),pointer :: ptr2(:,:)=>NULL()
  integer xloc, yloc, k,iset
  integer ier
  integer zex(4),zex072(4), zex127(4)
  real(r_kind) :: val
  character(len=2) :: var
  character(len=80):: ifname(1)
  character(len=80):: ofname
! logical :: fexist
!ifname = 'xinc.eta.nc4'
!ofname = 'outvec_bkgtest'
!inquire(file=ifname(1),exist=fexist)
!if(.not.fexist) then
!  call die ('main',': fishy', 99)
!endif
!
  if (mod(mype,3) /= 0) return
!
!            sfc  ~500  ~10   ~1
  zex072 = (/  1,   23,  48,  58 /)
  zex127 = (/  1,   53, 106, 116 /)
  iset=-1
  if (nsig==72) then
    zex=zex072
    iset=1
  endif
  if (nsig==127) then
    zex=zex127
    iset=1
  endif
  xloc=min(20,lat2)
  yloc=min(20,lon2)
  val=one
  var='sf'
  var='q'
  var='t'
  var='tv'
  if (iset<0) call die(myname_,'no input set',99)
  call gsi_bundlegetpointer(bundle,trim(var),ptr3,ier)
  if(ier==0) then
     if(var=='sf' .or. var=='vp') then
       val=val*1e-5
     endif
     do k=1,size(zex)
        ptr3(xloc,yloc,zex(k)) = val
     enddo
     if (mype==0) print *, myname_, ': var= ', trim(var)
     return
  endif
  if(var == 'tv') then
     call gsi_bundlegetpointer(bundle,trim(var),ptr3,ier)
     if(ier==0) then
        do k=1,size(zex)
           ptr3(xloc,yloc,zex(k)) = val
        enddo
        if (mype==0) print *, myname_, ': var= ', trim(var)
        return
     endif
  endif
  if (var == 'ps') then
     call gsi_bundlegetpointer(bundle,'ps',ptr2,ier)
     if(ier==0) then
        ptr2(xloc,yloc) = 100.
        if (mype==0) print *, myname_, ': var= ', 'ps(Pa)'
        return
     endif
  endif
  end subroutine set_silly_
!--------------------------------------------------------
  subroutine be_cv_space0_

  type(control_vector) :: gradx,grady

! apply B to vector: all in control space

! allocate vectors
  call allocate_cv(gradx)
  call allocate_cv(grady)
  gradx=zero
  grady=zero

  call set_silly_(gradx%step(1))
  call gsi2model_units_ad_(gradx%step(1))

  call bkerror(gradx,grady, &
               1,nsclen,npclen,ntclen)
  if (l_hyb_ens) then
     call bkerror_a_en(gradx,grady)
  endif

  if(bkgv_write_cv/='null') &
  call write_bundle(grady%step(1),bkgv_write_cv)

  call gsi2model_units_(grady%step(1))

! clean up
  call deallocate_cv(gradx)
  call deallocate_cv(grady)

  end subroutine be_cv_space0_

  subroutine be_cv_space1_(gradx,internalcv,bypassbe)

  type(control_vector) :: gradx
  logical,optional,intent(in) :: internalcv
  logical,optional,intent(in) :: bypassbe

  type(control_vector) :: grady

  logical bypassbe_

  bypassbe_ = .false.
  if (present(bypassbe)) then
     if (bypassbe) bypassbe_ = .true.
  endif

! apply B to vector: all in control space
  if (present(internalcv)) then
     if(internalcv) call set_silly_(gradx%step(1))
  endif

! convert model units to gsi
  call gsi2model_units_ad_(gradx%step(1))

! allocate vectors
  call allocate_cv(grady)

  if (bypassbe_) then
     grady=gradx
  else
     grady=zero
     call bkerror(gradx,grady, &
                  1,nsclen,npclen,ntclen)
     if (l_hyb_ens) then
        call bkerror_a_en(gradx,grady)
     endif
  endif

  if(bkgv_write_cv/='null') &
  call write_bundle(grady%step(1),bkgv_write_cv)

! return result in input vector
  gradx=grady

! convert units back to model units
  call gsi2model_units_(gradx%step(1))

! clean up
  call deallocate_cv(grady)

  end subroutine be_cv_space1_
!--------------------------------------------------------
  subroutine be_sv_space0_

  type(gsi_bundle), allocatable :: mval(:)
  type(control_vector) :: gradx,grady
  type(predictors)     :: sbias
  integer ii

! start work space
  allocate(mval(nsubwin))
  do ii=1,nsubwin
      call allocate_state(mval(ii))
      mval(ii) = zero
  end do
  call allocate_preds(sbias)

  call allocate_cv(gradx)
  call allocate_cv(grady)
  gradx=zero
  grady=zero

! get test vector (mval)
! call get_state_perts_ (mval(1))
!
  call set_silly_(mval(1))
  call gsi2model_units_ad_(mval(1))

  call control2state_ad(mval,sbias,gradx)

! apply B to input (transformed) vector
  call bkerror(gradx,grady, &
               1,nsclen,npclen,ntclen)
  if (l_hyb_ens) then
     call bkerror_a_en(gradx,grady)
  endif

  call control2state(grady,mval,sbias)

! if so write out fields from gsi (in GSI units)
  if(bkgv_write_sv/='null') &
  call write_bundle(mval(1),bkgv_write_sv)

! convert back to model units (just for consistency here)
  call gsi2model_units_(mval(1))

! clean up work space
  call deallocate_cv(gradx)
  call deallocate_cv(grady)
  call deallocate_preds(sbias)
  do ii=1,nsubwin
      call deallocate_state(mval(ii))
  end do
  deallocate(mval)

  end subroutine be_sv_space0_
!--------------------------------------------------------
  subroutine be_sv_space1_(mval,internalsv,bypassbe)

  type(gsi_bundle) :: mval(1)
  type(gsi_bundle) :: eval(1)
  logical,optional,intent(in) :: internalsv
  logical,optional,intent(in) :: bypassbe

  type(control_vector) :: gradx,grady
  type(predictors)     :: sbias
  logical bypassbe_
  integer ii,ier

  if (nsubwin/=1) then
     if(ier/=0) call die(myname,'cannot handle this nsubwin =',nsubwin)
  endif
  if (ntlevs_ens/=1) then
     if(ier/=0) call die(myname,'cannot handle this ntlevs_ens =',ntlevs_ens)
  endif

  bypassbe_ = .false.
  if (present(bypassbe)) then
     if (bypassbe) bypassbe_ = .true.
  endif

! start work space
  if (l_hyb_ens) then
     do ii=1,ntlevs_ens
       call allocate_state(eval(ii))
    end do
  endif
  call allocate_preds(sbias)
  call allocate_cv(gradx)
  call allocate_cv(grady)
  gradx=zero
  grady=zero

! get test vector (mval)
! call get_state_perts_ (mval(1))
  if (present(internalsv)) then
     if (internalsv) call set_silly_(mval(1))
  endif

! convert from model to gsi units
  call gsi2model_units_ad_(mval(1))

  if (l_hyb_ens) then
     eval=mval(1)
     call ensctl2state_ad(eval,mval,gradx)
  endif
  call control2state_ad(mval,sbias,gradx)

! apply B to input (transformed) vector
  if (bypassbe_) then
    grady=gradx
  else
    call bkerror(gradx,grady, &
                 1,nsclen,npclen,ntclen)
    if (l_hyb_ens) then
       call bkerror_a_en(gradx,grady)
    endif
  endif

  call control2state(grady,mval,sbias)
  if (l_hyb_ens) then
     call ensctl2state(grady,mval,eval)
  end if

! if so write out fields from gsi (in GSI units)
  if(bkgv_write_sv/='null') &
  call write_bundle(mval(1),bkgv_write_sv)

! convert from gsi to model units
  call gsi2model_units_(mval(1))

! clean up work space
  call deallocate_cv(gradx)
  call deallocate_cv(grady)
  call deallocate_preds(sbias)
  if (l_hyb_ens) then
     do ii=ntlevs_ens,1,-1
       call deallocate_state(eval(ii))
    end do
  endif

  end subroutine be_sv_space1_
!--------------------------------------------------------
  subroutine get_state_perts_(fc)
  use m_grid2sub1var, only: grid2sub1var
  use gridmod, only: nlon,nlat,lat1,lon1
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none
  type(gsi_bundle) :: fc
  real(r_kind),allocatable :: grdfld(:,:,:)
  real(r_kind),allocatable :: subfld(:,:,:)
  real(r_kind), pointer :: ptr3d(:,:,:)=>NULL()
  integer ii,ier
  if (mype==0) then
     allocate(grdfld(nlat,nlon,nsig))
     grdfld=zero
  else
     allocate(grdfld(0,0,0))
  endif
  allocate(subfld(lat2,lon2,nsig))
  call grid2sub1var (grdfld,subfld,0,ier)
  do ii=1,fc%n3d
     call gsi_bundlegetpointer(fc,trim(fc%r3(ii)%shortname),ptr3d,ier)
     ptr3d = subfld
  enddo
  deallocate(subfld)
  deallocate(grdfld)
  end subroutine get_state_perts_
!--------------------------------------------------------
  subroutine befname_ (fname,root)
  implicit none
  character(len=*),intent(in) :: fname
  integer, intent(in) :: root
  character(len=*), parameter :: myname_ = myname//"*befname"
  integer ier,clen
  if(mype==root) then
    write(6,'(3a)') myname_, ": reading B error-coeffs from ", trim(fname)
    berror_stats = trim(fname)
  endif
  clen=len(berror_stats)
  call mpi_bcast(berror_stats,clen,mpi_character,root,gsi_mpi_comm_world,ier)
  end subroutine befname_
!--------------------------------------------------------
  subroutine gsi2model_units_(bundle)
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none
  type(gsi_bundle) bundle
  real(r_kind),pointer :: ptr2(:,:)  =>NULL()
  real(r_kind),pointer :: ptr3(:,:,:)=>NULL()
  integer ier
  call gsi_bundlegetpointer(bundle,'ps',ptr2,ier)
  if(ier==0) then
     ptr2 = ptr2 * Pa_per_kPa
  endif
  call gsi_bundlegetpointer(bundle,'oz',ptr3,ier)
  if(ier==0) then
     ptr3 = ptr3 * constoz
  endif
  end subroutine gsi2model_units_
!--------------------------------------------------------
  subroutine gsi2model_units_ad_(bundle)
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none
  type(gsi_bundle) bundle
  real(r_kind),pointer :: ptr2(:,:)  =>NULL()
  real(r_kind),pointer :: ptr3(:,:,:)=>NULL()
  integer ier
  call gsi_bundlegetpointer(bundle,'ps',ptr2,ier)
  if(ier==0) then
     ptr2 = ptr2 * Pa_per_kPa
  endif
  call gsi_bundlegetpointer(bundle,'oz',ptr3,ier)
  if(ier==0) then
     ptr3 = ptr3 * constoz
  endif
  end subroutine gsi2model_units_ad_
!--------------------------------------------------------
  subroutine final_guess_
  end subroutine final_guess_
end module m_gsibec
