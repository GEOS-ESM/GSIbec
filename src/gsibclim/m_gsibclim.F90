module m_gsibclim

!use mpi

use constants, only: zero,one
use m_kinds, only: i_kind,r_kind
use m_mpimod, only: mype
use gridmod, only: nlon,nlat,lon2,lat2,lon2,nsig
use guess_grids, only: nfldsig
use guess_grids, only: guess_grids_init
use guess_grids, only: guess_grids_final
use m_rf, only: rf_set,rf_unset
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
use mpeu_util, only: die
use gsimod, only: gsimain_initialize
use gsimod, only: gsimain_finalize
use berror, only: simcv
use jfunc, only: nsubwin,nsclen,npclen,ntclen

implicit none

private
public gsibclim_init
public gsibclim_final

interface gsibclim_init
  module procedure init_
end interface gsibclim_init
interface gsibclim_final
  module procedure final_
end interface gsibclim_final
character(len=*), parameter :: myname ="m_gsibclim"
contains
  subroutine init_

  integer :: ier
  logical :: already_init_mpi

  ier=0
  call mpi_initialized(already_init_mpi,ier)
  if(ier/=0) call die(myname,'mpi_initialized(), ieror =',ier)
  if(.not.already_init_mpi) then
     call mpi_init(ier)
     if(ier/=0) call die(myname,'mpi_init(), ier =',ier)
  endif

  call gsimain_initialize()
  call set_()
  call set_pointer_()
  call guess_grids_init()
  call rf_set(mype)

  end subroutine init_
  subroutine final_

  call rf_unset()
  call guess_grids_final()
  call unset_()
  call gsimain_finalize()

  end subroutine final_
  subroutine set_
   use constants, only: pi,one,half,rearth
   use gridmod, only: rlats,rlons,wgtlats
   use gridmod, only: coslon,sinlon
   use gridmod, only: rbs2
   use gridmod, only: sp_a
   use gridmod, only: create_grid_vars
   use gridmod, only: use_sp_eqspace
   use compact_diffs, only: cdiff_created
   use compact_diffs, only: cdiff_initialized
   use compact_diffs, only: create_cdiff_coefs
   use compact_diffs, only: inisph
!  use mp_compact_diffs_mod1, only: init_mp_compact_diffs1
!  use compact_diffs, only: uv2vordiv
   implicit none
   real(r_kind) :: dlat,dlon,pih
   integer i,j,i1,ifail
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
      print *, 'Gaussian Grid in Use'
      call gengrid_vars
   endif

   if(.not.cdiff_created()) call create_cdiff_coefs()
   if(.not.cdiff_initialized()) call inisph(rearth,rlats(2),wgtlats(2),nlon,nlat-2)
!  call init_mp_compact_diffs1(nsig+1,mype,.false.)
  end subroutine set_
 
  subroutine unset_
   use compact_diffs, only: cdiff_created
   use compact_diffs, only: destroy_cdiff_coefs
   if(cdiff_created()) call destroy_cdiff_coefs
  end subroutine unset_

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
    use state_vectors, only: ns2d,levels
    use constants, only : max_varname_length
    use bias_predictors, only: setup_predictors
    use control_vectors, only: nc2d,nc3d
    use control_vectors, only: setup_control_vectors
    use state_vectors, only: setup_state_vectors
    implicit none

    integer(i_kind) n_ensz,nval_lenz_tot,nval_lenz_enz

    integer(i_kind) n_ens,npred,jpch_rad,npredp,npcptype,nvals_levs,nval_len
    integer(i_kind) nvals_len,nval_levs,nclen,nrclen,nclen1,nclen2,nval2d
    logical lsqrtb

    npred=0
    jpch_rad=0
    npredp=0
    npcptype=0
    lsqrtb=.false.
    n_ens=0

    nvals_levs=ns2d+sum(levels)
    nvals_len=nvals_levs*latlon11

    nval_levs=max(0,nc3d)*nsig+max(0,nc2d)
    nval_len=nval_levs*latlon11
    nsclen=npred*jpch_rad
    npclen=npredp*npcptype
    ntclen=0
    nclen=nsubwin*nval_len+nsclen+npclen+ntclen
    nrclen=nsclen+npclen+ntclen
    nclen1=nclen-nrclen
    nclen2=nclen1+nsclen
  
    n_ensz=0
    nval_lenz_enz=0
    nval2d=latlon11

    CALL setup_control_vectors(nsig,lat2,lon2,latlon11,latlon1n, &
                               nsclen,npclen,ntclen,nclen,nsubwin,nval_len,lsqrtb,n_ens, &
                               nval_lenz_enz)
    CALL setup_predictors(nrclen,nsclen,npclen,ntclen)
    CALL setup_state_vectors(latlon11,latlon1n,nvals_len,lat2,lon2,nsig)

  end subroutine set_pointer_

end module m_gsibclim
