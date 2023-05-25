module gsi_enperts_mod
use m_kinds, only: i_kind,r_single
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_grid
use gsi_bundlemod, only: gsi_gridcreate
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundleunset
use control_vectors, only: nc3d,cvars3d
use control_vectors, only: nc2d,cvars2d
implicit none
private
public :: gsi_enperts
public :: gsi_create_ensemble
public :: gsi_destroy_ensemble
! following is for storage of ensemble perturbations:

!   def en_perts            - array of ensemble perturbations
!   def nelen               - length of one ensemble perturbation vector

type gsi_enperts
  integer(i_kind) nelen
  type(gsi_bundle),allocatable :: en_perts(:,:)
  real(r_single),dimension(:,:,:),allocatable:: ps_bar
end type gsi_enperts

interface gsi_create_ensemble
  module procedure create_ensemble_
end interface
interface gsi_destroy_ensemble
  module procedure destroy_ensemble_
end interface

character(len=*),parameter :: myname = 'gsi_enperts_mod'
logical, parameter :: debug = .true.

contains

  subroutine create_ensemble_(epts)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_ensemble        allocate space for ensembles
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: allocate space for ensemble perturbations used with the 
!             hybrid ensemble option.
!
! program history log:
!   2009-06-16  parrish
!   2010-02-20  parrish  modifications for dual resolution
!   2011-02-28  parrish - introduce more complete use of gsi_bundlemod to eliminate hard-wired variables
!   2011-08-31  todling - revisit en_perts (single-prec) in light of extended bundle
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use hybrid_ensemble_parameters, only: n_ens,grd_ens,ntlevs_ens

    implicit none

    type(gsi_enperts) :: epts

    type(gsi_grid) :: grid_ens
    integer(i_kind) n,istatus,m
    character(len=*),parameter::myname_=trim(myname)//'*create_ensemble'

    epts%nelen=grd_ens%latlon11*(max(0,nc3d)*grd_ens%nsig+max(0,nc2d))
!   create ensemble perturbations bundles (using newly added r_single capability

    allocate(epts%en_perts(n_ens,ntlevs_ens))
    call gsi_gridcreate(grid_ens,grd_ens%lat2,grd_ens%lon2,grd_ens%nsig)
 
    do m=1,ntlevs_ens
       do n=1,n_ens
          call gsi_bundlecreate(epts%en_perts(n,m),grid_ens,'ensemble perts',istatus, &
                                names2d=cvars2d,names3d=cvars3d,bundle_kind=r_single)
          if(istatus/=0) then
             write(6,*)trim(myname_),': trouble creating en_perts bundle'
             call stop2(999)
          endif
       enddo
    enddo


    allocate(epts%ps_bar(grd_ens%lat2,grd_ens%lon2,ntlevs_ens) )
    if(debug) then
       write(6,*)' in create_ensemble, grd_ens%latlon11,grd_ens%latlon1n,n_ens,ntlevs_ens=', &
                                 grd_ens%latlon11,grd_ens%latlon1n,n_ens,ntlevs_ens
       write(6,*)' in create_ensemble, total bytes allocated=',4*epts%nelen*n_ens*ntlevs_ens
    end if
    return

  end subroutine create_ensemble_

  subroutine destroy_ensemble_(epts)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_ensemble       deallocate space for ensembles
!   prgmmr: parrish          org: np22                date: 2009-06-16
!
! abstract: deallocate space for ensemble perturbations used with the 
!             hybrid ensemble option.
!
! program history log:
!   2009-06-16  parrish
!   2011-02-28  parrish, replace specific ensemble perturbation arrays with pseudo-bundle en_perts array
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use hybrid_ensemble_parameters, only: l_hyb_ens,n_ens,ntlevs_ens
    implicit none

    type(gsi_enperts) :: epts
    integer(i_kind) istatus,n,m

    if(l_hyb_ens) then
       do m=1,ntlevs_ens
          do n=1,n_ens
             call gsi_bundleunset(epts%en_perts(n,m),istatus)
             if(istatus/=0) then
                write(6,*)'in destroy_ensemble: trouble destroying en_perts bundle'
                call stop2(999)
             endif
          enddo
       enddo
       deallocate(epts%ps_bar)
       deallocate(epts%en_perts)
    end if
    return

  end subroutine destroy_ensemble_
end module gsi_enperts_mod
