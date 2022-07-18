module guess_grids
use m_kinds, only: i_kind, r_kind
use m_mpimod, only: mype
use mpeu_util, only: tell,die
use constants, only: fv,one,max_varname_length
use constants, only: kPa_per_Pa
use gridmod, only: nlon,nlat,lon2,lat2,nsig,idsl5
use gridmod, only: ak5,bk5
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_metguess_mod, only: gsi_metguess_bundle
use gsi_metguess_mod, only: gsi_metguess_get
use gsi_metguess_mod, only: gsi_metguess_create_grids
use gsi_metguess_mod, only: gsi_metguess_destroy_grids
use m_rf, only: rf_set,rf_unset
implicit none
private
!
public :: ges_prsi
public :: ges_prsl
public :: ges_tsen
public :: ges_qsat
public :: ges_z

public :: geop_hgtl
public :: isli2
public :: fact_tv
public :: tropprs

public :: guess_grids_init
public :: guess_grids_final
public :: gsiguess_get_ref_gesprs
public :: gsiguess_basics
public :: gsiguess_bkgcov_init
public :: gsiguess_bkgcov_final

public :: nfldsig
public :: ntguessig

public :: tsensible
logical, parameter ::  tsensible = .false.   ! jfunc: here set as in jfunc
                          !        gsi handles this completely
                          !        incorrectly - this should
                          !        just be controlled in the
                          !        cv table tv for virt. t; t
                          !        for sensible t - would need
                          !        to generalize the spots
                          !        where cold thinks only temp
                          !        var in cv is tv to be tv and t
logical, parameter ::  use_compress = .true.   ! wired for now

! For now turned into wired-in parameters
integer(i_kind),parameter :: nfldsig =  1
integer(i_kind),parameter :: ntguessig = 1

real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsl
real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsi
real(r_kind),allocatable,dimension(:,:,:,:):: ges_tsen
real(r_kind),allocatable,dimension(:,:,:,:):: ges_qsat
real(r_kind),allocatable,dimension(:,:,:  ):: ges_z

real(r_kind),allocatable,dimension(:,:,:,:):: geop_hgtl
real(r_kind),allocatable,dimension(:,:,:,:):: geop_hgti
real(r_kind),allocatable,dimension(:,:,:):: fact_tv
real(r_kind),allocatable,dimension(:,:):: tropprs
integer(i_kind),allocatable,dimension(:,:):: isli2

interface guess_grids_init; module procedure init_; end interface
interface guess_grids_final; module procedure final_; end interface
interface gsiguess_get_ref_gesprs; module procedure get_ref_gesprs_; end interface

interface gsiguess_basics
  module procedure guess_basics0_
  module procedure guess_basics2_
  module procedure guess_basics3_
end interface gsiguess_basics

interface gsiguess_bkgcov_init
  module procedure bkgcov_init_
end interface gsiguess_bkgcov_init

interface gsiguess_bkgcov_final
  module procedure bkgcov_final_
end interface gsiguess_bkgcov_final

logical, save :: initialized_ = .false.
logical, save :: iamset_ = .false.

character(len=*), parameter :: myname="guess_grids"
contains
!--------------------------------------------------------
subroutine init_(mockbkg)
  logical,optional :: mockbkg
  integer ier
  logical mockbkg_
  call create_metguess_grids_(mype,ier)
  mockbkg_=.false.
  if (present(mockbkg)) then
    if(mockbkg) mockbkg_ = .true.
  endif
  if(mockbkg_) then
    call guess_basics0_
    if (mype==0) then
       print *, "Generating mock guess-fields -- for testing only"
   endif
  else
    if (mype==0) then
       print *, "User expected to provide guess-fields"
    endif
  endif
  allocate(ges_tsen(lat2,lon2,nsig,nfldsig))
  allocate(ges_prsi(lat2,lon2,nsig+1,nfldsig))
  allocate(ges_prsl(lat2,lon2,nsig+1,nfldsig))
  allocate(ges_qsat(lat2,lon2,nsig,nfldsig))
  allocate(ges_z(lat2,lon2,nfldsig))
  allocate(geop_hgtl(lat2,lon2,nsig,nfldsig))
  allocate(geop_hgti(lat2,lon2,nsig,nfldsig))
  allocate(isli2(lat2,lon2))
  allocate(fact_tv(lat2,lon2,nsig))
  allocate(tropprs(lat2,lon2))
end subroutine init_
!--------------------------------------------------------
subroutine other_set_
  implicit none
  integer ier
  if(iamset_) return
  call load_vert_coord_
  call load_prsges_
  call load_geop_hgt_
  call load_guess_tsen_
  iamset_ = .true.
end subroutine other_set_
!--------------------------------------------------------
subroutine bkgcov_init_
  if(initialized_) return
  call other_set_()  ! a little out of place, but ...
  call rf_set(mype)
  initialized_ = .true.
end subroutine bkgcov_init_
!--------------------------------------------------------
subroutine bkgcov_final_
  use m_mpimod, only: mype
  implicit none
  integer ier
  call rf_unset
  initialized_ = .false.
  iamset_ = .false.
end subroutine bkgcov_final_
!--------------------------------------------------------
subroutine final_
  use m_mpimod, only: mype
  implicit none
  integer ier
  deallocate(tropprs)
  deallocate(fact_tv)
  deallocate(isli2)
  deallocate(geop_hgti)
  deallocate(geop_hgtl)
  deallocate(ges_qsat)
  deallocate(ges_prsl)
  deallocate(ges_prsi)
  deallocate(ges_tsen)
  call destroy_metguess_grids_(mype,ier)
end subroutine final_
!--------------------------------------------------------
subroutine load_vert_coord_
use m_set_eta, only: set_eta
implicit none
integer ks
real(r_kind) :: ptop,pint
call set_eta (nsig, ks, ptop, pint, ak5, bk5)
ak5=kPa_per_Pa*ak5
ak5=ak5(nsig:1:-1)
bk5=bk5(nsig:1:-1)
end subroutine load_vert_coord_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_geop_hgt_ --- Populate guess geopotential height
!
! !INTERFACE:
!
  subroutine load_geop_hgt_

! !USES:

    use constants, only: one,eps, rd, grav, half, t0c, fv
    use constants, only: cpf_a0, cpf_a1, cpf_a2, cpf_b0, cpf_b1, cpf_c0, cpf_c1, cpf_d, cpf_e
    use constants, only: psv_a, psv_b, psv_c, psv_d
    use constants, only: ef_alpha, ef_beta, ef_gamma
    use gridmod,   only: lat2, lon2, nsig, twodvar_regional

    implicit none

! !INPUT PARAMETERS:


! !DESCRIPTION: populate guess geopotential height
!
! !REVISION HISTORY:
!   2003-10-15  treadon
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-10-28  treadon - replace "tiny" with "tiny_r_kind"
!   2004-12-15  treadon - replace use of Paul van Delst's Geopotential
!                         function with simple integration of hydrostatic
!                         equation (done to be consistent with Lidia
!                         Cucurull's GPS work)
!   2005-05-24  pondeca - add regional surface analysis option
!   2010-08-27  cucurull - add option to compute and use compressibility factors in geopot heights
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20      date: 2003-10-15
!
!EOP
!-------------------------------------------------------------------------

    character(len=*),parameter::myname_=myname//'*load_geop_hgt_'
    real(r_kind),parameter:: thousand = 1000.0_r_kind

    integer(i_kind) i,j,k,jj,ier,istatus
    real(r_kind) h,dz,rdog
    real(r_kind),dimension(nsig+1):: height
    real(r_kind) cmpr, x_v, rl_hm, fact, pw, tmp_K, tmp_C, prs_sv, prs_a, ehn_fct, prs_v
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_q=>NULL()
    real(r_kind),dimension(:,:  ),pointer::ges_zz=>NULL()

    if (twodvar_regional) return

    rdog = rd/grav

    if (use_compress) then

!     Compute compressibility factor (Picard et al 2008) and geopotential heights at midpoint 
!     of each layer

       do jj=1,nfldsig
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'z' ,ges_zz ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'q' ,ges_q ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
          ier=ier+istatus
          if(ier/=0) exit
          do j=1,lon2
             do i=1,lat2
                k  = 1
                fact    = one + fv * ges_q(i,j,k)
                pw      = eps + ges_q(i,j,k)*( one - eps )
                tmp_K   = ges_tv(i,j,k) / fact
                tmp_C   = tmp_K - t0c
                prs_sv  = exp(psv_a*tmp_K**2 + psv_b*tmp_K + psv_c + psv_d/tmp_K)  ! Pvap sat, eq A1.1 (Pa)
                prs_a   = thousand * exp(half*(log(ges_prsi(i,j,k,jj)) + log(ges_prsl(i,j,k,jj))))     ! (Pa) 
                ehn_fct = ef_alpha + ef_beta*prs_a + ef_gamma*tmp_C**2 ! enhancement factor (eq. A1.2)
                prs_v   = ges_q(i,j,k) * prs_a / pw   ! vapor pressure (Pa)
                rl_hm   = prs_v / prs_sv    ! relative humidity
                x_v     = rl_hm * ehn_fct * prs_sv / prs_a     ! molar fraction of water vapor (eq. A1.3)
 
                ! Compressibility factor (eq A1.4 from Picard et al 2008)
                cmpr = one - (prs_a/tmp_K) * (cpf_a0 + cpf_a1*tmp_C + cpf_a2*tmp_C**2 &
                           + (cpf_b0 + cpf_b1*tmp_C)*x_v + (cpf_c0 + cpf_c1*tmp_C)*x_v**2 ) &
                           + (prs_a**2/tmp_K**2) * (cpf_d + cpf_e*x_v**2)

                h  = rdog * ges_tv(i,j,k)
                dz = h * cmpr * log(ges_prsi(i,j,k,jj)/ges_prsl(i,j,k,jj))
                height(k) = ges_zz(i,j) + dz   

                do k=2,nsig
                   fact    = one + fv * half * (ges_q(i,j,k-1)+ges_q(i,j,k))
                   pw      = eps + half * (ges_q(i,j,k-1)+ges_q(i,j,k)) * (one - eps)
                   tmp_K   = half * (ges_tv(i,j,k-1)+ges_tv(i,j,k)) / fact
                   tmp_C   = tmp_K - t0c
                   prs_sv  = exp(psv_a*tmp_K**2 + psv_b*tmp_K + psv_c + psv_d/tmp_K)  ! eq A1.1 (Pa)
                   prs_a   = thousand * exp(half*(log(ges_prsl(i,j,k-1,jj))+log(ges_prsl(i,j,k,jj))))   ! (Pa)
                   ehn_fct = ef_alpha + ef_beta*prs_a + ef_gamma*tmp_C**2 ! enhancement factor (eq. A1.2)
                   prs_v   = half*(ges_q(i,j,k-1)+ges_q(i,j,k) ) * prs_a / pw   ! (Pa)
                   rl_hm   = prs_v / prs_sv    ! relative humidity
                   x_v     = rl_hm * ehn_fct * prs_sv / prs_a     ! molar fraction of water vapor (eq. A1.3)
                   cmpr    = one - (prs_a/tmp_K) * ( cpf_a0 + cpf_a1*tmp_C + cpf_a2*tmp_C**2 &
                             + (cpf_b0 + cpf_b1*tmp_C)*x_v + (cpf_c0 + cpf_c1*tmp_C)*x_v**2 ) &
                             + (prs_a**2/tmp_K**2) * (cpf_d + cpf_e*x_v**2)
                   h       = rdog * half * (ges_tv(i,j,k-1)+ges_tv(i,j,k))
                   dz      = h * cmpr * log(ges_prsl(i,j,k-1,jj)/ges_prsl(i,j,k,jj))
                   height(k) = height(k-1) + dz
                end do

                do k=1,nsig
                   geop_hgtl(i,j,k,jj)=height(k) - ges_zz(i,j)
                end do
             enddo
          enddo
       enddo
       if(ier/=0) return

!      Compute compressibility factor (Picard et al 2008) and geopotential heights at interface
!      between layers

       do jj=1,nfldsig
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'z'  ,ges_zz ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'q'  ,ges_q ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
          ier=ier+istatus
          if(ier/=0) exit
          do j=1,lon2
             do i=1,lat2
                k=1
                height(k) = ges_zz(i,j)

                do k=2,nsig
                   fact    = one + fv * ges_q(i,j,k-1)
                   pw      = eps + ges_q(i,j,k-1)*(one - eps)
                   tmp_K   = ges_tv(i,j,k-1) / fact
                   tmp_C   = tmp_K - t0c
                   prs_sv  = exp(psv_a*tmp_K**2 + psv_b*tmp_K + psv_c + psv_d/tmp_K)  ! eq A1.1 (Pa)
                   prs_a   = thousand * exp(half*(log(ges_prsi(i,j,k-1,jj))+log(ges_prsi(i,j,k,jj)))) 
                   ehn_fct = ef_alpha + ef_beta*prs_a + ef_gamma*tmp_C**2 ! enhancement factor (eq. A1.2)
                   prs_v   = ges_q(i,j,k-1) * prs_a / pw   ! vapor pressure (Pa)
                   rl_hm   = prs_v / prs_sv    ! relative humidity
                   x_v     = rl_hm * ehn_fct * prs_sv / prs_a     ! molar fraction of water vapor (eq. A1.3)
                   cmpr    = one - (prs_a/tmp_K) * ( cpf_a0 + cpf_a1*tmp_C + cpf_a2*tmp_C**2 &
                            + (cpf_b0 + cpf_b1*tmp_C)*x_v + (cpf_c0 + cpf_c1*tmp_C)*x_v**2 ) &
                            + (prs_a**2/tmp_K**2) * (cpf_d + cpf_e*x_v**2)
                   h       = rdog * ges_tv(i,j,k-1)
                   dz      = h * cmpr * log(ges_prsi(i,j,k-1,jj)/ges_prsi(i,j,k,jj))
                   height(k) = height(k-1) + dz
                enddo

                k=nsig+1
                fact    = one + fv* ges_q(i,j,k-1)
                pw      = eps + ges_q(i,j,k-1)*(one - eps)
                tmp_K   = ges_tv(i,j,k-1) / fact
                tmp_C   = tmp_K - t0c
                prs_sv  = exp(psv_a*tmp_K**2 + psv_b*tmp_K + psv_c + psv_d/tmp_K)  ! eq A1.1 (Pa)
                prs_a   = thousand * exp(half*(log(ges_prsi(i,j,k-1,jj))+log(ges_prsl(i,j,k-1,jj))))     ! (Pa)
                ehn_fct = ef_alpha + ef_beta*prs_a + ef_gamma*tmp_C**2 ! enhancement factor (eq. A1.2)
                prs_v   = ges_q(i,j,k-1) * prs_a / pw  
                rl_hm   = prs_v / prs_sv    ! relative humidity
                x_v     = rl_hm * ehn_fct * prs_sv / prs_a     ! molar fraction of water vapor (eq. A1.3)
                cmpr    = one - (prs_a/tmp_K) * ( cpf_a0 + cpf_a1*tmp_C + cpf_a2*tmp_C**2 &
                          + (cpf_b0 + cpf_b1*tmp_C)*x_v + (cpf_c0 + cpf_c1*tmp_C)*x_v**2 ) &
                          + (prs_a**2/tmp_K**2) * (cpf_d + cpf_e*x_v**2)
                h       = rdog * ges_tv(i,j,k-1)
                dz      = h * cmpr * log(ges_prsi(i,j,k-1,jj)/ges_prsl(i,j,k-1,jj))
                height(k) = height(k-1) + dz
 
                do k=1,nsig+1
                   geop_hgti(i,j,k,jj)=height(k) - ges_zz(i,j)
                end do
             enddo
          enddo
       enddo
       if(ier/=0) return

    else

!      Compute geopotential height at midpoint of each layer
       do jj=1,nfldsig
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'z'  ,ges_zz  ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
          ier=ier+istatus
          if(ier/=0) exit
          do j=1,lon2
             do i=1,lat2
                k  = 1
                h  = rdog * ges_tv(i,j,k)
                dz = h * log(ges_prsi(i,j,k,jj)/ges_prsl(i,j,k,jj))
                height(k) = ges_zz(i,j) + dz
 
                do k=2,nsig
                   h  = rdog * half * (ges_tv(i,j,k-1)+ges_tv(i,j,k))
                   dz = h * log(ges_prsl(i,j,k-1,jj)/ges_prsl(i,j,k,jj))
                   height(k) = height(k-1) + dz
                end do

                do k=1,nsig
                   geop_hgtl(i,j,k,jj)=height(k) - ges_zz(i,j)
                end do
             end do
          end do
       end do
       if(ier/=0) return

!      Compute geopotential height at interface between layers
       do jj=1,nfldsig
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'z'  ,ges_zz  ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
          ier=ier+istatus
          if(ier/=0) call die(myname_,'not all fields available, ier=',ier)
          do j=1,lon2
             do i=1,lat2
                k=1
                height(k) = ges_zz(i,j)

                do k=2,nsig
                   h  = rdog * ges_tv(i,j,k-1)
                   dz = h * log(ges_prsi(i,j,k-1,jj)/ges_prsi(i,j,k,jj))
                   height(k) = height(k-1) + dz
                end do

                k=nsig+1
                h = rdog * ges_tv(i,j,k-1)
                dz = h * log(ges_prsi(i,j,k-1,jj)/ges_prsl(i,j,k-1,jj))
                height(k) = height(k-1) + dz

                do k=1,nsig+1
                   geop_hgti(i,j,k,jj)=height(k) - ges_zz(i,j)
                end do
             end do
          end do
       end do

    endif

    return
  end subroutine load_geop_hgt_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_prsges --- Populate guess pressure arrays
!
! !INTERFACE:
!
  subroutine load_prsges_

! !USES:

    use constants,only: zero,one,rd_over_cp,one_tenth,half,ten,rd,r1000
    use gridmod,  only: lat2,lon2,nsig,idvc5
    use gridmod,  only: ck5,tref5
    implicit none

! !DESCRIPTION: populate guess pressure arrays
!
! !REVISION HISTORY:
!   2003-10-15  kleist
!   2004-03-22  parrish, regional capability added
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-05-24  pondeca - add regional surface analysis option
!   2006-04-14  treadon - unify global calculations to use ak5,bk5
!   2006-04-17  treadon - add ges_psfcavg and ges_prslavg for regional
!   2006-07-31  kleist  - use ges_ps instead of ln(ps)
!   2007-05-08  kleist  - add fully generalized coordinate for pressure calculation
!   2011-07-07  todling - add cap for log(pressure) calculation
!   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS
!                         core
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist          org: w/nmc20     date: 2003-10-15
!
!EOP
!-------------------------------------------------------------------------

!   Declare local parameter
    character(len=*),parameter::myname_=myname//'*load_prsges'
    real(r_kind),parameter:: r1013=1013.0_r_kind

!   Declare local variables
    real(r_kind) kap1,kapr,trk
    real(r_kind),dimension(:,:)  ,pointer::ges_ps=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
    real(r_kind) pinc(lat2,lon2)
    integer(i_kind) i,j,k,ii,jj,itv,ips,kp
    logical ihaveprs(nfldsig)

    kap1=rd_over_cp+one
    kapr=one/rd_over_cp

    ihaveprs=.false.
    do jj=1,nfldsig
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps,ips)
       if(ips/=0) call die(myname_,': ps not available in guess, abort',ips)
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv,itv)
       if(idvc5==3) then
          if(itv/=0) call die(myname_,': tv must be present when idvc5=3, abort',itv)
       endif

!!!!!!!!!!!!  load delp to ges_prsi in read_fv3_netcdf_guess !!!!!!!!!!!!!!!!!

       do k=1,nsig+1
          do j=1,lon2
             do i=1,lat2
                   if (idvc5==1 .or. idvc5==2) then
                      ges_prsi(i,j,k,jj)=ak5(k)+(bk5(k)*ges_ps(i,j))
                   else if (idvc5==3) then
                      if (k==1) then
                         ges_prsi(i,j,k,jj)=ges_ps(i,j)
                      else if (k==nsig+1) then
                         ges_prsi(i,j,k,jj)=zero
                      else
                         trk=(half*(ges_tv(i,j,k-1)+ges_tv(i,j,k))/tref5(k))**kapr
                         ges_prsi(i,j,k,jj)=ak5(k)+(bk5(k)*ges_ps(i,j))+(ck5(k)*trk)
                         call die(myname_,'opt removed ',99)
                      end if
                   end if
                ges_prsi(i,j,k,jj)=max(ges_prsi(i,j,k,jj),zero)
             end do
          end do
       end do
       ihaveprs(jj)=.true.
    end do


!      load mid-layer pressure by using phillips vertical interpolation
       if (idsl5/=2) then
          do jj=1,nfldsig
             if(.not.ihaveprs(jj)) then
                call tell(myname,'3d pressure has not been calculated somehow',99)
                exit ! won't die ...
             endif
             do j=1,lon2
                do i=1,lat2
                   do k=1,nsig
                      ges_prsl(i,j,k,jj)=((ges_prsi(i,j,k,jj)**kap1-ges_prsi(i,j,k+1,jj)**kap1)/&
                           (kap1*(ges_prsi(i,j,k,jj)-ges_prsi(i,j,k+1,jj))))**kapr
                   end do
                end do
             end do
          end do

!      load mid-layer pressure by simple averaging
       else
          do jj=1,nfldsig
             if(.not.ihaveprs(jj)) then
                call tell(myname,'3d pressure has not been calculated somehow',99)
                exit ! won't die ...
             endif
             do j=1,lon2
                do i=1,lat2
                   do k=1,nsig
                      ges_prsl(i,j,k,jj)=(ges_prsi(i,j,k,jj)+ges_prsi(i,j,k+1,jj))*half
                   end do
                end do
             end do
          end do
       endif

    return
  end subroutine load_prsges_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_prsges --- Populate guess pressure arrays
!
! !INTERFACE:

  subroutine get_ref_gesprs_(prs)

! !USES: 

  use constants, only: zero,one_tenth,r100,r1000,ten
  use gridmod, only: idvc5
  use gridmod, only: nsig
  implicit none

! !INPUT PARAMETERS:

  real(r_kind), dimension(nsig+1), intent(out) :: prs

! !DESCRIPTION: get reference pressures
!
! !REVISION HISTORY:
!   2020-05-11  Todling  - bug fix for idvc5=1,2,3: ak5 are in cbar, thus 
!                          needed multiply by 10 to be in mb
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   unknonw       org: w/nmc20     date: 2003-10-15
!
!EOP
!-------------------------------------------------------------------------

  integer(i_kind) k

! get some reference-like pressure levels
  do k=1,nsig+1
        if (idvc5==1 .or. idvc5==2) then
           prs(k)=ten*ak5(k)+(bk5(k)*r1000)
        else if (idvc5==3) then
           if (k==1) then
              prs(k)=r1000
           else if (k==nsig+1) then
              prs(k)=zero
           else
              prs(k)=ten*ak5(k)+(bk5(k)*r1000)! +(ck5(k)*trk)
           end if
        end if
  enddo
  end subroutine get_ref_gesprs_

  subroutine load_guess_tsen_
  implicit none
  character(len=*), parameter :: myname_ = myname//'*get_guess_tsen_'
  real(r_kind),dimension(:,:,:),pointer::tv=>NULL()
  real(r_kind),dimension(:,:,:),pointer::q =>NULL()
  integer jj,ier,istatus
  do jj=1,nfldsig
     istatus=0
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv',tv,ier); istatus=ier+istatus
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'q' , q,ier); istatus=ier+istatus
     if (istatus/=0) cycle ! call die(myname_,'cannot retrieve pointers',istatus)
     ges_tsen(:,:,:,jj) = tv/(one+fv*q)
  enddo
  end subroutine load_guess_tsen_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: create_metguess_grids --- initialize meterological guess
!
! !INTERFACE:
!
  subroutine create_metguess_grids_(mype,istatus)

! !USES:
  use gridmod, only: lat2,lon2,nsig
  implicit none

! !INPUT PARAMETERS:

  integer(i_kind), intent(in)  :: mype

! !OUTPUT PARAMETERS:

  integer(i_kind), intent(out) :: istatus

! !DESCRIPTION: initialize meteorological background fields beyond 
!               the standard ones - wired-in this module.
!
! !REVISION HISTORY:
!   2011-04-29  todling
!   2013-10-30  todling - update interface
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; Linux Cluster
!
! !AUTHOR: 
!   todling         org: w/nmc20     date: 2011-04-29
!
!EOP
!-------------------------------------------------------------------------
   character(len=*),parameter::myname_=myname//'*create_metguess_grids_'
   integer(i_kind) :: nmguess                   ! number of meteorol. fields (namelist)
   character(len=max_varname_length),allocatable:: mguess(:)   ! names of meterol. fields

   istatus=0
  
!  When proper connection to ESMF is complete,
!  the following will not be needed here
!  ------------------------------------------
   call gsi_metguess_get('dim',nmguess,istatus)
   if(istatus/=0) then
      if(mype==0) write(6,*) myname_, ': trouble getting number of met-guess fields'
      return
   endif
   if(nmguess==0) return
   if (nmguess>0) then
       allocate (mguess(nmguess))
       call gsi_metguess_get('gsinames',mguess,istatus)
       if(istatus/=0) then
          if(mype==0) write(6,*) myname_, ': trouble getting name of met-guess fields'
          return
       endif

!      Allocate memory for guess fields
!      --------------------------------
       call gsi_metguess_create_grids(lat2,lon2,nsig,nfldsig,istatus)
       if(istatus/=0) then
          if(mype==0) write(6,*) myname_, ': trouble allocating mem for met-guess'
          return
       endif
   endif

  end subroutine create_metguess_grids_

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_metguess_grids --- destroy meterological background
!
! !INTERFACE:
!
  subroutine destroy_metguess_grids_(mype,istatus)
! !USES:
  implicit none
! !INPUT PARAMETERS:
  integer(i_kind),intent(in)::mype
! !OUTPUT PARAMETERS:
  integer(i_kind),intent(out)::istatus
! !DESCRIPTION: destroy meterological background
!
! !REVISION HISTORY:
!   2011-04-29  todling
!   2013-10-30  todling - update interface
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; Linux Cluster
!
! !AUTHOR: 
!   todling         org: w/nmc20     date: 2011-04-29
!
!EOP
  character(len=*),parameter::myname_=myname//'destroy_metguess_grids'
  istatus=0
  call gsi_metguess_destroy_grids(istatus)
       if(istatus/=0) then
          if(mype==0) write(6,*) myname_, ': trouble deallocating mem for met-guess'
          return
       endif
  end subroutine destroy_metguess_grids_

  subroutine guess_basics0_
  real(r_kind),dimension(:,:,:),pointer::tv=>NULL()
  real(r_kind),dimension(:,:,:),pointer::u =>NULL()
  real(r_kind),dimension(:,:,:),pointer::v =>NULL()
  real(r_kind),dimension(:,:,:),pointer::q =>NULL()
  real(r_kind),dimension(:,:  ),pointer::ps=>NULL()
  integer jj,ier
  do jj=1,nfldsig
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'u',u,ier)
     if (ier==0) then
        u = 20.
     endif
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'v',v,ier)
     if (ier==0) then
        v = 20.
     endif
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv',tv,ier)
     if (ier==0) then
        tv = 300.
     endif
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'q' , q,ier)
     if (ier==0) then
        q = 10-6
     endif
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps',ps,ier)
     if (ier==0) then
        ps = 100000. * kPa_per_Pa
     endif
  enddo
  end subroutine guess_basics0_
!--------------------------------------------------------
  subroutine guess_basics2_(vname,var)
  character(len=*),intent(in) :: vname
  real(r_kind),dimension(:,:),pointer::var
  integer jj,ier
  do jj=1,nfldsig
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),trim(vname),var,ier)
     if (ier/=0) then
       call die(myname,'pointer to '//trim(vname)//" not found",ier)
     endif
  enddo
  end subroutine guess_basics2_
!--------------------------------------------------------
  subroutine guess_basics3_(vname,var)
  character(len=*),intent(in) :: vname
  real(r_kind),dimension(:,:,:),pointer::var
  integer jj,ier
  do jj=1,nfldsig
     call gsi_bundlegetpointer(gsi_metguess_bundle(jj),trim(vname),var,ier)
     if (ier/=0) then
       call die(myname,'pointer to '//trim(vname)//" not found",ier)
     endif
  enddo
  end subroutine guess_basics3_
!--------------------------------------------------------
end module guess_grids
