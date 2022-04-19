module guess_grids
use m_kinds, only: i_kind, r_kind
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
implicit none
private
!
public :: ges_prsi
public :: ges_prsl
public :: ges_tsen
public :: isli2
public :: fact_tv

public :: guess_grids_init
public :: guess_grids_final
public :: guess_grids_get_ref_gesprs
!public :: guess_basics

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

! For now turned into wired-in parameters
integer(i_kind),parameter :: nfldsig =  1
integer(i_kind),parameter :: ntguessig = 1

real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsl
real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsi
real(r_kind),allocatable,dimension(:,:,:,:):: ges_tsen
real(r_kind),allocatable,dimension(:,:,:):: fact_tv
integer(i_kind),allocatable,dimension(:,:):: isli2

interface guess_grids_init; module procedure init_; end interface
interface guess_grids_final; module procedure final_; end interface
interface guess_grids_get_ref_gesprs; module procedure get_ref_gesprs_; end interface
!interface guess_basics; module procedure guess_basics_; end interface

character(len=*), parameter :: myname="guess_grids"
contains
subroutine init_
  use m_mpimod, only: mype
  implicit none
  integer ier
  allocate(ges_tsen(lat2,lon2,nsig,nfldsig))
  allocate(ges_prsi(lat2,lon2,nsig+1,nfldsig))
  allocate(ges_prsl(lat2,lon2,nsig+1,nfldsig))
  allocate(isli2(lat2,lon2))
  allocate(fact_tv(lat2,lon2,nsig))
  call create_metguess_grids_(mype,ier)
  call guess_basics_
  call load_vert_coord_
  call load_prsges_
  call load_guess_tsen_
end subroutine init_
subroutine final_
  use m_mpimod, only: mype
  implicit none
  integer ier
  call destroy_metguess_grids_(mype,ier)
  deallocate(fact_tv)
  deallocate(isli2)
  deallocate(ges_prsl)
  deallocate(ges_prsi)
  deallocate(ges_tsen)
end subroutine final_

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
!_RTreal(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
    real(r_kind) pinc(lat2,lon2)
    integer(i_kind) i,j,k,ii,jj,itv,ips,kp
    logical ihaveprs(nfldsig)

    kap1=rd_over_cp+one
    kapr=one/rd_over_cp

    ihaveprs=.false.
    do jj=1,nfldsig
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps,ips)
       if(ips/=0) call die(myname_,': ps not available in guess, abort',ips)
!      call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv,itv)
!      if(idvc5==3) then
!         if(itv/=0) call die(myname_,': tv must be present when idvc5=3, abort',itv)
!      endif

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
!                     else
!                        trk=(half*(ges_tv(i,j,k-1)+ges_tv(i,j,k))/tref5(k))**kapr
!                        ges_prsi(i,j,k,jj)=ak5(k)+(bk5(k)*ges_ps(i,j))+(ck5(k)*trk)
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

  subroutine guess_basics_
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
  end subroutine guess_basics_
end module guess_grids
