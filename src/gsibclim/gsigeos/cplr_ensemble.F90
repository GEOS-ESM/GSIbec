module cplr_ensemble

    use stub_ensmod, only: stubEnsemble => ensemble

    implicit none
    private
    public :: ensemble
    public :: ensemble_typemold

    type, extends(stubEnsemble) :: ensemble
      private
      contains
      procedure :: get_user_ens => get_geos_ens
      procedure :: get_user_Nens => get_geos_Nens
      procedure :: put_user_ens => put_geos_ens
      procedure :: non_gaussian_ens_grid
      procedure, nopass:: mytype => typename
    end type ensemble

    character(len=*),parameter:: myname="geos_ensmod"

    type(ensemble),target:: mold_

contains

function ensemble_typemold()
  implicit none
  type(ensemble),pointer:: ensemble_typemold
  ensemble_typemold => mold_
end function ensemble_typemold

function typename()
  implicit none
  character(len=:),allocatable:: typename
  typename='['//myname//'::ensemble]'
end function typename

subroutine get_geos_ens(this,grd,member,ntindex,tau,atm_bundle,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_user_ens_    pretend atmos bkg is the ensemble
!   prgmmr: todling          org: np22                date: 2011-09-13
!
! abstract: Read in GEOS ensemble members in to GSI ensemble.
!
! program history log: 
!   2011-09-13  todling  - created for testing purposes
!   2011-10-15  el akkraoui - add vor/div calculation call
!   2011-11-13  todling  - add vor/div calculation for dual-res case
!   2012-01-04  todling  - q on input is indeed spec-hum (later converted to rh)
!   2014-10-30  todling  - generalized interface (pass bundle)
!                        - add qi/ql/sst
!   2015-10-19  todling  - revisit var handling; now controlled by supporting code
!
!   input argument list:
!     grd      - structure variable containing information about grid
!     member   - index for ensemble member
!
!   output argument list:
!     atm_bundle   - bundle w/ ensemble fields
!     iret      - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
use m_mpimod, only: mype
use m_kinds, only: i_kind,r_kind
use general_sub2grid_mod, only: sub2grid_info
use gsi_bundlemod, only: gsi_grid
use gsi_bundlemod, only: gsi_gridcreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_metguess_mod, only: gsi_metguess_bundle
use gsi_4dvar, only: min_offset,l4densvar,ibdate,ens_fmnlevs
use geos_StateIO, only: State_get
use jfunc, only: iadate
use hybrid_ensemble_parameters, only: uv_hyb_ens
use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
! following needed for vor/div calculation ... need to be careful about resolution
use constants, only: zero,rearth
use gridmod, only: nlat,nlon
use compact_diffs, only: cdiff_created
use compact_diffs, only: cdiff_initialized
use compact_diffs, only: create_cdiff_coefs
use compact_diffs, only: inisph
#ifdef USE_ALL_ORIGINAL
use timermod, only: timer_ini,timer_fnl
#endif /* USE_ALL_ORIGINAL */
implicit none
!  Declare passed variables
   class(ensemble)                      , intent(inout) :: this
   type(sub2grid_info)                   ,intent(in   ) :: grd
   integer(i_kind)                       ,intent(in   ) :: member
   integer(i_kind)                       ,intent(in   ) :: ntindex
   integer(i_kind)                       ,intent(in   ) :: tau
   integer(i_kind)                       ,intent(  out) :: iret
   type(gsi_bundle)                      ,intent(inout) :: atm_bundle                      

!  Declare internal variables
   character(len=*),parameter::myname='geos_get_ens_'
   character(len=40) evar
   integer(i_kind) nymd,nhms,istatus,ii,ier
   integer(i_kind) ida(8),jda(8)
   logical,save :: first=.true.
   real(r_kind) fha(5)
   real(r_kind),pointer,dimension(:,:  ) :: iptr2d
   real(r_kind),pointer,dimension(:,:,:) :: iptr3d
   real(r_kind),pointer,dimension(:,:  ) :: optr2d
   real(r_kind),pointer,dimension(:,:,:) :: optr3d
   type(gsi_grid) grid
   type(gsi_bundle) flds

!  associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
!  end associate

!  Only works for non-dual resolution ...
   if(grd%nlat/=nlat.or.grd%nlon/=nlon) then
      if (first) then
        if(mype==0) then
           write(6,*) myname, ': grd%(nlat,nlon) = ', grd%nlat,grd%nlon
           write(6,*) myname, ': ges%(nlat,nlon) = ', nlat,nlon
           write(6,*) myname, ': dual resolution hybrid analysis in use'
        endif
        first=.false.
      endif
   endif

!  Consistency check
   if(.not. uv_hyb_ens) then
      if (mype==0) then
         write(6,*)trim(myname),': must have uv as part of ensemble members'
         write(6,*)trim(myname),': set uv_hyb_ens to true'
      endif
      call stop2(999)
   endif

!  Create temporary bundle to hold input field
   call gsi_gridcreate(grid,grd%lat2,grd%lon2,grd%nsig)
   call gsi_bundlecreate(flds,grid,'ensemble member',istatus, &
                         names2d=cvars2d,names3d=cvars3d)
   if(istatus/=0) then
      write(6,*)trim(myname),': trouble creating temporary bundle'
      call stop2(999)
   endif

!  Read in a single ensemble member
   if(l4densvar) then
      ida(1:3)=ibdate(1:3)
      ida(5:6)=ibdate(4:5)
      jda(:)=0
      fha(:)=0.0
      fha(3)=ens_fmnlevs(ntindex)-180.0_r_kind ! NCEP counts time from previous syn analysis (180min=3hr)
      !call w3movdat(fha,ida,jda)
      nymd=jda(1)*10000+jda(2)*100+jda(3)
      nhms=jda(5)*10000+jda(6)*100
   else
      nymd = iadate(1)*10000 + iadate(2)*100 + iadate(3)
      nhms = iadate(4)*10000 + iadate(5)*100
   endif
#ifdef USE_ALL_ORIGINAL
   call timer_ini('GetEns')
#endif /* USE_ALL_ORIGINAL */
   call state_get(flds,grd,nymd,nhms,member,tau=tau)
#ifdef USE_ALL_ORIGINAL
   call timer_fnl('GetEns')
#endif /* USE_ALL_ORIGINAL */

!  take care of rank-2 fields
   do ii=1,nc2d
      evar=trim(cvars2d(ii)) ! in general output name same as input (but not always!)
      call gsi_bundlegetpointer (flds      ,evar,iptr2d,istatus)
      call gsi_bundlegetpointer (atm_bundle,evar,optr2d,ier)
      if(istatus==0 .and. ier==0) then
         optr2d=iptr2d
      else
         if (ier/=0.and.mype==0) then
            write(6,*) myname,': field ',evar,' not in ens CV, skipping'
         endif
         if (istatus/=0.and.mype==0) then
            write(6,*) myname,': field ',evar,' not in member file, skipping'
         endif
      endif
   enddo

!  take care of rank-3 fields
   do ii=1,nc3d
      evar=trim(cvars3d(ii)) ! in general output name same as input (but not always!)
      call gsi_bundlegetpointer (atm_bundle,evar,optr3d,ier)
      call gsi_bundlegetpointer (flds,evar,iptr3d,istatus)
      if(istatus==0 .and. ier==0) then
         optr3d=iptr3d
      else
         if (ier/=0.and.mype==0) then
            write(6,*) myname,': field ',evar,' not in ens CV, skipping'
         endif
         if (istatus/=0.and.mype==0) then
            write(6,*) myname,': field ',evar,' not in member file, skipping'
         endif
      endif
   enddo
   iret=0 ! ignore prior error codes

!  Clean up
   call gsi_bundledestroy(flds)

end subroutine get_geos_ens

subroutine get_geos_Nens(this,grd,members,ntindex,tau,atm_bundle,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_user_Nens_    pretend atmos bkg is the ensemble
!   prgmmr: todling          org: np22                date: 2011-09-13
!
! abstract: Read in GEOS ensemble members in to GSI ensemble.
!
! program history log: 
!   2011-09-13  todling  - created for testing purposes
!   2011-10-15  el akkraoui - add vor/div calculation call
!   2011-11-13  todling  - add vor/div calculation for dual-res case
!   2012-01-04  todling  - q on input is indeed spec-hum (later converted to rh)
!   2014-10-30  todling  - generalized interface (pass bundle)
!                        - add qi/ql/sst
!   2015-10-19  todling  - revisit var handling; now controlled by supporting code
!   2017-06-23  todling  - add as generalization of original single-bundle version
!
!   input argument list:
!     grd      - structure variable containing information about grid
!     members  - number of members to read at once
!
!   output argument list:
!     atm_bundle  - bundle w/ ensemble fields
!     iret        - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
use m_mpimod, only: mype
use m_kinds, only: i_kind,r_kind
use general_sub2grid_mod, only: sub2grid_info
use gsi_bundlemod, only: gsi_grid
use gsi_bundlemod, only: gsi_gridcreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_metguess_mod, only: gsi_metguess_bundle
use gsi_4dvar, only: min_offset,l4densvar,ibdate,ens_fmnlevs
use geos_StateIO, only: State_get
use jfunc, only: iadate
use hybrid_ensemble_parameters, only: uv_hyb_ens
use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
! following needed for vor/div calculation ... need to be careful about resolution
use constants, only: zero,rearth
use gridmod, only: nlat,nlon
use compact_diffs, only: cdiff_created
use compact_diffs, only: cdiff_initialized
use compact_diffs, only: create_cdiff_coefs
use compact_diffs, only: inisph
#ifdef USE_ALL_ORIGINAL
use timermod, only: timer_ini,timer_fnl
#endif /* USE_ALL_ORIGINAL */
implicit none
!  Declare passed variables
   class(ensemble)                      , intent(inout) :: this
   type(sub2grid_info)                   ,intent(in   ) :: grd
   integer(i_kind)                       ,intent(in   ) :: members
   integer(i_kind)                       ,intent(in   ) :: ntindex
   integer(i_kind)                       ,intent(in   ) :: tau
   integer(i_kind)                       ,intent(  out) :: iret
   type(gsi_bundle)                      ,intent(inout) :: atm_bundle(:)                      

!  Declare internal variables
   character(len=*),parameter::myname='geos_get_Nens_'
   character(len=40) evar
   integer(i_kind) nymd,nhms,istatus,ii,ier
   integer(i_kind) ida(8),jda(8)
   integer(i_kind) mm
   logical,save :: first=.true.
   real(r_kind) fha(5)
   real(r_kind),pointer,dimension(:,:  ) :: iptr2d
   real(r_kind),pointer,dimension(:,:,:) :: iptr3d
   real(r_kind),pointer,dimension(:,:  ) :: optr2d
   real(r_kind),pointer,dimension(:,:,:) :: optr3d
   type(gsi_grid) grid
   type(gsi_bundle),allocatable:: flds(:)

!  associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
!  end associate

!  Only works for non-dual resolution ...
   if(grd%nlat/=nlat.or.grd%nlon/=nlon) then
      if (first) then
        if(mype==0) then
           write(6,*) myname, ': grd%(nlat,nlon) = ', grd%nlat,grd%nlon
           write(6,*) myname, ': ges%(nlat,nlon) = ', nlat,nlon
           write(6,*) myname, ': dual resolution hybrid analysis in use'
        endif
        first=.false.
      endif
   endif

!  Consistency check
   if(.not. uv_hyb_ens) then
      if (mype==0) then
         write(6,*)trim(myname),': must have uv as part of ensemble members'
         write(6,*)trim(myname),': set uv_hyb_ens to true'
      endif
      call stop2(999)
   endif

!  Create temporary bundle to hold input field
   call gsi_gridcreate(grid,grd%lat2,grd%lon2,grd%nsig)
   allocate(flds(members))
   do mm=1,members
      call gsi_bundlecreate(flds(mm),grid,'ensemble member',istatus, &
                            names2d=cvars2d,names3d=cvars3d)
      if(istatus/=0) then
         write(6,*)trim(myname),': trouble creating temporary bundle for member ', mm
         call stop2(999)
      endif
   enddo

!  Read in ensemble members
   if(l4densvar) then
      ida(1:3)=ibdate(1:3)
      ida(5:6)=ibdate(4:5)
      jda(:)=0
      fha(:)=0.0
      fha(3)=ens_fmnlevs(ntindex)-180.0_r_kind ! NCEP counts time from previous syn analysis (180min=3hr)
      !call w3movdat(fha,ida,jda)
      nymd=jda(1)*10000+jda(2)*100+jda(3)
      nhms=jda(5)*10000+jda(6)*100
   else
      nymd = iadate(1)*10000 + iadate(2)*100 + iadate(3)
      nhms = iadate(4)*10000 + iadate(5)*100
   endif
#ifdef USE_ALL_ORIGINAL
   call timer_ini('GetEns')
#endif /* USE_ALL_ORIGINAL */
   call state_get(flds,grd,nymd,nhms,tau=tau)
#ifdef USE_ALL_ORIGINAL
   call timer_fnl('GetEns')
#endif /* USE_ALL_ORIGINAL */

!  take care of rank-2 fields
   do mm=1,members
      do ii=1,nc2d
         evar=trim(cvars2d(ii)) ! in general output name same as input (but not always!)
         call gsi_bundlegetpointer (flds(mm)      ,evar,iptr2d,istatus)
         call gsi_bundlegetpointer (atm_bundle(mm),evar,optr2d,ier)
         if(istatus==0 .and. ier==0) then
            optr2d=iptr2d
         else
            if (ier/=0.and.mype==0) then
               write(6,*) myname,': field ',evar,' not in ens CV, skipping'
            endif
            if (istatus/=0.and.mype==0) then
               write(6,*) myname,': field ',evar,' not in member file, skipping'
            endif
         endif
      enddo
   enddo

!  take care of rank-3 fields
   do mm=1,members
      do ii=1,nc3d
         evar=trim(cvars3d(ii)) ! in general output name same as input (but not always!)
         call gsi_bundlegetpointer (atm_bundle(mm),evar,optr3d,ier)
         call gsi_bundlegetpointer (flds(mm)      ,evar,iptr3d,istatus)
         if(istatus==0 .and. ier==0) then
            optr3d=iptr3d
         else
            if (ier/=0.and.mype==0) then
               write(6,*) myname,': field ',evar,' not in ens CV, skipping'
            endif
            if (istatus/=0.and.mype==0) then
               write(6,*) myname,': field ',evar,' not in member file, skipping'
            endif
         endif
      enddo
   enddo
   iret=0 ! ignore prior error codes

!  Clean up
   do mm=members,1,-1
      call gsi_bundledestroy(flds(mm))
   enddo
   deallocate(flds)

end subroutine get_geos_Nens

subroutine put_geos_ens(this,grd,member,ntindex,pert,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    put_gsi_ens_    write out an internally gen ens to file
!   prgmmr: todling          org: np22                date: 2011-12-01
!
! abstract: Write out GSI ensemble to file.
!
! program history log: 
!   2011-10-01  todling  - created for testing purposes
!   2012-05-15  el akkraoui/todling  - overload for r4/r8; pass time index
!   2015-10-19  todling  - add qi/ql/qr/qs and should work for all cases of CV/SV
!
!   input argument list:
!     grd      - structure variable containing information about grid
!     pert     - ensemble perturbations
!     member   - index for ensemble member
!
!   output argument list:
!     iret      - return code, 0 for successful read.
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
use m_mpimod, only: mype
use m_kinds, only: i_kind,r_kind,r_single
use constants, only: zero
use general_sub2grid_mod, only: sub2grid_info
use gsi_bundlemod, only: gsi_grid
use gsi_bundlemod, only: gsi_gridcreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: gsi_bundlegetpointer
use geos_StateIO, only: state_put
use jfunc, only: iadate
use gridmod, only: nlat,nlon
implicit none
!  Declare passed variables
   class(ensemble),    intent(inout):: this
   type(sub2grid_info),intent(in)   :: grd
   integer(i_kind),    intent(in)   :: ntindex! time index
   integer(i_kind),    intent(in)   :: member ! member index
   type(gsi_bundle),   intent(inout):: pert
   integer(i_kind),    intent(out)  :: iret

!  Declare internal variables
   character(len=*),parameter::myname='geos_put_ens_'
   character(len=40) evar
   logical iamsingle
   integer(i_kind) nymd,nhms,isf,ivp,ipnt,ier,istatus
   integer(i_kind) im,jm,km
   real(r_kind),pointer,dimension(:,:  ) :: pptr2d,optr2d
   real(r_kind),pointer,dimension(:,:  ) :: ps
   real(r_kind),pointer,dimension(:,:,:) :: pptr3d,optr3d
   real(r_kind),pointer,dimension(:,:,:) :: tv,q
   real(r_kind),allocatable,dimension(:,:,:) :: p3d,rh
   type(gsi_grid) grid
   type(gsi_bundle) flds

!  Declare fields in file (should come from resource file)
   integer(i_kind),parameter:: no2d=2
   integer(i_kind),dimension(no2d)::iptr2d
   character(len=4), parameter :: ovars2d(no2d) = (/ 'ps  ', 'z   '/)
   integer(i_kind),parameter:: no3d=10
   integer(i_kind),dimension(no3d)::iptr3d
   character(len=5), parameter :: ovars3d(no3d) = (/ 'u    ', 'v    ',&
                                                     'tv   ', 'q    ',&
                                                     'cw   ', 'oz   ',&
                                                     'qi   ', 'ql   ',&
                                                     'qr   ', 'qs   ' /)

!  associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
!  end associate

   iret=0
   im=grd%lat2
   jm=grd%lon2
   km=grd%nsig

!  Only works for non-dual resolution ...
   if(grd%nlat/=nlat.or.grd%nlon/=nlon) then
      if(mype==0) then
         write(6,*) myname, ': grd%(nlat,nlon) = ', grd%nlat,grd%nlon
         write(6,*) myname, ': ges%(nlat,nlon) = ', nlat,nlon
         write(6,*) myname, ': dual resolution hybrid analysis in use'
      endif
      call stop2(999) ! this is only ready for single resolution
                      ! calc of u/v below prevents dual-resolution
   endif

!  Create temporary bundle to hold input field
   call gsi_gridcreate(grid,grd%lat2,grd%lon2,grd%nsig)
   call gsi_bundlecreate(flds,grid,'ensemble member',istatus, &
                         names2d=ovars2d,names3d=ovars3d)
   if(istatus/=0) then
      if(mype==0) write(6,*)trim(myname),': trouble creating temporary bundle'
      call stop2(999)
   endif
   iamsingle = pert%allkinds==r_single

!  PS
   call gsi_bundlegetpointer (pert,'ps',ipnt,istatus)
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,'ps',optr2d,istatus)
      if(iamsingle) then
        optr2d=pert%r2(ipnt)%qr4
      else
        optr2d=pert%r2(ipnt)%qr8
      endif
   else
      if(mype==0) write(6,*) myname,': ps ens member not in incoming pert'
      call stop2(999)
   endif

!  Z
!  call gsi_bundlegetpointer (pert,'z',pptr2d,istatus)
!  if(istatus==0) then
!     call gsi_bundlegetpointer (flds,'z',optr2d,istatus)
!     optr2d=pptr2d
!  else
!     if(mype==0) write(6,*) myname,': z ens member not in incoming pert'
!     call stop2(999)
!  endif
!  SF/VP->U/V
   istatus=0
   call gsi_bundlegetpointer (pert,'sf',isf,ier);istatus=ier+istatus
   call gsi_bundlegetpointer (pert,'vp',ivp,ier);istatus=ier+istatus
   if(istatus/=0) then
      istatus=0
      call gsi_bundlegetpointer (pert,'u',isf,ier);istatus=ier+istatus
      call gsi_bundlegetpointer (pert,'v',ivp,ier);istatus=ier+istatus
   endif
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,'u',optr3d,ier)
      call gsi_bundlegetpointer (flds,'v',pptr3d,ier)
      if(iamsingle) then
        optr3d=pert%r3(isf)%qr4 ! already u
        pptr3d=pert%r3(ivp)%qr4 ! already v
      else
        optr3d=pert%r3(isf)%qr8 ! already u
        pptr3d=pert%r3(ivp)%qr8 ! already v
      endif
   else
      if(mype==0) write(6,*) myname,': st/vp ens member not in incoming pert'
      call stop2(999)
   endif

!  T
   call gsi_bundlegetpointer (pert,'t',ipnt,istatus)
   if(istatus/=0) then
      call gsi_bundlegetpointer (pert,'tv',ipnt,istatus)
   endif
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,'tv',optr3d,istatus)
      if(iamsingle) then
        optr3d=pert%r3(ipnt)%qr4
      else
        optr3d=pert%r3(ipnt)%qr8
      endif
   else
      if(mype==0) write(6,*) myname,': tv ens member not in incoming pert'
      call stop2(999)
   endif

!  RH->Q
   call gsi_bundlegetpointer (pert,'q',ipnt,istatus)
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,'ps',ps,istatus)
      call gsi_bundlegetpointer (flds,'tv',tv,istatus)
      call gsi_bundlegetpointer (flds,'q' ,q ,istatus)
      allocate(rh(size(q,1),size(q,2),size(q,3)))
      if(iamsingle) then
        rh=pert%r3(ipnt)%qr4
      else
        rh=pert%r3(ipnt)%qr8
      endif
      allocate(p3d(im,jm,km))
      p3d=zero ! for now
      call normal_rh_to_q(rh,tv,p3d,q)
      deallocate(p3d)
      if(iamsingle) then
        pert%r3(ipnt)%qr4=rh
      else
        pert%r3(ipnt)%qr8=rh
      endif
      deallocate(rh)
   else
      if(mype==0) write(6,*) myname,': q ens member not in incoming pert'
      call stop2(999)
   endif

!  OZ
   call gsi_bundlegetpointer (pert,'oz',ipnt,istatus)
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,'oz',optr3d,istatus)
      if(iamsingle) then
        optr3d=pert%r3(ipnt)%qr4
      else
        optr3d=pert%r3(ipnt)%qr8
      endif
   else
      if(mype==0) write(6,*) myname,': oz ens member not in incoming pert'
      call stop2(999)
   endif

!  CW
   call gsi_bundlegetpointer (pert,'cw',ipnt,istatus)
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,'cw',optr3d,istatus)
      if(iamsingle) then
        optr3d=pert%r3(ipnt)%qr4
      else
        optr3d=pert%r3(ipnt)%qr8
      endif
   else
      if(mype==0) write(6,*) myname,': cw ens member not in incoming pert'
!     call stop2(999)
   endif

!  QI
   evar='qi'
   call gsi_bundlegetpointer (pert,evar,ipnt,istatus)
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,evar,optr3d,istatus)
      if(iamsingle) then
        optr3d=pert%r3(ipnt)%qr4
      else
        optr3d=pert%r3(ipnt)%qr8
      endif
   else
      if(mype==0) write(6,*) myname,': ',evar,' ens member not in incoming pert'
!     call stop2(999)
   endif

!  QL
   evar='ql'
   call gsi_bundlegetpointer (pert,evar,ipnt,istatus)
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,evar,optr3d,istatus)
      if(iamsingle) then
        optr3d=pert%r3(ipnt)%qr4
      else
        optr3d=pert%r3(ipnt)%qr8
      endif
   else
      if(mype==0) write(6,*) myname,': ',evar,' ens member not in incoming pert'
!     call stop2(999)
   endif

!  QR
   evar='qr'
   call gsi_bundlegetpointer (pert,evar,ipnt,istatus)
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,evar,optr3d,istatus)
      if(iamsingle) then
        optr3d=pert%r3(ipnt)%qr4
      else
        optr3d=pert%r3(ipnt)%qr8
      endif
   else
      if(mype==0) write(6,*) myname,': ',evar,' ens member not in incoming pert'
!     call stop2(999)
   endif

!  QS
   evar='qs'
   call gsi_bundlegetpointer (pert,evar,ipnt,istatus)
   if(istatus==0) then
      call gsi_bundlegetpointer (flds,evar,optr3d,istatus)
      if(iamsingle) then
        optr3d=pert%r3(ipnt)%qr4
      else
        optr3d=pert%r3(ipnt)%qr8
      endif
   else
      if(mype==0) write(6,*) myname,': ',evar,' ens member not in incoming pert'
!     call stop2(999)
   endif

!  if(mype==0) Write out this ensemble member (as full state for now)
!  TODO: used NT to define time within window (RTodling)
   nymd = iadate(1)*10000 + iadate(2)*100 + iadate(3)
   nhms = iadate(4)*10000
   call state_put(flds,grd,nymd,nhms,member)

!  Clean up
   call gsi_bundledestroy(flds)

end subroutine put_geos_ens

subroutine non_gaussian_ens_grid (this,elats,elons)
use m_mpimod, only: mype
use m_kinds, only: i_kind,r_kind
use constants, only: one,half
use hybrid_ensemble_parameters, only: nlat_ens,nlon_ens
! Define a regular grid ... (not guassian as the routine name implies)
implicit none
class(ensemble), intent(inout) :: this
real(r_kind),intent(out) :: elats(:),elons(:)
!real(r_kind),intent(out) :: elats(nlat_ens),elons(nlon_ens)
character(len=*),parameter :: myname_=myname//'non_gaussian_ens_grid'
!
real(r_kind) dlon,dlat,pi,pih
integer(i_kind) i,j
!
!associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
!end associate
!
if (size(elats)/=nlat_ens.or.size(elons)/=nlon_ens) then
   if(mype==0) then
      write(6,*) myname_,': inconsistent ens nlat/nlon'
      write(6,*) myname_,':  actual(vec) ', size(elats),size(elons)
      write(6,*) myname_,': defined(vec) ', nlat_ens,nlon_ens
   endif
   call stop2(999)
endif
!
pi=4.0_r_kind*atan(1.0_r_kind)
dlon=(pi+pi)/float(nlon_ens)
dlat=pi/float(nlat_ens-1)
do i=1,nlon_ens                   ! from 0 to 2pi
   elons (i)=(i-one)*dlon
end do
pih =half*pi
do j=1,nlat_ens                   ! from -pi/2 to +pi/2
   elats(j)=(j-one)*dlat - pih
end do

end subroutine non_gaussian_ens_grid

end module cplr_ensemble
