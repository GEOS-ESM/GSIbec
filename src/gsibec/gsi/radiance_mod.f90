module radiance_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    radiance_mod
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This module contains variables and routines related
!            to cloud and aerosol usages for radiance assimilation
!
! program history log:
!   2015-07-20 Yanqiu Zhu
!   2016-10-27 Yanqiu - add ATMS
!
! subroutines included:
!   sub radiance_mode_init           -  guess init
!   radiance_mode_destroy
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

! !USES:

  use m_kinds, only: r_kind,i_kind
  use constants, only: zero,half
  use m_mpimod, only: mype
  implicit none
  save

! set subroutines to public
  public :: radiance_mode_init
  public :: radiance_mode_destroy

  public :: icloud_fwd,icloud_cv,iallsky,cw_cv,ql_cv
  public :: n_actual_clouds,n_clouds_fwd,n_clouds_jac
  public :: cloud_names,cloud_names_jac,cloud_names_fwd
  public :: idx_cw,idx_ql,idx_qi,idx_qr,idx_qs,idx_qg,idx_qh

  public :: iaerosol_fwd,iaerosol_cv,iaerosol
  public :: n_actual_aerosols,n_aerosols_fwd,n_aerosols_jac
  public :: aerosol_names,aerosol_names_fwd,aerosol_names_jac

  character(len=20),save,allocatable,dimension(:) :: cloud_names
  character(len=20),save,allocatable,dimension(:) :: cloud_names_fwd
  character(len=20),save,allocatable,dimension(:) :: cloud_names_jac
  character(len=20),save,allocatable,dimension(:) :: aerosol_names
  character(len=20),save,allocatable,dimension(:) :: aerosol_names_fwd
  character(len=20),save,allocatable,dimension(:) :: aerosol_names_jac
  logical :: icloud_fwd,icloud_cv,iallsky,cw_cv,ql_cv
  logical :: iaerosol_fwd,iaerosol_cv,iaerosol,iprecip
  integer(i_kind) :: n_actual_clouds,n_clouds_jac,n_clouds_fwd
  integer(i_kind) :: n_actual_aerosols,n_aerosols_fwd,n_aerosols_jac
  integer(i_kind) :: idx_cw,idx_ql,idx_qi,idx_qr,idx_qs,idx_qg,idx_qh

contains

  subroutine radiance_mode_init
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_mode_init
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This routine sets default values for variables used in
!            the radiance processing routines.
!
! program history log:
!   2015-07-20  zhu     
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    use m_kinds, only: i_kind,r_kind
    use gsi_metguess_mod, only: gsi_metguess_get
    use gsi_chemguess_mod, only: gsi_chemguess_get
    use mpeu_util, only: getindex
    use control_vectors, only: cvars3d
    implicit none

    integer(i_kind) icw_av,iql_av,iqi_av,iqtotal,ier
    integer(i_kind) iqr,iqs,iqg 
    integer(i_kind) indx_p25,indx_dust1,indx_dust2,ip25_av,idust1_av,idust2_av

!   initialize variables
    icloud_fwd=.false.
    icloud_cv=.false.
    iallsky=.false.
    cw_cv=.false.
    ql_cv=.false.

    n_actual_clouds=0
    n_clouds_fwd=0 
    n_clouds_jac=0

    iaerosol_fwd=.false.
    iaerosol_cv=.false.
    iaerosol=.false.

    iprecip=.false.

    n_actual_aerosols=0
    n_aerosols_fwd=0
    n_aerosols_jac=0

!   inquire number of clouds 
    call gsi_metguess_get ( 'clouds::3d', n_actual_clouds, ier )
    if (n_actual_clouds>0) then
       allocate(cloud_names(n_actual_clouds))
       call gsi_metguess_get ('clouds::3d', cloud_names, ier)
       call gsi_metguess_get ('clouds_4crtm_fwd::3d', n_clouds_fwd, ier)
       n_clouds_fwd=max(0,n_clouds_fwd)
       if (n_clouds_fwd>0) then
          icloud_fwd=.true.
          allocate(cloud_names_fwd(max(n_clouds_fwd,1)))
          call gsi_metguess_get ('clouds_4crtm_fwd::3d', cloud_names_fwd, ier)

          call gsi_metguess_get ('clouds_4crtm_jac::3d', n_clouds_jac, ier )
          n_clouds_jac=max(0,n_clouds_jac)
          if (n_clouds_jac>0) then
             allocate(cloud_names_jac(max(n_clouds_jac,1)))
             call gsi_metguess_get ('clouds_4crtm_jac::3d', cloud_names_jac, ier)
          end if
          iqr = getindex(cloud_names_fwd,'qr')
          iqs = getindex(cloud_names_fwd,'qs')
          iqg = getindex(cloud_names_fwd,'qg')
          if (iqr>0 .or. iqs>0 .or. iqg>0) iprecip = .true.
       end if

!      inquire number of clouds to participate in CRTM calculations
       call gsi_metguess_get ( 'i4crtm::ql', idx_ql, ier )
       call gsi_metguess_get ( 'i4crtm::qi', idx_qi, ier )
       call gsi_metguess_get ( 'i4crtm::qr', idx_qr, ier )
       call gsi_metguess_get ( 'i4crtm::qs', idx_qs, ier )
       call gsi_metguess_get ( 'i4crtm::qg', idx_qg, ier )
       call gsi_metguess_get ( 'i4crtm::qh', idx_qh, ier )
!      if (idx_ql>10 .or. idx_qi>10 .or. idx_qr>10 .or. idx_qs>10 &
!         .or. idx_qg>10 .or. idx_qh>10) icloud_fwd=.true.

!      Determine whether or not cloud-condensate is the control variable
!      (ges_cw=ges_ql+ges_qi)
       icw_av=getindex(cvars3d,'cw')
       iql_av=getindex(cvars3d,'ql')
       iqi_av=getindex(cvars3d,'qi')

!      Determine whether or not total moisture (water vapor+total cloud
!      condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')

       if (icw_av>0) cw_cv=.true.
       if (iql_av>0) ql_cv=.true.
       if (icw_av>0 .or. iql_av>0 .or. iqi_av>0 .or. iqtotal>0) icloud_cv=.true.
       if (icloud_cv .and. icloud_fwd) iallsky=.true.

    end if  ! end of (n_actual_clouds>0)


!   inquire number of aerosols
    call gsi_chemguess_get ( 'aerosols::3d', n_actual_aerosols, ier )
    if (n_actual_aerosols > 0) then
       iaerosol_fwd=.true.
       allocate(aerosol_names(n_actual_aerosols))
       call gsi_chemguess_get ('aerosols::3d',aerosol_names,ier)
       indx_p25   = getindex(aerosol_names,'p25')
       indx_dust1 = getindex(aerosol_names,'dust1')
       indx_dust2 = getindex(aerosol_names,'dust2')

       call gsi_chemguess_get ( 'aerosols_4crtm::3d', n_aerosols_fwd, ier )
       if (n_aerosols_fwd >0) then
          allocate(aerosol_names_fwd(n_aerosols_fwd))
          call gsi_chemguess_get ( 'aerosols_4crtm::3d', aerosol_names_fwd, ier)  
       end if
       call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', n_aerosols_jac, ier )
       if (n_aerosols_jac >0) then
          allocate(aerosol_names_jac(n_aerosols_jac))
          call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', aerosol_names_jac, ier)  
       end if
    endif

!   Determine whether aerosols are control variables
    ip25_av=getindex(cvars3d,'p25')
    idust1_av=getindex(cvars3d,'dust1')
    idust2_av=getindex(cvars3d,'dust2')
    if (ip25_av>0 .or. idust1_av>0 .or. idust2_av>0) iaerosol_cv=.true.

    if (iaerosol_cv .and. iaerosol_fwd) iaerosol=.true.

    if (mype==0) then
       write(6,*) 'radiance_mode_init: icloud_fwd=',icloud_fwd,' iallsky=',iallsky, &
                  ' cw_cv=',cw_cv,' ql_cv=',ql_cv,' iaerosol_fwd=',iaerosol_fwd,' iaerosol=',iaerosol, &
                  ' iprecip=',iprecip
       write(6,*) 'radiance_mode_init: n_actual_clouds=',n_actual_clouds
       if (n_actual_clouds>0) write(6,*) 'radiance_mode_init: cloud_names=',cloud_names  
       write(6,*) 'radiance_mode_init: n_clouds_fwd=',n_clouds_fwd
       if (n_clouds_fwd>0) write(6,*) 'radiance_mode_init: cloud_names_fwd=',cloud_names_fwd
       write(6,*) 'radiance_mode_init: n_clouds_jac=',n_clouds_jac
       if (n_clouds_jac>0) write(6,*) 'radiance_mode_init: cloud_names_jac=',cloud_names_jac
       write(6,*) 'radiance_mode_init: n_actual_aerosols=',n_actual_aerosols
       if (n_actual_aerosols>0) write(6,*) 'radiance_mode_init: aerosol_names=',aerosol_names
       write(6,*) 'radiance_mode_init: n_aerosols_fwd=',n_aerosols_fwd
       if (n_aerosols_fwd>0) write(6,*) 'radiance_mode_init: aerosol_names_fwd=',aerosol_names_fwd
       write(6,*) 'radiance_mode_init: n_aerosols_jac=',n_aerosols_jac
       if (n_aerosols_jac>0) write(6,*) 'radiance_mode_init: aerosol_names_jac=',aerosol_names_jac
    end if
    
  end subroutine radiance_mode_init

  subroutine radiance_mode_destroy
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    radiance_mode_destroy
!
!   prgrmmr:     yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This routine deallocate arrays
!
! program history log:
!   2015-07-20  zhu     
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
!$$$ end documentation block

    implicit none
 
    if(allocated(cloud_names)) deallocate(cloud_names)
    if(allocated(cloud_names_fwd)) deallocate(cloud_names_fwd)
    if(allocated(cloud_names_jac)) deallocate(cloud_names_jac)
  
    if(allocated(aerosol_names)) deallocate(aerosol_names)
    if(allocated(aerosol_names_fwd)) deallocate(aerosol_names_fwd)
    if(allocated(aerosol_names_jac)) deallocate(aerosol_names_jac)

  end subroutine radiance_mode_destroy

end module radiance_mod

