module setup_radcov
!$$$   module documentation block
!                .      .    .                                       .
! module:    setup_radcov
!
!   prgrmmr:    yanqiu zhu      org: np23                date: 2015-07-20
!
! abstract:  This module contains variables and routines related
!            to cloud usages for radiance assimilation. Extracted
!            from radiance_mod to just the essential covariance settings.
!
! program history log:
!   2015-07-20 Yanqiu Zhu
!   2026-07-24 Extracted to setup_radcov
!
! subroutines included:
!   sub setup_radcov_init
!   setup_radcov_destroy
!
! attributes:
!   language: f90
!
!$$$ end documentation block

  use m_kinds, only: r_kind,i_kind
  use m_mpimod, only: mype
  implicit none
  save

  public :: setup_radcov_init
  public :: setup_radcov_destroy

  public :: icloud_cv, n_clouds_fwd, cloud_names_fwd

  character(len=20),save,allocatable,dimension(:) :: cloud_names_fwd
  logical :: icloud_cv
  integer(i_kind) :: n_clouds_fwd

contains

  subroutine setup_radcov_init
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setup_radcov_init
!
! abstract:  This routine sets default values for variables used in
!            the radiance covariance processing.
!$$$ end documentation block

    use m_kinds, only: i_kind,r_kind
    use gsi_metguess_mod, only: gsi_metguess_get
    use mpeu_util, only: getindex
    use control_vectors, only: cvars3d
    implicit none

    integer(i_kind) icw_av,iql_av,iqi_av,iqtotal,ier
    integer(i_kind) n_actual_clouds

!   initialize variables
    icloud_cv=.false.
    n_clouds_fwd=0 

!   inquire number of clouds 
    call gsi_metguess_get ( 'clouds::3d', n_actual_clouds, ier )
    if (n_actual_clouds>0) then
       call gsi_metguess_get ('clouds_4crtm_fwd::3d', n_clouds_fwd, ier)
       n_clouds_fwd=max(0,n_clouds_fwd)
       if (n_clouds_fwd>0) then
          allocate(cloud_names_fwd(max(n_clouds_fwd,1)))
          call gsi_metguess_get ('clouds_4crtm_fwd::3d', cloud_names_fwd, ier)
       end if

!      Determine whether or not cloud-condensate is the control variable
!      (ges_cw=ges_ql+ges_qi)
       icw_av=getindex(cvars3d,'cw')
       iql_av=getindex(cvars3d,'ql')
       iqi_av=getindex(cvars3d,'qi')

!      Determine whether or not total moisture (water vapor+total cloud
!      condensate) is the control variable
       iqtotal=getindex(cvars3d,'qt')

       if (icw_av>0 .or. iql_av>0 .or. iqi_av>0 .or. iqtotal>0) icloud_cv=.true.

    end if  ! end of (n_actual_clouds>0)

    if (mype==0) then
       write(6,*) 'setup_radcov_init: icloud_cv=',icloud_cv
       write(6,*) 'setup_radcov_init: n_clouds_fwd=',n_clouds_fwd
       if (n_clouds_fwd>0) write(6,*) 'setup_radcov_init: cloud_names_fwd=',cloud_names_fwd
    end if
    
  end subroutine setup_radcov_init

  subroutine setup_radcov_destroy
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setup_radcov_destroy
!
! abstract:  This routine deallocate arrays
!$$$ end documentation block

    implicit none
 
    if(allocated(cloud_names_fwd)) deallocate(cloud_names_fwd)

  end subroutine setup_radcov_destroy

end module setup_radcov
