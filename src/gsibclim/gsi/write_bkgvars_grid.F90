subroutine write_bkgvars_grid(a,b,c,d,mype)
!$$$  subroutine documentation block
!
! subprogram:    write_bkgvars_grid
!
!   prgrmmr:
!
! abstract:  modified routine to write out files to compare spectral computation
!            of horizontal derivatives with the derivatives that are being
!            carried around for the dynamical balance constraint
!
! program history log:
!   2008-03-27  safford -- add subprogram doc block, rm unused vars and uses
!
!   input argument list:
!     mype     - mpi task id
!     a        -
!     b        -
!     c        -
!     d        -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
  use m_kinds, only: r_kind,i_kind,r_single
  use gridmod, only: nlat,nlon,nsig,lat2,lon2
#ifndef HAVE_BACIO
  use stub_bacio_mod, only: ba_open,ba_close,ba_wryte
#endif
  implicit none

  integer(i_kind)                       ,intent(in   ) :: mype

  real(r_kind),dimension(lat2,lon2,nsig),intent(in   ) :: a,b,c
  real(r_kind),dimension(lat2,lon2)     ,intent(in   ) :: d

  character(255):: grdfile

  real(r_kind),dimension(nlat,nlon,nsig):: ag,bg,cg
  real(r_kind),dimension(nlat,nlon):: dg

  real(r_single),dimension(nlon,nlat,nsig):: a4,b4,c4
  real(r_single),dimension(nlon,nlat):: d4

  integer(i_kind) ncfggg,iret,i,j,k

! gather stuff to processor 0
  do k=1,nsig
     call gather_stuff2(a(1,1,k),ag(1,1,k),mype,0)
     call gather_stuff2(b(1,1,k),bg(1,1,k),mype,0)
     call gather_stuff2(c(1,1,k),cg(1,1,k),mype,0)
  end do
  call gather_stuff2(d,dg,mype,0)

  if (mype==0) then
     write(6,*) 'WRITE OUT NEW VARIANCES'
! load single precision arrays
     do k=1,nsig
        do j=1,nlon
           do i=1,nlat
              a4(j,i,k)=ag(i,j,k)
              b4(j,i,k)=bg(i,j,k)
              c4(j,i,k)=cg(i,j,k)
           end do
        end do
     end do
     do j=1,nlon
        do i=1,nlat
           d4(j,i)=dg(i,j)
        end do
     end do

! Create byte-addressable binary file for grads
     grdfile='bkgvar_rewgt.grd'
     ncfggg=len_trim(grdfile)
#ifdef HAVE_BACIO
     call baopenwt(22,grdfile(1:ncfggg),iret)
     call wryte(22,4*nlat*nlon*nsig,a4)
     call wryte(22,4*nlat*nlon*nsig,b4)
     call wryte(22,4*nlat*nlon*nsig,c4)
     call wryte(22,4*nlat*nlon,d4)
     call baclose(22,iret)
#else /* HAVE_BACIO */
     call ba_open(22,grdfile(1:ncfggg),4*nlat*nlon,iret)
     call ba_wryte(22,a4)
     call ba_wryte(22,b4)
     call ba_wryte(22,c4)
     call ba_wryte(22,d4)
     call ba_close(22,iret)
#endif /* HAVE_BACIO */
  end if
   
  return
end subroutine write_bkgvars_grid

subroutine write_bkgvars2_grid
!$$$  subroutine documentation block
!
! subprogram:    write_bkgvars2_grid
!
!   prgrmmr:
!
! abstract:  modified routine to write out files to compare spectral computation
!            of horizontal derivatives with the derivatives that are being
!            carried around for the dynamical balance constraint
!
! program history log:
!   2008-03-27  safford -- add subprogram doc block, rm unused vars and uses
!   2010-06-18  todling -- generalized to show all variances; create ctl
!   2010-10-20  pagowski - add cmaq
!   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS
!                           core
!   2018-02-15  wu      - add code for fv3_regional
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
  use m_kinds, only: r_kind,i_kind,r_single
  use m_mpimod, only: mype
  use constants, only: ten
  use guess_grids, only: gsiguess_get_ref_gesprs
  use gridmod, only: nlat,nlon,nsig
  use control_vectors, only: nc3d,nc2d,mvars
  use control_vectors, only: cvars3d,cvars2d,cvarsmd
  use berror, only: dssv,dssvs
  use mpeu_util, only: get_lun => luavail
#ifndef HAVE_BACIO
  use stub_bacio_mod, only: ba_open,ba_close,ba_wryte
#endif
  implicit none

  character(255):: grdfile

  real(r_kind),dimension(nlat,nlon,nsig,nc3d):: ag
  real(r_kind),dimension(nlat,nlon,nc2d+mvars):: dg

  real(r_single),dimension(nlon,nlat,nsig,nc3d):: a4
  real(r_single),dimension(nlon,nlat,nc2d+mvars):: d4

  real(r_kind)   ,dimension(nsig+1)::prs
  integer(i_kind) ncfggg,iret,lu,i,j,k,n

! gather stuff to processor 0
  do n=1,nc3d
     do k=1,nsig
        call gather_stuff2(dssv(1,1,k,n),ag(1,1,k,n),mype,0)
     end do
  end do
  do n=1,nc2d
     call gather_stuff2(dssvs(1,1,n),dg(1,1,n),mype,0)
  end do
  do n=1,mvars
     call gather_stuff2(dssvs(1,1,nc2d+n),dg(1,1,nc2d+n),mype,0)
  end do

  call gsiguess_get_ref_gesprs(prs)

  if (mype==0) then
     write(6,*) 'WRITE OUT NEW VARIANCES'
!    Load single precision arrays
     do n=1,nc3d
        do k=1,nsig
           do j=1,nlon
              do i=1,nlat
                 a4(j,i,k,n)=ag(i,j,k,n)
              end do
           end do
        end do
     end do
     do n=1,nc2d+mvars
        do j=1,nlon
           do i=1,nlat
              d4(j,i,n)=dg(i,j,n)
           end do
        end do
     end do

!    Create byte-addressable binary file for grads
     grdfile='bkgvar_smooth.grd'
     ncfggg=len_trim(grdfile)
     lu=get_lun()
#ifdef HAVE_BACIO
     call baopenwt(lu,grdfile(1:ncfggg),iret)
!    Loop over 3d-variances
     do n=1,nc3d
        call wryte(lu,4*nlat*nlon*nsig,a4(1,1,1,n))
     enddo
!    Loop over 2d-variances
     do n=1,nc2d+mvars
        call wryte(lu,4*nlat*nlon,d4(1,1,n))
     enddo
     call baclose(lu,iret)
#else /* HAVE_BACIO */
     call ba_open(lu,grdfile(1:ncfggg),4*nlat*nlon,iret)
!    Loop over 3d-variances
     do n=1,nc3d
        call ba_wryte(lu,a4(:,:,:,n))
     enddo
!    Loop over 2d-variances
     do n=1,nc2d+mvars
        call ba_wryte(lu,d4(:,:,n))
     enddo
     call ba_close(lu,iret)
#endif /* HAVE_BACIO */

!    Now create corresponding grads table file
     lu=get_lun()
     open(lu,file='bkgvar_smooth.ctl',form='formatted')
     write(lu,'(2a)') 'DSET  ^', trim(grdfile)
     write(lu,'(2a)') 'TITLE ', 'gsi berror variances'
     write(lu,'(a,2x,e13.6)') 'UNDEF', 1.E+15 ! any other preference for this?
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'XDEF',nlon, 'LINEAR',   0.0, 360./nlon
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'YDEF',nlat, 'LINEAR', -90.0, 180./(nlat-1.)
     write(lu,'(a,2x,i4,2x,a,100(1x,f8.3))')    'ZDEF',nsig, 'LEVELS', prs(1:nsig) ! prs is in cbar (convert to mb)
     write(lu,'(a,2x,i4,2x,a)')   'TDEF', 1, 'LINEAR 12:00Z04JUL1776 6hr' ! any date suffices
     write(lu,'(a,2x,i4)')        'VARS',nc3d+nc2d+mvars
     do n=1,nc3d
        write(lu,'(a,1x,2(i4,1x),a)') trim(cvars3d(n)),nsig,0,trim(cvars3d(n))
     enddo
     do n=1,nc2d
        write(lu,'(a,1x,2(i4,1x),a)') trim(cvars2d(n)),   1,0,trim(cvars2d(n))
     enddo
     do n=1,mvars
        write(lu,'(a,1x,2(i4,1x),a)') trim(cvarsmd(n)),   1,0,trim(cvarsmd(n))
     enddo
     write(lu,'(a)') 'ENDVARS'
     close(lu)

  end if ! mype=0
   
  return
end subroutine write_bkgvars2_grid

subroutine write_bundle(bundle,fname)
!$$$  subroutine documentation block
!
! subprogram:    write_bundle
!
!   prgrmmr:
!
! abstract:  modified routine to write out files to compare spectral computation
!            of horizontal derivatives with the derivatives that are being
!            carried around for the dynamical balance constraint
!
! program history log:
!   2022-02-19  todling -- based on others, but using bundle
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:
!
!$$$
  use m_kinds, only: r_kind,i_kind,r_single
  use m_mpimod, only: mype
  use constants, only: ten
  use guess_grids, only: gsiguess_get_ref_gesprs
  use gridmod, only: nlat,nlon,nsig
! use control_vectors, only: nc3d,nc2d,mvars
! use control_vectors, only: cvars3d,cvars2d,cvarsmd
! use berror, only: dssv,dssvs
  use mpeu_util, only: get_lun => luavail
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
#ifndef HAVE_BACIO
  use stub_bacio_mod, only: ba_open,ba_close,ba_wryte
#endif
  implicit none

  type(GSI_Bundle) :: Bundle
  character(len=*),intent(in) :: fname

  character(255):: grdfile

  real(r_kind),pointer,dimension(:,:,:):: ptr3d=>NULL()
  real(r_kind),pointer,dimension(:,:)  :: ptr2d=>NULL()

  real(r_kind),allocatable,dimension(:,:,:,:):: ag
  real(r_kind),allocatable,dimension(:,:,:)  :: dg

  real(r_single),allocatable,dimension(:,:,:,:):: a4
  real(r_single),allocatable,dimension(:,:,:)  :: d4

  real(r_kind)   ,dimension(nsig+1)::prs
  integer(i_kind) ncfggg,iret,lu,i,j,k,n
  integer(i_kind) n2d,n3d,ier

  n3d = Bundle%n3d
  n2d = Bundle%n2d

  if (n3d>0) then
    allocate(ag(nlat,nlon,nsig,n3d))
    allocate(a4(nlon,nlat,nsig,n3d))
  else
    n3d=0
  endif

  if (n2d>0) then
    allocate(dg(nlat,nlon,n2d))
    allocate(d4(nlon,nlat,n2d))
  else
    n2d=0
  endif

! gather stuff to processor 0
  do n = 1, n3d
     call gsi_bundlegetpointer(Bundle,trim(Bundle%r3(n)%shortname),ptr3d,ier)
     do k=1,nsig
        call gather_stuff2(ptr3d(:,:,k),ag(1,1,k,n),mype,0)
     end do
  end do
  do n = 1, n2d
     call gsi_bundlegetpointer(Bundle,trim(Bundle%r2(n)%shortname),ptr2d,ier)
     call gather_stuff2(ptr2d,dg(1,1,n),mype,0)
  end do

  call gsiguess_get_ref_gesprs(prs)

  if (mype==0) then
     write(6,*) 'WRITE OUT BUNDLE'
!    Load single precision arrays
     do n=1,n3d
        do k=1,nsig
           do j=1,nlon
              do i=1,nlat
                 a4(j,i,k,n)=ag(i,j,k,n)
              end do
           end do
        end do
     end do
     do n=1,n2d
        do j=1,nlon
           do i=1,nlat
              d4(j,i,n)=dg(i,j,n)
           end do
        end do
     end do

!    Create byte-addressable binary file for grads
     grdfile=trim(fname)//'.grd'
     ncfggg=len_trim(grdfile)
     lu=get_lun()
#ifdef HAVE_BACIO
     call baopen(lu,grdfile(1:ncfggg),iret)
!    Loop over 3d-variances
     do n=1,n3d
        call wryte(lu,4*nlat*nlon*nsig,a4(1,1,1,n))
     enddo
!    Loop over 2d-variances
     do n=1,n2d
        call wryte(lu,4*nlat*nlon,d4(1,1,n))
     enddo
     call baclose(lu,iret)
#else /* HAVE_BACIO */
     call ba_open(lu,grdfile(1:ncfggg),4*nlat*nlon,iret)
!    Loop over 3d-variances
     do n=1,n3d
        call ba_wryte(lu,a4(:,:,:,n))
     enddo
!    Loop over 2d-variances
     do n=1,n2d
        call ba_wryte(lu,d4(:,:,n))
     enddo
     call ba_close(lu,iret)
#endif /* HAVE_BACIO */

!    Now create corresponding grads table file
     lu=get_lun()
     open(lu,file=trim(fname)//'.ctl',form='formatted')
     write(lu,'(2a)') 'DSET  ^', trim(grdfile)
     write(lu,'(2a)') 'TITLE ', 'gsi berror variances'
     write(lu,'(a,2x,e13.6)') 'UNDEF', 1.E+15 ! any other preference for this?
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'XDEF',nlon, 'LINEAR',   0.0, 360./nlon
     write(lu,'(a,2x,i4,2x,a,2x,f5.1,2x,f9.6)') 'YDEF',nlat, 'LINEAR', -90.0, 180./(nlat-1.)
     write(lu,'(a,2x,i4,2x,a,100(1x,f8.3))')    'ZDEF',nsig, 'LEVELS', prs(1:nsig) ! prs is in cbar (convert to mb)
     write(lu,'(a,2x,i4,2x,a)')   'TDEF', 1, 'LINEAR 12:00Z04JUL1776 6hr' ! any date suffices
     write(lu,'(a,2x,i4)')        'VARS',n3d+n2d
     do n=1,n3d
        write(lu,'(a,1x,2(i4,1x),a)') trim(Bundle%r3(n)%shortname),nsig,0,trim(Bundle%r3(n)%shortname)
     enddo
     do n=1,n2d
        write(lu,'(a,1x,2(i4,1x),a)') trim(Bundle%r2(n)%shortname), 1,0,trim(Bundle%r2(n)%shortname)
     enddo
     write(lu,'(a)') 'ENDVARS'
     close(lu)

  end if ! mype=0
   
  if (n3d>0) then
    deallocate(a4)
    deallocate(ag)
  endif

  if (n2d>0) then
    deallocate(d4)
    deallocate(dg)
  endif

  return
end subroutine write_bundle
