program test_bkerror_clim

!use mpi

use constants, only: zero,one
use m_kinds, only: i_kind,r_kind
use m_mpimod, only: mype
use gridmod, only: nlon,nlat,lon2,lat2,lon2,nsig
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
use berror, only: simcv
use jfunc, only: nsubwin,nsclen,npclen,ntclen
use m_gsibclim, only: gsibclim_init
use m_gsibclim, only: gsibclim_final

implicit none

character(len=*), parameter :: myname ="SABerror"

type(gsi_bundle), allocatable :: fcgrad(:)
type(gsi_bundle) :: aux
type(control_vector) :: gradx,grady
type(predictors)     :: sbias

character(len=80):: ifname(1)
character(len=80):: ofname
integer :: ii,ier
logical :: fexist
real(r_kind), pointer :: ptr3d(:,:,:)

call gsibclim_init

if (simcv) then
   call be_cv_space_()
else
   call be_sv_space_()
endif

call gsibclim_final

contains

  subroutine set_silly_(bundle)
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none
  type(gsi_bundle) bundle
  character(len=*), parameter :: myname_ = myname//'*set_silly_'
  real(r_kind),pointer :: ptr3(:,:,:)=>NULL()
  real(r_kind),pointer :: ptr2(:,:)=>NULL()
  integer k,iset
  integer zex(4),zex072(4), zex127(4)
  real(r_kind) :: val
  character(len=2) :: var
!ifname = 'xinc.eta.nc4'
!ofname = 'outvec_bkgtest'
!inquire(file=ifname(1),exist=fexist)
!if(.not.fexist) then
!  call die ('main',': fishy', 99)
!endif
!
  if (mod(mype,6) /= 0) return
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
  val=one
  var='t'
  var='sf'
  if (iset<0) call die(myname_,'no input set',99)
  call gsi_bundlegetpointer(bundle,trim(var),ptr3,ier)
  if(ier==0) then
     if(var=='sf' .or. var=='vp') then
       val=val*1e-5
     endif
     do k=1,size(zex)
        ptr3(10,10,zex(k)) = val
     enddo
     if (mype==0) print *, myname_, ': var= ', trim(var)
     return
  endif
  var='tv'
  call gsi_bundlegetpointer(bundle,trim(var),ptr3,ier)
  if(ier==0) then
     do k=1,size(zex)
        ptr3(10,10,zex(k)) = val
     enddo
     if (mype==0) print *, myname_, ': var= ', trim(var)
     return
  endif
  call gsi_bundlegetpointer(bundle,'ps',ptr2,ier)
  if(ier==0) then
     ptr2(10,10) = 100.
     if (mype==0) print *, myname_, ': var= ', 'ps'
     return
  endif
  end subroutine set_silly_

  subroutine be_cv_space_

! apply B to vector: all in control space

! allocate vectors
  call allocate_cv(gradx)
  call allocate_cv(grady)
  gradx=zero
  grady=zero

  call set_silly_(gradx%step(1))

  call bkerror(gradx,grady, &
               1,nsclen,npclen,ntclen)

  call write_bundle(grady%step(1),'cvbundle')

! clean up
  call deallocate_cv(gradx)
  call deallocate_cv(grady)

  end subroutine be_cv_space_

  subroutine be_sv_space_

! start work space
  allocate(fcgrad(nsubwin))
  do ii=1,nsubwin
      call allocate_state(fcgrad(ii))
      fcgrad(ii) = zero
  end do
  call allocate_preds(sbias)

  call allocate_cv(gradx)
  call allocate_cv(grady)
  gradx=zero
  grady=zero

! get test vector (fcgrad)
! call get_state_perts_ (fcgrad(1))
!
  call set_silly_(fcgrad(1))

  call control2state_ad(fcgrad,sbias,gradx)

! apply B to input (transformed) vector
  call bkerror(gradx,grady, &
               1,nsclen,npclen,ntclen)

  call control2state(grady,fcgrad,sbias)
  call write_bundle(fcgrad(1),'svbundle')

! clean up work space
  call deallocate_cv(gradx)
  call deallocate_cv(grady)
  call deallocate_preds(sbias)
  do ii=1,nsubwin
      call deallocate_state(fcgrad(ii))
  end do
  deallocate(fcgrad)

  end subroutine be_sv_space_

  subroutine get_state_perts_(fc)
  use m_grid2sub1var, only: grid2sub1var
  use gridmod, only: lat1,lon1
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none
  type(gsi_bundle) :: fc
  real(r_kind),allocatable :: grdfld(:,:,:)
  real(r_kind),allocatable :: subfld(:,:,:)
  integer ii,ier
  if (mype==0) then
     allocate(grdfld(nlat,nlon,nsig))
     grdfld=zero
  else
     allocate(grdfld(0,0,0))
  endif
  allocate(subfld(lat2,lon2,nsig))
  call grid2sub1var (grdfld,subfld,ier)
  do ii=1,fc%n3d
     call gsi_bundlegetpointer(fc,trim(fc%r3(ii)%shortname),ptr3d,ier)
     ptr3d = subfld
  enddo
  deallocate(subfld)
  deallocate(grdfld)
  end subroutine get_state_perts_

end program test_bkerror_clim
