subroutine outgrads1(f,nx,ny,label)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    outgrads1
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-18  lueken - added subprogram doc block
!   2012-12-11  parrish - assign np a value.
!
!   input argument list:
!    label
!    nx,ny
!    f
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: i_kind,r_single
  implicit none

  character(*)   ,intent(in   ) :: label
  integer(i_kind),intent(in   ) :: nx,ny
  real(r_single) ,intent(in   ) :: f(nx,ny)

  integer(i_kind) i,l,next,last,np,ntime,ioutdat,ioutcor,koutmax
  real(r_single) rlonmap0,undef,dlonmap,pinc,startp,rlatmap0,dlatmap
  character(80) dsdes,dsdat
  character(80) datdes(1000)
  character(1) blank
  data blank/' '/
  data undef/-9.99e33_r_single/

  ioutcor=10
  ioutdat=11
  np=1

  write(dsdes,'(a,".ctl")')trim(label)
  write(dsdat,'(a,".grd")')trim(label)
  open(unit=ioutcor,file=dsdes,form='formatted')
  open(unit=ioutdat,file=dsdat,form='unformatted')
  ntime=1
  rlonmap0=1._r_single
  dlonmap=1._r_single
  rlatmap0=1._r_single
  dlatmap=1._r_single
  startp=1._r_single
  pinc=1._r_single
  koutmax=1
  do i=1,1000
     write(datdes(i),'(80a1)')(blank,l=1,80)
  end do
  write(datdes(1),'("DSET ",a)')trim(dsdat)
  write(datdes(2),'("options big_endian sequential")')
  write(datdes(3),'("TITLE ",a)')trim(label)
  write(datdes(4),'("UNDEF ",e11.2)')undef
  write(datdes(5),'("XDEF ",i5," LINEAR ",f7.2,f7.2)')nx,rlonmap0,dlonmap
  write(datdes(6),'("YDEF ",i5," LINEAR ",f7.2,f7.2)')ny,rlatmap0,dlatmap
  next=7
  write(datdes(next),'("ZDEF ",i5," LINEAR ",f7.2,f7.2)')np,startp,pinc
  next=next+1
  write(datdes(next),'("TDEF ",i5," LINEAR 0Z23may1992 24hr")')koutmax
  next=next+1
  write(datdes(next),'("VARS 1")')
  next=next+1
  write(datdes(next),'("f   ",i5," 99 f   ")')np
  next=next+1
  write(datdes(next),'("ENDVARS")')
  last=next
  write(ioutcor,'(a80)')(datdes(i),i=1,last)
  close(ioutcor)

  write(ioutdat) f
  close(ioutdat)

end subroutine outgrads1
