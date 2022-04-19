module stub_bacio_mod
use m_kinds, only: r_single
use m_kinds, only: i_kind
implicit none
private
public ba_open
public ba_close
public ba_wryte
integer, save :: irec
character(len=256), save :: thisfname
interface ba_open; module procedure baopen_; end interface
interface ba_wryte
  module procedure wryte1_
  module procedure wryte2_
  module procedure wryte3_
end interface
interface ba_close; module procedure baclose_; end interface
contains
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine baopen_ (lu,fname,lrec,ier)
implicit none
integer(i_kind),intent(in) :: lu,lrec
integer(i_kind),intent(out):: ier
character(len=*),intent(in) :: fname
open(lu,file=trim(fname),form='unformatted',access='direct',&
        convert='little_endian',recl=lrec,iostat=ier)
irec=0
thisfname=trim(fname)
end subroutine baopen_
subroutine wryte1_(lu,fld)
implicit none
integer(i_kind),intent(in) :: lu
real(r_single),intent(in) :: fld(:)
irec=irec+1
write(lu,rec=irec) fld
end subroutine wryte1_
subroutine wryte2_(lu,fld)
implicit none
integer(i_kind),intent(in) :: lu
real(r_single),intent(in) :: fld(:,:)
irec=irec+1
write(lu,rec=irec) fld
end subroutine wryte2_
subroutine wryte3_(lu,fld)
implicit none
integer(i_kind),intent(in) :: lu
real(r_single),intent(in) :: fld(:,:,:)
real(r_single),allocatable::tmp(:,:)
integer k
allocate(tmp(size(fld,1),size(fld,2)))
do k=1,size(fld,3)
  irec=irec+1
  tmp=fld(:,:,k)
  write(lu,rec=irec) tmp
enddo
deallocate(tmp)
end subroutine wryte3_
subroutine baclose_(lu,ier)
implicit none
integer(i_kind),intent(in) :: lu
integer(i_kind),intent(out):: ier
close(lu,iostat=ier)
print *, 'Records in file ', trim(thisfname), ':', irec
irec=0
end subroutine baclose_
end module stub_bacio_mod
