program akbk
implicit none
integer, parameter :: nlev = 129
integer, parameter :: lu = 10
real(8) ak(nlev),bk(nlev)
integer k,iostat
character(len=80) crec
    open(lu,file='akbk.txt',form='formatted')
    do k=1,nlev
       read(lu,'(a80)',iostat=iostat) crec
       if (iostat /= 0) exit
       read(crec,*,iostat=iostat) ak(k),bk(k)
    enddo
    close(lu)
!   do k=nlev-1,1,-1 ! disregard redundant level at the top
!      write(6,'(f10.3,2x,f11.8)') ak(k), bk(k)
!   enddo
    write(6,'(5(f10.3,a))') (ak(k), ',',k=nlev-1,1,-1)
    write(6,'(5(f11.8,a))') (bk(k), ',',k=nlev-1,1,-1)
end program akbk

