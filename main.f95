program ising
!  metropolis algorithm for the ising model on a square lattice
   use common
   integer :: nequil,pass,ipass
   real (kind = double), dimension(5) :: cum
   integer :: tpass
   call initial(nequil,cum)
!  equilibrate system
   do pass = 1,nequil
      call metropolis()
      write(53,*) pass, E
   end do

     do y = 1,L
      do x = 1,L
        write(52,*) spin(x,y)
      end do
     end do
   ! read last equilibriate configuration
    read(unit=52,fmt=*,IOSTAT=KODE)
!  accumulate data while updating spins
   do tpass = T,(1000*T),1
    do ipass = 1,mcs
      call metropolis()
      call data(cum)
    end do
   end do
   call output(cum)

end program ising
