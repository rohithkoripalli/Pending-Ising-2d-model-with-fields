module common

public :: initial,metropolis,DeltaE
public :: data,output,save_config,read_config
integer, public, parameter :: double = 8
real (kind = double), public :: T,E,M
integer, public, dimension(:,:), allocatable :: spin
real (kind = double), public, dimension(-8:8) :: w
integer, public, dimension(12) :: seed
integer, public :: N,L,mcs
integer, public :: accept
contains

subroutine initial(nequil,cum)
   integer, intent (out) :: nequil
   real (kind = double), dimension(:), intent (out) :: cum
   integer :: x,y,up,right,sums,i,dE
   real :: rnd
   open(unit=11,file='C:\Users\MY PC\Desktop\c_tut\ising last\input.txt',status='old',action='read')
   read(11,*) L
   read(11,*) T
   read(11,*) nequil
   read(11,*) mcs
   read(11,*) h
   allocate(spin(L,L))
   N = L*L

   seed(1) = 1239
   seed(2) = 9863        ! need for Fortran 90
   call random_seed(put=seed)
   M = 0.0
!  random initial configuration
   do y = 1,L
      do x = 1,L
         call random_number(rnd)
         if (rnd < 0.5) then
            spin(x,y) = 1
         else
            spin(x,y) = -1
         end if
         M = M + spin(x,y)

      end do
   end do

!  compute initial energy
   E = 0.0
   do y = 1,L
!  periodic boundary conditions
      if (y == L) then
         up = 1
      else
         up = y + 1
      end if
      do x = 1,L
         if (x == L) then
            right = 1
         else
            right = x + 1
         end if
         sums = spin(x,up) + spin(right,y)
         E = E - spin(x,y)*sums
      end do
   end do
!  compute boltzmann probability ratios
   do dE = -8,8,4
      w(dE) = exp(-dE/T)
   end do
   accept = 0
   do i = 1,5
      cum(i) = 0.0
   end do

end subroutine initial

subroutine metropolis()
!  one Monte Carlo step per spin
   integer :: ispin,x,y,dE
   real :: rnd
   do ispin = 1,N
!     random x and y coordinates for trial spin
      call random_number(rnd)
      x = int(L*rnd) + 1
      call random_number(rnd)
      y = int(L*rnd) + 1
      dE = DeltaE(x,y)
      call random_number(rnd)
      if (rnd <= w(dE)) then
         spin(x,y) = -spin(x,y)
         accept = accept + 1
         M = M + 2*spin(x,y)
         E = E + dE

      end if
   end do
end subroutine metropolis





function DeltaE(x,y) result (DeltaE_result)
!  periodic boundary conditions
   integer, intent (in) :: x,y
   integer :: DeltaE_result
   integer :: left
   integer :: right
   integer :: up
   integer :: down
   if (x == 1) then
      left = spin(L,y)
      right = spin(2,y)
   else if (x == L) then
      left = spin(L-1,y)
      right = spin(1,y)
   else
      left = spin(x-1,y)
      right = spin(x+1,y)
   end if
   if (y == 1) then
      up = spin(x,2)
      down = spin(x,L)
   else if (y == L) then
      up = spin(x,1)
      down = spin(x,L-1)
   else
      up = spin(x,y+1)
      down = spin(x,y-1)
   end if
   DeltaE_result = (2*spin(x,y)*(left + right + up + down)) + (h * spin(x,y))
end function DeltaE
subroutine data(cum)
!  accumulate data after every Monte Carlo step per spin
   real (kind = double), dimension(:), intent (inout) :: cum
   integer :: tpass
   cum(1) = cum(1) + E
   cum(2) = cum(2) + E*E
   cum(3) = cum(3) + M
   cum(4) = cum(4) + M*M
   cum(5) = cum(5) + abs(M)
   write(54,*) tpass,cum(1),cum(2),cum(3),cum(4),cum(5)
end subroutine data

subroutine output(cum)
   real (kind = double), dimension(:), intent (inout) :: cum
   real (kind = double) :: norm,eave,e2ave,mave,m2ave,abs_mave
   real :: acceptance_prob
   real (kind = double) :: T
   integer, dimension(12) :: seed_old
   acceptance_prob = real(accept)/real((N*mcs))
   eave = cum(1)/mcs
   e2ave = cum(2)/mcs
   mave = cum(3)/mcs
   m2ave = cum(4)/mcs
   abs_mave = cum(5)/mcs

end subroutine output
subroutine save_config()
   character(len = 32) :: config
   print *, "file name of configuration?"
   read *, config
   open(unit=5,file=config,status="new",action="write")
   write(unit=5,fmt="(f13.6)") T
!   write(unit=5,fmt="(i4)") spin
   close(unit=5)
end subroutine save_config

subroutine read_config()
   integer :: x,y
   character(len = 20) :: file_name
   print *, "filename ?"
   read *, file_name
   open(unit=4,file=file_name,status="old",action="read")
   read(unit=4,fmt=*) T
   do y = 1,L
      do x = 1,L
         read(unit=1,fmt=*) spin(x,y)
      end do
   end do
   close(unit=4)
end subroutine read_config


end module common
