program timer
   implicit none
   integer :: i,n
   integer,allocatable :: x(:)
   real :: ini, fin
   call cpu_time(ini)
   n = 1000000000
   allocate(x(n))
   do concurrent (i = 1: n)
      x(i) = i
   end do
   call cpu_time(fin)
   print '("Time = ",f6.3," seconds.")',fin-ini
end program timer
