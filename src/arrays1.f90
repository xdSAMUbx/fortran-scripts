program arrays1
   use iso_fortran_env
   implicit none
   integer(int32) :: i
   integer(int32), allocatable:: x(:)

   allocate(x(10))
   x = [(i, i=1,10)]
   write(*,'(10(i0, 1x))')x
   deallocate(x)

end program arrays1
