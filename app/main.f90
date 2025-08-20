program prueba
   use iso_fortran_env
   use mod_eye
   implicit none
   integer, allocatable :: x(:,:)
   integer :: n = 10

   x = eye(n)
   write(*,'(10(i0, 1x))') x
   deallocate(x)

end program prueba
