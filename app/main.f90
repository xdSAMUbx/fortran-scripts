program prueba
   use iso_fortran_env
   use mod_point
   implicit none
   integer(int64) :: i,j
   integer(int64) :: x(3)=[(i,i=1,3)], y(3)=[(j,j=1,3)]
   type(iPoint2d),allocatable :: res(:)
   res = point(x,y)
   write(*,'(2(i0,1x))') res
end program prueba
