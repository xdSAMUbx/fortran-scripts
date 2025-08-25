module mod_meshgrid
   use iso_fortran_env
   implicit none
   type :: Point2D
      real(real32) :: x,y
   end type Point2D
   interface point
      module procedure pointSca, pointArr
   end interface point
   contains
   pure subroutine pointSca(x,y,res)
      implicit none
      real(real32), intent(in) :: x, y
      type(Point2D), intent(out) :: res
      res%x = x ; res%y = y
   end subroutine pointSca
   pure subroutine pointArr(x,y,res)
      implicit none
      real(real32), intent(in) :: x(:), y(:)
      type(Point2D), allocatable, intent(out) :: res(:)
      integer :: i, n
      n = size(x)
      allocate(res(n))
      do concurrent(i = 1 : n)
         call pointSca(x(i),y(i),res(i))
      end do
   end subroutine pointArr
end module mod_meshgrid
program matrix2
   use iso_fortran_env
   use mod_meshgrid
   implicit none
   type(Point2D), allocatable :: p(:)
   allocate(p(2))
   call point([1.0,2.0],[2.0,4.0],p)
   write(*,'(a,2(f4.2,a))') "El punto es: (", p(2)%x, ",", p(2)%y, ")"
end program matrix2
