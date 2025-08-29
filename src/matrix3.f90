module mod_matrixPoint
   use iso_fortran_env
   implicit none
   type Point
      real(real32) x, y
   end type Point
   contains
   pure subroutine grid(sizeGrid, n, res)
      implicit none
      integer(int32), intent(in) :: n
      real(real32), intent(in), optional :: sizeGrid(:)
      type (Point), allocatable, intent(out) :: res(:)
      real(real32) :: xi, yi, xf, yf, pasox, pasoy
      integer :: ix, iy, i
      allocate(res(n*n))

      xi = sizeGrid(1)
      yi = sizeGrid(2)
      xf = sizeGrid(3)
      yf = sizeGrid(4)
      pasox = (xf - xi)/real(n-1,real32)
      pasoy = (yf - yi)/real(n-1,real32)

      do concurrent (i = 1 : n*n)
         ix = mod(i-1, n) + 1        ! columna
         iy = (i-1) / n + 1          ! fila

         res(i)%x = xi + (ix-1)*pasox
         res(i)%y = yi + (iy-1)*pasoy
      end do

   end subroutine grid
end module mod_matrixPoint
program matrix3
   use iso_fortran_env
   use mod_matrixPoint
   implicit none
   real(real32) :: ini, fin
   real(real32) :: items(4)
   type(Point), allocatable :: gridMat(:)
   integer(int32) :: n
   call cpu_time(ini)
   n = 1000
   items = [1.0,1.0,real(n,real32),real(n,real32)]
   call grid(items,n,gridMat)
   call cpu_time(fin)
   print '("Time = ",f0.15," seconds.")',fin-ini
end program matrix3
