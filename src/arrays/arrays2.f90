module mod_linspace
   use iso_fortran_env
   implicit none
   contains
   subroutine linspace(ini, fin, res)
      implicit none
      real(real32), intent(in) :: ini, fin
      real(real32), intent(out) :: res(:)
      integer(int32) :: i, n
      real(real32) :: step
      n = size(res)
      step = (fin - ini) / real(n-1,real32)
      do concurrent (i = 1: n)
         res(i) = ini + real(i-1,real32) * step
      end do
   end subroutine linspace
end module mod_linspace
program arrays2
   use iso_fortran_env
   use mod_linspace
   implicit none
   integer(int32) :: z = 10
   real(real32) :: x = 1.0, y = 100.0
   real(real32), allocatable :: a(:)
   allocate(a(z))
   call linspace(x,y,a) ! Modifica el arreglo creado en 0 ya que se guarda el espacio en memoria automáticamente
   write(*,'(5(f6.2,1x))')a
end program arrays2
