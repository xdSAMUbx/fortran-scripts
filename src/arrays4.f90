module mod_geomspace
   use iso_fortran_env
   implicit none
   contains
   subroutine geomspace(ini, fin, res)
      implicit none
      real(real32), intent(in) :: ini, fin
      real(real32), intent(out) :: res(:)
      real(real32) :: step
      integer(int32) :: i, n
      n = size(res)
      step = (fin/ini)**(real(1.0/(n - 1), real32))
      do concurrent(i = 1 : n)
         res(i) = ini * step ** (real(i-1,real32))
      end do
   end subroutine geomspace
end module mod_geomspace
program arrays4
   use iso_fortran_env
   use mod_geomspace
   implicit none
   real(real32) :: x = 1.0, y = 128.0 ! Falta verificar si es 0 el inicial
   integer(int32) :: z = 8
   real(real32), allocatable :: a(:)
   allocate(a(z))
   call geomspace(x,y,a)
   write(*,'(10(f6.2,1x))') a
end program arrays4
