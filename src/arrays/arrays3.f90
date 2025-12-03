module mod_arange
   use iso_fortran_env
   implicit none
   contains
   subroutine arange(ini,fin,step,res)
      real(real32), intent(in) :: ini,fin,step
      real(real32), allocatable, intent(out) :: res(:)
      integer(int32) :: i, n
      n = int(((fin - ini) / step),int32) + 1
      allocate(res(n))
      do concurrent(i=1:n)
         res(i) = ini + real(i-1,real32) * step
      end do
   end subroutine arange
end module mod_arange
program arrays3
   use iso_fortran_env
   use mod_arange
   implicit none
   real(real32) :: x = 0.0, y = 1000.0, z = 0.00001, ini, fin
   real(real32), allocatable :: a(:)
   call cpu_time(ini)
   call arange(x,y,z,a) ! El programa tarda menos si no se debe mostrar el resultado
   call cpu_time(fin)
   write(*,'(a,f6.4)') "El programa tardo: ", fin - ini
end program arrays3
