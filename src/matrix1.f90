module mod_eye
   use iso_fortran_env
   implicit none
   contains
   subroutine eye(n, res)
      integer(int32), intent(in) :: n
      real(real32),allocatable, intent(out) :: res(:,:)
      integer(int32) :: i
      allocate(res(n,n))
      res = 0.0
      do concurrent (i = 1 : n)
         res(i,i) = 1
      end do
   end subroutine eye
end module mod_eye
program matrix1
   use iso_fortran_env
   use mod_eye
   implicit none
   real(real32), allocatable :: a(:,:)
   call eye(4, a)
   write(*,'(4(f4.1, 1x))') a
end program matrix1
