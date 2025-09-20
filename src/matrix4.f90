module modJ
  use iso_fortran_env
  implicit none
contains
  pure function J(n) result(res)
    implicit none
    integer(int32), intent(in) :: n
    real(real32), allocatable :: res(:,:)
    integer :: i
    allocate(res(n,n)) 
    res = 1.0_real32 
  end function J
end module modJ
program matrix4
  use iso_fortran_env
  use modJ
  implicit none
  integer(int32) :: n = 5
  real(real32), allocatable :: A(:,:)
  A = J(n)
  write(*,'(5(f4.2,1x))') A
end program matrix4
