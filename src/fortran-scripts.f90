module fortran_scripts
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fortran-scripts!"
  end subroutine say_hello
end module fortran_scripts
