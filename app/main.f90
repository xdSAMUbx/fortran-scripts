program main
  use fortran_scripts, only: say_hello
  implicit none
  integer::i

  write(*,*)
  call say_hello()
end program main
