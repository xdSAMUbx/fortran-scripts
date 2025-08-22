program main
   use iso_fortran_env
   implicit none
   integer :: me, np
   me = this_image()
   np = num_images()

   write(*,*) "Hola desde: ",me," de ",np
end program main
