program main
   use iso_fortran_env
   implicit none
   integer :: ti, ni

   ti = this_image()
   ni = num_images()

   write(*,*) "Hola desde: ", ti, " con un número de imagenes: ",ni
end program main
