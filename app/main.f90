module mod_pack
   use iso_fortran_env
   use mod_point
   implicit none
   contains

end module mod_pack

program prueba
   use iso_fortran_env
   use mod_point
   implicit none
   integer(int32), allocatable :: a(:,:)
   integer(int32) :: i, j, x, y, n
   type(iPoint2d) :: p

   n = 5
   allocate(a(5,5))
   a = reshape([1,2,3,0,0,0,4,5,6,7,0,9,0,0,12,13,14,15,16,0,17,18,20,0,&
      22,0,23,0,0,0],[5,5])
   write(*,'(5(i3,1x))') a
   write(*,*)

   do j = 1, 5       ! ahora primero recorres filas
      do i = 1, 5    ! y luego columnas
         if (a(i,j) /= 0) then
            write(*,"(a,i3,a,i0,a,i0,a)") "El valor es: ", a(i,j), &
               " y su posición es: (",j, ",",i, ")"
            p = point(i,j)
            write(*,*) p
         end if
      end do
   end do

end program prueba
