program prueba
   use iso_fortran_env
   use mod_eye
   implicit none

   integer(int64) :: mat(3,3), vec(3), i,j
   mat = reshape([1,2,3,4,5,6,7,8,9],[3,3])
   vec = 0
   ! Recorrido por columnas
   write(*,'(3(i0,1x))') mat
   do concurrent (i = 1:3, j=1:2)
      if (mat(i,j) == 0) then
         vec = mat(:,j)
         mat(:,j) = mat(:,j+1)
         mat(:,j+1) = vec
      end if
   end do

   write(*,'(3(i0,1x))') vec
   write(*,'(3(i0,1x))') mat
end program prueba
