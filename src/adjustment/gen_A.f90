module pruebas
    implicit none
contains
    subroutine gen_A_matrix(ini,fin,mtx_A)
        implicit none
        character(len=*), intent(in) :: ini(:), fin(:)
        real(kind=4), allocatable, intent(out) :: mtx_A(:,:)
        allocate(mtx_A(size(ini),size(fin)))
        mtx_A = 0
        write(*,*) mtx_A
    end subroutine gen_A_matrix
end module pruebas

program leer_csv
    use iso_fortran_env
    use pruebas
    implicit none
    character(len=5) :: v(2) = ["Holaa","Mundo"]
    character(len=5) :: z(2) = ["Holaa","Mundo"]
    real(kind=4), allocatable :: A(:,:)
    call gen_A_matrix(v,z,A)
end program leer_csv