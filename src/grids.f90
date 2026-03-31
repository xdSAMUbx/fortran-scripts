module grid_functions
    use iso_fortran_env
    use legendre_functions
    implicit none
contains
    pure subroutine CARTESIAN_SEC(ini, fin, steps, sec)
        real(real64), intent(in) :: ini, fin
        integer(int32), intent(in) :: steps
        real(real64), allocatable, intent(out) :: sec(:)

        integer(int32) :: n
        real(real64) :: dist

        dist = (fin - ini) / steps
        allocate(sec(0:steps))
        do n = 0, steps
            sec(n) = ini + real(n,real64) * dist
        end do
    end subroutine CARTESIAN_SEC
end module grid_functions

program grids
    use iso_fortran_env
    use grid_functions
    implicit none

    integer(int32) :: n, Nmax = 5
    real(real64) :: ini, fin
    real(real64), allocatable :: sec(:)

    ini = 10.0_real64
    fin = 15.0_real64
    call CARTESIAN_SEC(ini, fin, Nmax, sec)
    do n = 0, Nmax
        write(*,'("El valor ",I0," es: ",F5.2)') n, sec(n)
    end do

end program grids