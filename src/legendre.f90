module prueba_legendre
    use iso_fortran_env
    implicit none
contains
    subroutine to_radians(val1)
        implicit none
        double precision, parameter :: pi = acos(-1.0d0)
        real(real64), intent(inout) :: val1
        val1 = val1 * (pi / 180.0)
    end subroutine to_radians    

    pure subroutine leg_func(n, theta, p_n)
        implicit none
        real(real64), intent(in) :: theta
        integer(int16), intent(in) :: n
        real(real64), intent(out) :: p_n
        integer(int16) :: i
        real(real64) :: p_n0, p_n1
        
        if ( n == 0 ) then
            p_n = 1.0_real64
            return
        else if ( n == 1 ) then
            p_n = cos(theta)
            return
        end if

        p_n0 = 1.0_real64
        p_n1 = cos(theta)

        do i = 2, n
            p_n = - ((i - 1.0_real64) / i) * p_n0 &
                + ((2 * i - 1.0_real64) / i) * cos(theta) * p_n1
            p_n0 = p_n1
            p_n1 = p_n
        end do
        
    end subroutine leg_func
end module prueba_legendre

program physical_geodesy
    use iso_fortran_env
    use prueba_legendre
    implicit none
    real(real64) :: prueba, ang

    ang = 10.0_real64
    call to_radians(ang)
    call leg_func(10000_int16, ang, prueba)

    write(*,*) "La respuesta es: ", prueba
    
end program physical_geodesy