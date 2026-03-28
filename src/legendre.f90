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

    subroutine legendre_function(n, theta, p_n)
        implicit none
        integer(int32), intent(in) :: n
        real(real64), intent(in) :: theta
        real(real64), intent(out) :: p_n

        integer(int32) :: i
        real(real64) :: p_n0, p_n1, ct, ri
        
        ct = theta
        call to_radians(ct)
        ct = cos(ct)

        select case(n)
        case (0)
            p_n = 1.0_real64
        case (1)
            p_n = ct
        case default
            p_n0 = 1.0_real64
            p_n1 = ct
            do i = 2, n
                ri = real(i, real64)
                p_n = ((2.0_real64*ri - 1.0_real64) * ct * p_n1 &
                    - (ri - 1.0_real64) * p_n0) / ri
                p_n0 = p_n1
                p_n1 = p_n 
            end do
        end select
        
    end subroutine legendre_function

    subroutine normalized_legendre_function(n, theta, p_n)
        implicit none
        integer(int32), intent(in) :: n
        real(real64), intent(in) :: theta
        real(real64), intent(out) :: p_n

        call legendre_function(n, theta, p_n)
        p_n = sqrt(2.0_real64 * real(n, real64) + 1.0_real64) * p_n

    end subroutine normalized_legendre_function
end module prueba_legendre

program physical_geodesy
    use iso_fortran_env
    use prueba_legendre
    implicit none
    real(real64) :: prueba, ang

    ang = 10.0_real64
    call normalized_legendre_function(100000_int32, ang, prueba)

    write(*,*) "La respuesta es: ", prueba
    
end program physical_geodesy