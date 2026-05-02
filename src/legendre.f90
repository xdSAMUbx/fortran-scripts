module legendre_functions
    use iso_fortran_env
    implicit none

    real(real64), parameter :: PI     = 3.141592653589793_real64
    real(real64), parameter :: ROOT_3 = 1.7320508075688772_real64
    real(real64), parameter :: B      = 1.0e+280_real64
    real(real64), parameter :: BINV   = 1.0e-280_real64

contains

    subroutine radians(val1)
        real(real64), intent(inout) :: val1
        val1 = real(val1 * PI / 180.0_real64,real64)
    end subroutine

    subroutine ALF_ZONAL(Nmax, teta, P)
        integer(int32), intent(in)  :: Nmax
        real(real64),   intent(in)  :: teta
        real(real64), allocatable, intent(out) :: P(:)

        integer(int32) :: n
        real(real64)   :: x, a, b_, rn

        allocate(P(0:Nmax))
        P = 0.0_real64

        x    = cos(teta * (PI / 180.0_real64))
        P(0) = 1.0_real64
        if (Nmax < 1) return
        P(1) = ROOT_3 * x

        do n = 2, Nmax
            rn = real(n, real64)
            a  = sqrt((2.0_real64*n - 1.0_real64) * (2.0_real64*n + 1.0_real64)) / rn
            b_ = (rn - 1.0_real64) / rn &                     ! ✅ real64, no entera
                 * sqrt((2.0_real64*n + 1.0_real64) / (2.0_real64*n - 3.0_real64))

            P(n) = a * x * P(n-1) - b_ * P(n-2)
        end do

    end subroutine ALF_ZONAL

    subroutine MFC_ALF_FUKU(Nmax, teta, P)
        integer(int32), intent(in)  :: Nmax
        real(real64),   intent(in)  :: teta
        real(real64), allocatable, intent(out) :: P(:,:)

        integer(int32) :: n, m, xe, xe1, xe2
        real(real64)   :: a, b_, t, u, xf, xf1, xf2
        logical        :: in_xrange

        u = sin(teta * PI / 180.0_real64)
        t = cos(teta * PI / 180.0_real64)

        allocate(P(0:Nmax, 0:Nmax))
        P = 0.0_real64

        P(0,0) = 1.0_real64
        P(1,0) = ROOT_3 * t
        P(1,1) = ROOT_3 * u

        xf = 1.0_real64
        xe = 0

        do m = 0, Nmax

            if (m >= 2) then
                xf = sqrt((2.0_real64*m + 1.0_real64) &
                         / (2.0_real64*m)) * u * xf
                if (abs(xf) < BINV .and. xf /= 0.0_real64) then
                    xf = xf * B   ;   xe = xe - 1
                else if (abs(xf) > B) then
                    xf = xf * BINV   ;   xe = xe + 1
                end if
            else if (m == 1) then
                xf = ROOT_3 * u
                xe = 0
            end if

            in_xrange = (xe < 0)
            if (.not. in_xrange) P(m,m) = xf
            if (m >= Nmax) cycle

            xf1 = sqrt(2.0_real64*m + 3.0_real64) * t * xf
            xe1 = xe
            if (.not. in_xrange) P(m+1,m) = xf1

            xf2 = xf   ;   xe2 = xe

            do n = m+2, Nmax
                a = sqrt(((2.0_real64*n - 1.0_real64)*(2.0_real64*n + 1.0_real64)) &
                    / (real(n-m, real64) * real(n+m, real64)))
                b_ = sqrt(((2.0_real64*n + 1.0_real64) * real(n+m-1, real64) &
                    * real(n-m-1, real64)) &
                    / ((2.0_real64*n - 3.0_real64) * real(n+m, real64) &
                    * real(n-m, real64)))

                if (in_xrange) then
                    if (xe2 == xe1) then
                        xf = a*t*xf1 - b_*xf2
                    else if (xe2 < xe1) then
                        xf = a*t*xf1 - b_*(xf2 * BINV**real(xe1-xe2, real64))
                    else
                        xf = a*t*(xf1 * BINV**real(xe2-xe1, real64)) - b_*xf2
                    end if
                    xe = xe1

                    if (abs(xf) > B) then
                        xf = xf * BINV   ;   xe = xe + 1
                    else if (abs(xf) < BINV .and. xf /= 0.0_real64) then
                        xf = xf * B      ;   xe = xe - 1
                    end if

                    if (xe >= 0) then
                        in_xrange = .false.
                        P(n,m) = xf
                        if (xe1 >= 0) then
                            P(n-1,m) = xf1
                        else
                            P(n-1,m) = xf1 * B**real(-xe1, real64)
                        end if
                    end if

                else
                    P(n,m) = a*t*P(n-1,m) - b_*P(n-2,m)
                end if

                xf2 = xf1   ;   xe2 = xe1
                xf1 = xf    ;   xe1 = xe
            end do

            in_xrange = .true.
        end do

    end subroutine MFC_ALF_FUKU

end module legendre_functions

program physical_geodesy
    use iso_fortran_env
    use legendre_functions
    implicit none

    integer(int32) :: n, m, Nmax = 5
    real(real64) :: lat, ini, fin
    real(real64), allocatable :: P(:,:)

    lat = 0.0_real64
    call cpu_time(ini)
    call MFC_ALF_FUKU(Nmax, lat, P)
    call cpu_time(fin)
    do m = 0, Nmax
        do n = m, Nmax
            write(*,'("P(",I0,",",I0,") = ",ES20.10)') n, m, P(n,m)
        end do
    end do
    write(*,'("Tiempo Final de Calculo",F10.2)') fin - ini
end program physical_geodesy