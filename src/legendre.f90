module legendre_functions
    use iso_fortran_env
    implicit none
 
    real(real64), parameter :: PI     = 3.141592653589793_real64
    real(real64), parameter :: ROOT_3 = 1.7320508075688772_real64
    real(real64), parameter :: B    = 1.0e+280_real64
    real(real64), parameter :: BINV = 1.0e-280_real64
 
contains
 
    subroutine radians(val1)
        real(real64), intent(inout) :: val1
        val1 = val1 * PI / 180.0_real64
    end subroutine radians
 
    ! P_bar_{n,0}(cos theta), theta en grados
    subroutine ALF_ZONAL(Nmax, teta, P)
        integer(int32), intent(in)               :: Nmax
        real(real64),   intent(in)               :: teta
        real(real64),   allocatable, intent(out) :: P(:)
        integer(int32) :: n
        real(real64)   :: x, rn, a, b_
        allocate(P(0:Nmax))
        P = 0.0_real64
        x    = cos(teta * PI / 180.0_real64)
        P(0) = 1.0_real64
        if (Nmax < 1) return
        P(1) = ROOT_3 * x
        do n = 2, Nmax
            rn = real(n, real64)
            a  = sqrt((2.0_real64*rn - 1.0_real64) * (2.0_real64*rn + 1.0_real64)) / rn
            b_ = ((rn - 1.0_real64) / rn) &
                 * sqrt((2.0_real64*rn + 1.0_real64) / (2.0_real64*rn - 3.0_real64))
            P(n) = a * x * P(n-1) - b_ * P(n-2)
        end do
    end subroutine ALF_ZONAL
 
    ! Devuelve P(n,m) = xf * B^xe  para todo n,m = 0..Nmax
    subroutine MFC_ALF_FUKU_EXT(Nmax, teta, XF, XE)
        integer(int32), intent(in)                :: Nmax
        real(real64),   intent(in)                :: teta
        real(real64),   allocatable, intent(out)  :: XF(:,:)
        integer(int32), allocatable, intent(out)  :: XE(:,:)
 
        integer(int32) :: n, m, k
        real(real64)   :: t, u, a, b_, sfactor
        real(real64)   :: xf0, xf1, xf2
        integer(int32) :: xe0, xe1, xe2
 
        u = sin(teta * PI / 180.0_real64)
        t = cos(teta * PI / 180.0_real64)
 
        allocate(XF(0:Nmax, 0:Nmax), XE(0:Nmax, 0:Nmax))
        XF = 0.0_real64 ;  XE = 0
 
        ! --- Columna m=0 (zonales, nunca desbordan) ---
        XF(0,0) = 1.0_real64
        if (Nmax >= 1) XF(1,0) = ROOT_3 * t
        do n = 2, Nmax
            a  = sqrt(((2.0_real64*n-1.0_real64)*(2.0_real64*n+1.0_real64)) &
                       / real(n*n, real64))
            b_ = ((real(n,real64)-1.0_real64)/real(n,real64)) &
                 * sqrt((2.0_real64*n+1.0_real64)/(2.0_real64*n-3.0_real64))
            XF(n,0) = a*t*XF(n-1,0) - b_*XF(n-2,0)
        end do
 
        if (Nmax < 1) return
 
        ! --- Semilla: P_bar_{1,1} = sqrt(3)*sin(theta) ---
        xf0 = ROOT_3 * u ;  xe0 = 0
        call xrescale(xf0, xe0)
        XF(1,1) = xf0 ;  XE(1,1) = xe0
 
        do m = 1, Nmax
            ! Recuperar diagonal P_bar_{m,m}
            xf0 = XF(m,m) ;  xe0 = XE(m,m)
 
            ! Calcular siguiente diagonal P_bar_{m+1,m+1}
            if (m < Nmax) then
                xf1 = sqrt((2.0_real64*(m+1)+1.0_real64)/(2.0_real64*(m+1))) * u * xf0
                xe1 = xe0
                call xrescale(xf1, xe1)
                XF(m+1,m+1) = xf1 ;  XE(m+1,m+1) = xe1
            end if
 
            if (m >= Nmax) cycle
 
            ! P_bar_{m+1,m}
            xf1 = sqrt(2.0_real64*m + 3.0_real64) * t * xf0
            xe1 = xe0
            call xrescale(xf1, xe1)
            XF(m+1,m) = xf1 ;  XE(m+1,m) = xe1
 
            if (m+1 >= Nmax) cycle
 
            xf2 = xf0 ;  xe2 = xe0   ! P_bar_{m,m}
 
            ! Recurrencia n = m+2 .. Nmax
            do n = m+2, Nmax
                a  = sqrt(((2.0_real64*n-1.0_real64)*(2.0_real64*n+1.0_real64)) &
                           / (real(n-m,real64)*real(n+m,real64)))
                b_ = sqrt(((2.0_real64*n+1.0_real64)*real(n+m-1,real64) &
                           *real(n-m-1,real64)) &
                           / ((2.0_real64*n-3.0_real64)*real(n+m,real64) &
                           *real(n-m,real64)))
 
                ! xf0*B^xe0 = a*t*xf1*B^xe1 - b_*xf2*B^xe2
                ! Sacamos B^max(xe1,xe2):
                if (xe1 >= xe2) then
                    xe0 = xe1
                    sfactor = 1.0_real64
                    do k = 1, xe1 - xe2
                        sfactor = sfactor * BINV
                        if (sfactor == 0.0_real64) exit
                    end do
                    xf0 = a*t*xf1 - b_*xf2*sfactor
                else
                    xe0 = xe2
                    sfactor = 1.0_real64
                    do k = 1, xe2 - xe1
                        sfactor = sfactor * BINV
                        if (sfactor == 0.0_real64) exit
                    end do
                    xf0 = a*t*xf1*sfactor - b_*xf2
                end if
 
                call xrescale(xf0, xe0)
                XF(n,m) = xf0 ;  XE(n,m) = xe0
 
                xf2 = xf1 ;  xe2 = xe1
                xf1 = xf0 ;  xe1 = xe0
            end do
 
        end do
 
    end subroutine MFC_ALF_FUKU_EXT
 
    ! MFC_ALF_FUKU: versión original — devuelve double normal.
    ! P(n,m) = P_bar_{n,m}  cuando cabe en double (xe==0), 0.0 otherwise.
    ! Para sintesis de alto grado usar MFC_ALF_FUKU_EXT directamente.
    subroutine MFC_ALF_FUKU(Nmax, teta, P)
        integer(int32), intent(in)               :: Nmax
        real(real64),   intent(in)               :: teta
        real(real64),   allocatable, intent(out) :: P(:,:)
        real(real64),   allocatable :: XF(:,:)
        integer(int32), allocatable :: XE(:,:)
        integer(int32) :: n, m
        call MFC_ALF_FUKU_EXT(Nmax, teta, XF, XE)
        allocate(P(0:Nmax, 0:Nmax))
        P = 0.0_real64
        do m = 0, Nmax
            do n = m, Nmax
                if (XE(n,m) == 0) P(n,m) = XF(n,m)
            end do
        end do
        deallocate(XF, XE)
    end subroutine MFC_ALF_FUKU
 
    subroutine xrescale(xf, xe)
        real(real64),   intent(inout) :: xf
        integer(int32), intent(inout) :: xe
        do while (abs(xf) > B .and. xf /= 0.0_real64)
            xf = xf * BINV ;  xe = xe + 1
        end do
        do while (abs(xf) < BINV .and. xf /= 0.0_real64)
            xf = xf * B    ;  xe = xe - 1
        end do
    end subroutine xrescale
 
end module legendre_functions