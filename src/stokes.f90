module mod_stokes
    use iso_fortran_env
    use legendre_functions
    implicit none
contains

    subroutine sph_dist(lata,latb,lona,lonb,dist)
        real(kind=8),intent(in) :: lata,latb,lona,lonb
        real(kind=8),intent(out) :: dist
        real(8) :: tmp,rlata,rlatb,rlona,rlonb
        rlata = lata; rlatb = latb; rlona = lona; rlonb = lonb

        call radians(rlata); call radians(rlatb)
        call radians(rlona); call radians(rlonb)

        tmp = cos(rlata) * cos(rlatb) + sin(rlata) * sin(rlatb) * cos(rlonb-rlona)

        dist = acos(tmp)
    end subroutine sph_dist
end module mod_stokes

program stokes_prueba
    use iso_fortran_env
    use mod_stokes
    implicit none
    real(8) :: lata, latb, lona, lonb, dist
    real(8), allocatable :: P(:,:)
    integer :: Nmax

    Nmax = 500
    lata = 4.7110_real64; lona = -74.0721_real64
    latb = 6.2476_real64; lonb = -75.5658_real64
    call sph_dist(lata,latb,lona,lonb,dist)
    write(*,'("PRUEBA DISTANCIA ESFÉRICA:",F20.10)') dist * 6378137.0_real64

    call MFC_ALF_FUKU(Nmax, 86.0_real64, P)
    write(*,'("P(",I0,",",I0,") = ",ES20.10)') Nmax, Nmax, P(Nmax,Nmax)

end program stokes_prueba