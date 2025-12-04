module algorithms
    use iso_fortran_env
    implicit none
contains
    ! ==============================================================
    ! Descripción: La subrutina swap permite cambiar entre palabras
    !   si la palabra debe ir por encima de la anterior entonces 
    !   cambia str1 (string 1) por str2 (string 2), entonces
    !   si se utiliza el array ["Zanahoria", "Banana"] deberá
    !   de devolver ["Banana", "Zanahoria"]
    ! Parametros:
    !   - str1: Cadena de caracteres, idealmente es la palabra
    !       actual para comparar
    !   - str2: Cadena de caracteres, idealmente es la palabra
    !       siguiente con la cual se quiere comparar
    ! ===============================================================
    subroutine swap(str1, str2)
        implicit none
        character(len=*), intent(inout) :: str1, str2
        character(len=len(str1)) :: temp
        
        temp = str1
        str1 = str2
        str2 = temp
        
    end subroutine swap

    subroutine particion(arr)
        character(len=*), allocatable, intent(inout) :: arr(:)
        character(len=len(arr)) :: pivot
        integer :: i

        pivot = arr(1)
        do i = 1, size(arr)
            if ( llt(arr(i),pivot) ) then
                call swap(arr(i), pivot)
            end if
        end do

        do i = 1, size(arr)
            write(*,*) arr(i)
        end do
    end subroutine particion
end module algorithms

module pruebas
    implicit none
contains
    ! Esta subrutina genera la matriz A
    subroutine gen_A_matrix(ini,fin,mtx_A)
        implicit none
        ! =======================================================================
        ! La función gen_A_matrix utiliza la metodología de matriz de incidencia
        ! utilizada en la teoria de grafos para generar la matriz de diseño que
        ! permita describir el sentido de la red de nivelación a ajustar.
        ! Parametros:
        !   - ini: Vector de strings que contiene la nomenclatura de los vértices
        !           de donde inicia.
        !   - fin: Vector de strings que contiene la nomenclatura de los vértices
        !           de donde finaliza
        ! Salida:
        !   - mtx_A: Matriz de diseño A sin condicionamiento.
        ! =======================================================================
        character(len=*), intent(in) :: ini(:), fin(:)
        real(kind=4), allocatable, intent(out) :: mtx_A(:,:)
        real(kind=4) :: n,m ! Número de observaciones y número de vértices
        n = size(ini)
        m = size(fin)
        allocate(mtx_A(n,m))
        mtx_A = 0
        write(*,*) mtx_A
    end subroutine gen_A_matrix
end module pruebas

program generador_matriz_A
    use iso_fortran_env
    use algorithms
    use pruebas
    implicit none
    character(len=10), allocatable :: v(:)
    allocate(v(4))
    v = [character(len=10) :: "A15-TBM-1","15-BM-1","BM-1","BM-2"]
    call particion(v)
end program generador_matriz_A