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
        if ( lgt(str1,str2) ) then
            temp = str1
            str1 = str2
            str2 = temp
        end if
    end subroutine swap

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
    character(len=10) :: str1, str2
    str1 = "Zanahoria"
    str2 = "Banana"
    call swap(str1,str2)
    write(*,*) str1, str2
end program generador_matriz_A