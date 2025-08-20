module mod_eye
    use iso_fortran_env
    implicit none
    interface eye
    module procedure eyeInt, eyeReal
    end interface 
    contains
    function eyeInt(n) result(mat)
        integer(int32) :: i
        integer(int32), intent(in) :: n
        integer(int32), allocatable :: mat(:,:)
        allocate(mat(n,n))
        mat = 0
        do concurrent (i=1:n)
            mat(i,i) = 1
        end do
    end function eyeInt

    function eyeReal(n) result(mat)
        integer(int32) :: i
        integer(int32), intent(in) :: n
        real(real64), allocatable :: mat(:,:)
        allocate(mat(n,n))
        mat = 0
        do concurrent (i=1:n)
            mat(i,i) = 1.0
        end do
    end function eyeReal
end module mod_eye