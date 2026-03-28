module csv_reader
    use iso_fortran_env
    implicit none
    type:: csv_column
        character(len=:), allocatable :: name
        integer :: dtype

        integer(int32), allocatable :: ival(:)
        real(real64), allocatable :: rval(:)
        character(len=256), allocatable :: sval(:)
    end type csv_column

    type :: csv_table
        integer(int32) :: nrows
        integer(int32) :: ncols
        type(csv_column), allocatable :: cols(:)
    end type csv_table
contains
    subroutine read_csv(path, sep, header, table)
        character(len=*), intent(in) :: path
        character(len=1), intent(in) :: sep
        logical, intent(in) :: header
        type(csv_table), intent(out) :: table

        integer :: io, ios
        logical :: exists
        character(len=4096) :: buffer

        inquire(file=path, exist=exists)
        if (.not. exists) stop "Archivo no encontrado"
        open(newunit=io, file=path, status="old", action="read")
            do
                read(io, '(A)', iostat=ios) buffer
                if( ios /= 0) exit
                write(*,*) trim(buffer)
            end do
        close(io)
    end subroutine read_csv

end module csv_reader

program prueba_lectura
    use csv_reader
    use iso_fortran_env
    implicit none
    type(csv_table) :: t
    character(len=:), allocatable :: path

    path = "/home/xdsamubx/programacion/geodetic_adjust/data/CSV/EjercicioGomez1.csv"
    call read_csv(path,',',.true.,t)

end program prueba_lectura