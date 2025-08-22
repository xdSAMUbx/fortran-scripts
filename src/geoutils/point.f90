
module mod_point
   use iso_fortran_env
   implicit none
   public :: point, iPoint2d,rPoint2d, iPoint3d, rPoint3d, &
      iPoint4d, rPoint4d
!------------------------------------------
!------------- Definición de tipos --------
!------------------------------------------
   type :: iPoint2d
   integer(int32) :: x, y
   end type iPoint2d

   type, extends(iPoint2d) :: iPoint3d
   integer(int32) :: z
   end type iPoint3d

   type, extends(iPoint3d) :: iPoint4d
   integer(int32) :: t
   end type iPoint4d

   type :: rPoint2d
   real(real64) :: x, y
   end type rPoint2d

   type, extends(rPoint2d) :: rPoint3d
   real(real64) :: z
   end type rPoint3d

   type, extends(rPoint3d) :: rPoint4d
   real(real64) :: t
   end type rPoint4d


!------------------------------------------
!------------- Interfaz pública -----------
!------------------------------------------
   interface point
      module procedure iPoint2ds
      module procedure iPoint2darr
      module procedure iPoint3ds
      module procedure iPoint3darr
      module procedure iPoint4ds
      module procedure iPoint4darr
      module procedure rPoint2ds
      module procedure rPoint2darr
      module procedure rPoint3ds
      module procedure rPoint3darr
      module procedure rPoint4ds
      module procedure rPoint4darr
   end interface point

   contains

!------------------------------------------
!------------- Constructores --------------
!------------------------------------------
! integer part
   pure function iPoint2ds(x, y) result(res)
   implicit none
   integer(int32), intent(in) :: x, y
   type(iPoint2d) :: res

   res = iPoint2d(x, y)
   end function iPoint2ds

   pure function iPoint2darr(x, y) result(res)
   implicit none
   integer(int32), intent(in) :: x(:), y(:)
   type(iPoint2d), allocatable :: res(:)
   integer :: i

   allocate(res(size(x)))
   do concurrent (i=1:size(x))
      res(i) = iPoint2d(x(i), y(i))
   end do
   end function iPoint2darr

   pure function iPoint3ds(x, y, z) result(res)
   implicit none
   integer(int32), intent(in) :: x, y, z
   type(iPoint3d) :: res

   res = iPoint3d(x, y, z)
   end function iPoint3ds

   pure function iPoint3darr(x, y, z) result(res)
   implicit none
   integer(int32), intent(in) :: x(:), y(:), z(:)
   type(iPoint3d), allocatable :: res(:)
   integer :: i

   allocate(res(size(x)))
   do concurrent (i=1:size(x))
      res(i) = iPoint3d(x(i), y(i), z(i))
   end do
   end function iPoint3darr

   pure function iPoint4ds(x, y, z, t) result(res)
   implicit none
   integer(int32), intent(in) :: x, y, z, t
   type(iPoint4d) :: res

   res = iPoint4d(x, y, z, t)
   end function iPoint4ds

   pure function iPoint4darr(x, y, z, t) result(res)
   implicit none
   integer(int32), intent(in) :: x(:), y(:), z(:), t(:)
   type(iPoint4d), allocatable :: res(:)
   integer :: i

   allocate(res(size(x)))
   do concurrent (i=1:size(x))
      res(i) = iPoint4d(x(i), y(i), z(i), t(i))
   end do
   end function iPoint4darr

! real part
   pure function rPoint2ds(x, y) result(res)
   implicit none
   real(real64), intent(in) :: x, y
   type(rPoint2d) :: res

   res = rPoint2d(x, y)
   end function rPoint2ds

   pure function rPoint2darr(x, y) result(res)
   implicit none
   real(real64), intent(in) :: x(:), y(:)
   type(rPoint2d), allocatable :: res(:)
   integer :: i

   allocate(res(size(x)))
   do concurrent (i=1:size(x))
      res(i) = rPoint2d(x(i), y(i))
   end do
   end function rPoint2darr

   pure function rPoint3ds(x, y, z) result(res)
   implicit none
   real(real64), intent(in) :: x, y, z
   type(rPoint3d) :: res

   res = rPoint3d(x, y, z)
   end function rPoint3ds

   pure function rPoint3darr(x, y, z) result(res)
   implicit none
   real(real64), intent(in) :: x(:), y(:), z(:)
   type(rPoint3d), allocatable :: res(:)
   integer :: i

   allocate(res(size(x)))
   do concurrent (i=1:size(x))
      res(i) = rPoint3d(x(i), y(i), z(i))
   end do
   end function rPoint3darr

   pure function rPoint4ds(x, y, z, t) result(res)
   implicit none
   real(real64), intent(in) :: x, y, z, t
   type(rPoint4d) :: res

   res = rPoint4d(x, y, z, t)
   end function rPoint4ds

   pure function rPoint4darr(x, y, z, t) result(res)
   implicit none
   real(real64), intent(in) :: x(:), y(:), z(:), t(:)
   type(rPoint4d), allocatable :: res(:)
   integer :: i

   allocate(res(size(x)))
   do concurrent (i=1:size(x))
      res(i) = rPoint4d(x(i), y(i), z(i), t(i))
   end do
   end function rPoint4darr


end module mod_point
