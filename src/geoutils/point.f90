
module mod_point
   use iso_fortran_env
   implicit none
   type :: iPoint2d
      integer(int64) :: x, y
   end type iPoint2d

   type, extends(iPoint2d) :: iPoint3d
      integer(int64) :: z
   end type iPoint3d

   type, extends(iPoint3d) :: iPoint4d
      integer(int64) :: t
   end type iPoint4d

   type :: rPoint2d
      real(real128) :: x, y
   end type rPoint2d

   type, extends(rPoint2d) :: rPoint3d
      real(real128) :: z
   end type rPoint3d

   type, extends(rPoint3d) :: rPoint4d
      real(real128) :: t
   end type rPoint4d

   contains

   pure function iPoint2ds(x, y) result(res)
      implicit none

      integer(int64), intent(in) :: x, y
      type(iPoint2d) :: res

      res%x = x
      res%y = y
   end function iPoint2ds

   pure function iPoint2darr(x, y) result(res)
      implicit none

      integer(int64), intent(in) :: x(:), y(:)
      integer(int64), allocatable :: res
   end function iPoint2darr

   pure function iPoint3ds(x, y, z) result(res)
      implicit none

      integer(int64), intent(in) :: x, y, z
      type(iPoint3d)  :: res

      res%x = x
      res%y = y
      res%z = z
   end function iPoint3ds

   pure function iPoint3darr(x, y, z) result(res)
      implicit none

      integer(int64), intent(in) :: x(:), y(:), z(:)
      integer(int64), allocatable :: res
   end function iPoint3darr

   pure function iPoint4ds(x, y, z, t) result(res)
      implicit none

      integer(int64), intent(in) :: x, y, z, t
      type(iPoint4d)  :: res

      res%x = x
      res%y = y
      res%z = z
      res%t = t
   end function iPoint4ds

   pure function iPoint4darr(x, y, z, t) result(res)
      implicit none

      integer(int64), intent(in) :: x(:), y(:), z(:), t(:)
      integer(int64), allocatable :: res
   end function iPoint4darr

   pure function rPoint2ds(x, y) result(res)
      implicit none

      real(real128), intent(in) :: x, y
      type(rPoint2d) :: res

      res%x = x
      res%y = y
   end function rPoint2ds

   pure function rPoint2darr(x, y) result(res)
      implicit none

      real(real128), intent(in) :: x(:), y(:)
      real(real128), allocatable :: res
   end function rPoint2darr

   pure function rPoint3ds(x, y, z) result(res)
      implicit none

      real(real128), intent(in) :: x, y, z
      type(rPoint3d)  :: res

      res%x = x
      res%y = y
      res%z = z
   end function rPoint3ds

   pure function rPoint3darr(x, y, z) result(res)
      implicit none

      real(real128), intent(in) :: x(:), y(:), z(:)
      real(real128), allocatable :: res
   end function rPoint3darr

   pure function rPoint4ds(x, y, z, t) result(res)
      implicit none

      real(real128), intent(in) :: x, y, z, t
      type(rPoint4d)  :: res

      res%x = x
      res%y = y
      res%z = z
      res%t = t
   end function rPoint4ds

   pure function rPoint4darr(x, y, z, t) result(res)
      implicit none

      real(real128), intent(in) :: x(:), y(:), z(:), t(:)
      real(real128), allocatable :: res
   end function rPoint4darr

end module mod_point
