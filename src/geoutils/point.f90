
module mod_point
   use iso_fortran_env
   implicit none
   !-----------------------------------------------------------------
   ! ---------------------- integer datatype ----------------------
   !-----------------------------------------------------------------

   !> iPoint2d: It let you create a data type for
   !> integer and contains the iso fortran type int64
   type :: iPoint2d
   integer(int64) :: x, y
   end type iPoint2d

   !> iPoint2d: It let you create a data type for
   !> integer and contains the iso fortran type int64
   type, extends (iPoint2d) :: iPoint3d
   integer(int64) :: z
   end type iPoint3d

   !> iPoint3d: It let you create a data type for
   !> integer and contains the iso fortran type int64
   type, extends (iPoint3d) :: iPoint4d
   integer(int64) :: t
   end type iPoint4d

   !-----------------------------------------------------------------
   ! ---------------------- real datatype ----------------------
   !-----------------------------------------------------------------

   !> rPoint2d: It let you create a data type for
   !> real and contains the iso fortran type real128
   type :: rPoint2d
   real(real128) :: x, y
   end type rPoint2d

   !> rPoint2d: It let you create a data type for
   !> real and contains the iso fortran type real128
   type, extends (rPoint2d) :: rPoint3d
   real(real128) :: z
   end type rPoint3d

   !> rPoint3d: It let you create a data type for
   !> real and contains the iso fortran type real128
   type, extends (rPoint3d) :: rPoint4d
   real(real128) :: t
   end type rPoint4d

   contains
   !> iPoint2ds: Let you create a scalar datatype for
   !> a Point2d derived type
   function iPoint2ds result(res)
   end function iPoint2ds

   !> iPoint2darr: Let you create a array datatype for
   !> a Point2d derived type
   function iPoint2darr result(res)
   end function iPoint2darr

   !> iPoint3ds: Let you create a scalar datatype for
   !> a Point3d derived type
   function iPoint3ds result(res)
   end function iPoint3ds

   !> iPoint3darr: Let you create a array datatype for
   !> a Point3d derived type
   function iPoint3darr result(res)
   end function iPoint3darr

   !> iPoint4ds: Let you create a scalar datatype for
   !> a Point4d derived type
   function iPoint4ds result(res)
   end function iPoint4ds

   !> iPoint4darr: Let you create a array datatype for
   !> a Point4d derived type
   function iPoint4darr result(res)
   end function iPoint4darr

   !> rPoint2ds: Let you create a scalar datatype for
   !> a Point2d derived type
   function rPoint2ds result(res)
   end function rPoint2ds

   !> rPoint2darr: Let you create a array datatype for
   !> a Point2d derived type
   function rPoint2darr result(res)
   end function rPoint2darr

   !> rPoint3ds: Let you create a scalar datatype for
   !> a Point3d derived type
   function rPoint3ds result(res)
   end function rPoint3ds

   !> rPoint3darr: Let you create a array datatype for
   !> a Point3d derived type
   function rPoint3darr result(res)
   end function rPoint3darr

   !> rPoint4ds: Let you create a scalar datatype for
   !> a Point4d derived type
   function rPoint4ds result(res)
   end function rPoint4ds

   !> rPoint4darr: Let you create a array datatype for
   !> a Point4d derived type
   function rPoint4darr result(res)
   end function rPoint4darr

end module mod_point
