#:include 'checks.fypp'

module testmod
   implicit none

   contains

   subroutine someFunction(ind, uplo)
      integer, intent(in) :: ind
      character, intent(in) :: uplo

      @:ASSERT(ind > 0, msg="Index must be positive")
      @:ASSERT(uplo == 'U' .or. uplo == 'L')

      ! Do something useful here
      ! :

#:block DEBUG_CODE
      print *, 'We are in debug mode'
      print *, 'The value of ind is', ind
#:endblock DEBUG_CODE

   end subroutine someFunction

end module testmod
