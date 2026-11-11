!=====================!
!   dbs : dbsfmlink   !
!=====================!
! DBS $Id$
subroutine DBSFMLINK(I)
  implicit none

  !     PURPOSE: TO PASS SOME INFORMATION FROM FFE TO THE DATABASE
  !              EXTENSION, WITHOUT INCLUDING DBSCOM.F77 IN FFE SUBROUTINES.
  !              THIS SEEMS NECESSARY IN ORDER TO COMPILE FFE FOR BOTH PC AND UNIX.
  !     AUTH: S. REBAIN -- FMSC -- JUNE 2006

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: I

  ICANPR = I

end
