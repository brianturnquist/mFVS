!=============================================!
!   Retrieve scheduled activity expressions   !
!=============================================!
! BASE $Id$
subroutine OPGET3 (IACTK,IDT,IYR1,IYR2,ISQNUM,MXPM,NPRMS, &
     & PRMS,KODE)
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JUNE 1981 - MOSCOW

  !     OPGET3 IS MUCH LIKE OPGET2 EXCEPT THAT THIS ROUTINE RETURNS
  !     INFORMATION ABOUT ACTIVITIES WHICH HAVE ALREADY BEEN ACOMPLISHED.
  !     THE SEARCH ITSELF IS DONE BY OPGET23 (IN OPGET2.F90), WHICH IS
  !     SHARED WITH OPGET2; SEE THERE FOR THE ARGUMENTS.

  ! Variables
  integer :: KODE,NPRMS,ISQNUM,IYR1,IYR2,IDT,IACTK,MXPM
  real    :: PRMS(MXPM)

  call OPGET23 (.true.,IACTK,IDT,IYR1,IYR2,ISQNUM,MXPM,NPRMS, &
       & PRMS,KODE)
  return
end subroutine OPGET3
