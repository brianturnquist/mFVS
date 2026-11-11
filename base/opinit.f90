!====================================!
!   Initialize activity scheduling   !
!====================================!
! BASE $Id$
subroutine OPINIT
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JUNE 1981 - MOSCOW

  !     OPINIT IS USED TO INITIALIZE OPTION PROCESSING POINTERS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'

  ! Variables
  integer :: I

  ISEQDN = 0
  IMGL = 1
  IMPL = 1
  ITOPRM = MAXPRM
  IEVA = 1
  ICOD = 1
  IEVT = 1
  ICACT = 1
  ILGNUM = 0
  ITST5 = 0
  IEPT = MAXACT
  LOPEVN = .false.
  do I = 1,MXTST4
     LTSTV4(I) = .false.
     10 continue
  end do
  LBSETS = .false.
  do I = 1,MAXEVA
     LENAGL(I) = -1
     20 continue
  end do
  LENSLS = -1
  return
end subroutine OPINIT
