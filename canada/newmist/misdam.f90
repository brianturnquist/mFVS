!=============================!
!   canada/newmist : misdam   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine MISDAM(ITREE,ICODES)
  implicit none

  !  **MISDAM--MS  Date of last revision:  07/15/94

  !  Purpose:
  !     Processes damage codes to determine whether the tree in
  !  question has dwarf mistletoe. The codes are as follows:
  !     30 Dwarf Mistletoe
  !     31 Lodgepole Pine Mistletoe
  !     32 Western Larch Mistletoe
  !     33 Douglas Fir Mistletoe
  !     34 Ponderosa Pine Mistletoe
  !  Code 30 is recorded if mistletoe is found on a species other
  !  than LP, WL, DF or PP.

  !  Call list definitions:
  !     ICODES: (I) Array of mistletoe damage codes.
  !     ITREE:  (I) Current tree record number.

  !  Local variable definitions:
  !     DEBUG:  Logical flag to turn debug on or off.
  !     J:      Loop counter.

  !  Common block variables and parameters:
  !     DMRATE: From DMCOM;
  !     ICYC:   From CONTRL; cycle index number.
  !     IMIST:  From MISCOM; tree mistletoe rating (Hawksworth 0-6).
  !     JOSTND: From CONTRL; unit number of stand output.
  !     MISFLG: From MISCOM; logical flag to turn DM effects on or off.

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'MISCOM.f90'
  include 'DMCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: ITREE,J
  integer :: ICODES(6)

  !.... Data statements.

  !.... Check for debug.

  call DBCHK(DEBUG,'MISDAM',6,ICYC)

  !.... Initializations.

  IMIST(ITREE) = 0

  !.... Check mistletoe processing option; if MISFLG is FALSE then
  !.... process without the effects of dwarf mistletoe (skip out of
  !.... this routine).

  if (.not.MISFLG) go to 9000

  !.... Process damage codes.

  do J = 1,5,2
     if (ICODES(J).eq.30.or.ICODES(J).eq.31.or.ICODES(J).eq.32.&
          &OR.ICODES(J).eq.33.or.ICODES(J).eq.34) then
        IMIST(ITREE) = ICODES(J+1)
        if (IMIST(ITREE).gt.6) IMIST(ITREE) = 6
        if (IMIST(ITREE).lt.0) IMIST(ITREE) = 0

        !           Copy value of IMIST to new S+I variable

        DMRATE(ITREE) = IMIST(ITREE)

     end if
     100 continue
  end do

  if (DEBUG) write(JOSTND,9010)ICYC,ITREE,IMIST(ITREE)
  9010 format(' MISDAM: Cycle = ',I5,'  IMIST(',I4,')= ',I2)

  !.... Common return.

  9000 continue

  return
end
