!=====================!
!   mistoe : misdam   !
!=====================!
subroutine MISDAM(ITREE,ICODES)

  ! MISTOE

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
  !     ICYC:   From CONTRL; cycle index number.
  !     IMIST:  From MISCOM; tree mistletoe rating (Hawksworth 0-6).
  !     JOSTND: From CONTRL; unit number of stand output.
  !     MISFLG: From MISCOM; logical flag to turn DM effects on or off.

  implicit none

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'MISCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: ICODES(6),ITREE,J

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
     if ((ICODES(J).ge.30.and. ICODES(J).le.34)) then
        IMIST(ITREE) = ICODES(J+1)
        if (IMIST(ITREE).gt.6) IMIST(ITREE) = 6
        if (IMIST(ITREE).lt.0) IMIST(ITREE) = 0

        !....       PROCESS FIA DWARF MISTLETO DAMAGE CODES.
        !....       WHEN FIA CODES FOR ARCEUTHOBIUM ARE FOUND, THE
        !....       SEVERITY CODES 1 - 6 WILL BE RETAINED. OTHERWISE,
        !....       SEVERITY CODE 3 IS ASSIGNED.
        !....
        !....    These codes are from the FIA website document 8/25/2023
        !....    FIA Database Description and User Guide for Phase 2 (version 9.0.1)
        !....    pages H-29 and H-30
        !....
     else if (ICODES(J).eq.23005.or. ICODES(J).eq.23006&
          &.or. ICODES(J).eq.23007.or. ICODES(J).eq.23008&
          &.or. ICODES(J).eq.23009.or. ICODES(J).eq.23010&
          &.or. ICODES(J).eq.23011.or. ICODES(J).eq.23012&
          &.or. ICODES(J).eq.23013.or. ICODES(J).eq.23014&
          &.or. ICODES(J).eq.23015.or. ICODES(J).eq.23016&
          &.or. ICODES(J).eq.23017.or. ICODES(J).eq.23021&
          &.or. ICODES(J).eq.23023.or. ICODES(J).eq.23024) then

        if (ICODES(J+1).ge.1.and. ICODES(J+1).le.6) then
           IMIST(ITREE) = ICODES(J+1)
        else
           IMIST(ITREE) = 3
        end if
     end if
     100 continue
  end do

  if (DEBUG) write(JOSTND,9010)ICYC,ITREE,IMIST(ITREE)
  9010 format(' MISDAM: Cycle = ',I5,'  IMIST(',I4,')= ',I2)

  !.... Common return.

  9000 continue

  return
end
