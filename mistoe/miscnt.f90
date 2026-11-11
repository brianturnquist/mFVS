!=====================!
!   mistoe : miscnt   !
!=====================!
! MISTOE $Id$
subroutine MISCNT(MSPCNT)

  !  Purpose:
  !     Counts the number of tree records with any mistletoe infection
  !  greater than 0 and returns the values by species in array MSPCNT.
  !  Note: MSPCNT species are ordered according to IREF, i.e. the order
  !  of occurance of species in the treelist.

  !  Call list definitions:
  !     MSPCNT: (O) Array of number of DM infected records by species.

  !  Local variable definitions:
  !     DEBUG:  Logical flag to turn debug on or off.
  !     I:      Loop counter.
  !     ISPTR:  Pointer to current tree record's species number in
  !                species ordered sort list.

  !  Common block variables and parameters:
  !     ICYC:   From CONTRL; cycle index number.
  !     IMIST:  From MISCOM; array containing tree record DMR's.
  !     IREF:   From CONTRL; index to order of occurance of species.
  !     ISP:    From ARRAYS; species code number.
  !     ITRN:   From CONTRL; current number of tree records.
  !     JOSTND: From CONTRL; unit number of stand output.
  !     MAXSP:  From PRGPRM; maximum number of species.

  implicit none

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'MISCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: I,ISPTR,MSPCNT(MAXSP)

  !.... Data statements.

  !.... Check for debug.

  call DBCHK(DEBUG,'MISCNT',6,ICYC)

  if (DEBUG) write(JOSTND,100) ICYC
  100 format(' Begin MISCNT: Cycle = ',I5)

  !.... Clear the MSPCNT array.

  do I = 1,MAXSP
     MSPCNT(I) = 0
     150 continue
  end do

  !.... Loop through the treelist counting mistletoe infected records.
  !.... Save counts by species according to IREF (ordered index based
  !.... on occurance of species in species ordered sort list).

  do I = 1,ITRN
     if (IMIST(I).gt.0) then
        ISPTR = IREF(ISP(I))
        MSPCNT(ISPTR) = MSPCNT(ISPTR)+1
     end if
     200 continue
  end do

  !.... Common return.

  if (DEBUG) write(JOSTND,9010) ICYC
  9010 format(' End MISCNT: Cycle = ',I5)

  return
end
