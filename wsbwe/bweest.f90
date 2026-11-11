!====================!
!   wsbwe : bweest   !
!====================!
! WSBWE $Id$
subroutine BWEEST
  implicit none

  !     SETS THE YEARS BW-CAUSED DEFOLIATION OCCURED IN IBWHST
  !     THEREBY PROVIDING THE LINKS BETWEEN THE BUDWORM MODEL AND
  !     THE ESTABLISHMENT MODEL.  CALLED BY BWCUPL.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JANUARY 1984

  !     minor changes by K.Sheehan 7/96 to remove IDBLVL

  !  Revision History:
  !    17-MAY-2005 Lance R. David (FHTET)
  !       Added FVS parameter file PRGPRM.F77.
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESWSBW.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'

  ! Variables
  integer :: I,IHERE,IHOST,ISZI
  real    :: XXX

  !     FIND THE MAXIMUM AMOUNT OF DEFOLIATON IN FOR ANY HOST, AND
  !     ANY TREE SIZE.

  IHERE = -1
  XXX = 1.0
  do IHOST = 1,6
     if (IFHOST(IHOST).ne.1) goto 20
     do ISZI = 1,3
        if (APRBYR(IHOST,ISZI,2,ICUMYR).lt.XXX)&
             &XXX = APRBYR(IHOST,ISZI,2,ICUMYR)
        10 continue
     end do
     20 continue
  end do

  !     IF THE DEFOLIATION IS GREATER THAN A CRITICAL VALUE, THEN THE
  !     YEAR IS "ADDED" TO THE LIST OF BUDWORM YEARS.

  if (XXX.lt..90) then

     !        CHECK OUT THE ENTRIES IN IBWHST.  IF THE LIST IS EMPTY,
     !        BRANCH TO ADD.

     if (NBWHST.le.0) then
        IHERE = 1
        NBWHST = 1
        goto 100
     end if

     !        IF THE YEAR IS ALREADY IN THE HISTORY, WERE DONE.

     IHERE = 0
     do I = 1,NBWHST
        if (IYRCUR.ge.IBWHST(1,I).and.IYRCUR.le.IBWHST(2,I)) goto 200
        30 continue
     end do

     !        SEE IF THE CURRENT BUDWORM YEAR CAN BE ADDED TO AN ENTRY.

     do I = 1,NBWHST
        if (IYRCUR-IBWHST(2,I).eq.1) then
           IHERE = I
           IBWHST(2,I) = IYRCUR
           goto 200
        end if
        40 continue
     end do

     !        IS THERE ROOM IN THE LIST, IF YES, BRANCH TO ADD ENTRY.

     if (NBWHST.lt.20) then
        NBWHST = NBWHST+1
        IHERE = NBWHST
        goto 100
     end if

     !        FIND THE OLDEST ENTRY (BASED ON THE "ENDING" DATE), AND
     !        ADD THE ENTRY AT THAT LOCATION.

     IHERE = 1
     do I = 2,NBWHST
        if (IBWHST(2,I).lt.IBWHST(2,IHERE)) IHERE = I
        70 continue
     end do
  end if

  !    Added work-arround to avoid array out of bounds when
  !     starting defoliation model in middle of cycle (RNH Dec 98)

  if (IHERE .eq. -1) go to 200

  !     ADD THE ENTRY AT LOCATION "IHERE"

  100 continue
  IBWHST(1,IHERE) = IYRCUR
  IBWHST(2,IHERE) = IYRCUR
  200 continue

  return
end
