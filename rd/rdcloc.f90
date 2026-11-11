!=================!
!   rd : rdcloc   !
!=================!
! RD $Id$
subroutine RDCLOC
  implicit none

  !  THIS ROUTINE LOCATES THE ROOT DISEASE CENTERS IN THE STAND
  !  INSURING THAT NONE OF THE CENTERS OVERLAP.

  !  CALLED BY :
  !     RDSETP  [ROOT DISEASE]

  !  CALLS     :
  !     RDRANN  (FUNCTION)  [ROOT DISEASE]

  !  Revision History :
  !   02/01/94 - Last revision date.
  !   08/27/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'RDADD.f90'

  ! Variables
  integer :: II,JJ,NTRY
  real    :: AX1(100),AX2(100),AY1(100),AY2(100)
  real    :: RDRANN

  data AX1, AX2, AY1, AY2 /400*0.0/

  DIMEN = SQRT(SAREA) * 208.7

  !     BEGIN TO LOCATE THE CENTERS RANDOMLY. SET THE COUNTER NTRY.
  !     IF TOO MANY TRIES ARE REQUIRED TO LOCATE A CENTER SO THAT IT
  !     DOES NOT OVERLAP ANY PREVIOUS CENTERS THEN WE ASSUME THAT
  !     CENTERS ARE SO NUMMEROUS THAT NON-OVERLAPPING CENTERS IS NOT
  !     ATTAINABLE.

  !     IF THE STAND IS TO BE RUN AS ONE CENTER THEN MAKE SURE THAT STAND
  !     AREA IS COMPLETELY COVERED BY THE CENTER. ASSUME THAT THE RADIUS
  !     OF THE CENTER IS EQUAL TO HALF THE DIAGONAL OF THE STAND (USE
  !     PYTHAGORAS: R^2 = (.5 * DIMEN)^2 + (.5 * DIMEN)^2). ALSO ASSUME
  !     THAT THE CENTER OF THE CENTER IS IN THE CENTER OF THE STAND.

  !      IF (LONECT(IRRSP) .EQ. 1) THEN
  !         PCENTS(IRRSP,1,3) = 0.5 * DIMEN * SQRT(2.0)
  !         PCENTS(IRRSP,1,1) = 0.5 * DIMEN
  !         PCENTS(IRRSP,1,2) = 0.5 * DIMEN
  !         GOTO 200
  !      ENDIF

  !     SINCE USING THE DIAGONAL DOESN'T SEEM TO WORK, TRY SETTING THE RADIUS TO BE
  !     THE SAME AS ONE SIDE OF THE SQUARE STAND

  if (LONECT(IRRSP) .eq.1) then
     PCENTS(IRRSP,1,3) = DIMEN
     PCENTS(IRRSP,1,1) = 0.5 * DIMEN
     PCENTS(IRRSP,1,2) = 0.5 * DIMEN
     goto 200
  end if

  do II = 1,NCENTS(IRRSP)
     NTRY = 0
     PCENTS(IRRSP,II,3) = SQRT(PAREA(IRRSP) /&
          &(3.14159 * NCENTS(IRRSP))) * 208.7

     101 continue
     NTRY = NTRY + 1
     if (NTRY .gt.20) goto 100
     PCENTS(IRRSP,II,1) = RDRANN(0) * DIMEN
     PCENTS(IRRSP,II,2) = RDRANN(0) * DIMEN

     !        CALCULATE MAXIMUN EXTENT OF DISEASE CIRCLE IN X AND Y
     !        DIRECTIONS.

     AX2(II) = PCENTS(IRRSP,II,1) + PCENTS(IRRSP,II,3)
     AX1(II) = PCENTS(IRRSP,II,1) - PCENTS(IRRSP,II,3)
     AY2(II) = PCENTS(IRRSP,II,2) + PCENTS(IRRSP,II,3)
     AY1(II) = PCENTS(IRRSP,II,2) - PCENTS(IRRSP,II,3)

     !        LOOP THROUGH ALL PREVIOUS CENTER LOCATIONS TO SEE IF THIS
     !        CENTER OVERLAPS ANY OF THEM. IF ANY OVERLAP RETURN TO CHOOSE
     !        NEW LOCATION FOR CURRENT CENTER.

     if (II .eq.1) goto 100

     do JJ = 1, II-1
        if ((PCENTS(IRRSP,II,1) .lt. AX2(JJ) .and.&
             &PCENTS(IRRSP,II,1) .gt.&
             &AX1(JJ)) .or. (PCENTS(IRRSP,II,2) .lt. AY2(JJ) .and.&
             &PCENTS(IRRSP,II,2) .gt. AY1(JJ))) goto 101
        110 continue
     end do
     100 continue
  end do
  200 continue

  return
end
