!=======================================!
!   Compute regeneration tree heights   !
!=======================================!
! ACD $Id$
subroutine ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER,GENTIM, &
     & TRAGE,SI,LOSTND)
  implicit none

  ! Common blocks
  include 'PRGPRM.F77'
  include 'CONTRL.F77'
  include 'ESPARM.F77'
  include 'ESCOMN.F77'

  ! Variables
  real    :: MAPNE(108)
  real    :: SI,TRAGE,GENTIM,ELEV,DELAY,DILATE,EMSQR,HHT,CARAGE
  real    :: YRS,H,AGET,HTG1,HTMAX,AGE
  integer :: LOSTND,IHTSER,I,IAGE,MODE0,IVAR,N,ITIME
  logical :: DEBUG

  data MAPNE/ &
       & 20,   10,   15,   20,   15,   20,   20,   20,    5,   20, &
       & 15,   20,   20,   10,   20,   20,   20,   20,   20,   10, &
       & 10,   15,   20,   15,   10,   20,   20,   20,   20,   20, &
       & 20,   20,   20,   20,   20,   20,   20,   20,   20,   20, &
       & 20,   20,   20,   35,   35,   20,   10,   20,   20,   20, &
       & 10,   20,   15,   20,   10,   10,   10,   10,   10,   10, &
       & 10,   30,   10,   10,   30,   30,   20,   10,   10,   20, &
       & 10,   10,   10,   20,   10,   10,   10,   20,   20,   10, &
       & 25,   25,   10,   25,   25,   10,   10,   20,   10,   10, &
       & 10,   10,   20,   20,   20,   20,   20,   10,   10,   10, &
       & 10,   10,   10,   10,   10,   10,   10,   10/

  !  ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
  !  CREATED BY THE ESTABLISHMENT MODEL.

  !  COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
  !  THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER
  !  OF YEARS BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF
  !  THE CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
  !  THE TREE.

  !  FIRST CALL HTCALC TO GET THE HT AT THE LOWEST REFERENCE AGE, GIVEN A
  !  SITE INDEX FROM CARMEAN'S CURVES. THEN INTERPOLATE HT GIVEN THE AGE
  !  OF THE TREE ASSUMING A STRAIGHT LINE RELATIONSHIP THROUGH THE ORIGIN.

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'ESSUBH',6,ICYC)
  if (DEBUG) write(LOSTND,9980)ICYC
  9980 format(' ENTERING SUBROUTINE ESSUBH  CYCLE =',I5)
  IAGE = MAPNE(I)
  CARAGE = FLOAT(IAGE)
  MODE0 = 1
  IVAR = 3
  YRS = 0.
  H = 0.
  AGET = CARAGE
  call HTCALC (MODE0,IVAR,I,SI,YRS,H,AGET,HTMAX, &
       & HTG1,LOSTND,DEBUG)
  HHT = (H/CARAGE)*MIN(5.0,TIME-DELAY)
  if (DEBUG) write(LOSTND,*)' IN ESSUBH SI,CARAGE,TRAGE,H,HHT= ', &
       & SI,CARAGE,TRAGE,H,HHT

  if (DEBUG) write(LOSTND,*)' IN ESSUBH DELAY,TIME,GENTIM,TRAGE= ', &
       & DELAY,TIME,GENTIM,TRAGE
  N = DELAY+0.5
  if (N.lt.-3) N = -3
  DELAY = FLOAT(N)
  ITIME = TIME+0.5
  if (N.gt.ITIME) DELAY = TIME
  AGE = TIME-DELAY-GENTIM+TRAGE
  if (AGE.lt.1.0) AGE = 1.0
  TRAGE = TIME-DELAY
  if (DEBUG)write(LOSTND,*)' LEAVING ESSUBH AGE,TRAGE= ',AGE,TRAGE

  return
end subroutine ESSUBH
