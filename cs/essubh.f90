!=================================================!
!   Compute subsequent and planted tree heights   !
!=================================================!
! CS $Id$
subroutine ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER,GENTIM,&
     &TRAGE,SI,LOSTND)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'

  ! Variables
  integer :: MAPCS(96)
  real    :: SI,TRAGE,GENTIM,ELEV,DELAY,DILATE,EMSQR,HHT,CARAGE,YRS,H
  real    :: AGET,HTG1,HTMAX,AGE
  integer :: LOSTND,IHTSER,I,IAGE,IVAR,MODE0,N,ITIME
  logical :: DEBUG
  real    :: RDANUW
  integer :: IDANUW

  data MAPCS/&
       &10,   10,   10,   15,   20,   10,    5,   20,   20,   25,&
       &25,   25,   25,   20,   20,   20,   20,   20,   20,   20,&
       &20,   10,   20,   20,   20,   35,   20,   15,   20,   10,&
       &15,   20,   20,   20,   10,   20,   20,   20,   20,   20,&
       &20,   20,   20,   20,   20,   35,   10,   20,   10,   10,&
       &10,   10,   10,   30,   10,   30,   10,   10,   10,   10,&
       &20,   10,   35,   30,   10,   10,   20,   10,   10,   20,&
       &20,   10,   10,   20,   20,   20,   10,   20,   20,   20,&
       &20,   10,   10,   10,   10,   10,   10,   10,   10,   20,&
       &10,   20,   25,   20,   10,   10/

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

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  HTMAX = 0.0
  RDANUW = DILATE
  RDANUW = ELEV
  RDANUW = EMSQR
  IDANUW = IHTSER

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'ESSUBH',6,ICYC)
  if (DEBUG) write(LOSTND,9980)ICYC
  9980 format(' ENTERING SUBROUTINE ESSUBH  CYCLE =',I5)
  IAGE = MAPCS(I)
  CARAGE = FLOAT(IAGE)
  MODE0 = 1
  IVAR = 2
  YRS = 0.
  H = 0.
  AGET = CARAGE
  call HTCALC (MODE0,IVAR,I,SI,YRS,H,AGET,HTMAX,&
       &HTG1,LOSTND,DEBUG)
  HHT = (H/CARAGE)*MIN(5.0,TIME-DELAY)
  if (DEBUG) write(LOSTND,*)' IN ESSUBH SI,CARAGE,TRAGE,H,HHT= ',&
       &SI,CARAGE,TRAGE,H,HHT

  if (DEBUG) write(LOSTND,*)' IN ESSUBH DELAY,TIME,GENTIM,TRAGE= ',&
       &DELAY,TIME,GENTIM,TRAGE
  N = INT(DELAY+0.50)
  if (N.lt.-3) N = -3
  DELAY = FLOAT(N)
  ITIME = INT(TIME+0.5)
  if (N.gt.ITIME) DELAY = TIME
  AGE = TIME-DELAY-GENTIM+TRAGE
  if (AGE.lt.1.0) AGE = 1.0
  TRAGE = TIME-DELAY
  if (DEBUG)write(LOSTND,*)' LEAVING ESSUBH AGE,TRAGE= ',AGE,TRAGE

  return
end
