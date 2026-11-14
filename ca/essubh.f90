!=================================================!
!   Compute subsequent and planted tree heights   !
!=================================================!
! CA $Id$
subroutine ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER,GENTIM,&
     &TRAGE)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'

  !     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
  !     CREATED BY THE ESTABLISHMENT MODEL.

  !     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
  !     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER
  !     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE
  !     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
  !     THE TREE.

  ! Variables
  integer :: IHTSER,I,N,ITIME
  real    :: TRAGE,GENTIM,ELEV,DELAY,DILATE,EMSQR,HHT,AGE
  real    :: RDANUW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = DILATE
  RDANUW = ELEV
  RDANUW = EMSQR
  IDANUW = IHTSER

  N = INT(DELAY+0.5)
  if (N.lt.-3) N = -3
  DELAY = FLOAT(N)
  ITIME = INT(TIME+0.5)
  if (N.gt.ITIME) DELAY = TIME
  AGE = TIME-DELAY-GENTIM+TRAGE
  if (AGE.lt.1.0) AGE = 1.0
  TRAGE = TIME-DELAY
  go to (20, 20, 20, 20, 10, 10, 20, 20, 10, 30,&
       &30, 30, 30, 30, 20, 20, 20, 20, 20, 20,&
       &30, 20, 20, 40, 20, 50, 50, 50, 50, 40,&
       &40, 50, 50, 50, 50, 50, 50, 50, 40, 40,&
       &50, 50, 50, 50, 50, 50, 50, 50, 50, 20),I

  !     HEIGHT OF TALLEST SUBSEQUENT SPECIES 5:RF (6,9)

  10 continue
  HHT = 1.0
  go to 70

  !     HEIGHT OF TALLEST SUBS. SPECIES 7:DF (1-4,8,15-20,22-23,25,50)

  20 continue
  HHT = 2.0
  go to 70

  !     HT OF TALLEST SUBS. SPECIES 12:LP (10-14,21)

  30 continue
  HHT = 3.0
  go to 70

  !     HT OF TALLEST SUBS. SPECIES 31:BO (24,30-32,35,39,40)

  40 continue
  HHT = 1.0
  go to 70

  !     HT OF TALLEST SUBS. SPECIES 37:MA (26-29,33,34,36-39,41-49)

  50 continue
  HHT = 2.0
  go to 70

  70 continue
  return
end
