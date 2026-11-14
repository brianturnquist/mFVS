!=================================================!
!   Compute subsequent and planted tree heights   !
!=================================================!
! STRP $Id$
subroutine ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER,GENTIM,&
     &TRAGE)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESCOMN.f90'

  !     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
  !     CREATED BY THE ESTABLISHMENT MODEL.

  ! Variables
  integer :: IHTSER,I,N,ITIME
  real    :: TRAGE,GENTIM,ELEV,DELAY,DILATE,EMSQR,HHT,AGE

  !     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
  !     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER
  !     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE
  !     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
  !     THE TREE.

  N = INT(DELAY+0.5)
  if (N.lt.-3) N = -3
  DELAY = FLOAT(N)
  ITIME = INT(TIME+0.5)
  if (N.gt.ITIME) DELAY = TIME
  AGE = TIME-DELAY-GENTIM+TRAGE
  if (AGE.lt.1.0) AGE = 1.0
  TRAGE = TIME-DELAY
  go to (10,20,30,40,50,60,70,80,90,100,110),I

  !     HEIGHT OF TALLEST SUBSEQUENT SPECIES 1

  10 continue
  HHT = 1.0
  go to 120

  !     HEIGHT OF TALLEST SUBS. SPECIES 2

  20 continue
  HHT = 1.0
  go to 120

  !     HT OF TALLEST SUBS. SPECIES 3

  30 continue
  HHT = 1.0
  go to 120

  !     HT OF TALLEST SUBS. SPECIES 4

  40 continue
  HHT = 1.0
  go to 120

  !     HT OF TALLEST SUBS. SPECIES 5

  50 continue
  HHT = 1.0
  go to 120

  !     HT OF TALLEST SUBS. SPECIES 6

  60 continue
  HHT = 1.0
  go to 120

  !     HT OF TALLEST SUBS. SPECIES 7

  70 continue
  HHT = 1.0
  go to 120

  !     HT OF TALLEST SUBS. SPECIES 8

  80 continue
  HHT = 1.0
  go to 120

  !     HT OF TALLEST SUBS. SPECIES 9

  90 continue
  HHT = 1.0
  go to 120

  !     HEIGHT OF TALLEST SUBS. SPECIES 10

  100 continue
  HHT = 1.0
  go to 120
  110 continue

  !     HT OF TALLEST SUBS. SPECIES 11

  HHT = 1.0
  120 continue
  return
end
