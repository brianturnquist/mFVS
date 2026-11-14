!=================================================!
!   Compute subsequent and planted tree heights   !
!=================================================!
! NC $Id$
subroutine ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,ISER,GENTIM,&
     &TRAGE)
  implicit none

  !     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
  !     CREATED BY THE ESTABLISHMENT MODEL.

  !     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
  !     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER
  !     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE
  !     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
  !     THE TREE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESCOMN.f90'

  !  DECLARATIONS

  ! Variables
  integer :: I,ISER,N,ITIME
  real    :: HHT,DELAY,DILATE,ELEV,EMSQR,GENTIM,TRAGE,AGE
  real    :: RDANUW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = DILATE
  RDANUW = ELEV
  RDANUW = EMSQR
  IDANUW = ISER

  N = INT(DELAY+0.5)
  if (N.lt.-3) N = -3
  DELAY = FLOAT(N)
  ITIME = INT(TIME+0.5)
  if (N.gt.ITIME) DELAY = TIME
  AGE = TIME-DELAY-GENTIM+TRAGE
  if (AGE.lt.1.0) AGE = 1.0
  TRAGE = TIME-DELAY
  select case (I)

     !     HEIGHT OF TALLEST SUBSEQUENT SPECIES 1 (OC)

  case (1)
     HHT = 1.0

     !     HEIGHT OF TALLEST SUBS. SPECIES 2 (SP)

  case (2)
     HHT = 1.0

     !     HT OF TALLEST SUBS. SPECIES 3 (DF)

  case (3)
     HHT = 1.0

     !     HT OF TALLEST SUBS. SPECIES 4 (WF)

  case (4)
     HHT = 1.0

     !     HT OF TALLEST SUBS. SPECIES 5 ( M)

  case (5)
     HHT = 7.0

     !     HT OF TALLEST SUBS. SPECIES 6 (IC)

  case (6)
     HHT = 1.0

     !     HT OF TALLEST SUBS. SPECIES 7 (BO)

  case (7)
     HHT = 7.0

     !     HT OF TALLEST SUBS. SPECIES 8 (TO)

  case (8)
     HHT = 7.0

     !     HT OF TALLEST SUBS. SPECIES 9 (RF)

  case (9)
     HHT = 1.0

     !     HEIGHT OF TALLEST SUBS. SPECIES 10 (PP)

  case (10)
     HHT = 0.8

     !     HT OF TALLEST SUBS. SPECIES 11 (OH)

  case (11)
     HHT = 7.0

     !     HT OF TALLEST SUBS. SPECIES 12 (RW)

  case (12)
     HHT = 2.0

  end select
  return
end
  !**END OF CODE SEGMENT
