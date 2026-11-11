!=================================================!
!   Compute subsequent and planted tree heights   !
!=================================================!
! SO $Id$
subroutine ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV,IHTSER,GENTIM,&
     &TRAGE)
  implicit none

  !     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
  !     CREATED BY THE ESTABLISHMENT MODEL.
  !     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
  !     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER
  !     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE
  !     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
  !     THE TREE.

  !     CALLED FROM **ESTAB
  !     CALLS **SMHTGF

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'PDEN.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,IHTSER,ITIME,N,MODE0
  real    :: AGE,EMSQR,DILATE,DELAY,ELEV,GENTIM,H,HHT,TRAGE
  real    :: CR
  real    :: RDANUW
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = DILATE
  RDANUW = ELEV
  RDANUW = EMSQR
  IDANUW = IHTSER

  !  CHECK FOR DEBUG

  call DBCHK (DEBUG,'ESSUBH',6,ICYC)
  if (DEBUG)write(JOSTND,*)' ENTERING ESSUBH - ICYC,I,HHT,TRAGE= ',&
       &ICYC,I,HHT,TRAGE

  N = INT(DELAY+0.5)
  if (N.lt.-3) N = -3
  DELAY = FLOAT(N)
  ITIME = INT(TIME+0.5)
  if (N.gt.ITIME) DELAY = TIME
  AGE = TIME-DELAY-GENTIM+TRAGE
  if (AGE.lt.1.0) AGE = 1.0
  TRAGE = TIME-DELAY

  select case(I)

     !     HEIGHT OF TALLEST SUBSEQUENT SPECIES FROM SP 9,30 CA VARIANT

  case(9,27)
     HHT = 1.0

     !     HT OF TALLEST SUBS. SPECIES 24 (AS)

  case (24)
     HHT = 5.0

     !  CALL SMHTGF TO CALCULATE SEEDLING HEIGHT AT AGE "AGE".
     !  CR IS ONLY USED FOR SP NO. 9 IN SMHTGF, SO IS SET =0.

  case default
     CR = 0.
     call SMHTGF (MODE0,ICYC,I,H,CR,AGE,HHT,JOSTND,DEBUG)

  end select

  if (DEBUG) then
     write(JOSTND,*)' LEAVE ESSUBH -ICYC,I,AGE,TRAGE,TIME,DELAY,HHT= ',&
          &ICYC,I,AGE,TRAGE,TIME,DELAY,HHT
  end if

  return
end
