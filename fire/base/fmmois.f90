!=========================!
!   fire/base : fmmois   !
!=========================!
! FIRE-BASE $Id$
subroutine FMMOIS (FMOIS, MOIS)
  implicit none

  !     CALLED FROM: FMBURN

  !  PURPOSE:
  !     THIS SUBROUTINE RETURNS THE PRESET MOISTURE LEVELS

  !  CALL LIST DEFINITIONS:
  !     FMOIS:   MOISTURE MODEL NUMBER (1-4; 0 OR OTHER VALUES LEAVE MOIS
  !              UNCHANGED)
  !     MOIS:    MOISTURE VALUES

  !  LOCAL VARIABLE DEFINITIONS:
  !     MOIS: (1,*):DEAD STUFF: ,1=0-.25;,2=.25-1;,3=1-3;,4=3+;,5=DUFF
  !           (2,*):LIVE STUFF: ,1=LIVE WOODY;,2=LIVE HERB

  !     THE PRESET MOISTURE LEVELS DIFFER BY REGION. EACH TABLE BELOW HAS
  !     ONE COLUMN PER MOISTURE MODEL (FMOIS 1-4), AND THE ROWS ARE:
  !     1HR (0-.25"), 10HR (.25-1"), 100HR (1-3"), 3+", DUFF, LIVE WOODY,
  !     LIVE HERB.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: FMOIS
  real    :: MOIS(2,5)
  logical :: DEBUG
  real    :: TBL(7,4)

  !     INLAND WEST AND PACIFIC NORTHWEST (DEFAULT): AK BC BM CI EC EM IE
  !     KT OP PN SO WC
  real, parameter :: MOISIE(7,4) = reshape((/&
       &.04, .04, .05, .10, .15, .7, .7,&
       &.08, .08, .1, .15, .5, 1.1, 1.1,&
       &.12, .12, .14, .25, 1.25, 1.5, 1.5,&
       &.16, .16, .18, .50, 2.0, 1.5, 1.5/), (/7,4/))

  !     LAKE STATES AND NORTHEAST: LS NE ON
  real, parameter :: MOISLS(7,4) = reshape((/&
       &.05, .08, .12, .15, .40, .89, .60,&
       &.07, .09, .14, .17, .75, 1.05, .82,&
       &.10, .13, .17, .21, 1.0, 1.35, 1.16,&
       &.19, .29, .22, .25, 1.75, 1.40, 1.20/), (/7,4/))

  !     SOUTHERN AND CENTRAL STATES: CS SN
  real, parameter :: MOISSN(7,4) = reshape((/&
       &.05, .07, .12, .17, .40, .55, .55,&
       &.06, .08, .13, .18, .75, .80, .80,&
       &.07, .09, .14, .20, 1.0, 1.0, 1.0,&
       &.16, .16, .18, .50, 1.75, 1.5, 1.5/), (/7,4/))

  !     CENTRAL ROCKIES, TETONS AND UTAH: CR TT UT
  real, parameter :: MOISUT(7,4) = reshape((/&
       &0.04, 0.04, 0.05, 0.10, 0.15, 0.70, 0.70,&
       &0.05, 0.06, 0.08, 0.15, 0.50, 0.90, 0.90,&
       &0.08, 0.10, 0.12, 0.16, 1.25, 1.20, 1.20,&
       &0.10, 0.12, 0.15, 0.18, 2.00, 1.40, 1.40/), (/7,4/))

  !     CALIFORNIA AND WESTERN SIERRAS: CA NC OC WS
  real, parameter :: MOISWS(7,4) = reshape((/&
       &0.03, 0.04, 0.05, 0.10, 0.15, 0.70, 0.70,&
       &0.08, 0.08, 0.10, 0.15, 0.50, 1.10, 1.10,&
       &0.12, 0.12, 0.14, 0.25, 1.25, 1.50, 1.50,&
       &0.12, 0.12, 0.14, 0.25, 1.25, 1.50, 1.50/), (/7,4/))

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMMOIS',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING ROUTINE FMMOIS CYCLE = ',I2)

  !     BEGIN ROUTINE

  if (FMOIS .lt. 1 .or. FMOIS .gt. 4) return

  select case (VARACD)
  case ('LS','NE','ON')
     TBL = MOISLS
  case ('CS','SN')
     TBL = MOISSN
  case ('CR','TT','UT')
     TBL = MOISUT
  case ('CA','NC','OC','WS')
     TBL = MOISWS
  case default
     TBL = MOISIE
  end select

  MOIS(1,1:5) = TBL(1:5,FMOIS)
  MOIS(2,1:2) = TBL(6:7,FMOIS)

  return
end subroutine FMMOIS
