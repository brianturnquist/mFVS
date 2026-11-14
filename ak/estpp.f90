!=========================================!
!   Predict regenerating trees per plot   !
!=========================================!
! AK $Id$
subroutine ESTPP (VAL,IFT,DEBUG,JOSTND,TPP)
  implicit none

  !     PREDICTS THE NUMBER OF NATURAL REGENERATING TREES
  !     TO ESTABLISH PER PLOT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESCOM2.f90'
  include 'ESCOMN.f90'

  !  VARIABLE DEFINITIONS:

  !  VAL      -- RANDOM NUMBER DRAW
  !  IFT      -- STAND FOREST TYPE CATEGORY WHERE:
  !                1 = 122
  !                2 = 125
  !                3 = 270
  !                4 = 271
  !                5 = 281
  !                6 = 301
  !                7 = 304
  !                8 = 305
  !                9 = 703
  !               10 = 901
  !               11 = 902
  !               12 = 904
  !               13 = 911
  !               14 = OTHER (NO ADVANCED REGENERATION)
  !  BB AND BBIFT   -- WEIBULL SCALE PARAMETER BY FOREST TYPE
  !  CC AND CCIFT   -- WEIBULL SHAPE PARAMETER BY FOREST TYPE
  !  TPP      -- NUMBER OF TREES PER STOCKED PLOT

  ! Variables
  real    :: BB,CC,TPP,VAL,BBIFT(14),CCIFT(14)
  integer :: IFT,JOSTND
  logical :: DEBUG

  data BBIFT / &
       & 5.617512, 13.273635, 4.291578, 9.770818, 9.452008, 8.075209, &
       & 9.635466,  3.632153, 3.794353, 6.107958, 5.251271, 5.617512, &
       & 2.695103,  5.617512 /

  data CCIFT / &
       & 1.213626, 1.405827, 1.442975, 1.496351, 1.523877, 1.274965, &
       & 1.505655, 1.414683, 1.462014, 1.399479, 1.268050, 1.213626, &
       & 1.709675, 1.213626 /

  !     CALCULATE TREES PER STOCKED PLOT.
  !     LP,YC,RC FOREST TYPES: USE WESTERN HEMLOCK FOREST TYPE WEIBULL
  !     DISTRIBUTION.
  BB = BBIFT(IFT)
  CC = CCIFT(IFT)
  TPP = BB * (-1*ALOG(1-VAL))**(1/CC)

  !     SET TO 0 IF FOREST TYPE IS OTHER (14)
  if (IFT.eq.14)TPP = 0.
  if (DEBUG)write(JOSTND,*)' IN ESTPP', ' IFT=',IFT,' VAL=', VAL, &
       & ' TPP=',TPP
  return
end subroutine ESTPP
