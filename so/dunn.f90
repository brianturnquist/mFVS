!===============!
!   so : dunn   !
!===============!
! SO $Id$
subroutine DUNN (SS)
  implicit none

  ! THIS SUBROUTINE PROCESSES THE DUNNING CODE INFORMATION THAT WAS
  ! ENTERED BY KEYWORD.

  ! WHEN A DUNNING CODE IS ENTERED (IE ANY OF THE SITEAR VALUES BETWEEN
  ! 0 AND 7)  THEN SITE VALUES FOR ALL SPECIES ARE AUTOMATICALLY SET.  IF
  ! ANY SITEAR VALUES ARE BETWEEN 8 AND 10 THIS IS AN ERROR AND THE
  ! DEFAULT VALUE SET IN GRINIT IS MAINTAINED. THIS ROUTINE IS CALLED FROM
  ! INITRE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  SPECIES ORDER:
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  ! Variables
  real    :: ADJFAC(MAXSP),DUNN50(8),DUNN99(8),SS,DU50,DU99
  integer :: I99(MAXSP),IST,ISPC
  data ADJFAC/&
       &0.90, 0.90, 1.00, 1.00, 0.90, 0.76, 0.82, 1.00, 1.00, 1.00,&
       &0.57, 1.00, 1.00, 1.00, 1.00, 0.90, 1.00, 1.00, 0.90, 0.76,&
       &1.00, 0.57, 0.57, 0.57, 0.57, 1.00, 0.76, 0.57, 1.00, 1.00,&
       &1.00, 1.00, 0.57/
  data I99/&
       &0,    1,    0,    0,    1,    0,    0,    1,    0,    1,&
       &1,    0,    1,    0,    1,    1,    0,    1,    0,    1,&
       &1,    1,    1,    2,    1,    1,    0,    1,    1,    1,&
       &1,    0,    1/
  data DUNN50/106.,90.,75.,56.,49.,39.,31.,23./
  data DUNN99/140.,121.,102.,81.,67.,54.,44.,36./

  IST = INT(SS+1.0)
  DU50 = DUNN50(IST)
  DU99 = DUNN99(IST)

  !  SET SITE INDEX VALUES BASED ON DUNNING VALUES ENTERED.

  do ISPC = 1,MAXSP
     SITEAR(ISPC) = DU50 * ADJFAC(ISPC)
     if (I99(ISPC).eq.1)SITEAR(ISPC) = DU99 * ADJFAC(ISPC)
     20 continue
  end do

  !  ADJUST ASPEN FROM THE CURRENT SETTING AT 50 YEAR BASE AGE TO AN
  !  80 YEAR BASE AGE BY INTERPOLATION.

  SITEAR(24) = SITEAR(24)+(DU99*ADJFAC(24)-SITEAR(24))*(3./5.)

  !  ADJUST MOUNTAIN HEMLOCK TO METRIC

  SITEAR(5) = SITEAR(5)/3.28083333

  return
end
