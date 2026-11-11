!=================================!
!   Compute excess tree heights   !
!=================================!
! EM $Id$
subroutine ESXCSH (HTMAX,HTMIN,TIME,II,DRAW,HHT)
  implicit none

  !     SUBROUTINE TO ASSIGN HEIGHTS TO EXCESS TREES

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  real    :: BB(3,MAXSP),CC(3,MAXSP),SHIFT(MAXSP)
  real    :: HHT,DRAW,TIME,HTMIN,HTMAX,CLASS,XUPPR,XX
  integer :: II,ITIME

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  data SHIFT/ 4.0, 4.0, 2.0, 0.0, 2.0, 0.0, 4.0, 2.0, 2.0, 4.0,&
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.0, 0.0/

  !     COEFFICIENTS FOR ARRAYS BB AND CC PREDICT TREE HEIGHT 'CLASS'
  !     RATHER THAN ACTUAL HEIGHT; E.G.
  !     CLASS  DF,GF,C,H,S,AF  WP,WL,LP,PP
  !        1         .6            1.0
  !        2         .8            1.2
  !        3        1.0            1.4    ETC.
  !     PLOT AGE(YRS): 3 THRU 7  8 THRU 12  13 THRU 20

  data BB/      2.121455,  5.060402,   5.979549,&
       &6.643726, 11.422982,  19.618871,&
       &3.816083,  8.161474,  10.987699,&
       &0.,        0.,         0.,&
       &2.921356,  4.581383,  10.333282,&
       &0.,        0.,         0.,&
       &7.360424, 10.928846,  25.214411,&
       &1.466152,  5.159270,   9.272780,&
       &2.921356,  4.581383,  10.333282,&
       &2.779221,  9.033310,  14.131212,&
       &21*0.,&
       &3.347712,  6.806825,  13.553455,&
       &3*0./

  data CC/       .745850,   .782170,    .842171,&
       &.902909,  1.166155,   1.306380,&
       &.996732,   .845413,    .948037,&
       &0.,        0.,         0.,&
       &.885137,   .871559,   1.043759,&
       &0.,        0.,         0.,&
       &1.148084,  1.232333,   1.117025,&
       &.722527,   .739031,   1.125510,&
       &.885137,   .871559,   1.043759,&
       &.899325,  1.074932,    .930698,&
       &21*0.,&
       &.567768,   .894628,   1.214044,&
       &3*0./

  ITIME = 1
  if (TIME.gt.7.5.and.TIME.lt.12.5) ITIME = 2
  if (TIME.gt.12.5) ITIME = 3

  select case (II)

  case(1:3,5,7:10,18)
     CLASS = (HTMAX/0.2) -SHIFT(II)
     XUPPR = 1.0-EXP(-(((CLASS-HTMIN)/BB(ITIME,II))**CC(ITIME,II)))
     XX = XUPPR*DRAW
     HHT = ((-(ALOG(1.00-XX)))**(1.0/CC(ITIME,II)))*BB(ITIME,II)+HTMIN
     HHT = 0.2*(HHT+SHIFT(II))

  case(4,6)
     HHT = 0.5

  case(11:17,19)
     HHT = 5.0

  end select

  return
end
