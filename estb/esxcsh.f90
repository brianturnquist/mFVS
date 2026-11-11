!=================================!
!   Compute excess tree heights   !
!=================================!
! ESTB $Id$
subroutine ESXCSH (HTMAX,HTMIN,TIME,II,DRAW,HHT)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'

  !     SUBROUTINE TO ASSIGN HEIGHTS TO EXCESS TREES

  ! Variables
  real    :: BB(3,MAXSP),CC(3,MAXSP),SHIFT(MAXSP)
  integer :: II,ITIME
  real    :: HHT,DRAW,TIME,HTMIN,HTMAX,CLASS,XUPPR,XX
  data SHIFT/4.0,4.0,4*2.0,4.0,2.0,2.0,4.0,2.0/

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
       &3.089571,  5.830185,  10.105748,&
       &3.347712,  6.806825,  13.553455,&
       &3.169513,  4.506403,   8.940539,&
       &7.360424, 10.928846,  25.214411,&
       &1.466152,  5.159270,   9.272780,&
       &2.921356,  4.581383,  10.333282,&
       &2.779221,  9.033310,  14.131212,&
       &3.347712,  6.806825,  13.553455/
  data CC/       .745850,   .782170,    .842171,&
       &.902909,  1.166155,   1.306380,&
       &.996732,   .845413,    .948037,&
       &.800681,   .832278,    .954081,&
       &.567768,   .894628,   1.214044,&
       &.640554,   .813543,    .943493,&
       &1.148084,  1.232333,   1.117025,&
       &.722527,   .739031,   1.125510,&
       &.885137,   .871559,   1.043759,&
       &.899325,  1.074932,    .930698,&
       &.567768,   .894628,   1.214044/

  ITIME = 1
  if (TIME.gt.7.5.and.TIME.lt.12.5) ITIME = 2
  if (TIME.gt.12.5) ITIME = 3
  CLASS = (HTMAX/0.2) -SHIFT(II)
  XUPPR = 1.0-EXP(-(((CLASS-HTMIN)/BB(ITIME,II))**CC(ITIME,II)))
  XX = XUPPR*DRAW
  HHT = ((-(ALOG(1.00-XX)))**(1.0/CC(ITIME,II)))*BB(ITIME,II)+HTMIN
  HHT = 0.2*(HHT+SHIFT(II))
  return
end
