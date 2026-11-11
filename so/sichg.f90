!================!
!   so : sichg   !
!================!
! SO $Id$
subroutine SICHG(ISISP,SSITE,SIAGE)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'

  !  THIS ROUTINE TAKES THE SITE FROM THE STDINFO CARD AND/OR
  !  THE DEFAULT VALUE FROM SITSET, AND DETERMINES IF IT IS A
  !  BHA OR TTA SITE CURVE.  THE RETURN VARIABLE SIAGE REPRESENTS THE
  !  SPECIES BASE AGE + AGE2BH IF THE SITE SPECIES SITE CURVE
  !  IS A TTA CURVE AND SPECIES CURVE A BHA CURVE, AND SO ON.
  !  THEN, THE SITE FOR EACH SPECIES MAY BE ESTIMATED USING
  !  THE SITE SPECIES CURVE IN HTCALC.
  !  DATA STATEMENTS FOR A AND B CONTAIN COEFFICIENTS FOR A LINEAR
  !  RELATION OF AGE TO BREAST HEIGHT AS A FUNCTION OF SITE INDEX.
  !  THE LINEAR RELATION IS AN ESTIMATE FROM THE LOWER END POINT OF
  !  A HEIGHT GROWTH CURVE TO THE ORIGIN (0 AGE; 0 TOT TREE HT.)
  !  THESE COEFFICIENTS SHOULD BE COMPATIBLE WITH SIMILAR VALUES USED
  !  IN THE SMHTGF SUBROUTINE.

  !  CALLED FROM SITSET
  !  SPECIES ORDER:
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  ! Variables
  integer :: DIFF,REFAGE(MAXSP)
  character(len=1) :: ISILOC
  character(len=1) :: REFLOC(MAXSP)
  real    :: SSITE,AGE2BH
  integer :: ISISP,I
  real    :: A(MAXSP),B(MAXSP),SIAGE(MAXSP)

  data A /&
       &18.43316,  8.00000,  8.50000,  9.01840, 45.24969,&
       &9.01840, 10.65724, 33.72545,  0.00000,  8.00000,&
       &34.0000,  9.01840, 13.88200,  9.01840, 7.938059,&
       &34.0000,  8.11680, 24.62857,  6.15767, 11.56252,&
       &11.56252,  3.28241, 11.56252,  34.0000, 11.56252,&
       &11.56252,  00.0000, 11.56252, 11.56252, 11.56252,&
       &11.56252,  8.50000, 11.56252/

  data B /&
       &-0.13399, -0.04286, -0.05000, -0.05700, -1.28885,&
       &-0.05700, -0.10667,-0.274509, -0.00000, -0.04286,&
       &-0.20000, -0.05700, -0.06588, -0.05700, -0.02873,&
       &-0.20000, -0.05661, -0.20465, -0.03596, -0.05586,&
       &-0.05586, -0.02987, -0.05586, -0.20000, -0.05586,&
       &-0.05586, -0.00000, -0.05586, -0.05586, -0.05586,&
       &-0.05586, -0.05000, -0.05586/

  data REFLOC/&
       &'T', 'B', 'B', 'B', 'B', 'B', 'T', 'B', 'B', 'B',&
       &'T', 'B', 'B', 'B', 'B', 'T', 'B', 'T', 'B', 'B',&
       &'B', 'T', 'B', 'B', 'B', 'B', 'B', 'B', 'B', 'B',&
       &'B', 'B', 'B'/

  data REFAGE/&
       &50, 100,  50,  50, 100,  50,  50, 100,  50, 100,&
       &100,  50, 100,  50, 100, 100,  50, 100,  50, 100,&
       &100,  20, 100,  80, 100, 100,  50, 100, 100, 100,&
       &100,  50, 100/

  ! ISILOC IS THE PLACE THE AGE FOR THE SITE SPECIES.

  ISILOC = REFLOC(ISISP)
  do I = 1,MAXSP

     !  SET UP THE ARRAY TO TELL WHETHER YOU NEED TO SLIDE UP OR DOWN THE SIT
     !  LINE TO ADJUST FOR TOTAL AGE OR BREAST HIGH AGE

     if (ISILOC .eq. 'T' .and. REFLOC(I) .eq. 'B')DIFF = -1
     if (ISILOC .eq. REFLOC(I))DIFF = 0
     if (ISILOC .eq. 'B' .and. REFLOC(I) .eq. 'T')DIFF = 1
     AGE2BH = 0.0
     if (DIFF .lt.0.or. DIFF .gt.0) then
        AGE2BH = A(I) + B(I)*SSITE
        if ((ISISP .ne.5) .and. (I .eq.5))&
             &AGE2BH = A(I) + B(I)*(SSITE/3.281)
        if ((ISISP .eq.5) .and. (I .ne.5))&
             &AGE2BH = A(I) + B(I)*(SSITE*3.281)
     end if
     if (I.eq.9)AGE2BH = 18.
     if (I.eq.27)AGE2BH = 2.
     SIAGE(I) = REFAGE(I) + AGE2BH*DIFF
     100 continue
  end do
  return
end
