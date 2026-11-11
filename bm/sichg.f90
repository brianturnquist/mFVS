!================!
!   bm : sichg   !
!================!
! BM $Id$
subroutine SICHG(ISISP,SSITE,SIAGE)
  implicit none

  ! THIS ROUTINE TAKES THE SITE FROM THE STDINFO CARD AND CALCULATES AN AGE
  ! THAT CURVE, GIVEN THE SITE, WHERE A NEW HT (OR PROXY AGE) MAY ME LOOKE
  ! UP.

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  character(len=1) :: ISILOC

  character(len=1) :: REFLOC(MAXSP)

  integer :: DIFF,I,ISISP

  integer :: REFAGE(MAXSP)

  real    :: AGE2BH,SSITE

  real    :: A(MAXSP),B(MAXSP),SIAGE(MAXSP)

  !  DATA STATEMENTS:

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
  !  17=OS, 18=OH

  !  SPECIES EXPANSION:
  !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
  !  LM USES UT LM
  !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
  !  YC USES WC YC
  !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
  !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
  !  OS USES BM PP BARK COEFFICIENT
  !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

  !  DATA STATEMENTS FOR A AND B CONTAIN COEFFICIENTS FOR A LINEAR
  !  RELATION OF AGE TO BREAST HEIGHT AS A FUNCTION OF SITE INDEX.
  !  THE LINEAR RELATION IS AN ESTIMATE FROM THE LOWER END POINT OF
  !  A HEIGHT GROWTH CURVE TO THE ORIGIN (0 AGE; 0 TOT TREE HT.)
  !  THESE COEFFICIENTS SHOULD BE COMPATIBLE WITH SIMILAR VALUES USED
  !  IN ESSUBH AND REGSMT SUBROUTINES.

  data A/ 18.43316,  8.11668,   8.50000,  9.01840, 45.24969,&
       &34.0000, 10.65724,  33.72545, 13.88200,  8.00000,&
       &34.0000,  34.0000,  11.56252, 11.56252,  34.0000,&
       &11.56252,   8.0000,  11.56252/

  data B/ -0.13399, -0.05661,  -0.05000, -0.05700, -1.28885,&
       &-0.20000, -0.10667, -0.274509, -0.06588, -0.04286,&
       &-0.20000, -0.20000,  -0.05586, -0.05586, -0.20000,&
       &-0.05586, -0.04286,  -0.05586/

  data REFLOC/ 'T',      'B',       'B',      'B',      'B',&
       &'T',      'T',       'B',      'B',      'B',&
       &'T',      'T',       'B',      'B',      'B',&
       &'B',      'B',       'B'/

  data REFAGE/  50,       50,        50,       50,      100,&
       &100,       50,       100,      100,      100,&
       &100,      100,       100,      100,       80,&
       &100,      100,       100/

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
        if (ISISP .ne.5.and. I .eq.5)AGE2BH = A(I) + B(I)*(SSITE/3.281)
        if (ISISP .eq.5.and. I .ne.5)AGE2BH = A(I) + B(I)*(SSITE*3.281)
     end if
     SIAGE(I) = REFAGE(I) + AGE2BH*DIFF
     100 continue
  end do
  return
end
