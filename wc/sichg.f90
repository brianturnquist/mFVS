!================!
!   wc : sichg   !
!================!
! WC $Id$
subroutine SICHG(ISISP,SSITE,SIAGE)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  integer :: DIFF,REFAGE(MAXSP)
  character(len=1) :: ISILOC
  character(len=1) :: REFLOC(MAXSP)
  real    :: A(MAXSP),B(MAXSP),SIAGE(MAXSP)
  integer :: ISISP,I
  real    :: SSITE,AGE2BH

  !  DATA STATEMENTS FOR A AND B CONTAIN COEFFICIENTS FOR A LINEAR
  !  RELATION OF AGE TO BREAST HEIGHT AS A FUNCTION OF SITE INDEX.
  !  THE LINEAR RELATION IS AN ESTIMATE FROM THE LOWER END POINT OF
  !  A HEIGHT GROWTH CURVE TO THE ORIGIN (0 AGE; 0 TOT TREE HT.)
  !  THESE COEFFICIENTS SHOULD BE COMPATIBLE WITH SIMILAR VALUES USED
  !  IN ESSUBH AND REGSMT SUBROUTINES.

  data A /21.35000, 9.01840,   9.01840, 33.72545, 14.81367,&
       &14.81367, 7.938059, 11.56252,  8.00000, 33.72545,&
       &10.65724, 8.000000, 21.02281, 21.02281,  8.00000,&
       &11.56252, 11.56252, 11.56252,  6.15767, 45.24969,&
       &11.56252,  3.28241, 5*11.56252, 4.94166,11.56252,&
       &8.11668,9*11.56252/
  data B /-0.10290, -0.05700, -0.05700, -0.27451, -0.11174,&
       &-0.11174, -0.02873, -0.05586, -0.04286, -0.27451,&
       &-0.10667, -0.04259, -0.15978, -0.15978, -0.04286,&
       &-0.05586, -0.05586, -0.05586, -0.03596, -1.28885,&
       &-0.05586, -0.02987, 5*-0.05586,-0.02419, -0.05586,&
       &-0.05661,9*-0.05586/
  data REFLOC/&
       &10*'B','T',10*'B','T',17*'B'/
  data REFAGE/&
       &100,50,50,100,50,50,4*100,50,7*100,50,100,100,20,5*100,50,&
       &100,50,9*100/

  ! ISILOC IS THE PLACE THE AGE FOR THE SITE SPECIES.

  ISILOC = REFLOC(ISISP)
  do I = 1,MAXSP

     !  SET UP THE ARRAY TO TELL WHETHER YOU NEED TO SLIDE UP OR DOWN THE SIT
     ! LINE TO ADJUST FOR TOTAL AGE OR BREAST HIGH AGE

     if (ISILOC .eq. 'T' .and. REFLOC(I) .eq. 'B')DIFF = -1
     if (ISILOC .eq. REFLOC(I))DIFF = 0
     if (ISILOC .eq. 'B' .and. REFLOC(I) .eq. 'T')DIFF = 1
     AGE2BH = 0.0
     if (DIFF .lt.0.or. DIFF .gt.0) then
        AGE2BH = A(I) + B(I)*SSITE
        if (ISISP .ne.20.and. I .eq.20)AGE2BH = A(I) + B(I)*(SSITE/3.281)
        if (ISISP .eq.20.and. I .ne.20)AGE2BH = A(I) + B(I)*(SSITE*3.281)
     end if
     SIAGE(I) = REFAGE(I) + AGE2BH*DIFF
     100 continue
  end do
  return
end
