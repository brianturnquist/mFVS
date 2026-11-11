!===================!
!   lpmpb : mpber   !
!===================!
! LPMPB $Id$
subroutine MPBER (NOER)
  implicit none

  !     NOER  = TRUE WHEN MINIMUM CONDITIONS MET FOR AN OUTBREAK
  !             IE NOT AN 'ERROR'

  ! Revision History
  !   05/31/00 GEB
  !     Adding variable IDXLP, the index of Lodgepole pine in arrays
  !     Defined in mpblkd.f, which is variant dependent
  !   12/01/98 RNH
  !     Adapated to 24 species (CR variant)
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  logical :: NOER

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'

  integer :: I,I1,I2,II
  real    :: P,D

  !     CHECK FOR EXECUTION-ERROR CAUSING CONDITIONS.

  NOER = .true.
  ILP = 0
  CNTLP = 0.0
  BALPP = 0.0
  A45DBH = 0.0
  TLP45 = 0.0

  !     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
  !     with new species mapping and new location for LP (RNH Dec98, GEB May2000)

  I1 = ISCT(IDXLP,1)
  !      I1 = ISCT(7,1)

  if ( I1.eq.0 ) go to 90

  !     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
  !     with new species mapping and new location for LP (RNH Dec98, GEB May2000)

  I2 = ISCT(IDXLP,2)
  !      I2 = ISCT(7,2)

  ILP = I2-I1+1
  if ( NCLASS .le.0 ) go to 90
  if ( ILP .lt.1 ) go to 90

  do II = I1, I2
     I = IND1(II)
     P = PROB(I)
     D = DBH(I)
     CNTLP = CNTLP + P
     BALPP = BALPP + 0.005454154 * D * D * P
     if (D .lt.4.5 )  go to 80
     TLP45 = TLP45 + P
     A45DBH = A45DBH + D * P
     80 continue
  end do
  if ( CNTLP .lt..01 ) go to 90
  if ( TLP45.lt.1. ) go to 90

  !     PROPORTION BASAL AREA IN LPP

  PBALPP = BALPP / BA

  !     AVERAGE LODGEPOLE DBH:

  A45DBH = A45DBH  / TLP45
  go to 100

  90 continue
  NOER = .false.
  100 continue
  return
end
