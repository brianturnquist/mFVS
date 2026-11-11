!====================!
!   lpmpb : grpsum   !
!====================!
! LPMPB $Id$
subroutine GRPSUM (NRECS,IPT,ATR,P,WT)
  implicit none

  !     CALLED FROM GARBEL OR COMPRS    N.L. CROOKSTON JAN 1978
  !     MODIFIED MAY 1980 JAN 1981

  !          NRECS = LENGTH OF POINTER ARRAY
  !          IPT   = POINTER ARRAY
  !          ATR   = ATTRIBUTE TO BE PROCESSED (REAL)
  !          WT    = WEIGHT OF ATTRIBUTE
  !          P     = SUM (P(I) * WT * ATR(I))
  !          PROB  = WEIGHT OF EACH MEMBER OF ATR

  ! Revision History
  !   04/03/02 Lance R. David (FHTET)
  !     Arrays dimensioned "1" changed to "*" so that array size is
  !     inherited from calling routine.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  integer :: NRECS,IPT(*),II,I
  real    :: WT,P(*),ATR(*),AVE,STDV,XN

  AVE = 0.0
  STDV = 0.0
  XN = FLOAT(NRECS)

  do II = 1, NRECS
     I = IPT(II)
     AVE = AVE + ATR(I)
     STDV = STDV + ATR(I) ** 2
     17 continue
  end do

  !     USE THE MAXIMUM LIKLIHOOD ESTIMATE OF THE VARIANCE.

  STDV = (STDV-AVE*AVE/XN)/XN
  if ( STDV .gt.0.000000001 ) go to 18
  STDV = 1.0
  go to 21
  18 continue
  STDV = SQRT ( STDV )
  21 continue
  AVE = AVE/XN

  do II = 1, NRECS
     I = IPT(II)
     P(I) = P(I) + WT * (ATR(I) - AVE ) / STDV
     30 continue
  end do

  return
end
