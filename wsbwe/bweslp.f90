!====================!
!   wsbwe : bweslp   !
!====================!
! WSBWE $Id$
function BWESLP(XX,X,Y,N)
  implicit none

  !     A LINEAR-INTERPOLATION FUNCTION

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
  !     INT-MOSCOW FORESTRY SC. LAB. AUG 1980

  !     DESCRIPTION :

  !     SUPPLIED WITH THE BWMOD AS WRITTEN AT THE MODELING
  !     WORKSHOP.  THE NAME IS CHANGED FROM 'SLP' TO 'BWESLP'.
  !     THE FUNCTION RETURNS THE VALUE OF A LINEAR SEGMENTED
  !     FUNCTION DEFINED BY THE PAIRS X(I), Y(I) EVALUATED AT
  !     X=XX FOR X INCLUDED IN THE INTERVAL ?X(1),X(N)!
  !     IF XX < X(1), F(XX)=X(1).  IF XX > X(N), XX=X(N).

  !     PARAMETERS :

  !     XX - POINT AT WHICH FUNCTION IS TO BE EVALUATED.
  !     X  - ARRAY OF X(I), SEGMENT ENDPOINTS FOR X VARIABLE.
  !     Y  - ARRAY OF Y(I), SEGMENT ENDPOINTS FOR Y VARIABLE.
  !     N  - SIZE OF ARRAYS X AND Y

  ! Revision History:
  !   05-MAY-00 Lance David (FHTET)
  !      .The last occurence of SLP had been translated to BWSLP instead
  !       of BWESLP. Fixed it.
  !      .Added weather and outbreak random number seeds WSEEDR and OBSEER.
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Variables
  integer :: I,N,NN
  real    :: XX,X(N),Y(N),BWESLP

  !     WRITE (16,*) 'FUNCTION BWESLP: XX=',XX,' N=',N               ! TEMP DEBUG
  !     WRITE (16,*) '                  X=',X                        ! TEMP DEBUG
  !     WRITE (16,*) '                  Y=',Y                        ! TEMP DEBUG

  NN = N-1
  do I = 1,NN
     if (XX.lt.X(I).or.XX.gt.X(I+1)) go to 30
     BWESLP = Y(I)+((Y(I+1)-Y(I))/(X(I+1)-X(I)))*(XX-X(I))
     goto 9000
     30 continue
  end do
  BWESLP = Y(N)
  if (XX.lt.X(1)) BWESLP = Y(1)

  9000 continue

  !     WRITE (16,*) 'FUNCTION BWESLP=',BWESLP                       ! TEMP DEBUG

  return
end
