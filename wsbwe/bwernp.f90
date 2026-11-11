!====================!
!   wsbwe : bwernp   !
!====================!
! WSBWE $Id$
function BWERNP (XMEAN,XMXVAR)
  implicit none

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--APRIL 1989

  !     CALLED FROM :

  !     BWEDAM - APPLY BUDWORM DAMAGE TO PROGNOSIS SAMPLE TREES.

  !     SUBROUTINES CALLED :

  !     BWEBET - BETA DISTRIBUTION RANDOM NUMBER GENERATOR.

  !     PARAMETERS :

  !     XMEAN = A MEAN PROPORTION...ON A 0 TO 1.0 RANGE.
  !     XMXVAR= THE MAX VARIANCE AT XMEAN=.5

  !     BWERNP  = A BETA RANDOM NUMBER WHEREBY THE VARIANCE IS A
  !              SIMPLE FUNCTION OF THE MEAN.

  !  Revision History:
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Previous noted revision 12/05/90
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Variables
  integer :: KODE
  real    :: BWERNP,C,SC,V,VAR,W,XM,XMEAN,XMXVAR,X(1)

  data C/2.7/,SC/0.9304/
  X(1) = 0.0

  !     IF THE MEAN IS CLOSE TO EITHER ZERO OR ONE, THEN SKIP THIS STUFF.

  if (XMEAN.lt.0.001.or. XMEAN.gt.0.999) then
     BWERNP = XMEAN
  else

     !        COMPUTE THE VARIANCE AS A FUNCTION OF THE MEAN AND XMXVAR.
     !        ASSUME THAT THE RELATIONSHIP IS SYMMETRICAL ABOUT .5

     if (XMEAN.gt.0.5) then
        XM = 1.-XMEAN
     else
        XM = XMEAN
     end if
     VAR = (SC*XMXVAR)*(C*(XM*(C-1.))**(C-1.)*EXP(-((C-1.)*XM)**C))

     !        COMPUTE THE PARAMETERS OF THE BETA

     W = (XMEAN*(1.-XMEAN)/VAR)-1.
     V = XMEAN*W
     W = (1.-XMEAN)*W

     !        GET ONE BETA VARIATE.

     !        WRITE (16,*) 'IN BWERNP: bCALL BWEBET V=',V,' W=',W,' X=',X
     call BWEBET (V,W,X,1,KODE)
     !        WRITE (16,*) 'IN BWERNP: aCALL BWEBET V=',V,' W=',W,' X=',X
     if (KODE.lt.0) then
        BWERNP = XMEAN
     else
        BWERNP = X(1)
     end if
  end if
  return
end
