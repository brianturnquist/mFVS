!========================!
!   fire/base : fmhide   !
!========================!
! FIRE-BASE $Id$
subroutine FMHIDE (ISTD, IYR)
  implicit none

  !     CALLED FROM: FMMAIN
  !     CALLS:   CFVOL

  !  PURPOSE:
  !     PRINT SOME VARIABLES FOR DEBUGGING/CHECKING.  AT SOME POINT, WE
  !     MAY WANT TO PLACE THESE INTO PROPER FILES.

  !  CALL LIST DEFINITIONS:
  !     ISTD:    CURRENT STAND
  !     IYR:     CURRENT YEAR

  !  LOCAL VARIABLE DEFINITIONS:

  !  COMMON BLOCK VARIABLES AND PARAMETERS:

  !.... PARAMETER STATEMENTS.

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  include 'FMCOM.f90'

  !.... VARIABLE DECLARATIONS.
  ! Variables
  real    :: SNVIS,SNVIH
  real    :: TOTD(2),TOTV(2)
  integer :: IYR,ISTD,I,JCL

  if (JCOUT .le.0) return

  TOTD(1) = 0.0
  TOTD(2) = 0.0
  TOTV(1) = 0.0
  TOTV(2) = 0.0

  do I = 1, NSNAG

     if ((DENIS(I)+DENIH(I)) .le.0.0) goto 100

     !        CALCULATE SNAG VOLUME

     SNVIS = 0.0
     SNVIH = 0.0

     if (DENIH(I) .gt.0.0) then
        call FMSVOL (I, HTIH(I), SNVIH,.false.,0)
        SNVIH = SNVIH*DENIH(I)
     end if

     if (DENIS(I) .gt.0.0) then
        call FMSVOL (I, HTIS(I), SNVIS,.false.,0)
        SNVIS = SNVIS*DENIS(I)
     end if

     !        SUM VOLUMES AND DENSITIES OF SNAGS <> 12 INCHES

     JCL = 2
     if (DBHS(I) .lt.12.0) JCL = 1

     TOTD(JCL) = TOTD(JCL) + DENIS(I) + DENIH(I)
     TOTV(JCL) = TOTV(JCL) + SNVIS + SNVIH

     100 continue
  end do

  write(JCOUT,300) IYR,ISTD,TOTD(1),TOTD(2),TOTV(1),TOTV(2),&
       &(CWDNEW(1,I),I = 1,11),(CWDNEW(2,I),I = 1,11)
  300 format(I4,1X,I4,1X,2(F6.1,1X),2(F6.0,1X),1X,11(F7.2,1X),&
       &11(F6.2,1X))

  do I = 1,MXFLCL
     CWDNEW(1,I) = 0.0
     CWDNEW(2,I) = 0.0
     400 continue
  end do

  return
end
