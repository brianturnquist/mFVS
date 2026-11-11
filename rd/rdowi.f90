!================!
!   rd : rdowi   !
!================!
! RD $Id$
subroutine RDOWI
  implicit none

  !  Purpose :
  !     Windthrow model subroutine.  This subroutine is used to
  !     normalize the array that holds the relative susceptibility
  !     to windthrow for each tree species.

  !  Called By :
  !     RDOAGM  [ROOT DISEASE]

  !  Local Variables :
  !     ISPI   - INT
  !              Counter used to loop through the tree species.
  !              Used as index to species arrays.
  !     SUM    - REAL
  !              Sum of all tree species susceptibilites to windthrow.

  !  Common Block Variables Used :
  !     IRTSPC - (RDCOM)   (I)
  !     MAXSP  - (PRGPRM)  (I)
  !     ROWIBP - (RDCOM)   (I/O)

  !  Revision History :
  !     06/12/96 - Matthew K. Thompson
  !                Removed the entry point RDOWIN and put it into its
  !                own subroutine.
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'RDCOM.f90'

  ! Variables
  integer :: ISPI
  real    :: SUMX

  !.... Normalize relative susceptability to windthrow

  SUMX = 0.0
  do ISPI = 1,MAXSP
     SUMX = SUMX + ROWIBP(IRTSPC(ISPI),1)
     10 continue
  end do

  do ISPI = 1,MAXSP
     ROWIBP(IRTSPC(ISPI),2) = ROWIBP(IRTSPC(ISPI),1) / SUMX
     20 continue
  end do

  return
end
