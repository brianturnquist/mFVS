!==================!
!   wpbr : brsor   !
!==================!
! WPBR $Id$
subroutine BRSOR
  implicit none

  !  Purpose:
  !   This subroutine is called to sort and match tree IDs to insure
  !   proper mapping of Blister Rust Model canker data and FVS Model
  !   tree data.  This subroutine is called by TRESOR which is called
  !   from the the PROCESS KEYWORD section in INITRE.

  !  Revision History:

  !  09-APR-2001 Lance R. David
  !     Update for stock type that can now be supplied through canker file.
  !  09-MAY-2006 Lance R. David
  !     Added exit if blister rust not active.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: IDTR,IID,STK,IC1,IC2,IERR,IPOS,J,K,L,N
  real    :: AGE,DO1,DU1,GI1
  logical :: DEBUG

  !.... Exit if Blister Rust not active
  if (.not. BRYES) goto 300

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRSOR',5,ICYC)
  if (DEBUG) write(JOSTND,11) ICYC
  11 format('Entering subroutine BRSOR: cycle = ',I2)

  !.... Sort the FVS list of tree IDs.

  call I4PSRT(IREC1,IDTREE,IND1,.true.)

  !.... Initializations.

  IERR = 0
  K = 1

  !.... Top of BRUST tree loop. If the tree count is greater than the
  !.... number of unique BRUST tree IDs, then skip out.

  100 continue
  if (K.gt.INCAN) go to 200
  IDTR = IBRTID(K)

  !.... Invalid BRUST tree ID.

  if (IDTR.le.0) then
     K = K+1
     go to 100
  end if

  !.... Call binary search routine to find the position of the BRUST
  !.... tree ID that matches the FVS tree ID.

  call I4PBSR(IREC1,IDTREE,IND1,IDTR,IPOS)

  !.... If tree ID cannot be found in the canker file that matches the
  !.... tree ID in the tree data file, then delete the canker info.

  if (IPOS.le.0) then
     IERR = IERR+1
     do J = 1,10
        DUP(J,K) = 0.0
        DOUT(J,K) = 0.0
        GIRDL(J,K) = 0.0
        123 continue
     end do
     ILCAN(K) = 0
     ITCAN(K) = 0
     IBRTID(K) = 0
     BRAGE(K) = 0.0
     ISTOTY(K) = 0
     K = K+1
     go to 100
  end if

  !.... If cankers are already in their proper position based on the
  !.... input data (a chance occurrance) then go on to next canker.

  if (IPOS.eq.K) then
     K = K+1
     go to 100
  end if

  !.... Swap canker data.

  N = MAX(ILCAN(K),ILCAN(IPOS))
  do L = 1,N

     !....    Store contents of identified position in temporary variables.

     DU1 = DUP(L,IPOS)
     DO1 = DOUT(L,IPOS)
     GI1 = GIRDL(L,IPOS)

     !....    Load BRUST values into FVS position.

     DUP(L,IPOS) = DUP(L,K)
     DOUT(L,IPOS) = DOUT(L,K)
     GIRDL(L,IPOS) = GIRDL(L,K)

     !....    Load temporary variables into position vacated by
     !....    "Load BRUST values . . "

     DUP(L,K) = DU1
     DOUT(L,K) = DO1
     GIRDL(L,K) = GI1
     150 continue
  end do

  !.... Free up indicated position by saving the values.

  IC1 = ILCAN(IPOS)
  IC2 = ITCAN(IPOS)
  IID = IBRTID(IPOS)
  AGE = BRAGE(IPOS)
  STK = ISTOTY(IPOS)

  !.... Load appropriate values into indicated position.

  ILCAN(IPOS) = ILCAN(K)
  ITCAN(IPOS) = ITCAN(K)
  IBRTID(IPOS) = IDTR
  BRAGE(IPOS) = BRAGE(K)
  ISTOTY(IPOS) = ISTOTY(K)

  !.... Store values that used to be in position IPOS

  ILCAN(K) = IC1
  ITCAN(K) = IC2
  IBRTID(K) = IID
  BRAGE(K) = AGE
  ISTOTY(K) = STK

  !.... Process next tree ID.

  go to 100

  !.... Common return.

  200 continue

  !.... If there are IDs that have no match then print error message.

  if (IERR.gt.0) write (JOSTND,250) IERR
  250 format(/,'**** WARNING ****', I5,' Canker IDs have been found',&
       &' that have no match in the tree ID list.  These cankers'&
       &' will be ignored.')

  if (DEBUG) write(JOSTND,255) ICYC
  255 format('Leaving subroutine BRSOR: cycle = ',I2)
  300 continue
  return
end
