!=====================================!
!   Compute overlapping crown cover   !
!=====================================!
! BASE $Id$
subroutine COVOLP (DEBUG,JOSTND,NTREES,INDEX,CRAREA,COVER,CCCOEF)
  implicit none

  !     N.L.CROOKSTON - RMRS MOSCOW - OCTOBER 1997

  !     OUTPUT:
  !     COVER = PERCENT COVER THAT ACCOUNTS FOR CROWN OVERLAP

  !     INPUT:
  !     DEBUG = .TRUE. IF DEBUGING OUTPUT IS REQUESTED.
  !     JOSTND= THE DATA SET REF NUMBER FOR OUTPUT.
  !     NTREES= THE NUMBER OF VALUES IN INDEX (COULD BE EQUAL TO ITRN).
  !     INDEX = A VECTOR OF TREE RECORD POINTERS THAT WILL BE INCLUDED
  !             IN THE CALCULATIONS (COULD BE PASSED AS IND).
  !             IF INDEX(1) IS ZERO, THE INDICES ARE NOT USED.
  !     CRAREA= LOADED WITH THE CROWN AREA (PER ACRE) PROJECTED BY EACH
  !             TREE RECORD
  !     CCCOEF= USER-SPECIFIED COEFFICIENT FOR SPECIFYING THE DISTRIBUTION
  !       OF THE TREE RECORDS %CC IS BEING CALCULATED FOR.

  ! Variables
  real    :: CRAREA(*),COVER,SUM,CCCOEF,PCCU
  integer :: INDEX(*),NTREES,JOSTND,I
  logical :: DEBUG

  COVER = 0.
  SUM = 0.

  if (DEBUG) write (JOSTND,'('' IN COVOLP, NTREES ='',I4)') &
       & NTREES

  if (NTREES.eq.0) goto 30

  if (INDEX(1).eq.0) then
     do I = 1,NTREES
        SUM = SUM+CRAREA(I)
     end do
  else
     do I = 1,NTREES
        SUM = SUM+CRAREA(INDEX(I))
     end do
  end if

  PCCU = CCCOEF*(SUM/43560.0)

  if (PCCU.gt.5.) then
     COVER = 100.
  else
     COVER = (1.0-EXP(-PCCU))*100.0
  end if

  !      IF(COVER.GT.PCCU)COVER=PCCU

  30 continue
  if (DEBUG) write (JOSTND,40) SUM,COVER,CCCOEF
  40 format (' SUM=',E14.7,'; COVER=',F8.1,'; CCCOEF=',F8.6)
  return
end subroutine COVOLP
