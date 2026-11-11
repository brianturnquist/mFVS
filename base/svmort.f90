!=================================!
!   Stand visualization: SVMORT   !
!=================================!
! BASE $Id$
subroutine SVMORT (ICALLER, CUMMORT, ICURYEAR)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999
  !     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
  !     L.R.DAVID     -- FHTET FORT COLLINS -- JULY 2005

  !     INPUT:
  !     ICALLER = IDENTIFICATION OF THE CALLER.
  !               0 - CALLED FROM GRINIT FOR FINAL MORTALITY...MORTALITY
  !                   COMES FROM WK2...AND THIS IS THE LAST CALL FOR
  !                   THE CYCLE (IMORTCNT IS RESET TO ZERO).
  !               1 - CALLED FROM ELSEWHERE FOR INCREMENTAL MORTALITY
  !                   AS STORED IN CUMMORT (THESE CALLS MUST COME PRIOR
  !                   TO ANY CALLS WHERE ICALLER IS 0).
  !               2 - CALLED FROM WESTWIDE PINE BEETLE FOR INCREMENTAL
  !     CUMMORT = VECTOR OF CUMMULATIVE TREE MORTIALITY WHEN ICALLER
  !               IS 1 OR 2, AND IS IGNORED WHEN ICALLER IS 0. ONLY THE
  !               ADDITIONAL MORTALITY FROM THE LAST CALL IS PROCESSED.

  !     UPDATES THE VISUALIZATION FOR MORTALITY. THIS CALL
  !     ASSUMES THAT ALL THE TREES THAT "DIE" ARE STANDING
  !     SNAGS.

  !     CALLED FROM GRADD JUST BEFORE UPDATE.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'
  include 'SVDATA.F77'

  !     SUM= TOTAL CHANGE IN MORT THIS CALL
  !     CREATE DUMMY VERSIONS OF SSNG,DSNG,CTCRWN

  ! Variables
  real    :: SUM,TEMP
  integer :: ICURYEAR,ICALLER,I
  real    :: SSNG(1),DSNG(1),CTCRWN(1)
  real    :: CUMMORT(*)
  SUM = 0.

  !     CHECK TO SEE IF MORT HAS NOT BEEN DONE YET THIS CYCLE.
  !     IF IT HAS NOT, PROCESS ALL CHANGES.

  if (ICALLER .eq. 0) then
     if (IMORTCNT .eq. 0) then

        !           INITIALIZE SVMSAVE WITH WK2'S VALUES, AND FIND THE SUM
        !           OF ALL THE VALUES IN WK2.

        do I = 1, ITRN
           SVMSAVE(I) = WK2(I)
           SUM = SUM + WK2(I)
        end do
     else

        !           MODIFY WK2, REMOVING MORTALITY THAT HAS ALREADY BEEN DONE
        !           DURING THIS CYCLE.  THIS NEW VALUE WILL BE PASSED INTO
        !           SVRMOV, AND THEN WK2 WILL HAVE THE OLD VALUE ADDED BACK IN.

        do I = 1, ITRN
           TEMP = WK2(I)
           WK2(I) = WK2(I) - SVMSAVE(I)
           if (WK2(I).lt. 0.0) WK2(I) = 0.0
           SUM = SUM + WK2(I)
           SVMSAVE(I) = TEMP
        end do

     end if
  else

     !        IF MORT HAS ALREADY BEEN DONE DURING THIS CYCLE, THEN ONLY
     !        PROCESS THE NEW MORTALITY.

     if (IMORTCNT .eq. 0) then

        !           INITIALIZE SVMSAVE WITH CUMMORT'S VALUES, AND FIND THE
        !           SUM OF ALL THE VALUES IN CUMMORT.

        do I = 1, ITRN
           SVMSAVE(I) = CUMMORT(I)
           SUM = SUM + CUMMORT(I)
        end do
     else

        !           MODIFY CUMMORT, REMOVING MORTALITY THAT HAS ALREADY BEEN DONE
        !           DURING THIS CYCLE.  THIS NEW CUMMORT WILL BE PASSED INTO
        !           SVRMOV, AND THEN CUMMORT WILL HAVE THE OLD VALUE ADDED BACK IN.

        do I = 1, ITRN
           TEMP = CUMMORT(I)
           CUMMORT(I) = CUMMORT(I) - SVMSAVE(I)
           if (CUMMORT(I).lt.0.0) CUMMORT(I) = 0.0
           SUM = SUM + CUMMORT(I)
           SVMSAVE(I) = TEMP
        end do
     end if

     !        INCREMENT IMORTCNT TO SHOW THAT THE NUMBER OF TIMES SVMORT
     !        HAS BEEN CALLED HAS INCREASED.

     IMORTCNT = IMORTCNT + 1

  end if

  !     THIS ROUTINE WILL ONLY CALL SVRMOV WHEN THERE IS SOME AMOUNT OF CHANGE.

  if (SUM .gt. 0.) then
     if (ICALLER .eq. 0) then
        call SVRMOV (WK2,2,SSNG,DSNG,CTCRWN,ICURYEAR)
     else
        if (ICALLER .eq. 1) then
           I = 1
        else if (ICALLER .eq. 2) then
           I = 3
        end if
        call SVRMOV (CUMMORT,I,SSNG,DSNG,CTCRWN,ICURYEAR)
     end if
  end if

  !     RESTORE THE VALUES STORED IN THE CUMMORT/WK2 ARRAY.

  if (ICALLER .eq. 0) then
     do I = 1, ITRN
        WK2(I) = SVMSAVE(I)
     end do

     !        RESET THE MORTALITY CALL COUNTER TO 0...

     IMORTCNT = 0

  else
     do I = 1, ITRN
        CUMMORT(I) = SVMSAVE(I)
     end do
  end if

  return
end subroutine SVMORT
