!===============================!
!   Compile event expressions   !
!===============================!
! BASE $Id$
subroutine EVCOMP (IRC,IREAD,JOSTND,RECORD,LDEBUG,IRECNT,LKECHO)
  implicit none

  !     CALLED FROM EVIF.  READS LOGICAL EXPRESSIONS AND CALLS
  !     ALGCMP TO COMPILE THEM.

  !     EVENT MONITOR ROUTINE - N.L. CROOKSTON  - APRIL 1987
  !     FORESTRY SCIENCES LABORATORY - MOSCOW, ID 83843

  !     IRC   = RETURN CODE: 0=OK,1=SOME ERROR.
  !     IREAD = READER DATA SET REFERENCE NUMBER.
  !     JOSTND= OUTPUT DATA SET REFERENCE NUMBER.
  !     LDEBUG= DEBUG OUTPUT IS REQUESTED.
  !     IRECNT= RECORD COUNTER.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'

  ! Variables
  integer :: IRECNT,JOSTND,IREAD,IRC,I,J,ISTLNB,I1,I2,II,IRTNCD
  logical :: LDEBUG,LKECHO
  character(len=*) :: RECORD
  integer :: LENREC
  LENREC = LEN(RECORD)

  !     READ THE LOGICAL EXPRESSION AND PLACE IT INTO THE CEXP ARRAY.

  I = 1
  if (LKECHO)write(JOSTND,'(1X)')
  5 continue
  read (IREAD,'(A)',end = 50) RECORD
  IRECNT = IRECNT+1
  do J = 1,LENREC
     call UPCASE(RECORD(J:J))
     10 continue
  end do
  if (LKECHO)write(JOSTND,'(T12,A)') RECORD(1:ISTLNB(RECORD))
  I1 = I
  I2 = I1+LENREC-1
  if (I2.gt.MXEXPR) I2 = MXEXPR
  II = 0
  do J = I1,I2
     II = II+1
     CEXPRS(J) = RECORD(II:II)
     15 continue
  end do

  !     IF II IS LESS THAN LENREC, AND CHARACTERS EXIST FROM II
  !     TO LENREC, THEY WILL NOT BE SEEN BY THE PROGRAM.
  !     ISSUE AN ERROR MESSAGE.

  if (II.lt.LENREC) then
     if (RECORD(II+1:LENREC).ne.' ') then
        write (JOSTND,'(/T12,I2,'' CHARS IGNORED: '',A)') &
             & (LENREC-II),RECORD(II+1:LENREC)
        call RCDSET (2,.true.)
     end if
  end if

  !     CHECK FOR AMPERSANDS...IF THERE IS ONE, THEN BRANCH BACK AND
  !     READ ANOTHER RECORD.

  do J = I1,I2
     if (CEXPRS(J).ne.'&') goto 20
     I = J+1
     CEXPRS(J) = ' '
     goto 5
     20 continue
  end do

  !     FIND THE LAST NON-BLANK CHARACTER.

  I = 0
  do J = 1,I2
     I = I2-J+1
     if (CEXPRS(I).ne.' ') goto 40
     30 continue
  end do
  40 continue
  if (LDEBUG) write (JOSTND,45) I,ICOD,IMPL
  45 format (' IN EVCOMP, LENGTH=',I4,'; ICOD=',I5,'; IMPL=',I5)

  !     CALL ALGCMP TO COMPILE THE EXPRESSION.

  call ALGCMP (IRC,.true.,CEXPRS,I,JOSTND,LDEBUG,1000, &
       & IPTODO,MXPTDO,IEVCOD,ICOD,MAXCOD,PARMS,IMPL,ITOPRM,MAXPRM)

  !     SET ALL NON-ZERO RETURN CODES TO 1, ISSUE A GENERAL PURPOSE
  !     ERROR MESSAGE, AND RETURN.

  if (IRC.gt.0) then
     IRC = 1
     call ERRGRO (.true.,12)
  end if
  return
  50 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  return
end subroutine EVCOMP
