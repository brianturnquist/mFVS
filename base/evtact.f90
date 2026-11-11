!===============================!
!   Link activities to events   !
!===============================!
! BASE $Id$
subroutine EVTACT
  implicit none

  !     ESTABLISHES THE LINKAGE BETWEEN AN EVENT, REFERENCED BY THE MOST
  !     RECENTLY ASSIGNED INTERNAL EVENT NUMBER (IENV), AND A GROUP OF
  !     ACTIVITIES.

  !     EVENT MONITOR ROUTINE - NL CROOKSTON - AUG 1982 - MOSCOW, ID
  !     MODIFIED TO ADD LABEL PROCESSING IN JAN 1987

  !     ENTRY   DESCRIPTION

  !     EVTHEN  CALLED WHEN THE THEN KEYWORD IS SPECIFIED, THE OPTION
  !             ACTIVITY KEYWORDS WHICH FOLLOW ARE LINKED TO AN EVENT
  !             NUMBER AND STORED FOR FUTURE SCHEDULING.
  !     EVALSO  CALLED WHEN ALSOTRY KEYWORD IS SPECIFIED.
  !             CLOSES OFF
  !             A PREVIOUS ENTRY IN IEVACT, ESTABLISHES NEW ENTRY IN
  !             IEVACT, AND ACCOMPLISHES SOME ERROR CHECKING.
  !     EVEND   CALLED WHEN ENDIF IS SPECIFIED, CLOSES OFF PREVIOUS
  !             ENTRY IN IEVACT, RETURN ACTIVITY STORAGE MODE TO NORMAL.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'

  ! Variables
  logical :: LNOTBK(12),DEBUG,LKECHO
  character(len=8) :: KEYWRD
  character(len=10) :: KARD(12)
  character(len=80) :: RECORD
  real    :: ARRAY(12)
  integer :: IPRMPT,IRECNT,IREAD,JOSTND,ICEX,J,K,I,IAMP,IRC,IRTNCD
  equivalence (RECORD,WKSTR3)

  entry EVALSO (DEBUG,JOSTND,IREAD,IRECNT,KEYWRD,ARRAY,LNOTBK, &
       & KARD,IPRMPT)

  !     IF LOPEVN IS FALSE, THE ALSOTRY KEYWORD HAS BEEN SPECIFIED
  !     WITHOUT SPECIFYING AN EVENT AND A THEN KEYWORD, AND IS THEREFORE
  !     ILLEGAL.

  if (.not.LOPEVN) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,16)
     return
  end if

  !     CLOSE OFF THE LAST ENTRY IN IEVACT, THE EVENT/ACTIVITY LINKAGE
  !     ARRAY.  IF NO ACTIVITIES HAVE BEEN SPECIFIED, THE ACTIVITY GROUP
  !     IS TERMED A NULL ALTERNATIVE; THEN: SET THE POINTERS TO ZERO.

  if (IEVACT(IEVA,4).le.IEPT) then
     IEVACT(IEVA,4) = 0
     IEVACT(IEVA,5) = 0
  else

     !        ELSE: SET THE SECOND POINTER EQUAL TO THE LAST ACTIVITY ENTRY
     !        (NOTE THAT IEPT IS DECREMENTED AS ACTIVITIES ARE ENTERED).

     IEVACT(IEVA,5) = IEPT+1
  end if

  !     INCREMENT IEVA, SO THAT IT POINTS TO THE NEXT AVAILABLE ROW
  !     IN IEVACT

  IEVA = IEVA+1
  goto 5

  entry EVTHEN (DEBUG,JOSTND,IREAD,IRECNT,KEYWRD,ARRAY,LNOTBK, &
       & KARD,IPRMPT,LKECHO)

  !     IF LOPEVN IS TRUE AND THE LEG COUNT IS ZERO, THIS THEN IS LEGAL
  !     AS IT FOLLOWS AN IF AND NOT ANOTHER THEN OR ALSOTRY.

  if (.not.LOPEVN .or. ILGNUM.gt.0) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,16)
     return
  end if
  5 continue

  !     VERIFY THAT THERE IS ROOM IN IEVACT TO STORE THE ACTIVITIY GROUP.

  if (IEVA.gt.MAXEVA) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,17)
     return
  end if

  !     INCREMENT THE BRANCH LEG COUNTER.

  ILGNUM = ILGNUM+1

  !     IF THERE ARE OVER 9 ACTIVITY GROUPS, ISSUE ERROR MSG

  if (ILGNUM.gt.9) call ERRGRO (.true.,22)

  !     ESTABLISH AN ENTRY IN IEVACT.

  IEVACT(IEVA,1) = IEVT
  IEVACT(IEVA,2) = 0
  IEVACT(IEVA,3) = ILGNUM
  IEVACT(IEVA,4) = IEPT

  !     WRITE INITIAL KEYWORD MESSAGE.

  if (LKECHO)write(JOSTND,6) KEYWRD
  6 format (/A8,'   ACTIVITIES WHICH FOLLOW WILL NOT BE ', &
       & 'SCHEDULED UNTIL THE EVENT HAPPENS (WHEN THE ', &
       & 'LOGICAL EXPRESSION IS TRUE).')

  !     PROCESS THE BRANCH WEIGHT MULTIPLIER CODED ON THE KEYWORD.

  !     IF IPRMPT IS ZERO, CHECK ARRAY(1)...IF NOT BLANK STORE IT AS
  !     THE MULTIPLIER AND WRITE OUT A MESSAGE.

  if (IPRMPT.eq.0) then
     if (LNOTBK(1)) then
        if (IMPL.le.ITOPRM) then
           PARMS(IMPL) = ARRAY(1)
           IEVACT(IEVA,6) = IMPL
           IMPL = IMPL+1
           if (LKECHO) &
                &          WRITE(JOSTND,'(T12,''BRANCH WEIGHT MULTIPLIER = '',      &
                &                F12.5)') ARRAY(1)
        else
           IEVACT(IEVA,6) = 0
           call ERRGRO (.true.,10)
           write (JOSTND,'(T12,''MULTIPLIER IGNORED.'')')
        end if
     end if
  else

     !        MOVE THE EXPRESSION INTO CEXPRS.

     ICEX = 0
     do J = IPRMPT,7
        do K = 1,10
           ICEX = ICEX+1
           CEXPRS(ICEX) = KARD(J)(K:K)
           call UPCASE(CEXPRS(ICEX))
        end do
     end do

     !        WRITE THE FIRST LINE.

     write (JOSTND,20) (CEXPRS(I),I = 1,ICEX)
     20 format (T12,'MULTIPLIER= ',70A1)

     !        LOOK FOR AN AMPERSAND

     IAMP = 0
     do I = 1,ICEX
        if (CEXPRS(I).eq.'&') then
           IAMP = I
           goto 35
        end if
        30 continue
     end do
     35 continue

     !        IF THERE IS AN AMPERSAND, READ ANOTHER RECORD, ADD IT TO CEXPRS

     if (IAMP.gt.0) then
        ICEX = IAMP
        read (IREAD,'(A)',end = 200) RECORD
        do I = 80,1,-1
           if (RECORD(I:I).eq.' ') goto 40
           J = I
           goto 45
           40 continue
        end do
        J = 1
        45 continue
        IRECNT = IRECNT+1
        IAMP = 0
        do I = 1,J
           call UPCASE (RECORD(I:I))
           CEXPRS(ICEX) = RECORD(I:I)
           if (CEXPRS(ICEX).eq.'&') then
              IAMP = ICEX
              goto 55
           end if
           ICEX = ICEX+1
           if (ICEX.gt.MXEXPR) then
              write (JOSTND,'(T12,A)') RECORD
              call ERRGRO (.true.,4)
              IEVACT(IEVA,6) = -I
              return
           end if
           50 continue
        end do
        ICEX = ICEX-1
        55 continue
        write (JOSTND,'(T12,A)') RECORD
        goto 35
     end if

     !        TRIM OFF TRAILING BLANKS.

     K = ICEX
     do I = K,1,-1
        if (CEXPRS(I).eq.' ') goto 60
        ICEX = I
        goto 65
        60 continue
     end do
     65 continue

     !        STORE THE LOCATION OF THE OPCODE IN KODE.

     I = ICOD

     !        COMPILE THE EXPRESSION.

     call ALGCMP(IRC,.false.,CEXPRS,ICEX,JOSTND,DEBUG,1000, &
          & IPTODO,MXPTDO,IEVCOD,ICOD,MAXCOD,PARMS,IMPL,ITOPRM,MAXPRM)

     !        IF THE RETURN CODE FROM THE COMPILATION IS NON ZERO, ISSUE
     !        AN ERROR MESSAGE AND SET THE BRANCH MULT CODE TO ZERO.

     if (IRC.gt.0) then
        IEVACT(IEVA,6) = 0
        call ERRGRO (.true.,12)
     else

        !        STORE LOCATION OF THE OPERATION CODE (SET THE POINTER NEG).

        IEVACT(IEVA,6) = -I
     end if
  end if
  return
  200 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  return

  entry EVEND (DEBUG,JOSTND,IRECNT,KEYWRD,ARRAY,LNOTBK,KARD, &
       & IPRMPT,LKECHO)

  !     SET THE ACTIVITY STORAGE MODE TO NORMAL,
  !     CLOSE THE EVENT ACTIVITY GROUP LINKAGE ARRAY, AND
  !     INCREMENT THE INTERNAL EVENT NUMBER.
  !     IF THE ACTIVITY STORAGE MODE IS NORMAL, THEN THE ENDIF WAS IN
  !     THE INCORRECT SEQUENCE.

  if (.not.LOPEVN .or. ILGNUM.le.0) then

     !        IF IPRMPT IS LE -1, CALL WAS MADE WHEN A PROCESS KEYWORD
     !        WAS DETECTED. SUPPRESS WRITING AN ERROR MESSAGE.

     if (IPRMPT.le.-1) return
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,16)
  else
     LOPEVN = .false.
     ILGNUM = 0
     IEVT = IEVT+1
     if (IEVACT(IEVA,4).le.IEPT) then
        IEVACT(IEVA,4) = 0
        IEVACT(IEVA,5) = 0
     else
        IEVACT(IEVA,5) = IEPT+1
     end if
     IEVA = IEVA+1
     if (LKECHO)write(JOSTND,300) KEYWRD
     300 format (/A8,'   ACTIVITIES WHICH FOLLOW WILL BE SCHEDULED.')
  end if
end subroutine EVTACT
