!=========================================!
!   Add activities from keyword records   !
!=========================================!
! BASE $Id$
subroutine OPNEWC (KODE,JOSTND,IREAD,IDT,IACTK,KEYWRD,KARD, &
     & IPRMPT,IRECNT,ICYC)
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JULY 1988 - MOSCOW

  !     OPNEWC IS USED TO ADD USER-SPCEIFED OPTIONS TO THE ACTIVITY LIST
  !     WHEN THE OPTION USES EXPRESSIONS AS ARGUMENTS.

  !     KODE  = THE RETURN CODE WHERE:
  !             0   ALL WENT OK,
  !             1   OPTION COULD NOT BE ADDED BECAUSE OF SOME ERROR
  !     JOSTND= OUTPUT LOGICAL UNIT NUMBER.
  !     IREAD = INPUT READER LOGICAL UNIT NUMBER.
  !     IDT   = THE DATE, CYCLE, OR A ZERO (ALL CYCLES) CODE TO
  !             INDECATE WHEN THE ACTIVITY IS TO BE IMPLIMENTED.
  !     IACTK = THE ACTIVITY CODE.
  !     KEYWRD= THE KEYWORD.
  !     KARD  = ARRAY OF FIELDS STORED AS CHARACTERS.
  !     IPRMPT= FIELD NUMBER WHERE 'PARMS' STARTS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'

  ! Variables
  integer :: ICYC,IRECNT,IPRMPT,IACTK,IDT,IREAD,JOSTND,KODE,ICEX
  integer :: J,K,I,IAMP,ISTLNB,IRC,IRTNCD
  character(len=80) :: RECORD
  equivalence (RECORD,WKSTR3)
  character(len=10) :: KARD(12)
  character(len=8) :: KEYWRD
  logical :: DEBUG

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'OPNEWC',6,ICYC)

  !     MOVE THE EXPRESSION INTO CEXPRS.

  ICEX = 0
  do J = IPRMPT,7
     do K = 1,10
        ICEX = ICEX+1
        CEXPRS(ICEX) = KARD(J)(K:K)
        call UPCASE(CEXPRS(ICEX))
     end do
  end do

  !     WRITE THE FIRST LINE.

  write (JOSTND,20) KEYWRD,IDT,(CEXPRS(I),I = 1,ICEX)
  20 format (/A8,'   DATE/CYCLE=',I5,';  ',70A1)

  !     LOOK FOR AN AMPERSAND

  IAMP = 0
  do I = 1,ICEX
     if (CEXPRS(I).eq.'&') then
        IAMP = I
        goto 35
     end if
     30 continue
  end do
  35 continue

  !     IF THERE IS AN AMPERSAND, READ ANOTHER RECORD AND ADD IT TO CEXPRS

  if (IAMP.gt.0) then
     ICEX = IAMP
     read (IREAD,'(A)',end = 200) RECORD
     J = ISTLNB(RECORD)
     if (J.eq.0) J = 1
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
           call ERRGRO (.true.,4)
           KODE = 1
           return
        end if
        50 continue
     end do
     ICEX = ICEX-1
     55 continue
     write (JOSTND,'(T12,A)') RECORD(1:J)
     goto 35
  end if

  !     TRIM OFF TRAILING BLANKS.

  K = ICEX
  do I = K,1,-1
     if (CEXPRS(I).eq.' ') goto 60
     ICEX = I
     goto 65
     60 continue
  end do
  65 continue

  !     STORE THE LOCATION OF THE OPCODE IN KODE.

  KODE = ICOD

  !     COMPILE THE EXPRESSION.

  call ALGCMP(IRC,.false.,CEXPRS,ICEX,JOSTND,DEBUG,1000, &
       & IPTODO,MXPTDO,IEVCOD,ICOD,MAXCOD,PARMS,IMPL,ITOPRM,MAXPRM)

  !     IF THE RETURN CODE FROM THE COMPILATION IS NON ZERO, STORE
  !     THE ACTIVITY.

  if (IRC.gt.0) then
     KODE = 1
     call ERRGRO (.true.,12)
     return
  end if

  !     STORE THE ACTIVITY AND THE PARAMETERS; INSURE THAT THE
  !     ACTIVITY IS LISTED AS 'NOT ACCOMPLISHED'.
  !     IF THE ACTIVITY IS TO BE STORED UNTIL AN EVENT OCCURS, THEN:
  !     STORE THE POINTERS IN THE BOTTOM OF IACT.

  I = IMGL
  if (LOPEVN) I = IEPT
  IACT(I,1) = IACTK
  IACT(I,4) = 0
  IACT(I,5) = 0
  IACT(I,2) = -KODE
  IACT(I,3) = 0
  IDATE(I) = IDT
  LEVUSE = .true.

  !     IF THE ACTIVITY IS BEING STORED FOR SCHEDULING AFTER AN EVENT,
  !     THEN: UPDATE THE VALUE OF IEPT.

  if (LOPEVN) then
     IEPT = IEPT-1
  else

     !        ELSE: STORE THE POINTER IN IOPSRT AND UPDATE IMGL.

     IOPSRT(IMGL) = IMGL
     IMGL = IMGL+1
  end if
  return
  200 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  return
end subroutine OPNEWC
