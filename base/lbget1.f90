!============================!
!   Read label set members   !
!============================!
! BASE $Id$
subroutine LBGET1 (IREAD,IRECNT,RECORD,IRPS,LENMEM,MEM,KIND,KODE)
  implicit none

  !     READ RECORDS FORM IREAD AND PUT THE FIRST MEMBER IN
  !     STRING MEM WITH LENGTH LENMEM.  THIS ROUTINE IS DESIGNED
  !     TO CREATE TWO KINDS OF MEMBERS, AS DEFINED BELOW (SEE KIND).

  !     PART OF THE LABEL PROCESSING COMPONENT OF THE PROGNOSIS SYSTEM
  !     N.L. CROOKSTON -- INTERMOUNTAIN RESEARCH STATION -- JAN 1987

  !     IREAD = DATA SET REFERENCE NUMBER
  !     IRECNT= NUMBER OF RECORDS READ ON IREAD.
  !     RECORD= THE READ RECORD.
  !     IRPS  = FIRST POSITION IN RECORD WHERE THE LABEL MAY BE.
  !             IF IRPS=0 ON INPUT, A RECORD IS READ.
  !     LENMEM= THE LENGTH OF THE STRING HOLDING THE MEMBER
  !     MEM   = THE MEMBER.
  !     KIND  = 0 FOR A NORMAL LABEL (PERIODS ARE DELETED),
  !             1 FOR A HARVEST POLICY LABEL (ONE PERIOD IS KEPT).
  !     KODE  = 0 PROCESSING ENDED WITHOUT ERROR.
  !             1 STRING MEM IS OVER FULL.
  !             2 END-OF-DATA HAS OCCURRED IN IREAD.

  ! Variables
  character(len=250) :: MEM
  character(len=*) :: RECORD
  logical :: LBKADD,LPERD,LPDONE
  integer :: KODE,KIND,LENMEM,IRPS,IRECNT,IREAD,MXLEN,I,ISTLNB

  !     INITIALIZE KODE AND MXLEN

  KODE = 0
  if (KIND.eq.0) then
     MXLEN = 250
  else
     MXLEN = 40
  end if

  !     INITIALIZE LENMEM AND THE MEMBER STRING.

  LENMEM = 0
  LBKADD = .false.
  LPERD = .false.
  LPDONE = .true.
  MEM = ' '

  !     IF THE POSTION OF THE RECORD IS ZERO, THEN READ A RECORD.

  10 continue
  if (IRPS.eq.0) then
     read (IREAD,'(A)',end = 200) RECORD
     IRECNT = IRECNT+1
  end if

  !     LOOP THROUGH THE INPUT STRING AND EXTRACT THE FIRST MEMBER.

  20 continue
  IRPS = IRPS+1

  !     IF THE END OF THE RECORD IS REACHED, THE END OF THE MEMBER
  !     MUST HAVE BEEN REACHED BY DEFINITION.

  if (IRPS.gt.LEN(RECORD)) goto 300

  !     IF THE CHARACTER IS AN AMPERSAND ('&'), THEN READ ANOTHER
  !     RECORD.

  if (RECORD(IRPS:IRPS).eq.'&') then
     IRPS = 0
     goto 10
  end if

  !     IF THE LENGTH OF THE MEMBER IS ZERO, THEN SKIP ALL BLANKS,
  !     COMMAS, AND PERIODS.

  if (LENMEM.eq.0) then
     if (RECORD(IRPS:IRPS).eq.' ') goto 20
     if (RECORD(IRPS:IRPS).eq.',') goto 20
     if (RECORD(IRPS:IRPS).eq.'.') goto 20
  end if

  !     WHEN THE LENGTH IS GREATER THAN ZERO, A COMMA INDICATES THE END
  !     OF THE MEMBER.

  if (RECORD(IRPS:IRPS).eq.',') goto 300

  !     IF THE CHARACTER IS A BLANK, SET A FLAG INDICATING THAT A BLANK
  !     SHOULD BE ADDED TO THE MEMBER BEFORE ANOTHER NON-BLANK CHAR.

  if (RECORD(IRPS:IRPS).eq.' ') then
     LBKADD = .true.
     goto 20
  end if

  !     IF THE CHAR. IS A PERIOD, THEN PROCESS ACCORDING TO VALUE OF
  !     KIND. (0=NORMAL,1=HARVEST).

  if (RECORD(IRPS:IRPS).eq.'.') then
     if (KIND.eq.0) goto 20
     if (LPERD) goto 20
     LPERD = .true.
     LPDONE = .false.
     goto 20
  end if

  !     ADD THE CHARACTER TO THE STRING.
  !     IF A KIND IS 1 AND A PERIOD IS NEEDED, THEN THERE MUST BE 3
  !     CHARS AVAILABLE.
  !     IF A BLANK IS NEEDED, THEN  THERE MUST BE TWO CHARS AVAILABLE,
  !     IF A BLANK NOR A PERIOD IS NEEDED, THEN ONE CHAR IS NEEDED.

  if (LPERD .and. .not.LPDONE) then
     LPDONE = .true.
     LBKADD = .false.
     if (LENMEM+3.le.MXLEN) then
        LENMEM = LENMEM+1
        MEM(LENMEM:LENMEM+1) = '. '
        LENMEM = LENMEM+2
        MEM(LENMEM:LENMEM) = RECORD(IRPS:IRPS)
        goto 20
     else
        KODE = 1
        goto 310
     end if
  end if
  if (LBKADD) then
     if (LENMEM+2.le.MXLEN) then
        LENMEM = LENMEM+1
        MEM(LENMEM:LENMEM) = ' '
        LENMEM = LENMEM+1
        MEM(LENMEM:LENMEM) = RECORD(IRPS:IRPS)
        LBKADD = .false.
     else
        KODE = 1
        goto 310
     end if
  else
     if (LENMEM+1.le.MXLEN) then
        LENMEM = LENMEM+1
        MEM(LENMEM:LENMEM) = RECORD(IRPS:IRPS)
     else
        KODE = 1
        goto 310
     end if
  end if

  !     GET THE NEXT CHARACTER.

  goto 20
  200 continue
  KODE = 2
  300 continue
  return

  !     STRING MEM IS OVER FULL. DROP RECORDS AS NECESSARY TO GET PAST
  !     ALL OF THE REMAINING RECORDS THAT DEFINE THE LABEL.

  310 continue
  I = ISTLNB(RECORD)
  if (I.gt.0) then
     if (RECORD(I:I).eq.'&') then
        read (IREAD,'(A)',end = 200) RECORD
        IRECNT = IRECNT+1
        goto 310
     end if
  end if
  return
end subroutine LBGET1
