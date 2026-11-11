!===============================================!
!   Open files with platform specific options   !
!===============================================!
! BASE $Id$
subroutine MYOPEN (IUNIT,CNAME,ISTAT,IRECL,IZNUL,IFORM,IACC, &
     & ICNTRL,KODE)
  implicit none

  !     THIS ROUTINE CONTAINS MACHINE-DEPENDENT FILE OPEN COMMANDS
  !     FOR PC FORTRAN AND IBM AIX xlf

  !     IF THE NAME FIELD IS BLANK, A NAME IS CREATED USING THE
  !     UNIT NUMBER AS A SUFFIX TO THE STRING 'FILE', LIKE FILE1,
  !     FILE11, ETC.

  !     ALL OF THE FILES IN THE PRGNOSIS MODEL ARE OPENED
  !     WITH THIS ROUTINE.

  !     IUNIT = DATA SET REFERENCE NUMBER (LOGICAL UNIT NUMBER)
  !     CNAME = FILE NAME
  !     ISTAT = FILE STATUS, 1=UNKNOWN, 2=NEW, 3=OLD, 4=SCRATCH, AND
  !             5=REPLACE.
  !     IRECL = RECORD LENGTH OF THE RECORDS IN CHARACTERS (BYTES)
  !             OR WORDS, DEPENDING ON THE CONTEXT.
  !     IZNUL = 1 IF BLANKS ARE TREATED AS NULL'S, 0 IF BLANKS ARE
  !             TREATED AS ZEROS.
  !     IFORM = 1 IF FORMATTED, 2 IF UNFORMATTED
  !     IACC  = 1 IF SEQUENTIAL, 2 IF DIRECT
  !     ICNTRL= 0 IF LIST, 1 IF FORTRAN CARRIAGECONTROL.
  !     KODE  = 0 IF FILE OPEN OCCURRED WITHOUT ERROR, 1 IF ERROR.

  ! Variables
  integer :: KODE,ICNTRL,IACC,IFORM,IZNUL,IRECL,ISTAT,IUNIT
  character(len=*) :: CNAME
  character(len=7) :: CSTAT
  character(len=7) :: CCNTRL
  character(len=4) :: CZNUL
  character(len=11) :: CFORM
  character(len=11) :: CACC

  !     SET THE RETURN CODE EQUAL TO ZERO.

  KODE = 0

  goto (21,22,23,24,25),ISTAT
  21 continue
  CSTAT = 'UNKNOWN'
  goto 30
  22 continue
  CSTAT = 'NEW'
  goto 30
  23 continue
  CSTAT = 'OLD'
  goto 30
  24 continue
  CSTAT = 'SCRATCH'
  goto 30
  25 continue
  CSTAT = 'REPLACE'
  30 continue

  !     SET UP THE BLANK PARAMETER.

  if (IZNUL.eq.0) then
     CZNUL = 'ZERO'
  else
     CZNUL = 'NULL'
  end if

  !     SET UP THE FORM PARAMETER:

  if (IFORM.eq.1) then
     CFORM = 'FORMATTED'
  else
     CFORM = 'UNFORMATTED'
  end if

  !     SET UP THE ACCESS PARAMETER:

  if (IACC.eq.1) then
     CACC = 'SEQUENTIAL'
  else
     CACC = 'DIRECT'
  end if

  !     SET UP CARRIAGE CONTROL FOR DG AND OTHER SYSTEMS THAT USE IT.

  if (ICNTRL.eq.1) then
     CCNTRL = 'FORTRAN'
  else
     CCNTRL = 'LIST'
  end if

  !     PC PROFORT, LAHEY F77L-LF95, AND MICROSOFT VERSION 4

  !     DIXON ADDED CARRIAGECONTROL TO FORMATTED OPENS 7-20-95 TO
  !     ELIMINATE THE 1 CHARACTER OFFSET ON DG VS PC OUTPUTS. THIS
  !     WORKS FOR LAHEY BUT MAY NOT WITH PROFORT OR MICROSOFT.

  !     FRESH IS TEMPORARILY MAPPED TO UNKNOWN
  !*    IF(ISTAT.EQ.5) CSTAT='UNKNOWN'

  !*    IF (IACC.EQ.1) THEN
  !*       IF (ISTAT.NE.4) THEN
  !*          IF (IFORM.EQ.1) THEN
  !  (FORMATTED),(NOT SCRATCH),(SEQUENTIAL)
  !*               OPEN (UNIT=IUNIT,FILE=CNAME,STATUS=CSTAT,
  !*   >               BLANK=CZNUL,ACCESS=CACC,FORM=CFORM,ERR=100,
  !*   >               CARRIAGECONTROL=CCNTRL)
  !*          ELSE
  !  (UNFORMATTED),(NOT SCRATCH),(SEQUENTIAL)
  !*             OPEN (UNIT=IUNIT,FILE=CNAME,STATUS=CSTAT,
  !*   >               ACCESS=CACC,FORM=CFORM,ERR=100)
  !*          ENDIF
  !*       ELSE
  !*          IF (IFORM.EQ.1) THEN
  !  (FORMATTED),(SCRATCH),(SEQUENTIAL)
  !*             OPEN (UNIT=IUNIT,STATUS=CSTAT,
  !*   >               BLANK=CZNUL,ACCESS=CACC,FORM=CFORM,ERR=100,
  !*   >               CARRIAGECONTROL=CCNTRL)
  !*          ELSE
  !  (UNFORMATTED),(SCRATCH),(SEQUENTIAL)
  !*             OPEN (UNIT=IUNIT,STATUS=CSTAT,
  !*   >               ACCESS=CACC,FORM=CFORM,ERR=100)
  !*          ENDIF
  !*       ENDIF
  !*    ELSE
  !*         IF (ISTAT.NE.4) THEN
  !*          IF (IFORM.EQ.1) THEN
  !  (FORMATTED),(NOT SCRATCH),(DIRECT)
  !*             OPEN (UNIT=IUNIT,FILE=CNAME,STATUS=CSTAT,RECL=IRECL,
  !*   >               BLANK=CZNUL,ACCESS=CACC,FORM=CFORM,ERR=100,
  !*   >               CARRIAGECONTROL=CCNTRL)
  !*          ELSE
  !  (UNFORMATTED),(NOT SCRATCH),(DIRECT)
  !*             OPEN (UNIT=IUNIT,FILE=CNAME,STATUS=CSTAT,RECL=IRECL,
  !*   >               ACCESS=CACC,FORM=CFORM,ERR=100)
  !*          ENDIF
  !*       ELSE
  !*          IF (IFORM.EQ.1) THEN
  !  (FORMATTED),(SCRATCH),(DIRECT)
  !*             OPEN (UNIT=IUNIT,STATUS=CSTAT,RECL=IRECL,
  !*   >               BLANK=CZNUL,ACCESS=CACC,FORM=CFORM,ERR=100,
  !*   >               CARRIAGECONTROL=CCNTRL)
  !*          ELSE
  !  (UNFORMATTED),(SCRATCH),(DIRECT)
  !*             OPEN (UNIT=IUNIT,STATUS=CSTAT,RECL=IRECL,
  !*   >               ACCESS=CACC,FORM=CFORM,ERR=100)
  !*          ENDIF
  !*       ENDIF
  !*    ENDIF

  !     IBM-AIX xlf FORTRAN (RS6000).

  if (IACC.eq.1) then
     if (ISTAT.ne.4) then
        open (UNIT = IUNIT,FILE = CNAME,STATUS = CSTAT, &
             & BLANK = CZNUL,ACCESS = CACC,FORM = CFORM,ERR = 100)
     else
        if (IFORM.eq.1) then
           open (UNIT = IUNIT,STATUS = CSTAT, &
                & BLANK = CZNUL,ACCESS = CACC,FORM = CFORM,ERR = 100)
        else
           open (UNIT = IUNIT,STATUS = CSTAT, &
                & ACCESS = CACC,FORM = CFORM,ERR = 100)
        end if
     end if
  else
     if (ISTAT.ne.4) then
        if (IFORM.eq.1) then
           open (UNIT = IUNIT,FILE = CNAME,STATUS = CSTAT,RECL = IRECL, &
                & BLANK = CZNUL,ACCESS = CACC,FORM = CFORM,ERR = 100)
        else
           open (UNIT = IUNIT,FILE = CNAME,STATUS = CSTAT,RECL = IRECL, &
                & ACCESS = CACC,FORM = CFORM,ERR = 100)
        end if
     else
        if (IFORM.eq.1) then
           open (UNIT = IUNIT,STATUS = CSTAT,RECL = IRECL, &
                & BLANK = CZNUL,ACCESS = CACC,FORM = CFORM,ERR = 100)
        else
           open (UNIT = IUNIT,STATUS = CSTAT,RECL = IRECL, &
                & ACCESS = CACC,FORM = CFORM,ERR = 100)
        end if
     end if
  end if

  !     DONE.  RETURN

  return
  100 continue
  KODE = 1
  return
end subroutine MYOPEN
