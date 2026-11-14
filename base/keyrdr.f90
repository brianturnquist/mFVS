!==========================!
!   Read keyword records   !
!==========================!
! BASE $Id$
subroutine KEYRDR (INUNIT,IOUT,LDEBUG,KEYWRD,LNOTBK, &
     & ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)
  implicit none

  !     KEYWORD CARD READER FOR THE STAND PROGNOSIS SYSTEM

  !     ARGUMENTS:
  !     INUNIT= READER REFERENCE NUMBER
  !     IOUT  = PRINTER REFERENCE NUMBER
  !     LDEBUG= TRUE IF DEBUGGING.
  !     KEYWRD= CHARACTER*8 KEYWORD.
  !     LNOTBK= DIMENSION 7, VALUES ARE TRUE IF THE CORRESPONDING
  !             NUMERIC FIELD IS NOT BLANK, FALSE IF THEY ARE.
  !     ARRAY = DIMENSION 7, VALUES READ FROM THE NUMERIC FIELD.
  !     IRECNT= RECORD COUNTER.
  !     KODE  = RETURN CODE:
  !             0= NO ERRORS FOUND.
  !             1= FIRST COLUMN OF CARD WAS BLANK OR INVALID CHAR DATA
  !                WAS FOUND.
  !             2= END-OF-FILE WAS FOUND.
  !             3= STOP WAS FOUND.
  !             LESS THAN ZERO:  PARMS STATEMENT IS ON THE RECORD START-
  !             ING IN FIELD IABS(KODE).
  !     KARD  = A CHARACTER IMAGE OF THE INPUT FIELDS
  !     LFLAG = .TRUE. IF THE HEADING NEEDS TO BE CREATED, FALSE OTHERWISE

  !  DECLARATIONS AND DATA STATEMENTS:

  ! Variables
  integer :: KODE,IRECNT,IOUT,INUNIT,I,ISTLNB,NF,IP,J,K
  real    :: ARRAY(12)
  character(len=110) :: CKEYFN
  common /CKEYFN/ CKEYFN
  character(len=130) :: RECORD
  character(len=10) :: KARD
  character(len=8) :: KEYWRD
  character(len=8) :: TMP
  character(len=*) :: CFN
  logical :: LDEBUG,LNOTBK,LCOM,LFLAG,LKECHO
  dimension KARD(12),LNOTBK(12)

  !  READ KEYWORD AND LOCATE BLANK PARAMETER FIELDS.

  LCOM = .false.
  5 continue
  read (INUNIT,10,end = 30) RECORD
  10 format (A130)
  IRECNT = IRECNT + 1
  if (RECORD(1:1).eq.'!') goto 5
  if (LFLAG .and. RECORD.eq.' ') goto 5

  !     CHECK FOR "COMMENT" KEYWORD AND PROCESS.

  TMP = RECORD(1:8)
  do I = 1,8
     call UPCASE (TMP(I:I))
  end do
  if (TMP.eq.'STOP') then
     if (.not.LFLAG) write (IOUT,'(/'' STOP'')')
     KODE = 3
     return
  end if
  if (LFLAG) then
     call GROHED (IOUT)
     write (IOUT,12)TRIM(CKEYFN)
     12 format (/130('-')//T49,'OPTIONS SELECTED BY INPUT'// &
          & 'KEYWORD FILE NAME: ',A/ &
          & 130('-')/'KEYWORD    PARAMETERS:'/ &
          & '--------   ',119('-'))
     LFLAG = .false.
  end if
  if (RECORD(1:1).eq.'*'.or. RECORD.eq.' ') then
     if (.not.LCOM) then
        write (IOUT,'(/)')
        LCOM = .true.
     end if
     write (IOUT,'(T12,A)') RECORD(1:MAX(1,ISTLNB(RECORD)))
     goto 5
  else
     LCOM = .false.
  end if
  if (TMP.eq.'COMMENT') then
     if (LKECHO)write (IOUT,'(/A)') TMP
     14 continue
     read (INUNIT,'(A)',end = 30) RECORD
     IRECNT = IRECNT+1
     TMP = RECORD (1:4)
     do I = 1,4
        call UPCASE (TMP(I:I))
     end do
     if (TMP(1:4).eq.'END ') then
        if (LKECHO)write (IOUT,'(/A4)') TMP(1:4)
        goto 5
     else
        if (LKECHO)write (IOUT,'(T12,A)') RECORD(1:ISTLNB(RECORD))
        goto 14
     end if
  end if

  !     CHECK FOR THE PRESENCE OF A 'P' SO THAT THE PARMS STATEMENT
  !     MAY BE DETECTED.

  NF = 12
  K = 11
  do while (K <= 73)
     IP = INDEX(RECORD(K:73),'P')
     if (IP.eq.0) IP = INDEX(RECORD(K:73),'p')
     if (IP.gt.0) then

        !        BORROW THE USE OF KEYWRD TO HOLD "PARMS" IN MIXED, THEN UPPER
        !        CASE...

        KEYWRD(1:5) = RECORD(IP+K-1:IP+K+4)
        do I = 1,5
           call UPCASE (KEYWRD(I:I))
        end do
        if (KEYWRD(1:5).eq.'PARMS') then
           IP = K+IP-11
           NF = (IP-1)/10
           exit
        end if
        K = K+IP
        cycle
     else
        exit
     end if
  end do

  !     LOAD THE KEYWORD INTO KEYWRD...DECODE THE FIELDS.

  KEYWRD = RECORD(1:8)
  J = 1
  do I = 1,NF
     J = J+10
     KARD(I) = RECORD(J:J+9)
     ARRAY(I) = 0.0

     !     MAKE SURE ALL OF THE CHARACTERS IN KARD ARE NUMBERS...

     do K = 1,10
        if (INDEX(' .+-eE0123456789',KARD(I)(K:K)).eq.0) goto 25
        15 continue
     end do
     read (KARD(I),'(G10.0)',ERR = 25) ARRAY(I)
     25 continue
  end do
  goto 40

  30 continue
  KODE = 2
  return

  40 continue
  call UPKEY(KEYWRD)
  KODE = 0
  do I = 1,NF
     LNOTBK(I) = KARD(I).ne.' '
     50 continue
  end do
  if (NF.lt.12) then
     KODE = -(NF+1)
     do I = NF+1,7
        LNOTBK(I) = .false.
        ARRAY(I) = 0.0
        J = I*10+1
        KARD(I) = RECORD(J:J+9)
        55 continue
     end do
  end if

  if ( LDEBUG ) call KEYDMP (IOUT,IRECNT,KEYWRD,ARRAY,KARD)
  return

  !     KEYWORD FILE NAME FROM FILOPN FOR WRITING TO OUTPUT FILE

  entry KEYFN(CFN)
  CKEYFN = ' '
  I = LEN_TRIM(CFN)
  if (I.gt.LEN(CKEYFN)) then
     CKEYFN = CFN(1:4)//'...'//CFN(I+8-LEN(CKEYFN):)
  else
     CKEYFN = CFN
  end if
  return
end subroutine KEYRDR
