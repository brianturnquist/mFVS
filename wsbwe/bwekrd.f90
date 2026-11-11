!====================!
!   wsbwe : bwekrd   !
!====================!
! WSBWE $Id$
subroutine BWEKRD (INUNIT,IOUT,KEYWRD,LNOTBK,&
     &ARRAY,IRECNT,KODE,KARD)
  implicit none

  !  **BWEKRD                 DATE OF LAST REVISION:  03/23/17

  !  Also revised by K.Sheehan 7/1/96 to run on PowerStation

  !     KEYWORD CARD READER FOR THE STAND PROGNOSIS SYSTEM

  !     ARGUMENTS:
  !     INUNIT= READER REFERENCE NUMBER
  !     IOUT  = PRINTER REFERENCE NUMBER
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
  !             LESS THAN ZERO:  PARMS STATEMENT IS ON THE RECORD START-
  !             ING IN FIELD IABS(KODE).

  !  Revision History:
  !    14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.
  !    23-MAR-2017 Lance R. David (FMSC)
  !       Was not properly processing alph entries in fields. Made
  !       consistent with FVS base routine keyword reader (keyrdr.f).

  !  DECLARATIONS AND DATA STATEMENTS:

  ! Variables
  character(len=80) :: RECORD
  character(len=10) :: KARD(12)
  character(len=8) :: KEYWRD
  integer :: I,INUNIT,IOS,IOUT,IP,IRECNT,ISTLNB,J,K,KODE,NF
  real    :: ARRAY(12)
  logical :: LNOTBK(12),LCOM

  !  READ KEYWORD AND LOCATE BLANK PARAMETER FIELDS.

  LCOM = .false.
  5 continue
  !      READ (INUNIT,10,END=30) RECORD
  read (INUNIT,10,end = 30,iostat = ios) RECORD
  10 format (A80)
  IRECNT = IRECNT + 1
  if (RECORD(1:1).eq.'!') goto 5
  if (RECORD(1:1).eq.'*'.or. RECORD.eq.' ') then
     if (.not.LCOM) then
        write (IOUT,'(/)')
        LCOM = .true.
     end if
     write (IOUT,'(T13,A)') RECORD(1:ISTLNB(RECORD))
     goto 5
  else
     LCOM = .false.
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
  call BWEUKY (KEYWRD)
  KODE = 0
  do I = 1,NF
     LNOTBK(I) = KARD(I).ne.' '
     50 continue
  end do
  if (NF.lt.7) then
     KODE = -(NF+1)
     do I = NF+1,7
        LNOTBK(I) = .false.
        ARRAY(I) = 0.0
        J = I*10+1
        KARD(I) = RECORD(J:J+9)
        55 continue
     end do
  end if

  60 continue
  return
end
