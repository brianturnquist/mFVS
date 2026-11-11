!===================================!
!   Read user defined expressions   !
!===================================!
! BASE $Id$
subroutine ALGEXP (CEXPRS,LENCEX,MXEXPR,CLEFT,LCLFT,RECORD, &
     & IRECNT,IREAD,JOSTND,IRC)
  implicit none

  !     CALLED FROM EVUSRV AND HVIN (AND PERHAPS OTHER PLACES).
  !     READS USER DEFINED VARIABLES.

  !     EVENT MONITOR - N.L. CROOKSTON  - APRIL 1987
  !     FORESTRY SCIENCES LABORATORY - MOSCOW, ID 83843

  !     CEXPRS= THE EXPRESSION CHARACTER ARRAY.
  !     LENCEX= THE LENGTH OF THE EXPRESSSION.
  !     MXEXPR= THE MAX LENGTH OF CEXPRS.
  !     CLEFT = THE PORTION OF THE EXPRESSION ON THE LEFT HAND SIDE OF
  !             THE EQUAL SIGN. MAX SIZE IS 20 CHARS...RETURNED SIZE IS
  !             UPTO 8 NONBLANK CHARS ALL CHARS.
  !     LCLFT = NONBLANK LENGTH OF CLEFT.
  !     RECORD= CHARACTER STRING
  !     IREAD = READER DATA SET REFERENCE NUMBER.
  !     JOSTND= OUTPUT DATA SET REFERENCE NUMBER.
  !     IRC   = RETURN CODES: 0=OK,1='END  ' WAS FOUND.

  ! Variables
  integer :: IRC,JOSTND,IREAD,IRECNT,LCLFT,LENCEX,MXEXPR,ISTLNB
  integer :: IAMP,IEQU,I,J,I1,I2,IRTNCD
  character(len=1) :: CEXPRS(MXEXPR)
  character(len=31) :: CHRSPCL
  character(len=20) :: CLEFT
  character(len=*) :: RECORD
  character(len=5) :: end
  integer :: RECLEN
  RECLEN = LEN(RECORD)

  !     LOAD VARIABLE WITH SPECIAL CHARACTERS THAT ARE INVALID FOR USE
  !     IN USER DEFINED VARIABLE NAMES. THE 2 VALID SPECIAL CHARACTERS
  !     NOT PRESENT IN STRING BELOW ARE _ (UNDERSCORE) AND $ (DOLLAR SIGN)

  data CHRSPCL /'^!"#%&''()*+,-./:;<=>?@[\]^`{|}~'/

  !     READ THE EXPRESSION AND PLACE IT INTO THE CEXPRS ARRAY.

  1 continue
  CLEFT = ' '
  LENCEX = 0
  LCLFT = 0
  5 continue
  read (IREAD,'(A)',end = 200) RECORD
  IRECNT = IRECNT+1

  !     IF THIS RECORD DOES NOT FOLLOW AN &, THEN IT CAN BE A
  !     COMMENT IF THE FIRST CHARACTER IS * or !

  if (LENCEX.eq.0 .and.(RECORD(1:1).eq.'*' .or. &
       & RECORD(1:1).eq.'!')) then
     write (JOSTND,'(T12,A)') RECORD(1:ISTLNB(RECORD))
     goto 5
  end if

  !     CONVERT THE TO UPPER CASE...AND LOOK FOR AN AMPERSAND AND AN
  !     EQUAL SIGN.

  IAMP = 0
  IEQU = 0
  do I = 1,RECLEN
     if (RECORD(I:I).eq.'&') then
        IAMP = I
        goto 15
     end if
     if (RECORD(I:I).eq.'=') IEQU = I
     10 continue
  end do
  15 continue

  !     IF THE END OPTION HAS BEEN REACHED...RETURN.

  end = RECORD(1:5)
  do I = 1,5
     call UPCASE(end(I:I))
  end do
  if (end.eq.'END  ') then
     write (JOSTND,'(/''END'')')
     goto 100
  end if
  write (JOSTND,'(T12,A)') RECORD(1:ISTLNB(RECORD))

  !     IF AN EQUAL SIGN WAS FOUND, OR IF CLEFT IS BLANK AND AN
  !     AMPERSAND WAS FOUND, THEN LOAD CLEFT.

  if (IEQU.gt.0 .or. (LCLFT.eq.0.and.IAMP.gt.0)) then
     J = IEQU-1
     if (J.le.0) J = IAMP-1
     do I = 1,J
        if (RECORD(I:I).ne.' ') then
           LCLFT = LCLFT+1
           if (LCLFT.le.20) then
              CLEFT(LCLFT:LCLFT) = RECORD(I:I)
              call UPCASE(CLEFT(LCLFT:LCLFT))
           end if
        end if
        20 continue
     end do

     !        SCAN USER DEFINED VARIABLE NAME FOR INVALID SPECIAL CHARACTERS
     !        IN THE FIRST 8 POSITIONS, REJECT AS INVALID NAME IF FOUND.

     do I = 1,8
        do J = 1,31
           if (CLEFT(I:I) .eq. CHRSPCL(J:J)) then
              goto 310
           end if
        end do
     end do
     !        IF THE USER DEFINED VARIABLE NAME IS OVER 8 CHARS, TRUNCATE IT

     if (LCLFT.gt.8) then
        LCLFT = 8
        CLEFT(9:) = ' '
        write (JOSTND,25) RECORD(1:IEQU-1),CLEFT(1:8)
        25 format (/T12,'"',A,'" WILL BE SHORTENED TO "',A,'"')
     end if
     if (IEQU.eq.0 .and. IAMP.gt.0) goto 5
  end if

  !     PROCESS THE RIGHT HAND SIDE...IF LCLFT IS 0, THERE IS NO LEFT HAND
  !     SIDE.  GOTO 1 TO FIND ANOTHER EXPRESSION.

  if (LCLFT.le.0) goto 1

  !     FIND THE FIRST NONBLANK CHAR.

  J = RECLEN
  if (IAMP.gt.0) J = IAMP-1
  I2 = IEQU+1
  if (I2.gt.J) I2 = J
  do I = I2,J
     if (RECORD(I:I).eq.' ') goto 30
     I1 = I
     goto 40
     30 continue
  end do

  !     ALL CHARS ARE BLANK...READ ANOTHER RECORD

  goto 5
  40 continue

  !     FIND THE LAST NONBLANK CHAR.

  do I = 1,J
     I2 = J-I+1
     if (RECORD(I2:I2).ne.' ') goto 60
     50 continue
  end do
  60 continue

  !     LOAD CEXPRS

  if (LENCEX+I2+1-I1.gt.MXEXPR) goto 300
  do I = I1,I2
     LENCEX = LENCEX+1
     CEXPRS(LENCEX) = RECORD(I:I)
     call UPCASE(CEXPRS(LENCEX))
     80 continue
  end do

  !     IF AN AMPERSAND WAS REACHED, ADD ONE BLANK TO THE EXPRESSION
  !     AND BRANCH TO READ MORE OF THE EXPRESSION.

  if (IAMP.gt.0) then
     if (LENCEX.lt.MXEXPR) then
        LENCEX = LENCEX+1
        CEXPRS(LENCEX) = ' '
     end if
     goto 5
  end if
  IRC = 0
  return
  100 continue
  IRC = 1
  return
  200 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  300 continue
  call ERRGRO (.true.,5)
  goto 1
  310 continue
  call ERRGRO (.false.,39)
  goto 1
end subroutine ALGEXP
