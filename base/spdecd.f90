!==========================!
!   Decode species codes   !
!==========================!
! BASE $Id$
subroutine SPDECD (IPOS,ISP,CNSP,JOSTND,IRECNT, &
     & KEYWRD,ARRAY,KARD)
  implicit none

  !     DECODE THE SPECIES CODE.
  !     IPOS  = THE INDEX OF WHICH MEMBER OF KARD AND ARRAY THE SPECIES
  !             CODE IS LOCATED.
  !     ISP   = I*4 NUMERIC SPECIES CODE, -999 IF NO SPECIES FOUND.
  !     CNSP  = C*4 VECTOR OF LENGTH MAXSP, SPECIES CODES, LAST CHAR
  !             IS IGNORED.
  !     MAXSP = LENGTH OF CNSP
  !     JOSTND= STD OUTPUT.
  !     IRECNT= RECORD COUNT.
  !     KEYWRD= C*8 KEYWORD.
  !     ARRAY = R*4 ARRAY OF PARAMETERS.
  !     KARD  = C*10 ARRAY OF PARAMETERS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: IRECNT,JOSTND,ISP,IPOS,IFLAG,J,I
  character(len=10) :: KARD(12)
  character(len=8) :: KEYWRD
  character(len=4) :: CNSP(MAXSP)
  character(len=4) :: TEMP
  real    :: ARRAY(12)

  ISP = IFIX(ARRAY(IPOS))

  !  CHECK FOR SPECIES GROUP NAME. IFLAG=1 MEANS A SPECIES GROUP NAME
  !  WAS FOUND.

  IFLAG = 0
  call SGDECD(ISP,KARD(IPOS),IFLAG)
  if (IFLAG .ne. 0) then
     ARRAY(IPOS) = FLOAT(ISP)
     go to 100
  end if

  !     IF ISP IS OUT OF RANGE, THEN WE HAVE AN ERROR.

  if (ISP.ge.0 .and. ISP.le.MAXSP) then

     !        IF ISP IS ZERO, THEN THE ALPHA CODE MAY BE PRESENT.

     if (ISP.eq.0) then

        !           LOAD TEMP WITH UP TO THREE NON-BLANK CHARS IN KARD.

        TEMP = 'ALL'
        J = 0
        do I = 1,10
           if (KARD(IPOS)(I:I).ne.' ') then
              TEMP = ' '
              do J = 1,MIN(3,10-I+1)
                 if (KARD(IPOS)(I+J-1:I+J-1).eq.'0') then
                    TEMP = 'ALL'
                    goto 50
                 end if
                 TEMP(J:J) = KARD(IPOS)(I+J-1:I+J-1)
                 call UPCASE(TEMP(J:J))
                 5 continue
              end do
              goto 20
           end if
           10 continue
        end do

        !           NOTHING WAS FOUND.

        goto 50
        20 continue

        !           TRY TO DECODE THE SPECIES CODE.
        !           'ALL' IS SPECIES CODE ZERO.

        if (TEMP.ne.'ALL') then

           !              DEAL WITH ONE CHARACTER SPECIES CODES.

           if (TEMP(2:).eq.' ') then
              TEMP(2:) = TEMP(1:1)
              TEMP(1:1) = ' '
           end if
           do I = 1,MAXSP
              if (TEMP(1:2).eq.CNSP(I)(1:2)) then
                 ISP = I
                 goto 50
              end if
              30 continue
           end do

           !              NO VALID SPECIES CODE WAS FOUND.  SIGNAL AN ERROR.

           call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
           if (KEYWRD .eq. 'SPGROUP') then
              call ERRGRO (.true.,29)
           else
              call ERRGRO (.true.,4)
           end if
           ISP = -999
           KARD(IPOS) = TEMP
           return
        end if
        50 continue
        KARD(IPOS) = TEMP
        ARRAY(IPOS) = FLOAT(ISP)
     else
        ISP = IFIX(ARRAY(IPOS))
        KARD(IPOS) = CNSP(ISP)(1:2)
     end if

  else
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     if (KEYWRD .eq. 'SPGROUP') then
        call ERRGRO (.true.,29)
     else
        call ERRGRO (.true.,4)
     end if
     ISP = -999
  end if

  100 continue
  return
end subroutine SPDECD
