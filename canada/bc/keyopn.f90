!===================================!
!   Process file opening keywords   !
!===================================!
! CANADA-BC $Id$
subroutine KEYOPN (IREAD,RECORD,JOSTND,IRECNT,KEYWRD,&
     &ARRAY,KARD)
  implicit none

  !     PROCESSES THE OPEN KEYWORD.

  !     ARGUMENTS:
  !     IREAD =THE KEYWORD READER DATA SET REFERENCE NUMBER.
  !     RECORD=CHARACTER RECORD BUFFER.
  !     JOSTND=STANDARD OUTPUT FILE NUMBER.
  !     IRECNT=RECORD COUNTER NUMBER.
  !     KEYWRD=KEYWORD.
  !     ARRAY =PARAMETERS.
  !     KARD  =CHAR*10... THE FILEDS.

  ! Variables
  integer :: IRECNT,JOSTND,IREAD,IM,IUNIT,IZNUL,IS,IFORM,IRLEN,I,KODE
  integer :: IRTNCD
  real    :: ARRAY(7)
  character(len=*) :: RECORD
  character(len=10) :: KARD(7)
  character(len=8) :: KEYWRD
  character(len=7) :: CSTAT
  character(len=4) :: CFOUR
  character(len=3) :: EXTENSION
  IM = 0

  read (IREAD,'(A)',end = 80) RECORD
  IRECNT = IRECNT+1
  if (ARRAY(1).le.0.0) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     return
  end if
  IUNIT = IFIX(ARRAY(1))
  if (ARRAY(2).eq.0) then
     IZNUL = 0
     CFOUR = 'ZERO'
  else
     IZNUL = 1
     CFOUR = 'NULL'
  end if
  IS = INT(ARRAY(3)+1.)
  if (IS.le.0.or.IS.gt.4) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     return
  end if
  if (IS.eq.1) CSTAT = 'UNKNOWN'
  if (IS.eq.2) CSTAT = 'NEW'
  if (IS.eq.3) CSTAT = 'OLD'
  if (IS.eq.4) then
     CSTAT = 'FRESH'
     IS = 5
  end if
  if (ARRAY(4).gt.0) IM = INT(ARRAY(4))
  IFORM = 1
  if (ARRAY(5).gt.0) IFORM = 2
  if (IFORM .eq.1.and. IM .lt.150) IM = 150
  call UNBLNK (RECORD,IRLEN)

  !  FORCE .KCP OR .ADD FILES TO USE IS=3 (CSTAT='OLD') SO OPEN ERROR
  !  IS GENERATED WHEN FILE DOES NOT EXIST

  EXTENSION = '   '
  do I = IRLEN,2,-1
     if (RECORD(I-1:I-1).eq. '.') then
        EXTENSION = RECORD(I:I+2)
        go to 10
     end if
  end do
  10 continue
  if ((EXTENSION.eq.'KCP').or.(EXTENSION.eq.'ADD'))IS = 3

  !     CALL MYOPEN TO OPEN THE FILE.  MACHINE SPECIFIC CODE IS IN MYOPEN

  call MYOPEN (IUNIT,RECORD,IS,IM,IZNUL,IFORM,1,0,KODE) ! US style
  !     CALL MYOPEN (IUNIT,RECORD,IS,IM,IZNUL,IFORM,1,1,KODE)  ! BC/ON style (assume deprecated 2020)

  write (JOSTND,40) KEYWRD,IUNIT,CFOUR,CSTAT,IM,IFORM,&
       &TRIM(RECORD(1:IRLEN))
  40 format (/A8,'   DATA SET REFERENCE NUMBER = ',I5,'; BLANK=',A4,&
       &'; STATUS=',A7/T12,&
       &'MAXIMUM RECORD LENGTH (IGNORED ON SOME MACHINES) =',I4,&
       &'; FILE FORM=',I2,' (1=FORMATTED, 2=UNFORMATTED)',&
       &/T12,'DATA SET NAME = ',A)

  if (((KODE.eq.1).and.(EXTENSION.eq.'KCP')).or.&
       &((KODE.eq.1).and.(EXTENSION.eq.'ADD'))) then
     call ERRGRO(.true.,31)
  else if (KODE.eq.1) then
     write (JOSTND,50)
  end if
  50 format (/T12,'**********   OPEN FAILED   **********')
  return
  80 continue
  call ERRGRO (.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  return
end
