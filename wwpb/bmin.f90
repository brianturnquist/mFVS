!=================!
!   wwpb : bmin   !
!=================!
! WWPB $Id$
subroutine BMIN(LKECHO)
  !      IMPLICIT NONE

  !     OPTION PROCESSOR FOR STAND-LEVEL WWPB MODEL KEYWORDS
  !     CREATED 8/18/05 AJ McMAHAN, ITX, Inc. Ft. Collins, CO
  !     CALLED FROM: INITRE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'
  include 'BMPCOM.f90'

  ! Variables
  integer :: KWCNT
  parameter (KWCNT = 10)

  character(len=8) :: TABLE(KWCNT)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=10) :: KARD(12)
  character(len=250) :: OUTNAM
  logical :: LNOTBK(12),LKECHO
  integer :: NPRMS,IDT,NUMBER,KODE,IPRMPT,MYACT,KEY
  real    :: PRMS(7),ARRAY(12)

  data TABLE /&
       &'MAINOUT ','TREEOUT ','BKPOUT  ','VOLOUT  ','END     ',&
       &'        ','        ','        ','        ','        '/

  !     **********            EXECUTION BEGINS            **********

  10 continue
  call KEYRDR (IREAD,JOSTND,.false.,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
  !               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.

  if (KODE.lt.0) then
     IPRMPT = -KODE
  else
     IPRMPT = 0
  end if
  if (KODE .le.0) go to 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call ERRGRO (.true.,6)
  goto 10
  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.false.,JOSTND)

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

  if (KODE .eq.0) goto 90
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if
  goto 90

  !     SPECIAL END-OF-FILE TARGET

  80 continue
  call ERRGRO (.false.,2)
  90 continue

  !     PROCESS OPTIONS

  go to( 100,200,300,400,500,600,700,800,900,1000),NUMBER

  100 continue

  !                        OPTION NUMBER 1 -- MAINOUT

  OUTNAM = KWDFIL(:ISTLNB(KWDFIL))//'.bmm'

  !     IF THE FILE HASN'T BEEN OPENED YET, OPEN IT
  if (.not. LBMAIN) then
     call MYOPEN (JBMAIN,OUTNAM,1,133,0,1,1,0,KODE)
     if (KODE.gt.0) then
        write (*,'('' OPEN FAILED FOR '')') OUTNAM
     end if
     LBMAIN = .true.
  end if

  IDT = 1
  PRMS(1) = 100
  PRMS(2) = 5.0
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  if (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
  if (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))

  if (LKECHO)write(JOSTND,115) KEYWRD,OUTNAM,JBMAIN,IDT,INT(PRMS(1)),&
       &INT(PRMS(2))
  115 format(/1X,A8,'   THE WWPB MODEL *MAIN* OUTPUT FILE: ',A,&
       &'(UNIT REFERENCE #: ',I3,')'/T13,'WILL BEGIN',&
       &' PRINTING IN DATE/CYCLE ',I4,', FOR ',I4,&
       &' YEARS USING ',I2,' YEAR INCREMENTS.')

  NPRMS = 2
  MYACT = 2701 !
  call OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
  goto 10

  200 continue

  !                        OPTION NUMBER 2 -- TREEOUT

  OUTNAM = KWDFIL(:ISTLNB(KWDFIL))//'.bmt'

  !     IF THE FILE HASN'T BEEN OPENED YET, OPEN IT
  if (.not. LBMTRE) then
     call MYOPEN (JBMTRE,OUTNAM,1,133,0,1,1,0,KODE)
     if (KODE.gt.0) then
        write (*,'('' OPEN FAILED FOR '')') OUTNAM
     end if
     LBMTRE = .true.
  end if

  IDT = 1
  PRMS(1) = 100
  PRMS(2) = 5.0
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  if (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
  if (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))

  if (LKECHO)write(JOSTND,215) KEYWRD,OUTNAM,JBMTRE,IDT,INT(PRMS(1)),&
       &INT(PRMS(2))
  215 format(/1X,A8,'   THE WWPB MODEL *DETAILED TREE* OUTPUT FILE: ',A,&
       &'(UNIT REFERENCE #: ',I3,')'/&
       &T13,'WILL BEGIN PRINTING IN DATE/CYCLE ',I4,', FOR ',I4,&
       &' YEARS USING ',I2,' YEAR INCREMENTS.')

  NPRMS = 2
  MYACT = 2702 !
  call OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
  goto 10

  300 continue

  !                        OPTION NUMBER 3 -- BKPOUT

  OUTNAM = KWDFIL(:ISTLNB(KWDFIL))//'.bmb'

  !     IF THE FILE HASN'T BEEN OPENED YET, OPEN IT
  if (.not. LBMBKP) then
     call MYOPEN (JBMBKP,OUTNAM,1,133,0,1,1,0,KODE)
     if (KODE.gt.0) then
        write (*,'('' OPEN FAILED FOR '')') OUTNAM
     end if
     LBMBKP = .true.
  end if

  IDT = 1
  PRMS(1) = 100
  PRMS(2) = 5.0
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  if (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
  if (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))

  if (LKECHO)write(JOSTND,315) KEYWRD,OUTNAM,JBMBKP,IDT,INT(PRMS(1)),&
       &INT(PRMS(2))
  315 format(/1X,A8,'   THE WWPB MODEL *DETAILED BKP* OUTPUT FILE: ',A,&
       &'(UNIT REFERENCE #: ',I3,')'/&
       &T13,'WILL BEGIN PRINTING IN DATE/CYCLE ',I4,', FOR ',I4,&
       &' YEARS USING ',I2,' YEAR INCREMENTS.')

  NPRMS = 2
  MYACT = 2703 !
  call OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
  goto 10

  400 continue

  !                        OPTION NUMBER 4 -- VOLOUT

  OUTNAM = KWDFIL(:ISTLNB(KWDFIL))//'.bmv'

  !     IF THE FILE HASN'T BEEN OPENED YET, OPEN IT
  if (.not. LBMVOL) then
     call MYOPEN (JBMVOL,OUTNAM,1,133,0,1,1,0,KODE)
     if (KODE.gt.0) then
        write (*,'('' OPEN FAILED FOR '')') OUTNAM
     end if
     LBMVOL = .true.
  end if

  IDT = 1
  PRMS(1) = 100
  PRMS(2) = 5.0
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  if (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
  if (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))

  if (LKECHO)write(JOSTND,415) KEYWRD,OUTNAM,JBMVOL,IDT,INT(PRMS(1)),&
       &INT(PRMS(2))
  415 format(/1X,A8,'   THE WWPB MODEL *DETAILED VOLUME* OUTPUT FILE: ',&
       &A,'(UNIT REFERENCE #: ',I3,')'/&
       &T13,'WILL BEGIN PRINTING IN DATE/CYCLE ',I4,', FOR ',I4,&
       &' YEARS USING ',I2,' YEAR INCREMENTS.')

  NPRMS = 2
  MYACT = 2704 !
  call OPNEW(KODE,IDT,MYACT,NPRMS,PRMS)
  goto 10

  500 continue

  !                        OPTION NUMBER 5 -- END

  if (LKECHO)write(JOSTND,510) KEYWRD
  510 format (/1X,A8,'   END OF SINGLE-STAND WWPB OPTIONS.')
  goto 2000

  600 continue
  !                       OPTION NUMBER 6--

  goto 10

  700 continue
  !                       OPTION NUMBER 7--

  goto 10

  800 continue
  !                       OPTION NUMBER 8--

  goto 10

  900 continue
  !                       OPTION NUMBER 9--

  goto 10

  1000 continue
  !                       OPTION NUMBER 10--

  goto 10

  2000 continue
  return

  !     CALL FROM OPLIST

  entry BMKEY (KEY,PASKEY)
  PASKEY = TABLE(KEY)
  return

end
