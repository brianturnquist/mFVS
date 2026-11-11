!====================!
!   vbase : prtexm   !
!====================!
! VBASE $Id: prtexm.f 2438 2018-07-05 16:54:21Z gedixon $
subroutine PRTEXM (INPUT,IPRINT,ITITLE)
  implicit none

  !     READS THE DATA THAT COMPRISES THE EXAMPLE TREE AND STAND
  !     ATTRIBUTE TABLE FROM AN UNFORMATTED DATA SET REFERENCED BY
  !     INPUT, AND PRINTS THE TABLE IN LINE PRINTER FORMAT USING THE
  !     DATA SET REFERENCE NUMBER IPRINT. IF EITHER INPUT OR IPRINT
  !     ARE ZERO, THE ROUTINE EXITS.

  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JUNE 1980

  !     IRT   = THE UNFORMATTED RECORD TYPE IDENTIFER:

  !             0= STAND IDENTIFIER RECORD; FIRST RECORD IN FILE.
  !             1= STAND ATTRIBUTE RECORD WITHOUT A RESIDUAL RECORD
  !             2= STAND ATTRIBUTE RECORD; A RESIDUAL RECORD FOLLOWS
  !             3= THE RESIDUAL RECORD WHICH SHOULD ONLY FOLLOW A TYPE
  !                2 RECORD.
  !             4= YEAR AND PERIOD LENGTH RECORD.
  !             5= EXAMPLE TREE RECORDS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  character(len=4) :: MGTID
  character(len=26) :: NPLT
  character(len=72) :: ITITLE

  character(len=3) :: IONSP(6)

  integer :: I,I1,INPUT,IPRINT,IRT,IRTNCD,IS

  integer :: I2(2),IFRAC(6),IOICR(6)

  real    :: A5(5),DBHIO(6),DGIO(6),HTIO(6),PCTIO(6),PRBIO(6)

  !  DATA STATEMENTS:

  data IFRAC/10,30,50,70,90,100/

  if (INPUT*IPRINT .eq.0) return
  rewind INPUT

  !  SET FLAG INDICATING THAT SAMPLE TREES HAVE NOT BEEN RESELECTED.

  IS = 0

  !  READ THE FIRST RECORD, THEN WRITE HEADINGS

  read (INPUT,end = 160) IRT,NPLT,MGTID
  if (IRT .ne.0) call ERRGRO(.false.,19)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  call GHEADS (NPLT,MGTID,0,IPRINT,ITITLE)
  go to 125
  90 continue
  read (INPUT,end = 155) IRT,I1,A5
  if (IRT .ne.1.and. IRT .ne.2) call ERRGRO(.false.,19)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  select case (VARACD)

     !  EASTERN TWIGS AND ONTARIO VARIANTS

  case ('CS','LS','NE','ON')
     write (IPRINT,100) I1,A5(1),A5(2),A5(3),A5(4)
     100 format(73X,I3,6X,F4.1,4X,F6.0,4X,F5.0,6X,F5.1)
     if (IRT .eq.1) go to 125
     read (INPUT,end = 155) IRT,A5
     if (IRT .ne.3) call ERRGRO(.false.,19)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     write (IPRINT,110) A5(1),A5(2),A5(3),A5(4)
     110 format(69X,'RESIDUAL:',4X,F4.1,4X,F6.0,4X,F5.0,6X,F5.1)

     !  WESTERN AND SOUTHERN VARIANTS

  case default
     write (IPRINT,115) I1,A5
     115 format(73X,I3,6X,F4.1,4X,F6.0,4X,F5.0,6X,F5.1,3X,F6.1)
     if (IRT .eq.1) go to 125
     read (INPUT,end = 155) IRT,A5
     if (IRT .ne.3) call ERRGRO(.false.,19)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     write (IPRINT,120) A5
     120 format(69X,'RESIDUAL:',4X,F4.1,4X,F6.0,4X,F5.0,6X,F5.1,3X,F6.1)

  end select

  125 continue
  read (INPUT,end = 165) IRT,I2
  if (IRT .ne.4) call ERRGRO(.false.,19)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  !  IF DATE IS NEGETIVE, THE SAMPLE TREES WERE RESELECTED.  WRITE
  !  **, SET FLAG FOR MSG, AND SET DATE POSITIVE.

  if (I2(1).lt.0) goto 135
  write (IPRINT,130) I2
  130 format(/I4,42X,'(',I3,' YRS)'/)
  goto 145
  135 continue
  I2(1) = -I2(1)
  IS = 1
  write (IPRINT,140) I2
  140 format(/I4,' **',39X,'(',I3,' YRS)'/)
  145 continue
  read (INPUT,end = 155) IRT,IONSP,DBHIO,HTIO,IOICR,DGIO,PCTIO,PRBIO
  if (IRT .ne.5) call ERRGRO(.false.,19)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  write (IPRINT,150) (IFRAC(I),IONSP(I),DBHIO(I),HTIO(I),IOICR(I),&
       &DGIO(I),PCTIO(I),PRBIO(I),  I = 1,6)
  150 format(7X,I3,5X,A3,5X,F6.2,3X,F6.2,3X,I3,3X,F6.2,4X,F5.1,&
       &1X,F7.2)
  go to 90

  !  END OF DATA TARGETS:

  155 call ERRGRO(.false.,19)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  160 call ERRGRO (.true.,20)
  165 continue
  rewind INPUT
  if (IS.eq.1) write (IPRINT,170)
  170 format (/'** NOTE:  DUE TO HARVEST, COMPRESSION, OR REGENER',&
       &'ATION ESTABLISHMENT, NEW SAMPLE TREES WERE SELECTED.')

  return
end
