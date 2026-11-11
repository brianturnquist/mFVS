!=====================!
!   vorganon : orin   !
!=====================!
! VORGANON $Id$
subroutine ORIN (DEBUG,LKECHO,LNOTRE)
  implicit none

  !     ORGANON EXTENSION KEYWORD PROCESSOR.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'
  include 'ORGANON.f90'

  ! Variables
  integer :: KWCNT
  parameter (KWCNT = 7)

  logical :: LNOTBK(12)
  logical :: LKECHO,LNOTRE
  logical :: DEBUG

  character(len=8) :: TABLE(KWCNT)
  character(len=8) :: KEYWRD
  character(len=10) :: KARD(12)
  character(len=80) :: FNAME
  integer :: KODE,IPRMPT,NUMBER
  integer :: ORGINP

  real    :: ARRAY(12)

  ! INITIALIZATIONS:

  data TABLE /&
       &'END     ',& ! Option 100
       &'ORGINFO ',& ! Option 200
       &'ORGVOLS ',& ! Option 300
       &'INPFILE ',& ! Option 400
       &'OSUCFVOL',& ! Option 500
       &'OSUBFVOL',& ! Option 600
       &'ORGOFF  '/ ! Option 700

  !     **********          EXECUTION BEGINS          **********

  10 continue

  ! CALL KEYWORD READER

  call KEYRDR (IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,&
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

  !  CHECK FOR VALID ORGANON KEYWORD

  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.false.,JOSTND)

  !  RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=KEYWORD NOT SPELLED CORRECTLY

  if (KODE .eq.0) goto 90
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if
  goto 90

  !     SPECIAL END-OF-FILE TARGET (USED IF YOU READ PARAMETER CARDS)

  call ERRGRO (.false.,2)

  !     PROCESS OPTIONS

  90 continue
  go to(100, 200, 300, 400, 500, 600, 700), NUMBER

  !  ==========  OPTION NUMBER  1: END       ===========================END

  100 continue
  if (LKECHO) then
     write(JOSTND,105) KEYWRD
     105 format (/A8,'   END OF ORGANON KEYWORDS')
  end if
  go to 2000

  !  ==========  OPTION NUMBER  2: ORGINFO   =======================ORGINFO

  !  F1 = MODEL TYPE = (1,2,3)
  !  F2 = UNEVEN-AGED = 0,  EVEN-AGED = 1
  !  F3 = NO MAXSDI_MORTALITY = 0, MAXSDI_MORTALITY = 1
  !  F4 = HEIGHT CALIBRATION FLAG (0 = NO, 1 = YES (DEFAULT))
  !  F5 = HEIGHT-TO-CROWN BASE CALIBRATION FLAG (0 = NO, 1 = YES (DEFAULT))
  !  F6 = DIAMETER GROWTH CALIBRATION FLAG (0 = NO, 1 = YES (DEFAULT))

  200 continue

  !  F1:
  !  ASSIGN THE MODEL TYPE. INITIALIZED TO 0 IN **GRINIT**; DEFAULT SET TO
  !  1 OR 2 DEPENDING ON VARIANT IN **SITSET** IF IT ISN'T SET HERE

  if (LNOTBK(1)) then
     IMODTY = IFIX(ARRAY(1))

     select case (VARACD)

     case ('OC')
        if (IMODTY .ne.1) then
           if (LKECHO ) write(JOSTND,202) KEYWRD
           202 format (/1X,A8,'   INVALID VERSION. USING VERSION 1 = SWO'&
                &)
           IMODTY = 1
        end if

     case default
        if ((IMODTY .lt.2) .or. (IMODTY .gt.3)) then
           if (LKECHO ) write(JOSTND,204) KEYWRD
           204 format (/1X,A8,'   INVALID VERSION. USING VERSION 2 = NWO'&
                &)
           IMODTY = 2
        end if

     end select
  end if

  !  F2:
  !  ASSIGN THE UNEVEN-AGE/EVEN-AGED FLAG FROM THE SECOND POSITION
  !  DEFAULT IS THAT THE STAND IS EVEN AGED (SET IN **GRINIT**)
  !  0=UNEVEN-AGED STAND; 1=EVEN-AGED STAND
  !  IF THIS FLAG INDICATES EVEN-AGED BUT STAND AGE DOES NOT GET SET USING
  !  THE STDINFO KEYWORD OR FROM THE ORGANON .INP FILE, THEN THIS WILL
  !  GET SET TO UNEVEN-AGED IN **SITSET**

  if (LNOTBK(2)) then
     INDS(4) = IFIX(ARRAY(2))
     if ((INDS(4).lt.0) .or. (INDS(4).gt.1)) then
        INDS(4) = 1
        if (LKECHO ) write(JOSTND,206) KEYWRD
        206 format (/1X,A8,'   INVALID EVEN-AGED/UNEVEN-AGED FLAG.',&
             &' USING 1 = EVEN-AGED')
     end if
  end if

  !  F3:
  !  ASSIGN THE ADDITIONAL MORTALITY FLAG
  !  0 = NO ADDITIONAL MORTALITY
  !  1 = CALCULATE ADDITIONAL MORTALITY USING MAX SDI (DEFAULT SET IN **GRINIT**)

  if (LNOTBK(3)) then
     INDS(9) = IFIX(ARRAY(3))
     if ((INDS(9).lt.0) .or. (INDS(9).gt.1)) then
        INDS(9) = 1
        if (LKECHO ) write(JOSTND,208) KEYWRD
        208 format (/1X,A8,'   INVALID MORTALITY FLAG. USING 1 = YES')
     end if
  end if

  !  F4:
  !  ASSIGN HEIGHT CALIBRATION VALUES. DEFAULT SET TO 1 = YES (CALCULATE
  !  AND USE CALIBRATION VALUES) IN **GRINIT**.

  if (LNOTBK(4)) then
     INDS(1) = IFIX(ARRAY(4))
     if ((INDS(1).lt.0) .or. (INDS(1).gt.1)) then
        INDS(1) = 1
        if (LKECHO) write(JOSTND,210) KEYWRD
        210 format (/1X,A8,'   INVALID HEIGHT CALIBRATION FLAG.',&
             &' USING 1 = YES')
     end if
  end if

  !  F5:
  !  ASSIGN HEIGHT-TO-CROWN BASE CALIBRATION VALUES. DEFAULT SET TO 1 = YES
  !  (CALCULATEAND USE CALIBRATION VALUES) IN **GRINIT**.

  if (LNOTBK(5)) then
     INDS(2) = IFIX(ARRAY(5))
     if ((INDS(2).lt.0) .or. (INDS(2).gt.1)) then
        INDS(2) = 1
        if (LKECHO) write(JOSTND,212) KEYWRD
        212 format (/1X,A8,'   INVALID HEIGHT-TO-CROWN BASE CALIBRATION ',&
             &' FLAG. USING 1 = YES')
     end if
  end if

  !  F6:
  !  ASSIGN HEIGHT CALIBRATION VALUES. DEFAULT SET TO 1 = YES (CALCULATE
  !  AND USE CALIBRATION VALUES) IN **GRINIT**.

  if (LNOTBK(6)) then
     INDS(3) = IFIX(ARRAY(6))
     if ((INDS(3).lt.0) .or. (INDS(3).gt.1)) then
        INDS(3) = 1
        if (LKECHO) write(JOSTND,214) KEYWRD
        214 format (/1X,A8,'   INVALID DIAMETER GROWTH CALIBRATION FLAG.',&
             &' USING 1 = YES')
     end if
  end if

  ! WRITE THE KEYWORD OUTPUT TO THE OUTPUT FILE.
  ! ORGINFO:
  !   ORGANON MODEL TYPE (1=SWO,2=NWO,3=SMC)
  !   STAND TYPE (0=UNEVEN-AGED,1=EVEN-AGED)= 0
  !   ADDITIONAL MAX-SDI MORTALITY (0=NO,1=YES)
  !   HEIGHT CALIBRATION (0=NO,1=YES)
  !   HEIGHT-TO-CROWN BASE CALIBRATION (0=NO,1=YES)
  !   DIAMETER GROWTH CALIBRATION (0=NO,1=YES)

  if (LKECHO) then
     write(JOSTND,216) KEYWRD,IMODTY,INDS(4),INDS(9),INDS(1),INDS(2),&
          &INDS(3)
     216 format (/A8,'   ORGANON ',&
          &'VARIANT (1=SWO,2=NWO,3=SMC) = ',I2,&
          &'; EVEN-AGED (0=NO,1=YES) = ',I2,&
          &'; MAX-SDI MORTALITY (0=NO,1=YES) = ',I2,';',/,11X,&
          &'HEIGHT CALIBRATION (0=NO,1=YES)=',I2,&
          &'; HEIGHT-TO-CROWN BASE CALIBRATION (0=NO,1=YES)=',I2,&
          &';',/,11X,'DIAMETER GROWTH CALIBRATION (0=NO,1=YES)=',I2)
  end if
  goto 10

  !  ==========  OPTION NUMBER  3: ORGVOLS   ======================ORGVOLS

  !  F1 = VOLUME COMPUTATION METHOD (0 is FVS NVEL EQUATIONS; 1 IS ORGANON OSU
  !       VOLUME EQUATIONS) DEFAULT IS 0

  !  THESE VARIABLES ARE INITIALIZED IN SUBROUTINE **GRINIT**, AND ONLY
  !  USED IN THE CALL TO THE ORGANON **VOLCAL** SUBROUTINE WHEN ORGANON
  !  VOLUMES ARE BEING COMPUTED.

  300 continue

  !  F1: ORGANON VOLUMES ON/OFF.

  if (LNOTBK(1)) then
     if (ARRAY(1) .ge.1.) then
        LORGVOLS = .true.
     end if
  end if

  ! WRITE THE KEYWORD OUTPUT TO THE OUTPUT FILE.

  if (LKECHO) then
     if (ARRAY(1) .ge.1.) then
        write(JOSTND,310) KEYWRD
        310 format (/A8,&
             &'   ORGANON OSU VOLUME EQUATIONS WILL BE USED WHEN COMPUTING',&
             &' VOLUMES.')
     end if
  end if
  go to 10

  !  ==========  OPTION NUMBER  4: INPFILE   ======================INPFILE

  !  SUPPLEMENTAL RECORD = FILENAME

  !  USE UNIT NUMBER 80 FOR ORGANON .INP FILE INPUT

  400 continue

  ! READ FILE NAME FROM THE SUPPLEMENTAL RECORD

  read(IREAD,402)FNAME
  402 format(A80)

  ! OPEN THE FILE

  ORGINP = 80
  open(UNIT = ORGINP,FILE = TRIM(FNAME),STATUS = 'OLD',RECL = 101,ERR = 420)
  if (LKECHO) then
     write(JOSTND,404) KEYWRD,FNAME
     404 format(/A8,'   FILE NAME = ',A80)
  end if

  call ORG_INTREE(ORGINP,LKECHO)

  BAF = -1.0
  BRK = 0.0
  LNOTRE = .true.

  !  IF STARTING FROM A PREPPED FILE, TURN THE ORGANON PREPARED FILE
  !  SWITCH TO TRUE SO **PREPARE** WON'T GET CALLED FROM **CRATET**

  LORGPREP = .true.

  close(80)
  go to 10

  !  FILE DID NOT OPEN

  420 continue
  if (LKECHO) then
     write(JOSTND,422) KEYWRD,FNAME
     422 format (/1X,A8,'   FILE DID NOT OPEN: FILE NAME = ',A80)
  end if
  go to 10

  !  ==========  OPTION NUMBER  5: OSUCFVOL    ======================OSUCFVOL
  !                                              DEFAULT   VARIABLE
  !  F1 = SOFTWOODS CF TOP DIB, IN INCHES          0.0     CFTD
  !  F2 = SOFTWOODS CF STUMP HEIGHT, IN FEET       0.0     CFSH
  !  F3 = HARDWOODS CF TOP DIB, IN INCHES          0.0     CFTDHW

  !  THESE VARIABLES ARE: IN THE ORGANON.F77 COMMON BLOCK, INITIALIZED IN
  !  SUBROUTINE **GRINIT**, AND ONLY USED IN THE CALL TO THE ORGANON **VOLCAL**
  !  SUBROUTINE WHEN ORGANON VOLUMES ARE BEING COMPUTED (LORGVOLS=.TRUE.)

  500 continue

  !  F1: SOFTWOODS CF TOP DIB, IN INCHES.

  if (LNOTBK(1)) then
     CFTD = ARRAY(1)
     if (CFTD .lt.0.0) CFTD = 0.0
     if (CFTD .gt.6.0) CFTD = 6.0
  end if

  !  F2: SOFTWOODS CF STUMP HEIGHT, IN FEET.

  if (LNOTBK(2)) then
     CFSH = ARRAY(2)
     if (CFSH .ge.4.5) CFSH = 4.49
  end if

  !  F3: HARDWOODS CF TOP DIB, IN INCHES.

  if (LNOTBK(3)) then
     CFTDHW = ARRAY(3)
  end if

  ! WRITE THE KEYWORD OUTPUT TO THE OUTPUT FILE.

  if (LKECHO) then
     write(JOSTND,511) KEYWRD,CFTD,CFSH,CFTDHW
     511 format (/A8,&
          &'   SOFTWOODS TOP DIB IS ',F4.1,' INCHES; ',&
          &'SOFTWOODS STUMP HEIGHT IS ',F4.1,' FEET; ',&
          &'HARDWOODS TOP DIB IS ',F4.1,' INCHES')
  end if
  go to 10

  !  ==========  OPTION NUMBER  6: OSUBFVOL   ======================OSUBFVOL

  !                                              DEFAULT   VARIABLE
  !  F1 = BF TOP DIB, IN INCHES                    6.0     LOGTD
  !  F2 = LOG TRIM ALLOWANCE, IN INCHES            8.0     LOGTA
  !  F3 = BF STUMP HT, IN FEET                     0.0     LOGSH
  !  F4 = TARGET LOG LENGTH, IN FEET              32.0     LOGLL
  !  F5 = MINIMUM LOG LENGTH, IN FEET              8.0     LOGML

  !  THESE VARIABLES ARE: IN THE ORGANON.F77 COMMON BLOCK, INITIALIZED IN
  !  SUBROUTINE **GRINIT**, AND ONLY USED IN THE CALL TO THE ORGANON **VOLCAL**
  !  SUBROUTINE WHEN ORGANON VOLUMES ARE BEING COMPUTED (LORGVOLS=.TRUE.)

  600 continue

  !  F1: BF LOG TOP DIAMETER, IN INCHES.

  if (LNOTBK(1)) then
     LOGTD = ARRAY(1)
     if (LOGTD .lt.2.0) LOGTD = 2.0
  end if

  !  F2: BF LOG TRIM, IN INCHES.

  if (LNOTBK(2)) then
     LOGTA = ARRAY(2)
  end if

  !  F3: BF STUMP HEIGHT

  if (LNOTBK(3)) then
     LOGSH = ARRAY(3)
     if (LOGSH .ge.4.5) LOGSH = 4.49
  end if

  !  F4: BF TARGET LOG LENGTH, IN FEET.

  if (LNOTBK(4)) then
     LOGLL = ARRAY(4)
  end if

  !  F5: MINIMUM LOG LENGTH, IN FEET.

  if (LNOTBK(5)) then
     LOGML = ARRAY(5)
  end if

  ! WRITE THE KEYWORD OUTPUT TO THE OUTPUT FILE.

  if (LKECHO .and. LORGVOLS ) then
     write(JOSTND,611) KEYWRD, LOGTD,LOGTA,LOGSH,LOGLL,LOGML
     611 format (/A8,&
          &'   MINIMUM LOG TOP DIAMETER IS ',F4.1,' INCHES; ',&
          &'TRIM ALLOWANCE IS ',F4.1,' INCHES; ',&
          &'STUMP HEIGHT IS ',F4.1,' FEET; ',/11X,&
          &'LOG LENGTH IS ', F4.1, ' FEET; ',&
          &'MINIMUM LOG LENGTH IS ',F4.1,' FEET')
  end if
  go to 10

  !  ==========  OPTION NUMBER  8: ORGOFF    =======================ORGOFF

  700 continue
  LORGANON = .false.
  if (LKECHO) then
     write(JOSTND,701) KEYWRD
     701 format (/A8,&
          &'   ORGANON GROWTH AND MORTALITY EQUATIONS TURNED OFF')
  end if
  go to 10

  2000 continue
  return
end
