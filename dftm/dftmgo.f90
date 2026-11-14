!===================!
!   dftm : dftmgo   !
!===================!
! DFTM $Id$
subroutine DFTMGO(L)
  implicit none

  !     PARAMETERS:
  !       L     = TRUE WHEN THE TUSSOCK MOTH MODEL IS TO BE CALLED

  !     NICK CROOKSTON                     JAN 1978

  !     EVENTS CONTROLED BY THIS ROUTINE:

  !     1) DECIDES IF AN OUTBREAK WILL TAKE PLACE DURING THIS CYCLE
  !        ITMETH CONTROLS THE METHOD THAT THE DECISION IS MADE:
  !        1 = ACTIVITY SCHEDULE IS UNDERCONTROL...MANSTART WAS SPECIFIED
  !        2 = THE OUTBREAK PROBABILTIY IS COMPAIRED TO A RANDOM NUMBER
  !            (RANSTART).
  !        3 = THE OUTBREAK PROBABILITY IS COMPAIRED TO 0.5 (CRTSTART).
  !     2) SETS LOGICAL VARIABLES 'LDF' AND 'LGF'
  !     3) SETS NUMBER OF CLASSES TO NUMBER 'MIN0(POSSABLE,REQUESTED)'

  !  REVISION HISTORY:
  !    01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'TMCOM1.f90'

  !     TMBASE= THE TUSSOCK MOTH MODEL BASE PREIOD

  ! Variables
  integer :: TMBASE
  logical :: L
  integer :: NPRMS,IDT,IDF,I1,I2,II,I,IGF,ISPOT,J
  integer :: IYR1,IYR2,IACTK,KODE
  real    :: X
  real    :: PRMS(2),PROTBK

  data TMBASE /  5  /

  L = .false.

  if (TMDEBU) write (JOSTND,*) '*** IN SUBROUTINE DFTMGO '

  !     CALCULATE THE PROBABILITY OF AN OUTBREAK

  call TMOTPR (PROTBK)

  !     ARE WE GOING TO SIMULATE AN OUTBREAK?

  !     FIND OUT IF ONE OF THE TUSSOCK MODE ACTIVITY CODES ARE
  !     PRESENT FOR THE PERIOD COVERED BY THIS CYCLE (NOTE THAT THE
  !     YEARS COVERED DIFFER FROM THE CYCLE BOUNDARYS, THEREFORE,
  !     OPGET2 IS USED RATHER THAN OPGET TO FIND THE ACTIVITY.
  !     NOTE THAT IDT IS RETURNED AS A ZERO IF THERE IS NO OUTBREAK.

  IYR1 = IY(ICYC) - 2
  IYR2 = IY(ICYC) + 2
  IACTK = 810

  call OPGET2(IACTK,IDT,IYR1,IYR2,1,2,NPRMS,PRMS,KODE)

  if (KODE .gt.0) IACTK = 811
  if (KODE .gt.0) call OPGET2(IACTK,IDT,IYR1,IYR2,1,2,&
       &NPRMS,PRMS,KODE)

  !     SCALE AND SAVE THE OUTBREAK PROBABILITY

  PROTBK = PROTBK * PRBSCL
  TMPRB(ICYC) = PROTBK

  !     IF THERE IS NO OUTBREAK SCHEDULED, RETURN.

  if (KODE .gt.0) then
     if (TMDEBU) write (JOSTND,*) '*** LEAVING SUBROUTINE DFTMGO; ',&
          &' NO OUTBREAK SCHEDULED'
     return
  end if

  !     LET TMYRS KEEP A RECORD OF REGIONAL OUTBREAKS.

  TMYRS(ICYC) = IDT
  goto (60,25,24),ITMETH

  24 continue
  call TMRANN(X)
  if (X .ge. PROTBK) go to 50
  go to 60

  25 continue
  if (PROTBK .le.0.5) go to 50
  go to 60

  50 continue
  call OPDEL2(IYR1,IYR2,IACTK,1)
  if (TMDEBU) write (JOSTND,*) '*** LEAVING SUBROUTINE DFTMGO'
  return

  60 continue

  !     ARE WE GOING TO SIMULATE TM ON DOUGLAS FIR?

  IDF = 0
  CNTDF = 0.0
  if (.not. LDF) go to 100
  I1 = ISCT(IDFCOD,1)
  if (I1.eq.0) go to 90
  I2 = ISCT(IDFCOD,2)
  IDF = I2-I1+1
  if (NCLAS(1) .le.0) go to 90
  if (IDF .lt.1) go to 90

  do II = I1, I2
     I = IND1(II)
     CNTDF = CNTDF + PROB(I)
     80 continue
  end do

  if (CNTDF .lt..01) go to 90
  go to 100

  90 continue
  LDF = .false.

  100 continue

  !     ARE WE GOING TO SIMULATE TM ON GRAND FIR?

  IGF = 0
  CNTGF = 0.0
  if (.not. LGF) go to 195

  I1 = ISCT(IGFCOD,1)
  if (I1.eq.0) go to 190

  I2 = ISCT(IGFCOD,2)
  IGF = I2 - I1 + 1
  if (IGF .lt.1) go to 190
  if (NCLAS(2) .le.0) go to 190

  do II = I1, I2
     I = IND1(II)
     CNTGF = CNTGF + PROB(I)
     130 continue
  end do

  if ( CNTGF .lt..01 ) go to 190
  go to 195

  190 continue
  LGF = .false.

  195 continue
  L = (LDF .or. LGF)

  !     SET NUMBER OF ACTUAL CLASSES PER SPECIES.

  NACLAS(1) = 0
  NACLAS(2) = 0
  if (LDF) NACLAS(1) = MIN0(IDF, NCLAS(1))
  if (LGF) NACLAS(2) = MIN0(IGF, NCLAS(2))

  if (L) go to 210

  write (JODFTM,200)
  200 format (/,'***** WARNING:  A REQUESTED OR PREDICTED',&
       &' TUSSOCK MOTH OUTBREAK CAN NOT BE SIMULATED.')
  call OPDEL2 (IYR1, IYR2, IACTK, 1)
  go to 220

  210 continue
  if (.not. TMDEBU) go to 230

  220 continue
  write (JODFTM,225) CNTDF, IDF, NACLAS(1), NCLAS(1), LDF,&
       &CNTGF, IGF, NACLAS(2), NCLAS(2), LGF
  225 format (/,'DUMP OF CONTROLING VARIABLES:',//,&
       &T14,'TREES/ACRE  NUMBER OF TREE  NUMBER OF CLASSES',&
       &'      T=GO',/,T29,'RECORDS      ACTUAL    REQUESTED',&
       &'    F=NO GO',//&
       &' DOUGLAS FIR:',T13,F10.3,I11,2I13,L8,//,&
       &' GRAND FIR:',  T13,F10.3,I11,2I13,L8)

  230 continue

  !     INSURE TMWORK IS KEEPING A HISTORY OF OUTBREAKS

  TMWORK(ICYC) = L

  !     INSURE THAT AN ENDING CYCLE EXISTS AND THAT NO CYCLES
  !     EXIST IN THE MEANTIME.

  if (.not. L) go to 245

  !     SIGNAL THE OPTION PROCESSING ROUTINES THAT THE OUTBREAK "HAS"
  !     HAPPENED.

  call OPDON2(IACTK,IY(ICYC),IYR1,IYR2,1,KODE)

  !     IF MANUAL SCHEDULING WAS USED (MANSCHED) ALLOW IFINT TO BE
  !     EQUAL TO TMBASE + OR - 1

  if ( ITMSCH .eq.1.and. IABS(IFINT-TMBASE) .le.1 ) go to 245

  !     INSURE THAT IFINT EQUALS TMBASE

  call INSCYC (ICYC,TMBASE,ISPOT,.true.,.false.,JODFTM,TMDEBU)
  if (ISPOT .eq.0) go to 235

  write (JODFTM,9001) TMBASE, IY(ISPOT)
  9001 format (/'***** WARNING:  TO INSURE A',I2,' YEAR TUSSOCK MOTH',&
       &' CYCLE LENGTH A CYCLE WAS INSERTED AT YEAR',I5,'.')

  235 continue
  if (IFINT .ge. TMBASE) go to 245

  !     IFINT IS TOO SMALL:  ONE OR MORE CYCLES MUST BE DELETED

  J = ICYC+1
  write (JODFTM,9002) TMBASE, IY(J)
  9002 format (/'***** WARNING:  TO INSURE A',I2,' YEAR TUSSOCK MOTH',&
       &' CYCLE LENGTH, THE CYCLE SCHEDULED TO BEGIN IN',I5,&
       &' WAS CANCELED.')

  NCYC = NCYC - 1

  do I = J, NCYC
     !       LDEBUG(I) = LDEBUG(I+1)
     !       LIST(I) = LIST(I+1)
     IY(I) = IY(I+1)
     240 continue
  end do

  IY(NCYC+1) = IY(NCYC+2)
  IFINT = IY(ICYC+1) - IY(ICYC)
  FINT = IFINT
  !     DEBUG = LDEBUG(ICYC)
  call OPCYCL (NCYC,IY)
  call OPCSET (ICYC)
  go to 235

  245 continue
  if (TMDEBU) then
     write (JODFTM,250) L
     250 format (/,'T=SIMULATION WILL GO; F=NO GO   " ',L1,' "')
     write (JOSTND,*) '*** LEAVING SUBROUTINE DFTMGO'
  end if

  return
end
