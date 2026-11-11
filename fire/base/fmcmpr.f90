!========================!
!   fire/base : fmcmpr   !
!========================!
! FIRE-BASE $Id$
subroutine FMCMPR (NCLAS)
  implicit none

  !  THIS SUBROUTINE IS USED TO COMPRESS THE TREE RECORDS ARRAYS FOR
  !  THE FIRE/SNAG MODEL.

  !  CALLED BY :
  !     COMPRS  [PROGNOSIS]

  !  PARAMETERS :
  !     NCLAS  -  NUMBER OF CLASSES (RECORDS) FOLLOWING COMPRESSION
  !     PROB   -  TREES/ACRE FOR EACH RECORD
  !     IND    -  INDEX OF RECORD BEING COMPRESSED
  !     IND1   -  TARGET INDEX OF RECORD FOLLOWING COMPRESSION

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'

  ! Variables
  real    :: TXP6(0:5),TXP7(0:5)
  logical :: DEBUG
  integer :: NCLAS,I1,ICL,I2,K,JJ,I,IREC
  real    :: XP,TXP,TXP4,TXP5,TXP8,TXP9

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCMPR',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,LFMON
  7 format(' ENTERING ROUTINE FMCMPR CYCLE = ',I2,' LFMON=',L2)

  !     NO COMPRESSION IS REQUIRED IF THE FIRE/SNAG MODEL IS ABSENT

  if (.not. LFMON) return

  I1 = 1
  do ICL = 1, NCLAS

     !     SET THE POINTERS TO THE TREES IN THE CLASS

     I2 = IND1(ICL)

     !       IF THERE IS ONLY ONE RECORD IN CLASS ICL, THEN: SKIP THE CLASS

     IREC1 = IND(I1)
     if (I1.eq. I2) goto 480

     !        CREATE CUMULATIVE GROUPS. THOSE NOT IN #/ACRE ARE WEIGHTED

     !         XP   = PROB(IREC1)

     XP = FMPROB(IREC1)
     TXP = XP
     K = I1 + 1

     TXP4 = OLDHT(IREC1)  * XP
     TXP5 = OLDCRL(IREC1) * XP
     do JJ = 0, 5
        TXP6(JJ) = OLDCRW(IREC1, JJ) * XP
        TXP7(JJ) = CROWNW(IREC1, JJ) * XP
     end do
     TXP8 = GROW(IREC1) * XP

     TXP9 = FMICR(IREC1) * XP

     do I = K, I2
        IREC = IND(I)
        !            XP   = PROB(IREC)
        XP = FMPROB(IREC)
        TXP = TXP + XP
        TXP4 = TXP4 + OLDHT(IREC)  * XP
        TXP5 = TXP5 + OLDCRL(IREC) * XP
        do JJ = 0, 5
           TXP6(JJ) = TXP6(JJ) + OLDCRW(IREC, JJ) * XP
           TXP7(JJ) = TXP7(JJ) + CROWNW(IREC, JJ) * XP
        end do
        TXP8 = TXP8  + GROW(IREC)   * XP
        TXP9 = TXP9  + FMICR(IREC1) * XP
     end do

     !        MOVE COMPRESSED VALUES TO THE 'IREC1' POSITION

     FMPROB(IREC1) = TXP
     if (TXP .gt.0.) then
        OLDHT(IREC1) = TXP4 / TXP
        OLDCRL(IREC1) = TXP5 / TXP
        do JJ = 0, 5
           OLDCRW(IREC1, JJ) = TXP6(JJ) / TXP
           CROWNW(IREC1, JJ) = TXP7(JJ) / TXP
        end do
        GROW(IREC1) = INT(TXP8  / TXP)
        FMICR(IREC1) = INT(TXP9  / TXP + 0.5)
     else
        OLDHT(IREC1) = 0.
        OLDCRL(IREC1) = 0.
        do JJ = 0, 5
           OLDCRW(IREC1, JJ) = 0.
           CROWNW(IREC1, JJ) = 0.
        end do
        GROW(IREC1) = 0
        FMICR(IREC1) = 0
     end if

     !        REDEFINE I1 SO THAT IT POINTS TO THE FIRST INDEX IN
     !        THE NEXT CLASS.

     480 continue
     I1 = I2+1

     500 continue
  end do

  return
end
