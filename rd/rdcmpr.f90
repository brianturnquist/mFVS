!=================!
!   rd : rdcmpr   !
!=================!
! RD $Id$
subroutine RDCMPR (NCLAS,PROB,IND,IND1)
  implicit none

  !  THIS SUBROUTINE IS USED TO COMPRESS THE TREE RECORDS ARRAYS FOR
  !  THE ROOT DISEASE MODEL.

  !  CALLED BY :
  !     COMPRS  [PROGNOSIS]

  !  CALLS     :
  !     RDATV   (SUBROUTINE)   [ROOT DISEASE]

  !  PARAMETERS :
  !     NCLAS  -
  !     PROB   -
  !     IND    -
  !     IND1   -
  !C  Revision History:
  !    22-JUL-02 Lance R. David (FHTET)
  !      Previous revision date noted was March 7, 1995.
  !      Removed unused array PROBO. It was also unused in the old
  !      annosus model.
  !   08/27/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/01/2016 Lance R. David (FMSC)
  !     Moved two conditions to exit to top.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'CONTRL.f90'
  include 'RDARRY.f90'
  include 'RDADD.f90'

  ! Variables
  integer :: I,I1,I2,ICL,IDI,IENDYR,IND(MAXTRE),IND1(MAXTRE),IP,IREC,J,JJ,K,NCLAS
  real    :: PROB(MAXTRE),TPAREA,ROOT1,ROOT3,TXP,TXP1,TXP2,TXP3,TXP4,TXP5,TXP7,TXP10,TXP14(2),TXP15(3),XP
  logical :: LGO,LTEE

  TXP7 = 0.0

  !     IENDYR IS THE LAST YEAR OF PROBI, PROPI THAT WILL HAVE INFO
  !     WE DONT NEED TO DO ANY COMPRESSION ON LATER YEARS
  IENDYR = ICYC + 1

  !     DO 557 I=1,ITRN
  !     WRITE(JOSTND,777)  I,PROBI(I,1,1),PROBIU(I),PROBIT(I),
  !    >      FPROB(I)
  ! 777 FORMAT(' IN RDCMPR BEFORE COMPRESS',I5,4F8.2)
  ! 557 CONTINUE

  call RDATV(LGO,LTEE)
  if (.not. LGO) return
  if (.not. LTEE .and. LSTART) return

  !     IF ROOT DISEASE NOT ACTIVE OR NO PATCH AREA THEN RETURN. ALSO
  !     IF USING MANUAL DATA INITIALIZATION AND WE ARE IN CYCLE 0 ALSO
  !     RETURN BECAUSE ROOT DISEASE ARRAYS ARE EMPTY. NOTE CODE IN RRMN1.

  TPAREA = 0.0
  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     99 continue
  end do
  if (TPAREA .eq.0.0) return

  !     COMPRESS THE ROOT DISEASE TREE RECORDS.

  I1 = 1
  do ICL = 1,NCLAS

     !        SET THE POINTERS TO THE TREES IN THE CLASS

     I2 = IND1(ICL)

     !        IF THERE IS ONLY ONE RECORD IN CLASS ICL, THEN: SKIP THE CLASS

     IREC1 = IND(I1)

     !        WRITE(JOSTND,333) I1,ICL,I2,IREC1
     !  333    FORMAT(' RDCMPR I1,ICL,I2,IREC1',4I5)

     if (I1.eq. I2) goto 480

     !        CREATE A COMULATIVE PROB FOR ALL THE TREES IN THE CLASS.

     XP = PROB(IREC1)
     TXP = XP
     K = I1 + 1
     TXP1 = FPROB(IREC1)
     TXP2 = PROBIT(IREC1)
     TXP3 = PROBIU(IREC1)
     TXP4 = PROBL(IREC1)
     TXP5 = RRKILL(IREC1)
     TXP7 = RDKILL(IREC1)
     TXP10 = WK22(IREC1)
     TXP14(1) = FFPROB(IREC1,1)
     TXP14(2) = FFPROB(IREC1,2)

     do J = 1,4
        if (J .lt.4) TXP15(J) = OAKL(J,IREC1)

        TXP12(J) = 0.0
        if (XMTH(J,IREC1) .lt.0.0) goto 799
        TXP12(J) = XMTH(J,IREC1)
        799 continue
     end do

     JJ = 0
     do J = 1,IENDYR
        do IP = 1,2
           JJ = JJ + 1
           TXP8(JJ) = PROBI(IREC1,J,IP)
           890 continue
        end do
        899 continue
     end do

     do I = K,I2
        IREC = IND(I)
        !           WRITE(JOSTND,334) IREC
        !  334       FORMAT(' IREC',I5)
        XP = PROB(IREC)
        TXP = TXP + XP
        TXP1 = TXP1 + FPROB(IREC)
        TXP2 = TXP2 + PROBIT(IREC)
        TXP3 = TXP3 + PROBIU(IREC)
        TXP4 = TXP4 + PROBL(IREC)
        TXP5 = TXP5 + RRKILL(IREC)
        TXP7 = TXP7 + RDKILL(IREC)
        TXP10 = TXP10 + WK22(IREC)
        TXP14(1) = TXP14(1) + FFPROB(IREC,1)
        TXP14(2) = TXP14(2) + FFPROB(IREC,2)

        do J = 1,4
           if (J .lt.4) TXP15(J) = TXP15(J) + OAKL(J,IREC)

           if (XMTH(J,IREC) .lt.0.0) goto 699
           TXP12(J) = TXP12(J) + XMTH(J,IREC)
           699 continue
        end do

        JJ = 0
        do J = 1,IENDYR
           do IP = 1,2
              JJ = JJ + 1
              TXP8(JJ) = TXP8(JJ) + PROBI(IREC,J,IP)
              880 continue
           end do
           888 continue
        end do
        215 continue
     end do

     !        COMPUTE THE WEIGHTED AVERAGES OF ROOT RADIUS VARIABLES

     XP = PROB(IREC1)
     ROOT1 = ROOTL(IREC1) * XP
     ROOT3 = XP * RROOTT(IREC1)

     do J = 1,4
        ROOT4(J) = 0.0
        if (XMTH(J,IREC1) .lt.0.0) goto 499
        ROOT4(J) = ROOTH(J,IREC1) * XMTH(J,IREC1)
        499 continue
     end do

     JJ = 0
     do J = 1,IENDYR
        do IP = 1,2
           JJ = JJ + 1
           PROP1(JJ) = PROPI(IREC1,J,IP) * PROBI(IREC1,J,IP)
           901 continue
        end do
        905 continue
     end do

     !        ADD IN THE OTHER TREES WHICH FORM THE CLASS.

     I1 = I1 + 1

     do I = I1,I2
        IREC = IND(I)
        XP = PROB(IREC)
        ROOT1 = ROOT1 + ROOTL(IREC) * XP
        ROOT3 = ROOT3 + RROOTT(IREC) * XP

        do J = 1,4
           if (XMTH(J,IREC) .lt.0.0) goto 498
           ROOT4(J) = ROOT4(J) + ROOTH(J,IREC) * XMTH(J,IREC)
           498 continue
        end do

        JJ = 0
        do J = 1,IENDYR
           do IP = 1,2
              JJ = JJ + 1
              PROP1(JJ) = PROP1(JJ) + PROPI(IREC,J,IP) *&
                   &PROBI(IREC,J,IP)
              907 continue
           end do
           902 continue
        end do

        300 continue
     end do

     !        DIVIDE BY THE TOTAL PROB AND MOVE THE VALUES INTO
     !        THE 'IREC1' POSITION IN THE ARRAYS.

     if (TXP .eq.0.0) then
        ROOTL(IREC1) = 0.0
     else
        ROOTL(IREC1) = ROOT1 / TXP
     end if

     FPROB(IREC1) = TXP1
     PROBIT(IREC1) = TXP2
     WK22(IREC1) = TXP10
     FFPROB(IREC1,1) = TXP14(1)
     FFPROB(IREC1,2) = TXP14(2)

     do J = 1,4
        if (J .lt.4) OAKL(J,IREC1) = TXP15(J)

        if (TXP12(J) .le.0.0) goto 599
        XMTH(J,IREC1) = TXP12(J)
        ROOTH(J,IREC1) = ROOT4(J) / TXP12(J)
        599 continue
     end do

     if (TXP .eq.0.0) then
        RROOTT(IREC1) = 0.0
     else
        RROOTT(IREC1) = ROOT3 / TXP
     end if

     PROBIU(IREC1) = TXP3
     PROBL(IREC1) = TXP4
     RRKILL(IREC1) = TXP5
     RDKILL(IREC1) = TXP7

     JJ = 0
     do J = 1,IENDYR
        do IP = 1,2
           JJ = JJ + 1
           PROBI(IREC1,J,IP) = TXP8(JJ)
           PROPI(IREC1,J,IP) = PROP1(JJ) / (TXP8(JJ) + .00001)
           885 continue
        end do
        889 continue
     end do

     !        END OF COMPRESSION LOOP

     480 continue

     !        REDEFINE I1 SUCH THAT IT POINTS TO THE FIRST
     !        INDEX IN THE NEXT CLASS.

     I1 = I2+1
     500 continue
  end do

  !     DO 558 I=1,ITRN
  !     WRITE(JOSTND,833)  I,PROBI(I,1),PROBIU(I),PROBIT(I),
  !    >     FPROB(I)
  ! 833 FORMAT(' IN RDCMPR AFTER  COMPRESS',I5,4F8.2)
  ! 558 CONTINUE

  return
end
