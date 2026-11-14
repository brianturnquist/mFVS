!====================!
!   wsbwe : bweage   !
!====================!
! WSBWE $Id$
subroutine BWEAGE (TOTR)
  implicit none

  !     AGE THE FOLIAGE ONE YEAR.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JANUARY 1984

  !     minor changes by K.Sheehan 7/96 to remove LBWDEB

  !     CALLED FROM :

  !       BWEDR - DRIVE THE BUDWORM MODEL FOR ONE STAND IN ONE YEAR.

  !     FUNCTIONS AND SUBROUTINES CALLED :

  !       BWESLP  - LINEAR INTERPOLATION FUNCTION.

  !     PARAMETERS :

  !     TOTR   - TOTAL REMAINING FOLIAGE, OUTPUT

  ! Revision History:
  !  17-MAY-2005 Lance R. David (FHTET)
  !     Added FVS parameter file PRGPRM.F77.
  !  14-JUL-2010 Lance R. David (FMSC)
  !     Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'

  ! Variables
  integer :: I,ICMYR,ICROWN,IFAGE,IHOST,IREM,ISZI
  real    :: BWESLP,CUM,DIF,DIV,FA,FR,PR,PRALL,PRB,XMULT
  real    :: RECRX(4),RECRY(4),RECHST(6),TOTR(6,3)

  data RECRX/0.,.2,.8,1./,RECRY/.6,.6,0.,0./,&
       &RECHST/0.5,1.0,0.5,0.5,1.0,0.5/

  !     ********************** EXECUTION BEGINS **************************

  !     COMPUTE THE RETAINED PROPORTION OF BIOMASS ARRAY.

  do IHOST = 1,6
     if (IFHOST(IHOST).eq.0) goto 120
     do ICROWN = 1,9
        ISZI = (ICROWN+2)/3

        DIV = FOLADJ(IHOST,ICROWN,1)
        PRBIO(IHOST,ICROWN,1) = .0
        if (FNEW (ICROWN,IHOST).lt..00001) FNEW (ICROWN,IHOST) = 0.
        if (DIV.gt..00001) PRBIO(IHOST,ICROWN,1) = FNEW (ICROWN,IHOST)/DIV

        DIV = FOLADJ(IHOST,ICROWN,2)
        PRBIO(IHOST,ICROWN,2) = .0
        if (FOLD1(ICROWN,IHOST).lt..00001) FOLD1(ICROWN,IHOST) = 0.
        if (DIV.gt..00001) PRBIO(IHOST,ICROWN,2) = FOLD1(ICROWN,IHOST)/DIV

        DIV = FOLADJ(IHOST,ICROWN,3)
        PRBIO(IHOST,ICROWN,3) = .0
        if (FOLD2(ICROWN,IHOST).lt..00001) FOLD2(ICROWN,IHOST) = 0.
        if (DIV.gt..00001) PRBIO(IHOST,ICROWN,3) = FOLD2(ICROWN,IHOST)/DIV

        DIV = FOLADJ(IHOST,ICROWN,4)
        PRBIO(IHOST,ICROWN,4) = .0
        if (FREM (ICROWN,IHOST).lt..00001) FREM (ICROWN,IHOST) = 0.
        if (DIV.gt..00001) PRBIO(IHOST,ICROWN,4) = FREM (ICROWN,IHOST)/DIV

        !     COMPUTE THE TOTAL REMAINING FOLIAGE (TOTR INITIALIZED IN BWDRIV).

        if (BWTPHA(IHOST,ISZI).le.0.0) goto 100
        TOTR(IHOST,ISZI) = TOTR(IHOST,ISZI)+FNEW(ICROWN,IHOST)&
             &+FOLD1(ICROWN,IHOST)+FOLD2(ICROWN,IHOST)&
             &+FREM (ICROWN,IHOST)
        100 continue
     end do
     120 continue
  end do

  !     AGE THE FOLIAGE

  do IHOST = 1,6
     if (IFHOST(IHOST).eq.0) goto 230
     do ICROWN = 1,9

        !     ADJUST THE REMAINING FOLIAGE.
        !     IREM=1, DEFOLIATION HAS STOPED, REMAINING MOVES TOWARD THE
        !             POTENTIAL.
        !     IREM=2, DEFOLIATION HAS STARTED, REMAINING MOVES UP, OLD
        !             NEEDLES ARE RETAINED.
        !     IREM=3, DEFOLIATION IS CONTINOUS AND HEAVY, REMAINING MOVE.
        !             DOWN, OLD NEEDLES START TO DIE.

        IREM = 1
        PRB = PRBIO(IHOST,ICROWN,1)
        if (PRB.ge..80) goto 150
        IREM = 2
        ISZI = (ICROWN+2)/3

        !     USE LAST YEARS TOTAL TREE DEFOLIATION AS A MEASURE OF
        !     WHAT IS 'HEAVY' DEFOLIATION. IF NCUMYR = 0, THERE ISN'T AN
        !     ESTIMATE OF LAST YEARS DEFOLIATION.

        if (NCUMYR.le.0) goto 150
        ICMYR = NCUMYR
        if (ICUMYR.gt.0) ICMYR = ICUMYR
        CUM = 0.
        if (ICMYR.gt.0) CUM = CUMDEF(IHOST,ISZI,ICMYR)
        if (CUM.ge.40.) IREM = 3
        150 continue
        FR = FREM(ICROWN,IHOST)
        PR = FOLPOT(IHOST,ICROWN,4)
        PRALL = PR
        do IFAGE = 1,3
           PRALL = PRALL+FOLPOT(IHOST,ICROWN,IFAGE)
           155 continue
        end do
        XMULT = 0.0
        DIV = FOLPOT(IHOST,ICROWN,3)
        if (DIV.gt.0.000001) XMULT = FOLPOT(IHOST,ICROWN,4)/DIV
        FA = FOLD2(ICROWN,IHOST)*XMULT

        !     BRANCH TO COMPUTE THE REMAINING FOLIAGE.

        goto (160,180,190),IREM
        160 continue

        !     MOVE THE REMAINING TOWARD THE POTENTIAL.

        DIF = FR-PR

        !     IF THE DIFFERENCE IS POSITIVE THERE IS MORE ACTUAL REMAINING
        !     THAN POTENTIAL, LET SOME OF THE NEEDLES DROP.

        if (DIF.ge.0.0) then
           FA = PR
           if (DIF/PR .gt..10) FA = FR-(DIF*.40)
           FR = FA
        else

           !        THE DIFFERENCE IS NEGATIVE.  ADD NEEDLES FROM 2-YR GROUP.

           FR = FR+FA
           if (FR.gt.PR) FR = PR
        end if
        goto 200
        180 continue

        !     ADD NEEDLES FROM 2-YR GROUP.

        FR = FR+FA*BWESLP(PRB,RECRX,RECRY,4)*RECHST(IHOST)
        if (FR.gt.PRALL) FR = PRALL
        goto 200
        190 continue

        !     ADD NEEDLES FROM 2-YR GROUP, THEN KILL SOME.

        FR = (FR+FA*BWESLP(PRB,RECRX,RECRY,4))*.85
        if (FR.gt.PRALL) FR = PRALL
        200 continue
        FREM(ICROWN,IHOST) = FR

        !     AGE THE 1-YEAR AND 2-YEAR OLD FOLIAGE.

        XMULT = 0.0
        DIV = FOLPOT(IHOST,ICROWN,2)
        if (DIV.gt..00001) XMULT = FOLPOT(IHOST,ICROWN,3)/DIV
        FOLD2(ICROWN,IHOST) = FOLD1(ICROWN,IHOST)*XMULT
        XMULT = 0.0
        DIV = FOLPOT(IHOST,ICROWN,1)
        if (DIV.gt..00001) XMULT = FOLPOT(IHOST,ICROWN,2)/DIV
        FOLD1(ICROWN,IHOST) = FNEW (ICROWN,IHOST)*XMULT

        !     COMPUTE THE NEW BUD CROP.

        XMULT = 0.0
        DIV = FOLPOT(IHOST,ICROWN,2)
        if (DIV.gt..00001) XMULT = FOLD1(ICROWN,IHOST)/DIV
        FNEW (ICROWN,IHOST) = FOLPOT(IHOST,ICROWN,1) *&
             &BWESLP( ((PRBIO(IHOST,ICROWN,2)*.6) + (XMULT * .4)),&
             &RELFX(1,ICROWN),RELFY(1,ICROWN),4)

        !     AGE ADJUSTED POTENTIAL FOLIAGE.

        FA = FOLADJ(IHOST,ICROWN,4)
        FA = FA+FOLADJ(IHOST,ICROWN,3)
        PR = FOLPOT(IHOST,ICROWN,4)
        if (FA.gt.PR) FA = PR
        FR = FREM(ICROWN,IHOST)
        if (FA.lt.FR) FA = FR
        FOLADJ(IHOST,ICROWN,4) = FA
        do I = 1,2
           IFAGE = 4-I
           XMULT = 0.0
           DIV = FOLPOT(IHOST,ICROWN,IFAGE-1)
           if (DIV.gt..00001) XMULT = FOLPOT(IHOST,ICROWN,IFAGE)/DIV
           FOLADJ(IHOST,ICROWN,IFAGE) = FOLADJ(IHOST,ICROWN,IFAGE-1)*XMULT
           210 continue
        end do
        FOLADJ(IHOST,ICROWN,1) = FNEW(ICROWN,IHOST)
        220 continue
     end do
     230 continue
  end do

  return
end
