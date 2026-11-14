!===================!
!   wwpb : bmfmrt   !
!===================!
! WWPB $Id$
subroutine BMFMRT (ISTD, IYR, SH, FM)

  !     CALLED FROM: BMFIRE

  !  **BMFMRT  DATE OF LAST REVISION:  June 14, 1994

  !  PURPOSE:
  !     THIS SUBROUTINE CALCULATES THE PROBABILITY OF TREE MORTALITY.
  !     FROM INFORMATION SENT FROM E. WEINHARDT

  !  CALL LIST DEFINITIONS:
  !     SH:   SCORCH HEIGHT (IN FEET)

  !  LOCAL VARIABLE DEFINITIONS:
  !     ISIZ:   LOOP COUNTER OVER DBH SIZE CLASSES
  !     ITYP:   LOOP COUNTER OVER HOST/NON-HOST TYPES
  !     DBH:    MEAN DBH OF A SIZE CLASS
  !     PMORT:  PROBABILITY (PROPORTION) OF MORTALITY

  !  COMMON BLOCK VARIABLES AND PARAMETERS:
  !     ISPH:   FROM BMFCOM; MAIN SPECIES IN HOST AND NON-HOST CLASSES
  !     NSCL:   FROM BMPRM; NUMBER OF DBH SIZE CLASSES
  !     SCORCH: FROM BMCOM; PROPORTION OF TREES IN SIZE CLASS
  !             SEVERELY SCORCHED BY FIRE

  !.... PARAMETER STATEMENTS.

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... COMMON INCLUDE FILES.

  include 'BMCOM.f90'
  include 'BMFCOM.f90'

  !.... VARIABLE DECLARATIONS.

  ! Variables
  real    :: CSLP
  integer :: ISIZ,ITYP
  integer :: J,JJ
  real    :: DBH(NSCL)
  real    :: PMORT(NSCL,2)
  real    :: SH

  !.... CHECK FOR DEBUG.

  if (LBMDEB) write(JBMBPR,10) ISTD,IYR
  10 format(' Begin BMFMRT: Stand = ', I8,' Year = ',I5)

  !.... BEGIN ROUTINE

  DBH(1) = UPSIZ(1) / 2.0
  do ISIZ = 2,NSCL
     DBH(ISIZ) = UPSIZ(ISIZ-1) + (UPSIZ(ISIZ) - UPSIZ(ISIZ-1)) / 2.0
     15 continue
  end do

  !.... 1=DORMANT SEASON,  -1 FOR GROWING SEASON

  SEAS = -1

  !.... 1 = HOST, 2 = NONHOST

  do ITYP = 1,2

     !....    1 = SMALLEST, NSCL=LARGEST

     do ISIZ = 1,NSCL

        if (TREE(ISTD,ISIZ, ITYP) .gt.0.0) then

           !....          COMPUTE SCORCH LENGTH SL

           !              CROWN LENGTH

           CRL = HTS(ISTD,ISIZ,ITYP) * (CRS(ISTD,ISIZ,ITYP) / 100.0)

           !              SCORCH LENGTH

           SL = SH - (HTS(ISTD,ISIZ,ITYP) - CRL)

           if (SL .lt.0.0) SL = 0.0
           if (SL .gt. CRL) SL = CRL

           !              PERCENT OF CROWN LENGTH SCORCHED

           CSLP = 100.0 * (SL / CRL)

           if (DBH(ISIZ) .gt.1.0) then

              !                 IF HOST TYPE IS PP

              !                  IF ((ITYP .EQ. 1).AND.(HSPEC(PBSPEC,10) .EQ. 1)) THEN
              if (ISPH(ISTD,ITYP) .eq.10) then

                 if (CSLP .lt.90.0) then
                    ILS = 1
                    IHS = 0
                 else if (CSLP .lt.100.0) then
                    ILS = 0
                    IHS = 1
                 else if (CSLP .eq.100.0) then
                    ILS = -1
                    IHS = -1
                 end if

                 if (DBH(ISIZ) .le.4.5) then
                    D = 3
                 else if (DBH(ISIZ) .le.7.5) then
                    D = 6
                 else if (DBH(ISIZ) .le.10.5) then
                    D = 9
                 else
                    D = 12
                 end if

                 PMORT(ISIZ,ITYP) = 1.0 / (1 + EXP(-1.16 + 1.04&
                      &* SEAS + 1.94 * ILS - .12 * IHS + .14 * 2.54 * D))

              else
                 !....                ALL OTHER SPECIES

                 CSLP = 100. * (SL * (2.0 * CRL - SL) / (CRL * CRL))

                 !....                CALCULATE BARK THICKNESS

                 if (ISPH(ISTD,ITYP) .eq.1) then
                    !                        white pine
                    BTH = .054 + .025 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.2) then
                    !                        larch
                    BTH = -0.045 + 0.0629 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.3) then
                    !                        douglas fir
                    BTH = 0.065 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.4) then
                    !                        grand fir
                    BTH = 0.043 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.5) then
                    !                        western hemlock
                    BTH = 0.022 + 0.043 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.6) then
                    !                        cedar
                    BTH = -0.152 + 0.021 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.7) then
                    !                        lodgepole pine
                    BTH = 0.0271 + 0.0143 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.8) then
                    !                        spruce
                    BTH = 0.126 + 0.025 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.9) then
                    !                        sub-alpine fir
                    BTH = 0.015 * DBH(ISIZ)
                 else if (ISPH(ISTD,ITYP) .eq.10) then
                    !                        ponderosa pine
                    BTH = -0.0376 + 0.0584 * DBH(ISIZ)
                 else
                    !                        other (WPB)
                    BTH = 0.0271 + 0.022 * DBH(ISIZ)
                 end if

                 X = EXP(-1.941 + 6.316 * (1 - EXP(-BTH))&
                      &- .000535 * CSLP * CSLP)
                 PMORT(ISIZ, ITYP) = 1 / (1 + X)

              end if

           else
              !                 DBH < 1

              if ((CSLP .gt.50) .or. ((FM .ne.1)&
                   &.and. (FM .ne.8) .and. (FM .ne.9))&
                   &.or. (HTS(ISTD,ISIZ,ITYP) .lt.3)) then
                 PMORT(ISIZ, ITYP) = 1.0

              else
                 if (ISPH(ISTD,ITYP) .eq.10) then

                    !....                   ASSUME DBH = 1 AND CSLP < 50

                    P1 = 1.0 / (1.0 + EXP(-1.16 + 1.04 * SEAS&
                         &+ 1.94 + .14 * 2.54 * 3.0))
                 else

                    !....                   CALCULATE BARK THICKNESS WITH DBH=1
                    if (ISPH(ISTD,ITYP) .eq.1) then
                       !                        white pine
                       BTH = .054 + .025
                    else if (ISPH(ISTD,ITYP) .eq.2) then
                       !                        larch
                       BTH = -0.045 + 0.0629
                    else if (ISPH(ISTD,ITYP) .eq.3) then
                       !                        douglas fir
                       BTH = 0.065
                    else if (ISPH(ISTD,ITYP) .eq.4) then
                       !                        grand fir
                       BTH = 0.043
                    else if (ISPH(ISTD,ITYP) .eq.5) then
                       !                        western hemlock
                       BTH = 0.022 + 0.043
                    else if (ISPH(ISTD,ITYP) .eq.6) then
                       !                        cedar
                       BTH = -0.152 + 0.021
                    else if (ISPH(ISTD,ITYP) .eq.7) then
                       !                        lodgepole pine
                       BTH = 0.0271 + 0.0143
                    else if (ISPH(ISTD,ITYP) .eq.8) then
                       !                        spruce
                       BTH = 0.126 + 0.025
                    else if (ISPH(ISTD,ITYP) .eq.9) then
                       !                        sub-alpine fir
                       BTH = 0.015
                    else if (ISPH(ISTD,ITYP) .eq.10) then
                       !                        ponderosa pine
                       BTH = -0.0376 + 0.0584
                    else
                       !                        other (WPB)
                       BTH = 0.0271 + 0.022
                    end if

                    P1 = 1.0 / (1.0 + EXP(-1.941 + 6.316 *&
                         &(1.0 - EXP(-BTH)) - .000535 * CSLP * CSLP))
                 end if

                 !                ***NEED TO GET TRUE EXPRESSION HERE.
                 !                    H1 IS HT OF TREE W DBH=1

                 H1 = 6.0

                 PMORT(ISIZ, ITYP) = P1 + (1.0 - P1)&
                      &* (1.0 - (HTS(ISTD,ISIZ,ITYP) - 3.0)&
                      &/ (H1 - 3.0))
                 if (PMORT(ISIZ,ITYP).lt.0.0) PMORT(ISIZ,ITYP) = 0.0
              end if

           end if

        end if

        110 continue
     end do
     100 continue
  end do

  !.... ---CALCULATE MORTALITY: PMORT PROP OF TREES DIE, REST MAY BE SEVERELY
  !     ---SCORCHED (IF CLSP IS HIGH ENOUGH) AND BECOME "SPECIAL" TREES.

  !     I ASSUME THAT ALL BURNED TREES REMAIN STANDING INITIALLY

  do ITYP = 1,2
     do ISIZ = 1,NSCL
        OAKILL(ISTD,ISIZ,ITYP) = OAKILL(ISTD,ISIZ,ITYP)&
             &+ PMORT(ISIZ,ITYP)

        !....       SOME BEETLE-KILLED TREES WILL BE BURNED BY FIRE SO BKP CANNOT
        !             EMERGE.  THESE TREES ARE REMOVED FROM PBKILL AND ALLKLL ARRAYS
        !             (note: pbkill (and allkll?) are in tpa here)

        if (ITYP .eq.1) then
           PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) * (1-PMORT(ISIZ,1))
           ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) * (1-PMORT(ISIZ,1))
        end if

        !           CREATE STANDING DEAD WOOD (WHY NOT DOWNED WOOD TOO?)
        J = L2D(ISIZ)
        if (ITYP .eq.1.and. J .gt.0)&
             &DWPHOS(ISTD,1,J) = DWPHOS(ISTD,1,J) +&
             &PMORT(ISIZ,ITYP) * TREE(ISTD,ISIZ,ITYP)
        J = J + 1
        JJ = IQPTYP(ISTD,ITYP)
        SDWP(ISTD,JJ,J,1) = SDWP(ISTD,JJ,J,1) + PMORT(ISIZ,ITYP)&
             &* TREE(ISTD,ISIZ,ITYP) * TVOL(ISTD,ISIZ,ITYP)

        !....       IF MORE THAN 1/4 CROWN SCORCHED AND IS HOST THEN SCORCH
        !           IS A PROPORTION

        if ((ITYP .eq.1) .and. (CSLP .gt.25.0))&
             &SCORCH(ISTD,ISIZ) = 1.0 - PMORT(ISIZ, ITYP)

        210 continue
     end do
     200 continue
  end do

  if (LBMDEB) write(JBMBPR,99) ISTD,IYR
  99 format(' End BMFMRT: Stand = ', I8,' Year = ',I5)

  return
end
