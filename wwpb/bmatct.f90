!===================!
!   wwpb : bmatct   !
!===================!
! WWPB $Id$
subroutine BMATCT(IYR)

  !     CALLED FROM BMDRV
  !     CALLS:  SPLAAR
  !             SPLADS
  !             SPLALO
  !             GPGET2

  !  **BMATCT  Date of last revision:  09/28/05

  !  Purpose:

  !     This routine moves BKP between stands, and between each stand
  !     and the Outside world. Each stand and the Outside is given a
  !     "score" based on how it looks from the source stand.
  !     Then all the scores are scaled and the appropriate proportion of
  !     BKP is moved from away from the source stand into the target.
  !     This is done for both the primary pest and for Ips.
  !     In cases where Ips is the primary pest, only one set
  !     of calculations are done. The numerator of the score equation
  !     is calculated in SUBROUTINE BMCNUM

  !  Call list definitions:
  !     IYR:    Current year.

  !  Local variable definitions:
  !     ASPEC:  Variable used to scale attractiveness of "special" trees
  !     ATTOJ:  ATTractiveness of the Outside for each beetle type in the
  !             current stand
  !     ATTWP:  average ATTractiveness of the World (area within RMAX) of
  !             each point in the Outside
  !     BAHO:   Basal Area of Host per acre in the Outside
  !     BAO:    total Basal Area per acre in the Outside
  !     BASP0:  Basal Area of Special trees per acre in the Outside
  !     BKPO:   the amount of BKP produced per acre in the Outside this year,
  !             for each beetle type
  !     CDIST:  half the length of a square with area=CAREA
  !     DDWT:   the variable known as 'c' in the dispersal equations
  !     IPASS:  Loop counter for current "pass" over stands
  !     KO:     K value (amount of nice host per acre) of the Outside, for
  !             each beetle type
  !     KAV:    AVerage K value (amount of nice host per acre) of the area
  !             within RMAX of each point in the Outside
  !     KL:     the area-weighted average K value (amount of nice host per
  !             acre)in the Landscape
  !     KLS:    the area-weighted average K value for the Stockable parts of
  !             the Landscape
  !     LOK:    Logical indicating if OW values are to change this year.
  !     NEWBKP: Array containing the new value for bkp for each stand and
  !             beetle type
  !     NPASS:  Number of "passes" to make over stands (will be 2 if both
  !             MPB/WPB and Ips are simulated)
  !     NZERO:  Scalar used to reduce attractiveness of stands
  !     PROP:   Proportional attractivenss rating of each stand relative
  !             to all other stands
  !     RADO:   RADius of Outside world (with the landscape at its center)
  !     RATTJO: Relative Attractiveness of current stand for outside beetles
  !             of the current type.
  !     RVO:    host Rating Value in the Outside (all effects combined)
  !     RVOND:  host Rating Value Outside due to everything except Not Drought
  !     SCORE:  Array containing the score for each stand's attractiveness
  !             for each beetle type
  !     SPO:    number of SPecial trees per acre in the Outside
  !     TDIST:  half the length of a square with area=TAREA
  !     TIPSDV: Total BKP for stand if Ips is a driving variable
  !     TOTBKP: Accumulates total BKP across stands
  !     TOTSC:  Array containing the total stand attractiveness for each
  !             beetle type

  !  Common block variables and parameters:
  !     BKP:    From BMCOM; beetle kill pressure
  !     BKPIps: From BMCOM; Ips beetle kill pressure
  !     CBAHO:  'Constant' value of Basal Area per acre of Host in the Outside
  !     CBAO:   'Constant' value of total Basal Area per acre in the Outside
  !     CBASPO: 'Constant' value of Basal Area per acre of SPecial trees Outside
  !     CBKPO:  'Constant' value of BKP produced per stockable acre in the Outside
  !     CRVOND: 'Constant' value of host Rating Value Outside except Not Drought
  !     CSPO:   'Constant' value of Basal Area per acre of Host in the Outside
  !     IPSON:  From BMCOM; logical operator for Ips as a driving variable
  !     NUMER:  (2,BMSTND) Numerator of scoring equation for # special trees
  !              First element is for MBP/WPB; second is for Ips.
  !     PBSPEC: From BMCOM; switch identifying which pine beetle species
  !             are being modeled
  !     RVOD:   host Rating Value Outside due to Drought effects
  !     STOCK:  logical variable to indicate whether each stand is STOCKable
  !     STOCKO: proportion of the Outside that is STOCKable
  !     TFOOD:  From BMCOM; total beetle attractive 'food'
  !     ATTC:   the 'ATTractiveness Constant' of each stand (eqn.9)
  !     ATTBYK: ATTractiveness of each stand to itself, divided by its K value
  !     AREAL:  total area of the Landscape, in sq.mi.
  !     AREALP: approximate area of the Landscape that is within RMAX of the
  !             Outside (P for 'prime'), in sq.mi.
  !     AREALS: total Stockable area of the Landscape, in sq.mi.
  !     AREAO:  total area of the Outside, in sq.mi.
  !     RADL:   RADius of a circle just large enough to enclose the Landscape
  !     RADLC:  RADius that the Landscape would have if it was circular
  !     SDD:    From BMCOM:

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'
  include 'PPCNTL.f90'
  include 'BMCOM.f90'
  include 'BMPCOM.f90'

  !.... Call Statement Variable Declarations:
  ! Variables
  integer :: IYR

  ! Convert acres to square miles and meters to miles: All units are
  ! now in miles. (This is not true of all routines.)

  real    :: AC2SQM,M2MI,MI2M

  parameter (AC2SQM = 1./640.)
  parameter (M2MI = 1./1609.34)
  parameter (MI2M = 1609.34)

  !.... Variable declarations.

  integer :: IPASS,ISIZ
  integer :: IRC,DUM(1)
  integer :: TARG,CURR

  logical :: LOK
  real    :: PRMS(7)

  real    :: DIST,TAREA,CAREA,XX,R2
  real    :: NEWBKP(2,MXSTND)
  real    :: PROP
  real    :: ATTOJ(2),RATTJO
  real    :: SCORE(2,MXSTND)
  real    :: TOTBKP(2)
  real    :: TOTSC(2)

  real    :: ASPEC(2),NZERO(2),DDWT(2),RMAX(2),DX,TX(2),SX(2)
  real    :: RADO(2),KL(2),KLS(2),KAV(2),KO(2),BKPO(2)
  real    :: ATTWP(2)
  real    :: XP,YP,XMIN,XMAX,YMIN,YMAX,XMID,YMID
  real    :: D,DMAX,CDIST,TDIST

  real    :: SPO(2),BAO,BAHO(2),BASPO(2)
  real    :: XSPO(2),XBAO,XBAHO(2),XBASPO(2),XBKPO(2),XRVO
  real    :: TBAHO,TBASPO,TSPO
  real    :: RVO,RVOND
  real    :: MIN
  real    :: ALPHA
  real    :: TESTVAR
  real    :: AREAOX(2)
  !...AREAOX added for new BKPin from OW calculation. 9/2/99 (ajm)
  if (LBMDEB) write(JBMBPR,5) IYR
  5 format(' Begin BMATCT: Year= ',I5)

  ! Begin Routine

  NPASS = 1
  if (IPSON) NPASS = 2

  !     Initializations from ATTRACT keyword.  The first element of each array
  !     gives the value for the main beetle, the second element gives the value
  !     for Ips when it acts as a secondary beetle.
  ASPEC(1) = USERA(PBSPEC)
  NZERO(1) = SELFA(PBSPEC)
  DDWT(1) = USERC(PBSPEC)
  RMAX(1) = URMAX(PBSPEC)

  if (NPASS .eq.2) then
     ASPEC(2) = USERA(3)
     NZERO(2) = SELFA(3)
     DDWT(2) = USERC(3)
     RMAX(2) = URMAX(3)
  end if

  !     If the outside world is not being used, then don't bother calling the
  !     option processor.

  if (OUTOFF) goto 3

  !     Call the option processor to see if any values are going to change this
  !     year. Once values are changed, they will remain that value until another
  !     call to the option processor changes them again. Note that since we are
  !     not in a stand loop, we do not need the initial IF statement used in
  !     other calls.

  IYR1 = IYR
  NPRMS = 6

  call GPGET2 (310,IYR1,7,NPRMS,PRMS(1),1,I,DUM,LOK)

  if (LOK) then

     if (PRMS(1) .ge.0.0) CBKPO(1) = PRMS(1)
     if (PRMS(2) .ge.0.0) CBAHO(1) = PRMS(2)
     if (PRMS(3) .ge.0.0) CBASPO(1) = PRMS(3)
     if (PRMS(4) .ge.0.0) CSPO(1) = PRMS(4)
     if (PRMS(5) .ge.0.0) CBAO = PRMS(5)
     if (PRMS(6) .ge.0.0) CRVOND = PRMS(6)
  end if

  !     Now call the option processor to see if values for Ips (as a driving
  !     variable) will change this year. Only bother calling if Ips is a DV.

  if (IPSON) then

     IYR1 = IYR
     NPRMS = 4

     call GPGET2 (311,IYR1,7,NPRMS,PRMS(1),1,I,DUM,LOK)

     if (LOK) then
        if (PRMS(1) .ge.0.0) CBKPO(2) = PRMS(1)
        if (PRMS(2) .ge.0.0) CBAHO(2) = PRMS(2)
        if (PRMS(3) .ge.0.0) CBASPO(2) = PRMS(3)
        if (PRMS(4) .ge.0.0) CSPO(2) = PRMS(4)
     end if

  end if

  3 continue

  !                      START HERE IN THE FIRST YEAR.

  ! Skip all the spatial stuff and initial averaging if you've already done it.
  if (ACDONE) goto 50

  !         First, find ATTBYK, which is needed even if Outside is turned off.
  !         This is found from equation 9, with R2 = the radius that the stand
  !         would have if it was a circle, and R1 = 0.

  do CURR = 1,BMSTND
     call SPLAAR(BMSDIX(CURR), CAREA, IRC)
     CAREA = CAREA * AC2SQM
     R2 = SQRT(CAREA / PIE)

     do IPASS = 1,NPASS
        if (R2.gt. RMAX(IPASS)) R2 = RMAX(IPASS)
        ATTBYK(IPASS,CURR) = (PIE / DDWT(IPASS))&
             &* (LOG(ABS(DDWT(IPASS) * R2 * R2 + NZERO(IPASS)))&
             &- LOG(ABS(NZERO(IPASS))))
        7 continue
     end do
     8 continue
  end do

  !         If the Outside is turned off, this is all you need - go past Go
  !         and collect $200.
  if (OUTOFF) then
     ACDONE = .true.
     goto 50
  end if

  !         Zero some landscape-average counters that may be needed.
  XBAO = 0.0
  XRVO = 0.0
  do IPASS = 1,NPASS
     XBKPO(IPASS) = 0.0
     XBAHO(IPASS) = 0.0
     XSPO(IPASS) = 0.0
     XBASPO(IPASS) = 0.0
     9 continue
  end do

  !         Figure out centre of landscape:
  XMAX = -1.0
  YMAX = -1.0
  XMIN = 1.e35
  YMIN = 1.e35
  do CURR = 1,BMSTND
     call SPLALO(BMSDIX(CURR), XP, YP, IRC)
     if (XP .gt. XMAX) XMAX = XP
     if (XP .lt. XMIN) XMIN = XP
     if (YP .gt. YMAX) YMAX = YP
     if (YP .lt. YMIN) YMIN = YP
     10 continue
  end do

  XMID = (XMIN + XMAX) / 2.
  YMID = (YMIN + YMAX) / 2.

  !         Find RADL, the greatest extent of Landscape, including stand area.
  !         Also in this loop, add up any landscape averages that are needed
  !         for calculating KO.  Values are needed if C[value] is less than
  !         zero and the outside world is not floating.

  AREAL = 0.0
  AREALS = 0.0
  DMAX = -1.
  do CURR = 1,BMSTND
     call SPLAAR(BMSDIX(CURR), CAREA, IRC)
     CAREA = CAREA * AC2SQM

     AREAL = AREAL + CAREA

     D = SQRT(CAREA / PIE) * MI2M

     call SPLALO(BMSDIX(CURR), XP, YP, IRC)
     D = D + SQRT((XMID-XP)**2 + (YMID-YP)**2)
     if (D .gt. DMAX) DMAX = D

     !           Do not include non-stockable stands in the following averages.
     if (.not. STOCK(CURR)) goto 13
     AREALS = AREALS + CAREA

     !           If needed, add up the total BA of each current stand, and average RV.
     if ((CBAO .lt.0.0) .and. (UFLOAT .ne. -1.0))&
          &XBAO = XBAO + CAREA*(BAH(CURR,NSCL+1)+BANH(CURR,NSCL+1))
     if ((CRVOND .lt.0.0) .and. (UFLOAT .ne. -1.0))&
          &XRVO = XRVO + CAREA * GRFSTD(CURR)

     !           If needed, add up host BA, special trees and BKP of each current
     !           stand, for each beetle type.
     do IPASS = 1,NPASS

        if ((CBKPO(IPASS) .lt.0.0) .and. (UFLOAT .ne. -1.0)) then
           if ((PBSPEC .ne.3) .and. (IPASS .eq.1)) then
              XBKPO(IPASS) = XBKPO(IPASS)+(CAREA*BKP(CURR))
           else
              XBKPO(IPASS) = XBKPO(IPASS)+(CAREA*BKPIPS(CURR))
           end if
        end if

        if (((CBAHO(IPASS) .ge.0.0) .and. (CSPO(IPASS) .ge.0.0)&
             &.and. (CBASPO(IPASS) .ge.0.0)) .or. (UFLOAT .eq. -1.0))&
             &goto 12

        !             Find the host size-threshold for each pest species
        if (IPASS .eq.1) then
           MIN = ISCMIN(PBSPEC)
        else
           MIN = ISCMIN(3)
        end if

        !             Add up the amount of host and special trees in each size class
        TBAHO = 0.0
        TBASPO = 0.0
        TSPO = 0.0
        do ISIZ = 1,NSCL
           if (ISIZ .ge. MIN) TBAHO = TBAHO + BAH(CURR,ISIZ)
           XX = TREE(CURR,ISIZ,1) * SPCLT(CURR,ISIZ,IPASS)
           TSPO = TSPO + XX
           if (ISIZ .ge. MIN) TBASPO = TBASPO + XX * MSBA(ISIZ)
           11 continue
        end do

        XBAHO(IPASS) = XBAHO(IPASS) + CAREA * TBAHO
        XSPO(IPASS) = XSPO(IPASS) + CAREA * TSPO
        XBASPO(IPASS) = XBASPO(IPASS) + CAREA * TBASPO
        12 continue
     end do

     13 continue
  end do

  RADL = DMAX * M2MI

  !         If needed, divide landscape sums by AREALS to get the required averages.
  !         Re-initialize any array values that have been set to negative numbers
  !         because the initial averages were missing.
  if (UFLOAT .eq. -1.0) goto 21

  if (CBAO .lt.0.0) CBAO = XBAO / AREALS
  if (CRVOND .lt.0.0) CRVOND = (XRVO / AREALS) / RVOD

  do IPASS = 1,NPASS
     if (CBKPO(IPASS) .lt.0.0)&
          &CBKPO(IPASS) = XBKPO(IPASS) / AREALS
     if (CBAHO(IPASS) .lt.0.0)&
          &CBAHO(IPASS) = XBAHO(IPASS) / AREALS
     if (CBASPO(IPASS) .lt.0.0)&
          &CBASPO(IPASS) = XBASPO(IPASS) / AREALS
     if (CSPO(IPASS) .lt.0.0)&
          &CSPO(IPASS) = XSPO(IPASS) / AREALS
     20 continue
  end do
  21 continue

  !         equations 5, 11, 15 and 16:
  RADLC = SQRT(AREAL / PIE)

  do IPASS = 1,NPASS
     RADO(IPASS) = RADL + RMAX(IPASS)
     AREAO(IPASS) = (PIE * RADO(IPASS) * RADO(IPASS)) - AREAL

     if (RADLC .gt. RMAX(IPASS)) then
        AREALP(IPASS) = AREAL - (PIE * (RADLC - RMAX(IPASS))**2)
     else
        AREALP(IPASS) = AREAL
     end if
     211 continue
  end do

  !       Find the 'attractiveness constant' of each stockable stand.  TX is the
  !       logarithmic term in equation 9 (which is constant for all stands), and
  !       SX is the stand-summation term.  Distances and areas are converted here
  !       to MILES or SQUARE MILES.  CDIST and TDIST are used to ensure that no
  !       pair of current and target stands are considered to be at closer AVERAGE
  !       distance than is physically possible (i.e. closer than the distance between
  !       the centers of two squares with their areas).
  do IPASS = 1,NPASS
     TX(IPASS) = (PIE / DDWT(IPASS))&
          &* (LOG(ABS(DDWT(IPASS)*RMAX(IPASS)*RMAX(IPASS)+NZERO(IPASS)))&
          &- LOG(ABS(NZERO(IPASS))))
     22 continue
  end do

  do CURR = 1,BMSTND
     if (.not. STOCK(CURR)) goto 30
     call SPLAAR(BMSDIX(CURR),CAREA,IRC)
     CDIST = 0.5 * SQRT(CAREA * AC2SQM)

     SX(1) = 0.0
     SX(2) = 0.0
     do TARG = 1,BMSTND

        if (TARG .eq. CURR) then
           do IPASS = 1,NPASS
              SX(IPASS) = SX(IPASS) + ATTBYK(IPASS,CURR)
              23 continue
           end do

        else
           call SPLADS(BMSDIX(TARG),BMSDIX(CURR),DIST,IRC)
           DIST = DIST * M2MI
           call SPLAAR(BMSDIX(TARG),TAREA,IRC)
           TAREA = TAREA * AC2SQM
           TDIST = 0.5 * SQRT(TAREA)
           if (DIST .lt. (CDIST+TDIST)) DIST = CDIST + TDIST

           do IPASS = 1,NPASS
              if (DIST .le. RMAX(IPASS)) then
                 DX = DDWT(IPASS) * DIST * DIST
                 SX(IPASS) = SX(IPASS) + (TAREA / (DX + NZERO(IPASS)))
              end if
              24 continue
           end do
        end if

        25 continue
     end do

     !         calculate ATTC from equation 9, and make sure it's not negative
     do IPASS = 1,NPASS
        ATTC(IPASS,CURR) = TX(IPASS) - SX(IPASS)
        if (ATTC(IPASS,CURR) .lt.0.0) ATTC(IPASS,CURR) = 0.0
        27 continue
     end do

     30 continue
  end do

  ! This is the end of the "spatial stuff".
  ACDONE = .true.
  50 continue

  !                     START HERE IN THE SECOND YEAR.

  ! You can skip some stuff if the Outside is turned off.
  if (OUTOFF) goto 91

  ! Calculate KL.  If the Outside is floating, also find average BKP in stockable
  ! stands, and KLS.
  do IPASS = 1,NPASS
     KL(IPASS) = 0.0
     KLS(IPASS) = 0.0
     XBKPO(IPASS) = 0.0
     60 continue
  end do

  do CURR = 1,BMSTND
     call SPLAAR(BMSDIX(CURR),CAREA,IRC)
     CAREA = CAREA * AC2SQM

     do IPASS = 1,NPASS
        XX = CAREA * NUMER(IPASS,CURR)
        KL(IPASS) = KL(IPASS) + XX

        if ((UFLOAT .eq. -1.0) .and. (STOCK(CURR))) then
           KLS(IPASS) = KLS(IPASS) + XX

           if ((PBSPEC .ne.3) .and. (IPASS .eq.1)) then
              XBKPO(IPASS) = XBKPO(IPASS)+(CAREA*BKP(CURR))
           else
              XBKPO(IPASS) = XBKPO(IPASS)+(CAREA*BKPIPS(CURR))
           end if

        end if

        70 continue
     end do
     75 continue
  end do

  do IPASS = 1,NPASS
     KL(IPASS) = KL(IPASS) / AREAL
     76 continue
  end do

  ! At this point, all initial values for the Outside are known.  Get KO
  ! and BKPO for the current year.  If the Outside is floating, these are just
  ! the landscape averages for stockable stands.  Otherwise, KO must be calculated
  ! from some component values.
  if (UFLOAT .eq. -1.0) then

     do IPASS = 1,NPASS
        KO(IPASS) = KLS(IPASS) / AREALS
        BKPO(IPASS) = XBKPO(IPASS) / AREALS
        80 continue
     end do

  else

     BAO = CBAO
     RVOND = CRVOND
     RVO = RVOND * RVOD
     do IPASS = 1,NPASS
        BAHO(IPASS) = CBAHO(IPASS)
        BASPO(IPASS) = CBASPO(IPASS)
        SPO(IPASS) = CSPO(IPASS)
        BKPO(IPASS) = CBKPO(IPASS)
        KO(IPASS) = (ASPEC(IPASS)*SPO(IPASS) + 1) * BAO&
             &* (BAHO(IPASS)+BASPO(IPASS)) / RVO
        85 continue
     end do

     !     If this is a bad reproductive year then reduce the BKP outside as well
     !     Notice that it may only be a bad year for one beetle type.

     if (LBAD) then
        if (IBADBB .ne.3.or. (IBADBB .eq.3.and. NPASS .eq.1))&
             &BKPO(1) = BKPO(1) * BADREP(PBSPEC)
        if (IBADBB .ge.3.and. NPASS .eq.2)&
             &BKPO(2) = BKPO(2) * BADREP(3)
     end if

  end if

  ! If parts of the Outside are non-stockable, reduce KO and BKPO accordingly.
  if (STOCKO .lt.1.0) then
     do IPASS = 1,NPASS
        KO(IPASS) = KO(IPASS) * STOCKO
        BKPO(IPASS) = BKPO(IPASS) * STOCKO
        87 continue
     end do
  end if

  ! Calculate KAV from equation 17, and ATTWP from equation 18:
  do IPASS = 1,NPASS
     !        KAV(IPASS) = (KL(IPASS) + KO(IPASS)) / 2

     KAV(IPASS) = ((AREAO(IPASS) * KO(IPASS))&
          &+ (AREALP(IPASS) * KL(IPASS)))&
          &/ (AREAO(IPASS) + AREALP(IPASS))
     !        ATTWP(IPASS) = (PIE / DDWT(IPASS)) * KL(IPASS) *
     ATTWP(IPASS) = (PIE / DDWT(IPASS)) * KAV(IPASS) *&
          &(LOG(ABS(DDWT(IPASS) *RMAX(IPASS) *RMAX(IPASS) +NZERO(IPASS)))&
          &- LOG(ABS(NZERO(IPASS))))
     !...  KAV (above) changed to KL (ajm 8/30/99)
     90 continue
  end do

  !                   START HERE IF OUTSIDE IS TURNED OFF.

  91 continue

  ! Zero some BKP counters
  do CURR = 1,BMSTND
     if (.not. STOCK(CURR)) goto 95
     do IPASS = 1,NPASS
        NEWBKP(IPASS,CURR) = 0.0
        BKPOUT(IPASS,CURR) = 0.0
        BKPIN(IPASS,CURR) = 0.0
        SELFBKP(IPASS,CURR) = 0.0 !NEW AUG 05 AJM
        94 continue
     end do
     95 continue
  end do

  ! Loop over each source stand, dispersing the beetles from stockable stands.
  do CURR = 1,BMSTND
     if (.not. STOCK(CURR)) goto 100
     call SPLAAR(BMSDIX(CURR),CAREA,IRC)

     !       For each beetle type, zero TOTSC, find ATTOJ from equation 9, and
     !       find the total BKP in the stand (while CAREA is still in acres).
     do IPASS = 1,NPASS
        TOTSC(IPASS) = 0.0
        if (.not. OUTOFF) ATTOJ(IPASS) = KO(IPASS) * ATTC(IPASS,CURR)
        if ((PBSPEC .ne.3) .and. (IPASS .eq.1)) then
           TOTBKP(IPASS) = CAREA * BKP(CURR)
        else
           TOTBKP(IPASS) = CAREA * BKPIPS(CURR)
        end if
        101 continue
     end do

     CAREA = CAREA * AC2SQM
     CDIST = 0.5 * SQRT(CAREA)

     !       Loop over the target stands finding their total attractiveness
     do TARG = 1,BMSTND

        !         For your own stand, attractiveness is given by equation 4
        if (TARG .eq. CURR) then
           do IPASS = 1,NPASS
              SCORE(IPASS,TARG) = NUMER(IPASS,CURR) * ATTBYK(IPASS,CURR)
              TOTSC(IPASS) = TOTSC(IPASS) + SCORE(IPASS,TARG)
              119 continue
           end do

           !         For others, attractiveness is approximated from equation 1,
           !         except that stands further than RMAX have an attractiveness of 0.
           !         Again, CDIST and TDIST are used to ensure that no pair of stands
           !         are taken to be closer on average than is physically possible.
        else
           call SPLADS(BMSDIX(TARG),BMSDIX(CURR),DIST,IRC)
           DIST = DIST * M2MI
           call SPLAAR(BMSDIX(TARG),TAREA,IRC)
           TAREA = TAREA * AC2SQM
           TDIST = 0.5 * SQRT(TAREA)
           if (DIST .lt. (CDIST+TDIST)) DIST = CDIST + TDIST

           do IPASS = 1,NPASS
              if (DIST .le. RMAX(IPASS)) then
                 DX = DDWT(IPASS) * DIST * DIST
                 !                DX= (DDWT(IPASS) * DIST) ** 2
                 SCORE(IPASS,TARG) = TAREA * NUMER(IPASS,TARG)&
                      &/ (DX + NZERO(IPASS))
                 TOTSC(IPASS) = TOTSC(IPASS) + SCORE(IPASS,TARG)
              else
                 SCORE(IPASS,TARG) = 0.0
              end if
              120 continue
           end do

        end if
        110 continue
     end do

     !       Calculate BKP flux between the current source stand and the
     !       Outside world for each beetle type,unless the Outside world is
     !       turned off.
     if (OUTOFF) goto 124

     do IPASS = 1, NPASS

        !         Add attractiveness of Outside to the total for the current stand,
        !         and calculate BKP lost to the Outside (BKPOUT).
        TOTSC(IPASS) = TOTSC(IPASS) + ATTOJ(IPASS)

        if (TOTSC(IPASS) .gt.0.) BKPOUT(IPASS,CURR) = TOTBKP(IPASS)&
             &* (ATTOJ(IPASS) / TOTSC(IPASS))

        !         Use eqn.24 to calculate RATTJO
        XX = KO(IPASS) * AREAO(IPASS) * ATTWP(IPASS)
        if (XX .gt.0.0) then
           RATTJO = (CAREA * NUMER(IPASS,CURR) * ATTOJ(IPASS)) / XX

        else
           RATTJO = 0.0
        end if

        !         Find total BKP in Outside, and calculate BKP entering the current
        !         stand from the outside world (BKPIN).  Add this amount to NEWBKP.
        !         BKPIN(IPASS,CURR)= BKPO(IPASS) * (AREAO(IPASS) / AC2SQM) *RATTJO
        AREAOX(IPASS) = 2753.04 * (1- EXP(-0.06366 * RADLC)) **1.5938&
             &* (1 - EXP(-0.0687 * RMAX(IPASS))) ** 0.8827
        !         BKPIN(IPASS,CURR)= BKPO(IPASS)* (AREAO(IPASS)/AC2SQM)* RATTJO
        BKPIN(IPASS,CURR) = BKPO(IPASS)* (AREAOX(IPASS)/AC2SQM)* RATTJO
        NEWBKP(IPASS,CURR) = NEWBKP(IPASS,CURR) + BKPIN(IPASS,CURR)
        !          WRITE (38, '(35X, 2F9.2)') AREAOX(1), AREAO
        123 continue
     end do

     124 continue

     !       Allocate BKP from the current stand to each target stand.
     do TARG = 1,BMSTND
        do IPASS = 1,NPASS
           PROP = 0.
           if (TOTSC(IPASS) .gt.0.0)&
                &PROP = SCORE(IPASS,TARG) / TOTSC(IPASS)
           !    NEW VAR TO HOLD NEWBKP FROM SELF AJM 8.05
           if (TARG.eq.CURR)SELFBKP(IPASS,TARG) = PROP*TOTBKP(IPASS)
           NEWBKP(IPASS,TARG) = NEWBKP(IPASS,TARG)+(PROP*TOTBKP(IPASS))

           201 continue
        end do
        200 continue
     end do

     100 continue
  end do

  do CURR = 1,BMSTND

     !     *** I THINK THERE SHOULD BE A STOCK() TEST HERE... SO I  ***
     !     *** PUT ONE IN                                           ***

     if (.not. STOCK(CURR)) goto 300

     !       Change back to PER AREA (Acres)

     call SPLAAR(BMSDIX(CURR),CAREA,IRC)

     do 301, IPASS = 1,2
     NEWBKP(IPASS,CURR) = NEWBKP(IPASS,CURR) / CAREA
     BKPOUT(IPASS,CURR) = BKPOUT(IPASS,CURR) / CAREA
     BKPIN(IPASS,CURR) = BKPIN(IPASS,CURR) / CAREA
     SELFBKP(IPASS,CURR) = SELFBKP(IPASS,CURR) / CAREA
     301 continue

     !       Calculate how much BKP actually makes it into the stand.
     !       Any remainder is lost and not reallocated to another stand.
     !       (TFOOD includes a multiplier for prob host ba in stand -- see
     !       BMCNUM for calculation.)

     !...The 0.4 multiplier replaced by ALPHA. SDD is from varyrain (MJO, Zhang July98).

     if (SDD .lt.0) then
        !           ALPHA = (0.565 - (0.085 * SDD))
        ALPHA = (0.365 - (0.178 * SDD))
     else
        !           ALPHA = (0.565 - (0.034 * SDD))
        ALPHA = (0.365 - (0.034 * SDD))
     end if

     if (ALPHA .gt.0.90) ALPHA = 0.90
     if (ALPHA .lt.0.01) ALPHA = 0.01
     if (PBSPEC .ne.3) then
        if ((NEWBKP(1,CURR) .gt.0.0)&
             &.and. (TFOOD(CURR,1) .gt.0.0)) then
           BKP(CURR) = TFOOD(CURR,1) * (1.0 -&
                &EXP(-(ALPHA * NEWBKP(1,CURR) / (TFOOD(CURR,1) + 1E-6))))
           BKPS(CURR) = BKP(CURR) / NEWBKP(1,CURR)
        else
           BKP(CURR) = 0.0
           BKPS(CURR) = 1.0
        end if
        BKPA(CURR) = BKP(CURR)

        if (NEWBKP(2,CURR) .gt.0.0) then
           BKPIPS(CURR) = TFOOD(CURR,2) * (1.0 -&
                &EXP(-(ALPHA * NEWBKP(2,CURR) / (TFOOD(CURR,2) + 1E-6))))
        else
           BKPIPS(CURR) = 0.0
        end if

     else
        if (NEWBKP(1,CURR) .gt.0.0) then
           BKPIPS(CURR) = TFOOD(CURR,1) * (1.0 -&
                &EXP(-(ALPHA * NEWBKP(1,CURR) / (TFOOD(CURR,1) + 1E-6))))
           BKPS(CURR) = BKPIPS(CURR) / NEWBKP(1,CURR)
        else
           BKPIPS(CURR) = 0.0
           BKPS(CURR) = 1.0
        end if
        BKPA(CURR) = BKPIPS(CURR)
     end if

     !     Put a tiny amount of BKP in the stand as a background level (too little to
     !     do anything unless the small trees are really sick)

     !        IF ((BKP(CURR) .EQ. 0.0) .AND. (BAH(CURR,NSCL+1) .GT. 0.0))
     !     &         BKP(CURR)= 0.25 * MSBA(1) / CAREA

     !        IF ((BKPIPS(CURR) .EQ. 0.0) .AND. (BAH(CURR,NSCL+1) .GT. 0.0))
     !     &         BKPIPS(CURR)= 0.25 * MSBA(1) / CAREA

     300 continue
  end do

  if (LBMDEB) write(JBMBPR,2) IYR
  2 format(' End BMATCT: Year= ',I5)

  return
end
