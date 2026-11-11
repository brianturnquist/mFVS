!==============================!
!   canada/fire/bc : fmcroww   !
!==============================!
! CANADA-FIRE-BC $Id$
subroutine FMCROWW (SPI,D,H,ITRNC,IC,HP,SG,XV)
  implicit none

  !     CALLED FROM: FMCROW

  !  Purpose:
  !     This subroutine calculates CROWNW(tree,size), the weight of
  !     various sizes of crown material that is associated with each tree
  !     record in the current stand.  These weights depend on tree
  !     species, diameter, height, and crown ratio according to the
  !     relationships described in Brown & Johnston, 1976, 'Debris
  !     Prediction System', Fuel Science RWU 2104, which is itself
  !     based primarily on Res Paper INT-197.

  !     NOTE:  whitebark pine from Brown
  !     NOTE:  oak from Chojnacky (1992)
  !     NOTE:  pinyon and juniper from Grier (1992)
  !     NOTE:  bigleaf maple from Snell & Little (1983)
  !     NOTE:  The allocation between crown size class 4 and 5 is
  !            somewhat arbitrary, with class 5 currently not receiving any.

  !     SPECIES MAPPING (BASED ON CR AND ELABORATED FROM THERE)

  !     1 = SUBALPINE FIR, corkbark, subalpine larch
  !     2 = ASPEN
  !     3 = DOUGLAS-FIR, bigcone
  !     4 = GRAND FIR, white, Cal red, shasta red, red, Pacific silver, noble
  !     5 = BIGLEAF MAPLE, mountain, bigtooth
  !     6 = WESTERN HEMLOCK
  !     7 = WESTERN REDCEDAR, pacific yew
  !     8 = WESTERN LARCH, Alaska cedar
  !     9 = BRISTLECONE PINE
  !    10 = MADRONE, white alder
  !    11 = LODGEPOLE PINE, limber, knobcone, foxtail, Coulter, Gray, Washoe
  !    12 = PINYON PINE, singleleaf
  !    13 = PONDEROSA PINE, Monterey
  !    14 = WHITEBARK PINE
  !    15 = WESTERN WHITE PINE, southwestern white, Jeffrey, sugar
  !    16 = ROCKY MTN JUNIPER, western, Utah, California
  !    17 = TANOAK, chinkapin, coast live oak, canyon live oak, Engelmann oak
  !                 interior live oak, nutmeg, laurel, California live oak,
  !    18 = ENGELMANN SPRUCE, blue, white, brewer, Sitka
  !    19 = GIANT SEQUOIA, coast redwood
  !    20 = INCENSE CEDAR
  !    21 = CALIFORNIA BLACK OAK - based on tanoak, blue oak, valley oak
  !    22 = GAMBEL OAK
  !    23 = RED ALDER
  !    24 = MOUNTAIN HEMLOCK
  !    25 = PONDEROSA PINE - BLACK HILLS

  !  Local variable definitions:
  !     C:        length of live Crown
  !     D;        Dbh
  !     DBR1-4:   DRY WT (KG) OF BRANCH COMPONENTS
  !     DEADWT:   WeighT of DEAD crown of the focal tree
  !     DOMPCT:   Percentile at which trees are considered dominant or co-dom.
  !     DP1-3:    Dead crown Proportions, each being the sum of the pro-
  !               portions of all smaller material.
  !     DBHCUT:   Breakpoint for sm. and lg. tree crown biomass estimates
  !     DCTLOW:   point where interpolation between sm. and lg. tree
  !               estimates starts
  !     DCTHGH:   point where interpolation between sm. and lg. tree
  !               estimates stops
  !     H:        Height
  !     HPCT:     Height PerCenTile ranking of each tree list element
  !     HPOINT:   Height POINTer: specifies which tree list element has
  !               the tallest trees, next tallest, and so on.
  !     ITRNC     truncation height (aspen only)
  !     LIVEWT:   WeighT of LIVE crown of the focal tree
  !     P1-4:     live crown Proportions, each being the sum of the
  !               proportions of all smaller material.
  !     R:        crown Ratio
  !     SP:       SPecies
  !     SMWGT:    WeiGT give to SMall tree estimate during interpolation
  !     TOTWT:    TOTal crown WeighT of the focal tree
  !     XVAL,YVAL:used to get SMWGT with ALGSLP

  !  Common block variables and parameters:

  !.... Parameter statements.
  !.... Parameter include files.
  !.... Variable declarations.

  ! Variables
  integer :: SPI,IC,ITRNC
  real    :: D,H,HP,SG,XV(0:5)

  integer :: J

  logical :: LDUM1
  real    :: C,DEADWT,DOMPCT,DP1,DP2,DP3
  real    :: LIVEWT,TEMP,P1,P2,P3,P4,R,TOTWT,DRC,X,X0
  real    :: V,DBHCUT,LDM
  real    :: VM,XDUM2,XDUM3
  real    :: DBR1,DBR2,DBR3,DBR4,DBR5,DFOL
  real    :: DCTLOW,DCTHGH,SMWGT,XVAL(3),YVAL(3),ALGSLP

  !.... Variables for new Black Hills PP equations
  real    :: TFOL,T1HRL,T1HRD,TCR

  data DOMPCT /60.0/

  DP1 = 0.0
  DP2 = 0.0
  DP3 = 1.0

  P1 = 0.0
  P2 = 0.0
  P3 = 0.0
  P4 = 0.0

  DEADWT = 0.0
  LIVEWT = 0.0
  TOTWT = 0.0

  TCR = (real(IC)/100)
  C = (real(IC) / 100.0) * H
  if (H .gt.0.0) R = (C / H) * 10.0

  !     INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
  !     OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.

  do J = 0,5
     XV(J) = 0.0
  end do

  if ((D .eq.0.0) .or. (H .eq.0.0) .or. (C .eq.0.0)) goto 999

  !***********************1.  CANOPIES OF SMALL TREES*********************

  !  THE SMALL/LARGE BREAPOINT IS NORMALLY 1 INCH DBH BUT SOME SPECIES
  !  REQUIRE DIFFERENT BREAKPOINTS.  BETWEEN DCTLOW AND DCTHGH
  !  WEIGHTING BETWEEN THE SMALL/LARGE EQUATIONS IS DONE.

  select case (SPI)
  case  (9,12,16) ! bristlecone pine, pinyon pine, R.M juniper
     DBHCUT = 2.0
     DCTLOW = 1.0
     DCTHGH = 3.0
  case  (22) ! Gambel oak
     DBHCUT = 4.0
     DCTLOW = 3.0
     DCTHGH = 5.0
  case  (11) ! lodgepole pine
     DBHCUT = 1.0
     DCTLOW = 0.5
     DCTHGH = 3.0
  case default ! ALL OTHERS
     DBHCUT = 1.0
     DCTLOW = 0.5
     DCTHGH = 2.0
  end select

  !  SET WEIGHTS BETWEEN LG. AND SM. MODELS FOR TOTAL CROWN BIOMASS

  XVAL(1) = DCTLOW
  XVAL(2) = DBHCUT
  XVAL(3) = DCTHGH
  YVAL(1) = 1.0
  YVAL(2) = 0.5
  YVAL(3) = 0.0
  SMWGT = ALGSLP(D,XVAL,YVAL,3)

  if (D .gt. DCTHGH) goto 299

  !       Calculate the total crown weight depending on tree species:
  !       (Use formulas from Brown 1978, page 14)

  select case (SPI)

     !         subalpine fir, corkbark fir
  case (1)
     TOTWT = EXP(-3.335 + 2.303*LOG(H))

     !         aspen - no height-based eqn available; set D=0.75"
  case (2)
     TOTWT = EXP(0.1942 + 2.8426*LOG(0.75))

     !         Douglas-fir
  case (3)
     TOTWT = EXP(-4.212 + 2.7168*LOG(H))

     !         grand fir
  case (4)
     TOTWT = 0.4284 * H

     !         bigleaf maple - predict weight of 1" dbh tree (.977)
     !         and scale back that based on the height of the 1"
     !         tree (7.728)
  case (5)
     TOTWT = 0.977 * H / 7.728

     !         western hemlock, mountain hemlock
  case (6,24)
     TOTWT = EXP(-5.126 + 2.563*LOG(H))

     !         western redcedar, giant sequoia, incense cedar
  case (7,19,20)
     TOTWT = 0.04833 * H * H

     !         western larch
  case (8)
     TOTWT = 0.1128*H + 0.00813 * H * H

     !         madrone -  weight predicted from height (8.2176 ft) of
     !         1" dbh tree and scaled from that based on the weight
     !         (0.54599 lbs) of the 1" tree
  case (10)
     TOTWT = 0.54599 * H / 8.2176

     !         lodgepole pine
  case (11)
     TOTWT = 0.03111 * H * H

     !         ponderosa pine
  case (13,25)
     TOTWT = 0.3451 * H

     !         whitebark pine
  case (14)
     TOTWT = 0.070 + 0.02446 * H * H

     !         western white pine
  case (15)
     TOTWT = 0.3292 * H

     !         tanoak - predicted from height (5.1213 ft) of 1" dbh tree
     !         (WS-FFE) and scaled from that based on the computed weight
     !         (0.81135 lbs) of the 1" tree
  case (17)
     TOTWT = 0.81135 * H / 5.1213

     !         Engelmann spruce
  case (18)
     TOTWT = EXP(-3.932 + 2.571*LOG(H))

     !         california black oak
     !         hardwood predicted from height (5.3539 ft, WS-FFE) of 1" dbh tree
     !         and scaled from that based on the weight (0.81135 lbs) of
     !         the 1" tree (derived from tanoak large tree)
  case (21)
     TOTWT = 0.81135 * H / 5.3539

     !         red alder -  predict weight of 1" dbh tree (.277)
     !         and scale back that based on the height of
     !         the 1" tree (7.728)
  case (23)
     TOTWT = 0.277 * H / 7.728

  end select

  select case (SPI)

     !         subalpine fir, grand fir;
     !         western hemlock, western redcedar
     !         Engelmann spruce, mountain hemlock
  case (1,4,6,7,18,24)
     XV(0) = 0.62 * TOTWT
     XV(1) = 0.26 * TOTWT
     XV(2) = 0.12 * TOTWT

     !         quaking aspen - assume half in foliage; quarter in others
  case (2)
     XV(0) = 0.50 * TOTWT
     XV(1) = 0.25 * TOTWT
     XV(2) = 0.25 * TOTWT

     !         Douglas-fir, lodgepole pine, whitebark pine, white pine
  case (3,11,14,15)
     XV(0) = 0.52 * TOTWT
     XV(1) = 0.27 * TOTWT
     XV(2) = 0.21 * TOTWT

     !         bigleaf maple - use proportions at 1" dbh
  case (5)
     XV(0) = 0.20 * TOTWT
     XV(1) = 0.09 * TOTWT
     XV(2) = 0.71 * TOTWT

     !         western larch
  case (8)
     XV(0) = 0.40 * TOTWT
     XV(1) = 0.42 * TOTWT
     XV(2) = 0.18 * TOTWT

     !         bristlecone pine
  case (9)
     X = (D / 2)
     XV(0) = 8.874 * X
     XV(1) = 3.428 * X
     XV(2) = 4.429 * X
     XV(3) = 0.590 * X

     !         madrone -  predicted from partitioning of 1" dbh tree using
     !         large tree equation
  case (10)
     XV(0) = 0.43 * TOTWT
     XV(1) = 0.35 * TOTWT
     XV(2) = 0.22 * TOTWT

     !         ponderosa pine
  case (13,25)
     XV(0) = 0.57 * TOTWT
     XV(1) = 0.14 * TOTWT
     XV(2) = 0.29 * TOTWT

     !         pinyon, bristlecone, juniper, oak based on plugging in a
     !         2" (juniper, pinyon, bristlecone) or 4" (gambel oak) drc
     !         tree and computing biomass components (as per large trees),
     !         then scaling the result by the diameter.
     !         these values are already in lbs and don't need further conversion.

     !         pinyon pine
  case (12)
     X = (D / 2)
     XV(0) = 3.177 * X
     XV(1) = 0.977 * X
     XV(2) = 1.084 * X
     XV(3) = 0.079 * X

     !         juniper
  case (16)
     X = (D / 2)
     XV(0) = 0.256 * X
     XV(1) = 0.126 * X
     XV(2) = 1.388 * X
     XV(3) = 0.454 * X

     !         tanoak, California black oak use proportions at 1" dbh
  case (17,21)
     XV(0) = 0.38 * TOTWT
     XV(1) = 0.32 * TOTWT
     XV(2) = 0.30 * TOTWT

     !         giant sequoia, incense cedar: use proportions at 1" dbh
  case (19,20)
     XV(0) = 0.52 * TOTWT
     XV(1) = 0.28 * TOTWT
     XV(2) = 0.20 * TOTWT

     !         Gambel oak
  case (22)
     X = (D / 4)
     XV(0) = 3.2119 * X
     XV(1) = 1.606  * X
     XV(2) = 15.8115 * X
     XV(3) = 7.7877 * X

     !         red alder
  case (23)
     XV(0) = 0.32 * TOTWT
     XV(1) = 0.39 * TOTWT
     XV(2) = 0.29 * TOTWT

  end select

  do J = 0,5
     XV(J) = MAX(0.0, XV(J) * SMWGT)
  end do

  if (D .lt. DCTLOW) goto 999

  !***********************2.  CANOPIES OF LARGE TREES*********************

  !       Limit D as instructed in Brown & Johnson:

  299 if (D .gt.40.0) D = 40.0

  !       Using relationships which depend on species, calculate the live
  !       and dead crown weights.  Scale these back for intermediate or
  !       suppressed trees (trees below DOMPCT percentile in height; only done
  !       for some species).  Also calculate the necessary partitioning
  !       coefficients.  Species found in the NI are taken from Brown (1978,
  !       page 10-11); other species are as noted << MAKE SURE TO REFERENCE >>

  select case (SPI)

     !         subalpine fir
  case (1)

     LIVEWT = 0.1862*D*D*R + 1.066
     if (D .le.16.0) then
        DEADWT = EXP(4.0365*LOG(D) - 6.5431)
     else
        DEADWT = 0.31 * LIVEWT
     end if
     P1 = 0.5966 * EXP(-0.04247*D)
     P2 = 0.8643 * EXP(-0.03733*D)
     if (D .le.2.9) then
        P3 = 1.0
     else
        P3 = 1.0221 - 0.01083*D
     end if
     P4 = 1.0
     if (D .lt.1.5) then
        DP1 = 1.0
     else
        DP1 = 1.2105 * D**(-0.565)
     end if
     DP2 = 1.0

     !         aspen
  case (2)

     !            CALL NATCRS (V,VM,XDUM1,SPI,D,H,.FALSE.,XDUM2,ITRNC,
     !     &          XDUM3,LDUM1,LDUM2,-1)

     call CFVOL (SPI,D,H,D*D*H,V,VM,XDUM3,.false.,.false.,XDUM2,&
          &ITRNC,LDUM1)

     !           INPUT   D (IN) CONVERTED TO (M)
     !           INPUT   V (FT**3)CONVERTED TO (M**3);
     !           OUTPUT  DBR1, DBR2, ETC ALL METRIC (KG)

     !           DBR1: BRANCH BIOMIASS  >10CM (    >3.94") - LARGE
     !           DBR2: BRANCH BIOMIASS 6-10CM (2.36-3.94")
     !           DBR3: BRANCH BIOMIASS 2- 6CM (0.78-2.36") - SMALL
     !           DFOL: FOLIAGE

     D = D * 2.54 / 100.
     V = V * 0.028317

     DBR1 = -3.3 - 56.9*V + 581.8*D*D
     DBR2 = -1.8 - 42.2*V + 634.3*D*D
     DBR3 = -1.2 - 29.0*V + 271.1*D*D
     DFOL = 0.5 +  0.2*V +  89.2*D*D

     !         Douglas-fir
  case (3)

     if (HP .lt. DOMPCT) then
        LIVEWT = EXP(0.1508 + 1.8621*LOG(D))
        DEADWT = EXP(-1.928 + 2.353*LOG(D))
     else
        if (D .lt.17.0) then
           LIVEWT = EXP(1.1368 + 1.5819*LOG(D))
        else
           LIVEWT = 1.0237*D*D - 20.74
        end if
        DEADWT = 0.01094*D*D*D
     end if
     if (D .gt.36.0) then
        P1 = 0.227
        P2 = 0.315
        P3 = 0.465
     else
        P1 = 0.484  * EXP(-0.02102*D)
        P2 = 0.7289 * EXP(-0.02332*D)
        if (D .le.2.9) then
           P3 = 1.0
        else
           P3 = 1.0342 - 0.01584*D
        end if
     end if
     if (D .le.14.0) then
        P4 = 1.0
     else
        P4 = 1.0221 - 0.001821*D
     end if
     if (D .lt.1.8) then
        DP1 = 1.0
     else
        DP1 = 0.08355 + (1.5893/D)
     end if
     if (D .lt.9.0) then
        DP2 = 1.0
     else
        DP2 = 1.5673 * EXP(-0.05232*D)
     end if

     !         grand fir
  case (4)

     LIVEWT = EXP(1.3094 + 1.6076*LOG(D))
     if (D .le.18.0) then
        DEADWT = EXP(3.5638*LOG(D) - 5.3154)
     else
        DEADWT = 0.38*LIVEWT
     end if
     if (D .gt.36.0) then
        P1 = 0.286
        P2 = 0.378
        P3 = 0.488
     else
        P1 = 1.0 / (1.5916 + 0.05294*D)
        P2 = 1.0 / (1.1495 + 0.04165*D)
        if (D .le.2.9) then
           P3 = 1.0
        else
           P3 = 1.0267 - 0.01495*D
        end if
     end if
     P4 = 1.0
     if (D .lt.3.0) then
        DP1 = 1.0
     else
        if (D .gt.27.0) then
           DP1 = 0.01
        else
           DP1 = 1.4336 * EXP(-0.1816*D)
        end if
     end if
     if (D .lt.8.0) then
        DP2 = 1.0
     else
        DP2 = 1.2623 * EXP(-0.0347*D)
     end if

     !         bigleaf maple (Snell & Little 1983)
  case (5)
     LIVEWT = EXP(-0.0582 + 2.1505 * LOG(D))
     DEADWT = EXP(-3.3678 + 2.5033 * LOG(D))

     P1 = 1.0/(4.6762 + 0.1091 * D**2.0390)
     P2 = 1.0/(3.3212 + 0.0777 * D**2.0496)
     if (D .lt.1.9) then
        P3 = 1.0
     else
        P3 = 1.0/(0.9341 + 0.0158 * D**2.1627)
     end if
     if (D .lt.4.8) then
        P4 = 1.0
     else
        P4 = 1.0/(0.8625 + 0.0093 * D**1.7070)
     end if

     DP1 = EXP(-1.0444 - 0.1892 * D)
     if (D .lt.1.0) then
        DP2 = 1.0
     else
        DP2 = EXP(0.0553 - 0.0660 * D)
     end if
     if (D .lt.2.5) then
        DP3 = 1.0
     else
        DP3 = EXP(0.0083 - 0.0033 * D)
     end if

     !         western hemlock
  case (6)
     LIVEWT = 0.3729*D*D + 0.284*D*C - 0.005525*D*D*C - 4.501
     DEADWT = EXP(3.3664*LOG(D) - 6.6768)

     if (D .le.40.0) then
        P1 = 0.5474 * EXP(-0.03697*D)
        P2 = 0.8352* EXP(-0.03802*D)
        if (D .le.2.9) then
           P3 = 1.0
        else
           P3 = 1.0781 * EXP(-0.02735*D)
        end if
     else
        P1 = 0.125
        P2 = 0.183
        P3 = 0.361
     end if
     P4 = 1.0
     if (D .lt.4.0) then
        DP1 = 1.0
     else
        if (D .gt.28.0) then
           DP1 = 0.005
        else
           DP1 = 1.9608 * EXP(-0.2064*D)
        end if
     end if
     if (D .lt.12.0) then
        DP2 = 1.0
     else
        DP2 = 1.0 / (0.2772 + 0.06141*D)
     end if

     !         western redcedar, incense cedar
  case (7,20)

     LIVEWT = EXP(1.7273*LOG(D*R) - 2.8086)
     DEADWT = 0.01063*D*D*D
     P1 = 0.6174 * EXP(-0.02326*D)
     P2 = 0.7562 * EXP(-0.02411*D)
     if (D .le.2.9) then
        P3 = 1.0
     else
        P3 = 1.0602 * EXP(-0.02226*D)
     end if
     P4 = 1.0
     if (D .lt.1.5) then
        DP1 = 1.0
     else
        DP1 = -0.01578 + (1.4673/D)
     end if
     if (D .lt.8.0) then
        DP2 = 1.0
     else
        DP2 = 1.4534 * EXP(-0.05395*D)
     end if

     !         western larch
  case (8)

     LIVEWT = EXP(0.4373 + 1.6786*LOG(D))
     if (HP .lt. DOMPCT) then
        if (D .le.7.5) then
           LIVEWT = 0.5 * LIVEWT
        else
           LIVEWT = 0.6 * LIVEWT
        end if
     end if
     P1 = 0.3468 * EXP(-0.04343*D)
     P2 = 0.745  * EXP(-0.03622*D)
     if (D .le.2.9) then
        P3 = 1.0
     else
        P3 = 1.05448 * EXP(-0.0213*D)
     end if
     if (D .le.11) then
        P4 = 1.0
     else
        P4 = 0.9223 + 0.7197/D
     end if
     DEADWT = 0.0
     DP1 = 0.0
     DP2 = 0.0

     !         bristlecone pine, pinyon pine (single stem)
  case (9,12)

     !         COEFFICIENTS TO DERIVE DRC FROM DBH ARE
     !         FROM CHOJNACKY (1999) WJAF 14:14-16, TABLE 2.
     !         (REARRANGED, SINCE CHOJNACKY FITS DBH FROM DRC). REGRESSIONS
     !         ARE FROM GRIER ET AL. 1992 FOR. ECOL. MGMT. 50:331, TABLE 2.

     !         ONLY BRISTLECONE NEEDS TO HAVE ITS DBH CONVERTED TO DRC,
     !         SINCE FOR PINYON, DRC IS WHAT USERS ENTER
     !         STILL CONVERT TO CM --SAR 10/13
     DRC = D*2.54
     if (SPI .eq.9) DRC = 2.54 * (D + 1.9410) / 1.0222

     !           INPUT   DRC (CM) - DIAMETER AT ROOT COLLAR
     !           OUTPUT  DBR1, DBR2, ETC ALL METRIC (KG)

     !           DFOL: FOLIAGE
     !           DBR1: BRANCH BIOMIASS <1/4"  - SMALL
     !           DBR2: BRANCH BIOMIASS 1/4-1"
     !           DBR3: BRANCH BIOMIASS 1-3"   - LARGE
     !           DBR4: 0.0 - IN BASE FVS EQNS
     !           DBR5: 0.0 - IN BASE FVS EQNS

     !           GRIER ET AL <1 CATEGORY SPLIT TO GIVE TWICE AS MUCH
     !           IN 1/4-1" (67%), COMPARED TO <1/4" CATEGORY (33%)
     !           1-3" CATEGORY IS SPLIT SO 25% GOES INTO DBR3; REMAINDER
     !           ASSUMED TO BE COUNTED BY FVS BASE EQNS (>1.5" BRANCH).
     !           STEMWOOD IS ASSUMED TO BE INCLUDED IN THE BASE FVS
     !           VOLUME EQUATIONS, EVEN FOR <1.5" UTILIZATION. SO NO
     !           STEMWOOD IS INCLUDED.

     DFOL = 10**(-0.946 + 1.565 * LOG10(DRC))
     DBR1 = 10**(-1.613 + 2.088 * LOG10(DRC)) * 0.33
     DBR2 = 10**(-1.613 + 2.088 * LOG10(DRC)) * 0.67
     DBR3 = 10**(-2.971 + 3.007 * LOG10(DRC)) * 0.25
     DBR4 = 0.0
     DBR5 = 0.0

     !           OLD TWIGS ADDED INTO <1/4"

     DBR1 = DBR1 + 10**(-1.873 + 1.675 * LOG10(DRC))

     !           DEAD BRANCHES DIVIDED ACCORDING TO DISTRIBUTION
     !           OF ALL LIVING BRANCHES 0.25-1.5"; IN STRANGE CASE
     !           OF NO BRANCH BIOMASS, PUT IN SMALL

     X = DBR2 + DBR3
     TEMP = 10**(-5.400 + 4.470 * LOG10(DRC))
     if (X .gt.1.0E-6) then
        DBR2 = DBR2 + (TEMP * DBR2/X)
        DBR3 = DBR3 + (TEMP * DBR3/X)
     else
        DBR2 = TEMP
     end if

     !         madrone (Snell & Little 1983. lbs ovendry weight)
  case (10)

     LIVEWT = EXP(-0.7881 + 2.4839 * LOG(D))
     DEADWT = EXP(-2.3938 + 2.2936 * LOG(D))

     P1 = 1.0/(1.6013 + 0.3591 * D**1.3090)
     P2 = 1.0/(1.0357 + 0.2263 * D**1.3567)
     P3 = 1.0/(1.0281 + 0.0084 * D**2.1850)
     P4 = 1.0
     if (D .ge.4.2) then
        P4 = 1.0/(0.8778 + 0.0115 * D**1.6394)
     end if

     DP1 = -0.0632 + (0.7214 * D**0.25) - (0.4655 * LOG(D))

     DP2 = 1.0
     if (D .ge.2.5) then
        DP2 = 1.2671 - (0.1686 * D**0.5)
     end if
     DP3 = 1.0
     if (D .ge.7.6) then
        DP3 = EXP(0.0281 - (0.0004714 * D * D))
     end if

     !         lodgepole pine
  case (11)

     LIVEWT = 0.02238*D*D*D + 0.1233*D*D*R - 2.0
     if (D .le.10.0) then
        DEADWT = (0.026*D - 0.025) * LIVEWT
     else
        DEADWT = 0.235 * LIVEWT
     end if
     if (HP .lt. DOMPCT) then
        if (D .le.7.5) then
           LIVEWT = 0.5 * LIVEWT
           DEADWT = 0.5 * DEADWT
        else
           LIVEWT = 0.6 * LIVEWT
           DEADWT = 0.6 * DEADWT
        end if
     end if
     P1 = 0.4933 - 0.01167*D
     P2 = 0.7767 - 0.01464*D
     if (D .le.3.9) then
        P3 = 1.0
     else
        P3 = 1.0494 - 0.01402*D
     end if
     P4 = 1.0
     if (D .gt.20.0) then
        DP1 = 0.139
        DP2 = 0.226
     else
        if (D .lt.1.5) then
           DP1 = 1.0
        else
           DP1 = 1.3527 * D**(-0.7585)
        end if
        if (D .lt.9.0) then
           DP2 = 1.0
        else
           DP2 = 2.7979 * EXP(-0.1257*D)
        end if
     end if

     !         ponderosa pine
  case (13)

     if (HP .lt. DOMPCT) then
        LIVEWT = EXP(-0.7572 + 2.216*LOG(D))
        DEADWT = EXP(-2.5176 + 2.51*LOG(D))
        P1 = 0.6501 * EXP(-0.1544*D)
        P2 = 0.8435 * EXP(-0.1665*D)
        P3 = 1.0865 * EXP(-0.0833*D)
        P4 = 1.0
     else
        LIVEWT = EXP(2.2812*LOG(D) + 1.5098*LOG(R) - 3.0957)
        DEADWT = EXP(2.8376*LOG(D) - 3.7398)
        P1 = 0.5578 * EXP(-0.04754*D)
        if (D .ge.31.0) then
           P2 = P1 + 0.01
        else
           P2 = 0.6254 * EXP(-0.05114*D)
        end if
        if (D .le.1.0) then
           P3 = 1.0
           P4 = 1.0
        else
           P3 = 0.985 * EXP(-0.03102*D)
           if (D .le.6.5) then
              P4 = 1.0
           else
              P4 = 1.083 - 0.01306*D
           end if
        end if
     end if
     if (D .gt.30.0) then
        DP1 = 0.004
        DP2 = 0.06
     else
        DP1 = (1.4114 / D) - 0.04345
        DP2 = 1.0621 - 0.03342*D
     end if

     !         whitebark pine
  case (14)

     LIVEWT = 0.06056*D*D*D + 0.05477*D*D*R + 0.646
     DEADWT = 0.001713*D*D*C + 0.33

     if (D .gt.20.0) then
        P1 = 0.242
     else
        P1 = 0.5120 * EXP(-0.03737*D)
     end if

     if (D .gt.20.0) then
        P2 = 0.268
     else
        P2 = 0.8644 * EXP(-0.05854*D)
     end if

     if (D .le.3.9) then
        P3 = 1.0
     else if (D .gt.20.0) then
        P3 = 0.670
     else
        P3 = 1.0733 * EXP(-0.02376*D)
     end if

     P4 = 1.0

     if (D .lt.1.4) then
        DP1 = 1.0
     else
        DP1 = 0.268 + (1.1733/D)
     end if
     DP2 = 1.0

     !         white pine
  case (15)

     LIVEWT = 0.0947 * D * D * R
     DEADWT = EXP(2.6076*LOG(D) - 4.397)
     P1 = 0.5497 * EXP(-0.0345*D)
     P2 = 0.9138 - 0.0978*SQRT(D)
     if (D .le.3.9) then
        P3 = 1.0
     else
        P3 = 1.0564 * EXP(-0.0181*D)
     end if
     P4 = 1.0
     DP1 = 1.0077 * D**(-0.4556)
     if (D .lt.7.0) then
        DP2 = 1.0
     else
        DP2 = 1.0291 - 0.004964*D
     end if

     !         western juniper (single stem)
  case (16)

     !           METHODOLOGY AS PER PINYON PINE, EXCEPT <1" CATEGORY
     !           ALL PLACED IN .25-1" CATEGORY; "FOLIAGE TWIGS" SPLIT
     !           INTO FOLIAGE (67%), <.25" CATEGORIES. (33%). 1-3"
     !           CATEGORY IS SPLIT SO 25% GOES INTO DBR3; REMAINDER
     !           ASSUMED TO BE COUNTED BY FVS BASE EQNS (>1.5" BRANCH).

     !           LINE BELOW IS NOT NEEDED SINCE USERS ENTER DRC--SAR 4/03
     !           DRC = 2.54 * (D + 1.1841) / 0.9823
     !           STILL CONVERT TO CM --SAR 10/13
     DRC = D*2.54

     DFOL = 10**(-1.737 + 1.382 * LOG10(DRC)) * 0.67
     DBR1 = 10**(-1.737 + 1.382 * LOG10(DRC)) * 0.33
     DBR2 = 10**(-1.476 + 1.787 * LOG10(DRC))
     DBR3 = 10**(-1.356 + 1.782 * LOG10(DRC)) * 0.25
     DBR4 = 0.0
     DBR5 = 0.0

     !           DEAD BRANCHES DIVIDED ACCORDING TO DISTRIBUTION
     !           OF ALL LIVING BRANCHES 0.25-1.5"; IN STRANGE CASE
     !           OF NO BRANCH BIOMASS, PUT IN SMALL

     X = DBR2 + DBR3
     TEMP = 10**(-3.543 + 2.774 * LOG10(DRC))
     if (X .gt.1.0E-6) then
        DBR2 = DBR2 + (TEMP * DBR2/X)
        DBR3 = DBR3 + (TEMP * DBR3/X)
     else
        DBR2 = TEMP
     end if

     !        tanoak, black oak
     !        snell & little 1983, and snell 1979 (to justify
     !        lumping black oak with tanoak; equations use
     !        coefficients from *tanoak* and predict ovendry
     !        weight in pounds
  case (17,21)

     LIVEWT = EXP(-0.3169 + 2.2774 * LOG(D))
     DEADWT = EXP(-2.4895 + 2.0374 * LOG(D))

     P1 = 1.0/(1.7936 + 0.5952 * D**0.7239)
     P2 = 1.0/(0.9940 + 0.4229 * D**0.6520)
     if (D .lt.1.5) then
        P3 = 1.0
     else
        P3 = 1.0/(0.8759 + 0.0927 * D**0.7843)
     end if
     P4 = 1.0

     DP1 = -0.1424 + (0.7684 * D**0.25) - (0.4730 * LOG(D))
     if (D .lt.4.1) then
        DP2 = 1.0
     else
        DP2 = EXP(-2.810 + (4.379 * D**0.25) - (1.691 * D**0.5))
     end if
     if (D .lt.7.9) then
        DP3 = 1.0
     else
        DP3 = 1.027 - (0.003439 * D)
     end if

     !         Engelmann spruce
  case (18)

     LIVEWT = EXP(1.0404 + 1.7096*LOG(D))
     DEADWT = EXP(3.6172*LOG(D) - 6.686)
     if (D .lt.40.0) then
        P1 = 0.5738 * EXP(-0.0325*D)
        P2 = 0.8519* EXP(-0.02811*D)
        if (D .le.2.9) then
           P3 = 1.0
        else
           P3 = 1.03781 - 0.01537*D
        end if
     else
        P1 = 0.158
        P2 = 0.277
        P3 = 0.423
     end if
     P4 = 1.0
     if (D .lt.1.8) then
        DP1 = 1.0
     else
        DP1 = 1.4657 * D**(-0.6454)
     end if
     if (D .lt.10.0) then
        DP2 = 1.0
     else
        DP2 = 1 / (0.847 + 0.01678*D)
     end if

     !         sequoia - use cedar for weights, and western hemlock
     !         for proportions - mike landram
  case (19)

     LIVEWT = EXP(1.7273*LOG(D*R) - 2.8086)
     DEADWT = 0.01063*D**3

     P1 = 0.5474 * EXP(-0.03697*D)
     P2 = 0.8352* EXP(-0.03802*D)
     if (D .le.2.9) then
        P3 = 1.0
     else
        P3 = 1.0781 * EXP(-0.02735*D)
     end if
     P4 = 1.0

     if (D .lt.4.0) then
        DP1 = 1.0
     else
        if (D .gt.28.0) then
           DP1 = 0.005
        else
           DP1 = 1.9608 * EXP(-0.2064*D)
        end if
     end if
     if (D .lt.12.0) then
        DP2 = 1.0
     else
        DP2 = 1.0 / (0.2772 + 0.06141*D)
     end if

     !         Gambel oak - imperial equation
  case (22)

     !         USING CHOJNACKY 1992 (TABLE 2) WITH COEFFICIENTS
     !         FOR Q. SPP, Q. GAMBELLI AND Q. ILEX
     !         (CHOJNACKY PERS. COMM.) - DROBINSON OCT/2000
     !         SOME DOUBLE COUNTING IS PROBABLY PRESENT HERE

     !           LINE BELOW IS NOT NEEDED SINCE USERS ENTER DRC--SAR 4/03
     !           DRC = (D + 0.5766) / 0.8841
     DRC = D
     X = DRC * DRC * H / 1000.

     !           VOLUME IN PLANT & BRANCHES > 1.5" (FT**3)

     X0 = 7.1046
     if (X .le. X0) then
        V = -0.0534 + (2.3077 * X) + (0.0467 * X**2)
     else
        V = -0.0534 + (2.3077 * X) +&
             &(0.0467 * 3.0 * (X0**2 - X0**3/X))
     end if

     if (V .le..01) then
        V = .01
     end if

     !           WT OF PLANT & BRANCHES > 1.5" (POUNDS)

     V = V * SG * 2000.0

     !           WT OF FOLIAGE AND BRANCHES < 2" (POUNDS)
     !           ** NOTE 2" VS 1.5" DISCREPANCY **
     !           WEIGHT OF TWIGS SET TO HALF OF FOLIAGE (LIKE PINYON & JUNIPER);
     !           REMAINING BRANCHES DIVIDED BETWEEN 0.25-1" AND 1-3" BY
     !           PUTTING 2/3 IN THE SMALLER CATEGORY AND 1/3 IN THE LARGER
     !           (SIMILAR TO PINYON AND JUNIPER RELATIVE PREDICTIONS)

     DFOL = 10**(-0.5655 + 0.8382 * LOG10(V) - 0.0094 * H)
     DBR3 = 10**( 0.3036 + 0.7752 * LOG10(V) - 0.0049 * H)

     DBR1 = DFOL * 0.5
     DBR2 = MAX(0.0, ((DBR3-DBR1) * 0.67))
     DBR3 = MAX(0.0, ((DBR3-DBR1) * 0.33))
     DBR4 = 0.0
     DBR5 = 0.0

     !         red alder (Snell & Little 1983)
  case (23)

     LIVEWT = EXP(-1.3290 + 2.6232 * LOG(D))
     DEADWT = EXP(-4.3788 + 2.6243 * LOG(D))

     P1 = 1.0/(2.7638 + 0.2155 * D**1.3364)
     P2 = 1.0/(1.286 + 0.1016 * D**1.3525)
     if (D .lt.2.1) then
        P3 = 1.0
     else
        P3 = 1.0/(0.8847 + 0.0441 * D**1.3021)
     end if
     if (D .lt.6.1) then
        P4 = 1.0
     else
        P4 = 1.0/(0.995 + 0.0013 * D**1.9736)
     end if

     DP1 = EXP(-.6880 - 0.1532 * D)
     if (D .lt.2.5) then
        DP2 = 1.0
     else
        DP2 = EXP(0.2134 - 0.0869 * D)
     end if
     if (D .lt.11.0) then
        DP3 = 1.0
     else
        DP3 = EXP(0.3473 - 0.0315 * D)
     end if

     !         mountain hemlock - total from Gholz (1979)
     !         partitioning from western hemlock
  case (24)
     LDM = LOG(D * 2.54)
     LIVEWT = EXP(-3.8169 + 1.9756 * LDM) +& ! foliage
          &EXP(-5.2581 + 2.6045 * LDM) ! live branches
     DEADWT = EXP(-9.9449 + 3.2845 * LDM)

     LIVEWT = LIVEWT*2.2046 ! convert kg to lbs
     DEADWT = DEADWT*2.2046

     if (D .le.40.0) then
        P1 = 0.5474 * EXP(-0.03697*D)
        P2 = 0.8352* EXP(-0.03802*D)
        if (D .le.2.9) then
           P3 = 1.0
        else
           P3 = 1.0781 * EXP(-0.02735*D)
        end if
     else
        P1 = 0.125
        P2 = 0.183
        P3 = 0.361
     end if
     P4 = 1.0
     if (D .lt.4.0) then
        DP1 = 1.0
     else
        if (D .gt.28.0) then
           DP1 = 0.005
        else
           DP1 = 1.9608 * EXP(-0.2064*D)
        end if
     end if
     if (D .lt.12.0) then
        DP2 = 1.0
     else
        DP2 = 1.0 / (0.2772 + 0.06141*D)
     end if

     !         ponderosa pine - black hills
     !         based on Keyser and Smith, Forest Science 56(2) 2010
  case (25)
     !           INSERT NEW CODE FOR CROWN BIOMASS OF NEEDLE AND 1-HOUR FUELS
     TFOL = (0.0865*((D*2.54)**1.8916)*(TCR**1.1358))*2.2046
     T1HRL = 1.5439*(TCR**5.6131)*2.2046
     T1HRD = 0.0

     !           REST IS SAME AS 13 ABOVE
     if (HP .lt. DOMPCT) then
        LIVEWT = EXP(-0.7572 + 2.216*LOG(D))
        DEADWT = EXP(-2.5176 + 2.51*LOG(D))
        P1 = 0.6501 * EXP(-0.1544*D)
        P2 = 0.8435 * EXP(-0.1665*D)
        P3 = 1.0865 * EXP(-0.0833*D)
        P4 = 1.0
     else
        LIVEWT = EXP(2.2812*LOG(D) + 1.5098*LOG(R) - 3.0957)
        DEADWT = EXP(2.8376*LOG(D) - 3.7398)
        P1 = 0.5578 * EXP(-0.04754*D)
        if (D .ge.31.0) then
           P2 = P1 + 0.01
        else
           P2 = 0.6254 * EXP(-0.05114*D)
        end if
        if (D .le.1.0) then
           P3 = 1.0
           P4 = 1.0
        else
           P3 = 0.985 * EXP(-0.03102*D)
           if (D .le.6.5) then
              P4 = 1.0
           else
              P4 = 1.083 - 0.01306*D
           end if
        end if
     end if
     if (D .gt.30.0) then
        DP1 = 0.004
        DP2 = 0.06
     else
        DP1 = (1.4114 / D) - 0.04345
        DP2 = 1.0621 - 0.03342*D
     end if

  end select

  !       CALCULATE FINAL CROWN WEIGHT FOR TREES > 1" DBH

  X = 1 - SMWGT
  select case (SPI)

     !         MOST SPECIES
  case default

     !           MOST CALCULATIONS ARE DERIVED FROM BROWN'S METHODOLOGY

     !           Check whether anything came out negative that shouldn't have, and
     !           if so, make it be zero.  Check also that none of the proportions
     !           are greater than 1, or less than the preceding one.

     if (LIVEWT .lt.0.0) LIVEWT = 0.0
     if (DEADWT .lt.0.0) DEADWT = 0.0

     if (P1.lt.0.0)   P1 = 0.0
     if (P2.lt.0.0)   P2 = 0.0
     if (P3.lt.0.0)   P3 = 0.0
     if (P4.lt.0.0)   P4 = 0.0
     if (DP1.lt.0.0) DP1 = 0.0
     if (DP2.lt.0.0) DP2 = 0.0
     if (DP3.lt.0.0) DP3 = 0.0

     if (P1.gt.1.0)   P1 = 1.0
     if (P2.gt.1.0)   P2 = 1.0
     if (P3.gt.1.0)   P3 = 1.0
     if (P4.gt.1.0)   P4 = 1.0
     if (DP1.gt.1.0) DP1 = 1.0
     if (DP2.gt.1.0) DP2 = 1.0
     if (DP3.gt.1.0) DP3 = 1.0

     if (P2.lt. P1)    P2 = P1
     if (P3.lt. P2)    P3 = P2
     if (P4.lt. P3)    P4 = P3
     if (DP2.lt. DP1) DP2 = DP1
     if (DP3.lt. DP2) DP3 = DP2

     !           Divide the live and dead crown weights among the different size
     !           classes according to the appropriate proportions.  Size Class 5
     !           does not get anything under the current rules (Jim says this is
     !           right).

     if (SPI .eq.25) then ! ponderosa pine - black  hills
        if (TFOL  .lt.0.0)  TFOL = 0.0
        if (T1HRL .lt.0.0)  T1HRL = 0.0
        if (T1HRD .lt.0.0)  T1HRD = 0.0

        XV(0) = XV(0) + (TFOL) * X
        XV(1) = XV(1) + (T1HRL + T1HRD) * X
     else
        XV(0) = XV(0) + (LIVEWT *  P1) * X
        XV(1) = XV(1) + (LIVEWT * (P2-P1) + DEADWT * DP1) * X
     end if

     XV(2) = XV(2) + (LIVEWT * (P3-P2) + DEADWT * (DP2-DP1)) * X
     XV(3) = XV(3) + (LIVEWT * (P4-P3) + DEADWT * (DP3-DP2)) * X
     XV(4) = XV(4) + (LIVEWT * (1.0-P4)+ DEADWT * (1.0-DP3)) * X

     !         quaking aspen
  case (2)

     !           weights to shift breakpoints; values for <0.25" and .25-1"
     !           are guesses based on fig 4.2 of ffe model desc. feb/99

     !           august 2003 - further modification: comparison with bigtooth
     !           aspen (eastern species) showed that foliage biomass was approximately
     !           equal, but XV(1,2,3) were markedly lower (3-5 fold!). XV(4) was
     !           similar. This demonstrates that the assumptions used to fill the
     !           gaps in the quaking aspen crown were probably very wrong. The fix is:
     !           set <.25" (xv(1)) to 0.75 of foliage (xv(0)); set 0.25-1" (xv(2)) to
     !           2x foliage (similar to value in bigtooth); leave 1-3" alone (xv(3),
     !           since quaking aspen has a subjectively lighter crown than bigtooth,
     !           and since the data for xv(3) are more quantitative, while xv(1) and
     !           xv(2) are linked to computed foliage biomass only through a comparison
     !           with bigtooth aspen.

     XV(0) = XV(0) + MAX(0.0,DFOL*2.2046) ! modified
     XV(1) = XV(1) + MAX(0.0,DFOL*0.75*2.2046)
     XV(2) = XV(2) + MAX(0.0,(DBR3*0.139+DFOL*2.0)*2.2046)
     XV(3) = XV(3) +&
          &MAX(0.0,(DBR3*0.861+DBR2*0.405)*2.2046)
     XV(4) = XV(4) +&
          &MAX(0.0,(DBR1+DBR2*0.595)*2.2046)

     !            XV(0) = XV(0) + MAX(0.0,DFOL*2.2046)  ! original
     !            XV(1) = XV(1) + MAX(0.0,DFOL*0.25*2.2046)
     !            XV(2) = XV(2) + MAX(0.0,(DBR3*0.139+DFOL*0.50)*2.2046)
     !            XV(3) = XV(3) +
     !     &                      MAX(0.0,(DBR3*0.861+DBR2*0.405)*2.2046)
     !            XV(4) = XV(4) +
     !     &                     MAX(0.0,(DBR1+DBR2*0.595)*2.2046)

     !         bristlecone pine, pinyon pine, juniper
     !         convert kg->lb (dry weight)
  case (9,12,16)
     XV(0) = XV(0) + MAX(0.0,DFOL * 2.2046) * X
     XV(1) = XV(1) + MAX(0.0,DBR1 * 2.2046) * X
     XV(2) = XV(2) + MAX(0.0,DBR2 * 2.2046) * X
     XV(3) = XV(3) + MAX(0.0,DBR3 * 2.2046) * X
     XV(4) = XV(4) + MAX(0.0,DBR4 * 2.2046) * X
     XV(5) = XV(5) + MAX(0.0,DBR5 * 2.2046) * X

     !         Gambel oak
  case (22)

     XV(0) = XV(0) + MAX(0.0,DFOL) * X
     XV(1) = XV(1) + MAX(0.0,DBR1) * X
     XV(2) = XV(2) + MAX(0.0,DBR2) * X
     XV(3) = XV(3) + MAX(0.0,DBR3) * X
     XV(4) = XV(4) + MAX(0.0,DBR4) * X
     XV(5) = XV(5) + MAX(0.0,DBR5) * X

  end select

  do J = 0,5
     XV(J) = MAX(0.0,XV(J))
  end do

  999 continue

  return
end
