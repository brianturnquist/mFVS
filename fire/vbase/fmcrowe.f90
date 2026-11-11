!==========================!
!   fire/vbase : fmcrowe   !
!==========================!
! FIRE-VBASE $Id$
subroutine FMCROWE (SPILS,SPIYV,D,H,IC,SG,XV)
  implicit none

  !     CALLED FROM: FMCROW

  !  PURPOSE:
  !     THIS SUBROUTINE CALCULATES CROWNW(TREE,SIZE), THE WEIGHT OF
  !     VARIOUS SIZES OF CROWN MATERIAL THAT IS ASSOCIATED WITH EACH TREE
  !     RECORD IN THE CURRENT STAND.  THESE WEIGHTS COME FROM:
  !     1-NATIONAL-SCALE ESTIMATORS FOR UNITED STATES TREE SPECIES, BY
  !     JENKINS ET. AL. (FOR. SCI. 2003 49(1))
  !     2-ESTIMATING ASPEN CROWN FUELS IN NORTHEASTERN MINNESOTA, BY R.M.
  !     LOOMIS AND P.J. ROUSSOPOULOS (RES. PAP. NC-156)
  !     3-ESTIMATING NORTHERN RED OAK CROWN COMPONENT WEIGHTS IN THE
  !     NORTHEASTERN UNITED STATES, BY R.M. LOOMIS AND R.W. BLANK
  !     (RES. PAP. NC-194)
  !     4-ESTIMATING FOLIAGE AND BRANCHWOOD QUANTITIES IN SHORTLEAF PINE
  !     BY R.M. LOOMIS, R.E. PHARES, AND J.S. CROSBY (FOREST SCIENCE,
  !     VOL 12, ISSUE 1, 1966)
  !     5-PREDICTING CROWN WEIGHT AND BOLE VOLUME OF FIVE WESTERN
  !     HARDWOODS BY J.A. KENDALL SNELL AND SUSAN N. LITTLE
  !     (RES. PAP. PNW-151, MARCH 1983)

  !  LOCAL VARIABLE DEFINITIONS:

  !     SPILS = SPECIES NUMBER AS DEFINED BELOW (SAME AS FOR LS VARIANT)
  !     SPIYV = SPECIES NUMBER IN VARIANT CALLING ROUTINE
  !     D = DBH
  !     H = HEIGHT
  !     C = CROWN RATIO EXPRESSED AS A PERCENT

  !     LILPCE = BIOMASS OF THE SMALL PIECE MISSING SINCE VOLUME
  !            EQUATIONS GO TO 4" DIB AND CROWN EQUATIONS START AT 4" DOB
  !     HTLP = HT CALC NEEDED TO ESTIMATE LILPCE.  HT AT 4" DOB.
  !     DIB  = DIB VALUE THAT CORRESPONDS TO 4" DOB.
  !     TTOPW  = TOTAL TOP WEIGHT
  !     FOL    = FOLIAGE BIOMASS
  !     P1 - P3 = PROPORTIONS OF CROWN MATERIAL IN DIFFERENT SIZE CLASSES
  !     F1 - F4 = PROPORTIONS USED IN CALCULATING MAPLE BIOMASS (SLIGHTLY
  !              DIFFERENT THEN P1-P3, MATCH SNELL AND LITTLE NOTATION)
  !     HTF     = HEIGHT TO A FOUR INCH TOP DIAMETER (IB)
  !     UMBTW   = UNMERCHANTABLE BOLE TIP WEIGHT BY SIZE CLASS.
  !              (1 = 0-.25, 2 = 0-1, 3 = 0-3, 4 = 0 - 4)
  !     ANGLE, TEMPHT, DBRK  = USED IN CALCULATING UMBTW
  !     TOTABV = TOTAL ABOVE GROUND BIOMASS (AS PER JENKINS ET. AL.)

  !   SPECIES LIST (SPILS)
  !     1   jack pine
  !     2   scotch pine
  !     3   red pine natural
  !     4   red pine plantation
  !     5   white pine
  !     6   white spruce
  !     7   Norway spruce
  !     8   balsam fir
  !     9   black spruce
  !     10  tamarack
  !     11  n. white cedar
  !     12  eastern hemlock
  !     13  other softwoods
  !     14  eastern redcedar
  !     15  black ash
  !     16  green ash
  !     17  cottonwood
  !     18  silver maple
  !     19  red maple
  !     20  black cherry
  !     21  American elm
  !     22  slippery elm
  !     23  rock elm
  !     24  yellow birch
  !     25  basswood
  !     26  sugar maple
  !     27  black maple
  !     28  American beech
  !     29  white ash
  !     30  white oak
  !     31  swamp white oak
  !     32  bur oak
  !     33  chinkapin oak
  !     34  northern red oak
  !     35  black oak
  !     36  northern pin oak
  !     37  bitternut hickory
  !     38  pignut hickory
  !     39  shagbark hickory
  !     40  bigtooth aspen
  !     41  quaking aspen
  !     42  balsam poplar
  !     43  paper birch
  !     44  commercial hardwoods
  !     45  butternut
  !     46  black walnut
  !     47  eastern hophornbeam
  !     48  black locust
  !     49  non-commercial hardwoods
  !     50  boxelder
  !     51  striped maple
  !     52  mountain maple
  !     53  American hornbeam
  !     54  American chestnut
  !     55  hackberry / sugarberry
  !     56  flowering dogwood
  !     57  hawthorn
  !     58  apple sp.
  !     59  black gum / tupelos
  !     60  sycamore
  !     61  pin cherry
  !     62  choke cherry
  !     63  wild plum
  !     64  willow
  !     65  black willow
  !     66  diamond willow
  !     67  sassafras
  !     68  American mountain ash

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'COEFFS.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'

  !  VARIABLE DECLARATIONS

  ! Variables
  real    :: D,H,SG,XV(0:5)
  integer :: SPILS,SPIYV,IC

  integer :: J,K
  real    :: C,ANGLE,XNEG1
  real    :: TTOPW,FOL,DBRK(0:3),TOTABV,BARK,WOOD,BRANCH
  real    :: P1,P2,P3,TEMP,F1,F2,F3,F4,HTF,TEMPHT,MYPI,UMBTW(4)
  real    :: LILPCE,DIB,DOBF,HTLP,TEMPD,BRATIO,VT,VT1,VT2
  real    :: DX,HX
  logical :: DEBUG,LMERCH

  data MYPI/3.14159/

  call DBCHK (DEBUG,'FMCROWE',7,ICYC)
  if (DEBUG) write(JOSTND,'('' ENTERING FMCROWE'')')

  !     GET SOME VARIABLES YOU'LL NEED.

  DX = D ! STORE ORIGINAL D, H
  HX = H
  C = real(IC)

  !  INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
  !  OF THIS LOOP IF THE TREE HAS NO DIAMETER, OR HEIGHT.

  do J = 0,5
     XV(J) = 0.0
     if (J.gt.0.and. J.lt.5) UMBTW(J) = 0
  end do
  FOL = 0.0
  TTOPW = 0.0
  LILPCE = 0.0

  if ((D .eq.0.0) .or. (H .eq.0.0)) goto 999

  !  LETS GET ABOVE GROUND BIOMASS ESTIMATES FROM JENKINS ET. AL. (FOR. SCI. 49(1))
  !  FOR TREES LESS THAN 1 INCH, GET THE 1 INCH ESTIMATE AND SCALE BACK

  if (DEBUG) write(JOSTND,*) 'ABOUT TO CALC TOTABV'
  if (D .lt.1.0) then
     D = 1.0
  end if
  TOTABV = 0.
  FOL = 0.
  BARK = 0.
  WOOD = 0.
  BRANCH = 0.

  select case (SPILS)

     !       aspen/alder/cottonwood/willow
  case (17,40:42,64:66)
     TOTABV = EXP(-2.2094 + 2.3867*LOG(D*2.54))

     !       soft maple/birch
  case (18,19,24,43,49:52)
     TOTABV = EXP(-1.9123 + 2.3651*LOG(D*2.54))

     !       mixed hardwood
  case (15,16,20:23,25,29,44:48,53:63,67,68)
     TOTABV = EXP(-2.4800 + 2.4835*LOG(D*2.54))

     !       hard maple/oak/hickory/beech
  case (26:28,30:39)
     TOTABV = EXP(-2.0127 + 2.4342*LOG(D*2.54))

     !       cedar/larch (taxodiaceae is put here)
  case (10,11,13,14)
     TOTABV = EXP(-2.0336 + 2.2592*LOG(D*2.54))

     !       true fir/hemlock
  case (8,12)
     TOTABV = EXP(-2.5384 + 2.4814*LOG(D*2.54))

     !       pine
  case (1:5)
     TOTABV = EXP(-2.5356 + 2.4349*LOG(D*2.54))

     !       spruce
  case (6,7,9)
     TOTABV = EXP(-2.0773 + 2.3323*LOG(D*2.54))

  end select

  TOTABV = TOTABV * 2.2046

  select case (SPILS)

     !       hardwoods
  case (15:68)
     FOL = EXP(-4.0813 + 5.8816/(D*2.54))
     BARK = EXP(-2.0129 - 1.6805/(D*2.54))
     WOOD = EXP(-0.3065 - 5.4240/(D*2.54))

     !       softwoods
  case (1:14)
     FOL = EXP(-2.9584 + 4.4766/(D*2.54))
     BARK = EXP(-2.0980 - 1.1432/(D*2.54))
     WOOD = EXP(-0.3737 - 1.8055/(D*2.54))

  end select

  FOL = FOL * TOTABV
  BARK = BARK * TOTABV
  WOOD = WOOD * TOTABV
  BRANCH = TOTABV - (FOL + BARK + WOOD)

  !     RESET D

  if (DX .lt.1.0) then
     D = DX
     FOL = D * FOL
     BARK = D * BARK
     WOOD = D * WOOD
     BRANCH = D * BRANCH
     TOTABV = D * TOTABV
  end if

  if (BRANCH .lt.0) BRANCH = 0
  TTOPW = BRANCH

  if (DEBUG) write(JOSTND,*) 'D = ',D,'H = ',H,'TTOPW = ',TTOPW

  !     FOR SMALL TREES IN THE EAST WE WANT TO ADD IN AN ESTIMATE OF THE BOLE VOLUME TOO.
  !     SINCE SMALL TREES ARE UNMERCHANTABLE, THE "CROWN" IS REALY THE WHOLE TREE.

  !     FOR TREES LESS THAN THE MERCH DBH, ESTIMATE THE VOLUME OF THE BREAKPOINT TREE
  !     AND USE THIS WITH THE STANDARD VOLUME ESTIMATE OF THE BREAKPOINT TREE TO
  !     CREATE AN ADJUSTMENT FACTOR.

  TEMP = 0.0
  if ((DX .lt. DBHMIN(SPIYV)) .and.&
       &((VARACD .eq. 'SN') .or. (VARACD .eq. 'LS') .or.&
       &(VARACD .eq. 'NE') .or. (VARACD .eq. 'CS') .or.&
       &(VARACD .eq. 'ON'))) then

     D = DBHMIN(SPIYV)

     call HTDBH(IFOR,SPIYV,D,H,0)

     XNEG1 = -1.0
     LMERCH = .false.
     call FMSVL2(SPIYV,D,H,XNEG1,VT,0,&
          &' ',LMERCH,DEBUG,JOSTND)
     VT1 = 0.0015*DX*DX*HX !!! MVD eqn. - volume of a cone minus some % for loss of tip volume
     VT2 = 0.0015*D*D*H
     VT = (VT/VT2)*VT1
     TEMP = SG * VT / P2T
     TTOPW = TTOPW + TEMP

     !     RESET D and H

     D = DX
     H = HX

  end if

  if (DEBUG) write(JOSTND,*) 'D = ',D,'H = ',H,'FOL = ',FOL,&
       &'DBHMIN= ',DBHMIN(SPIYV), 'TTOPW = ',TTOPW,' TEMP = ',TEMP

  !     NOW WE NEED TO ALLOCATE THE CROWN BIOMASS TO DIFFERENT SIZE CLASSES.
  !     EACH SPECIES IS MAPPED TO EITHER SHORTLEAF PINE, ASPEN, RED OAK, OR
  !     MAPLE, SINCE THESE ARE THE SPECIES WE HAVE SOME SORT OF SIZE
  !     BREAKDOWN FOR.  REFERENCES ARE 1)LOOMIS AND BLANK, 2) LOOMIS, PHARES,
  !     AND CROSBY, 3) LOOMIS AND ROUSSOPOULOS AND 4) SNELL AND LITTLE

  !     P1 = PROPORTION IN 0-.25 INCH CLASS
  !     P2 = PROPORTION IN 0-1 INCH CLASS
  !     P3 = PROPORTION IN 0-3 INCH CLASS

  !     SNELL AND LITTLE DO IT A LITTLE DIFFERENTLY.  FOR MAPLE,
  !     F1 = PROPORTION OF FOLIAGE
  !     F2 = PROPORTION OF FOLIAGE + 0-.25
  !     F3 = PROPORTION OF FOLIAGE + 0-1 INCH
  !     F4 = PROPORTION OF FOLIAGE + 0-3 INCH

  if (DEBUG) write(JOSTND,*) 'ABOUT TO CALC PROPORTIONS'
  if (DEBUG) write(JOSTND,*) 'SPILS = ',SPILS, 'SPIYV= ', SPIYV,&
       &'D = ',D, 'C = ',C

  P1 = 0.0
  P2 = 0.0
  P3 = 0.0
  F1 = 0.0
  F2 = 0.0
  F3 = 0.0
  F4 = 0.0

  select case (SPILS)

     !       red oak
  case (30:39) ! OAKS AND HICKORIES
     P1 = 6.4735*(D**(-1.1313))*(C**(-0.5777))
     P2 = 36.8351*(D**(-0.9345))*(C**(-0.7014))
     P3 = 28.2916*(D**(-0.8658))*(C**(-0.4084))

     !       shortleaf pine
  case (1:14) ! ALL THE CONIFERS
     P1 = 3.525*(D**(-0.778))*(C**(-0.412))
     P2 = 5.989*(D**(-0.565))*(C**(-0.346))
     P3 = 8.585*(D**(-0.517))*(C**(-0.223))
     if (D .le.1.5) P1 = 0.5
     if (D .le.1.5) P2 = 1.0
     if ((D .le.10.5) .or. (C .le.35)) P3 = 1

     !       maple
  case (18,19,26,27,49:52) ! ALL THE MAPLES
     F1 = 1.0/(4.6762 + 0.1091*D**2.0390)
     F2 = 1.0/(3.3212 + 0.0777*D**2.0496)
     F3 = 1.0/(0.9341 + 0.0158*D**2.1627)
     F4 = 1.0/(0.8625 + 0.0093*D**1.7070)
     if (D .lt.1.9) F3 = 1.0
     if (D .lt.4.8) F4 = 1.0

     !       aspen
  case default ! ALL THE REST
     P1 = 1.856*(D*2.54)**(-0.773)
     P2 = 5.317*(D*2.54)**(-0.718)
     P3 = 1.793*(D*2.54)**(-0.185)

  end select

  !     BECAUSE THE ABOVE EQUATIONS THAT PREDICT PROPORTIONS IN EACH SIZE
  !     CLASS ARE BASED ON DIFFERENT TOP ASSUMPTIONS, WE NEED TO GET THE
  !     BIOMASS IN THE UNMERCH. TIP (BOLEWOOD ABOVE 4 INCH TOP DIAM) TO
  !     REALIGN THE PROPORTIONS.  FIRST, WE GET THE HEIGHT AT A 4 IN TOP
  !     DIAMETER.  ASSUMING A CONE, WE CAN GET VOLUME, CONVERT TO BIOMASS AND
  !     ALSO BREAK THE CONE UP INTO PIECES TO FIGURE OUT WHAT GOES IN EACH
  !     SIZE CLASS.

  if (DEBUG) write(JOSTND,*) 'ABOUT TO CALC BOLE TIP'

  !     NOW GET THE HEIGHT (HTF) AT A 4 INCH TOP DIAM (IB)
  DOBF = 4.0 / BRATIO(SPIYV,D,H)
  if ((D .gt. DOBF) .and. (D .gt. DBHMIN(SPIYV))) then

     HTF = 4.5 + (H - 4.5)/D*(D - DOBF) !assumes constant taper

     !       CALCULATE TOTAL VOLUME OF UNMERCH TIP

     if ((H - HTF) .gt.0) then
        TEMP = (H - HTF)*4*4*MYPI/12/12/12
     else
        TEMP = 0
     end if
     UMBTW(4) = (SG * TEMP / P2T)

     !       CALCULATE AMOUNT IN DIFFERENT SIZE CLASSES
     !       UMBTW = UNMERCHANTABLE BOLE TIP WEIGHT BY SIZE CLASS.
     !       (1 = 0-.25, 2 = 0-1, 3 = 0-3, 4 = 0 - 4)

     DBRK(0) = 0.0
     DBRK(1) = 0.25
     DBRK(2) = 1.0
     DBRK(3) = 3.0
     ANGLE = ATAN((H - HTF)/2.0)

     do J = 1,3
        TEMPHT = DBRK(J)/2*TAN(ANGLE)
        TEMP = (TEMPHT)*DBRK(J)*DBRK(J)*MYPI/12/12/12
        UMBTW(J) = (SG * TEMP / P2T)
     end do

     !       CALCULATE THE AMOUNT IN THE LITTLE MISSING PIECE, I.E. THE AMOUNT
     !       MISSING DUE TO THE FACT THAT THE VOLUME EQUATIONS GO TO 4 INCHES DIB
     !       AND THE CROWN EQUATIONS START AT 4 INCHES DOB.
     !       THIS ENTAILS USING BRATIO TO GET THE DIB THAT CORRESPONDS TO 4 IN DOB
     !       AND GETTING THE HEIGHT AT THIS DIAMETER.
     !       THIS LILPCE NEEDS TO BE SUBRACTED FROM UMBTW, SINCE NOT INCLUDED IN THE
     !       TTOPW CALCULATION. LATER IT WILL
     !       BE ADDED BACK INTO CROWNW, SO THE PIECE IS NOT EXCLUDED.

     if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'LS') .or.&
          &(VARACD .eq. 'NE') .or. (VARACD .eq. 'CS') .or.&
          &(VARACD .eq. 'ON')) then
        DIB = 4 * BRATIO(SPIYV,D,H)
        HTLP = 4.5 + (H - 4.5)/D*(D - 4.0) !assumes constant taper
        if ((HTLP - HTF) .gt.0) then
           LILPCE = MYPI*(HTLP - HTF)/12/12/12*(4*4 + 4*DIB + DIB*DIB)
        else
           LILPCE = 0.0
        end if
        LILPCE = LILPCE * SG/P2T
        if (LILPCE .lt.0) LILPCE = 0
        UMBTW(4) = UMBTW(4) - LILPCE
     end if
  else

     !       IF TREE IS LESS THAN 4" DBH OR UNMERCH, DO THE SAME THING
     !       FIRST CALCULATE TOTAL BIOMASS OF STEMWOOD ABOVE BREAST HEIGHT

     if (H .gt.4.5) then
        TEMP = (H - 4.5)*D*D*MYPI/12/12/12
        UMBTW(4) = (SG * TEMP / P2T)

        !         CALCULATE AMOUNT IN DIFFERENT SIZE CLASSES
        !         (1 = 0-.25, 2 = 0-1, 3 = 0-3, 4 = 0 - 4)

        DBRK(0) = 0.0
        DBRK(1) = 0.25
        DBRK(2) = 1.0
        DBRK(3) = 3.0
        ANGLE = ATAN((H - 4.5)/(D/2.0))

        do J = 1,3
           if ((J .eq.1) .or.&
                &(J .gt.1.and. D .gt. DBRK(J-1))) then
              TEMPD = MIN(DBRK(J), D)
              TEMPHT = TEMPD/2*TAN(ANGLE)
              TEMP = (TEMPHT)*TEMPD*TEMPD*MYPI/12/12/12
              UMBTW(J) = (SG * TEMP / P2T)
           end if
        end do
     end if

     !       NOW ADD STEM MATERIAL LESS THAN 4.5 FT IN HEIGHT.  ASSUME A CYLINDER.

     TEMP = MYPI*D*D/4/12/12*MIN(4.5,H)
     if (D .le.0.25) then
        K = 1
     else if (D .le.1.0) then
        K = 2
     else if (D .le.3.0) then
        K = 3
     else
        K = 4
     end if
     do J = K,4
        UMBTW(J) = UMBTW(J) + TEMP
     end do
  end if

  if (TTOPW .lt.0) TTOPW = 0
  if (LILPCE .lt.0) LILPCE = 0
  if (FOL .lt.0)    FOL = 0
  if (P1.lt.0.0)   P1 = 0.0
  if (P2.lt.0.0)   P2 = 0.0
  if (P3.lt.0.0)   P3 = 0.0
  if (P1.gt.1.0)   P1 = 1.0
  if (P2.gt.1.0)   P2 = 1.0
  if (P3.gt.1.0)   P3 = 1.0
  if (P2.lt. P1)    P2 = P1
  if (P3.lt. P2)    P3 = P2
  if (F1.lt.0.0)   F1 = 0.0
  if (F2.lt.0.0)   F2 = 0.0
  if (F3.lt.0.0)   F3 = 0.0
  if (F4.lt.0.0)   F4 = 0.0
  if (F1.gt.1.0)   F1 = 1.0
  if (F2.gt.1.0)   F2 = 1.0
  if (F3.gt.1.0)   F3 = 1.0
  if (F4.gt.1.0)   F4 = 1.0
  if (F2.lt. F1)    F2 = F1
  if (F3.lt. F2)    F3 = F2
  if (F4.lt. F3)    F4 = F3
  if (UMBTW(1) .lt.0) UMBTW(1) = 0
  if (UMBTW(2) .lt.0) UMBTW(2) = 0
  if (UMBTW(3) .lt.0) UMBTW(3) = 0
  if (UMBTW(4) .lt.0) UMBTW(4) = 0
  if (UMBTW(2) .lt. UMBTW(1)) UMBTW(2) = UMBTW(1)
  if (UMBTW(3) .lt. UMBTW(2)) UMBTW(3) = UMBTW(2)
  if (UMBTW(4) .lt. UMBTW(3)) UMBTW(4) = UMBTW(3)

  !  WHEN CALCULATING CROWN BIOMASS FOR EACH SIZE CLASS, ADJUST THE VALUES
  !  BASED ON THE UNMERCHANTABLE STEMWOOD.  THIS HELPS CORRECT THE PROBLEM
  !  THAT THE UNMERCH. STEMWOOD WASN'T INCLUDED WHEN PREDICTING
  !  PROPORTIONS IN EACH SIZE CLASS (IN LITERATURE).

  select case (SPILS)

     !       PROPORTION ESTIMATES FROM MAPLE AND SHORTLEAF PINE DID NOT SEEM
     !       TO INCLUDE ANY BOLEWOOD--JUST BRANCHWOOD

  case (18,19,26,27,49:52) ! maple
     if (TTOPW .lt. UMBTW(4)) TTOPW = UMBTW(4)
     TTOPW = TTOPW + FOL
     XV(0) = FOL
     XV(1) = (TTOPW-UMBTW(4))*(F2-F1)+UMBTW(1)
     XV(2) = (TTOPW-UMBTW(4))*(F3-F2)+(UMBTW(2)-UMBTW(1))
     XV(3) = (TTOPW-UMBTW(4))*(F4-F3)+(UMBTW(3)-UMBTW(2))
     XV(4) = (TTOPW-UMBTW(4))*(1-F4)+(UMBTW(4)-UMBTW(3))
     XV(5) = 0

  case (1:14) ! shortleaf pine
     if (TTOPW .lt. UMBTW(4)) TTOPW = UMBTW(4)
     XV(0) = FOL
     XV(1) = (TTOPW-UMBTW(4))*P1+UMBTW(1)
     XV(2) = (TTOPW-UMBTW(4))*(P2-P1)+(UMBTW(2)-UMBTW(1))
     XV(3) = (TTOPW-UMBTW(4))*(P3-P2)+(UMBTW(3)-UMBTW(2))
     XV(4) = (TTOPW-UMBTW(4))*(1-P3)+(UMBTW(4)-UMBTW(3))
     XV(5) = 0

     !       RED OAK CROWN PROPORTION PAPER INCLUDES BOLEWOOD LESS THAN 1 INCH

  case (30:39) ! red oak
     if (TTOPW .lt. (UMBTW(4) - UMBTW(2)))&
          &TTOPW = UMBTW(4) - UMBTW(2)
     XV(0) = FOL
     XV(1) = (TTOPW-UMBTW(4)+UMBTW(2))*P1
     XV(2) = (TTOPW-UMBTW(4)+UMBTW(2))*(P2-P1)
     XV(3) = (TTOPW-UMBTW(4)+UMBTW(2))*(P3-P2)+&
          &(UMBTW(3)-UMBTW(2))
     XV(4) = (TTOPW-UMBTW(4)+UMBTW(2))*(1-P3)+&
          &(UMBTW(4)-UMBTW(3))
     XV(5) = 0

     !       ASPEN CROWN PROPORTION PAPER INCLUDES BOLEWOOD LESS THAN .25 INCHES

  case default ! aspen
     if (TTOPW .lt. (UMBTW(4) - UMBTW(1)))&
          &TTOPW = UMBTW(4) - UMBTW(1)
     XV(0) = FOL
     XV(1) = (TTOPW-UMBTW(4)+UMBTW(1))*P1
     XV(2) = (TTOPW-UMBTW(4)+UMBTW(1))*(P2-P1)+&
          &(UMBTW(2)-UMBTW(1))
     XV(3) = (TTOPW-UMBTW(4)+UMBTW(1))*(P3-P2)+&
          &(UMBTW(3)-UMBTW(2))
     XV(4) = (TTOPW-UMBTW(4)+UMBTW(1))*(1-P3)+&
          &(UMBTW(4)-UMBTW(3))
     XV(5) = 0

  end select

  !     FOR LARGE TREES ADD THE LILPCE BACK IN TO THE RIGHT SIZE CLASS.

  if ((D .gt. DOBF) .and. (D .gt. DBHMIN(SPIYV))) then
     XV(4) = XV(4) + LILPCE
  end if

  999 continue

  return
end
