!=================!
!   ws : smhtgf   !
!=================!
! WS $Id$
subroutine SMHTGF(NSPC,D,CR,BA,BAL,SI,H,JOSTND,DEBUG,HTGR)
  implicit none

  ! THIS SUBROUTINE COMPUTES THE HEIGHT GROWTH FOR SMALL TREES (HTGR)
  ! IN REAL SCALE.  THIS ROUTINE IS CALLED FROM REGENT.

  ! Variables
  logical :: DEBUG
  integer :: NSPC,JOSTND
  real    :: HTGR,SI,BAL,BA,CR,D,TEMBAL,FACTOR
  real    :: RDANUW
  real    :: HTMAX,AGE1,AGE2,H1,H2,H

  !     SPECIES LIST FOR WESTERN SIERRAS VARIANT.

  !     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
  !     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
  !     3 = WHITE FIR (WF)                    ABIES CONCOLOR
  !     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
  !     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
  !     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
  !     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
  !     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
  !     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
  !    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
  !    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
  !    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
  !    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
  !    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
  !    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
  !    16 = COULTER PINE (CP)                 PINUS COULTERI
  !    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
  !    18 = MONTEREY PINE (MP)                PINUS RADIATA
  !    19 = GRAY PINE (GP)                    PINUS SABINIANA
  !         (OR CALIFORNIA FOOTHILL PINE)
  !    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
  !    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
  !    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
  !    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
  !    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
  !    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
  !    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
  !    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
  !    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
  !    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
  !    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
  !    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
  !    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
  !         (OR CALIFORNIA WHITE OAK)
  !    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
  !    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
  !    35 = GIANT CHINQUAPIN (GC)             CHRYSOLEPIS CHRYSOPHYLLA
  !    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
  !    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
  !    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
  !    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
  !    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
  !    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
  !    42 = OTHER SOFTWOODS (OS)
  !    43 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !    FROM EXISTING WS EQUATIONS --
  !      USE 1(SP) FOR 11(WP) AND 24(MH)
  !      USE 2(DF) FOR 22(BD)
  !      USE 3(WF) FOR 13(SF)
  !      USE 4(GS) FOR 23(RW)
  !      USE 8(PP) FOR 18(MP)
  !      USE 34(TO) FOR 35(GC), 36(AS), 37(CL), 38(MA), AND 39(DG)
  !      USE 31(BO) FOR 28(LO), 29(CY), 30(BL), 32(VO), 33(IO), 40(BM), AND
  !                     43(OH)

  !    FROM CA VARIANT --
  !      USE CA11(KP) FOR 12(PM), 14(KP), 15(FP), 16(CP), 17(LM), 19(GP), 20(WE),
  !                       25(WJ), 26(WJ), AND 27(CJ)
  !      USE CA12(LP) FOR 9(LP) AND 10(WB)

  !    FROM SO VARIANT --
  !      USE SO30(MC) FOR 41(MC)

  !    FROM UT VARIANT --
  !      USE UT17(GB) FOR 21(GB)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = BA
  RDANUW = D

  TEMBAL = BAL
  if (TEMBAL .lt.5.0) TEMBAL = 5.0

  select case (NSPC)

     !  NOTE: SPECIES 21=GB, AND 41=MC DO NOT USE THIS ROUTINE

     !  SPECIES USING WS EQUATIONS: PINES
     !  1=SP, 5=IC, 6=JP, 8=PP, 11=WP, 18=MP, 24=MH, 42=OS
     !  TEST RUNS SHOW NEED FOR + & * CONSTANTS TO MATCH GROWTH
     !  DATA.  GD  3-24.94

  case(1,5,6,8,11,18,24,42)
     HTGR = 0.7452 - 0.003271*BAL - 0.1632*CR + 0.0217*CR*CR&
          &+ 0.00536*SI
     HTGR = EXP(HTGR)
     if (NSPC.eq.5.or.&
          &NSPC.eq.1.or. NSPC.eq.11.or. NSPC.eq.24) then
        HTGR = HTGR*1.75*(.80+0.004*(SI-50.))
     else
        HTGR = HTGR*1.50*(.80+0.004*(SI-50.))
     end if

     !  SPECIES USING WS EQUATIONS: FIRS
     !  2=DF, 3=WF, 7=RF, 13=SF, 22=BD
     !  TEST RUNS SHOW NEED FOR + CONSTANT & * CONSTANT TO MATCH GROWTH
     !  DATA.  GD 3-24-94

  case(2,3,7,13,22)
     HTGR = -0.2495 - 0.00111*BAL + 0.0100*CR*CR
     HTGR = EXP(HTGR)
     if (NSPC.eq.2.or. NSPC.eq.22.or.&
          &NSPC.eq.4.or. NSPC.eq.23) then
        HTGR = (HTGR+1.)*2.5*(.80+0.004*(SI-50.))
     else
        HTGR = (HTGR+0.75)*2.0*(.80+0.004*(SI-50.))
     end if

     !  SPECIES USING WS EQUATIONS: BLACK OAK
     !  28=LO, 29=CY, 30=BL, 31=BO, 32=VO, 33=IO, 40=BM, 43=OH

  case(28:33,40,43)
     HTGR = EXP(3.817 - 0.7829*ALOG(TEMBAL))

     !  SPECIES USING WS EQUATIONS: TANOAK
     !  34=TO, 35=GC, 36=AS, 37=CL, 38=MA, 39=DG

  case(34:39)
     HTGR = EXP(3.385 - 0.5898*ALOG(TEMBAL))

     !  SPECIES USING CA EQUATIONS: LODGEPOLE PINE
     !  9=LP, 10=WB

  case(9,10)
     FACTOR = 0.80+0.004*(SI-50.)
     HTGR = EXP(0.7452 - 0.003271*BAL - 0.1632*CR + 0.0217*CR*CR&
          &+ 0.00536*SI) * FACTOR * 1.75

     !  SPECIES USING CA EQUATIONS: KNOBCONE PINE
     !  12=PM, 14=KP, 15=FP, 16=CP, 17=LM, 19=GP, 20=WE, 25=WJ, 26=UJ, 27=CJ

  case(12,14:17,19:20,25:27)
     FACTOR = 0.80+0.004*(SI-50.)
     HTGR = EXP(0.7452 - 0.003271*BAL - 0.1632*CR + 0.0217*CR*CR&
          &+ 0.00536*SI) * FACTOR * 1.75

     !  RW(23) AND GS(4) EQUATION.

  case(4,23)
     HTMAX = (2.242202*SI)
     if (HTMAX-H.le.1.) then
        HTGR = 0.0

        !  CALCULATE CURRENT AGE AND AGE 5 YEARS HENCE
     else
        AGE1 = 1/-0.010742*&
             &LOG(1-((H)/2.242202/SI)**(1/0.919076))
        AGE2 = AGE1 + 5

        ! ESTIMATE H1 BASED ON AGE1 AND H2 BASED ON AGE2
        H1 = 2.242202*SI*(1-EXP(-0.010742*AGE1))**0.919076
        H2 = 2.242202*SI*(1-EXP(-0.010742*AGE2))**0.919076

        ! COMPUTE HEIGHT INCREMENT BASED ON SUBTRACTION OF H2 AND H1
        HTGR = H2 - H1
     end if
     if (DEBUG)write(JOSTND,*) "RW DEBUG"," H=", H, ' AGE1=', AGE1,&
          &" AGE2=", AGE2, " H1=", H1, " H2=", H2," SI=", SI,&
          &" HTGR=", HTGR
  end select

  !  IF ESTIMATED HEIGHT INCREMENT IS CLOSE TO ZERO

  if (HTGR .lt.0.1) HTGR = 0.1

  return
end
