!======================================!
!   Compute large tree height growth   !
!======================================!
! WS $Id$
subroutine HTGF
  implicit none

  !   THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !   INCREMENT FOR EACH CYCLE AND LOADS IT INTO ARRAY HTG().
  !   THIS ROUTINE IS CALLED FROM **TREGRO** DURING REGULAR CYCLING.
  !   ENTRY **HTCONS** IS CALLED FROM **RCON** TO LOAD SITE
  !   DEPENDENT CONSTANTS THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'MULTCM.f90'
  include 'HTCAL.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'
  include 'GGCOM.f90'

  external RANN
  ! Variables
  logical :: DEBUG
  integer :: ISPC,I1,I2,I3,I,ITFN,INDX,IPASS,ILAT,ITLAT
  integer :: ICLS
  real    :: HGLAT2(5,MAXSP),HGEL2(MAXSP),HGELQ2(MAXSP),HGSL2(MAXSP),HGSI(MAXSP),HGDG2(MAXSP),&
       &HGRDG2(MAXSP),HGBAI2(MAXSP),HGBA2(MAXSP),HGLBA2(MAXSP),HGBLT2(MAXSP),HGCR2(MAXSP),HGBAD2(MAXSP),&
       &HGDSQ(MAXSP)
  real    :: MAXHTG,MXHTG1(MAXSP),MXHTG2(MAXSP)
  real    :: SCALE,ALBA,XHT,BAL,BAI,HTNEW,TEMHTG,DGI,SINDX,H,D
  real    :: SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,D1,D2,XHT2
  real    :: HHE1,HHE2,HHU1,HHU2,XWT,PCCFI,AP,TSITE,AGETEM,RATIO,AGEFUT,DFUT,HGE,HGU,ZZRAN,POTHTG,&
       &AGP10,HGUESS,RELHT,CRC,CRB,CRA,HGMDCR,RHB,RHR,RHM,RHYXS,RHX,RHK,FCTRKX,FCTRRB,RHXS,FCTRXB,FCTRM,&
       &HGMDRH,WTCR,WTRH,HTGMOD,HTNOW,TEMPH,BARK,BRATIO,BAUTBA,HNOW,BACHLO,ADJUST,XMOD,CRATIO,CRMOD,RHMOD
  real    :: DGLT,LTHTG,BRAT,DG10,HGBND
  real    :: MISHGF

  !   MODEL COEFFICIENTS AND CONSTANTS:

  !    IND2 -- ARRAY OF POINTERS TO SMALL TREES.

  !   SCALE -- TIME FACTOR DERIVED BY DIVIDING FIXED POINT CYCLE
  !            LENGTH BY GROWTH PERIOD LENGTH FOR DATA FROM
  !            WHICH MODELS WERE DEVELOPED.

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

  !    GS AND RW USE NEW REALTIONSHIP FROM CASTLE 2019.

  data MXHTG1/&
       &5.4918,   5.4191,   5.4283,   5.4918,   5.2011,&
       &5.4916,   5.4283,   5.4916,       0.,       0.,&
       &5.4918,       0.,   5.4283,       0.,       0.,&
       &0.,       0.,   5.4916,       0.,       0.,&
       &0.,   5.4191,   5.4918,   5.4918,       0.,&
       &0.,       0.,   4.6570,   4.6570,   4.6570,&
       &4.6570,   4.6570,   4.6570,   5.2011,   5.2011,&
       &5.2011,   5.2011,   5.2011,   5.2011,   4.6570,&
       &0.,   5.4916,   4.6570/

  data MXHTG2/&
       &-12.6438,  -8.8274,  -9.1641, -12.6438,  -7.7610,&
       &-9.5992,  -9.1641,  -9.5992,       0.,       0.,&
       &-12.6438,       0.,  -9.1641,       0.,       0.,&
       &0.,       0.,  -9.5992,       0.,       0.,&
       &0.,  -8.8274, -12.6438, -12.6438,       0.,&
       &0.,       0., -21.9333, -21.9333, -21.9333,&
       &-21.9333, -21.9333, -21.9333,  -7.7610,  -7.7610,&
       &-7.7610,  -7.7610,  -7.7610,  -7.7610, -21.9333,&
       &0.,  -9.5992, -21.9333/

  data HGDG2/&
       &0.,       0.,       0.,       0.,       0.,&
       &2.9620,       0.,   3.7974,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,   3.7974,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,   2.9620,       0./

  data HGRDG2/&
       &10.1890,  15.3820,   8.7587,  10.1890,   6.5757,&
       &0.,   8.7587,       0.,       0.,       0.,&
       &10.1890,       0.,   8.7587,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,  15.3820,  10.1890,  10.1890,       0.,&
       &0.,       0.,   6.5757,   6.5757,   6.5757,&
       &6.5757,   6.5757,   6.5757,   6.5757,   6.5757,&
       &6.5757,   6.5757,   6.5757,   6.5757,   6.5757,&
       &0.,       0.,   6.5757/

  data HGBAI2/&
       &0.,  -0.0694,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,  -0.0694,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0./

  data HGBA2/&
       &0.,  -0.0285,       0.,       0.,   0.0080,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,  -0.0285,       0.,       0.,       0.,&
       &0.,       0.,   0.0080,   0.0080,   0.0080,&
       &0.0080,   0.0080,   0.0080,   0.0080,   0.0080,&
       &0.0080,   0.0080,   0.0080,   0.0080,   0.0080,&
       &0.,       0.,   0.0080/

  data HGLBA2/&
       &1.2845,   6.1190,   1.2324,   1.2845,       0.,&
       &0.,   1.2324,   1.4843,       0.,       0.,&
       &1.2845,       0.,   1.2324,       0.,       0.,&
       &0.,       0.,   1.4843,       0.,       0.,&
       &0.,   6.1190,   1.2845,   1.2845,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0./

  data HGBLT2/&
       &0.,       0.,       0.,       0.,  -0.0079,&
       &0.,       0.,  -0.0142,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,  -0.0142,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,  -0.0079,  -0.0079,  -0.0079,&
       &-0.0079,  -0.0079,  -0.0079,  -0.0079,  -0.0079,&
       &-0.0079,  -0.0079,  -0.0079,  -0.0079,  -0.0079,&
       &0.,       0.,  -0.0079/

  data HGBAD2/&
       &0.,       0.,  -0.0399,       0.,       0.,&
       &0.,  -0.0399,       0.,       0.,       0.,&
       &0.,       0.,  -0.0399,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0./

  data HGCR2/&
       &0.,       0.,   0.0363,       0.,       0.,&
       &0.,   0.0363,       0.,       0.,       0.,&
       &0.,       0.,   0.0363,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0./

  data HGDSQ/&
       &-0.0098,       0.,  -0.0098,  -0.0098,       0.,&
       &-0.0082,  -0.0098,  -0.0086,       0.,       0.,&
       &-0.0098,       0.,  -0.0098,       0.,       0.,&
       &0.,       0.,  -0.0086,       0.,       0.,&
       &0.,       0.,  -0.0098,  -0.0098,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,  -0.0082,       0./

  !  COEFFICIENTS--CROWN RATIO (CR) BASED HT. GRTH. MODIFIER

  data CRA /100.0/, CRB /3.0/, CRC /-5.0/

  !  COEFFICIENTS--RELATIVE HEIGHT (RH) BASED HT. GRTH. MODIFIER

  data RHK /1.0/, RHXS /0.0/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'HTGF',4,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE HTGF  CYCLE =',I5)
  if (DEBUG)write(JOSTND,*) 'IN HTGF AT BEGINNING,HTCON=',&
       &HTCON,'RMAI=',RMAI,'ELEV=',ELEV
  SCALE = FINT/YR
  ISMALL = 0
  ALBA = 0.0
  if (BA.gt.0.0)ALBA = ALOG(BA)

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)
  if (DEBUG)write(JOSTND,*)'HTGF- IY(ICYC),XHMULT= ',&
       &IY(ICYC), XHMULT

  !   BEGIN SPECIES LOOP:

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)
     if (DEBUG)write(JOSTND,100)HTCON(ISPC),ITLAT,ELEV,SLOPE,&
          &SITEAR(ISPC)
     100 format(' IN HTGF 100F HTCON,ITLAT,ELEV,SLOPE,SITE=',F10.4,I5,&
          &2F10.4,F6.0)
     XHT = XHMULT(ISPC)
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) then
        XHT2 = HCOR2(ISPC)
     else
        XHT2 = 1.0
     end if
     SINDX = SITEAR(ISPC)

     !   BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.0
        if (PROB(I).le.0.0) then
           if (LTRIP) then
              ITFN = ITRN+2*I-1
              HTG(ITFN) = 0.
              HTG(ITFN+1) = 0.
           end if
           go to 30
        end if

        HGUESS = 0.
        select case (ISPC)

           !  SPECIES USING EQUATIONS FROM THE UT VARIANT (21=GB)

           !  THESE ESTIMATES ARE NO LONGER USED; GB TREES OF ALL SIZES ARE GROWTH
           !  WITH EQUATIONS IN REGENT

        case(21)

           !   FOR 21=GB:
           !   BYPASS CALCULATION IF DIAMETER IS LESS THAN LOWER LIMIT
           !   SAVE POINTERS TO SMALL TREES

           if (DBH(I).lt.0.5.or. HT(I).le.4.5) then
              ISMALL = ISMALL + 1
              IND2(ISMALL) = I
              go to 201
           end if
           ADJUST = 1.0
           D = DBH(I)
           ICLS = IFIX(D + 1.0)
           if (ICLS .gt.41) ICLS = 41
           BARK = BRATIO(ISPC,D,HT(I))
           BAUTBA = BAU(ICLS)/BA
           HNOW = HT(I)
           HHE1 = 0.0
           HHE2 = 0.0
           HHU1 = 0.0
           HHU2 = 0.0
           XWT = 0.0
           PCCFI = PCCF(ITRE(I))
           DGI = DG(I)
           AP = ABIRTH(I)

           !  ADJUST AP FOR BREAST HIGH AGE CURVES IF NECESSARY

           TSITE = SITEAR(ISPC)
           if (TSITE.lt.20.)TSITE = 20.
           AP = AP-(4.5/(-0.22+0.0155*TSITE))
           if (AP.lt.1.)AP = 1.

           !  USE SITE INDEX CURVES AS BASIS FOR HEIGHT IN EVEN-AGED STANDS
           !  SITE CURVE BREAST HEIGHT AGE, BASE AGE 100 ENG SPRUCE/SALPINE FIR
           !  ALEXANDER 1967.  RES PAPER RM32

           TSITE = SITEAR(ISPC)
           AGETEM = AP
           if (AGETEM .lt.30.0) AGETEM = 30.0
           HHE1 = (2.75780*TSITE**0.83312) * ((1.0 - EXP(-0.015701*AGETEM))&
                &**(22.71944*TSITE**(-0.63557))) + 4.5
           if (AP .lt. AGETEM) HHE1 = ((HHE1 - 4.5) / AGETEM) * AP+4.5
           RATIO = 1.0 - BAUTBA
           if (RATIO .lt.0.728) RATIO = 0.728
           HHE1 = HHE1 * RATIO

           HHU1 = 42.269377 * (1.0 - EXP(-0.165687 * D))&
                &** 1.184734 + 4.5

           if (DEBUG)write(JOSTND,*)' IN HTGF 1ST CALL TO GEMHT= ',&
                &HHE1,HHU1,TSITE,D,AP,AGETEM,BAUTBA,RATIO

           AGEFUT = AP + 10.0
           AGETEM = AGEFUT
           if (AGETEM .lt.30.0) AGETEM = 30.0
           DFUT = D + DGI/BARK
           HHE2 = (2.75780*TSITE**0.83312) * ((1.0 - EXP(-0.015701*AGETEM))&
                &**(22.71944*TSITE**(-0.63557))) + 4.5
           if (AGEFUT .lt. AGETEM) HHE2 = ((HHE2 - 4.5) / AGETEM) * AGEFUT+4.5
           RATIO = 1.0 - BAUTBA
           if (RATIO .lt.0.728) RATIO = 0.728
           HHE2 = HHE2 * RATIO

           HHU2 = 42.269377 * (1.0 - EXP(-0.165687 * DFUT))&
                &** 1.184734 + 4.5

           if (DEBUG)write(JOSTND,*)' IN HTGF 2ND CALL TO GEMHT= ',&
                &HHE2,HHU2,TSITE,DFUT,AGEFUT,AGETEM,BAUTBA,RATIO

           ! IF EVEN-AGED,UNEVEN-AGED AND BA .LT. 70, OR UNEVEN-AGED AND PCT(I)
           ! IS .GE.40 (OVERSTORY) THEN HTG(I) IS EVEN-AGED HT GROWTH ESTIMATE.

           HTG(I) = (HHE2 - HHE1)*ADJUST

           ! IF STAND IS UNEVEN-AGED AND BA IS .GE. TO 70 THEN USE A BLEND.
           ! TREES WITH PCT() .LE. 10 (OVERTOPPED TREES) GET UNEVEN-AGED
           ! HEIGHT GROWTH; TREES WITH PCT() .GT. 10 AND .LT. 40 GET A
           ! WEIGHTED AVERAGE OF EVEN/UNEVEN-AGED HEIGHT GROWTH.

           if (AGERNG .gt.40.0.and. BA .ge.70.0) then
              if (PCT(I) .le.10.)HTG(I) = HHU2 - HHU1
              if ((PCT(I) .gt.10.0) .and. (PCT(I) .lt.40.0)) then
                 HGE = (HHE2 - HHE1)*ADJUST
                 HGU = HHU2 - HHU1
                 XWT = ((PCT(I)-10.)*(10./3.))/100.
                 HTG(I) = XWT*HGE + (1.0-XWT)*HGU
              end if
           end if

           !     ADD RANDOM INCREMENT TO HTG.  GETS AWAY FROM ALL TREES HAVING
           !     THE SAME INCREMENT (ESP UNDER EDMINSTERS EVEN-AGED LOGIC).

           190 continue
           ZZRAN = 0.0
           if (DGSD .gt.0.0) then
              ZZRAN = BACHLO(0.0,1.0,RANN)
              if (ZZRAN .gt. DGSD .or. ZZRAN .lt. (-DGSD)) go to 190
              if (DEBUG)write(JOSTND,9984) I,HTG(I),ZZRAN,SCALE
              9984 format(1H ,'IN HTGF 9984 FORMAT',I5,2X,3(F10.4,2X))
           end if
           HTG(I) = HTG(I) + ZZRAN*0.1

           !  IF STAGNATION EFFECT IS ON FOR THIS SPECIES,
           !  ONLY REDUCE HT GROWTH BY HALF OF DSTAG, NOT DSTAG. DIXON 3-9-93

           if (ISTAGF(ISPC).ne.0)HTG(I) = HTG(I)*(DSTAG+1.0)*.5
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
           if (DEBUG)&
                &write(JOSTND,*)' HTGF I,ISPC,XWT,HHE1,HHE2,HHU1,HHU2,HT,HTG = ',&
                &I,ISPC,XWT,HHE1,HHE2,HHU1,HHU2,HT(I),HTG(I)

           201 continue
           HTG(I) = HTG(I)*SCALE*XHMULT(ISPC)*XHT2

           !       APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT.

           HTG(I) = HTG(I)*MISHGF(I,ISPC)

           TEMHTG = HTG(I)

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
              HTG(I) = SIZCAP(ISPC,4)-HT(I)
              if (HTG(I) .lt.0.1) HTG(I) = 0.1
           end if

           if (.not. LTRIP) go to 30
           ITFN = ITRN + 2*I - 1
           HTG(ITFN) = TEMHTG

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(ITFN)+HTG(ITFN)).gt.SIZCAP(ISPC,4)) then
              HTG(ITFN) = SIZCAP(ISPC,4)-HT(ITFN)
              if (HTG(ITFN) .lt.0.1) HTG(ITFN) = 0.1
           end if

           HTG(ITFN+1) = TEMHTG

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(ITFN+1)+HTG(ITFN+1)).gt.SIZCAP(ISPC,4)) then
              HTG(ITFN+1) = SIZCAP(ISPC,4)-HT(ITFN+1)
              if (HTG(ITFN+1) .lt.0.1) HTG(ITFN+1) = 0.1
           end if

           if (DEBUG)write(JOSTND,9001)HTG(ITFN),HTG(ITFN+1)
           9001 format('  LOWER HTG = ',F8.4,'  UPPER HTG = ',F8.4)

           !  SPECIES USING EQUATIONS FROM THE SO VARIANT (41=MC)
           !  IN SO, MISC. SPECIES - USE CURTIS, FOR. SCI. 20:307-316.
           !  CURTIS CURVES ARE PRESENTED IN METRIC (3.2808 ?)

           ! EXCESSIVE HT GROWTH -- APPROX 30-40 FT/CYCLE, TAKE OUT METRIC MULT
           ! DIXON 11-05-92

        case(41)
           BAL = ((100.0-PCT(I))/100.0)*BA
           H = HT(I)
           D = DBH(I)

           SITAGE = 0.0
           SITHT = 0.0
           AGMAX = 0.0
           HTMAX = 0.0
           HTMAX2 = 0.0
           D2 = 0.0
           call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,DEBUG)

           if (H .ge. HTMAX) then
              HTG(I) = 0.1
              HTG(I) = SCALE*XHT*HTG(I)*XHT2
              go to 161
           end if
           if (SITAGE .gt. AGMAX) then
              POTHTG = 0.10
              go to 1319
           else
              AGP10 = SITAGE+10.0
           end if
           HGUESS = (SINDX - 4.5) / ( 0.6192 - 5.3394/(SINDX - 4.5)&
                &+ 240.29 * AGP10**(-1.4) +(3368.9/(SINDX - 4.5))&
                &*AGP10**(-1.4))
           HGUESS = HGUESS + 4.5
           if (DEBUG)write(JOSTND,*)' SINDX,ISPC,AGP10,I,HGUESS= '
           if (DEBUG)write(JOSTND,*) SINDX,ISPC,AGP10,I,HGUESS

           POTHTG = HGUESS-SITHT

           if (DEBUG)write(JOSTND,*)' I,ISPC,AGP10,SITHT,HGUESS= ',&
                &I,ISPC,AGP10,SITHT,HGUESS

           1319 continue

           !  HEIGHT GROWTH MODIFIERS

           if (DEBUG)write(JOSTND,*) ' AT 1319 CONTINUE FOR TREE',I,' HT= ',&
                &HT(I),' AVH= ',AVH
           RELHT = 0.0
           if (AVH .gt.0.0) RELHT = HT(I)/AVH
           if (RELHT .gt.1.5)RELHT = 1.5

           !     REVISED HEIGHT GROWTH MODIFIER APPROACH.

           !     CROWN RATIO CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
           !     GROWTH PEAKS IN MID-RANGE OF CR, DECREASES SOMEWHAT FOR LARGE
           !     CROWN RATIOS DUE TO PHOTOSYNTHETIC ENERGY PUT INTO CROWN SUPPORT
           !     RATHER THAN HT. GROWTH.  CROWN RATIO FOR THIS COMPUTATION NEEDS
           !     TO BE IN (0-1) RANGE; DIVIDE BY 100.  FUNCTION IS HOERL'S
           !     SPECIAL FUNCTION (REF. P.23, CUTHBERT&WOOD, FITTING EQNS. TO DATA
           !     WILEY, 1971).  FUNCTION OUTPUT CONSTRAINED TO BE 1.0 OR LESS.

           HGMDCR = (CRA * (ICR(I)/100.0)**CRB) * EXP(CRC*(ICR(I)/100.0))
           if (HGMDCR .gt.1.0) HGMDCR = 1.0

           !     RELATIVE HEIGHT CONTRIBUTION.  DATA AND READINGS INDICATE HEIGHT
           !     GROWTH IS ENHANCED BY STRONG TOP LIGHT AND HINDERED BY HIGH
           !     SHADE EVEN IF SOME LIGHT FILTERS THROUGH.  ALSO RESPONSE IS
           !     GREATER FOR GIVEN LIGHT AS SHADE TOLERANCE INCREASES.  FUNCTION
           !     IS GENERALIZED CHAPMAN-RICHARDS (REF. P.2 DONNELLY ET AL. 1992.
           !     THINNING EVEN-AGED FOREST STANDS...OPTIMAL CONTROL ANALYSES.
           !     USDA FOR. SERV. RES. PAPER RM-307).
           !     PARTS OF THE GENERALIZED CHAPMAN-RICHARDS FUNCTION USED TO
           !     COMPUTE HGMDRH BELOW ARE SEGMENTED INTO FACTORS
           !     FOR PROGRAMMING CONVENIENCE.

           RHB = (-1.45)
           RHR = 15.0
           RHM = 1.10
           RHYXS = 0.10
           RHX = RELHT
           FCTRKX = ( (RHK/RHYXS)**(RHM-1.0) ) - 1.0
           FCTRRB = -1.0*( RHR/(1.0-RHB) )
           FCTRXB = RHX**(1.0-RHB) - RHXS**(1.0-RHB)
           FCTRM = -1.0/(RHM-1.0)

           if (DEBUG)&
                &write(JOSTND,*) ' HTGF-HGMDRH FACTORS = ',&
                &ISPC, RHX, FCTRKX, FCTRRB, FCTRXB, FCTRM

           HGMDRH = RHK * ( 1.0 + FCTRKX*EXP(FCTRRB*FCTRXB) ) ** FCTRM

           !     APPLY WEIGHTED MODIFIER VALUES.

           WTCR = .25
           WTRH = 1.0 - WTCR
           HTGMOD = WTCR*HGMDCR + WTRH*HGMDRH

           !    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
           !    MULTIPLIED BY XHMULT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

           if (DEBUG) then
              write(JOSTND,*)' IN HTGF, I= ',I,' ISPC= ',ISPC,'HTGMOD= ',&
                   &HTGMOD,' ICR= ',ICR(I),' HGMDCR= ',HGMDCR
              write(JOSTND,*)' HT(I)= ',HT(I),' AVH= ',AVH,' RELHT= ',RELHT,&
                   &' HGMDRH= ',HGMDRH
           end if

           if (HTGMOD .ge.2.0) HTGMOD = 2.0
           if (HTGMOD .le.0.0) HTGMOD = 0.1

           HTG(I) = POTHTG * HTGMOD

           HTNOW = HT(I)+POTHTG
           if (DEBUG)write(JOSTND,901)ICR(I),PCT(I),BA,DG(I),HT(I),&
                &POTHTG,BAL,AVH,HTG(I),DBH(I),RMAI,HGUESS
           901 format(' HTGF',I5,13F9.2)

           !    HEIGHT GROWTH EQUATION, EVALUATED FOR EACH TREE EACH CYCLE
           !    MULTIPLIED BY SCALE TO CHANGE FROM A YR. PERIOD TO FINT AND
           !    MULTIPLIED BY XHMULT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.
           !    CHECK FOR HT GT MAX HT FOR THE SITE AND SPECIES

           TEMPH = H + HTG(I)
           if (TEMPH .gt. HTMAX) then
              HTG(I) = HTMAX - H
           end if
           if (HTG(I).lt.0.1)HTG(I) = 0.1

           161 continue
           if (DEBUG)write(JOSTND,*)&
                &' I,SCALE,HTG,HTMAX, H= ',I,SCALE,HTG(I),HTMAX,H

           !  SPECIES USING EQUATIONS FROM THE CA VARIANT (9=LP, 10=WB, 12=PM,
           !  14=KP, 15=FP, 16=CP, 17=LM, 19=GP, 20=WE, 25=WJ, 26=UT, 27=CJ)

        case(4,9:10,12,14:17,19:20,23,25:27)
           AGP10 = 0.0
           H = HT(I)

           SITAGE = 0.0
           SITHT = 0.0
           AGMAX = 0.0
           HTMAX = 0.0
           HTMAX2 = 0.0
           D1 = DBH(I)
           D2 = 0.0
           DGLT = DG(I)

           !  DETERMINE WHICH SPECIES TO PRCOESS FROM CA
           select case(ISPC)

              !  PROCESS RW AND GS WHICH USE THE FOLLOWING FUNCTIONAL FORM

              !  HI = EXP(X)
              !  X = B1 + D1^2 + LOG(D1) + LOG(SI) + LOG(DGLT) + LOG(H)

              !  WHERE
              !  HI = 10 YEAR HEIGHT GROWTH
              !  D1 = DIAMETER AT BREAST HEIGHT
              !  SI = SITE INDEX (BASE AGE 50)
              !  DGLT = 10-YEAR DIAMETER GROWTH
              !  H = TOTAL TREE HEIGHT

           case(4,23)

              ! CONVERT DGLT TO OUTSIDE BARK DIAMETER GROWTH
              BRAT = BRATIO(ISPC,D1,H)
              DG10 = DGLT/BRAT

              ! APPLY CONSTRAINTS IF H OF INCOMING TREE IS LESS THAN 4.5 FT
              ! HERE DG10 IS ASSUMED TO BE 0.1
              if (H .lt.4.5) DG10 = 0.1

              if (DEBUG)write(JOSTND,*)' IN HTGF - RW/GS DEBUG',' I=',I,&
                   &' ISPC=',ISPC,' D=',D1,' BRAT=',BRAT,' DGLT=', DGLT,&
                   &' DG10=',DG10

              LTHTG = EXP(1.412947 - 0.000204*D1**2 + 0.31971*LOG(D1) +&
                   &0.394005*LOG(SINDX) + 0.399888*LOG(DG10) - 0.451708*LOG(H))

              ! BOUND HEIGHT GROWTH BASED ON THE HEIGHT OF RECORD. BOUNDING IS
              ! APPLIED TO AVOID HAVING TREES REACH UNREALISTIC HEIGHTS.THE HEIGHT
              ! GROWTH BOUNDING FUNCTION PROPORTIONALLY ADJUSTS HEIGHT GROWTH VALUES
              ! SO HEIGHTS OF A RECORD WILL EVENTUALLY CONVERGE TO THE UPPER HEIGHT
              ! BOUNDING VALUE. LOWER BOUNDING VALUE (217 FT) IS BASED ON MAXIMUM TREE
              ! HEIGHT FOUND IN THE DATASET USED TO FIT THE RW HEIGHT GROWTH EQUATION.
              ! UPPER BOUNDING VALUE FOUND IN THE DATASET IS BASED ON CURRENT HEIGHT
              ! MAXIMUM FOUND IN NATURE: HYPERION REDWOOD (~380 FT).

              ! BOUNDING LOGIC:
              ! 1) IF HT IS BETWEEN 217 FT AND 380 FT, THEN HEIGHT GROWTH IS
              !    BOUND.
              ! 2) IF THE HT IS BELOW 217 FT THEN HEIGHT GROWTH IS NOT
              !    BOUND.
              ! 3) IF THE HT IS ABOVE 380 FT, THEN HEIGHT GROWTH BOUNDING
              !    VALUE IS SET TO 0.1.
              if (H .ge.217.0.and. H .lt.380.0) then
                 HGBND = 1.0 - ((H - 217.0)/(380.0 - 217.0))
                 if (HGBND .lt.0.1) HGBND = 0.1
              else if (H .lt.217.0) then
                 HGBND = 1.0
              else
                 HGBND = 0.1
              end if

              if (DEBUG)write(JOSTND,*)'IN HTGF - RW/GS DEBUG',' H=',H,&
                   &' HGBND=',HGBND,' LTHTG=',LTHTG

              HTG(I) = LTHTG * HGBND

              ! DEBUG
              if (DEBUG)write(JOSTND,*)'IN HTGF - RW/GS DEBUG',' D=', D1,&
                   &' H=',H,' SI=',SINDX, ' DG10', DG10,' HTG10YR=',HTG(I)

              !  PROCESS ALL OTHER SPECIES WHICH USE CA EQUATIONS

           case default
              call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
                   &DEBUG)

              !  NORMAL HEIGHT INCREMENT CALCULATON BASED ON INCOMMING TREE AGE
              !  FIRST CHECK FOR MAX, ASMYPTOTIC HEIGHT

              if (SITAGE .gt. AGMAX) then
                 POTHTG = 0.10
                 go to 1320
              else
                 AGP10 = SITAGE+10.0
              end if

              ! R5 USE DUNNING/LEVITAN SITE CURVE.
              ! SPECIES DIFFERENCES ARE ARE ACCOUNTED FOR BY THE SPECIES
              ! SPECIFIC SITE INDEX VALUES WHICH ARE SET AFTER KEYWORD PROCESSING.

              call HTCALC(IFOR,SINDX,ISPC,AGP10,HGUESS,JOSTND,DEBUG)

              POTHTG = HGUESS - SITHT

              if (DEBUG)write(JOSTND,91200)I,ISPC,AGP10,HGUESS,H
              91200 format(' IN GUESS AN AGE--I,ISPC,AGEP10,HGUESS,H ',2I5,3F10.2)

              ! ASSIGN A POTENTIAL HTG FOR THE ASYMPTOTIC AGE

              1320 continue
              XMOD = 1.0
              CRATIO = ICR(I)/100.0
              RELHT = H/AVH
              if (RELHT .gt.1.0)RELHT = 1.0
              if (PCCF(ITRE(I)) .lt.100.0)RELHT = 1.0

              !  THE TREE HEIGHT GROWTH MODIFIER (SMHMOD) IS BASED ON THE RITCHIE &
              !  HANN WORK (FOR.ECOL.&MGMT. 1986. 15:135-145).  THE ORIGINAL COEFF.
              !  (1.117148) IS CHANGED TO 1.016605 TO MAKE THE SMALL TREE HEIGHTS
              !  CLOSE TO THE SITE INDEX CURVE.  THE MODIFIER HAS TWO PARTS, ONE
              !  (CRMOD) FOR TREE VIGOR USING CROWN RATIO AS A SURROGATE; OTHER
              !  (RHMOD) FOR COMPETITION FROM NEIGHBORING TREES USING RELATIVE TREE
              !  HEIGHT AS A SURROGATE.

              CRMOD = (1.0-EXP(-4.26558*CRATIO))
              RHMOD = (EXP(2.54119*(RELHT**0.250537-1.0)))
              XMOD = 1.016605*CRMOD*RHMOD
              HTG(I) = POTHTG * XMOD
           end select

           if (HTG(I) .lt.0.1) HTG(I) = 0.1

           if (DEBUG)write(JOSTND,902)ICR(I),PCT(I),BA,DG(I),HT(I),&
                &POTHTG,AVH,HTG(I),DBH(I),RMAI,HGUESS,AGP10,XMOD,ABIRTH(I)
           902 format(' HTGF',I5,14F9.2)

           !  HTG IS MULTIPLIED BY SCALE TO CHANGE FROM A YR  PERIOD TO FINT AND
           !  MULTIPLIED BY XHT TO APPLY USER SUPPLIED GROWTH MULTIPLIERS.

           HTG(I) = SCALE*XHT*HTG(I)*XHT2

           if (DEBUG)write(JOSTND,*)' I=',I,&
                &' D=',DBH(I),' DG=',DG(I),' H=',H,' HTG=',HTG(I)

           !       APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT.

           HTG(I) = HTG(I)*MISHGF(I,ISPC)

           TEMHTG = HTG(I)

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
              HTG(I) = SIZCAP(ISPC,4)-HT(I)
              if (HTG(I) .lt.0.1) HTG(I) = 0.1
           end if

           if (.not.LTRIP) go to 30
           ITFN = ITRN+2*I-1
           HTG(ITFN) = TEMHTG

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(ITFN)+HTG(ITFN)).gt.SIZCAP(ISPC,4)) then
              HTG(ITFN) = SIZCAP(ISPC,4)-HT(ITFN)
              if (HTG(ITFN) .lt.0.1) HTG(ITFN) = 0.1
           end if

           HTG(ITFN+1) = TEMHTG

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(ITFN+1)+HTG(ITFN+1)).gt.SIZCAP(ISPC,4)) then
              HTG(ITFN+1) = SIZCAP(ISPC,4)-HT(ITFN+1)
              if (HTG(ITFN+1) .lt.0.1) HTG(ITFN+1) = 0.1
           end if

           if (DEBUG) write(JOSTND,9003) HTG(ITFN),HTG(ITFN+1)
           9003 format( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)

           !  SPECIES USING EQUATIONS FROM THE WS VARIANT

        case default
           BAL = ((100.0-PCT(I))/100.0)*BA
           if (BAL .le.0.0)BAL = .001
           HTG(I) = HTCON(ISPC)&
                &+ HGDG2(ISPC)*DG(I)&
                &+ HGRDG2(ISPC)*SQRT(DG(I)) + HGBA2(ISPC)*BA&
                &+ HGBAI2(ISPC)*(((DBH(I)+DG(I))**2.0)- DBH(I)*DBH(I))&
                &+ HGLBA2(ISPC)*ALBA + HGBLT2(ISPC)*BAL&
                &+ HGBAD2(ISPC)*BAL/DBH(I) + HGCR2(ISPC) * FLOAT(ICR(I))&
                &+ HGDSQ(ISPC)*DBH(I)*DBH(I)

           ! THE FOLLOWING MODIFIERS HAVE BEEN ADDED BECAUSE TREES WERE NOT
           ! HITTING SITE HEIGHT

           select case (ISPC)
           case(42)
              HTG(I) = HTG(I) *1.5
           case(2,22)
              HTG(I) = HTG(I) *1.25
           case(3,13)
              HTG(I) = HTG(I) *1.5
           case(5)
              HTG(I) = HTG(I) *1.2
           case(6)
              HTG(I) = HTG(I) *1.5
           case(7)
              HTG(I) = HTG(I) *1.2
           case(8,18)
              HTG(I) = HTG(I)*1.15
           case(34:39)
              HTG(I) = HTG(I)*0.75
           end select
           if (DEBUG)write(JOSTND,130)I,ISPC,DBH(I),HT(I),&
                &ICR(I),DG(I),BA,BAL,HTG(I)
           130 format(' IN HTGF I,ISPC,DBH,HT,ICR,DG,BA,BAL,HTG=',&
                &2I5,2F7.2,I5,4F7.2)

           ! THIS NEXT TRAP IS TEMPORARY WHILE DOLPH DOES SOME FURTHER ANALYSES.
           ! THE DSQ TERM CAUSES NEGATIVE HTG ESTIMATES FOR LARGE TREES.
           ! DIXON 8-18-93

           if (HTG(I) .lt.0.5) HTG(I) = 0.5

           ! THE FOLLOWING IS SO WE DON'T GET BIG HTG WHEN SMALL DG
           ! FOR LARGE TREES. DIXON 8-18-93

           if (DG(I).lt.1.0.and. DBH(I).gt.30.) HTG(I) = HTG(I)*DG(I)

           ! CHECK TO SEE THAT PREDICTED HTG DOES NOT EXCEED MAXIMUM BOUND
           ! FOR JP,OS, AND PM. (DID NOT HAVE A MAX HT FN FOR THESE SPECIES.

           BAI = ((DBH(I) + DG(I))**2.0) - DBH(I)*DBH(I)
           if (ISPC.eq.42.or. ISPC.eq.6) then
              MAXHTG = -2.16 + 4.22 * ALOG(BAI)
              if (HTG(I) .gt. MAXHTG) HTG(I) = MAXHTG
           end if

           ! CHECK TO SEE THAT HT + HTG DOES NOT EXCEED MAXIMUM ALLOWABLE HT.
           ! USE PP COEFFICIENTS FOR JP,OC AND LP FOR THIS CHECK.

           INDX = I
           IPASS = 0
           131 continue
           IPASS = IPASS + 1
           select case (ISPC)
           case(28:33,40,43)
              HTMAX = EXP(MXHTG1(ISPC)+MXHTG2(ISPC)/&
                   &((DBH(I)+DG(INDX)+1.)**2.0))+4.5
           case default
              HTMAX = EXP(MXHTG1(ISPC)+MXHTG2(ISPC)/(DBH(I)+DG(INDX)+1.))+4.5
           end select
           HTNEW = HT(I)+HTG(INDX)

           if (DEBUG)write(JOSTND,9131)&
                &INDX,ISPC,HT(I),HTG(INDX),HTNEW,HTMAX,DBH(I),DG(INDX)
           9131 format(' IN HTGF 9131F ',2I5,6F10.3)
           if (HTNEW .gt. HTMAX) HTG(INDX) = HTMAX-HT(I)
           if (HTG(INDX) .lt.0.1)HTG(INDX) = 0.1
           if (IPASS .eq.2) go to 141
           if (IPASS .eq.3) go to 142

           ! END OF HTG AND MAX HT CHECK SEQUENCE

           if (DEBUG)write(JOSTND,140)I,ISPC,BAI,MAXHTG,HTG(I)
           140 format(1H ,'HTGF 140F I,ISPC,BAI,MAXHTG,HTG = ',2I5,3F10.4)

           ! ADJUST PREDICTED HTG FOR TIME INTERVAL AND USER SPECIFIED MULTIPLIER

           HTG(I) = HTG(I)*SCALE*XHT*XHT2

           !       APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT.

           HTG(I) = HTG(I)*MISHGF(I,ISPC)

           TEMHTG = HTG(I)
           if (DEBUG)write(JOSTND,9225)ICYC,HTG(I),SCALE
           9225 format(' IN HTGF CYCLE ',I5,2F10.1)

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
              HTG(I) = SIZCAP(ISPC,4)-HT(I)
              if (HTG(I) .lt.0.1) HTG(I) = 0.1
           end if

           if (.not.LTRIP) go to 5
           ITFN = ITRN+2*I-1
           DGI = DG(I)
           if (DGI .lt.0.01) DGI = 0.01
           HTG(ITFN) = TEMHTG * DG(ITFN)/DGI
           INDX = ITFN
           go to 131
           141 continue

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(ITFN)+HTG(ITFN)).gt.SIZCAP(ISPC,4)) then
              HTG(ITFN) = SIZCAP(ISPC,4)-HT(ITFN)
              if (HTG(ITFN) .lt.0.1) HTG(ITFN) = 0.1
           end if

           DGI = DG(I)
           if (DGI .lt.0.01) DGI = 0.01
           HTG(ITFN+1) = TEMHTG * DG(ITFN+1)/DGI
           INDX = ITFN + 1
           go to 131
           142 continue

           ! CHECK FOR SIZE CAP COMPLIANCE.

           if ((HT(ITFN+1)+HTG(ITFN+1)).gt.SIZCAP(ISPC,4)) then
              HTG(ITFN+1) = SIZCAP(ISPC,4)-HT(ITFN+1)
              if (HTG(ITFN+1) .lt.0.1) HTG(ITFN+1) = 0.1
           end if

        end select

        !   END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

        5 continue

        !**   IF(DEBUG)THEN
        !**   HTNEW=HT(I)+HTG(I)
        !**   WRITE (JOSTND,9000) HTG(I),CON,XHT2,H2COF,D,
        !**  & WK1(I),HTNEW,I,ISPC
        !9000 FORMAT(' 9000 HTGF, HTG=',F8.4,' CON=',F12.4,' XHT2=',F8.4,
        !**  & ' H2COF=',F12.8,' D=',F8.4/' WK1=',F8.4,
        !**  & ' HTNEW=',F8.4,' I=',I4,' ISPC=',I2)
        !**   ENDIF

        if (.not.LTRIP) go to 30
        if (DEBUG) write(JOSTND,9002) HTG(ITFN),HTG(ITFN+1)
        9002 format( ' UPPER HTG =',F8.4,' LOWER HTG =',F8.4)

        30 continue
     end do

     !   END OF SPECIES LOOP

     40 continue
  end do
  if (DEBUG)write(JOSTND,60)ICYC
  60 format(' LEAVING SUBROUTINE HTGF   CYCLE =',I5)
  return

  entry HTCONS

  !  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT
  !  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC
  !  CONTAINS HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT
  !  DEPENDENT COEFFICIENTS FOR THE DIAMETER INCREMENT TERM, HGH2
  !  CONTAINS HABITAT DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED
  !  TERM, AND HGHC CONTAINS SPECIES DEPENDENT INTERCEPTS.  HABITAT
  !  TYPE IS INDEXED BY ITYPE (SEE /PLOT/ COMMON AREA).

  data HGLAT2/&
       &-14.053,-14.053,-14.053,-14.053,-14.053,&
       &-39.027,-39.027,-39.027,-39.027,-39.027,&
       &-12.246,-10.685,-10.685,-10.685,-10.685,&
       &-14.053,-14.053,-14.053,-14.053,-14.053,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-0.268, -0.268, -0.268,  3.097, -0.268,&
       &-12.246,-10.685,-10.685,-10.685,-10.685,&
       &-5.554, -5.554, -5.554, -4.282, -5.554,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &-14.053,-14.053,-14.053,-14.053,-14.053,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &-12.246,-10.685,-10.685,-10.685,-10.685,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &-5.554, -5.554, -5.554, -4.282, -5.554,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &-39.027,-39.027,-39.027,-39.027,-39.027,&
       &-14.053,-14.053,-14.053,-14.053,-14.053,&
       &-14.053,-14.053,-14.053,-14.053,-14.053,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &-3.831,  1.711, -3.831, -2.898, -3.831,&
       &0.0,    0.0,    0.0,    0.0,    0.0,&
       &-0.268, -0.268, -0.268,  3.097, -0.268,&
       &-3.831,  1.711, -3.831, -2.898, -3.831/

  data HGEL2/&
       &0.,       0.,  -0.0453,       0.,       0.,&
       &0.,  -0.0453,       0.,       0.,       0.,&
       &0.,       0.,  -0.0453,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0./

  data HGELQ2/&
       &0.,       0.,       0.,       0.,  -0.0007,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,  -0.0007,  -0.0007,  -0.0007,&
       &-0.0007,  -0.0007,  -0.0007,  -0.0007,  -0.0007,&
       &-0.0007,  -0.0007,  -0.0007,  -0.0007,  -0.0007,&
       &0.,       0.,  -0.0007/

  data HGSL2/&
       &0.,       0.,   3.2180,       0.,       0.,&
       &0.,   3.2180,       0.,       0.,       0.,&
       &0.,       0.,   3.2180,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0.,       0.,       0.,&
       &0.,       0.,       0./

  data HGSI/&
       &0.0881,   0.0974,   0.0713,   0.0881,   0.0643,&
       &0.0687,   0.0713,   0.0598,       0.,       0.,&
       &0.0881,       0.,   0.0713,       0.,       0.,&
       &0.,       0.,   0.0598,       0.,       0.,&
       &0.,   0.0974,   0.0881,   0.0881,       0.,&
       &0.,       0.,   0.0643,   0.0643,   0.0643,&
       &0.0643,   0.0643,   0.0643,   0.0643,   0.0643,&
       &0.0643,   0.0643,   0.0643,   0.0643,   0.0643,&
       &0.,   0.0687,   0.0643/

  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES.

  ITLAT = NINT(TLAT)
  if (ITLAT .le.35.) ILAT = 1
  if (ITLAT .eq.36.) ILAT = 2
  if (ITLAT .eq.37.) ILAT = 3
  if (ITLAT .eq.38.) ILAT = 4
  if (ITLAT .ge.39.) ILAT = 5

  do ISPC = 1,MAXSP
     select case (ISPC)

        !  SPECIES USING EQUATIONS FROM THE UT VARIANT (21=GB)

     case(21)
        HTCON(ISPC) = 0.0

        !  SPECIES USING EQUATIONS FROM THE SO VARIANT (41=MC)

     case(41)
        HTCON(ISPC) = 0.0

        !  SPECIES USING EQUATIONS FROM THE CA VARIANT (9=LP, 10=WB, 12=PM,
        !  14=KP, 15=FP, 16=CP, 17=LM, 19=GP, 20=WE, 25=WJ, 26=UT, 27=CJ)

     case(9:10,12,14:17,19:20,25:27)
        HTCON(ISPC) = 0.0

        !  SPECIES USING EQUATIONS FROM THE WS VARIANT

     case default
        HTCON(ISPC) = HGLAT2(ILAT,ISPC) + HGEL2(ISPC)*ELEV&
             &+ HGELQ2(ISPC)*ELEV*ELEV + HGSL2(ISPC)*SLOPE&
             &+ HGSI(ISPC)*SITEAR(ISPC)
     end select

     !  NOTE: CANNOT INCORPORATE HCOR2 ADJUSTMENT INTO HTCON( ) SINCE THE
     !        LARGE TREE HTG EQUATION IS NOT A LOG-LINEAR REGRESSION

     50 continue
  end do

  return
end
