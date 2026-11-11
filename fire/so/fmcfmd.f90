!======================!
!   fire/so : fmcfmd   !
!======================!
! FIRE-SO $Id$
subroutine FMCFMD (IYR, FMD)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMBURN
  !  PURPOSE:
  !     THIS SUBROUTINE RETURNS TWO TYPES OF INFORMATION: THE FUEL MODEL
  !     THAT WOULD BE USED IF THE STATIC FUEL MODEL OPTION IS SELECTED
  !     (STORED AS IFMD(1), WITH A WEIGTH OF FWT(1)=1.0 AND THE CLOSEST
  !     THE CLOSEST FUEL MODELS (UP TO 4) AND THEIR WEIGHTINGS FOR USE
  !     BY THE DYNAMIC FUEL MODEL

  !  CALL LIST DEFINITIONS:
  !     FMD:     FUEL MODEL NUMBER

  !  LOCAL VARIABLE DEFINITIONS:
  !     DSTLG:   YEARS SINCE LAST DISTURBANCE: FIRE OR HARVESTING
  !     IPASO:   SORNEC PLANT ASSOCIATION GROUP -> OREGON PLANT ASSOC'N
  !              'SUPER-GROUP' (EG DRY LODGEPOLE PINE)

  !  COMMON BLOCK VARIABLES AND PARAMETERS:
  !     SMALL:   SMALL FUELS FROM DYNAMIC FUEL MODEL
  !     LARGE:   LARGE FUELS FROM DYNAMIC FUEL MODEL

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  include 'FMFCOM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'SSTGMC.f90'

  !     LOCAL VARIABLE DECLARATIONS

  ! Variables
  integer :: ICLSS

  parameter(ICLSS = 14)

  integer :: FMD
  integer :: IPTR(ICLSS),ITYP(ICLSS)
  integer :: IPASO(92)
  real    :: XPTS(ICLSS,2),EQWT(ICLSS)
  real    :: WD(MAXSP),WT1(2),WT2(2),WT3(2),X(2),Y(2),X3
  logical :: DEBUG
  real    :: XSNG,XM,XMLP,XA,AFWT
  integer :: IYR,J,I,IPAG,IFMST
  real    :: DSTLG
  real    :: ALGSLP

  !     THIS TABLE IS THE BASIS FOR THE IPASO VARIABLE. IT IS BASED ON
  !     THE 92 PLANT ASSOCIATIONS FROM NICK CROOKSTON, AND ANNOTATED BY
  !     ELIZABETH WITH THE 8 PLANT GROUPS USED BY THE OREGON MODEL

  !  1:{CDS612 PSME-ABCO/SYAL/LIBO}      DRY MC
  !  2:{CDS613 PSME-ABCO/SYAL/FORB}      DRY MC
  !  3:{CDS614 PSME-ABCO/SYAL/CARU}      DRY MC
  !  4:{CEM111 PIEN/CAEU}                WET MC
  !  5:{CEM221 PIEN/EQAR-STRO}           WET MC
  !  6:{CEM222 PIEN/CLUN}                WET MC
  !  7:{CEM311 PIEN/VAOC2-FORB}          WET MC
  !  8:{CEM312 PIEN/VAOC2/CAEU}          WET MC
  !  9:{CLC111 PICO-PIAL/PELA}           DRY LP
  ! 10:{CLC112 PICO-PIAL/ARCO2}          DRY LP
  ! 11:{CLF111 PICO/FORB}                WET LP
  ! 12:{CLG311 PICO/STOC-BASIN}          DRY LP
  ! 13:{CLG313 PICO/STOC-LUCA-LINU}      DRY LP
  ! 14:{CLG314 PICO/STOC-LUCA-PUM}       DRY LP
  ! 15:{CLG315 PICO/FRVI/FEID}           DRY LP
  ! 16:{CLG411 PICO/CAPE-LUCA-PUM}       DRY LP
  ! 17:{CLG412 PICO/CAPE-LUCA-PEEU}      DRY LP
  ! 18:{CLG413 PICO/CAPE-STOC-BASIN}     DRY LP
  ! 19:{CLG415 PICO/SIHY-CAPE}           DRY LP
  ! 20:{CLH111 PICO/POTR/FRVI}           WET LP
  ! 21:{CLM111 PICO/CANE-ELGL-WET}       WET LP
  ! 22:{CLM112 PICO/POPR}                WET LP
  ! 23:{CLM113 PICO/CAEU}                WET LP
  ! 24:{CLM114 PICO/CAAQ}                WET LP
  ! 25:{CLM211 PICO/ARUV-PUM}            WET LP
  ! 26:{CLM311 PICO/VAOC2-PUM}           WET LP
  ! 27:{CLM312 PICO/VAOC2/CAEU}          WET LP
  ! 28:{CLM313 PICO/SPDO-FORB}           WET LP
  ! 29:{CLM314 PICO/SPDO/CAEU}           WET LP
  ! 30:{CLM411 PICO/XETE-PUM}            DRY LP
  ! 31:{CLM911 PICO/PIEN/ELPA2}          WET LP
  ! 32:{CLS112 PICO/ARTR-RHYO}           DRY LP
  ! 33:{CLS211 PICO/PUTR/STOC-PUM}       DRY LP
  ! 34:{CLS212 PICO/PUTR/CAPE-PUM}       DRY LP
  ! 35:{CLS213 PICO/PUTR/FORB-PUM}       DRY LP
  ! 36:{CLS214 PICO/PUTR/FEID-PUM}       DRY LP
  ! 37:{CLS215 PICO/RICE-PUTR/STOC-PUM}  DRY LP
  ! 38:{CLS216 PICO/PUTR-RHYO}           DRY LP
  ! 39:{CLS311 PICO/ARNE}                DRY LP
  ! 40:{CLS412 PICO/VASC-PUM}            DRY LP
  ! 41:{CLS413 PICO/VASC-FORB}           DRY LP
  ! 42:{CLS414 PICO/VASC/CAPE}           DRY LP
  ! 43:{CLS911 PICO/CEVE-ARPA-PUM}       DRY LP
  ! 44:{CMS111 TSME/VASC-DES}            HEM
  ! 45:{CPC211 PIPO-JUOC/CELE/FEID}      DRY PP
  ! 46:{CPF111 PIPO/WYMO}                WET PP
  ! 47:{CPG212 PIPO/CAPE-FEID-LALA2}     WET PP
  ! 48:{CPH311 PIPO-POTR/PONE}           WET PP
  ! 49:{CPS111 PIPO/PUTR-ARTR/FEID}      DRY PP
  ! 50:{CPS112 PIPO/PUTR-ARTR/SIHY}      DRY PP
  ! 51:{CPS121 PIPO/ARTR/PONE}           DRY PP
  ! 52:{CPS211 PIPO/PUTR/FEID-PUM}       DRY PP
  ! 53:{CPS212 PIPO/PUTR/STOC-PUM}       DRY PP
  ! 54:{CPS213 PIPO/PUTR-ARPA/STOC-PUM}  DRY PP
  ! 55:{CPS214 PIPO/PUTR-ARPA/CAPE-PUM}  DRY PP
  ! 56:{CPS215 PIPO/PUTR/CAPE-PUM}       DRY PP
  ! 57:{CPS216 PIPO/PUTR/FEID-AGSP-PUM}  DRY PP
  ! 58:{CPS217 PIPO/PUTR-ARPA/FEID-PUM}  DRY PP
  ! 59:{CPS218 PIPO/PUTR/SIHY-RHYO}      DRY PP
  ! 60:{CPS311 PIPO/PUTR-CEVE/STOC-PUM}  DRY PP
  ! 61:{CPS312 PIPO/PUTR-CEVE/CAPE-PUM}  DRY PP
  ! 62:{CPS314 PIPO/PUTR-CEVE/FEID}      DRY PP
  ! 63:{CPS511 PIPO/SYAL-FLOOD}          WET PP
  ! 64:{CRG111 ABMAS/CAPE}               DRY MC
  ! 65:{CRS111 ABMAS/ARNE}               DRY MC
  ! 66:{CRS112 ABMAS-TSME/ARNE/CAPE}     DRY MC
  ! 67:{CRS311 ABMAS/CACH/CHUM-CAPE}     DRY MC
  ! 68:{CWC111 ABCO-PIPO-CADE/AMAL}      DRY MC
  ! 69:{CWC211 ABCO/CEVE-CACH/PTAQ}      DRY MC
  ! 70:{CWC212 ABCO/CEVE-CACH/CARU}      DRY MC
  ! 71:{CWC213 ABCO/CEVE/CAPE-PTAQ}      DRY MC
  ! 72:{CWC215 ABCO-PSME-CEVE/ARUV}      DRY MC
  ! 73:{CWC311 ABCO-PICO/STOC-CAPE}      DRY MC
  ! 74:{CWC411 ABCO-PIPO-PILA/RIVI}      DRY MC
  ! 75:{CWC412 ABCO-PIPO-PILA/ARPA}      DRY MC
  ! 76:{CWC911 PIEN-BOTTOMS}             WET LP
  ! 77:{CWF431 ABCO/CLUN}                WET MC
  ! 78:{CWH111 ABCO/CEVE-CACH}           DRY MC
  ! 79:{CWH112 ABCO/CACH-PAMY/CHUM}      DRY MC
  ! 80:{CWH211 ABCO-PIPO-POTR/CAPE}      DRY MC
  ! 81:{CWM111 ABCO/ALTE}                WET MC
  ! 82:{CWS112 ABCO/CEVE-ARPA-PUM}       DRY MC
  ! 83:{CWS113 ABCO/CEVE-ARPA/CAPE-PEEU} DRY MC
  ! 84:{CWS114 ABCO/CEVE-PUM}            DRY MC
  ! 85:{CWS115 ABCO/CEVE/CAPE}           DRY MC
  ! 86:{CWS116 ABCO/CEVE/CEPR-FRVI}      DRY MC
  ! 87:{CWS117 ABCO-PIPO/ARPA/BERE}      DRY MC
  ! 88:{CWS312 ABCO/SYAL/FRVI}           DRY MC
  ! 89:{CWS313 ABCO-PIPO/SYAL/STJA}      DRY MC
  ! 90:{HQM121 POTR/ELGL}                WET LP
  ! 91:{HQM411 POTR-PICO/SPDO/CAEU}      WET LP
  ! 92:{HQS221 POTR/SYAL/ELGL}           WET LP

  data IPASO /&
       &3,  3,  3,  4,  4,  4,  4,  4,  6,  6,&
       &7,  6,  6,  6,  6,  6,  6,  6,  6,  7,&
       &7,  7,  7,  7,  7,  7,  7,  7,  7,  6,&
       &7,  6,  6,  6,  6,  6,  6,  6,  6,  6,&
       &6,  6,  6,  5,  1,  2,  2,  2,  1,  1,&
       &1,  1,  1,  1,  1,  1,  1,  1,  1,  1,&
       &1,  1,  2,  3,  3,  3,  3,  3,  3,  3,&
       &3,  3,  3,  3,  3,  7,  4,  3,  3,  3,&
       &4,  3,  3,  3,  3,  3,  3,  3,  3,  7,&
       &7,  7 /

  !     FIXED VALUES FOR INTERPOLATION FUNCTION

  data     Y / 0.0, 1.0 /

  !     THESE ARE THE INTEGER TAGS ASSOCIATED WITH EACH FIRE MODEL
  !     CLASS. THEY ARE RETURNED WITH THE WEIGHT

  data IPTR / 1,2,3,4,5,6,7,8,9,10,11,12,13,14 /

  !     THESE ARE 0 FOR REGULAR LINES, -1 FOR HORIZONTAL AND 1 FOR
  !     VERTICAL LINES. IF ANY OF THE LINES DEFINED BY XPTS() ARE OF
  !     AN UNUSUAL VARIETY, THIS MUST BE ENTERED HERE SO THAT
  !     SPECIAL LOGIC CAN BE INVOKED.  IN THIS CASE, ALL THE LINE
  !     SEGMENTS HAVE A |SLOPE| THAT IS > 0 AND LESS THAN INIF.

  data ITYP / ICLSS * 0 /

  !     XPTS: FIRST COLUMN ARE THE SMALL FUEL VALUES FOR EACH FIRE MODEL
  !     WHEN LARGE FUEL= 0 (I.E. THE X-INTERCEPT OF THE LINE). SECOND
  !     COLUMN CONTAINS THE LARGE FUEL VALUE FOR EACH FIRE MODEL WHEN
  !     SMALL FUEL=0 (I.E. THE Y-INTERCEPT OF THE LINE).

  data ((XPTS(I,J), J = 1,2), I = 1,ICLSS) /&
       &5., 15.,& ! FMD   1
       &5., 15.,& ! FMD   2
       &5., 15.,& ! FMD   3
       &5., 15.,& ! FMD   4
       &5., 15.,& ! FMD   5
       &5., 15.,& ! FMD   6
       &5., 15.,& ! FMD   7
       &5., 15.,& ! FMD   8
       &5., 15.,& ! FMD   9
       &15., 30.,& ! FMD  10 ! shares with 11
       &15., 30.,& ! FMD  11
       &30., 60.,& ! FMD  12 ! shares with 14
       &45.,100.,& ! FMD  13
       &30., 60./ ! FMD  14

  !     Initialize some of the other variables

  do I = 1,ICLSS
     EQWT(I) = 0.0
  end do

  !     BEGIN ROUTINE

  call DBCHK (DEBUG,'FMCFMD',6,ICYC)

  if (DEBUG) write(JOSTND,1) ICYC,IYR,LUSRFM
  1 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' LUSRFM=',L5)

  !     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.

  if (LUSRFM) return

  if (DEBUG) write(JOSTND,7) ICYC,IYR,HARVYR,LDYNFM,PERCOV,FMKOD,&
       &SMALL,LARGE
  7 format(' FMCFMD CYCLE= ',I2,' IYR=',I5,' HARVYR=',I5,&
       &' LDYNFM=',L2,' PERCOV=',F7.2,' FMKOD=',I4,&
       &' SMALL=',F7.2,' LARGE=',F7.2)

  !     IF USER-SPECIFIED FM DEFINITIONS, THEN WE ARE DONE.

  if (LUSRFM) return

  !     THE FOLLOWING FOREST CODES ARE USED
  !     CALIFORNIA = REGION 5
  !       505 KLAMATH NATIONAL FOREST
  !       506 LASSEN
  !       509 MODOC
  !       511 PLUMAS
  !     OREGON = REGION 6
  !       601 DESCHUTES NATIONAL FOREST
  !       602 FREMONT
  !       620 WINEMA
  !     INDUSTRY
  !       701 ASSUMED TO BE CALIFORNIA (GARY DIXON)

  if ((KODFOR .ge.500.and. KODFOR .lt.600)&
       &.or. KODFOR .eq.701) then ! CALIFORNIA

     !       COMPUTE WEIGHTS FOR DOMINANT SPECIES

     call SOSPDM(DBH,FMPROB,ISP,ITRN,WD)

     do I = 1,MAXSP

        if (WD(I) .gt.0.0) then

           !         DOMINANT: PINYON JUNIPER, JEFFREY PINE,
           !         INCENSE CEDAR (WHITE PINE, SUGAR PINE)

           select case (I)

           case (1,2,6,11)

              !             RULE WAS <30=2, >30=9
              !             CHANGED TO <25=2; >35=9; INTERPOLATE BETWEEN THE TWO

              X(1) = 25.0
              X(2) = 35.0
              J = 2

              WT1(J) = ALGSLP(PERCOV,X,Y,2)
              WT1(J-1) = 1.0 - WT1(J)

              if (WT1(1) .gt.0.0) then
                 EQWT(2) = EQWT(2) + WT1(1) * WD(I)
              end if

              if (WT1(2) .gt.0.0) then
                 EQWT(9) = EQWT(9) + WT1(2) * WD(I)
              end if

              !         PONDEROSA PINE, WHITE FIR, DOUGLAS-FIR,
              !         MOUNTAIN HEMLOCK, ENGELMANN SPRUCE

              !         THIS MODEL SELECTION IS DERIVED FROM AN AMBIGUOUS SET OF RULES
              !         IN THE SORNEC DOCUMENT. AMBIGUOUS=NON-DICHOTOMOUS.

           case (10,4,3,5,8)

              XSNG = 0.0
              do J = 1,NSNAG
                 if (DBHS(J) .gt. RMSQD)&
                      &XSNG = XSNG + DENIH(J) + DENIS(J)
              end do

              ! WT1: AVERAGE HEIGHT (AVH FROM **PLOT.F77**)
              X(1) = 30.0
              X(2) = 40.0
              J = 2
              WT1(J) = ALGSLP(AVH,X,Y,2)
              WT1(J-1) = 1.0 - WT1(J)

              ! WT2: PERCOV
              X(1) = 55.0
              X(2) = 65.0
              J = 2
              WT2(J) = ALGSLP(PERCOV,X,Y,2)
              WT2(J-1) = 1.0 - WT2(J)

              ! WT3: SNAG DENSITY
              X(1) = 3.5
              X(2) = 4.5
              J = 2
              WT3(J) = ALGSLP(XSNG,X,Y,2)
              WT3(J-1) = 1.0 - WT3(J)

              if (WT1(2) .gt.0.0.or. WT2(2) .gt.0.0) then
                 if (WT3(1) .gt.0.0) then
                    EQWT(9) = EQWT(9) +&
                         &WD(I) * WT1(2) * WT2(1) * WT3(1)
                    EQWT(9) = EQWT(9) +&
                         &WD(I) * WT1(2) * WT2(2) * WT3(1)
                    EQWT(9) = EQWT(9) +&
                         &WD(I) * WT1(1) * WT2(2) * WT3(1)
                 end if
                 if (WT3(2) .gt.0.0) then
                    EQWT(10) = EQWT(10) +&
                         &WD(I) * WT1(2) * WT2(1) * WT3(2)
                    EQWT(10) = EQWT(10) +&
                         &WD(I) * WT1(2) * WT2(2) * WT3(2)
                    EQWT(10) = EQWT(10) +&
                         &WD(I) * WT1(1) * WT2(2) * WT3(2)
                 end if
              end if
              if (WT1(1) .gt.0.0.and. WT2(1) .gt.0.0) then
                 EQWT(5) = EQWT(5) + WD(I) * WT1(1) * WT2(1)
              end if

              !         LODGEPOLE PINE, RED FIR (SUBALPINE FIR)

              !         USE SPECIES_CCF/STAND_CCF TO FIND IF THE STAND IS PURE PL OR PURE RED FIR
              !         PURE = >95% OF CCF. OTHERWISE USE WK2 (PREDICTED MORTALITY) TO FIND OUT
              !         IF LP MORTALITY (BY BASAL AREA) IS >50% OF TOTAL MORTALITY

           case (7,9)

              XA = 1.0
              if (RELDEN .gt.1.0E-6)&
                   &XA = MAX(RELDSP(7),RELDSP(9))/RELDEN

              ! WT1: RELATIVE DENSITY PL OR RF
              X(1) = 0.85
              X(2) = 0.95
              J = 2
              WT1(J) = ALGSLP(XA,X,Y,2)
              WT1(J-1) = 1.0 - WT1(J)

              if (WT1(2) .gt.0.0) then
                 EQWT(8) = EQWT(8) + WD(I) * WT1(2)
              end if

              if (WT1(1) .gt.0.0) then

                 XM = 0.0
                 XMLP = 0.0
                 do J = 1,ITRN
                    XA = WK2(J) * DBH(J)**2
                    if (ISP(I) .eq.7) XMLP = XMLP + XA
                    XM = XM + XA
                 end do

                 XA = 0.0
                 if (XM .gt.1.0E-6) XA = XMLP/XM

                 ! WT2: RELATIVE PL MORTALITY
                 X(1) = 0.45
                 X(2) = 0.55
                 J = 2
                 WT2(J) = ALGSLP(XA,X,Y,2)
                 WT2(J-1) = 1.0 - WT2(J)

                 if (WT2(1) .gt.0.0)&
                      &EQWT(8) = EQWT(8)  + WD(I) * WT1(1) * WT2(1)
                 if (WT2(2) .gt.0.0)&
                      &EQWT(10) = EQWT(10) + WD(I) * WT1(1) * WT2(2)

              end if

           case default
              ! fm 8 is the default for R5 when none of the above
              ! species is dominant
              EQWT(8) = EQWT(8) + WD(I)

           end select
        end if
     end do

  else ! OREGON

     !       FIND ONE OF THE FOLLOWING GROUPS FROM AMONG THE 92 SORNEC PLANT
     !       ASSOCIATION GROUPS:

     !         DRY PONDEROSA PINE (1)
     !         WET PONDEROSA PINE (2)
     !         DRY MIXED CONIFER  (3)
     !         WET MIXED CONIFER  (4)
     !         HEMLOCK            (5)
     !         DRY LODGEPOLE PINE (6)
     !         WET LODGEPOLE PINE (7)
     !         JUNIPER SHRUBLAND  (8)

     IPAG = IPASO(ITYPE)
     DSTLG = real(MIN((IYR-HARVYR), (IYR-BURNYR)))

     !     DETERMINE STRUCTURE CLASS IF IN LODGEPOLE PINE OR JUNIPER TYPES
     if ((IPAG .eq.6) .or. (IPAG .eq.7) .or. (IPAG .eq.8)) then
        call FMSSTAGE(TPAMIN,CCMIN,PCTSMX,SAWDBH,SSDBH,GAPPCT,&
             &IFMST,X3,FMPROB,FMICR)
     end if

     select case (IPAG)

     case (1) ! DRY PONDEROSA PINE

        ! WT1: PERCOV
        X(1) = 35.0
        X(2) = 45.0
        J = 2
        WT1(J) = ALGSLP(PERCOV,X,Y,2)
        WT1(J-1) = 1.0 - WT1(J)

        if (WT1(2) .gt.0.0) then
           EQWT(9) = EQWT(9) + WT1(2)
        end if
        if (WT1(1) .gt.0.0) then

           !             REMOVE RULES THAT WERE ONLY IN EFFECT FOR 1 YEAR; IE FMD=8
           !             IN YR 1 AFTER FIRE.- SARAH AND ELIZABETH (APR 2000)

           ! WT2: DSTLG
           X(1) = 5.0
           X(2) = 7.0
           J = 2
           WT2(J) = ALGSLP(DSTLG,X,Y,2)
           WT2(J-1) = 1.0 - WT2(J)

           if (WT2(1) .gt.0.0) then
              EQWT(2) = EQWT(2) + WT1(1) * WT2(1)
           end if
           if (WT2(2) .gt.0.0) then
              EQWT(6) = EQWT(6) + WT1(1) * WT2(2)
           end if

        end if

     case (2) ! WET PONDEROSA PINE

        !           ORIGINALLY >70=9, <70=5; CHANGE TO >80=9, <60=5

        ! WT1: PERCOV
        X(1) = 60.0
        X(2) = 80.0
        J = 2
        WT1(J) = ALGSLP(PERCOV,X,Y,2)
        WT1(J-1) = 1.0 - WT1(J)

        if (WT1(1) .gt.0.0) then
           EQWT(5) = EQWT(5) + WT1(1)
        end if
        if (WT1(2) .gt.0.0) then
           EQWT(9) = EQWT(9) + WT1(2)
        end if

        !     PLANT ASSOCIATION INDEX ITYPE IS USED TO IDENTIFY BITTERBRUSH (PUTR)
        !     CEANOTHUS (CEVE) AND MANZANITA (ARPA,ARNE) IN WET PP
        !     THESE RULES NEVER APPLY, SINCE BITTERBRUSH, CEANOTHUS/MANZANITA
        !     PLANT ASSOCIATIONS NEVER OCCUR IN WET PPINE GROUPS.

        !           IF (KODFOR .EQ. 602) THEN ! 'FREMONT'
        !           ! IF BITTERBRUSH
        !           IF (DISTLG .LE. 10) FMD = 8
        !           ! IF CEANOTHUS/MANZANITA
        !           IF (DISTLG .LE. 2) FMD = 5
        !             ELSE
        !           IF (DSTLG .LE. 2) THEN
        !           ! IF CEANOTHUS/MANZANITA
        !             FMD = 5
        !           ! IF BITTERBRUSH
        !             FMD = 6
        !           ENDIF
        !              ENDIF

     case (3) ! DRY MIXED CONIFER

        !           ORIGINALLY >50=8, <50=5; CHANGE TO >60=8, <40=5

        ! WT1: PERCOV
        X(1) = 40.0
        X(2) = 60.0
        J = 2
        WT1(J) = ALGSLP(PERCOV,X,Y,2)
        WT1(J-1) = 1.0 - WT1(J)

        if (WT1(1) .gt.0.0) then
           EQWT(5) = EQWT(5) + WT1(1)
        end if
        if (WT1(2) .gt.0.0) then
           EQWT(8) = EQWT(8) + WT1(2)
        end if

     case (4) ! WET MIXED CONIFER

        !           ORIGINALLY >50=8, <50=5; CHANGE TO >60=8, <40=5

        ! WT1: PERCOV
        X(1) = 40.0
        X(2) = 60.0
        J = 2
        WT1(J) = ALGSLP(PERCOV,X,Y,2)
        WT1(J-1) = 1.0 - WT1(J)

        if (WT1(1) .gt.0.0) then
           EQWT(5) = EQWT(5) + WT1(1)
        end if
        if (WT1(2) .gt.0.0) then
           EQWT(8) = EQWT(8) + WT1(2)
        end if

     case (5) ! HEMLOCK

        EQWT(8) = 1.0

     case (6) ! DRY LODGEPOLE PINE

        ! WT1: ACTBH - ACTIVE CROWN BASE HEIGHT
        X(1) = 2.0
        X(2) = 4.0
        J = 2
        WT1(J) = ALGSLP(real(ACTCBH),X,Y,2)
        WT1(J-1) = 1.0 - WT1(J)

        if (WT1(1) .gt.0.0) then
           if (IFMST .le.1) then
              EQWT(5) = EQWT(5) + WT1(1)
           else if (IFMST .ge.2) then
              EQWT(10) = EQWT(10) + WT1(1)
           end if
        end if

        if (WT1(2) .gt.0.0) then
           EQWT(8) = EQWT(8) + WT1(2)
        end if

     case (7) ! WET LODGEPOLE PINE

        if (IFMST .le.1) then

           ! WT1: ACTBH - ACTIVE CROWN BASE HEIGHT
           X(1) = 2.0
           X(2) = 4.0
           J = 2
           WT1(J) = ALGSLP(real(ACTCBH),X,Y,2)
           WT1(J-1) = 1.0 - WT1(J)

           if (WT1(1) .gt.0.0) then
              EQWT(5) = EQWT(5) + WT1(1)
           end if
           if (WT1(2) .gt.0.0) then
              EQWT(8) = EQWT(8) + WT1(2)
           end if

        else if (IFMST .gt.1.and. IFMST .lt.5) then

           EQWT(8) = 1.0

        else

           ! WT1: PERCOV
           X(1) = 40.0
           X(2) = 60.0
           J = 2
           WT1(J) = ALGSLP(PERCOV,X,Y,2)
           WT1(J-1) = 1.0 - WT1(J)

           if (WT1(1) .gt.0.0) then
              EQWT(3) = EQWT(3) + WT1(1)
           end if
           if (WT1(2) .gt.0.0) then
              EQWT(8) = EQWT(8) + WT1(2)
           end if

        end if

     case (8) ! JUNIPER SHRUBLAND

        !           ORIGINALLY WAS >70=8, <70=1 OR 6, DEPENDING ON STRUCTURAL STAGE
        !           CHANGE TO >80=8, <60=1 OR 6

        ! WT1: PERCOV
        X(1) = 60.0
        X(2) = 80.0
        J = 2
        WT1(J) = ALGSLP(PERCOV,X,Y,2)
        WT1(J-1) = 1.0 - WT1(J)

        if (WT1(1) .gt.0.0) then
           if (IFMST .le.1) then
              EQWT(1) = EQWT(1) + WT1(1)
           else
              EQWT(6) = EQWT(6) + WT1(1)
           end if
        end if
        if (WT1(2) .gt.0.0) then
           EQWT(8) = EQWT(8) + WT1(2)
        end if

     end select

  end if ! END OF OREGON RULES

  !     END OF DETAILED LOW FUEL MODEL SELECTION

  !     DURING THE 5 YEARS AFTER AN ENTRY, AND ASSUMING THAT SMALL+LARGE
  !     ACTIVIVITY FUELS HAVE JUMPED BY 10%, THEN MODEL 11 AND 14 ARE
  !     CANDIDATE MODELS, SHARING WITH 10 AND 12 RESPECTIVELY. THE
  !     WEIGHT OF THE SHARED RELATIONSHIP DECLINES FROM PURE 11 INITIALLY,
  !     TO PURE 10 AFTER THE PERIOD EXPIRES. SIMILARLY, COMPUTE WEIGHT FOR
  !     MODEL 14 ACTIVITY FUELS, SHARED WITH CURRENT MODEL 12. THE
  !     RELATIONSHIP CHANGES IN THE SAME WAS AS THE 10/11 FUELS.

  AFWT = MAX(0.0, 1.0 - (IYR - HARVYR) / 5.0)
  if (SLCHNG .ge. SLCRIT .or. LATFUEL) then
     LATFUEL = .true.
     EQWT(11) = AFWT
     EQWT(14) = AFWT
     if (AFWT .le.0.0) LATFUEL = .false.
  end if
  if (.not. LATFUEL) AFWT = 0.0

  !     MODELS 10,12,13 ARE ALWAYS CANDIDATE MODELS FOR NATURAL FUELS
  !     OTHER MODELS ARE ALSO CANDIDATES, DEPENDING ON COVER TYPE, ETC

  EQWT(10) = 1.0 - AFWT
  EQWT(12) = 1.0 - AFWT
  EQWT(13) = 1.0

  !     CALL FMDYN TO RESOLVE WEIGHTS, SORT THE WEIGHTED FUEL MODELS
  !     FROM THE HIGHEST TO LOWEST, SET FMD (UGING THE HIGHEST WEIGHT)

  call FMDYN(SMALL,LARGE,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYNFM,FMD)

  if (DEBUG) write (JOSTND,8) FMD
  8 format (' FMCFMD, FMD=',I4)

  return
end

  !     COMPUTE THE DOMINANT SPECIES WTIH WEIGHTS

  !     D  = REAL VECTOR OF DBH
  !     P  = REAL VECTOR OF STEMS/ACRE
  !     SP = INTEGER VECTOR OF SPECIES
  !     N  = INTEGER LENGTH OF D(), P() AND SP VECTORS
  !     WDOM = WEIGHTED DOMINANT CATEGORY [ RETURNED ]

subroutine SOSPDM(D,P,SP,N,WDOM)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  real    :: D,P,WDOM
  integer :: SP,N
  dimension D(N),P(N),SP(N),WDOM(MAXSP)

  integer :: I,J,K,PASS,BASP
  real    :: DT,XRAN,JITTER
  real    :: XBA(MAXSP)
  double precision :: SAVESO

  data PASS /50/, JITTER /0.2/
  !/
  !     ZERO BINS TO WEIGHTED DOMINANT CATEGORY AND COMPLEXITY INDEX

  do I = 1,MAXSP
     WDOM(I) = 0.0
  end do

  !     POPULATE BASIC SIZE CATEGORIES USING UNIFORM RANDOM JITTER.
  !     THE IDEA HERE IS TO SAMPLE DIAMETERS JITTERED
  !     BY 20%, THEN RECOMPUTE A WEIGHTED DOMINANT CATEGORY.

  call RANNGET(SAVESO)

  do J = 1,PASS

     !     ZERO BINS TO HOLD BA-BASED WEIGHTS FOR THE SPECIES CATEGORIES

     do I = 1,MAXSP
        XBA(I) = 0.0
     end do

     do I = 1,N
        call RANN(XRAN)
        DT = D(I) * (1.0 + JITTER * ((XRAN * 2.0) - 1.0))
        K = SP(I)
        XBA(K) = XBA(K) + (DT * DT * P(I))
     end do

     BASP = 7 ! LP IS DEFAULT FOR NO BA
     do K = 1,MAXSP
        if (XBA(K) .gt. XBA(BASP)) BASP = K
     end do

     WDOM(BASP) = WDOM(BASP) + 1.0
  end do

  call RANNPUT(SAVESO)

  do I = 1,MAXSP
     WDOM(I) = WDOM(I)/real(PASS)
  end do

  return
end
