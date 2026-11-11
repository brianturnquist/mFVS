!=====================!
!   fire/ci : fmcba   !
!=====================!
! FIRE-CI $Id$
subroutine FMCBA (IYR,ISWTCH)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMMAIN

  !  Purpose:
  !     Find the dominant species (by basal area). Set the initial live
  !     and dead fuel values as well. The dead fuels are only initialized
  !     in the first year, but COVTYP and the live fuels must be done
  !     each year.

  !  Local variable definitions:
  !     BAMOST:  The highest basal area in a single species
  !     CAREA:   The area covered by the crown at its widest point (sqft)
  !     COVINI:  The seral cover type to be used for initiating fuels in
  !              bare stands
  !     CRL:     Crown length
  !     CWIDTH:  The maximum width of the crowns (ft)
  !     FUINIE:  The initial fuel loadings for established stands (from JBrown)
  !     FUINII:  The initial fuel loadings for initiating stands (from JBrown)
  !     FULIVE:  The herb/shrub/regen for established stands (from JBrown)
  !     FULIVI:  The herb/shrub/regen for initiating stands (from JBrown)
  !     ISWTCH:  =1 if called by SVSTART
  !              =0 if called by any other subroutine (FMMAIN, FMPPHV)
  !     TOTBA:   The total basal area in the stand (used in the fuels calcs)
  !     TOTCRA:  The sum of the area of the crowns, per acre (sqft)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'CICOM.f90'
  include 'FMCOM.f90'

  !  Variable declarations.

  !     MAXIMUM NUMBER OF VEGETATION CODES; MUST MATCH THE
  !     DIMENSION OF THE ICITYP ARRAY IN CI **BLKDAT**

  ! Variables
  logical :: DEBUG
  integer :: MXVCODE
  parameter (MXVCODE = 130)
  integer*1 :: COVINI(MXVCODE),MAPPVG(MXVCODE),MAPS9B(MXVCODE),MAPDRY(MXVCODE)
  integer :: MYACT(3)
  integer :: IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC,PVG,S9B
  real    :: BAMOST,TOTCRA,CWIDTH
  real    :: FULIVE(2,MAXSP),FULIVI(2,MAXSP)
  real    :: FUINIE(MXFLCL,MAXSP),FUINII(MXFLCL,MAXSP)
  real    :: STFUEL(MXFLCL,2),XCOV(2),YLOAD(2),FOTOVAL(MXFLCL)
  real    :: PRMS(12),FOTOVALS(9)
  real    :: BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD,DCYMLT

  !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

  !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
  !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
  !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
  !     4 = GRAND FIR (GF)                   ABIES GRANDIS
  !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
  !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
  !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
  !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
  !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
  !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
  !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
  !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
  !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
  !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
  !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
  !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
  !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
  !    18 = OTHER SOFTWOODS (OS)
  !    19 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE IE VARIANT:
  !      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
  !      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
  !      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
  !      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)

  !  FROM THE UT VARIANT:
  !      USE 12(WJ) FOR 14(WJ)
  !      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
  !                                                  REALLY WC39=OT)

  !     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER

  !                  herbs, shrubs

  data FULIVE /&
       &0.15,   0.1,& !WP
       &0.2,   0.2,& !WL
       &0.2,   0.2,& !DF
       &0.15,   0.1,& !GF
       &0.2,   0.2,& !WH
       &0.2,   0.2,& !RC
       &0.2,   0.1,& !LP
       &0.15,   0.2,& !ES
       &0.15,   0.2,& !AF
       &0.2,  0.25,& !PP
       &0.2,  0.10,& !WB uses LP
       &0.2,   0.2,& !PY uses DF
       &0.25,  0.25,& !AS - Ottmar 2000
       &0.04,  0.05,& !WJ - Ottmar 2000
       &0.25,  0.25,& !MC - use CR AS
       &0.2,  0.10,& !LM uses UT WB/LM
       &0.25,  0.25,& !CW use AS
       &0.15,   0.2,& !OS
       &0.25,  0.25/ !OH use AS

  !     INITIAL LIVE FUEL LOADING FOR INTIALIZING STANDS WITH 10% COVER

  !                  herbs, shrubs

  data FULIVI /&
       &0.30,   2.0,& !WP
       &0.4,   2.0,& !WL
       &0.4,   2.0,& !DF
       &0.30,   2.0,& !GF
       &0.4,   2.0,& !WH
       &0.4,   2.0,& !RC
       &0.4,   1.0,& !LP
       &0.30,   2.0,& !ES
       &0.30,   2.0,& !AF
       &0.25,  0.10,& !PP
       &0.4,   1.0,& !WB uses LP
       &0.4,   2.0,& !PY uses DF
       &0.18,  1.32,& !AS - Ottmar 2000
       &0.13,  1.63,& !WJ - Ottmar 2000
       &0.18,  1.32,& !MC - use CR AS
       &0.4,   1.0,& !LM uses LP
       &0.18,  1.32,& !CW use AS
       &0.30,   2.0,& !OS
       &0.18,  1.32/ !OH use AS

  !     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
  !     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

  !      <.25   to1   1-3   3-6  6-12 12-20 20-35 35-50   >50   Lit  Duf

  data FUINIE /&
       &1.0,  1.0,  1.6, 10.0, 10.0, 10.0,  0.0,  0.0,  0.0,  0.8, 30.0,& !WP
       &0.9,  0.9,  1.6,  3.5,  3.5,  0.0,  0.0,  0.0,  0.0,  0.6, 10.0,& !WL
       &0.9,  0.9,  1.6,  3.5,  3.5,  0.0,  0.0,  0.0,  0.0,  0.6, 10.0,& !DF
       &0.7,  0.7,  3.0,  7.0,  7.0,  0.0,  0.0,  0.0,  0.0,  0.6, 25.0,& !GF
       &2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0,& !WH
       &2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0,& !RC
       &0.9,  0.9,  1.2,  7.0,  8.0,  0.0,  0.0,  0.0,  0.0,  0.6, 15.0,& !LP
       &1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0,& !ES
       &1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0,& !AF
       &0.7,  0.7,  1.6,  2.5,  2.5,  0.0,  0.0,  0.0,  0.0,  1.4,  5.0,& !PP
       &0.9,  0.9,  1.2,  7.0,  8.0,  0.0,  0.0,  0.0,  0.0,  0.6, 15.0,& !WB use LP
       &0.9,  0.9,  1.6,  3.5,  3.5,  0.0,  0.0,  0.0,  0.0,  0.6, 10.0,& !PY use DF
       &0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8,& !AS - Ottmar 2000
       &0.2,  0.8,  2.3,  1.4,  3.0,  0.0,  0.0,  0.0,  0.0,  0.5,  0.0,& !WJ - Ottmar 2000
       &0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8,& !MC - use CR AS
       &0.9,  0.9,  1.2,  7.0,  8.0,  0.0,  0.0,  0.0,  0.0,  0.6, 15.0,& !LM use LP
       &0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8,& !CW use AS
       &1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0,& !OS
       &0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8/ !OH use AS

  !     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
  !     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

  !      <.25   to1   1-3   3-6  6-12  12-20 20-35 35-50  >50   Lit  Duf

  data FUINII /&
       &0.6,  0.6,  0.8,  6.0,  6.0,  6.0,  0.0,  0.0,  0.0,  0.4, 12.0,& !WP
       &0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0,& !WL
       &0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0,& !DF
       &0.5,  0.5,  2.0,  2.8,  2.8,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0,& !GF
       &1.6,  1.6,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0,& !WH
       &1.6,  1.6,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0,& !RC
       &0.6,  0.7,  0.8,  2.8,  3.2,  0.0,  0.0,  0.0,  0.0,  0.3,  7.0,& !LP
       &0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0,& !ES
       &0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0,& !AF
       &0.1,  0.1,  0.2,  0.5,  0.5,  0.0,  0.0,  0.0,  0.0,  0.5,  0.8,& !PP
       &0.6,  0.7,  0.8,  2.8,  3.2,  0.0,  0.0,  0.0,  0.0,  0.3,  7.0,& !WB use LP
       &0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0,& !PY use DF
       &0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6,& !AS - Ottmar 2000
       &0.0,  0.1,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.3,  0.0,& !WJ - Ottmar 2000
       &0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6,& !MC - use CR AS
       &0.6,  0.7,  0.8,  2.8,  3.2,  0.0,  0.0,  0.0,  0.0,  0.3,  7.0,& !LM use LP
       &0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6,& !CW use AS
       &0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0,& !OS
       &0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6/ !OH use AS

  !     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
  !     DERIVED FROM VEG CODES IN ICITYP ARRAY IN **BLKDAT**

  data (COVINI(I), I = 1,  50) /&
       &16,16,16,16,10,10,10,10,10,10,&
       &10,10,10,10, 3, 3, 3, 3, 3, 3,&
       &3, 3, 3, 3, 3, 3, 3, 3, 3, 3,&
       &3, 3, 3, 3, 3, 3, 3, 3, 3, 3,&
       &3, 3, 3, 3, 3, 3, 3, 3, 3, 3/
  data (COVINI(I), I = 51, 100) /&
       &3, 3, 3, 3, 3, 8, 8, 8, 8, 8,&
       &4, 4, 4, 4, 4, 4, 4, 4, 4, 4,&
       &4, 4, 4, 4, 4, 9, 9, 9, 9, 9,&
       &9, 9, 9, 9, 9, 9, 9, 9, 9, 9,&
       &9, 9, 9, 9, 9, 9, 9, 9, 9, 9/
  data (COVINI(I), I = 101, 130) /&
       &9, 9, 9, 9, 9, 9, 9, 9, 9, 9,&
       &9, 9, 9, 9, 9, 9, 9, 9, 9, 9,&
       &9, 9,11,11, 7, 7, 7, 7, 7, 3/

  !     PLANT VEGETATION GROUP FOR EACH OF THE ICITYP VEG
  !     CODES (ICITYP ARRAY IN **BLKDAT**); PROVIDED BY
  !     KATHY GEIER-HAYES.

  data (MAPPVG(I), I = 1,  50) /&
       &10,10,10,10, 1, 1, 1, 1, 1, 1,&
       &1, 2, 2, 1, 1, 1, 1, 1, 1, 3,&
       &2, 2, 2, 4, 3, 3, 2, 4, 2, 2,&
       &4, 2, 4, 2, 4, 4, 2, 2, 4, 4,&
       &2, 4, 4, 4, 4, 4, 1, 1, 3, 3/
  data (MAPPVG(I), I = 51, 100) /&
       &3, 4, 4, 4, 4, 7, 7, 7, 9, 7,&
       &5, 5, 6, 6, 6, 6, 6, 6, 6, 6,&
       &5, 6, 6, 6, 6, 7, 9, 8, 8, 8,&
       &9, 9, 9, 8,10, 7, 9, 9, 9, 9,&
       &9, 8, 8, 8,10, 8, 8, 8, 7, 7/
  data (MAPPVG(I), I = 101, 130) /&
       &10,11, 7, 7, 7, 7, 7, 7, 7,10,&
       &11, 8,10, 7, 7,10,10,11,11,10,&
       &10,11,11,11,10,10,10,10,10, 4/

  !     HABITAT CODES THAT INCLUDE SNOWBERRY (PHMA) OR
  !     NINEBARK (SYAL) ARE 1; ALL OTHERS ARE 0.

  data (MAPS9B(I), I = 1,  50) /&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 1, 1, 0, 0, 0, 0, 0, 0, 0,&
       &1, 1, 1, 1, 0, 0, 1, 1, 1, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
  data (MAPS9B(I), I = 51, 100) /&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 1, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
  data (MAPS9B(I), I = 101, 130) /&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0/

  !     HABITAT MOISTURE CODES PROVIDED BY
  !     KATHY GEIER-HAYES. 0 = DRY; 1 = MESIC
  !     2 = MOIST

  data (MAPDRY(I), I = 1,  50) /&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 1, 0, 0, 0, 0, 0,&
       &1, 1, 0, 0, 1, 1, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 0, 0,&
       &0, 0, 0, 0, 0, 0, 0, 0, 1, 0/
  data (MAPDRY(I), I = 51, 100) /&
       &1, 0, 0, 0, 0, 1, 2, 1, 2, 0,&
       &1, 0, 1, 1, 1, 1, 1, 1, 1, 0,&
       &0, 1, 1, 1, 1, 1, 2, 1, 1, 1,&
       &2, 2, 2, 1, 0, 1, 2, 2, 2, 1,&
       &2, 1, 1, 1, 1, 1, 1, 1, 1, 1/
  data (MAPDRY(I), I = 101, 130) /&
       &1, 1, 1, 0, 1, 1, 1, 1, 0, 1,&
       &1, 1, 0, 0, 0, 0, 0, 0, 1, 1,&
       &1, 1, 1, 1, 1, 0, 0, 1, 0, 1/

  data MYACT / 2521, 2548, 2553 /

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCBA',5,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMCBA CYCLE = ',I2)

  !  Zero out the cumulative variables

  COVTYP = 0
  PERCOV = 0.0
  BIGDBH = 0.0
  TOTBA = 0.0

  !  Loop through the tree list

  if (ITRN.gt.0) then

     !  Zero out the cummulative variables

     BAMOST = 0.0
     TOTCRA = 0.0

     do KSP = 1,MAXSP
        FMTBA(KSP) = 0.0
     end do

     do I = 1,ITRN
        if (FMPROB(I) .gt.0.0) then

           KSP = ISP(I)

           FMTBA(KSP) = FMTBA(KSP) +&
                &FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

           if (DBH(I) .gt. BIGDBH) BIGDBH = DBH(I)

           !  Calculate the crown width of the tree and total the area
           !  encompassed by all trees

           CWIDTH = CRWDTH(I)
           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           TOTCRA = TOTCRA + CAREA*FMPROB(I)
        end if

        !         Use this loop to zero this variable, for lack of a better place.

        CURKIL(I) = 0.0
     end do

     !  Determine which species has the most basal area
     !  -> that will be the cover type

     do KSP = 1,MAXSP
        if (FMTBA(KSP) .gt. BAMOST) then
           BAMOST = FMTBA(KSP)
           COVTYP = KSP
        end if
        TOTBA = TOTBA + FMTBA(KSP)
     end do

     !  Use the crown width information to determine the percent cover
     !  of the stand. Use the equation sent by Nick which assumes that
     !  crowns are randomly distrubuted in the stand:

     !  PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))

     PERCOV = 100.0 * (1.0 - EXP(-TOTCRA/43560.))

  end if

  !  If there are no trees (COVTYP=0) in cycle 1,
  !  use the optional ICINDX if it is valid. Otherwise
  !  issue a warning and use a default LP cover. After the
  !  first cycle, use the previous cover type if no cover is
  !  present.

  if (COVTYP .eq.0) then
     if (IYR .eq. IY(1)) then
        if (ICINDX .ge.1.and. ICINDX .le. MXVCODE)&
             &COVTYP = COVINI(ICINDX)
        if (COVTYP .eq.0) then
           WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',  &
                &       'BASAL AREA AND NO VALID HABITAT INFORMATION:',            &
                &       /1X,'*** COVER TYPE SET TO DOUGLAS-FIR',/1X)")
           call RCDSET (2,.true.)
           COVTYP = 3
        end if
     else
        COVTYP = OLDCOVTYP
     end if
  end if

  OLDCOVTYP = COVTYP

  !  Load live fuels as a function of PERCOV...assume that the initiating
  !  stands correspond to about 10% cover and established are 60% or more.

  XCOV(1) = 10.
  XCOV(2) = 60.
  do I = 1,2
     YLOAD(1) = FULIVI(I,COVTYP)
     YLOAD(2) = FULIVE(I,COVTYP)
     FLIVE(I) = ALGSLP(PERCOV,XCOV,YLOAD,2)
  end do

  if (DEBUG) write(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
  8 format(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',&
       &2F6.3)

  !  Initialize the dead fuels only for the first year of the simulation

  if (IYR .eq. IY(1)) then

     !  MODIFY CWD DECAY RATE BASED ON HABITAT MOISTURE GROUP
     !       0 = DRIER - LOWER RATE
     !       1 = MESIC - UNCHANGED
     !       2 = WETTER - HIGHER RATE

     !  ONLY DO THIS IF DURING THE NORMAL CALL, NOT FROM SVSTART

     if ( ISWTCH .ne.1 ) then
        DCYMLT = 1.0
        select case (MAPDRY(ICINDX))
        case (0)
           DCYMLT = 0.66
        case (1)
           DCYMLT = 1.0
        case (2)
           DCYMLT = 1.33
        end select

        do I = 1,MXFLCL
           do J = 1,4
              if (SETDECAY(I,J) .lt.0) then
                 DKR(I,J) = DKR(I,J) * DCYMLT
                 if (I .le.10) TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
              end if
           end do
        end do
     end if

     !sng      IF (IYR .EQ. IY(1)) THEN

     !  Load dead fuels as a function of PERCOV...assume that the initiating
     !  stands correspond to about 10% cover and established are 60% or more.

     XCOV(1) = 10.
     XCOV(2) = 60.
     do ISZ = 1,MXFLCL
        YLOAD(1) = FUINII(ISZ,COVTYP)
        YLOAD(2) = FUINIE(ISZ,COVTYP)
        STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
        STFUEL(ISZ,1) = 0
     end do

     !  CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

     call OPFIND(1,MYACT(2),J)
     if (J .gt.0) then
        call OPGET(J,2,JYR,IACTK,NPRM,PRMS)
        if ((PRMS(1) .ge.0) .and. (PRMS(2) .ge.0)) then
           call FMPHOTOVAL(NINT(PRMS(1)), NINT(PRMS(2)), FOTOVAL,&
                &FOTOVALS)
           do I = 1, MXFLCL
              if (FOTOVAL(I) .ge.0) STFUEL(I,2) = FOTOVAL(I)
              if (I .le.9) STFUEL(I,1) = FOTOVALS(I)
           end do

           !  IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
           !  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
           !  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

           if (FOTOVAL(1).ge.0.and. ISWTCH.ne.1) call OPDONE(J,IYR)

        else
           WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',    &
                &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ', &
                &      'REQUIRED.',/1X)")
           call RCDSET (2,.true.)
        end if
     end if

     !  CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
     !  FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

     call OPFIND(1,MYACT(1),J)
     if (J .gt.0) then
        call OPGET(J,12,JYR,IACTK,NPRM,PRMS)
        if (PRMS(2) .ge.0) STFUEL(3,2) = PRMS(2)
        if (PRMS(3) .ge.0) STFUEL(4,2) = PRMS(3)
        if (PRMS(4) .ge.0) STFUEL(5,2) = PRMS(4)
        if (PRMS(5) .ge.0) STFUEL(6,2) = PRMS(5)
        if (PRMS(6) .ge.0) STFUEL(10,2) = PRMS(6)
        if (PRMS(7) .ge.0) STFUEL(11,2) = PRMS(7)
        if (PRMS(8) .ge.0) STFUEL(1,2) = PRMS(8)
        if (PRMS(9) .ge.0) STFUEL(2,2) = PRMS(9)
        if (PRMS(1) .ge.0) then
           if ((PRMS(8) .lt.0) .and. (PRMS(9) .lt.0)) then
              STFUEL(1,2) = PRMS(1) * 0.5
              STFUEL(2,2) = PRMS(1) * 0.5
           end if
           if ((PRMS(8) .lt.0) .and. (PRMS(9) .ge.0)) then
              STFUEL(1,2) = MAX(PRMS(1) - PRMS(9),0.)
           end if
           if ((PRMS(8) .ge.0) .and. (PRMS(9) .lt.0)) then
              STFUEL(2,2) = MAX(PRMS(1) - PRMS(8),0.)
           end if
        end if
        if (PRMS(10) .ge.0) STFUEL(7,2) = PRMS(10)
        if (PRMS(11) .ge.0) STFUEL(8,2) = PRMS(11)
        if (PRMS(12) .ge.0) STFUEL(9,2) = PRMS(12)

        !  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

        if ( ISWTCH .ne.1 ) call OPDONE(J,IYR)

     end if

     call OPFIND(1,MYACT(3),J)
     if (J .gt.0) then
        call OPGET(J,9,JYR,IACTK,NPRM,PRMS)
        if (PRMS(1) .ge.0) STFUEL(1,1) = PRMS(1)
        if (PRMS(2) .ge.0) STFUEL(2,1) = PRMS(2)
        if (PRMS(3) .ge.0) STFUEL(3,1) = PRMS(3)
        if (PRMS(4) .ge.0) STFUEL(4,1) = PRMS(4)
        if (PRMS(5) .ge.0) STFUEL(5,1) = PRMS(5)
        if (PRMS(6) .ge.0) STFUEL(6,1) = PRMS(6)
        if (PRMS(7) .ge.0) STFUEL(7,1) = PRMS(7)
        if (PRMS(8) .ge.0) STFUEL(8,1) = PRMS(8)
        if (PRMS(9) .ge.0) STFUEL(9,1) = PRMS(9)

        !  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

        if ( ISWTCH .ne.1 ) call OPDONE(J,IYR)

     end if

     !  DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
     !  OF BASAL AREA IN THE STAND.

     do ISZ = 1,MXFLCL
        if (TOTBA .gt.0.0) then
           do KSP = 1,MAXSP
              if (FMTBA(KSP) .gt.0.0) then
                 do J = 1,2
                    PRCL = FMTBA(KSP) / TOTBA
                    IDC = DKRCLS(KSP)
                    ADD = PRCL * STFUEL(ISZ,J)
                    CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + ADD
                 end do
              end if
           end do
        else
           IDC = DKRCLS(COVTYP)
           do J = 1,2
              CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + STFUEL(ISZ,J)
           end do
        end if
     end do

  end if
  return

  !  HOOK TO ALLOW THE MAPPVG() ARRAY TO BE READ BY **FMCFMD*

  entry CIPVG(PVG)
  PVG = MAPPVG(ICINDX)
  return

  !  HOOK TO ALLOW THE MAPS9B() ARRAY TO BE READ BY **FMCFMD*

  entry CIS9B(S9B)
  S9B = MAPS9B(ICINDX)
  return

  !  ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
  !  IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

  entry SNGCOE
  return

end
