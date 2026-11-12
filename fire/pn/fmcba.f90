!=====================!
!   fire/pn : fmcba   !
!=====================!
! FIRE-PN $Id$
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
  !     COVINI  The seral cover type to be used for initiating fuels in
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

  !  Common block variables and parameters:

  !     Parameter statements.

  !     Parameter include files.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !     Common include files.
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  !     Variable declarations.

  !     MAXIMUM NUMBER OF VEGETATION CODES; MUST MATCH THE
  !     DIMENSION OF THE VEGETATION CODE ARRAY IN WS **HABTYP**

  ! Variables
  integer :: MXR6CODE
  parameter (MXR6CODE = 75)

  real    :: BAMOST,TOTCRA,CWIDTH
  integer*2 :: COVINI6(MXR6CODE),MAPFGS(MXR6CODE),MAPDRY(MXR6CODE)
  real    :: FULIVE(2,MAXSP),FULIVI(2,MAXSP)
  real    :: FUINIE(MXFLCL,MAXSP),FUINII(MXFLCL,MAXSP)
  real    :: STFUEL(MXFLCL,2),XCOV(2),YLOAD(2),FOTOVAL(MXFLCL)
  real    :: PRMS(12),FOTOVALS(9)
  logical :: DEBUG

  integer :: MYACT(3)

  integer :: IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
  integer :: ICT(MAXSP),IFGS,IWET
  real    :: BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD
  integer :: PNWMC(75),PNWMD(75),TEMP,MOIST,K
  real    :: DKRADJ(3,3,3)

  !     EACH PN HABITAT CODE MAPS TO EITHER WARM (1), MODERATE (2)
  !     OR COLD (3).  (FROM FMR6SDCY)

  data (PNWMC(I), I = 1,  50) /&
       &2, 2, 2, 3, 3, 3, 3, 3, 2, 3,&
       &3, 3, 3, 3, 3, 3, 3, 3, 3, 3,&
       &3, 3, 3, 3, 3, 2, 2, 2, 2, 2,&
       &2, 2, 2, 2, 2, 2, 2, 2, 2, 2,&
       &2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
  data (PNWMC(I), I = 51,  75) /&
       &2, 2, 2, 2, 2, 2, 2, 2, 2, 2,&
       &2, 2, 2, 2, 2, 2, 3, 2, 2, 2,&
       &2, 2, 2, 2, 2/

  !     EACH PN HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).  (FROM FMR6SDCY)

  data (PNWMD(I), I = 1,  50) /&
       &3, 3, 3, 3, 3, 3, 3, 1, 3, 2,&
       &1, 1, 2, 2, 2, 2, 2, 2, 2, 2,&
       &2, 2, 1, 2, 2, 1, 1, 2, 1, 1,&
       &2, 3, 2, 1, 2, 2, 3, 2, 3, 2,&
       &3, 2, 2, 2, 2, 2, 1, 3, 3, 1/
  data (PNWMD(I), I = 51,  75) /&
       &3, 2, 2, 2, 2, 2, 2, 1, 2, 1,&
       &1, 2, 2, 2, 2, 2, 2, 1, 1, 1,&
       &1, 1, 1, 1, 1/

  data (((DKRADJ(I,J,K), K = 1,3), J = 1,3), I = 1,3) /&
       &1.21,   2,  1.21, 1.35, 2, 1.35,   1.7,   2,  1.7,&
       &0.825, 1.3, 0.825,    1, 2,    1,  1.35,   2, 1.35,&
       &0.75,   1,  0.75, 0.75, 1, 0.75, 0.925, 1.7, 0.925/

  !     BREAKPOINTS (% CC) OF INTERPOLATION FUNCTION TO PROVIDE WEIGHTED
  !     ESTIMATE OF LIVE (EVERY TIMESTEP) AND DEAD (INITIAL) FUEL

  data XCOV / 10.0, 60.0 /

  !     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER

  !                  herbs, shrubs
  data FULIVE /&
       &0.15, 0.10,& ! 1 = Pacific silver fir - use GF
       &0.15, 0.10,& ! 2 = white fir (wf) - use GF
       &0.15, 0.10,& ! 3 = grand fir
       &0.15, 0.10,& ! 4 = subalpine fir - use GF
       &0.15, 0.10,& ! 5 = Cal red fir/Shasta red fir - use GF
       &0.30, 0.20,& ! 6 = Sitka spruce - use ES
       &0.15, 0.10,& ! 7 = noble fir - use GF
       &0.20, 0.20,& ! 8 = Alaska cedar/western larch - use RC
       &0.20, 0.20,& ! 9 = incense-cedar - use DF
       &0.30, 0.20,& !10 = Engelmann spruce
       &0.20, 0.10,& !11 = lodgepole pine
       &0.20, 0.25,& !12 = Jeffrey pine - use PP
       &0.20, 0.25,& !13 = sugar pine - use PP
       &0.15, 0.10,& !14 = western white pine
       &0.20, 0.25,& !15 = ponderosa pine
       &0.20, 0.20,& !16 = Douglas-fir
       &0.20, 0.20,& !17 = coast redwood - use DF
       &0.20, 0.20,& !18 = western redcedar
       &0.20, 0.20,& !19 = western hemlock
       &0.15, 0.10,& !20 = mountain hemlock - use GF
       &0.20, 0.20,& !21 = bigleaf  - use DF
       &0.20, 0.20,& !22 = red alder - use DF
       &0.20, 0.20,& !23 = white alder/pacific madrone - use DF
       &0.20, 0.20,& !24 = paper birch - use DF
       &0.25, 0.25,& !25 = giant chinkapin/tanoak - use QA Ottmar and others 2000b
       &0.25, 0.25,& !26 = quaking aspen - Ottmar and others 2000b
       &0.25, 0.25,& !27 = black cottonwood - use QA Ottmar and others 2000b
       &0.23, 0.22,& !28 = Or. white oak/Cal. black oak - Gambel oak Ottmar
       &0.14, 0.35,& !29 = juniper (Ottmar Volume I)
       &0.20, 0.20,& !30 = subalpine larch - use WL
       &0.20, 0.10,& !31 = whitebark pine - use LP
       &0.20, 0.10,& !32 = knobcone pine - use LP
       &0.20, 0.20,& !33 = Pacific yew - use DF
       &0.20, 0.20,& !34 = Pacific dogwood - use DF
       &0.25, 0.25,& !35 = hawthorn (r4 definition) - use QA Ottmar and others 2000b
       &0.25, 0.25,& !36 = bitter cherry - use QA Ottmar and others 2000b
       &0.25, 0.25,& !37 = willow - use QA Ottmar and others 2000b
       &0.00, 0.00,& !38 = ---
       &0.25, 0.25/ !39 = other - use QA Ottmar and others 2000b

  !     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER

  !                  herbs, shrubs
  data FULIVI /&
       &0.30, 2.00,& ! 1 = Pacific silver fir - use GF
       &0.30, 2.00,& ! 2 = white fir (wf) - use GF
       &0.30, 2.00,& ! 3 = grand fir
       &0.30, 2.00,& ! 4 = subalpine fir - use GF
       &0.30, 2.00,& ! 5 = Cal red fir/Shasta red fir - use GF
       &0.30, 2.00,& ! 6 = Sitka spruce - use ES
       &0.30, 2.00,& ! 7 = noble fir - use GF
       &0.40, 2.00,& ! 8 = Alaska cedar/western larch - use RC
       &0.40, 2.00,& ! 9 = incense-cedar - use DF
       &0.30, 2.00,& !10 = Engelmann spruce
       &0.40, 1.00,& !11 = lodgepole pine
       &0.25, 0.10,& !12 = Jeffrey pine - use PP
       &0.25, 0.10,& !13 = sugar pine - use PP
       &0.30, 2.00,& !14 = western white pine
       &0.25, 0.10,& !15 = ponderosa pine
       &0.40, 2.00,& !16 = Douglas-fir
       &0.40, 2.00,& !17 = coast redwood - use DF
       &0.40, 2.00,& !18 = western redcedar
       &0.40, 2.00,& !19 = western hemlock
       &0.30, 2.00,& !20 = mountain hemlock - use GF
       &0.40, 2.00,& !21 = bigleaf  - use DF
       &0.40, 2.00,& !22 = red alder - use DF
       &0.40, 2.00,& !23 = white alder/pacific madrone - use DF
       &0.40, 2.00,& !24 = paper birch - use DF
       &0.18, 2.00,& !25 = giant chinkapin/tanoak - use QA Ottmar and others 2000b, modified.
       &0.18, 1.32,& !26 = quaking aspen - Ottmar and others 2000b
       &0.18, 1.32,& !27 = black cottonwood - use QA Ottmar and others 2000b
       &0.55, 0.35,& !28 = Or. white oak/Cal. black oak - Gambel oak Ottmar
       &0.10, 2.06,& !29 = juniper (Ottmar Volume I)
       &0.40, 2.00,& !30 = subalpine larch - use WL
       &0.40, 1.00,& !31 = whitebark pine - use LP
       &0.40, 1.00,& !32 = knobcone pine - use LP
       &0.40, 2.00,& !33 = Pacific yew - use DF
       &0.40, 2.00,& !34 = Pacific dogwood - use DF
       &0.18, 1.32,& !35 = hawthorn (r4 definition) - use QA Ottmar and others 2000b
       &0.18, 1.32,& !36 = bitter cherry - use QA Ottmar and others 2000b
       &0.18, 1.32,& !37 = willow - use QA Ottmar and others 2000b
       &0.00, 0.00,& !38 = ---
       &0.18, 1.32/ !39 = other - use QA Ottmar and others 2000b

  !     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
  !     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

  !     <.25 to1  1-3   3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
  data FUINIE /&
       &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0,& ! 1 = Pacific silver fir
       &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0,& ! 2 = white fir (wf) - use GF
       &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0,& ! 3 = grand fir
       &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0,& ! 4 = subalpine fir
       &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0,& ! 5 = Cal red fir/Shasta red fir - use GF
       &0.7, 0.7, 3.0, 7.0, 7.0,10.0, 0.0,0.0,0.0,1.0,35.0,& ! 6 = Sitka spruce
       &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0,& ! 7 = noble fir - use GF
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& ! 8 = Alaska cedar/western larch - use RC
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& ! 9 = incense-cedar - use DF
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !10 = Engelmann spruce - use DF
       &0.9, 0.9, 1.2, 7.0, 8.0, 0.2, 0.0,0.0,0.0,0.6,30.0,& !11 = lodgepole pine
       &0.7 ,0.7, 1.6, 2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0,& !12 = Jeffrey pine - use PP
       &0.7 ,0.7, 1.6, 2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0,& !13 = sugar pine - use PP
       &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0,& !14 = western white pine
       &0.7 ,0.7, 1.6, 2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0,& !15 = ponderosa pine
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !16 = Douglas-fir
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !17 = coast redwood - use DF
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !18 = western redcedar
       &0.7, 0.7, 3.0, 7.0, 7.0,10.0, 0.0,0.0,0.0,1.0,35.0,& !19 = western hemlock
       &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0,& !20 = mountain hemlock
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !21 = bigleaf  - use DF
       &0.7, 0.7, 1.6, 2.5, 2.5, 5.0, 0.0,0.0,0.0,0.8,30.0,& !22 = red alder
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !23 = white alder/pacific madrone - use DF
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !24 = paper birch - use DF
       &0.7, 0.7, 0.8, 1.2, 1.2, 0.5, 0.0,0.0,0.0,1.4, 0.0,& !25 = giant chinkapin/tanoak
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8,& !26 = quaking aspen - Ottmar 2000b
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8,& !27 = black cottonwood - use AS
       &0.7, 0.7, 0.8, 1.2, 1.2, 0.5, 0.0,0.0,0.0,1.4, 0.0,& !28 = Or. white oak/Cal. black oak
       &0.1, 0.2, 0.4, 0.5, 0.8, 1.0, 0.0,0.0,0.0,0.1, 0.0,& !29 = juniper - Ottmar Volume I
       &0.9 ,0.9, 1.6, 3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0,& !30 = subalpine larch - use WL
       &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0,& !31 = whitebark pine
       &0.9, 0.9, 1.2, 7.0, 8.0, 0.2, 0.0,0.0,0.0,0.6,30.0,& !32 = knobcone pine - use LP
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !33 = Pacific yew - use DF
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0,& !34 = Pacific dogwood - use DF
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8,& !35 = hawthorn (r4 definition) - use AS
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8,& !36 = bitter cherry - use AS
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8,& !37 = willow - use AS
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0, 0.0,& !38 = ---
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8/ !39 = other - use AS

  !     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
  !     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

  !     <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
  data FUINII /&
       &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0,& ! 1 = Pacific silver fir
       &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0,& ! 2 = white fir (wf) - use GF
       &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0,& ! 3 = grand fir
       &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0,& ! 4 = subalpine fir
       &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0,& ! 5 = Cal red fir/Shasta red fir - use GF
       &0.5, 0.5, 2.0, 2.8, 2.8, 6.0, 0.0,0.0,0.0,0.5,12.0,& ! 6 = Sitka spruce - use ES
       &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0,& ! 7 = noble fir - use GF
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& ! 8 = Alaska cedar/western larch - use RC
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& ! 9 = incense-cedar - use DF
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !10 = Engelmann spruce
       &0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3,12.0,& !11 = lodgepole pine
       &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8,& !12 = Jeffrey pine - use PP
       &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8,& !13 = sugar pine - use PP
       &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0,& !14 = western white pine
       &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8,& !15 = ponderosa pine
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !16 = Douglas-fir
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !17 = coast redwood - use DF
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !18 = western redcedar
       &0.5, 0.5, 2.0, 2.8, 2.8, 6.0, 0.0,0.0,0.0,0.5,12.0,& !19 = western hemlock
       &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0,& !20 = mountain hemlock
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !21 = bigleaf  - use DF
       &0.1, 0.1, 0.2, 0.5, 0.5, 3.0, 0.0,0.0,0.0,0.4,12.0,& !22 = red alder
       &1.1, 1.1, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !23 = white alder/pacific madrone - use DF
       &1.1, 1.1, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !24 = paper birch - use DF
       &0.1, 0.1, 0.1, 0.2, 0.2, 0.0, 0.0,0.0,0.0,0.5, 0.0,& !25 = giant chinkapin/tanoak
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6,& !26 = quaking aspen - Ottmar 2000b
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6,& !27 = black cottonwood - use AS
       &0.1, 0.1, 0.1, 0.2, 0.2, 0.0, 0.0,0.0,0.0,0.5, 0.0,& !28 = Or. white oak/Cal. black oak
       &0.2, 0.4, 0.2, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.2, 0.0,& !29 = juniper - Ottmar Volume I
       &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0,& !30 = subalpine larch - use WL
       &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0,& !31 = whitebark pine
       &0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3,12.0,& !32 = knobcone pine - use LP
       &1.1, 1.1, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !33 = Pacific yew - use DF
       &1.1, 1.1, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0,& !34 = Pacific dogwood - use DF
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6,& !35 = hawthorn (r4 definition) - use AS
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6,& !36 = bitter cherry - use AS
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6,& !37 = willow - use AS
       &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0, 0.0,& !38 = ---
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6/ !39 = other - use AS

  !     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
  !     DERIVED FROM R6 VEG CODES AND PN **HABTYP**

  data (COVINI6(I), I = 1,  50) /&
       &16, 16, 16,  4,  4,  4,  4,  1,  1,  1,&
       &1,  1,  1,  1,  1,  1,  1,  1,  1,  1,&
       &1,  1,  1,  1,  1, 19, 19, 19, 19, 19,&
       &19, 19, 19, 19, 19, 19, 19, 19, 19, 19,&
       &19, 19, 19, 19, 19, 19, 19, 19, 19, 19/
  data (COVINI6(I), I = 51,  75) /&
       &19, 19, 19, 19, 19, 19, 19, 19, 19, 19,&
       &19, 19, 19, 19, 19, 19, 20,  6,  6,  6,&
       &6,  6,  6,  6,  6/

  !     EACH PN HABITAT CODE MAPS TO EITHER FORB (1), GRASS (2)
  !     OR SHRUB (3), FOR USE BY **FMCFMD**. THOSE WITHOUT ANY
  !     VALID MAPPING ARE SET TO 0

  data (MAPFGS(I), I = 1,  50) /&
       &3, 3, 3, 3, 3, 3, 3, 1, 1, 1,&
       &1, 1, 0, 3, 3, 3, 3, 3, 3, 3,&
       &3, 3, 3, 3, 3, 1, 1, 1, 1, 1,&
       &3, 1, 0, 1, 3, 3, 3, 3, 3, 3,&
       &3, 3, 3, 3, 3, 3, 3, 3, 3, 1/
  data (MAPFGS(I), I = 51,  75) /&
       &3, 3, 3, 3, 3, 3, 3, 3, 3, 3,&
       &3, 3, 3, 3, 3, 3, 3, 1, 1, 1,&
       &3, 3, 3, 3, 3/

  !     INDEXING BASED ON ITYPE; ELEMENTS MUST MATCH ONE OF
  !     TYPES FOUND IN **HABTYP**.
  !     0 -> DRY HABITAT
  !     1 -> MESIC HABITAT
  !     2 -> MOIST HABITAT
  !     MAPPING PROVIDED BY TOM DEMEO (ECOLOGIST, USDA FOR SERV,
  !     PNW, PORTLAND) AND KIM MELLEN (DITTO)

  data (MAPDRY(I), I = 1,  50) /&
       &0, 0, 0, 0, 0, 0, 0, 2, 0, 0,&
       &1, 2, 1, 1, 0, 1, 1, 0, 1, 2,&
       &1, 1, 2, 0, 1, 2, 2, 1, 2, 2,&
       &2, 0, 1, 2, 1, 1, 0, 1, 0, 0,&
       &0, 2, 1, 1, 1, 1, 2, 0, 0, 2/
  data (MAPDRY(I), I = 51,  75) /&
       &0, 1, 0, 0, 0, 2, 1, 2, 1, 2,&
       &2, 1, 1, 0, 2, 2, 2, 2, 2, 2,&
       &2, 2, 2, 2, 2/

  data MYACT / 2521, 2548, 2553 /

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCBA',5,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMCBA CYCLE = ',I2)

  !     OP SHARES THESE TABLES; ITS SPECIES 24 IS TANOAK (USE QA OTTMAR AND
  !     OTHERS 2000B FOR LIVE FUELS, GC FOR INITIAL FUELS) INSTEAD OF PN
  !     PAPER BIRCH (USE DF)

  if (VARACD .eq. 'OP') then
     FULIVE(:,24) = (/0.25, 0.25/)
     FULIVI(:,24) = (/0.18, 2.00/)
     FUINIE(:,24) = (/0.7, 0.7, 0.8, 1.2, 1.2, 0.5, 0.0,0.0,0.0,1.4, 0.0/)
     FUINII(:,24) = (/0.1, 0.1, 0.1, 0.2, 0.2, 0.0, 0.0,0.0,0.0,0.5, 0.0/)
  end if

  !     BEGIN ROUTINE. ZERO OUT THE CUMMULATIVE VARIABLES

  COVTYP = 0

  !     CA VARIANTS USE TOP 2 SPP FOR LIVE FUELS AND INITIAL
  !     CWD INTERPOLATION; %CC CHANGE ALSO REQUIRES SAVING
  !     OLD PERCOV

  PERCOV = 0.0
  BIGDBH = 0.0
  TOTBA = 0.0

  !     LOOP THROUGH THE TREE LIST

  if (ITRN .gt.0) then

     !       ZERO OUT THE CUMMULATIVE VARIABLES

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

           !           CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
           !           ENCOMPASSED BY ALL TREES

           CWIDTH = CRWDTH(I)

           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           TOTCRA = TOTCRA + CAREA*FMPROB(I)
        end if

        !         USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.

        CURKIL(I) = 0.0

     end do

     !       CALCULATE TOTAL COVER TYPE

     do KSP = 1,MAXSP
        TOTBA = TOTBA + FMTBA(KSP)
     end do

     !       DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
     !       COVTYP    - FFE-WIDE DOMINANT
     !       COVCA(2)  - CA-FFE VARIANTS ONLY: PRIMARY & SECONDARY BA

     call RDPSRT(MAXSP,FMTBA,ICT,.true.)
     if (FMTBA(ICT(1)) .gt.0.001) COVTYP = ICT(1)

     !       Use the crown width information to determine the percent cover
     !       of the stand. Use the equation sent by Nick which assumes that
     !       crowns are randomly distrubuted in the stand:

     !       PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))

     PERCOV = 100. * (1.0 - EXP(-TOTCRA/43560.))

  end if

  !     IF THERE ARE NO TREES (COVTYP=0) IN CYCLE 1,
  !     USE THE OPTIONAL ITYPE IF IT IS VALID. OTHERWISE
  !     ISSUE A WARNING AND USE A DEFAULT LP COVER. AFTER THE
  !     FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
  !     PRESENT.

  if (COVTYP .eq.0) then
     if (IYR .eq. IY(1)) then

        if (ITYPE .ge.1.and. ITYPE .le. MXR6CODE)&
             &COVTYP = COVINI6(ITYPE) ! R6 HABITAT TYPE

        if (COVTYP .eq.0) then
           WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',  &
                &       'BASAL AREA AND NO VALID HABITAT INFORMATION:',            &
                &       /1X,'*** COVER TYPE SET TO DOUGLAS-FIR',/1X)")
           call RCDSET (2,.true.)
           COVTYP = 16
        end if
     else
        COVTYP = OLDCOVTYP
     end if

  end if

  OLDCOVTYP = COVTYP

  !     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
  !     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
  !       IN WS,NC,CA VARIANTS, THE TOP 2 SPECIES ARE USED TO INTIALIZE THE POOLS

  do I = 1,2 ! herbs, shrub loop
     YLOAD(1) = FULIVI(I,COVTYP)
     YLOAD(2) = FULIVE(I,COVTYP)
     FLIVE(I) = ALGSLP(PERCOV,XCOV,YLOAD,2)
  end do

  if (DEBUG) write(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
  8 format(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',&
       &2F6.3)

  !     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION

  if (IYR .eq. IY(1)) then

     TEMP = PNWMC(ITYPE)
     MOIST = PNWMD(ITYPE)

     do I = 1,9
        do J = 1,4
           if (I .le.3) then
              K = 1
           else if (I .le.5) then
              K = 2
           else
              K = 3
           end if
           !       adjust the decay rates only if the user hasn't reset them with FuelDcay
           !       also, adjust the decay rates if smaller wood is decaying more slowly than larger wood.
           !       in this case, bump up the decay rate of the smaller wood to that of the larger wood.
           if ((SETDECAY(I,J) .lt.0) .and. (ISWTCH .ne.1)) then
              DKR(I,J) = DKR(I,J)*DKRADJ(TEMP,MOIST,K)
              if (DKR(I,J) .gt.1.0) DKR(I,J) = 1.0
              TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
           end if
        end do
     end do

     do I = 9,2,-1
        do J = 1,4
           if (((DKR(I,J)-DKR(I-1,J)) .gt.0).and.(ISWTCH.ne.1)) then
              if (SETDECAY(I-1,J) .lt.0) then
                 DKR(I-1,J) = DKR(I,J)
                 TODUFF(I-1,J) = DKR(I-1,J) * PRDUFF(I-1,J)
              end if
           end if
        end do
     end do

     !       LOAD DEAD FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
     !       STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
     !       IN WS,NC,CA VARIANTS, THE TOP 2 SPECIES ARE USED TO INTIALIZE THE POOLS

     do ISZ = 1,MXFLCL ! CWD size category loop
        YLOAD(1) = FUINII(ISZ,COVTYP)
        YLOAD(2) = FUINIE(ISZ,COVTYP)
        STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
        STFUEL(ISZ,1) = 0
     end do

     !       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

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

           !           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
           !           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
           !           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

           if (FOTOVAL(1).ge.0.and. ISWTCH.ne.1) call OPDONE(J,IYR)

        else
           WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',    &
                &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ', &
                &      'REQUIRED.',/1X)")
           call RCDSET (2,.true.)
        end if
     end if

     !       CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
     !       FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

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

        !         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

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

        !         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

        if ( ISWTCH .ne.1 ) call OPDONE(J,IYR)

     end if

     !       DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
     !       OF BASAL AREA IN THE STAND. ASSUME THE FUELS ARE UNPILED AND HARD.
     !       THIS LAST ASSUMPTION MAY CHANGE IN THE FUTURE.

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

  !     HOOK TO ALLOW THE MAPFGS() ARRAY TO BE READ BY **FMCFMD*

  entry PNFGS(IFGS)
  IFGS = MAPFGS(ITYPE)
  return

  !     HOOK TO ALLOW THE MAPWET() ARRAY TO BE READ BY **FMCFMD*

  entry PNWET(IWET)
  IWET = MAPDRY(ITYPE)
  return

  entry SNGCOE

  !     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
  !     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

  return
end
