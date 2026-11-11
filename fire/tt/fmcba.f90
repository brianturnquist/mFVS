!=====================!
!   fire/tt : fmcba   !
!=====================!
! FIRE-TT $Id$
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

  !  Common block variables and parameters:

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'CICOM.f90'
  include 'FMCOM.f90'

  !.... Variable declarations.

  !     MAXIMUM NUMBER OF VEGETATION CODES; MUST MATCH THE
  !     DIMENSION OF THE ICITYP ARRAY IN CI **BLKDAT**

  ! Variables
  integer :: MXVCODE
  parameter (MXVCODE = 363)

  integer*1 :: COVINI(MXVCODE)
  real    :: BAMOST,TOTCRA,CWIDTH
  real    :: FULIVE(2,MAXSP),FULIVI(2,MAXSP)
  real    :: FUINIE(MXFLCL,MAXSP),FUINII(MXFLCL,MAXSP)
  real    :: STFUEL(MXFLCL,2),XCOV(2),YLOAD(2),FOTOVAL(MXFLCL)
  real    :: PRMS(12),FOTOVALS(9)
  logical :: DEBUG

  integer :: MYACT(3)

  integer :: IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
  real    :: BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD

  ! SPECIES ORDER FOR TETONS VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH

  ! VARIANT EXPANSION:
  ! BS USES ES EQUATIONS FROM TT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! PP USES PP EQUATIONS FROM CI
  ! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! MM USES MM EQUATIONS FROM IE
  ! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM TT

  !     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER

  !     herbs, shrubs

  data FULIVE /&
       &0.2,   0.1,& !1  WB - use LP
       &0.2,   0.1,& !2  LM - use LP
       &0.2,   0.2,& !3  DF
       &0.04,  0.05,& !4  PM - Ottmar, 2000
       &0.15,   0.2,& !5  BS - use ES
       &0.25,  0.25,& !6  AS - Ottmar, 2000
       &0.2,   0.1,& !7  LP
       &0.15,   0.2,& !8  ES
       &0.15,   0.2,& !9  AF
       &0.2,  0.25,& !10 PP - use CI PP
       &0.04,  0.05,& !11 UJ - Ottmar, 2000
       &0.04,  0.05,& !12 RM - Ottmar, 2000
       &0.20,  0.20,& !13 BI - from SO BM, use CR DF
       &0.2,   0.2,& !14 MM - use IE MM
       &0.25,  0.25,& !15 NC - use aspen Ottmar, 2000
       &0.25,  0.25,& !16 MC - use aspen Ottmar, 2000
       &0.2,   0.1,& !17 OS - use LP
       &0.25,  0.25/ !18 OH - use aspen Ottmar, 2000

  !     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER

  !     herbs, shrubs

  data FULIVI /&
       &0.4,  1.0,& !1  WB - use LP
       &0.4,  1.0,& !2  LM - use LP
       &0.4,  2.0,& !3  DF
       &0.13, 1.63,& !4  PM - Ottmar, 2000
       &0.3,  2.0,& !5  BS - use ES
       &0.18, 1.32,& !6  AS - Ottmar, 2000
       &0.4,  1.0,& !7  LP
       &0.3,  2.0,& !8  ES
       &0.3,  2.0,& !9  AF
       &0.25, 0.10,& !10 PP - use CI PP
       &0.13, 1.63,& !11 UJ - Ottmar, 2000
       &0.13, 1.63,& !12 RM - Ottmar, 2000
       &0.40, 2.00,& !13 BI - from SO BM, use CR DF
       &0.4,  2.0,& !14 MM - use IE MM
       &0.18, 1.32,& !15 NC - use aspen Ottmar, 2000
       &0.18, 1.32,& !16 MC - use aspen Ottmar, 2000
       &0.4,  1.0,& !17 OS - use LP
       &0.18, 1.32/ !18 OH - use aspen Ottmar, 2000

  !     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
  !     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

  !     <.25  to1  1-3   3-6 6-12 12-20 20-35 35-50 >50  Lit  Duf

  data FUINIE /&
       &0.9, 0.9, 1.2,  7.0, 8.0,  0.0, 0.0,  0.0, 0.0, 0.6, 15.0,& !1  WB - use LP
       &0.9, 0.9, 1.2,  7.0, 8.0,  0.0, 0.0,  0.0, 0.0, 0.6, 15.0,& !2  LM - use LP
       &0.9, 0.9, 1.6,  3.5, 3.5,  0.0, 0.0,  0.0, 0.0, 0.6, 10.0,& !3  DF
       &0.2, 0.8, 2.3,  1.4, 3.0,  0.0, 0.0,  0.0, 0.0, 0.5,  0.0,& !4  PM - Ottmar, 2000
       &1.1, 1.1, 2.2, 10.0,10.0,  0.0, 0.0,  0.0, 0.0, 0.6, 30.0,& !5  BS - use ES
       &0.2, 0.6, 2.4,  3.6, 5.6,  0.0, 0.0,  0.0, 0.0, 1.4, 16.8,& !6  AS - Ottmar, 2000
       &0.9, 0.9, 1.2,  7.0, 8.0,  0.0, 0.0,  0.0, 0.0, 0.6, 15.0,& !7  LP
       &1.1, 1.1, 2.2, 10.0,10.0,  0.0, 0.0,  0.0, 0.0, 0.6, 30.0,& !8  ES
       &1.1, 1.1, 2.2, 10.0,10.0,  0.0, 0.0,  0.0, 0.0, 0.6, 30.0,& !9  AF
       &0.7, 0.7, 1.6,  2.5, 2.5,  0.0, 0.0,  0.0, 0.0, 1.4,  5.0,& !10 PP - use CI PP
       &0.2, 0.8, 2.3,  1.4, 3.0,  0.0, 0.0,  0.0, 0.0, 0.5,  0.0,& !11 UJ - Ottmar, 2000
       &0.2, 0.8, 2.3,  1.4, 3.0,  0.0, 0.0,  0.0, 0.0, 0.5,  0.0,& !12 RM - Ottmar, 2000
       &0.9, 0.9, 1.6,  3.5, 3.5,  0.0, 0.0,  0.0, 0.0, 0.6, 10.0,& !13 BI - from SO BM, use CR DF
       &0.9, 0.9, 1.6,  3.5, 3.5,  0.0, 0.0,  0.0, 0.0, 0.6, 10.0,& !14 MM - use IE MM
       &0.2, 0.6, 2.4,  3.6, 5.6,  0.0, 0.0,  0.0, 0.0, 1.4, 16.8,& !15 NC - use aspen Ottmar, 2000
       &0.2, 0.6, 2.4,  3.6, 5.6,  0.0, 0.0,  0.0, 0.0, 1.4, 16.8,& !16 MC - use aspen Ottmar, 2000
       &0.9, 0.9, 1.2,  7.0, 8.0,  0.0, 0.0,  0.0, 0.0, 0.6, 15.0,& !17 OS - use LP
       &0.2, 0.6, 2.4,  3.6, 5.6,  0.0, 0.0,  0.0, 0.0, 1.4, 16.8/ !18 OH - use aspen Ottmar, 2000

  !     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
  !     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

  !     <.25  to1  1-3   3-6 6-12 12-20 20-35 35-50 >50  Lit  Duf

  data FUINII /&
       &0.6, 0.7, 0.8,  2.8, 3.2,  0.0, 0.0,  0.0, 0.0, 0.3,  7.0,& !1  WB - use LP
       &0.6, 0.7, 0.8,  2.8, 3.2,  0.0, 0.0,  0.0, 0.0, 0.3,  7.0,& !2  LM - use LP
       &0.5, 0.5, 1.0,  1.4, 1.4,  0.0, 0.0,  0.0, 0.0, 0.3,  5.0,& !3  DF
       &0.0, 0.1, 0.0,  0.0, 0.0,  0.0, 0.0,  0.0, 0.0, 0.3,  0.0,& !4  PM - Ottmar, 2000
       &0.7, 0.7, 1.6,  4.0, 4.0,  0.0, 0.0,  0.0, 0.0, 0.3, 12.0,& !5  BS - use ES
       &0.1, 0.4, 5.0,  2.2, 2.3,  0.0, 0.0,  0.0, 0.0, 0.8,  5.6,& !6  AS - Ottmar, 2000
       &0.6, 0.7, 0.8,  2.8, 3.2,  0.0, 0.0,  0.0, 0.0, 0.3,  7.0,& !7  LP
       &0.7, 0.7, 1.6,  4.0, 4.0,  0.0, 0.0,  0.0, 0.0, 0.3, 12.0,& !8  ES
       &0.7, 0.7, 1.6,  4.0, 4.0,  0.0, 0.0,  0.0, 0.0, 0.3, 12.0,& !9  AF
       &0.1, 0.1, 0.2,  0.5, 0.5,  0.0, 0.0,  0.0, 0.0, 0.5,  0.8,& !10 PP - use CI PP
       &0.0, 0.1, 0.0,  0.0, 0.0,  0.0, 0.0,  0.0, 0.0, 0.3,  0.0,& !11 UJ - Ottmar, 2000
       &0.0, 0.1, 0.0,  0.0, 0.0,  0.0, 0.0,  0.0, 0.0, 0.3,  0.0,& !12 RM - Ottmar, 2000
       &0.5, 0.5, 1.0,  1.4, 1.4,  0.0, 0.0,  0.0, 0.0, 0.3,  5.0,& !13 BI - from SO BM, use CR DF
       &0.5, 0.5, 1.0,  1.4, 1.4,  0.0, 0.0,  0.0, 0.0, 0.3,  5.0,& !14 MM - use IE MM
       &0.1, 0.4, 5.0,  2.2, 2.3,  0.0, 0.0,  0.0, 0.0, 0.8,  5.6,& !15 NC - use aspen Ottmar, 2000
       &0.1, 0.4, 5.0,  2.2, 2.3,  0.0, 0.0,  0.0, 0.0, 0.8,  5.6,& !16 MC - use aspen Ottmar, 2000
       &0.6, 0.7, 0.8,  2.8, 3.2,  0.0, 0.0,  0.0, 0.0, 0.3,  7.0,& !17 OS - use LP
       &0.1, 0.4, 5.0,  2.2, 2.3,  0.0, 0.0,  0.0, 0.0, 0.8,  5.6/ !18 OH - use aspen Ottmar, 2000

  !     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
  !     DERIVED FROM VEG CODES IN R4HABT ARRAY IN **HABTYP**
  !     THIS IS SIMILAR TO THE MAPPING USED BY UT-FFE

  data (COVINI(I), I = 1,  50) /&
       &2,  2,  2,  2,  2,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  2,  0,  0,  0,&
       &0,  3,  0,  8,  9,  9,  9,  9,  9,  9,&
       &9,  9,  9,  9,  0,  0,  7,  7,  7,  7,&
       &0,  0,  0,  0,  0,  0,  0,  3,  3,  3/
  data (COVINI(I), I = 51, 100) /&
       &3,  0,  0,  9,  9,  9,  0,  0,  0,  0,&
       &0,  0,  0,  6,  6,  6,  6,  6,  6,  6,&
       &6,  6,  6,  6,  6,  6,  6,  6,  6,  6,&
       &6,  6,  6,  6,  6,  6,  6,  6,  6,  6,&
       &6,  6,  6,  6,  6,  6,  6,  6,  6,  6/
  data (COVINI(I), I = 101, 150) /&
       &6,  6,  6,  6,  6,  6,  6,  6,  6,  6,&
       &6,  6,  6,  6,  6,  6,  6,  3,  3,  3,&
       &3,  6,  6,  6,  6,  6,  6,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
  data (COVINI(I), I = 151, 200) /&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  9,  0/
  data (COVINI(I), I = 201, 250) /&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
  data (COVINI(I), I = 251, 300) /&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  0,  0,  0,  0,  0,  0,  0,&
       &0,  0,  0,  2,  2,  2,  2,  0,  0,  0,&
       &3,  3,  3,  3,  3,  3,  3,  3,  3,  3,&
       &3,  3,  3,  3,  3,  3,  3,  3,  3,  3/
  data (COVINI(I), I = 301, 350) /&
       &3,  3,  3,  3,  3,  3,  0,  8,  8,  8,&
       &8,  8,  8,  8,  9,  9,  9,  9,  9,  9,&
       &9,  9,  9,  9,  9,  9,  9,  9,  9,  9,&
       &9,  9,  9,  9,  9,  8,  9,  9,  9,  9,&
       &9,  9,  9,  9,  9,  9,  9,  9,  9,  9/
  data (COVINI(I), I = 351, 363) /&
       &9,  9,  9,  9,  9,  9,  0,  0,  0,  7,&
       &7,  7,  7/

  data MYACT / 2521, 2548, 2553 /

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCBA',5,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMCBA CYCLE = ',I2)

  !.... Begin routine.

  !     Zero out the cummulative variables

  COVTYP = 0
  PERCOV = 0.0
  BIGDBH = 0.0
  TOTBA = 0.0

  !     Loop through the tree list

  if (ITRN.gt.0) then

     !       Zero out the cummulative variables

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

           !           Calculate the crown width of the tree and total the area
           !           encompassed by all trees

           CWIDTH = CRWDTH(I)
           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           TOTCRA = TOTCRA + CAREA*FMPROB(I)
        end if

        !         Use this loop to zero this variable, for lack of a better place.

        CURKIL(I) = 0.0
     end do

     !        Determine which species has the most basal area
     !        -> that will be the cover type

     do KSP = 1,MAXSP
        if (FMTBA(KSP) .gt. BAMOST) then
           BAMOST = FMTBA(KSP)
           COVTYP = KSP
        end if
        TOTBA = TOTBA + FMTBA(KSP)
     end do

     !        Use the crown width information to determine the percent cover
     !        of the stand. Use the equation sent by Nick which assumes that
     !        crowns are randomly distrubuted in the stand:

     !        PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))

     PERCOV = 100.0 * (1.0 - EXP(-TOTCRA/43560.))

  end if

  !     If there are no trees (COVTYP=0) in cycle 1,
  !     use the optional ITYPE if it is valid. Otherwise
  !     issue a warning and use a default LP cover. After the
  !     first cycle, use the previous cover type if no cover is
  !     present.

  if (COVTYP .eq.0) then
     if (IYR .eq. IY(1)) then
        if (ITYPE .ge.1.and. ITYPE .le. MXVCODE)&
             &COVTYP = COVINI(ITYPE)
        if (COVTYP .eq.0) then
           WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',  &
                &       'BASAL AREA AND NO VALID HABITAT INFORMATION:',            &
                &       /1X,'*** COVER TYPE SET TO LODGEPOLE PINE',/1X)")
           call RCDSET (2,.true.)
           COVTYP = 7
        end if
     else
        COVTYP = OLDCOVTYP
     end if
  end if

  OLDCOVTYP = COVTYP

  !     Load live fuels as a function of PERCOV...assume that the initiating
  !     stands correspond to about 10% cover and established are 60% or more.

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

  !     Initialize the dead fuels only for the first year of the simulation

  if (IYR .eq. IY(1)) then

     !sng      IF (IYR .EQ. IY(1)) THEN

     !        Load dead fuels as a function of PERCOV...assume that the initiating
     !        stands correspond to about 10% cover and established are 60% or more.

     XCOV(1) = 10.
     XCOV(2) = 60.
     do ISZ = 1,MXFLCL
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

     !        CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
     !        FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

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

        !           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

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

     !        DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
     !        OF BASAL AREA IN THE STAND.

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

  entry SNGCOE

  !     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
  !     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

  return
end
