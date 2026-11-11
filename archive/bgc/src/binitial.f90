!================================!
!   archive/bgc/src : binitial   !
!================================!
subroutine BINITIAL

  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'

  !      CALL BETAS    ! NOW INITIALIZED IN BGCINIT.F
  !      CALL SOILH2O  ! now initialized in bgcinit.f
  print *,'in binitial'
  call HABTYPE !Commented out 10/00 ajm.  Variables set in HABTYPE
  !      used in BHTGROWTH, which is no longer used!
  call BIOMASS
  call SP2INT
  return
end

subroutine BIOMASS

  ! THIS SUBROUTINE CONVERTS TREE DIMENSIONS TO STEM VOLUME, THEN TO
  ! DRY BIOMASS USING CONVERSION OF 1 M3 WOOD=500 KG BIOMASS (1 CUFT=14.16
  ! KG BIOMASS). BIOMASS IN LEAVES, BRANCHES, AND ROOTS ARE ESTIMATED
  ! BY APPLYING RATIOS OF LEAF/STEM, BRANCHES/STEM, AND ROOT/STEM DERIVED
  ! FROM PUBLISHED BIOMASS EQUATIONS.

  ! GRASSES AND SHRUBS USE PUBLISHED BIOMASS EQUATIONS

  ! BIOMASS IS CONVERTED TO CARBON USING A CONVERSION =.5 KG C/1 KG BIOMASS

  ! VOLUME EQUATIONS
  ! SOURCES: UNPUBLISHED CHAMPION INTERNATIONAL WESTERN OPERATION

  ! BIOMASS EQUATIONS
  ! SOURCES: DF-GOWER,VOGT,GRIER. 1992. metric units
  !          WL-GOWER,GRIER,VOGT,VOGT. 1987. metric units
  !          PP-GHOLZ,GRIER,CAMPBELL,BROWN. 1979. metric
  !          LP-AS ABOVE
  !          SHRUB-OLSON,MARTIN. 1981. (Pachistima). metric
  !          GRASS-AS ABOVE (pine grass)

  !           SLA or B1(13,x)-RUNNING,HUNT. 1993.

  !   Revised 11/12/02.  Removing index ISTND, and removing PPE common "includes"
  !                      (PPEPRM, PPCNTL, & PRGPRM).  AJM
  !           These changes--also made in BGCFVS, BGCGROW, BGCINT, BGCGO,
  !           BGCIN, and BGCCOM.f77--remove all PPE funtionality.
  !           The FVS-BGC code is now, once again, a single stand model.

  ! Variables
  real    :: STEMB,STEMW,BRANCH,TWIG,VOL,ENGD,ENGH
  character(len=4) :: SPEC
  ! Common blocks
  include 'BGCCOM.f90'
  include 'ENTITY.f90'
  include 'SITE.f90'
  !      INCLUDE 'PPCNTL.F77'  !Added for index ISTND in IBCYC (now dimensioned
  !                            by MXSTND).  ajm 8/29/00. !! REMOVED 11/02 ajm
  !      INCLUDE 'PRGPRM.F77'                             ! DITTO.

  HTLIMIT = 1.3 !*!  1.3m = 4.5 ft (the pt at which sm trees become lg trees)

  ! IF STAND-BGC HAS ALREADY BEEN INITIALIZED, SKIP THIS SUBROUTINE UNLESS NEW
  ! ENTITIES HAVE BEEN ADDED BY FVS - FROM THE REGEN MODEL FOR EXAMPLE.
  ! IBCYC IS SET IN BGCGROW, NB IS SET IN BENTYLOAD, BENTYUPDT AND BKILL.
  ! NLF IS SET IN BKILL.

  !      IF(IBCYC(ISTND).EQ.1 .AND. NB.GT.NLF) THEN    !removed 11/02 ajm
  if (IBCYC.eq.1.and. NB.gt.NLF) then
     I1 = NLF+1
     I2 = NB
     !      ELSE IF(IBCYC(ISTND).EQ.0) THEN               !removed 11/02 ajm
  else if (IBCYC.eq.0) then
     I1 = 1
     I2 = NB
     stressdays = 0.0 !*!
     sumla = 0.0 !*!
     sumla_gr = 0.0 !*!
     sumla_sm = 0.0 !*!
     sumla_lg = 0.0 !*!
     sumla_sh = 0.0 !added 10/00 ajm
     ! NEW VARIABLES FOR NEW METHOD OF WATER BUCKET CALCULATION.
     ! NOW, USE ROOT BIOMASS INSTEAD OF LEAF AREA
     SUMRT = 0.0
     SUMRT_GR = 0.0
     SUMRT_SH = 0.0
     SUMRT_SM = 0.0
     SUMRT_LG = 0.0

  else
     goto 20
  end if

  do I = I1,I2
     if (ID(I).eq.'T') then

        ! For small trees(dbh < 2.0 inches), use formula for a parabaloid to calculate
        ! volume and then convert to carbon. Set root and foliar carbon
        ! approximately=stem C. Basal area for small trees is calculated
        ! from basal diameter.

        if (D(I).lt.5.08) then
           V = BA(I)*H(I)/2.0 ! parabolic volume in cubic meters inside bark.
           !            BOLE=V*500.            ! VOLUME TO BIOMASS
           !            DOB=5.08                ! REFERENCE DIAMETER (CM) FOR BIOMASS RATIOS
           V2 = V*500.*B2(18) ! 500 converts volume (m3) to biomass (KgC). B2(18) to kgC.
           STEM(I) = V2*(1+B2(22)) ! Increase by 25% for branches, twigs, and bark.
           ! For trees with height less than or equal to 4.5 feet (1.3m) assign leaf carbon based
           ! on estimated proportion. Otherwise use Melinda Moeur's foliar biomass equation. Convert
           ! lbs to Kg using .4535923.
           if (H(I).le.1.3) then
              LEAF(I) = (.36/.28)*STEM(I)
           else
              LEAF(I) = EXP( -2.05828 + .4035*LOG(H(I)*3.3) +&
                   &1.25837*LOG(H(I)*3.3*CR(I)) -&
                   &.12975*LOG(TPH(I)/2.47) ) * .4535923 * B2(18)
           end if

           !  Implementing new model.  Initializing root biomass to 2*leaf biomass (as inferred
           !  from Koch 1987). AJM 1/12/01  This also for some--not all--large trees, below.

           !              ROOT(I)=LEAF(I)
           ROOT(I) = 2.0 * LEAF(I)

           ! For trees with DBH >= 2 inches use CIC equations to get stem volume, then
           ! estimate leaf, branch and root biomass using ratios from biomass equations.
           ! Convert DBH from cm to inches and HT to feet for the CIC equations. Convert
           ! calculated bole volume in cubic feet to biomass using 14.16 Kg per cuft.
           ! Volume equation defaults to DF.
        else if (D(I).ge.5.08) then
           ENGD = D(I)/2.54
           ENGH = H(I)*3.28
           !            SPEC='LP'     !SPP(I) commented out 10/16/00 ajm
           call BCVTS(SPEC,ENGD,ENGH,VOL)
           BOLE = VOL*14.16
           DOB = D(I)
           !          END IF

           !============== DOUGLAS-FIR ===========================================
           if (SPEC.eq.'DF') then
              !            IF (SPP(I).EQ.'DF') THEN
              ! Compute biomass ratios
              STEMW = 10**(-1.024 + 2.416*LOG10(DOB)) ! Stemwood biomass eq
              RLF2S = 10**(-2.347 + 2.478*LOG10(DOB)) / STEMW ! Leaf/Stemwood biomass
              RBR2S = 10**(-2.336 + 2.650*LOG10(DOB)) / STEMW ! Branch/stemwood biomass
              RTW2S = 10**(-4.108 + 2.749*LOG10(DOB)) / STEMW ! Twig/stemwood biomass
              RBK2S = 10**(-1.234 + 2.128*LOG10(DOB)) / STEMW ! Bark/stemwood biomass
              ! Compute biomass in plant parts and convert to carbon using B2(18)
              BARK = BOLE*RBK2S
              BRANCH = BOLE*RBR2S
              TWIG = BOLE*RTW2S
              ! Calculate leaf carbon
              LEAF(I) = BOLE*RLF2S*B2(18)
              ! Calculate total stem carbon
              STEM(I) = (BOLE+BARK+BRANCH+TWIG)*B2(18)
              ! Set root carbon = leaf carbon
              ROOT(I) = LEAF(I)
              !=============== WESTERN LARCH ========================================
           else if (SPEC.eq.'WL') then
              !           ELSE IF (SPP(I).EQ.'WL') THEN
              ! Compute biomass ratios
              STEMW = 10**(-1.158 + 2.460*LOG10(DOB)) ! Stemwood biomass eq
              RLF2S = 10**(-2.779 + 2.499*LOG10(DOB)) / STEMW ! Leaf/Stemwood biomass
              RBR2S = 10**(-2.484 + 2.648*LOG10(DOB)) / STEMW ! Branch/stemwood biomass
              RTW2S = 10**(-3.754 + 2.166*LOG10(DOB)) / STEMW ! Twig/stemwood biomass
              RBK2S = 10**(-1.322 + 2.020*LOG10(DOB)) / STEMW ! Bark/stemwood biomass
              ! Compute biomass in plant parts and convert to carbon using B2(18)
              BARK = BOLE*RBK2S
              BRANCH = BOLE*RBR2S
              TWIG = BOLE*RTW2S
              ! Calculate leaf carbon
              LEAF(I) = BOLE*RLF2S*B2(18)
              ! Calculate total stem carbon
              STEM(I) = (BOLE+BARK+BRANCH+TWIG)*B2(18)
              ! Set root carbon = leaf carbon
              ROOT(I) = LEAF(I)
              !=============== PONDEROSA PINE =======================================
           else if (SPEC.eq.'PP') then
              !           ELSE IF (SPP(I).EQ.'PP') THEN
              ! Compute biomass ratios
              STEMW = EXP(-4.4907 + 2.7587*LOG(DOB)) ! Stemwood biomass eq
              RLF2S = EXP(-4.2612 + 2.0967*LOG(DOB)) / STEMW ! Leaf/Stemwood biomass
              RBR2S = EXP(-5.3855 + 2.7185*LOG(DOB)) / STEMW ! Branch/stemwood biomass
              RBK2S = EXP(-4.2063 + 2.2312*LOG(DOB)) / STEMW ! Bark/stemwood biomass
              ! Compute biomass in plant parts and convert to carbon using B2(18)
              BARK = BOLE*RBK2S
              BRANCH = BOLE*RBR2S
              ! Calculate leaf carbon
              LEAF(I) = BOLE*RLF2S*B2(18)
              ! Calculate total stem carbon
              STEM(I) = (BOLE+BARK+BRANCH)*B2(18)
              ! Set root carbon = leaf carbon
              ! *****Changing to 2:1.  AJM 1/12/01******
              !         ROOT(I)=LEAF(I)
              ROOT(I) = 2.0 * LEAF(I)
              !============== LODGEPOLE PINE ========================================
           else if (SPEC.eq.'LP') then
              !           ELSE IF (SPP(I).EQ.'LP') THEN
              ! Compute biomass ratios
              STEMW = EXP(-2.9849 + 2.4287*LOG(DOB)) ! Stemwood biomass eq
              STEMDF = 10**(-1.024 + 2.416*LOG10(DOB)) ! DF Stemwood biomass eq
              RLF2S = EXP(-3.6187 + 1.8362*LOG(DOB)) / STEMW ! Leaf/Stemwood biomass
              RBR2S = EXP(-4.6004 + 2.3533*LOG(DOB)) / STEMW ! Branch/stemwood biomass
              RBK2S = 10**(-1.234 + 2.128*LOG10(DOB)) / STEMDF ! Bark/stemwood biomass from DF
              ! Compute biomass in plant parts and convert to carbon using B2(18)
              BRANCH = BOLE*RBR2S
              BARK = BOLE*RBK2S
              ! Calculate leaf carbon
              LEAF(I) = BOLE*RLF2S*B2(18)
              ! Calculate total stem carbon
              STEM(I) = (BOLE+BRANCH+BARK)*B2(18)
              ! Set root carbon = leaf carbon
              ! *****Changing to 2:1.  AJM 1/12/01******
              !         ROOT(I)=LEAF(I)
              ROOT(I) = 2.0 * LEAF(I)
              !============== ALL OTHER SPECIES ======================================
           else !use DF biomass for all other species
              ! Compute biomass ratios
              STEMW = 10**(-1.024 + 2.416*LOG10(DOB)) ! Stemwood biomass eq
              RLF2S = 10**(-2.347 + 2.478*LOG10(DOB)) / STEMW ! Leaf/Stemwood biomass
              RBR2S = 10**(-2.336 + 2.650*LOG10(DOB)) / STEMW ! Branch/stemwood biomass
              RTW2S = 10**(-4.108 + 2.749*LOG10(DOB)) / STEMW ! Twig/stemwood biomass
              RBK2S = 10**(-1.234 + 2.128*LOG10(DOB)) / STEMW ! Bark/stemwood biomass
              ! Compute biomass in plant parts and convert to carbon using B2(18)
              BARK = BOLE*RBK2S
              BRANCH = BOLE*RBR2S
              TWIG = BOLE*RTW2S
              ! Calculate leaf carbon
              LEAF(I) = BOLE*RLF2S*B2(18)
              ! Calculate total stem carbon
              STEM(I) = (BOLE+BARK+BRANCH+TWIG)*B2(18)
              ! Set root carbon = leaf carbon
              ! *****Changing to 2:1.  AJM 1/12/01******
              !         ROOT(I)=LEAF(I)
              ROOT(I) = 2.0 * LEAF(I)
           end if
        end if
        !         EXPAND(I)=TPH(I)
        LA(I) = LEAF(I) * B1(13,1)
     else if (ID(I).eq.'S') then
        !  original (in Olson & Martin, 1981) H in cm, PCOVER in %, and
        !  biomass in g/.5m2. H is converted to m & biomass to kgC/ha.
        !  Note: the 1/500 & AREA convert g/.5m2 to kg/ha.
        !      : H(I)*100 converts height in meters to cm.
        !      : the 0.? divides above-ground biomass into leaf & stem (??:??)
        STEM(I) = (1.66075 + 0.06348*PCOVER(I)*(H(I)*100.))&
             &* (1./500.) * AREA * 0.7 * B2(18)
        LEAF(I) = (1.66075 + 0.06348*PCOVER(I)*(H(I)*100.))&
             &* (1./500.) * AREA * 0.3 * B2(18)
        ROOT(I) = LEAF(I)
        EXPAND(I) = 1.
        LA(I) = LEAF(I) * B1(13,2)
     else if (ID(I).eq.'G') then
        LEAF(I) = (0.78009 + 0.25822*PCOVER(I))&
             &* (1./500.) * AREA * B2(18)
        ROOT(I) = LEAF(I)
        STEM(I) = 0.0
        EXPAND(I) = 1.
        LA(I) = LEAF(I) * B1(13,3)
     end if

     !  added line for shrubs below.  ajm 10/00
     SUMLA = SUMLA + LA(I)*EXPAND(I)
     SUMRT = SUMRT + ROOT(I)*EXPAND(I)
     if (ID(I).eq.'G') then !*!
        SUMLA_GR = SUMLA_GR + LA(I)*EXPAND(I) !*!
        SUMRT_GR = SUMRT_GR + ROOT(I)*EXPAND(I)
     else if (ID(I).eq.'S') then ! added 10/00 ajm
        SUMLA_SH = SUMLA_SH + LA(I)*EXPAND(I)
        SUMRT_SH = SUMRT_SH + ROOT(I)*EXPAND(I)
     else if ((ID(I).eq.'T') .and. (H(I).lt.HTLIMIT)) then !*!
        SUMLA_SM = SUMLA_SM + LA(I)*EXPAND(I) !*!
        SUMRT_SM = SUMRT_SM + ROOT(I)*EXPAND(I)
     else !*!  added 12/97
        SUMLA_LG = SUMLA_LG + LA(I)*EXPAND(I) !*!
        SUMRT_LG = SUMRT_LG + ROOT(I)*EXPAND(I)
     end if !*!
     10 continue
  end do
  20 return
end

subroutine SP2INT

  ! THIS SUBROUTINE CONVERTS THE CHARACTER VARIABLE, "SPP", TO
  ! AN INTEGER VARIABLE, "ISP". ALSO, IT CONVERTS THE CHARACTER
  ! VARIABLE "ID" TO AN INTEGER VARIABLE "IB".

  ! Common blocks
  include 'ENTITY.f90'
  print *, 'in sp2int'
  do I = 1,NB
     if (SPP(I).eq.'PP') then
        BGCISP(I) = 1
     else if (SPP(I).eq.'DF') then
        BGCISP(I) = 2
     else if (SPP(I).eq.'WL') then
        BGCISP(I) = 3
     else if (SPP(I).eq.'ES') then
        BGCISP(I) = 4
     else if (SPP(I).eq.'LP') then
        BGCISP(I) = 5
     else if (SPP(I).eq.'AF') then
        BGCISP(I) = 6
     else if (SPP(I).eq.'GF') then
        BGCISP(I) = 7
     else if (SPP(I).eq.'BP') then
        BGCISP(I) = 8
     else if (SPP(I).eq.'WP') then
        BGCISP(I) = 9
     else if (SPP(I).eq.'RC') then
        BGCISP(I) = 10
     else if (SPP(I).eq.'WH') then
        BGCISP(I) = 11
     else if (SPP(I).eq.'SH') then !If BGC gets compiled with variants having shasta red fir
        BGCISP(I) = 12 !this will need to be changed. (Its code is 'SH') ajm 10/00
     else if (SPP(I).eq.'GR') then
        BGCISP(I) = 13

        !...The following are temporay "fixes" for CR-variant species.  Adding species
        !   Equating white and corkbark fir with grand fir; blue spruce w/ ES
        !   mtn hemlock with western hemlock; bristlecone, liber, pinyon, sw white,
        !   and juniper  w/ whitebark pine.  The BGCISP codes (in BGC) used only for crown
        !   width and CCF calculations.
        !   Now, all gymnosperms are included from ALL western variants,
        !   EXCEPT: SO, NC, CA, AK, PN, WS, WC
        !   (That is, all conifers in the following are accounted for:
        !   BM, CI, CR, EC, EM, KT, NI, TT, UT)

     else if (SPP(I) .eq. 'BS') then
        BGCISP(I) = 4
     else if (SPP(I) .eq. 'CB') then
        BGCISP(I) = 7
     else if (SPP(I) .eq. 'WF') then
        BGCISP(I) = 7
     else if (SPP(I) .eq. 'MH') then
        BGCISP(I) = 11
     else if (SPP(I) .eq. 'BC') then
        BGCISP(I) = 8
     else if (SPP(I) .eq. 'WB') then !Same as BP, above!
        BGCISP(I) = 8
     else if (SPP(I) .eq. 'LM') then
        BGCISP(I) = 8
     else if (SPP(I) .eq. 'PI') then
        BGCISP(I) = 8
     else if (SPP(I) .eq. 'JU') then
        BGCISP(I) = 8
     else if (SPP(I) .eq. 'WS') then
        BGCISP(I) = 4
     else if (SPP(I) .eq. 'SF') then
        BGCISP(I) = 7
     else if (SPP(I) .eq. 'L') then ! Same as WL
        BGCISP(I) = 3
     else if (SPP(I) .eq. 'S') then ! Same as ES
        BGCISP(I) = 4
     else if (SPP(I) .eq. 'C') then ! Same as RC
        BGCISP(I) = 10

     end if

     if (ID(I).eq.'T') then
        IBLB(I) = 1
     else if (ID(I).eq.'S') then
        IBLB(I) = 2
     else if (ID(I).eq.'G') then
        IBLB(I) = 3
     end if
     10 continue
  end do
  return
end

  !     SUBROUTINE SOILH2O ! moved to bgcinit.f

  !  calculates maximum volumetric soil water content from sand, silt
  !  and clay fractions.  Maximum volumetric content can also be read
  !  in input. Called once for a site.
  !    PSAND=percent sand.     S(12) in BIOME-BGC
  !    PSILT=percent silt.     S(13) in BIOME-BGC (NOT USED)
  !    PCLAY=percent clay.     S(14) in BIOME-BGC
  !    VOLMAX=maximum volumetric water content. S(3) in BIOME=BGC
  !    AA and BB are texture coefficients

  !      REAL PSAND, PCLAY
  !      INCLUDE 'SITE.F77'

  !      PSAND=S(12)
  !      PCLAY=S(14)
  !      VOLMAX=S(3)
  !  calculate texture coefficients
  !      AA=100.*EXP(-4.396-0.0715*PCLAY-4.880E-4*PSAND**2.
  !     &           -4.285E-5*PSAND**2.*PCLAY)
  !      BB=-3.14-0.00222*PCLAY**2.-3.484E-5*PSAND**2.*PCLAY
  !  calculate maximum volumetric water content if not given in input.

  !      IF(VOLMAX.EQ.0.0) VOLMAX=EXP((LOG(33.)-AA)/BB)
  !      WRITE(*,*) 'WATER: ',AA,BB,VOLMAX
  !      RETURN
  !      END

  !     SUBROUTINE BETAS

  ! This subroutine reads the B() & S() variables
  !    The B1() variables are lifeform specific.
  !    The B2() variables cover all lifeforms.
  !    The S() variables represent site characteristics.

  !      INCLUDE 'SITE.F77'
  !      OPEN(61,FILE='BETA.DAT',STATUS='OLD')
  !      OPEN(62,FILE='SITE.DAT',STATUS='OLD')
  !      READ(61,10) ((B1(I,J),J=1,3),I=1,13)
  !   10 FORMAT(3(F8.4,1X))
  !      READ(61,'(F8.0)') (B2(K),K=1,22)
  !      READ(62,'(F8.0)') (S(L),L=1,15)

  !     DO 20 K=1,22
  !        WRITE(*,100) (B2(K),K=1,22)
  !  100   FORMAT (F8.4)
  !  20 CONTINUE
  !      RETURN
  !     END

subroutine HABTYPE

  !   THIS SUBROUTINE SETS HABITAT TYPE AND NATIONAL FOREST LOCATION
  !   FLAGS FOR THE PROGNOSIS-BASED HEIGHT GROWTH SUBROUTINE.
  !      -- DWC, 1/24/94.

  ! Common blocks
  include 'SITE.f90'
  print *, 'in habtype'
  if (HTYPE.eq.250.or. HTYPE.eq.260.or. HTYPE.eq.280.or.&
       &HTYPE.eq.290.or. HTYPE.eq.310.or. HTYPE.eq.320.or.&
       &HTYPE.eq.330) then
     IHAB1 = 1
     IHAB2 = 3
  else if (HTYPE.eq.690.or. HTYPE.eq.710.or.&
       &HTYPE.eq.720) then
     IHAB1 = 2
     IHAB2 = 3
  else if (HTYPE.eq.130.or. HTYPE.eq.170.or. HTYPE.eq.660.or.&
       &HTYPE.eq.730.or. HTYPE.eq.830.or. HTYPE.eq.850.or.&
       &HTYPE.eq.999) then
     IHAB1 = 3
     IHAB2 = 3
  else if (HTYPE.eq.420.or. HTYPE.eq.470) then
     IHAB1 = 4
     IHAB2 = 3
  else if (HTYPE.eq.510.or. HTYPE.eq.620.or. HTYPE.eq.640.or.&
       &HTYPE.eq.670.or. HTYPE.eq.680) then
     IHAB1 = 5
     IHAB2 = 3
  else if (HTYPE.eq.520) then
     IHAB1 = 6
     IHAB2 = 3
  else if (HTYPE.eq.530) then
     IHAB1 = 7
     IHAB2 = 1
  else if (HTYPE.eq.550.or. HTYPE.eq.570.or. HTYPE.eq.610) then
     IHAB1 = 8
     IHAB2 = 2
  else if (HTYPE.eq.540) then
     IHAB1 = 8
     IHAB2 = 3
  end if

  if (NFLOC.eq.5.or. NFLOC.eq.17) then
     ILOC = 1
  else if (NFLOC.eq.6.or. NFLOC.eq.18) then
     ILOC = 2
  else
     ILOC = 3
  end if

  return
end
