!===============================!
!   archive/bgc/src : bupdate   !
!===============================!
subroutine BUPDATE(I,YEAR)

  !   THIS SUBROUTINE UPDATES THE CARBON POOLS & TREE DIMENSIONS DURING
  !   THE YEAR, BASED ON THE "FLAG" VARIABLE SETTING IN GSV.FOR.
  !      -- DWC, 8/11/93, 1/4/94, 8/23-24/94.
  !      -- KSM 7/7/95
  !      -- modified to handle trees < 1.3m in height. KSM 3/2/96
  !      --Added a new output file for Waring Vigor Index.  This index is written from here, because
  !        it is in this subroutine where the new stem biomass increment is calculated.  Leaf area is
  !        a common block variable, so it is available here.  AJM 2/6/01.
  !      --ADDING NEW LOGIC PARALLELING LOGIC IN BENTYUPDATE AND BENTYLOAD.
  !        New formula for shape of bole.  As with volume, assume parabola.
  !        We calculated parabola's "a" parameter in BENTYLOAD.  So now we can
  !        backcalculate a DBH from the BD...points on the same parabola.
  !        Heretofore, we assumed triangles for backcalculation of DBH from BD.
  !       Amended 1/04 ajm.  adding FBOLE and stemB*FBOLE to vigor output file.
  !       Waring Vigor's new stemwood is actually new stemwood IN THE BOLE.
  !       4/04 AJM.  Adding another new output file.  NPP & GPP by tissue, along with MR and GR by tissue.

  ! Variables
  integer :: L,YEAR
  real    :: LEAFB,STEMB,ROOTB,TOTALB
  real    :: HINC,NEWD,NEWBD,NEWH,VNEW,VOLD,VOL
  real    :: MAXHT,TOT,DOB,THT,VOLINC
  real    :: PSN,LPSN,SPSN,FPSN,TOTNET
  real    :: NLEAF,NSTEM,NFROOT,NNSTEM
  real    :: FF,BI,AIB,AIC,HCB,SLP,DA,BSCONST,GG
  real    :: A1,A2,A3,C1
  character(len=4) :: SPEC
  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'

  ! First, copy current variable values to old variables
  ! Then, convert the carbon increment to biomass in the components-
  ! leaf, stem, roots, and total. Then, convert biomass to tree
  ! dimensions. Last, update tree dimensions.

  ! COPY CURRENT VALUES TO OLD-VALUE VARIABLES
  if (ID(I).eq.'T') then
     O_D(I) = D(I)
     O_BD(I) = BD(I)
     O_H(I) = H(I)
     O_CR(I) = CR(I)
     O_CW(I) = CW(I)
     O_TPH(I) = TPH(I)
     O_BA(I) = BA(I)
     O_TOTBA = TOTBA
  else if (ID(I).eq.'S') then
     O_PCOVER(I) = PCOVER(I)
     O_H(I) = H(I)
  else if (ID(I).eq.'G') then
     O_PCOVER(I) = PCOVER(I)
     O_H(I) = H(I)
  end if
  O_LEAF(I) = LEAF(I)
  O_STEM(I) = STEM(I)
  O_ROOT(I) = ROOT(I)
  O_LA(I) = LA(I)
  O_SUMLA = SUMLA !not currently used--DWC,2/17/94

  ! subtract maintenance respiration from photosynthesis
  PSN = ETYPSN(I) - ETYMRESP(I)
  ! zero-out growth respiration and psn if psn <= 0
  if (PSN.le.0.0) then
     PSN = 0.0
     ETYGRESP(I) = 0.0
     ETYLFGR(I) = 0.0
     ETYSTGR(I) = 0.0
     ETYFRGR(I) = 0.0
     !       ETYCRGR(I)=0.0
  end if
  ! actual carbon allocation; leaf, stem, coarse root, fine root
  LPSN = PSN * LEAFCF(I)
  SPSN = PSN * STEMCF(I)
  !     CPSN=PSN * CROOTCF(I)
  FPSN = PSN * FROOTCF(I)
  ! final net carbon allocation minus growth resp.; l, s, cr, fr
  NLEAF = LPSN - ETYLFGR(I)
  NSTEM = SPSN - ETYSTGR(I)
  NFROOT = FPSN - ETYFRGR(I)
  !     NCROOT=CPSN - ETYCRGR(I)
  if (NLEAF.lt.0.0) NLEAF = 0.0
  if (NSTEM.lt.0.0) NSTEM = 0.0
  if (NFROOT.lt.0.0) NFROOT = 0.0
  TOTNET = NLEAF + NSTEM + NFROOT !TOTAL NET PSN (kgC/ha/yr)
  ! expand net carbon back to biomass
  CNVRT = 1/B2(18)
  LEAFB = NLEAF * CNVRT
  STEMB = NSTEM * CNVRT
  ROOTB = NFROOT * CNVRT
  TOTALB = LEAFB + STEMB + ROOTB

  ! UPDATE THE CARBON POOLS WITH THE INCREMEMENTS
  LEAF(I) = LEAF(I) + NLEAF
  STEM(I) = STEM(I) + NSTEM
  ROOT(I) = ROOT(I) + NFROOT

  ! FOR DECIDUOUS PLANTS, SUBTRACT THE COST OF LEAFING OUT IN THE SPRING
  !      IF(ID(I).EQ.'G'.OR.ID(I).EQ.'S') THEN
  !         COSTL=LEAF(I)*B2(7)
  !         COSTR=ROOT(I)*B2(7)
  !         LEAF(I)=LEAF(I)-COSTL
  !         ROOT(I)=ROOT(I)-COSTR
  !      ENDIF

  ! CALCULATE INCREMENT OF PLANT DIMENSIONS
  ! Calculate increments of plant dimensions. For trees with DBH >= 2.0 inches (5.08 cm)
  ! use Stage's recommendation (a la Korol et al) to increment H & D. Method uses (1) volume
  ! increment equation derived by differencing CIC volume equation used in biomass initialization
  ! and (2) a 'develpmental' H:D equation from authors data. Gives two equations in two unknowns,
  ! H and D, since volume increment is predicted from Stand-BGC.

  ! first, update trees
  if (ID(I).eq.'T') then
     L = BGCISP(I)
     ! remove branch, twig and bark carbon from total stem allocation prior to updating
     ! dimensions. Predict fraction of total stem carbon in bole wood using equations
     ! fitted to data generated from biomass equations found in BINITIAL. Default to
     ! lodgepole pine. For trees with D(I) < 5.08 cm., use basal diameter, BD(I), in
     ! place of DBH. Diameters are metric.
     DOB = D(I)
     if (D(I).lt.5.08) DOB = BD(I)
     FBOLE = 0.43684 + 0.05830*LOG(DOB)
     if (SPP(I).eq.'PP') FBOLE = 0.41290 + 0.06303*LOG(DOB)
     if (SPP(I).eq.'DF') FBOLE = 0.63794 + 0.03000*LOG(DOB)
     if (SPP(I).eq.'WL') FBOLE = 0.64127 + 0.04553*LOG(DOB)
     if (SPP(I).eq.'LP') FBOLE = 0.43684 + 0.05830*LOG(DOB)
     ! Calculate carbon increment to bole
     CARBINC = FBOLE*NSTEM
     ! Convert to wood volume increment in cubic meters
     VOLINC = CARBINC*(1.0/B2(18))*(1.0/500.0) !kgC-->m3
     ! Convert to cubic feet for use with CIC volume equations
     VOLINC = VOLINC*35.3
     ! Convert current tree dimensions to English units.
     THT = H(I)*3.28
     DOB = DOB/2.54
     BASD = BD(I)/2.54
     ! Now calculate new diameter and height using differenced volume
     ! equation and H:D development equation.
     if (D(I).ge.5.08) then
        ! Volume equation is Champion CVTS. DBH and HT in english units:

        !       EQ 1.    LOG10(VOL) = a0+a1*LOG10(DBH)+a2*LOG10(HT)

        ! The H:D equation was developed from Milners dissertation stem analysis data
        ! from western Montana and is:

        !       EQ 2.    LOG10(HT) = c0+c1*LOG10(DBH)

        ! Eq 1 is differenced to get change in volume. Eq 2 is then solved for LOGDBH
        ! or LOGHT and substituted in Eq 1 to get an expression for new D and H.

        ! Set coefficients for EQ 1 and EQ 2 by species. Default to DF.
        if (SPP(I).eq.'PP') then
           c1 = 1.2182
           a1 = 1.87254
           a2 = 0.89306
        else if (SPP(I).eq.'DF') then
           c1 = .86235
           a1 = 1.82212
           a2 = 0.95883
        else if (SPP(I).eq.'WL') then
           c1 = 0.93298
           a1 = 1.95789
           a2 = 0.86407
        else if (SPP(I).eq.'LP') then
           c1 = 1.01854
           a1 = 1.87765
           a2 = 1.03312
        else ! Use DF coefficients
           c1 = .86235
           a1 = 1.82212
           a2 = 0.95883
        end if
        ! Get CVTS for old DBH (v) and calculate new CVTS (V). Use
        ! English units.
        SPEC = SPP(I)
        call BCVTS(SPEC, DOB, THT, VOL)
        VOLD = VOL
        VNEW = VOL+VOLINC
        ! Solve for new height and diameter.
        NEWD = DOB*(10.0)**(LOG10(VNEW/VOLD)/(c1*a2+a1))
        NEWH = THT*10**(LOG10(VNEW/VOLD)/(a2+a1/c1))
        ! Convert to metric
        D(I) = NEWD*2.54
        H(I) = NEWH/3.28
     else
        ! For small trees use volume equation for parabaloid and a H:BD equation that
        ! interpolates between height=0 and height at 2" DBH. Basal diameter corresponding
        ! to DBH=2" is assumed to be DBH+2*BRK.
        REFD = 2.0
        SPEC = SPP(I)
        call BARK(SPEC,REFD,BRK)
        call HTDIAM(SPEC,REFD,REFHT)
        ! Compute slope of straight line H:BD curve. Set coefficients for parabaloid
        ! volume (cuft)
        c1 = REFHT/(REFD+2*BRK)
        a1 = 0.002727 !0.00003925 IS FOR METRIC
        a2 = 2.0
        a3 = 1.0
        ! TROUBLESHOOT
        write(99,100)YEAR,I,BRK,REFHT,C1,VOLINC,A_(I),DOB,D(I),BD(I),&
             &BASD
        100 format(I4,1X,I6,1X,9(F12.4,1X))
        ! Solve for new height and basal diameter.
        NEWBD = (VOLINC/(a1*c1**a3) + BASD**(a2+a3))**(1/(a2+a3))
        NEWH = (((c1**a2)/a1)*VOLINC + THT**(a2+a3))**(1/(a2+a3))
        ! Convert to metric
        BD(I) = NEWBD*2.54
        H(I) = NEWH/3.28

        ! COMMENTING OUT THIS BLOCK, INSERTING NEW LOGIC. AJM 7/03
        ! In bentyload (and bentyupdate), we inserted new logic, redefining the SHAPE
        ! of the bole as a parabola defined as (BA/2)^2=4aHT.  Parameter "a" was
        ! calculated by tree, (kept in common), in aforementioned routines.  We use it
        ! here to get new DBH.
        ! Note: "A_" assumes diameters are in inches and heights in feet.

        ! if updated height for seedlings is > 1.3m then give it a D(I)
        !            IF(H(I).GT.1.3) THEN
        !                D(I)=BD(I)*(1-1.3/H(I)) !! derived from similar triangles
        !            ENDIF

        if (NEWH.gt.4.5) then
           D(I) = 4.*SQRT(A_(I)*(NEWH-4.5))*2.54 !! DIAM ON TREE'S PARABOLA 4.5' FROM ITS NEW BASE
        end if
     end if

     ! update basal area of trees
     if (D(I).lt.5.08) then
        BA(I) = BD(I)**2. * 0.00007854
     else
        BA(I) = D(I)**2. * 0.00007854
     end if
     TOTBA = TOTBA + BA(I)

     ! Write out old and new diameter and height
     !      WRITE(73,997)YEAR, I,ID(I), TREENO(I), VOLINC, D(I), O_D(I), H(I),
     !     &              O_H(I)
     !  997 FORMAT(2(I4,1X),A1,1X,I8,5(1X,F7.3))

     !  Adding a new output file: This will generate a Vigor Index: grams of stemwood produced (this
     !  year) per unit leaf area.
     !  Assumptions: The grams of stemwood is NetPP (for this year--after growth and maint. resp) for
     !  all non-root and non-leaf tissue (in biomass, not C).  LA is as of beginning of year.  AJM 2/01.
     !  Post-processing will be required to calculate a vigor based on 5-yr (or whatever) production.
     !  Post-processing is also required to calculate stand-level vigor.

     !  Amending 1/04 ajm.  adding FBOLE and FBOLE*Stembiomass to vigor.out, and
     !  convert LA to proj LA, and convert the expanded variables to bole NPP and Proj LA.

     if (Z1(1) .eq.1.and. I .eq.1) write(76,910) ! Headers
     write(76,900)YEAR,TREENO(I),STEMB*1000,LA(I),LA(I)/B2(15),& ! "* 1000" converts to grams.
          &FBOLE,STEMB*1000*FBOLE,(STEMB*1000*FBOLE)/(LA(I)/B2(15)),&
          &TPH(I),STEMB*EXPAND(I)*FBOLE,LA(I)/B2(15)*EXPAND(I)

     !  The last two "* EXPAND(I)" terms are for postprocessing vigor up to stand-level.

     900 format(I4,1X,I7,1X,3(F10.3,1X),f6.4,5(1x,F10.3))
     910 format('YEAR    TREE   WOOD_NPP      LA(AS)  LA(PROJ)  FBOLE',&
          &'   BOLE_NPP      VIGOR        TPH   BOLE_NPP   LA(PROJ)'/,&
          &'               g/entity        m^2        m^2  propn   g/entity',&
          &'      g/m^2                 Kg/ha     m^2/ha')

     write(78,920)TREENO(I),etypsn(I),totnet,&
          &lpsn,yrmrlf(I),etylfgr(I),nleaf,&
          &spsn,yrmrst(I),etystgr(I),nstem,&
          &fpsn,yrmrrt(I),etyfrgr(I),nfroot
     920 format(i6,14(2X,F10.3))

     ! update shrubs and grasses
     !!!!added shrubs back in -- DWC,8/5/94
  else if (ID(I).eq.'S') then
     !all shrub cover updates occur in TRNOVR.FOR
     !        MAXHT=1.0  Changed to 2 meters (since Cover-model input is
     !                   initialized at 2 m for tall shrubs.  ajm 11/1/00.
     !       fixed "TOT" line, below. Before my change, it began in col6 ajm 1/04
     MAXHT = 2.0
     TOT = LEAFB + STEMB
     if (H(I).lt.MAXHT) then
        HINC = ((TOT*500. / AREA) - 1.66075) *&
             &(1./0.06348) * (1./PCOVER(I))
     else
        HINC = 0.0
     end if
     H(I) = H(I) + HINC
     if (H(I).gt.MAXHT) H(I) = MAXHT
     !       WRITE(*,996) I,PCOVER(I),H(I),LEAF(I),STEM(I),ROOT(I)
  else if (ID(I).eq.'G') then
     HINC = 0.0
     H(I) = H(I)+HINC
     !all grass cover updates occur in TRNOVR.FOR
     !       WRITE(*,996) I,PCOVER(I),H(I),LEAF(I),STEM(I),ROOT(I)
     ! 996   FORMAT(I4,5(1X,F8.2))
  end if
  return
end
