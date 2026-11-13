!====================================!
!   Compute input stand statistics   !
!====================================!
! BASE $Id$
subroutine STATS
  implicit none

  !  THIS ROUTINE COMPUTES STATISTICS THAT DESCRIBE THE INPUT
  !  DISTRIBUTION OF STAND ATTRIBUTES AMONG SAMPLE PLOTS.  CALLED FROM
  !  **MAIN**.  **TVALUE** IS CALLED TO CALCULATE STUDENT'S T FOR
  !  CONSTRUCTION OF CONFIDENCE INTERVALS.

  !  THE METRIC VARIANTS (BC, ON) PRINT PER-HECTARE VALUES, REPORT FOUR
  !  ATTRIBUTES IN A DIFFERENT ORDER (TREES, CUBIC VOLUME, BOARD FEET,
  !  WHICH IS ZEROED, AND BASAL AREA) AND HAVE NO BIOMASS OR CARBON
  !  STATISTICS (LMETRICVAR BRANCHES). THE STATISTICS THEMSELVES ARE
  !  COMPUTED BY THE SAME CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'VOLSTD.f90'
  include 'PLOT.f90'
  include 'METRIC.f90'

  ! Variables
  character(len=19) :: LABELS(6)
  character(len=16) :: LABELM(4)
  character(len=4) :: SP(MAXSP)
  character(len=4) :: SPECCD(MAXSP)
  real    :: TOTCF(MAXSP),SIGLEVEL,TOTTR(MAXSP),TOTBA(MAXSP),TOTBF(MAXSP),SUMT(MAXPLT),SUMBA(MAXPLT), &
       & SUMCF(MAXPLT),SUMBF(MAXPLT),ITPA(MAXSP),IBA(MAXSP),IBF(MAXSP),ICF(MAXSP),TPA(MAXSP),BAREA(MAXSP), &
       & BFVOL(MAXSP),CFVOL(MAXSP),TOTBIO(MAXSP),SUMBIO(MAXPLT),IBIO(MAXSP),AGBIO(MAXSP),TOTCARB(MAXSP), &
       & SUMCARB(MAXPLT),ICARB(MAXSP),AGCARB(MAXSP)
  integer :: IFLG(MAXSP),I,ISPC,J,IALP,IERR,NDF,ROWS,CNT,IYEAR,K
  real    :: P,TBA,TBF,TCF,T,SUM,SUMSQ,ST,XBAR,S,SS,SE,UL,UU,CV,TBIOMASS,TCARB
  real    :: SEU,SEP,SCF,SBF,SBA,SBIO,SCARB,IDIST(8,6)
  equivalence (WK3,SUMT),(WK3(MAXPLT+1),SUMBA), &
       & (WK4,SUMCF),(WK4(MAXPLT+1),SUMBF), &
       & (WK5,TOTTR),(WK5(MAXSP+1),TOTBA), &
       & (WK6,TOTCF),(WK6(MAXSP+1),TOTBF)
  data LABELS/ &
       & 'TREES/ACRE         ', &
       & 'BASAL AREA/ACRE    ', &
       & 'CUBIC FEET/ACRE    ', &
       & 'BOARD FEET/ACRE    ', &
       & 'ABVGRD BIOMASS/ACRE', &
       & 'ABVGRD CARB/ACRE   '/

  !  LABELM: METRIC LABELS, IN THE METRIC TWIN'S ORDER.

  data LABELM/ &
       & 'BOARD FEET/HA   ', &
       & 'CUBIC METERS/HA ', &
       & 'TREES/HA        ', &
       & 'BASAL AREA/HA   '/

  !  IF THERE ARE NO TREE RECORDS, OR STATS OPTION WAS NOT REQUESTED,
  !  RETURN.

  if (.not.LSTATS.or.ITRN.eq.0) return

  !  INITIALIZE.

  do I = 1,MAXSP
     TOTTR(I) = 0.0
     TOTBA(I) = 0.0
     TOTBF(I) = 0.0
     TOTCF(I) = 0.0
     IFLG(I) = 0
     ITPA(I) = 0.0
     IBA(I) = 0.0
     IBF(I) = 0.0
     ICF(I) = 0.0
     IBIO(I) = 0.0
     ICARB(I) = 0.0
     TPA(I) = 0.0
     BAREA(I) = 0.0
     BFVOL(I) = 0.0
     CFVOL(I) = 0.0
     TOTBIO(I) = 0.0
     TOTCARB(I) = 0.0

     1 continue
  end do
  do I = 1,MAXPLT
     SUMT(I) = 0.0
     SUMBA(I) = 0.0
     SUMBF(I) = 0.0
     SUMCF(I) = 0.0
     SUMBIO(I) = 0.0
     SUMCARB(I) = 0.0
     2 continue
  end do
  do I = 1,8
     do J = 1,6
        IDIST(I,J) = 0.0
     end do
  end do
  SIGLEVEL = 0.0
  ROWS = 0
  CNT = 0
  IYEAR = 0

  !  ACCUMULATE SUMS FOR OVERALL STATISTICS.

  do I = 1,ITRN
     ISPC = ISP(I)
     J = ITRE(I)
     P = PROB(I)
     TBA = DBH(I)*DBH(I)*P*0.005454154
     TBF = BFV(I)*P
     TCF = CFV(I)*P
     if (.not. LMETRICVAR) then
        TBIOMASS = (ABVGRD_BIO(I)*P)/2000
        TCARB = (ABVGRD_CARB(I)*P)/2000
     end if
     TOTTR(ISPC) = TOTTR(ISPC)+P
     TOTBA(ISPC) = TOTBA(ISPC)+TBA
     TOTCF(ISPC) = TOTCF(ISPC)+TCF
     TOTBF(ISPC) = TOTBF(ISPC)+TBF
     if (.not. LMETRICVAR) then
        TOTBIO(ISPC) = TOTBIO(ISPC)+TBIOMASS
        TOTCARB(ISPC) = TOTCARB(ISPC)+TCARB
     end if

     IFLG(ISPC) = 1
     if (IPTINV.le.1) go to 30
     SUMT(J) = SUMT(J)+IPTINV*P
     SUMBA(J) = SUMBA(J)+IPTINV*TBA
     SUMBF(J) = SUMBF(J)+IPTINV*TBF
     SUMCF(J) = SUMCF(J)+IPTINV*TCF
     if (.not. LMETRICVAR) then
        SUMBIO(J) = SUMBIO(J)+IPTINV*TBIOMASS
        SUMCARB(J) = SUMCARB(J)+IPTINV*TCARB
     end if
     30 continue
  end do
  do ISPC = 1,MAXSP
     TOTTR(ISPC) = TOTTR(ISPC)/GROSPC
     TOTBA(ISPC) = TOTBA(ISPC)/GROSPC
     TOTCF(ISPC) = TOTCF(ISPC)/GROSPC
     TOTBF(ISPC) = TOTBF(ISPC)/GROSPC
     if (.not. LMETRICVAR) then
        TOTBIO(ISPC) = TOTBIO(ISPC)/GROSPC
        TOTCARB(ISPC) = TOTCARB(ISPC)/GROSPC
     end if
     35 continue
  end do

  !  WRITE TABLE HEADING FOR GENERAL SPECIES SUMMARY.

  write(JOSTND,9000)
  9000 format(/)
  if (LMETRICVAR) then
     write(JOSTND,9101)
     9101 format(T9,'GENERAL SPECIES SUMMARY FOR THE CRUISE', &
          & ' (PER HECTARE)')
     write(JOSTND,9102)
     9102 format(/2X,'SPECIES',T18,'NMV (CU M)',T31,'GTV (CU M)', &
          & T52,'TREES',T61,'BASAL AREA',/,73('-'))
  else
     write(JOSTND,9001)
     9001 format(T9,'GENERAL SPECIES SUMMARY FOR THE CRUISE', &
          & ' (PER ACRE)')
     write(JOSTND,9002)
     9002 format(/2X,'SPECIES',T23,'TREES',T35,'BASAL AREA', &
          & T52,'CUBIC FEET',T69,'BOARD FEET',T86,'ABOVE GROUND BIOMASS', &
          & T113,'ABOVE GROUND CARBON'/,131('-'))
  end if
  ROWS = 0
  do I = 1,MAXSP
     if (IFLG(I).eq.0) go to 40
     ROWS = ROWS+1
     SP(ROWS) = JSP(I)
     ITPA(ROWS) = TOTTR(I)
     IBA(ROWS) = TOTBA(I)
     IBF(ROWS) = TOTBF(I)
     ICF(ROWS) = TOTCF(I)
     if (LMETRICVAR) then
        write(JOSTND,9103) JSP(I), &
             & NSP(I,1)(1:2), &
             & TOTBF(I)*FT3pACRtoM3pHA, &
             & TOTCF(I)*FT3pACRtoM3pHA, &
             & TOTTR(I)/ACRtoHA, &
             & TOTBA(I)*FT2pACRtoM2pHA
        9103 format(1X,A4,'=',A4,T18,F10.1,T31,F10.1,T47,F10.1,T61,F10.1)
     else
        IBIO(ROWS) = TOTBIO(I)
        ICARB(ROWS) = TOTCARB(I)
        write(JOSTND,9003) JSP(I),NSP(I,1)(1:2), &
             & TOTTR(I),TOTBA(I),TOTCF(I),TOTBF(I),TOTBIO(I),TOTCARB(I)
        9003 format(1X,A4,'=',A4,T18,F10.1,T35,F10.1,T52,F10.1,T68,F10.1, &
             & T96,F10.1,T122,F10.1)
     end if
     40 continue
  end do
  if (IPTINV.gt.1) go to 50

  !  1 POINT.  PRINT ERROR MESSAGE AND GO WRITE SPECIES STATS TO DATABASE.

  write(JOSTND,9004)
  9004 format(' DISTRIBUTION OF ATTRIBUTES AMONG SAMPLE POINTS', &
       & ' CANNOT BE COMPUTED WITH ONE SAMPLE POINT.'/)
  go to 150
  50 continue
  write(JOSTND,9005)
  9005 format(//T19,'DISTRIBUTION OF STAND ATTRIBUTES AMONG SAMPLE' &
       & ,' POINTS'/)
  IALP = INT(100.0-100.0*ALPHA+0.5)
  write(JOSTND,9008)IALP
  9008 format(T31,'STANDARD  COEFF OF SAMPLE',T67,I4,'%', &
       & T83,'SAMPLING ERROR IN')
  if (LMETRICVAR) then
     write(JOSTND,9106)
     9106 format(' CHARACTERISTIC',T25,'MEAN DEVIATION VARIATION   SIZE', &
          & '     CONFIDENCE  LIMITS    PERCENT     UNITS',/,16('-'), &
          & 3X,9('-'),1X,9('-'),1X,9('-'),1X,6('-'),1X,22('-'),4X,17('-'))
  else
     write(JOSTND,9006)
     9006 format('CHARACTERISTIC',T25,'MEAN DEVIATION VARIATION   SIZE', &
          & '     CONFIDENCE  LIMITS    PERCENT     UNITS',/,16('-'), &
          & 3X,9('-'),1X,9('-'),1X,9('-'),1X,6('-'),1X,22('-'),4X,17('-'))
  end if

  !  CALL **TVALUE TO APPROXIMATE 'T' FOR (IPTINV-1) DF AT THE 'ALPHA'
  !  PROBABILITY LEVEL.

  NDF = IPTINV-1
  call TVALUE(NDF,ALPHA,T,IERR)

  !  COMPUTE AND PRINT STATISTICS FOR TREES PER ACRE.

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     ST = SUMT(I)/GROSPC
     SUM = SUM+ST
     SUMSQ = SUMSQ+ST*ST
     60 continue
  end do
  if (SUM.gt.0.0) go to 65
  if (LMETRICVAR) then
     write(JOSTND,9107)  LABELM(3),SUM,SUM
  else
     write(JOSTND,9007)  LABELS(1),SUM,SUM
  end if
  9007 format(A19,T19,3(1X,F9.2),1X,I6,1X,F9.2,4X,F9.2, &
       & 4X,F6.1,1X,F10.1)
  9107 format(A16,T19,3(1X,F9.2),1X,I6,1X,F9.2,4X,F9.2, &
       & 4X,F6.1,1X,F10.1)
  go to 70
  65 continue
  XBAR = SUM/PI
  S = 0.0
  SS = SUMSQ-SUM*SUM/PI
  if (SS.le.0.0) go to 67
  S = SQRT(SS/(PI-1.0))
  67 continue
  SE = S/SQRT(PI)
  UL = XBAR-T*SE
  if (UL.lt.0.0) UL = 0.0
  UU = XBAR+T*SE
  CV = S/XBAR
  SEU = T*SE
  SEP = SEU*100./XBAR
  if (LMETRICVAR) then
     write(JOSTND,9107) LABELM(3), &
          & XBAR /ACRtoHA, &
          & S    /ACRtoHA, &
          & CV, &
          & IPTINV, &
          & UL   /ACRtoHA, &
          & UU   /ACRtoHA, &
          & SEP  /ACRtoHA, &
          & SEU
     IDIST(1,1) = XBAR /ACRtoHA
     IDIST(2,1) = S    /ACRtoHA
     IDIST(3,1) = CV
     IDIST(4,1) = IPTINV
     IDIST(5,1) = UL   /ACRtoHA
     IDIST(6,1) = UU   /ACRtoHA
     IDIST(7,1) = SEP  /ACRtoHA
     IDIST(8,1) = SEU
  else
     write(JOSTND,9007)  LABELS(1),XBAR,S,CV,IPTINV, &
          & UL,UU,SEP,SEU
     IDIST(1,1) = XBAR
     IDIST(2,1) = S
     IDIST(3,1) = CV
     IDIST(4,1) = IPTINV
     IDIST(5,1) = UL
     IDIST(6,1) = UU
     IDIST(7,1) = SEP
     IDIST(8,1) = SEU
  end if
  70 continue

  !  THE METRIC VARIANTS REPORT CUBIC VOLUME AND BOARD FEET NEXT AND
  !  BASAL AREA LAST (THEN GO TO 150); THE IMPERIAL VARIANTS REPORT
  !  BASAL AREA NEXT. THE METRIC ORDER IS PRESERVED BY THE BRANCHES TO
  !  75, 115 AND 150.

  if (LMETRICVAR) go to 75

  !  COMPUTE AND PRINT STATISTICS FOR BASAL AREA PER ACRE.

  115 continue
  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SBA = SUMBA(I)/GROSPC
     SUM = SUM+SBA
     SUMSQ = SUMSQ+SBA*SBA
     120 continue
  end do
  if (SUM.gt.0.0) go to 130
  if (LMETRICVAR) then
     write(JOSTND,9107) LABELM(4),SUM,SUM
  else
     write(JOSTND,9007) LABELS(2),SUM,SUM
  end if
  return
  130 continue
  XBAR = SUM/PI
  S = 0.0
  SS = SUMSQ-SUM*SUM/PI
  if (SS.le.0.0) go to 137
  S = SQRT(SS/(PI-1.0))
  137 continue
  SE = S/SQRT(PI)
  UL = XBAR-T*SE
  if (UL.lt.0.0) UL = 0.0
  UU = XBAR+T*SE
  CV = S/XBAR
  SEU = T*SE
  SEP = SEU*100./XBAR
  if (LMETRICVAR) then
     write(JOSTND,9107) LABELM(4), &
          & XBAR *FT2pACRtoM2pHA, &
          & S    *FT2pACRtoM2pHA, &
          & CV, &
          & IPTINV, &
          & UL   *FT2pACRtoM2pHA, &
          & UU   *FT2pACRtoM2pHA, &
          & SEP  *FT2pACRtoM2pHA, &
          & SEU
     IDIST(1,4) = XBAR *FT2pACRtoM2pHA
     IDIST(2,4) = S    *FT2pACRtoM2pHA
     IDIST(3,4) = CV
     IDIST(4,4) = IPTINV
     IDIST(5,4) = UL   *FT2pACRtoM2pHA
     IDIST(6,4) = UU   *FT2pACRtoM2pHA
     IDIST(7,4) = SEP  *FT2pACRtoM2pHA
     IDIST(8,4) = SEU
  else
     write(JOSTND,9007) LABELS(2),XBAR,S,CV,IPTINV, &
          & UL,UU,SEP,SEU

     IDIST(1,2) = XBAR
     IDIST(2,2) = S
     IDIST(3,2) = CV
     IDIST(4,2) = IPTINV
     IDIST(5,2) = UL
     IDIST(6,2) = UU
     IDIST(7,2) = SEP
     IDIST(8,2) = SEU
  end if

  SIGLEVEL = (1-ALPHA)*100
  if (LMETRICVAR) go to 150

  !  COMPUTE AND PRINT STATISTICS FOR CUBIC FOOT VOLUME.

  75 continue
  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SCF = SUMCF(I)/GROSPC
     SUM = SUM+SCF
     SUMSQ = SUMSQ+SCF*SCF
     80 continue
  end do
  if (SUM.gt.0.0) go to 85
  if (LMETRICVAR) then
     write(JOSTND,9107)  LABELM(2),SUM,SUM
  else
     write(JOSTND,9007)  LABELS(3),SUM,SUM
  end if
  go to 90
  85 continue
  XBAR = SUM/PI
  S = 0.0
  SS = SUMSQ-SUM*SUM/PI
  if (SS.le.0.0) go to 87
  S = SQRT(SS/(PI-1.0))
  87 continue
  SE = S/SQRT(PI)
  UL = XBAR-T*SE
  if (UL.lt.0.0) UL = 0.0
  UU = XBAR+T*SE
  CV = S/XBAR
  SEU = T*SE
  SEP = SEU*100./XBAR
  if (LMETRICVAR) then
     write(JOSTND,9107) LABELM(2), &
          & XBAR *FT3pACRtoM3pHA, &
          & S    *FT3pACRtoM3pHA, &
          & CV, &
          & IPTINV, &
          & UL   *FT3pACRtoM3pHA, &
          & UU   *FT3pACRtoM3pHA, &
          & SEP  *FT3pACRtoM3pHA, &
          & SEU
     IDIST(1,2) = XBAR *FT3pACRtoM3pHA
     IDIST(2,2) = S    *FT3pACRtoM3pHA
     IDIST(3,2) = CV
     IDIST(4,2) = IPTINV
     IDIST(5,2) = UL   *FT3pACRtoM3pHA
     IDIST(6,2) = UU   *FT3pACRtoM3pHA
     IDIST(7,2) = SEP  *FT3pACRtoM3pHA
     IDIST(8,2) = SEU
  else
     write(JOSTND,9007)  LABELS(3),XBAR,S,CV,IPTINV, &
          & UL,UU,SEP,SEU
     IDIST(1,3) = XBAR
     IDIST(2,3) = S
     IDIST(3,3) = CV
     IDIST(4,3) = IPTINV
     IDIST(5,3) = UL
     IDIST(6,3) = UU
     IDIST(7,3) = SEP
     IDIST(8,3) = SEU
  end if
  90 continue

  !  COMPUTE AND PRINT STATISTICS FOR BOARD FOOT VOLUME.
  !  (THE METRIC TWIN PRINTS THIS LINE WITH EVERY VALUE BUT THE SAMPLE
  !  SIZE MULTIPLIED BY ZERO.)

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SBF = SUMBF(I)/GROSPC
     SUM = SUM+SBF
     SUMSQ = SUMSQ+SBF*SBF
     100 continue
  end do
  if (SUM.gt.0.0) go to 105
  if (LMETRICVAR) then
     write(JOSTND,9107)  LABELM(1),SUM,SUM
  else
     write(JOSTND,9007)  LABELS(4),SUM,SUM
  end if
  go to 110
  105 continue
  XBAR = SUM/PI
  S = 0.0
  SS = SUMSQ-SUM*SUM/PI
  if (SS.le.0.0) go to 107
  S = SQRT(SS/(PI-1.0))
  107 continue
  SE = S/SQRT(PI)
  UL = XBAR-T*SE
  if (UL.lt.0.0) UL = 0.0
  UU = XBAR+T*SE
  CV = S/XBAR
  SEU = T*SE
  SEP = SEU*100./XBAR
  if (LMETRICVAR) then
     write(JOSTND,9107) LABELM(1), &
          & XBAR *0., &
          & S    *0., &
          & CV   *0., &
          & IPTINV, &
          & UL   *0., &
          & UU   *0., &
          & SEP  *0., &
          & SEU  *0.
     IDIST(1,3) = XBAR *0.
     IDIST(2,3) = S    *0.
     IDIST(3,3) = CV   *0.
     IDIST(4,3) = IPTINV
     IDIST(5,3) = UL   *0.
     IDIST(6,3) = UU   *0.
     IDIST(7,3) = SEP  *0.
     IDIST(8,3) = SEU  *0.
  else
     write(JOSTND,9007) LABELS(4),XBAR,S,CV,IPTINV, &
          & UL,UU,SEP,SEU
     IDIST(1,4) = XBAR
     IDIST(2,4) = S
     IDIST(3,4) = CV
     IDIST(4,4) = IPTINV
     IDIST(5,4) = UL
     IDIST(6,4) = UU
     IDIST(7,4) = SEP
     IDIST(8,4) = SEU
  end if
  110 continue
  if (LMETRICVAR) go to 115

  !  COMPUTE AND PRINT STATISTICS FOR ABOVE GROUND BIOMASS

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SBIO = SUMBIO(I)/GROSPC
     SUM = SUM+SBIO
     SUMSQ = SUMSQ+SBIO*SBIO
     170 continue
  end do
  if (SUM.gt.0.0) go to 171
  write(JOSTND,9007)  LABELS(5),SUM,SUM
  go to 175
  171 continue
  XBAR = SUM/PI
  S = 0.0
  SS = SUMSQ-SUM*SUM/PI
  if (SS.le.0.0) go to 172
  S = SQRT(SS/(PI-1.0))
  172 continue
  SE = S/SQRT(PI)
  UL = XBAR-T*SE
  if (UL.lt.0.0) UL = 0.0
  UU = XBAR+T*SE
  CV = S/XBAR
  SEU = T*SE
  SEP = SEU*100./XBAR
  write(JOSTND,9007)  LABELS(5),XBAR,S,CV,IPTINV, &
       & UL,UU,SEP,SEU
  IDIST(1,5) = XBAR
  IDIST(2,5) = S
  IDIST(3,5) = CV
  IDIST(4,5) = IPTINV
  IDIST(5,5) = UL
  IDIST(6,5) = UU
  IDIST(7,5) = SEP
  IDIST(8,5) = SEU
  175 continue

  !  COMPUTE AND PRINT STATISTICS FOR ABOVE GROUND CARBON

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SCARB = SUMCARB(I)/GROSPC
     SUM = SUM+SCARB
     SUMSQ = SUMSQ+SCARB*SCARB
     140 continue
  end do
  if (SUM.gt.0.0) go to 141
  write(JOSTND,9007)  LABELS(6),SUM,SUM
  go to 145
  141 continue
  XBAR = SUM/PI
  S = 0.0
  SS = SUMSQ-SUM*SUM/PI
  if (SS.le.0.0) go to 142
  S = SQRT(SS/(PI-1.0))
  142 continue
  SE = S/SQRT(PI)
  UL = XBAR-T*SE
  if (UL.lt.0.0) UL = 0.0
  UU = XBAR+T*SE
  CV = S/XBAR
  SEU = T*SE
  SEP = SEU*100./XBAR
  write(JOSTND,9007)  LABELS(6),XBAR,S,CV,IPTINV, &
       & UL,UU,SEP,SEU
  IDIST(1,6) = XBAR
  IDIST(2,6) = S
  IDIST(3,6) = CV
  IDIST(4,6) = IPTINV
  IDIST(5,6) = UL
  IDIST(6,6) = UU
  IDIST(7,6) = SEP
  IDIST(8,6) = SEU
  145 continue

  !  POPULATE ARRAYS WITH NON-ZERO VALUES

  150 CNT = 0
  do I = 1,MAXSP
     CNT = CNT+1
     SPECCD(CNT) = SP(I)
     if (LMETRICVAR) then
        TPA(CNT) = ITPA(I)/ACRtoHA
        BAREA(CNT) = IBA(I)*FT2pACRtoM2pHA
        BFVOL(CNT) = IBF(I)*0.
        CFVOL(CNT) = ICF(I)*FT3pACRtoM3pHA
     else
        TPA(CNT) = ITPA(I)
        BAREA(CNT) = IBA(I)
        BFVOL(CNT) = IBF(I)
        CFVOL(CNT) = ICF(I)
        AGBIO(CNT) = IBIO(I)
        AGCARB(CNT) = ICARB(I)
     end if
  end do
  IYEAR = IY(1)

  !     CALL DBSSTATS FOR POPULATING THE DATABASE WITH CRUISE STATISTICS.
  !     THE METRIC DBSSTATS HAS NO BIOMASS OR CARBON ARGUMENTS, SO THE
  !     METRIC VARIANTS REACH IT THROUGH DBSSTATSM.

  !     DATABASE OUTPUT FOR STAND (REQUIRES MULTIPLE SAMPLE POINTS)

  if (IPTINV.eq.1) go to 160

  if (LMETRICVAR) then
     K = 3
     do I = 1,3
        call DBSSTATSM(SP(I),TPA(I),BAREA(I),CFVOL(I), &
             & BFVOL(I),IDIST(1,I),IDIST(2,I),IDIST(3,I), &
             & IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I), &
             & IDIST(8,I),LABELM(K),2,IYEAR)
        K = K-1
     end do
     call DBSSTATSM(SP(I),TPA(I),BAREA(I),CFVOL(I), &
          & BFVOL(I),IDIST(1,I),IDIST(2,I),IDIST(3,I), &
          & IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I), &
          & IDIST(8,I),LABELM(4),2,IYEAR)
  else
     do I = 1,6
        call DBSSTATS(SP(I),TPA(I),BAREA(I),CFVOL(I),BFVOL(I), &
             & AGBIO(I),AGCARB(I),IDIST(1,I),IDIST(2,I),IDIST(3,I), &
             & IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I), &
             & IDIST(8,I),LABELS(I),2,IYEAR)
     end do
  end if

  !     DATABASE OUPUT FOR GENERAL SPECIES SUMMARY

  160 continue
  if (LMETRICVAR) then
     do I = 1,ROWS
        call DBSSTATSM(SP(I),TPA(I),BAREA(I),CFVOL(I), &
             & BFVOL(I),IDIST(1,I),IDIST(2,I),IDIST(3,I), &
             & IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I), &
             & IDIST(8,I),LABELM(I),1,IYEAR)
     end do
  else
     do I = 1,ROWS
        call DBSSTATS(SP(I),TPA(I),BAREA(I),CFVOL(I),BFVOL(I), &
             & AGBIO(I),AGCARB(I),IDIST(1,I),IDIST(2,I),IDIST(3,I), &
             & IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I), &
             & IDIST(8,I),LABELS(I),1,IYEAR)
     end do
  end if

  return
end subroutine STATS
