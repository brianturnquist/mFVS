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

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'VOLSTD.f90'
  include 'PLOT.f90'

  ! Variables
  character(len=19) :: LABELS(6)
  character(len=4) :: SP(MAXSP)
  character(len=4) :: SPECCD(MAXSP)
  real    :: TOTCF(MAXSP),SIGLEVEL,TOTTR(MAXSP),TOTBA(MAXSP),TOTBF(MAXSP),SUMT(MAXPLT),SUMBA(MAXPLT), &
       & SUMCF(MAXPLT),SUMBF(MAXPLT),ITPA(MAXSP),IBA(MAXSP),IBF(MAXSP),ICF(MAXSP),TPA(MAXSP),BAREA(MAXSP), &
       & BFVOL(MAXSP),CFVOL(MAXSP),TOTBIO(MAXSP),SUMBIO(MAXPLT),IBIO(MAXSP),AGBIO(MAXSP),TOTCARB(MAXSP), &
       & SUMCARB(MAXPLT),ICARB(MAXSP),AGCARB(MAXSP)
  integer :: IFLG(MAXSP),I,ISPC,J,IALP,IERR,NDF,ROWS,CNT,IYEAR
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
     TBIOMASS = (ABVGRD_BIO(I)*P)/2000
     TCARB = (ABVGRD_CARB(I)*P)/2000
     TOTTR(ISPC) = TOTTR(ISPC)+P
     TOTBA(ISPC) = TOTBA(ISPC)+TBA
     TOTCF(ISPC) = TOTCF(ISPC)+TCF
     TOTBF(ISPC) = TOTBF(ISPC)+TBF
     TOTBIO(ISPC) = TOTBIO(ISPC)+TBIOMASS
     TOTCARB(ISPC) = TOTCARB(ISPC)+TCARB

     IFLG(ISPC) = 1
     if (IPTINV.le.1) go to 30
     SUMT(J) = SUMT(J)+IPTINV*P
     SUMBA(J) = SUMBA(J)+IPTINV*TBA
     SUMBF(J) = SUMBF(J)+IPTINV*TBF
     SUMCF(J) = SUMCF(J)+IPTINV*TCF
     SUMBIO(J) = SUMBIO(J)+IPTINV*TBIOMASS
     SUMCARB(J) = SUMCARB(J)+IPTINV*TCARB
     30 continue
  end do
  do ISPC = 1,MAXSP
     TOTTR(ISPC) = TOTTR(ISPC)/GROSPC
     TOTBA(ISPC) = TOTBA(ISPC)/GROSPC
     TOTCF(ISPC) = TOTCF(ISPC)/GROSPC
     TOTBF(ISPC) = TOTBF(ISPC)/GROSPC
     TOTBIO(ISPC) = TOTBIO(ISPC)/GROSPC
     TOTCARB(ISPC) = TOTCARB(ISPC)/GROSPC
     35 continue
  end do

  !  WRITE TABLE HEADING FOR GENERAL SPECIES SUMMARY.

  write(JOSTND,9000)
  9000 format(/)
  write(JOSTND,9001)
  9001 format(T9,'GENERAL SPECIES SUMMARY FOR THE CRUISE', &
       & ' (PER ACRE)')
  write(JOSTND,9002)
  9002 format(/2X,'SPECIES',T23,'TREES',T35,'BASAL AREA', &
       & T52,'CUBIC FEET',T69,'BOARD FEET',T86,'ABOVE GROUND BIOMASS', &
       & T113,'ABOVE GROUND CARBON'/,131('-'))
  ROWS = 0
  do I = 1,MAXSP
     if (IFLG(I).eq.0) go to 40
     ROWS = ROWS+1
     SP(ROWS) = JSP(I)
     ITPA(ROWS) = TOTTR(I)
     IBA(ROWS) = TOTBA(I)
     IBF(ROWS) = TOTBF(I)
     ICF(ROWS) = TOTCF(I)
     IBIO(ROWS) = TOTBIO(I)
     ICARB(ROWS) = TOTCARB(I)
     write(JOSTND,9003) JSP(I),NSP(I,1)(1:2), &
          & TOTTR(I),TOTBA(I),TOTCF(I),TOTBF(I),TOTBIO(I),TOTCARB(I)
     9003 format(1X,A4,'=',A4,T18,F10.1,T35,F10.1,T52,F10.1,T68,F10.1, &
          & T96,F10.1,T122,F10.1)
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
  write(JOSTND,9006)
  9006 format('CHARACTERISTIC',T25,'MEAN DEVIATION VARIATION   SIZE', &
       & '     CONFIDENCE  LIMITS    PERCENT     UNITS',/,16('-'), &
       & 3X,9('-'),1X,9('-'),1X,9('-'),1X,6('-'),1X,22('-'),4X,17('-'))

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
  write(JOSTND,9007)  LABELS(1),SUM,SUM
  9007 format(A19,T19,3(1X,F9.2),1X,I6,1X,F9.2,4X,F9.2, &
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
  70 continue

  !  COMPUTE AND PRINT STATISTICS FOR BASAL AREA PER ACRE.

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SBA = SUMBA(I)/GROSPC
     SUM = SUM+SBA
     SUMSQ = SUMSQ+SBA*SBA
     120 continue
  end do
  if (SUM.gt.0.0) go to 130
  write(JOSTND,9007) LABELS(2),SUM,SUM
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

  SIGLEVEL = (1-ALPHA)*100

  !  COMPUTE AND PRINT STATISTICS FOR CUBIC FOOT VOLUME.

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SCF = SUMCF(I)/GROSPC
     SUM = SUM+SCF
     SUMSQ = SUMSQ+SCF*SCF
     80 continue
  end do
  if (SUM.gt.0.0) go to 85
  write(JOSTND,9007)  LABELS(3),SUM,SUM
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
  90 continue

  !  COMPUTE AND PRINT STATISTICS FOR BOARD FOOT VOLUME.

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SBF = SUMBF(I)/GROSPC
     SUM = SUM+SBF
     SUMSQ = SUMSQ+SBF*SBF
     100 continue
  end do
  if (SUM.gt.0.0) go to 105
  write(JOSTND,9007)  LABELS(4),SUM,SUM
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
  110 continue

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
     TPA(CNT) = ITPA(I)
     BAREA(CNT) = IBA(I)
     BFVOL(CNT) = IBF(I)
     CFVOL(CNT) = ICF(I)
     AGBIO(CNT) = IBIO(I)
     AGCARB(CNT) = ICARB(I)
  end do
  IYEAR = IY(1)

  !     CALL DBSSTATS FOR POPULATING THE DATABASE WITH CRUISE STATISTICS

  !     DATABASE OUTPUT FOR STAND (REQUIRES MULTIPLE SAMPLE POINTS)

  if (IPTINV.eq.1) go to 160

  do I = 1,6
     call DBSSTATS(SP(I),TPA(I),BAREA(I),CFVOL(I),BFVOL(I), &
          & AGBIO(I),AGCARB(I),IDIST(1,I),IDIST(2,I),IDIST(3,I), &
          & IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I), &
          & IDIST(8,I),LABELS(I),2,IYEAR)
  end do

  !     DATABASE OUPUT FOR GENERAL SPECIES SUMMARY

  160 do I = 1,ROWS
     call DBSSTATS(SP(I),TPA(I),BAREA(I),CFVOL(I),BFVOL(I), &
          & AGBIO(I),AGCARB(I),IDIST(1,I),IDIST(2,I),IDIST(3,I), &
          & IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I), &
          & IDIST(8,I),LABELS(I),1,IYEAR)
  end do

  return
end subroutine STATS
