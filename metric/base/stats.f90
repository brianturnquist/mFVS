!====================================!
!   Compute input stand statistics   !
!====================================!
! METRIC-BASE $Id: stats.f 4010 2022-06-28 22:23:31Z donrobinson $
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
  include 'METRIC.f90'

  ! Variables
  character(len=16) :: LABELS(4)
  character(len=4) :: SP(MAXSP)
  character(len=4) :: SPECCD(MAXSP)
  real    :: TOTCF(MAXSP),SIGLEVEL,TOTTR(MAXSP),TOTBA(MAXSP),TOTBF(MAXSP),SUMT(MAXPLT),SUMBA(MAXPLT),&
       &SUMCF(MAXPLT),SUMBF(MAXPLT),ITPA(MAXSP),IBA(MAXSP),IBF(MAXSP),ICF(MAXSP),TPA(MAXSP),BAREA(MAXSP),&
       &BFVOL(MAXSP),CFVOL(MAXSP)
  integer :: IFLG(MAXSP),I,ISPC,J,IALP,IERR,NDF,ROWS,CNT,IYEAR,K
  real    :: P,TBA,TBF,TCF,T,SUM,SUMSQ,ST,XBAR,S,SS,SE,UL,UU,CV
  real    :: SEU,SEP,SCF,SBF,SBA,IDIST(8,4)
  equivalence (WK3,SUMT),(WK3(MAXPLT+1),SUMBA),&
       &(WK4,SUMCF),(WK4(MAXPLT+1),SUMBF),&
       &(WK5,TOTTR),(WK5(MAXSP+1),TOTBA),&
       &(WK6,TOTCF),(WK6(MAXSP+1),TOTBF)
  data LABELS/&
       &'BOARD FEET/HA   ',&
       &'CUBIC METERS/HA ',&
       &'TREES/HA        ',&
       &'BASAL AREA/HA   '/

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
     TPA(I) = 0.0
     BAREA(I) = 0.0
     BFVOL(I) = 0.0
     CFVOL(I) = 0.0
     1 continue
  end do
  do I = 1,MAXPLT
     SUMT(I) = 0.0
     SUMBA(I) = 0.0
     SUMBF(I) = 0.0
     SUMCF(I) = 0.0
     2 continue
  end do
  do I = 1,8
     do J = 1,4
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
     TOTTR(ISPC) = TOTTR(ISPC)+P
     TOTBA(ISPC) = TOTBA(ISPC)+TBA
     TOTCF(ISPC) = TOTCF(ISPC)+TCF
     TOTBF(ISPC) = TOTBF(ISPC)+TBF
     IFLG(ISPC) = 1
     if (IPTINV.le.1) go to 30
     SUMT(J) = SUMT(J)+IPTINV*P
     SUMBA(J) = SUMBA(J)+IPTINV*TBA
     SUMBF(J) = SUMBF(J)+IPTINV*TBF
     SUMCF(J) = SUMCF(J)+IPTINV*TCF
     30 continue
  end do
  do ISPC = 1,MAXSP
     TOTTR(ISPC) = TOTTR(ISPC)/GROSPC
     TOTBA(ISPC) = TOTBA(ISPC)/GROSPC
     TOTCF(ISPC) = TOTCF(ISPC)/GROSPC
     TOTBF(ISPC) = TOTBF(ISPC)/GROSPC
     35 continue
  end do

  !  WRITE TABLE HEADING FOR GENERAL SPECIES SUMMARY.

  write(JOSTND,9000)
  9000 format(/)
  write(JOSTND,9001)
  9001 format(T9,'GENERAL SPECIES SUMMARY FOR THE CRUISE',&
       &' (PER HECTARE)')
  write(JOSTND,9002)
  9002 format(/2X,'SPECIES',T18,'NMV (CU M)',T31,'GTV (CU M)',&
       &T52,'TREES',T61,'BASAL AREA',/,73('-'))
  ROWS = 0
  do I = 1,MAXSP
     if (IFLG(I).eq.0) go to 40
     ROWS = ROWS+1
     SP(ROWS) = JSP(I)
     ITPA(ROWS) = TOTTR(I)
     IBA(ROWS) = TOTBA(I)
     IBF(ROWS) = TOTBF(I)
     ICF(ROWS) = TOTCF(I)
     write(JOSTND,9003) JSP(I),&
          &NSP(I,1)(1:2),&
          &TOTBF(I)*FT3pACRtoM3pHA,&
          &TOTCF(I)*FT3pACRtoM3pHA,&
          &TOTTR(I)/ACRtoHA,&
          &TOTBA(I)*FT2pACRtoM2pHA
     9003 format(1X,A4,'=',A4,T18,F10.1,T31,F10.1,T47,F10.1,T61,F10.1)
     40 continue
  end do
  if (IPTINV.gt.1) go to 50

  !  1 POINT.  PRINT ERROR MESSAGE AND GO WRITE SPECIES STATS TO DATABASE.

  write(JOSTND,9004)
  9004 format(' DISTRIBUTION OF ATTRIBUTES AMONG SAMPLE POINTS',&
       &' CANNOT BE COMPUTED WITH ONE SAMPLE POINT.'/)
  goto 150
  50 continue
  write(JOSTND,9005)
  9005 format(//T19,'DISTRIBUTION OF STAND ATTRIBUTES AMONG SAMPLE'&
       &,' POINTS'/)
  IALP = INT(100.0-100.0*ALPHA+0.5)
  write(JOSTND,9008)IALP
  9008 format(T31,'STANDARD  COEFF OF SAMPLE',T67,I4,'%',&
       &T83,'SAMPLING ERROR IN')
  write(JOSTND,9006)
  9006 format(' CHARACTERISTIC',T25,'MEAN DEVIATION VARIATION   SIZE',&
       &'     CONFIDENCE  LIMITS    PERCENT     UNITS',/,16('-'),&
       &3X,9('-'),1X,9('-'),1X,9('-'),1X,6('-'),1X,22('-'),4X,17('-'))

  !  CALL **TVALUE TO APPROXIMATE 'T' FOR (IPTINV-1) DF AT THE 'ALPHA'
  !  PROBABILITY LEVEL.

  NDF = IPTINV-1
  call TVALUE(NDF,ALPHA,T,IERR)

  !  COMPUTE AND PRINT STATISTICS FOR TREES PER HECTARE.

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     ST = SUMT(I)/GROSPC
     SUM = SUM+ST
     SUMSQ = SUMSQ+ST*ST
     60 continue
  end do
  if (SUM.gt.0.0) go to 65
  write(JOSTND,9007)  LABELS(3),SUM,SUM
  9007 format(A16,T19,3(1X,F9.2),1X,I6,1X,F9.2,4X,F9.2,&
       &4X,F6.1,1X,F10.1)
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
  write(JOSTND,9007) LABELS(3),&
       &XBAR /ACRtoHA,&
       &S    /ACRtoHA,&
       &CV,&
       &IPTINV,&
       &UL   /ACRtoHA,&
       &UU   /ACRtoHA,&
       &SEP  /ACRtoHA,&
       &SEU
  IDIST(1,1) = XBAR /ACRtoHA
  IDIST(2,1) = S    /ACRtoHA
  IDIST(3,1) = CV
  IDIST(4,1) = IPTINV
  IDIST(5,1) = UL   /ACRtoHA
  IDIST(6,1) = UU   /ACRtoHA
  IDIST(7,1) = SEP  /ACRtoHA
  IDIST(8,1) = SEU
  70 continue

  !  COMPUTE AND PRINT STATISTICS FOR CUBIC METER VOL.

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SCF = SUMCF(I)/GROSPC
     SUM = SUM+SCF
     SUMSQ = SUMSQ+SCF*SCF
     80 continue
  end do
  if (SUM.gt.0.0) go to 85
  write(JOSTND,9007)  LABELS(2),SUM,SUM
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
  write(JOSTND,9007) LABELS(2),&
       &XBAR *FT3pACRtoM3pHA,&
       &S    *FT3pACRtoM3pHA,&
       &CV,&
       &IPTINV,&
       &UL   *FT3pACRtoM3pHA,&
       &UU   *FT3pACRtoM3pHA,&
       &SEP  *FT3pACRtoM3pHA,&
       &SEU
  IDIST(1,2) = XBAR *FT3pACRtoM3pHA
  IDIST(2,2) = S    *FT3pACRtoM3pHA
  IDIST(3,2) = CV
  IDIST(4,2) = IPTINV
  IDIST(5,2) = UL   *FT3pACRtoM3pHA
  IDIST(6,2) = UU   *FT3pACRtoM3pHA
  IDIST(7,2) = SEP  *FT3pACRtoM3pHA
  IDIST(8,2) = SEU
  90 continue

  !  COMPUTE AND PRINT STATISTICS FOR BOARD FOOT VOLUME (IF NOT METRIC).

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SBF = SUMBF(I)/GROSPC
     SUM = SUM+SBF
     SUMSQ = SUMSQ+SBF*SBF
     100 continue
  end do
  if (SUM.gt.0.0) go to 105
  write(JOSTND,9007)  LABELS(1),SUM,SUM
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
  write(JOSTND,9007) LABELS(1),&
       &XBAR *0.,&
       &S    *0.,&
       &CV   *0.,&
       &IPTINV,&
       &UL   *0.,&
       &UU   *0.,&
       &SEP  *0.,&
       &SEU  *0.
  IDIST(1,3) = XBAR *0.
  IDIST(2,3) = S    *0.
  IDIST(3,3) = CV   *0.
  IDIST(4,3) = IPTINV
  IDIST(5,3) = UL   *0.
  IDIST(6,3) = UU   *0.
  IDIST(7,3) = SEP  *0.
  IDIST(8,3) = SEU  *0.
  110 continue

  !  COMPUTE AND PRINT STATISTICS FOR BASAL AREA PER HECTARE.

  SUM = 0.0
  SUMSQ = 0.0
  do I = 1,IPTINV
     SBA = SUMBA(I)/GROSPC
     SUM = SUM+SBA
     SUMSQ = SUMSQ+SBA*SBA
     120 continue
  end do
  if (SUM.gt.0.0) go to 130
  write(JOSTND,9007) LABELS(4),SUM,SUM
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
  write(JOSTND,9007) LABELS(4),&
       &XBAR *FT2pACRtoM2pHA,&
       &S    *FT2pACRtoM2pHA,&
       &CV,&
       &IPTINV,&
       &UL   *FT2pACRtoM2pHA,&
       &UU   *FT2pACRtoM2pHA,&
       &SEP  *FT2pACRtoM2pHA,&
       &SEU
  IDIST(1,4) = XBAR *FT2pACRtoM2pHA
  IDIST(2,4) = S    *FT2pACRtoM2pHA
  IDIST(3,4) = CV
  IDIST(4,4) = IPTINV
  IDIST(5,4) = UL   *FT2pACRtoM2pHA
  IDIST(6,4) = UU   *FT2pACRtoM2pHA
  IDIST(7,4) = SEP  *FT2pACRtoM2pHA
  IDIST(8,4) = SEU

  SIGLEVEL = (1-ALPHA)*100

  !  POPULATE ARRAYS WITH NON-ZERO VALUES

  150 CNT = 0
  do I = 1,MAXSP
     CNT = CNT+1
     SPECCD(CNT) = SP(I)
     TPA(CNT) = ITPA(I)/ACRtoHA
     BAREA(CNT) = IBA(I)*FT2pACRtoM2pHA
     BFVOL(CNT) = IBF(I)*0.
     CFVOL(CNT) = ICF(I)*FT3pACRtoM3pHA
  end do
  IYEAR = IY(1)

  !     CALL DBSSTATS FOR POPULATING THE DATABASE WITH CRUISE STATISTICS

  !     DATABASE OUTPUT FOR STAND (REQUIRES MULTIPLE SAMPLE POINTS)

  if (IPTINV.eq.1) go to 160
  K = 3
  do I = 1,3
     call DBSSTATS(SP(I),TPA(I),BAREA(I),CFVOL(I),&
          &BFVOL(I),IDIST(1,I),IDIST(2,I),IDIST(3,I),&
          &IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I),&
          &IDIST(8,I),LABELS(K),2,IYEAR)
     K = K-1
  end do
  call DBSSTATS(SP(I),TPA(I),BAREA(I),CFVOL(I),&
       &BFVOL(I),IDIST(1,I),IDIST(2,I),IDIST(3,I),&
       &IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I),&
       &IDIST(8,I),LABELS(4),2,IYEAR)

  !     DATABASE OUPUT FOR GENERAL SPECIES SUMMARY

  160 do I = 1,ROWS
     call DBSSTATS(SP(I),TPA(I),BAREA(I),CFVOL(I),&
          &BFVOL(I),IDIST(1,I),IDIST(2,I),IDIST(3,I),&
          &IDIST(4,I),SIGLEVEL,IDIST(5,I),IDIST(6,I),IDIST(7,I),&
          &IDIST(8,I),LABELS(I),1,IYEAR)
  end do

  return
end
