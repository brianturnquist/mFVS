!====================================!
!   Compute input stand statistics   !
!====================================!
! BASE $Id$
subroutine STATS
  use VARCFG_MOD, only: CFG_LMETRICVAR
  implicit none

  !  THIS ROUTINE COMPUTES STATISTICS THAT DESCRIBE THE INPUT
  !  DISTRIBUTION OF STAND ATTRIBUTES AMONG SAMPLE PLOTS.  CALLED FROM
  !  **MAIN**.  **TVALUE** IS CALLED TO CALCULATE STUDENT'S T FOR
  !  CONSTRUCTION OF CONFIDENCE INTERVALS.

  !  THE METRIC VARIANTS (LMETRICVAR: BC AND ON) REPORT PER HECTARE, IN
  !  THEIR OWN COLUMN AND ROW ORDER AND WITHOUT BIOMASS OR CARBON:
  !    - SPECIES TABLE: NET MERCHANTABLE VOLUME (THE BFV TOTAL, PRINTED
  !      AS CUBIC METERS), GROSS VOLUME, TREES, BASAL AREA.
  !    - SAMPLE POINT TABLE: TREES, CUBIC METERS, BOARD FEET (EVERY
  !      VALUE MULTIPLIED BY ZERO), BASAL AREA.
  !  THEIR SAMPLE POINT TABLE CONVERTS THE SAMPLING ERROR IN PERCENT AND
  !  NOT THE SAMPLING ERROR IN UNITS. THIS IS KEPT AS IT WAS IN THE FORMER
  !  METRIC COPY OF THIS ROUTINE; SEE FOUND_BUGS.md SECTION 23.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'VOLSTD.f90'
  include 'PLOT.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: NSTAT
  character(len=19) :: LABELS(6),LABIMP(6),LABMET(4)
  character(len=4) :: SP(MAXSP)
  character(len=4) :: SPECCD(MAXSP)
  real    :: TOTCF(MAXSP),SIGLEVEL,TOTTR(MAXSP),TOTBA(MAXSP),TOTBF(MAXSP),SUMT(MAXPLT),SUMBA(MAXPLT), &
       & SUMCF(MAXPLT),SUMBF(MAXPLT),ITPA(MAXSP),IBA(MAXSP),IBF(MAXSP),ICF(MAXSP),TPA(MAXSP),BAREA(MAXSP), &
       & BFVOL(MAXSP),CFVOL(MAXSP),TOTBIO(MAXSP),SUMBIO(MAXPLT),IBIO(MAXSP),AGBIO(MAXSP),TOTCARB(MAXSP), &
       & SUMCARB(MAXPLT),ICARB(MAXSP),AGCARB(MAXSP)
  integer :: IFLG(MAXSP),I,ISPC,J,IALP,IERR,NDF,ROWS,CNT,IYEAR
  real    :: P,TBA,TBF,TCF,T,TBIOMASS,TCARB
  real    :: IDIST(8,6)
  logical :: LPOS
  equivalence (WK3,SUMT),(WK3(MAXPLT+1),SUMBA), &
       & (WK4,SUMCF),(WK4(MAXPLT+1),SUMBF), &
       & (WK5,TOTTR),(WK5(MAXSP+1),TOTBA), &
       & (WK6,TOTCF),(WK6(MAXSP+1),TOTBF)
  data LABIMP/ &
       & 'TREES/ACRE         ', &
       & 'BASAL AREA/ACRE    ', &
       & 'CUBIC FEET/ACRE    ', &
       & 'BOARD FEET/ACRE    ', &
       & 'ABVGRD BIOMASS/ACRE', &
       & 'ABVGRD CARB/ACRE   '/
  data LABMET/ &
       & 'TREES/HA           ', &
       & 'CUBIC METERS/HA    ', &
       & 'BOARD FEET/HA      ', &
       & 'BASAL AREA/HA      '/

  !  IF THERE ARE NO TREE RECORDS, OR STATS OPTION WAS NOT REQUESTED,
  !  RETURN.

  NSTAT = MERGE(4,6,CFG_LMETRICVAR)
  if (.not.LSTATS.or.ITRN.eq.0) return

  !  INITIALIZE.

  if (LMETRICVAR) then
     LABELS(1:4) = LABMET
     LABELS(5:6) = ' '
  else
     LABELS = LABIMP
  end if
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

  !  ACCUMULATE SUMS FOR OVERALL STATISTICS. THE METRIC VARIANTS DO NOT
  !  REPORT BIOMASS OR CARBON, SO THEIR SUMS STAY ZERO.

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
     if (.not.LMETRICVAR) then
        TBIOMASS = (ABVGRD_BIO(I)*P)/2000
        TCARB = (ABVGRD_CARB(I)*P)/2000
        TOTBIO(ISPC) = TOTBIO(ISPC)+TBIOMASS
        TOTCARB(ISPC) = TOTCARB(ISPC)+TCARB
     end if

     IFLG(ISPC) = 1
     if (IPTINV.le.1) go to 30
     SUMT(J) = SUMT(J)+IPTINV*P
     SUMBA(J) = SUMBA(J)+IPTINV*TBA
     SUMBF(J) = SUMBF(J)+IPTINV*TBF
     SUMCF(J) = SUMCF(J)+IPTINV*TCF
     if (.not.LMETRICVAR) then
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
     if (.not.LMETRICVAR) then
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
     write(JOSTND,9102)
  else
     write(JOSTND,9001)
     write(JOSTND,9002)
  end if
  9001 format(T9,'GENERAL SPECIES SUMMARY FOR THE CRUISE', &
       & ' (PER ACRE)')
  9002 format(/2X,'SPECIES',T23,'TREES',T35,'BASAL AREA', &
       & T52,'CUBIC FEET',T69,'BOARD FEET',T86,'ABOVE GROUND BIOMASS', &
       & T113,'ABOVE GROUND CARBON'/,131('-'))
  9101 format(T9,'GENERAL SPECIES SUMMARY FOR THE CRUISE', &
       & ' (PER HECTARE)')
  9102 format(/2X,'SPECIES',T18,'NMV (CU M)',T31,'GTV (CU M)', &
       & T52,'TREES',T61,'BASAL AREA',/,73('-'))
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
     if (LMETRICVAR) then
        write(JOSTND,9103) JSP(I), &
             & NSP(I,1)(1:2), &
             & TOTBF(I)*FT3pACRtoM3pHA, &
             & TOTCF(I)*FT3pACRtoM3pHA, &
             & TOTTR(I)/ACRtoHA, &
             & TOTBA(I)*FT2pACRtoM2pHA
     else
        write(JOSTND,9003) JSP(I),NSP(I,1)(1:2), &
             & TOTTR(I),TOTBA(I),TOTCF(I),TOTBF(I),TOTBIO(I),TOTCARB(I)
     end if
     9003 format(1X,A4,'=',A4,T18,F10.1,T35,F10.1,T52,F10.1,T68,F10.1, &
          & T96,F10.1,T122,F10.1)
     9103 format(1X,A4,'=',A4,T18,F10.1,T31,F10.1,T47,F10.1,T61,F10.1)
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
  else
     write(JOSTND,9006)
  end if
  9006 format('CHARACTERISTIC',T25,'MEAN DEVIATION VARIATION   SIZE', &
       & '     CONFIDENCE  LIMITS    PERCENT     UNITS',/,16('-'), &
       & 3X,9('-'),1X,9('-'),1X,9('-'),1X,6('-'),1X,22('-'),4X,17('-'))
  9106 format(' CHARACTERISTIC',T25,'MEAN DEVIATION VARIATION   SIZE', &
       & '     CONFIDENCE  LIMITS    PERCENT     UNITS',/,16('-'), &
       & 3X,9('-'),1X,9('-'),1X,9('-'),1X,6('-'),1X,22('-'),4X,17('-'))

  !  CALL **TVALUE TO APPROXIMATE 'T' FOR (IPTINV-1) DF AT THE 'ALPHA'
  !  PROBABILITY LEVEL.

  NDF = IPTINV-1
  call TVALUE(NDF,ALPHA,T,IERR)

  !  COMPUTE AND PRINT THE STATISTICS FOR EACH ATTRIBUTE, IN THE ORDER OF
  !  THE LABELS. IF THE BASAL AREA SUM IS ZERO, NOTHING FURTHER IS
  !  PRINTED OR WRITTEN TO THE DATABASE.

  if (LMETRICVAR) then
     call PTSTAT (SUMT,1,1,ACRtoHA,LPOS)
     call PTSTAT (SUMCF,2,2,FT3pACRtoM3pHA,LPOS)
     call PTSTAT (SUMBF,3,3,0.,LPOS)
     call PTSTAT (SUMBA,4,2,FT2pACRtoM2pHA,LPOS)
     if (.not.LPOS) return
     SIGLEVEL = (1-ALPHA)*100
  else
     call PTSTAT (SUMT,1,0,1.,LPOS)
     call PTSTAT (SUMBA,2,0,1.,LPOS)
     if (.not.LPOS) return
     SIGLEVEL = (1-ALPHA)*100
     call PTSTAT (SUMCF,3,0,1.,LPOS)
     call PTSTAT (SUMBF,4,0,1.,LPOS)
     call PTSTAT (SUMBIO,5,0,1.,LPOS)
     call PTSTAT (SUMCARB,6,0,1.,LPOS)
  end if

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
     end if
     AGBIO(CNT) = IBIO(I)
     AGCARB(CNT) = ICARB(I)
  end do
  IYEAR = IY(1)

  !     CALL DBSSTATS FOR POPULATING THE DATABASE WITH CRUISE STATISTICS

  !     DATABASE OUTPUT FOR STAND (REQUIRES MULTIPLE SAMPLE POINTS)

  if (IPTINV.eq.1) go to 160

  do I = 1,NSTAT
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

contains

  subroutine PTSTAT (PSUM,K,MODE,FAC,LPOS)

    !  COMPUTES AND PRINTS THE SAMPLE POINT STATISTICS FOR ONE ATTRIBUTE
    !  AND STORES THEM IN COLUMN K OF IDIST FOR THE DATABASE.

    !  PSUM -- THE ATTRIBUTE'S TOTAL ON EACH SAMPLE POINT
    !  K    -- ROW OF THE PRINTED TABLE: INDEX TO LABELS AND IDIST
    !  MODE -- UNIT CONVERSION OF THE PRINTED AND STORED VALUES:
    !          0 = NONE
    !          1 = MEAN, DEVIATION, LIMITS AND PERCENT ERROR DIVIDED BY FAC
    !          2 = THE SAME FIVE VALUES MULTIPLIED BY FAC
    !          3 = ALL SEVEN REAL VALUES MULTIPLIED BY ZERO
    !  LPOS -- RETURNED FALSE IF THE ATTRIBUTE SUMS TO ZERO OR LESS, IN
    !          WHICH CASE ONLY THE ZERO SUM IS PRINTED

    real    :: PSUM(MAXPLT),FAC
    integer :: K,MODE,IP
    logical :: LPOS
    real    :: SUM,SUMSQ,ST,XBAR,S,SS,SE,UL,UU,CV,SEU,SEP

    SUM = 0.0
    SUMSQ = 0.0
    do IP = 1,IPTINV
       ST = PSUM(IP)/GROSPC
       SUM = SUM+ST
       SUMSQ = SUMSQ+ST*ST
    end do
    LPOS = SUM.gt.0.0
    if (.not.LPOS) then
       write(JOSTND,9007) LABELS(K),SUM,SUM
       return
    end if
    XBAR = SUM/PI
    S = 0.0
    SS = SUMSQ-SUM*SUM/PI
    if (SS.gt.0.0) S = SQRT(SS/(PI-1.0))
    SE = S/SQRT(PI)
    UL = XBAR-T*SE
    if (UL.lt.0.0) UL = 0.0
    UU = XBAR+T*SE
    CV = S/XBAR
    SEU = T*SE
    SEP = SEU*100./XBAR
    select case (MODE)
    case (1)
       XBAR = XBAR/FAC
       S = S/FAC
       UL = UL/FAC
       UU = UU/FAC
       SEP = SEP/FAC
    case (2)
       XBAR = XBAR*FAC
       S = S*FAC
       UL = UL*FAC
       UU = UU*FAC
       SEP = SEP*FAC
    case (3)
       XBAR = XBAR*0.
       S = S*0.
       CV = CV*0.
       UL = UL*0.
       UU = UU*0.
       SEP = SEP*0.
       SEU = SEU*0.
    end select
    write(JOSTND,9007) LABELS(K),XBAR,S,CV,IPTINV,UL,UU,SEP,SEU
    9007 format(A19,T19,3(1X,F9.2),1X,I6,1X,F9.2,4X,F9.2, &
         & 4X,F6.1,1X,F10.1)
    IDIST(1,K) = XBAR
    IDIST(2,K) = S
    IDIST(3,K) = CV
    IDIST(4,K) = IPTINV
    IDIST(5,K) = UL
    IDIST(6,K) = UU
    IDIST(7,K) = SEP
    IDIST(8,K) = SEU

    return
  end subroutine PTSTAT

end subroutine STATS
