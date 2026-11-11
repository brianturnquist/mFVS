!=========================================!
!   Calibrate tree growth relationships   !
!=========================================!
! NE $Id$
subroutine CRATET
  implicit none

  !  THIS SUBROUTINE IS CALLED PRIOR TO PROJECTION.  IT HAS THE
  !  FOLLOWING FUNCTIONS:

  !    1)  CALL **RCON** TO LOAD SITE DEPENDENT MODEL COEFFICIENTS.
  !    2)  REGRESSION TO ESTIMATE COEFFICIENTS OF LOCAL HEIGHT-
  !        DIAMETER RELATIONSHIP.
  !    3)  DUB IN MISSING HEIGHTS.
  !    4)  CALL **DENSE** TO COMPUTE STAND DENSITY.
  !    5)  SCALE CROWN RATIOS AND CALL **CROWN** TO DUB IN ANY MISSING
  !        VALUES.
  !    6)  DEFINE DG BASED ON CALIBRATION CONTROL PARAMETERS AND
  !        CALL **DGDRIV** TO CALIBRATE DIAMETER GROWTH EQUATIONS.
  !    7)  DELETE DEAD TREES FROM INPUT TREE LIST AND REALIGN IND1
  !        AND ISCT.
  !    8)  PRINT A TABLE DESCRIBING CONTROL PARAMETERS AND INPUT
  !        VARIABLES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'HTCAL.f90'
  include 'VARCOM.f90'

  !  INTERNAL VARIABLES.

  !      KNT2 -- USED TO STORE COUNTS FOR PRINTING IN CONTROL
  !              SUMMARY TABLE.
  !     SPCNT -- USED TO ACCUMULATE NUMBER OF TREES PER ACRE BY
  !              SPECIES AND TREE CLASS FOR CALCULATION OF
  !              INITIAL SPECIES-TREE CLASS COMPOSITION VECTOR.

  ! Variables
  logical :: DEBUG,MISSCR,TKILL
  character(len=4) :: UNDER
  integer :: IMR,IM,IS,JJ,NH,K1,K2,K3,K4,I1,I2,I3,IPTR,ISPC,II,I,J
  integer :: KNT2(MAXSP),KNT(MAXSP)
  real    :: SPCNT(MAXSP,3)
  real    :: D2,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2
  real    :: Q,SUMX,H,D,BX,XX,YY,XN,HS,AX

  !  INITIALIZE INTERNAL VARIABLES:

  data UNDER/'----'/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CRATET',6,ICYC)

  !  IF THERE ARE TREE RECORDS, BRANCH TO PREFORM CALIBRATION.

  AX = 0.
  if (ITRN.gt.0) goto 1

  !   CALL MAICAL TO CALCULATE MAI

  call MAICAL
  call RCON
  ONTREM(7) = 0.
  call DENSE
  call DGDRIV
  call REGENT(.false.,1)
  1 continue
  do I = 1,MAXSP
     SPCNT(I,1) = 0.0
     SPCNT(I,2) = 0.0
     SPCNT(I,3) = 0.0
     if (ISCT(I,1).eq.0) goto 5
     J = IREF(I)
     IUSED(J) = NSP(I,1)
     5 continue
  end do
  if ((ITRN.le.0).and.(IREC2.ge.MAXTP1))go to 245

  !  PRINT SPECIES LABELS AND NUMBER OF OBSERVATIONS IN CONTROL
  !  TABLE.  THEN, RESET COUNTERS TO ZERO.

  write(JOSTND,'(//''CALIBRATION STATISTICS:''//)')
  write(JOSTND,9000) (IUSED(I), I = 1,NUMSP)
  9000 format ((T49,11(1X,A2,3X)/))
  if (NUMSP .le.11) then
     write(JOSTND,9001) (UNDER, I = 1,NUMSP)
  else
     write(JOSTND,9001) (UNDER, I = 1,11)
  end if
  9001 format (T49,11(A4,2X))
  write(JOSTND,9002) (KOUNT(I), I = 1,NUMSP)
  9002 format(/,'NUMBER OF RECORDS PER SPECIES',&
       &((T49,11(I4,2X)/)))
  do I = 1,MAXSP
     KNT(I) = 0
     KNT2(I) = 0
     10 continue
  end do

  !   CALL MBACAL TO IDENTIFY SITE SPECIES

  call MBACAL

  !   CALL MAICAL TO CALCULATE MAI

  call MAICAL

  !  CALL **RCON** TO INITIALIZE SITE DEPENDENT MODEL COEFFICIENTS.

  call RCON

  !  CALL **RDPSRT** AND **DENSE** TO COMPUTE INITIAL STAND DENSITY
  !  STATISTICS.  ONTREM(7) IS SET TO ZERO HERE TO ASSURE THAT RELDM1
  !  WILL BE ASSIGNED IN **DENSE** IN THE FIRST CYCLE.

  do I = 1,ITRN
     IND(I) = IND1(I)
     15 continue
  end do
  call RDPSRT(ITRN,DBH,IND,.false.)
  ONTREM(7) = 0.0

  !  PREPARE INPUT DATA FOR DIAMETER GROWTH MODEL CALIBRATION.  IF
  !  IDG IS 1, CONVERT THE PAST DIAMETER MEASUREMENT CARRIED IN DG TO
  !  DIAMETER GROWTH.  IF IDG IS 3, CONVERT THE CURRENT DIAMETER
  !  MEASUREMENT CARRIED IN DG TO DIAMETER GROWTH.  ACTUAL DIAMETER
  !  INCREMENT MEASUREMENTS WILL BE CORRECTED FOR BARK GROWTH IN
  !  THE CALIBRATION ROUTINE. (THIS CODE WAS MOVED HERE SO THAT THE
  !  BACKDATING ALGORITHM IN **DENSE**, INVOKED DURING CALIBRATION,
  !  IS CORRECT.)

  Q = 1.0
  if (IDG.eq.3) Q = -1.0
  do II = 1,ITRN
     I = IND1(II)
     if (I.ge.IREC2) go to 230
     if (DG(I).le.0.0) go to 220
     if (IDG.eq.0.or.IDG.eq.2) go to 230
     DG(I) = Q*(DBH(I)-DG(I))
     go to 230
     220 continue
     DG(I) = -1.0
     230 continue
  end do

  !  SET LBKDEN TRUE IF DIAMETERS ARE TO BE BACKDATED FOR DENSITY
  !  CALCULATIONS.  AFTER THIS CALL TO DENSE, INSURE LBKDEN=FALSE.

  LBKDEN = IDG.lt.2
  call DENSE
  LBKDEN = .false.

  !  DELETE NON-PROJECTABLE RECORDS, AND REALIGN POINTERS TO THE
  !  SPECIES ORDERED SORT.

  if (IREC2.eq.MAXTP1) go to 60
  do I = IREC2,MAXTRE
     ISPC = ISP(I)
     IPTR = IREF(ISPC)
     if (IMC(I).eq.7)KNT(IPTR) = KNT(IPTR)+1
     if (DEBUG) write(JOSTND,9003) I,IMC(I),ISPC
     9003 format('IN CRATET: DEAD TREE RECORD:  I=',I4,',  IMC=',I2,&
          &',  SPECIES=',I2,' (9003 CRATET)')
     if (ITRN.gt.0) then
        I1 = ISCT(ISPC,1)
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           if (IND1(I3).eq.I) go to 40
           30 continue
        end do
        40 IND1(I3) = IND1(I2)
        ISCT(ISPC,2) = I2-1
        if (ISCT(ISPC,2).ge.ISCT(ISPC,1)) go to 50
        ISCT(ISPC,1) = 0
        ISCT(ISPC,2) = 0
     end if
     50 continue
  end do

  !  WRITE CALIBRATION TABLE ENTRY FOR NON-PROJECTABLE RECORDS AND RESET
  !  KNT ARRAY TO ZERO.

  write(JOSTND,9004) (KNT(I),I = 1,NUMSP)
  9004 format(/,'NUMBER OF RECORDS CODED AS RECENT MORTALITY',&
       &((T49,11(I4,2X)/)))

  !  RESET TREE RECORD COUNTERS AND SAVE THE NUMBER OF SPECIES.

  ITRN = IREC1
  if ((ITRN.le.0).and.(IREC2.ge.MAXTP1))goto 245
  if (ITRN.le.0)goto 60
  ISPC = NUMSP

  !  MAKE SURE THAT ALL THE SPECIES ORDER SORTS AND THE IND2 ARRAY
  !  ARE IN THE PROPER ORDER. FIRST, SAVE THE SPECIES REFERENCES.

  do I = 1,MAXSP
     KNT(I) = IREF(I)
     51 continue
  end do
  call SPESRT

  !  IF THE NUMBER OF SPECIES HAS CHANGED, WE MUST REWRITE THE
  !  COLUMN HEADINGS.

  if (ISPC.ne.NUMSP) then
     write(JOSTND,52)
     52 format (/'***** NOTE:  SPECIES HAVE BEEN DROPPED.')
     do I = 1,MAXSP
        if (ISCT(I,1).eq.0) goto 55
        J = IREF(I)
        IUSED(J) = NSP(I,1)
        55 continue
     end do
     write(JOSTND,9000) (IUSED(I), I = 1, NUMSP)
     write(JOSTND,9001) (UNDER, I = 1, NUMSP)
  else

     !        RESET THE REFERENCES.

     do I = 1,MAXSP
        IREF(I) = KNT(I)
        57 continue
     end do
  end if

  !  SORT REMAINING TREE RECORDS IN ORDER OF DESCENDING DIAMETER.
  !  STORE POINTERS TO SORTED ORDER IN IND.

  call RDPSRT(ITRN,DBH,IND,.true.)
  60 continue
  do I = 1,MAXSP
     KNT(I) = 0
     65 continue
  end do

  !  ENTER LOOP TO ADJUST HEIGHT-DBH MODEL FOR LOCAL CONDITIONS.  IF
  !  THERE ARE 3 OR MORE TREES WITH MEASURED HEIGHTS FOR A GIVEN
  !  SPECIES, ADJUST THE INTERCEPT (ASYMPTOTE) IN THE MODEL SO THAT
  !  THE MEAN RESIDUAL FOR THE MEASURED TREES IS ZERO.
  !  IF LHTDRG IS FALSE FOR A GIVEN SPECIES THEN ALL DUBBING IS DONE WITH
  !  DEFAULT VALUES.

  do ISPC = 1,MAXSP
     AA(ISPC) = 0.
     BB(ISPC) = 0.
     I1 = ISCT(ISPC,1)
     if (I1.le.0) go to 141
     I2 = ISCT(ISPC,2)
     IPTR = IREF(ISPC)

     !  INITIALIZE SUMS FOR THIS SPECIES.

     K1 = 0
     K2 = 0
     K3 = 0
     K4 = 0
     SUMX = 0.0

     !  ENTER TREE LOOP WITHIN SPECIES.

     do I3 = I1,I2
        I = IND1(I3)
        H = HT(I)
        NH = NORMHT(I)
        D = DBH(I)
        BX = HT2(ISPC)

        !  BYPASS SUMS FOR TREES WITH MISSING HEIGHT OR TRUNCATED TOPS.

        if (H.le.4.5.or. NH.lt.0.or. D.lt.3.0) go to 70
        K1 = K1+1
        XX = BX/(D+1.)
        YY = ALOG(H-4.5)
        SUMX = SUMX+YY-XX
        go to 80

        !  COUNT NUMBER OF MISSING HEIGHTS AND BROKEN OR DEAD TOPS.  LOAD THE
        !  ARRAY IND2 WITH SUBSCRIPTS TO THE RECORDS WITH MISSING HEIGHTS.

        70 continue
        if (NH.lt.0) K3 = K3+1
        if (H.gt.0.0.and. NH.eq.0) go to 80
        K2 = K2+1
        IND2(K2) = I

        !  END OF SUMMATION LOOP FOR THIS SPECIES.

        !*** ONE LINE FIX FOR GROWTH METHOD 1 PROBLEM. DIXON 11-16-90.
        if (HT(I) .le.0.1) HTG(I) = 0.0
        80 continue
     end do

     !  IF THERE ARE LESS THAN THREE OBSERVATIONS OR LHTDRG IS FALSE THEN
     !   DUB HEIGHTS USING DEFAULT COEFFICIENTS FOR THIS SPECIES.

     KNT(IPTR) = K3
     if (K1.lt.3.or..not. LHTDRG(ISPC)) go to 100
     XN = FLOAT(K1)
     AA(ISPC) = SUMX/XN

     !  IF THE INTERCEPT IS NEGATIVE, USE THE DEFAULT VALUE.

     if (AA(ISPC).ge.0.0) then
        IABFLG(ISPC) = 0
     end if

     !  DUB IN MISSING HEIGHTS.
     !  A VALUE LESS THAN ZERO STORED IN 'HT' => THAT TREE WAS TOP KILLED.
     !  CONSEQUENTLY, A VALUE OF 80% OF THE PREDICTED HEIGHT IS STORED AS
     !  THE TRUNCATED HEIGHT.

     100 continue
     AX = HT1(ISPC)
     BX = HT2(ISPC)
     if (IABFLG(ISPC) .eq.0) AX = AA(ISPC)
     if (K2.eq.0) go to 140
     do JJ = 1,K2
        II = IND2(JJ)
        D = DBH(II)
        TKILL = NORMHT(II) .lt.0

        if (D .le.0.1) then
           H = 1.01
           go to 115
        end if

        H = EXP(AX+BX/(D+1.0))+4.5

        if (DEBUG) write(JOSTND,88) AX,BX,D,H
        88 format('CRATET DUBBED HEIGHT: AX,BX,D,H=',4F8.2)

        !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
        !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
        !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
        !        ENTIRITY FOR CLARITY.

        if (.not.LHTDRG(ISPC) .or.&
             &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
           call HTDBH (IFOR,ISPC,D,H,0)
           if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,H= '&
                &,IFOR,ISPC,D,H
        end if

        if (H .lt.4.5) H = 4.5
        115 continue
        if (TKILL) go to 120
        HT(II) = H
        K4 = K4+1
        go to 125
        120 continue
        NORMHT(II) = INT(H*100.0+0.5)
        if (ITRUNC(II).eq.0) then
           if (HT(II).gt.0.0) then
              ITRUNC(II) = INT(80.0*HT(II)+0.5)
           else
              ITRUNC(II) = INT(80.0*H+0.5)
              HT(II) = H
           end if
        else
           if (HT(II).gt.0.0) then
              if (HT(II).lt.(real(ITRUNC(II))*0.01))&
                   &HT(II) = real(ITRUNC(II))*0.01
           else
              HT(II) = real(ITRUNC(II))*0.01
           end if
        end if
        if (real(NORMHT(II))*0.01.lt.HT(II)) NORMHT(II) = INT(HT(II)*100.0)
        125 continue
        130 continue
     end do
     KNT2(IPTR) = K4

     !  END OF SPECIES LOOP.

     140 continue
     if (DEBUG) then
        write(JOSTND,9005) ISPC,AX,BX,IABFLG(ISPC)
        9005 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
             &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,'  FLAG=',I3,&
             &' (9005 CRATET)')
     end if

     !  LOOP THROUGH DEAD TREES AND DUB MISSING HEIGHTS FOR THIS SPECIES.

     141 continue
     if (IREC2.gt. MAXTRE) go to 150
     do II = IREC2,MAXTRE
        if (ISP(II).ne.ISPC) go to 145
        AX = HT1(ISPC)
        BX = HT2(ISPC)
        if (IABFLG(ISPC) .eq.0) AX = AA(ISPC)
        D = DBH(II)
        TKILL = NORMHT(II) .lt.0
        if (HT(II).gt.0..and. TKILL) go to 142
        if (HT(II).gt.0.) go to 146

        if (D .le.0.1) then
           H = 1.01
           go to 144
        end if

        H = EXP(AX+BX/(D+1.0))+4.5

        !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
        !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
        !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
        !        ENTIRITY FOR CLARITY.

        if (.not.LHTDRG(ISPC) .or.&
             &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
           call HTDBH (IFOR,ISPC,D,H,0)
           if (DEBUG)write(JOSTND,*)'INVENTORY EQN DUBBING IFOR,ISPC,D,H= '&
                &,IFOR,ISPC,D,H
        end if

        if (H .lt.4.5) H = 4.5
        144 continue
        if (TKILL) go to 142
        HT(II) = H
        go to 146
        142 continue
        if (HT(II) .gt.0.) then
           NORMHT(II) = INT(HT(II)*100.0+0.5)
        else
           NORMHT(II) = INT(H*100.0+0.5)
        end if
        if (ITRUNC(II).eq.0) then
           if (HT(II).gt.0.0) then
              ITRUNC(II) = INT(80.0*HT(II)+0.5)
           else
              ITRUNC(II) = INT(80.0*H+0.5)
              HT(II) = H
           end if
        else
           if (HT(II).gt.0.0) then
              if (HT(II).lt.(real(ITRUNC(II))*0.01))&
                   &HT(II) = real(ITRUNC(II))*0.01
           else
              HT(II) = real(ITRUNC(II))*0.01
           end if
        end if
        if (real(NORMHT(II))*0.01.lt.HT(II)) NORMHT(II) = INT(HT(II)*100.0)

        !   CALL FIRE SNAG MODEL TO ADD THE DEAD TREES TO THE
        !   SNAG LIST; DEFLATE PROB(II), WHICH WAS TEMPORARILY
        !   ADJUSTED TO ALLOW BACKDATING FOR CALIBRATION PURPOSES,
        !   IN **NOTRE**

        146 continue
        if (TKILL) then
           HS = real(ITRUNC(II)) * 0.01
        else
           HS = HT(II)
        end if
        call FMSSEE (II,ISPC,D,HS,&
             &(PROB(II)/(FINT/FINTM)),3,.false.,JOSTND)

        145 continue
     end do

     150 continue
  end do

  !  END OF HEIGHT DUBBING SEGMENT.  PRINT CONTROL TABLE ENTRIES FOR
  !  USEABLE HEIGHTS AND MISSING HEIGHTS, AND REINITIALIZE COUNTERS.

  write(JOSTND,9006) (KNT2(I),I = 1,NUMSP)
  9006 format(/,'NUMBER OF RECORDS WITH MISSING HEIGHTS',&
       &((T49,11(I4,2X)/)))
  write(JOSTND,9007) (KNT(I),I = 1,NUMSP)
  9007 format(/,'NUMBER OF RECORDS WITH BROKEN OR DEAD TOPS',&
       &((T49,11(I4,2X)/)))
  do I = 1,MAXSP
     KNT(I) = 0
     KNT2(I) = 0
     160 continue
  end do

  !  CHECK FOR MISSING CROWNS ON LIVE TREES.
  !  SAVE PCT IN OLDPCT TO RETAIN AN OLD PCTILE VALUE.

  MISSCR = .false.
  do I = 1,ITRN
     OLDPCT(I) = PCT(I)
     if (ICR(I).le.0) then
        MISSCR = .true.
        ISPC = ISP(I)
        IPTR = IREF(ISPC)
        KNT2(IPTR) = KNT2(IPTR)+1
     end if
     if (ITRE(I).le.0) ITRE(I) = 9999
     190 continue
  end do

  !  CHECK FOR MISSING CROWNS ON CYCLE 0 DEAD TREES.

  if (IREC2.gt. MAXTRE) go to 192
  do I = IREC2,MAXTRE
     if (ICR(I).le.0)MISSCR = .true.
     191 continue
  end do
  192 continue

  !  CALL **CROWN** IF ANY CROWN RATIOS ARE MISSING.

  if (MISSCR)call CROWN

  !  PRINT CONTROL TABLE ENTRY FOR MISSING CROWN RATIOS; RESET COUNTERS.

  write(JOSTND,9008) (KNT2(I),I = 1,NUMSP)
  9008 format(/,'NUMBER OF RECORDS WITH MISSING CROWN RATIOS',&
       &((T49,11(I4,2X)/)))
  do I = 1,MAXSP
     KNT2(I) = 0
     200 continue
  end do

  !   CALL AVHT40 TO CALCULATE AVERAGE HEIGHT. THIS CALL IS NEEDED
  !   IN SORNEC BECAUSE DGF ROUTINE USES AVH IN CALCULATION OF DDS.

  call AVHT40

  !  CALL DGDRIV TO CALIBRATE DIAMETER GROWTH EQUATIONS.

  if (DEBUG)write(JOSTND,*)'CALL DGDRIV FROM CRATET SECOND TIME'
  call DGDRIV

  !  PREPARE INPUT DATA FOR HEIGHT GROWTH MODEL CALIBRATION; IT'S DONE
  !  THE SAME AS THE DIAMETER GROWTH MODEL SEEN ABOVE.

  if (IHTG.eq.0.or. IHTG.eq.2) goto 236
  Q = 1.
  if (IHTG.eq.3) Q = -1.
  do I = 1,ITRN
     if (HTG(I).le.0.) goto 233
     HTG(I) = Q * (HT(I)-HTG(I))
     if (HT(I) .le.0.0) HTG(I) = 0.0
     233 continue
  end do
  236 continue

  !  ESTIMATE MISSING TREE AGE

  if (DEBUG)write(JOSTND,*)'IN CRATET, CALLING FINDAG'
  do I = 1,ITRN
     if (ABIRTH(I) .le.0.) then
        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        ISPC = ISP(I)
        D = DBH(I)
        H = HT(I)
        D2 = 0.0
        call FINDAG(I,ISPC,D,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
             &DEBUG)
        if (SITAGE .gt.0.)ABIRTH(I) = SITAGE
     end if
  end do

  !  CALL REGENT TO CALIBRATE THE SMALL TREE HEIGHT INCREMENT MODEL.

  call REGENT(.false.,1)

  !  LOAD SPCNT WITH NUMBER OF TREES PER ACRE BY SPECIES AND TREE
  !  CLASS.

  do I = 1,ITRN
     IS = ISP(I)
     IM = IMC(I)
     SPCNT(IS,IM) = SPCNT(IS,IM)+PROB(I)
     240 continue
  end do

  !  COMPUTE DISTRIBUTION OF TREES PER ACRE AND SPECIES-TREE CLASS
  !  COMPOSITION BY TREES PER ACRE.

  245 continue
  call PCTILE(ITRN,IND,PROB,WK3,ONTCUR(7))
  call DIST(ITRN,ONTCUR,WK3)
  call COMP(OSPCT,IOSPCT,SPCNT)
  if (ITRN.le.0) go to 500

  !  CALL **DENSE** TO CALCULATE STAND DENSITY STATISTICS FOR
  !  INITIAL INVENTORY.

  if (DEBUG) write(JOSTND,9013) ICYC
  9013 format('CALLING DENSE, CYCLE=',I2)
  call DENSE

  ! COUNT AND PRINT NUMBER OF MISTLETOE RECORDS

  do I = 1,MAXSP
     KNT(I) = 0
     246 continue
  end do
  do I = 1,ITRN
     call MISGET(I,IMR)
     if (IMR .eq.0)go to 247
     IPTR = IREF(ISP(I))
     KNT(IPTR) = KNT(IPTR) + 1
     247 continue
  end do
  write(JOSTND,248) (KNT(I),I = 1,NUMSP)
  248 format(/,'NUMBER OF RECORDS WITH MISTLETOE',((T49,11(I4,2X)/)))

  !  CALL **SDICHK** TO SEE IF INITIAL STAND SDI IS ABOVE THE SPECIFIED
  !  MAXIMUM SDI.  RESET MAXIMUM SDI IF THIS IS THE CASE.

  call SDICHK

  500 continue
  if (DEBUG)write(JOSTND,510)ICYC
  510 format('LEAVING SUBROUTINE CRATET  CYCLE =',I5)

  return
end
