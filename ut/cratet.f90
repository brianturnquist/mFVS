!=========================================!
!   Calibrate tree growth relationships   !
!=========================================!
! UT $Id$
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
  include 'GGCOM.f90'

  !  INTERNAL VARIABLES.

  !      KNT2 -- USED TO STORE COUNTS FOR PRINTING IN CONTROL
  !              SUMMARY TABLE.
  !     SPCNT -- USED TO ACCUMULATE NUMBER OF TREES PER ACRE BY
  !              SPECIES AND TREE CLASS FOR CALCULATION OF
  !              INITIAL SPECIES-TREE CLASS COMPOSITION VECTOR.

  ! Variables
  logical :: DEBUG,MISSCR,TKILL
  character(len=4) :: UNDER
  integer :: I,J,II,ISPC,IPTR,I1,I2,I3,K1,K2,K3,K4,JJ,IS,IM,NH,IDM1
  real    :: AX,Q,SUMX,H,D,P,BX,XX,YY,XN,HS,P2,P3,P4,YOUNG,OLD
  integer :: KNT2(MAXSP),KNT(MAXSP)
  real    :: SPCNT(MAXSP,3)
  real    :: D1,D2,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2
  real    :: TEMCCF,DM1,CCFT,CW,SI

  ! SPECIES ORDER FOR UTAH VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
  ! 21=BI, 22=BE, 23=OS, 24=OH

  ! VARIANT EXPANSION:
  ! GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
  ! NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM UT

  !  INITIALIZE INTERNAL VARIABLES:

  data UNDER/'----'/

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'CRATET',6,ICYC)

  !  GROWTH EQUATIONS FOR WB, LM, WF, BS, LP, ES, AF, PP, AND OS WERE FIT
  !  USING SITE INDEX ADJUSTED TO A 50-YEAR AGE BASE. ADJUST THE SITE VALUES
  !  FOR THESE SPECIES TO THIS BASIS.

  !  FIRST NEED AN ESTIMATE OF STAND CCF

  TEMCCF = 0.
  do I = 1,IREC1
     P = PROB(I)
     D = DBH(I)
     DM1 = 0.
     IDM1 = 0
     call CCFCAL(ISP(I),D,DM1,IDM1,P,.false.,CCFT,CW,1)
     TEMCCF = TEMCCF + CCFT
  end do
  if (TEMCCF .lt.125.)TEMCCF = 125.

  if (DEBUG) then
     write(JOSTND,*)'SITEAR BEFORE = '
     write(JOSTND,8998) (NSP(J,1)(1:2),SITEAR(J),J = 1,MAXSP)
     8998 format ((T12,8(A3,'=',F6.0,:,'; '),A,'=',F6.0))
  end if
  do ISPC = 1,MAXSP
     SI = SITEAR(ISPC)
     H = 0.
     select case (ISPC)

        ! SPECIES USING ALEXANDER, TACKLE, & DAHMS 1967, RES NOTE RM-29

     case(1,2,7,23)
        H = 9.89311 - 0.19177*(50.0) + 0.00124*(50.0**2.0)&
             &- 0.00082*(TEMCCF-125.)*SI + 0.01387*(50.0)*SI&
             &- 0.0000455*(50.0**2.0)*SI
        SITEAR(ISPC) = H

        ! SPECIES USING ALEXANDER 1967, RES NOTE RM-32

     case(4,5,8,9)
        H = 4.5 + (2.75780*(SI**0.83312)) * (1.0 - EXP(-0.015701*50.))**&
             &(22.71944*(SI**(-0.63557)))
        SITEAR(ISPC) = H

        ! SPECIES USING MEYER 1961 rev., TECH BULL 630

     case(10)
        H = (3.635794*(SI**0.916307)) / (1.0 + EXP(6.09478 -&
             &(0.96483*ALOG(50.)) - (0.277025*ALOG(SI))))
        SITEAR(ISPC) = H
     end select
     if (DEBUG)write(16,*)'ISPC,SI,TEMCCF,H= ',ISPC,SI,TEMCCF,H
  end do
  if (DEBUG) then
     write(JOSTND,*)'SITEAR AFTER = '
     write(JOSTND,8998) (NSP(J,1)(1:2),SITEAR(J),J = 1,MAXSP)
  end if

  !  END OF SITE ADJUSTMENT SECTION

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

        select case(ISPC)
        case (20:21)
           if (H.le.4.5.or. NH.lt.0.or. D.lt.5.0) go to 70
        case default
           if (H.le.4.5.or. NH.lt.0.or. D.lt.3.0) go to 70
        end select
        K1 = K1+1
        XX = BX / (D + 1.)
        YY = ALOG(H - 4.5)
        SUMX = SUMX + YY - XX
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

     !  IF THERE ARE LESS THAN THREE OBSERVATIONS OR LHTDRG IS FALSE
     !  THEN DUB HEIGHTS USING DEFAULT COEFFICIENTS FOR THIS SPECIES.

     KNT(IPTR) = K3
     if (K1.lt.3.or..not.LHTDRG(ISPC)) go to 100
     XN = real(K1)
     AA(ISPC) = SUMX/XN

     !  IF THE INTERCEPT IS NEGATIVE, USE DEFAULT VALUES.

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

        select case(ISPC)

        case (20:21)

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (.not.LHTDRG(ISPC) .or.&
                &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
              if (ISPC .eq.20) then
                 P2 = 1709.7229
                 P3 = 5.8887
                 P4 = -0.2286
              else
                 P2 = 76.5170
                 P3 = 2.2107
                 P4 = -0.6365
              end if
              if (D .ge.3.) then
                 H = 4.5 + P2 * EXP(-1.*P3*D**P4)
              else
                 H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
              end if
           else
              if (D .lt.5.0) then
                 H = 0.0994 + 4.9767*D
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if
           end if

        case default
           H = EXP(AX+BX/(D+1.0))+4.5
        end select

        if (DEBUG) write(JOSTND,88) AX,BX,D,H
        88 format('CRATET DUBBED HEIGHT: AX,BX,D,H=',4F8.2)
        if (H .lt.4.5)H = 4.5
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

     !  END OF SPECIES LOOP.  PRINT HEIGHT-DIAMETER COEFFICIENTS ON
     !  DEBUG UNIT IF DESIRED.

     140 continue
     if (.not.DEBUG) go to 141
     write(JOSTND,9005) ISPC,AX,BX,IABFLG(ISPC)
     9005 format('HEIGHT-DIAMETER COEFFICIENTS FOR SPECIES ',I2,&
          &':  INTERCEPT=',F10.6,'  SLOPE=',F10.6,'  IABFLG=',I4,&
          &' (9005 CRATET)')

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

        select case(ISPC)

        case (20:21)

           !  USE INVENTORY EQUATIONS IF CALIBRATION OF THE HT-DBH FUNCTION IS TURNED
           !  OFF, OR IF WYKOFF CALIBRATION DID NOT OCCUR.
           !  NOTE: THIS SIMPLIFIES TO IF(IABFLB(ISPC).EQ.1) BUT IS SHOWN IN IT'S
           !        ENTIRITY FOR CLARITY.

           if (.not.LHTDRG(ISPC) .or.&
                &(LHTDRG(ISPC) .and. IABFLG(ISPC).eq.1)) then
              if (ISPC .eq.20) then
                 P2 = 1709.7229
                 P3 = 5.8887
                 P4 = -0.2286
              else
                 P2 = 76.5170
                 P3 = 2.2107
                 P4 = -0.6365
              end if
              if (D .ge.3.) then
                 H = 4.5 + P2 * EXP(-1.*P3*D**P4)
              else
                 H = ((4.5+P2*EXP(-1.*P3*(3.**P4))-4.51)*(D-0.3)/2.7)+4.51
              end if
           else
              if (D .lt.5.0) then
                 H = 0.0994 + 4.9767*D
              else
                 H = EXP(AX+BX/(D+1.0))+4.5
              end if
           end if

        case default
           H = EXP(AX+BX/(D+1.0))+4.5
        end select

        if (H .lt.4.5)H = 4.5
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
  !   BECAUSE DGF USES AVH IN CALCULATION OF DDS.

  call AVHT40

  !  ESTIMATE MISSING TOTAL TREE AGES

  AGERNG = 1000.
  YOUNG = 1000.
  OLD = 0.
  if (DEBUG)write(JOSTND,*)'IN CRATET, CALLING FINDAG'
  do I = 1,ITRN
     H = HT(I)
     if (ABIRTH(I) .le.0.0.or. ABIRTH(I).gt.999.) then
        SITAGE = 0.0
        SITHT = 0.0
        AGMAX = 0.0
        HTMAX = 0.0
        HTMAX2 = 0.0
        ISPC = ISP(I)
        D1 = DBH(I)
        D2 = 0.0
        call FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX,HTMAX,HTMAX2,&
             &DEBUG)
        if (SITAGE .gt.0.)ABIRTH(I) = SITAGE
     end if

     !  EXCLUDE SEEDLINGS FROM EA/UEA DETERMINATION. TREES EXACTLY 4.5'
     !  ARE EXCLUDED BECAUSE WE HAVE THEM AS .1 DBH AND THIS WOULD
     !  PROBABLY MESS UP THE CALCUALTION.  DIXON. 5-2-91.

     if (H .gt.4.5) then
        if (ABIRTH(I).le.1.)go to 210
        if (ABIRTH(I) .lt. YOUNG) YOUNG = ABIRTH(I)
        if (ABIRTH(I) .gt. OLD) OLD = ABIRTH(I)
     end if
     210 continue
  end do

  ! COMPUTE AGE RANGE.

  AGERNG = ABS(OLD - YOUNG)

  !  CALL DGDRIV TO CALIBRATE DIAMETER GROWTH EQUATIONS.

  if (DEBUG) write(JOSTND,*)'CALL DGDRIV FROM CRATET SECOND',&
       &' TIME'
  call DGDRIV

  !  PREPARE INPUT DATA FOR HEIGHT GROWTH MODEL CALIBRATION; IT'S DONE
  !  THE SAME AS THE DIAMETER GROWTH MODEL SEEN ABOVE.

  if (IHTG.eq.0.or. IHTG.eq.2) goto 236
  Q = 1.
  if (IHTG.eq.3) Q = -1.
  do I = 1,ITRN
     if (HTG(I).le.0.) goto 233
     HTG(I) = Q * (HT(I)-HTG(I))
     if (HT(I).le.0.0) HTG(I) = 0.0
     233 continue
  end do
  236 continue

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

  call DENSE

  ! COUNT AND PRINT NUMBER OF MISTLETOE RECORDS

  call MISCNT(KNT)
  write(JOSTND,248) (KNT(I),I = 1,NUMSP)
  248 format(/,'NUMBER OF RECORDS WITH MISTLETOE',T49,11(I4,2X)/)

  !  CALL **SDICHK** TO SEE IF INITIAL STAND SDI IS ABOVE THE SPECIFIED
  !  MAXIMUM SDI.  RESET MAXIMUM SDI IF THIS IS THE CASE.

  call SDICHK

  500 continue
  if (DEBUG)write(JOSTND,510)ICYC
  510 format('LEAVING SUBROUTINE CRATET  CYCLE =',I5)

  return
end
