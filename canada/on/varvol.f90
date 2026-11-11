!========================!
!   canada/on : varvol   !
!========================!
! CANADA-ON $Id$
RECURSIVE subroutine VARVOL
  implicit none

  !  THIS SUBROUTINE CALLS THE APPROPRIATE VOLUME CALCULATION ROUTINE
  !  FROM THE NATIONAL CRUISE SYSTEM VOLUME LIBRARY FOR METHB OR METHC
  !  EQUAL TO 6.  IT ALSO CONTAINS ANY OTHER SPECIAL VOLUME CALCULATION
  !  METHOD SPECIFIC TO A VARIANT (METHB OR METHC = 5, 8)

  !  FOR ONTARIO:
  !    VN = TOTAL MERCH CUBIC (PULPWOOD AND SAWLOG) (GTV)
  !    VM = MERCH SAWLOG CUBIC FOOT VOLUME (GMV)
  !    VC = VOLUME WITH CULL, BASED ON GMV
  !    BBFV = NOT USED

  !  THIS SUBROUTINE INCLUDES LOGIC FOR ALL ONTARIO.
  !  NOTE THAT THE NATCRS PART OF THE ROUTINE IS THE SAME AS THE LS\VARVOL

  !  IF **NATCRS** IS CALLED, THIS SUBROUTINE CALLS **R9CLARK**
  !  IF **OCFVOL** IS CALLED, THEN **HONVOL**, **ZAKVOL**, **ZAKHT** ARE CALLED
  !  IF **OBFVOL** IS CALLED, THEN THERE ARE NO CALLS

  !  SCRIBNER BOARD FEET ARA CALCULATED FOR MN (CHIPPEWA AND SUPERIOR),
  !  WI (CHEQUAMEGON-NICOLET), AND UPPER MI (OTTAWA AND HIAWATHA NFS),
  !  AND INTERNATIONAL 1/4 INCH IS USED EVERYWHERE ELSE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'VOLSTD.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'

  ! Variables
  real    :: VOL(15),BOLTHT(21),LOGLEN(7,21),BBFV1,UPSHT1
  logical :: TKILL,CTKFLG,BTKFLG,DEBUG,DONE,LCONE,LTOTAL
  character(len=1) :: CTYPE
  character(len=1) :: HTTYP
  character(len=2) :: PROD
  integer :: I1,I01,I,ISPC,IERR,IT,ITRNC
  real    :: BRATIO,TDIBC,TDIBB,SAWBF,SAWCU,X01,TOTCU
  real    :: VM,VN,BBFV,H,D,BARK,VMAX,X02,X03
  real    :: VM2,ZHT
  real    :: OVM,OVN
  character(len=10) :: EQNC
  character(len=10) :: EQNB

  character(len=3) :: ONEQ(MAXSP)

  !     DEFINE SPECIES DATA ARRAYS
  !     DEFINE WHICH VOLUME EQUATION EACH SPECIES WILL USE.
  data ONEQ/&
       &'ZAK','HON','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK',&
       &'ZAK','HON','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK','ZAK','HON',&
       &'HON','HON','HON','ZAK','HON','ZAK','ZAK','HON','HON','HON',&
       &'HON','HON','HON','HON','HON','HON','HON','HON','HON','ZAK',&
       &'ZAK','ZAK','ZAK','ZAK','HON','ZAK','HON','ZAK','ZAK','ZAK',&
       &'ZAK','ZAK','HON','ZAK','HON','HON','ZAK','HON','ZAK','HON',&
       &'HON','HON','HON','ZAK','ZAK','ZAK','ZAK','HON','ZAK','ZAK',&
       &'ZAK','ZAK'/

  !  DEFINE VARIABLES
  !    EQUATION NUMBERS:
  !        DONE    --LOGICAL OPERATOR TO SIGNIFY ALL VOLUMES HAVE
  !                  BEEN COMPUTED

  !  NATIONAL CRUISE SYSTEM ROUTINES (METHOD = 6)

  entry NATCRS (VN,VM,BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,&
       &CTKFLG,BTKFLG,IT)

  !  CHECK FOR DEBUG

  call DBCHK(DEBUG,'VARVOL',6,ICYC)
  if (DEBUG)write(JOSTND,*)'IN VARVOL, ICYC= ',ICYC

  !  INITIALIZE VOLUME ARRAY TO ZERO

  do I = 1,15
     VOL(I) = 0.
     10 continue
  end do

  DONE = .false.

  !  REAL CONSTANT ARGUMENTS

  UPSHT1 = 0.
  X01 = 0.
  X02 = 2.
  X03 = 0.

  !  INTEGER CONSTANT ARGUMENTS

  IERR = 0
  I01 = 0
  I1 = 1

  !  CHARACTER CONSTANT ARGUMENTS

  CTYPE = 'F'
  HTTYP = 'L'

  !  TREE DBH IS LESS THAN PULPWOOD MIN DBH, OR NO PULPWOOD HT

  if (D.lt.DBHMIN(ISPC)) then

     !  CASE 1: TREE DOES NOT MEET PULPWOOD SPECS

     TOTCU = 0.
     X02 = 0.
     X03 = 0.

     !  TREE DBH IS >PULPWOOD MIN DBH BUT <SAWTIMBER MIN DBH, OR NO SAW HT

  else if (D.lt.BFMIND(ISPC)) then

     !  CASE 2: TREE ONLY HAS PULPWOOD (PROD='02')

     if (DEBUG)write(JOSTND,*)' IT,CASE 2 PWVOL VEQNNC,D,H= ',&
          &IT,VEQNNC(ISPC),D,H

     PROD = '02'
     X02 = 0.
     call R9CLARK(VEQNNC(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,X03,&
          &H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I01,I1,I01,I01,PROD,IERR,&
          &CTYPE,UPSHT1)

     if (DEBUG)write(JOSTND,*)' AFTER CASE 2 PWVOL IERR,VOL=',IERR,VOL

     TOTCU = VOL(4)
     SAWCU = 0.0
     SAWBF = 0.0
     DONE = .true.

     !  TREE DBH IS >SAWTIMBER MIN DBH, POSITIVE SAW HT AND PULP HT

  else

     !  CASE 3: TREE HAS SAWTIMBER AND PULPWOOD (PROD='01' AND TOPWOOD)

     !  IF THE VOLEQNUM KEYWORD WAS USED TO SET DIFFERENT EQ. NOS. FOR CF AND
     !  FOR BF THEN 2 CALLS TO R9CLARK ARE REQUIRED OTHERWISE ONE CALL IS
     !  ENOUGH

     if (DEBUG)write(JOSTND,*)' IT,CASE 3 SBVOL VEQNNC(ISPC),H,D,',&
          &IT,VEQNNC(ISPC),H,D

     PROD = '01'
     call R9CLARK(VEQNNC(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,X03,&
          &H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I1,I1,I01,I1,PROD,IERR,&
          &CTYPE,UPSHT1)

     !  THE NEXT VERSION OF CRUISE PROCESSING WILL USE THE FOLLOWING CONDITIONAL
     !  LOGIC. THE LOGIC APPEARS IN 4 PLACES (10/27/2011)

     !        IF((KODFOR.EQ.903).OR.(KODFOR.EQ.909).OR.(KODFOR.EQ.913).OR.
     !     &     (KODFOR.EQ.907).OR.(KODFOR.EQ.910))THEN
     SAWBF = VOL(2)
     !        ELSE
     !          SAWBF=VOL(10)
     !        ENDIF

     SAWCU = VOL(4)
     TOTCU = SAWCU+VOL(7)
     if (DEBUG)write(JOSTND,*)' AFTER CASE 3(CALL 2)TCVOL IERR,VOL= '&
          &,IERR,VOL
     if (VEQNNB(ISPC).eq.VEQNNC(ISPC)) then
        DONE = .true.
     else
        if (DEBUG)write(JOSTND,*)' IT,CASE 3 SBVOL VEQNNB(ISPC),H,D,',&
             &'FORST= ',IT,VEQNNB(ISPC),H,D
        call R9CLARK(VEQNNB(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,&
             &X03,H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I1,I1,I01,&
             &I1,PROD,IERR,CTYPE,UPSHT1)
        !          IF((KODFOR.EQ.903).OR.(KODFOR.EQ.909).OR.(KODFOR.EQ.913).OR.
        !     &       (KODFOR.EQ.907).OR.(KODFOR.EQ.910))THEN
        SAWBF = VOL(2)
        !          ELSE
        !            SAWBF=VOL(10)
        !          ENDIF
        DONE = .true.
        if (DEBUG)write(JOSTND,*)' AFTER CASE 3 (CALL 2)TCVOL IERR,',&
             &'VOL= ',IERR,VOL
     end if
     if (DEBUG)write(JOSTND,*)' IT,AFTER CASE 3 TCVOL IERR,VOL= ',&
          &IERR,VOL
  end if
  if (D.ge.BFMIND(ISPC)) then
     if (IT.gt.0)HT2TD(IT,1) = X02
  else
     if (IT.gt.0)HT2TD(IT,1) = 0.
  end if
  if (D.ge.DBHMIN(ISPC)) then
     if (IT.gt.0)HT2TD(IT,2) = X03
  else
     if (IT.gt.0)HT2TD(IT,2) = 0.
  end if
  if (DONE)goto 50

  !--TREE DID NOT MEET PULPWOOD SPECS

  if (D .lt. BFMIND(ISPC)) then

     !  CASE 4: TREE DOES NOT MEET SAWTIMBER OR PULPWOOD SPECS

     SAWCU = 0.0
     SAWBF = 0.0
  else

     !  CASE 5: STRANGE CASE SPECS MET AND TREE ONLY HAS SAWTIMBER(PROD='01')
     !  IF THE VOLEQNUM KEYWORD WAS USED TO SET DIFFERENT EQ. NOS. FOR CF AND
     !  FOR BF THEN 2 CALLS TO R9CLARK ARE REQUIRED OTHERWISE ONE CALL IS
     !  ENOUGH

     if (DEBUG)write(JOSTND,*)' IT,CASE 5 R9VOL VEQNNC(ISPC),D= ',&
          &VEQNNC(ISPC),D

     PROD = '01'
     call R9CLARK(VEQNNC(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,&
          &X03,H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I1,I1,I01,I01,&
          &PROD,IERR,CTYPE,UPSHT1)

     if (DEBUG)write(JOSTND,*)' AFTER CASE 5 (CALL 1)R9VOL IERR,VOL=',&
          &IERR,VOL

     SAWCU = VOL(4)
     !        IF((KODFOR.EQ.903).OR.(KODFOR.EQ.909).OR.(KODFOR.EQ.913).OR.
     !     &     (KODFOR.EQ.907).OR.(KODFOR.EQ.910))THEN
     SAWBF = VOL(2)
     !        ELSE
     !          SAWBF=VOL(10)
     !        ENDIF
     if (VEQNNB(ISPC).ne.VEQNNC(ISPC)) then
        if (DEBUG)write(JOSTND,*)' IT,CASE 5 R9VOL VEQNNB(ISPC),D= ',&
             &IT,VEQNNB(ISPC),D
        call R9CLARK(VEQNNB(ISPC),X01,BFTOPD(ISPC),TOPD(ISPC),D,X02,&
             &X03,H,LOGDIA,BOLTHT,LOGLEN,LOGVOL,VOL,I01,I1,I1,I01,I01,PROD,&
             &IERR,CTYPE,UPSHT1)
        !        IF((KODFOR.EQ.903).OR.(KODFOR.EQ.909).OR.(KODFOR.EQ.913).OR.
        !     &     (KODFOR.EQ.907).OR.(KODFOR.EQ.910))THEN
        SAWBF = VOL(2)
        !        ELSE
        !          SAWBF=VOL(10)
        !        ENDIF
        if (DEBUG)write(JOSTND,*)' AFTER CASE 5 (CALL 2)R9VOL IERR,VOL=',&
             &IERR,VOL
     end if
     if (D.ge.BFMIND(ISPC)) then
        if (IT.gt.0)HT2TD(IT,1) = X02
     else
        if (IT.gt.0)HT2TD(IT,1) = 0.
     end if
     if (D.ge.DBHMIN(ISPC)) then
        if (IT.gt.0)HT2TD(IT,2) = X03
     else
        if (IT.gt.0)HT2TD(IT,2) = 0.
     end if
  end if

  !     SET RETURN VALUES.

  50 continue
  if (DEBUG)write(JOSTND,*)' TOTCU=',TOTCU,' SAWCU=',SAWCU,&
       &' SAWBF=',SAWBF
  VN = TOTCU
  VMAX = VN
  VM = SAWCU
  BBFV = SAWBF
  CTKFLG = .true.
  BTKFLG = .true.
  if (VN.le.0.) then
     VN = 0.
     CTKFLG = .false.
  end if
  if (VM.le.0.) then
     VM = 0.
     BTKFLG = .false.
  end if
  if (BBFV.le.0.) then
     BBFV = 0.
     BTKFLG = .false.
  end if
  if (DEBUG)write(JOSTND,*)' RETURNING VN,VMAX,VM,BBFV,IERR= ',&
       &VN,VMAX,VM,BBFV,IERR

  !  VOLUME PROCESSING ERRORS ARE PROCESSED HERE

  if (IERR.eq.12)call ERRGRO(.true.,36)

  return

  !     ENTER ANY OTHER CUBIC HERE

  entry OCFVOL (VN,VM,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,&
       &CTKFLG,IT)

  !     CHECK FOR DEBUG

  call DBCHK(DEBUG,'VARVOL',6,ICYC)
  if (DEBUG)write(JOSTND,*)'IN VARVOL, OCFVOL'

  if (METHC(ISPC).eq.5) then

     !  IF METHC =5 USE GEVORKIANTZ METHOD

     call GVRVOL (VN,VM,BBFV1,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,&
          &CTKFLG,BTKFLG,IT)
  else

     !     SET INITIAL VALUES.  IF DIAMETER LIMITS OR TREE VALUE CLASS NOT MET
     !     THEN RETURN.
     !     (Note, in original varvol, routine would return even if IMC=3.
     !      This has been changed in the Ontario routine to accept IMC=3.
     !           Therefore, IMC(IT) no longer in the IF statement)

     VN = 0.0001
     VM = 0.0001
     VMAX = 0.0
     !      IF (IMC(IT) .GT. 3 .OR. D .LT. DBHMIN(ISPC))GOTO 100
     if (D .lt. DBHMIN(ISPC))goto 100

     !     DETERMINE WHICH TYPE OF VOLUME TO CALCULATE

     if (ONEQ(ISPC) .eq. 'ZAK') then

        !     CALCULATE GTV (= NET CUBIC FOOT VOLUME (VN) = PULPWOOD + SAWLOG)

        LTOTAL = .true.
        call ZAKVOL (VN,ISPC,D,H,H,IT,BARK,LTOTAL)
        if (DEBUG)write(JOSTND,*) 'SPEC=',ISPC,' IT=',IT,' VN=',VN,&
             &' IMC=',IMC(IT),' D=',D,' SI=',SITEAR(ISPC),' VM=',VM

        !     CALCULATE GMV (= MERCH CUBIC FOOT VOLUME (VM) = SAWLOG)

        LTOTAL = .false.
        call ZAKHT  (D,H,BARK,ISPC,IT,ZHT) ! need Ht @ minDBH
        VM = 0.0
        if (ZHT .gt.0.0) then
           call ZAKVOL (VM,ISPC,D,H,ZHT,IT,BARK,LTOTAL)
        end if
     else
        call HONER (VN,VM,ISPC,D,H,IT)
     end if
  end if

  !     SET RETURN VALUES HERE

  100 continue
  VMAX = VN
  CTKFLG = .true.
  if (VN.le.0.) then
     VN = 0.
     CTKFLG = .false.
  end if
  if (VM.le.0.) then
     VM = 0.
  end if

  return

  entry OBFVOL (BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,&
       &BTKFLG,IT)

  !     CALCULATE BOARD FEET (IN FVS-ON THIS IS NET MERCHANTABLE VOLUME: NMV)
  !     BY COMPUTING GMV. TO MINIMIZE CODE CHANGES, THIS REQUIRE RECALCULATING
  !     MERCH CUBIC FEET (= GROSS MERCH VOL) ONCE AGAIN USING CODE COPIED FROM OCFVOL

  !     CHECK FOR DEBUG
  call DBCHK(DEBUG,'VARVOL',6,ICYC)
  if (DEBUG) then
     write(JOSTND,*)'IN OBFVOL ISPC,SI,D,TOPD =', ISPC,SITEAR(ISPC),&
          &D,TOPD(ISPC)
  end if

  if (METHB(ISPC).eq.5) then

     !  GEVORKIANTZ METHOD (METHB=5)

     OVN = 0.0
     OVM = 0.0

     call GVRVOL(OVN,OVM,BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,&
          &CTKFLG,BTKFLG,IT)
  else

     OVN = 0.0001
     OVM = 0.0001
     VMAX = 0.0
     if (IMC(IT) .gt.3.or. D .lt. DBHMIN(ISPC))goto 200

     !     DETERMINE WHICH TYPE OF VOLUME TO CALCULATE

     if (ONEQ(ISPC) .eq. 'ZAK') then

        !     CALCULATE NET CUBIC FOOT VOLUME (VN) == PULPWOOD + SAWLOG

        LTOTAL = .true.
        call ZAKVOL (OVN,ISPC,D,H,H,IT,BARK,LTOTAL)
        if (DEBUG)write(JOSTND,*) 'SPEC=',ISPC,' IT=',IT,' OVN=',OVN,&
             &' IMC=',IMC(IT),' D=',D,' SI=',SITEAR(ISPC),' OVM=',OVM

        !     CALCULATE MERCH CUBIC FOOT VOLUME (VM) == SAWLOG

        LTOTAL = .false.
        call ZAKHT  (D,H,BARK,ISPC,IT,ZHT) ! need Ht @ minDBH
        OVM = 0.0
        if (ZHT .gt.0.0) then
           call ZAKVOL (OVM,ISPC,D,H,ZHT,IT,BARK,LTOTAL)
        end if
     else
        call HONER (OVN,OVM,ISPC,D,H,IT)
     end if

     !     SET RETURN VALUES HERE

     200 continue

     VM2 = MAX(OVM,0.0)
     call MOWRASKI(VM2,IT,ISPC,BBFV)
  end if

  BTKFLG = .true.
  if (BBFV .le.0.0) then
     BBFV = 0.0
     BTKFLG = .false.
  end if
  return

  !  ENTRY POINT FOR SENDING VOLUME EQN NUMBER TO THE FVS-TO-NATCRZ ROUTINE
  !  NOTE: RETURNED EQN NUMBER IS FOR BOARD FOOT SAWTIMBER.
  !        PULPWOOD PORTIONS USE 911 FOR FIRST THREE CHARACTERS;
  !        CUBIC FOOT SAWTIMBER USES 912 FOR FIRST THREE CHARACTERS.

  entry GETEQN(ISPC,D,H,EQNC,EQNB,TDIBC,TDIBB)
  EQNC = VEQNNC(ISPC)
  EQNB = VEQNNB(ISPC)
  TDIBC = TOPD(ISPC)*BRATIO(ISPC,D,H)
  TDIBB = BFTOPD(ISPC)*BRATIO(ISPC,D,H)
  return

end
