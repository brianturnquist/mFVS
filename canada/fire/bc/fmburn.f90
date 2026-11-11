!=============================!
!   canada/fire/bc : fmburn   !
!=============================!
! CANADA-FIRE-BC $Id$
subroutine FMBURN (IYR, FMD, LNMOUT)
  implicit none

  !     CALLED FROM: FMMAIN
  !     CALLS    FMCFMD
  !              FMCFIR
  !              FMCONS
  !              FMEFF
  !              FMFINT
  !              FMFOUT
  !              FMMOIS
  !              FMPOFL
  !              FMCFMD3

  !  PURPOSE:
  !     THIS SUBROUTINE IS THE DRIVING PART OF THE FIRE INTENSITY AND EFFECTS
  !     SUBMODEL. IT FIRST DETERMINES THE ENVIRONMENTAL VARIABLES, THEN
  !     SELECTS THE APPROPRIATE FUEL MODEL. NEXT, ALL THE OTHER SUBROUTINES
  !     ARE CALLED TO DETERMINE PROB OF MORT.
  !     THIS IS BASED ON INFORMATION FROM JIM BROWN AND ELIZABETH REINHARDT.

  !  CALL LIST DEFINITIONS:
  !     IYR:  CURRENT YEAR
  !     FMD:  FUEL MODEL THAT IS USED IN THE STATIC CASE
  !     LNMOUT: TRUE IF NORMAL OUTPUT PROCESS, FALSE TO SUPPRESS ALL OUTPUTS

  !  LOCAL VARIABLE DEFINITIONS:
  !     FMOIS:  FUEL MOISTURE CODES
  !     SCH:     SCORCH HEIGHT (IN FEET)
  !     STLAST: LAST STAND THAT COULD BE CALLED
  !     HPA:    HEAT PER UNIT AREA
  !     IFTYPE = 1 IF USER USED FLAMEADJ KEYWORD
  !     USRFL = TRUE IS USER ENTERED FLAME LENGTH ON FLAMEADJ KEYWORD
  !     MKODE = MORTALITY CODE (0=TURN OFF FFE MORTALITY, 1=FFE ESTIMATES MORTALITY)
  !     PSBURN = PERCENTAGE OF THE STAND THAT IS BURNED

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'

  !     VARIABLE DECLARATIONS.

  ! Variables
  integer :: FMOIS,FMD,IFMD,I,J,INL,INDD,IRTNCD
  real    :: FLMULT
  real    :: OLDFL
  real    :: FPRMS(4),CPRMS(6),MPRMS(7),DPRMS(1),PRMS(13)
  real    :: TMP2(3),TMP3(3)
  real    :: SUMPS,XSUR(2,3),XFML(2,3),XDEP,XEXT,ROS
  character(len=8) :: CFTMP
  integer :: MYACTS(5)
  logical :: DEBUG,LOK,LNMOUT,USRFL,LCFTMP
  data     MYACTS/2505,2506,2507,2529,2539/
  integer :: IYR,ICOND,ITODO,IFT,NPRM,IACTK,JYR,IFTYPE,JTODO,IFC,KTODO,JDO,IFIRE,MKODE
  real    :: SWIND,FLAME,WMULT,ALGSLP,BYRAM,CRRATE,CRTCBH,PSMOKE,PMRT,PVOLKL,HPA,FINTEN,FLB,FLT,&
       &TMPINT,TMPFLAME,MAXINT,MININT,UCRBURN,PSBURN
  integer :: CANBURN,FCLS
  real    :: INTSTY

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMBURN',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC, IYR
  7 format(' FMBURN CYCLE=',I2,' IYR=',I5)

  !  IF NO FIRES ARE SCHEDULED OR NO TREES ARE IN THE STAND, BUT
  !  IF FUEL BURNING HAS BEEN DONE THEN CALL THE ROUTINE WHICH
  !  PRINTS THE FIRE REPORTS.

  ICOND = 0
  call OPFIND(1,MYACTS(2),ITODO)
  if (ITODO.gt.0) then
     do IFT = 1,ITODO
        call OPGET(IFT,3,JYR,IACTK,NPRM,CPRMS)
        !          IF (JYR .NE. IYR) GO TO 422
        ICOND = 1
        422 continue
     end do
  end if
  ! This is an inecessary call to have the fire reports printed for the PILEBURN keyword. M.S. 2/4/20
  !      IF ((ICOND .EQ. 0 .OR. COVTYP .LE. 0) .AND. (LFLBRN)) THEN
  !      CALL FMFOUT(IYR,0.0,0, 0,'FUELBURN')
  !      ENDIF

  !  CHECK TO SEE IF THERE IS A DROUGHT STATE; THIS
  !  AFFECTS THE FUEL MODEL IN SOME VARIANTS (UT/CR/LS)

  call OPFIND(1,MYACTS(4),ITODO)
  if (ITODO.eq.0) goto 406
  do JDO = 1,ITODO
     call OPGET(JDO,1,JYR,IACTK,NPRM,DPRMS)
     !        IF (JYR .EQ. IYR) THEN
     call OPDONE(JDO,IYR)
     IDRYB = JYR
     IDRYE = INT(real(JYR) + DPRMS(1) - 1.)
     goto 406
     !        ENDIF
  end do
  406 continue

  !  CHECK TO SEE IF THE USER HAS ALTERED THE DEFINITION
  !  OF THE FUEL MODEL PARAMETERS THIS YEAR.
  !  THIS IS CALLED HERE RATHER THAN IN FMGFMV BECAUSE
  !  WE ONLY WANT TO HAVE TO CHECK THIS ONCE IN A YEAR
  !  (AND FMGFMV CAN BE CALLED MANY TIMES).

  call OPFIND(1,MYACTS(5),ITODO)
  if (ITODO.eq.0) goto 399
  do JDO = 1,ITODO
     call OPGET(JDO,13,JYR,IACTK,NPRM,PRMS)
     !        IF (JYR .EQ. IYR) THEN
     LOK = .true.
     SUMPS = 0.0
     INL = 0
     INDD = 0
     do I = 1, 2 ! ZERO TEMP COPIES
        do J = 1, 3
           XSUR(I,J) = 0.0
           XFML(I,J) = 0.0
        end do
     end do

     IFMD = INT(PRMS(1))
     if (IFMD .lt.1.or. IFMD .gt. MXDFMD) LOK = .false.

     if (LOK) then ! TEST EVERYTHING BEFORE COPYING

        do I = 2,4
           if (PRMS(I) .lt.0.0) then
              XSUR(1,I-1) = SURFVL(IFMD,1,I-1)
           else
              XSUR(1,I-1) = PRMS(I)
           end if
        end do
        if (PRMS(5) .lt.0.0) then
           XSUR(2,1) = SURFVL(IFMD,2,1)
        else
           XSUR(2,1) = PRMS(5)
        end if
        if (PRMS(12) .lt.0.0) then
           XSUR(2,2) = SURFVL(IFMD,2,2)
        else
           XSUR(2,2) = PRMS(12)
        end if

        do I = 1,2
           do J = 1,3
              SUMPS = SUMPS + XSUR(I,J)
           end do
        end do
        if (SUMPS .le.0.0001) LOK = .false.

     end if

     if (LOK) then
        do I = 6,8
           if (PRMS(I) .lt.0.0) then
              XFML(1,I-5) = FMLOAD(IFMD,1,I-5)
           else
              XFML(1,I-5) = PRMS(I)
           end if
        end do
        if (PRMS(9) .lt.0.0) then
           XFML(2,1) = FMLOAD(IFMD,2,1)
        else
           XFML(2,1) = PRMS(9)
        end if
        if (PRMS(13) .lt.0.0) then
           XFML(2,2) = FMLOAD(IFMD,2,2)
        else
           XFML(2,2) = PRMS(13)
        end if

        do I = 1,3
           if (XFML(1,I) .gt.0.0) INDD = INDD + 1
           if (XFML(2,I) .gt.0.0) INL = INL + 1
        end do

        if (INDD .le.0.and. INL .le.0) LOK = .false.

     end if

     if (LOK) then
        if (PRMS(10) .lt.0.0) then
           XDEP = FMDEP(IFMD)
        else
           XDEP = PRMS(10)
        end if
        if (XDEP .lt.0.0) LOK = .false.
     end if

     if (LOK) then
        if (PRMS(11) .lt.0.0) then
           XEXT = MOISEX(IFMD)
        else
           XEXT = PRMS(11)
        end if
        if (XEXT .lt.0.0.or. XEXT .gt.1.0) LOK = .false.
     end if

     !  ALL PARAMETERS ARE OK AFTER READING FROM PRMS
     !  OR COPYING FROM EXISTING VALUES: ASSIGN

     if (LOK) then
        do I = 1,2
           do J = 1,3
              SURFVL(IFMD,I,J) = INT(XSUR(I,J))
              FMLOAD(IFMD,I,J) = XFML(I,J)
           end do
        end do
        FMDEP(IFMD) = XDEP
        MOISEX(IFMD) = XEXT
        call OPDONE(JDO,IYR)
     else
        call OPDEL1(JDO)
     end if

     !        ENDIF
  end do
  399 continue

  !  CALL NEW ROUTINE TO LOAD INFORMATION FOR CALCULATING THE
  !  FUEL MODEL VARIABLES

  call FMCFMD3(IYR, FMD)

  !  IF USING MODELLED FUEL LOADS TO PREDICT FIRE BEHAVIOR, LOAD CUSTOM
  !  FUEL MODEL 89 WITH THE RIGHT PARAMETERS.

  !sb: moved to fmcfmd3

  !  COMPUTE CANOPY BASE HEIGHT, CROWN BULK DENSITY AND TOTAL
  !  CANOPY LOAD; RESULTS IN **FMFCOM** ACTCBH,CBD,TCLOAD

  !sb: moved to fmcfmd3
  !      CALL FMPOCR(IYR)

  !sb: moved to fmcfmd3
  !     &    VARACD .EQ. 'SN')) CALL FMCFMD (IYR, FMD)
  !        CALL FMCFMD2 (IYR, FMD)

  !  CALCULATE AND PRINT THE POTENTIAL FLAME LENGTH REPORT

  !sb: move to end of routine
  !      CALL FMPOFL (IYR, FMD, LNMOUT)

  !  CHECK WHETHER A FIRE IS SCHEDULED FOR THIS YEAR (KEYWORD SIMFIRE)
  !  IF NOT, OR IF THE COVER TYPE IS 0 (NO TREES IN THE STAND), THEN
  !  RETURN.

  ICOND = 0
  call OPFIND(1,MYACTS(2),ITODO)
  if (ITODO.gt.0) then
     do IFT = 1,ITODO
        call OPGET(IFT,6,JYR,IACTK,NPRM,CPRMS)
        !          IF (JYR .NE. IYR) GO TO 400
        call OPDONE (IFT,IYR)

        !  GET THE SIMFIRE PARAMETER VALUES.

        SWIND = CPRMS(1)
        FMOIS = INT(CPRMS(2))
        ATEMP = INT(CPRMS(3))
        MKODE = NINT(CPRMS(4))
        PSBURN = CPRMS(5)
        BURNSEAS = NINT(CPRMS(6))
        ICOND = 1
        400 continue
     end do
  end if

  !  SET THE DEFAULT VALUES FOT THE SIMFIRE VARIABLES
  !  IF THE KEYWORD WAS NOT USED.

  if (ICOND .eq.0) then
     SWIND = 20
     ATEMP = 70
     FMOIS = 1
     MKODE = 1
     PSBURN = 100.
     BURNSEAS = 1
  end if

  !  IF NO FIRES ARE SCHEDULED OR NO TREES ARE IN THE STAND, AND
  !  IF FUEL BURNING HAS NOT BEEN DONE THEN CALL THE ROUTINE WHICH
  !  PRINTS THE FIRE REPORTS JUST TO CHECK IF THE FIRST YEAR OF
  !  A POTENTIAL FIRE PRINTING WAS SCHEDULED. AFTERWARDS, WHETHER
  !  FUEL BURNING WAS DONE OR NOT, RETURN.

  if (DEBUG) write (JOSTND,10) ICOND,COVTYP,LFLBRN
  10 format (' FMBURN, ICOND=',I3,' COVTYP=',I4,&
       &' LFLBRN=',L2)

  if (ICOND .eq.0.or. COVTYP .le.0) then
     if (.not. LFLBRN) then

        !  THIS CALL DOES NOT CREATE AN OUTPUT...IT IS USED TO
        !  INSURE THAT OPTIONS ARE PROCESSED.

        call FMFOUT(IYR,0.0,0,-1,'  NONE  ')
     end if
     return
  end if

  !  NOW CHECK FOR THE FLAMEADJ KEYWORD.

  IFTYPE = 0
  call OPFIND(1,MYACTS(3),JTODO)
  if (JTODO.gt.0) then
     do IFC = 1,JTODO
        call OPGET(IFC,4,JYR,IACTK,NPRM,FPRMS)
        !          IF (JYR .NE. IYR) GO TO 405
        call OPDONE (IFC,IYR)

        FLAME = FPRMS(2)
        FLMULT = FPRMS(1)
        CRBURN = FPRMS(3)
        if (NPRM.ge.4) then
           SCH = FPRMS(4)
        else
           SCH = -1.
        end if
        if (CRBURN.gt.-1.) then
           CRBURN = CRBURN*.01
        end if
        CFTMP = 'USER_DEF'
        IFTYPE = 1
        USRFL = .false.
        if (FLAME .gt.0.0) USRFL = .true.
        405 continue
     end do
  end if

  if (IFTYPE.eq.0) then
     FLAME = -1.0
     FLMULT = 1.0
     CRBURN = -1.0
     SCH = -1.0
     USRFL = .false.
     CFTMP = '*NOT_SET'
  end if

  !  NOW CHECK FOR THE MOISTURE PARAMETERS.

  call OPFIND(1,MYACTS(1),KTODO)
  if (KTODO.gt.0) then
     do JDO = 1,KTODO
        call OPGET(JDO,7,JYR,IACTK,NPRM,MPRMS)
        !          IF (JYR .NE. IYR) GO TO 415
        call OPDONE (JDO,IY(ICYC))
        FMOIS = 0
        MOIS(1,1) = MPRMS(1)*.01
        MOIS(1,2) = MPRMS(2)*.01
        MOIS(1,3) = MPRMS(3)*.01
        MOIS(1,4) = MPRMS(4)*.01
        MOIS(1,5) = MPRMS(5)*.01
        MOIS(2,1) = MPRMS(6)*.01
        MOIS(2,2) = MPRMS(7)*.01
        415 continue
     end do
  end if

  !  BEGIN ROUTINE:
  !  SELECT FUEL MOISTURE VALUES IF USER DID NOT ENTER THEM.

  call FMMOIS(FMOIS, MOIS)

  !  MODIFY WIND SPEED BASED ON CROWN CANOPY CLOSURE:

  WMULT = ALGSLP(PERCOV,CANCLS,CORFAC,4)
  FWIND = SWIND * WMULT

  !  IN CR/UT/LS/SN/CS, FIND FUEL MODEL AGAIN, NOW THAT WIND SPEED AND FUEL
  !  MOISTURES ARE KNOWN.
  !  TT ADDED TO THIS SINCE THE EXPANDED VARIANT NOW MODELS JUNIPER. 06/03/10

  if (IFLOGIC .eq.0) then
     select case (VARACD)
     case ('CR','CS','LS','ON','SN','TT','UT')
        call FMCFMD (IYR, FMD)
     case default
     end select
  end if

  if (DEBUG) write (JOSTND,60) SWIND,FWIND,WMULT,PERCOV
  60 format (' FMBURN, SWIND=',F7.3,' FWIND=',F7.3,' WMULT=',F7.3,&
       &' PERCOV=',F7.3)

  !     IF THE USER SPECIFIED SCH, FLAME, AND CRBURN, THEN SIMPLY BRANCH
  !     TO THE FIRE EFFECTS CALCULATIONS.

  if (SCH.gt.-1..and. FLAME.gt.-1..and. CRBURN.gt.-1.) then
     CFTMP = 'USER_DEF'
     if (SCH .gt. PBSCOR) BURNYR = IYR
     if (CRBURN .gt.0) then
        if (CRBURN .lt.100.0) then
           FIRTYPE = 2 ! PASSIVE
        else
           FIRTYPE = 1 ! active
        end if
     else
        FIRTYPE = 3 ! surface
     end if
     goto 490
  end if

  !  IF THE FLAME LENGTH WAS NOT ENTERED BY THE USER:

  OLDFL = 0.0
  BYRAM = 0.0

  if (FLAME .le.0.0) then

     !  COMPUTE BYRAM'S FIRELINE INTENSITY, GET THE FLAME LENGTH

     call FMFINT(IYR, BYRAM, FLAME, 1, HPA, 1)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return

     if (CFIM_ON) then
        !         NOTE THAT FOR CFIM, WE NEED TO KNOW POTENTIAL CONSUMPTION BEFORE WE
        !           CALCULATE INTENSITY
        CANBURN = 0
        call FMCONS(FMOIS, 0, 0.0, IYR, 1, PSMOKE,PSBURN)
        call FMCFIM(IYR, FMD, SWIND, BYRAM, FLAME, CANBURN, ROS)
     end if

     OLDFL = FLAME

     !  A FLAME LENGTH MULTIPLIER WAS ADDED, SO FLAME IS MODIFIED

     if (FLMULT .ne.1.0) FLAME = OLDFL * FLMULT
  else

     !  WE NEED TO CALL FMFINT ANYWAYS TO GET SOME INFORMATION FOR CALCULATING
     !  THE TYPE OF FIRE AND THE AMOUNT OF CROWN BURNED.

     call FMFINT(IYR, BYRAM, OLDFL, 1, HPA, 1)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return

     if (CFIM_ON) then
        !         NOTE THAT FOR CFIM, WE NEED TO KNOW POTENTIAL CONSUMPTION BEFORE WE
        !           CALCULATE INTENSITY
        CANBURN = 0
        call FMCONS(FMOIS, 0, 0.0, IYR, 1, PSMOKE,PSBURN)
        call FMCFIM(IYR, FMD, SWIND, BYRAM, OLDFL, CANBURN, ROS)
     end if
     OLDFL = 0.0
  end if

  !  FLAME LENGTH HAS BEEN MODIFIED FROM THAT CALCULATED BY FMFINT
  !  OR WAS ENTERED BY THE USER SO
  !  WE NEED TO (RE)DETERMINE BYRAM SO CAN CALCULATE SCH

  if (FLAME .ne. OLDFL) then

     !  (RECALCULATION BASED ON LINE IN FMFINT WHICH SAYS:
     !  FLAME = 0.45 * (BYRAM/60.0)**(0.46)

     BYRAM = 60.0 * ((FLAME / 0.45) ** (1.0 / 0.46))
  end if

  !  CALCULATE SCORCH HEIGHT (IN FEET)
  !  BUT FIRST CONVERT BYRAM TO BTU/FT/SECOND (RATHER THAN MIN)

  BYRAM = BYRAM / 60.0
  SCH = (63.0 / (140.0 - ATEMP)) * (BYRAM ** (7.0 / 6.0)&
       &/ (BYRAM + FWIND ** 3.0) ** 0.5)

  if (FLAG(1) .eq.1) goto 500

  !  FIRE OCCURRED

  if (SCH .gt. PBSCOR) BURNYR = IYR

  !  DETERMINE THE TYPE OF FIRE AND THE AMOUNT OF CROWN BURNING
  !  (IF NOT USER ASSIGNED).
  !  CALCULATE THE STAND LOWER CROWN BOTTOM HEIGHT (ACTCBH)
  !  AND THE CROWN BULK DENSITY (CBD) SO THAT WE CAN CALCULATE
  !  THE FLAME CRITIAL FLAME LENGTH AND THE CRITIAL DENSITY LATER

  CRRATE = 0
  CRTCBH = 0

  if (DEBUG) write (JOSTND,12) IFTYPE,CRBURN
  12 format (' FMBURN MAY CALL FMCFIR,IFTYPE=',I2,' CRBURN=',F5.1)

  !  DON'T NEED TO DO CROWN FIRE CALCS IN THE SN/CS FFE.  JUST NEED TO SET
  !  CRBURN AND FIRTYPE TO CORRESPOND TO A SURFACE FIRE

  if ((VARACD .eq. 'SN') .or. (VARACD .eq. 'CS')) then
     FIRTYPE = 3
     CRBURN = 0
     CFTMP = 'SURFACE'
  else
     if ((CRBURN.eq.-1.) .or. (.not. USRFL)) then

        !  NEED TO GO INTO FMCFIR EVEN IF USER SETS % CROWNING
        !  IF THEY DIDN'T ENTER FLAME LENGTH BECAUSE VALUES CALCULATED
        !  WITHIN THIS ROUTINE ARE NEEDED FOR INTENSITY/FL CALCULATION.
        !  MUST SAVE THE USERS % CROWNING  AND FIRE TYPE VALUES THOUGH.

        UCRBURN = CRBURN
        LCFTMP = .false.
        if (CFTMP .eq. 'USER_DEF') LCFTMP = .true.
        call FMCFIR(IYR,1,WMULT,INT(SWIND),CFTMP,TMP2,TMP3,HPA)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return

        if (CFIM_ON) call FMCANC (IYR,1,CFTMP,CANBURN,ROS,INTSTY,FCLS)
        if (UCRBURN .ge.0) CRBURN = UCRBURN
        if (LCFTMP) CFTMP = 'USER_DEF'

     else

        !  IF FMCFIR ISN'T CALLED, WE NEED TO SET FIRTYPE, SO THAT
        !  THE FIRE IS VISUALIZED CORRECTLY IN SVS

        if (CRBURN .gt.0) then
           if (CRBURN .lt.100.0) then
              FIRTYPE = 2 ! PASSIVE
           else
              FIRTYPE = 1 ! active
           end if
        else
           FIRTYPE = 3 ! surface
        end if

     end if
  end if ! VARACD eq sn / cs

  !  MODIFY FLAME LENGTH AND SCORCH HEIGHT TO ACCOUNT FOR
  !  CROWN FIRE BEHAVIOR.

  if (CRBURN.gt.0.) then
     if (.not. USRFL) then ! users didn't alter FL
        FINTEN = (HPA+TCLOAD*7744.8*CRBURN)*RFINAL/60.
        FLB = 0.45*FINTEN**.46
        FLT = 0.2*FINTEN**.667
        FLAME = FLB+CRBURN*(FLT-FLB)
        if (FLMULT .ne.1.0) FLAME = FLAME * FLMULT
     end if

     if (USRFL .or. FLMULT.ne.1.0) then

        !  IF USER SETS FLAME LENGTH OR FLAME LENGTH MULTIPLIER,
        !  INTENSITY AND SCORCH HEIGHT NEED TO BE BASED ON THAT.

        TMPINT = CRBURN*((FLAME/.2)**(1/.667))&
             &+ (1-CRBURN)*((FLAME/.45)**(1/.46))
        MAXINT = MAX((FLAME/.21)**(1/.667), (FLAME/.45)**(1/.46))
        MININT = MIN((FLAME/.21)**(1/.667), (FLAME/.45)**(1/.46))

        do I = 1,200
           FLB = 0.45*TMPINT**.46
           FLT = 0.2*TMPINT**.667
           TMPFLAME = FLB+CRBURN*(FLT-FLB)

           if (TMPFLAME .ge. FLAME*0.99.and.&
                &TMPFLAME .le. FLAME*1.01) then
              FINTEN = TMPINT
              goto 205
           else if (TMPFLAME .lt. FLAME) then
              MININT = TMPINT
              TMPINT = .5*TMPINT + .5*MAXINT
           else
              MAXINT = TMPINT
              TMPINT = .5*TMPINT + .5*MININT
           end if
           if (I .eq.200) FINTEN = TMPINT

           200 continue
        end do
        205 continue

     end if
     SCH = (63.0 / (140.0 - ATEMP)) * (FINTEN ** (7.0 / 6.0)&
          &/ (FINTEN + FWIND ** 3.0) ** 0.5)
  end if
  490 continue

  !  CALL FMCBA IF A FIRE OCCURED AND USING THE SN VARIANT SO THAT LIVE FUELS
  !  (HERB AND SHRUB) VALUES CAN BE UPDATED SINCE THEY ARE BASED ON TIME
  !  SINCE LAST FIRE.

  if (BURNYR .eq. IYR .and. VARACD .eq. 'SN') then
     call FMCBA (IYR,0)
  end if

  !  CALCULATE SOIL HEATING USING FOFEM

  call FMSOILHEAT(IYR, LNMOUT)

  !  CALCULATE PROBABILITY OF TREE MORTALITY, AND REDUCE THE FM TREELIST

  PMRT = 0.0
  call FMEFF(IYR, FMD, FLAME, 0, PMRT, PVOLKL, MKODE,PSBURN)

  !  CALCULATE FUEL CONSUMPTION AND SMOKE PRODUCED

  PSMOKE = 0.0
  call FMCONS(FMOIS, 0, 0.0, IYR, 0, PSMOKE,PSBURN)

  500 continue ! TARGET IF NO FIRE BURNED.

  !  PRINT FIRE REPORTS
  !  NOTE THAT IF FUEL BURNING WAS DONE AS WELL, THE FLAG
  !  IS CHANGED FROM A 1 (STAND FIRE ONLY) TO A 2 (STAND AND
  !  TREATMENT FIRE).

  IFIRE = 1
  if (LFLBRN) IFIRE = 2

  if (DEBUG) write (JOSTND,20) FLAME,FMD,IFIRE,CFTMP
  20 format (' FMBURN CALLING FMFOUT, FLAME=',F7.2,&
       &' FMD=',I7,' IFIRE=',I2,' CFTMP=',A)

  if (LNMOUT) call FMFOUT(IYR, FLAME, FMD, IFIRE, CFTMP)

  return

end
