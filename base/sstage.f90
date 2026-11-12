!======================================!
!   Classify stand structural stages   !
!======================================!
! BASE $Id$
subroutine SSTAGE(INBA,INICYCLE,LSUPRT)
  implicit none

  !   THIS SUBROUTINE CALCULATES THE STAND STRUCTURAL STAGE CLASSES, AS
  !   DEFINED BY STAGE ET AL.

  !   N.L.CROOKSTON - INT MOSCOW - JUNE 1996 AND WITH
  !   A.R.STAGE - INT MOSCOW - JUNE 1997

  !   INIBA = 1 IF CALL IS PRIOR TO THINNINGS.  OTHER VALUES MEAN AFTER
  !           THINNING.
  !   INICYCLE = THE CYCLE NUMBER.
  !   LSUPRT = IF TRUE, SUPPRESS THE PRINTING, EVEN IF IT IS REQUESTED.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'SSTGMC.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IOUT,ICD,MSP1,MSP2,IS3OK,IS2OK,I2,IS1OK,IS3I2,IS3I1
  integer :: IS2I2,IS2I1,IS1I2,IS1I1,ILARGE,IILG,ID2I2,ID2I1,IHTLS3
  integer :: IHTS1,IHTS2,IHTS3,IHTSS1,IHTSS2,IHTSS3,IHTLS1,IHTLS2
  integer :: ICRBS1,ICRBS2,ICRBS3,ICRCV1,ICRCV2,ICRCV3,ICOVR,NTREES
  integer :: II,ID1I1,ID1I2
  real    :: COVER,DMIND,CRS3,CRS2,CRS1,DIFF,X,SUMPRB,DIFF1,DIFF2
  real    :: DBHS3,DBHS2,DBHS1,XBAMAX
  logical :: LSUPRT

  real    :: SSWK(MAXTRE)

  integer :: INDEX(MAXTRE),IHTLS,IHTSS,NSTR,DBSKODE
  integer :: INBA,IBA,INICYCLE,ICYCLE,I,J
  real    :: XHTLS,XHTSS,XNSTR,XSDI
  real    :: TPA1,TPA2,TPA3

  logical :: DEBUG

  character(len=3) :: SP11
  character(len=3) :: SP21
  character(len=3) :: SP12
  character(len=3) :: SP22
  character(len=3) :: SP13
  character(len=3) :: SP23
  integer :: ISP11,ISP21,ISP12,ISP22,ISP13,ISP23,J1,J2

  !     STAGE CLASS CODES ARE:

  !     0 = BG = BARE GROUND
  !     1 = SI = STAND INITIATION
  !     2 = SE = STEM EXCLUSION
  !     3 = UR = UNDERSTORY REINITIATION
  !     4 = YM = YOUNG FOREST MULTISTRATA
  !     5 = OS = OLD FOREST SINGLE STRATUM
  !     6 = OM = OLD FOREST MULTISTRATA

  character(len=4) :: SSCODES(7) = (/ '0=BG','1=SI','2=SE','3=UR','4=YM','5=OS','6=OM' /)

  !     DEFINE TEMPORARY VARIABLES SO THAT STRUCTURE STAGE LOGIC CAN BE
  !     CALLED BY FFE THRU ENTRY POINT BELOW.

  real    :: TMPTPA,TMPCCM,TMPPCT,TMPSAW,TMPSSD,TMPGAP,TMPDBH
  real    :: FMTPA,FMCCM,FMPCT,FMSAW,FMSSD,FMGAP,FMDBH
  real    :: TMPPRB(MAXTRE),FPROB(MAXTRE)
  integer :: TMPSCL,FMSTCL,FMFLAG,TMPICR(MAXTRE),FICR(MAXTRE)

  TMPTPA = TPAMIN
  TMPCCM = CCMIN
  TMPPCT = PCTSMX
  TMPSAW = SAWDBH
  TMPSSD = SSDBH
  TMPGAP = GAPPCT
  FMFLAG = 0
  IBA = INBA
  ICYCLE = INICYCLE

  !     UPDATE CCCOEF,  AS THERE MAYBE A CCADJ ACTIVITY

  call UPDATECCCOEF(INICYCLE,INBA)

  if (INICYCLE.gt.1.and.INBA.eq.2.and.CCCOEF2.ne.1)CCCOEF = CCCOEF2

  do I = 1, ITRN
     TMPPRB(I) = PROB(I)
     TMPICR(I) = ICR(I)
  end do

  !     IF WE ARE NOT CALCULATING SSTAGE, THEN SET THE EVENT MONITOR
  !     VARIABLES TO FALSE AND RETURN.

  if (ICYCLE.eq.1) then
     if (.not. LCALC) then
        call EVUST4(16)
        call EVUST4(17)
        call EVUST4(18)
        call EVUST4(19)
        call EVUST4(24)
        call EVUST4(25)
     end if
  end if

  if (.not.LCALC) return

  goto 10

  !     ENTRY POINT FOR CALLS FROM FFE

  entry FMSSTAGE(FMTPA,FMCCM,FMPCT,FMSAW,FMSSD,FMGAP,FMSTCL,FMDBH, &
       & FPROB,FICR)
  TMPTPA = FMTPA
  TMPCCM = FMCCM
  TMPPCT = FMPCT
  TMPSAW = FMSAW
  TMPSSD = FMSSD
  TMPGAP = FMGAP
  FMFLAG = 1

  do I = 1, ITRN
     TMPPRB(I) = FPROB(I)
     TMPICR(I) = FICR(I)
  end do

  10 continue

  !     IF THIS CALL FOLLOWS THINNING, AND IF THERE ARE NO REMOVALS,
  !     THE THE STAGE AFTER THINNING IS THE SAME AS THE ONE PRIOR.
  !     IF CALLED FROM FFE, THEN USE AFTER THINNING VALUES.

  if (FMFLAG .eq. 1) then
     XBAMAX = ATSDIX
     IBA = 2
     ICYCLE = ICYC
  else
     XBAMAX = BTSDIX
     if (IBA.ne.1 .and. ONTREM(7).gt.0.)XBAMAX = ATSDIX
  end if

  !     SETUP THE DEBUG FLAG.

  DEBUG = .false.
  call DBCHK (DEBUG,'SSTAGE',6,ICYCLE)

  if (DEBUG) write (JOSTND,'('' IN SSTAGE, IBA, ICYCLE='',2I5)') &
       & IBA,ICYCLE

  !     INITIALIZE

  NSTR = 0
  IS1OK = 0
  IS2OK = 0
  IS3OK = 0
  TMPSCL = 0
  TMPDBH = 0.
  XHTLS = 0.
  XHTSS = 0.
  XNSTR = 0.
  XSDI = 0.

  DBHS1 = 0.
  DBHS2 = 0.
  DBHS3 = 0.
  IHTS1 = 0
  IHTS2 = 0
  IHTS3 = 0
  IHTLS = 0
  IHTSS = 0
  IHTSS1 = 0
  IHTSS2 = 0
  IHTSS3 = 0
  IHTLS1 = 0
  IHTLS2 = 0
  IHTLS3 = 0
  ICRBS1 = 0
  ICRBS2 = 0
  ICRBS3 = 0
  ICRCV1 = 0
  ICRCV2 = 0
  ICRCV3 = 0
  ICOVR = 0
  ISP11 = 0
  ISP21 = 0
  ISP12 = 0
  ISP22 = 0
  ISP13 = 0
  ISP23 = 0
  TPA1 = 0
  TPA2 = 0
  TPA3 = 0
  SP11 = '-- '
  SP21 = '-- '
  SP12 = '-- '
  SP22 = '-- '
  SP13 = '-- '
  SP23 = '-- '

  !     LOAD THE INDEX WITH TREES THAT REPRESENT OVER A LIMITED
  !     TREES/ACRE

  NTREES = 0
  do I = 1,ITRN
     if (TMPPRB(I).gt. 0.00001) then
        NTREES = NTREES+1
        INDEX(NTREES) = I
     end if

     if (DEBUG) write (JOSTND, &
          &    '('' IN SSTAGE. I, DBH, HT, TMPICR, TMPPRB= '',                &
          &    I4,F9.3,F9.3,I4,F9.5)') &
          & I,DBH(I),HT(I),TMPICR(I),TMPPRB(I)

  end do
  if (NTREES.eq.0) goto 100

  if (NTREES.le.1) then

     I = INDEX(1)
     WK6(I) = CRWDTH(I)

     !        NOTE:  0.785398 = PI/4

     WK6(I) = WK6(I)*WK6(I)*TMPPRB(I)*0.785398
     ICRCV1 = IFIX((WK6(I)/43560.)+.5)

     if (WK6(I) .lt. 435.60*TMPCCM) then
        TMPSCL = 0
        if (TMPPRB(I).ge. TMPTPA) TMPSCL = 1
     else if (DBH(I).lt. TMPSSD) then
        TMPSCL = 1
     else if (DBH(I).lt. TMPSAW) then
        TMPSCL = 2
        if (SDIAC .lt. .01*TMPPCT*XBAMAX) &
             & TMPSCL = 1
     else
        TMPSCL = 5
     end if
     TMPDBH = DBH(I)
     DBHS1 = TMPDBH
     IHTS1 = IFIX(HT(I)+.5)
     IHTSS1 = IHTS1
     IHTLS1 = IHTS1
     ICRBS1 = IFIX(HT(I)*(1.-FLOAT(TMPICR(I))*.01)+.5)
     SP11 = JSP(ISP(I))(1:3)
     ISP11 = ISP(I)
     TPA1 = TMPPRB(I)
     goto 80
  end if

  call RDPSRT (NTREES,HT,INDEX,.false.)

  !     CALCULATE THE CCF-BASED "CROWN WIDTH" FOR EACH TREE...AND
  !     CONVERT THIS TO CROWN COVER.

  do II = 1,NTREES
     I = INDEX(II)
     WK6(I) = CRWDTH(I)
     WK6(I) = WK6(I)*WK6(I)*TMPPRB(I)*0.785398

     if (DEBUG) write (JOSTND, &
          &      '('' IN SSTAGE. I, II, DBH, HT, TMPICR, WK6= '',             &
          &      I4,I4,F9.3,F9.3,I4,F9.1)') &
          & I,II,DBH(I),HT(I),TMPICR(I),WK6(I)

  end do

  !     FIND THE TWO LARGEST DIFFERENCES.  THE LARGEST DIFFERENCE
  !     WILL BE DIFF1 AND THE SECOND LARGEST DIFFERENCE WILL BE
  !     DIFF2.

  DIFF1 = -1E20
  DIFF2 = DIFF1

  !     THE FOLLOWING POINTERS POINT TO THE FIRST AND THE LAST TREE
  !     RECORD IN A GAP.  THAT IS, A GAP MAY SPAN MORE THAN ONE TREE
  !     RECORD (IN THE CASE THAT WE FIND INSEGNIFICANT "LADDER TREES").

  ID1I1 = 0
  ID1I2 = 0
  ID2I1 = 0
  ID2I2 = 0

  IILG = 1
  ILARGE = INDEX(IILG)
  SUMPRB = 0.0

  do II = 2,NTREES

     ISMALL = INDEX(II)

     !        IF THE SMALLER TREE CREATES A GAP...
     !        (A GAP MUST BE AT LEAST 10 FEET)...

     X = HT(ILARGE)*TMPGAP*.01
     if (X .lt. 10.) X = 10.

     if (HT(ISMALL) .lt. HT(ILARGE)-X) then

        !           IF THE SMALL TREE HAS AN INSIGNIFICANT PROB, THEN SKIP IT!

        if (TMPPRB(ISMALL)+SUMPRB .lt. 2.0) then
           SUMPRB = SUMPRB+TMPPRB(ISMALL)
        else
           DIFF = HT(ILARGE)-HT(ISMALL)

           if (DIFF .gt. DIFF1) then
              DIFF2 = DIFF1
              DIFF1 = DIFF
              ID2I1 = ID1I1
              ID2I2 = ID1I2
              ID1I1 = IILG
              ID1I2 = II
           else if (DIFF .gt. DIFF2) then
              DIFF2 = DIFF
              ID2I1 = IILG
              ID2I2 = II
           end if

           ILARGE = ISMALL
           IILG = II
           SUMPRB = 0.0
        end if
     else

        !        IF THE SMALLER TREE DOES NOT CREATE A GAP...THEN SEE IF
        !        IF IT IS AN INSIGNIFICANT "LADER" TREE.

        if (TMPPRB(ISMALL)+SUMPRB.lt.2.0) then
           SUMPRB = SUMPRB+TMPPRB(ISMALL)
        else

           !              IT IS A LADDER TREE...AND NO GAP WAS FOUND.
           !              RESET THE SUMMATION AND MOVE THE "TOP" TREE DOWN.

           ILARGE = ISMALL
           IILG = II
           SUMPRB = 0.0

        end if
     end if
     if (DEBUG) write (JOSTND,15) II,IILG,ILARGE,ISMALL, &
          & HT(ILARGE),HT(ISMALL), &
          & SUMPRB,ID1I1,ID1I2,ID2I1,ID2I2
     15 format(' II=',i4,' IILG=',I4,' LG=',i4,' SM=',I4,' HTL&S=', &
          & 2F7.1,' SPB=',F7.4,' D11=',I4,' 2=',I4, &
          & ' D21=',I4,' 2=',I4)
  end do

  !     MAKE SURE THAT THE "UPPER" STRATUM IS ON "TOP". AT THIS
  !     POINT, THE LARGEST GAP MAY BE BELOW THE UPPER GAP.

  if (ID1I1.gt.ID2I1 .and. ID2I1.gt.0) then

     !         FIRST SWITCH THE POINTERS TO THE FIRST TREE IN THE GAP, THEN
     !         THE POINTERS TO THE LAST TREE IN THE GAP.

     II = ID1I1
     ID1I1 = ID2I1
     ID2I1 = II
     II = ID1I2
     ID1I2 = ID2I2
     ID2I2 = II

  end if

  !     THESE POINTERS POINT TO THE TOP AND BOTTOM OF EACH STRATUM...
  !     AND NSTR IS THE NUMBER OF POTENTIAL STRATUM.

  NSTR = 1
  IS1I1 = 1
  IS1I2 = NTREES
  IS2I1 = 0
  IS2I2 = 0
  IS3I1 = 0
  IS3I2 = 0

  if (ID1I1.gt.0) then
     NSTR = 2
     IS1I2 = ID1I1
     IS2I1 = ID1I2
     IS2I2 = NTREES
  end if

  if (ID2I1.gt.0) then
     NSTR = 3
     IS2I2 = ID2I1
     IS3I1 = ID2I2
     IS3I2 = NTREES
  end if

  if (DEBUG) write (JOSTND,20) ID1I1,ID1I2,ID2I1,ID2I2, &
       & DIFF1,DIFF2,NSTR,IS1I1,IS1I2,IS2I1,IS2I2,IS3I1,IS3I2

  20 format(' ID1I1=',I4,' ID1I2=',I4,' ID2I1=',I4,' ID2I2=',I4, &
       & ' DIFF1&2=',2E14.7/ &
       & ' NSTR=',I2,' IS1I1=',I4,' IS1I2=',I4,' IS2I1=',I4, &
       & ' IS2I2=',I4,' IS3I1=',I4,' IS3I2=',I4)

  !     THE FIRST POTENTIAL STRATUM CONTAINS TREES FROM 1 TO ID1I1
  !     THE SECOND CONTAINS TREES FROM ID1I2 TO ID2I1
  !     THE THIRD CONTAINS TREES FROM LOC2+1 TO NTREES

  !     COMPUTE AND SAVE THE COVER IN EACH POTENTIAL STRATUM AND
  !     CHECK FOR MIN COVER.

  !     CHECK THE FIRST....

  !     FOR THE PURPOSES OF COMPUTING THE COVER, INCLUDE THE TREES THAT
  !     ARE WITHIN THE GAP IN THE UPPER STRATUM.

  I2 = MAX(IS1I2,IS2I1-1)
  call COVOLP (DEBUG,JOSTND,I2-IS1I1+1,INDEX(IS1I1),WK6,CRS1, &
       & CCCOEF)
  if (DEBUG) write (JOSTND,'('' I2='',I4,'' CRS1='',F8.2)') I2,CRS1
  if (CRS1.gt. TMPCCM) IS1OK = 1

  !     CHECK THE SECOND....

  CRS2 = 0.
  if (NSTR .ge. 2) then
     I2 = MAX(IS2I2,IS3I1-1)
     call COVOLP (DEBUG,JOSTND,I2-IS2I1+1,INDEX(IS2I1),WK6,CRS2, &
          & CCCOEF)
     if (DEBUG) write (JOSTND,'('' I2='',I4,'' CRS2='',F8.2)') &
          & I2,CRS2
     if (CRS2.gt. TMPCCM) IS2OK = 1
  end if

  !     CHECK THE THIRD...

  CRS3 = 0.
  if (NSTR .ge. 3) then
     call COVOLP (DEBUG,JOSTND,IS3I2-IS3I1+1,INDEX(IS3I1),WK6,CRS3, &
          & CCCOEF)
     if (DEBUG) write (JOSTND,'('' I2='',I4,'' CRS3='',F8.2)') &
          & I2,CRS3
     if (CRS3.gt. TMPCCM) IS3OK = 1
  end if

  if (DEBUG) write (JOSTND, 30) 1, CRS1, CRS2, CRS3, &
       & IS1OK, IS2OK, IS3OK, NSTR
  30 format(I2,' CRS1,2,3=',3F8.2,' IS1,2,3OK=',3I2,' NSTR=',I2)

  !     COUNT THE STRATA

  NSTR = IS1OK + IS2OK + IS3OK

  if (DEBUG) write (JOSTND, 30) 2, CRS1, CRS2, CRS3, &
       & IS1OK, IS2OK, IS3OK, NSTR

  !     IF THERE ARE NOT VALID STRATA AND IF THE NUMBER OF TREES ARE
  !     TPAMIN (TMPTPA), THEN WE ARE DONE.  OTHERWISE, FORM ONE STRATUM OF
  !     ALL THE TREES.

  if (NSTR.eq.0) then
     if (TPROB.lt.TMPTPA) goto 80
     IS1OK = 1
     NSTR = 1
     IS1I1 = 1
     IS1I2 = NTREES
  end if

  !     THE CALLS BELOW TO SSTGHTPA COMPUTE THE TREES PER ACRE IN EACH
  !     STRATUM.  THESE VALUES INCLUDE THE TREES IN THE GAPS (LIKE THE
  !     COVER CALCULATIONS ABOVE).

  J1 = MAX(IS1I2,IS2I1-1)
  J2 = MAX(IS2I2,IS3I1-1)

  call SSTGHTPA (IS1I1,J1,INDEX,TMPPRB,MAXTRE,TPA1)
  call SSTGHTPA (IS2I1,J2,INDEX,TMPPRB,MAXTRE,TPA2)
  call SSTGHTPA (IS3I1,IS3I2,INDEX,TMPPRB,MAXTRE,TPA3)

  call SSTGHP (IS1I1,IS1I2,INDEX,WK6,SSWK,DBH,HT,TMPICR,ISP,TMPPRB, &
       & MAXTRE,MAXSP,DBHS1,IHTS1,IHTSS1,IHTLS1,ICRBS1,MSP1,MSP2)
  if (MSP1.gt.0) SP11 = JSP(MSP1)(1:3)
  if (MSP2.gt.0) SP21 = JSP(MSP2)(1:3)
  if (MSP1.gt.0) ISP11 = MSP1
  if (MSP2.gt.0) ISP21 = MSP2

  call SSTGHP (IS2I1,IS2I2,INDEX,WK6,SSWK,DBH,HT,TMPICR,ISP,TMPPRB, &
       & MAXTRE,MAXSP,DBHS2,IHTS2,IHTSS2,IHTLS2,ICRBS2,MSP1,MSP2)
  if (MSP1.gt.0) SP12 = JSP(MSP1)(1:3)
  if (MSP2.gt.0) SP22 = JSP(MSP2)(1:3)
  if (MSP1.gt.0) ISP12 = MSP1
  if (MSP2.gt.0) ISP22 = MSP2

  call SSTGHP (IS3I1,IS3I2,INDEX,WK6,SSWK,DBH,HT,TMPICR,ISP,TMPPRB, &
       & MAXTRE,MAXSP,DBHS3,IHTS3,IHTSS3,IHTLS3,ICRBS3,MSP1,MSP2)
  if (MSP1.gt.0) SP13 = JSP(MSP1)(1:3)
  if (MSP2.gt.0) SP23 = JSP(MSP2)(1:3)
  if (MSP1.gt.0) ISP13 = MSP1
  if (MSP2.gt.0) ISP23 = MSP2

  !     SELECT THE STRATUM THAT WILL DICTATE THE "DOM" STRATUM.

  if (IS1OK.eq.1) then
     IS1OK = 2
     TMPDBH = DBHS1
     DMIND = DBH(INDEX(IS1I2))
     IHTLS = IHTLS1
     IHTSS = IHTSS1
  else if (IS2OK.eq.1) then
     IS2OK = 2
     TMPDBH = DBHS2
     DMIND = DBH(INDEX(IS2I2))
     IHTLS = IHTLS2
     IHTSS = IHTSS2
  else if (IS3OK.eq.1) then
     IS3OK = 2
     TMPDBH = DBHS3
     DMIND = DBH(INDEX(IS3I2))
     IHTLS = IHTLS3
     IHTSS = IHTSS3
  end if

  !     CLASSIFIY THE STAND STRUCTURAL STAGE

  if (NSTR.eq.1) then
     if (TMPDBH .lt. TMPSSD) then
        TMPSCL = 1
     else if (TMPDBH .lt. TMPSAW) then
        TMPSCL = 2
        if (IBA.eq.1) then
           XSDI = SDIBC
        else
           XSDI = SDIAC
        end if
        if (XSDI .lt. .01*TMPPCT*XBAMAX) &
             & TMPSCL = 1
     else
        TMPSCL = 5

        !           IF THE MIN DBH OF THE DOM STRATUM IS LESS THAN A
        !           CRITICAL VALUE (WE PICKED 3.0, FOR NOW), THEN THE
        !           STAND IS MULTISTRATUM (A.K.A, CONTINUOUS STRATUM).

        if (DMIND .lt. 3.0) TMPSCL = 6
     end if
  else if (NSTR.eq.2) then
     if (TMPDBH.lt. TMPSSD) then
        TMPSCL = 1
     else if (TMPDBH .lt. TMPSAW) then
        TMPSCL = 3
     else
        TMPSCL = 6
     end if
  else
     if (TMPDBH.lt. TMPSSD) then
        TMPSCL = 1
     else if (TMPDBH.lt. TMPSAW) then
        TMPSCL = 4
     else
        TMPSCL = 6
     end if
  end if

  if (DEBUG) write (JOSTND, &
       &    '('' IN SSTAGE. TMPSSD,TMPSAW,TMPPCT,XSDI,XBAMAX= '',          &
       &    F9.1,F9.1,F9.1,F9.1,F9.1)') &
       & TMPSSD,TMPSAW,TMPPCT,XSDI,XBAMAX

  if (DEBUG) write (JOSTND, &
       &    '('' IN SSTAGE. TMPTPA,TMPCCM,TMPGAP= '',                      &
       &    F9.1,F9.1,F9.1)') &
       & TMPTPA,TMPCCM,TMPGAP

  ICRCV1 = IFIX(CRS1+.5)
  ICRCV2 = IFIX(CRS2+.5)
  ICRCV3 = IFIX(CRS3+.5)

  80 continue

  call COVOLP (DEBUG,JOSTND,NTREES,INDEX,WK6,COVER,CCCOEF)

  ICOVR = IFIX(COVER+.5)
  if (NTREES.le.1) ICRCV1 = ICOVR

  !     SET THE STRUCTURE CLASS AND LEAVE IF THE CALL IS FROM FFE.  OTHERWISE
  !     STAY, PRINT, AND SET EVENT MONITOR VARIABLES.

  if (DEBUG) write (JOSTND, &
       & '('' IN SSTAGE. ISTRCL & DBHDOM= '',I2,F9.1)') &
       & TMPSCL,TMPDBH

  100 continue

  if (FMFLAG .eq. 1) then
     FMSTCL = TMPSCL
     FMDBH = TMPDBH
     return
  else
     ISTRCL = TMPSCL
     DBHDOM = TMPDBH
  end if

  ICD = 0
  if (IBA.ne.1) ICD = 1

  if (DEBUG) write (JOSTND, &
       & '('' IN SSTAGE. ISTRCL & DBHDOM= '',I2,F9.1)') &
       & ISTRCL,DBHDOM
  if (IBA.eq.1) then
     XHTLS = FLOAT(IHTLS)
     XHTSS = FLOAT(IHTSS)
     XNSTR = FLOAT(NSTR)
     call EVSET4(16,FLOAT(ISTRCL))
     call EVSET4(18,DBHDOM)
     call EVSET4(24,FLOAT(ICOVR))
     call EVSET4(40,XHTLS)
     call EVSET4(42,XHTSS)
     call EVSET4(44,XNSTR)

     call EVUST4(17)
     call EVUST4(19)
     call EVUST4(25)
     call EVUST4(41)
     call EVUST4(43)
     call EVUST4(45)
  else
     XHTLS = FLOAT(IHTLS)
     XHTSS = FLOAT(IHTSS)
     XNSTR = FLOAT(NSTR)
     call EVSET4(17,FLOAT(ISTRCL))
     call EVSET4(19,DBHDOM)
     call EVSET4(25,FLOAT(ICOVR))
     call EVSET4(41,XHTLS)
     call EVSET4(43,XHTSS)
     call EVSET4(45,XNSTR)
  end if

  !     SET THE IOSTR, WHICH IS USED BY STRSTAT, AN EVENT MONITOR FUNCTION.

  if (IBA .eq. 1) then
     do I = 1,33
        do J = 1,2
           OSTRST(I,J) = 0
        end do
     end do
  end if

  J = 1
  if (IBA.ne.1) J = 2
  OSTRST(1,J) = DBHS1 ! nominal dbh for stratum 1
  OSTRST(2,J) = FLOAT(IHTS1) ! nominal height for stratum 1
  OSTRST(3,J) = FLOAT(IHTLS1) ! height of the tallest tree in stratum 1
  OSTRST(4,J) = FLOAT(IHTSS1) ! height of the shortest tree in stratum 1
  OSTRST(5,J) = FLOAT(ICRBS1) ! height to crown base for stratum 1
  OSTRST(6,J) = FLOAT(ICRCV1) ! canopy cover for stratum 1
  OSTRST(7,J) = ISP11 ! major species 1 for stratum 1
  OSTRST(8,J) = ISP21 ! major species 2 for stratum 1
  OSTRST(9,J) = FLOAT(IS1OK) ! stratum status code for stratum 1
  OSTRST(10,J) = TPA1 ! trees per acre for stratum 1
  OSTRST(11,J) = DBHS2 ! nominal dbh for stratum 2
  OSTRST(12,J) = FLOAT(IHTS2) ! nominal height for stratum 2
  OSTRST(13,J) = FLOAT(IHTLS2) ! height of the tallest tree in stratum 2
  OSTRST(14,J) = FLOAT(IHTSS2) ! height of the shortest tree in stratum 2
  OSTRST(15,J) = FLOAT(ICRBS2) ! height to crown base for stratum 2
  OSTRST(16,J) = FLOAT(ICRCV2) ! canopy cover for stratum 2
  OSTRST(17,J) = ISP12 ! major species 1 for stratum 2
  OSTRST(18,J) = ISP22 ! major species 2 for stratum 2
  OSTRST(19,J) = FLOAT(IS2OK) ! stratum status code for stratum 2
  OSTRST(20,J) = TPA2 ! trees per acre for stratum 2
  OSTRST(21,J) = DBHS3 ! nominal dbh for stratum 3
  OSTRST(22,J) = FLOAT(IHTS3) ! nominal height for stratum 3
  OSTRST(23,J) = FLOAT(IHTLS3) ! height of the tallest tree in stratum 3
  OSTRST(24,J) = FLOAT(IHTSS3) ! height of the shortest tree in stratum 3
  OSTRST(25,J) = FLOAT(ICRBS3) ! height to crown base for stratum 3
  OSTRST(26,J) = FLOAT(ICRCV3) ! canopy cover for stratum 3
  OSTRST(27,J) = ISP13 ! major species 1 for stratum 3
  OSTRST(28,J) = ISP23 ! major species 2 for stratum 3
  OSTRST(29,J) = FLOAT(IS3OK) ! stratum status code for stratum 3
  OSTRST(30,J) = TPA3 ! trees per acre for stratum 3
  OSTRST(31,J) = NSTR ! number of strata
  OSTRST(32,J) = FLOAT(ICOVR) ! canopy cover
  OSTRST(33,J) = FLOAT(ISTRCL) ! structure class

  DBSKODE = 1
  if (LMETRICVAR) then
     call DBSSTRCLASS(IY(ICYCLE), NPLT, ICD,&
          &DBHS1*INtoCM,&
          &INT(IHTS1*FTtoM),&
          &INT(IHTLS1*FTtoM),&
          &INT(IHTSS1*FTtoM),&
          &INT(ICRBS1*FTtoM),&
          &ICRCV1,SP11,SP21,IS1OK,&
          &DBHS2*INtoCM,&
          &INT(IHTS2*FTtoM),&
          &INT(IHTLS2*FTtoM),&
          &INT(IHTSS2*FTtoM),&
          &INT(ICRBS2*FTtoM),&
          &ICRCV2,SP12,SP22,IS2OK,&
          &DBHS3*INtoCM,&
          &INT(IHTS3*FTtoM),&
          &INT(IHTLS3*FTtoM),&
          &INT(IHTSS3*FTtoM),&
          &INT(ICRBS3*FTtoM),&
          &ICRCV3,SP13,SP23,IS3OK,&
          &NSTR,ICOVR,SSCODES(ISTRCL+1), DBSKODE, NTREES)
  else
     call DBSSTRCLASS(IY(ICYCLE), NPLT, ICD, &
          & DBHS1,IHTS1,IHTLS1,IHTSS1,ICRBS1,ICRCV1,SP11,SP21,IS1OK, &
          & DBHS2,IHTS2,IHTLS2,IHTSS2,ICRBS2,ICRCV2,SP12,SP22,IS2OK, &
          & DBHS3,IHTS3,IHTLS3,IHTSS3,ICRBS3,ICRCV3,SP13,SP23,IS3OK, &
          & NSTR,ICOVR,SSCODES(ISTRCL+1), DBSKODE, NTREES)
  end if
  if (DBSKODE.eq.0) return

  if (ICYCLE.eq.1) then
     if (.not. LSUPRT .and. LPRNT .and. IRREF.eq.-1) then
        call GETID (IRREF)
        call GETLUN(IOUT)
        write (IOUT,85) IRREF,NPLT,MGMID, &
             & (' ------------ Stratum',I,' ------------',I = 1,3)
        85 format (1X,I5,' $#*%' &
             & //'Structural statistics for stand: ',A, &
             & '  MgmtID: ',A//T8,3(A,I2,A)/ &
             & '     Rm',3('       ---Height-- -Crown- -Major- C'), &
             & ' N Tot Struc'/ &
             & 'Year Cd',3('  DBH  Nom  Lg  Sm Bas Cov Sp1 Sp2 D'), &
             & ' S Cov Class'/ &
             & '---- --',3(' ----- --- --- --- --- --- --- --- -'), &
             & ' - --- -----' &
             & /'$#*%')
     end if
  end if

  if (IRREF.gt.0 .and. .not. LSUPRT .and. NTREES .gt. 0) then
     call GETLUN(IOUT)
     if (LMETRICVAR) then
        write (IOUT,90) IRREF,IY(ICYCLE),ICD,&
             &DBHS1*INtoCM,&
             &INT(IHTS1*FTtoM),&
             &INT(IHTLS1*FTtoM),&
             &INT(IHTSS1*FTtoM),&
             &INT(ICRBS1*FTtoM),&
             &ICRCV1,SP11,SP21,IS1OK,&
             &DBHS2*INtoCM,&
             &INT(IHTS2*FTtoM),&
             &INT(IHTLS2*FTtoM),&
             &INT(IHTSS2*FTtoM),&
             &INT(ICRBS2*FTtoM),&
             &ICRCV2,SP12,SP22,IS2OK,&
             &DBHS3*INtoCM,&
             &INT(IHTS3*FTtoM),&
             &INT(IHTLS3*FTtoM),&
             &INT(IHTSS3*FTtoM),&
             &INT(ICRBS3*FTtoM),&
             &ICRCV3,SP13,SP23,IS3OK,&
             &NSTR,ICOVR,SSCODES(ISTRCL+1)
     else
        write (IOUT,90) IRREF,IY(ICYCLE),ICD, &
             & DBHS1,IHTS1,IHTLS1,IHTSS1,ICRBS1,ICRCV1,SP11,SP21,IS1OK, &
             & DBHS2,IHTS2,IHTLS2,IHTSS2,ICRBS2,ICRCV2,SP12,SP22,IS2OK, &
             & DBHS3,IHTS3,IHTLS3,IHTSS3,ICRBS3,ICRCV3,SP13,SP23,IS3OK, &
             & NSTR,ICOVR,SSCODES(ISTRCL+1)
     end if
     90 format (1X,I5,1X,I4,I3,3(1X,F5.1,1X,I3,1X,I3,1X, &
          & I3,1X,I3,1X,I3,1X,A3,1X,A3,1X,I1),1X,I1,1X,I3,2X,A4)
  end if

  return
end subroutine SSTAGE

subroutine SSTGHP (I1,I2,INDEX,WK6,WK4,DBH,HT,TMPICR,ISP,TMPPRB, &
     & MAXTRE,MAXSP,DBHNOM,IHT,IHTS,IHTL,ICRB,MSP1,MSP2)

  !     THIS IS A PRIVATE HELPER ROUTINE CALLED BY SSTAGE
  !     (AND, PLEASE, NO OTHER ROUTINES SHOULD CALL THIS ROUTINE).

  ! Variables
  real    :: TMPPRB
  integer :: TMPICR

  dimension INDEX(MAXTRE),WK6(MAXTRE),WK4(MAXTRE),DBH(MAXTRE), &
       & HT(MAXTRE),TMPICR(MAXTRE),ISP(MAXTRE),TMPPRB(MAXTRE)

  !     INITIALIZE.

  do I = 1,MAXSP
     WK4(I) = 0.
  end do

  MSP1 = 0
  MSP2 = 0
  ACB = 0.
  SP = 0.
  SUM = 0.

  if (I1.eq.0 .or. I2.eq.0) return

  !     PICK THE LARGEST AND SMALLEST TREES IN THE STRATUM.

  IHTL = IFIX(HT(INDEX(I1))+.5)
  IHTS = IFIX(HT(INDEX(I2))+.5)

  !     COMPUTE THE NOMINAL DIAMETER AND HEIGHT FOR THE TREES IN THE
  !     STRATUM.  ALSO, AND AVERAGE CROWN BASE HEIGHT AND TOTAL PERCENT
  !     COVER.

  !     FIRST, COMPUTE THE PERCENTILE IN THE CROWN COVER DISTRIBUTION.
  !     CUT OFF THE LIST OF TREES IN THE STRATUM AT 95% COVER. ...
  !     WHILE WE ARE AT IT, COMPUTE THE AVERAGE CROWN BASE HT AND
  !     FIND THE SUM OF BA BY SPECIES.

  ITOP = I2
  I3 = -1
  do II = I1,ITOP
     I = INDEX(II)

     !        SUM IS THE SUM OF COVER, SP IS THE SUM OF PROB FOR THE
     !        TREES IN THE STRATUM, AND ACB IS THE AVERAGE CROWN BASE HT.

     SUM = SUM+WK6(I)
     SP = SP +TMPPRB(I)
     ACB = ACB+((HT(I)*(1.-FLOAT(TMPICR(I))*.01))*TMPPRB(I))

     !        SUM THE COVER BY SPECIES.

     IS = ISP(I)
     WK4(IS) = WK4(IS)+(WK6(I))

     !        41382 = .95 * 43560  ... SET I3 TO POINT TO 95% COVER CUT OFF.
     !        (THIS PART OF THE ROUTINE DOES NOT ACCOUNT FOR OVERLAP).

     if (SUM.gt.41382. .and. I3.eq.-1) I3 = II
  end do

  !     IF THERE WAS LESS THAN 95% COVER, I3 SHOULD BE EQUAL TO I2.

  if (I3.eq.-1) I3 = I2

  !     FIND THE SPECIES INDICES FOR THE TOP 2 CONTRIBUTORS TO COVER
  !     (NOTE: THE STARTING VALUES OF X1 AND X2 MUST BE ZERO OF THE
  !     METHOD WILL NOT WORK).

  X1 = 0.
  X2 = 0.
  do I = 1,MAXSP
     if (WK4(I).gt.X1) then
        X2 = X1
        X1 = WK4(I)
        MSP2 = MSP1
        MSP1 = I
     else if (WK4(I).gt.X2) then
        X2 = WK4(I)
        MSP2 = I
     end if
  end do

  !     COMPUTE THE AVERAGE CROWN BASE.

  ICRB = 0
  if (SP .gt. 0.0001) ICRB = IFIX(ACB/SP+.5)

  do II = I1,I3
     I = INDEX(II)
     WK4(I) = WK6(I)/TMPPRB(I)
  end do

  call RDPSRT (I3-I1+1,WK4,INDEX(I1),.false.)
  call PCTILE (I3-I1+1,INDEX(I1),WK6,WK4,T)

  !     FIND THE 70 PERCENTILE TREE... (30 %TILE "DOWN FROM THE TOP").

  I70 = 0
  DIFF = 1E30
  do II = I1,I3
     I = INDEX(II)
     if (ABS(WK4(I)-70.) .lt. DIFF) then
        I70 = II
        DIFF = ABS(WK4(I)-70.)
     end if
  end do

  K1 = I70-4
  if (K1.lt.I1) K1 = I1
  K2 = I70+4
  if (K2.gt.I3) K2 = I3

  SD = 0.
  SH = 0.
  SP = 0.

  do II = K1,K2
     I = INDEX(II)
     SD = SD+(DBH(I)*TMPPRB(I))
     SH = SH+(HT(I) *TMPPRB(I))
     SP = SP+TMPPRB(I)
  end do

  DBHNOM = 0.
  IHT = 0
  if (SP.gt. .0001) then
     DBHNOM = SD/SP
     IHT = IFIX(SH/SP+.5)
  end if

  return
end subroutine SSTGHP

subroutine SSTGHTPA (I1,I2,INDEX,TMPPRB,MAXTRE,STRTPA)
  implicit none

  !     THIS IS A PRIVATE HELPER ROUTINE CALLED BY SSTAGE
  !     (AND, PLEASE, NO OTHER ROUTINES SHOULD CALL THIS ROUTINE).

  !     THIS ROUTINE COMPUTES THE TREES PER ACRE IN EACH STRATUM.
  !     IT IS DIFFERENT THAN THE OTHER HELPER ROUTINE (SSTGHP) BECAUSE
  !     I WANTED TO INCLUDE THE TREES IN THE "GAPS" BETWEEN THE STRATA
  !     (LIKE THE COVER CALCULATIONS DO).  SAR - AUG 2006

  ! Variables
  integer :: II,I,I1,I2,MAXTRE
  integer :: INDEX(MAXTRE)
  real    :: TMPPRB(MAXTRE),STRTPA

  STRTPA = 0.

  if (I1.eq.0 .or. I2.eq.0) return

  do II = I1,I2
     I = INDEX(II)
     STRTPA = STRTPA +TMPPRB(I)
  end do

  return
end subroutine SSTGHTPA

subroutine UPDATECCCOEF(INICYCLE,INBA)
  implicit none

  !     THIS IS A PRIVATE HELPER ROUTINE CALLED BY SSTAGE
  !     (AND, PLEASE, NO OTHER ROUTINES SHOULD CALL THIS ROUTINE).

  !     PROCESS CCADJ ACTIVITIES TO INSURE CCCOEF IS UP TO DATE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  !     ACTIVITY 444 = CCADJ KEYWORD

  ! Variables
  real    :: PRM(1)
  integer :: MYACTS(1),ITODO,NTODO,IACTK,NP,IDATE,INBA,INICYCLE
  data MYACTS/444/
  call OPFIND(1,MYACTS,NTODO)
  if (NTODO.eq.0) return
  do ITODO = 1,NTODO
     call OPGET(ITODO,1,IDATE,IACTK,NP,PRM)
     if (NP.lt.1) cycle
     if (INICYCLE.gt.1.and.INBA.eq.1) then
        CCCOEF2 = PRM(1)
     else
        CCCOEF = PRM(1)
     end if
     call OPDONE(1,IDATE)
  end do
  return
end subroutine UPDATECCCOEF
