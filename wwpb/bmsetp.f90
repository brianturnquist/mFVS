!===================!
!   wwpb : bmsetp   !
!===================!
! WWPB $Id$
subroutine BMSETP

  !     Shamelessly adapted from MDSETP.

  !     SET UP A MULTI-STAND MOUNTAIN PINE BEETLE RUN. THIS IS DONE
  !     BY SETTING UP A LIST OF PPE STAND NUMBERS IN BMSDIX AND
  !     SETTING THE BEGINNING AND ENDING YEARS OF THE OUTBREAK
  !     THAT OCCURS INSIDE THE CURRENT MASTER CYCLE.

  !     AN EXTENSION OF THE PARALLEL PROCESSING EXTENSION (PROGNOSIS)
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--MAY 1987

  !     CALLED FROM:  ALSTD1
  !     CALLS:  CH8BSR
  !             CH8SRT
  !             C26BSR:THIS REPLACES CH8BSR (ABOVE)[AJM 3/21/00]
  !             C26SRT: THIS REPLACES CH8SRT (ABOVE)[AJM 3/21/00]
  !             GPADD
  !             GPGET
  !             RCDSET
  !             SPLAAR
  !             SPLAEX
  !             SPNBEX

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'ARRAYS.f90'
  include 'PPCNTL.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'
  include 'BMPCOM.f90'

  ! Variables
  real    :: PRMS(7)
  integer :: MYLIST(MXSTND)
  integer :: BMSTD0,jstk2ns,bmstkd
  real    :: AREA

  !     IF THIS IS THE FIRST MASTER CYCLE (=2), THEN CARRY OUT THE
  !     INITIAL ASSIGNMENTS: BMINI() I.E., SET THE INTITAL NUMBER OF DEAD BKP-
  !     PRODUCING TREES BY STAND AND SIZE CLASS.
  !     NOTE: BMCNT IS THE NUMBER OF RECORDS READ-IN VIA KEYWORD BMHIST
  !     IT CAN BE GREATER THAN THE NUMBER OF STANDS IN THE RUN--e.g.  A STAND
  !     MAY BE INTIALIZED WITH BKP VIA MORE THAN 1 SUPPLEMENTAL RECORD.

  !     ALSO NOTE THAT BMSTDS() ARRAY WAS INITIALIZED IN BMPPIN WITH THE
  !     BMHIST SUPPLEMENTAL RECORD STAND IDS, WHICH MAY HAVE BEEN DUPLICATED.
  !     i.e.  A STAND MAY BE INITIALIZED WITH BKP INTO TREES OF A FEW SIZE CLASSES
  !     NECESSITATING MORE THAN ONE RECORD IN THE BMHIST SUPPLEMETAL RECORD LIST.
  !     BMSTDS ARRAY IS "CORRECTED" BELOW SO THAT IT CONTAINS (WITHOUT DUPLICATION)
  !     THE PPE STAND IDS

  if (MICYC.eq.2) then

     if (LBMDEB) write(JOPPRT,'('' IN BMSETP, BMCNT= '',I4)')BMCNT

     if (BMCNT.gt.0) then
        !         CALL CH8SRT (NOSTND,STDIDS,MYLIST,.TRUE.)
        call C26SRT (NOSTND,STDIDS,MYLIST,.true.)
        do I = 1,BMCNT
           J = IFIX(BMINI(I,PBSPEC,1))
           if ((J .gt.0) .and. (J .le. NSCL)) then
              !              CALL CH8BSR (NOSTND,STDIDS,MYLIST,BMSTDS(I),IP)
              call C26BSR (NOSTND,STDIDS,MYLIST,BMSTDS(I),IP)
              if (IP .gt.0) then
                 if (PBSPEC .lt.3) PBKILL(IP,J) = BMINI(I,PBSPEC,2)
                 !                 DEBUG
                 if (LBMDEB) then
                    write (JOPPRT,5)i,bmstds(i),ip,j,bmini(i,pbspec,1),&
                         &bmini(i,pbspec,2),pbkill(ip,j)
                    5 format(' IN BMSETP, CYCLE1 BMHIST DATA: ',&
                         &1x,I6,1X,a26,i5,I3,F4.0,2(1X,F5.2))
                 end if

                 if (PBSPEC .eq.3) ALLKLL(IP,J) = BMINI(I,PBSPEC,2)
              end if
           end if
           if (IPSON .and. (IP .gt.0)) then
              J = IFIX(BMINI(I,3,1))
              if ((J .gt.0) .and. (J .le. NSCL))&
                   &ALLKLL(IP,J) = BMINI(I,3,2)
           end if

           10 continue
        end do
     else
        BMCNT = 0
     end if

  end if

  !     Set up year boundaries for dispersal

  !      BMSTND=0 !I don't think we need this.  we use ibmyr1 now as the flag to bail if
  !               ! "disperse" is ended
  IBMYR1 = 0
  call GPGET (301,IBMYR1,7,NPRMS,PRMS,MXSTND,BMSTD0,BMSDIX)

  !     IF THE NUMBER OF STANDS SCHEDULED FOR THIS MASTER CYCLE IS ZERO
  !     THEN: BRANCH TO END...NO OUTBREAK THIS MASTER CYCLE.

  !     I THINK WE WHOULD KEY ON IBMYR1 INSTEAD OF BMSTD0.  AJM 12/05
  !     BMSTD0 STAYS POSITIVE EVEN AFTER OUTBREAK IS HALTED

  !      IF (BMSTD0 .LE. 0) GOTO 150
  if (IBMYR1.le.0) goto 150
  if (LBMDEB) write (JBMBPR,30) MICYC,BMSTD0,IBMYR1,&
       &(BMSDIX(I),I = 1,BMSTD0)
  30 format (/' IN BMSETP: MICYC=',I5,' BMSTD0=',I5,' IBMYR1=',I5,&
       &' BMSDIX IS...'/((1X,10I5)))

  !     LOAD STAND AND MANAGEMENT IDENTIFICATIONS.

  !     NEW LOGIC DEC 05:  AJM
  !     SKIP A BUNCH OF THIS INITIALIZATION IF ITS ALREADY DONE!

  if (LBMSETP)go to 65
  do I = 1, BMSTD0
     BMSTDS(I) = STDIDS(BMSDIX(I))
     BMMGID(I) = MGMIDS(BMSDIX(I))
     40 continue
  end do

  !     APPEND NONSTOCKABLE STANDS TO BM STANDS.
  !     MORE THAN MXSTND IN TOTAL WILL RESUL  IN LOST DATA.

  !  ** THIS LOGIC IS MOVED TO BELOW (SEE DO 44), AFTER WE CHECK TO SEE IF
  !     THE NON-STOCKED ID HAS ALREADY BEEN ENTERED! **

  !      DO 41 I = 1, NSSTND
  !        J= BMSTD0 + I
  !        IF (J .GT. MXSTND) GOTO 42
  !        BMSDIX(J) = J
  !        BMSTDS(J) = NSSTDS(I)
  !        BMMGID(J) = 'NONE'
  !   41 CONTINUE
  !   42 CONTINUE

  !     troubleshoot ajm 12.05
  if (lbmdeb) write (jbmbpr,47)bmstd0,nsstnd
  47 format (/' in bmsetp: bmstd0=',i5,'nsstnd=',i4)

  !     EXTRACT THE STAND NEIGHBORS AND LOCATION DATA FOR THIS RUN.
  !     FIRST, SORT THE INDEX LIST ON THE STAND IDENTS LIST. NOTE
  !     THAT 'BMSTND' IS REDEFINED *HERE* TO INCLUDE NONSTOCKABLE
  !     STANDS. CAVEAT EMPTOR.

  !      BMSTND = MIN(MXSTND, (BMSTD0 + NSSTND))
  !      BMEND = BMSTND - NSSTND

  !      MODIFICATIONS AJM DEC 2005
  !      BMSTND IS CURRENT # OF STANDS IN THE RUN.  THIS MAY BE INCREASED LATER (BELOW)
  !      DUE TO POSSIBLE INCLUSION OF NON-STOCKED POLYGONS.
  !      BMSTKD IS A NEW VARIABLE KEEPING TRACK OF THE NUMBER OF STOCKED STANDS
  !      WHICH FOR NOW IS SET = TO BMSTD0, BUT IT MAY BE DECREMENTED IF USER
  !      DEFINES A HERETOFORE STOCKED STAND AS NONSTOCKED.
  !      AT THIS POINT BMSTKD IS BEING USED FOR INFORMATIONAL PURPOSES ONLY

  BMSTND = BMSTD0
  BMSTKD = BMSTND
  !       bmend=bmstnd-nsstnd  ! DONE BELOW.
  !      (this could be done here, but it would be incorrectly calculated if the NSSTNDs
  !      were truly supplemental e.g. 100 stocked stands plus 100 additional nsstnds
  !      would yield ZERO BMEND stands.  wrong!)

  !     CALL CH8SRT (BMSTND, BMSTDS, MYLIST, .TRUE.)
  call C26SRT (BMSTND, BMSTDS, MYLIST, .true.)

  !     SET ALL STANDS TO STOCKABLE, THEN MARK NONSTOCKABLE STANDS
  !     ALSO INITIALIZE DB-WRITING VARIABLES (ONCE PER SIMULATION ONLY)

  do I = 1, BMSTND
     STOCK(I) = .true.

     !     INITIALIZE OUTPUT WRITING ACTIVITY PARAMETERS TIME ZERO ONLY
     !     THESE ARE RESET IN BMSDIT IF USER REQUESTS OUTPUT.  AJM 9/1/5

     if (.not. LDBINIT) then
        do IOUT = 1,4
           IBEG(IOUT,I) = -1
           IEND(IOUT,I) = -1
           ISTP(IOUT,I) = -1
           45 continue
        end do

        !     INITIIALIZE WWPBM-DB TABLE FLAGS. THESE ARE LATER SET (TO 1 OR 2)
        !     VIA DB KEYWORDS (in DBSIN)IF DB TABLE IS REQUESTED
        !     J=TABLE # (1=MAIN,2=TREE; 3=BKP; 4=VOL); I=STAND INDEX
        !     FETCHED INTO WWPBM IN BMSDIT
        do J = 1,4
           JBMDB(J,I) = -1
           46 continue
        end do

        !     INITIALIZE WWPBM-RECOGNIZED DBS CASEIDs TO -1, FETCHED LATER
        !     IN BMSDIT IF DB OUTPUT REQUESTED

        BMCASEID(I) = -1 ! THE STAND'S CASE ID (FOR DBS)

     end if

     43 continue
  end do

  !     NOW THAT WE'VE LOOPED THROUGH ALL STANDS AND INITIALIZED DB FLAGS
  !     MARK DBINIT FLAG TO TRUE.

  LDBINIT = .true.

  !     THIS IS TOO COMPLICATED, SINCE NONSTOCKABLES ARE KNOWN
  !     TO BE AT THE END, A PRIORI. BUT IT WORKS.

  !     LOOK FOR NONSTOCK STAND ID IN CURRENT LIST OF STANDS.
  !     IF FOUND, MARK AS NONSTOCKED, ELSE
  !     INCREMENT THE NUMBER OF BMSTNDs
  jstk2ns = 0
  do I = 1, NSSTND
     !        CALL CH8BSR (BMSTND, BMSTDS, MYLIST, NSSTDS(I), IP)
     call C26BSR (BMSTND, BMSTDS, MYLIST, NSSTDS(I), IP)
     if (IP .gt.0) then
        STOCK(IP) = .false.
        BMMGID(IP) = 'NSTK'
        jstk2ns = jstk2ns+1
     else if (IP .eq.0) then
        BMSTND = BMSTND+1
        J = BMSTND
        if (J .gt. MXSTND) goto 48 ! ISSUE WARNING?!
        BMSDIX(J) = J
        BMSTDS(J) = NSSTDS(I)
        BMMGID(J) = 'NSTK'
        STOCK(J) = .false.
     end if
     44 continue
  end do
  !     adjust downward the count of stocked stands
  !     for stands that were previously stocked stands but that were
  !     just now marked non-stocked because they were listed as such
  !     (i.e. as a supplemental record under keyword NONSTOCK)
  bmstkd = bmstkd-jstk2ns
  !  bmend is another counter derived differently.  it should agree
  bmend = bmstnd-nsstnd
  48 continue

  !     RE-SORT TO UPDATE MYLIST

  call C26SRT (BMSTND, BMSTDS, MYLIST, .true.)

  !     EXTRACT THE LOCATIONS AND AREA DATA.

  call SPLAEX (BMSTDS, BMSTND, MYLIST, IRC)

  !     IF THE RETURN CODE IS NOT ZERO, THEN THESE DATA ARE MISSING OR
  !     INCOMPLETE. THE REQUESTED OUTBREAK CAN NOT HAPPEN. SET A FLAG
  !     NOW...ISSUE ERROR MSG, ETC., BELOW.

  LBMSPR = IRC.eq.0
  if (LBMDEB) write(JBMBPR,'(/'' IN BMSETP: LBMSPR='',L2)') LBMSPR
  !      WRITE (*,*) BMSTD0, BMSTND, IRC
  if (LBMSPR) then

     !        COMPUTE TOTAL ACRES (Stockable stands)
     !     !! NO! THIS IS FETCHING TOTAL ACREAGE FOR ALL STANDS STOCKED AND OTHERWISE!
     !        BECAUSE BMSTND NOW INCLUDES NONSTOCKS AS WELL!

     TACRES = 0.0
     STKACR = 0.0
     do I = 1,BMSTND
        call SPLAAR (I,AREA,IRC)
        LBMSPR = IRC .eq.0
        !          IF (.NOT.LBMSPR) GOTO 60
        if (.not.LBMSPR) goto 100
        TACRES = TACRES+AREA
        if (STOCK(I))STKACR = STKACR+AREA
        50 continue
     end do
     !   60 CONTINUE
  else
     goto 100
  end if

  !     I'M LEAVING THE FOLLOWING IN, EVEN THOUGH IT SEEMS LIKE WE NEVER NEED NEIGHBORS DATA
  !     AJM DEC 05

  !     IF THE LOCATIONS AND AREA DATA ARE PRESENT, THEN: PREPARE THE
  !     NEIGHBORS DATA. SET LNEAR FALSE IF THERE ARE MISSING DATA.

  !      IF (LBMSPR) THEN
  call SPNBEX (BMSTDS,K,MYLIST,IRC)
  LNEAR = IRC .eq.0
  if (LBMDEB) write(JBMBPR,'(/'' IN BMSETP: LNEAR='',L2)') LNEAR

  !     BMSTAND IDS, NONSTOCK STATUS, AND AREA INFO DONE.  WE DON'T NEED TO REPEAT
  !     ANY OF THE ABOVE ANY TIME IN THE FUTURE.  SET "DONE" FLAG TO TRUE, AND
  !     ENTER THIS ROUTINE AT STATEMENT LABEL 65 IN SUBSEQUENT CYCLES

  LBMSETP = .true.

  65 continue

  !     THIS IS THE STUFF THAT NEEDS TO BE CHECKED EACH CYCLE!

  !     Set up year boundaries for dispersal.

  NPRMS = 1
  IBMYR2 = IBMYR1+IFIX(PRMS(1))-1
  IBMMRT = IBMYR2-IBMYR1+1

  if (LBMDEB) write (JBMBPR,70) IBMYR1,IBMYR2
  70 format (/' IN BMSETP: IBMYR1=',I5,' IBMYR2=',I5)

  ! If the duration goes over the boundary of a master cycle, then reschedule
  ! the outbreak for the next cycle and adjust the ending year to occur in the
  ! n-1 year within the current master cycle.

  if (IBMYR2.gt.MIY(MICYC)) then
     PRMS(1) = IBMYR2 - MIY(MICYC) + 1
     call GPADD (KODE,MIY(MICYC),301,NPRMS,PRMS(1),BMSTD0,BMSDIX)
     IBMYR2 = MIY(MICYC) - 1
     IBMMRT = IBMYR2-IBMYR1+1
     if (LBMDEB) write (JBMBPR,80) PRMS(1),IBMYR2
     80 format (/' IN BMSETP:  AN OUTBREAK IS SCHEDULED FOR THE ',&
          &'NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,&
          &'  NEW IBMYR2=',I5)
  end if
  go to 150
  !      ELSE
  100 continue

  !        TURN OFF THE OUTBREAK, AND WRITE AN ERROR MSG.

  IBMYR1 = 0
  BMSTND = 0
  write (JBMBPR,140) MIY(MICYC-1)
  140 format (/' ******** ERROR:  MPB/WPB/I DISPERSAL',&
       &' MODEL RUN SCHEDULED FOR ',I4,' WAS CANCELLED '/T19,&
       &'DUE TO A LACK OF REQUIRED SPATIAL DATA.')
  call RCDSET (2,.true.)

  !       ENDIF

  150 continue
  if (LBMDEB) write (JBMBPR,160) BMSTD0,MIY(MICYC-1),LBMSPR,LBMSETP,&
       &IBMYR1,IBMYR2,bmstnd,nsstnd,bmstkd,bmend,jstk2ns,&
       &TACRES,STKACR
  160 format (/' IN BMSETP: BMSTD0=',I5,'; MIY(MICYC-1)=',I5,&
       &'; LBMSPR=',L2,'; LBMSETP=',l2,'; IBMYR1=',I5,'; IBMYR2=',I5,/&
       &'  bmstnd= ',i5,'; nsstnd=',i5,'; bmstkd=',i5,'; bmend=',i5,&
       &'; jstk2ns= ',i5,'; TACRES=',F7.2,'; STKACRE=',F7.2)

  return
end
