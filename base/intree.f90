!=============================!
!   Read input tree records   !
!=============================!
! BASE $Id$
subroutine INTREE (RECORD,IRDPLV,ISDSP,SDLO,SDHI,LKECHO)
  implicit none

  !     IRDPLV= 0 IF PLOT SPECIFIC SITE VARIABLES ARE NOT BEING READ,
  !             1 IF THEY ARE BEING READ.

  !  THIS ROUTINE READS THE STAND TREE DATA AND SETS ALL VARIABLES
  !  WHICH ARE TREE RECORD SPECIFIC.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'HTCAL.f90'
  include 'ESTREE.f90'
  include 'STDSTK.f90'

  ! Variables
  integer :: IG,IULIM,IGRP,IPP,IAXE,ISPI,IUP,IHIT3,IHIT2,IHIT1,I,IK
  integer :: J,ITREI,ITH,IMC1,ITRRR,ITP1RR,K,NPNVRS,ISCRN,IPTKNT,IMAX
  integer :: II,INOSPC,IPVARS,IDAMCD,IRDPLV,ISDSP,IRTNCD
  real    :: SDHI,SDLO,THT
  logical :: DEBUG,LDELTR,LRDGO,LRDTE,LKECHO,LINCL,LCONN
  integer*4 :: IDCMP1,DBSKODE
  character(len=8) :: CSPI
  character(len=*) :: RECORD
  character(len=8) :: ANOSPC(1000)
  integer :: ICNTR1,ICNTR2,I3
  integer :: IOSTAT,IOSTATUS,FIASPCD
  dimension IPVARS(5),IDAMCD(6)
  common / INTREECOM / CSPI
  data IDCMP1/10000000/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'INTREE',6,ICYC)

  if ( .not. DEBUG ) go to 8
  write (JOSTND,5) ISTDAT,TREFMT(1:80),TREFMT(81:160),MORDAT,IRDPLV
  5 format (/' ENTERING SUBROUTINE INTREE.',/ &
       & ' DATA SET REFERENCE NUMBER =',I2,/ &
       & ' TREE DATA FORMAT:',/30X,A80/30X,A80,/, &
       & ' MORDAT: ',L2,'; IRDPLV=',I3)
  8 continue

  !     SET INITIAL VALUES FOR SPECIES TRANSLATION.

  INOSPC = 0
  do ICNTR1 = 1,1000
     ANOSPC(ICNTR1) = '        '
  end do

  !     SET FLAG TO SEE IF A FILE IS CONNECTED TO ISTDAT. WE WILL TRY TO OPEN
  !     ONE IF NECESSARY.

  LCONN = .false.

  !  IF MORE TREE RECORDS ARE TO BE READ AS IF THEY ARE PART
  !  OF THE STAND THAT WAS BEING READ DURING THE LAST CALL TO
  !  THIS SUBROUTINE THE LOGICAL VARIABLE 'MORDAT' MUST BE SET
  !  TRUE.  IF IT IS FALSE, WE ASSUME THAT A NEW STAND IS BEING READ.

  if (.not.MORDAT) then

     !        SET INITIAL VALUES

     IRECRD = 0
     IREC2 = MAXTP1
     IMAX = MAXTP1
     LSTKNT = 1
     NSTKNT = 0
     IOSTAT = 0

     !     INITIALIZE DAMAGE/SEVERITY ARRAY.

     do II = 1,MAXTRE
        do I = 1,6
           DAMSEV(I,II) = 0
        end do
     end do
  end if
  IPTKNT = 0
  if (LSTKNT.gt.1) IPTKNT = LSTKNT-1
  ISCRN = 0
  do I = 1,5
     IPVARS(I) = 0
     9 continue
  end do
  10 continue

  !     SET THE UPPER LIMIT ON THE NUMBER OF VALUES BEING READ ON EACH
  !     RECORD.

  NPNVRS = 1
  if (IRDPLV.gt.0) NPNVRS = 5

  !   CHECK TO SEE IF THE WESTERN ROOT DISEASE MODEL VER. 3.0 IS ACTIVE
  !   AND IF TREELIST INITIALIZATION IS BEING USED. IF IT IS THEN SET
  !   MAX NUMBER OF TREE RECORDS THAT MODEL CAN PROCESS TO IRRTRE.

  call RDATV(LRDGO,LRDTE)
  if (LRDGO .and. LRDTE) then
     call RDTRES(ITRRR,ITP1RR)
     IMAX = ITP1RR
  end if

  !  INSURE THERE IS ROOM IN THE ARRAYS TO STORE MORE TREE RECORDS

  if (IREC1+1 .lt. MIN0(IREC2,IMAX)) go to 30
  15 continue
  LSTKNT = IPTKNT

  if (LRDGO .and. LRDTE) write(JOSTND,895) ITRRR
  895 format('**** MAX NUMBER OF TREE RECORDS',I5,' EXCEEDED FOR', &
       & ' WESTERN ROOT DISEASE MODEL VER 3.0 ****')

  call ERRGRO(.false.,13)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  30 continue
  I = IREC1 + 1

  !  READ STAND DATA FILE            **********

  31 continue

  !  DETERMINE IF DATA IS COMING FROM DATABASE OR FILE

  DBSKODE = 1
  call DBSTREESIN(ITREI,IDTREE(I),PROB(I),ITH,CSPI, &
       & DBH(I),DG(I),HT(I),THT,HTG(I),ICR(I),IDAMCD(1),IDAMCD(2), &
       & IDAMCD(3),IDAMCD(4),IDAMCD(5),IDAMCD(6), &
       & IMC1,KUTKOD(I),IPVARS(1),IPVARS(2),IPVARS(3),IPVARS(4), &
       & IPVARS(5),DBSKODE,DEBUG,JOSTND,LKECHO,ABIRTH(I),LBIRTH(I), &
       & CULL(I),DECAYCD(I),WDLDSTEM(I))
  !  MAKE CORRECTIONS TO DECAY CODE IF NECESSARY
  if (ITH.gt.5.and.(DECAYCD(I).lt.1.or.DECAYCD(I).gt.5)) then
     DECAYCD(I) = 3
  else if (ITH .le. 5 .and. DECAYCD(I) .ne. 0) then
     DECAYCD(I) = 0
  end if
  if (DBSKODE.eq.0) then
     if (.not.LCONN) inquire(UNIT = ISTDAT,exist = LCONN)
     if (LCONN) inquire(UNIT = ISTDAT,opened = LCONN)
     if (.not.LCONN) then
        call fvsGetKeywordFileName(RECORD,len(RECORD),II)
        if (RECORD.eq.' ') goto 900
        II = index(RECORD,".k")
        if (II == 0) II = index(RECORD,".K")
        if (II == 0) II = len_trim(RECORD)
        RECORD = TRIM(RECORD(:II-1))//".TRE"
        inquire(FILE = TRIM(RECORD),exist = LCONN)
        if (.not.LCONN) then
           RECORD = TRIM(RECORD(:II-1))//".tre"
           inquire(FILE = TRIM(RECORD),exist = LCONN)
        end if
        if (LCONN) open(UNIT = ISTDAT,FILE = TRIM(RECORD),STATUS = 'OLD')
        inquire(UNIT = ISTDAT,exist = LCONN)
        if (.not.LCONN) goto 900
     end if
     read(ISTDAT,'(A)',IOSTAT = IOSTATUS)RECORD
     if (IOSTATUS.ne.0)goto 900
     if (RECORD(1:1).eq.'*') goto 31
     if (INDEX(RECORD,'-999').gt. 0) goto 900
     read (RECORD,TREFMT) ITREI,IDTREE(I),PROB(I),ITH,CSPI, &
          & DBH(I),DG(I),HT(I),THT,HTG(I),ICR(I),(IDAMCD(J),J = 1,6), &
          & IMC1,KUTKOD(I),IPVARS(1),IPVARS(2),IPVARS(3),IPVARS(4), &
          & IPVARS(5),ABIRTH(I)
     !     &        ,CULL(I),DECAYCD(I),WDLDSTEM(I)
     if (ABIRTH(I).le.0) then
        ABIRTH(I) = 0
        LBIRTH(I) = .false.
     else
        LBIRTH(I) = .true.
     end if
  else if (DBSKODE.eq.-1) then
     goto 900
  end if
  if (NPNVRS.eq.1) then
     do IK = 1,5
        IPVARS(IK) = 0
     end do
  end if
  IRECRD = IRECRD + 1
  if ( DEBUG ) then
     write(JOSTND,*) ' ',I,' IRECRD=',IRECRD,' ITREI=',ITREI,' IDT=', &
          & IDTREE(I),' PROB=',PROB(I),' ITH=',ITH,' CSPI=',CSPI, &
          & ' DBH=',DBH(I),' DG=',DG(I),' HT=',HT(I),' THT=',THT
     write(JOSTND,*) ' ',I,' HTG=',HTG(I),' ICR=',ICR(I),' IDAMCD=', &
          & IDAMCD,' IMC1=',IMC1,' KUTKOD= ',KUTKOD(I),' IPVARS=', &
          & (IPVARS(K),K = 1,5),' ABIRTH= ',ABIRTH(I),' DBSKODE=', &
          & DBSKODE, ' LBIRTH= ',LBIRTH(I)
  end if
  if (IDTREE(I) .gt. IDCMP1-1) write(JOSTND,35)
  35 format('IN INTREE: TREE ID NUMBER IS LARGER THAN MAXIMUM', &
       & ' ALLOWED')

  ! IF THIS IS A SITE TREE, THEN STORE THE SITE RELATED INFORMATION.
  ! IF THE TREE IS ON THE PLOT, THEN CONTINUE TO PROCESS THE TREE NORMALLY.
  ! IF THE TREE IS OFF THE PLOT, THEN READ THE NEXT TREE RECORD.

  IHIT1 = 0
  IHIT2 = 0
  IHIT3 = 0
  if (IDAMCD(1) .eq. 28) then
     IHIT1 = 1
  else if (IDAMCD(3) .eq. 28) then
     IHIT2 = 1
  else if (IDAMCD(5) .eq. 28) then
     IHIT3 = 1
  end if
  if (IHIT1.gt.0 .or. IHIT2.gt.0 .or. IHIT3.gt.0) then
     NSITET = NSITET + 1
     if (NSITET .gt. MAXSTR)go to 34
     if (CSPI .eq. '        ') CSPI = 'OT      '
     CSPI = ADJUSTL(CSPI)
     do IUP = 1,8
        call UPCASE(CSPI(IUP:IUP))
     end do

     !  CHECK FOR VALID SPECIES CODE AND SET OUTPUT DATA BASE
     !  TABLE FORMAT ARRAY JSPIN BASED ON INPUT SPECIES CODE FORMAT

     do ISPI = 1,MAXSP
        if (CSPI.eq.JSP(ISPI)) then
           JSPIN(ISPI) = 1
           if (JSPINDEF.le.0)JSPINDEF = 1
           go to 33
        else if (CSPI.eq.FIAJSP(ISPI)) then
           JSPIN(ISPI) = 2
           if (JSPINDEF.le.0)JSPINDEF = 2
           go to 33
        else if (CSPI.eq.PLNJSP(ISPI)) then
           JSPIN(ISPI) = 3
           if (JSPINDEF.le.0)JSPINDEF = 3
           go to 33
        end if
     end do
     call SPCTRN (CSPI, ISPI)
     33 continue
     SITETR(NSITET,1) = ISPI
     SITETR(NSITET,2) = DBH(I)
     SITETR(NSITET,3) = HT(I)
     SITETR(NSITET,4) = ABIRTH(I)
     IAXE = 0
     if (IHIT1 .gt. 0) then
        if (IDAMCD(2).eq.1 .or. IDAMCD(2).eq.3)SITETR(NSITET,5) = 1.
        if (IDAMCD(2).eq.2 .or. IDAMCD(2).eq.4)SITETR(NSITET,5) = 2.
        if (IDAMCD(2).eq.1 .or. IDAMCD(2).eq.2)SITETR(NSITET,6) = 1.
        if (IDAMCD(2).eq.3 .or. IDAMCD(2).eq.4) then
           SITETR(NSITET,6) = 2.
           IAXE = 1
        end if
     else if (IHIT2 .gt. 0) then
        if (IDAMCD(4).eq.1 .or. IDAMCD(4).eq.3)SITETR(NSITET,5) = 1.
        if (IDAMCD(4).eq.2 .or. IDAMCD(4).eq.4)SITETR(NSITET,5) = 2.
        if (IDAMCD(4).eq.1 .or. IDAMCD(4).eq.2)SITETR(NSITET,6) = 1.
        if (IDAMCD(4).eq.3 .or. IDAMCD(4).eq.4) then
           SITETR(NSITET,6) = 2.
           IAXE = 1
        end if
     else
        if (IDAMCD(6).eq.1 .or. IDAMCD(6).eq.3)SITETR(NSITET,5) = 1
        if (IDAMCD(6).eq.2 .or. IDAMCD(6).eq.4)SITETR(NSITET,5) = 2
        if (IDAMCD(6).eq.1 .or. IDAMCD(6).eq.2)SITETR(NSITET,6) = 1
        if (IDAMCD(6).eq.3 .or. IDAMCD(6).eq.4) then
           SITETR(NSITET,6) = 2
           IAXE = 1
        end if
     end if
     if (IAXE .eq. 1)go to 30
  end if
  34 continue

  !  CONVERT INPUT CROWN CLASS VALUES (1-9) TO PERCENT
  !  CROWN RATIO (1-99) IF NEEDED

  if (ICR(I).le.0)goto 37
  if (ICR(I).lt.10) then
     ICR(I) = ICR(I)*10-5
  else
     if (ICR(I).gt.99)ICR(I) = 99
  end if
  37 continue

  !  COUNT UP THE NUMBER OF POINTS.  'IPVEC' IS A SUB-PLOT I.D. VECTOR
  !  USED TO KEEP TRACK OF SUB-PLOT IDENTIFICATION CODES THAT HAVE
  !  ALREADY BEEN USED.
  !  ADDITIONAL TESTING AND VALIDATION OF PLOT COUNTS SPECIFIED ON DESIGN
  !  KEYWORD OR NUM_PLOTS OF INPUT DATABASE STANDINIT TABLE TO ENSURE THAT
  !  TREE RECORD PLOT ID CAN BE USED IN PROCESSES UTILITZING PLOT LEVEL
  !  VARIABLES, SUCH AS DENSITIES, CAN BE USED AND TPA EXPANSIONS ARE CORRECT:
  !    1 - IF NUM_PLOTS IS 1, PLOT ID WILL BE RETAINED WITH THE FOLLOWING
  !        EXCEPTION. IN ORDER TO ACCOMMODATE CIRCUMSTANCE WHERE TREE
  !        RECORDS WERE ACCUMULATED FROM MULTIPLE PLOTS AND HAVE MORE
  !        THAN ONE PLOT ID EXISTING ON THE TREE RECORDS BUT THE STAND HAS
  !        BEEN DEFINED AS A SINGLE PLOT, THE PLOT ID USED WILL BE 1.
  !        LRD 02/04/2022

  !    *** NEXT 2 SCENERIOS ARE EVELUATED IN CALLING SUBROUTINE INITRE.F ***
  !    2 - IF NUM_PLOTS IS >1 AND COUNTED TREE RECORD PLOT IDS IS LESS THAN
  !        THAT VALUE, PRESENCE OF NONSTOCKED PLOTS IS ASSUMED.
  !    3   IF NUM_PLOTS IS >1 AND COUNTED TREE RECORD PLOT IDS IS GREATER THAN
  !        THAT VALUE, THE RUN WILL BE TERMINATED BECAUSE ACCURATE TPA VALUES
  !        CAN NOT BE CALCULATED.

  if ( IPTKNT .lt. LSTKNT ) go to 50
  do IPP = LSTKNT, IPTKNT
     if ( IPVEC(IPP) .eq. ITREI) then
        !         TREE RECORD IS ON EXISTING PLOT
        go to 60
     else
        if (IPTINV .eq. 1) then ! ALL TREES ON 1 PLOT (1 ABOVE)
           !           A SINGLE PLOT HAS BEEN SPECIFIED BUT NEW PLOT ID HAS BEEN
           !           READ. PLOT ID WILL BE SET TO 1 SINCE MULTIPLE PLOT IDS
           !           EXIST IN THE INPUT TREE RECORDS.
           ITREI = 1
           IPVEC(IPP) = 1
           go to 60
        end if
     end if
     40 continue
  end do
  50 continue
  IPTKNT = IPTKNT + 1
  if (IPTKNT .gt. MAXPLT) go to 15
  IPVEC(IPTKNT) = ITREI
  ITRE(I) = IPTKNT

  !  IF POINT SPECIFIC SITE DATA ARE BEING READ AND IF THIS PLOT IS
  !  A STOCKABLE PLOT (IMC1.GT.800), THEN: CALL THE ESTABLISHMENT
  !  SYSTEM TO STORE THE SITE DATA.

  call ESPLT1 (ITREI,IMC1,NPNVRS,IPVARS(1))
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  goto 65
  60 continue
  ITRE(I) = IPP
  65 continue

  !  FLAG INCOMING TREE RECORDS SO MORTS WILL BE ABLE TO APPLY CORRECT
  !  MORTALITY RATES.  VECTOR IESTAT IS PART OF THE REGENERATION
  !  ESTABLISHMENT SYSTEM.

  IESTAT(I) = 0

  !  INITIALIZE SERIAL CORRELATION.

  ZRAND(I) = -999.

  !  COUNT THE NUMBER ON NON-STOCKABLE SUB-PLOTS.
  !  IF IMC1 IS EQUAL TO 8 THE POINT IS NON-STOCKABLE

  if ( IMC1 .ne. 8 ) go to 70
  NSTKNT = NSTKNT + 1
  go to 30
  70 continue

  !  TRANSLATE SPECIES CODES TO 1 THROUGH MAXSP AND
  !  SET SPECIES DATA BASE TABLE OUTPUT FORMAT ARRAY
  !  JSPIN BASED ON INPUT SPECIES CODE FORMAT

  if (CSPI .eq. '        ') CSPI = 'OT      '
  CSPI = ADJUSTL(CSPI)
  do IUP = 1,8
     call UPCASE(CSPI(IUP:IUP))
  end do
  do ISPI = 1,MAXSP
     if (CSPI.eq.JSP(ISPI)) then
        JSPIN(ISPI) = 1
        if (JSPINDEF.le.0)JSPINDEF = 1
        go to 72
     else if (CSPI.eq.FIAJSP(ISPI)) then
        JSPIN(ISPI) = 2
        if (JSPINDEF.le.0)JSPINDEF = 2
        go to 72
     else if (CSPI.eq.PLNJSP(ISPI)) then
        JSPIN(ISPI) = 3
        LFIA = .true.
        if (JSPINDEF.le.0)JSPINDEF = 3
        go to 72
     end if
     71 continue
  end do

  !  SET SPECIES CODE TO MAXSP IF CODE GIVEN IS INVALID.
  !  SPECIES ARE TRANSLATED TO APPROPRIATE VALID SPECIES CODE.

  call SPCTRN (CSPI, ISPI)

  !  WRITE SPECIES MAPPING

  if (INOSPC.eq.0) then
     write(JOSTND,73)
     INOSPC = 1
  end if
  73 format(/)
  do ICNTR2 = 1,INOSPC
     if (CSPI.eq.ANOSPC(ICNTR2)) go to 74
  end do
  write(JOSTND,75) CSPI,JSP(ISPI)
  75 format(T12,'NOTE: INPUT SPECIES CODE (',A8,')WAS SET TO (' &
       & ,A4,') FOR THIS PROJECTION.')
  INOSPC = INOSPC+1
  if (INOSPC.gt.1000)INOSPC = 1000
  ANOSPC(INOSPC) = CSPI
  74 continue

  72 continue

  !  SAVE SPECIES POINTERS IN ISP

  ISP(I) = ISPI

  !---------- HANDLE WOODLAND SPECIES STEM COUNT NOW THAT WE KNOW INPUT SPECIES
  read(FIAJSP(ISPI), '(I4)')FIASPCD
  select case (FIASPCD)
  case (62,  63,  65,  66, 69, 106, 133, 134, &
       & 143, 321, 322, 475, 803, 810, 814, 843)
     if (WDLDSTEM(I) .eq. 0) WDLDSTEM(I) = 1
  case default
     WDLDSTEM(I) = 0
  end select

  !  CHECK TO SEE IF THIS IS A PROJECTABLE TREE RECORD. TREES WITH
  !  HISTORY CODES 6,7,8 AND 9 ARE DEAD TREES ACCORDING TO REGION 1
  !  STANDARDS. CODES 6 AND 7 REPRESENT RECENT DEAD TREES.

  !  SCREEN TREE OUT IF IT FALLS WITHIN THE SPECIES/DBH SCREEN. THIS WAS
  !  SET UP FOR VARIANTS LIKE AK TO INSURE THAT LARGE NUMBER OF SEEDLINGS
  !  DO NOT ENTER THE SYSTEM AND GIVE MISLEADING RESULTS WHEN THEY
  !  GET LARGER AND CREATE 100'S OF FT SQ OF BASAL AREA.

  !  SET LINCL

  LINCL = .false.
  if (ISDSP.eq.0 .or. ISDSP.eq.ISP(I)) then
     LINCL = .true.
  else if (ISDSP.lt.0) then
     IGRP = -ISDSP
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
           LINCL = .true.
           go to 710
        end if
        705 continue
     end do
  end if
  710 continue

  !   ADD CONDITIONAL FOR TREES WITH HT < 4.5,
  !   WILL DUB DBH OF NULL ENTRIES WITH VALUE OF 0.1
  if (HT(I) > 0 .and. HT(I) < 4.5 .and. DBH(I) .eq. 0) DBH(I) = 0.1

  LDELTR = (DBH(I) .lt. 0.0001) .or. &
       & (LINCL .and. (DBH(I) .lt. SDLO .or. DBH(I) .ge. SDHI))

  !   CALL DAMCDS TO PROCESS DAMAGE CODES AND OTHER TREE ATTRIBUTES FOR
  !   ANY EXTENSIONS THAT MAY BE ACTIVE.

  call DAMCDS (I,IDAMCD)

  !     NOW, IF THE TREE IS 'FLAGED' TO BE DELETED, THEN BRANCH TO
  !     READ ANOTHER TREE RECORD.

  if (LDELTR) then
     if (DBH(I) .ge. 0.0001) ISCRN = ISCRN+1
     go to 30
  end if

  !  IF THE TREE WAS TOPKILLED (AS OBSERVED DURING SAMPLING), THE
  !  VALUE OF HEIGHT IS FLAGGED AS NEGATIVE - THIS IS CORRECTED TO
  !  A HEIGHT AS PREDICTED BY THE HEIGHT-DIAMETER CURVE IN SUBROUTINE
  !  'CRATET'. REGION 1 DAMAGE CODES: 96=BROKEN TOP, 97=DEAD TOP.
  !   SEARCH THROUGH THE DAMAGE ARRAY TO FIND ANY TOP DEAD OR BROKEN
  !   TREES. EVEN IF DAMAGE CODE 96 OR 97 ARE NOT PRESENT, IF HEIGHT
  !   TO THE BROKEN TOP IS ENTERED, FLAG THE TREE.
  !   WHEN BOTH TOTAL TREE HEIGHT (HT) AND TOPKILL HEIGHT (THT) ARE
  !   ENTERED, HT MUST BE GREATER THAN THT. IF NOT, HT IS SET TO ZERO
  !   AND THE HEIGHT DETERMINED IN CRATET WILL BE USED.

  ITRUNC(I) = 0
  NORMHT(I) = 0
  do K = 1,5,2
     if (IDAMCD(K) .ne. 96 .and. IDAMCD(K) .ne. 97) go to 81
     go to 82
     81 continue
  end do
  if (THT .gt. 0.)go to 82
  go to 90

  82 continue
  !     PROCESS TOPKILL TREE

  NORMHT(I) = -1
  if (THT.le.0.0) go to 90
  ITRUNC(I) = INT(THT*100.0+0.5)
  !      write(*,*) 'at 82: I=',i,' NORMHT=',normht(i),' HT=',ht(i),
  !     & ' THT=',tht,
  !     & ' ITRUNC=',(itrunc(i)/100.0),' ITH=',ith,' IDAMCD=',idamcd
  if (HT(I) .le. THT) then
     HT(I) = 0.0
  end if
  !      write(*,*) ' NORMHT=',normht(i),' HT=',ht(i),' THT=',tht,
  !     & ' ITRUNC=',itrunc(i)
  90 continue

  !  IF THE TREE HEIGHT IS ZERO (MISSING), THEN INSURE THAT THE HEIGHT
  !  GROWTH IS ZERO AS WELL

  if (HT(I).eq.0.0 .and. IHTG.eq.3) HTG(I) = 0.0

  !  IF GROWTH METHODS ARE FOR REMEASUREMENT DATA, STORE THE
  !  INCOMING VALUES FOR PRINTING IN **FVSSTD**

  if (IDG.eq.1 .or. IDG.eq.3) PDBH(I) = DG(I)
  if (IHTG.eq.1 .or. IHTG.eq.3) PHT(I) = HTG(I)

  !  STORE ALL DEAD TREES IN THE BOTTOM OF THE ARRAYS
  !  TREES WITH HISTORY CODES 6,7 ARE RECENT DEAD (GET IMC()=7)
  !  TREES WITH HISTORY CODES 8,9 ARE OLDER  DEAD (GET IMC()=9)

  if ( ITH .lt. 6 .or. ITH .gt. 9 ) go to  100
  IREC2 = IREC2 - 1
  ITRE(IREC2) = ITRE(I)
  IDTREE(IREC2) = IDTREE(I)

  !  NOTE: INFLATION OF DEAD TREE PROB'S IS IN NOTRE.  CROWN RATIOS
  !        FOR TREES LESS THAN 3 INCHES ARE SET ZERO IN CRATET.

  PROB(IREC2) = PROB(I)
  DBH(IREC2) = DBH(I)
  DG(IREC2) = DG(I)
  HT(IREC2) = HT(I)
  ITRUNC(IREC2) = ITRUNC(I)
  NORMHT(IREC2) = NORMHT(I)
  ICR(IREC2) = ICR(I)
  ISP(IREC2) = ISP(I)
  ISPECL(IREC2) = ISPECL(I)
  DEFECT(IREC2) = DEFECT(I)
  IMC(IREC2) = 7
  if (ITH.eq.8 .or. ITH.eq.9) IMC(IREC2) = 9
  ABIRTH(IREC2) = ABIRTH(I)
  LBIRTH(IREC2) = LBIRTH(I)
  KUTKOD(IREC2) = KUTKOD(I)
  CULL(IREC2) = CULL(I)
  DECAYCD(IREC2) = DECAYCD(I)

  read(FIAJSP(ISP(I)), '(I4)')FIASPCD
  select case (FIASPCD)
  case (62,  63,  65,  66, 69, 106, 133, 134, &
       & 143, 321, 322, 475, 803, 810, 814, 843)
     if (WDLDSTEM(I) .eq. 0) then
        WDLDSTEM(IREC2) = 1
     else
        WDLDSTEM(IREC2) = WDLDSTEM(I)
     end if
  case default
     WDLDSTEM(IREC2) = 0
  end select

  do I3 = 1,6
     DAMSEV(I3,IREC2) = DAMSEV(I3,I)
  end do

  ! RESET ARRAY VALUES TO ZERO IN CASE THIS IS THE LAST RECORD
  ! IN THE INPUT DATA --- DON'T WANT THESE VALUES HANGING AROUND
  ! WHERE THEY AREN'T SUPPOSED TO BE.  GD 5/10/96

  PROB(I) = 0.
  DBH(I) = 0.
  DG(I) = 0.
  HT(I) = 0.
  ITRUNC(I) = 0
  NORMHT(I) = 0
  ICR(I) = 0
  ISP(I) = 0
  ISPECL(I) = 0
  DEFECT(I) = 0
  IDTREE(I) = 0
  HTG(I) = 0.
  ITRE(I) = 0
  NORMHT(I) = 0
  PDBH(I) = 0.
  PHT(I) = 0.
  ZRAND(I) = -999.
  ABIRTH(I) = 0.
  LBIRTH(I) = .false.
  KUTKOD(I) = 0
  CULL(I) = 0
  DECAYCD(I) = 0
  WDLDSTEM(I) = 0
  do I3 = 1,6
     DAMSEV(I3,I) = 0
  end do

  I = IREC2
  go to 200
  100 continue
  if (IMC1.gt.3)IMC1 = 3
  if (IMC1.le.0)IMC1 = 1
  IMC(I) = IMC1
  IREC1 = I
  200 continue

  !  ESTABLISH THIS TREE RECORD AS A LINK IN THE CHAIN SORT.

  call LNKCHN (I)

  go to 10

  900 continue

  !  END OF TREE DATA ON THIS FILE

  LSTKNT = IPTKNT + 1

  if (NSITET .gt. MAXSTR)NSITET = MAXSTR

  !   CALL INTREX TO READ ANY ADDITIONAL DATA THAT MAY BE NEEDED BY
  !   EXTENSIONS.

  !***  CALL INTREX (IDTREE)

  if (ISCRN .gt. 0) write(JOSTND,901) ISCRN
  901 format(/,T12,'**** NOTE: ',I5,' TREE RECORDS HAVE BEEN', &
       & ' SCREENED OUT BY THE DIAMETER SCREEN.')
  if ((DBSKODE.eq.-1).and.LKECHO)write (JOSTND,940) IRECRD
  940 format (T12,'NUMBER ROWS PROCESSED:',I5)
  if (DEBUG) then
     write(JOSTND,945) IRECRD,IPTKNT,DBSKODE
     945 format (/' LEAVING SUBROUTINE INTREE.   RECORDS READ=',I5, &
          & '; PLOTS COUNTED = ',I5,'; DBSKODE=',I4)
     if (NSITET.gt.0)write(JOSTND,950)((SITETR(I,J),J = 1,6),I = 1,NSITET)
     950 format(/' SITE INDEX TREES:',/,7X,'ISPC',7X,'DBH',8X,'HT', &
          & 7X,'AGE',4X,'T OR B',4X,'ON OFF',20(/,1X,F10.0,2F10.1,3F10.0))
  end if

  return
end subroutine INTREE
