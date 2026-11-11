!===========================!
!   metric/vbase : disply   !
!===========================!
! METRIC-BASE $Id$
subroutine DISPLY
  implicit none

  !  **DISPLY** PRINTS STAND AND TREE STATISTICS AT THE BEGINNING
  !  OF THE PROJECTION, AND AT THE END OF EACH CYCLE.  ESTIMATES
  !  STAND AGE IF MISSING FROM THE STNDINFO KEYWORD

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'ECON.f90'
  include 'SCREEN.f90'
  include 'SUMTAB.f90'
  include 'VARCOM.f90'
  include 'METRIC.f90'

  !  DIMENSIONS FOR INTERNAL VARIABLES (USED AS OUTPUT LABELS):

  ! Variables
  integer :: IRT,JYR,I,IOAGE,IKNT,IXF,IFIA,ISWT,J,JSDI,ISNOFT
  integer :: IRTNCD
  real    :: DUM1,SUMAGE,AGEKNT,SDIBCTMP,SDIACTMP
  character(len=9) :: AT1
  character(len=9) :: AT2
  character(len=9) :: AT3
  character(len=9) :: AT4
  character(len=10) :: STD(3)

  !  DATA STATEMENTS:

  data AT1/'REMOVAL  '/,AT2/'VOLUME:  '/,AT3/&
       &'RESIDUAL '/,AT4/'ACCRETION'/

  !  IF ICL6 IS NEGATIVE,  PROJECTION IS COMPLETE AND WE NEED ONLY TO
  !  PRINT THE FINAL LINE FOR  THE SAMPLE TREE OUTPUT  (DISPLAYING
  !  STAND ATTRIBUTES FOLLOWING THINNING), AND THE FINAL LINE OF THE
  !  SUMMARY OUTPUT.   BRANCH TO STATEMENT 34.

  if (.not.LSTART.or.ITABLE(2).eq.1) go to 10
  IRT = 0
  write (JOTREE) IRT,NPLT,MGMID
  10 continue
  if (ICL6.lt.0) go to 34

  !  ASSIGN JYR.

  JYR = IY(ICYC+1)

  !   IF USER HAS SPECIFIED KEYWORDS THAT ALTER VOLUME EQUATION PARAMETERS
  !   (IE VOLUMME BFVOLUME,ETC) THEN PRINT LABEL THAT INDICATES USER
  !   SPECIFIED STANDARDS. THESE LABLES ARE VARIANT SPECIFIC

  if ((VARACD .eq. 'CS') .or. (VARACD .eq. 'LS') .or.&
       &(VARACD .eq. 'NE') .or. (VARACD .eq. 'SN')) then

     if (LCVOLS) then
        STD(1) = 'USER MERCH'
        STD(2) = 'SAWLG'
     else
        STD(1) = 'MERCH'
        STD(2) = 'SAWLG'
     end if

     if (LBVOLS) then
        STD(3) = 'USER SAWLG'
     else
        STD(3) = 'SAWLG'
     end if

  else if ((VARACD .eq. 'ON')) then

     if (LCVOLS) then
        STD(1) = 'GTV'
        STD(2) = 'GMV'
     else
        STD(1) = 'GTV'
        STD(2) = 'GMV'
     end if

     if (LBVOLS) then
        STD(3) = 'NMV'
     else
        STD(3) = 'NMV'
     end if

  else

     if (LCVOLS) then
        STD(1) = 'USER TOTAL'
        STD(2) = 'USER MERCH'
     else
        STD(1) = 'TOTAL'
        STD(2) = 'MERCH'
     end if

     if (LBVOLS) then
        STD(3) = 'USER MERCH'
     else
        STD(3) = 'MERCH'
     end if
  end if

  !  WRITE REMOVAL STATISTICS.  BYPASS IF LSTART IS TRUE OR NUMBER
  !  OF TREES REMOVED WAS LESS THAN OR EQUAL TO ZERO.
  !  SKIP PRINTING OF STAND COMPOSITION TABLE IF NOT WANTED.

  if (ITABLE(1) .eq.1) go to 32
  if (LSTART) go to 30
  if (ONTREM(7).le.0.0) go to 20
  write(JOSTND,9003)
  9003 format(/)
  write(JOSTND,9004) AT1,&
       &(ONTREM(I)*INtoCM,I = 1,6),&
       &ONTREM(7)/ACRtoHA,&
       &(OSPTT(I),IOSPTT(I),I = 1,4)
  9004 format(6X,A9,3X,5F7.1,F8.1,F9.0,' TREES   ',&
       &3(F5.0,'% ',A3,','),F5.0,'% ',A3)

  select case (VARACD)
  case ('BC')
     write(JOSTND,9008) AT2,&
          &STD(1),&
          &(OCVREM(I)*INtoCM,I = 1,6),&
          &OCVREM(7)*FT3pACRtoM3pHA,&
          &(OSPTV(I),IOSPTV(I),I = 1,4),&
          &STD(2),&
          &(OMCREM(I)*INtoCM,I = 1,6),&
          &OMCREM(7)*FT3pACRtoM3pHA,&
          &(OSPMR(I),IOSPTV(I),I = 1,4)
  case default
     write(JOSTND,9005) AT2,&
          &STD(1),&
          &(OCVREM(I)*INtoCM,I = 1,6),&
          &OCVREM(7)*FT3pACRtoM3pHA,&
          &(OSPTV(I),IOSPTV(I),I = 1,4),&
          &STD(2),&
          &(OMCREM(I)*INtoCM,I = 1,6),&
          &OMCREM(7)*FT3pACRtoM3pHA,&
          &(OSPMR(I),IOSPTV(I),I = 1,4),&
          &STD(3),&
          &(OBFREM(I)*INtoCM,I = 1,6),&
          &OMCREM(7)*FT3pACRtoM3pHA
  end select
  9005 format(6X,A9/8X,A10,5F7.1,F8.1,F9.0,' CU M    ',&
       &3(F5.0,'% ',A3,','),F5.0,'% ',A3/&
       &8X,A10,5F7.1,F8.1,F9.0,' CU M    ',&
       &3(F5.0,'% ',A3,','),F5.0,'% ',A3/&
       &8X,A10,5F7.1,F8.1,F9.0,' CU M    ',&
       &3(F5.0,'% ',A3,','),F5.0,'% ',A3)
  9008 format(6X,A9/8X,A10,5F7.1,F8.1,F9.0,' CU M    ',&
       &3(F5.0,'% ',A3,','),F5.0,'% ',A3/&
       &8X,A10,5F7.1,F8.1,F9.0,' CU M    ',&
       &3(F5.0,'% ',A3,','),F5.0,'% ',A3)

  write(JOSTND,9003)
  write(JOSTND,9004) AT3,&
       &(ONTRES(I)*INtoCM,I = 1,6),&
       &ONTRES(7)/ACRtoHA,&
       &(OSPRT(I),IOSPRT(I),I = 1,4)

  !  WRITE ACCRETION AND MORTALITY STATISTICS.  BYPASSED IF LSTART
  !  IS TRUE. IF MORTALITY FUNCTIONS HAVE BEEN MODIFIED BY CERTAIN
  !  KEYWORDS THEN CHANGE LABEL FROM 'MORTALITY' TO 'USER MORT'.

  20 continue
  write(JOSTND,9003)
  write(JOSTND,9006) AT4,&
       &(OACC(I)*INtoCM,I = 1,6),&
       &OACC(7)*FT3pACRtoM3pHA,&
       &(OSPAC(I),IOSPAC(I),I = 1,4)

  if (LMORT) then
     write(JOSTND,9006) 'USER MORT',&
          &(OMORT(I)*INtoCM,I = 1,6),&
          &OMORT(7)*FT3pACRtoM3pHA,&
          &(OSPMO(I),IOSPMO(I),I = 1,4)
  else
     write(JOSTND,9006) 'MORTALITY',&
          &(OMORT(I)*INtoCM,I = 1,6),&
          &OMORT(7)*FT3pACRtoM3pHA,&
          &(OSPMO(I),IOSPMO(I),I = 1,4)
  end if
  9006 format(6X,A9,3X,5F7.1,F8.1,F9.0,' CU M/YR ',&
       &3(F5.0,'% ',A3,','),F5.0,'% ',A3)

  !  WRITE CURRENT STAND STATISTICS.

  30 continue
  write(JOSTND,9003)
  write(JOSTND,9007) JYR,&
       &(ONTCUR(I)*INtoCM,I = 1,6),&
       &ONTCUR(7)/ACRtoHA,&
       &(OSPCT(I),IOSPCT(I),I = 1,4)
  9007 format(/I4,'  TREES',T19,5F7.1,F8.1,F9.0,' TREES   ',&
       &3(F5.0,'% ',A3,','),F5.0,'% ',A3)

  select case (VARACD)
  case ('BC')
     write(JOSTND,9008) AT2,&
          &STD(1),&
          &(OCVCUR(I)*INtoCM,I = 1,6),&
          &OCVCUR(7)*FT3pACRtoM3pHA,&
          &(OSPCV(I),IOSPCV(I),I = 1,4),&
          &STD(2),&
          &(OMCCUR(I)*INtoCM,I = 1,6),&
          &OMCCUR(7)*FT3pACRtoM3pHA,&
          &(OSPMC(I),IOSPMC(I),I = 1,4)
  case default
     write(JOSTND,9005) AT2,&
          &STD(1),&
          &(OCVCUR(I)*INtoCM,I = 1,6),&
          &OCVCUR(7)*FT3pACRtoM3pHA,&
          &(OSPCV(I),IOSPCV(I),I = 1,4),&
          &STD(2),&
          &(OMCCUR(I)*INtoCM,I = 1,6),&
          &OMCCUR(7)*FT3pACRtoM3pHA,&
          &(OSPMC(I),IOSPMC(I),I = 1,4),&
          &STD(3),&
          &(OBFCUR(I)*INtoCM,I = 1,6),&
          &OBFCUR(7)*FT3pACRtoM3pHA,&
          &(OSPBV(I),IOSPBV(I),I = 1,4)
  end select
  32 continue

  !  WRITE OUTPUT FOR SAMPLE TREES.  FIRST, OUTPUT STAND CONDITIONS
  !  FOR PREVIOUS CYCLE.  BYPASS IF LSTART IS TRUE. ALSO SKIP TREE
  !  DATA IF TABLE IS NOT DESIRED.

  if (LSTART) go to 39
  34 continue
  IOAGE = IAGE+IY(ICYC)-IY(1)
  if (ITABLE(2) .eq.1) go to 41

  !  GO TO STATEMENT 35 AND WRITE BOTH PRE AND POST THINNING STATISTICS
  !  IF THINNING OCCURRED IN PREVIOUS CYCLE.

  if (LZEIDE) then
     JSDI = INT(SDIBC2 + 0.5)
  else
     JSDI = INT(SDIBC + 0.5)
  end if
  if (ONTREM(7).gt.0.0) go to 35
  IRT = 1
  write(JOTREE) IRT,&
       &IOAGE,&
       &ORMSQD*INtoCM,&
       &OLDTPA/ACRtoHA,&
       &OLDBA*FT2pACRtoM2pHA,&
       &OLDAVH*FTtoM,&
       &RELDM1,&
       &JSDI/ACRtoHA
  go to 36
  35 continue
  IRT = 2
  write(JOTREE) IRT,&
       &IOAGE,&
       &ORMSQD*INtoCM,&
       &OLDTPA/ACRtoHA,&
       &OLDBA*FT2pACRtoM2pHA,&
       &OLDAVH*FTtoM,&
       &RELDM1,&
       &JSDI/ACRtoHA
  IRT = 3
  JSDI = INT(SDIAC + 0.5)
  if (LZEIDE) then
     JSDI = INT(SDIAC2 + 0.5)
  else
     JSDI = INT(SDIAC + 0.5)
  end if
  write(JOTREE) IRT,&
       &ATAVD*INtoCM,&
       &ATTPA/ACRtoHA,&
       &ATBA*FT2pACRtoM2pHA,&
       &ATAVH*FTtoM,&
       &ATCCF,&
       &JSDI/ACRtoHA
  36 continue

  !  IF ICL6.LT.0 BRANCH TO STMT. 90 TO LOAD LAST LINE OF SUMMARY OUTPUT

  if (ICL6.lt.0) go to 90

  !  WRITE YEAR  AND PERIOD LENGTH.

  39 continue
  if (ITABLE(2).eq.1)go to 41

  !  IF SAMPLE TREES WERE RESELECTED THIS CYCLE (IFST=99), THEN SET
  !  SIGN OF YEAR NEGETIVE.

  I = JYR
  if (IFST.eq.0) goto 40
  IFST = 0
  if (LSTART) goto 40
  I = -I
  40 continue
  IRT = 4
  write(JOTREE) IRT,I,IFINT

  !  WRITE EXAMPLE TREE DATA.

  IRT = 5
  write(JOTREE) IRT,&
       &IONSP,&
       &DBHIO*INtoCM,&
       &HTIO*FTtoM,&
       &IOICR,&
       &DGIO*INtoCM,&
       &PCTIO,&
       &PRBIO/ACRtoHA
  41 continue

  !  LOAD THE ARRAY CONTAINING SUMMARY DATA.

  IKNT = ICYC+1

  !  CALCULATE FOREST TYPE (IFORTP), SIZE CLASS (ISZCL),
  !  AND STOCKING CLASS (ISTCL)

  DUM1 = 0.
  IXF = 2
  ISNOFT = IFORTP
  call FORTYP(IXF,DUM1)

  !  ESTIMATE STAND AGE FROM SIZE CLASS AND ABIRTH
  !  IF IT WAS MISSING FROM THE INPUT DATA, SET IT FROM THIS ESTIMATE..

  if (LSTART) then

     !  INITIALIZE THE SUM OF REMOVALS

     TRTPA = 0.
     TRTCUFT = 0.
     TRMCUFT = 0.
     TRBDFT = 0.
     SUMAGE = 0.
     AGEKNT = 0.
     !*        WRITE(JOSTND,*)' IAGE,ISZCL,ITRN= ',IAGE,ISZCL,ITRN
     if (ISZCL.eq.0.or. ISZCL.gt.3) then
        ICAGE = 0
        go to 42
     else if (ISZCL .eq.3) then
        do I = 1,ITRN
           if (DBH(I).lt.5.) then
              SUMAGE = SUMAGE+ABIRTH(I)*PROB(I)
              AGEKNT = AGEKNT+PROB(I)
              !*            WRITE(JOSTND,*)' I,D,AGE,PROB,SUMAGE,AGEKNT= ',
              !*     &      I,DBH(I),ABIRTH(I),PROB(I),SUMAGE,AGEKNT
           end if
        end do
     else if (ISZCL .eq.2) then
        do I = 1,ITRN
           if (FIAJSP(ISP(I))(1:1).eq.'-') then
              IFIA = 998
           else
              read(FIAJSP(ISP(I)),'(I4)') IFIA
           end if
           if ((IFIA.lt.300..and. DBH(I).ge.5..and. DBH(I).lt.9.) .or.&
                &(IFIA.ge.300..and. DBH(I).ge.5..and. DBH(I).lt.11.)) then
              SUMAGE = SUMAGE+ABIRTH(I)*PROB(I)
              AGEKNT = AGEKNT+PROB(I)
              !*            WRITE(JOSTND,*)' I,IFIA,D,AGE,PROB,SUMAGE,AGEKNT= ',
              !*     &      I,IFIA,DBH(I),ABIRTH(I),PROB(I),SUMAGE,AGEKNT
           end if
        end do
     else
        do I = 1,ITRN
           if (FIAJSP(ISP(I))(1:1).eq.'-') then
              IFIA = 998
           else
              read(FIAJSP(ISP(I)),'(I4)') IFIA
           end if
           if ((IFIA.lt.300..and. DBH(I).ge.9.) .or.&
                &(IFIA.ge.300..and. DBH(I).ge.11.)) then
              SUMAGE = SUMAGE+ABIRTH(I)*PROB(I)
              AGEKNT = AGEKNT+PROB(I)
              !*            WRITE(JOSTND,*)' I,IFIA,D,AGE,PROB,SUMAGE,AGEKNT= ',
              !*     &      I,IFIA,DBH(I),ABIRTH(I),PROB(I),SUMAGE,AGEKNT
           end if
        end do
     end if
     if (AGEKNT.gt.0.) then
        !*        WRITE(JOSTND,*)' AGEKNT= ',AGEKNT
        ICAGE = IFIX(SUMAGE/AGEKNT)
     else
        ICAGE = 0
     end if
     !*        WRITE(JOSTND,*)' SUMAGE,AGEKNT,ICAGE= ',SUMAGE,AGEKNT,ICAGE

     !  REMOVE THE COMMENT CHARACTERS (C**) IN THE FOLLOWING STATEMENT
     !  TO ENABLE REPORTING THE INITIAL STAND AGE, AGECMP, (IF 0 IN STDINFO
     !  KEYWORD) IN THE SUMMARY STATISTICS TABLE.  DO **NOT** USE FOR THE
     !  FOLLOWING VARIANTS, AK, CI, IE, KT, NI, TT, UT, WS,
     !  WHICH DO NOT CONSISTENTLY USE SITE CURVES FOR HEIGHT GROWTH.

     !**        IF(IAGE .EQ. 0)IAGE=ICAGE
  end if
  42 continue

  IOSUM(01,IKNT) = IY(IKNT)
  IOSUM(03,IKNT) = INT(ONTCUR(7)/GROSPC+0.5)
  IOSUM(04,IKNT) = INT(OCVCUR(7)/GROSPC+0.5)
  IOSUM(05,IKNT) = INT(OMCCUR(7)/GROSPC+0.5)
  IOSUM(06,IKNT) = INT(OBFCUR(7)/GROSPC+0.5)

  IOSUM(18,IKNT) = IFORTP
  IOSUM(19,IKNT) = ISZCL
  IOSUM(20,IKNT) = ISTCL

  !  IF LSTART IS TRUE,  DONT LOAD REMOVAL AND GROWTH STATISTICS.

  if (LSTART) goto 100

  !  ENTER HERE TO LOAD FINAL LINE OF SUMMARY OUTPUT.

  90 continue
  IKNT = ICYC
  IOSUM(02,IKNT) = IOAGE
  if (ONTREM(7).le.0.0) goto 91
  IOSUM(03,IKNT) = INT(OLDTPA/GROSPC+.5)
  IOSUM(11,IKNT) = INT(ATBA/GROSPC+0.5)
  IOSUM(12,IKNT) = INT(ATCCF/GROSPC+0.5)
  IOSUM(13,IKNT) = INT(ATAVH+0.5)
  QDBHAT(IKNT) = ATAVD
  SDIACTMP = SDIAC
  if (LZEIDE)SDIACTMP = SDIAC2
  ISDIAT(IKNT) = INT(SDIACTMP/GROSPC+0.5)
  goto 92
  91 continue
  IOSUM(11,IKNT) = INT(OLDBA/GROSPC+0.5)
  IOSUM(12,IKNT) = INT(RELDM1/GROSPC+0.5)
  IOSUM(13,IKNT) = INT(OLDAVH+0.5)
  QDBHAT(IKNT) = ORMSQD
  SDIBCTMP = SDIBC
  if (LZEIDE)SDIBCTMP = SDIBC2
  ISDIAT(IKNT) = INT(SDIBCTMP/GROSPC+0.5)
  92 continue
  SDIBCTMP = SDIBC
  if (LZEIDE)SDIBCTMP = SDIBC2
  ISDI(IKNT) = INT(SDIBCTMP/GROSPC+0.5)
  IOSUM(07,IKNT) = INT(ONTREM(7)/GROSPC+0.5)
  IOSUM(08,IKNT) = INT(OCVREM(7)/GROSPC+0.5)
  IOSUM(09,IKNT) = INT(OMCREM(7)/GROSPC+0.5)
  IOSUM(10,IKNT) = INT(OBFREM(7)/GROSPC+0.5)

  !  SUM THE REMOVALS

  if (IKNT.le.NCYC) then
     TRTPA = TRTPA   + ONTREM(7)
     TRTCUFT = TRTCUFT + OCVREM(7)
     TRMCUFT = TRMCUFT + OMCREM(7)
     TRBDFT = TRBDFT  + OBFREM(7)
  end if
  IOSUM(14,IKNT) = IFINT
  IOSUM(15,IKNT) = INT(OACC(7)/GROSPC+0.5)
  IOSUM(16,IKNT) = INT(OMORT(7)/GROSPC+0.5)
  QSDBT(IKNT) = ORMSQD
  IOLDBA(IKNT) = INT(OLDBA/GROSPC+0.5)
  IBTCCF(IKNT) = INT(RELDM1/GROSPC+0.5)
  IBTAVH(IKNT) = INT(OLDAVH+0.5)
  if (SAMWT .ge..99999) ISWT = IFIX(SAMWT+.0001)
  if (SAMWT .lt..99999) ISWT = IFIX(SAMWT*100000.+.5)
  IOSUM(17,IKNT) = ISWT
  100 continue
  if (ICL6.gt.0) go to 150

  !  PRINT THE EXAMPLE TREE RECORD AND STAND ATTRIBUTE TABLE.
  !  IF NOT DESIRED THEN SKIP.

  if (ITABLE(2) .eq.1) go to 125

  !  WRITE AN END OF FILE MARK ON SUMMARY TREE SCRATCH FILE.

  ENDFILE JOTREE
  call PRTEXM (JOTREE,JOSTND,ITITLE)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  125 continue

  !  PRINT THE SUMMARY OUTPUT

  do I = 7,10
     IOSUM(I,IKNT) = 0
     130 continue
  end do
  do I = 14,16
     IOSUM(I,IKNT) = 0
     140 continue
  end do

  !  LOAD FINAL MAI VALUE

  if (MAIFLG .eq.0) then
     if (IOSUM(2,IKNT).gt.0.) then
        if ((VARACD .eq. 'CS') .or. (VARACD .eq. 'LS') .or.&
             &(VARACD .eq. 'NE') .or. (VARACD .eq. 'SN') .or.&
             &(VARACD .eq. 'ON')) then
           BCYMAI(IKNT) = (IOSUM(4,IKNT)+TOTREM)/IOSUM(2,IKNT)
        else
           BCYMAI(IKNT) = (IOSUM(5,IKNT)+TOTREM)/IOSUM(2,IKNT)
        end if
     else
        BCYMAI(IKNT) = 0.
     end if
  else
     BCYMAI(IKNT) = 0.
  end if

  !     CALL **ECEND** TO OUTPUT LAST LINES OF ECONOMIC OUTPUT

  if (LECON) call ECEND
  I = 0
  if (LSUMRY) I = JOSUM

  !     SKIP PRINTING OF SUMMARY OUTPUT IF NOT DESIRED.

  J = JOSTND
  if (ITABLE(3).eq.1) J = 0
  call GROHED (JOSTND)
  call LBSPLW (JOSTND)
  call SUMOUT (IOSUM,20,0,JOSTND,J,I,IKNT,MGMID,NPLT,SAMWT,&
       &ITITLE,IPTINV)
  call OPLIST (.false.,NPLT,MGMID,ITITLE)

  !     CALL ECLBL TO OUTPUT ECONOMIC LABEL INFORMATION.

  if (LECON) call ECLBL
  150 continue
  if (LSTART.and.LSCRN) call SUMHED
  if ((.not.LSTART).and.LSCRN)&

       !       THESE PRINTED VALUES AREN'T QUITE RIGHT, SINCE IOSUM() ARRAY
       !       HAS ALREADY BEEN CONVERTED TO INT

       &write (JOSCRN,170) IOSUM(1,IKNT),&
       &NINT(IOSUM(3,IKNT)/ACRtoHA),&
       &NINT(IOLDBA(IKNT)*FT2pACRtoM2pHA),&
       &NINT(ISDI(IKNT)/ACRtoHA),&
       &NINT(IBTAVH(IKNT)*FTtoM),&
       &QSDBT(IKNT)*INtoCM,&
       &NINT(IOSUM(4,IKNT)*FT3pACRtoM3pHA),&
       &NINT(IOSUM(7,IKNT)/ACRtoHA),&
       &NINT(IOSUM(8,IKNT)*FT3pACRtoM3pHA ),&
       &NINT(IOSUM(9,IKNT)*FT3pACRtoM3pHA),&
       &NINT(IOSUM(11,IKNT)*FT2pACRtoM2pHA),&
       &NINT(ISDIAT(IKNT)/ACRtoHA),&
       &NINT(IOSUM(13,IKNT)*FTtoM),&
       &QDBHAT(IKNT)*INtoCM,&
       &NINT(IOSUM(15,IKNT)*FT3pACRtoM3pHA),&
       &NINT(IOSUM(16,IKNT)*FT3pACRtoM3pHA)
  170 format (I4,I6,3I4,F5.1,4I6,3I4,F5.1,I4,I4)
  if (.not.LSTART) call DBSSUMRY2
  return
end
