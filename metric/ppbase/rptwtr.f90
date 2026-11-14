!============================!
!   metric/ppbase : rptwtr   !
!============================!
! METRIC-PPBASE $Id$
subroutine RPTWTR (NCYC2,IONER)
  implicit none

  !     PRINTS THE STAND COMPOSITION, AND SAMPLE TREE-STAND ATTRIBUTE
  !     TABLES.

  !     NOTE THAT THIS IS A REWRITE OF DISPLY, AND A POOR ONE AT THAT.

  !     PART OF THE PARALLEL PROCESSING EXTENSION OF PROGNOSIS SYSTEM.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--FEB 1982.

  !     NCYC2 = CYCLE NUMBER.
  !     IONER = A LIST OF POINTERS TO THE ELEMENTS THAT MAKE UP THE
  !             OUTPUT FOR THE REPORT.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'PPCNTL.f90'
  include 'METRIC.f90'

  !     STORAGE ALLOCATION:

  ! Variables
  integer :: NCYC2,IC2,IPLACE,IOAGE,JYR,IRT,I,JSDI
  integer :: IONER(*)

  real    :: ALL(15000),
  integer :: ISNUMS(3000),ICYCS(3000),
  integer :: LOCS(3000),IELTS(3000),IPTS(3000)
  equivalence (ALL(1),BFV(1)),(ALL(1),ISNUMS(1)),&
       &(ALL(3001 ),ICYCS(1)),&
       &(ALL(6001 ),LOCS (1)),&
       &(ALL(9001 ),IELTS(1)),&
       &(ALL(12001),IPTS (1))

  !  DIMENSIONS FOR INTERNAL VARIABLES (USED AS OUTPUT LABELS):

  character(len=9) :: AT1
  character(len=9) :: AT2
  character(len=9) :: AT3
  character(len=9) :: AT4
  character(len=10) :: STD(3)

  !  DATA STATEMENTS:

  data AT1/'REMOVAL  '/,AT2/'VOLUME:  '/,AT3/&
       &'RESIDUAL '/,AT4/'ACCRETION'/

  !     IF OUTPUT IS CURRENTLY BEING PRINTED IN THE KEYWORD TABLE; THEN:
  !     CLOSE THE KEYWRD TABLE IF OTHER MAJOR TABLES WILL BE PRINTED;
  !     OTHERWISE LEAVE IT OPEN FOR MORE KEYWORDS.
  !     NOTE: LFLAG=TRUE IF A NEW KEYWORD-TABLE HEADING IS NEEDED, AND
  !     FALSE IF THE CURRENT OUTPUT IS PART OF THE KEYWORD TABLE.

  if (LFLAG) goto 20
  if (.not.(LSMPT.or.LSNDCP)) goto 20

  !     CLOSE KEYWORD TABLE AND INDECATE A NEW ONE WILL BE NEEDED.

  write (JOPPRT,10)
  10 format (1X,130('-'))
  LFLAG = .true.
  20 continue

  !     CREATE THE REQUESTED REPORTS...LOOP THRU ALL CYCLES, INCLUDING
  !     THE 'BEGINNING' AND 'ENDING' OUTPUTS.

  do IC2 = 1,NCYC2
     IPLACE = IONER(IC2)
     IPTELS = IELTS(IPLACE)
     IPLACE = LOCS(IPLACE)

     if (PDEBUG) write (JOPPRT,25) IPLACE,IPTELS
     25 format (/' RPTWTR CALLING GETPRT, IPLACE=',I5,'; IPTELS=',I5)
     call GETPRT(IPLACE,JYR,IOAGE)

     !     GETPRT SETS THE NECESSARY VARIABLES WHICH CONTROL WHICH WRITE
     !     STMTS WILL BE USED BY THIS ROUTINE.  THE USER CONTROLS THE
     !     VALUES OF LSNDCP, LSMPT, AND LCHEAPO, USING KEYWORDS.

     if (PDEBUG) write (JOPPRT,26) LFLAG,LSTART,LSNDCP,LSMPT,ICL6
     26 format (/' IN RPTWTR: LFLAG=',L2,'; LSTART=',L2,'; LSNDCP=',L2,&
          &'; LSMPT=',L2,'; ICL6=',I5)

     !     IF WE ARE AT THE 'BEGINNING' OUTPUT AND THE STAND COMPOSTION
     !     TABLE HAS BEEN REQUESTED, THEN WRITE A HEADING.

     if (LSTART.and.LSNDCP)&
          &call GHEADS (NPLT,MGMID,JOPPRT,0,ITITLE)

     !     IF EXAMPLE TREE/STAND ATTRIBUTE TABLE IS REQUESTED; THEN: WRITE
     !     HEADER RECORD TO THE SCRATCH FILE.

     if (.not.LSTART .or..not.LSMPT) goto 30
     IRT = 0
     write (JOTREE) IRT,NPLT,MGMID
     30 continue

     !   IF USER HAS SPECIFIED KEYWORDS THAT ALTER VOLUME EQUATION PARAMETERS
     !   (IE VOLUMME BFVOLUME,ETC) THEN PRINT LABEL THAT INDICATES USER
     !   SPECIFIED STANDARDS.

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

     if (ICL6.lt.0) goto 70

     !     WRITE REMOVAL STATISTICS.  BYPASS IF LSTART IS TRUE OR NUMBER
     !     OF TREES REMOVED WAS LESS THAN OR EQUAL TO ZERO.

     if (.not.LSNDCP) goto 60
     if (LSTART) goto 50
     if (ONTREM(7).le.0.0) goto 40
     write (JOPPRT,9003)
     9003 format(/)
     write(JOPPRT,9004) AT1,(ONTREM(I)*INtoCM,I = 1,6),&
          &ONTREM(7)/ACRtoHA,(OSPTT(I),IOSPTT(I),I = 1,4)
     9004 format(7X,A9,3X,5F7.1,F8.1,F9.0,' TREES   ',&
          &3(F5.0,'% ',A3,','),F5.0,'% ',A3)
     write(JOSTND,9005) AT2,STD(1),(OCVREM(I)*INtoCM,I = 1,6),&
          &OCVREM(7)*FT3pACRtoM3pHA,(OSPTV(I),IOSPTV(I),I = 1,4),&
          &STD(2),(OMCREM(I)*INtoCM,I = 1,6),OMCREM(7)*FT3pACRtoM3pHA

     9005 format(7X,A9/9X,A10,5F7.1,F8.1,F9.0,' CU M    ',&
          &3(F5.0,'% ',A3,','),F5.0,'% ',A3/&
          &9X,A10,5F7.1,F8.1,F9.0,' CU M    ',&
          &3(F5.0,'% ',A3,','),F5.0,'% ',A3)
     write(JOSTND,9003)
     write(JOSTND,9004) AT3,(ONTRES(I)*INtoCM,I = 1,6),&
          &ONTRES(7)/ACRtoHA,(OSPRT(I),IOSPRT(I),I = 1,4)

     !     WRITE ACCRETION AND MORTALITY STATISTICS.  BYPASSED IF LSTART
     !     IS TRUE.

     40 continue
     write (JOPPRT,9003)
     write (JOPPRT,9006) AT4,(OACC(I)*INtoCM,I = 1,6),&
          &OACC(7)*FT3pACRtoM3pHA,(OSPAC(I),IOSPAC(I),I = 1,4)
     if (LMORT) then
        write(JOSTND,9006) 'USER MORT',(OMORT(I)*INtoCM,I = 1,6),&
             &OMORT(7)*FT3pACRtoM3pHA,(OSPMO(I),IOSPMO(I),I = 1,4)
     else
        write(JOSTND,9006) 'MORTALITY',(OMORT(I)*INtoCM,I = 1,6),&
             &OMORT(7)*FT3pACRtoM3pHA,(OSPMO(I),IOSPMO(I),I = 1,4)
     end if
     9006 format(7X,A9,3X,5F7.1,F8.1,F9.0,' CU M/YR ',&
          &3(F5.0,'% ',A3,','),F5.0,'% ',A3)

     !     WRITE CURRENT STAND STATISTICS.

     50 continue
     write (JOPPRT,9003)
     write (JOPPRT,9007) JYR,(ONTCUR(I)*INtoCM,I = 1,6),&
          &ONTCUR(7)/ACRtoHA,(OSPCT(I),IOSPCT(I),I = 1,4)
     9007 format(/,1X,I4,'  TREES',T20,5F7.1,F8.1,F9.0,' TREES   ',&
          &3(F5.0,'% ',A3,','),F5.0,'% ',A3)
     write(JOSTND,9005) AT2,STD(1),(OCVCUR(I),I = 1,6),&
          &OCVCUR(7)*FT3pACRtoM3pHA,(OSPCV(I),IOSPCV(I),I = 1,4),&
          &STD(2),(OMCCUR(I),I = 1,6),OMCCUR(7)*FT3pACRtoM3pHA,&
          &(OSPMC(I),IOSPMC(I),I = 1,4)
     60 continue

     !     WRITE OUTPUT FOR SAMPLE TREES.  FIRST, OUTPUT STAND CONDITIONS
     !     FOR PREVIOUS CYCLE.  BYPASS IF LSTART IS TRUE.

     if (LSTART) goto 100
     70 continue

     !     BYPASS WRITTING OF SAMPLE TREE DATA IF THE TABLE HAS BEEN
     !     SURPRESSED BY THE USER.

     if (.not.LSMPT) goto 90

     !     GOTO STATEMENT 80 AND WRITE BOTH PRE AND POST THINNING STATISTICS
     !     IF THINNING OCCURRED IN PREVIOUS CYCLE.

     JSDI = SDIBC + 0.5
     if (ONTREM(7).gt.0.0) goto 80
     IRT = 1
     write(JOTREE) IRT,IOAGE,ORMSQD*INtoCM,OLDTPA/ACRtoHA,&
          &OLDBA*FT2pACRtoM2pHA,OLDAVH*FTtoM,RELDM1,JSDI
     goto 90
     80 continue
     IRT = 2
     write(JOTREE) IRT,IOAGE,ORMSQD*INtoCM,OLDTPA/ACRtoHA,&
          &OLDBA*FT2pACRtoM2pHA,OLDAVH*FTtoM,RELDM1,JSDI
     IRT = 3
     JSDI = SDIAC + 0.5
     write(JOTREE) IRT,ATAVD*INtoCM,ATTPA/ACRtoHA,&
          &ATBA*FT2pACRtoM2pHA,ATAVH*FTtoM,ATCCF,JSDI
     90 continue

     !     IF THIS IS THE LAST OF THE OUTPUT, SKIP TO ECON DATA.

     if (ICL6.lt.0) goto 120

     !     WRITE YEAR AND PERIOD LENGTH.

     100 continue
     if (.not.LSMPT) goto 120

     I = JYR
     if (IFST.ne.0) I = -I
     IRT = 4
     write(JOTREE) IRT,I,IFINT

     !     WRITE EXAMPLE TREE DATA.

     IRT = 5
     write(JOTREE) IRT,IONSP,DBHIO*INtoCM,HTIO*FTtoM,IOICR,&
          &DGIO*INtoCM,PCTIO,PRBIO
     120 continue
     130 continue
     200 continue
  end do
  if (ICL6.gt.0) go to 150

  !     PRINT THE EXAMPLE TREE RECORD AND STAND ATTRIBUTE TABLE.

  if (LSMPT) call PRTEXM (JOTREE,JOSTND,ITITLE)
  150 continue
  return
end
