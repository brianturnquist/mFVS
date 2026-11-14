!=================!
!   wpbr : brin   !
!=================!
! WPBR $Id$
subroutine BRIN(PASKEY,ARRAY,LNOTBK,LKECHO)
  implicit none
  ! THIS IS JUST A TEST FILE UPDATE FOR GIT REPO PULL REQUEST.
  ! GO AHEAD AND REJECT TO THE ETHER.

  !  Purpose:
  !     This subroutine reads the White Pine Blister Rust keywords.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  23-APR-1999 Lance David (FHTET)
  !     Changed code to eliminate floating-point comparison warnings from
  !     FORTRAN 90 compiler (Lahey LF90).
  !     Added automatic scheduling of pruning when excising is scheduled.
  !  18-MAY-1999 Lance David (FHTET)
  !     Added debug code. Corrected IF construct in keyword INACT
  !     processing because it only allowed one canker inactivation rate
  !     to be changed at a time.
  !  06-SEP-00 Lance R. David (FHTET)
  !     Added use of FVS subroutine UPCASE to eliminate case sensitivity in
  !     COMMENT's "END" keyword processing.
  !  14-DEC-00 Lance R. David (FHTET)
  !     Date (field 1) on prune keyword set to cycle 1 if blank or zero.
  !  09-JAN-01 Lance R. David (FHTET)
  !     RUSTINDX field 2 - Stand Deviation Factor will be calculated if
  !     blank or 0.0, values > 0.0 will set static deviation factor.
  !  28-FEB-01 Lance R. David (FHTET)
  !     Deactivated automatic scheduling of prune activity with EXCISE
  !     keyword processing.
  !     Due to problems experienced with other models, literal activity
  !     codes have been changed to variable IACT and number of parameters
  !     to variable NP in calls to option processor routines.
  !  06-MAR-01 Lance R. David (FHTET)
  !     Added pathological pruning option (field 5)to prune keyword.
  !     Added GROWRATE keyword for branch and bole canker growth rates.
  !  14-MAR-01 Lance R. David (FHTET)
  !     Added DEVFACT keyword (activity code 1010) processing.
  !     Field 2 of the RUSTINDX keyword the original single deviation
  !     factor value and was deactivated.
  !  27-MAR-01 Lance R. David (FHTET)
  !     Added entry validation for GROWRATE and DEVFACT keywords.
  !  24-APR-01 Lance R. David (FHTET)
  !     Added species dimension for PRPSTK and RESIST arrays in STOCK
  !     keyword processing.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM. Instead of just being and indicator of a
  !     species being a host, BRSPM holds the array index value for that
  !     species and is used to access all species-specific BR arrays.
  !  16-MAY-2001 Lance R. David (FHTET)
  !     Expanded canker growth rate variables by species and stock type
  !     in keyword GROWRATE.
  !  23-MAY-2001 Lance R. David (FHTET)
  !     File names for canker data, BR canker list, and BR tree list
  !     default to same name as FVS main keyword file with extensions
  !     of .can, .brc and .brt respectively.
  !  06-NOV-2002 Lance R. David (FHTET)
  !     Added options to RUSTINDX keyword for use of new suboutine (BRICAL)
  !     and rust index equations. Added Rust index assignment methods 3&4
  !     supplemental record for the parameters of the equations.
  !  10-NOV-2003 - Lance R. David (FHTET)
  !     Added LFLAG to KEYRDR call statement argument list.
  !  15-MAY-2006 - Lance R. David (FHTET)
  !     Added BROUT keyword to control output written to standard FVS
  !     output file.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: DEBUG,LOPEN,LREDF,LNOTBK(12),LALLSP,LALLST,LKECHO
  integer :: I,I3,I4,I5,IACT,ICLS,IDT,IKEY,ISIZE,ITSP,J,KSP,KODE,LENTH,NP,NMBER
  real    :: ARRAY(12),PRMS(10),EXCESS,BOTMP,BRTMP,REDFAC
  character(len=4) :: C4TMP
  character(len=8) :: TABLE(20)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=10) :: KARD(12)
  character(len=80) :: RECORD
  character(len=80) :: DNAME
  character(len=80) :: TNAME
  character(len=80) :: CNAME

  !.... Data statements.

  data ISIZE/20/

  data TABLE/'PRUNE','PRNSPECS','EXCISE','EXSPECS','RIBES','INACT',&
       &'END','COMMENT','GROWRATE','BRTLST','BRCLST','CANFMT',&
       &'DEVFACT','RUSTINDX','BRSEED','STOCK','CANKDATA','BROUT',&
       &' ',' '/

  !.... Check for debug.

  call DBCHK(DEBUG,'BRIN',4,ICYC)

  if (DEBUG) write(JOSTND,10)ICYC,PASKEY,ARRAY,TABLE
  10 format('Begin BRIN: Cycle= ',I5,/,' PASKEY=',A8,/,&
       &' ARRAY=',7F7.3,/,' TABLE=',20A8)

  !.... Initializations.

  BRYES = .true.
  TNAME = ' '
  CNAME = ' '
  DNAME = ' '

  !.... Load the passed keyword into KEYWRD and branch to the FNDKEY
  !.... rountine.

  KEYWRD = PASKEY

  !.... Top of the loop to read keywords.

  90 continue

  !.... Read keyword from input keyword file.

  call KEYRDR(IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,ARRAY,IRECNT,&
       &KODE,KARD,LFLAG,LKECHO)

  !.... Process keyword errors.
  !.... Return codes 0 = no error, 1 = column 1 blank, 2 = EOF

  if (KODE.ne.0) then
     if (KODE.eq.2) call ERRGRO(.false.,2)
     call ERRGRO(.true.,6)
     go to 90
  end if

  !.... Find the keyword.

  call FNDKEY(NMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

  !.... Return codes 0 = no error, 1 = keyword not found.

  if (KODE.ne.0) then
     call ERRGRO(.true.,1)
     go to 90
  end if

  !.... Process the keyword.

  go to (100,200,300,400,500,600,700,800,900,1000,&
       &1100,1200,1300,1400,1500,1600,1700,1800),NMBER

  100 continue

  !.... ==========  Option Number 1: PRUNE ===========================

  !.... This keyword triggers a pruning management activity in the
  !.... year specified.
  !.... Field 1 - year/cycle
  !.... Field 2 - success rate of pruning
  !.... Field 3 - prune infected prunable trees
  !.... Field 4 - prune uninfected/clean trees
  !.... Field 5 - pathological pruning flag,
  !....           0 = (default) change crown base height when pruned
  !....           1 = do not change crown base height when pruned

  IDT = 1
  if (LNOTBK(1) .and. ARRAY(1) .gt.0.0) IDT = INT(ARRAY(1))
  PRMS(1) = SRATE(1)
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  PRMS(2) = ARRAY(3)
  PRMS(3) = ARRAY(4)
  if (LNOTBK(5) .and. ARRAY(5) .ne.0) then
     !....    A non-zero entry in field 5 indicates pathological pruning.
     PRMS(4) = 1
  else
     PRMS(4) = 0
  end if

  !.... If field 3 is 1 then pruning will be done on prunable trees.
  !.... If field 4 is 1 then pruning will be done on clean trees.
  !.... If neither field is 1 then an error message is printed and
  !.... pruning will not be done.

  if (INT(ARRAY(3)).eq.0.and. INT(ARRAY(4)).eq.0) then
     write(JOSTND,101)
     101 format(/,'**** WARNING **** PRUNE ACTIVITY HAS BEEN ',&
          &'SPECIFIED BUT NEITHER PRUNABLE TREES NOR CLEAN TREES '/,&
          &'**** WARNING **** HAVE BEEN TARGETED FOR TREATMENT. ',&
          &'PRUNING WILL NOT OCCUR.')
     go to 90
  end if

  IACT = 1001
  NP = 4
  call OPNEW(KODE,IDT,IACT,NP,PRMS)
  if (KODE.gt.0) go to 90

  !.... Output for PRUNE keyword.

  if (LKECHO)write(JOSTND,110) KEYWRD,IDT,PRMS(1)
  110 format(/,A8,'   PRUNING SCHEDULED IN CYCLE/YEAR ',I4,&
       &';  SUCCESS RATE = ',F6.2,';')
  if (INT(ARRAY(3)).eq.1.and. INT(ARRAY(4)).eq.1) then
     write (JOSTND,111)
     111 format(T12,'BOTH PRUNABLE INFECTED AND CLEAN TREES HAVE',&
          &' BEEN TARGETED FOR PRUNING')
  else if (INT(ARRAY(3)).eq.1) then
     if (LKECHO)write(JOSTND,112)
     112 format(T12,'PRUNABLE INFECTED TREES HAVE BEEN TARGETED',&
          &' FOR PRUNING')
  else if (INT(ARRAY(4)).eq.1) then
     if (LKECHO)write(JOSTND,113)
     113 format(T12,'CLEAN TREES HAVE BEEN TARGETED FOR PRUNING')
  end if

  if (INT(PRMS(4)) .eq.1) then
     if (LKECHO)write(JOSTND,114)
     114 format(T12,'PATHOLOGICAL PRUNING IS SPECIFIED; THEREFOR,',/,&
          &T12,'CROWN BASE HEIGHTS WILL NOT BE CHANGED WHEN PRUNED.')
  else
     if (LKECHO)write(JOSTND,115)
     115 format(T12,&
          &'CROWN BASE HEIGHTS WILL BE INCREASED ON PRUNED TREES.')
  end if
  go to 90

  200 continue

  !.... ==========  Option Number 2: PRNSPECS  ========================

  !.... This keyword is used to change the default values for defining
  !.... prunable cankers that would qualify for removal when the prune
  !.... keyword is specified.
  !.... Date/Cycle 0 changes values, but does not schedule an activity
  !.... with the option processor.
  !.... Field 3 is provided in feet and converted to centimeters for
  !.... processing.
  !.... Fields 4 and 5 are provided in inches and converted to
  !.... centimeters for processing.
  !.... The parameters are stored in the event monitor in feet and inches
  !.... and will be converted to centimeters when the activity occurs.

  IDT = 0
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  PRMS(1) = HTPRPR
  PRMS(2) = HTMAX(1)/30.48
  PRMS(3) = OUTDST/2.54
  PRMS(4) = OUTNLD/2.54

  if (IDT.eq.0) then
     if (LNOTBK(2)) then
        HTPRPR = ARRAY(2)
        PRMS(1) = ARRAY(2)
     end if
     if (LNOTBK(3)) then
        HTMAX(1) = ARRAY(3)*30.48
        PRMS(2) = ARRAY(3)
     end if
     if (LNOTBK(4)) then
        OUTDST = ARRAY(4)*2.54
        PRMS(3) = ARRAY(4)
     end if
     if (LNOTBK(5)) then
        OUTNLD = ARRAY(5)*2.54
        PRMS(4) = ARRAY(5)
     end if
  else
     if (LNOTBK(2)) PRMS(1) = ARRAY(2)
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
     if (LNOTBK(5)) PRMS(4) = ARRAY(5)
     IACT = 1002
     NP = 4
     call OPNEW(KODE,IDT,IACT,NP,PRMS)
     if (KODE.gt.0) go to 90
  end if

  !.... Output for PRNSPECS keyword.

  if (LKECHO)write(JOSTND,210) KEYWRD,IDT,(PRMS(I),I = 1,4)
  210 format(/,A8,'   PRUNE PARAMETERS CHANGED IN DATE/CYCLE ',I4,&
       &';  MAXIMUM PRUNABLE HEIGHT AS A PROPORTION OF TOTAL',&
       &' TREE HEIGHT = ',F4.2,';',/T12,'MAXIMUM ABSOLUTE PRUNE',&
       &' HEIGHT = ',F5.1,' FEET;  MINIMUM DISTANCE OUT FOR PRUNABLE',&
       &' CANKER = ',F4.1,' INCHES;',/T12,'MINIMUM DISTANCE OUT FOR',&
       &' NON-LETHAL CANKER = ',F4.1,' INCHES')
  go to 90

  300 continue

  !.... ==========  Option Number 3: EXCISE  ===========================

  !.... This keyword triggers an excise management activity in the year
  !.... specified.

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  PRMS(1) = SRATE(2)
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)

  IACT = 1003
  NP = 1
  call OPNEW(KODE,IDT,IACT,NP,PRMS)
  if (KODE.gt.0) go to 90

  !.... Output for EXCISE keyword.

  if (LKECHO)write(JOSTND,310) KEYWRD,IDT,PRMS(1)
  310 format(/,A8,'   EXCISING SCHEDULED IN DATE/CYCLE ',I4,&
       &';  SUCCESS RATE = ',F4.2)

  !.... Automatic PRUNE activity scheduling.
  !.... When the EXCISE activity is scheduled, PRUNE on prunable trees
  !.... only will also be scheduled for the same date.
  !.... IDT     date remains same as EXCISE date.
  !.... PRMS(1) is success rate of pruning.
  !.... PRMS(2) set to 1 to target prunable trees.
  !.... PRMS(3) set to 0 to not prune clean (uninfected) trees.

  !     The automatic scheduling has been deactivated per work session
  !     with Geral McDonald and John Schwandt 12-DEC-00.
  !     Lance David 28-FEB-01

  !     KEYWRD=TABLE(1)
  !     PRMS(1)=SRATE(1)
  !     PRMS(2)=1
  !     PRMS(3)=0

  !     IACT = 1001
  !     NP = 3
  !     CALL OPNEW(KODE,IDT,IACT,NP,PRMS)
  !     IF(KODE.GT.0) GO TO 90

  !.... Output for EXCISE initiated PRUNE keyword.

  !     WRITE(JOSTND,320) KEYWRD,IDT,PRMS(1)
  ! 320 FORMAT(/,A8,'   EXCISE INITIATED PRUNING SCHEDULED IN CYCLE',
  !    &       '/YEAR ',I4,';  SUCCESS RATE = ',F6.2,';',/,
  !    &       T12,'ONLY PRUNABLE TREES HAVE BEEN TARGETED FOR PRUNING')

  go to 90

  400 continue

  !.... ==========  Option Number 4: EXSPECS  ==========================

  !.... This keyword changes the default values which define how status
  !.... code 3 (excisable) is assigned to a canker, which causes the
  !.... canker to be removed when a excise activity is performed.
  !.... Date/Cycle 0 changes values, but does not schedule an activity
  !.... with the option processor.
  !.... Field 3 is provided in feet and fields 2 and 6 are in inches; all
  !.... are converted to centimeters for processing.
  !.... The parameters are stored in the event monitor in feet and inches
  !.... and will be converted to centimeters when the activity occurs.

  IDT = 0
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  PRMS(1) = EXDMIN/2.54
  PRMS(2) = HTMAX(2)/30.48
  PRMS(3) = GIRMAX
  PRMS(4) = GIRMRT
  PRMS(5) = HTMIN/2.54

  if (IDT.eq.0) then
     if (LNOTBK(2)) then
        EXDMIN = ARRAY(2)*2.54
        PRMS(1) = ARRAY(2)
     end if
     if (LNOTBK(3)) then
        HTMAX(2) = ARRAY(3)*30.48
        PRMS(2) = ARRAY(3)
     end if
     if (LNOTBK(4)) then
        GIRMAX = ARRAY(4)
        PRMS(3) = ARRAY(4)
     end if
     if (LNOTBK(5)) then
        GIRMRT = ARRAY(5)
        PRMS(4) = ARRAY(5)
     end if
     if (LNOTBK(6)) then
        HTMIN = ARRAY(6)*2.54
        PRMS(5) = ARRAY(6)
     end if
  else
     if (LNOTBK(2)) PRMS(1) = ARRAY(2)
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
     if (LNOTBK(5)) PRMS(4) = ARRAY(5)
     if (LNOTBK(6)) PRMS(5) = ARRAY(6)
     IACT = 1004
     NP = 5
     call OPNEW(KODE,IDT,IACT,NP,PRMS)
     if (KODE.gt.0) go to 90
  end if

  !.... Output for EXSPECS keyword.

  if (LKECHO)write(JOSTND,410) KEYWRD,IDT,(PRMS(I),I = 1,5)
  410 format(/,A8,'   EXCISE PARAMETERS CHANGED IN DATE/CYCLE ',I4,&
       &';  MINIMUM TREE DBH NECESSARY FOR EXCISING = ',&
       &F5.1,' INCHES;',/T12,'MAXIMUM ABSOLUTE EXCISE HEIGHT = ',&
       &F5.1,' FEET;  MAXIMUM PERCENT GIRDLE = ',F5.1,'%;',/T12,&
       &'PERCENT GIRDLING WHICH CAUSES MORTALITY OR TOP KILL = ',F5.1,&
       &'%;'/T12,'MINIMUM BOLE CANKER HEIGHT NECESSARY FOR EXCISING = '&
       &,F5.1,' INCHES')
  go to 90

  500 continue

  !.... ==========  Option Number 5: RIBES =============================

  !.... This keyword keyword changes the population values for
  !.... Ribes H, L and V modifies the stand rust index.  The values
  !.... provided for each ribes species is old number of bushes per acre,
  !.... and new number of bushes per acre.
  !.... This keyword functions in two ways:
  !.... 1- as an activity when date (field 1) is > 0;
  !....    Fields 1-6 values are used to calculate rust indexes based on
  !....    old and new ribes populations. If both old and new population
  !....    values are present, the reduction factor is calculated.
  !....    If only new population values are present, the rust index is
  !....    calculated.
  !.... 2- calculates rust index from ribes populations when date = 0;
  !....    Fields 3, 5 and 7 values (new bushes/acre) will be used to
  !....    to calculate rust index directly.  No activity is scheduled.

  IDT = 1
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  if (IDT.eq.0) then

     !....    Calculate stand rust index.

     if (LNOTBK(2)) RIBUS(1,1) = ARRAY(2)
     if (LNOTBK(3)) RIBUS(2,1) = ARRAY(3)
     if (LNOTBK(4)) RIBUS(1,2) = ARRAY(4)
     if (LNOTBK(5)) RIBUS(2,2) = ARRAY(5)
     if (LNOTBK(6)) RIBUS(1,3) = ARRAY(6)
     if (LNOTBK(7)) RIBUS(2,3) = ARRAY(7)

     call BRIBES(REDFAC,LREDF)

     !....    Output for RIBES keyword when date/cycle = 0.

     if (LKECHO)&
          &write(JOSTND,510) KEYWRD,RIDEF,((RIBUS(I,J),I = 2,2),J = 1,3)
     510 format(/,A8,'   STAND RUST INDEX INITIALLY SET TO ',F7.5,';',&
          &/T12,'CALCULATED FROM NEW BUSHES PER ACRE VALUES SHOWN '&
          &'BELOW:',/T12,'RIBES H.   NEW BUSHES/ACRE = ',F6.1,';',&
          &/T12,'RIBES L.   NEW BUSHES/ACRE = ',F6.1,';',&
          &/T12,'RIBES V.   NEW BUSHES/ACRE = ',F6.1)
  else

     !....    Schedule an activity.

     PRMS(1) = RIBUS(1,1)
     PRMS(2) = RIBUS(2,1)
     PRMS(3) = RIBUS(1,2)
     PRMS(4) = RIBUS(2,2)
     PRMS(5) = RIBUS(1,3)
     PRMS(6) = RIBUS(2,3)

     if (LNOTBK(2)) PRMS(1) = ARRAY(2)
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
     if (LNOTBK(5)) PRMS(4) = ARRAY(5)
     if (LNOTBK(6)) PRMS(5) = ARRAY(6)
     if (LNOTBK(7)) PRMS(6) = ARRAY(7)
     IACT = 1005
     NP = 6
     call OPNEW(KODE,IDT,IACT,NP,PRMS)
     if (KODE.gt.0) go to 90

     !....    Output for RIBES keyword when date/cycle not 0.

     if (LKECHO)write(JOSTND,530) KEYWRD,IDT,(PRMS(I),I = 1,6)
     530 format(/,A8,'   RUST INDEX TO BE MODIFIED IN DATE/CYCLE ',&
          &I4,' BASED ON CHANGE IN NUMBER OF RIBES BUSHES;',&
          &/T12,'RIBES H.   OLD BUSHES/ACRE = ',F6.1,&
          &';  NEW BUSHES/ACRE = ',F6.1,';',&
          &/T12,'RIBES L.   OLD BUSHES/ACRE = ',F6.1,&
          &';  NEW BUSHES/ACRE = ',F6.1,';',&
          &/T12,'RIBES V.   OLD BUSHES/ACRE = ',F6.1,&
          &';  NEW BUSHES/ACRE = ',F6.1)
  end if
  go to 90

  600 continue

  !.... ==========  Option Number 6: INACT ===========================

  !.... Changes the default value for yearly canker inactivation rate.
  !.... for branch and bole cankers. This is an activity.
  !.... Date/Cycle 0 changes values, but does not schedule an activity
  !.... with the option processor.

  if (DEBUG) then
     write(JOSTND,*) ' at INACT: ARRAY=',ARRAY
     write(JOSTND,*) '          LNOTBK=',LNOTBK
     write(JOSTND,*) '       RATINV(1)=',RATINV(1)
     write(JOSTND,*) '       RATINV(2)=',RATINV(2)
  end if

  IDT = 0
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  PRMS(1) = RATINV(1)
  PRMS(2) = RATINV(2)

  if (IDT.eq.0) then
     if (LNOTBK(2)) then
        RATINV(1) = ARRAY(2)
        PRMS(1) = ARRAY(2)
     end if
     if (LNOTBK(3)) then
        RATINV(2) = ARRAY(3)
        PRMS(2) = ARRAY(3)
     end if
  else
     if (LNOTBK(2)) PRMS(1) = ARRAY(2)
     if (LNOTBK(3)) PRMS(2) = ARRAY(3)
     IACT = 1006
     NP = 2
     call OPNEW(KODE,IDT,IACT,NP,PRMS)
     if (KODE.gt.0) go to 90
  end if

  !.... Output for INACT keyword.

  if (LKECHO)write(JOSTND, 610) KEYWRD,IDT,PRMS(1),PRMS(2)
  610 format(/,A8,'   CANKER INACTIVATION RATES TO BE CHANGED IN',&
       &' DATE/CYCLE ',I4,';',/T12,'NEW VALUE FOR BRANCH CANKERS = ',&
       &F5.3,';  NEW VALUE FOR BOLE CANKERS = ',F5.3)
  go to 90

  700 continue

  !.... ==========  Option Number 7: END =============================

  !.... End of Blister Rust keywords.

  if (LKECHO)write(JOSTND,710) KEYWRD
  710 format(/,A8,'   END OF BLISTER RUST KEYWORDS')

  !.... If Blister Rust canker and/or tree list requested, open files.

  if (BRCL) then
     inquire(UNIT = IDCOUT,OPENED = LOPEN)
     if (LOPEN) close(UNIT = IDCOUT)
     call MYOPEN(IDCOUT,CNAME,1,132,0,1,1,0,KODE)
     if (KODE.ne.0) then
        print*,'*BRIN* Error opening filename: ',CNAME
        go to 9000
     end if
  end if

  if (BRTL) then
     inquire(UNIT = IDTOUT,OPENED = LOPEN)
     if (LOPEN) close(UNIT = IDTOUT)
     call MYOPEN(IDTOUT,TNAME,1,132,0,1,1,0,KODE)
     if (KODE.ne.0) then
        print*,'*BRIN* Error opening filename: ',TNAME
        go to 9000
     end if
  end if

  !.... Return.

  go to 9000

  800 continue

  !.... ==========  Option Number 8: COMMENT =========================

  !.... This keyword enables the user to include comment lines within the
  !.... keyword list.  These comments are written with the standard
  !.... keyword output at the beginning of the blister rust information.

  if (LKECHO)write(JOSTND,810) KEYWRD
  810 format(/,A8)

  !.... Read comment statments until 'END' is encountered.

  802 continue
  read(IREAD,820,end = 8900) RECORD
  820 format(A80)
  IRECNT = IRECNT+1
  C4TMP = RECORD(1:4)
  call UPCASE (C4TMP(1:1))
  call UPCASE (C4TMP(2:2))
  call UPCASE (C4TMP(3:3))
  if (C4TMP .eq. 'END ') then
     if (LKECHO)write(JOSTND,815) RECORD(1:4)
     815 format(/,A4)
     go to 90
  else
     if (LKECHO)write(JOSTND,830) RECORD
     830 format(T12,A80)
  end if
  go to 802

  900 continue

  !.... ==========  Option Number 9: GROWRATE =========================
  !....
  !.... Annual growth rates (cm) for branch and bole cankers.
  !.... field 1 - FVS Tree species code
  !.... field 2 - Stock type code
  !.... field 3 - Bole canker growth rate
  !.... field 4 - Branch canker growth rate
  !....
  !.... I3 is the FVS tree species code
  !.... I4 is the BR host tree species code
  !.... I5 is the BR host stock type

  if (LKECHO)write(JOSTND,910) KEYWRD
  910 format(/,A8,'   CHANGE ANNUAL GROWTH RATES FOR CANKERS.')
  !    &  ,/,
  !    &  T12,'FIELDS 1 AND 2 FOR SPECIES AND STOCK TYPE NOT YET ACTIVE,')

  I3 = 0
  I4 = 0
  BOTMP = 0.0
  BRTMP = 0.0
  LALLSP = .false.
  LALLST = .false.

  if (LNOTBK(1) .and. ARRAY(1) .ne.0.0) then
     I3 = INT(ARRAY(1))
     I4 = BRSPM(I3)
  end if

  !.... Vaildate species code
  if (I3.lt.0.or. I3.gt. MAXSP) then
     write(JOSTND,914) KEYWRD, I3
     914 format(/,A8,'   ***** INVALID SPECIES CODE ',I3,'. *****')
     go to 90
  else if (I4.eq.0.and. I3.ne.0) then
     write(JOSTND,915) KEYWRD, I3, NSP(I3,1)(1:2)
     915 format(/,A8,'   ***** NON-HOST SPECIES CODE ',&
          &I3,' - ',A,'. *****')
     go to 90
  end if

  !.... Vaildate stock type.
  I5 = INT(ARRAY(2))
  if (I5.lt.0.or. I5.gt.4) then
     write(JOSTND,917) KEYWRD, I5
     917 format(/,A8,'   ***** INVALID STOCK TYPE ',I1,'. *****')
     go to 90
     I5 = INT(ARRAY(2))
  end if

  if (LNOTBK(3)) then
     if (ARRAY(3).ge.0.1.and. ARRAY(3).le.10.0) then
        BOTMP = ARRAY(3)
     else
        write(JOSTND,918)
        918 format(T12,'*** INVALID BOLE CANKER GROWTH RATE. VALID ',&
             &'RANGE IS 0.1 TO 10.0 -- DEFAULT VALUE(S) RETAINED. ***')
     end if
  end if

  if (LNOTBK(4)) then
     if (ARRAY(4).ge.0.1.and. ARRAY(4).le.10.0) then
        BRTMP = ARRAY(4)
     else
        write(JOSTND,919)
        919 format(T12,'*** INVALID BRANCH CANKER GROWTH RATE. VALID ',&
             &'RANGE IS 0.1 TO 10.0 -- DEFAULT VALUE(S) RETAINED. ***')
     end if
  end if

  if (I4.eq.0) then
     !....   All species will be changed
     LALLSP = .true.
     if (I5.eq.0) then
        !....     All stock types (and species) will be changed
        LALLST = .true.
        do I4 = 1, NBRSP
           do I5 = 1, 4
              if (BOTMP .ne.0.0) BOGRTH(I4,I5) = BOTMP
              if (BRTMP .ne.0.0) BRGRTH(I4,I5) = BRTMP
           end do
        end do
     else
        !....     Change 1 stock type for all species
        do I4 = 1, NBRSP
           if (BOTMP .ne.0.0) BOGRTH(I4,I5) = BOTMP
           if (BRTMP .ne.0.0) BRGRTH(I4,I5) = BRTMP
        end do
     end if
  else
     !....   Changing only 1 species
     if (I5.eq.0) then
        LALLST = .true.
        !....     Change all stock types for 1 species
        do I5 = 1, 4
           if (BOTMP .ne.0.0) BOGRTH(I4,I5) = BOTMP
           if (BRTMP .ne.0.0) BRGRTH(I4,I5) = BRTMP
        end do
     else
        !....     Changing 1 species, 1 stock type
        if (BOTMP .ne.0.0) BOGRTH(I4,I5) = BOTMP
        if (BRTMP .ne.0.0) BRGRTH(I4,I5) = BRTMP
     end if
  end if

  if (LALLSP .and. LALLST) then
     if (LKECHO)write(JOSTND,925)
  else if (LALLSP .and..not. LALLST) then
     if (LKECHO)write(JOSTND,926) I5
  else if (.not. LALLSP .and. LALLST) then
     if (LKECHO)write(JOSTND,927) BRSPC(I4)
  else
     if (LKECHO)write(JOSTND,928) BRSPC(I4), I5
  end if

  925 format(T12,'ALL BLISTER RUST HOST SPECIES AND STOCK TYPES',&
       &' CANKER GROWTH RATES SET AS FOLLOWS:')
  926 format(T12,'ALL BLISTER RUST HOST SPECIES STOCK TYPE ',I1,&
       &' CANKER GROWTH RATES SET AS FOLLOWS:')
  927 format(T12,'ALL STOCK TYPES OF BLISTER RUST HOST SPECIES ',A3,&
       &' CANKER GROWTH RATES SET AS FOLLOWS:')
  928 format(T12,'BLISTER RUST HOST SPECIES ',A3,' STOCK TYPE ',I1,&
       &' CANKER GROWTH RATES SET AS FOLLOWS:')

  !.... If all species and/or stock types were changed, indices need reset
  !.... because do loops will have their values at 1 too high.
  if (LALLSP) I4 = NBRSP
  if (LALLST) I5 = 4

  !.... Now write canker growth rate values that have been set.
  if (LKECHO)write(JOSTND,930) BOGRTH(I4,I5),BRGRTH(I4,I5)
  930 format(T12,'BOLE CANKER WIDEST HORIZONTAL WIDTH GROWTH ',&
       &'RATE (CM) = ',F6.3,/,&
       &T12,'BRANCH CANKER RADIAL GROWTH RATE (CM) = ',F6.3)
  go to 90

  1000 continue

  !.... ==========  Option Number 10:  BRTLST =========================

  !.... Keyword to write list of trees affected by the WPBR Model to
  !.... a file.  This is an activity and the default cycle is all
  !.... cycles including cycle 0.  Default unit is 26 and the default
  !.... file name is BRTOUT.
  !.... Field 1 - date/cycle (0 - all cycles, otherwise cycle 0 and
  !....                       the cycle specified will be written)
  !.... Field 2 - unit number.
  !.... Field 3 - if not blank, indicates that a file name is specified
  !....           on a supplemental record.  The file name may be up to
  !....           30 characters.
  !....           If field 3 is blank, the file name will be the same
  !....           as the main FVS keyword file with ".brt" extension.

  BRTL = .true.
  IDT = 0
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (LNOTBK(2)) IDTOUT = IFIX(ARRAY(2))

  if (LNOTBK(3)) then
     read(IREAD,1010,end = 8900) TNAME
     1010 format(A80)
  else
     !....    Load tname with the first 76 characters of the FVS keyword
     !....    file name and concatenate .brt extension.
     TNAME = KWDFIL(1:76)//'.brt'
  end if

  !.... Remove blanks from file name.
  call UNBLNK(TNAME,LENTH)

  IACT = 1007
  NP = 0
  call OPNEW(KODE,IDT,IACT,NP,PRMS)
  if (KODE.gt.0) go to 90

  !.... Output for BRTLST keyword.

  if (IDT.eq.0) then
     if (LKECHO)write(JOSTND,1020) KEYWRD,IDTOUT,TNAME
     1020 format(/,A8,'   BLISTER RUST TREE LIST WILL BE WRITTEN TO',&
          &' UNIT NUMBER ',I2,' FOR ALL CYCLES;',&
          &/T12,'TREELIST FILENAME = ',A50)
  else
     if (LKECHO)write(JOSTND,1021) KEYWRD,IDTOUT,IDT,TNAME
     1021 format(/,A8,'   BLISTER RUST TREE LIST WILL BE WRITTEN TO',&
          &' UNIT NUMBER ',I2,' FOR DATE/CYCLE ',I4,';',&
          &/T12,'TREELIST FILENAME = ',A50)
  end if
  go to 90

  1100 continue

  !.... ==========  Option Number 11: BRCLST ==========================

  !.... Keyword to write list of cankers being tracked by WPBR Model to
  !.... a file.  This is an activity and the default cycle is all
  !.... cycles including cycle 0.  Default unit is 26 and the default
  !.... file name is BRCOUT.
  !.... Field 1 - date/cycle (0 - all cycles, otherwise cycle 0 and
  !....                       the cycle specified will be written)
  !.... Field 2 - unit number.
  !.... Field 3 - if not blank, indicates that a file name is specified
  !....           on a supplemental record.  The file name may be up to
  !....           80 characters.
  !....           If field 3 is blank, the file name will be the same
  !....           as the main FVS keyword file with ".brc" extension.

  BRCL = .true.
  IDT = 0
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (LNOTBK(2)) IDCOUT = IFIX(ARRAY(2))
  if (LNOTBK(3)) then
     read(IREAD,1110,end = 8900) CNAME
     1110 format(A80)
  else
     !....    Load cname with the first 76 characters of the FVS keyword
     !....    file name and concatenate .brc extension.
     CNAME = KWDFIL(1:76)//'.brc'
  end if

  !.... Remove blanks from file name.
  call UNBLNK(CNAME,LENTH)

  IACT = 1008
  NP = 0
  call OPNEW(KODE,IDT,IACT,NP,PRMS)
  if (KODE.gt.0) go to 90

  !.... Output for BRCLST keyword.

  if (IDT.eq.0) then
     if (LKECHO)write(JOSTND,1120) KEYWRD,IDCOUT,CNAME
     1120 format(/,A8,'   BLISTER RUST CANKER LIST WILL BE WRITTEN TO',&
          &' UNIT NUMBER ',I2,' FOR ALL CYCLES;',&
          &/T12,'CANKER LIST FILENAME = ',A50)
  else
     if (LKECHO)write(JOSTND,1121) KEYWRD,IDCOUT,IDT,CNAME
     1121 format(/,A8,'   BLISTER RUST CANKER LIST WILL BE WRITTEN TO',&
          &' UNIT NUMBER ',I2,' FOR DATE/CYCLE ',I4,';',&
          &/T12,'CANKER LIST FILENAME = ',A50)
  end if
  go to 90

  1200 continue

  !.... ==========  Option Number 12: CANFMT ==========================

  !.... Keyword to change the default format for reading canker data.

  if (LKECHO)write(JOSTND,1210) KEYWRD
  1210 format(/,A8,'   THE FORMAT BELOW WILL BE USED FOR READING ',&
       &'CANKER DATA. THE ORDER OF THE VARIABLES IS:',/,T12,&
       &'TREE ID, STOCK TYPE, TREE AGE, DIST UP, DIST OUT, ',&
       &'% GIRDLE, TOTAL CANKER COUNT')
  read(IREAD,1220,end = 8900) ICFMT
  1220 format (A80)
  if (LKECHO)write(JOSTND,1230) ICFMT
  1230 format(T13,A80)
  go to 90

  1300 continue

  !.... ==========  Option Number 13: DEVFACT  ========================

  !.... Species and Stock Type Deviation Factor.
  !....
  !.... The purpose of the deviation factor is to truncate the infection
  !.... level and assure a proportion of clean (or escape) trees in the
  !.... stand.
  !....
  !.... This keyword is a activity and may be scheduled by cycle/year or
  !.... condition evaluated by the FVS Event Monitor.
  !.... Field 1 -- Cycle/year (entry of 0 will apply new dev. factors now)
  !.... Field 2 -- Tree species code
  !.... Fields 3-6 -- Deviation Factors for stock types 1 thru 4.

  IDT = 0
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (LNOTBK(2)) KSP = IFIX(ARRAY(2))

  !     Write initial output for keyword.

  if (IDT.eq.0) then
     if (LKECHO)write(JOSTND,1316) KEYWRD
  else
     if (LKECHO)write(JOSTND,1317) KEYWRD, IDT
  end if

  !     Vaildate species code
  if (KSP .lt.0.or. KSP .gt. MAXSP) then
     write(JOSTND,1305) KEYWRD, KSP
     1305 format(/,A8,'   ***** INVALID SPECIES CODE ',I3,'. *****')
     go to 90
  else if (BRSPM(KSP) .eq.0) then
     write(JOSTND,1306) KEYWRD, KSP, NSP(KSP,1)(1:2)
     1306 format(/,A8,'   ***** NON-HOST SPECIES CODE ',&
          &I3,' - ',A,'. *****')
     go to 90
  end if

  if (IDT .eq.0) then
     !        Apply new deviation factors now.

     LDFACT = .true.
     if (LNOTBK(3)) then
        if (ARRAY(3).ge.0.01.and. ARRAY(3).le.6.0) then
           DFACT(KSP,1) = ARRAY(3)
        else
           if (LKECHO)write(JOSTND,1321) ARRAY(3), DFACT(KSP,1)
        end if
     end if
     if (LNOTBK(4)) then
        if (ARRAY(4).ge.0.01.and. ARRAY(4).le.6.0) then
           DFACT(KSP,2) = ARRAY(4)
        else
           if (LKECHO)write(JOSTND,1321) ARRAY(4), DFACT(KSP,2)
        end if
     end if
     if (LNOTBK(5)) then
        if (ARRAY(5).ge.0.01.and. ARRAY(5).le.6.0) then
           DFACT(KSP,3) = ARRAY(5)
        else
           if (LKECHO)write(JOSTND,1321) ARRAY(5), DFACT(KSP,3)
        end if
     end if
     if (LNOTBK(6)) then
        if (ARRAY(6).ge.0.01.and. ARRAY(6).le.6.0) then
           DFACT(KSP,4) = ARRAY(6)
        else
           if (LKECHO)write(JOSTND,1321) ARRAY(6), DFACT(KSP,4)
        end if
     end if
  else
     !        Load default/current values into PRMS array.
     !        Update with new values provided with keyword.
     !        Schedule activity.

     PRMS(1) = KSP
     PRMS(2) = DFACT(KSP,1)
     PRMS(3) = DFACT(KSP,2)
     PRMS(4) = DFACT(KSP,3)
     PRMS(5) = DFACT(KSP,4)
     if (LNOTBK(3)) then
        if (ARRAY(3).ge.0.01.and. ARRAY(3).le.6.0) then
           PRMS(2) = ARRAY(3)
        else
           if (LKECHO)write(JOSTND,1321) ARRAY(3), PRMS(2)
        end if
     end if
     if (LNOTBK(4)) then
        if (ARRAY(4).ge.0.01.and. ARRAY(4).le.6.0) then
           PRMS(3) = ARRAY(4)
        else
           if (LKECHO)write(JOSTND,1321) ARRAY(4), PRMS(4)
        end if
     end if
     if (LNOTBK(5)) then
        if (ARRAY(5).ge.0.01.and. ARRAY(5).le.6.0) then
           PRMS(4) = ARRAY(5)
        else
           if (LKECHO)write(JOSTND,1321) ARRAY(5), PRMS(4)
        end if
     end if
     if (LNOTBK(6)) then
        if (ARRAY(6).ge.0.01.and. ARRAY(6).le.6.0) then
           PRMS(5) = ARRAY(6)
        else
           if (LKECHO)write(JOSTND,1321) ARRAY(6), PRMS(5)
        end if
     end if

     IACT = 1010
     NP = 5
     call OPNEW(KODE,IDT,IACT,NP,PRMS)
     if (KODE.gt.0) then
        write(JOSTND,1313) KEYWRD
        1313 format(/,A8,'   ***** Activity Scheduling Error *****')
        go to 90
     end if
  end if

  !     Write final output for keyword.

  if (IDT.eq.0) then
     if (LKECHO)&
          &write(JOSTND,1320) NSP(KSP,1)(1:2), (DFACT(KSP,I),I = 1,4)
  else
     if (LKECHO)write(JOSTND,1320) NSP(KSP,1)(1:2), (PRMS(I),I = 2,5)
  end if

  1316 format(/,A8,'   STAND DEVIATION FACTORS INITIALIZED FOR')
  1317 format(/,A8,'   STAND DEVIATION FACTORS WILL BE RESET AT ',&
       &'CYCLE/YEAR ',I4,' FOR')
  1320 format(T12,'TREE SPECIES = ',A,/,T12,'STOCK TYPE 1 = ',F5.3,&
       &/,T12,'STOCK TYPE 2 = ',F5.3,/,T12,'STOCK TYPE 3 = ',F5.3,&
       &/,T12,'STOCK TYPE 4 = ',F5.3)
  1321 format(T12,'*** INVALID DEVIATION FACTOR ENTERED: ',F5.3,&
       &' DEFAULT VALUE: ',F5.3,' RETAINED. ***')
  go to 90

  1400 continue

  !.... ==========  Option Number 14: RUSTINDX  =======================
  !....
  !.... Keyword to change the default value of Rust Index, Stand
  !.... Deviation Factor, Rust Index Assignment Method, and Ribes
  !.... species proportions of population for RI calculation.
  !....
  !.... Stand Deviation Factor (F2) has been relocated to new
  !.... keyword DEVFACT.
  !.... LRD 14-MAR-01

  if (LNOTBK(1)) RIDEF = ARRAY(1)
  if (LKECHO)write(JOSTND,1410) KEYWRD,RIDEF

  if (LNOTBK(3)) then
     RIMETH = ARRAY(3)
     if (RIMETH.eq.1) then
        if (LKECHO)write(JOSTND,1412)
     else if (RIMETH.eq.2) then
        if (LKECHO)write(JOSTND,1413)
     else if (RIMETH.eq.3) then
        if (LKECHO)write(JOSTND,1414)
     else if (RIMETH.eq.4) then
        if (LKECHO)write(JOSTND,1418)
     else
        RIMETH = 0
        if (LKECHO)write(JOSTND,1415)
     end if
  else
     if (LKECHO)write(JOSTND,1415)
  end if

  if (RIMETH.eq.1.or. RIMETH.eq.2) then

     !....    Process ribes proportions of population values.

     if (LNOTBK(4)) RIBPRP(1) = ARRAY(4)
     if (LNOTBK(5)) RIBPRP(2) = ARRAY(5)
     if (LNOTBK(6)) RIBPRP(3) = ARRAY(6)

     1405 continue
     if (RIBPRP(1).lt.0.0) RIBPRP(1) = 0.0
     if (RIBPRP(2).lt.0.0) RIBPRP(2) = 0.0
     if (RIBPRP(3).lt.0.0) RIBPRP(3) = 0.0

     EXCESS = 1.0-RIBPRP(1)-RIBPRP(2)-RIBPRP(3)

     if (EXCESS.gt.0.01.or.EXCESS.lt.-0.01) then

        !....       Adjust proportions if total is not within 1 percent.

        RIBPRP(1) = RIBPRP(1)+(EXCESS/3.0)
        RIBPRP(2) = RIBPRP(2)+(EXCESS/3.0)
        RIBPRP(3) = RIBPRP(3)+(EXCESS/3.0)
        go to 1405
     end if
     if (LKECHO)write(JOSTND,1416) RIBPRP(1),RIBPRP(2),RIBPRP(3)

  else if (RIMETH.eq.3) then
     !....    For Rust Index length of exposure equation, parameters
     !....    may be provided.
     !....    Read supplemental record for equation parameters. This
     !....    is a unformatted read, so four values must be present on
     !....    the record. A zero will cause the default value for the
     !....    parameter to be retained.

     !....    Default values for Gaussian equation
     MINRI = 1.238867E-3
     MAXRI = 7.222711E-3
     PKAGE = 23.34596387
     PKSHP = 4.078136888

     read (IREAD,*) ARRAY(1), ARRAY(2), ARRAY(3), ARRAY(4)
     if (ARRAY(1).gt.0.0) MINRI = ARRAY(1)
     if (ARRAY(2).gt.0.0) MAXRI = ARRAY(2)
     if (ARRAY(3).gt.0.0) PKAGE = ARRAY(3)
     if (ARRAY(4).gt.0.0) PKSHP = ARRAY(4)
     if (LKECHO)write(JOSTND,1417) MINRI,MAXRI,PKAGE,PKSHP

  else if (RIMETH.eq.4) then
     !....    For Rust Index length of exposure equation, parameters
     !....    may be provided.
     !....    Read supplemental record for equation parameters. This
     !....    is a unformatted read, so four values must be present on
     !....    the record. A zero will cause the default value for the
     !....    parameter to be retained.

     !....    default values for log function
     MINRI = 9.6E-5
     MAXRI = 4.7E-3
     PKAGE = 1.66
     PKSHP = 1.086

     read (IREAD,*) ARRAY(1), ARRAY(2), ARRAY(3), ARRAY(4)
     if (ARRAY(1).gt.0.0) MINRI = ARRAY(1)
     if (ARRAY(2).gt.0.0) MAXRI = ARRAY(2)
     if (ARRAY(3).gt.0.0) PKAGE = ARRAY(3)
     if (ARRAY(4).gt.0.0) PKSHP = ARRAY(4)
     if (LKECHO)write(JOSTND,1417) MINRI,MAXRI,PKAGE,PKSHP
  end if

  !.... Output for RUSTINDX keyword.

  1410 format(/,A8,'   STAND RUST INDEX INITIALLY SET TO ',F7.5,';')
  1412 format(T12,'RUST INDEX WILL BE CALCULATED FROM BASAL AREA AT ',&
       &'INITIALIZATION AND USED THROUGHOUT THE SIMULATION')
  1413 format(T12,'RUST INDEX WILL BE CALCULATED FROM BASAL AREA ',&
       &'EACH CYCLE AND APPLIED TO ALL TREES.')
  1414 format(T12,'RUST INDEX BASED ON STAND AGE GAUSSIAN FUNCTION ',&
       &'EACH CYCLE AND APPLIED TO ALL TREES')
  1415 format(T12,'RUST INDEX SHOWN ABOVE WILL BE USED ',&
       &'THROUGHOUT THE SIMULATION')
  1416 format(T12,'RIBES SPECIES PROPORTIONS OF POPULATION WHICH WILL ',&
       &'BE USED FOR RUST INDEX CALCULATION ARE:',/T15,&
       &'RIBES H. = ',F4.2,';  RIBES L. = ',F4.2,';  RIBES V. = ',F4.2)
  1417 format(T12,'THE PARAMETERS USED IN EQUATION OF RI ASSIGNMENT ',&
       &'METHOD SELECTED ARE AS FOLLOWS.',&
       &/,T15,'MINIMUM RUST INDEX VALUE: ',F11.9,&
       &/,T15,'MAXIMUM RUST INDEX VALUE: ',F11.9,&
       &/,T15,'STAND AGE WHERE RI PEAKS: ',F10.5,&
       &/,T15,'CURVE SHAPE DEFINED BY:   ',F10.7)
  1418 format(T12,'RUST INDEX BASED ON STAND AGE LOG FUNCTION',&
       &'EACH CYCLE AND APPLIED TO ALL TREES.')

  go to 90

  1500 continue

  !.... ==========  Option Number 15: BRSEED =========================

  !.... Changes the default random number generator seed value.

  call BRNSED(LNOTBK(1),ARRAY(1))

  !.... Output for BRSEED keyword.

  if (LKECHO)write(JOSTND,1510) KEYWRD,ARRAY(1)
  1510 format(/,A8,'   RANDOM SEED FOR BLISTER RUST MODEL = ',F10.0)
  go to 90

  1600 continue

  !.... ==========  Option Number 16: STOCK  =========================

  !.... Changes the default rust index adjustment factors for host pine
  !.... stock, wild (natural regen) and three families of planting stock
  !.... (F1, F2, and GCOP - General Combiner Open Pollinated) and
  !.... their respective proportion of the host pine population.
  !.... This is an activity.
  !.... Date/Cycle 0 changes values, but does not schedule an activity
  !.... with the option processor.
  !.... fields are: 1-date, 2-tree species, 3-stock type,
  !....             4-proportion of population, 5-RI adjustment

  IDT = 0
  if (LNOTBK(1)) IDT = INT(ARRAY(1))
  ITSP = IFIX(ARRAY(2))
  ICLS = IFIX(ARRAY(3))
  if (ICLS.ge.1.and. ICLS.le.4.and. BRSPM(ITSP).ne.0) then
     continue
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO(.true.,4)
     go to 90
  end if

  PRMS(1) = ARRAY(2)
  PRMS(2) = ARRAY(3)
  PRMS(3) = PRPSTK(ITSP,ICLS)
  PRMS(4) = RESIST(ITSP,ICLS)

  if (IDT.eq.0) then
     if (LNOTBK(4)) then
        PRPSTK(ITSP,ICLS) = ARRAY(4)
        PRMS(3) = ARRAY(4)
     end if
     if (LNOTBK(5)) then
        RESIST(ITSP,ICLS) = ARRAY(5)
        PRMS(4) = ARRAY(5)
     end if
  else
     if (LNOTBK(4)) PRMS(3) = ARRAY(4)
     if (LNOTBK(5)) PRMS(4) = ARRAY(5)
     IACT = 1009
     NP = 4
     call OPNEW(KODE,IDT,IACT,NP,PRMS)
     if (KODE.gt.0) go to 90
  end if

  !.... Output for STOCK keyword.

  if (LKECHO)write(JOSTND,1620) KEYWRD,IDT,ITSP,ICLS,PRMS(3),PRMS(4)
  1620 format(/,A8,'   MIX OF STOCK TYPES TO BE CHANGED IN ',&
       &'DATE/CYCLE ',I4,';',/T12,'TREE SPECIES=',I3,'; STOCK TYPE = ',&
       &I2,';  PROPORTION OF STOCK IN POPULATION = ',F4.2,&
       &';  RI ADJUSTMENT = ',F4.2)
  go to 90

  1700 continue

  !.... ==========  Option Number 17: CANKDATA  =======================

  !.... Read canker data.  Field 1 contains the logical unit number from
  !.... which to read the canker data.  Field 2 contains the units of
  !.... measure for the canker data where 0 = U.S. (data is in ft./in.)
  !.... and 1 = metric (data is in cm.).  Field 3 is the "initialize
  !.... canker conditions" flag; if 0 then canker data will be read in
  !.... normally and the canker count will just be added to the ITCAN
  !.... array.  If 1 then the canker count listed in the canker data
  !.... will be used to randomly generate cankers up to 10 (or the canker
  !.... count - whichever is less) minus the ones that have already been
  !.... read into the ILCAN array from the canker data.  Supplemental
  !.... record(s) contain either the name of the canker data file (up to
  !.... 80 characters including a pathname) or the canker data itself.

  if (LNOTBK(1)) ICIN = IFIX(ARRAY(1))
  if (LNOTBK(2)) then
     if (IFIX(ARRAY(2)).eq.1) then
        LMETRIC = .true.
     end if
  end if
  if (LNOTBK(3)) then
     if (IFIX(ARRAY(3)).eq.1) then
        CKINIT = .true.
     end if
  end if

  !.... Output for CANKDATA keyword.

  if (LMETRIC) then
     if (LKECHO)write(JOSTND,1720) KEYWRD
     1720 format(/,A8,'   CANKER DATA PROVIDED IN CENTIMETERS',&
          &' (DISTANCE UP AND DISTANCE OUT);')
  else
     write (JOSTND,1725) KEYWRD
     1725 format(/,A8,'   CANKER DATA PROVIDED IN FEET (DISTANCE UP)',&
          &' AND INCHES (DISTANCE OUT);')
  end if

  if (CKINIT) then
     if (LKECHO)write(JOSTND,1730)
     1730 format(T12,'CANKER DATA WILL BE RANDOMLY GENERATED AT',&
          &' INITIALIZATION ACCORDING TO CANKER COUNTS READ IN;')
  end if

  !.... If unit number = 15 then we will read canker data from the
  !.... keyword file (directly following the CANKDATA keyword).
  !.... Otherwise, read the name of the canker data file from the
  !.... supplemental record (directly following the CANKDATA keyword).

  if (ICIN.eq.15) then
     if (LKECHO)write(JOSTND,1705)
     1705 format(T12,'CANKER DATA WILL BE READ FROM SUPPLEMENTAL',&
          &' RECORDS FOLLOWING THIS KEYWORD')
     ICIN = IREAD
  else
     read(IREAD,1710,end = 8900) DNAME
     1710 format(A80)

     if (DNAME .eq. ' ') then
        !....       If no filename provided on supplemental record, load
        !....       dname with the first 76 characters of the FVS keyword
        !....       file name and concatenate .can extension.
        DNAME = KWDFIL(1:76)//'.can'
     end if

     !....    Remove blanks from file name.
     call UNBLNK(DNAME,LENTH)

     !....    Open the canker data file.

     inquire(UNIT = ICIN,OPENED = LOPEN)
     if (LOPEN) close(UNIT = ICIN)
     call MYOPEN(ICIN,DNAME,1,132,0,1,1,0,KODE)
     if (KODE.ne.0) then
        print*,'*BRIN* Error opening filename: ',DNAME
        go to 9000
     end if
     if (LKECHO)write(JOSTND,1715) ICIN,DNAME
     1715 format(T12,'CANKER DATA WILL BE READ FROM UNIT NUMBER ',I2,&
          &';',/T12,'CANKER DATA FILENAME = ',A80)
  end if

  !.... Read the canker data; either from the external file or from
  !.... supplemental records.

  call BRCANK
  go to 90

  1800 continue

  !.... ==========  Option Number 18: BROUT  =======================

  !.... Option 18 Controls the summary output written to the FVS output
  !.... file.
  !....
  !.... Field 1: controls "Stand Summary Statistics for Blister Rust Hosts"
  !....          Default is on
  !.... Field 2: controls "Stand DBH Class Statistics for Blister Rust Hosts"
  !....          Default is off

  if (LNOTBK(1)) then
     if (IFIX(ARRAY(1)).ne.0) then
        LBRSUM = .true.
     else
        LBRSUM = .false.
     end if
  end if
  if (LNOTBK(2)) then
     if (IFIX(ARRAY(2)).ne.0) then
        LBRDBH = .true.
     else
        LBRDBH = .false.
     end if
  end if

  !.... Output for BROUT keyword.

  if (LKECHO)write(JOSTND,1820) KEYWRD
  1820 format(/,A8,'   BLISTER RUST SUMMARY AND DBH CLASS ',&
       &'STATISTICS TABLES CONTROL.')
  if (LKECHO .and. LBRSUM)write(JOSTND,1821)
  1821 format(T12,'STAND SUMMARY STATISTICS WILL BE PRINTED.')
  if (LKECHO .and. LBRDBH)write(JOSTND,1822)
  1822 format(T12,'STAND DBH CLASS STATISTICS WILL BE PRINTED.')
  if (LKECHO .and..not. LBRDBH&
       &.and..not. LBRSUM)write(JOSTND,1823)
  1823 format(T12,'BOTH STAND DBH CLASS AND SUMMARY STATISTICS '&
       &'IS SUPPRESSED.')

  go to 90

  !.... Special entry to retrieve keywords

  entry BRKEY(IKEY,PASKEY)
  PASKEY = TABLE(IKEY)
  go to 9020

  !.... Found EOF prematurely.

  8900 continue
  call ERRGRO(.true.,2)

  !.... Common return.

  9000 continue

  if (DEBUG) write(JOSTND,9010)ICYC
  9010 format('End BRIN: Cycle = ',I5)

  9020 return
end
