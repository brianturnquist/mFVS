!=================!
!   dbs : dbsin   !
!=================!
! DBS $Id$
subroutine DBSIN (KEYWRD,ARRAY,ISDSP,SDLO,SDHI,LNOTBK,LKECHO)
  implicit none

  !     OPTION PROCESSOR FOR DATA BASE CONNECTIVITY

  ! Common blocks
  include  'PRGPRM.f90'

  include  'CONTRL.f90'

  include  'PLOT.f90'

  include  'DBSCOM.f90'

  ! Variables
  integer :: KWCNT,STRLEN,IDT,ISDSP
  parameter (KWCNT = 36)

  character(len=8) :: TABLEME(KWCNT)
  character(len=8) :: PASKEY
  character(len=8) :: CSPOUT
  character(len=*) :: KEYWRD
  character(len=10) :: KARD(12)
  character(len=20) :: TABLENAME
  character(len=20) :: COLUMN
  character(len=50) :: TYPEMSG(2)
  character(len=3) :: YESNO(2)
  character(len=5000) :: SQLSTR
  character(len=5000) :: STNDKODS
  character(len=5000) :: STNDSQL
  character(len=5000) :: COLNAMES
  character(len=5000) :: TMPSTR

  logical :: LNOTBK(*),LACT,LKECHO
  integer :: KEY,I,J,IRTNCD
  integer :: I1,I2,I3,I4
  integer :: KODE,IPRMPT,NUMBER
  real    :: ARRAY(*),SDLO,SDHI

  data TABLEME /&
       &'END     ','DSNOUT  ','SQLOUT  ','SUMMARY ','COMPUTE ',&
       &'TREELIST','STANDIN ','CLIMREPT','DRIVERS ','DSNIN   ',&
       &'STANDSQL','TREESQL ','POTFIRE ','FUELSOUT','DSNESTAB',&
       &'SQLIN   ','CUTLIST ','MISRPTS ','FUELREPT','BURNREPT',&
       &'MORTREPT','SNAGSUM ','SNAGOUT ','STRCLASS','PPBMMAIN',&
       &'PPBMTREE','PPBMBKP ','PPBMVOL ','CARBRPTS','ECONRPTS',&
       &'ATRTLIST','DWDVLOUT','DWDCVOUT','RDSUM   ','RDDETAIL',&
       &'RDBBMORT'/

  data TYPEMSG/'DATABASE TABLE AND STANDARD OUTPUT ARE GENERATED',&
       &'ONLY THE DATABASE TABLE IS GENERATED'/
  data YESNO/'YES','NO'/

  !     **********          EXECUTION BEGINS          **********

  10 continue
  call KEYRDR (IREAD,JOSTND,.false.,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
  !               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.

  if (KODE.lt.0) then
     IPRMPT = -KODE
  else
     IPRMPT = 0
  end if
  if (KODE .le.0) go to 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  call ERRGRO (.true.,6)
  goto 10
  30 continue

  call FNDKEY (NUMBER,KEYWRD,TABLEME,KWCNT,KODE,.false.,JOSTND)

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

  if (KODE .eq.0) goto 90
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if
  goto 90

  !     SPECIAL END-OF-FILE TARGET

  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  90 continue

  !     PROCESS OPTIONS

  go to( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,&
       &1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,2100,2200,&
       &2300,2400,2500,2600,2700,2800,2900,3000,3100,3200,&
       &3300,3400,3500,3600), NUMBER

  100 continue
  !                        OPTION NUMBER 1 -- END
  if (LKECHO)write(JOSTND,110) KEYWRD
  110 format (/A8,'   END OF DATA BASE OPTIONS.')
  return
  200 continue
  !                        OPTION NUMBER 2 -- DSNOUT
  if (CASEID.ne. "") then
     call ERRGRO(.true.,16)
     write(JOSTND,205) TRIM(DSNOUT)
     205 format (/T12,'DSNOUT CONNECTION CAN NOT BE REDEFINED.',&
          &' DSN FOR OUTPUT REMAINS: ',A)
     read (IREAD,'(A)',end = 100) TMPSTR(1:1)
     goto 10
  end if
  read (IREAD,'(A)',end = 100) DSNOUT
  if (LKECHO)write(JOSTND,210) KEYWRD, TRIM(DSNOUT)
  210 format(/A8,'   DSN FOR OUTPUT CONNECTION IS ',A)

  !     SIMPLY CALL THE DBS OPEN FUNCTION

  call DBSOPEN(DSNOUT,EnvHndlOut,ConnHndlOut,DBMSOUT,JOSTND,&
       &LKECHO,KODE)
  CASEID = ""

  !     CHECK TO SEE IF A CONNECTION WAS SUCCESSFULLY OPENED
  if (KODE.eq.0) then
     ICOMPUTE = 0
     ISUMARY = 0
     IATRTLIST = 0
     ITREELIST = 0
     ICUTLIST = 0
     IDM1 = 0
     IDM2 = 0
     IDM3 = 0
     IDM5 = 0
     IDM6 = 0
     IPOTFIRE = 0
     IFUELS = 0
     IFUELC = 0
     IDWDVOL = 0
     IDWDCOV = 0
     IBURN = 0
     ICMRPT = 0
     ICHRPT = 0
     ICLIM = 0
     IMORTF = 0
     ISSUM = 0
     ISDET = 0
     ISTRCLAS = 0
     IBMMAIN = 0
     IBMBKP = 0
     IBMTREE = 0
     IBMVOL = 0
     IDBSECON = 0

     write(JOSTND,215)TRIM(DSNOUT)
     215 format(T12,'DBS ERROR: OUTPUT CONNECTION FAILED FOR DSN:',A)
  else
     if (LKECHO)write(JOSTND,220) TRIM(DBMSOUT)
     220 format(T12,'CONNECTION DATA BASE TYPE:',A)
  end if
  goto 10
  300 continue

  !                        OPTION NUMBER 3 -- SQLOUT
  if (LKECHO)write(JOSTND,310) KEYWRD,'OUTPUT'
  310 format(/A8,'   SQL COMMAND FOR ',A,' CONNECTION')
  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,SQLSTR,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  if (KODE.gt.0) goto 10
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (.not.LNOTBK(1)) then

     !       QUERY IS FOR IMMEDIATE EXECUTION
     call DBSEXECSQL(SQLSTR,ConnHndlOut,.false.,KODE)
  else

     !       QUERY IF FOR SCHEDULED EXECUTION
     call OPNEW (KODE,IDT,102,0,ARRAY)
     if (KODE.eq.0) call OPCACT (KODE,SQLSTR)
     if (LKECHO)write(JOSTND,320) IDT
     320 format (T12,'DATE/CYCLE=',I5,&
          &'; SQL COMMAND IS PENDING.')
  end if
  goto 10
  400 continue
  !                        OPTION NUMBER 4 -- SUMMARY
  ISUMARY = 1
  if (LKECHO)write(JOSTND,410) KEYWRD
  410 format(/A8,'   SUMMARY STATISTICS SENT TO SPECIFIED DATABASE.')
  goto 10
  500 continue
  !                        OPTION NUMBER 5 -- COMPUTE
  ICOMPUTE = 1
  IADDCMPU = 0
  if (ARRAY(1).gt.0) IADDCMPU = 1
  I = 1
  if (IADDCMPU.eq.1) I = 2
  I_CMPU = 0
  if (ARRAY(2).gt.0) I_CMPU = 1
  J = 2
  if (IADDCMPU.eq.1) J = 1
  if (LKECHO)write(JOSTND,510) KEYWRD,YESNO(I),YESNO(J)
  510 format(/A8,'   COMPUTE VARIABLES SENT TO SPECIFIED DATABASE.'/&
       &T12,'ADD NEW VARIABLES TO EXISTING TABLE: ',A/&
       &T12,'INCLUDE VARIABLES STARTING WITH UNDERSCORE: ',A)

  goto 10
  600 continue
  !                        OPTION NUMBER 6 -- TREELIST
  ITREELIST = 1
  I = 1
  ISPOUT6 = 0
  if (ARRAY(1).gt.0) ITREELIST = INT(ARRAY(1))
  if (ARRAY(2).gt.0) ISPOUT6 = INT(ARRAY(2))
  if (ISPOUT6.eq.1) then
     CSPOUT = 'ALPHA'
  else if (ISPOUT6.eq.2) then
     CSPOUT = 'FIA'
  else if (ISPOUT6.eq.3) then
     CSPOUT = 'PLANTS'
  else
     CSPOUT = 'NORMAL'
  end if
  I = ITREELIST
  if (LKECHO)write(JOSTND,610) KEYWRD,YESNO(I),CSPOUT
  610 format(/A8,'   TREE INFORMATION FROM OUTPUT SENT ',&
       &'TO SPECIFIED DATABASE.'/&
       &T12,'CREATE TEXT FILE REPORT: ',A/&
       &T12,'SPECIES CODE OUTPUT FORMAT: ',A)
  goto 10
  700 continue
  !                        OPTION NUMBER 7 -- STANDIN

  if (LKECHO)write(JOSTND,710) KEYWRD,TRIM(DSNIN)
  710 format(/A8,' STANDIN COMMAND FOR INPUT CONNECTION: ',A)

  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,STNDKODS,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  if (KODE.eq.0.and.LEN(TRIM(STNDKODS)).gt.0) then

     !       CREATE QUERY STRING
     if (TRIM(DBMSOUT).eq.'EXCEL') then
        TABLENAME = '[FVS_StandInit$]'
     else
        TABLENAME = 'FVS_StandInit'
     end if
     !CREATE THE COLNAMES STRING FOR THE SQL QUERY
     COLNAMES = ''
     do while(LEN(TRIM(STNDKODS)).gt.0)
        call DBSPRS(COLUMN,STNDKODS,' ')
        TMPSTR = TRIM(COLNAMES)
        COLNAMES = TRIM(TMPSTR)//TRIM(COLUMN)//','
     end do
     !STRIP OFF TRAILING COMMA
     STRLEN = LEN(TRIM(COLNAMES))
     TMPSTR = TRIM(COLNAMES)
     COLNAMES = TMPSTR(:STRLEN-1)
     !CREATE QUERY
     write(STNDSQL,*)'SELECT ',TRIM(COLNAMES),' FROM ',&
          &TRIM(TABLENAME),' WHERE Stand_ID = ',CHAR(39),TRIM(NPLT),&
          &CHAR(39),' AND Inv_Year = ',IY(1)

     call DBSSTANDIN(STNDSQL,LKECHO)
  end if

  goto 10
  800 continue
  !                        OPTION NUMBER 8 -- CLIMATE
  ICLIM = 1
  if (LKECHO) write(JOSTND,810) KEYWRD
  810 format(/A8,'   OUTPUT THE CLIMATE-FVS TABLE TO SPECIFIED ',&
       &'DATABASE.')
  goto 10
  900 continue
  !                        OPTION NUMBER 9 -- DRIVERS
  write(JOSTND,910) KEYWRD
  910 format(/A8,'   ODBC DRIVERS AND ATTRIBUTES:')
  call DBSDRIV(JOSTND)
  goto 10
  1000 continue
  !                        OPTION NUMBER 10 -- DSNIN

  read (IREAD,'(A)',end = 100) DSNIN
  if (LKECHO)write(JOSTND,1010) KEYWRD, TRIM(DSNIN)
  1010 format(/A8,'   DSN FOR INPUT CONNECTION IS ',A)

  !     SIMPLY CALL THE DBS OPEN FUNCTION

  call DBSOPEN(DSNIN,EnvHndlIn,ConnHndlIn,DBMSIN,JOSTND,&
       &LKECHO,KODE)
  !     CHECK TO SEE IF CONNECTION WAS SUCCESSFUL
  if (KODE.eq.0) then
     write(JOSTND,1015)TRIM(DSNIN)
     1015 format(T12,'DBS ERROR: INPUT CONNECTION FAILED FOR DSN:',A)
  else
     if (LKECHO)write(JOSTND,1020) TRIM(DBMSIN)
     1020 format(T12,'CONNECTION DATA BASE TYPE:',A)
  end if

  goto 10
  1100 continue
  !                        OPTION NUMBER 11 -- STANDSQL
  if (LKECHO)write(JOSTND,1110) KEYWRD,TRIM(DSNIN)
  1110 format(/A8,T12'STANDSQL COMMAND FOR INPUT CONNECTION: ',A)

  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,STNDSQL,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  if (KODE.eq.0) then
     !       PARSE OUT AND REPLACE WITH USER DEFINED AND EVENT MONITOR VAR VALS
     call DBSPRSSQL(STNDSQL,.false.,KODE)
     if (KODE.eq.0) then
        !         THERE WAS A PROBLEM IN PARSING THE SQL STATEMENT
        write(JOSTND,1120) 'STANDSQL',TRIM(STNDSQL)
        1120 format ('********  WARNING: ',A,' CAN NOT PREFORM'&
             &' SUBSTITUTIONS IN SQL COMMAND: ',A)
        call RCDSET(2,.true.)
        return
     end if
     call DBSSTANDIN(STNDSQL,LKECHO)
  end if
  goto 10
  1200 continue
  !                        OPTION NUMBER 12 -- TREESQL
  if (LKECHO)write(JOSTND,1210) KEYWRD,TRIM(DSNIN)
  1210 format(/A8,T12'TREESQL COMMAND FOR INPUT CONNECTION: ',A)

  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,SQLSTR,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  if (KODE.eq.0) then
     !        MAKE SURE WE HAVE AN OPEN CONNECTION
     if (ConnHndlIn.eq.-1) call DBSOPEN(DSNIN,EnvHndlIn,&
          &ConnHndlIn,DBMSIN,JOSTND,LKECHO,KODE)
     !        ALLOCATE A STATEMENT HANDLE

     iRet = fvsSQLAllocHandle(SQL_HANDLE_STMT,ConnHndlIn,&
          &StmtHndlTree)
     if (iRet.ne.SQL_SUCCESS .and.&
          &iRet.ne. SQL_SUCCESS_WITH_INFO) then
        call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlIn,&
             &'DBSIN:DSN Connection')
     else
        !           PARSE OUT AND REPLACE WITH USER DEFINED AND EVENT MONITOR VAR VALS
        call DBSPRSSQL(SQLSTR,.false.,KODE)
        if (KODE.eq.0) then
           !             THERE WAS A PROBLEM IN PARSING THE SQL STATEMENT
           if (LKECHO)write(JOSTND,1120) 'TREESQL',TRIM(SQLSTR)
           call RCDSET(2,.true.)
           return
        end if
        !          EXECUTE QUERY
        iRet = fvsSQLExecDirect(StmtHndlTree,trim(SQLSTR),&
             &int(len_trim(SQLSTR),SQLINTEGER_KIND))
        if (iRet.eq.SQL_SUCCESS .or.&
             &iRet.eq. SQL_SUCCESS_WITH_INFO) then
           ITREEIN = 1
           call INTREE(TMPSTR,2,ISDSP,SDLO,SDHI,LKECHO)
           call fvsGetRtnCode(IRTNCD)
           if (IRTNCD.ne.0) return
           MORDAT = .true.
        else
           call DBSDIAGS(SQL_HANDLE_STMT,StmtHndlTree,&
                &'DBSIN:Executing TreeSQL KeyWrd: '//trim(SQLSTR))
        end if
     end if
     ITREEIN = 0
  end if
  goto 10
  1300 continue
  !                        OPTION NUMBER 13 -- POTFIRE
  call FMLNKD(LACT)
  if (LACT) then
     IPOTFIRE = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DON'T WANT TO PRINT TO FILE
        IPOTFIRE = 2
        I = 2
     end if
     if (LKECHO)write(JOSTND,1310) KEYWRD,YESNO(I)
     1310 format(/A8,'   POTFIRE STATISTICS SENT TO SPECIFIED DATABASE.'/&
          &T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IPOTFIRE))
  else
     if (LKECHO)write(JOSTND,1310) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10
  1400 continue
  !                        OPTION NUMBER 14 -- FUELSOUT
  call FMLNKD(LACT)
  if (LACT) then
     IFUELS = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DON'T WANT TO PRINT TO FILE
        IFUELS = 2
        I = 2
     end if
     if (LKECHO)write(JOSTND,1410) KEYWRD,YESNO(I)
     1410 format(/A8,'   FUELS STATISTICS SENT TO SPECIFIED DATABASE.'/&
          &T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IFUELS))
  else
     write(JOSTND,1410) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10
  1500 continue
  !                        OPTION NUMBER 15 -- DSNESTAB
  read (IREAD,'(A)') DSNRGIN
  if (LKECHO)write(JOSTND,1510) KEYWRD, TRIM(DSNRGIN)
  1510 format(/A8,'   DSN FOR REGEN INPUT CONNECTION IS ',A)

  !     SIMPLY CALL THE DBS OPEN FUNCTION

  call DBSOPEN(DSNRGIN,EnvHndlRgnIn,ConnHndlRgnIn,DBMSRGIN,&
       &JOSTND,LKECHO,KODE)
  if (KODE .eq.0) then
     IRGIN = 1
     if (LKECHO)write(JOSTND,1515) TRIM(DBMSRGIN)
     1515 format(T12,'SUCCESSFUL CONNECTION TO DATABASE TYPE:',A)
  else
     write(JOSTND,1516)
     1516 format(T12,'CONNECTION TO DATABASE FAILED.')
     call  DBSDIAGS(SQL_HANDLE_DBC,ConnHndlRgnIn,&
          &'DSNRGIN:DSN Connection')
  end if
  goto 10
  1600 continue
  !                        OPTION NUMBER 16 -- SQLIN
  if (LKECHO)write(JOSTND,310) KEYWRD,'INPUT'
  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,SQLSTR,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  if (KODE.gt.0) goto 10
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (.not.LNOTBK(1)) then

     !       QUERY IS FOR IMMEDIATE EXECUTION
     call DBSEXECSQL(SQLSTR,ConnHndlIn,.false.,KODE)
  else

     !       QUERY IF FOR SCHEDULED EXECUTION
     call OPNEW (KODE,IDT,101,0,ARRAY)
     if (KODE.eq.0) call OPCACT (KODE,SQLSTR)
     if (LKECHO)write(JOSTND,320) IDT
  end if
  goto 10
  1700 continue
  !                        OPTION NUMBER 17 -- CUTLIST
  ICUTLIST = 1
  I = 1
  ISPOUT17 = 0
  if (ARRAY(1).gt.0) ICUTLIST = INT(ARRAY(1))
  if (ARRAY(2).gt.0) ISPOUT17 = INT(ARRAY(2))
  if (ISPOUT17.eq.1) then
     CSPOUT = 'ALPHA'
  else if (ISPOUT17.eq.2) then
     CSPOUT = 'FIA'
  else if (ISPOUT17.eq.3) then
     CSPOUT = 'PLANTS'
  else
     CSPOUT = 'NORMAL'
  end if
  I = ICUTLIST
  if (LKECHO)write(JOSTND,1710) KEYWRD,CSPOUT,YESNO(I)
  1710 format(/A8,T12,'CUTS INFORMATION FROM OUTPUT SENT ',&
       &'TO SPECIFIED DATABASE.'/&
       &T12,'SPECIES CODE OUTPUT FORMAT: ',A/&
       &T12,'CREATE TEXT FILE REPORT: ',A)
  goto 10
  1800 continue
  !                        OPTION NUMBER 18 -- MISRPTS
  IDM1 = 1
  IDM2 = 1
  IDM3 = 1
  IDM5 = 1
  IDM6 = 1
  I = 1
  if (ARRAY(1).gt.0) then
     IDM1 = INT(ARRAY(1))
     IDM2 = INT(ARRAY(1))
     IDM3 = INT(ARRAY(1))
     IDM5 = INT(ARRAY(1))
     IDM6 = INT(ARRAY(1))
  end if
  I = IDM1
  if (LKECHO)write(JOSTND,1810) KEYWRD,YESNO(I)
  1810 format(/A8,'   MISTLETOE INFORMATION FROM OUTPUT SENT ',&
       &'TO SPECIFIED DATABASE.'/&
       &T12,'CREATE TEXT FILE REPORTS: ',A)
  goto 10
  1900 continue
  !                        OPTION NUMBER 19 -- FUELREPT
  call FMLNKD(LACT)
  if (LACT) then
     IFUELC = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IFUELC = 2
        I = 2
     end if
     if (LKECHO)write(JOSTND,1910) KEYWRD,YESNO(I)
     1910 format(/A8,'   CONSUMPTION STATS SENT TO SPECIFIED DATABASE.'/&
          &T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IFUELC))
  else
     write(JOSTND,1910) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10
  2000 continue
  !                        OPTION NUMBER 20 -- BURNREPT
  call FMLNKD(LACT)
  if (LACT) then
     IBURN = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IBURN = 2
        I = 2
     end if
     if (LKECHO)write(JOSTND,2010) KEYWRD,YESNO(I)
     2010 format(/A8,'   BURN CONDITION STATS SENT TO SPECIFIED ',&
          &'DATABASE.'/&
          &T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IBURN))
  else
     write(JOSTND,2010) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10
  2100 continue
  !                        OPTION NUMBER 21 -- MORTREPT
  call FMLNKD(LACT)
  if (LACT) then
     IMORTF = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IMORTF = 2
        I = 2
     end if
     ISPOUT21 = 0
     if (ARRAY(2).gt.0) ISPOUT21 = INT(ARRAY(2))
     if (ISPOUT21.eq.1) then
        CSPOUT = 'ALPHA'
     else if (ISPOUT21.eq.2) then
        CSPOUT = 'FIA'
     else if (ISPOUT21.eq.3) then
        CSPOUT = 'PLANTS'
     else
        CSPOUT = 'NORMAL'
     end if
     if (LKECHO)write(JOSTND,2110) KEYWRD,CSPOUT,&
          &YESNO(I)
     2110 format(/A8,'   FFE MORTALITY INFO SENT TO SPECIFIED DATABASE.'/&
          &T12,'SPECIES CODE OUTPUT FORMAT: ',A/&
          &T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IMORTF))
  else
     write(JOSTND,2110) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10
  2200 continue
  !                        OPTION NUMBER 22 -- SNAGSUM
  call FMLNKD(LACT)
  if (LACT) then
     ISSUM = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        ISSUM = 2
        I = 2
     end if
     if (LKECHO)write(JOSTND,2210) KEYWRD,YESNO(I)
     2210 format(/A8,'   SUMMARY SNAG INFO SENT TO SPECIFIED DATABASE.'/&
          &T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(ISSUM))
  else
     write(JOSTND,2210) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10
  2300 continue
  !                        OPTION NUMBER 23 -- SNAGOUT
  call FMLNKD(LACT)
  if (LACT) then
     ISDET = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        ISDET = 2
        I = 2
     end if
     ISPOUT23 = 0
     if (ARRAY(2).gt.0) ISPOUT23 = INT(ARRAY(2))
     if (ISPOUT23.eq.1) then
        CSPOUT = 'ALPHA'
     else if (ISPOUT23.eq.2) then
        CSPOUT = 'FIA'
     else if (ISPOUT23.eq.3) then
        CSPOUT = 'PLANTS'
     else
        CSPOUT = 'NORMAL'
     end if
     if (LKECHO)write(JOSTND,2310) KEYWRD,CSPOUT,&
          &YESNO(I)
     2310 format(/A8,'   DETAILED SNAG INFO SENT TO SPECIFIED DATABASE.'/&
          &T12,'SPECIES CODE OUTPUT FORMAT: ',A/&
          &T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(ISDET))
  else
     write(JOSTND,2310) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10
  2400 continue
  !                        OPTION NUMBER 24 -- STRCLASS
  ISTRCLAS = 1
  I = 1
  if (ARRAY(1).gt.1) then
     !       WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
     ISTRCLAS = 2
     I = 2
  end if
  if (LKECHO)write(JOSTND,2410) KEYWRD,YESNO(I)
  2410 format(/A8,'   STRUCTURE CLASS INFO SENT TO SPECIFIED DATABASE.'/&
       &T12,'INCLUDE REPORT IN OUT FILE: ',A)
  if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(ISTRCLAS))
  goto 10
  2500 continue
  !                        OPTION NUMBER 25 --PPBMMAIN
  call BMLNKD(LACT)
  if (LACT) then
     IBMMAIN = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IBMMAIN = 2
     end if
     if (LKECHO)write(JOSTND,2510) KEYWRD
     2510 format(/A8,'   WWPBM MAIN OUTPUT SENT TO SPECIFIED DATABASE.')
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IBMMAIN))
  else
     write(JOSTND,2510) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if

  goto 10
  2600 continue
  !                        OPTION NUMBER 26 --PPBMTREE
  call BMLNKD(LACT)
  if (LACT) then
     IBMTREE = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IBMTREE = 2
     end if
     if (LKECHO)write(JOSTND,2610) KEYWRD
     2610 format(/A8,'   WWPBM TREE-LEVEL OUTPUT SENT TO SPECIFIED ',&
          &'DATABASE.')
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IBMTREE))
  else
     write(JOSTND,2610) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if

  goto 10
  2700 continue
  !                        OPTION NUMBER 27 --PPBMBKP
  call BMLNKD(LACT)
  if (LACT) then
     IBMBKP = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IBMBKP = 2
     end if
     if (LKECHO)write(JOSTND,2710) KEYWRD
     2710 format(/A8,'   WWPBM DETAILED BKP OUTPUT SENT TO'&
          &' SPECIFIED DATABASE.')
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IBMBKP))
  else
     write(JOSTND,2710) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if

  goto 10
  2800 continue
  !                        OPTION NUMBER 28 --PPBMVOL
  call BMLNKD(LACT)
  if (LACT) then
     IBMVOL = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IBMVOL = 2
     end if
     if (LKECHO)write(JOSTND,2810) KEYWRD
     2810 format(/A8,'   WWPBM DETAILED VOLUME OUTPUT SENT TO '&
          &'SPECIFIED DATABASE.')
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IBMMAIN))
  else
     write(JOSTND,2810) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if

  goto 10

  2900 continue
  !                        OPTION NUMBER 29 -- CARBRPTS
  call FMLNKD(LACT)
  if (LACT) then
     ICMRPT = 1
     ICHRPT = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        ICMRPT = 2
        ICHRPT = 2
        I = 2
     end if
     if (LKECHO)write(JOSTND,2910) KEYWRD,YESNO(I)
     2910 format(/A8,'   CARBON REPORTS SENT TO SPECIFIED DATABASE.'/&
          &T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(I))
  else
     write(JOSTND,2910) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10

  3000 continue
  !                        OPTION NUMBER 30 -- ECONRPTS
  IDBSECON = 2
  ISPOUT30 = 0
  if (NINT(ARRAY(1)) == 1) IDBSECON = 1
  if (ARRAY(2).gt.0) ISPOUT30 = INT(ARRAY(2))
  if (ISPOUT30.eq.1) then
     CSPOUT = 'ALPHA'
  else if (ISPOUT30.eq.2) then
     CSPOUT = 'FIA'
  else if (ISPOUT30.eq.3) then
     CSPOUT = 'PLANTS'
  else
     CSPOUT = 'NORMAL'
  end if
  if (LKECHO) write(JOSTND,3010) KEYWRD,CSPOUT
  3010 format(/A8,'   ECON REPORTS SENT TO SPECIFIED DATABASE.'/&
       &T12,'SPECIES CODE OUTPUT FORMAT: ',A)
  goto 10
  3100 continue
  !                        OPTION NUMBER 31 -- ATRTLIST
  IATRTLIST = 1
  I = 1
  ISPOUT31 = 0
  if (ARRAY(1).gt.0) IATRTLIST = INT(ARRAY(1))
  if (ARRAY(2).gt.0) ISPOUT31 = INT(ARRAY(2))
  if (ISPOUT31.eq.1) then
     CSPOUT = 'ALPHA'
  else if (ISPOUT31.eq.2) then
     CSPOUT = 'FIA'
  else if (ISPOUT31.eq.3) then
     CSPOUT = 'PLANTS'
  else
     CSPOUT = 'NORMAL'
  end if
  I = IATRTLIST
  if (LKECHO)write(JOSTND,3110) KEYWRD,CSPOUT,YESNO(I)
  3110 format(/A8,'   TREE INFORMATION FROM OUTPUT SENT ',&
       &'TO SPECIFIED DATABASE.'/&
       &T12,'SPECIES CODE OUTPUT FORMAT: ',A/&
       &T12,'CREATE TEXT FILE REPORT: ',A)
  goto 10
  3200 continue
  !                        OPTION NUMBER 32 -- DWDVLOUT
  call FMLNKD(LACT)
  if (LACT) then
     IDWDVOL = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IDWDVOL = 2
        I = 2
     end if
     if (LKECHO)write(JOSTND,3210) KEYWRD,YESNO(I)
     3210 format(/A8,'   DOWN WOOD VOLUME REPORT SENT TO SPECIFIED ',&
          &'DATABASE.'/ T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IDWDVOL))
  else
     write(JOSTND,3210) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10
  3300 continue
  !                        OPTION NUMBER 33 -- DWDCVOUT
  call FMLNKD(LACT)
  if (LACT) then
     IDWDCOV = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
        IDWDCOV = 2
        I = 2
     end if
     if (LKECHO)write(JOSTND,3310) KEYWRD,YESNO(I)
     3310 format(/A8,'   DOWN WOOD COVER REPORT SENT TO SPECIFIED ',&
          &'DATABASE.'/ T12,'INCLUDE REPORT IN OUT FILE: ',A)
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IDWDCOV))
  else
     write(JOSTND,3310) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if
  goto 10

  3400 continue
  !                        OPTION NUMBER 34 -- RDSUM
  IRD1 = 1
  if (LKECHO) write(JOSTND,3410) KEYWRD
  3410 format(/A8,'   ROOT DISEASE SUMMARY TABLE SENT ',&
       &'TO SPECIFIED DATABASE.')
  goto 10

  3500 continue
  !                        OPTION NUMBER 35 -- RDDETAIL
  IRD2 = 1
  if (LKECHO) write(JOSTND,3510) KEYWRD
  3510 format(/A8,'   ROOT DISEASE DETAIL TABLE SENT ',&
       &'TO SPECIFIED DATABASE.')
  goto 10

  3600 continue
  !                        OPTION NUMBER 36 -- RDBBMORT
  IRD3 = 1
  if (LKECHO) write(JOSTND,3610) KEYWRD
  3610 format(/A8,'   ROOT DISEASE BARK BEETLE MORTALITY ',&
       &'TABLE SENT TO SPECIFIED DATABASE.')
  goto 10

  !.... Special entry to retrieve keywords.

  entry DBSKEY (KEY,PASKEY)
  PASKEY = TABLEME(KEY)
  return

  !     ENTRY TO FETCH WWPBM OUTPUT TABLE FLAGS
  !     CALLED FROM BMSDIT

  entry DBSWW(I1,I2,I3,I4)
  I1 = IBMMAIN
  I2 = IBMTREE
  I3 = IBMBKP
  I4 = IBMVOL
  return

end

subroutine DBSIN_GETCMD(INFILE,JOSTND,IRECNT,CMDBUF,KODE,LKECHO)
  implicit none

  !     PRIVATE ROUTINE TO DBSIN, USED TO LOAD THE CMDBUF.
  ! Variables
  logical :: LKECHO
  integer :: INFILE,JOSTND,IRECNT,KODE
  integer :: I1,I2,I3,MXLEN,IRTNCD
  character(len=*) :: CMDBUF
  character(len=140) :: LINE
  character(len=140) :: UCLINE

  MXLEN = LEN(CMDBUF)

  I1 = 1
  10 continue
  read (INFILE,'(A)',end = 100) LINE
  IRECNT = IRECNT+1
  UCLINE = LINE
  do I3 = 1,LEN_TRIM(LINE)
     call UPCASE(UCLINE(I3:I3))
  end do
  if (INDEX(UCLINE,'ENDSQL').eq.0) then
     I2 = LEN_TRIM(LINE)
     if (LKECHO)write(JOSTND,'(T12,A)') LINE(:I2)
     LINE = ADJUSTL(TRIM(LINE))
     I2 = LEN_TRIM(LINE)
     if (I1+I2.ge.MXLEN) then
        write(JOSTND,20)
        20 format (/'******** ERROR: SQL COMMAND TOO LONG.')
        KODE = 1
        call RCDSET (2,.true.)
     else
        CMDBUF(I1:) = LINE(:I2)
        I1 = I1+I2+1
        goto 10
     end if
  else
     KODE = 0
  end if
  return
  100 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

end
