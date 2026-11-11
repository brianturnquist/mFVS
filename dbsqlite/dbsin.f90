!======================!
!   dbsqlite : dbsin   !
!======================!
! DBSQLITE $Id$
subroutine DBSIN (KEYWRD,ARRAY,ISDSP,SDLO,SDHI,LNOTBK,LKECHO)
  implicit none

  !     OPTION PROCESSOR FOR DATA BASE CONNECTIVITY

  ! Common blocks
  include  'PRGPRM.f90'

  include  'CONTRL.f90'

  include  'PLOT.f90'

  include  'DBSCOM.f90'

  ! Variables
  integer :: KWCNT,STRLEN,IDT,ISDSP,iRet
  parameter (KWCNT = 43)
  integer,parameter :: MxMsg = 100
  character(LEN = MxMsg) :: Msg
  character(len=8) :: TABLEME(KWCNT)
  character(len=8) :: PASKEY
  character(len=8) :: CSPOUT
  character(len=*) :: KEYWRD
  character(len=10) :: KARD(12)
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

  integer :: fsql3_prepare,fsql3_errmsg

  data TABLEME /&
       &'END     ','DSNOUT  ','SQLOUT  ','SUMMARY ','COMPUTDB',&
       &'TREELIDB','STANDIN ','CLIMREDB','DRIVERS ','DSNIN   ',&
       &'STANDSQL','TREESQL ','POTFIRDB','FUELSOUT','DSNESTAB',&
       &'SQLIN   ','CUTLIDB ','MISRPTS ','FUELREDB','BURNREDB',&
       &'MORTREDB','SNAGSUDB','SNAGOUDB','STRCLSDB','PPBMMAIN',&
       &'PPBMTREE','PPBMBKP ','PPBMVOL ','CARBREDB','ECONRPTS',&
       &'ATRTLIDB','DWDVLDB ','DWDCVDB ','RDSUM   ','RDDETAIL',&
       &'RDBBMORT','CALBSTDB','INVSTATS','REGREPTS','VBCSUMDB',&
       &'VBCTRLDB','VBCCUTDB','VBCATRDB'/

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

  !c   80 CONTINUE
  !c      CALL ERRGRO(.FALSE.,2)
  !c      CALL fvsGetRtnCode(IRTNCD)
  !c      IF (IRTNCD.NE.0) RETURN

  90 continue

  !     PROCESS OPTIONS

  go to( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,&
       &1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,&
       &2100,2200,2300,2400,2500,2600,2700,2800,2900,3000,&
       &3100,3200,3300,3400,3500,3600,3700,3800,3900,4000,&
       &4100,4200,4300), NUMBER

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
     205 format (/T12,'DSNOUT DATA BASE CAN NOT BE REDEFINED.',&
          &' DSN FOR OUTPUT REMAINS: ',A)
     read (IREAD,'(A)',end = 100) TMPSTR(1:1)
     goto 10
  end if
  read (IREAD,'(A)',end = 100) DSNOUT
  if (LKECHO)write(JOSTND,210) KEYWRD, TRIM(DSNOUT)
  210 format(/A8,'   DSN FOR OUTPUT DATA BASE IS ',A)
  call DBSOPEN(.true.,.false.,KODE)
  CASEID = ""

  !     CHECK TO SEE IF A DATA BASE WAS SUCCESSFULLY OPENED
  if (KODE.ne.0) then
     ICOMPUTE = 0
     ISUMARY = 0
     IATRTLIST = 0
     ITREELIST = 0
     ICUTLIST = 0
     IVBCSUM = 0
     IVBCTRELST = 0
     IVBCCUTLST = 0
     IVBCATRLST = 0
     IDM1 = 0
     IDM2 = 0
     IDM3 = 0
     IDM5 = 0
     IDM6 = 0
     IPOTFIRE = 0
     IPOTFIREC = 0
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
     ICALIB = 0

     write(JOSTND,215)TRIM(DSNOUT)
     215 format(T12,'********  ERROR: OUTPUT OPEN FAILED FOR DSN:',A)
  else

     !       OUTPUT DATABASE HAS BEEN OPENED. CREATE CASE

     call DBSCASE(1)
  end if
  goto 10
  300 continue

  !                        OPTION NUMBER 3 -- SQLOUT
  if (LKECHO)write(JOSTND,310) KEYWRD,'OUTPUT'
  310 format(/A8,'   SQL COMMAND FOR ',A,' DATA BASE')
  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,SQLSTR,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  if (KODE.gt.0) goto 10
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (.not.LNOTBK(1)) then

     !       QUERY IS FOR IMMEDIATE EXECUTION
     call DBSEXECSQL(SQLSTR,IoutDBref,.false.,KODE)
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
  if (LNOTBK(1) .and. ARRAY(1).eq.2) ISUMARY = 2
  if (LKECHO .and. ISUMARY.eq.1) write(JOSTND,410) KEYWRD
  410 format(/A8,'   SUMMARY STATISTICS SENT TO DATABASE.')
  if (LKECHO .and. ISUMARY.eq.2) write(JOSTND,420) KEYWRD
  420 format(/A8,'   SUMMARY VERSION 2 STATISTICS SENT TO DATABASE.')
  goto 10
  500 continue
  !                        OPTION NUMBER 5 -- COMPUTDB
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
  !                        OPTION NUMBER 6 -- TREELIDB
  ITREELIST = 1
  I = 1
  ISPOUT6 = 0
  if (ARRAY(1).gt.0) ITREELIST = int(ARRAY(1))
  if (ARRAY(2).gt.0) ISPOUT6 = int(ARRAY(2))
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
  710 format(/A8,' STANDIN COMMAND FOR INPUT DATA BASE: ',A)

  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,STNDKODS,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  if (KODE.eq.0.and.LEN(TRIM(STNDKODS)).gt.0) then

     !       CREATE QUERY STRING
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
          &'FVS_StandInit WHERE Stand_ID = ',CHAR(39),TRIM(NPLT),&
          &CHAR(39),' AND Inv_Year = ',IY(1)

     call DBSSTANDIN(STNDSQL,LKECHO)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if

  goto 10
  800 continue
  !                        OPTION NUMBER 8 -- CLIMREDB
  ICLIM = 1
  if (LKECHO) write(JOSTND,810) KEYWRD
  810 format(/A8,'   OUTPUT THE CLIMATE-FVS TABLE TO SPECIFIED ',&
       &'DATABASE.')
  goto 10
  900 continue
  !                        OPTION NUMBER 9 -- DRIVERS
  write(JOSTND,910) KEYWRD
  910 format(/A8,'   KEYWORD NOT SUPPORTED')
  goto 10
  1000 continue
  !                        OPTION NUMBER 10 -- DSNIN

  read (IREAD,'(A)',end = 100) DSNIN
  if (LKECHO)write(JOSTND,1010) KEYWRD, TRIM(DSNIN)
  1010 format(/A8,'   INPUT DATA BASE IS ',A)
  call DBSOPEN(.false.,.true.,KODE)
  if (KODE.ne.0) then
     write(JOSTND,1015)TRIM(DSNIN)
     1015 format(T12,'********   ERROR: INPUT OPEN FAILED FOR DSN:',A)
  end if
  goto 10
  1100 continue
  !                        OPTION NUMBER 11 -- STANDSQL
  if (LKECHO)write(JOSTND,1110) KEYWRD,TRIM(DSNIN)
  1110 format(/A8,T12,'STANDSQL COMMAND FOR INPUT DATA BASE: ',A)

  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,STNDSQL,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  if (KODE.eq.0) then
     !       PARSE OUT AND REPLACE WITH USER DEFINED AND EVENT MONITOR VAR VALS
     call DBSPRSSQL(STNDSQL,.false.,KODE)
     if (KODE.eq.0) then
        !         THERE WAS A PROBLEM IN PARSING THE SQL STATEMENT
        write(JOSTND,1120) 'STANDSQL',TRIM(STNDSQL)
        1120 format ('********  ERROR: ',A,' CAN NOT PREFORM',&
             &' SUBSTITUTIONS IN SQL COMMAND: ',A)
        call RCDSET(2,.true.)
        return
     end if
     call DBSSTANDIN(STNDSQL,LKECHO)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  1200 continue
  !                        OPTION NUMBER 12 -- TREESQL
  if (LKECHO)write(JOSTND,1210) KEYWRD,TRIM(DSNIN)
  1210 format(/A8,T12,'TREESQL COMMAND FOR INPUT DATA BASE: ',A)

  call DBSIN_GETCMD(IREAD,JOSTND,IRECNT,SQLSTR,KODE,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  !     MAKE SURE WE HAVE AN OPEN DATA BASE

  if (IinDBref.eq.-1) call DBSOPEN(.false.,.true.,KODE)

  if (KODE.eq.0) then
     !        PARSE OUT AND REPLACE WITH USER DEFINED AND EVENT MONITOR VAR VALS
     call DBSPRSSQL(SQLSTR,.false.,KODE)
     if (KODE.eq.0) then
        !          THERE WAS A PROBLEM IN PARSING THE SQL STATEMENT
        write(JOSTND,1120) 'TREESQL',TRIM(SQLSTR)
        call RCDSET(2,.true.)
        return
     end if
     !        EXECUTE QUERY
     iRet = fsql3_prepare(IinDBref,trim(SQLSTR)//CHAR(0))
     if (iRet.eq.0) then
        ITREEIN = 1
        call INTREE(TMPSTR,2,ISDSP,SDLO,SDHI,LKECHO)
        MORDAT = .true.
     else
        iRet = fsql3_errmsg(IinDBref, Msg, MxMsg)
        write (JOSTND,1220) TRIM(SQLSTR), Msg(:iRet)
        1220 format(/T12,'SQL=',A/T12,'Error Msg=',A/,&
             &'********   ERROR: SQL COMMAND FAILED.')
        call RCDSET (2,.true.)
     end if
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     ITREEIN = 0
  end if
  goto 10
  1300 continue
  !                        OPTION NUMBER 13 -- POTFIRDB
  call FMLNKD(LACT)
  if (LACT) then
     IPOTFIRE = 1
     IPOTFIREC = 1
     I = 1
     if (ARRAY(1).gt.1) then
        !          WE ARE REDIRECTING OUTPUT AND DON NOT WANT TO PRINT TO FILE
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
        !          WE ARE REDIRECTING OUTPUT AND DO NOT WANT TO PRINT TO FILE
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
  write(JOSTND,1510) KEYWRD
  1510 format(/A8,'   NOT SUPPORTED')
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
     call DBSEXECSQL(SQLSTR,IinDBref,.false.,KODE)
  else

     !       QUERY IF FOR SCHEDULED EXECUTION
     call OPNEW (KODE,IDT,101,0,ARRAY)
     if (KODE.eq.0) call OPCACT (KODE,SQLSTR)
     if (LKECHO)write(JOSTND,320) IDT
  end if
  goto 10
  1700 continue
  !                        OPTION NUMBER 17 -- CUTLIDB
  ICUTLIST = 1
  I = 1
  ISPOUT17 = 0
  if (ARRAY(1).gt.0) ICUTLIST = int(ARRAY(1))
  if (ARRAY(2).gt.0) ISPOUT17 = int(ARRAY(2))
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
     IDM1 = int(ARRAY(1))
     IDM2 = IDM1
     IDM3 = IDM1
     IDM5 = IDM1
     IDM6 = IDM1
  end if
  I = IDM1
  if (LKECHO)write(JOSTND,1810) KEYWRD,YESNO(I)
  1810 format(/A8,'   MISTLETOE INFORMATION FROM OUTPUT SENT ',&
       &'TO SPECIFIED DATABASE.'/&
       &T12,'CREATE TEXT FILE REPORTS: ',A)
  goto 10
  1900 continue
  !                        OPTION NUMBER 19 -- FUELREDB
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
  !                        OPTION NUMBER 20 -- BURNREDB
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
  !                        OPTION NUMBER 21 -- MORTREDB
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
     if (ARRAY(2).gt.0) ISPOUT21 = int(ARRAY(2))
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
  !                        OPTION NUMBER 22 -- SNAGSUDB
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
  !                        OPTION NUMBER 23 -- SNAGOUDB
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
     if (ARRAY(2).gt.0) ISPOUT23 = int(ARRAY(2))
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
  !                        OPTION NUMBER 24 -- STRCLSDB
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
     2710 format(/A8,'   WWPBM DETAILED BKP OUTPUT SENT TO',&
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
     2810 format(/A8,'   WWPBM DETAILED VOLUME OUTPUT SENT TO ',&
          &'SPECIFIED DATABASE.')
     if (LKECHO)write(JOSTND,'(T12,A)') TRIM(TYPEMSG(IBMMAIN))
  else
     write(JOSTND,2810) KEYWRD,TRIM(DSNOUT)
     call ERRGRO (.true.,11)
  end if

  goto 10

  2900 continue
  !                        OPTION NUMBER 29 -- CARBREDB
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
  if (ARRAY(2).gt.0) ISPOUT30 = int(ARRAY(2))
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
  !                        OPTION NUMBER 31 -- ATRTLIDB
  IATRTLIST = 1
  I = 1
  ISPOUT31 = 0
  if (ARRAY(1).gt.0) IATRTLIST = int(ARRAY(1))
  if (ARRAY(2).gt.0) ISPOUT31 = int(ARRAY(2))
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
  !                        OPTION NUMBER 32 -- DWDVLDB
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
  !                        OPTION NUMBER 33 -- DWDCVDB
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
  3700 continue
  !                        OPTION NUMBER 37 -- CALBSTDB
  ICALIB = 1
  if (LKECHO) write(JOSTND,3710) KEYWRD
  3710 format(/A8,'   CALIBRATION STATISTICS ',&
       &'TABLE SENT TO SPECIFIED DATABASE.')
  goto 10
  3800 continue
  !                        OPTION NUMBER 38 -- INVSTATS
  ISTATS1 = 1
  ISTATS2 = 1
  if (LKECHO) write(JOSTND,3810) KEYWRD
  3810 format(/A8,'   INVENTORY STATISTICS ',&
       &'TABLES SENT TO SPECIFIED DATABASE.')
  goto 10
  3900 continue
  !                        OPTION NUMBER 39 -- REGREPRTS
  IREG1 = 1
  IREG2 = 1
  IREG3 = 1
  IREG4 = 1
  IREG5 = 1
  if (LKECHO) write(JOSTND,3910) KEYWRD
  3910 format(/A8,'   REGENERATION ',&
       &'TABLES SENT TO SPECIFIED DATABASE.')
  goto 10

  !                        OPTION NUMBER 40 -- VBCSUMDB
  4000 continue
  IVBCSUM = 1
  if (LKECHO) write(JOSTND,4010) KEYWRD
  4010 format(/A8,'   FIAVBC_SUMMARY STATISTICS SENT TO DATABASE ',&
       &'(REQUIRES FIAVBC KEYWORD).')
  goto 10

  !                        OPTION NUMBER 41 -- VBCTRLDB
  4100 continue
  IVBCTRELST = 1
  if (LKECHO)write(JOSTND,4110) KEYWRD
  4110 format(/A8,'   FIAVBC_TREELIST SENT TO DATABASE ',&
       &'(REQUIRES FIAVBC KEYWORD).')
  goto 10

  !                        OPTION NUMBER 42 -- VBCCUTDB
  4200 continue
  IVBCCUTLST = 1
  if (LKECHO)write(JOSTND,4210) KEYWRD
  4210 format(/A8,'   FIAVBC_CUTLIST SENT TO DATABASE ',&
       &'(REQUIRES FIAVBC KEYWORD).')
  goto 10

  !                        OPTION NUMBER 43 -- VBCATRDB
  4300 continue
  IVBCATRLST = 1
  if (LKECHO)write(JOSTND,4310) KEYWRD
  4310 format(/A8,'   FIAVBC_ATRLIST SENT TO DATABASE ',&
       &'(REQUIRES FIAVBC KEYWORD).')
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
     I2 = 0
     do I1 = 1,LEN_TRIM(CMDBUF)
        if (CMDBUF(I1:I1) .eq. ';') then
           I2 = I1
           exit
        end if
     end do
     if (I2.eq.0) I2 = LEN_TRIM(CMDBUF)+1
     CMDBUF(I2:) = ';'
     KODE = 0
  end if
  return
  100 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

end
