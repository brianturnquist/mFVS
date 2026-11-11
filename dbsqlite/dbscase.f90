!========================!
!   dbsqlite : dbscase   !
!========================!
! DBSQLITE $Id$
subroutine DBSCASE(IFORSURE)
  use FVSVERSION
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE PROGNOSIS MODEL
  !              OUTPUT.

  !     INPUT: IFORSURE - 1 NEED CONNECTION, 0 CHECK IF CONNECTION IS
  !                         NEEDED.
  !                       2 UPDATE SAMPLINGWT AND GROUPS IN CASES TABLE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'
  include 'KEYCOM.f90'
  include 'OPCOM.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'

  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_CLOSE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLINT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLISNULL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLNAME
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_EXEC
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_OPEN
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_RESET
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_STEP
  !DEC$ ATTRIBUTES DLLIMPORT :: FSQL3_TABLEEXISTS
#if !(_WIN64)
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ADDCOLIFABSENT' :: FSQL3_ADDCOLIFABSENT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_DOUBLE'    :: FSQL3_BIND_DOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_INT'       :: FSQL3_BIND_INT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_BIND_TEXT'      :: FSQL3_BIND_TEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_CLOSE'          :: FSQL3_CLOSE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLCNT'         :: FSQL3_COLCNT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLDOUBLE'      :: FSQL3_COLDOUBLE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLINT'         :: FSQL3_COLINT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLISNULL'      :: FSQL3_COLISNULL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLNAME'        :: FSQL3_COLNAME
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLREAL'        :: FSQL3_COLREAL
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTEXT'        :: FSQL3_COLTEXT
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_COLTYPE'        :: FSQL3_COLTYPE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_ERRMSG'         :: FSQL3_ERRMSG
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_EXEC'           :: FSQL3_EXEC
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_FINALIZE'       :: FSQL3_FINALIZE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_OPEN'           :: FSQL3_OPEN
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_PREPARE'        :: FSQL3_PREPARE
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_RESET'          :: FSQL3_RESET
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_STEP'           :: FSQL3_STEP
  !DEC$ ATTRIBUTES ALIAS:'_FSQL3_TABLEEXISTS'    :: FSQL3_TABLEEXISTS
#endif

  ! Variables
  integer :: fsql3_tableexists,fsql3_exec,fsql3_errmsg
  integer,parameter:: MaxStringLen = 255

  character(len=2000) :: SQLStmtStr
  character(len=10) :: DATO
  character(len=10) :: REV
  character(len=8) :: TIM
  character(len=32) :: SVN
  character(len=*) :: CFN
  character(len=MaxStringLen) :: TIMESTAMP
  integer :: IFORSURE,IFORSR,I,KODE,IRCODE
  character(len=36) :: CID

  !     If output DB is active, indicated by presence of a nonblank CASEID,
  !     update the SamplingWt and Groups in cases table that has been
  !     determined since creation of initial record.
  !     If IFORSURE is 2 and CASEID is still blank at this time, DB output
  !     is not active (no case record to update) and just return.
  !     The call is from INITRE in the PROCESS keyword section.

  !      write(*,*) 'in DBSCASE TOP - IFORESURE, CASEID',IFORSURE,CASEID

  if (IFORSURE.eq.2.and. CASEID.ne.' ') then
     if (LENSLS.eq.-1) then
        SLSET = ""
     end if
     write(SQLStmtStr,*) "UPDATE FVS_Cases SET SamplingWt = ",SAMWT,&
          &", Groups = '", TRIM(ADJUSTL(SLSET)),"'",&
          &", StandID = '", TRIM(ADJUSTL(NPLT)),"'",&
          &" WHERE CaseID = '",CASEID,"';"

     !        write(*,*) 'in DBSCASE - ',SQLStmtStr

     IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))

     if (IRCODE.ne.0) then
        IRCODE = fsql3_errmsg(IoutDBref,SQLStmtStr,LEN(SQLStmtStr)-1)
        print *," IoutDBref=",IinDBref,&
             &" ErrMsg =",SQLStmtStr(:IRCODE)
     end if

     return
  else if (IFORSURE.eq.2.and. CASEID.eq.' ') then
     return
  end if

  !     CHECK TO SEE IF WE ARE NEEDING TO CONTINUE

  IFORSR = IFORSURE
  if (IFORSR.eq.0) then
     if (ISUMARY.ge.1.or.&
          &ICOMPUTE.ge.1.or.&
          &ICALIB.ge.1.or.&
          &IATRTLIST.ge.1.or.&
          &ITREELIST.ge.1.or.&
          &ICUTLIST.ge.1.or.&
          &IDM1.ge.1.or.IDM2.ge.1.or.IDM3.ge.1.or.&
          &IDM5.ge.1.or.IDM6.ge.1.or.&
          &IPOTFIRE.ge.1.or.&
          &IPOTFIREC.ge.1.or.&
          &IFUELS.ge.1.or.&
          &ICMRPT.ge.1.or.&
          &ICHRPT.ge.1.or.&
          &ISTRCLAS.ge.1.or.&
          &IFUELC.ge.1.or.&
          &IBURN.ge.1.or.&
          &IMORTF.ge.1.or.&
          &ISSUM.ge.1.or.&
          &ISDET.ge.1.or.&
          &ICANPR.ge.1.or.&
          &IDWDVOL.ge.1.or.&
          &IDWDCOV.ge.1.or.&
          &IBMMAIN.ge.1.or.&
          &IBMBKP.ge.1.or.&
          &IBMTREE.ge.1.or.&
          &IBMVOL.ge.1.or.&
          &ISTATS1.ge.1.or.ISTATS2.ge.1.or.&
          &IREG1.ge.1.or.IREG2.ge.1.or.IREG3.ge.1.or.&
          &IREG4.ge.1.or.IREG5.ge.1) IFORSR = 1
  end if
  if (IFORSR.eq.0) return

  !     IF ALREADY HAVE A CURRENT CASE NUMBER, JUST BAIL

  if (CASEID.ne."") return
  KODE = 0

  !     MAKE SURE WE HAVE AN OPEN CONNECTION

  if (IoutDBref.eq.-1) call DBSOPEN(.true.,.false.,KODE)
  if (KODE.eq.1) then
     ICOMPUTE = 0
     ICALIB = 0
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
     IPOTFIREC = 0
     IFUELS = 0
     ICMRPT = 0
     ICHRPT = 0
     ISTRCLAS = 0
     IFUELC = 0
     IBURN = 0
     IMORTF = 0
     ISSUM = 0
     ISDET = 0
     ICANPR = 0
     IDWDVOL = 0
     IDWDCOV = 0
     IBMMAIN = 0
     IBMBKP = 0
     IBMTREE = 0
     IBMVOL = 0
     ISTATS1 = 0
     ISTATS2 = 0
     return
  end if

  IRCODE = fsql3_tableexists(IoutDBref,"FVS_Cases"//CHAR(0))
  if (IRCODE.eq.0) then
     SQLStmtStr = "CREATE TABLE FVS_Cases"//&
          &" (CaseID text primary key,"//&
          &"Stand_CN text not null,"//&
          &"StandID text not null,"//&
          &"MgmtID text,"//&
          &"RunTitle text,"//&
          &"KeywordFile text,"//&
          &"SamplingWt real,"//&
          &"Variant text,"//&
          &"Version text,"//&
          &"RV text,"//&
          &"Groups text,"//&
          &"RunDateTime text)"//CHAR(0)
     IRCODE = fsql3_exec(IoutDBref,SQLStmtStr)
     if (IRCODE .ne.0) return
  end if

  !     CREATE ENTRY FROM DATA FOR FVSRUN TABLE

  call UUIDGEN(CASEID)
  !     CREATE DATETIME

  call GRDTIM(DATO,TIM)
  TIMESTAMP = DATO(7:10)//'-'//DATO(1:5)//'-'//TIM

  !     GET THE REVISION.
  SVN = FVSVER_VERSION
  call REVISE(VARACD,REV)

  !     STRIP 3-CHARACTER EXTENSION (IF PRESENT) FROM KEYFNAME

  KEYFNAME = TRIM(KEYFNAME)
  I = INDEX(".KEY",KEYFNAME)
  if (I.gt.0.and. I.eq.LEN_TRIM(KEYFNAME)-3) then
     KEYFNAME = KEYFNAME(1:I-4)
  end if
  if (LENSLS.eq.-1) then
     SLSET = ""
  else
     call REMOVEQUOTES(SLSET)
  end if
  call REMOVEQUOTES(DBCN)
  call REMOVEQUOTES(NPLT)
  call REMOVEQUOTES(MGMID)
  call REMOVEQUOTES(ITITLE)
  call REMOVEQUOTES(KEYFNAME)
  if (KEYFNAME.eq.' ') then
     KEYFNAME = 'Unknown'
  else
     call REMOVEQUOTES(KEYFNAME)
  end if

  write(SQLStmtStr,*)"INSERT INTO FVS_Cases",&
       &" (CaseID,Stand_CN,StandID,MgmtID,RunTitle,KeywordFile,",&
       &"SamplingWt,Variant,Version,RV,Groups,RunDateTime) ",&
       &"VALUES('",CASEID,"','",&
       &TRIM(ADJUSTL(DBCN)),"','",&
       &TRIM(ADJUSTL(NPLT)),"','",&
       &TRIM(ADJUSTL(MGMID)),"','",&
       &TRIM(ADJUSTL(ITITLE)),"','",&
       &TRIM(ADJUSTL(KEYFNAME)),"',",&
       &SAMWT,",'",VARACD,"','",&
       &TRIM(ADJUSTL(SVN)),"','",&
       &TRIM(ADJUSTL(REV)),"','",&
       &TRIM(ADJUSTL(SLSET)),"','",&
       &TRIM(ADJUSTL(TIMESTAMP)),"');"

  !      write(*,*) 'in DBSCASE - ',SQLStmtStr

  IRCODE = fsql3_exec(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (IRCODE.ne.0) then
     IRCODE = fsql3_errmsg(IoutDBref,SQLStmtStr,LEN(SQLStmtStr)-1)
     print *," IoutDBref=",IinDBref,&
          &" ErrMsg =",SQLStmtStr(:IRCODE)
  end if
  return

  !     CALLED BY FILOPN: ENTRY TO SAVE THE KEYWORD FILE NAME AND TO SET
  !                       THE DEFAULT DBS CONNECTIONS

  entry DBSVKFN (CFN)
  I = LEN_TRIM(CFN)
  if (I.gt.LEN(KEYFNAME)) then
     KEYFNAME = CFN(1:4)//'...'//CFN(I+8-LEN(KEYFNAME):)
  else
     KEYFNAME = CFN
  end if
  return

  !     ENTRY FOR WWPBM, FETCHING CASEID
  !    (NOTE: THE WWPBM NEEDS TO KNOW AND SAVE (INTERNALLY) CASEID, BECAUSE
  !     IT IS DOING ITS DB-WRITING FROM WITHIN ITS OWN INTERNAL STAND LOOP

  entry DBSWW2(CID)
  CID = CASEID
  return

end

subroutine REMOVEQUOTES (CS)
  implicit none
  ! Variables
  character(LEN = *) :: CS
  integer :: I
  if (LEN_TRIM(CS).le.0) return
  do I = 1,LEN_TRIM(CS)
     if (CS(I:I) .eq. "'") CS(I:I) = ' '
  end do
  return
end
