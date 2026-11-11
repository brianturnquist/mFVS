!=========================!
!   dbsqlite : dbscalib   !
!=========================!
! DBSQLITE $Id$
subroutine DBSCALIB(ICFROM,CORTEM,NUMCAL,STDRAT)

  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE CALIBRATION STATS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'CALCOM.f90'
  include 'PLOT.f90'
  include 'DBSCOM.f90'

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

  !     ARGUMENT LIST

  ! Variables
  real    :: STDRAT(MAXSP),CORTEM(MAXSP)
  integer :: NUMCAL(MAXSP)

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_step,fsql3_reset,fsql3_bind_text

  integer :: ColNumber,K,KK,ISPEC,iRet,ICFROM
  character(len=2000) :: SQLStmtStr
  character(len=2) :: SPEC
  character(len=2) :: TSZ
  character(len=8) :: CSP1
  character(len=8) :: CSP2
  character(len=8) :: CSP3

  real*8  :: DCORTEM,DSTDRAT,DWCI,RCORMULT

  !     INITIALIZE VARIABLES

  if (ICALIB.eq.0) return

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  iRet = fsql3_tableexists(IoutDBref,&
       &"FVS_CalibStats"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_CalibStats ('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'TreeSize text not null,'//&
          &'SpeciesFVSnum int not null,'//&
          &'SpeciesFVS    text not null,'//&
          &'SpeciesPLANTS text not null,'//&
          &'SpeciesFIA    text not null,'//&
          &'NumTrees int null,'//&
          &'ScaleFactor real null,'//&
          &'StdErrRatio real null,'//&
          &'WeightToInput real null,'//&
          &'ReadCorMult real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ICALIB = 0
        return
     end if
  end if
  if (ICFROM.eq.1) then
     TSZ = 'LG'
  else
     TSZ = 'SM'
  end if
  write(SQLStmtStr,*) 'INSERT INTO FVS_CalibStats ',&
       &' (CaseID,StandID,TreeSize,SpeciesFVSnum,',&
       &'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
       &'NumTrees,ScaleFactor,',&
       &'StdErrRatio,WeightToInput,ReadCorMult) ',&
       &" VALUES ('",CASEID,"','",TRIM(NPLT),"','",TSZ,&
       &"',?,?,?,?,?,?,?,?,?);"
  iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
  iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     ICALIB = 0
     return
  end if

  do K = 1,MAXSP
     if (CORTEM(K).ne.1.0.or. NUMCAL(K).ge.FNMIN) then
        SPEC = NSP(MAXSP,1)(1:2)
        ISPEC = MAXSP
        do KK = 1,MAXSP
           if (K .ne. IREF(KK)) cycle
           ISPEC = KK
           SPEC = NSP(KK,1)(1:2)

           !           ASSIGN FVS, PLANTS AND FIA SPECIES CODES

           CSP1 = JSP(KK)
           CSP2 = PLNJSP(KK)
           CSP3 = FIAJSP(KK)
           exit
        end do

        !         BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

        ColNumber = 1 ! SpeciesFVSnum
        iRet = fsql3_bind_int(IoutDBref,ColNumber,KK)

        ColNumber = ColNumber+1 ! SpeciesFVS
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1,&
             &LEN_TRIM(CSP1))

        ColNumber = ColNumber+1 ! SpeciesPLANTS
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2,&
             &LEN_TRIM(CSP2))

        ColNumber = ColNumber+1 ! SpeciesFIA
        iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3,&
             &LEN_TRIM(CSP3))

        ColNumber = ColNumber+1 ! NumTrees
        iRet = fsql3_bind_int(IoutDBref,ColNumber,NUMCAL(K))

        DCORTEM = CORTEM(K)
        ColNumber = ColNumber+1 ! ScaleFactor
        iRet = fsql3_bind_double(IoutDBref,ColNumber,DCORTEM)

        if (ICFROM.eq.1) then
           DSTDRAT = STDRAT(K)
           ColNumber = ColNumber+1 ! StdErrRatio
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DSTDRAT)
        end if

        if (ICFROM.eq.1) then
           DWCI = WCI(K)
           ColNumber = ColNumber+1 ! WeightToInput (LG TREES ONLY)
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DWCI)
        end if

        if (ICFROM.eq.1) then
           RCORMULT = EXP(LOG(DCORTEM)/DWCI)
           ColNumber = ColNumber+1 ! ScaleFactor
        else
           RCORMULT = DCORTEM
           ColNumber = ColNumber+3 ! ScaleFactor
        end if

        iRet = fsql3_bind_double(IoutDBref,ColNumber,RCORMULT)

        iRet = fsql3_step(IoutDBref)
        iRet = fsql3_reset(IoutDBref)
     end if
  end do

  iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     ICALIB = 0
     return
  end if

  return
end
