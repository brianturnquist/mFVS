!=================================!
!   metric/dbsqlite : dbsfmmort   !
!=================================!
! DBSQLITE $Id$
subroutine DBSFMMORT(IYEAR,KILLED,TOTAL,BAKILL,&
     &VOKILL,KODE)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE MORTALITY REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
  !     INPUT:
  !              THE MORTALITY OUTPUT FROM THE FIRE MODEL.
  !              1: KILLED TREES PER ACRE FOR EACH SIZE CLASS
  !              2: TOTAL TREES PER ACRE FOR EACH SIZE CLASS
  !              3: MORTALITY IN TERMS OF BASAL AREA
  !              4: MORTALITY IN TERMS OF CUFT VOLUME
  !              5: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DBSCOM.f90'
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
  integer :: MXSP1
  parameter (MXSP1 = MAXSP + 1)
  integer :: IYEAR,iRet,KODE,I,J,ColNumber
  real    :: KILLED,TOTAL,BAKILL,VOKILL
  double precision :: KILLEDB,TOTALB,BAKILLB,VOKILLB
  dimension KILLED(MXSP1,8),TOTAL(MXSP1,8),KILLEDB(MXSP1,8),&
       &TOTALB(MXSP1,8)
  dimension BAKILL(MXSP1),VOKILL(MXSP1),BAKILLB(MXSP1),&
       &VOKILLB(MXSP1)
  character(len=2000) :: SQLStmtStr
  character(LEN = 8) :: CSP1
  character(LEN = 8) :: CSP2
  character(LEN = 8) :: CSP3

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_step,fsql3_reset,fsql3_bind_text

  if (IMORTF.eq.0) return
  if (IMORTF.eq.2) KODE = 0

  call DBSCASE(1)
  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  iRet = fsql3_tableexists(IoutDBref,&
       &"FVS_Mortality_Metric"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Mortality_Metric ('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year Int null,'//&
          &'SpeciesFVS text null,'//&
          &'SpeciesPLANTS text null,'//&
          &'SpeciesFIA text null,'//&
          &'Killed_class1 real null,'//&
          &'Total_class1 real null,'//&
          &'Killed_class2 real null,'//&
          &'Total_class2 real null,'//&
          &'Killed_class3 real null,'//&
          &'Total_class3 real null,'//&
          &'Killed_class4 real null,'//&
          &'Total_class4 real null,'//&
          &'Killed_class5 real null,'//&
          &'Total_class5 real null,'//&
          &'Killed_class6 real null,'//&
          &'Total_class6 real null,'//&
          &'Killed_class7 real null,'//&
          &'Total_class7 real null,'//&
          &'Bakill real null,'//&
          &'Volkill real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IMORTF = 0
        return
     end if
  end if
  SQLStmtStr = 'INSERT INTO FVS_Mortality_Metric (CaseID,'//&
       &'StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,'//&
       &'Killed_class1,Total_class1,'//&
       &'Killed_class2,'//&
       &'Total_class2,Killed_class3,Total_class3,Killed_class4,'//&
       &'Total_class4,Killed_class5,Total_class5,Killed_class6,'//&
       &'Total_class6,Killed_class7,Total_class7,Bakill,Volkill)'//&
       &" VALUES ('"//CASEID//"','"//TRIM(NPLT)//&
       &"',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);"//CHAR(0)

  iRet = fsql3_prepare(IoutDBref, SQLStmtStr)
  if (iRet .ne.0) then
     IMORTF = 0
     return
  end if

  do J = 1,MXSP1
     if (TOTAL(J,8) .le.0) cycle
     if (J.eq.MXSP1) then
        CSP1 = 'ALL'
        CSP2 = 'ALL'
        CSP3 = 'ALL'
     else

        !     ASSIGN FVS, PLANTS AND FIA SPECIES CODES

        CSP1 = JSP(J)
        CSP2 = PLNJSP(J)
        CSP3 = FIAJSP(J)

     end if

     !       ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

     BAKILLB(J) = BAKILL(J)
     VOKILLB(J) = VOKILL(J)

     do I = 1,8
        KILLEDB(J,I) = KILLED(J,I)
        TOTALB(J,I) = TOTAL(J,I)
     end do

     ColNumber = 1
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

     ColNumber = ColNumber+1
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1,&
          &len_trim(CSP1))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2,&
          &len_trim(CSP2))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3,&
          &len_trim(CSP3))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,1))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,1))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,2))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,2))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,3))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,3))
     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,4))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,4))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,5))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,5))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,6))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,6))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,KILLEDB(J,7))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,TOTALB(J,7))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,BAKILLB(J))

     ColNumber = ColNumber+1
     iRet = fsql3_bind_double(IoutDBref,ColNumber,VOKILLB(J))

     iRet = fsql3_step(IoutDBref)
     iRet = fsql3_reset(IoutDBref)

  end do
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     IMORTF = 0
  end if
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end
