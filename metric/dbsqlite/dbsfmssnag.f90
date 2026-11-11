!==================================!
!   metric/dbsqlite : dbsfmssnag   !
!==================================!
! DBSQLITE $Id$
subroutine DBSFMSSNAG(IYEAR,NPLT,HCL1,HCL2,HCL3,HCL4,HCL5,HCL6,&
     &HCL7,SCL1,SCL2,SCL3,SCL4,SCL5,SCL6,SCL7,HDSF,KODE)

  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE SUMMARY SNAG REPORT
  !              INFORMATION
  !     AUTH: S. REBAIN -- FMSC -- DECEMBER 2004
  !     INPUT:
  !              THE SUMMARY SNAG OUTPUT FROM THE FIRE MODEL.
  !              1: HARD SNAGS - DBH CLASS 1 (> 0 BY DEFAULT)
  !              2: HARD SNAGS - DBH CLASS 2 (> 12 BY DEFAULT)
  !              3: HARD SNAGS - DBH CLASS 3 (> 18 BY DEFAULT)
  !              4: HARD SNAGS - DBH CLASS 4 (> 24 BY DEFAULT)
  !              5: HARD SNAGS - DBH CLASS 5 (> 30 BY DEFAULT)
  !              6: HARD SNAGS - DBH CLASS 6 (> 36 BY DEFAULT)
  !              7: HARD SNAGS - TOTAL       (> 0)
  !              8: SOFT SNAGS - DBH CLASS 1 (> 0 BY DEFAULT)
  !              9: SOFT SNAGS - DBH CLASS 2 (> 12 BY DEFAULT)
  !             10: SOFT SNAGS - DBH CLASS 3 (> 18 BY DEFAULT)
  !             11: SOFT SNAGS - DBH CLASS 4 (> 24 BY DEFAULT)
  !             12: SOFT SNAGS - DBH CLASS 5 (> 30 BY DEFAULT)
  !             13: SOFT SNAGS - DBH CLASS 6 (> 36 BY DEFAULT)
  !             14: SOFT SNAGS - TOTAL       (> 0)
  !             15: HARD+SOFT  - TOTAL       (> 0)
  !             16: KODE FOR WHETHER THE REPORT ALSO DUMPS TO FILE

  !     NOTE: THE DBH CLASS BREAKS CAN BE CHANGED BY THE SNAGCLAS KEYWORD

  ! Common blocks
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

  ! Variables
  integer :: IYEAR,KODE,iRet,ColNumber
  real    :: HCL1,HCL2,HCL3,HCL4,HCL5,HCL6,HCL7,SCL1,SCL2,SCL3,SCL4,SCL5,SCL6,SCL7,HDSF
  double precision :: HCL1B,HCL2B,HCL3B,HCL4B,HCL5B,HCL6B,HCL7B,SCL1B,SCL2B,SCL3B,SCL4B,SCL5B,SCL6B,&
       &SCL7B,HDSFB
  character(len=2000) :: SQLStmtStr
  character(len=26) :: NPLT

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize

  if (ISSUM.eq.0) return
  if (ISSUM.eq.2) KODE = 0

  call DBSCASE(1)

  iRet = fsql3_tableexists(IoutDBref,&
       &"FVS_SnagSum_Metric"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_SnagSum_Metric ('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year Int null,'//&
          &'Hard_snags_class1 real null,'//&
          &'Hard_snags_class2 real null,'//&
          &'Hard_snags_class3 real null,'//&
          &'Hard_snags_class4 real null,'//&
          &'Hard_snags_class5 real null,'//&
          &'Hard_snags_class6 real null,'//&
          &'Hard_snags_total  real null,'//&
          &'Soft_snags_class1 real null,'//&
          &'Soft_snags_class2 real null,'//&
          &'Soft_snags_class3 real null,'//&
          &'Soft_snags_class4 real null,'//&
          &'Soft_snags_class5 real null,'//&
          &'Soft_snags_class6 real null,'//&
          &'Soft_snags_total  real null,'//&
          &'Hard_soft_snags_total real null);'//CHAR(0)
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        ISSUM = 0
        return
     end if
  end if

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  HCL1B = HCL1
  HCL2B = HCL2
  HCL3B = HCL3
  HCL4B = HCL4
  HCL5B = HCL5
  HCL6B = HCL6
  HCL7B = HCL7
  SCL1B = SCL1
  SCL2B = SCL2
  SCL3B = SCL3
  SCL4B = SCL4
  SCL5B = SCL5
  SCL6B = SCL6
  SCL7B = SCL7
  HDSFB = HDSF

  write(SQLStmtStr,*)'INSERT INTO FVS_SnagSum_Metric (CaseID,',&
       &'StandID,Year,Hard_snags_class1,Hard_snags_class2,',&
       &'Hard_snags_class3,Hard_snags_class4,Hard_snags_class5,',&
       &'Hard_snags_class6,Hard_snags_total,Soft_snags_class1,',&
       &'Soft_snags_class2,Soft_snags_class3,Soft_snags_class4,',&
       &'Soft_snags_class5,Soft_snags_class6,Soft_snags_total,',&
       &'Hard_soft_snags_total) VALUES(''',CASEID,''',''',TRIM(NPLT),&
       &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)

  iRet = fsql3_prepare(IoutDBref, SQLStmtStr)

  if (iRet .ne.0) then
     ISSUM = 0
     return
  end if

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL1B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL2B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL3B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL4B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL5B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL6B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HCL7B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL1B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL2B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL3B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL4B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL5B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL6B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,SCL7B)

  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,HDSFB)

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     ISSUM = 0
  end if
  return

end
