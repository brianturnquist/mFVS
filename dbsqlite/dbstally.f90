!=========================!
!   dbsqlite : dbstally   !
!=========================!
! DBSQLITE $Id$
subroutine DBSTALLY(PRBSTK,NTALLY,CSP,SUMTPA,SUMPCT,BESTTPA,&
     &BESTPCT,AVEHT,PASTPA,PASPCT,IYEAR,WTAVEHT,TOT,TOTTTPAA,&
     &TOTTTPAB,TOTTTPA3)
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE REGENERATION TALLY
  !              SUMMARY BY SPECIES OF THE TREES PER ACRE, PERCENTAGE OF TOTAL,
  !              AND AVERAGE HEIGHT FOR ALL TREES, BEST TREES,
  !              AND TREES LESS THAN 3 INCHES IN DIAMETER THAT REGENERATED
  !              DURING A GIVEN TALLY.
  !     AUTH: M. SHETTLES -- FMSC -- AUGUST 2019

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DBSCOM.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: ColNumber,iret,NTALLY,SUMTPA1,SUMPCT1,BESTTPA1
  integer :: I,PASTPA1,PASPCT1,BESTPCT1,IYEAR,TOT,IYEAR1
  real    :: PRBSTK,AVEHT,WTAVEHT,TOTTTPAA,TOTTTPAB,TOTTTPA3
  real    :: SUMTPA,SUMPCT,BESTTPA,PASTPA,PASPCT,BESTPCT
  double precision :: PRBSTK1,AVEHT1
  character(len=2000) :: SQLStmtStr
  character(len=3) :: CSP
  character(len=8) :: CSP1
  character(len=8) :: CSP2
  character(len=8) :: CSP3

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_bind_text
  if (IREG4.ne.1) return

  call DBSCASE(1)

  if (PRBSTK.gt.1) goto 100

  !     DEFINE TABLENAME

  iRet = fsql3_tableexists(IoutDBref,'FVS_Regen_Tally'//CHAR(0))

  !       TALLY SUMMARY

  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Regen_Tally('//&
          &'CaseID          text not null,'//&
          &'StandID         text not null,'//&
          &'Year            int  null,'//&
          &'ProbStock       real null,'//&
          &'TallyNumber     int  null,'//&
          &'SpeciesFVS      text null,'//&
          &'SpeciesPLANTS   text null,'//&
          &'SpeciesFIA      text null,'//&
          &'TpaAll          int  null,'//&
          &'PctOfTotalAll   int  null,'//&
          &'TpaBest         int  null,'//&
          &'PctOfTotalBest  int  null,'//&
          &'AverageHt       real null,'//&
          &'TpaSmall        int  null,'//&
          &'PctOfTotalSmall int  null);'//CHAR(0)

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IREG4 = 0
        return
     end if
  end if

  write(SQLStmtStr,*)'INSERT INTO FVS_Regen_Tally',&
       &' (CaseID,StandID,Year,ProbStock,TallyNumber,',&
       &'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
       &'TpaAll,PctOfTotalAll,TpaBest,',&
       &'PctOfTotalBest,AverageHt,TpaSmall,PctOfTotalSmall) ',&
       &'VALUES (''',CASEID,''',''',TRIM(NPLT),''',?,?,?,?,',&
       &'?,?,?,?,?,?,?,?,?);'

  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IREG4 = 0
     return
  end if

  !     ASSIGN FVS, PLANTS AND FIA SPECIES CODES

  if (CSP .eq. "ALL") then
     CSP1 = "ALL"
     CSP2 = "ALL"
     CSP3 = "ALL"
  else
     do I = 1,MAXSP
        if (CSP(1:2) .eq. JSP(I)) then
           CSP1 = JSP(I)
           CSP2 = PLNJSP(I)
           CSP3 = FIAJSP(I)
        end if
     end do
  end if

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  PRBSTK1 = PRBSTK
  if (TOT.eq.1) then
     AVEHT1 = AVEHT
  else
     AVEHT1 = WTAVEHT
  end if

  !     ASSIGN INTEGER VALUES TO REAL VARS

  if (TOT.eq.1) then
     SUMTPA1 = NINT(SUMTPA)
     BESTTPA1 = NINT(BESTTPA)
     PASTPA1 = NINT(PASTPA)
     PASPCT1 = NINT(PASPCT)
     BESTPCT1 = NINT(BESTPCT)
     SUMPCT1 = NINT(SUMPCT)
  else
     SUMTPA1 = NINT(TOTTTPAA)
     BESTTPA1 = NINT(TOTTTPAB)
     PASTPA1 = NINT(TOTTTPA3)
     PASPCT1 = 100
     BESTPCT1 = 100
     SUMPCT1 = 100
  end if
  IYEAR1 = IYEAR+1

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,PRBSTK1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,NTALLY)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1,&
       &LEN_TRIM(CSP1))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2,&
       &LEN_TRIM(CSP2))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3,&
       &LEN_TRIM(CSP3))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMTPA1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMPCT1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTTPA1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTPCT1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,AVEHT1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PASTPA1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PASPCT1)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     IREG4 = 0
  end if
  return
  100 continue

  !     DEFINE TABLENAME

  iRet = fsql3_tableexists(IoutDBref,'FVS_Regen_Tally'//CHAR(0))

  !      TALLY SUMMARY

  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_Regen_Tally('//&
          &'CaseID          text not null,'//&
          &'StandID         text not null,'//&
          &'Year            int  null,'//&
          &'TallyNumber     int  null,'//&
          &'SpeciesFVS      text null,'//&
          &'SpeciesPLANTS   text null,'//&
          &'SpeciesFIA      text null,'//&
          &'TpaAll          int  null,'//&
          &'PctOfTotalAll   int  null,'//&
          &'TpaBest         int  null,'//&
          &'PctOfTotalBest  int  null,'//&
          &'AverageHt       real null,'//&
          &'TpaSmall        int  null,'//&
          &'PctOfTotalSmall int  null);'//CHAR(0)

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IREG4 = 0
        return
     end if
  end if

  !     ASSIGN FVS, PLANTS AND FIA SPECIES CODES

  if (CSP .eq. "ALL") then
     CSP1 = "ALL"
     CSP2 = "ALL"
     CSP3 = "ALL"
  else
     do I = 1,MAXSP
        if (CSP(1:2) .eq. JSP(I)) then
           CSP1 = JSP(I)
           CSP2 = PLNJSP(I)
           CSP3 = FIAJSP(I)
        end if
     end do
  end if

  write(SQLStmtStr,*)'INSERT INTO FVS_Regen_Tally',&
       &' (CaseID,StandID,Year,TallyNumber,',&
       &'SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
       &'TpaAll,PctOfTotalAll,TpaBest,',&
       &'PctOfTotalBest,AverageHt,TpaSmall,PctOfTotalSmall) ',&
       &'VALUES (''',CASEID,''',''',TRIM(NPLT),''',?,?,?,',&
       &'?,?,?,?,?,?,?,?,?);'

  iRet = fsql3_prepare(IoutDBref,trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IREG4 = 0
     return
  end if

  !     ASSIGN REAL VALUES TO DOUBLE PRECISION VARS

  if (TOT.eq.1) then
     AVEHT1 = AVEHT
  else
     AVEHT1 = WTAVEHT
  end if

  !     ASSIGN INTEGER VALUES TO REAL VARS

  if (TOT.eq.1) then
     SUMTPA1 = NINT(SUMTPA)
     BESTTPA1 = NINT(BESTTPA)
     PASTPA1 = NINT(PASTPA)
     PASPCT1 = NINT(PASPCT)
     BESTPCT1 = NINT(BESTPCT)
     SUMPCT1 = NINT(SUMPCT)
  else
     SUMTPA1 = NINT(TOTTTPAA)
     BESTTPA1 = NINT(TOTTTPAB)
     PASTPA1 = NINT(TOTTTPA3)
     PASPCT1 = 100
     BESTPCT1 = 100
     SUMPCT1 = 100
  end if
  IYEAR1 = IYEAR+1

  ColNumber = 1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,NTALLY)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1,&
       &LEN_TRIM(CSP1))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2,&
       &LEN_TRIM(CSP2))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3,&
       &LEN_TRIM(CSP3))
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMTPA1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,SUMPCT1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTTPA1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,BESTPCT1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,AVEHT1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PASTPA1)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,PASPCT1)
  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (iRet.ne.0) then
     IREG4 = 0
  end if

end
