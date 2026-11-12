!=======================!
!   dbsqlite : dbsmis   !
!=======================!
! DBSQLITE $Id$
subroutine DBSMIS1(IYEAR,CNPLT,CSP,&
     &SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4,&
     &KODE)

  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE 1ST MISTLETOE REPORT INFORMATION
  !     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

  !     1:     Year
  !     2:     Stand ID
  !     3:     Species
  !     4:     Mean DMR for species - SPDMR4
  !     5:     Mean DMI for species - SPDMI4
  !     6:     Infected trees/acre for species - SPINF4
  !     7:     Mortality trees/acre for species - SPMRT4
  !     8:     Infected trees/acre % for species - SPPIN4
  !     9:     Mortality trees/acre % for species - SPPMR4
  !    10:     Stand trees/acre % for species - SPPOC4

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'DBSCOM.f90'

  !     ARGUMENT LIST

  ! Variables
  integer :: IYEAR
  real    :: SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4)
  real    :: SPPIN4(4),SPPMR4(4),SPPOC4(4)
  integer :: KODE,iRet

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_step,fsql3_reset,fsql3_bind_text

  integer :: ColNumber,I,I2

  double precision :: SPDMR4B,SPDMI4B
  integer :: ISPINF4,ISPMRT4,ISPPIN4,ISPPMR4,ISPPOC4

  character(len=2000) :: SQLStmtStr
  character(LEN = 26) :: CNPLT
  character(LEN = 2) :: CSP(4)
  character(LEN = 8) :: CSP1
  character(LEN = 8) :: CSP2
  character(LEN = 8) :: CSP3

  !     INITIALIZE VARIABLES

  if (IDM1.eq.0) return
  if (IDM1.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  if (LMETRICVAR) then
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_DM_Spp_Sum_Metric"//CHAR(0))
  else
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_DM_Spp_Sum"//CHAR(0))
  end if
  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Spp_Sum_Metric ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year Int null,'//&
             &'SpeciesFVS text null,'//&
             &'SpeciesPLANTS text null,'//&
             &'SpeciesFIA text null,'//&
             &'Mean_DMR real null,'//&
             &'Mean_DMI real null,'//&
             &'Inf_TPH int null,'//&
             &'Mort_TPH int  null,'//&
             &'Inf_TPH_Pct int null,'//&
             &'Mort_TPH_Pct int null,'//&
             &'Stnd_TPH_Pct int null);'//CHAR(0)
     else
        SQLStmtStr = 'CREATE TABLE FVS_DM_Spp_Sum ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year Int null,'//&
             &'SpeciesFVS text null,'//&
             &'SpeciesPLANTS text null,'//&
             &'SpeciesFIA text null,'//&
             &'Mean_DMR real null,'//&
             &'Mean_DMI real null,'//&
             &'Inf_TPA int null,'//&
             &'Mort_TPA int  null,'//&
             &'Inf_TPA_Pct int null,'//&
             &'Mort_TPA_Pct int null,'//&
             &'Stnd_TPA_Pct int null);'//CHAR(0)
     end if
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IDM1 = 0
        return
     end if
  end if

  if (LMETRICVAR) then
     write(SQLStmtStr,*) 'INSERT INTO FVS_DM_Spp_Sum_Metric',&
          &' (CaseID,StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
          &'Mean_DMR,Mean_DMI,Inf_TPH,',&
          &'Mort_TPH,Inf_TPH_Pct,Mort_TPH_Pct,Stnd_TPH_Pct) ',&
          &' VALUES (''',CASEID,''',''',TRIM(CNPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?);'
  else
     write(SQLStmtStr,*) 'INSERT INTO FVS_DM_Spp_Sum ',&
          &' (CaseID,StandID,Year,SpeciesFVS,SpeciesPLANTS,SpeciesFIA,',&
          &'Mean_DMR,Mean_DMI,Inf_TPA,',&
          &'Mort_TPA,Inf_TPA_Pct,Mort_TPA_Pct,Stnd_TPA_Pct) ',&
          &' VALUES (''',CASEID,''',''',TRIM(CNPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?);'
  end if
  iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
  iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IDM1 = 0
     return
  end if

  !     LOOP OVER 4 TOP SPECIES

  do I = 1,4

     !       IF THERE ARE LESS THAN 4 SPECIES INFECTED IN THE STAND,
     !       DO NOT WRITE THE BLANK RECORDS TO THE DATABASE.

     if (CSP(I) .eq. '**') cycle

     !     ASSIGN FVS, PLANTS AND FIA SPECIES CODES

     do I2 = 1,MAXSP
        if (CSP(I) .eq. JSP(I2)) then
           CSP1 = JSP(I2)
           CSP2 = PLNJSP(I2)
           CSP3 = FIAJSP(I2)
        end if
     end do

     !       DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS AND
     !       INTEGER COPIES OF REAL VECTOR INPUTS

     SPDMR4B = SPDMR4(I)
     SPDMI4B = SPDMI4(I)

     ISPINF4 = NINT(SPINF4(I))
     ISPMRT4 = NINT(SPMRT4(I))
     ISPPIN4 = NINT(SPPIN4(I))
     ISPPMR4 = NINT(SPPMR4(I))
     ISPPOC4 = NINT(SPPOC4(I))

     !       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

     ColNumber = 1 ! 1 YEAR
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

     ColNumber = ColNumber+1 ! SpeciesFVS
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP1,&
          &LEN_TRIM(CSP1))

     ColNumber = ColNumber+1 ! SpeciesPLANTS
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP2,&
          &LEN_TRIM(CSP2))

     ColNumber = ColNumber+1 ! SpeciesFIA
     iRet = fsql3_bind_text(IoutDBref,ColNumber,CSP3,&
          &LEN_TRIM(CSP3))

     ColNumber = ColNumber+1 ! MEAN DMR
     iRet = fsql3_bind_double(IoutDBref,ColNumber,SPDMR4B)

     ColNumber = ColNumber+1 ! MEAN DMI
     iRet = fsql3_bind_double(IoutDBref,ColNumber,SPDMI4B)

     ColNumber = ColNumber+1 ! TPA INFECTED
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPINF4)

     ColNumber = ColNumber+1 ! TPA MORTALITY
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPMRT4)

     ColNumber = ColNumber+1 ! % TPA INFECTED
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPPIN4)

     ColNumber = ColNumber+1 ! % TPA MORTALITY
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPPMR4)

     ColNumber = ColNumber+1 ! % STAND TPA
     iRet = fsql3_bind_int(IoutDBref,ColNumber,ISPPOC4)

     iRet = fsql3_step(IoutDBref)
     iRet = fsql3_reset(IoutDBref)
  end do

  if (LMETRICVAR) then
     iRet = fsql3_finalize(IoutDBref)
  end if

  iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
  if (.not. LMETRICVAR) then
     iRet = fsql3_finalize(IoutDBref)
  end if
  if (iRet.ne.0) then
     IDM1 = 0
     return
  end if

  return
end

subroutine DBSMIS2(IYEAR,NPLT,NAGE,&
     &ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM,&
     &ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV,STDMR,STDMI,KODE)

  implicit none
  include 'PRGPRM.f90'

  !     PURPOSE: TO POPULATE A DATABASE WITH THE 2ND MISTLETOE REPORT INFORMATION
  !     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

  !     1:     Year - IYEAR
  !     2:     Age - NAGE
  !     3:     Stand trees/acre - ISTTPAT
  !     4:     Stand basal area - IBA
  !     5:     Stand volume - ISTVOL
  !     6:     Infected trees/acre - ISTTPAI
  !     7:     Infected basal area - ISTBAI
  !     8:     Infected volume - ISTVOLI
  !     9:     Mortality trees/acre - ISTTPAM
  !     10:    Mortality basal area - ISTBAM
  !     11:    Mortality volume - ISTVOLM
  !     12:    Infected trees/acre % - ISTPIT
  !     13:    Infected volume % - ISTPIV
  !     14:    Mortality trees/acre % - ISTPMT
  !     15:    Mortality volume % - ISTPMV
  !     16:    Mean DMR - STDMR
  !     17:    Mean DMI - STDMI

  ! Common blocks
  include 'DBSCOM.f90'

  !     ARGUMENT LIST

  ! Variables
  integer :: IYEAR,NAGE
  character(LEN = 26) :: NPLT
  integer :: ISTTPAT,IBA,ISTVOL,ISTTPAI,ISTBAI,ISTVOLI,ISTTPAM,ISTBAM
  integer :: ISTVOLM,ISTPIT,ISTPIV,ISTPMT,ISTPMV
  real    :: STDMR,STDMI
  integer :: KODE,iRet

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_step,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize

  !     LOCAL VARIABLES

  integer :: ColNumber

  double precision :: STDMRB,STDMIB

  character(len=1000) :: SQLStmtStr

  !     INITIALIZE VARIABLES

  if (IDM2.eq.0) return
  if (IDM2.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  if (LMETRICVAR) then
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_DM_Stnd_Sum_Metric"//CHAR(0))
  else
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_DM_Stnd_Sum"//CHAR(0))
  end if
  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Stnd_Sum_Metric ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int null,'//&
             &'Age int null,'//&
             &'Stnd_TPH int null,'//&
             &'Stnd_BA int null,'//&
             &'Stnd_Vol int null,'//&
             &'Inf_TPH int null,'//&
             &'Inf_BA int null,'//&
             &'Inf_Vol int null,'//&
             &'Mort_TPH int null,'//&
             &'Mort_BA int null,'//&
             &'Mort_Vol int null,'//&
             &'Inf_TPH_Pct int null,'//&
             &'Inf_Vol_Pct int null,'//&
             &'Mort_TPH_Pct int null,'//&
             &'Mort_Vol_Pct int null,'//&
             &'Mean_DMR real null,'//&
             &'Mean_DMI real null);'//CHAR(0)
     else
        SQLStmtStr = 'CREATE TABLE FVS_DM_Stnd_Sum ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int null,'//&
             &'Age int null,'//&
             &'Stnd_TPA int null,'//&
             &'Stnd_BA int null,'//&
             &'Stnd_Vol int null,'//&
             &'Inf_TPA int null,'//&
             &'Inf_BA int null,'//&
             &'Inf_Vol int null,'//&
             &'Mort_TPA int null,'//&
             &'Mort_BA int null,'//&
             &'Mort_Vol int null,'//&
             &'Inf_TPA_Pct int null,'//&
             &'Inf_Vol_Pct int null,'//&
             &'Mort_TPA_Pct int null,'//&
             &'Mort_Vol_Pct int null,'//&
             &'Mean_DMR real null,'//&
             &'Mean_DMI real null);'//CHAR(0)
     end if
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IDM2 = 0
        return
     end if
  end if

  !     DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

  STDMRB = STDMR
  STDMIB = STDMI

  if (LMETRICVAR) then
     write(SQLStmtStr,*)'INSERT INTO FVS_DM_Stnd_Sum_Metric(CaseID,',&
          &'StandID,Year,Age,Stnd_TPH,Stnd_BA,Stnd_Vol,Inf_TPH,Inf_BA,',&
          &'Inf_Vol,Mort_TPH,Mort_BA,Mort_Vol,Inf_TPH_Pct,Inf_Vol_Pct,',&
          &'Mort_TPH_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI)',&
          &'VALUES (''',CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
  else
     write(SQLStmtStr,*) 'INSERT INTO FVS_DM_Stnd_Sum (CaseID,',&
          &'StandID,Year,Age,Stnd_TPA,Stnd_BA,Stnd_Vol,Inf_TPA,Inf_BA,',&
          &'Inf_Vol,Mort_TPA,Mort_BA,Mort_Vol,Inf_TPA_Pct,Inf_Vol_Pct,',&
          &'Mort_TPA_Pct,Mort_Vol_Pct,Mean_DMR,Mean_DMI)',&
          &'VALUES (''',CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?);'
  end if

  iRet = fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IDM2 = 0
     return
  end if

  !     BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

  ColNumber = 1 ! YEAR
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

  ColNumber = ColNumber+1 ! AGE
  iRet = fsql3_bind_int(IoutDBref,ColNumber,NAGE)

  ColNumber = ColNumber+1 ! STAND TPA
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTTPAT)

  ColNumber = ColNumber+1 ! STAND BA
  iRet = fsql3_bind_int(IoutDBref,ColNumber,IBA)

  ColNumber = ColNumber+1 ! STAND VOL
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTVOL)

  ColNumber = ColNumber+1 ! INF TPA
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTTPAI)

  ColNumber = ColNumber+1 ! INF BA
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTBAI)

  ColNumber = ColNumber+1 ! INF VOL
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTVOLI)

  ColNumber = ColNumber+1 ! MORT TPA
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTTPAM)

  ColNumber = ColNumber+1 ! MORT BA
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTBAM)

  ColNumber = ColNumber+1 ! MORT VOL
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTVOLM)

  ColNumber = ColNumber+1 ! INF TPA %
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTPIT)

  ColNumber = ColNumber+1 ! INF VOL %
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTPIV)

  ColNumber = ColNumber+1 ! 16 MORT TPA %
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTPMT)

  ColNumber = ColNumber+1 ! MORT VOL %
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ISTPMV)

  ColNumber = ColNumber+1 ! MEAN DMR
  iRet = fsql3_bind_double(IoutDBref,ColNumber,STDMRB)

  ColNumber = ColNumber+1 ! MEAN DMI
  iRet = fsql3_bind_double(IoutDBref,ColNumber,STDMIB)

  iRet = fsql3_step(IoutDBref)
  iRet = fsql3_finalize(IoutDBref)
  if (LMETRICVAR) then
     iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
  end if
  if (iRet.ne.0) then
     IDM2 = 0
  end if
  if (.not. LMETRICVAR) then
     return
  end if

  return
end

subroutine DBSMIS3(IYEAR,NPLT,NLABS,&
     &DCTPA,DCINF,DCMRT,DCDMR,DCDMI,KODE)

  implicit none
  include 'PRGPRM.f90'

  !     PURPOSE: TO POPULATE A DATABASE WITH THE 3RD MISTLETOE REPORT INFORMATION
  !     AUTH: D. ROBINSON, ESSA - BASED ON D. GAMMEL (DBSFUELS)

  !     1:     Year - IYEAR
  !     2:     Age - NAGE
  !     3:     Stand trees/acre - ISTTPAT
  !     4:     Stand basal area - IBA
  !     5:     Stand volume - ISTVOL
  !     6:     Infected trees/acre - ISTTPAI
  !     7:     Infected basal area - ISTBAI
  !     8:     Infected volume - ISTVOLI
  !     9:     Mortality trees/acre - ISTTPAM
  !     10:    Mortality basal area - ISTBAM
  !     11:    Mortality volume - ISTVOLM
  !     12:    Infected trees/acre % - ISTPIT
  !     13:    Infected volume % - ISTPIV
  !     14:    Mortality trees/acre % - ISTPMT
  !     15:    Mortality volume % - ISTPMV
  !     16:    Mean DMR - STDMR
  !     17:    Mean DMI - STDMI

  ! Common blocks
  include 'DBSCOM.f90'

  !     ARGUMENT LIST

  ! Variables
  integer :: IYEAR
  character(LEN = 26) :: NPLT
  character(LEN = 3) :: NLABS(5)
  real    :: DCTPA(10),DCINF(10),DCMRT(10),DCDMR(10),DCDMI(10)
  integer :: KODE,iRet

  !     LOCAL VARIABLES

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_step,fsql3_reset,fsql3_bind_text

  integer :: ColNumber,I,J

  double precision :: X(10)

  character(len=1000) :: SQLStmtStr

  !     INITIALIZE VARIABLES

  if (IDM3.eq.0) return
  if (IDM3.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  if (LMETRICVAR) then
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_DM_Sz_Sum_Metric"//CHAR(0))
  else
     iRet = fsql3_tableexists(IoutDBref,&
          &"FVS_DM_Sz_Sum"//CHAR(0))
  end if
  if (iRet.eq.0) then
     if (LMETRICVAR) then
        SQLStmtStr = 'CREATE TABLE FVS_DM_Sz_Sum_Metric ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int null,'//&
             &'Type text null,'//&
             &char(39)//'0-5cm'   //char(39)//' real null,'//&
             &char(39)//'5-10cm'  //char(39)//' real null,'//&
             &char(39)//'10-15cm' //char(39)//' real null,'//&
             &char(39)//'15-20cm' //char(39)//' real null,'//&
             &char(39)//'20-25cm' //char(39)//' real null,'//&
             &char(39)//'25-30cm' //char(39)//' real null,'//&
             &char(39)//'30-35cm' //char(39)//' real null,'//&
             &char(39)//'35-40cm' //char(39)//' real null,'//&
             &char(39)//'40-45cm' //char(39)//' real null,'//&
             &char(39)//'gt45cm'  //char(39)//' real null);'//CHAR(0)
     else
        SQLStmtStr = 'CREATE TABLE FVS_DM_Sz_Sum ('//&
             &'CaseID text not null,'//&
             &'StandID text not null,'//&
             &'Year int null,'//&
             &'Type text not null,'//&
             &char(39)//'0-3in'   //char(39)//' real null,'//&
             &char(39)//'3-5in'  //char(39)//' real null,'//&
             &char(39)//'5-7in' //char(39)//' real null,'//&
             &char(39)//'7-9in' //char(39)//' real null,'//&
             &char(39)//'9-11in' //char(39)//' real null,'//&
             &char(39)//'11-13in' //char(39)//' real null,'//&
             &char(39)//'13-15in' //char(39)//' real null,'//&
             &char(39)//'15-17in' //char(39)//' real null,'//&
             &char(39)//'17-19in' //char(39)//' real null,'//&
             &char(39)//'gt19in'  //char(39)//' real null);'//CHAR(0)
     end if
     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IDM3 = 0
        return
     end if
  end if

  if (LMETRICVAR) then
     write(SQLStmtStr,*) 'INSERT INTO FVS_DM_Sz_Sum_Metric',&
          &' (CaseID,StandID,Year,Type,',&
          &char(39),'0-5cm',   char(39),',',&
          &char(39),'5-10cm',  char(39),',',&
          &char(39),'10-15cm', char(39),',',&
          &char(39),'15-20cm', char(39),',',&
          &char(39),'20-25cm', char(39),',',&
          &char(39),'25-30cm', char(39),',',&
          &char(39),'30-35cm', char(39),',',&
          &char(39),'35-40cm', char(39),',',&
          &char(39),'40-45cm', char(39),',',&
          &char(39),'gt45cm',  char(39),') VALUES ',&
          &'(''',CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
  else
     write(SQLStmtStr,*) 'INSERT INTO FVS_DM_Sz_Sum',&
          &' (CaseID,StandID,Year,Type,',&
          &char(39),'0-3in',  char(39),',',&
          &char(39),'3-5in', char(39),',',&
          &char(39),'5-7in',char(39),',',&
          &char(39),'7-9in',char(39),',',&
          &char(39),'9-11in',char(39),',',&
          &char(39),'11-13in',char(39),',',&
          &char(39),'13-15in',char(39),',',&
          &char(39),'15-17in',char(39),',',&
          &char(39),'17-19in',char(39),',',&
          &char(39),'gt19in', char(39),') VALUES ',&
          &'(''',CASEID,''',''',TRIM(NPLT),&
          &''',?,?,?,?,?,?,?,?,?,?,?,?);'//CHAR(0)
  end if
  iRet = fsql3_exec(IoutDBref,"Begin;"//CHAR(0))
  iRet = fsql3_prepare(IoutDBref, TRIM(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IDM3 = 0
     return
  end if

  !     LOOP OVER 5 TYPES

  do I = 1,5

     !     LOOP OVER 5 TYPES, MAKING DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS

     select case (I)
     case (1)
        do J = 1,10
           X(J) = DCTPA(J)
        end do
     case (2)
        do J = 1,10
           X(J) = DCINF(J)
        end do
     case (3)
        do J = 1,10
           X(J) = DCMRT(J)
        end do
     case (4)
        do J = 1,10
           X(J) = DCDMR(J)
        end do
     case (5)
        do J = 1,10
           X(J) = DCDMI(J)
        end do
     end select

     iRet = fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))

     !       BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

     ColNumber = 1 ! 1 YEAR
     iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

     ColNumber = ColNumber+1 ! Label
     iRet = fsql3_bind_text(IoutDBref,ColNumber,NLABS(I),&
          &len_trim(NLABS(I)))

     do J = 1,10
        ColNumber = ColNumber+1 ! SIZE CLASSES 2-11
        iRet = fsql3_bind_double(IoutDBref,ColNumber,X(J))
     end do

     iRet = fsql3_step(IoutDBref)
     iRet = fsql3_reset(IoutDBref)

  end do

  if (LMETRICVAR) then
     iRet = fsql3_finalize(IoutDBref)
  end if

  iRet = fsql3_exec(IoutDBref,"Commit;"//CHAR(0))
  if (.not. LMETRICVAR) then
     iRet = fsql3_finalize(IoutDBref)
  end if
  if (iRet.ne.0) then
     IDM3 = 0
     if (.not. LMETRICVAR) then
        return
     end if
  end if

  return
end
