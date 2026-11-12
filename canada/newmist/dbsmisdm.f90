!=============================================!
!   canada/newmist : dbsmisdm                 !
!=============================================!
! CANADA-NEWMIST $Id$
!   DATABASE OUTPUT FOR THE CANADIAN (DM) DWARF MISTLETOE MODEL TABLES
!   (MOVED FROM metric/dbsqlite/dbsmis.f90; BC ONLY)


subroutine DBSMIS5(IYEAR,KODE)
  implicit none

  !     PURPOSE: TO OUTPUT THE TREELIST DM DATA TO THE DATABASE

  !     AUTH: D. ROBINSON, WITH INSPIRATION FROM
  !           D. GAMMEL -- SEM -- JULY 2002

  !            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
  !                     REDIRECT OF THE FLAT FILE REPORT OR IN
  !                     ADDITION TO

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESTREE.f90'
  include 'VARCOM.f90'
  include 'WORKCM.f90'
  include 'MISCOM.f90'
  include 'DMCOM.f90'
  include 'DBSCOM.f90'
  include 'METRIC.f90'

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
  integer :: IYEAR,KODE,iRet

  !     LOCAL VARIABLES

  integer :: fsql3_tableexists,fsql3_exec,fsql3_bind_int,fsql3_prepare,fsql3_bind_double,&
       &fsql3_finalize,fsql3_step,fsql3_reset,fsql3_bind_text

  integer :: ColNumber,I,J,K,L,I1,I2,I3,ISPC
  integer :: IDCMP1,IDCMP2
  data    IDCMP1,IDCMP2/10000000,20000000/

  double precision :: PB,DBHB,HTB,CWB
  double precision :: BRKPNTB(BPCNT),NEWSPRB(3),NEWINTB(3)
  double precision :: DMINFB(CRTHRD,DEAD_BC),DMINFB_BC(CRTHRD,ACTIVE)

  character(len=8) :: TID
  character(len=8) :: CSPECIE1
  character(len=1000) :: SQLStmtStr

  !     INITIALIZE VARIABLES

  if (IDM5.eq.0) return
  if (IDM5.eq.2) KODE = 0

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  iRet = fsql3_tableexists(IoutDBref,&
       &"FVS_DM_TreeList_Metric"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmtStr = 'CREATE TABLE FVS_DM_TreeList_Metric ('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year int null,'//&
          &'TreeID text null,'//&
          &'TreeIndex int null,'//&
          &'SpeciesFVS text null,'//&
          &'TPH real null,'//&
          &'DBH real null,'//&
          &'Ht real null,'//&
          &'CrWidth real null,'//&
          &'BrkPnt1 real null,'//&
          &'BrkPnt2 real null,'//&
          &'BrkPnt3 real null,'//&
          &'BrkPnt4 real null,'//&
          &'DMR int null,'//&
          &'NewSpr1 real null,'//&
          &'NewSpr2 real null,'//&
          &'NewSpr3 real null,'//&
          &'NewInt1 real null,'//&
          &'NewInt2 real null,'//&
          &'NewInt3 real null,'//&
          &'Immtr1 real null,'//&
          &'Immtr2 real null,'//&
          &'Immtr3 real null,'//&
          &'Latnt1 real null,'//&
          &'Latnt2 real null,'//&
          &'Latnt3 real null,'//&
          &'Suppr1 real null,'//&
          &'Suppr2 real null,'//&
          &'Suppr3 real null,'//&
          &'Actv1 real null,'//&
          &'Actv2 real null,'//&
          &'Actv3 real null,'//&
          &'BCImmtr1 real null,'//&
          &'BCImmtr2 real null,'//&
          &'BCImmtr3 real null,'//&
          &'BCLatnt1 real null,'//&
          &'BCLatnt2 real null,'//&
          &'BCLatnt3 real null,'//&
          &'BCSuppr1 real null,'//&
          &'BCSuppr2 real null,'//&
          &'BCSuppr3 real null,'//&
          &'BCActv1 real null,'//&
          &'BCActv2 real null,'//&
          &'BCActv3 real null,'//&
          &'BCKill1 real null,'//&
          &'BCKill2 real null,'//&
          &'BCKill3 real null);'//CHAR(0)

     iRet = fsql3_exec(IoutDBref,SQLStmtStr)
     if (iRet .ne.0) then
        IDM5 = 0
        iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
        return
     end if
  end if

  write(SQLStmtStr,*) 'INSERT INTO FVS_DM_TreeList_Metric ',&
       &'(CaseID,StandID,Year,',&
       &'TreeID,TreeIndex,SpeciesFVS,TPH,DBH,Ht,CrWidth,',&
       &'BrkPnt1,BrkPnt2,BrkPnt3,BrkPnt4,',&
       &'DMR,',&
       &'NewSpr1,NewSpr2,NewSpr3,',&
       &'NewInt1,NewInt2,NewInt3,',&
       &'Immtr1,Immtr2,Immtr3,',&
       &'Latnt1,Latnt2,Latnt3,',&
       &'Suppr1,Suppr2,Suppr3,',&
       &'Actv1,Actv2,Actv3,',&
       &'BCImmtr1,BCImmtr2,BCImmtr3,',&
       &'BCLatnt1,BCLatnt2,BCLatnt3,',&
       &'BCSuppr1,BCSuppr2,BCSuppr3,',&
       &'BCActv1,BCActv2,BCActv3,',&
       &'BCKill1,BCKill2,BCKill3) ',&
       &'VALUES (''',CASEID,''',''',TRIM(NPLT),''',',&
       &'?,?,?,?,',&
       &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,',&
       &'?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,',&
       &'?,?,?,?,?,?,?,?,?,?,?,?);'

  iRet = fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))
  if (iRet .ne.0) then
     IDM5 = 0
     iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
     return
  end if

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (MISFIT(ISPC) .ne.0.and. I1.ne.0) then
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           I = IND1(I3)

           PB = (PROB(I) / GROSPC)/ACRtoHA

           !           TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN
           !           COMPRESSED OR GENERATED THROUGH THE ESTAB SYSTEM.

           if (IDTREE(I) .gt. IDCMP1) then
              if (IDTREE(I) .gt. IDCMP2) then
                 write(TID,'(''CM'',I6.6)') IDTREE(I) - IDCMP2
              else
                 write(TID,'(''ES'',I6.6)') IDTREE(I) - IDCMP1
              end if
           else
              write(TID,'(I8)') IDTREE(I)
           end if

           !           DOUBLE PRECISION COPIES OF SINGLE PRECISION INPUTS
           !           CONVERT BREAKPOINTS FROM MESH-UNITS TO METERS

           do J = 1,BPCNT
              BRKPNTB(J) = BRKPNT(I,J) * real(MESH)
           end do
           do J = 1,CRTHRD
              NEWSPRB(J) = NEWSPR(I,J)
              NEWINTB(J) = NEWINT(I,J)
           end do

           do J = 1,CRTHRD
              do K = 1,DEAD_BC
                 DMINFB(J,K) = DMINF(I,J,K)
              end do
              do K = 1,ACTIVE
                 DMINFB_BC(J,K) = 0.0
                 do L = 1,MAXBC
                    DMINFB_BC(J,K) = DMINFB_BC(J,K) + DMINF_BC(I,J,K,L)
                 end do
              end do
           end do

           iRet = fsql3_prepare(IoutDBref, trim(SQLStmtStr)//CHAR(0))

           !           LOAD SPECIES CODES FROM FVS, PLANTS AND FIA ARRAYS.

           CSPECIE1 = JSP(ISP(I))

           !           BIND SQL STATEMENT PARAMETERS TO FORTRAN VARIABLES

           ColNumber = 1 ! Year
           iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)

           ColNumber = ColNumber+1 ! TreeID
           iRet = fsql3_bind_text(IoutDBref,ColNumber,TID,&
                &LEN_TRIM(TID))

           ColNumber = ColNumber+1 ! TreeIndex
           iRet = fsql3_bind_int(IoutDBref,ColNumber,I)

           ColNumber = ColNumber+1 ! FVSSpecies
           iRet = fsql3_bind_text(IoutDBref,ColNumber,CSPECIE1,&
                &LEN_TRIM(CSPECIE1))

           ColNumber = ColNumber+1 ! TPH
           iRet = fsql3_bind_double(IoutDBref,ColNumber,PB)

           ColNumber = ColNumber+1 ! DBH
           DBHB = DBH(I)*INtoCM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,DBHB)

           ColNumber = ColNumber+1 ! Ht
           HTB = HT(I)*FTtoM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,HTB)

           ColNumber = ColNumber+1 ! CrWidth
           CWB = CRWDTH(I)*FTtoM
           iRet = fsql3_bind_double(IoutDBref,ColNumber,CWB)

           do J = 1,BPCNT
              ColNumber = ColNumber+1 ! BRKPNT
              iRet = fsql3_bind_double(IoutDBref,ColNumber,BRKPNTB(J))
           end do

           ColNumber = ColNumber+1 ! DMR
           iRet = fsql3_bind_int(IoutDBref,ColNumber,DMRATE(I))

           do J = 1,CRTHRD
              ColNumber = ColNumber+1 ! NEWSPR
              iRet = fsql3_bind_double(IoutDBref,ColNumber,NEWSPRB(J))
           end do

           do J = 1,CRTHRD
              ColNumber = ColNumber+1 ! NEWINT
              iRet = fsql3_bind_double(IoutDBref,ColNumber,NEWINTB(J))
           end do

           do K = 1,ACTIVE
              do J = 1,CRTHRD
                 ColNumber = ColNumber+1 ! DMINF
                 iRet = fsql3_bind_double(IoutDBref,ColNumber,&
                      &DMINFB(J,K))
              end do
           end do

           do K = 1,ACTIVE
              do J = 1,CRTHRD
                 ColNumber = ColNumber+1 ! DMINF_BC
                 iRet = fsql3_bind_double(IoutDBref,ColNumber,&
                      &DMINFB_BC(J,K))
              end do
           end do

           K = DEAD_BC
           do J = 1,CRTHRD
              ColNumber = ColNumber+1 ! DMINF-DEAD
              iRet = fsql3_bind_double(IoutDBref,ColNumber,&
                   &DMINFB_BC(J,K))
           end do

           iRet = fsql3_step(IoutDBref)
           iRet = fsql3_reset(IoutDBref)
        end do
     end if
  end do

  iRet = fsql3_finalize(IoutDBref)
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))

  if (iRet.ne.0) then
     IDM5 = 0
  end if

  return
end

subroutine DBSMIS6(IYEAR,KODE)
  implicit none

  ! Common blocks
  include 'DBSCOM.f90'

  ! Variables
  integer :: IYEAR,KODE

  IDM6 = 0

  !     code for 6 is here:
  !     https://sourceforge.net/p/open-fvs/code/HEAD/tree/branches/ESSAdev/newmist/src/dbsmis.f

  return
end
