!==================================!
!   metric/dbsqlite : dbstreesin   !
!==================================!
! METRIC-DBSQLITE $Id$
subroutine DBSTREESIN(IPLOT,ITREE,RCOUNT,HISTORY,SPECIES,DBH,DG,&
     &HT,HTTOPK,HTG,CRWNR,DMG1,SVR1,DMG2,SVR2,DMG3,SVR3,TREEVAL,&
     &PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,SITEPREP,KODE,DEBUG,&
     &JOSTND,LKECHO,ABIRTH,LBIRTH)
  implicit none

  !     NOTE: ALL OF THE VARIABLES IN THIS ROUTINE ARE VOLATILE. THEY GET RESET
  !           AT EVERY CALL. BE CAREFUL.

  ! Common blocks
  include  'DBSCOM.f90'
  include  'METRIC.f90'

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
# IF _WIN64
# ELSE
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
# ENDIF

  ! Variables
  integer,parameter :: MxCname = 50,MxMsg = 500
  character(LEN = MxCname) :: ColName
  integer :: ITREE,IPLOT,HISTORY,CRWNR,DMG1,DMG2,DMG3,SVR1,SVR2
  integer :: SVR3,TREEVAL,PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,KODE
  integer :: SITEPREP,JOSTND
  real    :: DBH,DG,HT,HTG,HTTOPK,RCOUNT,ABIRTH,DIAMETER
  character(LEN = *) :: SPECIES
  integer :: iRet
  logical :: MATCHED,LKECHO,LBIRTH,DEBUG
  integer :: ColNumber,ColumnCount

  integer :: fsql3_step,fsql3_finalize,fsql3_colisnull,fsql3_colname,fsql3_colcnt,fsql3_colint,&
       &fsql3_coltext
  real*4  :: fsql3_colreal

  !     CHECK TO MAKE SURE WE ARE PULLING TREE INFO FROM DATABASE

  if (DEBUG) write(JOSTND,*) 'IN DBSTREESIN'

  if (ITREEIN.eq.0) then
     KODE = 0
     return
  end if

  iRet = fsql3_step(IinDBref)
  if (iRet.ne.1) then
     ITREEIN = 0
     KODE = -1
     iRet = fsql3_finalize(IinDBref)
     return
  end if

  ABIRTH = 0
  LBIRTH = .false.
  IPLOT = 0
  ITREE = 0
  RCOUNT = 0
  HISTORY = 0
  SPECIES = ' '
  DBH = 0
  DIAMETER = 0
  DG = 0
  HT = 0
  HTG = 0
  HTTOPK = 0
  CRWNR = 0
  DMG1 = 0
  DMG2 = 0
  DMG3 = 0
  SVR1 = 0
  SVR2 = 0
  SVR3 = 0
  TREEVAL = 0
  PRESCRIPT = 0
  SLOPE = 0
  ASPECT = 0
  HABITAT = 0
  TOPOCODE = 0
  SITEPREP = 0

  ColumnCount = fsql3_colcnt(IinDBref)
  do ColNumber = 0,ColumnCount-1
     MATCHED = .false.
     iRet = fsql3_colname(IinDBref,ColNumber,ColName,MxCname)
     ColName((iRet+1):) = ''

     !       BIND COLUMNS TO THEIR VARIABLES

     select case(ColName)

     case('PLOT_ID')
        IPLOT = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('TREE_ID')
        ITREE = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('TREE_COUNT')
        RCOUNT = fsql3_colreal(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('HISTORY')
        HISTORY = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('SPECIES')
        iRet = fsql3_coltext(IinDBref,ColNumber,&
             &SPECIES,LEN(SPECIES),CHAR(0))
        SPECIES(iRet+1:) = ' '
        SPECIES = ADJUSTL(SPECIES)

        !            CULL OUT FIA CODES THAT MAY HAVE THEIR LEADING 0
        !            TRIMMED FOR SOME REASON.
        !            ASSUME THAT ANY 2 CHARACTER FIA CODE NEEDS A LEADING 0

        if (LEN_TRIM(SPECIES).le.2) then
           if ((ICHAR(SPECIES(1:1)).ge.48).and.&
                &(ICHAR(SPECIES(1:1)).le.57).and.&
                &(ICHAR(SPECIES(2:2)).ge.48).and.&
                &(ICHAR(SPECIES(2:2)).le.57)) then
              SPECIES = '0'//SPECIES(:LEN(SPECIES)-1)
           end if
        end if

        MATCHED = .true.

     case('DBH')
        DBH = fsql3_colreal(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('DIAMETER')
        DIAMETER = fsql3_colreal(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('DG')
        DG = fsql3_colreal(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('HT')
        HT = fsql3_colreal(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('HTG')
        HTG = fsql3_colreal(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('HTTOPK')
        HTTOPK = fsql3_colreal(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('CRRATIO')
        CRWNR = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('DAMAGE1')
        DMG1 = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('DAMAGE2')
        DMG2 = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('DAMAGE3')
        DMG3 = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('SEVERITY1')
        SVR1 = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('SEVERITY2')
        SVR2 = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('SEVERITY3')
        SVR3 = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('TREEVALUE')
        TREEVAL = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('PRESCRIPTION')
        PRESCRIPT = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('SLOPE')
        SLOPE = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('ASPECT')
        ASPECT = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('HABITAT','PV_CODE')
        HABITAT = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('TOPOCODE')
        TOPOCODE = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('SITEPREP')
        SITEPREP = fsql3_colint(IinDBref,ColNumber,0)
        MATCHED = .true.

     case('AGE')
        if (fsql3_colisnull(IinDBref,ColNumber) .eq.0) then
           ABIRTH = fsql3_colreal(IinDBref,ColNumber,0)
           LBIRTH = .true.
        end if
        MATCHED = .true.

     end select

     if (ITREEIN.eq.1) then
        if (MATCHED) then
           if (LKECHO)write(JOSTND,4) TRIM(ColName),'USED'
           4 format (T12,A,T30,' WAS ',A)
        else
           if (LKECHO)write(JOSTND,4) TRIM(ColName),'IGNORED'
        end if
     end if
  end do
  if (DBH.eq.0) DBH = DIAMETER

  !  TREES WITH HISTORY CODES 6,7 ARE RECENT DEAD (GET IMC()=7)
  !  TREES WITH HISTORY CODES 8,9 ARE OLDER  DEAD (GET IMC()=9)

  if ((HISTORY.ge.6).and.(HISTORY.le.9)) then
     TREEVAL = 7
     if ((HISTORY.eq.8).or.(HISTORY.eq.9))TREEVAL = 9
  end if

  ITREEIN = 2
  return
end
