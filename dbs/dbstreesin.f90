!======================!
!   dbs : dbstreesin   !
!======================!
! DBS $Id$
subroutine DBSTREESIN(IPLOT,ITREE,RCOUNT,HISTORY,SPECIES,DBH,DG,&
     &HT,HTTOPK,HTG,CRWNR,DMG1,SVR1,DMG2,SVR2,DMG3,SVR3,TREEVAL,&
     &PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,SITEPREP,KODE,DEBUG,&
     &JOSTND,LKECHO,ABIRTH,LBIRTH)
  implicit none

  !     NOTE: ALL OF THE VARIABLE IN THIS ROUTINE ARE VOLITILE. THE GET RESET
  !           AT EVERY CALL. BE CAREFUL.
  !     DBSTREESIN
  !     PURPOSE: TO PARSE THE TREE RECORD SET FOR TREEDATA AND RETURN THE RECORD
  !         IN THE EXPECTED FORMAT FOR TREESIN
  !     AUTH: D. GAMMEL -- RMRS -- NOVEMBER 2002
  !     OVERHAUL: NL CROOKSTON  -- RMRS -- SEPTEMBER 2004

  ! Common blocks
  include  'DBSCOM.f90'

  ! Variables
  character(len=30) :: ColName
  integer :: ITREE,IPLOT,HISTORY,CRWNR,DMG1,DMG2,DMG3,SVR1,SVR2
  integer :: SVR3,TREEVAL,PRESCRIPT,SLOPE,ASPECT,HABITAT,TOPOCODE,KODE
  integer :: SITEPREP,JOSTND
  real    :: DBH,DG,HT,HTG,HTTOPK,RCOUNT,ABIRTH,XXX
  character(LEN = *) :: SPECIES
  integer :: I
  logical :: DEBUG,MATCHED,LKECHO,LBIRTH
  integer(SQLSMALLINT_KIND)::ColNumber,NameLen,ColumnCount
  integer(SQLLEN_KIND)::tmpNotUsed

  !     CHECK TO MAKE SURE WE ARE PULLING TREE INFO FROM DATABASE

  if (ITREEIN.eq.0) then
     KODE = 0
     return
  end if

  !     RETRIEVE COLUMN HEADINGS RETURNED IF WE HAVE NOT ALREADY DONE SO

  if (ITREEIN.ne.2) then
     ITREEIN = 2
     iRet = fvsSQLNumResultCols(StmtHndlTree,ColumnCount)
     if (LKECHO)write (JOSTND,'(/T12,''COLUMN PROCESSING RESULTS:'')')
     do ColNumber = 1,ColumnCount
        MATCHED = .false.

        !          GET COLUMN NAME

        iRet = fvsSQLColAttribute(StmtHndlTree, ColNumber,&
             &SQL_DESC_NAME, ColName,&
             &int(LEN(ColName),SQLUSMALLINT_KIND),&
             &NameLen, tmpNotUsed)
        ColName(NameLen+1:) = ' '

        !          UPPER CASE THE COLUMN NAME FOR EASIER COMPARISONS

        do I = 1, NameLen
           call UPCASE(ColName(I:I))
        end do

        !          BIND COLUMNS TO THEIR VARIABLES

        select case(ColName(:NameLen))

        case('PLOT_ID')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(1),int(4,SQLLEN_KIND),VAL_LI(1))
           MATCHED = .true.

        case('TREE_ID')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(2),int(4,SQLLEN_KIND),VAL_LI(2))
           MATCHED = .true.

        case('TREE_COUNT')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(3),int(4,SQLLEN_KIND),VAL_LI(3))
           MATCHED = .true.

        case('HISTORY')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(4),int(4,SQLLEN_KIND),VAL_LI(4))
           MATCHED = .true.

        case('SPECIES')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_CHAR,&
                &SPECIES,int(LEN(SPECIES),SQLLEN_KIND),VAL_LI(24))
           MATCHED = .true.

        case('DBH','DIAMETER')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(5),int(4,SQLLEN_KIND),VAL_LI(5))
           MATCHED = .true.

        case('DG')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(6),int(4,SQLLEN_KIND),VAL_LI(6))
           MATCHED = .true.

        case('HT')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(7),int(4,SQLLEN_KIND),VAL_LI(7))
           MATCHED = .true.

        case('HTG')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(8),int(4,SQLLEN_KIND),VAL_LI(8))
           MATCHED = .true.

        case('HTTOPK')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(9),int(4,SQLLEN_KIND),VAL_LI(9))
           MATCHED = .true.

        case('CRRATIO')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(10),int(4,SQLLEN_KIND),VAL_LI(10))
           MATCHED = .true.

        case('DAMAGE1')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(11),int(4,SQLLEN_KIND),VAL_LI(11))
           MATCHED = .true.

        case('DAMAGE2')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(12),int(4,SQLLEN_KIND),VAL_LI(12))
           MATCHED = .true.

        case('DAMAGE3')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(13),int(4,SQLLEN_KIND),VAL_LI(13))
           MATCHED = .true.

        case('SEVERITY1')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(14),int(4,SQLLEN_KIND),VAL_LI(14))
           MATCHED = .true.

        case('SEVERITY2')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(15),int(4,SQLLEN_KIND),VAL_LI(15))
           MATCHED = .true.

        case('SEVERITY3')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(16),int(4,SQLLEN_KIND),VAL_LI(16))
           MATCHED = .true.

        case('TREEVALUE')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(17),int(4,SQLLEN_KIND),VAL_LI(17))
           MATCHED = .true.

        case('PRESCRIPTION')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(18),int(4,SQLLEN_KIND),VAL_LI(18))
           MATCHED = .true.

        case('SLOPE')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(19),int(4,SQLLEN_KIND),VAL_LI(19))
           MATCHED = .true.

        case('ASPECT')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(20),int(4,SQLLEN_KIND),VAL_LI(20))
           MATCHED = .true.

        case('HABITAT','PV_CODE')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_CHAR,&
                &CTMP,int(LEN(CTMP),SQLLEN_KIND),VAL_LI(21))
           MATCHED = .true.

        case('TOPOCODE')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(22),int(4,SQLLEN_KIND),VAL_LI(22))
           MATCHED = .true.

        case('SITEPREP')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(23),int(4,SQLLEN_KIND),VAL_LI(23))
           MATCHED = .true.

        case('AGE')
           iRet = fvsSQLBindCol (StmtHndlTree,ColNumber,SQL_F_FLOAT,&
                &RTREEDATA(25),int(4,SQLLEN_KIND),VAL_LI(25))
           MATCHED = .true.
        end select

        if (MATCHED) then
           if (LKECHO)write(JOSTND,4) ColName(1:15),'BOUND'
           4 format (T12,A,' WAS ',A)
        else
           if (LKECHO)write(JOSTND,4) ColName(1:15),'IGNORED'
        end if
     end do
  end if

  !     INITIALIZE THE RESULT VECTOR.

  VAL_LI = SQL_NULL_DATA

  !     FETCH DATA FROM DATABASE

  iRet = fvsSQLFetch(StmtHndlTree)
  if (iRet.eq.SQL_SUCCESS.or.iRet&
       &.eq.SQL_SUCCESS_WITH_INFO) then
     KODE = 1
  else
     KODE = -1
  end if

  if (DEBUG) write (JOSTND,5) iRet,KODE
  5 format ('FETCH RETURN=',I6,' KODE=',I3)

  !     ASSIGN DATABASE VALUES TO VARIABLES
  !     CATCH NULLS AND ASSIGN VALUES TO 0 INSTEAD

  if (KODE.eq.1) then
     if (VAL_LI(5).eq.SQL_NULL_DATA) then
        DBH = 0
     else
        DBH = RTREEDATA(5)
     end if
     if (VAL_LI(6).eq.SQL_NULL_DATA) then
        DG = 0
     else
        DG = RTREEDATA(6)
     end if
     if (VAL_LI(7).eq.SQL_NULL_DATA) then
        HT = 0
     else
        HT = RTREEDATA(7)
     end if
     if (VAL_LI(8).eq.SQL_NULL_DATA) then
        HTG = 0
     else
        HTG = RTREEDATA(8)
     end if
     if (VAL_LI(9).eq.SQL_NULL_DATA) then
        HTTOPK = 0
     else
        HTTOPK = RTREEDATA(9)
     end if
     if (VAL_LI(1).eq.SQL_NULL_DATA) then
        IPLOT = 0
     else
        IPLOT = INT(RTREEDATA(1))
     end if
     if (VAL_LI(2).eq.SQL_NULL_DATA) then
        ITREE = 0
     else
        ITREE = INT(RTREEDATA(2))
     end if
     if (VAL_LI(3).eq.SQL_NULL_DATA) then
        RCOUNT = 0
     else
        RCOUNT = RTREEDATA(3)
     end if
     if (VAL_LI(4).eq.SQL_NULL_DATA) then
        HISTORY = 0
     else
        HISTORY = INT(RTREEDATA(4))
     end if
     if (VAL_LI(10).eq.SQL_NULL_DATA) then
        CRWNR = 0
     else
        CRWNR = INT(RTREEDATA(10))
     end if
     if (VAL_LI(11).eq.SQL_NULL_DATA) then
        DMG1 = 0
     else
        DMG1 = INT(RTREEDATA(11))
     end if
     if (VAL_LI(12).eq.SQL_NULL_DATA) then
        DMG2 = 0
     else
        DMG2 = INT(RTREEDATA(12))
     end if
     if (VAL_LI(13).eq.SQL_NULL_DATA) then
        DMG3 = 0
     else
        DMG3 = INT(RTREEDATA(13))
     end if
     if (VAL_LI(14).eq.SQL_NULL_DATA) then
        SVR1 = 0
     else
        SVR1 = INT(RTREEDATA(14))
     end if
     if (VAL_LI(15).eq.SQL_NULL_DATA) then
        SVR2 = 0
     else
        SVR2 = INT(RTREEDATA(15))
     end if
     if (VAL_LI(16).eq.SQL_NULL_DATA) then
        SVR3 = 0
     else
        SVR3 = INT(RTREEDATA(16))
     end if
     if (VAL_LI(17).eq.SQL_NULL_DATA) then
        TREEVAL = 0
     else
        TREEVAL = INT(RTREEDATA(17))
     end if
     if (VAL_LI(18).eq.SQL_NULL_DATA) then
        PRESCRIPT = 0
     else
        PRESCRIPT = INT(RTREEDATA(18))
     end if
     if (VAL_LI(19).eq.SQL_NULL_DATA) then
        SLOPE = 0
     else
        SLOPE = INT(RTREEDATA(19))
     end if
     if (VAL_LI(20).eq.SQL_NULL_DATA) then
        ASPECT = 0
     else
        ASPECT = INT(RTREEDATA(20))
     end if

     HABITAT = 0
     if (VAL_LI(21).ne.SQL_NULL_DATA.and.VAL_LI(21).gt.0) then
        CTMP(VAL_LI(21)+1:) = ' '
        read (CTMP,*,ERR = 10) HABITAT
        goto 20
        10 continue
        read (CTMP,*,ERR = 20) XXX
        HABITAT = IFIX(XXX)
        20 continue
     end if
     if (VAL_LI(22).eq.SQL_NULL_DATA) then
        TOPOCODE = 0
     else
        TOPOCODE = INT(RTREEDATA(22))
     end if
     if (VAL_LI(23).eq.SQL_NULL_DATA) then
        SITEPREP = 0
     else
        SITEPREP = INT(RTREEDATA(23))
     end if
     if (VAL_LI(24).eq.SQL_NULL_DATA) then
        SPECIES = ' '
     else
        SPECIES(VAL_LI(24)+1:) = ' '

        !  CULL OUT FIA CODES THAT MAY HAVE THEIR LEADING 0 TRIMMED FOR SOME
        !  REASON. ASSUME THAT ANY 2 CHARACTER FIA CODE NEEDS A LEADING 0

        if (LEN_TRIM(SPECIES).le.2) then
           if ((ICHAR(ADJUSTL(SPECIES(1:1))).ge.48).and.&
                &(ICHAR(ADJUSTL(SPECIES(1:1))).le.57).and.&
                &(ICHAR(ADJUSTL(SPECIES(2:2))).ge.48).and.&
                &(ICHAR(ADJUSTL(SPECIES(2:2))).le.57)) then
              SPECIES = '0'//ADJUSTL(SPECIES)
           end if
        end if

        SPECIES = ADJUSTL(SPECIES)
     end if
     if (VAL_LI(25).eq.SQL_NULL_DATA) then
        ABIRTH = 0
        LBIRTH = .false.
     else
        ABIRTH = RTREEDATA(25)
        LBIRTH = .true.
     end if

     !  TREES WITH HISTORY CODES 6,7 ARE RECENT DEAD (GET IMC()=7)
     !  TREES WITH HISTORY CODES 8,9 ARE OLDER  DEAD (GET IMC()=9)

     if ((HISTORY.ge.6).and.(HISTORY.le.9)) then
        TREEVAL = 7
        if ((HISTORY.eq.8).or.(HISTORY.eq.9))TREEVAL = 9
     end if
  end if

  !     RELEASE STMT HANDLE WHEN NECESSARY

  if (KODE.ne.1) then
     iRet = fvsSQLFreeHandle(SQL_HANDLE_STMT,StmtHndlTree)
  end if

end
