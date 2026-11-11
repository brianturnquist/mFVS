!========================!
!   dbsqlite : dbscmpu   !
!========================!
! DBSQLITE $Id$
subroutine DBSCMPU
  implicit none

  !     PURPOSE: TO POPULATE A DATABASE WITH THE COMPUTE TABLE
  !              INFORMATION

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'
  include 'CONTRL.f90'
  include 'KEYCOM.f90'
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
  integer :: MXLEN
  parameter(MXLEN = 10000)
  character(LEN = MXLEN) :: SQLStmt
  character(LEN = 8),dimension(MXTST5)::KWINSRT
  double precision,dimension(MXTST5)::CURVAL
  logical :: LDUPKW
  integer :: I,II,IX,I3,iRet,THISYR,ICY,NSRTNUM

  integer :: fsql3_tableexists,fsql3_exec,fsql3_addcolifabsent

  !     IF COMPUTE IS NOT TURNED ON OR THE NUMBER OF VARIABLES IS 0
  !     THEN RETURN

  if (ICOMPUTE.eq.0.or.ITST5.eq.0) return

  !     Do we need the table, or all all variables with a _

  II = 0
  do I = 1,ITST5
     if (.not.(CTSTV5(I)(1:1).eq.'_'.and.I_CMPU.lt.1)) then
        II = 1
        exit
     end if
  end do
  if (II.eq.0) return

  !     CALL DBSCASE TO MAKE SURE WE HAVE AN UP TO DATE CASEID

  call DBSCASE(1)

  !     CHECK TO SEE IF THE COMPUTE TABLE EXISTS IN DATBASE
  !     IF IT DOES NOT THEN WE NEED TO CREATE IT

  iRet = fsql3_exec (IoutDBref,"Begin;"//Char(0))
  iRet = fsql3_tableexists(IoutDBref,"FVS_Compute"//CHAR(0))
  if (iRet.eq.0) then
     SQLStmt = 'CREATE TABLE FVS_Compute('//&
          &'CaseID text not null,'//&
          &'StandID text not null,'//&
          &'Year int null'
     do I = 1,ITST5
        if (.not.(CTSTV5(I)(1:1).eq.'_'.and.I_CMPU.lt.1)) then
           SQLStmt = TRIM(SQLStmt)//', '//&
                &TRIM(CTSTV5(I))//' real null'
        end if
     end do
     iRet = fsql3_exec(IoutDBref,TRIM(SQLStmt)//');'//CHAR(0))
     if (iRet .ne.0) then
        ICOMPUTE = 0
        return
     end if
  else

     !       MAKE SURE ALL THE NEEDED COLUMNS EXIST

     do I = 1,ITST5
        if (.not.(CTSTV5(I)(1:1).eq.'_'.and.I_CMPU.lt.1)) then
           iRet = fsql3_addcolifabsent(IoutDBref,&
                &"FVS_Compute"//CHAR(0),&
                &TRIM(CTSTV5(I))//CHAR(0), "real"//CHAR(0))
        end if
     end do
  end if

  !     BUILD AND RUN THE INSERT COMMANDS

  do ICY = 1,NCYC

     !     Make a list of variables and correspoinding values
     !     for the current cycle

     NSRTNUM = 0
     THISYR = -1
     do II = IMGPTS(ICY,1),IMGPTS(ICY,2)
        I = IOPSRT(II)
        if (IACT(I,1).eq.33.and. IACT(I,4).gt.0) then
           if (THISYR.eq.-1) THISYR = IACT(I,4)
           if (IACT(I,4).ne.THISYR) then
              if (NSRTNUM.gt.0) then

                 !             Build and run an insert query

                 call INSERTCMPU(IoutDBref,KWINSRT,CURVAL,THISYR,&
                      &NPLT,CASEID,NSRTNUM)

              end if
              NSRTNUM = 0
              THISYR = IACT(I,4)
           end if
           IX = IFIX(PARMS(IACT(I,2)+1))
           if (IX.gt.500) IX = IX-500

           !           CHECK TO SEE IF WE WANT TO SKIP UNDERSCORE COMPUTES

           if (CTSTV5(IX)(1:1).eq.'_'.and.I_CMPU.lt.1) cycle
           if (NSRTNUM.eq.0) then
              KWINSRT(1) = CTSTV5(IX)
              CURVAL(1) = PARMS(IACT(I,2))
              NSRTNUM = 1
           else
              LDUPKW = .false.
              do I3 = 1,NSRTNUM
                 if (TRIM(KWINSRT(I3)).eq.TRIM(CTSTV5(IX))) then
                    CURVAL(I3) = PARMS(IACT(I,2))
                    LDUPKW = .true.
                    exit
                 end if
              end do
              if (.not.LDUPKW) then
                 NSRTNUM = NSRTNUM + 1
                 KWINSRT(NSRTNUM) = CTSTV5(IX)
                 CURVAL(NSRTNUM) = PARMS(IACT(I,2))
              end if
           end if
        end if
     end do

     !       More to insert?

     if (NSRTNUM.gt.0)&
          &call INSERTCMPU(IoutDBref,KWINSRT,CURVAL,THISYR,NPLT,CASEID,&
          &NSRTNUM)
  end do
  iRet = fsql3_exec (IoutDBref,"Commit;"//Char(0))
  return
end

subroutine INSERTCMPU(IoutDBref,KWINSRT,CURVAL,THISYR,STANDID,&
     &CASEID,NSRTNUM)
  implicit none

  !     INPUT: KWINSRT - THE ARRAY OF COMPUTE VARS TO INSERT
  !            CURVAL  - THE ARRAY OF VALUES ASSOCIATED W/ THE VARS
  !            NSRTNUM - THE NUMBER OF COMPUTE VARS TO INSERT

  ! Variables
  integer :: I,THISYR,IoutDBref,IRT,NSRTNUM
  character(LEN = 8) :: KWINSRT(NSRTNUM)
  double precision :: CURVAL(NSRTNUM)
  character(LEN = 26) :: STANDID
  character(LEN = 36) :: CASEID
  character(LEN = 5000) :: SQLStmt
  character(LEN = 500) :: VALS
  integer :: fsql3_prepare,fsql3_bind_int,fsql3_bind_double,fsql3_finalize,fsql3_errmsg,fsql3_step

  SQLStmt = " "
  VALS = " "
  if (NSRTNUM.eq.0) return
  do I = 1,NSRTNUM
     SQLStmt = TRIM(SQLStmt) // "," // TRIM(KWINSRT(I))
     VALS = TRIM(VALS)    // ",?"
  end do

  SQLStmt = "insert into FVS_Compute (CaseID,StandID,Year" //&
       &trim(SQLStmt) // ") values ('" // CASEID // "','" //&
       &TRIM(STANDID) // "',?" // TRIM(VALS) // ");" // CHAR(0)
  IRT = fsql3_prepare(IoutDBref,SQLStmt)
  if (IRT>0) then
     IRT = fsql3_errmsg(IoutDBref,VALS, 200)
     print *,"dbscmpu prepare error: ",TRIM(VALS)
  end if
  IRT = fsql3_bind_int(IoutDBref, 1, THISYR)
  do I = 1,NSRTNUM
     IRT = fsql3_bind_double(IoutDBref,I+1,CURVAL(I))
  end do
  IRT = fsql3_step(IoutDBref)
  if (IRT>0) then
     IRT = fsql3_errmsg(IoutDBref,VALS,200)
     print *,"dbscmpu step error: ",TRIM(VALS)
  end if
  IRT = fsql3_finalize(IoutDBref)
  if (IRT>0) then
     IRT = fsql3_errmsg(IoutDBref,VALS,200)
     print *,"dbscmpu finalize error: ",TRIM(VALS)
  end if
  return
end
