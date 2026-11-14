!=====================!
!   dbs : dbsprssql   !
!=====================!
! DBS $Id$
subroutine DBSPRSSQL(ORIGSTR,LSCHED,KODE)
  implicit none

  !     PURPOSE: TO PARSE OUT AND REPLACE EVENT MONITOR VARIABLES AND OTHER
  !              KEYWORDS THAT SHOULD BE WRAPPED BY % SIGNS IN THE SQL
  !              STATEMENT WITH THEIR VALUES
  !     AUTH: D. GAMMEL -- RMRS -- MARCH 2003
  !     INPUT: ORIGSTR  - STRING TO BE MODIFIED
  !            LSCHED   - SPECIFIES IF THIS IS A SCHEDULED EVENT OR NOT
  !            KODE     - RETURN KODE 0 - FAILED IN PARSING, 1 - OK

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'
  include 'CONTRL.f90'
  include 'KEYCOM.f90'
  include 'PLOT.f90'
  include 'DBSCOM.f90'

  ! Variables
  character(len=1) :: DELIMSTR
  character(LEN = *) :: ORIGSTR
  character(LEN = LEN(ORIGSTR)) :: STR
  character(LEN = LEN(ORIGSTR)) :: NEWSTR
  character(LEN = LEN(ORIGSTR)) :: TOKEN
  character(len=20) :: CTOK
  character(len=20) :: CMPUSTR
  integer :: KODE,I,IRETVAL,IRC
  real,dimension(1)::RVAL
  logical :: LSCHED

  STR = ORIGSTR
  DELIMSTR = '%'
  KODE = 1

  !     SEND STRING TO DBSPRS WITH DELIMITER
  call DBSPRS(TOKEN,STR,DELIMSTR)
  do while (LEN_TRIM(STR).gt.0)
     if (LEN_TRIM(ORIGSTR).ne.LEN_TRIM(TOKEN)) then
        !         FOUND A KEYWORD
        KODE = 0
        NEWSTR = TOKEN
        call DBSPRS(TOKEN,STR,DELIMSTR)
        do I = 1, LEN_TRIM(TOKEN)
           call UPCASE(TOKEN(I:I))
        end do
        !         REPLACE KEYWORD WITH VALUE
        select case(TRIM(TOKEN))
        case ('STANDID')
           if (NPLT.eq.' ') then
              NEWSTR = TRIM(NEWSTR)//'NULL'
           else
              NEWSTR = TRIM(NEWSTR)//TRIM(NPLT)
           end if
           KODE = 1
        case ('MGMTID')
           if (MGMID.eq.' ') then
              NEWSTR = TRIM(NEWSTR)//'NULL'
           else
              NEWSTR = TRIM(NEWSTR)//TRIM(MGMID)
           end if
           KODE = 1
        case ('STAND_CN')
           if (DBCN.eq.' ') then
              NEWSTR = TRIM(NEWSTR)//'NULL'
           else
              NEWSTR = TRIM(NEWSTR)//TRIM(DBCN)
           end if
           KODE = 1
        case('VARIANT')
           NEWSTR = TRIM(NEWSTR)//VARACD
           KODE = 1
        case('FVSCASE')
           call DBSCASE(1)
           NEWSTR = TRIM(NEWSTR)//' '//CASEID
        case default
           if (LSCHED) then
              !               CHECK AGAINST COMPUTES AND OTHER EVENT MONITOR VARS
              CTOK = TOKEN
              call ALGKEY(CTOK,LEN_TRIM(CTOK),IRETVAL,IRC)
              if (IRC.eq.0) then
                 !                 FOUND THE TOKEN
                 call EVLDX(RVAL,1,IRETVAL,IRC)
              end if
              if (IRETVAL.eq.0) then
                 !                 THE VALUE IS DEFINED SO GRAB VALUE TO REPLACE STRIPPED TOKEN
                 KODE = 1
                 if (FLOAT(IFIX(RVAL(1))).ne.RVAL(1)) then
                    write(CMPUSTR,'(E14.7)')RVAL(1)
                 else
                    write(CMPUSTR,'(I8)')IFIX(RVAL(1))
                 end if
                 NEWSTR = TRIM(NEWSTR)//' '//TRIM(adjustl(CMPUSTR))
              else
                 KODE = 1
                 NEWSTR = TRIM(NEWSTR)//' NULL'
              end if
           else
              NEWSTR = TRIM(NEWSTR)//TRIM(TOKEN)
           end if
        end select
     end if
     ORIGSTR = TRIM(NEWSTR)//TRIM(STR)
     STR = ORIGSTR
     call DBSPRS(TOKEN,STR,DELIMSTR)
  end do
end
