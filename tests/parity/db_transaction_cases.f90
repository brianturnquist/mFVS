!=====================================================================!
!   tests/parity : db_transaction_cases                               !
!=====================================================================!
! Direct cases for check_db_transactions.py. Each run calls DBSREFERENCE,
! or the frozen pre-batching routine renamed BASE_DBSREFERENCE, through
! the repository's SQLite wrapper in states that the regression decks do
! not reach. The cases are an outer transaction opened by the caller or
! left open by a failed batched writer, a prepare failure part-way
! through the species loop, and a commit refused because another
! connection holds a read lock. Each case prints what both connections
! observe. The script compares those lines and the final database rows
! between the two routines.
!
! usage: check <case> <base|current>; the database is out.db.

program DB_TRANSACTION_CASES
  implicit none
  include 'PRGPRM.f90'
  include 'DBSCOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  character(len=16) :: CASENAME,ROUTINE
  character(len=26) :: QUOTEDSTAND
  integer :: I,IRET,IREADER
  integer(kind=8) :: MEMORY
  real :: CRFILL(400)
  integer :: fsql3_open,fsql3_exec,fsql3_prepare,fsql3_step,fsql3_finalize,fsql3_close
  integer :: fx_autocommit,fx_statements,fx_count,fx_nobusy
  integer(kind=8) :: fx_memory

  call get_command_argument(1,CASENAME)
  call get_command_argument(2,ROUTINE)

  !     Species data with distinct values in every column. Species 5 is
  !     blank, which DBSREFERENCE skips.
  do I = 1,MAXSP
     write (JSP(I),'(A,I2.2)') 'S',I
     write (PLNJSP(I),'(A,I2.2)') 'PLN',I
     write (FIAJSP(I),'(I3.3)') 100+I
     write (VEQNNC(I),'(A,I2.2)') 'CEQ',I
     write (VEQNNB(I),'(A,I2.2)') 'BEQ',I
     SDIDEF(I) = 400.4+I
     SITEAR(I) = 50.6+I
     DBHMIN(I) = 1.0+0.25*I
     TOPD(I) = 4.0+0.125*I
     STMP(I) = 1.0+0.0625*I
     SCFMIND(I) = 9.0+0.5*I
     SCFTOPD(I) = 6.0+0.375*I
     SCFSTMP(I) = 1.5+0.03125*I
     BFMIND(I) = 9.5+0.5*I
     BFTOPD(I) = 7.0+0.25*I
     BFSTMP(I) = 1.0+0.1*I
  end do
  JSP(5) = ' '
  CFCTYPE = 'F'
  LZEIDE = .false.
  KODFOR = 118
  CASEID = '6f1c2a3e-0000-4000-8000-000000000001'

  IRET = fsql3_open(IoutDBref,'out.db'//char(0))
  IRET = fsql3_open(IREADER,'out.db'//char(0))
  IRET = fsql3_exec(IoutDBref,'CREATE TABLE FVS_Cases (CaseID text primary key);'//&
       &"INSERT INTO FVS_Cases VALUES ('"//CASEID//"');"//char(0))

  select case (trim(CASENAME))
  case ('normal')
     !     Two stands into one database: the table is created, then reused.
     call RUN_REFERENCE('STAND 1')
     call REPORT('first stand')
     MEMORY = fx_memory()
     call RUN_REFERENCE('STAND 2')
     call REPORT('second stand')
     write (*,'(A,I0)') 'memory bytes SQLite still holds for the second stand: ',&
          &fx_memory()-MEMORY
     write (*,'(A,I0)') 'close: ',fsql3_close(IoutDBref)

  case ('outer')
     !     The caller owns a transaction. Its rollback must remove this
     !     stand's rows, so DBSREFERENCE must not have committed them.
     call RUN_REFERENCE('STAND 0')
     IRET = fsql3_exec(IoutDBref,'Begin;'//char(0))
     call RUN_REFERENCE('STAND 1')
     call REPORT('inside caller transaction')
     IRET = fsql3_exec(IoutDBref,'Rollback;'//char(0))
     call REPORT('after caller rollback')

  case ('midfail')
     !     A quote in the third species code makes its INSERT unparsable.
     JSP(3) = "X'"
     call RUN_REFERENCE('STAND 1')
     call REPORT('after prepare failure')

  case ('busy')
     !     Another connection holds a read lock, so no commit can complete.
     !     The test removes the busy handler so each refusal is immediate,
     !     rather than after the wrapper's 20-second retry limit.
     call RUN_REFERENCE('STAND 1')
     IRET = fsql3_exec(IoutDBref,'CREATE TABLE LOCKER (X int);'//&
          &'INSERT INTO LOCKER VALUES (1);'//char(0))
     IRET = fsql3_prepare(IREADER,'SELECT X FROM LOCKER;'//char(0))
     IRET = fsql3_step(IREADER)
     IRET = fx_nobusy(IoutDBref)
     call RUN_REFERENCE('STAND 2')
     call REPORT('while another connection reads')
     IRET = fsql3_finalize(IREADER)
     call REPORT('after the read ends')

  case ('leaked')
     !     A stand ID containing a quote makes DBSFMCANPR's INSERT fail to
     !     prepare after its BEGIN; it returns without COMMIT.
     ICANPR = 1
     CRFILL = 0.0
     CRFILL(1:3) = (/ 1.5,2.5,3.5 /)
     QUOTEDSTAND = "STAND'1"
     call DBSFMCANPR(2000,CRFILL,QUOTEDSTAND)
     call REPORT('after failed canopy-profile writer')
     call RUN_REFERENCE('STAND 1')
     call REPORT('after reference rows')
     write (*,'(A,I0)') 'close: ',fsql3_close(IoutDBref)

  case default
     stop 'unknown case'
  end select

contains

  subroutine RUN_REFERENCE(STAND)
    character(len=*),intent(in) :: STAND
    NPLT = STAND
    if (ROUTINE .eq. 'base') then
       call BASE_DBSREFERENCE
    else
       call DBSREFERENCE
    end if
  end subroutine RUN_REFERENCE

  subroutine REPORT(LABEL)
    character(len=*),intent(in) :: LABEL
    write (*,'(A,": autocommit ",I0,", outstanding statements ",I0,'//&
         &'", rows ",I0,", rows seen by another connection ",I0)')&
         &LABEL,fx_autocommit(IoutDBref),fx_statements(IoutDBref),&
         &fx_count(IoutDBref),fx_count(IREADER)
  end subroutine REPORT

end program DB_TRANSACTION_CASES

!     The cases preset CASEID, so the real DBSCASE would return at once.
subroutine DBSCASE(IFORSURE)
  implicit none
  integer :: IFORSURE
  if (IFORSURE .lt. 0) stop 'unexpected DBSCASE argument'
end subroutine DBSCASE
