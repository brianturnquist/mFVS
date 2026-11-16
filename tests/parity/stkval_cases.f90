!=====================================!
!   tests : STKVAL correctness cases   !
!=====================================!
! Compare result bits, classes and diagnostics against the corrected
! pre-optimization routine. All generated listings use scratch files.
program STKVAL_CHECK
  implicit none
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  real :: INPUT_S(210),OLD_S(210),NEW_S(210),TRIAL_WEIGHTS(5),STAGE_VALUES(6),UNKNOWN_VALUES(6)
  integer :: CASE_NO,KTEST,SPTEST,OLD_SIZE,OLD_CLASS,LEFT_UNIT,RIGHT_UNIT
  integer :: IOS_LEFT,IOS_RIGHT,VALUE_START,FUTURE_CASES,STANDARD_CASES
  character(len=1024) :: LEFT_LINE,RIGHT_LINE
  character(len=4) :: TEST_CODES(5)
  logical :: TRACE
  common /STKVAL_TEST_CONTROL/ TRACE

  VARACD = VARIANTCD
  TRACE = .true.
  TEST_CODES = [' 121','0299',' 998','0999','    ']
  TRIAL_WEIGHTS = [0.01,0.1,1.,10.,1000.]
  FUTURE_CASES = 0
  STANDARD_CASES = 0
  do CASE_NO = 0,59
    ICYC = CASE_NO
    ITRN = 6
    if (CASE_NO.eq.0) ITRN = 0
    ISCT = 0
    FIAJSP = '    '
    do SPTEST = 1,3
      ISCT(SPTEST,1) = 2*SPTEST-1
      ISCT(SPTEST,2) = 2*SPTEST
      FIAJSP(SPTEST) = TEST_CODES(1+mod(CASE_NO+SPTEST,5))
    end do
    FIAJSP(MAXSP) = '0299'
    do KTEST = 1,6
      IND1(KTEST) = KTEST
    end do
    DBH(1:6) = [0.1,1.,4.99,5.,9.,18.]
    if (mod(CASE_NO,4).eq.0) DBH(1:6) = [0.1,0.3,1.,2.,3.,4.9]
    if (mod(CASE_NO,4).eq.1) DBH(1:6) = [5.,8.,9.,10.,20.,30.]
    PROB(1:6) = TRIAL_WEIGHTS(1+mod(CASE_NO/4,5))
    IMC(1:6) = 1
    if (mod(CASE_NO,5).eq.0) IMC(2) = 6
    do KTEST = 1,210
      INPUT_S(KTEST) = real(KTEST)/211.
    end do
    OLD_S = INPUT_S
    NEW_S = INPUT_S
    open(newunit=LEFT_UNIT,status='scratch',action='readwrite')
    open(newunit=RIGHT_UNIT,status='scratch',action='readwrite')
    JOSTND = LEFT_UNIT
    call BASE_STKVAL(OLD_S)
    OLD_SIZE = ISZCL
    OLD_CLASS = ISTCL
    JOSTND = RIGHT_UNIT
    call STKVAL(NEW_S)
    if (any(transfer(OLD_S,[0],210).ne.transfer(NEW_S,[0],210))) stop 1
    if (ISZCL.ne.OLD_SIZE.or.ISTCL.ne.OLD_CLASS) stop 2
    rewind(LEFT_UNIT)
    rewind(RIGHT_UNIT)
    do
      read(LEFT_UNIT,'(A)',iostat=IOS_LEFT) LEFT_LINE
      read(RIGHT_UNIT,'(A)',iostat=IOS_RIGHT) RIGHT_LINE
      if (IOS_LEFT.ne.IOS_RIGHT) stop 3
      if (IOS_LEFT.lt.0) exit
      if (IOS_LEFT.ne.0.or.LEFT_LINE.ne.RIGHT_LINE) stop 4
      if (index(LEFT_LINE,'IN STKVAL_3:').gt.0) then
        VALUE_START = index(LEFT_LINE,' - ')+3
        read(LEFT_LINE(VALUE_START:),*) STAGE_VALUES
        if (STAGE_VALUES(4).lt.20.) then
          FUTURE_CASES = FUTURE_CASES+1
        else
          STANDARD_CASES = STANDARD_CASES+1
        end if
      end if
    end do
    close(LEFT_UNIT)
    close(RIGHT_UNIT)
  end do
  if (FUTURE_CASES.eq.0.or.STANDARD_CASES.eq.0) stop 5
  ! Eastern FIA 299 has no coefficient row. Its stocking calculations must
  ! use the existing unknown-species fallback, while forest-type mapping stays
  ! specific to the original species code.
  select case (VARACD)
  case ('CS','LS','NE','SN','ON')
    FIAJSP(1:3) = '0299'
    OLD_S = INPUT_S
    open(newunit=LEFT_UNIT,status='scratch',action='readwrite')
    JOSTND = LEFT_UNIT
    call STKVAL(OLD_S)
    OLD_SIZE = ISZCL
    OLD_CLASS = ISTCL
    call READ_STAGE(LEFT_UNIT,UNKNOWN_VALUES)
    close(LEFT_UNIT)
    FIAJSP(1:3) = '0999'
    NEW_S = INPUT_S
    open(newunit=RIGHT_UNIT,status='scratch',action='readwrite')
    JOSTND = RIGHT_UNIT
    call STKVAL(NEW_S)
    call READ_STAGE(RIGHT_UNIT,STAGE_VALUES)
    close(RIGHT_UNIT)
    if (ISZCL.ne.OLD_SIZE.or.ISTCL.ne.OLD_CLASS) stop 6
    if (any(transfer(UNKNOWN_VALUES,[0],6).ne.transfer(STAGE_VALUES,[0],6))) stop 7
  end select
  write(*,*) 'PASS STKVAL ',VARACD,': 60 exact cases; low/high stocking=',FUTURE_CASES,STANDARD_CASES
contains
  subroutine READ_STAGE(UNIT,VALUES)
    implicit none
    integer :: UNIT,IOS,START
    real :: VALUES(6)
    character(len=1024) :: LINE
    rewind(UNIT)
    do
      read(UNIT,'(A)',iostat=IOS) LINE
      if (IOS.ne.0) stop 8
      if (index(LINE,'IN STKVAL_3:').gt.0) then
        START = index(LINE,' - ')+3
        read(LINE(START:),*) VALUES
        return
      end if
    end do
  end subroutine READ_STAGE
end program STKVAL_CHECK

subroutine DBCHK(DEBUG,NAME,NCHAR,CYCLE)
  implicit none
  logical :: DEBUG,TRACE
  character(len=*) :: NAME
  integer :: NCHAR,CYCLE
  common /STKVAL_TEST_CONTROL/ TRACE
  DEBUG = TRACE
end subroutine DBCHK
