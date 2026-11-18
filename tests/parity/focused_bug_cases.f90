!====================================================!
!   Focused pre-module correctness regression cases   !
!====================================================!
! Link with the selected real model objects. Each invocation isolates one
! repaired contract; the Python runner checks the resulting SQLite contents.
program FOCUSED_BUG_CASES
  implicit none
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'OPCOM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'VOLSTD.f90'
  include 'DBSCOM.f90'
  include 'OUTCOM.f90'
  include 'SUMTAB.f90'
  include 'PDEN.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  character(len=16) :: TESTCASE
  integer :: I,J,IRC,IRET,MODE,ITOP,EQERR
  integer :: fsql3_open,fsql3_close
  real :: VALUE(1),BEFOREBA,TESTD,TESTH,TESTBARK,BRATIO
  real :: EXPECTN,EXPECTM,DUMMYBF,MAXVOL,UNUSEDSCF,TESTHT,OBSERVED,EXPECTED
  logical :: CONEFLAG,CFKILL,BFKILL
  character(len=2) :: REGIONVAR
  character(len=10) :: EQUATION

  call get_command_argument(1,TESTCASE)
  if (TESTCASE.eq.'regent') then
     call fvsSetCmdLine('--keywordfile=emt01.key',23,IRC)
     if (IRC.ne.0) error stop 'keyword setup failed'
     call fvsSetStopPointCodes(4,-1)
     call FVS(IRC)
     if (IRC.ne.0) error stop 'did not reach stop 4'
     ! Use a defined missing-crown transition in the initialized native stand.
     ! All other species are excluded from this direct REGENT invocation.
     ISCT = 0
     ISCT(5,1) = 1
     ISCT(5,2) = 1
     IND1(1) = 1
     ISP(1) = 5
     DBH(1) = 2.99
     HT(1) = 50.0
     HTG(1) = 2.0
     DG(1) = 0.2
     ICR(1) = 0
     DGSD = 0.0
     LTRIP = .false.
     BEFOREBA = BA
     call REGENT(.false.,1)
     write(*,*) 'REGENT BA/CR/diameter: ',BEFOREBA,BA,ICR(1),DBH(1)+DG(1)
     if (BA.ne.BEFOREBA) error stop 'REGENT overwrote stand BA'
     if (ICR(1).lt.5.or.ICR(1).gt.95) error stop 'transition crown not returned'
  else
     call GRINIT
     VARACD = VARIANTCD
     JOSTND = 30
     open(JOSTND,file='listing.out',status='replace')
     select case (trim(TESTCASE))
     case ('constants')
        do I = 1,MAXPRM
           PARMS(I) = real(I)
           call EVLDX(VALUE,1,1000+I,IRC)
           if (IRC.ne.0.or.VALUE(1).ne.real(I)) error stop 'constant rejected or changed'
        end do
        call EVLDX(VALUE,1,MAXPRM+1001,IRC)
        if (IRC.ne.2) error stop 'invalid instruction accepted'
     case ('summary')
        IRET = fsql3_open(IoutDBref,'summary.db'//char(0))
        if (IRET.ne.0) error stop 'open failed'
        CASEID = '6f1c2a3e-0000-4000-8000-000000000001'
        ISUMARY = 1
        do I = 1,2
           call DBSSUMRY(2000+I,50+I,'stand',101,102,103,104,105,10.5,&
                &201,202,203,301,401,402,403,501,502,503,504,20.5,10,601,602,30.5,701,2,3)
        end do
        if (ISUMARY.ne.1) error stop 'summary disabled'
        IRET = fsql3_close(IoutDBref)
        if (IRET.ne.0) error stop 'summary close failed'
     case ('fia')
        IRET = fsql3_open(IoutDBref,'fia.db'//char(0))
        if (IRET.ne.0) error stop 'open failed'
        CASEID = '6f1c2a3e-0000-4000-8000-000000000001'
        LFIANVB = .true.
        IVBCCUTLST = 1
        IVBCATRLST = 1
        ITRN = 0
        ICYC = 1
        IY(1:2) = [2000,2010]
        do I = 1,3
           call DBS_FIAVBC_CUTLST
           call DBS_FIAVBC_ATRTLS
        end do
        IRET = fsql3_close(IoutDBref)
        if (IRET.ne.0) error stop 'FIA statements kept connection busy'
     case ('live')
        if (VARIANTCD.ne.'ON') error stop 'live case is ON'
        ITRN = 1
        LSTART = .true.
        LCVOLS = .false.
        LBVOLS = .false.
        BFMIND = 999.0
        do I = 1,3
           PROB(1) = 10.0
           ISP(1) = 2
           IMC(1) = 1
           DBH(1) = 12.0
           HT(1) = 40.0
           ITRUNC(1) = 4000
           NORMHT(1) = 8000
           if (I.eq.2) then
              HT(1) = 80.0
              ITRUNC(1) = 0
           else if (I.eq.3) then
              DBH(1) = 0.1
              HT(1) = 4.7
              ITRUNC(1) = 400
           end if
           ! Compute the oracle through the native provider and Behre
           ! correction, including an intact control and the small placeholder.
           TESTD = DBH(1)
           TESTH = HT(1)
           if (ITRUNC(1).gt.0) TESTH = NORMHT(1)/100.0
           TESTBARK = BRATIO(ISP(1),TESTD,TESTH)
           CONEFLAG = .false.
           call OCFVOL(EXPECTN,EXPECTM,ISP(1),TESTD,TESTH,ITRUNC(1).gt.0,TESTBARK,&
                &ITRUNC(1),MAXVOL,CONEFLAG,CFKILL,1)
           UNUSEDSCF = 0.0
           if (ITRUNC(1).gt.0.and.CFKILL.and.MAXVOL.gt.0.) then
              if (TESTD.lt.DBHMIN(ISP(1))) EXPECTM = 0.0
              call CFTOPK(ISP(1),TESTD,TESTH,EXPECTN,EXPECTM,UNUSEDSCF,MAXVOL,CONEFLAG,TESTBARK,ITRUNC(1))
           end if
           call VOLS
           if (CFV(1).ne.EXPECTN.or.WK1(1).ne.EXPECTM) error stop 'live top-kill products differ'
        end do
     case ('natcrs')
        if (VARIANTCD.ne.'ON') error stop 'native cruise case is ON'
        REGIONVAR = 'LS'
        call VOLEQDEF(REGIONVAR,9,'03','01',68,'01',EQUATION,EQERR)
        if (EQERR.ne.0) error stop 'equation lookup failed'
        VEQNNC(1) = EQUATION
        VEQNNB(1) = EQUATION
        LFIANVB = .false.
        DBHMIN(1) = 5.0
        BFMIND(1) = 8.0
        TOPD(1) = 4.0
        BFTOPD(1) = 6.0
        STMP(1) = 1.0
        do MODE = 6,10,4
           METHC(1) = MODE
           do ITOP = 0,1
              TESTD = 12.0
              TESTH = 80.0
              TESTBARK = BRATIO(1,TESTD,TESTH)
              call NATCRS(EXPECTN,EXPECTM,DUMMYBF,1,TESTD,TESTH,ITOP.eq.1,TESTBARK,&
                   &4000,MAXVOL,CFKILL,BFKILL,0)
              if (EXPECTN.le.0.0.or.EXPECTM.le.0.0) error stop 'empty native oracle'
              CONEFLAG = .false.
              UNUSEDSCF = 0.0
              if (ITOP.eq.1) call CFTOPK(1,TESTD,TESTH,EXPECTN,EXPECTM,UNUSEDSCF,&
                   &MAXVOL,CONEFLAG,TESTBARK,4000)
              SPS(1) = 1
              DBHS(1) = TESTD
              HTDEAD(1) = TESTH
              do J = 0,2
                 TESTHT = -1.0
                 if (ITOP.eq.1) TESTHT = 40.0
                 if (J.eq.0) then
                    call FMSVOL(1,TESTHT,OBSERVED,.false.,30)
                 else
                    call FMSVL2(1,TESTD,TESTH,TESTHT,OBSERVED,0,'D',J.eq.2,.false.,30)
                 end if
                 EXPECTED = max(0.005454154*TESTH,EXPECTN)
                 if (J.eq.2) EXPECTED = EXPECTM
                 if (OBSERVED.ne.EXPECTED) error stop 'native snag products differ'
              end do
           end do
        end do
     case default
        error stop 'unknown case'
     end select
  end if
  print *, 'PASS ',VARIANTCD,' ',trim(TESTCASE)
end program FOCUSED_BUG_CASES

