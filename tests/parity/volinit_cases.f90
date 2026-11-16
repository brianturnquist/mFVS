!=================================!
!   tests/parity : volinit_cases   !
!=================================!
! Direct cases for check_nvel_parsing.py covering the O09-O11 conversion
! sites. The same driver is linked with the frozen pre-O09 NVEL and with the
! current NVEL. Inputs, results and mutated arguments are written as bits,
! so the two outputs must match byte for byte. The first argument selects
! NVB_EcoProv, GetRegnWF, NVB_DefaultEq, VOLINIT or VOLINITNVB cases, or one
! deliberately invalid field that must stop both builds the same way.
module VOLINIT_EMIT
  implicit none
  ! Variant, region and forest triples whose default equations are used.
  character(len=2), parameter :: VARS(23) = ['IE','EM','KT','CR','CR','UT', &
    'TT','CA','NC','WS','PN','WC','SO','BM','AK','SN','CS','LS','NE','OC', &
    'OP','EC','CI']
  integer, parameter :: REGIONS(23) = [1,1,1,2,3,4,4,5,5,5,6,6,6,6,10,8,9, &
    9,9,5,6,6,4]
  character(len=2), parameter :: FORESTS(23) = ['03','02','14','06','01', &
    '01','03','05','10','15','12','05','01','16','05','05','05','09','22', &
    '05','09','06','02']
  integer, parameter :: SPECIES(25) = [202,122,108,93,19,17,15,263,242,316, &
    318,802,833,131,111,611,621,12,95,129,371,746,375,65,106]
contains
  subroutine EMIT(TAG,TEXT,INTS,REALS)
    character(len=*), intent(in) :: TAG,TEXT
    integer, intent(in) :: INTS(:)
    real, intent(in) :: REALS(:)
    write(*,'(a,"|",a,"|",*(z8.8,:,1x))') TAG,TEXT,[INTS,transfer(REALS,0,size(REALS))]
  end subroutine EMIT

  ! Park-Miller generator: deterministic and free of integer overflow.
  integer function PICK(STATE,N)
    integer(kind=8), intent(inout) :: STATE
    integer, intent(in) :: N
    STATE = mod(48271_8*STATE,2147483647_8)
    PICK = int(mod(STATE,int(N,8)))+1
  end function PICK

  ! Default equation for a variant/region/forest triple and species, called
  ! with local copies in case VOLEQDEF changes its arguments.
  character(len=10) function DEFAULT_EQ(V,SPCD)
    integer, intent(in) :: V,SPCD
    character(len=2) :: VAR,FORST,DIST,PROD
    integer :: REGN,SP,ERR
    VAR = VARS(V)
    REGN = REGIONS(V)
    FORST = FORESTS(V)
    DIST = '01'
    PROD = '01'
    SP = SPCD
    DEFAULT_EQ = ' '
    call VOLEQDEF(VAR,REGN,FORST,DIST,SP,PROD,DEFAULT_EQ,ERR)
  end function DEFAULT_EQ

  ! A 2-digit code in one of three spellings: '05', ' 5' or '5 '.
  character(len=2) function CODE2(N,FORM)
    integer, intent(in) :: N,FORM
    write(CODE2,'(i2.2)') N
    if (FORM.eq.2) write(CODE2,'(i2)') N
    if (FORM.eq.3.and.N.lt.10) write(CODE2,'(i1,1x)') N
  end function CODE2

  ! True when an I2 READ accepts TEXT, after NVB_EcoProv's padding if asked.
  logical function READABLE(TEXT,PAD)
    character(len=2), intent(in) :: TEXT
    logical, intent(in) :: PAD
    character(len=2) :: WORK
    integer :: VALUE,IOS
    WORK = TEXT
    if (PAD.and.WORK(2:2).lt.'0') then
      WORK(2:2) = WORK(1:1)
      WORK(1:1) = '0'
      if (WORK(2:2).lt.'0') WORK(2:2) = '0'
    end if
    read(WORK,'(i2)',iostat=IOS) VALUE
    READABLE = IOS.eq.0
  end function READABLE
end module VOLINIT_EMIT

program VOLINIT_CASES
  implicit none
  character(len=32) :: MODE
  call get_command_argument(1,MODE)
  select case (trim(MODE))
  case ('ecoprov')
    call ECOPROV_CASES
  case ('regnwf')
    call REGNWF_CASES
  case ('defaulteq')
    call DEFAULTEQ_CASES
  case ('volinit')
    call VOLINIT_CASES_RUN
  case ('volinitnvb')
    call VOLINITNVB_CASES_RUN
  case ('bad-forst','bad-han')
    call BAD_FIELD(trim(MODE))
  case default
    stop 'unknown mode'
  end select
end program VOLINIT_CASES

! NVB_EcoProv: every district, forest and regional table row in padded and
! blank spellings, then all two-character FORST/DIST fields its READs accept.
subroutine ECOPROV_CASES
  use VOLINIT_EMIT
  implicit none
  include 'dist_ecoprov.inc'
  character(len=1), parameter :: A(8) = [' ',achar(0),'+','-','0','1','5','9']
  integer, parameter :: SWEEP(6) = [1,2,6,8,9,10]
  character(len=2) :: FORST,DIST,F0,D0
  integer :: ROW,CODE,REGN,FF,DD,FA,FB,DA,DB,R,IPROV,N
  N = 0
  do ROW = 1,DistCnt
    CODE = nint(DistProv(ROW,1))
    REGN = CODE/10000
    FF = mod(CODE/100,100)
    DD = mod(CODE,100)
    do FA = 1,3
      do DA = 1,3
        FORST = CODE2(FF,FA)
        DIST = CODE2(DD,DA)
        call ONE(REGN,FORST,DIST)
      end do
    end do
  end do
  do ROW = 1,ForstCnt
    CODE = nint(ForstProv(ROW,1))
    call ONE(CODE/100,CODE2(mod(CODE,100),1),'99')
  end do
  do R = 1,size(SWEEP)
    do FA = 1,size(A)
      do FB = 1,size(A)
        do DA = 1,size(A)
          do DB = 1,size(A)
            F0 = A(FA)//A(FB)
            D0 = A(DA)//A(DB)
            if (READABLE(F0,.true.).and.READABLE(D0,.true.)) call ONE(SWEEP(R),F0,D0)
          end do
        end do
      end do
    end do
  end do
  write(*,'(a,i8)') 'COUNT NVB_EcoProv calls',N
contains
  subroutine ONE(REGN_IN,FORST_IN,DIST_IN)
    integer, intent(in) :: REGN_IN
    character(len=2), intent(in) :: FORST_IN,DIST_IN
    character(len=2) :: F,D
    integer :: RG
    RG = REGN_IN
    F = FORST_IN
    D = DIST_IN
    IPROV = -77777
    call NVB_EcoProv(RG,F,D,IPROV)
    call EMIT('E',FORST_IN//DIST_IN//'|'//F//D,[RG,IPROV],[real::])
    N = N+1
  end subroutine ONE
end subroutine ECOPROV_CASES

! GetRegnWF: every regional weight-factor row in three forest spellings and
! two products, absent species and regions, and an all-field FORST sweep.
subroutine REGNWF_CASES
  use VOLINIT_EMIT
  implicit none
  include 'regndftdata.inc'
  character(len=1), parameter :: A(8) = [' ',achar(0),'+','-','0','1','5','9']
  character(len=2), parameter :: PRODS(3) = ['01','02','20']
  character(len=2) :: F0
  integer :: ROW,FORM,P,K,R,FA,FB,N
  N = 0
  do ROW = 1,TOTDFT
    do FORM = 1,3
      do P = 1,size(PRODS)
        call ONE(nint(SPREGNDFTWF(ROW,1)),CODE2(nint(SPREGNDFTWF(ROW,2)),FORM), &
          nint(SPREGNDFTWF(ROW,3)),PRODS(P))
      end do
    end do
  end do
  do R = 0,10
    do K = 1,size(SPECIES)
      call ONE(R,'01',SPECIES(K),'01')
      call ONE(R,'99',SPECIES(K),'20')
    end do
  end do
  do FA = 1,size(A)
    do FB = 1,size(A)
      F0 = A(FA)//A(FB)
      if (READABLE(F0,.false.)) then
        do R = 1,10
          call ONE(R,F0,202,'01')
        end do
      end if
    end do
  end do
  write(*,'(a,i8)') 'COUNT GetRegnWF calls',N
contains
  subroutine ONE(REGN_IN,FORST_IN,SPCD_IN,PROD_IN)
    integer, intent(in) :: REGN_IN,SPCD_IN
    character(len=2), intent(in) :: FORST_IN,PROD_IN
    character(len=2) :: F,PR
    integer :: RG,SP
    real :: WTFAC,DEADWF
    RG = REGN_IN
    F = FORST_IN
    SP = SPCD_IN
    PR = PROD_IN
    WTFAC = -7.5e5
    DEADWF = -7.5e5
    call GetRegnWF(RG,F,SP,WTFAC,DEADWF,PR)
    call EMIT('G',FORST_IN//PROD_IN//'|'//F//PR,[RG,SP],[WTFAC,DEADWF])
    N = N+1
  end subroutine ONE
end subroutine REGNWF_CASES

! NVB_DefaultEq: every district row in nine FORST/DIST spellings with valid,
! absent, one-digit and four-digit species codes.
subroutine DEFAULTEQ_CASES
  use VOLINIT_EMIT
  implicit none
  include 'dist_ecoprov.inc'
  integer, parameter :: EXTRA(8) = [0,5,9,10,999,1000,2042,4999]
  character(len=2) :: FORST,DIST
  character(len=10) :: NVBEQ
  integer :: ROW,CODE,REGN,FA,DA,K,SPCD,ERRFLG,N,RG
  N = 0
  do ROW = 1,DistCnt
    CODE = nint(DistProv(ROW,1))
    do FA = 1,3
      do DA = 1,3
        do K = 1,size(SPECIES)+size(EXTRA)
          if (K.le.size(SPECIES)) then
            SPCD = SPECIES(K)
          else
            SPCD = EXTRA(K-size(SPECIES))
          end if
          RG = CODE/10000
          FORST = CODE2(mod(CODE/100,100),FA)
          DIST = CODE2(mod(CODE,100),DA)
          NVBEQ = 'XXXXXXXXXX'
          ERRFLG = -77777
          call NVB_DefaultEq(RG,FORST,DIST,SPCD,NVBEQ,ERRFLG)
          call EMIT('D',CODE2(mod(CODE/100,100),FA)//CODE2(mod(CODE,100),DA)// &
            '|'//FORST//DIST//NVBEQ,[RG,SPCD,ERRFLG],[real::])
          N = N+1
        end do
      end do
    end do
  end do
  write(*,'(a,i8)') 'COUNT NVB_DefaultEq calls',N
end subroutine DEFAULTEQ_CASES

! VOLINIT: default equations across variants, regions and species, FIA CU/BD
! equations with each CONSPEC width, Hann equations, a blank equation and
! cruise log weights, with padded, blank and NUL forest spellings.
subroutine VOLINIT_CASES_RUN
  use VOLINIT_EMIT
  implicit none
  character(len=10), parameter :: FIAEQ(6) = ['CU000059  ','CU000060  ', &
    'CU000111  ','BD000044  ','BD000045  ','cu000060  ']
  character(len=4), parameter :: CONSPECS(9) = ['316 ','0316','31 6','12  ', &
    '1   ','9999','A316','  12','    ']
  ! Hann equations; a leading 'B' would take VOLINIT's earlier BLM branch.
  character(len=10), parameter :: HANEQ(5) = ['H32HAN0202','H16HAN0122', &
    'H16HAN0 12','H16HAN01 2','H16HAN0   ']
  ! Cruise weights pass the species to NVB_RefSpcData, whose outputs are
  ! undefined below 10 (FOUND_BUGS.md, bug 52), so these spellings all give
  ! 0 (then the equation's species) or a code of 10 or more.
  character(len=4), parameter :: CRUISE(7) = ['316 ','0316','31 6','12  ', &
    '9999','A316','    ']
  character(len=2) :: FORMS(4)
  character(len=10) :: EQ10
  integer :: V,K,T,F,N,C,ERR,CODE
  integer :: COUNTS(2)
  N = 0
  COUNTS = 0
  do V = 1,size(VARS)
    FORMS = [FORESTS(V),FORESTS(V)(2:2)//' ',' '//FORESTS(V)(2:2),achar(0)//'5']
    do K = 1,size(SPECIES)
      EQ10 = DEFAULT_EQ(V,SPECIES(K))
      do T = 1,3
        F = 1+mod(K+T,4)
        call ONE(REGIONS(V),FORMS(F),EQ10,'    ',T,'F',1+mod(K,2))
      end do
      ! Cruise log weights read the species from CONSPEC or the equation.
      ! Spellings that defer to the equation need a valid species field.
      C = 1+mod(K,7)
      read(EQ10(8:10),'(i3)',iostat=ERR) CODE
      if (C.ge.6.and.(ERR.ne.0.or.CODE.lt.10)) C = 1+mod(K,5)
      call ONE(REGIONS(V),FORMS(1),EQ10,CRUISE(C),2,'C',1)
    end do
  end do
  do K = 1,size(FIAEQ)
    do C = 1,size(CONSPECS)
      call ONE(9,'09',FIAEQ(K),CONSPECS(C),2,'F',1)
    end do
  end do
  do K = 1,size(HANEQ)
    call ONE(7,'05',HANEQ(K),'    ',2,'F',1)
    call ONE(7,'05',HANEQ(K),'    ',3,'F',2)
  end do
  do C = 1,size(CONSPECS)
    if (CONSPECS(C)(1:1).eq.'A') cycle
    call ONE(6,'12','          ',CONSPECS(C),2,'F',1)
  end do
  write(*,'(a,i8,a,2i8)') 'COUNT VOLINIT calls',N,'; ERRFLAG 0 / nonzero',COUNTS
contains
  subroutine ONE(REGN_IN,FORST_IN,VOLEQ_IN,CONSPEC_IN,SIZE_IN,CTYPE_IN,STUMP_IN)
    integer, intent(in) :: REGN_IN,SIZE_IN,STUMP_IN
    character(len=*), intent(in) :: FORST_IN,VOLEQ_IN,CONSPEC_IN,CTYPE_IN
    real, parameter :: DBHS(3) = [6.0,14.0,28.0], HTS(3) = [40.0,85.0,120.0]
    real, parameter :: STUMPS(2) = [1.0,0.5]
    integer :: REGN,HTLOG,HTREF,FCLASS,TLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
    integer :: HTTFLL,BA,SI,ERRFLAG,IDIST
    real :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD,UPSHT1,UPSHT2
    real :: UPSD1,UPSD2,AVGZ1,AVGZ2,DBTBH,BTR,NOLOGP,NOLOGS
    real :: VOL(15),LOGVOL(7,20),LOGDIA(21,3),LOGLEN(20),BOLHT(21)
    character(len=10) :: VOLEQ
    character(len=4) :: CONSPEC
    character(len=2) :: FORST,PROD
    character(len=1) :: HTTYPE,LIVE,CTYPE
    REGN = REGN_IN
    FORST = FORST_IN
    VOLEQ = VOLEQ_IN
    CONSPEC = CONSPEC_IN
    CTYPE = CTYPE_IN
    MTOPP = merge(4.0,6.0,SIZE_IN.eq.1)
    MTOPS = 4.0
    STUMP = STUMPS(STUMP_IN)
    DBHOB = DBHS(SIZE_IN)
    DRCOB = 0.0
    HTTYPE = 'F'
    HTTOT = HTS(SIZE_IN)
    HTLOG = 0
    HT1PRD = 0.0
    HT2PRD = 0.0
    UPSHT1 = 0.0
    UPSHT2 = 0.0
    UPSD1 = 0.0
    UPSD2 = 0.0
    HTREF = 0
    AVGZ1 = 0.0
    AVGZ2 = 0.0
    FCLASS = merge(80,0,SIZE_IN.eq.2)
    DBTBH = 0.0
    BTR = 0.0
    CUTFLG = 1
    BFPFLG = 1
    CUPFLG = 1
    CDPFLG = 1
    SPFLG = 1
    PROD = '01'
    HTTFLL = 0
    LIVE = 'L'
    BA = 0
    SI = 0
    ERRFLAG = -77777
    IDIST = mod(SIZE_IN,3)*6
    TLOGS = -77777
    NOLOGP = -7.5e5
    NOLOGS = -7.5e5
    VOL = -7.5e5
    LOGVOL = -7.5e5
    LOGDIA = -7.5e5
    LOGLEN = -7.5e5
    BOLHT = -7.5e5
    call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTYPE,HTTOT, &
      HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS, &
      DBTBH,BTR,3,7,15,20,21,VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP, &
      NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,BA,SI, &
      CTYPE,ERRFLAG,IDIST)
    call EMIT('V',VOLEQ_IN//FORST_IN//CONSPEC_IN//CTYPE_IN//'|'//VOLEQ//FORST// &
      CONSPEC//PROD,[REGN,ERRFLAG,TLOGS,IDIST,BA,SI,FCLASS], &
      [MTOPP,MTOPS,STUMP,DBHOB,HTTOT,HT1PRD,HT2PRD,NOLOGP,NOLOGS,VOL, &
      reshape(LOGVOL,[140]),reshape(LOGDIA,[63]),LOGLEN,BOLHT])
    N = N+1
    if (ERRFLAG.eq.0) then
      COUNTS(1) = COUNTS(1)+1
    else
      COUNTS(2) = COUNTS(2)+1
    end if
  end subroutine ONE
end subroutine VOLINIT_CASES_RUN

! VOLINITNVB: default, NSVB, FIA and blank equations; FIA codes supplied,
! omitted or remapped; DBH-only, broken-top, woodland, dead and decayed trees;
! every cruise type; district numbers 0-99 and padded or blank forests.
subroutine VOLINITNVB_CASES_RUN
  use VOLINIT_EMIT
  use MRULES_MOD
  implicit none
  character(len=11), parameter :: NVBEQ(4) = ['NVB0240202 ','NVBM2400122', &
    'NVB0330316P','NVB0000131P']
  character(len=1), parameter :: CTYPES(4) = ['F','I','C','B']
  integer, parameter :: DISTS(6) = [0,1,6,12,57,99]
  ! Plausible diameter/height pairs, plus DBH-only trees (height 0). Shorter
  ! trees can reach the unrepaired R9DIB underflow (FOUND_BUGS.md, bug 6).
  real, parameter :: DBHS(10) = [3.0,3.0,6.0,9.0,14.5,22.0,22.0,35.0,9.0,14.5]
  real, parameter :: HTS(10) = [0.0,20.0,40.0,40.0,75.0,75.0,110.0,110.0,0.0,0.0]
  integer(kind=8) :: STATE
  character(len=11) :: VOLEQ
  character(len=2) :: FORMS(4)
  integer :: N,I,V,K,FIASPCD,SUCCEEDED,ERR,CODE
  N = 0
  SUCCEEDED = 0
  STATE = 1611_8
  do I = 1,4000
    V = PICK(STATE,size(VARS))
    K = PICK(STATE,size(SPECIES))
    FORMS = [FORESTS(V),FORESTS(V)(2:2)//' ',' '//FORESTS(V)(2:2),achar(0)//'5']
    select case (PICK(STATE,10))
    case (1,2)
      VOLEQ = NVBEQ(PICK(STATE,size(NVBEQ)))
    case (3)
      VOLEQ = 'CU000060'
    case (4)
      VOLEQ = ' '
    case default
      VOLEQ = DEFAULT_EQ(V,SPECIES(K))
    end select
    select case (PICK(STATE,8))
    case (1,2)
      ! Omitted codes are read from the equation; keep them to equations
      ! whose species field is a valid code (bug 52 covers codes below 10).
      FIASPCD = 0
      read(VOLEQ(8:10),'(i3)',iostat=ERR) CODE
      if (ERR.ne.0.or.CODE.lt.10.or.VOLEQ(1:3).ne.'NVB'.and.VOLEQ(1:2).eq.'CU') &
        FIASPCD = SPECIES(K)
    case (3)
      ! 2042 is remapped to 42; 5091 is a genuine 4-digit FIA code, written
      ! with I4 and overflowing NVB_DefaultEq's I3 species field.
      FIASPCD = merge(2042,5091,PICK(STATE,2).eq.1)
    case (4)
      FIASPCD = 204
    case default
      FIASPCD = SPECIES(K)
    end select
    call ONE(REGIONS(V),FORMS(PICK(STATE,4)),VOLEQ,FIASPCD)
  end do
  write(*,'(a,i8,a,i8)') 'COUNT VOLINITNVB calls',N,'; ERRFLAG 0',SUCCEEDED
contains
  subroutine ONE(REGN_IN,FORST_IN,VOLEQ_IN,FIA_IN)
    integer, intent(in) :: REGN_IN,FIA_IN
    character(len=*), intent(in) :: FORST_IN,VOLEQ_IN
    integer :: REGN,HTLOG,HTREF,FCLASS,DECAYCD,TLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG
    integer :: SPFLG,HTTFLL,BA,SI,ERRFLAG,IDIST,FIASPCD,MRULEFLG,P
    real :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1
    real :: UPSD2,AVGZ1,AVGZ2,DBTBH,BTR,CR,CULL,NOLOGP,NOLOGS,BRKHT,BRKHTD,CULLMSTOP
    real :: VOL(15),LOGVOL(7,20),LOGDIA(21,3),LOGLEN(20),BOLHT(21),DRYBIO(15),GRNBIO(15)
    character(len=11) :: VOLEQ
    character(len=4) :: CONSPEC
    character(len=2) :: FORST,PROD
    character(len=1) :: HTTYPE,LIVE,CTYPE
    type(MERCHRULES) :: MERRULES
    REGN = REGN_IN
    FORST = FORST_IN
    VOLEQ = VOLEQ_IN
    FIASPCD = FIA_IN
    CONSPEC = '    '
    P = PICK(STATE,size(DBHS))
    DBHOB = DBHS(P)
    HTTOT = HTS(P)
    ! A DBH-only tree with no FIA code also reaches bug 52; give it a height.
    if (FIASPCD.eq.0.and.HTTOT.eq.0.0) HTTOT = 40.0
    DRCOB = 0.0
    HTTYPE = 'F'
    HTLOG = 0
    HT1PRD = 0.0
    HT2PRD = 0.0
    UPSHT1 = 0.0
    UPSHT2 = 0.0
    UPSD1 = 0.0
    UPSD2 = 0.0
    HTREF = 0
    AVGZ1 = 0.0
    AVGZ2 = 0.0
    FCLASS = merge(80,0,PICK(STATE,2).eq.1)
    DBTBH = 0.0
    BTR = 0.0
    MTOPP = merge(4.0,6.0,PICK(STATE,2).eq.1)
    MTOPS = 4.0
    STUMP = merge(0.5,1.0,PICK(STATE,2).eq.1)
    CR = merge(0.0,40.0,PICK(STATE,3).eq.1)
    CULL = merge(10.0,0.0,PICK(STATE,4).eq.1)
    DECAYCD = PICK(STATE,6)-1
    LIVE = merge('D','L',PICK(STATE,3).eq.1)
    CTYPE = CTYPES(PICK(STATE,size(CTYPES)))
    PROD = merge('02','01',PICK(STATE,3).eq.1)
    IDIST = DISTS(PICK(STATE,size(DISTS)))
    BRKHT = 0.0
    BRKHTD = 0.0
    ! Draw first: .and. need not short-circuit, so the draw count stays fixed.
    P = PICK(STATE,4)
    if (HTTOT.gt.0.0.and.P.eq.1) then
      BRKHT = 0.6*HTTOT
      BRKHTD = 0.3*DBHOB
    end if
    CULLMSTOP = merge(5.0,0.0,PICK(STATE,5).eq.1)
    CUTFLG = 1
    BFPFLG = 1
    CUPFLG = 1
    CDPFLG = 1
    SPFLG = 1
    HTTFLL = 0
    BA = 0
    SI = 0
    MRULEFLG = 0
    ERRFLAG = -77777
    TLOGS = -77777
    NOLOGP = -7.5e5
    NOLOGS = -7.5e5
    VOL = -7.5e5
    LOGVOL = -7.5e5
    LOGDIA = -7.5e5
    LOGLEN = -7.5e5
    BOLHT = -7.5e5
    DRYBIO = -7.5e5
    GRNBIO = -7.5e5
    call VOLINITNVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTYPE,HTTOT, &
      HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS, &
      DBTBH,BTR,CR,CULL,DECAYCD,VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP, &
      NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,BA,SI, &
      CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,GRNBIO,MRULEFLG,MERRULES, &
      CULLMSTOP)
    call EMIT('N',VOLEQ_IN//FORST_IN//'|'//VOLEQ//FORST//CONSPEC//PROD//LIVE// &
      CTYPE,[REGN,ERRFLAG,TLOGS,IDIST,FIASPCD,DECAYCD,FCLASS], &
      [MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD,CR,CULL,BRKHT,BRKHTD, &
      NOLOGP,NOLOGS,VOL,DRYBIO,GRNBIO,reshape(LOGVOL,[140]), &
      reshape(LOGDIA,[63]),LOGLEN,BOLHT])
    N = N+1
    if (ERRFLAG.eq.0) SUCCEEDED = SUCCEEDED+1
  end subroutine ONE
end subroutine VOLINITNVB_CASES_RUN

! Fields that VOLINIT's READs reject. Both builds must stop with the same
! runtime error; only its source line may differ.
subroutine BAD_FIELD(MODE)
  implicit none
  character(len=*), intent(in) :: MODE
  integer :: REGN,HTLOG,HTREF,FCLASS,TLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
  integer :: HTTFLL,BA,SI,ERRFLAG,IDIST
  real :: MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD,UPSHT1,UPSHT2
  real :: UPSD1,UPSD2,AVGZ1,AVGZ2,DBTBH,BTR,NOLOGP,NOLOGS
  real :: VOL(15),LOGVOL(7,20),LOGDIA(21,3),LOGLEN(20),BOLHT(21)
  character(len=10) :: VOLEQ
  character(len=4) :: CONSPEC
  character(len=2) :: FORST,PROD
  character(len=1) :: HTTYPE,LIVE,CTYPE
  REGN = 6
  FORST = '12'
  VOLEQ = 'H16HAN0202'
  if (MODE.eq.'bad-forst') FORST = 'AB'
  if (MODE.eq.'bad-han') VOLEQ = 'H16HAN0ABC'
  CONSPEC = '    '
  MTOPP = 6.0
  MTOPS = 4.0
  STUMP = 1.0
  DBHOB = 14.0
  DRCOB = 0.0
  HTTYPE = 'F'
  HTTOT = 85.0
  HTLOG = 0
  HT1PRD = 0.0
  HT2PRD = 0.0
  UPSHT1 = 0.0
  UPSHT2 = 0.0
  UPSD1 = 0.0
  UPSD2 = 0.0
  HTREF = 0
  AVGZ1 = 0.0
  AVGZ2 = 0.0
  FCLASS = 0
  DBTBH = 0.0
  BTR = 0.0
  CUTFLG = 1
  BFPFLG = 1
  CUPFLG = 1
  CDPFLG = 1
  SPFLG = 1
  PROD = '01'
  HTTFLL = 0
  LIVE = 'L'
  BA = 0
  SI = 0
  CTYPE = 'F'
  IDIST = 1
  call VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTYPE,HTTOT, &
    HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS, &
    DBTBH,BTR,3,7,15,20,21,VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP, &
    NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,BA,SI, &
    CTYPE,ERRFLAG,IDIST)
  write(*,*) 'invalid field was accepted'
end subroutine BAD_FIELD
