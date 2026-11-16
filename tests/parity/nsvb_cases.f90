!==============================!
!   tests/parity : nsvb_cases   !
!==============================!
! Direct NSVB cases for check_nvel_parsing.py. The same driver is linked with
! the frozen pre-optimization NVEL and with the current NVEL. Every input and
! result is written as bits, so the two outputs must match byte for byte.
! The first argument selects SPEQCOEF, CALC_EQVW or NVBC cases, or one
! deliberately invalid field that must stop both builds the same way.
module NSVB_EMIT
  implicit none
contains
  subroutine EMIT(TAG,TEXT,INTS,REALS)
    character(len=*), intent(in) :: TAG,TEXT
    integer, intent(in) :: INTS(:)
    real, intent(in) :: REALS(:)
    write(*,'(a,"|",a,"|",*(z8.8,:,1x))') TAG,TEXT,[INTS,transfer(REALS,0,size(REALS))]
  end subroutine EMIT

  ! Every 3-digit species code in a table, codes absent from it, and fields
  ! that the original I3 READ accepts but DIGITREAD leaves to that READ.
  subroutine SPECIES_FIELDS(SP,CNT,EXTRA,FIELDS,N)
    integer, intent(in) :: CNT
    real, intent(in) :: SP(CNT)
    integer, intent(in) :: EXTRA(:)
    character(len=3), intent(out) :: FIELDS(:)
    integer, intent(out) :: N
    character(len=3), parameter :: ODD(7) = [character(len=3) :: &
      ' 12','12 ','1 2','+12','-12','   ','+5 ']
    integer :: I,CODE
    N = 0
    do I = 1,CNT+size(EXTRA)
      if (I.le.CNT) then
        CODE = nint(SP(I))
      else
        CODE = EXTRA(I-CNT)
      end if
      if (CODE.lt.0.or.CODE.gt.999) cycle
      if (N.gt.0) then
        if (any(FIELDS(1:N).eq.DIGITS3(CODE))) cycle
      end if
      N = N+1
      FIELDS(N) = DIGITS3(CODE)
    end do
    FIELDS(N+1:N+size(ODD)) = ODD
    N = N+size(ODD)
  end subroutine SPECIES_FIELDS

  character(len=3) function DIGITS3(CODE)
    integer, intent(in) :: CODE
    write(DIGITS3,'(i3.3)') CODE
  end function DIGITS3

  ! Park-Miller generator: deterministic and free of integer overflow.
  integer function PICK(STATE,N)
    integer(kind=8), intent(inout) :: STATE
    integer, intent(in) :: N
    STATE = mod(48271_8*STATE,2147483647_8)
    PICK = int(mod(STATE,int(N,8)))+1
  end function PICK
end module NSVB_EMIT

program NSVB_CASES
  implicit none
  character(len=32) :: MODE
  call get_command_argument(1,MODE)
  select case (trim(MODE))
  case ('speqcoef')
    call SPEQ_CASES
  case ('calc')
    call CALC_CASES
  case ('nvbc')
    call NVBC_CASES
  case ('bad-speqcoef','bad-calc','bad-nvbc')
    call BAD_FIELD(trim(MODE))
  case default
    stop 'unknown mode'
  end select
end program NSVB_CASES

! SPEQCOEF: normalized and nonnormalized VOLEQ(5:7), 'M' divisions, planted
! (STDORG) and other eleventh characters, missing species and READ-only fields.
subroutine SPEQ_CASES
  use NSVB_EMIT
  implicit none
  character(len=3), parameter :: DIVS(9) = [character(len=3) :: &
    '000','240','241','330','124',' 4 ','X2Z','24 ','-24']
  character(len=1), parameter :: FOURTH(2) = ['0','M']
  character(len=1), parameter :: LAST(4) = [' ','P','1','X']
  real :: SP(13*500),JK(9,5),R(9)
  integer :: K,CNT,I,J,L,M,NSP,SPCD,EQN,FOUND,MISSING
  character(len=3) :: FIELDS(600)
  character(len=11) :: VOLEQ,INPUT

  FOUND = 0
  MISSING = 0
  do K = 1,9
    call GETTABLE(K,CNT,SP,JK)
    call SPECIES_FIELDS(SP,CNT,[0,1,5,9,13,50,123,404,997,998,999],FIELDS,NSP)
    do I = 1,NSP
      do J = 1,size(DIVS)
        do L = 1,size(FOURTH)
          do M = 1,size(LAST)
            INPUT = 'NVB'//FOURTH(L)//DIVS(J)//FIELDS(I)//LAST(M)
            VOLEQ = INPUT
            SPCD = -77777
            EQN = -77777
            R = -7.5e5
            call SPEQCOEF(VOLEQ,CNT,SP,SPCD,EQN,R(1),R(2),R(3),R(4),R(5), &
              R(6),R(7),R(8),R(9))
            call EMIT('S'//achar(48+K),INPUT//'|'//VOLEQ,[SPCD,EQN],R)
            if (EQN.eq.0) then
              MISSING = MISSING+1
            else
              FOUND = FOUND+1
            end if
          end do
        end do
      end do
    end do
  end do
  write(*,'(a,2i8)') 'COUNT SPEQCOEF matched/unmatched',FOUND,MISSING
end subroutine SPEQ_CASES

! CALC_EQVW: every volume/weight table, Jenkins-group species codes, missing
! species with Jenkins and species-999 fallbacks, and short-tree returns.
subroutine CALC_CASES
  use NSVB_EMIT
  implicit none
  integer, parameter :: TABLES(7) = [1,2,3,6,7,8,9]
  character(len=4), parameter :: PREFIX(4) = ['0240','M240','0X2Z','0000']
  character(len=1), parameter :: SUFFIX(4) = [' ','P',' ','P']
  real, parameter :: D(5) = [0.5,7.3,1.0,7.3,24.0]
  real, parameter :: H(5) = [40.0,4.9,5.0,38.5,110.0]
  integer, parameter :: GROUP(3) = [0,3,10]
  real, parameter :: DENSITY(3) = [0.0,31.2,25.0]
  real :: SP(13*500),JK(9,5),VW,WDSG
  integer :: T,K,CNT,I,J,L,M,NSP,ERRFLG,SPGRPCD,COMPUTED
  character(len=3) :: FIELDS(600)
  character(len=11) :: VOLEQ,INPUT

  COMPUTED = 0
  do T = 1,size(TABLES)
    K = TABLES(T)
    call GETTABLE(K,CNT,SP,JK)
    call SPECIES_FIELDS(SP,CNT,[0,1,2,3,4,5,6,7,8,9,13,404,997],FIELDS,NSP)
    do I = 1,NSP
      do J = 1,size(PREFIX)
        do L = 1,size(D)
          do M = 1,size(GROUP)
            INPUT = 'NVB'//PREFIX(J)//FIELDS(I)//SUFFIX(J)
            VOLEQ = INPUT
            SPGRPCD = GROUP(M)
            WDSG = DENSITY(M)
            VW = -7.5e5
            ERRFLG = -77777
            call CALC_EQVW(VOLEQ,D(L),H(L),CNT,SP,JK,VW,ERRFLG,SPGRPCD,WDSG)
            call EMIT('C'//achar(48+K),INPUT//'|'//VOLEQ,[ERRFLG,SPGRPCD], &
              [D(L),H(L),VW,WDSG])
            if (ERRFLG.eq.0) COMPUTED = COMPUTED+1
          end do
        end do
      end do
    end do
  end do
  write(*,'(a,i8)') 'COUNT CALC_EQVW computed',COMPUTED
end subroutine CALC_CASES

! NVBC volume and biomass: a structured set over equation strings, then a
! deterministic sample over tree, region, product, decay and cruise inputs.
subroutine NVBC_CASES
  use NSVB_EMIT
  implicit none
  character(len=3), parameter :: SPECIES(12) = [character(len=3) :: &
    '015','064','122','202','263','316','611','802','833','065','999','013']
  character(len=4), parameter :: PREFIX(8) = [character(len=4) :: &
    '0240','M240','0330','0331','0 30','0000','M210','0120']
  character(len=4), parameter :: FOURDIGIT(4) = ['0202','2042','0064','9999']
  integer, parameter :: REGIONS(9) = [1,2,3,4,5,6,8,9,10]
  character(len=2), parameter :: FORESTS(5) = ['01','05','12','1 ',' 7']
  character(len=2), parameter :: DISTRICTS(3) = ['01','1 ','03']
  character(len=2), parameter :: PRODUCTS(3) = ['01','02','20']
  real, parameter :: DBHS(6) = [1.5,4.0,9.0,14.5,22.0,35.0]
  real, parameter :: HEIGHTS(5) = [0.0,12.0,35.0,70.0,120.0]
  character(len=1), parameter :: CTYPES(4) = ['F','I','C','B']
  integer(kind=8) :: STATE
  integer :: I,J,L,M,N,SUCCEEDED,FIASPCD,SPCODE,ESTIMATE
  integer :: REGN
  real :: DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,STUMP,BRKHT,BRKHTD,CR,CULL
  integer :: DECAYCD
  character(len=1) :: LIVE,CTYPE,ELEVENTH
  character(len=2) :: FORST,DIST,PROD
  character(len=11) :: INPUT
  character(len=3) :: FIELD

  SUCCEEDED = 0
  ! Structured cases: one ordinary tree for every equation-string form.
  do I = 1,size(SPECIES)
    FIELD = SPECIES(I)
    read(FIELD,'(i3)') SPCODE
    do J = 1,size(PREFIX)
      do L = 1,2
        ELEVENTH = merge(' ','P',L.eq.1)
        do M = 1,2
          FIASPCD = merge(0,SPCODE,M.eq.1)
          call ONE_TREE('NVB'//PREFIX(J)//SPECIES(I)//ELEVENTH,6,'05','01',14.5,88.0, &
            0.0,0.0,6.0,4.0,1.0,'01',0.0,0.0,'L',40.0,0.0,0,FIASPCD,'F')
        end do
      end do
    end do
  end do
  ! Four-digit species fields use the I4 READ when FIASPCD is not supplied.
  do I = 1,size(FOURDIGIT)
    do J = 1,2
      call ONE_TREE('NVB'//PREFIX(J)//FOURDIGIT(I),6,'05','01',14.5,88.0, &
        0.0,0.0,6.0,4.0,1.0,'01',0.0,0.0,'L',40.0,0.0,0,0,'F')
    end do
  end do
  ! Sampled cases.
  STATE = 20261116_8
  do N = 1,6000
    I = PICK(STATE,size(SPECIES))
    FIELD = SPECIES(I)
    read(FIELD,'(i3)') SPCODE
    ELEVENTH = merge(' ','P',PICK(STATE,3).ne.1)
    if (PICK(STATE,10).eq.1) then
      J = PICK(STATE,2)
      INPUT = 'NVB'//PREFIX(J)//FOURDIGIT(PICK(STATE,size(FOURDIGIT)))
      FIASPCD = 0
    else
      INPUT = 'NVB'//PREFIX(PICK(STATE,size(PREFIX)))//SPECIES(I)//ELEVENTH
      select case (PICK(STATE,3))
      case (1)
        FIASPCD = 0
      case (2)
        FIASPCD = SPCODE
      case default
        FIASPCD = 122
      end select
    end if
    REGN = REGIONS(PICK(STATE,size(REGIONS)))
    FORST = FORESTS(PICK(STATE,size(FORESTS)))
    DIST = DISTRICTS(PICK(STATE,size(DISTRICTS)))
    PROD = PRODUCTS(PICK(STATE,size(PRODUCTS)))
    DBHOB = DBHS(PICK(STATE,size(DBHS)))
    HTTOT = HEIGHTS(PICK(STATE,size(HEIGHTS)))
    MTOPP = merge(6.0,7.6,PICK(STATE,2).eq.1)
    MTOPS = 4.0
    STUMP = merge(0.5,1.0,PICK(STATE,2).eq.1)
    HT1PRD = 0.0
    HT2PRD = 0.0
    BRKHT = 0.0
    BRKHTD = 0.0
    if (PICK(STATE,4).eq.1) then
      BRKHT = max(4.6,0.6*HTTOT)
      BRKHTD = 0.35*DBHOB
    end if
    if (HTTOT.eq.0.0) then
      ESTIMATE = PICK(STATE,3)
      if (ESTIMATE.eq.1) HT2PRD = 25.0
      if (ESTIMATE.eq.2) then
        BRKHT = 20.0
        BRKHTD = 0.3*DBHOB
      end if
    end if
    LIVE = merge('L','D',PICK(STATE,3).ne.1)
    DECAYCD = PICK(STATE,6)-1
    CR = real(30*(PICK(STATE,3)-1))
    CULL = merge(0.0,8.0,PICK(STATE,2).eq.1)
    CTYPE = CTYPES(PICK(STATE,size(CTYPES)))
    call ONE_TREE(INPUT,REGN,FORST,DIST,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS, &
      STUMP,PROD,BRKHT,BRKHTD,LIVE,CR,CULL,DECAYCD,FIASPCD,CTYPE)
  end do
  write(*,'(a,i8)') 'COUNT NVBC without error',SUCCEEDED
contains
  subroutine ONE_TREE(VOLEQ_IN,REGN_IN,FORST_IN,DIST_IN,DBH_IN,HT_IN,HT1_IN, &
    HT2_IN,MTOPP_IN,MTOPS_IN,STUMP_IN,PROD_IN,BRKHT_IN,BRKHTD_IN,LIVE_IN, &
    CR_IN,CULL_IN,DECAY_IN,FIA_IN,CTYPE_IN)
    character(len=*), intent(in) :: VOLEQ_IN,FORST_IN,DIST_IN,PROD_IN,LIVE_IN,CTYPE_IN
    integer, intent(in) :: REGN_IN,DECAY_IN,FIA_IN
    real, intent(in) :: DBH_IN,HT_IN,HT1_IN,HT2_IN,MTOPP_IN,MTOPS_IN,STUMP_IN
    real, intent(in) :: BRKHT_IN,BRKHTD_IN,CR_IN,CULL_IN
    character(len=11) :: VOLEQ
    character(len=2) :: TFORST,TDIST,TPROD
    character(len=1) :: TLIVE,TCTYPE
    integer :: TREGN,TDECAY,TFIA,LOGST,ERRFLG
    real :: D,H,H1,H2,TOPP,TOPS,STMP,BRK,BRKD,TCR,TCULL,NOLOGP,NOLOGS
    real :: LOGLEN(20),LOGDIA(21,3),LOGVOL(7,20),BOLHT(21)
    real :: VOL(15),DRYBIO(15),GRNBIO(15)
    VOLEQ = VOLEQ_IN
    TFORST = FORST_IN
    TDIST = DIST_IN
    TPROD = PROD_IN
    TLIVE = LIVE_IN
    TCTYPE = CTYPE_IN
    TREGN = REGN_IN
    TDECAY = DECAY_IN
    TFIA = FIA_IN
    D = DBH_IN
    H = HT_IN
    H1 = HT1_IN
    H2 = HT2_IN
    TOPP = MTOPP_IN
    TOPS = MTOPS_IN
    STMP = STUMP_IN
    BRK = BRKHT_IN
    BRKD = BRKHTD_IN
    TCR = CR_IN
    TCULL = CULL_IN
    LOGST = -77777
    ERRFLG = -77777
    NOLOGP = -7.5e5
    NOLOGS = -7.5e5
    LOGLEN = -7.5e5
    LOGDIA = -7.5e5
    LOGVOL = -7.5e5
    BOLHT = -7.5e5
    VOL = -7.5e5
    DRYBIO = -7.5e5
    GRNBIO = -7.5e5
    call NVBC(TREGN,TFORST,TDIST,VOLEQ,D,H,TOPP,TOPS,H1,H2,STMP,TPROD,BRK,BRKD, &
      TLIVE,TCR,TCULL,TDECAY,LOGLEN,LOGDIA,LOGVOL,BOLHT,LOGST,NOLOGP,NOLOGS, &
      VOL,DRYBIO,GRNBIO,ERRFLG,TFIA,TCTYPE)
    call EMIT('N',VOLEQ_IN//'|'//VOLEQ//TFORST//TDIST//TPROD//TLIVE//TCTYPE, &
      [TREGN,TDECAY,TFIA,LOGST,ERRFLG], &
      [D,H,H1,H2,TOPP,TOPS,STMP,BRK,BRKD,TCR,TCULL,NOLOGP,NOLOGS,VOL,DRYBIO, &
      GRNBIO,LOGLEN,reshape(LOGDIA,[63]),reshape(LOGVOL,[140]),BOLHT])
    if (ERRFLG.eq.0) SUCCEEDED = SUCCEEDED+1
  end subroutine ONE_TREE
end subroutine NVBC_CASES

! Fields that the original READ rejects. Both builds must stop with the same
! runtime error; only its source line may differ.
subroutine BAD_FIELD(MODE)
  implicit none
  character(len=*), intent(in) :: MODE
  real :: SP(13*500),JK(9,5),R(9),VW,WDSG,NOLOGP,NOLOGS
  real :: LOGLEN(20),LOGDIA(21,3),LOGVOL(7,20),BOLHT(21),VOL(15),DRYBIO(15),GRNBIO(15)
  real :: D,H,H1,H2,TOPP,TOPS,STMP,BRK,BRKD,CR,CULL
  integer :: CNT,SPCD,EQN,ERRFLG,SPGRPCD,LOGST,DECAYCD,FIASPCD,REGN
  character(len=11) :: VOLEQ
  character(len=2) :: FORST,DIST,PROD
  character(len=1) :: LIVE,CTYPE

  call GETTABLE(1,CNT,SP,JK)
  select case (MODE)
  case ('bad-speqcoef')
    VOLEQ = 'NVB0330ABC'
    call SPEQCOEF(VOLEQ,CNT,SP,SPCD,EQN,R(1),R(2),R(3),R(4),R(5),R(6),R(7),R(8),R(9))
  case ('bad-calc')
    VOLEQ = 'NVB03302X1'
    SPGRPCD = 3
    WDSG = 0.0
    call CALC_EQVW(VOLEQ,10.0,50.0,CNT,SP,JK,VW,ERRFLG,SPGRPCD,WDSG)
  case ('bad-nvbc')
    VOLEQ = 'NVB0AB0202'
    FORST = '05'
    DIST = '01'
    PROD = '01'
    LIVE = 'L'
    CTYPE = 'F'
    D = 14.5
    H = 88.0
    H1 = 0.0
    H2 = 0.0
    TOPP = 6.0
    TOPS = 4.0
    STMP = 1.0
    BRK = 0.0
    BRKD = 0.0
    CR = 40.0
    CULL = 0.0
    DECAYCD = 0
    FIASPCD = 202
    REGN = 6
    call NVBC(REGN,FORST,DIST,VOLEQ,D,H,TOPP,TOPS,H1,H2,STMP,PROD,BRK,BRKD, &
      LIVE,CR,CULL,DECAYCD,LOGLEN,LOGDIA,LOGVOL,BOLHT,LOGST,NOLOGP,NOLOGS, &
      VOL,DRYBIO,GRNBIO,ERRFLG,FIASPCD,CTYPE)
  end select
  write(*,*) 'invalid field was accepted'
end subroutine BAD_FIELD
