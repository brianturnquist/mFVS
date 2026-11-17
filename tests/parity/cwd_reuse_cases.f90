!=====================================!
!   Paired CWD reuse and fallback cases !
!=====================================!
program CWD_REUSE_CASES
  use DEBUG_MOD, only: ANY_DEBUG,NVEL_DEBUG => DEBUG
  implicit none
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'
  include 'DBSTK.f90'
  include 'VOLSTD.f90'

  integer :: CASE_NO,MODE,NCALL,BASECALL,EXPECTED_CALLS
  integer :: EXPECT_CWD(size(CWD)),EXPECT_NEW(size(CWDNEW))
  real :: FALLH,FALLS,LEFTH,LEFTS,PAIRVOL,LASTVOL,BASELAST
  logical :: READY,ELIGIBLE
  common /CWD_TEST/ NCALL,LASTVOL

  do CASE_NO = 1,17
     do MODE = 1,2
        VARACD = VARIANTCD
        ICYC = 0
        JOSTND = 6
        ITOP = 0
        ANY_DEBUG = .false.
        NVEL_DEBUG%MODEL = .false.
        NVEL_DEBUG%VOLEQ = .false.
        METHC = 6
        if (VARACD.eq.'ON') METHC = 8
        VEQNNC = 'NVB0000122'
        VEQNNB = VEQNNC
        CWD = 0.0
        CWDNEW = 0.0
        SPS(1) = 1
        DBHS(1) = 12.0+CASE_NO
        HTDEAD(1) = 80.0
        HTIH(1) = 80.0
        HTIS(1) = 70.0
        DKRCLS = 1
        V2T = 0.02
        FALLH = 0.2
        FALLS = 0.1
        LEFTH = 0.8
        LEFTS = 0.5
        NCALL = 0
        LASTVOL = 0.0
        if (CASE_NO.eq.2.or.CASE_NO.eq.4) then
           FALLH = 0.0
           FALLS = 0.0
        end if
        if (CASE_NO.eq.3.or.CASE_NO.eq.4) then
           LEFTH = 0.0
           LEFTS = 0.0
        end if
        if (CASE_NO.eq.5) ITOP = 1
        if (CASE_NO.eq.6) NVEL_DEBUG%MODEL = .true.
        if (CASE_NO.eq.7) ANY_DEBUG = .true.
        if (CASE_NO.eq.8) NVEL_DEBUG%VOLEQ = .true.
        if (CASE_NO.eq.9) METHC = 1
        if (CASE_NO.eq.10) VEQNNC = '102DVE0122'
        if (CASE_NO.eq.11) VEQNNB = '102DVE0122'
        if (CASE_NO.eq.12) METHC = 8
        if (CASE_NO.eq.13) FALLS = 0.0
        if (CASE_NO.eq.14) LEFTS = 0.0
        if (CASE_NO.eq.16) SPS(1) = 2
        if (CASE_NO.eq.17.and.VARACD.eq.'ON') METHC = 6

        if (MODE.eq.1) then
           call BASE_CWD1(1,FALLH,FALLS)
        else
           ! Seed stale caller values: the first entry must reset both.
           READY = .true.
           PAIRVOL = -99.0
           call CWD1V(1,FALLH,FALLS,PAIRVOL,READY)
        end if
        HTIH(1) = 60.0
        HTIS(1) = 35.0
        if (MODE.eq.1) then
           call BASE_CWD2(1,LEFTH,LEFTS,80.0,70.0)
        else
           call CWD2V(1,LEFTH,LEFTS,80.0,70.0,PAIRVOL,READY)
           if (READY) error stop 'pair validity survives consumption'
        end if
        if (CASE_NO.eq.15) then
           ! Salvage/direct callers and CWD3 must not inherit paired state.
           if (MODE.eq.1) then
              call BASE_CWD1(1,0.3,0.0)
              call BASE_CWD2(1,0.2,0.1,80.0,70.0)
              call BASE_CWD3(1,20.0,0.4,100.0,0)
           else
              call CWD1(1,0.3,0.0)
              call CWD2(1,0.2,0.1,80.0,70.0)
              call CWD3(1,20.0,0.4,100.0,0)
           end if
        end if
        if (MODE.eq.1) then
           EXPECT_CWD = transfer(CWD,EXPECT_CWD)
           EXPECT_NEW = transfer(CWDNEW,EXPECT_NEW)
           BASECALL = NCALL
           BASELAST = LASTVOL
        else
           if (any(transfer(CWD,EXPECT_CWD).ne.EXPECT_CWD)) error stop 'CWD bits differ'
           if (any(transfer(CWDNEW,EXPECT_NEW).ne.EXPECT_NEW)) error stop 'new debris bits differ'
           if (LASTVOL.ne.BASELAST) error stop 'last volume state differs'
           ELIGIBLE = CASE_NO.ne.5
           if (VARACD.ne.'BC') then
              ELIGIBLE = ELIGIBLE.and..not.ANY_DEBUG.and.&
                   &.not.NVEL_DEBUG%MODEL.and..not.NVEL_DEBUG%VOLEQ
              if (VARACD.eq.'ON') then
                 ELIGIBLE = ELIGIBLE.and.METHC(1).ne.6.and.METHC(1).ne.10
              else
                 ! These cases intentionally select legacy or DVE fallback paths.
                 ELIGIBLE = ELIGIBLE.and.CASE_NO.ne.9.and.CASE_NO.ne.10.and.&
                      &CASE_NO.ne.11.and.CASE_NO.ne.12
              end if
           end if
           EXPECTED_CALLS = BASECALL
           if (ELIGIBLE.and.FALLH+FALLS.gt.0.0.and.LEFTH+LEFTS.gt.0.0)&
                &EXPECTED_CALLS = EXPECTED_CALLS-1
           if (NCALL.ne.EXPECTED_CALLS) error stop 'unexpected volume-call count'
        end if
     end do
  end do
  write(*,*) 'PASS CWD ',VARACD,': 17 exact fall/breakage/zero/early-return/debug/independent cases'
end program CWD_REUSE_CASES

subroutine FMSVL2(SP,D,H,XHT,VOL,CROWN,LIVEDEAD,MERCH,DEBUG,IOUT)
  implicit none
  integer :: SP,CROWN,IOUT,NCALL
  real :: D,H,XHT,VOL,LASTVOL
  character(len=1) :: LIVEDEAD
  logical :: MERCH,DEBUG
  common /CWD_TEST/ NCALL,LASTVOL
  NCALL = NCALL+1
  if (XHT.lt.0.0) XHT = H
  VOL = D*H*0.001
  if (SP.eq.2) VOL = 0.0
  LASTVOL = VOL
end subroutine FMSVL2

subroutine DBCHK(DEBUG,NAME,NCHAR,CYCLE)
  implicit none
  include 'DBSTK.f90'
  logical :: DEBUG
  character(len=*) :: NAME
  integer :: NCHAR,CYCLE
  DEBUG = ITOP.gt.0
end subroutine DBCHK
