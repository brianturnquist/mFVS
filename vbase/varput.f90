!==================================!
!   Write variant specific state   !
!==================================!
! BASE $Id$
subroutine VARPUT (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  use VARCFG_MOD, only: CFG_MAXSP
  use PUTGETSP_MOD, only: BFWRSP
  implicit none

  !     WRITE THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  !     SHARED BY ALL VARIANTS. VARIANTCD SELECTS THE VARIANT'S PAYLOAD,
  !     KEPT IN ITS OWN INTERNAL PROCEDURE WITH ITS OWN COMMON BLOCKS. THE
  !     FIELD ORDER, COUNTS AND WORK BUFFERS ARE THOSE OF THE FORMER
  !     PER-VARIANT ROUTINES. VARIANTS WITHOUT A PAYLOAD WRITE NOTHING.

  ! Common blocks
  include 'PRGPRM.f90'

  !     NOTE: THE ACTUAL STORAGE LIMIT FOR INTS, LOGICS, AND REALS
  !     IS MAXTRE (SEE PRGPRM).

  ! Variables
  integer :: ILIMIT,IPNT
  logical :: LOGICS(*)
  real    :: WK3(MAXTRE)
  integer :: INTS(*)
  real    :: REALS(*)

  select case (VARIANTCD)
  case ('AK')
     call PUTAK
  case ('BC')
     call PUTBC
  case ('CI','TT')
     call PUTCI
  case ('CR')
     call PUTCR
  case ('EM')
     call PUTEM
  case ('KT')
     call PUTKT
  case ('OC','OP')
     call PUTOR
  case ('SN')
     call PUTSN
  end select
  return

contains

  subroutine PUTAK

    !  AK

    include 'ESCOMN.f90'
    include 'PLOT.f90'
    include 'VARCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 2,MXR = 5)

    !     STORE THE INTEGER SCALARS IN THE ARRAY INTS

    INTS ( 1) = IIFORTP ! from PLOT.F77
    INTS ( 2) = IFT0 ! from ESCOMN.F77
    call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

    !     WRITE THE INTEGER ARRAYS

    !     STORE THE LOGICAL SCALARS.

    !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     STORE THE REAL SCALARS IN THE ARRAY REALS.

    !**   REALS( 1) =
    !**   CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

    !     WRITE THE REAL ARRAYS

    call BFWRSP (WK3,IPNT,ILIMIT,OCURFT,14,2)
    call BFWRIT (WK3, IPNT, ILIMIT, HTT11,     CFG_MAXSP,    2) ! from ESCOMN.F77
    call BFWRIT (WK3, IPNT, ILIMIT, HTT12,     CFG_MAXSP,    2)
    call BFWRIT (WK3, IPNT, ILIMIT, HTT13,     CFG_MAXSP,    2)
    call BFWRIT (WK3, IPNT, ILIMIT, XMAXPT,    MAXPLT,   2)
  end subroutine PUTAK

  subroutine PUTBC

    !  BC

    include 'BCPLOT.f90'
    include 'ESHAP.f90'
    integer :: MXL,MXR,MXI,IRECLN
    parameter (MXR = 2,MXL = 3,MXI = 1,IRECLN = 1024)
    integer :: I,J,K
    logical :: LC(MAXSP)
    real    :: RC(MAXSP)

    !     PUT THE INTEGER SCALARS
    !     CALL IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2) ! NONE

    !     PUT THE INTEGER ARRAYS
    !      CALL IFWRIT (WK3, IPNT, ILIMIT, JDST, 3, 2)

    !     PUT THE LOGICAL SCALARS
    LOGICS(1) = LV2ATV
    LOGICS(2) = LV2HTCP
    LOGICS(3) = LV2HDR
    call LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     PUT THE LOGICAL ARRAYS
    call LFWRIT (WK3, IPNT, ILIMIT, LLTDGOK, CFG_MAXSP, 2)
    call LFWRIT (WK3, IPNT, ILIMIT, LMHTDUB, CFG_MAXSP, 2)

    !     PUT THE LOGICAL ARRAYS FOR MORT, LTH & STG STRUCTURES
    LC(1:MAXSP_NATIVE) = MORT%FIT
    call LFWRIT (WK3,IPNT,ILIMIT, LC, CFG_MAXSP, 2)
    LC(1:MAXSP_NATIVE) = LTH%FIT
    call LFWRIT (WK3,IPNT,ILIMIT, LC, CFG_MAXSP, 2)
    LC(1:MAXSP_NATIVE) = STG%FIT
    call LFWRIT (WK3,IPNT,ILIMIT, LC, CFG_MAXSP, 2)

    !     PUT THE REAL SCALARS
    REALS(1) = SEISHT
    REALS(2) = ZBRSH
    call BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

    !     PUT THE REAL ARRAYS
    do I = 1,MRT_PY
       do J = 1,MRT_BA_UB
          do K = 1,MRT_DBH_UB
             call BFWRIT (WK3,IPNT,ILIMIT,TMRT(I,J,K), 1, 2)
          end do
       end do
    end do
    call BFWRIT (WK3,IPNT,ILIMIT,DGLD1,    CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,DGBAL1,   CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,DGCR1,    CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,DGDBAL1,  CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,DGDBAL2,  CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,DGCCF1,   CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,DGDSQ1,   CFG_MAXSP,  2)

    call BFWRIT (WK3,IPNT,ILIMIT,CRHTDBH,  CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,CRHT,     CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,CRDBH2,   CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,CRBAL,    CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,CRLNCCF,  CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,CRSD,     CFG_MAXSP,  2)

    call BFWRIT (WK3,IPNT,ILIMIT,V2SEICOR, CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,SEICON,   CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,XHT1B,    CFG_MAXSP,  2)
    call BFWRIT (WK3,IPNT,ILIMIT,XHT2B,    CFG_MAXSP,  2)

    !     PUT THE REAL ARRAYS FOR MORT STRUCTURE
    RC(1:MAXSP_NATIVE) = MORT%CON
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = MORT%INVDBH
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = MORT%DBH
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = MORT%DBHSQ
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = MORT%RDBH
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = MORT%BAL
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = MORT%SQRTBA
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = MORT%SPH
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)

    !     PUT THE REAL ARRAYS FOR LTH STRUCTURE
    RC(1:MAXSP_NATIVE) = LTH%MLT
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%A1
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%B1
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%B2
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%SI
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%B3
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%C
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%CN
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%CNSI
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%DG
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%DG2
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%DBH1
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%DBH2
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = LTH%HT2
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)

    !     PUT THE REAL ARRAYS FOR STG STRUCTURE
    RC(1:MAXSP_NATIVE) = STG%CON
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = STG%LNHT
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = STG%HT
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = STG%CCF
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = STG%BAL
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = STG%EXPMLT
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = STG%HCCFL
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = STG%CCFSL
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    RC(1:MAXSP_NATIVE) = STG%SD
    call BFWRIT (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
  end subroutine PUTBC

  subroutine PUTCI

    !  CI,TT

    include 'CICOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 1,MXR = 1)

    !     GET THE INTEGER SCALARS.

    INTS(1) = ICINDX
    call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    !**   CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  end subroutine PUTCI

  subroutine PUTCR

    !  CR

    include 'GGCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 1,MXR = 5)

    !     STORE THE INTEGER SCALARS IN THE ARRAY INTS

    INTS ( 1) = IGFOR
    call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

    !     WRITE THE INTEGER ARRAYS

    !     STORE THE LOGICAL SCALARS.

    !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     STORE THE REAL SCALARS IN THE ARRAY REALS.

    REALS( 1) = AGERNG
    REALS( 2) = SEEDS
    REALS( 3) = TPAT
    REALS( 4) = DSTAG
    REALS( 5) = BAINIT
    call BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

    !     WRITE THE REAL ARRAYS

    call BFWRSP (WK3,IPNT,ILIMIT,BCLAS,41,2)
    call BFWRIT (WK3, IPNT, ILIMIT, TBA,    CFG_MAXSP,    2)
    call BFWRIT (WK3, IPNT, ILIMIT, BAU,    41,       2)
    call BFWRIT (WK3, IPNT, ILIMIT, BARK1,  CFG_MAXSP,    2)
    call BFWRIT (WK3, IPNT, ILIMIT, BARK2,  CFG_MAXSP,    2)
    call BFWRIT (WK3, IPNT, ILIMIT, DBHMAX, CFG_MAXSP,    2)
    call BFWRSP (WK3,IPNT,ILIMIT,TCLAS,41,2)
    call BFWRIT (WK3, IPNT, ILIMIT, BREAK,  CFG_MAXSP,    2)
    call BFWRIT (WK3, IPNT, ILIMIT, SITELO, CFG_MAXSP,    2)
    call BFWRIT (WK3, IPNT, ILIMIT, SITEHI, CFG_MAXSP,    2)
  end subroutine PUTCR

  subroutine PUTEM

    !  EM

    include 'EMCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 1,MXR = 1)

    !     STORE THE INTEGER SCALARS.

    INTS(1) = IEMTYP
    call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

    !     STORE THE LOGICAL SCALARS.

    !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     STORE THE REAL SCALARS.

    !**   CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  end subroutine PUTEM

  subroutine PUTKT

    !  KT

    include 'KOTCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 2,MXR = 1)

    !     GET THE INTEGER SCALARS.

    INTS(1) = KKTYPE
    INTS(2) = KOTFOR
    call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    !**   CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  end subroutine PUTKT

  subroutine PUTOR

    !  OC,OP

    include 'ORGANON.f90'
    integer :: MXL,MXI,MXR,I,II
    parameter (MXL = 3,MXI = 8,MXR = 16)
    integer :: INTS2(1)

    !     PUT THE INTEGER SCALARS USING IFWRIT, MXI + 1 OF THEM

    INTS( 1) = CYCLG
    INTS( 2) = VERSION
    INTS( 3) = NPTS
    INTS( 4) = NTREES1
    INTS( 5) = STAGE
    INTS( 6) = BHAGE
    INTS( 7) = NTREES2
    INTS( 8) = IERROR
    call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)
    INTS2( 1) = ITEST
    call IFWRIT (WK3, IPNT, ILIMIT,INTS2,   1, 2)

    !     PUT THE INTEGER ARRAYS USING IFWRIT

    call IFWRIT (WK3,IPNT,ILIMIT, TREENO, 2000,   2)
    call IFWRIT (WK3,IPNT,ILIMIT,   PTNO, 2000,   2)
    call IFWRIT (WK3,IPNT,ILIMIT,SPECIES, 2000,   2)
    call IFWRIT (WK3,IPNT,ILIMIT,   USER, 2000,   2)
    call IFWRIT (WK3,IPNT,ILIMIT,   INDS,   30,   2)
    do I = 1,2000
       do II = 1,3
          call IFWRIT (WK3,IPNT,ILIMIT,PRAGE(I,II),1,   2)
          10 continue
       end do
       11 continue
    end do
    do I = 1,2000
       do II = 1,3
          call IFWRIT (WK3,IPNT,ILIMIT,BRCNT(I,II),1,   2)
          15 continue
       end do
       16 continue
    end do
    do I = 1,2000
       do II = 1,40
          call IFWRIT (WK3,IPNT,ILIMIT, BRHT(I,II),1,   2)
          20 continue
       end do
       21 continue
    end do
    do I = 1,2000
       do II = 1,40
          call IFWRIT (WK3,IPNT,ILIMIT,BRDIA(I,II),1,   2)
          25 continue
       end do
       26 continue
    end do
    do I = 1,2000
       do II = 1,40
          call IFWRIT (WK3,IPNT,ILIMIT,JCORE(I,II),1,   2)
          30 continue
       end do
       31 continue
    end do
    call IFWRIT (WK3,IPNT,ILIMIT,    NPR, 2000,   2)
    call IFWRIT (WK3,IPNT,ILIMIT, SERROR,   35,   2)
    do I = 1,2000
       do II = 1,6
          call IFWRIT (WK3,IPNT,ILIMIT,TERROR(I,II),1,   2)
          35 continue
       end do
       36 continue
    end do
    call IFWRIT (WK3,IPNT,ILIMIT, SWARNING,   9,   2)
    call IFWRIT (WK3,IPNT,ILIMIT,TWARNING, 2000,   2)
    call IFWRIT (WK3,IPNT,ILIMIT,    IORG, 2000,   2)

    !     PUT THE LOGICAL SCALARS USING LFWRIT, MXL OF THEM.

    LOGICS ( 1) = LORGANON
    LOGICS ( 2) = LORGVOLS
    LOGICS ( 3) = LORGPREP
    call LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     PUT THE REAL SCALARS USING BFWRIT, MXR OF THEM.

    REALS( 1) = SITE_1
    REALS( 2) = SITE_2
    REALS( 3) = MSDI_1
    REALS( 4) = MSDI_2
    REALS( 5) = MSDI_3
    REALS( 6) = BABT
    REALS( 7) = CFTD
    REALS( 8) = CFTDHW
    REALS( 9) = CFSH
    REALS(10) = LOGLL
    REALS(11) = LOGML
    REALS(12) = LOGTD
    REALS(13) = LOGSH
    REALS(14) = LOGTA
    REALS(15) = OCC
    REALS(16) = OAHT
    call BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

    !     PUT THE REAL ARRAYS USING BFWRIT.

    call BFWRIT (WK3,IPNT,ILIMIT,  DBH1, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT, HT1OR, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,   CR1, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,EXPAN1, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT, SCR1B, 2000,      2)
    do I = 1,3
       do II = 1,18
          call BFWRIT (WK3,IPNT,ILIMIT,ACALIB(I,II),1,    2)
          50 continue
       end do
       51 continue
    end do
    call BFWRIT (WK3,IPNT,ILIMIT,    PN,    5,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,   YSF,    5,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,  BART,    5,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,   YST,    5,      2)
    do I = 1,2000
       do II = 1,3
          call BFWRIT (WK3,IPNT,ILIMIT,PRLH(I,II),1,    2)
          55 continue
       end do
       56 continue
    end do
    do I = 1,2000
       do II = 1,3
          call BFWRIT (WK3,IPNT,ILIMIT,PRDBH(I,II),1,    2)
          60 continue
       end do
       61 continue
    end do
    do I = 1,2000
       do II = 1,3
          call BFWRIT (WK3,IPNT,ILIMIT,PRHT(I,II),1,    2)
          65 continue
       end do
       66 continue
    end do
    do I = 1,2000
       do II = 1,3
          call BFWRIT (WK3,IPNT,ILIMIT,PRCR(I,II),1,    2)
          70 continue
       end do
       71 continue
    end do
    do I = 1,2000
       do II = 1,3
          call BFWRIT (WK3,IPNT,ILIMIT,PREXP(I,II),1,    2)
          75 continue
       end do
       76 continue
    end do
    call BFWRIT (WK3,IPNT,ILIMIT, MGEXP, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,  DGRO, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,  HGRO, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,MORTEXP, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,CRCHNG, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,SCRCHNG, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,  STOR,   30,      2)
    call BFWRIT (WK3,IPNT,ILIMIT, RVARS,   30,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,  DBH2, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT, HT2OR, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,   CR2, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT,EXPAN2, 2000,      2)
    call BFWRIT (WK3,IPNT,ILIMIT, SCR2B, 2000,      2)
  end subroutine PUTOR

  subroutine PUTSN

    !  SN

    include 'SNCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 2,MXR = 1)

    !     GET THE INTEGER SCALARS.

    INTS(1) = KODIST
    INTS(2) = ISEFOR
    call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    !**   CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  end subroutine PUTSN

end subroutine VARPUT

!============================================!
!   Write variant specific character state   !
!============================================!
subroutine VARCHPUT (CBUFF, IPNT, LNCBUF)
  implicit none

  !     Put variant-specific character data. Only BC has any.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BCPLOT.f90'

  ! Variables
  integer :: LNCBUF,IPNT
  character(len=1) :: CBUFF(LNCBUF)
  character(len=1) :: CT
  integer :: I

  if (VARIANTCD .ne. 'BC') return

  !     PUT THE CHARACTER DATA FOR BEC STRUCTURE
  do I = 1,3
     CT = BEC%Region(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do
  do I = 1,4
     CT = BEC%Zone(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do
  do I = 1,3
     CT = BEC%SubZone(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do
  do I = 1,5
     CT = BEC%Series(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do
  do I = 1,7
     CT = BEC%FullName(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do
  do I = 1,15
     CT = BEC%PrettyName(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do

  !     PUT THE CHARACTER DATA FOR MORT STRUCTURE
  do I = 1,15
     CT = MORT%PrettyName(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do

  !     PUT THE CHARACTER DATA FOR LTH STRUCTURE
  do I = 1,15
     CT = LTH%PrettyName(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do

  !     PUT THE CHARACTER DATA FOR STG STRUCTURE
  do I = 1,15
     CT = STG%PrettyName(I:I)
     call CHWRIT (CBUFF,IPNT,LNCBUF,CT,2)
  end do

  return
end subroutine VARCHPUT
