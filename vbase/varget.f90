!=================================!
!   Read variant specific state   !
!=================================!
! BASE $Id$
subroutine VARGET (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  use VARCFG_MOD, only: CFG_MAXSP
  use PUTGETSP_MOD, only: BFRDSP
  implicit none

  !     READ THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  !     SHARED BY ALL VARIANTS. VARIANTCD SELECTS THE VARIANT'S PAYLOAD,
  !     KEPT IN ITS OWN INTERNAL PROCEDURE WITH ITS OWN COMMON BLOCKS. THE
  !     FIELD ORDER, COUNTS AND WORK BUFFERS ARE THOSE OF THE FORMER
  !     PER-VARIANT ROUTINES. VARIANTS WITHOUT A PAYLOAD READ NOTHING.

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
     call GETAK
  case ('BC')
     call GETBC
  case ('CI','TT')
     call GETCI
  case ('CR')
     call GETCR
  case ('EM')
     call GETEM
  case ('KT')
     call GETKT
  case ('OC','OP')
     call GETOR
  case ('SN')
     call GETSN
  end select
  return

contains

  subroutine GETAK

    !  AK

    include 'ESCOMN.f90'
    include 'PLOT.f90'
    include 'VARCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 2,MXR = 5)

    !     GET THE INTEGER SCALARS.

    call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
    IIFORTP = INTS ( 1)
    IFT0 = INTS ( 2)

    !     GET THE INTEGER ARRAYS.

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    !**   CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
    !**           = REALS( 1)

    !     READ THE REAL ARRAYS

    call BFRDSP (WK3,IPNT,ILIMIT,OCURFT,14,2)
    call BFREAD (WK3, IPNT, ILIMIT, HTT11,     CFG_MAXSP,    2) ! from ESCOMN.F77
    call BFREAD (WK3, IPNT, ILIMIT, HTT12,     CFG_MAXSP,    2)
    call BFREAD (WK3, IPNT, ILIMIT, HTT13,     CFG_MAXSP,    2)
    call BFREAD (WK3, IPNT, ILIMIT, XMAXPT,    MAXPLT,   2)
  end subroutine GETAK

  subroutine GETBC

    !  BC

    include 'BCPLOT.f90'
    include 'ESHAP.f90'
    integer :: MXL,MXR,MXI,IRECLN
    parameter (MXR = 2,MXL = 3,MXI = 1,IRECLN = 1024)
    integer :: I,J,K
    logical :: LC(MAXSP)
    real    :: RC(MAXSP)

    !     GET THE INTEGER SCALARS
    !     CALL IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2) ! NONE

    !     GET THE INTEGER ARRAYS
    !     CALL IFREAD (WK3, IPNT, ILIMIT, JDST, 3, 2)

    !     GET THE LOGICAL SCALARS.
    call LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)
    LV2ATV = LOGICS(1)
    LV2HTCP = LOGICS(2)
    LV2HDR = LOGICS(3)

    !     GET THE LOGICAL ARRAYS
    call LFREAD (WK3,IPNT,ILIMIT, LLTDGOK, CFG_MAXSP, 2)
    call LFREAD (WK3,IPNT,ILIMIT, LMHTDUB, CFG_MAXSP, 2)

    !     GET THE LOGICAL ARRAYS FOR MORT, LTH & STG STRUCTURES
    call LFREAD (WK3,IPNT,ILIMIT, LC, CFG_MAXSP, 2)
    MORT%FIT = LC(1:MAXSP_NATIVE)
    call LFREAD (WK3,IPNT,ILIMIT, LC, CFG_MAXSP, 2)
    LTH%FIT = LC(1:MAXSP_NATIVE)
    call LFREAD (WK3,IPNT,ILIMIT, LC, CFG_MAXSP, 2)
    STG%FIT = LC(1:MAXSP_NATIVE)

    !     GET THE REAL SCALARS
    call BFREAD (WK3,IPNT,ILIMIT,REALS,  MXR,  2)
    SEISHT = REALS(1)
    ZBRSH = REALS(2)

    !     GET THE REAL ARRAYS
    do I = 1,MRT_PY
       do J = 1,MRT_BA_UB
          do K = 1,MRT_DBH_UB
             call BFREAD (WK3,IPNT,ILIMIT,TMRT(I,J,K), 1, 2)
          end do
       end do
    end do
    call BFREAD (WK3,IPNT,ILIMIT,DGLD1,    CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,DGBAL1,   CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,DGCR1,    CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,DGDBAL1,  CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,DGDBAL2,  CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,DGCCF1,   CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,DGDSQ1,   CFG_MAXSP,  2)

    call BFREAD (WK3,IPNT,ILIMIT,CRHTDBH,  CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,CRHT,     CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,CRDBH2,   CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,CRBAL,    CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,CRLNCCF,  CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,CRSD,     CFG_MAXSP,  2)

    call BFREAD (WK3,IPNT,ILIMIT,V2SEICOR, CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,SEICON,   CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,XHT1B,    CFG_MAXSP,  2)
    call BFREAD (WK3,IPNT,ILIMIT,XHT2B,    CFG_MAXSP,  2)

    !     GET THE REAL ARRAYS FOR MORT STRUCTURE
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    MORT%CON = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    MORT%INVDBH = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    MORT%DBH = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    MORT%DBHSQ = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    MORT%RDBH = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    MORT%BAL = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    MORT%SQRTBA = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    MORT%SPH = RC(1:MAXSP_NATIVE)

    !     GET THE REAL ARRAYS FOR LTH STRUCTURE
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%MLT = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%A1 = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%B1 = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%B2 = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%SI = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%B3 = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%C = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%CN = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%CNSI = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%DG = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%DG2 = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%DBH1 = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%DBH2 = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    LTH%HT2 = RC(1:MAXSP_NATIVE)

    !     GET THE REAL ARRAYS FOR STG STRUCTURE
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%CON = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%LNHT = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%HT = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%CCF = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%BAL = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%EXPMLT = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%HCCFL = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%CCFSL = RC(1:MAXSP_NATIVE)
    call BFREAD (WK3,IPNT,ILIMIT, RC,    CFG_MAXSP,  2)
    STG%SD = RC(1:MAXSP_NATIVE)
  end subroutine GETBC

  subroutine GETCI

    !  CI,TT

    include 'CICOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 1,MXR = 1)

    !     GET THE INTEGER SCALARS.

    call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
    ICINDX = INTS(1)

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    !**   CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  end subroutine GETCI

  subroutine GETCR

    !  CR

    include 'GGCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 1,MXR = 5)

    !     GET THE INTEGER SCALARS.

    call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
    IGFOR = INTS ( 1)

    !     GET THE INTEGER ARRAYS.

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    call BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
    AGERNG = REALS( 1)
    SEEDS = REALS( 2)
    TPAT = REALS( 3)
    DSTAG = REALS( 4)
    BAINIT = REALS( 5)

    !     READ THE REAL ARRAYS

    call BFRDSP (WK3,IPNT,ILIMIT,BCLAS,41,2)
    call BFREAD (WK3, IPNT, ILIMIT, TBA,    CFG_MAXSP,    2)
    call BFREAD (WK3, IPNT, ILIMIT, BAU,    41,       2)
    call BFREAD (WK3, IPNT, ILIMIT, BARK1,  CFG_MAXSP,    2)
    call BFREAD (WK3, IPNT, ILIMIT, BARK2,  CFG_MAXSP,    2)
    call BFREAD (WK3, IPNT, ILIMIT, DBHMAX, CFG_MAXSP,    2)
    call BFRDSP (WK3,IPNT,ILIMIT,TCLAS,41,2)
    call BFREAD (WK3, IPNT, ILIMIT, BREAK,  CFG_MAXSP,    2)
    call BFREAD (WK3, IPNT, ILIMIT, SITELO, CFG_MAXSP,    2)
    call BFREAD (WK3, IPNT, ILIMIT, SITEHI, CFG_MAXSP,    2)
  end subroutine GETCR

  subroutine GETEM

    !  EM

    include 'EMCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 1,MXR = 1)

    !     GET THE INTEGER SCALARS.

    call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
    IEMTYP = INTS(1)

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    !**   CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  end subroutine GETEM

  subroutine GETKT

    !  KT

    include 'KOTCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 2,MXR = 1)

    !     GET THE INTEGER SCALARS.

    call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
    KKTYPE = INTS(1)
    KOTFOR = INTS(2)

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    !**   CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  end subroutine GETKT

  subroutine GETOR

    !  OC,OP

    include 'ORGANON.f90'
    integer :: MXL,MXI,MXR,I,II
    parameter (MXL = 3,MXI = 8,MXR = 16)
    integer :: INTS2(1)

    !     GET THE INTEGER SCALARS USING IFREAD, MXI + 1 OF THEM

    call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
    CYCLG = INTS( 1)
    VERSION = INTS( 2)
    NPTS = INTS( 3)
    NTREES1 = INTS( 4)
    STAGE = INTS( 5)
    BHAGE = INTS( 6)
    NTREES2 = INTS( 7)
    IERROR = INTS( 8)
    call IFREAD (WK3, IPNT, ILIMIT,INTS2,   1, 2)
    ITEST = INTS2( 1)

    !     GET THE INTEGER ARRAYS USING IFREAD

    call IFREAD (WK3,IPNT,ILIMIT, TREENO, 2000,   2)
    call IFREAD (WK3,IPNT,ILIMIT,   PTNO, 2000,   2)
    call IFREAD (WK3,IPNT,ILIMIT,SPECIES, 2000,   2)
    call IFREAD (WK3,IPNT,ILIMIT,   USER, 2000,   2)
    call IFREAD (WK3,IPNT,ILIMIT,   INDS,   30,   2)
    do I = 1,2000
       do II = 1,3
          call IFREAD (WK3,IPNT,ILIMIT,PRAGE(I,II),1,   2)
          10 continue
       end do
       11 continue
    end do
    do I = 1,2000
       do II = 1,3
          call IFREAD (WK3,IPNT,ILIMIT,BRCNT(I,II),1,   2)
          15 continue
       end do
       16 continue
    end do
    do I = 1,2000
       do II = 1,40
          call IFREAD (WK3,IPNT,ILIMIT, BRHT(I,II),1,   2)
          20 continue
       end do
       21 continue
    end do
    do I = 1,2000
       do II = 1,40
          call IFREAD (WK3,IPNT,ILIMIT,BRDIA(I,II),1,   2)
          25 continue
       end do
       26 continue
    end do
    do I = 1,2000
       do II = 1,40
          call IFREAD (WK3,IPNT,ILIMIT,JCORE(I,II),1,   2)
          30 continue
       end do
       31 continue
    end do
    call IFREAD (WK3,IPNT,ILIMIT,    NPR, 2000,   2)
    call IFREAD (WK3,IPNT,ILIMIT, SERROR,   35,   2)
    do I = 1,2000
       do II = 1,6
          call IFREAD (WK3,IPNT,ILIMIT,TERROR(I,II),1,   2)
          35 continue
       end do
       36 continue
    end do
    call IFREAD (WK3,IPNT,ILIMIT, SWARNING,   9,   2)
    call IFREAD (WK3,IPNT,ILIMIT,TWARNING, 2000,   2)
    call IFREAD (WK3,IPNT,ILIMIT,    IORG, 2000,   2)

    !     GET THE LOGICAL SCALARS USING LFREAD, MXL OF THEM.

    call LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)
    LORGANON = LOGICS( 1)
    LORGVOLS = LOGICS( 2)
    LORGPREP = LOGICS( 3)

    !     GET THE REAL SCALARS USING BFREAD, MXR OF THEM.

    call BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
    SITE_1 = REALS( 1)
    SITE_2 = REALS( 2)
    MSDI_1 = REALS( 3)
    MSDI_2 = REALS( 4)
    MSDI_3 = REALS( 5)
    BABT = REALS( 6)
    CFTD = REALS( 7)
    CFTDHW = REALS( 8)
    CFSH = REALS( 9)
    LOGLL = REALS(10)
    LOGML = REALS(11)
    LOGTD = REALS(12)
    LOGSH = REALS(13)
    LOGTA = REALS(14)
    OCC = REALS(15)
    OAHT = REALS(16)

    !     GET THE REAL ARRAYS.

    call BFREAD (WK3,IPNT,ILIMIT,  DBH1, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT, HT1OR, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,   CR1, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,EXPAN1, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT, SCR1B, 2000,      2)
    do I = 1,3
       do II = 1,18
          call BFREAD (WK3,IPNT,ILIMIT,ACALIB(I,II),1,    2)
          50 continue
       end do
       51 continue
    end do
    call BFREAD (WK3,IPNT,ILIMIT,    PN,    5,      2)
    call BFREAD (WK3,IPNT,ILIMIT,   YSF,    5,      2)
    call BFREAD (WK3,IPNT,ILIMIT,  BART,    5,      2)
    call BFREAD (WK3,IPNT,ILIMIT,   YST,    5,      2)
    do I = 1,2000
       do II = 1,3
          call BFREAD (WK3,IPNT,ILIMIT,PRLH(I,II),1,    2)
          55 continue
       end do
       56 continue
    end do
    do I = 1,2000
       do II = 1,3
          call BFREAD (WK3,IPNT,ILIMIT,PRDBH(I,II),1,    2)
          60 continue
       end do
       61 continue
    end do
    do I = 1,2000
       do II = 1,3
          call BFREAD (WK3,IPNT,ILIMIT,PRHT(I,II),1,    2)
          65 continue
       end do
       66 continue
    end do
    do I = 1,2000
       do II = 1,3
          call BFREAD (WK3,IPNT,ILIMIT,PRCR(I,II),1,    2)
          70 continue
       end do
       71 continue
    end do
    do I = 1,2000
       do II = 1,3
          call BFREAD (WK3,IPNT,ILIMIT,PREXP(I,II),1,    2)
          75 continue
       end do
       76 continue
    end do
    call BFREAD (WK3,IPNT,ILIMIT, MGEXP, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,  DGRO, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,  HGRO, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,MORTEXP, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,CRCHNG, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,SCRCHNG, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,  STOR,   30,      2)
    call BFREAD (WK3,IPNT,ILIMIT, RVARS,   30,      2)
    call BFREAD (WK3,IPNT,ILIMIT,  DBH2, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT, HT2OR, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,   CR2, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT,EXPAN2, 2000,      2)
    call BFREAD (WK3,IPNT,ILIMIT, SCR2B, 2000,      2)
  end subroutine GETOR

  subroutine GETSN

    !  SN

    include 'SNCOM.f90'
    integer :: MXL,MXI,MXR
    parameter (MXL = 1,MXI = 2,MXR = 1)

    !     GET THE INTEGER SCALARS.

    call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)
    KODIST = INTS(1)
    ISEFOR = INTS(2)

    !     GET THE LOGICAL SCALARS.

    !**   CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

    !     GET THE REAL SCALARS.

    !**   CALL BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  end subroutine GETSN

end subroutine VARGET

!===========================================!
!   Read variant specific character state   !
!===========================================!
subroutine VARCHGET (CBUFF, IPNT, LNCBUF)
  implicit none

  !     Get variant-specific character data. Only BC has any.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BCPLOT.f90'

  ! Variables
  integer :: LNCBUF,IPNT
  character(len=1) :: CBUFF(LNCBUF)
  character(len=1) :: CT
  integer :: I

  if (VARIANTCD .ne. 'BC') return

  !     GET THE CHARACTER DATA FOR BEC STRUCTURE
  do I = 1,3
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     BEC%Region(I:I) = CT
  end do
  do I = 1,4
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     BEC%Zone(I:I) = CT
  end do
  do I = 1,3
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     BEC%SubZone(I:I) = CT
  end do
  do I = 1,5
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     BEC%Series(I:I) = CT
  end do
  do I = 1,7
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     BEC%FullName(I:I) = CT
  end do
  do I = 1,15
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     BEC%PrettyName(I:I) = CT
  end do

  !     GET THE CHARACTER DATA FOR MORT STRUCTURE
  do I = 1,15
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     MORT%PrettyName(I:I) = CT
  end do

  !     GET THE CHARACTER DATA FOR LTH STRUCTURE
  do I = 1,15
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     LTH%PrettyName(I:I) = CT
  end do

  !     GET THE CHARACTER DATA FOR STG STRUCTURE
  do I = 1,15
     call CHREAD (CBUFF,IPNT,LNCBUF,CT,2)
     STG%PrettyName(I:I) = CT
  end do

  return
end subroutine VARCHGET
