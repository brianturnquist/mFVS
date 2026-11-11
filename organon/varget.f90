!=================================!
!   Read variant specific state   !
!=================================!
! ORGANON $Id$
subroutine VARGET (WK3,IPNT,ILIMIT,REALS,LOGICS,INTS)
  implicit none

  !     READ THE VARIANT SPECIFIC VARIABLES.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ORGANON.f90'

  !     NOTE: THE ACTUAL STORAGE LIMIT FOR INTS, LOGICS, AND REALS
  !     IS MAXTRE (SEE PRGPRM).

  !     ALL VARIABLES ARE IN THE ORGANON.F77 COMMON BLOCK

  ! Variables
  integer :: ILIMIT,IPNT,MXL,MXI,MXR,I,II
  parameter (MXL = 3,MXI = 8,MXR = 16)
  logical :: LOGICS(MXL)
  real    :: WK3(MAXTRE)
  integer*4 :: INTS(MXI)
  real*4  :: REALS(MXR)
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

  return
end
