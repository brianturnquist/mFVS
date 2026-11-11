!=================!
!   rd : rdppgt   !
!=================!
! RD $Id$
subroutine RDPPGT (WK3, IPNT, ILIMIT)
  implicit none

  !  Purpose:
  !     Get (read) the Root Disease model data for a given stand from
  !     buffer.
  !     This is part of the Parallel Processing Extension.

  !  Call list definitions:
  !     ILIMIT: (I)  Size of buffer WK3.
  !     IPNT:   (IO) Pointer to curent element in print buffer WK3.
  !     WK3:    (IO) Work array used as a buffer.

  !  Local variable definitions:
  !     I:      Loop counter.
  !     J:      Loop counter.
  !     K:      Loop counter.
  !     INTS:   Array of length MXI to hold integer values.
  !     LOGICS: Array of length MXL to hold logical values.
  !     REALS:  Array of length XMR to hold real values.

  !  Common block variables and parameters:
  !     See comments in common block files.

  !  Revision History :
  !     04-FEB-2000 - Lance R. David (FHTET)
  !       This subroutine was written.
  !     28-AUG-2001 - Lance R. David (FHTET)
  !       Added new RDCOM variables LBBON, LRTYPE, IDOBB, IROOT
  !     22-JUL-02 Lance R. David (FHTET)
  !       Removed unused array PROBO from RDARRY common area processing.
  !   09/10/14 Lance R. David (FMSC)
  !     Updated, added implicit none and declared variables.

  !.... Parameter statements.
  ! Variables
  integer :: MXL,MXR,MXI
  parameter (MXL = 11,MXR = 30,MXI = 45)

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'METRIC.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'RDADD.f90'
  include 'RDARRY.f90'
  include 'RDCOM.f90'
  include 'RDCRY.f90'

  !     READ ALL INTEGER VARIABLES WITH IFREAD, LOGICAL VARIABLES
  !     WITH LFREAD, AND REAL VARIABLES WITH BFREAD.
  !     LONG INTEGERS ARE EQIVALENCED TO REAL VARIABLES AND READ
  !     WITH BFREAD.
  !     DOUBLE PRECISION VARIABLES (RANDOM NUMBER SEEDS) ARE EQUIVALENCED
  !     TO REAL ARRAYS OF LENGTH 2 AND WRITTEN WITH BFREAD.

  !.... Variable declarations.

  logical :: LOGICS(MXL)
  integer :: INTS(MXI),I,J,K,ILIMIT,IPNT
  real    :: REALS(MXR),WK3(*)
  real    :: SEED0(2),SEED1(2)
  equivalence (SEED0,S0), (SEED1,S1)

  !     Read logical scalars from buffer.

  call LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

  !.... Load logical scalars from LOGICS array.

  !---- from common METRIC ------------
  LMTRIC = LOGICS ( 1)
  !---- from common RDADD ------------
  BB1GO = LOGICS ( 2)
  BB2GO = LOGICS ( 3)
  LPLINF = LOGICS ( 4)
  REINGO = LOGICS ( 5)
  RRMAN = LOGICS ( 6)
  RRTINV = LOGICS ( 7)
  WINGO = LOGICS ( 8)
  !---- from common RDCOM ------------
  LBBON = LOGICS ( 9)
  LRTYPE = LOGICS (10)
  LXNOTE = LOGICS (11)

  !     Read logical arrays from buffer.

  call LFREAD (WK3, IPNT, ILIMIT, LBORAX,      2, 2)
  call LFREAD (WK3, IPNT, ILIMIT, LPAREA, ITOTRR, 2)
  call LFREAD (WK3, IPNT, ILIMIT, LSPFLG,      3, 2)

  !.... Read integer scalars from buffer.

  call IFREAD (WK3, IPNT, ILIMIT, INTS, MXI, 2)

  !.... Load integer scalars from the INTS array.

  !---- from common RDADD ------------
  ISDOUT = INTS ( 1)
  MINRR = INTS ( 2)
  MAXRR = INTS ( 3)
  PINT = INTS ( 4)
  !---- from common RDCOM ------------
  IBBOUT = INTS ( 5)
  IDOBB = INTS ( 6)
  IDRDOUT(1) = INTS ( 7)
  IDRDOUT(2) = INTS ( 8)
  IDRDOUT(3) = INTS ( 9)
  IFRRC = INTS (10)
  IIEND = INTS (11)
  ILEND = INTS (12)
  IMCOUT = INTS (13)
  INFLAG = INTS (14)
  IOUNIT = INTS (15)
  IPUSH = INTS (16)
  IRCOMP = INTS (17)
  IRDOUT = INTS (18)
  IRFLAG = INTS (19)
  IRHAB = INTS (20)
  IRINIT = INTS (21)
  IRIPTY = INTS (22)
  IROOT = INTS (23)
  IRRSP = INTS (24)
  IRSNYR = INTS (25)
  IRSPTY = INTS (26)
  IRSTYP = INTS (27)
  IRUNIT = INTS (28)
  ISTEP = INTS (29)
  ISTFLG = INTS (30)
  IYEAR = INTS (31)
  JRSIT = INTS (32)
  NINSIM = INTS (33)
  NMONT = INTS (34)
  NNCENT = INTS (35)
  NNDEC = INTS (36)
  NNDMX = INTS (37)
  NNINF = INTS (38)
  NRSTEP = INTS (39)
  NSTUMP = INTS (40)
  NTREES = INTS (41)
  NUMBB = INTS (42)
  NUMTRE = INTS (43)
  MINRR = INTS (44)
  MAXRR = INTS (45)

  !.... Read real scalars from buffer.

  call BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)

  !.... Load real scalars from the REALS array.

  !---- from common RDADD ------------
  BODBH = REALS ( 1)
  BOTRT = REALS ( 2)
  SDISLP = REALS ( 3)
  SDNORM = REALS ( 4)
  SS = REALS ( 5)
  YINCPT = REALS ( 6)
  !---- from common RDCOM ------------
  AGECUR = REALS ( 7)
  CURAGE = REALS ( 8)
  DGTOT = REALS ( 9)
  DIMEN = REALS (10)
  DSEED = REALS (11)
  HTIMP = REALS (12)
  HTTOT = REALS (13)
  PINSET = REALS (14)
  PPUSH = REALS (15)
  PRREM = REALS (16)
  PTRE = REALS (17)
  ROTSIT = REALS (18)
  ROWIND = REALS (19)
  ROWMIN = REALS (20)
  RRIARE = REALS (21)
  RRIDIM = REALS (22)
  RRIMEN = REALS (23)
  RRSARE = REALS (24)
  RRSFRN = REALS (25)
  SAREA = REALS (26)
  TSTEMS = REALS (27)
  WINDN = REALS (28)
  YTKILL = REALS (29)
  OLDPRP = REALS (30)

  !.... Read double precision scalars (random number seeds) from buffer.

  call BFREAD (WK3, IPNT, ILIMIT, SEED0, 2, 2)
  call BFREAD (WK3, IPNT, ILIMIT, SEED1, 2, 2)

  !.... Read integer arrays from buffer.

  !     Note on processing arrays:
  !     When handling a 2-dimensional array, consider a string of values a
  !     complete column of the array and the length of the array as the
  !     number of rows. So, a call to IFREAD or BFREAD will process 1 column
  !     of the specified number of rows (length) in a 2-dimensional array.

  !     CALL IFREAD (WK3,IPNT,ILIMIT, array, length, 2)
  !---- from common RDADD ------------
  call IFREAD (WK3, IPNT, ILIMIT, DEDAGE,             5, 2)
  do I = 1, 50
     call IFREAD (WK3, IPNT, ILIMIT, IANPLT(1,I),   ITOTRR, 2) !(ITOTRR,50)
     20 continue
  end do
  call IFREAD (WK3, IPNT, ILIMIT, ISDATE,             3, 2)
  do I = 1, 100
     call IFREAD (WK3, IPNT, ILIMIT, ICENSP(1,I),   ITOTRR, 2) !(ITOTRR,100)
     22 continue
  end do
  call IFREAD (WK3, IPNT, ILIMIT, IDPLOT,        MAXTRE, 2)
  do I = 1, 50
     call IFREAD (WK3, IPNT, ILIMIT, INFISD(1,I),   ITOTRR, 2) !(ITOTRR,50)
     24 continue
  end do
  call IFREAD (WK3, IPNT, ILIMIT, IPRFL,         MAXTRE, 2)
  do I = 1, 50
     call IFREAD (WK3, IPNT, ILIMIT, IRDPLT(1,I),   ITOTRR, 2) !(ITOTRR,50)
     26 continue
  end do
  do I = 1, 100
     call IFREAD (WK3, IPNT, ILIMIT, JCENSP(1,I),   ITOTRR, 2) !(ITOTRR,100)
     28 continue
  end do
  call IFREAD (WK3, IPNT, ILIMIT, LONECT,        ITOTRR, 2)
  !---- from common RDARRY ------------
  call IFREAD (WK3, IPNT, ILIMIT, ISTEML,        IRRTRE, 2)
  call IFREAD (WK3, IPNT, ILIMIT, ISTEMI,        IRRTRE, 2)
  !---- from common RDCOM ------------
  call IFREAD (WK3, IPNT, ILIMIT, HOST,         3*MAXSP, 2)
  call IFREAD (WK3, IPNT, ILIMIT, IDITYP,        ITOTSP, 2)
  call IFREAD (WK3, IPNT, ILIMIT, IPCFLG,        ITOTRR, 2)
  call IFREAD (WK3, IPNT, ILIMIT, IRGEN,             10, 2)
  call IFREAD (WK3, IPNT, ILIMIT, IRTSPC,         MAXSP, 2)
  call IFREAD (WK3, IPNT, ILIMIT, ISPS,          ITOTSP, 2)
  do I = 1, 2
     do J = 1, 5
        do K = 1, 41
           call IFREAD (WK3, IPNT, ILIMIT, JRAGED(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
           30 continue
        end do
        31 continue
     end do
     32 continue
  end do
  do I = 1, 50
     call IFREAD (WK3, IPNT, ILIMIT, MCTREE(1,I),   ITOTRR, 2) !(ITOTRR,50)
     33 continue
  end do

  call IFREAD (WK3, IPNT, ILIMIT, NCENTS,        ITOTRR, 2)
  call IFREAD (WK3, IPNT, ILIMIT, NSCEN,         ITOTRR, 2)

  !---- from common RDCRY ------------
  !     no integer arrays

  !.... Read real arrays from buffer.

  !---- from common RDADD ------------
  do I = 1, 50
     call BFREAD (WK3, IPNT, ILIMIT, ANUINF(1,I),    ITOTRR, 2) !(ITOTRR,50)
     34 continue
  end do
  do I = 1, IRRTRE
     call BFREAD (WK3, IPNT, ILIMIT, BBKILL(1,I),         4, 2) !(4,IRRTRE)
     35 continue
  end do
  do I = 1, 2
     do J = 1, 5
        do K = 1, 3
           call BFREAD (WK3, IPNT, ILIMIT, DBHOUT(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
           36 continue
        end do
        37 continue
     end do
     38 continue
  end do
  do I = 1, 2
     do J = 1, 5
        do K = 1, 3
           call BFREAD (WK3, IPNT, ILIMIT, DBHUIN(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
           39 continue
        end do
        40 continue
     end do
     41 continue
  end do
  do I = 1, 2
     do J = 1, 2
        call BFREAD (WK3, IPNT, ILIMIT, DECFN(1,I,J),   ITOTRR, 2) !(ITOTRR,2,2)
        42 continue
     end do
     43 continue
  end do
  do I = 1, 4
     do J = 1, 2
        call BFREAD (WK3, IPNT, ILIMIT, DPROB(1,I,J),  IRRTRE, 2) !(IRRTRE,4,2)
        44 continue
     end do
     45 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, FFPROB(1,1),   IRRTRE, 2) !(IRRTRE,2)
  call BFREAD (WK3, IPNT, ILIMIT, FFPROB(1,2),   IRRTRE, 2) !----------
  do I = 1, IRRTRE
     call BFREAD (WK3, IPNT, ILIMIT, OAKL(1,I),          3, 2) !(3,IRRTRE)
     46 continue
  end do
  do I = 1, 50
     call BFREAD (WK3, IPNT, ILIMIT, PLPROP(1,I),   ITOTRR, 2) !(ITOTRR,50)
     47 continue
  end do
  do I = 1, IRRTRE
     call BFREAD (WK3, IPNT, ILIMIT, PROAKL(1,I),        3, 2) !(3,IRRTRE)
     48 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, RINNF,         ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RINUF,         ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RISTU,         ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRNEW,         ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RSITFN(1,1),   ITOTRR, 2) !(ITOTRR,2)
  call BFREAD (WK3, IPNT, ILIMIT, RSITFN(1,2),   ITOTRR, 2) !----------
  do I = 1, 2
     do J = 1, 5
        do K = 1, 3
           call BFREAD (WK3, IPNT, ILIMIT, RTOUT(1,I,J,K), ITOTRR,2) !(ITOTRR,2,5,3)
           49 continue
        end do
        50 continue
     end do
     51 continue
  end do
  do I = 1, 2
     do J = 1, 5
        do K = 1, 3
           call BFREAD (WK3, IPNT, ILIMIT, RTUIN(1,I,J,K), ITOTRR,2) !(ITITRR,2,5,3)
           52 continue
        end do
        53 continue
     end do
     54 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, SPDBH,         ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, SPINF,         ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, SPPROP,        ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, SPTRAN,        ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, SPYTK,         ITOTRR, 2)
  do I = 1, 2
     do J = 1, 5
        do K = 1, 3
           call BFREAD (WK3, IPNT, ILIMIT, STOUT(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,3)
           55 continue
        end do
        56 continue
     end do
     57 continue
  end do
  do I = 1, 2
     do J = 1, 5
        do K = 1, 3
           call BFREAD (WK3, IPNT, ILIMIT, STUIN(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,3)
           58 continue
        end do
        59 continue
     end do
     60 continue
  end do
  do I = 1, 2
     do J = 1, 2
        call BFREAD (WK3, IPNT, ILIMIT, YRSITF(1,I,J), ITOTRR, 2) !(ITOTRR,2,2)
        61 continue
     end do
     62 continue
  end do

  !---- from common RDARRY ------------
  call BFREAD (WK3, IPNT, ILIMIT, FPROB,         IRRTRE, 2)
  do I = 1, 41
     do J = 1, 2
        call BFREAD (WK3, IPNT, ILIMIT, PROBI(1,I,J),  IRRTRE, 2) !(IRRTRE,41,2)
        63 continue
     end do
     64 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, PROBIT,        IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, PROBIU,        IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, PROBL,         IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, PROP1,             82, 2)
  do I = 1, 41
     do J = 1, 2
        call BFREAD (WK3, IPNT, ILIMIT, PROPI(1,I,J),  IRRTRE, 2) !(IRRTRE,41,2)
        65 continue
     end do
     66 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, ROOTL,         IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRKILL,        IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, TXP8,              82, 2)
  call BFREAD (WK3, IPNT, ILIMIT, WINDWL,        IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, WINDWI,        IRRTRE, 2)
  !---- from common RDCOM ------------
  call BFREAD (WK3, IPNT, ILIMIT, AREANU,        ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, CORINF(1,1),   ITOTRR, 2) !(ITOTRR,2)
  call BFREAD (WK3, IPNT, ILIMIT, CORINF(1,2),   ITOTRR, 2) !----------
  do I = 1, 2
     do J = 1, 5
        call BFREAD (WK3, IPNT, ILIMIT, DBHD(1,I,J),   ITOTRR, 2) !(ITOTRR,2,5)
        67 continue
     end do
     68 continue
  end do
  do I = 1, 2
     do J = 1, 5
        do K = 1, 41
           call BFREAD (WK3, IPNT, ILIMIT, DBHDA(1,I,J,K),ITOTRR, 2) !(ITOTRR,2,5,41)
           69 continue
        end do
        70 continue
     end do
     71 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, DBIFAC,        ITOTSP, 2)
  do I = 1, 2
     do J = 1, 5
        do K = 1, 41
           call BFREAD (WK3, IPNT, ILIMIT, DECRAT(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
           72 continue
        end do
        73 continue
     end do
     74 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, DICLAS,           4, 2)
  call BFREAD (WK3, IPNT, ILIMIT, DSFAC,            2, 2)
  call BFREAD (WK3, IPNT, ILIMIT, EXPINF(1,1), ITOTRR, 2) !(ITOTRR,2)
  call BFREAD (WK3, IPNT, ILIMIT, EXPINF(1,2), ITOTRR, 2) !----------
  call BFREAD (WK3, IPNT, ILIMIT, FRINGE,      ITOTRR, 2)

  do I = 1, ITOTRR
     do J = 1, 2
        call BFREAD (WK3, IPNT, ILIMIT, HABFAC(1,I,J),ITOTSP,2) !(ITOTSP,ITOTRR,2)
        75 continue
     end do
     76 continue
  end do

  call BFREAD (WK3, IPNT, ILIMIT, HTIFAC,      ITOTSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, IURATE,     3*MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, IIRATE,     3*MAXSP, 2)
  do I = 1, 50
     call BFREAD (WK3, IPNT, ILIMIT, MCRATE(1,I), ITOTRR, 2) !(ITOTRR,50)
     77 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, MINDBH,     3*MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, MININF,     3*MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, ORATE,      3*MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, OFRATE,     3*MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, OOAREA,      ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, PAREA,       ITOTRR, 2)
  do I = 1, 100
     do J = 1, 3
        call BFREAD (WK3, IPNT, ILIMIT, PCENTS(1,I,J),ITOTRR,2) !(ITOTRR,100,3)
        78 continue
     end do
     79 continue
  end do
  do I = 1, ITOTRR
     call BFREAD (WK3, IPNT, ILIMIT, PCOLO(1,I),  ITOTSP, 2) !(ITOTSP,ITOTRR)
     80 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, PISIZE,      ITOTRR, 2)
  do I = 1, ITOTRR
     call BFREAD (WK3, IPNT, ILIMIT, PKILLS(1,I), ITOTSP, 2) !(ITOTSP,ITOTRR)
     81 continue
  end do
  do I = 1, ITOTRR
     call BFREAD (WK3, IPNT, ILIMIT, PNINF(1,I),  ITOTSP, 2) !(ITOTSP,ITOTRR)
     82 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, PRANKL,      IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, PRINF, ITOTRR+MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, PRKILL,      ITOTRR, 2)
  do I = 1, 2
     do J = 1, 5
        call BFREAD (WK3, IPNT, ILIMIT, PROBD(1,I,J), ITOTRR,2) !(ITOTRR,2,5)
        83 continue
     end do
     84 continue
  end do
  do I = 1, 2
     do J = 1, 5
        do K = 1, 41
           call BFREAD (WK3, IPNT, ILIMIT, PROBDA(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
           85 continue
        end do
        86 continue
     end do
     87 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, PROOT,        ITOTSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, PRPTOT, ITOTRR+MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, PRUN,         ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RADINF(1,1),      50, 2) !(50,2)
  call BFREAD (WK3, IPNT, ILIMIT, RADINF(1,2),      50, 2) !------
  call BFREAD (WK3, IPNT, ILIMIT, RDKILL,       IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, REINEK,            4, 2)
  call BFREAD (WK3, IPNT, ILIMIT, ROCRI,         MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, ROCRL,         MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, ROOT4,             4, 2)
  do I = 1, 2
     do J = 1, 5
        call BFREAD (WK3, IPNT, ILIMIT, ROOTD(1,I,J), ITOTRR, 2) !(ITOTRR,2,5)
        88 continue
     end do
     89 continue
  end do
  do I = 1, 2
     do J = 1, 5
        do K = 1, 41
           call BFREAD (WK3, IPNT, ILIMIT, ROOTDA(1,I,J,K),ITOTRR,2) !(ITOTRR,2,5,41)
           90 continue
        end do
        91 continue
     end do
     92 continue
  end do
  do I = 1, IRRTRE
     call BFREAD (WK3, IPNT, ILIMIT, ROOTH(1,I),       4, 2) !(4,IRRTRE)
     93 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, ROWDOM,      ITOTSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, ROWIBP(1,1), ITOTSP, 2) !(ITOTSP,2)
  call BFREAD (WK3, IPNT, ILIMIT, ROWIBP(1,2), ITOTSP, 2) !----------
  do I = 1, 100
     call BFREAD (WK3, IPNT, ILIMIT, RRATES(1,I), ITOTRR, 2) !(ITOTRR,100)
     94 continue
  end do
  do I = 1, 10
     call BFREAD (WK3, IPNT, ILIMIT, RRGEN(1,I),  ITOTRR, 2) !(ITOTRR,10)
     95 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, RRINCS,      ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRIRAD,      IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRJINC,      ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RROBMR,      ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RROBNK,       MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RROBOL,           4, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RROBRD,           4, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RROBSC,           4, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RROBTS,           4, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RROOTT,      IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRPSWT,      ITOTSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRRATE,      ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRRSET,      ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRSDBH,          50, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RRSRAD,          50, 2)
  call BFREAD (WK3, IPNT, ILIMIT, RSLOP,       ITOTSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, SDRATE,      IRRTRE, 2)
  do I = 1, 100
     do J = 1, 3
        call BFREAD (WK3, IPNT, ILIMIT, SHCENT(1,I,J),ITOTRR,2) !(ITOTRR,100,3)
        96 continue
     end do
     97 continue
  end do
  do I = 1, 50
     call BFREAD (WK3, IPNT, ILIMIT, SPRQMD(1,I), ITOTRR, 2) !(ITOTRR,50)
     98 continue
  end do
  do I = 1, ITOTRR
     call BFREAD (WK3, IPNT, ILIMIT, SSSFAC(1,I), ITOTSP, 2) !(ITOTSP,ITOTRR)
     99 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, STCUT,            5, 2)
  call BFREAD (WK3, IPNT, ILIMIT, STEMSI,       MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, STEMSL,       MAXSP, 2)
  call BFREAD (WK3, IPNT, ILIMIT, TNJUMP,      ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, TXP12,            4, 2)
  do I = 1, 4
     call BFREAD (WK3, IPNT, ILIMIT, WINDSP(1,I),  MAXSP, 2) !(MAXSP,4)
     100 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, WK22,        IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, XDBH,             2, 2)
  call BFREAD (WK3, IPNT, ILIMIT, XHT,              2, 2)
  call BFREAD (WK3, IPNT, ILIMIT, XMINKL,      ITOTRR, 2)
  call BFREAD (WK3, IPNT, ILIMIT, XMINLF,      ITOTRR, 2)
  do I = 1, IRRTRE
     call BFREAD (WK3, IPNT, ILIMIT, XMTH(1,I),        4, 2) !(4,IRRTRE)
     101 continue
  end do
  call BFREAD (WK3, IPNT, ILIMIT, XRRI,        IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, XRRS,            50, 2)
  call BFREAD (WK3, IPNT, ILIMIT, XXINF,            5, 2)
  call BFREAD (WK3, IPNT, ILIMIT, YDBH,             2, 2)
  call BFREAD (WK3, IPNT, ILIMIT, YHT,              2, 2)
  call BFREAD (WK3, IPNT, ILIMIT, YRRI,        IRRTRE, 2)
  call BFREAD (WK3, IPNT, ILIMIT, YRRS,            50, 2)
  call BFREAD (WK3, IPNT, ILIMIT, YYINF,            5, 2)
  call BFREAD (WK3, IPNT, ILIMIT, CDF,           1001, 2)
  !---- from common RDCRY ------------
  do I = 1, 2
     do J = 1, 5
        call BFREAD (WK3, IPNT, ILIMIT, PROBIN(1,I,J), ITOTRR, 2) !(ITOTRR,2,5)
        102 continue
     end do
     103 continue
  end do
  do I = 1, 2
     do J = 1, 5
        call BFREAD (WK3, IPNT, ILIMIT, CRNSTO(1,I,J), ITOTRR, 2) !(ITOTRR,2,5)
        104 continue
     end do
     105 continue
  end do

  return
end
