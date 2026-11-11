!====================!
!   lpmpb : mpbint   !
!====================!
! LPMPB $Id$
subroutine MPBINT
  implicit none

  !     INITIALIZES MPB VARIABLES.  CALLED FROM INITRE

  ! Revision History
  !   09/26/91 Last noted revision date.
  !   04/19/02 Lance R. David (FHTET)
  !     Random number generator initialization with default seed value.
  !     Added local variables LSET and MPSEED.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.
  !   08/22/14 Lance R. David (FMSC)
  !     Function name was used as variable name.
  !     changed variable INT to INCRS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'MPBCOM.f90'
  include 'COLCOM.f90'
  include 'MPBETA.f90'

  ! Variables
  logical :: LSET
  integer :: I,INC,INC1,KK,KODE
  real    :: XPRN(10),XZIN(10),MPSEED,PRCENT

  real    :: ADEN,AMPAVE,APD,B1INC,B3ALL,B3INC,BATK,BATKI,BNEW,BYINC,BYT,E1,E2,E3,EARAT,EDEN,EF3,EFFS,&
       &EGGS,FMX,GEX,GFML1,GFML2,GFML3,GTEB,PCTGEM,PCTGEX,PCTGF1,PCTGF2,PCTGF3,PCTGSV,PSE1,PSE3,PSPE1,PSPE3,&
       &PSURV,RHO1,RHO2,RHO3,SKILL,SS,TAGG,TMMPB,TRKILL,XEG,XXG,YOUNG

  data XZIN/0.0000,0.0000,0.0038,0.0128,0.0206,0.0353,0.0500,&
       &0.1429,0.1500,0.1500/

  !     NORMAL Q VALUES (COLES ORIGINALS)

  data XPRN/2 * 1.0,0.9935,0.982,0.965,0.909,0.743,0.309,0.285,&
       &0.285/

  !     HIGH Q VALUES (SLOW MORTALITY).

  !*    DATA PRNOIN/2 * 1.0,0.9970,0.990,0.985,0.935,0.850,0.455,0.325,
  !*   >           0.300/

  !     LOW Q VALUES (FAST MORTALITY).

  !*    DATA PRNOIN/1.,0.998,0.990,0.960,0.900,0.700,0.600,0.285,0.250,
  !*   >           0.200/

  EPS = 1.0D-6
  BMIN = 1.0D-10

  do I = 1,40
     WORKIN(I) = .false.
     100 continue
  end do

  LSADLP = .false.
  LRANST = .false.
  LMPB1 = .false.
  LEPI = .false.
  LAGG = .false.
  LPS = .false.
  LREP = .false.
  LDC = .false.
  LPOPDY = .false.
  LDAM = .true.
  LCRES = .true.
  MPBGRF = .true.
  DEBUIN = .false.
  LCURMR = .false.
  LINVMR = .false.

  JOMPBX = 0
  SADLPP = 0.0
  INCRS = 10
  BETTER(1) = 1.0
  BETTER(2) = 4.0
  MPBYR = 0
  NCLASS = 10
  AMP1 = 1200.0
  AMP2 = 600.0
  IBACK = 4
  CE = 1.0
  CF1 = 0.01
  CF2 = 0.01
  CF3 = 0.5
  CRITAD = 1.5
  NG = 2
  TAFAC = 1.7
  IBOUSE = 0
  HS = 1.0
  MPMXYR = 10
  DST(1) = 3000.0
  DST(2) = 500.0
  DST(3) = 500.0
  STRP = 0.95
  NDAMS = 0
  MPBON = 0
  PRCENT = 7.0
  PCTCO = 65.0
  FORLAT = 44.0
  NEPIYR = 0
  TAMIN = 1.7
  TAMAX = 3.0
  TATOL = 0.2
  EPIPRB = 0.5
  PRBSCL = 1.0
  STRBUG = 500.0
  EXCON = 640.0
  ISTDT = 1

  do I = 1,30
     ACTSRF(I) = 0.0
     AGGPH(I) = 0.9
     REPL(I) = 0.9
     PSPK(I) = 0.0
     PSDL(I) = 10.0
     PSPF(I) = 0.0
     DCPF(I) = 0.0
     DCPK(I) = 0.0
     200 continue
  end do

  do I = 1,10
     ZINMOR(I) = XZIN(I)
     PRNOIN(I) = XPRN(I)
     CURRMR(I) = 0.0
     300 continue
  end do

  !     OPEN DISK FILES FOR THE MPB ROUTINES.

  call MYOPEN (JOMPB, ' ', 4, 133, 0, 1, 1, 0, KODE)

  !     CALL EVUST4 TO SET VALUE FOR MPBTPAK TO UNDEFINED.

  call EVUST4(4)

  !.... Initialized random number generator with default seed value.

  MPSEED = 55329.0
  LSET = .true.
  call MPRNSD (LSET, MPSEED)

  return
end
