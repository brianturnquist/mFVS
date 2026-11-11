!========================!
!   canada/bc : BCPLOT   !
!========================!
! CANADA-BC $Id$

!  DEFINITIONS OF VARIABLES IN 'BCPLOT' COMMON BLOCK:

!     INDEXING CONSTANTS FOR FD & PL MORTALITY MODEL

! Variables
integer :: MRT_ICH,MRT_IDF
integer :: MRT_PW,MRT_LW,MRT_FD,MRT_BG,MRT_HW
integer :: MRT_CW,MRT_PL,MRT_SE,MRT_PY
integer :: MRT_BA_UB,MRT_DBH_UB

parameter (MRT_ICH = 1, MRT_IDF = 2)
parameter (MRT_PW = 1, MRT_LW = 2, MRT_FD = 3)
parameter (MRT_BG = 4, MRT_HW = 5, MRT_CW = 6)
parameter (MRT_PL = 7, MRT_SE = 8, MRT_PY = 9)
parameter (MRT_BA_UB = 5, MRT_DBH_UB = 4)

!     STRUCTURE FOR THE BEC INFORMATION

TYPE BEC_DESCRIPTION
   SEQUENCE
   character (LEN = 3)  :: Region !KAM  - longest strings
   character (LEN = 4)  :: Zone !ESSF - are shown here
   character (LEN = 3)  :: SubZone !mw2
   character (LEN = 5)  :: Series !01-YC
   character (LEN = 7)  :: FullName !ICHmw2
   character (LEN = 15) :: PrettyName !IDFmw1/01-YC
end TYPE

!     STRUCTURE FOR THE FITTED MORTALITY MODELS.

TYPE MRT_DESCRIPTION
   SEQUENCE
   character (LEN = 15)  :: PrettyName ! BEC to site series
   character (LEN = 1)   :: pad ! stop alignment warning
   logical             :: FIT(MAXSP) ! TRUE=fitted
   real                :: CON(MAXSP) ! constant
   real                :: INVDBH(MAXSP) ! 1./diameter (cm**-1)
   real                :: DBH(MAXSP) ! diameter (cm)
   real                :: DBHSQ(MAXSP) ! diameter (cm**2)
   real                :: RDBH(MAXSP) ! relative diameter (cm/cm)
   real                :: BAL(MAXSP) ! BAL (m**2/ha)
   real                :: SQRTBA(MAXSP) ! SQRT((m**2/ha)**0.5)
   real                :: SPH(MAXSP) ! stems/ha (all trees)
end TYPE

!     STRUCTURE FOR THE FITTED LARGE TREE HEIGHT GROWTH MODELS.

TYPE LTH_DESCRIPTION
   SEQUENCE
   character (LEN = 15)  :: PrettyName ! BEC to site series
   character (LEN = 1)   :: pad ! stop alignment warning
   logical             :: FIT(MAXSP) ! TRUE=fitted
   real                :: MLT(MAXSP) ! mult (READCORH keywd)
   real                :: A1(MAXSP) ! species constant
   real                :: B1(MAXSP) !
   real                :: B2(MAXSP) !
   real                :: SI(MAXSP) ! site constant
   real                :: B3(MAXSP) !
   real                :: C(MAXSP) !

   real                :: CN(MAXSP) ! new structure
   real                :: CNSI(MAXSP) !
   real                :: DG(MAXSP) !
   real                :: DG2(MAXSP) !
   real                :: DBH1(MAXSP) !
   real                :: DBH2(MAXSP) !
   real                :: HT2(MAXSP) !
end TYPE

!     STRUCTURE FOR THE SMALL TREE GROWTH MODELS.

TYPE STG_DESCRIPTION
   SEQUENCE
   character (LEN = 15)  :: PrettyName ! BEC to site series
   character (LEN = 1)   :: pad ! stop alignment warning
   logical             :: FIT(MAXSP) ! TRUE=fitted
   real                :: CON(MAXSP) ! spp,SI,site constants
   real                :: LNHT(MAXSP) ! ln(ht) - ln(m)
   real                :: HT(MAXSP) ! ht - m
   real                :: CCF(MAXSP) ! ccf
   real                :: BAL(MAXSP) ! BA larger trees - m**2ha
   real                :: EXPMLT(MAXSP) ! Mutliplier  !SBS only
   real                :: HCCFL(MAXSP) ! HT/CCF      !SBS only
   real                :: CCFSL(MAXSP) ! CCFS/CCFL   !SBS only
   real                :: SD(MAXSP) ! standard deviation
end TYPE

TYPE (BEC_DESCRIPTION) BEC
TYPE (MRT_DESCRIPTION) MORT
TYPE (LTH_DESCRIPTION) LTH
TYPE (STG_DESCRIPTION) STG

!     TMRT -- MORTALITY MODEL FOR FD & PL

real    :: TMRT(MRT_PY,MRT_BA_UB,MRT_DBH_UB)

!     LARGE TREE DG PARMS (NOT ALREADY IN BASE COMMON BLOCKS)

!     DGLD1   - LOG DBH COEFFS (NAME MODIFIED FROM NI)
!     DGBAL1  - BAL COEFFS     (NAME MODIFIED FROM NI)
!     DGCR1   - CR COEFFS      (NAME MODIFIED FROM NI)
!     DGDBAL1 - DBH/BAL COEFFS
!     DGDBAL2 - DBH/BAL COEFFS
!     DGCCF1  - CCF COEFFS     (NAME MODIFIED FROM NI)
!     DGDSQ1  - DSQ COEFFS     (NAME MODIFIED FROM NI)

real    :: DGLD1(MAXSP),DGBAL1(MAXSP),DGCR1(MAXSP)
real    :: DGDBAL1(MAXSP),DGDBAL2(MAXSP),DGCCF1(MAXSP)
real    :: DGDSQ1(MAXSP)

!     ** CROWN PARMS ** (NOT ALREADY IN BASE COMMON BLOCKS)
!     CRHTDBH - HT/DBH COEFFS
!     CRHT    - HT COEFFS
!     CRDBH2  - DBH**2 COEFFS
!     CRBAL   - BAL COEFFS
!     CRLNCCF - LN(CCF) COEFFS
!     CRSD    - STD DEV

real    :: CRHTDBH(MAXSP),CRHT(MAXSP),CRDBH2(MAXSP),CRBAL(MAXSP)
real    :: CRLNCCF(MAXSP),CRSD(MAXSP)

common /BCPLT1 /   BEC
common /BCPLT2 /   MORT,LTH,STG,TMRT
common /DGCOEFFS / DGLD1,DGBAL1,DGCR1,DGDBAL1,DGDBAL2,DGCCF1,&
     &DGDSQ1
common /CRCOEFFS / CRHTDBH,CRHT,CRDBH2,CRBAL,CRLNCCF,CRSD

!     VERSION 2 RE-MERGE: COMMON BLOCKS FOR INTERIM CALIBRATION OF MS,PP,ESSF
!     SOME V2 VARIABLES HAVE PERSISTED IN V3

!   LV2ATV   - .TRUE. IF VERSION 2 INTERIM CALIBRATION IS ACTIVE (BECSET)
!   LV2HTCP  - .TRUE. IF FIRST STAND HAS BEEN READ: CONTROLS COPYING OF ARCHIVE
!              HEIGHT PARAMETERS
!   LV2HDR   - .TRUE. IF VERSION 2 CALIBRATION INFO HAS BEEN PROCESSED & WRITTEN TO MAIN OUTPUT
!   XHT1B    - ARCHIVE COPY OF BLKDAT HT1 ARRAY
!   XHT2B    - ARCHIVE COPY OF BLKDAT HT2 ARRAY
!   LMHTDUB  - .TRUE. IF METRIC-BASED HT-DUBBING VALUES ARE IN USE (V2 & V3)
!   LLTDGOK  - .TRUE. IF SPP CAN GROW IN V2; FALSE OTHERWISE
!   SEICON   - BEC CONSTANTS  (V2)
!   V2SEICOR - CORRECTION TERMS (V2)
!   GMULT_C  - MORTALITY MODEL CALCULATE-ONCE (V2)
!   REIN_C   - MORTALITY MODEL CALCULATE-ONCE (V2)

logical :: LV2ATV,LV2HTCP,LV2HDR
logical :: LLTDGOK(MAXSP),LMHTDUB(MAXSP)
real    :: SEISHT
real    :: V2SEICOR(MAXSP),SEICON(MAXSP)
real    :: XHT1B(MAXSP),XHT2B(MAXSP)
real    :: GMULT_C(2,MAXSP),REIN_C(2,MAXSP)

common /V2PLOT/ V2SEICOR,SEICON,LV2ATV,LV2HDR,LLTDGOK,SEISHT,&
     &GMULT_C,REIN_C
common /V23DUB/ XHT1B,XHT2B,LV2HTCP,LMHTDUB
