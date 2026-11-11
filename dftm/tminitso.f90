!=====================!
!   dftm : tminitso   !
!=====================!
! DFTM $Id$
subroutine TMINIT
  implicit none

  !     INITIALIZES THE TUSSOCK MOTH SWITCHES AND VARIABLES

  !     THIS ROUTINE IS CALLED FROM **DFTMIN** ONCE PER STAND

  !  REVISION HISTORY:
  !    15-OCT-2004 Lance R. David (FHTET)
  !      Created from the original SORNEC variant 11 species version
  !      for the new SORNEC 33 species version.
  !    01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'TMCOM1.f90'
  include 'TMEGGS.f90'
  include 'UPPER.f90'
  include 'LOWER.f90'

  ! Variables
  integer :: I
  real    :: TMB0(66),TMB1(36),TMR0(18),TMSEED
  logical :: LSET

  data TMB0 /&
       &0.  ,   0.        , 0.    ,  0.   ,   0.   ,   0.&
       &,   0.  ,   0.        , 0.    ,  0.   ,   0.001,   0.001&
       &,  .002 ,   .003      , .006  ,  .013 ,   .035 ,   .028&
       &,  .025 ,   .028      , .031  ,  .034 ,   .035 ,   .035&
       &,   0.  ,   0.        , 0.    ,  0.   ,   0.   ,   0.&
       &,   0.  ,   0.        , 0.    ,  0.   ,   .001 ,   .001&
       &,  .001 ,   .002      , .003  ,  .010 ,   .016 ,   .042&
       &,  .005 ,   .006      , .007  ,  .021 ,   .033 ,   .056&
       &,  .50  ,   .62       , .75   ,  .80  ,   200. ,   200.&
       &,  150. ,   150.      , 0.    ,  .50  ,   .60  ,   .85&
       &,  .90  ,   .25       , 0.    ,  0.   ,   0.   ,   0.  /

  data TMR0 /&
       &.92  ,   .60       , .07   ,  0.   ,   0.   ,   0.&
       &,  .95  ,   .7        , .1    ,  .02  ,   0.   ,   0.&
       &,  .02  ,   .02       , .02   ,  .02  ,   .02  ,   .02 /

  data TMB1 /&
       &.02  ,   .02       , .02   ,  .02  ,   .02  ,   .02&
       &,  5.4  ,   6.25      , 6.25  ,  2.71 ,   2.27 ,   2.2&
       &,  5.4  ,   6.25      , 6.25  ,  3.69 ,   3.29 ,   3.2&
       &,  1.19 ,   .081      , .1    ,  .1147,   .0886,   .0625&
       &,  .197 ,   52.0626838, .02   ,  .01  ,   0.   ,.50, 6*0./

  !     THE VARIABLES IDFCOD AND IGFCOD ARE VARIANT DEPENDENT.
  !     FOR SOUTHERN OR/NORTHEAST CA 33 SPECIES (S033)
  !       FVS VARIANT THEY ARE :
  !         IDFCOD - 3  (DF SPECIES CODE)
  !         IGFCOD - 12 (GF SPECIES CODE)

  IDFCOD = 3
  IGFCOD = 12
  LNPV2 = .false.
  LNPV3 = .false.
  LRANSD = .false.
  LTMPRM = .false.
  LCHEM = .false.
  TMDEBU = .false.
  JODFTM = 10
  JODFEC = 0
  JOTMDK = 0
  ITMREP = 2
  TMDTRE = .false.
  LPUNCH = .false.
  LDF = .true.
  LGF = .true.
  LREDIS = .true.
  IBMTYP = 4
  NCLAS(1) = 20
  NCLAS(2) = 20
  IPRBMT = 1
  TMEVNT = 0.1
  TMWAIT = 30
  ITMSCH = 1
  ITMETH = 1
  ITMSLV = 0
  TMDEFL = 50.0
  PRBSCL = 1.0
  TOPO = 1.0
  TMASHD = 15.93
  TMPN1 = 0.5
  TMPAST = 1492
  TMSEED = 55329.0

  !.... Initialized random number generator with default seed value.

  LSET = .true.
  call TMRNSD (LSET, TMSEED)

  !     THESE EGG DEFAULTS ARE SITE SPECIFIC FROM THE BLUE MOUNTAINS,
  !     NORTHEASTERN OREGON.  THEY MAY BE CHANGED BUT SHOULD NEVER
  !     BE DEFAULTED TO ZERO.

  IEGTYP = 1
  DFEGG(1) = 11.0
  DFEGG(2) = 9.0
  DFEGG(3) = 7.0
  GFEGG(1) = 15.0
  GFEGG(2) = 10.0
  GFEGG(3) = 7.0
  DFREGG(1) = 9.0
  DFREGG(2) = 2.0
  DFREGG(3) = 0.0
  GFREGG(1) = 11.0
  GFREGG(2) = 3.0
  GFREGG(3) = 0.0

  do I = 1,40
     TMWORK(I) = .false.
     TMYRS(I) = 0
     5 continue
  end do

  TMYRS(41) = 0

  do I = 1,66
     B0(I) = TMB0(I)
     10 continue
  end do

  do I = 1,18
     R0(I) = TMR0(I)
     15 continue
  end do

  do I = 1,36
     B1(I) = TMB1(I)
     20 continue
  end do

  DFPNEW(1) = 26.9
  DFPNEW(2) = 12.6
  GFPNEW(1) = 35.2
  GFPNEW(2) = 7.3
  DFFBIO(1) = 213.8
  DFFBIO(2) = 64.2
  GFFBIO(1) = 227.0
  GFFBIO(2) = 63.7
  WEIGHT(1) = 1.0
  WEIGHT(2) = 1.0

  return
end
