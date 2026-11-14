!==============================!
!   archive/bgc/src : ENTITY   !
!==============================!

!  THIS IS A FILE OF THE VARIABLES FOR PLANT CHARACTERISTICS, ETC.
!        -- DWC, 7/93.
! 5/27/04 ajm  adding new variable sumwp_al.  This is the sum of daily SWPs to be
! used in allocation routine.  Now, water stress year is user-defineable (parameter
! B1(13,1).  We zero-out SUMWP on user-defineable water stress year, and assign
! the accumulated SUMWPs to date (for the last 365 days) to this new var SUMWP_AL
! ("al" for "allocation routine")
! 5/28 ADDING 2 NEW VARS.  WE ARE NOW ALSO BOOKKEEPPING PSN FOR THE USER-DEFINEABLE YEAR
! (DEFINED VIA B1(13,1).  NEW VARS ARE AFPSN2 AND AFMR2.
! ADDING VARs "COUNT" & "AFCOUNT", INDEXED BY ENTITY, FOR TEST OF NEW ALLOCATION FRACTION
! LOGIC (WHEREBY WE ACCUMULATE STRESDAYS ON NEW USER-DEFINED YEAR.  COUNT CHECKS
! TO SEE IF # OF DAYS ACCUMULATED IS CORREECT)AJM 6/2/04

! Variables
integer :: SIZE,ND,NUMYRS
real    :: AREA
parameter (SIZE = 1400)
parameter (AREA = 10000.0)
parameter (ND = 365)

character(len=60) :: CARD
character(len=1) :: ID(SIZE)
character(len=1) :: VID(SIZE)
character(len=4) :: SPP(SIZE)
character(len=4) :: VSP(SIZE)
logical :: LEAF_ON(SIZE),FROOT_ON(SIZE)
integer :: TLEAFON(SIZE),FLAG,TREENO(SIZE),ITON,ITOFF,IGON,IGOFF
integer :: NSTR,NUM,NUM2,ISOURCE,NT,NLF,ISON,ISOFF
integer :: NB,NVE,NR,IB,IBLB(SIZE),BGCISP(SIZE),INDEX(SIZE*2)
real    :: D(SIZE),BD(SIZE),H(SIZE),TPH(SIZE),PCOVER(SIZE)
real    :: CR(SIZE),VCOV(SIZE),VHT(SIZE)
real    :: SUMDG(SIZE),SUMHG(SIZE),PASSDG(SIZE),PASSHG(SIZE)
real    :: BA(SIZE),TOTBA,CCF(SIZE),SUMCCF,BARE,GROUND
real    :: LEAF(SIZE),STEM(SIZE),ROOT(SIZE),STAT(SIZE)
real    :: LA(SIZE),SUMLA,OCPNCY(SIZE),MAXCRHT,MINCRHT
real    :: INTERVAL(3,3),CW(SIZE),BETA(SIZE),EXPAND(SIZE)
real    :: CBOT(SIZE),CTOP(SIZE),ZONE(SIZE*2),LLA(SIZE,SIZE*2)
real    :: LAI(SIZE,SIZE*2),BASE(SIZE,SIZE*2),ZCOVER(SIZE*2)
real    :: SNOW,RAIN,SNWMELT,ZONELA(SIZE*2),FOLDENS(SIZE)
real    :: RATIO(SIZE),SITELAI
real    :: A_(SIZE),YRMRLF(SIZE),YRMRST(SIZE),YRMRRT(SIZE)

integer :: JD
real    :: TMAX,TMIN,RH,XRAD,PPT,TR

real    :: Z1(20)

real    :: ZQ1ABS(SIZE,SIZE*2),ZQ2ABS(SIZE,SIZE*2)
real    :: TZQ1(SIZE*2),TZQ2(SIZE*2)
real    :: ZQ1(SIZE*2),ZQ2(SIZE*2),ETZQ1(SIZE),ETZQ2(SIZE)
real    :: SUMCOV

!      REAL SWC, GRNDH20, MAXSWC, SNOWPACK, SITEWP
!      REAL BUCKET, VOLCONT, H2OMAX
real    :: GSFINAL,TRANSP,DAYPSN,TOTGRESP,TOTMRESP,TOTRESP
real    :: ABGRESP,SUMPSN,SUMTRANS,SUMGRESP,SUMMRESP,DAYTRANS
real    :: SUMWP(SIZE),SUMPSI(SIZE),ETYPSN(SIZE),ETYTRANS(SIZE)
real    :: ETYGRESP(SIZE),ETYMRESP(SIZE),ETYZQ1(SIZE)
real    :: ETYLFGR(SIZE),ETYSTGR(SIZE),ETYFRGR(SIZE)
real    :: AFPSN(SIZE),AFTRANS(SIZE),AFGRESP(SIZE),AFMRESP(SIZE)
real    :: YRPSN,YRTRANS,YRMRESP,YRGRESP,XPSN,TRANS
real    :: GRLEAF,GRSTEM,GRFROOTS,TSTEM

real    :: LEAFCF(SIZE),STEMCF(SIZE),FROOTCF(SIZE)
real    :: O_D(SIZE),O_BD(SIZE),O_H(SIZE),O_CR(SIZE),O_TPH(SIZE)
real    :: O_BA(SIZE),O_TOTBA,O_PCOVER(SIZE),O_CW(SIZE)
real    :: O_LEAF(SIZE),O_STEM(SIZE),O_ROOT(SIZE)
real    :: O_LA(SIZE),O_SUMLA
real    :: S_LEAF(SIZE),S_STEM(SIZE),S_ROOT(SIZE)
real    :: S_D(SIZE),S_BD(SIZE),S_H(SIZE),S_CR(SIZE),S_TPH(SIZE)

real    :: TOLEAF(SIZE),TOSTEM(SIZE),TOFROOT(SIZE),TOTALTO(SIZE)

integer :: IDEADFLG(SIZE),O_NB,COUNT(SIZE),AFCOUNT(SIZE)

integer :: nsl,sl,maxsl,bestsl,tf1,tf2 !*!
real    :: swc(SIZE),swc2(SIZE),sd(SIZE),outflow(SIZE),snowpack !*!
real    :: sitewp(SIZE),htlimit !*!
real    :: bucket(SIZE),h2omax(SIZE),swp(SIZE),sumsoilwp !*!
real    :: obucket(SIZE)
!  added sumla_sh, la_sh, trans_sh, daytrans_sh, and sumtrans_sh, below. ajm 10/00
real    :: sumla_gr,sumla_sm,sumla_lg,sumla_sh !*!
real    :: la_gr,la_sm,la_lg,la_sh !*!
real    :: trans_gr,trans_sm,trans_lg,trans_sh !*!
real    :: daytrans_gr,daytrans_sm,daytrans_lg,daytrans_sh !*!
real    :: sumtrans_gr,sumtrans_sm,sumtrans_lg,sumtrans_sh !*!
real    :: trans_lg1,trans_lg2,sumtrans_lg1,sumtrans_lg2 !*!
real    :: daytrans_lg1,daytrans_lg2 !*!
real    :: stress(SIZE),SUMWP_AL(SIZE),AFPSN2(SIZE),AFMR2(SIZE) !*!

! ADDING FOR NEW WATER BUCKET METHOD. AJM 7/03

real    :: SUMRT,SUMRT_GR,SUMRT_SH,SUMRT_SM,SUMRT_LG

common /BLBDAT/ TREENO, IBLB, IB, NB, NVE, NR, BGCISP, FLAG,&
     &NSTR, NUM, NUM2, VSP, VHT, VCOV, VID, ISOURCE, NT, NLF,&
     &D, BD, H, TPH, PCOVER, SUMDG, SUMHG, PASSDG, PASSHG,&
     &LEAF_ON, FROOT_ON, TLEAFON, IDEADFLG, NUMYRS,ITON,ITOFF,&
     &CR, LEAF, STEM, ROOT, STAT,IGON,IGOFF,ISON,ISOFF,&
     &BA, TOTBA, CCF, SUMCCF, BARE, GROUND,&
     &LA, SUMLA, OCPNCY, MAXCRHT, MINCRHT,&
     &INTERVAL, CW, BETA, EXPAND,&
     &CBOT, CTOP, ZONE, INDEX,&
     &LAI, BASE, LLA, ZCOVER, ZONELA, FOLDENS,&
     &RATIO, SITELAI,A_,YRMRLF,YRMRST,YRMRRT,&
     !     +  SWC, GRNDH20, MAXSWC, SNOWPACK, SITEWP, SUMPSI,
     !     +  BUCKET, VOLCONT, H2OMAX, SUMWP,
     &SNOWPACK, SUMWP,&
     &ZQ1, ZQ2, ETZQ1, ETZQ2, ZQ1ABS, ZQ2ABS, TZQ1, TZQ2, SUMCOV,&
     &GSFINAL, TRANSP, DAYPSN, TOTGRESP, TOTMRESP, TOTRESP,&
     &ABGRESP, SUMPSN, SUMTRANS, SUMGRESP, SUMMRESP, DAYTRANS,&
     &ETYPSN, ETYTRANS, ETYGRESP, ETYMRESP, ETYZQ1,&
     &ETYLFGR, ETYSTGR, ETYFRGR,&
     &AFPSN, AFTRANS, AFGRESP, AFMRESP,&
     &XPSN, TRANS, GRLEAF, GRSTEM, GRFROOTS,&
     &SNOW, RAIN, SNWMELT,&
     &YRPSN, YRTRANS, YRMRESP, YRGRESP, TSTEM,&

     &nsl, sl, maxsl, bestsl,swc, swc2, sd, outflow,&
     &tf1, tf2,&
     &sitewp, htlimit,&
     &bucket, obucket, h2omax, swp, sumsoilwp,&
     !  added sumla_sh, la_sh, trans_sh, daytrans_sh, sumtrans_sh, below.  ajm 10/00
     &sumla_gr, sumla_sm, sumla_lg, sumla_sh, sumpsi,&
     &la_gr, la_sm, la_lg, la_sh,&
     &trans_gr, trans_sm, trans_lg, trans_sh,&
     &daytrans_gr, daytrans_sm, daytrans_lg, daytrans_sh,&
     &sumtrans_gr, sumtrans_sm, sumtrans_lg, sumtrans_sh,&
     &trans_lg1, trans_lg2, sumtrans_lg1, sumtrans_lg2,&
     &daytrans_lg1, daytrans_lg2,&
     &stress,SUMWP_AL,AFPSN2,AFMR2,COUNT,AFCOUNT,&
     &SUMRT,SUMRT_GR,SUMRT_SH,SUMRT_SM,SUMRT_LG

common /BLBOLD/ O_D, O_BD, O_H, O_CR, O_TPH, O_BA, O_TOTBA,&
     &O_PCOVER, O_LEAF, O_STEM, O_ROOT, O_LA, O_SUMLA, O_NB, O_CW

common /BLBSAVE/S_D, S_BD, S_H, S_CR, S_TPH, S_LEAF, S_STEM,&
     &S_ROOT

common /BLBAF/ LEAFCF, STEMCF, FROOTCF

common /BLBTOVR/ TOLEAF, TOSTEM, TOFROOT, TOTALTO

common /BLBCLM/ JD, TMAX, TMIN, RH, XRAD, PPT, TR

common /BLBZVAR/ Z1

common /BLBCHR/ CARD, ID, SPP
