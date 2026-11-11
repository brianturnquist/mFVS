!========================!
!   organon : execute2   !
!========================!
! ORGANON $Id$

  !      SUBROUTINE EXECUTE(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,TREENO,
  !     1                   PTNO,SPECIES,USER,INDS,DBH1,HT1,CR1,SCR1,
  !     2                   EXPAN1,MGEXP,SITE_1,SITE_2,ACALIB,DFMSDI,
  !     3                   TFMSDI,OCMSDI,PN,YSF,BABT,BART,YST,NPR,PRAGE,
  !     4                   PRLH,PRDBH,PRHT,PRCR,PREXP,BRCNT,BRHT,BRDIA,
  !     5                   JCORE,SERROR,TERROR,SWARNING,TWARNING,IERROR,
  !     6                   DGRO,HGRO,CRCHNG,SCRCHNG,MORTEXP,STOR)

  !     REDIMENSIONED INDS(30) AND STOR(30) AND ADDED RVARS(30) WHICH INCLUDES
  !     SITE_1, SITE_2, DFMSDI, TFMSDI, OCMSDI, PDEN

  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE CKAGE TO CKAGE_RUN
  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - EDIT TO EDIT_RUN
  !  CHANGED THE NAME OF SUBROUTINE/SUBROUTINE CALL - SPGROUP TO SPGROUP_RUN

subroutine EXECUTE(CYCLG,VERSION,NPTS,NTREES1,STAGE,BHAGE,TREENO,&
     &PTNO,SPECIES,USER,INDS,DBH1,HT1,CR1,SCR1,&
     &EXPAN1,MGEXP,RVARS,ACALIB,PN,YSF,BABT,BART,YST,&
     &NPR,PRAGE,PRLH,PRDBH,PRHT,PRCR,PREXP,BRCNT,&
     &BRHT,BRDIA,JCORE,SERROR,TERROR,SWARNING,&
     &TWARNING,IERROR,DGRO,HGRO,CRCHNG,SCRCHNG,&
     &MORTEXP,NTREES2,DBH2,HT2,CR2,SCR2,EXPAN2,STOR,&
     &OCC2,OAHT2)
  implicit none
  ! Variables
  integer*4 :: CYCLG,VERSION,NPTS,NTREES1,STAGE,BHAGE,TREENO(2000),PTNO(2000),SPECIES(2000),&
       &USER(2000),PRAGE(2000,3),BRCNT(2000,3),BRHT(2000,40),BRDIA(2000,40),JCORE(2000,40),NPR(2000),&
       &SERROR(35),TERROR(2000,6),SWARNING(9),TWARNING(2000)
  integer*4 :: NXT,BNXT,ONXT,IB,BIG6,OTHER,NSPN,TDATAI(2000,3),SPGRP(2000),TCYCLE,FCYCLE
  integer*4 :: IERROR,YCYCLG,NTREES
  integer*4 :: INDS(30)
  !*****START NEW*****
  integer*4 :: NTREES2
  !*****END NEW*****
  real*4  :: DBH1(2000),HT1(2000),CR1(2000),EXPAN1(2000),SCR1(2000),SITE_1,SITE_2,ACALIB(3,18),MSDI_1,&
       &MSDI_2,MSDI_3,PDEN,PN(5),YSF(5),BABT,BART(5),YST(5),PRLH(2000,3),PRDBH(2000,3),PRHT(2000,3),&
       &PRCR(2000,3),PREXP(2000,3),OLD,MGEXP(2000),DGRO(2000),HGRO(2000),MORTEXP(2000),CRCHNG(2000),&
       &SCRCHNG(2000),NO,RD0,STOR(30),RVARS(30)
  real*4  :: TDATAR(2000,8),SBA1,BALL1(51),BAL1(500),GROWTH(2000,4),YF(5),YT(5),SCR(2000,3),&
       &VOLTR(2000,4),DEADEXP(2000),SYTVOL(2000,2),CCH(41),A1,A2,A1MAX,PA1MAX,RAAGE,SI_1,SI_2,RASI,&
       &CCFLL1(51),CCFL1(500),SBA2,CCFLL2(51),CCFL2(500),BALL2(51),BAL2(500),TPA2,SCCF2,CALIB(6,18),CC,X,&
       &MAXRAH,CON_RASI,SCCF1,TPA1,CON,QMD1,RD1,HT40,CCMAX,AHCB,SHCB
  !*****START NEW*****
  real*4  :: DBH2(2000),HT2(2000),CR2(2000),EXPAN2(2000),SCR2(2000)
  real*4  :: GWDG,GWHG,FR,OCC2,OAHT2
  !*****END NEW*****
  logical*2 :: CALH,CALC,CALD,EVEN,TRIPLE,POST,PRUNE,THIN,FERT,MORT,ERROR
  logical*2 :: WOODQ,TRIAL
  logical*2 :: OSTORY,INGRO,B6THIN
  !*****START NEW*****
  logical*2 :: GENETICS,SWISSNC
  !*****END NEW*****
  integer*4 :: I,II,J,IDXAGE,IDXCYC
  !      INTEGER*4 IANS,IYN

  !     SET THE MAXIMUM TREE LEVEL ARRAY SIZES

  NTREES = NTREES1
  YCYCLG = 5*CYCLG
  CALH = .false.
  CALC = .false.
  CALD = .false.
  EVEN = .false.
  TRIPLE = .false.
  PRUNE = .false.
  THIN = .false.
  FERT = .false.
  MORT = .false.
  POST = .false.
  OSTORY = .false.
  INGRO = .false.
  B6THIN = .false.
  !*****START NEW*****
  GENETICS = .false.
  SWISSNC = .false.
  !*****END NEW******
  if (INDS(1) .eq.1) then
     CALH = .true.
  end if
  if (INDS(2) .eq.1) then
     CALC = .true.
  end if
  if (INDS(3) .eq.1) then
     CALD = .true.
  end if
  if (INDS(4) .eq.1) then
     EVEN = .true.
  end if
  if (INDS(5) .eq.1) then
     TRIPLE = .true.
  end if
  if (INDS(6) .eq.1) then
     PRUNE = .true.
  end if
  if (INDS(7) .eq.1) then
     THIN = .true.
  end if
  if (INDS(8) .eq.1) then
     FERT = .true.
  end if
  if (INDS(9) .eq.1) then
     MORT = .true.
  end if
  if (INDS(11) .eq.1) then
     OSTORY = .true.
  end if
  if (INDS(12) .eq.1) then
     INGRO = .true.
  end if
  if (INDS(13) .eq.1) then
     B6THIN = .true.
  end if
  !*****START NEW*****
  if (INDS(14) .eq.1) then
     GENETICS = .true.
  end if
  if (INDS(15) .eq.1) then
     SWISSNC = .true.
  end if
  !*****END NEW******
  SITE_1 = RVARS(1)
  SITE_2 = RVARS(2)
  MSDI_1 = RVARS(3)
  MSDI_2 = RVARS(4)
  MSDI_3 = RVARS(5)
  !*****START NEW*****
  GWDG = RVARS(6)
  GWHG = RVARS(7)
  FR = RVARS(8)
  !*****END NEW******
  PDEN = RVARS(9)
  NO = STOR(1)
  RD0 = STOR(2)
  A1 = STOR(3)
  A2 = STOR(4)
  A1MAX = STOR(5)
  PA1MAX = STOR(6)
  !      CALL EDIT(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,
  !     1          CALH,CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,DBH1,HT1,CR1,
  !     2          EXPAN1,SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN,ACALIB,
  !     3          PN,YSF,BABT,BART,YST,SCR1,MGEXP,IB,NSPN,BIG6,OTHER,BNXT,
  !     4          ONXT,SPGRP,SERROR,TERROR,SWARNING,TWARNING,ERROR)
  !*****START NEW*****
  call EDIT_RUN(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,CALH,&
       &CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,GENETICS,SWISSNC,DBH1,&
       &HT1,CR1,EXPAN1,SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN,&
       &ACALIB,PN,YSF,BABT,BART,YST,SCR1,MGEXP,GWDG,GWHG,FR,IB,&
       &NSPN,BIG6,OTHER,BNXT,ONXT,SPGRP,SERROR,TERROR,SWARNING,&
       &TWARNING,ERROR)
  !*****END NEW******
  IERROR = 0
  if (ERROR) then
     IERROR = 1
     return
  end if
  NXT = BNXT+ONXT
  if (VERSION .eq.1) then
     IDXAGE = 500
     IDXCYC = 100
  else if (VERSION .eq.2.or. VERSION .eq.3) then
     IDXAGE = 120
     IDXCYC = 24
  else
     IDXAGE = 30
     IDXCYC = 30
  end if
  FCYCLE = 0
  TCYCLE = 0
  if (FERT .and. YSF(1) .eq. FLOAT(YCYCLG)) FCYCLE = 1
  if (THIN .and. YST(1) .eq. FLOAT(YCYCLG)) then
     TCYCLE = 1
     POST = .true.
  end if
  WOODQ = .false.
  if (INDS(10) .eq.1) then
     WOODQ = .true.
  end if
  TRIAL = .false.
  do I = 1,2000
     GROWTH(I,3) = 0.0
     GROWTH(I,4) = 0.0
     TDATAI(I,1) = 0
     TDATAI(I,2) = 0
     TDATAI(I,3) = 0
     TDATAR(I,1) = 0.0
     TDATAR(I,2) = 0.0
     TDATAR(I,3) = 0.0
     TDATAR(I,4) = 0.0
     SCR(I,1) = 0.0
     SCR(I,2) = 0.0
     SCR(I,3) = 0.0
     DEADEXP(I) = 0.0
     DBH2(I) = 0.0
     HT2(I) = 0.0
     CR2(I) = 0.0
     EXPAN2(I) = 0.0
     SCR2(I) = 0.0
     do II = 1,2
        SYTVOL(I,II) = 0.0
     end do
     do II = 1,4
        VOLTR(I,II) = 0.0
     end do
  end do
  do I = 1,NTREES
     TDATAI(I,1) = SPECIES(I)
     TDATAI(I,2) = SPGRP(I)
     !        TDATAI(I,2)=1
     TDATAI(I,3) = USER(I)
     TDATAR(I,1) = DBH1(I)
     TDATAR(I,2) = HT1(I)
     TDATAR(I,3) = CR1(I)
     TDATAR(I,4) = EXPAN1(I)
     SCR(I,1) = SCR1(I)
  end do
  do I = 1,18
     CALIB(4,I) = ACALIB(1,I)
     CALIB(5,I) = ACALIB(2,I)
     CALIB(6,I) = ACALIB(3,I)
     if (CALH) then
        CALIB(1,I) = (1.0+CALIB(4,I))/2.0 + (0.5**(0.5*(CYCLG)))&
             &*((CALIB(4,I)-1.0)/2.0)
     else
        CALIB(1,I) = 1.0
     end if
     if (CALC) then
        CALIB(2,I) = (1.0+CALIB(5,I))/2.0 + (0.5**(0.5*(CYCLG)))&
             &*((CALIB(5,I)-1.0)/2.0)
     else
        CALIB(2,I) = 1.0
     end if
     if (CALD) then
        CALIB(3,I) = (1.0+CALIB(6,I))/2.0 + (0.5**(0.5*(CYCLG)))&
             &*((CALIB(6,I)-1.0)/2.0)
     else
        CALIB(3,I) = 1.0
     end if
  end do
  do I = 1,5
     YF(I) = YSF(I)
     YT(I) = YST(I)
  end do
  !C
  !C     CALCULATE SPECIES GROUP
  !C
  !      CALL SPGROUP(VERSION,NSPN,NTREES,TDATAI)
  if (VERSION .eq.1) then
     if (SITE_1.le.0.0.and. SITE_2.gt.0.0) then
        SITE_1 = 1.062934*SITE_2
     else if (SITE_2.le.0.0) then
        SITE_2 = 0.940792*SITE_1
     end if
  else if (VERSION .eq.2.or. VERSION .eq.3) then
     !  Site index conversion equation from Nigh (1995, Forest Science 41:84-98)
     if (SITE_1.le.0.0.and. SITE_2.gt.0.0) then
        SITE_1 = 0.480 +( 1.110 * SITE_2)
     else if (SITE_2.le.0.0) then
        SITE_2 = -0.432 +( 0.899 * SITE_1)
     end if
  else
     if (SITE_2.le.0.0) then
        SITE_2 = 4.776377*SITE_1**0.763530587
     end if
  end if

  !     CHANGE TO EXPANSION FACTOR FOR STAND--NOT SAMPLE

  do J = 1,NTREES
     TDATAR(J,4) = TDATAR(J,4)/FLOAT(NPTS)
     MGEXP(J) = MGEXP(J)/FLOAT(NPTS)
     TDATAR(J,5) = TDATAR(J,4)
     if (PRUNE) then
        TDATAR(J,6) = SCR(J,1)
     else
        TDATAR(J,6) = TDATAR(J,3)
     end if
     if (SCR(J,1) .gt.0.0) then
        SCR(J,2) = SCR(J,1)
     end if
  end do

  !     CALCULATE RED ALDER SITE INDEX FOR TREES IN NATURAL STANDS

  MAXRAH = 0.0
  if (VERSION .le.3) then
     do J = 1,NTREES
        if (TDATAI(J,1) .eq.351.and. TDATAR(J,2) .gt. MAXRAH) then
           MAXRAH = TDATAR(J,2)
        end if
     end do
     RASI = CON_RASI(SITE_1)
  end if

  !     CALCULATE RED ALDER AGE FOR NATURAL STANDS

  RAAGE = 0.0
  if (MAXRAH .gt.0.0) then
     call RAGEA(MAXRAH,RASI,RAAGE)
     if (RAAGE .le.0.0) then
        RAAGE = 55.0
        call RASITE(MAXRAH,RAAGE,RASI)
     end if
     if ( RAAGE .gt.55.0 ) then
        RAAGE = 55.0
     end if
  end if
  SI_1 = SITE_1-4.5
  SI_2 = SITE_2-4.5

  !     INITIALIZE A1 AND A2

  if (CYCLG .le.0.or. A1.eq.0.0.or. A2.eq.0.0&
       &.or. OSTORY .or. INGRO .or. B6THIN) then
     call SUBMAX(TRIAL,VERSION,NTREES,TDATAI,TDATAR,MGEXP,MSDI_1,&
          &MSDI_2,MSDI_3,A1,A2)
  end if

  !     CALCULATE DENSITY VARIABLES AT SOG

  call SSTATS(VERSION,NTREES,TDATAI,TDATAR,SBA1,TPA1,SCCF1,&
       &BAL1,BALL1,CCFL1,CCFLL1)
  CON = 0.005454154
  QMD1 = SQRT(SBA1/(CON*TPA1))
  RD1 = TPA1/EXP(A1/A2-LOG(QMD1)/A2)

  !     CALCULATE H40 AND MAXIMUM CROWN CLOSURE AT SOG

  call HTFORTY(0.0,VERSION,IB,NTREES,TDATAI,TDATAR,MGEXP,HT40)
  if (HT40.lt.140.0) then
     CCMAX = 100.
  else
     CCMAX = 121.0-0.15*HT40
  end if
  if (RD1.lt.0.4) then
     CCMAX = CCMAX*0.6
  else if (RD1.ge.0.4.and. RD1.lt.0.8) then
     CCMAX = CCMAX*(0.2+RD1)
  end if

  !     CALCULATE CCH AND CROWN CLOSURE AT SOG

  call CRNCLO(0,0.0,VERSION,NTREES,TDATAI,TDATAR,SCR,MGEXP,&
       &CCH,CC)

  !     SET VARIABLES NEEDED BY THE EVENT MONITOR: LOCALLY OCC2 AND OAHT2

  OCC2 = 100.0 * (1.0 - EXP(-0.01*CC))
  OAHT2 = HT40

  OLD = 0.0
  do I = 1,NTREES
     TDATAR(I,8) = TDATAR(I,4)
     if (PRUNE) then
        TDATAR(I,7) = SCR(I,1)
     else
        TDATAR(I,7) = TDATAR(I,3)
     end if
     if (SCR(I,1) .gt.0.0) then
        SCR(I,3) = SCR(I,1)
     end if
  end do
  !      CALL GROW(VERSION,CYCLG,NTREES,NXT,BNXT,ONXT,IB,BIG6,
  !     1          OTHER,NSPN,STAGE,BHAGE,PTNO,TREENO,TDATAI,PRAGE,BRCNT,
  !     2          BRHT,BRDIA,JCORE,NPR,TCYCLE,FCYCLE,TRIPLE,WOODQ,POST,
  !     3          MORT,TDATAR,SI_1,SI_2,SBA1,BALL1,BAL1,CALIB,PN,YF,
  !     4          BABT,BART,YT,GROWTH,PRLH,PRDBH,PRHT,PRCR,PREXP,SCR,
  !     5          VOLTR,SYTVOL,CCH,OLD,MGEXP,DEADEXP,A1,A2,A1MAX,PA1MAX,
  !     6          NO,RD0,RAAGE,RASI,CCFLL1,CCFL1,SBA2,CCFLL2,CCFL2,
  !     7          BALL2,BAL2,TPA2,SCCF2)
  call GROW(VERSION,CYCLG,NTREES,IB,BIG6,OTHER,NSPN,STAGE,BHAGE,&
       &PTNO,TREENO,TDATAI,PRAGE,BRCNT,BRHT,BRDIA,JCORE,NPR,&
       &TCYCLE,FCYCLE,TRIPLE,WOODQ,POST,MORT,GENETICS,SWISSNC,&
       &TDATAR,SI_1,SI_2,SBA1,BALL1,BAL1,CALIB,PN,YF,BABT,BART,&
       &YT,GROWTH,PRLH,PRDBH,PRHT,PRCR,PREXP,SCR,VOLTR,SYTVOL,&
       &CCH,OLD,MGEXP,DEADEXP,A1,A2,A1MAX,PA1MAX,NO,RD0,RAAGE,&
       &RASI,CCFLL1,CCFL1,SBA2,CCFLL2,CCFL2,BALL2,BAL2,TPA2,&
       &SCCF2,GWDG,GWHG,FR,PDEN)
  NTREES2 = NTREES
  POST = .false.
  !      IF(.NOT. EVEN) THEN
  !         BHAGE=0
  !         STAGE=0
  !      ENDIF
  if (INDS(4) .eq.0) then
     BHAGE = 0
     STAGE = 0
  end if
  X = 100.0*(OLD/(BIG6-BNXT))
  if (X .ge.50.0) then
     SWARNING(7) = 1
  end if
  if (VERSION .eq.1) then
     if (EVEN .and. BHAGE .gt.500.) then
        SWARNING(7) = 1
     end if
  else if (VERSION .eq.2.or. VERSION .eq.3) then
     if (EVEN .and. BHAGE .gt.120.) then
        SWARNING(7) = 1
     end if
  else
     if (EVEN .and. STAGE .gt.30.) then
        SWARNING(7) = 1
     end if
  end if
  do I = 1,NTREES2
     SPECIES(I) = TDATAI(I,1)
     USER(I) = TDATAI(I,3)
     MGEXP(I) = 0.0
     DBH2(I) = TDATAR(I,1)
     HT2(I) = TDATAR(I,2)
     CR2(I) = TDATAR(I,3)
     EXPAN2(I) = TDATAR(I,4)*FLOAT(NPTS)
     SCR2(I) = SCR(I,1)
     HGRO(I) = GROWTH(I,1)
     DGRO(I) = GROWTH(I,2)
     MORTEXP(I) = DEADEXP(I)*FLOAT(NPTS)
     AHCB = (1.0-TDATAR(I,3))*TDATAR(I,2)
     SHCB = (1.0-SCR(I,1))*TDATAR(I,2)
     if (AHCB .gt. SHCB) then
        CRCHNG(I) = 0.0
        SCRCHNG(I) = SCR(I,1)-TDATAR(I,7)
     else
        CRCHNG(I) = TDATAR(I,3)-TDATAR(I,7)
        SCRCHNG(I) = 0.0
     end if
  end do
  STOR(1) = NO
  STOR(2) = RD0
  STOR(3) = A1
  STOR(4) = A2
  STOR(5) = A1MAX
  STOR(6) = PA1MAX
  return
end

  !      SUBROUTINE EDIT(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,
  !     1                SPECIES,CALH,CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,
  !     2                DBH,HT,CR,EXPAN,SITE_1,SITE_2,MSDI_1,MSDI_2,
  !     3                MSDI_3,PDEN,ACALIB,PN,YSF,BABT,BART,YST,SCR,MGEXP,
  !     4                IB,NSPN,BIG6,OTHER,BNXT,ONXT,SPGRP,SERROR,TERROR,
  !     5                SWARNING,TWARNING,ERROR)
  !*****START NEW*****
subroutine EDIT_RUN(CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES,&
     &CALH,CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,GENETICS,&
     &SWISSNC,DBH,HT,CR,EXPAN,SITE_1,SITE_2,MSDI_1,&
     &MSDI_2,MSDI_3,PDEN,ACALIB,PN,YSF,BABT,BART,YST,&
     &SCR,MGEXP,GWDG,GWHG,FR,IB,NSPN,BIG6,OTHER,BNXT,&
     &ONXT,SPGRP,SERROR,TERROR,SWARNING,TWARNING,ERROR)
  !*****END NEW******
  implicit none
  ! Variables
  integer*4 :: CYCLG,VERSION,NPTS,NTREES,STAGE,BHAGE,SPECIES(2000),IB,NSPN,BIG6,OTHER,BNXT,ONXT,IIB
  integer*4 :: I,J,SPGRP(2000),SERROR(35),TERROR(2000,6),SWARNING(9),TWARNING(2000),EXCAGE,YCYCLG
  real*4  :: DBH(2000),HT(2000),CR(2000),EXPAN(2000),SITE_1,SITE_2,MSDI_1,MSDI_2,MSDI_3,PDEN,ACALIB(3,&
       &18),PN(5),YSF(5),BABT,BART(5),YST(5),SCR(2000),MGEXP(2000),MAXDF,MAXGF,MAXWH,MAXPP,MAXIC,MAXHT,MAXRA,ICSI
  real*4  :: BA,SBA,B6SBA,HWSBA,B0,B1,PHT,OLD,Z50,X,PRA
  !*****START NEW*****
  real*4  :: GWDG,GWHG,FR
  !*****END NEW*****
  logical*2 :: CALH,CALC,CALD,EVEN,PRUNE,THIN,FERT,MORT,ERROR
  !*****START NEW*****
  logical*2 :: GENETICS,SWISSNC
  !*****END NEW*****
  logical*2 :: LDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  LDANUW = CALC
  LDANUW = CALD
  LDANUW = CALH
  LDANUW = MORT
  LDANUW = PRUNE

  ERROR = .false.
  YCYCLG = 5*CYCLG
  BIG6 = 0
  OTHER = 0
  BNXT = 0
  ONXT = 0
  MAXGF = 0.0
  MAXDF = 0.0
  MAXWH = 0.0
  MAXPP = 0.0
  MAXIC = 0.0
  MAXRA = 0.0
  do I = 1,2000
     do J = 1,6
        TERROR(I,J) = 0
     end do
     TWARNING(I) = 0
  end do
  do I = 1,35
     SERROR(I) = 0
  end do
  do I = 1,9
     SWARNING(I) = 0
  end do
  if (NTREES .lt.1.or. NTREES .gt.2000) then
     SERROR(1) = 1
  end if
  if (VERSION .lt.1.or. VERSION .gt.4) then
     SERROR(2) = 1
  end if
  if (NPTS .le.0) then
     SERROR(3) = 1
  end if
  if (SITE_1.le.0.0.and. SITE_2.le.0.0) then
     SERROR(4) = 1
  end if
  if (EVEN .and. BHAGE .le.0) then
     SERROR(6) = 1
  end if
  if (.not. EVEN .and. BHAGE .gt.0) then
     SERROR(7) = 1
  end if
  if (EVEN .and. (STAGE-BHAGE) .lt.1) then
     SERROR(8) = 1
  end if
  if (.not. EVEN .and. FERT) then
     SERROR(9) = 1
  end if
  do I = 1,5
     if (.not. FERT .and. (YSF(I) .ne.0.or. PN(I) .ne.0)) then
        SERROR(10) = 1
     end if
     if (FERT) then
        if (YSF(I).gt. STAGE .or. YSF(I) .gt.70.0) then
           SERROR(11) = 1
        end if
        if (I .eq.1) then
           if (PN(I) .le.0.0.or. PN(I) .gt.400.0) then
              SERROR(12) = 1
           end if
        else
           if (PN(I) .gt.400.0) then
              SERROR(12) = 1
           end if
        end if
     end if
  end do
  if (THIN .and. BART(1) .ge. BABT) then
     SERROR(13) = 1
  end if
  do I = 1,5
     if (.not. THIN .and. (YST(I) .ne.0.or. BART(I) .ne.0)) then
        SERROR(14) = 1
     end if
     if (THIN) then
        if (EVEN .and. YST(I).gt. STAGE) then
           SERROR(15) = 1
        end if
        if (I .gt.1) then
           !                  IF(YST(I) .GT. 0 .AND. BART(I) .LE. 0.0) THEN
           if (YST(I) .ne.0.and. BART(I) .le.0.0) then
              SERROR(16) = 1
           end if
        end if
        if (BABT .le.0.0) then
           SERROR(17) = 1
        end if
     end if
  end do
  !      IF(THIN .AND. YST(1) .EQ. 0.0) THEN
  if (THIN .and. YST(1) .eq. YCYCLG) then
     SERROR(18) = 1
     do I = 1,NTREES
        if (MGEXP(I) .gt.0.0) SERROR(18) = 0
     end do
  end if
  if (CYCLG .lt.0) then
     SERROR(19) = 1
  end if
  do I = 1,3
     do J = 1,18
        if (ACALIB(I,J) .gt.2.0.or. ACALIB(I,J) .lt.0.5) then
           SERROR(20) = 1
        end if
     end do
  end do
  if (MSDI_1.gt.1000.0.or. MSDI_2.gt.1000.0.or. MSDI_3.gt.&
       &1000.0) then
     SERROR(21) = 1
  end if

  !*****START NEW*****
  if (GENETICS) then
     if (.not. EVEN) then
        SERROR(22) = 1
     end if
     if (GWDG .lt.0.0.or. GWHG .lt.0.0) then
        SERROR(23) = 1
     end if
     if (GWDG .gt.20.0.or. GWHG .gt.20.0) then
        SERROR(24) = 1
     end if
  else
     if (GWDG .gt.0.0.or. GWHG .gt.0.0) then
        SERROR(25) = 1
     end if
  end if
  if (SWISSNC) then
     if (VERSION .eq.1.or. VERSION .eq.4) then
        SERROR(26) = 1
     end if
     if (.not. EVEN) then
        SERROR(27) = 1
     end if
     if (FR .lt.0.85) then
        SERROR(28) = 1
     end if
     if (FR .gt.7.0) then
        SERROR(29) = 1
     end if
     if (FERT .and. FR .lt.3.0) then
        SERROR(30) = 1
     end if
  else
     if (FR .gt.0.0) then
        SERROR(31) = 1
     end if
  end if
  !*****END NEW*****

  if (VERSION .ge.4.and. SITE_1.le.0.0) then
     SERROR(32) = 1
  end if
  if (VERSION .ge.4.and. PDEN .le.0.0) then
     SERROR(33) = 1
  end if
  if (.not. EVEN .and. VERSION .ge.4) then
     SERROR(34) = 1
  end if
  do I = 1,34
     if (SERROR(I) .eq.1) then
        ERROR = .true.
        return
     end if
  end do
  select case(VERSION)
  case(1)
     IB = 5
     NSPN = 18
  case(2,3)
     IB = 3
     NSPN = 11
  case(4)
     IB = 3
     NSPN = 7
  end select

  !     EDIT TREE RECORDS FOR ERRORS

  do I = 1,NTREES
     call CKSP_RUN(VERSION,I,SPECIES,TERROR)
     if (DBH(I) .le.0.09) then
        TERROR(I,2) = 1
     end if
     if (HT(I) .le.4.5) then
        TERROR(I,3) = 1
     end if
     if (CR(I) .le.0.0.or. CR(I) .gt.1.0) then
        TERROR(I,4) = 1
     end if
     if (EXPAN(I) .lt.0.0) then
        TERROR(I,5) = 1
     end if
     if (SCR(I) .lt.0.0.or. SCR(I) .gt.1.0) then
        TERROR(I,6) = 1
     end if
  end do
  do I = 1,NTREES
     do J = 1,6
        if (TERROR(I,J) .eq.1) then
           ERROR = .true.
           return
        end if
     end do
  end do
  do I = 1,NTREES
     select case(VERSION)
     case(1) !  SWO  BIG SIX
        if (SPECIES(I) .eq.122.and. HT(I) .gt. MAXPP) then
           MAXPP = HT(I)
        else if (SPECIES(I) .eq.81.and. HT(I) .gt. MAXIC) then
           MAXIC = HT(I)
        else if (SPECIES(I) .eq.202.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        else if (SPECIES(I) .eq.15.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        else if (SPECIES(I) .eq.17.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        else if (SPECIES(I) .eq.117.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        end if
     case(2,3)
        if (SPECIES(I) .eq.17.and. HT(I) .gt. MAXGF) then
           MAXGF = HT(I)
        else if (SPECIES(I).eq.202.and. HT(I) .gt. MAXDF) then
           MAXDF = HT(I)
        else if (SPECIES(I) .eq.263.and. HT(I) .gt. MAXWH) then
           MAXWH = HT(I)
        end if
     case(4)
        if (SPECIES(I) .eq.351.and. HT(I) .gt. MAXRA) then
           MAXRA = HT(I)
        end if
     end select
     call SPGROUP_RUN(VERSION,I,SPECIES,SPGRP)
     IIB = IB
     if (VERSION .ge.4) IIB = 1
     if (SPGRP(I).le.IIB) then
        BIG6 = BIG6+1
        if (EXPAN(I) .le.0.0) then
           BNXT = BNXT+1
        end if
     else
        OTHER = OTHER+1
        if (EXPAN(I) .le.0.0) then
           ONXT = ONXT+1
        end if
     end if
  end do

  !     DETERMINE IF SPECIES MIX CORRECT FOR STAND AGE

  SBA = 0.0
  B6SBA = 0.0
  HWSBA = 0.0
  do I = 1,NTREES
     if (EXPAN(I) .le.0.0) cycle
     BA = DBH(I)**2*EXPAN(I)
     SBA = SBA+BA
     if (SPGRP(I) .le. IIB)B6SBA = B6SBA+BA
     if (VERSION .eq.1) then
        if (SPECIES(I) .eq.361.or. SPECIES(I) .eq.431.or.&
             &SPECIES(I) .eq.818) then
           HWSBA = HWSBA+BA
        end if
     end if
  end do
  SBA = SBA*.005454154/FLOAT(NPTS)
  B6SBA = B6SBA*.005454154/FLOAT(NPTS)
  if (B6SBA .le.0) then
     SERROR(5) = 1
     ERROR = .true.
     return
  end if
  if (VERSION .ge.4) then
     if (SBA .gt.0.0) then
        PRA = B6SBA/SBA
     else
        PRA = 0.0
     end if
     if (PRA .lt.0.9) then
        SERROR(35) = 1
        ERROR = .true.
        return
     end if
  end if

  !     DETERMINE WARNINGS (IF ANY)

  select case (VERSION)
  case(1)
     if ((SITE_1.gt.0.0) .and. (SITE_1.lt.40.0.or. SITE_1&
          &.gt.150.0)) then
        SWARNING(1) = 1
     end if
     if ((SITE_2.gt.0.0) .and. (SITE_2.lt.50.0.or. SITE_2&
          &.gt.140.0)) then
        SWARNING(2) = 1
     end if
  case(2,3)
     if ((SITE_1.gt.0.0) .and. (SITE_1.lt.90.0.or. SITE_1&
          &.gt.142.0)) then
        SWARNING(1) = 1
     end if
     if ((SITE_2.gt.0.0) .and. (SITE_2.lt.90.0.or. SITE_2&
          &.gt.142.0)) then
        SWARNING(2) = 1
     end if
  case(4)
     if (SITE_1.lt.20.0.or. SITE_1.gt.125.0) then
        SWARNING(1) = 1
     end if
     if ((SITE_2.gt.0.0) .and. (SITE_2.lt.90.0.or. SITE_2&
          &.gt.142.0)) then
        SWARNING(2) = 1
     end if
  end select
  select case(VERSION)
  case(1)
     if (MAXPP .gt.0.0) then
        MAXHT = (SITE_2-4.5)*(1.0/(1.-EXP(-0.164985*(SITE_2-4.5)&
             &**0.288169)))+4.5
        if (MAXPP .gt. MAXHT) SWARNING(3) = 1
     end if
     if (MAXIC .gt.0.0) then
        ICSI = (0.66*SITE_1)-4.5
        MAXHT = ICSI*(1.0/(1.-EXP(-0.174929*ICSI**0.281176)))+4.5
        if (MAXIC .gt. MAXHT) SWARNING(3) = 1
     end if
     if (MAXDF .gt.0.0) then
        MAXHT = (SITE_1-4.5)*(1.0/(1.0 -&
             &EXP(-0.174929*(SITE_1-4.5)**0.281176)))+4.5
        if (MAXDF .gt. MAXHT) SWARNING(3) = 1
     end if
  case(2,3)
     if (MAXDF .gt.0.0) then
        Z50 = 2500.0/(SITE_1-4.5)
        MAXHT = 4.5+1.0/(-0.000733819+0.000197693*Z50)
        if (MAXDF .gt. MAXHT) SWARNING(3) = 1
     end if
     if (MAXGF .gt.0.0) then
        Z50 = 2500.0/(SITE_1-4.5)
        MAXHT = 4.5+1.0/(-0.000733819+0.000197693*Z50)
        if (MAXGF .gt. MAXHT) SWARNING(3) = 1
     end if
     if (MAXWH .gt.0.0) then
        Z50 = 2500.0/(SITE_2-4.5)
        MAXHT = 4.5+1.0/(0.00192+0.00007*Z50)
        if (MAXWH .gt. MAXHT) SWARNING(3) = 1
     end if
  case(4)
     if (MAXRA .gt.0.0) then
        call WHHLB_H40_RUN(SITE_1,20.0,150.0,MAXHT)
        if (MAXRA .gt. MAXHT) SWARNING(3) = 1
     end if
  end select
  if (EVEN .and. VERSION .le.3.and. BHAGE .lt.10) then
     SWARNING(4) = 1
  end if
  if ((VERSION .eq.1.and. (B6SBA + HWSBA) .lt. SBA*.2).or.&
       &(VERSION .eq.2.and. (B6SBA + HWSBA) .lt. SBA*.5).or.&
       &(VERSION .eq.3.and. (B6SBA + HWSBA) .lt. SBA*.5).or.&
       &(VERSION .eq.4.and. (B6SBA + HWSBA) .lt. SBA*.8)) then
     SWARNING(5) = 1
  end if
  if (NTREES .lt.50) then
     SWARNING(6) = 1
  end if
  call CKAGE(VERSION,NTREES,IB,SPGRP,PDEN,SITE_1,SITE_2,HT,OLD)
  X = 100.0*(OLD/(BIG6-BNXT))
  if (X .ge.50.0) SWARNING(7) = 1
  if (VERSION .eq.1) then
     if (EVEN .and. BHAGE .gt.500.) SWARNING(8) = 1
  else if (VERSION .eq.2.or. VERSION .eq.3) then
     if (EVEN .and. BHAGE .gt.120.) SWARNING(8) = 1
  else
     if (EVEN .and. STAGE .gt.30.) SWARNING(8) = 1
  end if
  if (EVEN) then
     select case(VERSION)
     case(1)
        EXCAGE = 500-STAGE-5
     case(2,3)
        EXCAGE = 120-STAGE-5
     case(4)
        EXCAGE = 30-STAGE-1
     end select
  else
     select case(VERSION)
     case(1)
        EXCAGE = 500-(CYCLG+1)*5
     case(2,3)
        EXCAGE = 120-(CYCLG+1)*5
     case(4)
        EXCAGE = 30-(CYCLG+1)*1
     end select
  end if
  if (EXCAGE .lt.0) then
     SWARNING(9) = 1
  end if
  B1 = -0.04484724
  do I = 1,NTREES
     select case(SPECIES(I))
     case(202)
        B0 = 19.04942539
     case(263)
        if (VERSION .eq.2.or. VERSION .eq.3)B0 = 19.04942539
        if (VERSION .eq.4)B0 = 19.04942539
     case(17,15)
        B0 = 16.26279948
     case(122)
        B0 = 17.11482201
     case(117)
        B0 = 14.29011403
     case default
        B0 = 15.80319194
     end select
     PHT = 4.5+B0*DBH(I)/(1.0-B1*DBH(I))
     if (HT(I) .gt. PHT) then
        TWARNING(I) = 1
     end if
  end do
  return
end

subroutine SPGROUP_RUN(VERSION,I,SPECIES,SPGRP)
  !     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST

  !     I = TREE INDICATOR

  implicit none
  ! Variables
  integer*4 :: VERSION,I,SPECIES(2000),SPGRP(2000),ISX,J

  integer*4 :: SCODE1(19) = (/            202,15,17,122,117,81,263,242,231,361,431,631,805,312,815,&
       &818,351,492,920 /)

  integer*4 :: SCODE2(11) = (/            202,17,263,242,231,361,312,815,351,492,920 /)

  integer*4 :: SCODE3(7) = (/            351,202,263,242,312,492,920 /)
  ISX = -9999
  select case (VERSION)
  case(1)
     do J = 1, 19
        if (SPECIES(I).eq. SCODE1(J)) then
           ISX = J
           if (ISX .gt.2) ISX = ISX-1
           exit
        end if
     end do
  case(2,3)
     do J = 1, 11
        if (SPECIES(I).eq. SCODE2(J)) then
           ISX = J
           exit
        end if
     end do
  case(4)
     do J = 1, 7
        if (SPECIES(I).eq. SCODE3(J)) then
           ISX = J
           exit
        end if
     end do
  end select
  SPGRP(I) = ISX
  return
end

subroutine CKSP_RUN(VERSION,I,SPECIES,TERROR)
  !     DETERMINE SPECIES GROUP FOR EACH TREE IN TREE LIST

  !     I = TREE INDICATOR

  implicit none
  ! Variables
  integer*4 :: VERSION,I,SPECIES(2000),J,TERROR(2000,6)
  logical*2 :: BAD

  integer*4 :: SCODE1(19) = (/            202,15,17,122,117,81,263,242,231,361,431,631,805,312,815,&
       &818,351,492,920 /)

  integer*4 :: SCODE2(11) = (/            202,17,263,242,231,361,312,815,351,492,920 /)

  integer*4 :: SCODE3(7) = (/            351,202,263,242,312,492,920 /)
  BAD = .true.
  select case (VERSION)
  case(1)
     do J = 1, 19
        if (SPECIES(I).eq. SCODE1(J)) then
           BAD = .false.
           exit
        end if
     end do
  case(2,3)
     do J = 1, 11
        if (SPECIES(I).eq. SCODE2(J)) then
           BAD = .false.
        end if
     end do
  case(4)
     do J = 1, 7
        if (SPECIES(I).eq. SCODE3(J)) then
           BAD = .false.
        end if
     end do
  end select
  if (BAD) then
     TERROR(I,1) = 1
  end if
  return
end

subroutine CKAGE(VERSION,NTREES,IB,SPGRP,PDEN,SITE_1,SITE_2,HT,&
     &OLD)
  implicit none
  ! Variables
  integer*4 :: VERSION,NTREES,IB,SPGRP(2000),K,ISISP
  real*4  :: PDEN,SITE,SITE_1,SITE_2,HT(2000),OLD,GEAGE,PHTGRO,IDXAGE,SI_UC,GP
  OLD = 0.0
  do K = 1,NTREES
     if (HT(K) .le.4.5) cycle
     select case(VERSION)
     case(1)

        !        GROWTH EFFECTIVE AGE FROM HANN AND SCRIVANI'S (1987) DOMINANT
        !        HEIGHT GROWTH EQUATION

        if (SPGRP(K) .eq.3) then
           SITE = SITE_2-4.5
           ISISP = 2
        else
           SITE = SITE_1-4.5
           if (SPGRP(K) .eq.5) SITE = SITE_1*0.66-4.5
           ISISP = 1
        end if
        call HS_HG(ISISP,SITE,HT(K),GEAGE,PHTGRO)
        IDXAGE = 500.0
     case(2)
        GP = 5.0
        if (SPGRP(K) .eq.3) then

           !           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
           !           DOMINANT HEIGHT GROWTH EQUATION

           SITE = SITE_2
           call F_HG(SITE,HT(K),GP,GEAGE,PHTGRO)
        else

           !        GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
           !        GROWTH EQUATION FOR DOUGLAS-FIR AND GRAND FIR

           SITE = SITE_1
           call B_HG(SITE,HT(K),GP,GEAGE,PHTGRO)
        end if
        IDXAGE = 120.0
     case(3)
        GP = 5.0
        if (SPGRP(K) .eq.3) then

           !           GROWTH EFFECTIVE AGE FROM FLEWELLING'S WESTERN HEMLOCK
           !           DOMINANT HEIGHT GROWTH EQUATION

           SITE = SITE_2
           call F_HG(SITE,HT(K),GP,GEAGE,PHTGRO)
        else

           !           GROWTH EFFECTIVE AGE FROM BRUCE'S (1981) DOMINANT HEIGHT
           !           GROWTH EQUATION FOR DOUGLAS-FIR AND GRAND FIR

           SITE = SITE_1
           call B_HG(SITE,HT(K),GP,GEAGE,PHTGRO)
        end if
        IDXAGE = 120.0
     case(4)
        GP = 1.0
        if (SPGRP(K) .eq.1) then

           !           GROWTH EFFECTIVE AGE FROM WEISKITTEL ET AL.'S (2009) RED ALDER
           !           DOMINANT HEIGHT GROWTH EQUATION

           SITE = SITE_1
           call WHHLB_SI_UC_RUN(SITE,PDEN,SI_UC)
           call WHHLB_GEA_RUN(HT(K),SI_UC,GEAGE)
        end if
        IDXAGE = 30.0
     end select
     if (SPGRP(K).le.IB.and.GEAGE.gt.IDXAGE) then
        OLD = OLD+1.0
     end if
  end do
  return
end

subroutine GET_ORGRUN_EDITION(EDITION)
  implicit none
  ! Variables
  real*4  :: EDITION
  EDITION = 9.1
  return
end

subroutine CROWN_CLOSURE(VERSION,NTREES,NPTS,SPECIES,DBH,HT,&
     &CR,SCR,EXPAN,CC)
  implicit none
  ! Variables
  integer*4 :: VERSION,NTREES,NPTS,SPECIES(2000),L,I,ISPGRP(2000)
  real*4  :: DBH(2000),HT(2000),CR(2000),SCR(2000),EXPAN(2000),CCH(41),CC,CL,HCB,MCW,LCW,HLCW,EXPFAC

  do I = 1,NTREES
     call SPGROUP_RUN(VERSION,I,SPECIES,ISPGRP)
  end do
  do L = 1,40
     CCH(L) = 0.
  end do
  CCH(41) = HT(1)
  do I = 2,NTREES
     if (HT(I).gt.CCH(41)) CCH(41) = HT(I)
  end do
  do I = 1,NTREES
     CL = CR(I)*HT(I)
     HCB = HT(I)-CL
     EXPFAC = EXPAN(I)/FLOAT(NPTS)
     select case(VERSION)
     case(1)
        call MCW_SWO(ISPGRP(I),DBH(I),HT(I),MCW)
        call LCW_SWO(ISPGRP(I),MCW,CR(I),SCR(I),DBH(I),HT(I),LCW)
        call HLCW_SWO(ISPGRP(I),HT(I),CR(I),SCR(I),HLCW)
        call CALC_CC(VERSION,ISPGRP(I),HLCW,LCW,HT(I),DBH(I),HCB,&
             &EXPFAC,CCH)
     case(2)
        call MCW_NWO(ISPGRP(I),DBH(I),HT(I),MCW)
        call LCW_NWO(ISPGRP(I),MCW,CR(I),SCR(I),DBH(I),HT(I),LCW)
        call HLCW_NWO(ISPGRP(I),HT(I),CR(I),SCR(I),HLCW)
        call CALC_CC(VERSION,ISPGRP(I),HLCW,LCW,HT(I),DBH(I),HCB,&
             &EXPFAC,CCH)
     case(3)
        call MCW_SMC(ISPGRP(I),DBH(I),HT(I),MCW)
        call LCW_SMC(ISPGRP(I),MCW,CR(I),SCR(I),DBH(I),HT(I),LCW)
        call HLCW_SMC(ISPGRP(I),HT(I),CR(I),SCR(I),HLCW)
        call CALC_CC(VERSION,ISPGRP(I),HLCW,LCW,HT(I),DBH(I),HCB,&
             &EXPFAC,CCH)
     case(4)
        call MCW_RAP(ISPGRP(I),DBH(I),HT(I),MCW)
        call LCW_RAP(ISPGRP(I),MCW,CR(I),SCR(I),DBH(I),HT(I),LCW)
        call HLCW_RAP(ISPGRP(I),HT(I),CR(I),SCR(I),HLCW)
        call CALC_CC(VERSION,ISPGRP(I),HLCW,LCW,HT(I),DBH(I),HCB,&
             &EXPFAC,CCH)
     end select
  end do
  CC = CCH(1)
  return
end

subroutine INGRO_FILL(VERSION,NTREES,NINGRO,SPECIES,SITE_1,&
     &SITE_2,ACALIB,DBH,HT,CR,EXPAN)
  !     ROUTINE TO CALCULATE MISSING CROWN RATIOS

  !     NINGRO = NUMBER OF TREES ADDED

  implicit none
  ! Variables
  integer*4 :: VERSION,NTREES,NINGRO,SPECIES(2000)
  integer*4 :: IB,SPGRP(2000),TDATAI(2000,3),I
  real*4  :: SITE_1,SITE_2,ACALIB(3,18),DBH(2000),HT(2000),CR(2000),EXPAN(2000)
  real*4  :: SI_1,SI_2,TDATAR(2000,8),GROWTH(2000,4),DEADEXP(2000),RHT,SBA,TPA,SCCF,BAL(500),BALL(51),&
       &CCFL(500),CCFLL(51),OG,SCCFL,HCB
  if (VERSION .eq.1) then
     if (SITE_1.le.0.0.and. SITE_2.gt.0.0) then
        SITE_1 = 1.062934*SITE_2
     else if (SITE_2.le.0.0) then
        SITE_2 = 0.940792*SITE_1
     end if
  else if (VERSION .eq.2.or. VERSION .eq.3) then
     if (SITE_1.le.0.0.and. SITE_2.gt.0.0) then
        SITE_1 = 0.480 + (1.110 * SITE_2)
     else if (SITE_2.le.0.0) then
        SITE_2 = -0.432 + (0.899 * SITE_1)
     end if
  else
     if (SITE_2.le.0.0) then
        SITE_2 = 4.776377*SITE_1**0.763530587
     end if
  end if
  select case(VERSION)
  case(1)
     IB = 5
  case(2,3)
     IB = 3
  case(4)
     IB = 3
  end select
  SI_1 = SITE_1-4.5
  SI_2 = SITE_2-4.5
  do I = NTREES-NINGRO,NTREES
     if (HT(I) .ne.0.0) cycle
     call SPGROUP_RUN(VERSION,I,SPECIES,SPGRP)

     !        CALCULATE HCB

     select case(VERSION)
     case(1)
        call HD_SWO(SPGRP(I),DBH(I),RHT)
     case(2)
        call HD_NWO(SPGRP(I),DBH(I),RHT)
     case(3)
        call HD_SMC(SPGRP(I),DBH(I),RHT)
     case(4)
        call HD_RAP(SPGRP(I),DBH(I),RHT)
     end select
     HT(I) = 4.5+ACALIB(1,SPGRP(I))*(RHT-4.5)
  end do
  do I = 1,2000
     GROWTH(I,3) = 0.0
     GROWTH(I,4) = 0.0
     TDATAI(I,1) = 0
     TDATAI(I,2) = 0
     TDATAI(I,3) = 0
     TDATAR(I,1) = 0.0
     TDATAR(I,2) = 0.0
     TDATAR(I,3) = 0.0
     TDATAR(I,4) = 0.0
     DEADEXP(I) = 0.0
  end do
  do I = 1,NTREES
     TDATAI(I,1) = SPECIES(I)
     TDATAI(I,2) = SPGRP(I)
     TDATAR(I,1) = DBH(I)
     TDATAR(I,2) = HT(I)
     TDATAR(I,3) = CR(I)
     TDATAR(I,4) = EXPAN(I)
  end do
  call OLDGRO(NTREES,IB,TDATAI,TDATAR,GROWTH,DEADEXP,0.0,OG)
  call SSTATS(VERSION,NTREES,TDATAI,TDATAR,SBA,TPA,SCCF,BAL,&
       &BALL,CCFL,CCFLL)
  do I = NTREES-NINGRO,NTREES
     if (CR(I) .ne.0.0) cycle

     !        CALCULATE HCB

     call GET_CCFL(DBH(I),CCFLL,CCFL,SCCFL)
     select case(VERSION)
     case(1)
        call HCB_SWO(SPGRP(I),HT(I),DBH(I),SCCFL,SBA,SI_1,SI_2,&
             &OG,HCB)
     case(2)
        call HCB_NWO(SPGRP(I),HT(I),DBH(I),SCCFL,SBA,SI_1,SI_2,&
             &OG,HCB)
     case(3)
        call HCB_SMC(SPGRP(I),HT(I),DBH(I),SCCFL,SBA,SI_1,SI_2,&
             &OG,HCB)
     case(4)
        call HCB_RAP(SPGRP(I),HT(I),DBH(I),SCCFL,SBA,SI_1,SI_2,&
             &OG,HCB)
     end select
     if (HCB .lt.0.) HCB = 0.0
     if (HCB .gt.0.95*HT(I)) HCB = 0.95*HT(I)
     CR(I) = (1.-(HCB/HT(I)))*ACALIB(2,SPGRP(I))
  end do
  return
end

  !      SUBROUTINE NCH40(H40,AGE,SI)
  !C
  !C     NIGH AND COURTIN (1998) RED ALDER
  !C
  !      IMPLICIT NONE
  !      REAL*4 SI,AGE,H40,MSI,MH40
  !      MSI=SI/3.28
  !      MH40=1.3+((1.693*(MSI-1.3))/(1.0+EXP(3.600-1.240*ALOG(AGE-0.5))))
  !      H40=3.28*MH40
  !      RETURN
  !      END
