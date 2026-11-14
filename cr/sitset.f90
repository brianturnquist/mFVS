!=====================================!
!   Initialize species site indices   !
!=====================================!
! CR $Id$
subroutine SITSET
  implicit none

  !  THIS SUBROUTINE IS USED TO SET ALL VARIANT SPECIFIC VARIABLES,
  !  AND VARIABLES DEPENDENT ON MODEL TYPE.

  !  IT IS ALSO USED TO SET SITE INDEX VALUES FOR SPECIES
  !  THAT HAVE NOT BEEN SET USING THE SITECODE KEYWORD, AND DEFAULT SDI
  !  MAXIMUM VALUES FOR SPECIES THAT HAVE NOT BEEN SET USING THE SDIDEF
  !  KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'ESCOMN.f90'
  include 'GGCOM.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'

  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG,I,J,JJ,K,INTFOR,IREGN,ISPC
  real    :: SDICON(MAXSP),TEM
  integer :: DEFMT(23)

  !  SPECIES ORDER:
  !   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
  !  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
  !  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
  !  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH

  !  SPECIES EXPANSION:
  !  UJ,AJ,RM,OJ,ER USE CR JU
  !  NC,PW USE CR CO
  !  GO,AW,EM,BK,SO USE CR OA
  !  PB USES CR AS
  !  PM,PD,AZ USE CR PI
  !  CI USES CR PP

  data SDICON/&
       &602., 423., 570., 562., 634., 687., 762., 423., 621., 409.,&
       &679., 348., 446., 621., 529., 497., 620., 620., 412., 562.,&
       &452., 452., 652., 403., 284., 423., 284., 466., 395., 411.,&
       &408., 354., 358., 348., 358., 446., 348., 452./

  data DEFMT/ 5, 3, 4, 5, 3, 4, 5, 5, 4, 4, 5, 4,&
       &2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/

  character(len=26) :: LABEL

  !  FINISH PROCESSING THE MODTYPE KEYWORD

  write(JOSTND,10)
  10 format(/'MODTYPE    KEYWORD NOW BEING PROCESSED')

  !  DECODE MODEL TYPE

  if (IMODTY .eq.1) LABEL = 'SOUTHWEST MIXED CONIFERS  '
  if (IMODTY .eq.2) LABEL = 'SOUTHWEST PONDEROSA PINE  '
  if (IMODTY .eq.3) LABEL = 'BLACK HILLS PONDEROSA PINE'
  if (IMODTY .eq.4) LABEL = 'SPRUCE-FIR                '
  if (IMODTY .eq.5) LABEL = 'LODGEPOLE PINE            '
  if (IMODTY.gt.0.and. IMODTY.le.5) then
     write (JOSTND,200) IMODTY,LABEL
     200 format(T12,'MODEL TYPE FOR THIS PROJECTION IS ',I4,'. ',A26)
  else
     IMODTY = DEFMT(IFOR)
     if (IMODTY .eq.1) LABEL = 'SOUTHWEST MIXED CONIFERS  '
     if (IMODTY .eq.2) LABEL = 'SOUTHWEST PONDEROSA PINE  '
     if (IMODTY .eq.3) LABEL = 'BLACK HILLS PONDEROSA PINE'
     if (IMODTY .eq.4) LABEL = 'SPRUCE-FIR                '
     if (IMODTY .eq.5) LABEL = 'LODGEPOLE PINE            '
     write(JOSTND,14)IMODTY,LABEL
     14 format(T12,'MODEL TYPE NOT RECOGNIZED. BEING SET TO',I4,&
          &'= ',A26,'  FOR FURTHER PROCESSING')
  end if

  ! THE FOLLOWING APPLY TO ONE OR MORE MODEL TYPES, AND MAY BE
  ! CHANGED IN SPECIFIC MODEL TYPE SECTIONS BELOW.

  !  FOR THIS VARIANT, SIGMAR WAS SET TO 0.2 JUST TO GET A LITTLE
  !  VARIATION IN THE SYSTEM. CARL'S SE VALUES LOOKED REALLY SUSPECT.
  !  EQNS FROM OTHER VARIANTS HAVE SIGMAR VALUES

  do I = 1,MAXSP
     SIGMAR(I) = 0.2
     50 continue
  end do
  SIGMAR( 4) = 0.260
  SIGMAR( 6) = 0.3433
  SIGMAR( 7) = 0.4125
  SIGMAR( 8) = 0.4243
  SIGMAR(10) = 0.4671
  SIGMAR(14) = 0.11645

  BARK1( 1) = 0.890
  BARK1( 2) = 0.890
  BARK1( 3) = 0.867
  BARK1( 4) = 0.890
  BARK1( 5) = 0.890
  BARK1( 6) = 0.9497
  BARK1( 7) = 0.9497
  BARK1( 8) = 0.87407
  BARK1( 9) = 0.9625
  BARK1(10) = 0.9625
  BARK1(11) = 0.9625
  BARK1(12) = 0.8967
  BARK1(13) = 0.8967
  BARK1(14) = 0.9625
  BARK1(15) = 0.9643
  BARK1(16) = 0.8967
  BARK1(17) = 0.9502
  BARK1(18) = 0.9502
  BARK1(19) = 0.9502
  BARK1(20) = 0.950
  BARK1(21) = 0.892
  BARK1(22) = 0.892
  BARK1(23) = 0.93789
  BARK1(24) = 0.93789
  BARK1(25) = 0.93789
  BARK1(26) = 0.93789
  BARK1(27) = 0.93789
  BARK1(28) = 0.950
  BARK1(29) = 0.8967
  BARK1(30) = 0.8967
  BARK1(31) = 0.8967
  BARK1(32) = 0.8967
  BARK1(33) = 0.8967
  BARK1(34) = 0.8967
  BARK1(35) = 0.8967
  BARK1(36) = 0.8967
  BARK1(37) = 0.8967
  BARK1(38) = 0.892

  BARK2( 1) = 0.
  BARK2( 2) = 0.
  BARK2( 3) = 0.
  BARK2( 4) = 0.
  BARK2( 5) = 0.
  BARK2( 6) = 0.
  BARK2( 7) = 0.
  BARK2( 8) = -0.185
  BARK2( 9) = -0.1141
  BARK2(10) = -0.1141
  BARK2(11) = -0.1141
  BARK2(12) = -0.4448
  BARK2(13) = -0.4448
  BARK2(14) = -0.1141
  BARK2(15) = 0.
  BARK2(16) = -0.4448
  BARK2(17) = -0.2528
  BARK2(18) = -0.2528
  BARK2(19) = -0.2528
  BARK2(20) = 0.
  BARK2(21) = -0.086
  BARK2(22) = -0.086
  BARK2(23) = -0.24096
  BARK2(24) = -0.24096
  BARK2(25) = -0.24096
  BARK2(26) = -0.24096
  BARK2(27) = -0.24096
  BARK2(28) = 0.
  BARK2(29) = -0.4448
  BARK2(30) = -0.4448
  BARK2(31) = -0.4448
  BARK2(32) = -0.4448
  BARK2(33) = -0.4448
  BARK2(34) = -0.4448
  BARK2(35) = -0.4448
  BARK2(36) = -0.4448
  BARK2(37) = -0.4448
  BARK2(38) = -0.086

  DBHMAX( 1) = 36.
  DBHMAX( 2) = 36.
  DBHMAX( 3) = 50.
  DBHMAX( 4) = 20.
  DBHMAX( 5) = 40.
  DBHMAX( 6) = 20.
  DBHMAX( 7) = 20.
  DBHMAX( 8) = 20.
  DBHMAX( 9) = 36.
  DBHMAX(10) = 36.
  DBHMAX(11) = 20.
  DBHMAX(12) = 30.
  DBHMAX(13) = 50.
  DBHMAX(14) = 20.
  DBHMAX(15) = 36.
  DBHMAX(16) = 50.
  DBHMAX(17) = 40.
  DBHMAX(18) = 46.
  DBHMAX(19) = 20.
  DBHMAX(20) = 30.
  DBHMAX(21) = 36.
  DBHMAX(22) = 36.
  DBHMAX(23) = 20.
  DBHMAX(24) = 20.
  DBHMAX(25) = 20.
  DBHMAX(26) = 20.
  DBHMAX(27) = 20.
  DBHMAX(28) = 30.
  DBHMAX(29) = 50.
  DBHMAX(30) = 50.
  DBHMAX(31) = 50.
  DBHMAX(32) = 50.
  DBHMAX(33) = 30.
  DBHMAX(34) = 30.
  DBHMAX(35) = 30.
  DBHMAX(36) = 50.
  DBHMAX(37) = 20.
  DBHMAX(38) = 20.

  !  DEFAULT MERCH SPECS ARE SET IN NI/VOLS

  ! DEFAULT MODEL TYPE IS 1 --- SOUTHWEST MIXED CONFIERS.  VARIANT
  ! SPECIFIC VARIABLES FOR THIS MODEL TYPE ARE SET IN BLKDAT AND GRINIT.

  ! MODEL TYPE 1 --- SOUTHWEST MIXED CONIFERS          **

  if (IMODTY .eq.1) then

     ! LOAD GRINIT VARIABLES FOR MODEL 2

     if (ELEV .le.0.) ELEV = 88.

  end if

  ! MODEL TYPE 2 -- SOUTHWEST PONDEROSA PINE.           **

  if (IMODTY .eq.2) then

     ! LOAD BLKDAT VARIABLES FOR MODEL 2

     SIGMAR(23) = 0.043
     SIGMAR(24) = 0.043
     SIGMAR(25) = 0.043
     SIGMAR(26) = 0.043
     SIGMAR(27) = 0.043
     SIGMAR(12) = 0.181
     SIGMAR(33) = 0.181
     SIGMAR(34) = 0.181
     SIGMAR(35) = 0.181

     ! LOAD GRINIT VARIABLES FOR MODEL 2

     if (ELEV .le.0.) ELEV = 88.

     ! MODEL TYPE 3 -- BLACK HILLS PONDEROSA PINE     **

  else if (IMODTY .eq.3) then

     ! LOAD BLKDAT VARIABLES FOR MODEL 3

     do I = 1,MAXSP
        SIGMAR(I) = 0.346
        1225 continue
     end do
     SIGMAR(17) = 0.468
     SIGMAR(18) = 0.468
     SIGMAR(19) = 0.468
     BARK1(12) = 0.
     BARK2(12) = 0.
     BARK1(13) = 0.
     BARK2(13) = 0.
     BARK1(16) = 0.
     BARK2(16) = 0.
     BARK1(29) = 0.
     BARK2(29) = 0.
     BARK1(30) = 0.
     BARK2(30) = 0.
     BARK1(31) = 0.
     BARK2(31) = 0.
     BARK1(32) = 0.
     BARK2(32) = 0.
     BARK1(33) = 0.
     BARK2(33) = 0.
     BARK1(34) = 0.
     BARK2(34) = 0.
     BARK1(35) = 0.
     BARK2(35) = 0.
     BARK1(36) = 0.
     BARK2(36) = 0.
     BARK1(37) = 0.
     BARK2(37) = 0.

     ! LOAD GRINIT VARIABLES FOR MODEL 3

     TLAT = 44.0
     if (ELEV .le.0.) ELEV = 55.

     ! LOAD GGCOM VARIABLES FOR MODEL 3

     DBHMAX(1) = 20.
     DBHMAX(2) = 20.
     DBHMAX(3) = 20.
     DBHMAX(5) = 20.
     DBHMAX(9) = 20.
     DBHMAX(10) = 20.
     DBHMAX(11) = 24.
     DBHMAX(12) = 20.
     DBHMAX(13) = 32.
     DBHMAX(14) = 30.
     DBHMAX(15) = 20.
     DBHMAX(16) = 24.
     DBHMAX(17) = 24.
     DBHMAX(18) = 24.
     DBHMAX(19) = 30.
     DBHMAX(20) = 24.
     DBHMAX(21) = 48.
     DBHMAX(22) = 48.
     DBHMAX(28) = 24.
     DBHMAX(29) = 24.
     DBHMAX(30) = 24.
     DBHMAX(31) = 24.
     DBHMAX(32) = 24.
     DBHMAX(33) = 20.
     DBHMAX(34) = 20.
     DBHMAX(35) = 20.
     DBHMAX(36) = 32.

     ! MODEL TYPE 4 -- SPRUCE-FIR                    **

  else if (IMODTY .eq.4) then

     ! LOAD BLKDAT VARIABLES FOR MODEL 4

     BARK1(12) = 0.
     BARK2(12) = 0.
     BARK1(13) = 0.
     BARK2(13) = 0.
     BARK1(16) = 0.
     BARK2(16) = 0.
     BARK1(29) = 0.
     BARK2(29) = 0.
     BARK1(30) = 0.
     BARK2(30) = 0.
     BARK1(31) = 0.
     BARK2(31) = 0.
     BARK1(32) = 0.
     BARK2(32) = 0.
     BARK1(33) = 0.
     BARK2(33) = 0.
     BARK1(34) = 0.
     BARK2(34) = 0.
     BARK1(35) = 0.
     BARK2(35) = 0.
     BARK1(36) = 0.
     BARK2(36) = 0.
     BARK1(37) = 0.
     BARK2(37) = 0.

     ! LOAD GRINIT VARIABLES FOR MODEL 4

     TLAT = 40.0
     if (ELEV .le.0.) ELEV = 90.

     ! LOAD GGCOM VARIABLES FOR MODEL 4

     DBHMAX(1) = 28.
     DBHMAX(2) = 28.
     DBHMAX(3) = 42.
     DBHMAX(4) = 36.
     DBHMAX(5) = 30.
     DBHMAX(7) = 40.
     DBHMAX(8) = 36.
     DBHMAX(9) = 20.
     DBHMAX(11) = 36.
     DBHMAX(12) = 20.
     DBHMAX(13) = 32.
     DBHMAX(14) = 30.
     DBHMAX(17) = 36.
     DBHMAX(19) = 36.
     DBHMAX(21) = 24.
     DBHMAX(22) = 24.
     DBHMAX(33) = 20.
     DBHMAX(34) = 20.
     DBHMAX(35) = 20.
     DBHMAX(36) = 32.

     ! MODEL TYPE 5 -- LODGEPOLE PINE                **

  else if (IMODTY .eq.5) then

     ! LOAD BLKDAT VARIABLES FOR MODEL 5

     BARK1(12) = 0.
     BARK2(12) = 0.
     BARK1(13) = 0.
     BARK2(13) = 0.
     BARK1(16) = 0.
     BARK2(16) = 0.
     BARK1(29) = 0.
     BARK2(29) = 0.
     BARK1(30) = 0.
     BARK2(30) = 0.
     BARK1(31) = 0.
     BARK2(31) = 0.
     BARK1(32) = 0.
     BARK2(32) = 0.
     BARK1(33) = 0.
     BARK2(33) = 0.
     BARK1(34) = 0.
     BARK2(34) = 0.
     BARK1(35) = 0.
     BARK2(35) = 0.
     BARK1(36) = 0.
     BARK2(36) = 0.
     BARK1(37) = 0.
     BARK2(37) = 0.

     ! LOAD GRINIT VARIABLES FOR MODEL 5

     TLAT = 40.
     if (ELEV .le.0.) ELEV = 90.

     ! LOAD GGCOM VARIABLES FOR MODEL 5

     DBHMAX(1) = 28.
     DBHMAX(2) = 28.
     DBHMAX(3) = 42.
     DBHMAX(4) = 36.
     DBHMAX(5) = 30.
     DBHMAX(7) = 40.
     DBHMAX(8) = 36.
     DBHMAX(9) = 20.
     DBHMAX(11) = 36.
     DBHMAX(12) = 20.
     DBHMAX(13) = 32.
     DBHMAX(14) = 30.
     DBHMAX(17) = 36.
     DBHMAX(19) = 36.
     DBHMAX(21) = 24.
     DBHMAX(22) = 24.
     DBHMAX(33) = 20.
     DBHMAX(34) = 20.
     DBHMAX(35) = 20.
     DBHMAX(36) = 32.
  end if

  ! IF SITEAR(I) HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD IT
  ! WITH DEFAULT SITE VALUES.

  TEM = 70.
  if (IMODTY .eq.3) TEM = 57.
  if (IMODTY .eq.4) TEM = 75.
  if (IMODTY .eq.5) TEM = 65.
  if (ISISP .gt.0) then
     if (SITEAR(ISISP) .gt.0.0) TEM = SITEAR(ISISP)
  end if
  if (IMODTY .eq.1.and. ISISP .eq.0) ISISP = 3
  if (IMODTY .eq.2.and. ISISP .eq.0) ISISP = 13
  if (IMODTY .eq.3.and. ISISP .eq.0) ISISP = 13
  if (IMODTY .eq.4.and. ISISP .eq.0) ISISP = 18
  if (IMODTY .eq.5.and. ISISP .eq.0) ISISP = 11
  do I = 1,MAXSP
     if (TEM .lt. SITELO(ISISP))TEM = SITELO(ISISP)
     if (SITEAR(I) .le.0.0) SITEAR(I) = SITELO(I) +&
          &(TEM-SITELO(ISISP))/(SITEHI(ISISP)-SITELO(ISISP))&
          &*(SITEHI(I)-SITELO(I))
     100 continue
  end do

  ! LOAD THE SDIDEF ARRAY

  do I = 1,MAXSP
     if (SDIDEF(I) .gt.0.0) go to 40
     if (BAMAX .gt.0.) then
        SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
     else
        SDIDEF(I) = SDICON(I)
     end if
     40 continue
  end do

  do I = 1,15
     J = (I-1)*10 + 1
     JJ = J+9
     if (JJ.gt.MAXSP)JJ = MAXSP
     write(JOSTND,90)(NSP(K,1)(1:2),K = J,JJ)
     90 format(/'SPECIES ',5X,10(A2,6X))
     write(JOSTND,91)(NINT(SDIDEF(K)),K = J,JJ )
     91 format('SDI MAX ',   10I8.0)
     if (JJ .eq. MAXSP)go to 93
     92 continue
  end do
  93 continue

  !  LOAD VOLUME DEFAULT MERCH. SPECS.

  if (IMODTY.eq.3) then
     do I = 1,MAXSP
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 9.0
        if (TOPD(I) .le.0.) TOPD(I) = 6.0
        if (BFTOPD(I) .le.0.0) BFTOPD(I) = 6.0
        if (BFMIND(I) .le.0.0) BFMIND(I) = 9.0
        if (SCFTOPD(I) .le.0.0) SCFTOPD(I) = 6.0
        if (SCFMIND(I) .le.0.0) SCFMIND(I) = 9.0
     end do
  else if ((IMODTY.eq.1).or.(IMODTY.eq.2).or.&
       &(IMODTY.eq.4).or.(IMODTY.eq.5)) then
     do I = 1,MAXSP
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 5.0
        if (TOPD(I) .le.0.) TOPD(I) = 4.0
        if (BFTOPD(I) .le.0.0) BFTOPD(I) = 6.0
        if (SCFTOPD(I) .le.0.0) SCFTOPD(I) = 6.0
        if (BFMIND(I) .le.0.0) then
           if (IFOR .lt. IGFOR) then
              BFMIND(I) = 7.0
           else
              BFMIND(I) = 9.0
           end if
        end if
        if (SCFMIND(I) .le.0.0) then
           if (IFOR .lt. IGFOR) then
              SCFMIND(I) = 7.0
           else
              SCFMIND(I) = 9.0
           end if
        end if
     end do
  end if

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'CR'

  if (LFIANVB) call NVB_REGION_CHECK
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     VOLEQ = '           '
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
     else if (METHC(ISPC).eq.10) then
        call NVBEQDEF(IFIASP,VOLEQ)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9)).and.&
          &(VEQNNB(ISPC).eq.'           ')) then
        VOLEQ = '           '
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC) = VOLEQ
     end if
  end do

  !  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE

  if (LFIA) then
     call FIAHEAD(JOSTND)
     write(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I = 1,MAXSP)
     211 format ((T12,8(A3,'=',A6,:,'; '),A,'=',A6))
  end if

  !  WRITE VOLUME EQUATION NUMBER TABLE

  call VOLEQHEAD(JOSTND)
  write(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J = 1,MAXSP)
  230 format(4(2X,A2,4X,A11,1X,A10,1X))

  return
end
