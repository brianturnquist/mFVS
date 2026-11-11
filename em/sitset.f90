!=====================================!
!   Initialize species site indices   !
!=====================================!
! EM $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES THAT WAS NOT ASSIGNED BY KEYWORD, AND LOADS THE SDIDEF
  ! ARRAY FOR EACH SPECIES WHICH WAS NOT ASSIGNED AN SDI MAXIMUM
  ! BY KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'EMCOM.f90'
  include 'VOLSTD.f90'

  ! Variables
  logical :: DEBUG
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG,JJ,K
  real    :: SDICON(9)
  real    :: BAMAXA(30)
  integer :: MAPSDI(122),I,ISDI,INTFOR,IREGN,ISPC,J
  integer :: MAPSIT(30,11),MAPSS(30)

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  data BAMAXA/&
       &140.,220.,250.,310.,240.,270.,310.,310.,200.,310.,&
       &290.,330.,380.,440.,500.,500.,390.,390.,440.,180.,&
       &290.,400.,350.,390.,260.,300.,220.,220.,160.,300./

  data MAPSDI/9*2,13*5,5*1,4,3*3,4*5,4*4,3,1,3*3,2,1,10*2,&
       &2*7,3,2*7,2*9,2*4,15*7,4*4,2*8,3*5,2*7,5,3*6,7,8,14*9,3*7,&
       &4,6,9,5*5/

  data SDICON /467.,634.,696.,768.,775.,751.,707.,661.,635./

  data MAPSIT/&
       &25,29,35,35,32,34,35,32,27,39,37,32,36,41,41,41,& !LM,LL
       &43,38,39,32,36,34,34,32,36,36,36,26,22,26,&
       &6, 8,10,10, 9,10,10, 9, 7,11, 9,11,12,12,12,12,& !RM
       &13,11,11, 9,10,10,10, 9,10,10,10, 7, 6, 7,&
       &36,43,51,50,44,49,48,46,41,55,46,52,59,58,58,58,& !AS
       &60,54,55,46,49,52,52,46,52,52,52,38,33,38,&
       &44,57,76,62,72,72,71,67,55,86,67,80,95,93,93,93,& !GA,CW,BA,PW,NC,OH
       &98,84,86,67,73,80,80,67,80,80,80,49,36,49,&
       &36,43,51,50,44,49,48,46,41,55,46,52,59,58,58,58,& !PB
       &60,54,55,46,49,52,52,46,52,52,52,38,33,38,&
       &35,51,57,60,48,55,45,38,44,60,64,47,47,47,47,47,& !WB,WL,OS
       &47,47,47,53,50,57,49,57,49,49,47,41,26,60,&
       &46,68,76,80,64,73,67,67,59,80,85,62,62,62,62,62,& !PP
       &62,62,62,70,67,76,65,76,65,65,62,55,35,80,&
       &26,44,49,51,41,47,39,33,38,51,42,40,40,40,40,40,& !DF
       &40,40,40,46,43,49,42,42,42,42,30,36,23,51,&
       &46,68,76,80,75,73,72,70,63,80,80,62,62,62,62,62,& !LP
       &62,62,62,74,76,76,68,68,68,65,35,51,35,80,&
       &46,68,76,78,64,73,61,51,59,80,85,68,68,68,68,68,& !ES
       &68,68,68,75,68,78,80,80,80,67,64,53,45,78,&
       &46,68,76,80,64,73,61,51,59,80,85,62,62,62,62,62,& !AF
       &62,62,62,70,67,76,76,76,76,65,62,55,35,80/

  data MAPSS/2*10,7*3,2*8,2*3,3*3,3,5*9,3,9,3,3*9,3,3/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)

  ! IF SITEAR() HAS NOT BEEN SET WITH SITECODE THEN CALCULATE VALUE HERE.
  ! SITE SPECIES DEFAULT WAS ALWAYS DOUGLAS-FIR (3) IN OLD EM. SETTING IT
  ! BY MAPPED HABITAT TYPE SEEMED LIKE A BETTER IDEA. INDEX ITYPE IS THE
  ! CORRECT INDEX POINTING TO ONE OF THE ORIGINAL 30 NI HABITAT TYPES.
  ! GD 2-9-09.

  ! ADDED MAPPING OF ALL SPECIES SITE INDICES BASED ON AVERAGE SITE INDEX
  ! FOUND IN PFISTER 1977 ADJUSTED FOR BASE AGE DIFFERENCES USING EQUATION
  ! BELOW
  !   SPECIES   BASE YR   REF
  !     WB       100      ALEXANDER TACKLE AND DAHMS REDUCED TO .75
  !     DF        50      MONSERUD
  !     LP       100      ALEXANDER TACKLE AND DAHMS
  !     AF       100      ALEXANDER
  !     ES       100      ALEXANDER
  !     PP       100      MEYER  BULL 630

  ! PFISTER 1977 BASE AGE CONVERSION EQUATION
  ! 50YR BASE = 1.0096 + .6279*100YR BASE

  if (ISISP.eq.0)ISISP = MAPSS(ITYPE)
  do I = 1,MAXSP
     if (SITEAR(I) .eq.0.0) then !IF SPECIES SI NOT SET
        if (I .eq.4.or. I.eq.5) then
           SITEAR(I) = MAPSIT(ITYPE,1)
        else if (I .eq.6) then
           SITEAR(I) = MAPSIT(ITYPE,2)
        else if (I .eq.12) then
           SITEAR(I) = MAPSIT(ITYPE,3)
        else if (I.eq.11.or. (I.ge.13.and. I.le.16) .or.&
             &I.eq.19) then
           SITEAR(I) = MAPSIT(ITYPE,4)
        else if (I .eq.17) then
           SITEAR(I) = MAPSIT(ITYPE,5)
        else if (I .eq.1.or. I .eq.2.or. I .eq.18) then
           SITEAR(I) = MAPSIT(ITYPE,6)
        else if (I .eq.10) then
           SITEAR(I) = MAPSIT(ITYPE,7)
        else if (I .eq.3) then
           SITEAR(I) = MAPSIT(ITYPE,8)
        else if (I .eq.7) then
           SITEAR(I) = MAPSIT(ITYPE,9)
        else if (I .eq.8) then
           SITEAR(I) = MAPSIT(ITYPE,10)
        else if (I .eq.9) then
           SITEAR(I) = MAPSIT(ITYPE,11)
        else
           SITEAR(I) = 70.
        end if
     end if
     30 continue
  end do

  ! LOAD BAMAX AND THE SDIDEF ARRAY

  ISDI = 5
  if (IEMTYP .gt.0) ISDI = MAPSDI(IEMTYP)
  do I = 1,MAXSP
     if (SDIDEF(I) .gt.0.0) go to 40
     if (BAMAX .gt.0.) then
        SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
     else
        SDIDEF(I) = SDICON(ISDI)
     end if
     40 continue
  end do
  if (BAMAX.le.0.) BAMAX = BAMAXA(ITYPE)

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

  !  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
  !  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
  !  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.

  do ISPC = 1,MAXSP
     if (METHB(ISPC).eq.999)METHB(ISPC) = 6
     if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     50 continue
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'EM'

  if (LFIANVB) call NVB_REGION_CHECK
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     VOLEQ = '           '
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
     else if (METHC(ISPC).eq.10) then
        call NVBEQDEF(IFIASP, VOLEQ)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (VEQNNC(ISPC)(11:11) .eq. CHAR(0)) VEQNNC(ISPC)(11:11) = " "
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
