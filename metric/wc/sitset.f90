!=====================================!
!   Initialize species site indices   !
!=====================================!
! METRIC-WC $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES, GIVEN A SITE INDEX AND SITE SPECIES, AND LOADS THE SDIDEF
  ! ARRAY WITH SDI MAXIMUMS FOR SPECIES WHICH WERE NOT ASSIGNED A VALUE
  ! USING THE SDIMAX KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IFIASP,ERRFLAG
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  character(len=4) :: ASPEC
  real    :: SIAGE(MAXSP),FMSDI(10)
  real    :: FORMAX,RSI,RSDI,AG,SINDX,SI
  integer :: NSISET,NSDSET,I,JSISP,INDEX,KNTECO,NTOHI,ISEQ,NUM,ISFLAG
  integer :: ITOHI,ISPC,J,JJ,K,INTFOR,IREGN
  logical :: DEBUG
  data FMSDI/950.,950.,900.,850.,825.,870.,885.,870.,825.,850./

  FORMAX = FMSDI(IFOR)

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)

  !  DETERMINE HOW MANY SITE VALUES AND SDI VALUES WERE SET VIA KEYWORD.

  NSISET = 0
  NSDSET = 0
  do I = 1,MAXSP
     if (SITEAR(I) .gt.0.) NSISET = NSISET+1
     if (SDIDEF(I) .gt.0.) NSDSET = NSDSET+1
     5 continue
  end do
  if (DEBUG)write(JOSTND,*)'ENTERING SITSET, SITE VALUES SET = ',&
       &NSISET,'  SDI VALUES SET = ',NSDSET

  !  SET SITE SPECIES AND SITE INDEX IF NOT SET BY KEYWORD.
  !  FOR REGION 6 FORESTS SET SDI DEFAULTS HERE ALSO.

  !     REGION 6 FOREST --- CALL **ECOCLS** WITH THE ECOCLASS CODE,
  !     AND GET BACK THE DEFAULT SITE SPECIES, ALL SITE INDICIES,
  !     AND DEFAULT SDI MAXIMUMS ASSOCIATED WITH THE ECOCLASS

  JSISP = 0
  INDEX = 0
  KNTECO = 0
  NTOHI = 0
  10 call ECOCLS (PCOM,ASPEC,RSDI,RSI,ISFLAG,NUM,INDEX,ISEQ)
  KNTECO = KNTECO+1
  if (DEBUG)write(JOSTND,*)'AFTER ECOCLS,PCOM,ASPEC,RSDI,RSI,',&
       &'ISFLAG,NUM,INDEX,ISEQ = ',PCOM,ASPEC,RSDI/ACRtoHA,RSI,ISFLAG,&
       &NUM,INDEX,ISEQ

  ! IF DEFAULT SDI IS OUT OF BOUNDS, RESET IT.

  ITOHI = 0
  if (RSDI .gt. FORMAX) then
     RSDI = FORMAX
     ITOHI = 1
  end if

  if (ISEQ .eq.0) go to 25
  if (JSISP.eq.0.and. ISFLAG.eq.1) JSISP = ISEQ
  if (ISISP.le.0.and. ISFLAG.eq.1) ISISP = ISEQ
  if (SITEAR(ISEQ).le.0..and. NSISET.eq.0) SITEAR(ISEQ) = RSI
  if (SDIDEF(ISEQ) .le.0.) then
     SDIDEF(ISEQ) = RSDI
     if (ITOHI .gt.0) NTOHI = NTOHI+1
  end if
  if (ISISP.gt.0.and. ISFLAG.eq.1) then
     if (SDIDEF(ISISP) .le.0.) then
        SDIDEF(ISISP) = RSDI
        if (ITOHI .gt.0) NTOHI = NTOHI+1
     end if
  end if
  25 continue
  if (NUM.gt.1.and. KNTECO.lt.NUM) then
     INDEX = INDEX+1
     go to 10
  end if

  !  ON THE CHANCE THAT A SITE SPECIES WAS NOT ENCOUNTERED IN **ECOCLS**
  !  PROVIDE A REGION 6 GLOBAL DEFAULT.

  if (ISISP .le.0) ISISP = 10
  if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 70.

  ! TRANSLATE SITE INDEX TO A REFERENCE AGE FOR EACH SPECIES.

  call SICHG(ISISP,SITEAR(ISISP),SIAGE)

  ! IF SITEAR HAS NOT BEEN SET WITH SITECODE KEYWORD,
  ! LOAD IT WITH SITE VALUES CALCULATED HERE.

  do ISPC = 1,MAXSP
     if (DEBUG)write(JOSTND,*)'IN SITSET ISISP,ISPC,SITEAR =',&
          &ISISP,ISPC,SITEAR(ISISP)*FTtoM
     if (SITEAR(ISPC) .gt.0.0) go to 30
     AG = SIAGE(ISPC)
     SINDX = SITEAR(ISISP)
     if (DEBUG)write(JOSTND,*)'CALLING HTCALC SINDX,ISISP,AG,SI=',&
          &SINDX,ISISP,AG,SI*FTtoM
     call HTCALC(SINDX,ISISP,AG,SI)
     if (DEBUG)write(JOSTND,*)'RETURN FROM HTCALC,SINDX,ISISP,AG,SI=',&
          &SINDX,ISISP,AG,SI*FTtoM
     if (ISPC.eq.20.and. ISISP.ne.20) then
        SI = SI/3.281
        if (SI.gt.28.)SI = 28.
     end if

     ! MISC. HARDWOODS USE CURTIS SI CURVE CREATED FOR DF. THE
     ! FOLLOWING EQUATIONS ARE USED TO REDUCE THEIR GROWTH UNTIL
     ! SI CURVES ARE FOUND FOR THESE SPECIES. 4/26/01 EES.

     if (ISPC.eq.21.and. ISISP.ne.21)SI = SI*.75
     if (ISPC.eq.23.and. ISISP.ne.23)SI = SI*.65
     if (ISPC.eq.24.and. ISISP.ne.24)SI = SI*1.5
     if (ISPC.eq.25.and. ISISP.ne.25)SI = SI*.70
     if (ISPC.eq.26.and. ISISP.ne.26)SI = SI*.75
     if (ISPC.eq.27.and. ISISP.ne.27)SI = SI*.85
     if (ISPC.eq.29.and. ISISP.ne.29)SI = SI*.23
     if (ISPC.eq.31.and. ISISP.ne.31)SI = SI*.70
     if (ISPC.eq.33.and. ISISP.ne.33)SI = SI*.25
     if (ISPC.eq.34.and. ISISP.ne.34)SI = SI*.60
     if (ISPC.eq.35.and. ISISP.ne.35)SI = SI*.25
     if (ISPC.eq.36.and. ISISP.ne.36)SI = SI*.50
     if (ISPC.eq.37.and. ISISP.ne.37)SI = SI*.50

     ! FOR WHITE OAK USE METHOD DERIVED BY GOULD TO GET ESTIMATE OF MAXIMUM
     ! HEIGHT (THINK OF AS A BASE AGE 300) FROM DF, FROM KING CURVE (BASE AGE 50).

     if (ISPC.eq.28.and. ISISP.ne.28)SI = 114.2*(1-EXP(-0.0266*SI))**2.26

     SITEAR(ISPC) = SI
     30 continue
  end do

  ! LOAD THE SDIDEF ARRAY

  do I = 1,MAXSP
     if (SDIDEF(I) .eq.0.0) SDIDEF(I) = SDIDEF(ISISP)
     80 continue
  end do
  if (BAMAX.le.0) BAMAX = SDIDEF(ISISP)*(PMSDIU/100.)*0.54542

  do I = 1,15
     J = (I-1)*10 + 1
     JJ = J+9
     if (JJ.gt.MAXSP)JJ = MAXSP
     write(JOSTND,90)(NSP(K,1)(1:2),K = J,JJ)
     90 format(/'SPECIES ',5X,10(A2,6X))
     write(JOSTND,91)(SDIDEF(K),K = J,JJ )
     91 format('SDI MAX ',   10F8.0)
     if (JJ .eq. MAXSP)go to 93
     92 continue
  end do
  93 continue
  if (NTOHI .gt.0)write(JOSTND,102)FORMAX
  102 FORMAT(/'*NOTE -- AT LEAST ONE DEFAULT MAXIMUM SDI EXCEEDED THE FO&
       &REST DEFAULT MAXIMUM. FOREST DEFAULT MAXIMUM OF ',F5.0,' USED.',/&
       &,'          YOU MAY NEED TO SPECIFICALLY RESET VALUES FOR THESE SP&
       &ECIES USING THE SDIMAX KEYWORD.')

  !  LOAD VOLUME DEFAULT MERCH. SPECS.

  select case(IFOR)
  case(7,8,9,10)
     do I = 1,MAXSP
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 7.0
        if (TOPD(I) .le.0.) TOPD(I) = 5.0
        if (BFTOPD(I) .le.0.) BFTOPD(I) = 5.0
        if (BFMIND(I) .le.0.) BFMIND(I) = 7.0
     end do
  case default
     do I = 1,MAXSP
        if ((DBHMIN(I) .le.0.).and.(I.eq.11)) DBHMIN(I) = 6.0
        if ((BFMIND(I) .le.0.).and.(I.eq.11)) BFMIND(I) = 6.0
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 7.0
        if (TOPD(I) .le.0.) TOPD(I) = 4.5
        if (BFTOPD(I) .le.0.) BFTOPD(I) = 4.5
        if (BFMIND(I) .le.0.) BFMIND(I) = 7.0
     end do
  end select

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'WC'

  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9)).and.&
          &(VEQNNB(ISPC).eq.'           ')) then
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
  230 format(4(3X,A2,4X,A10,1X,A10))

  return
end
