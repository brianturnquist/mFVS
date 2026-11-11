!=====================================!
!   Initialize species site indices   !
!=====================================!
! BM $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITELG ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'

  !  VARIABLE DEFINITIONS:

  !     SHI -- UPPER END OF THE SITE INDEX RANGE FOR THE SPECIES
  !  SHISSP -- UPPER END OF THE SITE INDEX RANGE FOR THE SITE SPECIES
  !     SLO -- LOWER END OF THE SITE INDEX RANGE FOR THE SPECIES
  !  SLOSSP -- LOWER END OF THE SITE INDEX RANGE FOR THE SITE SPECIES

  ! Variables
  logical :: DEBUG

  character(len=4) :: ASPEC
  character(len=2) :: DIST
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ

  integer :: ERRFLAG,I,IFIASP,INDEX,INTFOR,IREGN,ISEQ,ISFLAG,ISPC
  integer :: ITOHI,J,JJ,JSISP,K,KNTECO,NSDSET,NSISET,NTOHI,NUM

  real    :: AG,FORMAX,RSI,RSDI,SHI,SHISSP,SINDX,SLO,SLOSSP,TEM

  real    :: SI(MAXSP),SIAGE(MAXSP)

  !  DATA STATEMENTS:

  data FORMAX/850./

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
  !  17=OS, 18=OH

  !  SPECIES EXPANSION:
  !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
  !  LM USES UT LM
  !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
  !  YC USES WC YC
  !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
  !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
  !  OS USES BM PP BARK COEFFICIENT
  !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

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
       &'ISFLAG,NUM,INDEX,ISEQ = ',PCOM,ASPEC,RSDI,RSI,ISFLAG,&
       &NUM,INDEX,ISEQ

  ! IF DEFAULT SDI IS OUT OF BOUNDS, RESET IT.

  ITOHI = 0
  if (RSDI .gt. FORMAX) then
     RSDI = FORMAX
     ITOHI = 1
  end if

  !     LOOP THROUGH JSP ARRAY AND DETERMINE FVS SEQUENCE NUMBER FOR
  !     THIS SPECIES

  if (ISEQ .eq.0) go to 25
  if (JSISP.eq.0.and. ISFLAG.eq.1) JSISP = ISEQ
  if (ISISP.le.0.and. ISFLAG.eq.1) ISISP = ISEQ
  if (SITEAR(ISEQ).le.0..and. NSISET.eq.0) SITEAR(ISEQ) = RSI
  if (SDIDEF(ISEQ) .le.0.) then
     SDIDEF(ISEQ) = RSDI
     if (ITOHI .gt.0) NTOHI = NTOHI+1
  end if
  if (ISISP.gt.0.and. ISFLAG.eq.1) then
     if ((RSDI.le.0.).and.(SDIDEF(ISISP).le.0.)) then
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

  !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THE SITE SPECIES.

  SLOSSP = 0.
  SHISSP = 999.
  call SITERANGE (1,ISISP,SLOSSP,SHISSP)

  ! TRANSLATE THE SITES AND KEEP

  !  IF THE SITE SPECIES IS ONE FROM THE UT OR TT VARIANTS, USE AN
  !  ALTERNATE METHOD OF TRANSLATING SITE INDEX VALUES

  SLO = 0.
  SHI = 999.
  if (ISISP.eq.6.or. ISISP.eq.11.or. ISISP.eq.12.or. ISISP.eq.15)&
       &then
     TEM = 30.
     if (ISISP .gt.0.and. SITEAR(ISISP) .gt.0.0)TEM = SITEAR(ISISP)
     if (TEM.lt.SLOSSP)TEM = SLOSSP
     do I = 1,MAXSP

        !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

        SLO = 0.
        SHI = 999.
        call SITERANGE (1,I,SLO,SHI)

        SI(I) = SLO+(TEM-SLOSSP)/(SHISSP-SLOSSP)&
             &*(SHI-SLO)
        if (I .eq.5 ) then
           SI(I) = SI(I)/3.281
           if (SI(I).gt.28.)SI(I) = 28.
        end if
        20 continue
     end do
     go to 32
  else
     call SICHG(ISISP,SITEAR(ISISP),SIAGE)
  end if

  ! SI IS A VECTOR * MAXSP GIVING EQUIVALENT SITES FOR EACH SPECIES

  if (DEBUG) write(JOSTND,9003)ISISP,SITEAR(ISISP),SIAGE
  9003 format('MAIN REF SPECIES ',I5,' SITE INDEX ',F10.1/&
       &5X,11F6.1/5X,7F6.1)
  do ISPC = 1,MAXSP

     !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

     SLO = 0.
     SHI = 999.
     call SITERANGE (1,ISPC,SLO,SHI)

     AG = SIAGE(ISPC)
     SINDX = SITEAR(ISISP)
     call HTCALC(SINDX,ISISP,AG,SI(ISPC),JOSTND,DEBUG)

     ! CHANGE THE SITE INDEX OF HEMLOCK TO METRIC
     ! DELETED A REFERENCE TO SPECIES 6 AS WELL AS 5 SO HTCALC IS CONSISTENT
     ! WITH THE NEED TO DEMETRIC THE HEMLOCK SITE.  RALPH 1/24/89

     if (ISPC .eq.5 ) then
        SI(ISPC) = SI(ISPC)/3.281
        if (SI(ISPC).gt.28.)SI(ISPC) = 28.
     end if
     if (ISPC.eq.6.or. ISPC.eq.11.or. ISPC.eq.12.or. ISPC.eq.15) then
        TEM = SITEAR(ISISP)
        if (TEM.lt.SLOSSP)TEM = SLOSSP
        if (SITEAR(ISPC).le.0.0)&
             &SI(ISPC) = SLO + (TEM-SLOSSP)/(SHISSP-SLOSSP)&
             &*(SHI-SLO)
     end if
     30 continue
  end do
  if (DEBUG) write(JOSTND,9025) SINDX,ISISP,SI
  9025 format('SINDX ',F10.2,'SITE SP ',I5/' TRANSLATED SITES ',11F6.1/&
       &18X,7F6.1)

  ! IF SITEAR() HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD
  ! IT WITH SITE VALUES TRANSLATED FROM SITE INDEX OF SITE SPECIES.

  32 continue
  do I = 1,MAXSP
     if (SITEAR(I) .eq.0.0) SITEAR(I) = SI(I)
     35 continue
  end do

  ! LOAD THE SDIDEF ARRAY

  K = ISISP
  if (SDIDEF(K) .le.0.) K = JSISP
  do I = 1,MAXSP
     if (SDIDEF(I) .gt.0.0) go to 40
     if (BAMAX .gt.0.) then
        SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
     else
        SDIDEF(I) = SDIDEF(K)
     end if
     if (SDIDEF(I) .gt. FORMAX) then
        SDIDEF(I) = FORMAX
        NTOHI = NTOHI+1
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
  if (NTOHI .gt.0)write(JOSTND,102)FORMAX
  102 FORMAT(/'*NOTE -- AT LEAST ONE DEFAULT MAXIMUM SDI EXCEEDED THE FO&
       &REST DEFAULT MAXIMUM. FOREST DEFAULT MAXIMUM OF ',F5.0,' USED.',/&
       &,'          YOU MAY NEED TO SPECIFICALLY RESET VALUES FOR THESE SP&
       &ECIES USING THE SDIMAX KEYWORD.')

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'BM'

  if (LFIANVB) call NVB_REGION_CHECK
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     VOLEQ = '           '
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
     else if (METHC(ISPC).eq.10.and. VEQNNC(ISPC).eq.'          ') then
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
