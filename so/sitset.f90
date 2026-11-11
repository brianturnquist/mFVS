!=====================================!
!   Initialize species site indices   !
!=====================================!
! SO $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD, AND
  ! LOADS THE SDIDEF ARRAY WITH AN SDI MAX FOR EACH SPECIES WHICH WAS
  ! NOT ASSIGNED AN SDI VALUE BY KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'

  !  SPECIES ORDER:
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  ! Variables
  logical :: DEBUG
  character(len=2) :: FORST
  character(len=2) :: FORDUM
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  character(len=4) :: ASPEC
  integer :: IFIASP,TEMPSPCD,ERRFLAG
  integer :: NTOHI,NSISET,NSDSET,I,JSISP,INDEX,KNTECO,ISEQ,NUM,ISFLAG
  integer :: ITOHI,ISPC,K,J,JJ,INTFOR,IREGN,IRDUM
  real    :: SIAGE(MAXSP),SI(MAXSP),SLO,SHI,SLOSSP,SHISSP
  real    :: C6(MAXSP),C5(MAXSP)
  real    :: FORMAX,RSI,RSDI,TEM,SINDX,AG

  data C5 /&
       &272., 561., 570., 800., 687., 576., 679., 620., 1000., 365.,&
       &272., 800., 602., 790., 1000., 621., 423., 762., 682., 576.,&
       &441., 441., 629., 562., 452., 441., 440., 447., 785., 501.,&
       &501., 365., 441./

  data C6/&
       &447., 447., 767., 659., 758., 447., 541., 659., 750., 429.,&
       &500., 659., 659., 659., 659., 250., 250., 650., 650., 650.,&
       &200., 300., 300., 250., 250., 200., 200., 150., 250., 100.,&
       &100., 447., 250./

  data FORMAX/850./

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)

  !  DETERMINE HOW MANY SITE VALUES AND SDI VALUES WERE SET VIA KEYWORD.

  NTOHI = 0
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
  !  FOR REGION 6 FORESTS, AND WARM SPRINGS, SET SDI DEFAULTS HERE ALSO.

  if (IFOR.le.3.or. IFOR.eq.10) then
     if (CALCSDI.eq.' ')LZEIDE = .false.

     !       REGION 6 FOREST --- CALL **ECOCLS** WITH THE ECOCLASS CODE,
     !       AND GET BACK THE DEFAULT SITE SPECIES, ALL SITE INDICIES,
     !       AND DEFAULT SDI MAXIMUMS ASSOCIATED WITH THE ECOCLASS

     JSISP = 0
     INDEX = 0
     KNTECO = 0
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

     !       LOOP THROUGH JSP ARRAY AND DETERMINE FVS SEQUENCE NUMBER FOR
     !       THIS SPECIES

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
  else
     if (ISISP .le.0) ISISP = 4
     if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 50.
     JSISP = ISISP
  end if

  !  ON THE CHANCE THAT A SITE SPECIES WAS NOT ENCOUNTERED IN **ECOCLS**
  !  PROVIDE A REGION 6 GLOBAL DEFAULT.

  if (IFOR.le.3.or. IFOR.eq.10) then
     if (ISISP .le.0) ISISP = 10
     if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 70.
  end if

  !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THE SITE SPECIES.

  SLOSSP = 0.
  SHISSP = 999.
  call SITERANGE (1,ISISP,SLOSSP,SHISSP)

  !  TRANSLATE THE SITES AND KEEP

  !  SITE SPECIES 11(UT), 16(TT) AND 24(UT) USE THE FOLLOWING SITE
  !  METHOD

  !  SET SITE FOR UT VARIANT SPECIES 11 16, AND 24

  SLO = 0.
  SHI = 999.
  if (ISISP.eq.11.or. ISISP.eq.16.or. ISISP.eq.24) then
     TEM = 30.
     if (ISISP .gt.0.and. SITEAR(ISISP) .gt.0.0)TEM = SITEAR(ISISP)
     if (TEM.lt.SLOSSP)TEM = SLOSSP
     do I = 1,MAXSP

        !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

        SLO = 0.
        SHI = 999.
        call SITERANGE (1,I,SLO,SHI)

        SI(I) = SLO + (TEM-SLOSSP)/(SHISSP-SLOSSP)&
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

  ! SI IS A VECTOR X MAXSP GIVING EQUIVALENT SITES FOR EACH SPECIES

  if (DEBUG) write(JOSTND,9003)ISISP,SITEAR(ISISP),SIAGE
  9003 format('MAIN REF SPECIES ',I5,' SITE INDEX ',F10.1/&
       &((4X,11F6.1)/))

  if (DEBUG)write(JOSTND,*)'IN SITSET STARTING DO 30 LOOP'
  do ISPC = 1,MAXSP
     AG = SIAGE(ISPC)
     SINDX = SITEAR(ISISP)
     call HTCALC(IFOR,SINDX,ISISP,AG,SI(ISPC),JOSTND,DEBUG)

     !  CHANGE THE SITE INDEX OF HEMLOCK TO METRIC.

     if (ISPC .eq.5 ) then
        SI(ISPC) = SI(ISPC)/3.281
        if (SI(ISPC).gt.28.)SI(ISPC) = 28.
     end if
     if (ISPC.eq.11.or. ISPC.eq.16.or. ISPC.eq.24) then

        !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

        SLO = 0.
        SHI = 999.
        call SITERANGE (1,ISPC,SLO,SHI)

        TEM = SITEAR(ISISP)
        if (TEM.lt.SLOSSP)TEM = SLOSSP
        if (SITEAR(ISPC).le.0.0)&
             &SI(ISPC) = SLO + (TEM-SLOSSP)/(SHISSP-SLOSSP)&
             &*(SHI-SLO)
     end if

     ! DELETED A REFERENCE TO SPECIES 6 AS WELL AS 5 SO HTCALC IS CONSISTENT
     ! WITH THE NEED TO DEMETRIC THE HEMLOCK SITE.  RALPH 1/24/89

     30 continue
  end do
  if (DEBUG) write(JOSTND,9025) SINDX,ISISP,SI
  9025 format('IN SIADJ  SINDX ', F10.2,' SPECIES ',I5,&
       &' TRANSLATED SITES'/,((11F6.1)/))

  ! IF SITELG() HAS NOT BEEN SET WITH SITECOD KEYWORD, LOAD
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
     else if (IFOR.le.3.or. IFOR.eq.10) then
        SDIDEF(I) = SDIDEF(K) * (C6(I)/C6(K))
        if (SDIDEF(I) .gt. FORMAX) then
           SDIDEF(I) = FORMAX
           NTOHI = NTOHI+1
        end if
     else
        SDIDEF(I) = C5(I)
     end if
     40 continue
  end do

  !  WRITE SIDMAX, BY SPECIES, TO OUTPUT

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

  !  LOAD VOLUME DEFAULT MERCH. SPECS.

  select case(IFOR)
  case(1:3,10)
     do I = 1,MAXSP
        if (TOPD(I) .le.0.) TOPD(I) = 4.5
        if (BFTOPD(I) .le.0.) BFTOPD(I) = 4.5
        if (SCFTOPD(I) .le.0.) SCFTOPD(I) = 4.5
     end do
  case default
     do I = 1,MAXSP
        if (TOPD(I) .le.0.) TOPD(I) = 6.
        if (BFTOPD(I) .le.0.) BFTOPD(I) = 6.
        if (SCFTOPD(I) .le.0.) SCFTOPD(I) = 6.
     end do
  end select

  !  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
  !  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
  !  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.

  !  DESCHUTES, FREMONT, AND WINEMA (IFOR=1,2,3) ARE R6 FORESTS.
  !  WARM SPRINGS RESERVATION (IFOR=10) IS ALSO CONSIDERED A R6 FOREST
  !  KLAMATH, LASSEN, MODOC, AND PLUMAS (IFOR=4,5,6,7) ARE R5 FORESTS.
  !  SHASTA (IFOR=9) GETS MAPPED TO IFOR=4 IN **FORKOD**
  !  INDUSTRY (IFOR=8) USE R5 DEFAULTS.

  do ISPC = 1,MAXSP
     if (IFOR.lt.4.or. IFOR.eq.10) then
        if (METHB(ISPC).eq.999)METHB(ISPC) = 6
        if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     else
        if (METHB(ISPC).eq.999)METHB(ISPC) = 6
        if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     end if
     50 continue
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'SO'

  !  USE R5 EQNS FOR INDUSTRY LANDS; R6 EQNS FOR WARM SPRINGS
  !  HERE AND IN **INITRE** VOLEQNUM PROCESS
  !    INDUSTRY (KODFOR=701) USES KLAMATH (KODFOR=505)
  !    WARM SPRINGS (KODFOR=799) USES DESCHUTES (KODFOR=601)

  if (IREGN.eq.7) then
     select case (KODFOR)
     case (701)
        IRDUM = 5
        FORDUM = '05'
     case (799)
        IRDUM = 6
        FORDUM = '01'
     end select
  else
     IRDUM = IREGN
     FORDUM = FORST
  end if

  if (LFIANVB) call NVB_REGION_CHECK
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     VOLEQ = '           '
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then
        call VOLEQDEF(VAR,IRDUM,FORDUM,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
     else if (METHC(ISPC).eq.10) then
        TEMPSPCD = IFIASP
        if (IFIASP.eq.478) TEMPSPCD = 475
        call NVBEQDEF(TEMPSPCD,VOLEQ)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9)).and.&
          &(VEQNNB(ISPC).eq.'           ')) then
        VOLEQ = '           '
        call VOLEQDEF(VAR,IRDUM,FORDUM,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
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
