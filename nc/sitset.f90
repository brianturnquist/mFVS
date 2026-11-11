!=====================================!
!   Initialize species site indices   !
!=====================================!
! NC $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES THAT WAS NOT ASSIGNED BY KEYWORD, AND LOADS THE SDIDEF
  ! ARRAY FOR EACH SPECIES WHICH WAS NOT ASSIGNED AN SDI MAXIMUM
  ! BY KEYWORD.

  ! THIS SUBROUTINE ALSO SETS VARIABLES WHICH ARE FOREST SPECIFIC.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'

  ! Variables
  logical :: DEBUG
  character(len=2) :: FORST
  character(len=2) :: FORDUM
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  character(len=4) :: ASPEC
  integer :: IFIASP,ERRFLAG
  integer :: NTOHI,NSISET,NSDSET,I,JSISP,INDEX,KNTECO,ISEQ,NUM
  integer :: ISFLAG,ITOHI,ISPC,K,INTFOR,IREGN,IRDUM,J,JJ
  real    :: SIAGE(MAXSP),SI(MAXSP),C5(MAXSP),C6(MAXSP)
  real    :: FORMAX,RSI,RSDI,AG,SINDX

  !  DATA STATEMENTS

  data C5/365.,561.,570.,800.,515.,576.,406.,785.,1000.,365.,785.,&
       &1052/

  data C6/624.,647.,547.,759.,588.,706.,382.,759.,800.,571.,759.,&
       &1052/

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
  !  FOR REGION 6 FORESTS SET SDI DEFAULTS HERE ALSO.

  if (IFOR.eq.4.or. IFOR.eq.7) then
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
     if (ISISP .le.0) ISISP = 3
     if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 90.
     JSISP = ISISP
  end if

  !  ON THE CHANCE THAT A SITE SPECIES WAS NOT ENCOUNTERED IN **ECOCLS**
  !  PROVIDE A REGION 6 GLOBAL DEFAULT.

  if (IFOR.eq.4.or. IFOR.eq.7) then
     if (ISISP .le.0) ISISP = 3
     if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 90.
  end if

  ! TRANSLATE THE SITES AND KEEP

  call SICHG(ISISP,SITEAR(ISISP),SIAGE,JOSTND)

  ! SI IS A VECTOR X MAXSP GIVING EQUIVALENT SITES FOR EACH SPECIES

  if (DEBUG) write(JOSTND,9003)ISISP,SITEAR(ISISP),SIAGE
  9003 format('MAIN REF SPECIES ',I5,' SITE INDEX ',F10.1/&
       &5X,12F6.1)
  do ISPC = 1,MAXSP
     AG = SIAGE(ISPC)
     SINDX = SITEAR(ISISP)

     call HTCALC(SINDX,ISISP,AG,SI(ISPC),JOSTND,DEBUG)

     30 continue
  end do

  ! IF SITEAR() HAS NOT BEEN SET WITH SITECODE THEN CALCULATE VALUE HERE.

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
     else if (IFOR.eq.4.or. IFOR.eq.7) then
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
  case(4)
     do I = 1,MAXSP
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 9.0
        if (TOPD(I) .le.0.) TOPD(I) = 4.5
        if (BFMIND(I) .le.0.0) BFMIND(I) = 9.0
        if (BFTOPD(I) .le.0.) BFTOPD(I) = 4.5
        if (SCFMIND(I) .le.0.0) SCFMIND(I) = 9.0
        if (SCFTOPD(I) .le.0.0) SCFTOPD(I) = 4.5
     end do
  case(5,7)
     do I = 1,MAXSP
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 9.0
        if (TOPD(I) .le.0.) TOPD(I) = 5.0
        if (BFMIND(I) .le.0.0) BFMIND(I) = 9.0
        if (BFTOPD(I) .le.0.0) BFTOPD(I) = 5.0
        if (SCFMIND(I) .le.0.0) SCFMIND(I) = 9.0
        if (SCFTOPD(I) .le.0.0) SCFTOPD(I) = 5.0
     end do
  case default
     do I = 1,MAXSP
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 9.0
        if (TOPD(I) .le.0.) TOPD(I) = 6.0
        if (BFMIND(I) .le.0.0) BFMIND(I) = 9.0
        if (BFTOPD(I) .le.0.0) BFTOPD(I) = 6.0
        if (SCFMIND(I) .le.0.0) SCFMIND(I) = 9.0
        if (SCFTOPD(I) .le.0.0) SCFTOPD(I) = 6.0
     end do
  end select

  !  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
  !  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
  !  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.

  !  SISKIYOU (IFOR=4) IS A REGION 6 FOREST. ALL OTHERS FORESTS IN THIS
  !  VARIANT ARE IN REGION 5. SIMPSON & HOOPA USE R5 EQUATIONS.
  !  BLM (IFOR=7) USE NATCRS WITH BLM EQNS

  do ISPC = 1,MAXSP
     if (IFOR.eq.4.or. IFOR.eq.7) then
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
  VAR = 'NC'

  !  USE SPECIAL FORESTS FOR PRIVATE VOLUME EQS.
  !  NC SIMPSOM TIMBER (FC=800 USES R5 SAY 510)

  if (IREGN.eq.8) then
     IRDUM = 5
     FORDUM = '10'
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
        call NVBEQDEF(IFIASP, VOLEQ)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (VEQNNC(ISPC)(11:11) .eq. CHAR(0)) VEQNNC(ISPC)(11:11) = " "
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
