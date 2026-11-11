!=====================================!
!   Initialize species site indices   !
!=====================================!
! OP $Id$
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
  include 'ORGANON.f90'

  ! Variables
  logical :: DEBUG
  character(len=2) :: FORST
  character(len=2) :: FORDUM
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  character(len=4) :: ASPEC
  character(len=26) :: LABEL
  integer :: IFIASP,ERRFLAG
  integer :: NSISET,NSDSET,I,JSISP,INDEX,KNTECO,NTOHI,ISEQ,NUM
  integer :: ISFLAG,ITOHI,ISPC,J,JJ,K,INTFOR,IREGN,IRDUM
  real    :: SIAGE(MAXSP)
  real    :: FORMAX,RSI,RSDI,AG,SINDX,SI

  data FORMAX/950./

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)

  !  FINISH PROCESSING THE MODTYPE KEYWORD

  write(JOSTND,4)
  4 format(/'MODTYPE    KEYWORD NOW BEING PROCESSED')

  !  DECODE MODEL TYPE

  if (IMODTY .eq.2) LABEL = 'NORTHWEST OREGON          '
  if (IMODTY .eq.3) LABEL = 'STAND MANAGEMENT COOP     '
  if (IMODTY.eq.2.or. IMODTY.eq.3) then
     write (JOSTND,200) IMODTY,LABEL
     200 format(T12,'MODEL TYPE FOR THIS PROJECTION IS ',I4,'. ',A26)
  else
     IMODTY = 2
     LABEL = 'NORTHWEST OREGON          '
     write(JOSTND,14)IMODTY,LABEL
     14 format(T12,'MODEL TYPE NOT RECOGNIZED. BEING SET TO',I4,&
          &'= ',A26,'  FOR FURTHER PROCESSING')
  end if
  VERSION = IMODTY

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

  if (ISISP .le.0) ISISP = 16
  if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 100.

  ! TRANSLATE SITE INDEX TO A REFERENCE AGE FOR EACH SPECIES.

  call SICHG(ISISP,SITEAR(ISISP),SIAGE)

  ! IF SITEAR HAS NOT BEEN SET WITH SITECODE KEYWORD,
  ! LOAD IT WITH SITE VALUES CALCULATED HERE.

  ! SETTING FOR SPECIES 16 AND 19 IS FROM THE ORGANON CODE

  !  Site index conversion equation from Nigh (1995, Forest Science 41:84-98)

  if (DEBUG)write(JOSTND,*)' IN SITSET BEFORE ORG SITE EQNS SP 16 &',&
       &' 19 SITE INDEX = ',SITEAR(16),SITEAR(19)
  if (SITEAR(16).gt.0.0.or. SITEAR(19).gt.0.0) then
     if (SITEAR(16) .le.0.0) then
        SITEAR(16) = 0.480 +( 1.110 * SITEAR(19))
     else if (SITEAR(19) .le.0.0) then
        SITEAR(19) = -0.432 +( 0.899 * SITEAR(16))
     end if
  end if
  if (DEBUG)write(JOSTND,*)' IN SITSET AFTER ORG SITE EQNS SP 16 &',&
       &' 19 SITE INDEX = ',SITEAR(16),SITEAR(19)

  do ISPC = 1,MAXSP
     if (DEBUG)write(JOSTND,*)'IN SITSET ISISP,ISPC,SITEAR =',&
          &ISISP,ISPC,SITEAR(ISISP)
     if (SITEAR(ISPC) .gt.0.0) go to 30
     AG = SIAGE(ISPC)
     SINDX = SITEAR(ISISP)
     if (DEBUG)write(JOSTND,*)'CALLING HTCALC SINDX,ISISP,AG,SI=',&
          &SINDX,ISISP,AG,SI
     SI = 0.
     call HTCALC(SINDX,ISISP,AG,SI,JOSTND,DEBUG)
     if (DEBUG)write(JOSTND,*)'RETURN FROM HTCALC,SINDX,ISISP,AG,SI=',&
          &SINDX,ISISP,AG,SI
     if (ISPC.eq.20.and. ISISP.ne.20) then
        SI = SI/3.281
        if (SI.gt.28.)SI = 28.
     end if

     ! MISC. HARDWOODS USE CURTIS SI CURVE CREATED FOR DF. THE
     ! FOLLOWING EQUATIONS ARE USED TO REDUCE THEIR GROWTH UNTIL
     ! SI CURVES ARE FOUND FOR THESE SPECIES. 4/26/01 EES.

     if (ISPC.eq.21.and. ISISP.ne.21)SI = SI*.75
     if (ISPC.eq.23.and. ISISP.ne.23)SI = SI*.65
     if (ISPC.eq.24.and. ISISP.ne.24)SI = SI*.70
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
     ! HEIGHT AT AGE 100) FROM DF, FROM KING CURVE (BASE AGE 50).

     if (ISPC.eq.28.and. ISISP.ne.28)SI = 114.2*(1-EXP(-0.0266*SI))**2.26

     SITEAR(ISPC) = SI
     30 continue
  end do

  ! LOAD THE SDIDEF ARRAY

  do I = 1,MAXSP
     if (SDIDEF(I) .gt.0.0) go to 80
     SDIDEF(I) = SDIDEF(ISISP)
     80 continue
  end do
  if (BAMAX.le.0) BAMAX = SDIDEF(ISISP)*(PMSDIU/100.)*0.54542

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
  case(4,5,6)
     do I = 1,MAXSP
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 7.0
        if (TOPD(I) .le.0.) TOPD(I) = 5.0
        if (BFTOPD(I) .le.0.0) BFTOPD(I) = 5.0
        if (BFMIND(I) .le.0.0) BFMIND(I) = 7.0
        if (SCFTOPD(I) .le.0.0) SCFTOPD(I) = 5.0
        if (SCFMIND(I) .le.0.0) SCFMIND(I) = 7.0
     end do
  case default
     do I = 1,MAXSP
        if ((DBHMIN(I) .le.0.).and.(I.eq.11)) DBHMIN(I) = 6.0
        if ((BFMIND(I) .le.0.).and.(I.eq.11)) BFMIND(I) = 6.0
        if ((SCFMIND(I) .le.0.).and.(I.eq.11)) SCFMIND(I) = 6.0
        if (DBHMIN(I) .le.0.) DBHMIN(I) = 7.0
        if (TOPD(I) .le.0.) TOPD(I) = 4.5
        if (BFTOPD(I) .le.0.) BFTOPD(I) = 4.5
        if (BFMIND(I) .le.0.) BFMIND(I) = 7.0
        if (SCFTOPD(I) .le.0.) SCFTOPD(I) = 4.5
        if (SCFMIND(I) .le.0.) SCFMIND(I) = 7.0
     end do
  end select

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'OP'

  !  USE SPECIAL FORESTS FOR BIA LANDS
  !  QUINAULT IR (FC=800 USES OLYMIC 609), ALSO IN INITRE VOLEQNUM PROCESS

  if (IREGN.eq.8) then
     IRDUM = 6
     FORDUM = '09'
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

  !  SET ORGANON VARIABLES AS APPROPRIATE

  !  IF STAND AGE FROM STDINFO KEYWORD AND ORGANON .INP FILE IS 0
  !  SET EVEN/UNEVEN FLAG TO EVEN-AGED STAND

  ITEST = 0
  if (IAGE .eq.0) then
     if (INDS(4) .eq.1) then
        INDS(4) = 0 ! UNEVEN-AGED
        ITEST = 1
     end if
  end if

  if (RVARS(1) .eq.0.) RVARS(1) = SITEAR( 16 ) ! DOUGLAS-FIR SITE INDEX
  if (RVARS(2) .eq.0.) RVARS(2) = SITEAR( 19 ) ! WESTERN HEMLOCK SITE INDEX

  if (RVARS(3) .eq.0.) RVARS(3) = SDIDEF( 16 ) ! MAX SDI FOR DF
  if (RVARS(4) .eq.0.) RVARS(4) = SDIDEF(  3 ) ! MAX SDI FOR GF
  if (RVARS(5) .eq.0.) RVARS(5) = SDIDEF( 19 ) ! MAX SDI FOR WH

  !     DON'T ADJUST THESE FOR NOW. SHOWN HERE SO WE KNOW WHAT THEY ARE

  !     RVARS(6)   = DF GENETIC WORTH VALUE FOR DIAMETER GROWTH
  !     RVARS(7)   = DF GENETIC WORTH VALUE FOR HEIGHT GROWTH
  !     RVARS(8)   = N YRS FOLIAGE RETENTION FOR DF (SWISS NEEDLE CAST)
  !     RVARS(9)   = PLANTING DENSITY OF ALDER RAP VERSION

  !     RVARS(10-30)   = UNUSED.

  if (MSDI_1.eq.0.) MSDI_1 = SDIDEF(16) !DF MAX SDI
  if (MSDI_2.eq.0.) MSDI_2 = SDIDEF(15) !PP MAX SDI
  if (MSDI_3.eq.0.) MSDI_3 = SDIDEF(3) !GF MAX SDI

  return
end
