!=====================================!
!   Initialize species site indices   !
!=====================================!
! OC $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'
  include 'ORGANON.f90'

  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG
  character(len=4) :: ASPEC
  real    :: R5ADJ(MAXSP),R5SDI(MAXSP),R6ADJ(MAXSP)
  real    :: FORMAX,RSI,RSDI,HGUESS,ADJFAC
  integer :: NTOHI,NSISET,NSDSET,I,JSISP,INDEX,KNTECO,ISEQ,NUM
  integer :: ISFLAG,ITOHI,K,J,JJ,ISPC,INTFOR,IREGN
  logical :: DEBUG

  !   SPECIES ORDER

  !  1=PC  2=IC  3=RC  4=GF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
  ! 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
  ! 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
  ! 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=FL
  ! 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH 50=RW

  data R5SDI /&
       &570., 570., 570., 760., 800., 800., 600., 580., 580., 460.,&
       &430., 580., 430., 460., 430., 430., 460., 430., 430., 430.,&
       &330., 580., 1052., 570., 430., 550., 550., 550., 550., 550.,&
       &550., 550., 550., 550., 550., 550., 550., 550., 550., 550.,&
       &550., 550., 550., 550., 550., 550., 550., 550., 550., 1052./

  !  IF THESE ADJUSTMENT FACTORS CHANGE, ALSO CHANGE THEM IN DUNN.

  data R5ADJ/&
       &0.90, 0.76, 0.90, 1.00, 1.00, 1.00, 1.00, 0.90, 0.90, 0.90,&
       &0.90, 0.90, 0.90, 0.90, 1.00, 1.00, 0.90, 1.00, 0.90, 0.90,&
       &0.76, 0.76, 1.00, 0.76, 0.90, 0.57, 0.57, 0.57, 0.57, 0.57,&
       &0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57,&
       &0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 1.00/

  ! THE ADJUSTMENT FACTORS IN R6ADJ ARE RELATIVE TO HANN-SCRIVANI
  ! DF SITE INDEX.

  data R6ADJ/&
       &0.90, 0.70, 0.80, 1.00, 1.00, 1.00, 1.00, 0.95, 0.90, 0.90,&
       &0.90, 0.90, 0.90, 0.90, 0.94, 1.00, 0.94, 0.94, 0.90, 0.90,&
       &0.76, 0.76, 1.00, 0.40, 0.76, 0.28, 0.42, 0.34, 0.28, 0.40,&
       &0.56, 0.76, 0.28, 0.76, 0.56, 0.76, 0.76, 0.76, 0.40, 0.70,&
       &0.40, 0.76, 0.76, 0.40, 0.76, 0.25, 0.25, 0.25, 0.56, 1.00/

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

  if (DEBUG)write(JOSTND,*)'IFOR,ICL5,PCOM = ',IFOR,ICL5,PCOM
  if (IFOR .ge.6) then

     !       REGION 6 FOREST --- CALL **ECOCLS** WITH THE ECOCLASS CODE,
     !       AND GET BACK THE DEFAULT SITE SPECIES, ALL SITE INDICIES,
     !       AND DEFAULT SDI MAXIMUMS ASSOCIATED WITH THE ECOCLASS
     !       IF THE ECOCLASS CODE HASN'T BEEN DEFAULTED, DEFAULT IT HERE.
     !       (MAKE SURE THIS DEFAULT MATCHES THE ONE IN HABTYP)

     if (ICL5.eq.0) then
        ICL5 = 452
        PCOM = 'CWC221 '
        ITYPE = 46
        KODTYP = 452
     end if
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

     ! REGION 5 FORESTS

     if (ISISP .le.0) ISISP = 7
     if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 80.
     HGUESS = SITEAR(ISISP)
     JSISP = ISISP
  end if

  !  ON THE CHANCE THAT A SITE SPECIES WAS NOT ENCOUNTERED IN **ECOCLS**
  !  PROVIDE A REGION 6 GLOBAL DEFAULT.

  if (IFOR .ge.6) then
     if (ISISP .le.0) ISISP = 7
     if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 80.

     ! COMPUTE THE HANN-SCRIVANI DF SITE INDEX.

     HGUESS = SITEAR(ISISP)/R6ADJ(ISISP)
     if (DEBUG)write(JOSTND,*)'ISISP,SITEAR,R6ADJ,HGUESS= ',&
          &ISISP,SITEAR(ISISP),R6ADJ(ISISP),HGUESS
  end if

  ! IF SITEAR() HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD
  ! IT WITH SITE VALUES TRANSLATED FROM SITE INDEX OF SITE SPECIES.

  ! SETTING FOR SPECIES 18 AND 7 IS FROM THE ORGANON CODE

  if (DEBUG)write(JOSTND,*)' IN SITSET BEFORE ORG SITE EQNS SP 18 &',&
       &' 7 SITE INDEX = ',SITEAR(18),SITEAR(7)
  if (SITEAR(7).gt.0.0.or. SITEAR(18).gt.0.0) then
     if (SITEAR(7) .le.0.0) then
        SITEAR(7) = 1.062934*SITEAR(18)
     else if (SITEAR(18) .le.0.0) then
        SITEAR(18) = 0.940792*SITEAR(7)
     end if
  end if
  if (DEBUG)write(JOSTND,*)' IN SITSET AFTER ORG SITE EQNS SP 18 &',&
       &' 7 SITE INDEX = ',SITEAR(18),SITEAR(7)

  do I = 1,MAXSP
     if (IFOR .ge.6) then
        ADJFAC = R6ADJ(I)
     else
        ADJFAC = R5ADJ(I)
     end if
     if (SITEAR(I) .eq.0.0) SITEAR(I) = HGUESS*ADJFAC
     35 continue
  end do

  ! LOAD THE SDIDEF ARRAY

  K = ISISP
  if (SDIDEF(K) .le.0.) K = JSISP
  do I = 1,MAXSP
     if (SDIDEF(I) .gt.0.0) go to 40
     if (BAMAX .gt.0.) then
        SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
        if (SDIDEF(I) .gt. FORMAX) then
           SDIDEF(I) = FORMAX
           NTOHI = NTOHI+1
        end if
     else if (IFOR .ge.6) then
        SDIDEF(I) = SDIDEF(K)
        if (SDIDEF(I) .gt. FORMAX) then
           SDIDEF(I) = FORMAX
           NTOHI = NTOHI+1
        end if
     else
        SDIDEF(I) = R5SDI(I)
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
  case(6:10)
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
  !  BY KEYWORD IN INITRE.  ONLY CHANGE THOSE NOT SET BY KEYWORD.
  !  SET DEFAULTS TO USE NATCRS EQUATIONS.

  do ISPC = 1,MAXSP
     if (METHB(ISPC).eq.999)METHB(ISPC) = 6
     if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     150 continue
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'OC'

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

  if (RVARS(1) .eq.0.) RVARS(1) = SITEAR(  7 ) ! THE DOUGLAS-FIR SITE INDEX
  if (RVARS(2) .eq.0.) RVARS(2) = SITEAR( 18 ) ! THE PP SITE INDEX

  if (RVARS(3) .eq.0.) RVARS(3) = SDIDEF(  7 ) ! MAX SDI FOR DF
  if (RVARS(4) .eq.0.) RVARS(4) = SDIDEF(  4 ) ! MAX SDI FOR WF/GF
  if (RVARS(5) .eq.0.) RVARS(5) = SDIDEF( 18 ) ! MAX SDI FOR PP

  !     DON'T ADJUST THESE FOR NOW. SHOWN HERE SO WE KNOW WHAT THEY ARE

  !     RVARS(6)   = DF GENETIC WORTH VALUE FOR DIAMETER GROWTH
  !     RVARS(7)   = DF GENETIC WORTH VALUE FOR HEIGHT GROWTH
  !     RVARS(8)   = N YRS FOLIAGE RETENTION FOR DF (SWISS NEEDLE CAST)
  !     RVARS(9)   = PLANTING DENSITY OF ALDER RAP VERSION

  !     RVARS(10-30)   = UNUSED.

  if (MSDI_1.eq.0.) MSDI_1 = SDIDEF(7) !DF MAX SDI
  if (MSDI_2.eq.0.) MSDI_2 = SDIDEF(18) !PP MAX SDI
  if (MSDI_3.eq.0.) MSDI_3 = SDIDEF(4) !GF MAX SDI

  return
end
