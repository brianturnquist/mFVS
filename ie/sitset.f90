!=====================================!
!   Initialize species site indices   !
!=====================================!
! IE $Id$
subroutine SITSET
  implicit none

  !  THIS SUBROUTINE IS USED TO SET SIMULATION CONTROLLING VALUES
  !  THAT HAVE NOT BEEN SET USING THE KEYWORDS --- SDIMAX, BAMAX.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  !  SPECIES EXPANSION
  !  WB USE COEFFICIENTS FOR L
  !  LM AND PY USE COEFFICIENTS FROM TT FOR LM
  !  LL USE COEFFICIENTS FOR AF
  !  AS, MM, PB USE COEFFIECIENTS FOR AS FROM UT
  !  CO, OH USE COEFFIECIENTS FOR OH FROM CR
  !  OS USE COEFFEICIENTS FOR MH
  !  PI, JU USE COEFFICIENTS FROM UT

  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG
  real    :: BAMAXA(30)
  integer :: MAPSIT(30,MAXSP),MAPSS(30)
  integer :: ISPC,I,J,JJ,K,INTFOR,IREGN

  data BAMAXA/&
       &140.,220.,250.,310.,240.,270.,310.,310.,200.,310.,&
       &290.,330.,380.,440.,500.,500.,390.,390.,440.,180.,&
       &290.,400.,350.,390.,260.,300.,220.,220.,160.,300./

  data MAPSIT/&
       &330*0,&
       &30*0,& !WB
       &25,29,35,35,32,34,35,32,27,39,37,32,36,41,41,41,& !LM
       &43,38,39,32,36,34,34,32,36,36,36,26,22,26,&
       &30*0,& !LL
       &7, 9,13,12,10,12,12,11, 8,14,11,13,16,15,15,15,& !PI
       &16,14,14,11,12,13,13,11,13,13,13, 8, 6, 8,&
       &6, 8,10,10, 9,10,10, 9, 7,11, 9,11,12,12,12,12,& !JU
       &13,11,11, 9,10,10,10, 9,10,10,10, 7, 6, 7,&
       &25,29,35,35,33,34,35,32,27,39,37,32,36,41,41,41,& !PY
       &43,38,39,32,36,34,34,32,36,36,36,26,22,26,&
       &36,43,51,50,44,49,48,46,41,55,46,52,59,58,58,58,& !AS
       &60,54,55,46,49,52,52,46,52,52,52,38,33,38,&
       &44,57,76,62,72,72,71,67,55,86,67,80,95,93,93,93,& !CO
       &98,84,86,67,73,80,80,67,80,80,80,49,36,49,&
       &36,43,51,50,44,49,48,46,41,55,46,52,59,58,58,58,& !MM
       &60,54,55,46,49,52,52,46,52,52,52,38,33,38,&
       &36,43,51,50,44,49,48,46,41,55,46,52,59,58,58,58,& !PB
       &60,54,55,46,49,52,52,46,52,52,52,38,33,38,&
       &44,57,76,62,72,72,71,67,55,86,67,80,95,93,93,93,& !OH
       &98,84,86,67,73,80,80,67,80,80,80,49,36,49,&
       &30*0/ !OS

  data MAPSS/2*10,7*3,2*8,2*4,3*6,5,5*9,11,9,11,3*9,12,3/

  !  ASSIGN SITE INDEX BY SPECIES AND HABITAT TYPE

  if (ISISP.eq.0)ISISP = MAPSS(ITYPE)
  do ISPC = 1,MAXSP
     if (SITEAR(ISPC).eq.0.)SITEAR(ISPC) = MAPSIT(ITYPE,ISPC)
     5 continue
  end do

  !  SET SDIDEF AND BAMAX VALUES WHICH HAVE NOT BEEN SET BY KEYWORD.

  if (BAMAX.le.0) BAMAX = BAMAXA(ITYPE)
  do I = 1,MAXSP
     if (SDIDEF(I).le.0.) SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
     10 continue
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

  !  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
  !  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
  !  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.

  !  COLVILLE  (IFOR=5) IS A REGION 6 FOREST. ALL OTHERS FORESTS IN THIS
  !  VARIANT ARE IN REGION 1.

  do ISPC = 1,MAXSP
     if (IFOR.eq.5) then
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
  VAR = 'IE'

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
