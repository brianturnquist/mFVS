!=====================================!
!   Initialize species site indices   !
!=====================================!
! TT $Id$
subroutine SITSET
  implicit none

  !  THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  !  SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD, AND LOADS
  !  THE SDIDEF ARRAY WITH SDI MAXIMUMS FOR EACH SPECIES WHICH WAS NOT
  !  ASSIGNED AN SDI MAXIMUM USING THE SDIMAX KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG
  integer :: I,INTFOR,IREGN,ISPC,J,JJ,K
  real    :: SDICON(MAXSP),TEM,SITELO,SITEHI,SLOSSP,SHISSP

  ! SPECIES ORDER FOR TETONS VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH

  ! VARIANT EXPANSION:
  ! BS USES ES EQUATIONS FROM TT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! PP USES PP EQUATIONS FROM CI
  ! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! MM USES MM EQUATIONS FROM IE
  ! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM TT

  data SDICON /&
       &621., 409., 570., 358., 620., 562., 679., 620., 602., 446.,&
       &497., 411., 619., 680., 452., 501., 409., 452./

  ! IF SITEAR(I) HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD IT
  ! WITH DEFAULT SITE VALUES.

  TEM = 50.
  if (ISISP .gt.0) then
     if (SITEAR(ISISP) .gt.0.0) TEM = SITEAR(ISISP)
  end if
  if (ISISP .eq.0) ISISP = 3

  !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THE SITE SPECIES.

  SLOSSP = 0.
  SHISSP = 999.
  call SITERANGE (1,ISISP,SLOSSP,SHISSP)

  do I = 1,MAXSP

     !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

     SITELO = 0.
     SITEHI = 999.
     call SITERANGE (1,I,SITELO,SITEHI)

     if (TEM .lt. SLOSSP)TEM = SLOSSP
     if (SITEAR(I) .le.0.0) SITEAR(I) = SITELO +&
          &(TEM-SLOSSP)/(SHISSP-SLOSSP)&
          &*(SITEHI-SITELO)
     10 continue
  end do

  ! LOAD THE SDIDEF ARRAY.

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

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = 'TT'

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
