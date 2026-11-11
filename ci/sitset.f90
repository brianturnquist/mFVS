!=====================================!
!   Initialize species site indices   !
!=====================================!
! CI $Id$
subroutine SITSET
  implicit none

  !  THIS SUBROUTINE IS USED TO SET SIMULATION CONTROLLING VALUES
  !  THAT HAVE NOT BEEN SET USING THE KEYWORDS --- SDIMAX, BAMAX.

  !  IT IS ALSO USED TO SET DEFAULTS WHICH ARE DEPENDENT ON FOREST CODE
  !  WITHIN A VARIANT.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'CICOM.f90'
  include 'VOLSTD.f90'

  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG,ISPC,I,INTFOR,IREGN,J,JJ,K
  real    :: BAMAXA(130),SLO,SHI,TEM,SLOSSP,SHISSP
  real    :: R4SDI(MAXSP)

  !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

  !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
  !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
  !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
  !     4 = GRAND FIR (GF)                   ABIES GRANDIS
  !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
  !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
  !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
  !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
  !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
  !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
  !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
  !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
  !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
  !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
  !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
  !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
  !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
  !    18 = OTHER SOFTWOODS (OS)
  !    19 = OTHER HARDWOODS (OH)

  !  SURROGATE EQUATION ASSIGNMENT:

  !  FROM THE IE VARIANT:
  !      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
  !      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
  !      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
  !      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)

  !  FROM THE UT VARIANT:
  !      USE 12(WJ) FOR 14(WJ)
  !      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
  !                                                  REALLY WC39=OT)

  !  DATA STATEMENTS

  data BAMAXA/&
       &7*100., 130., 3*95., 220., 213., 140., 175., 154., 137.,&
       &122., 152., 100., 2*220., 233., 220., 175., 250., 2*190.,&
       &226., 140., 200., 210., 140., 160., 200., 160., 200., 160.,&
       &180., 160., 4*200., 160., 225., 2*160., 2*170., 215., 210.,&
       &225., 210., 220., 300., 225., 350., 280., 225., 280., 342.,&
       &200., 230., 282., 350., 280., 300., 280., 150., 253., 2*242.,&
       &250., 275., 225., 300., 220., 270., 220., 2*225., 2*250.,&
       &150., 230., 140., 175., 240., 140., 210., 200., 250., 230.,&
       &200., 170., 200., 170., 160., 230., 220., 160., 2*200., 2*180.,&
       &190., 120., 140., 156., 120., 210., 140., 160., 214., 130.,&
       &154., 120., 300., 2*130., 200., 100., 75., 2*100., 110.,&
       &3*150./
  data R4SDI/&
       &529.,423.,570.,562.,682.,762.,679.,620.,602.,446.,621.,&
       &576.,562.,272.,501.,409.,452.,409.,452./

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
     if (TEM .lt. SLOSSP)TEM = SLOSSP

     !  GET THE APPROPRIATE SITE INDEX RANGE VALUES FOR THIS SPECIES.

     SLO = 0.
     SHI = 999.
     call SITERANGE (1,I,SLO,SHI)

     if (SITEAR(I) .le.0.0) SITEAR(I) = SLO +&
          &(TEM-SLOSSP)/(SHISSP-SLOSSP)&
          &*(SHI-SLO)
     10 continue
  end do

  !  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
  !  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
  !  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.

  !  NEZ PERCE (IFOR=1) IS A REGION 1 FOREST. ALL OTHERS FORESTS IN THIS
  !  VARIANT ARE IN REGION 4.

  do ISPC = 1,MAXSP
     if (IFOR.eq.1) then
        if (METHB(ISPC).eq.999)METHB(ISPC) = 6
        if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     else
        if (METHB(ISPC).eq.999)METHB(ISPC) = 6
        if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     end if
     50 continue
  end do

  !  SET SDIDEF AND BAMAX VALUES WHICH HAVE NOT BEEN SET BY KEYWORD.
  !  If a user sets BAMAX, then set the SDI maximums by species with this equation:
  !     SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
  !  If a user hasn�t set BAMAX, then set SDI maximums based on region.
  !  If in R1 (ifor eq 1), then set the SDI maximums by species based on habitat type:
  !          BAMAX=BAMAXA(ICINDX)
  !          SDIDEF(I)=BAMAX/(0.5454154*(PMSDIU/100.))
  !  Or
  !          SDIDEF(I)= BAMAXA(ICINDX)/(0.5454154*(PMSDIU/100.))

  !  If in R4, then set SDI maximums by species based on R4SDI values and
  !     change the calculation method to zeide if not set by the user:
  !         IF(CALCSDI.EQ.' ')LZEIDE = .TRUE.
  !          SDIDEF(I) = R4SDI(I)

  if (CALCSDI.eq.' ' .and. IFOR .lt.2) LZEIDE = .false.
  if (BAMAX .gt.0.0) then
     do I = 1,MAXSP
        if (SDIDEF(I).le.0.)&
             &SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
     end do
  else
     BAMAX = BAMAXA(ICINDX)
     do I = 1,MAXSP
        if (IFOR .lt.2) then
           if (SDIDEF(I).le.0.)&
                &SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
        else
           if (SDIDEF(I).le.0.) SDIDEF(I) = R4SDI(I)
        end if
     end do
  end if

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
  VAR = 'CI'

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
