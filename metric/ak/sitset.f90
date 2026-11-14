!=====================================!
!   Initialize species site indices   !
!=====================================!
! METRIC-AK $Id$
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
  include 'METRIC.f90'

  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG,I,INTFOR,IREGN,ISPC,J,JJ,K
  real    :: SDICON(13),TEM

  data SDICON /750.,750.,750.,750.,750.,750.,&
       &750.,750.,750.,750.,750.,750.,750./

  ! IF SITEAR(I) HAS NOT BEEN SET WITH SITECODE KEYWORD, LOAD IT
  ! WITH DEFAULT SITE VALUES. CONVERT BETWEEN SS & WH; LP & AF
  ! GET .8 * SS; ALL OTHERS (EXCEPT RA & CW) GET WH.
  ! RED ALDER & COTTONWOOD GET TRANLATED FROM WH
  ! RED ALDER USES TOTAL AGE 20 HARRINGTON & CURTIS 1986
  ! COTTONWOOD USES BREAST AGE 100 CURTIS ET.AL. 1974

  TEM = 80.
  if (ISISP .gt.0.and. SITEAR(ISISP) .gt.0.0) then
     TEM = SITEAR(ISISP)
     if (ISISP .eq.8) TEM = 24.41 + 0.674 * SITEAR(8)
     if (ISISP .eq.7.or. ISISP .eq.9) then
        if (SITEAR(8).eq.0.0) SITEAR(8) = SITEAR(ISISP)/0.8
        TEM = 24.41 + 0.674 * SITEAR(8)
     end if
     if (ISISP .eq.10) TEM = (SITEAR(10)+5.8812)/0.5294
     if (ISISP .eq.11) TEM = (SITEAR(11)-18.3112)/1.2692
  end if
  if (SITEAR(8).eq.0.)SITEAR(8) = -6.62+1.152*TEM
  do I = 1,MAXSP
     if (I.eq.10.or. I.eq.11) go to 10
     if (I.eq.7.and. SITEAR(7).eq.0.0)SITEAR(7) = 0.8*SITEAR(8)
     if (I.eq.9.and. SITEAR(9).eq.0.0)SITEAR(9) = 0.8*SITEAR(8)
     if (SITEAR(I) .eq.0.0) SITEAR(I) = TEM
     10 continue
  end do
  if (ISISP .eq.0) ISISP = 5
  if (SITEAR(10) .eq.0.) SITEAR(10) = -5.8812+0.5294*SITEAR(5)
  if (SITEAR(11) .eq.0.) SITEAR(11) = 18.3112+1.2692*SITEAR(5)

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
     write(JOSTND,91)(SDIDEF(K)/ACRtoHA,K = J,JJ )
     91 format('SDI MAX ',   10F8.0)
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
  VAR = 'AK'

  !    R7 (CODE=701) BRITISH COLUMBIA / MAKAH INDIAN RESERVATION
  !    USES THE TONGAS VOLUME EQUATIONS

  if (IREGN.eq.7) then
     IREGN = 10
     FORST = '03'
  end if

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
