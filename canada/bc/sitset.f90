!=====================================!
!   Initialize species site indices   !
!=====================================!
! CANADA-BC $Id$
subroutine SITSET
  implicit none

  !  THIS SUBROUTINE IS USED TO SET SIMULATION CONTROLLING VALUES
  !  THAT HAVE NOT BEEN SET USING THE KEYWORDS --- SDIMAX, BAMAX.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'
  include 'BCPLOT.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IFIASP,ERRFLAG
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  logical :: LBAMX
  integer :: I,ISPC,INTFOR,IREGN,J,JJ,K,iSeries

  !     ASSIGN VERSION 2 PARAMETERS (MS,ESSF,PP)
  call BECSET

  LBAMX = .true.
  if (BAMAX .gt.0.) then
     LBAMX = .false.
     goto 40
  end if

  !  SET SDIDEF AND BAMAX VALUES WHICH HAVE NOT BEEN SET BY KEYWORD.

  BAMAX = -1.

  !     SOME ODD SITE SERIES CASES "01-ys, 01a, 01b" NEED SPECIAL ATTENTION
  if (INDEX(BEC%Series,'-') .gt.0) then
     iSeries = 1
  else if (INDEX(BEC%Series,'a') .gt.0) then
     iSeries = 1
  else if (INDEX(BEC%Series,'b') .gt.0) then
     iSeries = 1
  else
     read (BEC%Series,'(I4)') iSeries
  end if

  select case (BEC%Zone)
  case ("IDF") ! IDF: dk differ in V2 - becset.for
     select case (BEC%SubZone)
     case ("dk1")
        select case (iSeries)
        case (1)
           BAMAX = 60.
        case (2,4:7)
           BAMAX = 58.
        case (3)
           BAMAX = 55.
        end select
     case ("dk2")
        select case (iSeries)
        case (1,3:7)
           BAMAX = 89.
        end select
     case ("dk3")
        select case (iSeries)
        case default
           BAMAX = 53.
        end select
     case ("dk4")
        BAMAX = 40. ! needs review
     case ("dm1")
        select case (iSeries)
        case (1,3:7)
           BAMAX = 43.
        end select
     case ("dm2")
        select case (iSeries)
        case (1,3,5:7)
           BAMAX = 53.
        case (4)
           BAMAX = 52.
        end select
     case ("mw1","mw2")
        BAMAX = 47.
     case ("xh1")
        select case (iSeries)
        case (4)
           BAMAX = 32. ! advice from Walt Klenner (July 2021)
        case default
           BAMAX = 48.
        end select
     case ("xh2")
        select case (iSeries)
        case (1)
           BAMAX = 42. ! advice from Walt Klenner (July 2021)
        case default
           BAMAX = 48.
        end select
     case ("xm")
        !          advice from David Rusch Chilcotin (June 2021)
        !          advice from Walt Klenner Kamloops (July 2021)
        !          discord: BAMAX also depends on leading species:
        !          FD on /03 => 25, but PL on /03 => 30; ** using 25 **
        select case (iSeries)
        case (1,5,6,7)
           BAMAX = 30.
        case (2,3,4)
           BAMAX = 25.
        case (8)
           BAMAX = 40.
        end select
     end select

  case ("ICH") ! ICH
     select case (BEC%SubZone)
     case ("mk1")
        select case (iSeries)
        case (1)
           BAMAX = 59.
        case (2)
           BAMAX = 50.
        case (3)
           BAMAX = 46.
        case (4)
           BAMAX = 42.
        case (5:7)
           BAMAX = 50.
        end select
     case ("mw2")
        select case (iSeries)
        case (1:7)
           BAMAX = 89.
        end select
     case ("mw3")
        select case (iSeries)
        case (1)
           BAMAX = 81.
        case (2,3,6,7)
           BAMAX = 76.
        case (4,5)
           BAMAX = 69.
        end select
     case ("wk1")
        select case (iSeries)
        case (1,3:7)
           BAMAX = 67.
        end select
     case ("dw")
        BAMAX = 50.
     end select

  case ("ESSF") ! ESSF
     select case (BEC%SubZone)
     case ("dk")
        select case (iSeries)
        case (1)
           BAMAX = 64.
        case (2)
           BAMAX = 62.
        case (3)
           BAMAX = 61.
        case (4:7)
           BAMAX = 62.
        end select
     case ("wc4","wm")
        BAMAX = 62.
     end select

  case ("MS") ! MS
     select case (BEC%SubZone)
     case ("dk")
        select case (iSeries)
        case (1)
           BAMAX = 48. ! advice for SI 20, Walt Klenner (July 2021), formerly 57
        case (2,3)
           BAMAX = 55.
        case (4)
           BAMAX = 38. ! advice for SI 15, Walt Klenner (July 2021), formerly 46
        case (5:7)
           BAMAX = 55.
        end select
     case ("dm1")
        select case (iSeries)
        case (1,4)
           BAMAX = 63.
        case (2,3,7)
           BAMAX = 62.
        case (5)
           BAMAX = 57.
        case (6)
           BAMAX = 64.
        end select
     end select

  case ("PP") ! PP
     select case (BEC%SubZone)
     case ("dh2")
        select case (iSeries)
        case (1,3:7)
           BAMAX = 32.
        end select
     case ("xh2")
        select case (iSeries)
        case (1:4)
           BAMAX = 32.
        case (6,7)
           BAMAX = 21.
        end select
     end select

  case ("SBS") ! SBS
     BAMAX = 55.
     select case (BEC%SubZone)
     case ("dw1","dw2")
        BAMAX = 55.
     case ("mh")
        BAMAX = 60.
     end select

  case ("SBPS") ! SBPS
     BAMAX = 50.
     select case (BEC%SubZone)
     case ("dc")
        BAMAX = 50.
     case ("mk")
        BAMAX = 55.
     end select
  end select

  if (BAMAX .le.0.) then
     BAMAX = 10.
     call RCDSET(2,.true.)
     WRITE(JOSTND, "(/1X,' ******** WARNING: ',                      &
          &    ' MAXIMUM BASAL AREA UNDEFINED FOR ', A,': SET T0 ', F6.1)")&
          &BEC%prettyname, BAMAX
  end if
  BAMAX = BAMAX * M2pHAtoFT2pACR

  40 if (LBAMX) then
     write(JOSTND, 56) BEC%prettyname, BAMAX * FT2pACRtoM2pHA
     56 format(/1X,'MAXIMUM BASAL AREA FOR ',A,&
          &'IS SET BY DEFAULT TO:', F6.1, ' SQ M/HA.')
  else
     write(JOSTND, 57) BAMAX * FT2pACRtoM2pHA
     57 format(/1X,'MAXIMUM BASAL AREA FOR THIS STAND '&
          &'HAS BEEN CHANGED BY KEYWORD TO:', F6.1, ' SQ M/HA.')
  end if

  do I = 1,MAXSP
     if (SDIDEF(I).le.0.) SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
     10 continue
  end do

  do I = 1,15
     J = (I-1)*10 + 1
     JJ = J+9
     if (JJ.gt.MAXSP)JJ = MAXSP
     write(JOSTND,90)(NSP(K,1)(1:2),K = J,JJ)
     90 format(/' SPECIES ',5X,10(A2,6X))
     write(JOSTND,91)(SDIDEF(K)/ACRtoHA,K = J,JJ )
     91 format(' SDI MAX ',   10F8.0)
     if (JJ .eq. MAXSP)go to 93
     92 continue
  end do
  93 continue

  !     RESOLVE BEC/SITE INFORMATION (VERSION 2)

  call BECSET

  !  SET METHB & METHC DEFAULTS.  DEFAULTS ARE INITIALIZED TO 999 IN
  !  **GRINIT**.  IF THEY HAVE A DIFFERENT VALUE NOW, THEY WERE CHANGED
  !  BY KEYWORD IN INITRE. ONLY CHANGE THOSE NOT SET BY KEYWORD.

  do ISPC = 1,MAXSP
     if (METHB(ISPC).eq.999)METHB(ISPC) = 6
     if (METHC(ISPC).eq.999)METHC(ISPC) = 6
     50 continue
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  INTFOR = KODFOR - (KODFOR/100)*100
  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  IREGN = KODFOR/100
  DIST = '  '
  PROD = '  '
  VAR = VARACD
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
  230 format(4(2X,A2,4X,A10,1X,A10))

  return
end
