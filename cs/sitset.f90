!=====================================!
!   Initialize species site indices   !
!=====================================!
! CS $Id$
subroutine SITSET
  implicit none

  ! THIS SUBROUTINE LOADS THE SITELG ARRAY WITH A SITE INDEX FOR EACH
  ! SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  ! Variables
  logical :: DEBUG
  real    :: SDICON(MAXSP),ASITE(MAXSP),BSITE(MAXSP)
  integer :: I,J,JJ,K
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  integer :: IFIASP,ERRFLAG,ISPC,IREGN,KFORST

  !  LOAD SDI MAXIMUM VALUES

  data SDICON /&
       &354., 354., 490., 499., 480., 354., 529., 283., 283., 430.,&
       &704., 726., 430., 302., 302., 302., 230., 276., 302., 301.,&
       &301., 338., 289., 364., 423., 408., 408., 648., 421., 344.,&
       &590., 384., 282., 339., 420., 263., 282., 282., 227., 282.,&
       &478., 526., 371., 414., 408., 414., 361., 414., 342., 370.,&
       &315., 326., 336., 361., 423., 417., 311., 311., 417., 455.,&
       &405., 370., 384., 365., 342., 315., 342., 492., 492., 371.,&
       &492., 147., 338., 384., 520., 562., 343., 499., 499., 692.,&
       &400., 430., 447., 447., 257., 375., 422., 257., 463., 343.,&
       &404., 415., 492., 277., 304., 164./

  ! LOAD SITE INDEX COEFF

  data ASITE /&
       &0,      0,-5.1489,      0,      0,&
       &0,      0,      0,      0,      0,&
       &0,      0,      0,      0,      0,&
       &0,      0,      0,      0,      0,&
       &0,      0,      0,  18.19,-35.098,&
       &-35.098,-35.098,      0,      0,      0,&
       &0,      0,-26.067,-26.067,-26.067,&
       &-26.067,-26.067,-26.067,-26.067,-26.067,&
       &-7.01,  18.19,      0,-36.805,-36.805,&
       &-36.805,      0, -1.334, -1.334, -2.706,&
       &1.097,      0,      0,      0,      0,&
       &0,      0,      0,  2.656,      0,&
       &0,      0,      0,      0,      0,&
       &0,      0,      0,      0,      0,&
       &0,      0,      0,  3.543,  3.543,&
       &3.543,      0,      0,      0,      0,&
       &7.521,  7.521,  7.521,  7.521,      0,&
       &0,      0,      0,      0,      0,&
       &0,      0,      0,      0,      0,&
       &0/
  data BSITE /&
       &0.8,    0.8,  1.062,   1.25,   1.25,&
       &1.25,   1.35,   1.19,   1.19,   1.15,&
       &1.15,   1.15,   1.15,   1.08,   1.08,&
       &1.08,   1.08,   1.08,    1.1,    1.1,&
       &1.1,    1.1,    1.1, 0.7695,  1.729,&
       &1.729,  1.729,    1.3,   1.32,   1.32,&
       &1.32,   1.25,1.51754,1.51754,1.51754,&
       &1.51754,1.51754,1.51754,1.51754,1.51754,&
       &1.203, 0.7695,   1.22, 1.6748, 1.6748,&
       &1.6748,      1,  1.082,  1.082,  1.106,&
       &1.063,   0.96,   0.89,   0.89,   0.89,&
       &0.89,   0.83,   0.83,  0.965,    1.2,&
       &1.2,    1.2,    1.2,    1.2,    1.2,&
       &1.2,    1.2,   1.35,   1.35,   1.35,&
       &1.35,   1.35,   1.35, 1.2407, 1.2407,&
       &1.2407,    1.2,   1.35,   1.35,   1.35,&
       &1.074,  1.074,  1.074,  1.074,    0.8,&
       &0.8,    0.8,    0.8,    0.8,    0.9,&
       &0.9,    0.9,    0.9,    0.9,    0.9,&
       &0.9/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)

  !  DEFAULT MERCH LIMITS ARE SET IN LS/VOLS

  !  SET DEFAULT SITE SPECIES OF WHITE OAK WITH INDEX OF 65
  !  IF MISSING

  if (ISISP .le.0) ISISP = 47
  if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 65.

  !  BEGIN BY CALCULATING WO SI BASED ON SITE SPECIES

  if (ISISP .ne.47) SITEAR(47) = (-1*ASITE(ISISP)/BSITE(ISISP))&
       &+((1/BSITE(ISISP))*SITEAR(ISISP))

  !  SET SITE INDEX FOR ALL SPECIES

  do I = 1,MAXSP
     if (SITEAR(I) .lt..0001) SITEAR(I) = ASITE(I)&
          &+BSITE(I)*(SITEAR(47))
     5 continue
  end do

  !  SET SDIDEF VALUES IF NOT SET BY KEYWORD.

  do I = 1,MAXSP
     if (SDIDEF(I) .le.0.) then
        if (BAMAX .gt.0.) then
           SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
        else
           SDIDEF(I) = SDICON(I)
        end if
     end if
     15 continue
  end do

  !  LOAD VOLUME DEFAULT MERCH. SPECS.

  do ISPC = 1,MAXSP
     if (DBHMIN(ISPC).le.0.) then !SET **DBHMIN** DEFAULT
        if (ISPC.le.7) then !SOFTWOODS
           DBHMIN(ISPC) = 5.
        else !HARDWOODS
           select case(IFOR)
           case(1)
              DBHMIN(ISPC) = 5.
           case default
              DBHMIN(ISPC) = 6.
           end select
        end if
     end if
     if (TOPD(ISPC).le.0.) then !SET **TOPD** DEFAULT
        if (ISPC.le.7) then !SOFTWOODS
           TOPD(ISPC) = 4.
        else !HARDWOODS
           select case (IFOR)
           case(2)
              TOPD(ISPC) = 5.
           case default
              TOPD(ISPC) = 4.
           end select
        end if
     end if
     if (BFMIND(ISPC).le.0.) then !SET **BFMIND** DEFAULT
        if (ISPC.le.7) then !SOFTWOODS
           select case(IFOR)
           case(1)
              BFMIND(ISPC) = 9.
              if (ISPC.eq.1)BFMIND(ISPC) = 6.
           case default
              BFMIND(ISPC) = 9.
           end select
        else !HARDWOODS
           select case(IFOR)
           case(1)
              BFMIND(ISPC) = 9.
           case default
              BFMIND(ISPC) = 11.
           end select
        end if
     end if
     if (BFTOPD(ISPC).le.0.) then !SET **BFTOPD** DEFAULT
        if (ISPC.le.7) then !SOFTWOOD
           select case(IFOR)
           case(1)
              BFTOPD(ISPC) = 7.6
              if (ISPC.eq.1)BFTOPD(ISPC) = 5.
           case default
              BFTOPD(ISPC) = 7.6
           end select
        else !HARDWOODS
           select case(IFOR)
           case(1)
              BFTOPD(ISPC) = 7.6
           case default
              BFTOPD(ISPC) = 9.6
           end select
        end if
     end if
     if (SCFMIND(ISPC).le.0.) then !SET **SCFMIND** DEFAULT
        if (ISPC.le.7) then !SOFTWOODS
           select case(IFOR)
           case(1)
              SCFMIND(ISPC) = 9.
              if (ISPC.eq.1)SCFMIND(ISPC) = 6.
           case default
              SCFMIND(ISPC) = 9.
           end select
        else !HARDWOODS
           select case(IFOR)
           case(1)
              SCFMIND(ISPC) = 9.
           case default
              SCFMIND(ISPC) = 11.
           end select
        end if
     end if
     if (SCFTOPD(ISPC).le.0.) then !SET **SCFTOPD** DEFAULT
        if (ISPC.le.7) then !SOFTWOOD
           select case(IFOR)
           case(1)
              SCFTOPD(ISPC) = 7.6
              if (ISPC.eq.1)SCFTOPD(ISPC) = 5.
           case default
              SCFTOPD(ISPC) = 7.6
           end select
        else !HARDWOODS
           select case(IFOR)
           case(1)
              SCFTOPD(ISPC) = 7.6
           case default
              SCFTOPD(ISPC) = 9.6
           end select
        end if
     end if
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  KFORST = KODFOR-900
  IREGN = 9
  write(FORST,'(I2)')KFORST
  if (KFORST.lt.10)FORST(1:1) = '0'
  DIST = '  '
  PROD = '  '
  VAR = 'CS'

  if (LFIANVB) call NVB_REGION_CHECK
  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     VOLEQ = '           '
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9).or.&
          &(METHC(ISPC).eq.5)).and.(VEQNNC(ISPC).eq.'           ')) then
        if (METHC(ISPC).eq.5) then
           VOLEQ(1:7) = '900DVEE'
        else
           VOLEQ(1:7) = '900CLKE'
        end if
        PROD = '02'
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ
        !      WRITE(16,*)'PROD,IFIASP,ISPC,VEQNNC(ISPC)= ',PROD,IFIASP,ISPC,
        !     &VEQNNC(ISPC)
     else if (METHC(ISPC).eq.10) then
        call NVBEQDEF(IFIASP, VOLEQ)
        VEQNNC(ISPC) = VOLEQ
     end if
     if (VEQNNC(ISPC)(11:11) .eq. CHAR(0)) VEQNNC(ISPC)(11:11) = " "
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9).or.&
          &(METHB(ISPC).eq.5)).and.(VEQNNB(ISPC).eq.'           ')) then
        VOLEQ = '           '
        if (METHB(ISPC).eq.5) then
           VOLEQ(1:7) = '900DVEE'
        else
           VOLEQ(1:7) = '900CLKE'
        end if
        PROD = '01'
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC) = VOLEQ
     end if
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

  !  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE

  if (LFIA) then
     call FIAHEAD(JOSTND)
     write(JOSTND,211) (NSP(I,1)(1:2),FIAJSP(I),I = 1,MAXSP)
     211 format ((T12,8(A3,'=',A6,:,'; '),A,'=',A6))
  end if

  !  WRITE VOLUME EQUATION NUMBER TABLE

  call VOLEQHEAD(JOSTND)
  write(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),&
       &VEQNNB(J),J = 1,MAXSP)
  230 format(4(2X,A2,4X,A11,1X,A10,1X))

  return
end
