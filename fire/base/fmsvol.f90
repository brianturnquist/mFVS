!========================!
!   fire/base : fmsvol   !
!========================!
! FIRE-BASE $Id$
subroutine FMSVOL (II, XHT, VOL2HT, DEBUG, IOUT)
  implicit none

  !     CALLED FROM: FMSOUT
  !                  FMDOUT
  !                  FMHIDE
  !                  FMCWD
  !                  FMSALV

  !     CALLS:       CFVOL
  !                  NATCRS
  !                  OCFVOL
  !                  CFTOPK

  !  PURPOSE:
  !     Calculates the volume up to height HT of each snag in record II.

  !  CALL LIST DEFINITIONS:
  !     II       SNAG RECORD NUMBER
  !     XHT      HEIGHT UP TO WHICH VOLUME SHOULD BE CALCULATED
  !     VOL2HT   VOLUME UP TO HEIGHT HT OF EACH SNAG IN RECORD II
  !     MVOL     MERCH VOLUMNE UP TO HEIGHT

  !  LOCAL VARIABLE DEFINITIONS:
  !     MCF = MERCH. VOLUME
  !     JS = SNAG SPECIES
  !     IHT = AN INDEX OF HT

  !  COMMON BLOCK VARIABLES AND PARAMETERS:

  !.... PARAMETER STATEMENTS.

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  include 'FMCOM.f90'
  include 'CONTRL.f90'

  !.... VARIABLE DECLARATIONS.

  ! Variables
  real    :: MCF,VMAX,BFMAX,TCF,SCF
  logical :: LTKIL,LC,LCONE,CTKFLG,BTKFLG,DEBUG
  logical :: LMERCHIN,LMERCH
  integer :: JS,ISPC,IT
  real    :: D,H,BARK,XHT,VOL2HT
  integer :: IOUT,II,JSP,IHT
  integer :: CRWNRTO,CWN,DCY,WSTM
  real    :: XH,XD,BRATIO,D2H,BBFV,X,CL,BIODRY(15)
  character(len=1) :: LIVEDEAD
  character(len=1) :: LVD

  !     CALCULATE THE VOLUME

  JS = SPS(II)
  D = DBHS(II)
  H = HTDEAD(II)
  LMERCH = .false.
  CWN = 0
  CL = 0
  DCY = 0
  WSTM = 0
  if (LFIANVB) then
     LVD = 'D'
  else
     LVD = ' '
  end if

  goto 1000

  !     ENTRY POINT FOR SNAGS CREATED BY **CUTS**.

  entry FMSVL2(JSP,XD,XH,XHT,VOL2HT,CRWNRTO,&
       &LIVEDEAD,LMERCHIN,DEBUG,IOUT)

  JS = JSP
  D = XD
  H = XH
  LMERCH = LMERCHIN

  CWN = CRWNRTO
  CL = 0
  DCY = 0
  WSTM = 0
  BIODRY = 0
  if (LFIANVB) then
     LVD = LIVEDEAD
  else
     LVD = ' '
  end if

  1000 continue

  LC = .false.
  if (XHT .gt. -1) then
     LTKIL = .true.
  else
     LTKIL = .false.
     XHT = H
  end if

  BARK = BRATIO(JS,D,H)
  D2H = D * D * H
  IHT = INT(XHT * 100.0)

  !      This call was replaced by the code below (NATCRS, etc.).
  !      CALL CFVOL (JS,D,H,D2H,VOL2HT,VM,VMAX,LTKIL,LC,BARK,IHT,LDUM)

  !     Actually do the call to calculate the volumes. Note that this
  !     section of the code may have to change if there are any changes
  !     made within the base model routine VOLS or CFVOL
  !     Also note the following variable equivalencies:
  !     ITRUNC(I) = IHT
  !     TKILL = LTKIL

  ISPC = JS
  LCONE = LC
  IT = 0
  CTKFLG = LTKIL
  BTKFLG = .false.

  if (METHC(ISPC).eq.6.or. METHC(ISPC).eq.10) then
     call NATCRS (TCF,MCF,SCF,BBFV,ISPC,D,H,LTKIL,&
          &CWN,BARK,IHT,VMAX,BFMAX,&
          &CL,DCY,WSTM,BIODRY,&
          &LVD,CTKFLG,BTKFLG,-1)
  else if ((METHC(ISPC).eq.8).or.(METHC(ISPC).eq.5)) then
     call OCFVOL (TCF,MCF,ISPC,D,H,LTKIL,BARK,IHT,VMAX,LCONE,&
          &CTKFLG,IT)
  else
     call CFVOL (ISPC,D,H,D2H,TCF,MCF,VMAX,LTKIL,LCONE,BARK,IHT,&
          &CTKFLG)
  end if
  if (CTKFLG .and. LTKIL)&
       &call CFTOPK (ISPC,D,H,TCF,MCF,SCF,VMAX,LCONE,BARK,IHT)

  !     Give some small volume to very tiny trees.
  !     based on cone with D = 1 inch
  X = 0.005454154 * H

  if (VARACD .eq. 'CS' .or. VARACD .eq. 'LS'&
       &.or. VARACD .eq. 'NE' .or. VARACD .eq. 'SN') then
     VOL2HT = MAX(X,MCF)
     if (LMERCH) VOL2HT = SCF
  else
     VOL2HT = MAX(X,TCF)
     if (LMERCH) VOL2HT = MCF
  end if

  if (DEBUG) write(IOUT,40)ISPC,D,H,LCONE,TCF,VOL2HT
  40 format(' FMSVOL ISPC=',I3,' D=',F7.3,' H=',F7.3,&
       &' LCONE=',L2,' VN=',F7.3,' VOL2HT=',F10.3)

  return
end
