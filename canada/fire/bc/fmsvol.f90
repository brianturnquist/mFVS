!=============================!
!   canada/fire/bc : fmsvol   !
!=============================!
! CANADA-FIRE-BC $Id$
subroutine FMSVOL (II, XHT, VOL2HT, DEBUG, IOUT)
  implicit none

  !     CALLED FROM: FMSOUT
  !                  FMDOUT
  !                  FMHIDE
  !                  FMCWD
  !                  FMSALV

  !     CALLS:       CFVOL
  !                  CFTOPK

  !  PURPOSE:
  !     Calculates the volume up to height HT of each snag in record II.

  !  CALL LIST DEFINITIONS:
  !     II       SNAG RECORD NUMBER
  !     XHT      HEIGHT UP TO WHICH VOLUME SHOULD BE CALCULATED
  !     VOL2HT   VOLUME UP TO HEIGHT HT OF EACH SNAG IN RECORD II
  !     MVOL     MERCH VOLUME UP TO HEIGHT

  !  LOCAL VARIABLE DEFINITIONS:
  !     VM = MERCH. VOLUME
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
  real    :: VM,VMAX,VN
  logical :: LTKIL,LC,LCONE,CTKFLG,DEBUG
  logical :: LMERCHIN,LMERCH
  integer :: JS,ISPC,IT
  real    :: D,H,BARK,XHT,VOL2HT
  integer :: IOUT,II,JSP,IHT
  real    :: XH,XD,BRATIO,D2H,X

  !     CALCULATE THE VOLUME

  JS = SPS(II)
  D = DBHS(II)
  H = HTDEAD(II)
  LMERCH = .false.

  goto 1000

  !     ENTRY POINT FOR SNAGS CREATED BY **CUTS**.

  entry FMSVL2(JSP,XD,XH,XHT,VOL2HT,LMERCHIN,DEBUG,IOUT)

  JS = JSP
  D = XD
  H = XH
  LMERCH = LMERCHIN

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
  !      TKILL = LTKIL

  ISPC = JS
  LCONE = LC
  IT = 0
  CTKFLG = LTKIL

  !     BC VARIANT DOES NOT RECOGNIZE METHC=6 OR 8, SO JUST USE
  !        THE CALL TO CFVOL.
  !           NOTE: THIS CALL TO CFVOL DOES NOT SET VMAX, SO IF
  !                 WE NEED IT, THEN WE WILL JUST SET IT TO VN
  !     IF(METHC(ISPC).EQ.6) THEN
  !         CALL NATCRS (VN,VM,BBFV,ISPC,D,H,LTKIL,BARK,IHT,VMAX,
  !    1        CTKFLG,BTKFLG,-1)
  !     ELSEIF ((METHC(ISPC).EQ.8).OR.(METHC(ISPC).EQ.5)) THEN
  !         CALL OCFVOL (VN,VM,ISPC,D,H,LTKIL,BARK,IHT,VMAX,LCONE,
  !    1        CTKFLG,IT)
  !     ELSE
  call CFVOL (ISPC,D,H,D2H,VN,VM,VMAX,LTKIL,LCONE,BARK,IHT,&
       &CTKFLG)
  if (CTKFLG .and. LTKIL) then
     VMAX = VN
     call CFTOPK (ISPC,D,H,VN,VM,VMAX,LCONE,BARK,IHT)
  end if

  !     Give some small volume to very tiny trees.
  !     based on cone with D = 1 inch
  X = 0.005454154 * H
  VOL2HT = VN
  if (LMERCH) VOL2HT = VM

  if (DEBUG) write(IOUT,40)ISPC,D,H,LCONE,VN
  40 format(' FMSVOL ISPC=',I3,' D=',F7.3,' H=',F7.3,&
       &' LCONE=',L2,' VN=',F7.3)

  return
end
