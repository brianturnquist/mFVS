!==================!
!   dfb : dfbmod   !
!==================!
! DFB $Id$
subroutine DFBMOD
  implicit none

  !  THIS ROUTINE CALCULATES THE PROJECTED NUMBER OF DOUGLAS-FIR TREES TO
  !  KILL IN THE CURRENT DFB OUTBREAK.

  !  CALLED BY :
  !     DFBDRV  [DFB]

  !  CALLS :
  !     BACHLO (FUNCTION)   [PROGNOSIS]
  !     DFBRAN (FUNCTION)   [DFB]        - CALLED THROUGH BACHLO.

  !  LOCAL VARIABLES :
  !     NUMYRS - LENGTH OF OUTBREAK.
  !     DFBRAN - NAME OF RANDOM NUMBER GENERATOR ROUTINE CALLED BY BACHLO.

  !  COMMON BLOCK VARIABLES USED :
  !     BA9    - (DFBCOM)  INPUT
  !     BADF9  - (DFBCOM)  INPUT
  !     DEBUIN - (DFBCOM)  INPUT
  !     DFKILL - (DFBCOM)  OUTPUT
  !     EXPCTD - (DFBCOM)  INPUT
  !     EXSTDV - (DFBCOM)  INPUT
  !     ICYC   - (CONTRL)  INPUT
  !     IFINT  - (PL0T)    INPUT
  !     ILENTH - (DFBCOM)  INPUT
  !     IYOUT  - (DFBCOM)  INPUT
  !     JODFB  - (DFBCOM)  INPUT
  !     LINPRG - (DFBCOM)  INPUT
  !     OKILL  - (DFBCOM)  INPUT
  !     PERDD  - (DFBCOM)  INPUT
  !     PREKLL - (DFBCOM)  INPUT
  !     START  - (DFBCOM)  INPUT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DFBCOM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: NUMYRS
  real    :: BACHLO
  external DFBRAN

  !     SET NUMYRS TO THE LENGTH OF OUTBREAK IN YEARS.

  if (ILENTH .gt. IFINT) then
     NUMYRS = IFINT
  else
     NUMYRS = ILENTH
  end if

  if (NUMYRS .gt.10)  NUMYRS = 10

  if (LINPRG .and. ICYC .eq.1) then

     !        OUTBREAK IN PROGRESS. USE USER ENTERED VALUES.

     NUMYRS = NUMYRS - IYOUT
     if (IYOUT .gt.4.0) then

        !           OUTBREAK TO LONG TO USE THIS FUNCTION.  OUTBREAK ASSUMED TO
        !           BE OVER.

        DFKILL = 0.0
     else if (PREKLL .le.0.0) then

        !           USER DID NOT ENTER ANY MORTALITY VALUES.  CALCULATE
        !           HOW MANY TREES SHOULD BE KILLED TO FINISH A 4 YEAR OUTBREAK.
        !           IF DFKILL IS LESS THEN ZERO THEN RECALCULATE WITH NEW RANDOM
        !           NUMBER.

        100 continue
        DFKILL = BACHLO(EXPCTD,EXSTDV,DFBRAN) * (BADF9 / BA9) *&
             &4.0 * (1.0 - PERDD(IYOUT))
        if (DFKILL .lt.0.0) goto 100

     else

        !           USER ENTERED MORTALITY VALUES.
        !           CALCULATE HOW MANY MORE TO KILL.

        DFKILL = PREKLL / PERDD(IYOUT) - PREKLL
     end if
  else

     !        CALCULATE NUMBER OF DF TO KILL IF NEW OUTBREAK.
     !        IF DFKILL IS LESS THEN ZERO THEN RECALCULATE WITH NEW RANDOM
     !        NUMBER.

     200 continue
     DFKILL = BACHLO(EXPCTD,EXSTDV,DFBRAN) * (BADF9 / BA9) *&
          &NUMYRS + OKILL
     if (DFKILL .lt.0.0) goto 200
  end if

  400 continue

  if (DEBUIN) write (JODFB,*) ' ** LEAVING SUBROUTINE DFBMOD'

  return
end
