!===================!
!   wpbr : brcini   !
!===================!
! WPBR $Id$
subroutine BRCINI(IBRNO,HITE)
  implicit none

  !  Purpose:
  !  BRCINI uses the canker counts read from the canker data list
  !  (if the CANKDATA keyword is used - there is a required record that
  !  contains the canker count for the tree and has 0's for up, out, and
  !  girdle) to initialize canker and infection conditions for each tree.
  !  This routine randomly generates up, out, and %girdle measurements
  !  for any number of cankers up to 10 or up to the canker count,
  !  whichever is less, minus the number of cankers already loaded from
  !  the actual canker data.

  !  Parameters
  !     IBRNO  - index for current tree
  !     HITE   - current tree height (in meters)

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.
  ! Variables
  integer :: IBRNO,ITEMPC,J,NUMCNK
  real    :: HITE,HITEBC,PBOLE,TOUT,TUP,XRAN,YRAN,ZRAN,CRLEN
  logical :: DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRCINI',6,ICYC)
  if (DEBUG) write(JOSTND,23) ICYC
  23 format('Entering subroutine BRCINI: cycle = ',I2)

  !.... Calculate crown length in centimeters.
  !.... Height to base of crown is already in centimeters but HITE
  !.... is in meters.

  HITEBC = BRHTBC(IBRNO)
  CRLEN = (HITE*100.0)-HITEBC

  !.... Determine the number of cankers to be generated.  Take the total
  !.... count, which has been stored already in ITCAN, subtract the lethal
  !.... count, stored already in ILCAN (in subroutine BRCANK) and then
  !.... add that many cankers, up to 10 total.  Note: ITCAN already has
  !.... the correct total canker count; we may add cankers to ILCAN if
  !.... it's not already full and if ITCAN is greater that ILCAN.

  NUMCNK = ITCAN(IBRNO)-ILCAN(IBRNO)

  !.... Loop through for number of cankers, if any, and stop at 10.

  if (NUMCNK.gt.0) then
     do J = 1,NUMCNK

        !....       Check to see if the lethal canker array is already full.

        if (ILCAN(IBRNO).lt.10) then
           ILCAN(IBRNO) = ILCAN(IBRNO)+1
           ITEMPC = ILCAN(IBRNO)

           !....          Generate up and out positions for added canker.
           !....          Up position can be anywhere on the tree, including
           !....          below the base on the crown, whereas out will only be
           !....          generated for cankers above the base of the crown.

           call BRANN(XRAN)
           TUP = 100*HITE*XRAN
           if (TUP.lt.HITEBC) then
              TOUT = 0.0
           else
              TOUT = (35*SQRT(HITE)*(100*HITE-TUP)/CRLEN)*XRAN
           end if

           !....          If out < 50 then the canker is possibly lethal--different
           !....          probability than if the canker is farther out than 50 cm.
           !....          This stuff came from BRECAN.

           if (TOUT.lt.50.0) then
              PBOLE = 0.97-0.0158*TOUT
           else
              PBOLE = 35.4/TOUT**(1+(0.35*TOUT/50))
           end if
           if (PBOLE.lt.0.0) PBOLE = 0.0

           !....          Call random number generator. If PBOLE >= number then
           !....          create a bole canker, otherwise create a branch canker.

           call BRANN(YRAN)
           if (PBOLE.ge.YRAN) then

              !....             Bole canker.

              DOUT(ITEMPC,IBRNO) = 0.0

              !....             Call random number generator again and generate
              !....             a % girdle measurement.  Don't want to kill trees
              !....             in the first cycle so make sure girdling is never
              !....             more than 50%.

              call BRANN(ZRAN)
              GIRDL(ITEMPC,IBRNO) = ZRAN*50.0
           else

              !....             Branch canker.

              DOUT(ITEMPC,IBRNO) = TOUT
              GIRDL(ITEMPC,IBRNO) = 0.0
           end if

           !....          Set distance up and canker status.  Canker status will
           !....          be reset in routine BRCSTA.

           DUP(ITEMPC,IBRNO) = TUP
           ISTCAN(ITEMPC,IBRNO) = 0
        end if
        300 continue
     end do
  end if

  !.... Common return.

  if (DEBUG) write(JOSTND,330) ICYC
  330 format('Leaving subroutine BRCINI: cycle = ',I2)
  return
end
