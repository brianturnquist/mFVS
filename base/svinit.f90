!=================================!
!   Stand visualization: SVINIT   !
!=================================!
! BASE $Id$
subroutine SVINIT
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     D. ROBINSON   -- ESSA        -- MAY 2005

  !     INITIALIZE VISUALIZATION VARIABLES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'SVDATA.f90'
  include 'SVDEAD.f90'

  ! Variables
  integer :: I
  JSVOUT = 0
  JSVPIC = 91
  ICOLIDX = 2
  IGRID = 100
  IPLGEM = 1
  IMORTCNT = 0
  IRPOLES = 0
  IDPLOTS = 0
  IMETRIC = 0

  !     LOAD SNAG RATES FOR DECAY, AND HEIGHT LOSS.
  !     THESE COEFFICIENTS ARE THE NI-FFE DEFAULTS. THEN
  !     WILL NOT BE VALID FOR MOST OTHER VARIANTS - DR/ESSA

  !     04/10/08: The remaining decay coefficients below are
  !     currently used just when estimating snag height at time
  !     of death (in SVSNAD) for snags input via the treelist.
  !     When the FFE and SVSNAD logic are reconciled for incoming
  !     snags, the following coefficients can likely be eliminated.

  do I = 1,MAXSP
     if (I.ge.1 .and. I.le.3) then
        YHFHTS(I) = 33
        YHFHTH(I) = 16
        HRATE(I) = 0.9
     else if (I.ge.4 .and. I.le.9) then
        YHFHTS(I) = 27
        YHFHTH(I) = 13
        HRATE(I) = 1.1
     else
        YHFHTS(I) = 30
        YHFHTH(I) = 14
        HRATE(I) = 1.0
     end if
  end do

  return
end subroutine SVINIT
