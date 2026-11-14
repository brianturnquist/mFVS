!===================!
!   wpbr : brstar   !
!===================!
! WPBR $Id$
subroutine BRSTAR(HT,STAR)
  implicit none

  !  Purpose:
  !  Calculates the value of the sum of target area for a tree.

  !  Parameters
  !     Passed: HT     - Tree's height for current year in meters
  !                      (current height + proportion of height growth)
  !     Returned: STAR - Current sum of target area for the tree
  !                      (thousands of needles)

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !.... Local variable declarations.

  ! Variables
  real    :: HT,STAR,CFA

  !.... Calculate total needles on crown in thousands at given age (sum
  !.... target area).

  STAR = EXP(2.1717+(1.3633*ALOG(HT))-(0.13758/(HT**2)))+0.02

  if (HT.le.5.0) then

     !....    Calculate correction factor for trees less than 5 meters tall.

     CFA = (0.69-(3.58*ALOG(HT))+(12.3*ALOG(HT**2))+(19.7*ALOG(HT**3))&
          &+(7.76*ALOG(HT**4)))/(1-(4.59*ALOG(HT))+(11.53*ALOG(HT**2))&
          &+(21.03*ALOG(HT**3))+(7.7*ALOG(HT**4))+(0.3*ALOG(HT**5)))

     !....    Apply the correction factor to the needle total.

     STAR = STAR*CFA
  end if

  return
end
