!====================!
!   lpmpb : mpbdam   !
!====================!
! LPMPB $Id$
subroutine MPBDAM (II,ICODES)
  implicit none

  !     PROCESS THE MPB DAMAGE CODES.

  ! Revision History
  !   11/06/89 -
  !   07/10/07 - Lance R. David (FHTET)
  !     Time at which damage codes are processed is now at the end of
  !     keyword processing instead of during the reading of tree data.
  !     So, tree data items that were passed as arguments are now
  !     available from the FVS common area. Original arguments were:
  !     (II,IDTR,ICODES,IITH,IREC2,IISPI,LDELTR)
  !     FVS array IMC(II) is used as replacement for input tree history
  !     code (IITH).
  !     No need for special handling of dead tree index (IREC2) because
  !     dead trees are already at the end of the arrays. Argument II is
  !     correct index value for both live and dead.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'MPBCOM.f90'

  ! Variables
  integer :: I,II,ICODES(6)

  !     IF THE DAMAGE CODE IS 2, THERE IS MPB DAMAGE...IF THE
  !     CORRESPONDING SEVERITY CODE IS 3, SUCCESSFUL ATTACK, THEN
  !     SAVE THE TREE NUMBER IN A LIST OF NUMBERS.

  do I = 1,5,2
     if (ICODES(I) .eq.2) then
        if (ICODES(I+1) .eq.3) then
           NDAMS = NDAMS + 1

           !              IF THE TREE IS RECENT MORTALITY (DIED WITH MORTALITY
           !              OBSERVATION PERIOD, IMC (MANAGEMENT CODE) 7. IMC SET
           !              BASED ON TREE HISTORY CODE OF 6 OR 7 DURING READING
           !              OF INPUT TREE DATA IN SUBROUTINE INTREE.

           !               IF (IMC(II) .EQ. 7) THEN
           IPT(NDAMS) = II
           !               ENDIF
           goto 20
        end if
     end if
     10 continue
  end do

  20 continue
  !     write (16,*) 'IN MPBDAM: II= ',II,' IPT(',NDAMS,')= ',IPT(NDAMS)
  return
end
