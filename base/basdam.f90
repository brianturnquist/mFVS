!==========================================!
!   Process basal area and defect damage   !
!==========================================!
! BASE $Id$
subroutine BASDAM(ITREE,ICODES)
  implicit none

  !  SUBROUTINE TO PROCESS THE SPECIAL TREE STATUS CODES, AND
  !  PERCENT DEFECT CODES.

  !     ITREE  = TREE POINTER FOR LIVE TREES.
  !     ICODES = DAMAGE CODES ARRAY.
  !              25 = PERCENT DEFECT CODE FOR BOTH MERCH CF AND BF
  !              26 = PERCENT DEFECT CODE FOR MERCH CUBIC FOOT VOLUME
  !              27 = PERCENT DEFECT CODE FOR MERCH BOARD FOOT VOLUME
  !              28 = SITE TREE CODE; PROCESSED IN INTREE
  !              55 = SPECIAL STATUS CODE

  !    DEFECT() VALUES ARE CODED 11223344, WHERE
  !       11 = INPUT MC DEFECT PERCENT
  !       22 = INPUT BF DEFECT PERCENT
  !       33 = MC DEFECT % USED FOR A GIVEN CYCLE
  !       44 = BF DEFECT % USED FOR A GIVEN CYCLE

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'

  ! Variables
  integer :: ICODES(6),ITREE,J,ITEMP

  !  INITIALIZE BASE MODEL DEFECT AND SPECIAL STATUS VECTORS.

  ISPECL(ITREE) = 0
  DEFECT(ITREE) = 0

  !  PROCESS CODES.

  do J = 1,5,2
     if (ICODES(J).eq.25 .or. ICODES(J).eq.26) then
        ITEMP = ICODES(J+1)
        if (ITEMP .gt. 99) ITEMP = 99
        if (ITEMP .lt.  0) ITEMP = 0
        DEFECT(ITREE) = DEFECT(ITREE) + ITEMP*1000000
     end if
     if (ICODES(J).eq.25 .or. ICODES(J).eq.27) then
        ITEMP = ICODES(J+1)
        if (ITEMP .gt. 99) ITEMP = 99
        if (ITEMP .lt.  0) ITEMP = 0
        DEFECT(ITREE) = DEFECT(ITREE) + ITEMP*10000
     end if
     if (ICODES(J).eq.55) then
        ISPECL(ITREE) = ICODES(J+1)
        if (ISPECL(ITREE).gt.99) ISPECL(ITREE) = 99
        if (ISPECL(ITREE).lt.0) ISPECL(ITREE) = 0
     end if
     100 continue
  end do
  return
end subroutine BASDAM
