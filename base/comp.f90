!================================================!
!   Compute species and tree class composition   !
!================================================!
! BASE $Id$
subroutine COMP(CMP,ICMP,ATTR)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PERCENTAGE OF A GIVEN STAND
  !  ATTRIBUTE IN EACH SPECIES-TREE CLASS.  IT THEN LOADS CMP
  !  WITH THE FOUR LARGEST CLASSES AND LOADS ICMP WITH
  !  CORRESPONDING SPECIES-TREE CLASS LABELS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: MXSP3
  parameter (MXSP3 = MAXSP*3)
  integer :: J,I1,I2,I,ISPC,JMC,K
  real    :: WORK1(MXSP3),WORK2(MXSP3),ATTR(MAXSP,3),CMP(4),TTT

  !  TYPE DECLARATIONS FOR INTERNAL VARIABLES.

  character(len=3) :: ICMP(4)
  character(len=3) :: NS
  integer :: IDS(MXSP3)

  ! Data statements and initializations
  data NS/'---'/
  if (ITRN.le.0) goto 30
  J = 0
  do I1 = 1,MAXSP
     do I2 = 1,3
        J = J+1
        WORK1(J) = ATTR(I1,I2)
     end do
  end do

  !  SORT THE ATTRIBUTE ARRAY IN DESCENDING ORDER.

  call RDPSRT(MXSP3,WORK1,IDS,.true.)

  !  COMPUTE THE PERCENTAGE OF THE ATTRIBUTE IN EACH SPECIES-TREE
  !  CLASS.

  call PCTILE(MXSP3,IDS,WORK1,WORK2,TTT)

  !  FIND INDEX FOR SPECIES-TREE CLASS AND LOAD CMP AND ICMP.
  !  ISPC REFERENCES SPECIES, AND JMC REFERENCES TREE CLASS.

  do I = 1,4
     J = IDS(I)
     ISPC = (J+2)/3
     JMC = MOD(J,3)
     if (JMC.eq.0) JMC = 3
     K = IDS(I+1)
     CMP(I) = WORK2(J)-WORK2(K)
     ICMP(I) = NSP(ISPC,JMC)(1:3)
     if (CMP(I).le.0.0) ICMP(I) = NS
     20 continue
  end do
  return
  30 continue

  !  BRANCH HERE IF THERE ARE NO TREE RECORDS

  do I = 1,4
     CMP(I) = 0.
     ICMP(I) = NS
     40 continue
  end do
  return
end subroutine COMP
