!===============================================!
!   Compute diameter distribution percentiles   !
!===============================================!
! BASE $Id$
subroutine DIST(N,ATTR,PCTWK)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE DIAMETERS CORRESPONDING TO THE
  !  90, 70, 50, 30, AND 10 PERCENTILE POINTS IN THE DISTRIBUTION
  !  OF A STAND ATTRIBUTE.  THESE DIAMETERS ARE LOADED INTO THE
  !  FIRST 5 POSITIONS OF THE ARRAY ATTR.  THE SIXTH POSITION IN
  !  ATTR IS LOADED WITH THE DIAMETER OF THE LARGEST TREE REPRESENTED
  !  IN THE ATTRIBUTE.  IF THE VALUE OF IFST IS 1, INS IS LOADED
  !  WITH SUBSCRIPTS TO TREE RECORDS TO BE PRINTED.  N IS THE CURRENT
  !  LENGTH OF THE TREE RECORD LIST (ITRN).

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  !  DIMENSIONS FOR INTERNAL VARIABLES.

  ! Variables
  real    :: ATTR(7),PCTWK(*)
  integer :: N,N1,ITOP,I,J,IBOT,IPTR,MIDPTR,INDTOP
  real    :: PCTAGE

  !  IF THE TREE LIST IS NOT EMPTY, THEN:
  !  BEGIN BINARY SEARCH FOR PERCENTILE POINTS.

  if (ITRN.eq.0) goto 60
  N1 = N+1
  ITOP = 1
  PCTAGE = 90.0
  do I = 1,5
     J = 6-I
     if (ITOP.eq.N) go to 40
     IBOT = N1
     10 IPTR = (IBOT+ITOP)/2
     MIDPTR = IND(IPTR)
     if (PCTWK(MIDPTR).lt.PCTAGE) go to 20
     ITOP = IPTR
     go to 30
     20 IBOT = IPTR
     30 if (ITOP+1.lt.IBOT) go to 10

     !  PERCENTILE POINT HAS BEEN LOCATED.

     40 continue
     INDTOP = IND(ITOP)
     ATTR(J) = DBH(INDTOP)
     if (IFST.eq.1) INS(J) = INDTOP
     PCTAGE = PCTAGE-20.0
     50 continue
  end do

  !  ALL PERCENTILE POINTS HAVE BEEN LOCATED.  LOAD ATTR(6) AND
  !  INS(6) (IF NECESSARY), AND RESET IFST.

  J = IND(1)
  ATTR(6) = DBH(J)
  if (IFST.ne.1) return
  INS(6) = J
  IFST = 99

  !     RESET TRM SO THE TREES/ACRE LISTED FOR EXAMPLE TREES ARE CORRECT.

  TRM = 1.0
  return

  !  ZERO OUT THE DISTRIBUTION STATISTICS IF THE TREE LIST IS EMPTY

  60 continue
  do I = 1,6
     ATTR(I) = 0.
     INS(I) = 0
     70 continue
  end do
  return
end subroutine DIST
