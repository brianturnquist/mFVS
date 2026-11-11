!================!
!   rd : rddst   !
!================!
! RD $Id$
subroutine RDDST(N,RATTR,PCTWK,ODBH,INDX)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE DIAMETERS CORRESPONDING TO THE
  !  90, 70, 50, 30, AND 10 PERCENTILE POINTS IN THE DISTRIBUTION
  !  OF A ROOT DISEASE STAND ATTRIBUTE.  THESE DIAMETERS ARE LOADED
  !  FIRST 5 POSITIONS OF THE ARRAY RATTR.  THE SIXTH POSITION IN
  !  RATTR IS LOADED WITH THE DIAMETER OF THE LARGEST TREE REPRESENTED
  !  IN THE ATTRIBUTE.

  !  CALLED BY :
  !     RDDOUT  [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     N      - NUMBER OF RECORDS.  (INPUT)
  !     RATTR  - ARRAY THAT IS LOADED WITH THE DIAMETERS CORRESPONDING
  !              TO THE 10TH, 30TH, 50TH, 70TH, 90TH, AND 100TH
  !              PERCENTILE POINTS.  (OUTPUT)
  !     PCTWK  - ARRAY OF THE PERCENTILE POINTS OF THE TREE RECORDS.
  !              (INPUT)
  !     ODBH   - ARRAY OF THE DBH'S OF THE TREE RECORDS.  (INPUT)
  !     INDX   - INDEX ARRAY OF THE TREE RECORDS SORTED BY DBH. (INPUT)

  !  Revision History :
  !   11/06/89 - Last revision date.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'RDPARM.f90'

  ! Variables
  integer :: I,IBOT,INDX(IRRTRE),IPTR,ITOP,J,MIDPTR,N,N1

  real    :: ODBH(IRRTRE),PCTAGE,PCTWK(IRRTRE),RATTR(7)

  !     IF THE TREE LIST IS NOT EMPTY, THEN:
  !     BEGIN BINARY SEARCH FOR PERCENTILE POINTS.

  if (N .eq.0) goto 60
  N1 = N + 1
  ITOP = 1
  PCTAGE = 90.0

  do I = 1,5
     J = 6 - I
     if (ITOP .eq. N) goto 40
     IBOT = N1

     10 IPTR = (IBOT + ITOP) / 2
     MIDPTR = INDX(IPTR)
     if (PCTWK(MIDPTR) .lt. PCTAGE) goto 20
     ITOP = IPTR
     goto 30

     20 IBOT = IPTR

     30 if (ITOP + 1.lt. IBOT) goto 10

     !        PERCENTILE POINT HAS BEEN LOCATED.

     40 continue
     RATTR(J) = ODBH(INDX(ITOP))
     PCTAGE = PCTAGE - 20.0
     50 continue
  end do

  !     ALL PERCENTILE POINTS HAVE BEEN LOCATED.  LOAD RATTR(6)

  RATTR(6) = ODBH(INDX(1))

  60 return
end
