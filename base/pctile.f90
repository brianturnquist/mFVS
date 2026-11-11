!========================================!
!   Compute indexed vector percentiles   !
!========================================!
! BASE $Id$
subroutine PCTILE (N,INDEX,CHAR,PERCNT,TOT)
  implicit none

  !   THIS SUBROUTINE COMPUTES PERCENTILE WITHIN THE DISTRIBU-
  !   TION OF A REAL VECTOR (SUCH THAT THE LARGEST ELEMENT HAS
  !   A PERCNT((I)=100.0, WHERE I=INDEX(I)) AND ALSO COMPUTES
  !   THE SUMS OF THE VECTOR.  VARIABLES USED ARE:

  !     CHAR -- ARRAY LOADED WITH REAL VECTOR VALUES FOR EACH
  !             RECORD

  !    INDEX -- INDEXING ARRAY WHICH POINTS TO THE MEMBERS OF
  !             'CHAR' IN DESCENDING ORDER.

  !        N -- CURRENT NUMBER OF RECORDS

  !   PERCNT -- ARRAY LOADED WITH VECTOR PERCENTILES FOR EACH
  !             RECORD

  !      TOT -- SUM OF THE VECTOR VALUES

  !   DECLARATIONS

  ! Variables
  integer :: N,NM1,INDN,I,J,INDJP1,INDJ,INDX1,INDI
  real    :: TOT,PERCNT(*),PCTIN1,CHAR(*)
  integer :: INDEX(*)

  if (N .eq. 0) then
     PERCNT(1) = 0.
     TOT = 0.
  else
     PERCNT(1) = 100.
     TOT = CHAR(1)
  end if

  !   RETURN IF ONLY 1 RECORD

  if ( N .le. 1 )return
  NM1 = N - 1
  INDN = INDEX(N)
  PERCNT(INDN) = CHAR(INDN)

  !   ACCUMULATE THE SUM OF THE VECTORS

  do I = 1,NM1
     J = N-I
     INDJP1 = INDEX(J+1)
     INDJ = INDEX(J)
     PERCNT(INDJ) = PERCNT(INDJP1)+CHAR(INDJ)
     10 continue
  end do
  INDX1 = INDEX(1)

  !   DESIGNATE THE SUM

  TOT = PERCNT(INDX1)
  PERCNT(INDX1) = TOT/100.

  !   RETURN IF THE SUM IS LESS THAN OR EQUAL TO 0.0

  if ( TOT .le. 0.0 )  return
  PCTIN1 = PERCNT(INDX1)
  do I = 2,N
     INDI = INDEX(I)

     !   COMPUTE THE PERCENTILE WITHIN THE DISTRIBUTION

     PERCNT(INDI) = PERCNT(INDI)/PCTIN1
     20 continue
  end do
  PERCNT(INDX1) = 100.
  return
end subroutine PCTILE
