!==============================!
!   Sort four character keys   !
!==============================!
! BASE $Id$
subroutine CH4SRT (N,A,INDEX,LSEQ)
  implicit none

  !  CHARACTER*4 INDEX SORT.

  !  CH4SRT USES THE VECTOR INDEX TO INDIRECTLY ADDRESS THE ARRAY A.
  !  THE FIRST N ELEMENTS OF THE ARRAY INDEX REPRESENT INDICES OF
  !  ELEMENTS OF ARRAY A TO BE SORTED OVER.  THE FIRST N ELEMENTS OF THE
  !  INDEX ARRAY ARE REARRANGED SUCH THAT FOR EACH I FROM 1 TO N-1,
  !  A(INDEX(I)) IS LESS THAN A(INDEX(I+1)).  THE PHYSICAL ARRANGEMENT
  !  OF ARRAY A IS NOT ALTERED.

  !  IF LSEQ IS PASSED IN AS TRUE, THE VECTOR INDEX IS INITIALLY LOADED
  !  WITH VALUES FROM 1 TO N INCLUSIVE.  THIS SORTS OVER THE FIRST
  !  N ELEMENTS OF ARRAY A.

  !  IF LSEQ IS PASSED IN AS FALSE, THE FIRST N ELEMENTS OF INDEX ARE
  !  ASSUMED TO BE THE INDICES OF A TO BE SORTED OVER.

  !  THIS ALGORITHM IS AN ADAPTATION OF THE TECHNIQUE DESCRIBED IN:

  !       SCOWEN, R.A. 1965. ALGORITHM 271; QUICKERSORT. COMM ACM.
  !                    8(11) 669-670.

  !  DECLARATIONS:

  ! Variables
  logical :: LSEQ
  integer :: N,I
  integer :: INDEX(N),IPUSH(33),IL,IP,IU,INDIL,INDIP,INDIU,INDKL,INDKU,ITOP,JL,JU,KL,KU
  character(len=4) :: A(*)
  character(len=4) :: T

  !  LOAD IND WITH VALUES FROM 1 TO N.

  if (LSEQ) then
     do I = 1,N
        INDEX(I) = I
     end do
  end if

  !  RETURN IF FEWER THAN TWO ELEMENTS IN ARRAY A.

  if (N.lt.2) return

  !  BEGIN THE SORT.

  ITOP = 0
  IL = 1
  IU = N
  30 continue
  if (IU.le.IL) go to 40
  INDIL = INDEX(IL)
  INDIU = INDEX(IU)
  if (IU.gt.IL+1) go to 50
  if (A(INDIL).le.A(INDIU)) go to 40
  INDEX(IL) = INDIU
  INDEX(IU) = INDIL
  40 continue
  if (ITOP.eq.0) return
  IL = IPUSH(ITOP-1)
  IU = IPUSH(ITOP)
  ITOP = ITOP-2
  go to 30
  50 continue
  IP = (IL+IU)/2
  INDIP = INDEX(IP)
  T = A(INDIP)
  INDEX(IP) = INDIL
  KL = IL
  KU = IU
  60 continue
  KL = KL+1
  if (KL.gt.KU) go to 90
  INDKL = INDEX(KL)
  if (A(INDKL).le.T) go to 60
  70 continue
  INDKU = INDEX(KU)
  if (KU.lt.KL) go to 100
  if (A(INDKU).lt.T) go to 80
  KU = KU-1
  go to 70
  80 continue
  INDEX(KL) = INDKU
  INDEX(KU) = INDKL
  KU = KU-1
  go to 60
  90 continue
  INDKU = INDEX(KU)
  100 continue
  INDEX(IL) = INDKU
  INDEX(KU) = INDIP
  if (KU.le.IP) go to 110
  JL = IL
  JU = KU-1
  IL = KU+1
  go to 120
  110 continue
  JL = KU+1
  JU = IU
  IU = KU-1
  120 continue
  ITOP = ITOP+2
  IPUSH(ITOP-1) = JL
  IPUSH(ITOP) = JU
  go to 30
end subroutine CH4SRT
