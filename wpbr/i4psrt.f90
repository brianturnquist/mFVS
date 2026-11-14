!===================!
!   wpbr : i4psrt   !
!===================!
! WPBR $Id$
subroutine I4PSRT(N,A,INDXBR,LSEQ)
  implicit none

  !  Purpose:
  !  I4PSRT is an INTEGER*4 ascending identification sort.

  !  The vector INDXBR is initially loaded with values from 1 to N
  !  inclusive.  IAPSRT rearranges the elements of INDXBR so that
  !  INDXBR(1) is the subscript of the smallest element in the vector A,
  !  INDXBR(2) is the subscript of the second smallest element in A,...,
  !  and INDXBR(N) is the subscript of the largest element in A.  The
  !  physical arrangement of the vector A is not altered.  This
  !  algorithm is an adaptation of the technique described in:

  !     Scowen, R.A. 1965. Algorithm 271; Quickersort. Comm ACM.
  !     8(11) 669-670.

  !  Parameters:
  !  N     - The index value for the FVS and blist rust tree-specific arrays
  !          (current tree/record sequence).
  !  A     - Array of tree IDs (tree numbers, FVS array IDTREE).
  !  INDXBR - Array of index values for FVS and blister rust arrays
  !  LSEQ  - True if sorting is to be performed.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  10-MAY-1999 Lance R. David
  !     Added comments for parameters.
  !     Changed array named "INDEX" (which is a function name and should not
  !     be used as a variable) to "INDXBR".
  !     Changed parameter arrays to inherit dimension "(*)".
  !     Added debug code.

  !.... Local variable declarations.

  ! Variables
  logical :: LSEQ,DEBUG
  integer :: T,N
  integer*4 :: A(*)
  integer :: INDXBR(*),IPUSH(33)
  integer :: I,ICYC,IL,INDIL,INDIP,INDIU,INDKL,INDKU,IP,ITOP,IU,JL,JU,KL,KU

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'I4PSRT',6,ICYC)
  if (DEBUG) then
     write(16,*)' ENTER I4PSRT:  N=',N,' LSEQ=',LSEQ
     write(16,*)'    BEFORE SORTING'
     do I = 1,N
        write(16,*)'           INDXBR=',INDXBR(I),'    A=',A(I)
        8 continue
     end do
  end if

  !.... If LSEQ is FALSE, assume that A is partially sorted.
  !.... Otherwise, load INDXBR with values from 1 to N.

  if (LSEQ) then
     do I = 1,N
        INDXBR(I) = I
        10 continue
     end do
  end if

  !.... Return if fewer than two elements in array A.

  if (N.lt.2) go to 200

  !.... Sort.

  ITOP = 0
  IL = 1
  IU = N

  30 continue
  if (IU.le.IL) go to 40
  INDIL = INDXBR(IL)
  INDIU = INDXBR(IU)
  if (IU.gt.IL+1) go to 50
  if (A(INDIL).le.A(INDIU)) go to 40
  INDXBR(IL) = INDIU
  INDXBR(IU) = INDIL

  40 continue
  if (ITOP.eq.0) go to 200
  IL = IPUSH(ITOP-1)
  IU = IPUSH(ITOP)
  ITOP = ITOP-2
  go to 30

  50 continue
  IP = (IL+IU)/2
  INDIP = INDXBR(IP)
  T = A(INDIP)
  INDXBR(IP) = INDIL
  KL = IL
  KU = IU

  60 continue
  KL = KL+1
  if (KL.gt.KU) go to 90
  INDKL = INDXBR(KL)
  if (A(INDKL).le.T) go to 60

  70 continue
  INDKU = INDXBR(KU)
  if (KU.lt.KL) go to 100
  if (A(INDKU).lt.T) go to 80
  KU = KU-1
  go to 70

  80 continue
  INDXBR(KL) = INDKU
  INDXBR(KU) = INDKL
  KU = KU-1
  go to 60

  90 continue
  INDKU = INDXBR(KU)

  100 continue
  INDXBR(IL) = INDKU
  INDXBR(KU) = INDIP
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

  !.... Common return.

  200 continue

  if (DEBUG) then
     write(16,*)'    AFTER  SORTING'
     do I = 1,N
        write(16,*)'           INDXBR=',INDXBR(I),'    A=',A(I)
        208 continue
     end do
     write(16,*) ' EXIT I4PSRT'
  end if
  return
end
