!===================!
!   wpbr : i4pbsr   !
!===================!
! WPBR $Id$
subroutine I4PBSR (N,A,IORD,F,IP)
  implicit none

  !  Purpose:
  !  This is an INTEGER*4 keyed binary search.
  !  Part of the Stand Prognisis Model (Forest Vegetation Simulator)
  !  adapted from INTEGER search - NL Crookston - Jan. 1981 - Moscow, ID

  !  Binary search routine:  finds the subscript location of F in
  !  array A, if IORD is filled with the ascending order indices of
  !  array A, rather than the descending order indices.  Array A need
  !  not be in ascending order as long as the keys in IORD are in
  !  ascending order over A.

  !  Local variable definitions:
  !    N    = the length of A.
  !    A    = a list of INTEGER*4 values
  !    IORD = an array of keys sorted in ascengin order over A.
  !    F    = an INTEGER*4 value which you wish to find in A.
  !    IP   = the position in A where F was found; or 0 when F is not
  !           a member of A.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !.... Local variable declarations.

  ! Variables
  integer*4 :: A,F
  integer :: IORD,ITOP,IBOT,IMID,IP,I1,N,IB,IM,IN,IT
  dimension A(1),IORD(1)

  !.... Initializations.

  IMID = 1
  I1 = IORD(1)

  !.... If the value we search for is less than the smallest value in the
  !.... array, then skip out.

  if (F.le.A(I1)) go to 40
  IMID = N
  IN = IORD(N)

  !.... If the value we search for is greater tha  the largest value in
  !.... the array, then skip out.

  if (F.ge.A(IN)) go to 40

  !.... Initialize the top and bottom of the search partition to the top
  !.... and bottom of the array.

  ITOP = 1
  IBOT = N

  !.... Test the middle value of the partition.

  20 continue
  IMID = (IBOT+ITOP)/2
  IM = IORD(IMID)

  !.... If the middle value is greater than or equal to the search value,
  !.... set the top of the partition to the middle value plus one.

  if (F.gt.A(IM)) go to 30

  !.... If the middle value is less than what we search for, set the
  !.... bottom of the partition to the middle value minus one.

  IBOT = IMID-1
  IB = IORD(IBOT)

  !.... If the value of the bottom of the partition is greater than or
  !.... equal to the search value, then skip out.

  if (F.gt.A(IB)) go to 40

  !.... Re-search the partition.

  go to 20

  !.... Set the top of the partition to the middle value plus one.

  30 continue
  ITOP = IMID+1
  IT = IORD(ITOP)

  !.... If the value of the top of the partition is less than or equal
  !.... to the search value, the skip out.

  if (F.lt.A(IT)) go to 40

  !.... Re-search the partition.

  go to 20

  !.... This is the exit point for the routine.  If we got here, we
  !.... either found the search value as the middle of a partition,
  !.... or partitioned to the point where the search value could
  !.... not be contained in the partition.  At this point, we don't know
  !.... which so we make a final check, and return zero if not found.

  40 continue
  IP = IORD(IMID)
  if (F.ne.A(IP)) IP = 0
  return
end
