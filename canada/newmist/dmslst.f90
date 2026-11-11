!=============================!
!   canada/newmist : dmslst   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMSLST (DMRCls, n, SInd, SCD, SPtr, SLst)
  implicit none

  ! **DMSLST -- NISI  Date of last revision: April 14 1994

  ! Purpose:
  !   To simulate spread the model places source trees in sampling
  ! rings around each target. This routine chooses those source trees,
  ! and is called once for each DM category. It makes its choices
  ! based on the number of trees it has been 'told' to find, choosing
  ! at random from the relevant DM category based on the density
  ! (trees/acre) of each treelist record. The attributes of those
  ! records are subsequently used to simulate the spatial relationships
  ! between sources and target.

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     DMRANN

  ! Argument list definitions:

  !     INTEGER DMRCls  (I) The DM category from which trees will be
  !                          chosen at random.
  !     INTEGER n       (I) The number of trees to choose.
  !     INTEGER SInd    (I) Pointer to the treelist index occupying
  !                          corresponding to the cumulative
  !                          probability within the 'SrcCD()' array.
  !                          These records are sorted by DM category
  !                          into groups marked by the 'Sptr()' array.
  !     REAL    SCD     (I) The cumulative probability of each DM
  !                          group is computed by taking the relative
  !                          density (trees/acre) of each treelist
  !                          record and forming a cumulative
  !                          distribution.
  !     INTEGER SPtr    (I) Breakpoints demarcating the DM categories
  !                          ordered within the 'SInd() and 'SCD()'
  !                          arrays. Each value marks the *last* entry
  !                          in that category: eg: 'Sptr(3)' contains
  !                          the position of the last position with DM
  !                          rating 3; 'Sptr(2)+1' contains the first.
  !     INTEGER SLst    (O) List of trees selected to occupy the
  !                          sampling ring being processed.
  !                          Index 1: The index value to the treelist
  !                                   record.
  !                          Index 2: The number of occurrences of the
  !                                   record.

  ! Local variable definitions:

  !     LOGICAL  flg        A flag that is .TRUE. whenever a treelist
  !                          record has already been chosen; otherwise
  !                          it is .FALSE.
  !     INTEGER  i          Loop counter for the number of trees to
  !                          include in the sample 'n'.
  !     INTEGER  j          Loop counter for the relevant portion of
  !                          the 'SCD()' and 'SInd' arrays.
  !     INTEGER  k          Loop counter for the number of unique
  !                          treelist records already included.
  !     INTEGER  m          An incremental counter that records the
  !                          number of unique treelist records that
  !                          have been selected.
  !     INTEGER  pFrst      Position of first cumulative distribution
  !                          and treelist record in 'SCD()' and 'SInd()'
  !                          arrays.
  !     INTEGER  pLast      Position of last (etc.) in arrays.
  !     REAL     RND        A uniform {0,1} random number.

  ! Common block variables and parameters:

  !     MAXTRE   PRGPRM
  !     DSTLEN   DMCOM
  !     INDX     DMCOM
  !     KNT      DMCOM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DMCOM.f90'

  ! Subroutine arguments.

  ! Variables
  integer :: DMRCls
  integer :: n
  integer :: SInd
  real    :: SCD
  integer :: SPtr
  integer :: SLst

  dimension SInd(MAXTRE)
  dimension SCD(MAXTRE)
  dimension SPtr(0:6)
  dimension SLst(0:DSTLEN, INDX)

  ! Local variables.

  logical :: flg
  integer :: i,j,k,m
  integer :: pFrst,pLast
  real    :: RND

  ! Compute pointers to the location of the distribution within the
  ! 'SCD()' array. Note that 'SPtr()' is the position of the *last*
  ! item in the set.

  if (DMRCls .eq.0) then
     pFrst = 1
  else
     pFrst = SPtr(DMRCls - 1) + 1
  end if

  pLast = SPtr(DMRCls)

  ! Loop through the 'i' samples to be drawn, calling a random
  ! number each time. Then walk through the 'j' values of the
  ! cumulative distribution. When a source is chosen, check to see if
  ! it has already been selected, and increment KNT if required.
  ! 'SLst(0, INDX)' holds the number of unique source trees used to
  ! generate the sample.

  m = 0
  do i = 1, n
     call DMRANN(RND)
     do j = pFrst, pLast
        if (RND .le. SCD(j)) then
           flg = .false.
           do k = 1, m
              if (SLst(k, INDX) .eq. SInd(j)) then
                 SLst(k, KNT) = SLst(k, KNT) + 1
                 flg = .true.
                 goto 401
              end if
              400 continue
           end do
           401 continue

           ! Initialize a new element of 'SLst()' if the treelist record
           ! 'SInd()' is new.

           if (.not.flg) then
              m = m + 1
              SLst(m, KNT) = 1
              SLst(m, INDX) = SInd(j)
           end if

           goto 200
        end if

        300 continue
     end do
     200 continue
  end do

  SLst(0, INDX) = m

  return
end
