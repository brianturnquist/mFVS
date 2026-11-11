!=============================!
!   canada/newmist : dmtlst   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMTLST (Sp, tDMR, Ptr, Index, P, TLst)
  implicit none

  !  **DMTLST -- NISI  Date of last revision April 16 1994

  ! Purpose:
  !   To simulate spread the model places target trees at the center
  ! of sampling rings. This routine chooses those source trees,
  ! and is called once for each DM category. When it was first
  ! developed, it made its choices based on trees/acre density of each
  ! treelist record. This was subsequently modified (see commented-out
  ! code below) such that the relative proportion of the record was
  ! used to penalize the likelihood of choosing a record. This made it
  ! less likely that records with a lot of influence would skew the
  ! simulation. However, no *other* FVS extension behaves this way, and
  ! it has since been removed. Because of this change, this routine is
  ! now much simpler, and now only takes all the trees from the
  ! specified DM category and puts those tree's indices in the proper
  ! place to be accessed by the calling routine.

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     [none]

  ! Argument list definitions:

  !     INTEGER Sp      (I) The species code being processed.
  !     INTEGER tDMR    (I) The DM category being processed.
  !     INTEGER Ptr     (I) Matrix of **pointers to treelist, sorted
  !                          by species and DM class:
  !                          Index 1: Species code.
  !                          Index 2: DM rating.
  !                          Index 3: FST is first position in
  !                                   'Index()'.
  !     INTEGER Index   (I) Array containing pointers to the treelist.
  !                          It is sorted by species and DM rating and
  !                          is an analogue of the 'IND1()' array of
  !                          the base model.
  !     REAL    P       (I) The density (trees/acre) of each record of
  !                          the treelist.
  !     INTEGER TLst    (O) The trees to be used as targets; the
  !                          zero'th entry contains the number of trees
  !                          in the list.

  ! Local variable definitions:

  !     INTEGER i           Loop counter for treelist records.
  !     INTEGER j           Index to treelist records from 'i'.
  !     INTEGER k           Number of trees to place as targets.

  ! Common block variables and parameters:

  !     MAXSP   PRGPRM
  !     FST     DMCOM
  !     LST     DMCOM
  !     MAXTRE  PRGPRM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'DMCOM.f90'

  ! Subroutine arguments.

  ! Variables
  integer :: Sp
  integer :: tDMR
  integer :: Ptr
  integer :: Index
  real    :: P
  integer :: TLst

  dimension Ptr(MAXSP, 0:6, LST)
  dimension Index(MAXTRE)
  dimension P(MAXTRE)
  dimension TLst(0:MAXTRE)

  ! Local variables.

  integer :: i,j,k

  ! If there are no targets, then return empty-handed, setting TLst(0).

  k = 0
  if (Ptr(Sp, tDMR, FST) .gt.0) then

     ! Decide if this Target is going to play the game, receiving
     ! ballistic spread. 'DMGAME' [now defunct keyword] defaults to 1,
     ! making the likelihood the reciprocal of the trees/acre of the
     ! record in question. If not playing, fall through the loop.

     do i = Ptr(Sp, tDMR, FST), Ptr(Sp, tDMR, LST)
        j = Index(i)
        if (P(j) .gt.0.) then
           !            x = DMGAME / (P(j) * StndArea) /* see 'Purpose' for the
           !            CALL DMRANN(RND)                  explanation about this
           !            IF (RND .LE. x) THEN              this */
           k = k + 1
           TLst(k) = j
           !            END IF
        end if
        100 continue
     end do
  end if
  TLst(0) = k

  return
end
