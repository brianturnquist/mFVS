!=============================!
!   canada/newmist : dmfdns   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMFDNS (Sp, Ptr, Index, P, D)
  implicit none

  ! **DMFDNS -- NISI  Date of last revision April 7 1994

  ! Purpose:
  !   Find the density (trees/acre) of each DM class. This information
  ! is required for the calculation of probability distributions used
  ! to simulate the likelihood of neighbor-neighbor combinations.

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     [none]

  ! Argument list definitions:

  !     INTEGER Sp      (I) Species code, meaning is variant-dependent.
  !     INTEGER Ptr     (I) Matrix of **pointers to treelist, sorted
  !                          by species and DM class:
  !                          Index 1: Species code.
  !                          Index 2: DM rating.
  !                          Index 3: FST is first position in
  !                                   'Index()'.
  !                          array; LST is last position in array. This
  !                          mapping is analagous to the 'IND1()' array
  !                          of the base model, but with two levels of
  !                          indirection.
  !     INTEGER Index   (I) Array containing pointers to the treelist.
  !                          It is sorted by species and DM rating and
  !                          is an analogue of the 'IND1()' array of
  !                          the base model.
  !     REAL    P       (I) The density (trees/acre) of each record of
  !                          the treelist.
  !     REAL    D       (O) The density of each DM category of species
  !                          'Sp'.

  ! Local variable definitions:

  !     INTEGER i           Loop counter for DM categories.
  !     INTEGER j           Loop counter for treelist elements.

  ! Common block variables and parameters:

  !     MAXSP   PRGPRM
  !     MAXTRE  PRGPRM
  !     FST     DMCOM
  !     LST     DMCOM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DMCOM.f90'

  ! Subroutine arguments.

  ! Variables
  integer :: Sp
  integer :: Ptr
  integer :: Index
  real    :: P
  real    :: D

  dimension Ptr(MAXSP, 0:6, LST)
  dimension Index(MAXTRE)
  dimension P(MAXTRE)
  dimension D(0:6)

  ! Local variables.

  integer :: i,j

  ! Begin by setting the density to zero, then accumulate density for
  ! by adding the density 'P()' from each appropriate entry of the
  ! treelist. The appropriate trees are stored by the references
  ! stored in the 'Ptr()' array.

  do i = 0, 6
     D(i) = 0.
     if (Ptr(Sp, i, FST) .gt.0) then
        do j = Ptr(Sp, i, FST), Ptr(Sp, i, LST)
           D(i) = D(i) + P(Index(j))
           110 continue
        end do
     end if
     100 continue
  end do

  return
end
