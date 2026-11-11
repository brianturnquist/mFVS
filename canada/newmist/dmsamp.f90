!=============================!
!   canada/newmist : dmsamp   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMSAMP (TotD, D, CNB, Prop, S)
  implicit none

  ! **DMSAMP -- NISI  Date of last revision: April 12 1994

  ! Purpose:
  !   Determine the sample size 'S' to be used, based on a random selection
  ! from the cumulative distribution function. 'CNB()' is the cumulative
  ! density function for trees of all species. 'TotD' is the total density
  ! of all species in the stand. 'D' is the (autocorrelated) density within
  ! the ring.

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     DMRANN

  ! Argument list definitions:

  !     REAL      TotD  (I) The total density (trees/acre) of all trees
  !                          of all species.
  !     REAL      D     (I) The adjusted density (trees/acre) for the
  !                          Species and DMR category from which a
  !                          sample may be drawn.
  !     REAL      CNB   (I) Array containing the cumulative
  !                          distribution function describing the
  !                          likelihood of drawing 'n' trees of any
  !                          species or DMR category from within a
  !                          sampling ring.
  !     REAL      Prop  (I) Scalar the modifies the number drawn from
  !                          the sample, depending on how much of the
  !                          ring is in the stand.
  !     INTEGER   S     (O) The sample size obtained from a random
  !                          draw from within the appropriate species
  !                          and DM class: the sample may be zero.

  ! Local variable definitions:

  !     INTEGER   BigS      A random sample of the number of trees that
  !                          are found in the sample ring. They may be
  !                          of any species or DM class.
  !     INTEGER   j         Loop counter for the cumulative
  !                          distribution.
  !     SINGLE    RND       A unform random number.
  !     SINGLE    x         The proportion of all trees that will be in
  !                          the appropriate species and DM class for
  !                          this sample.

  ! Common block variables and parameters:

  !     DSTLEN   DMCOM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DMCOM.f90'

  ! Argument list variables.

  ! Variables
  real    :: TotD
  real    :: D
  real    :: CNB
  real    :: Prop
  integer :: S

  dimension CNB(0:DSTLEN)

  ! Local variables.

  integer :: BigS,j
  real    :: RND,x

  ! Draw a sample of trees, based on the cumulative distribution
  ! function that describes tree clumping. This larger sample is
  ! 'BigS'.

  call DMRANN(RND)
  BigS = 0
  do j = 0, DSTLEN
     if (RND .le. CNB(j)) then
        BigS = j
        goto 201
     end if
  end do

  ! From the larger sample representing all trees in the stand,
  ! determine how many will be of the required DMR, based on the ratio
  ! of the density of the appropriate tree class to the total tree
  ! density. This is stochastic: summed over all the DMR classes, the
  ! value of 'S' is not compelled to be 'BigS * D / TotD', but will
  ! tend to that value.

  201 S = 0
  if (BigS .gt.0) then
     x = Prop * (D / TotD)
     do j = 1, BigS
        call DMRANN(RND)
        if (RND .le. x) S = S + 1
     end do
  end if

  return
end
