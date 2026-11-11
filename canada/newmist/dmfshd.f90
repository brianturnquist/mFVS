!=============================!
!   canada/newmist : dmfshd   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMFSHD
  implicit none

  ! **DMFSHD --  DATE OF LAST REVISION: 02/16/96

  ! Purpose:
  !   This routine is responsible for estimating the amount of shading
  ! that occurs at each vertical 'layer' of the stand. It does this by
  ! simulating the position of each element of the treeslist, in
  ! proportion to its density, on a  100 x 100 grid of 1 meter squares.
  ! In order to simplify the computations at edges, an outside buffer
  ! makes to full simulation grid 121 x 121 meters. The (x,y) locations
  ! are random, with an implied Poisson distribution. The z-position
  ! and canopy information are provided by the base model and by the
  ! COVER extension. This information is widely used by the model, to
  ! estimate interception of seed and light-mediated effects. One of
  ! its drawbacks may be that it makes its estimates independent of the
  ! spatial pattern specified by the model/user, which may vary from
  ! highly clumped to highly regular. Because canopy overlapping rules
  ! and effects are not known, this would change the computed opacity
  ! in ways that are not obvious. I suspect, though, that the
  ! *distribution* of opacities (which is not even addressed here) is
  ! well correlated with the local density of trees; this might be used
  ! to give better information than only the mean opacity (which is
  ! what *is* computed).

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     DMRANN

  ! Argument list definitions:

  !     INTEGER Light (O)  Cumulative light remaining at each vertical
  !                         level within the stand.

  ! Local parameter and variable definitions:

  !     INTEGER i          General loop counter
  !     INTEGER j          General loop counter
  !     INTEGER s          Loop counter for traversing the x-direction
  !                         of the simulation grid
  !     INTEGER t          Loop counter for traversing the y-direction
  !                         of the simulation grid
  !     INTEGER u          Loop counter over heights.
  !     INTEGER v          Loop counter over treelist records.
  !     INTEGER n          Number of trees of a particular treeslist
  !                         record, to put on the grid.
  !     INTEGER SLstLn     Length of 'ShdLst()' array.
  !     INTEGER Grid       The dimension of the simulation grid
  !     INTEGER LowIn      The first position inside the edge buffer.
  !     INTEGER HighIn     The last position inside the edge buffer.
  !     INTEGER ShdLst     List of heights in which canopy is present.
  !     REAL    Tnumbr     Expected number of trees for a particular
  !                         treelist record.
  !     REAL    Frac       Fractional remainder of 'Tnumbr'.
  !     REAL    d          Dstnce (meters) from center of simulated
  !                         tree to the edge of its canopy.
  !     REAL    x          X-position (grid cell) of simulated tree.
  !     REAL    y          Y-position (grid cell) of simulated tree.
  !     REAL    sum        summation of opacity on grid.
  !     REAL    xp         X-position within canopy of simulated tree.
  !     REAL    yp         Y-position within canopy of simulated tree.
  !     REAL    Rad        Radius (m) of simulated tree at a given
  !                         height.
  !     REAL    Opq        Opacity of tree species (blockage per meter)
  !     REAL    RND        Uniform {0,1} random number.
  !     REAL    Cells      Cells along the side of the simulation grid.
  !     REAL    G          The grid itself!

  ! Common block variables and parameters:

  !     MXHT    DMCOM
  !     ITRN    CONTRL
  !     DMRDMX  DMCOM
  !     RADIUS  DMCOM
  !     PROB    ARRAYS
  !     MESH    DMCOM
  !     SQM2AC  DMCOM
  !     DMOPAQ  DMCOM
  !     ISP     ARRAYS
  !     Shade   DMCOM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'DMCOM.f90'

  ! Local variables and Parameters.

  ! Variables
  integer :: Grid
  integer :: LowIn
  integer :: HighIn
  real    :: Cells

  parameter (Grid = 121)
  parameter (LowIn = 11)
  parameter (HighIn = 110)
  parameter (Cells = 100.0)

  integer :: i,j,s,t,u,v
  integer :: n,SLstLn
  integer :: ShdLst(MXHT)
  real    :: Tnumbr,Frac
  real    :: d,x,y,sum,xp,yp,Rad
  real    :: Opq
  real    :: RND
  real    :: G(Grid,Grid)

  ! Zero the 'Shade()' array, then find which of the height categories
  ! actually have something in them.

  i = 0
  do u = 1, MXHT
     Shade(u) = 0.
     do v = 1, ITRN
        if (PROB(v) .gt.0.01.and.&
             &DMRDMX(v, u, RADIUS) .gt.0.1) then
           i = i + 1
           ShdLst(i) = u
           goto 350
        end if
        400 continue
     end do
     350 continue
     300 continue
  end do

  SLstLn = MIN0(i, MXHT)

  ! This code remains at the 1 meter grid cell (horizontal plane)
  ! resolution. Walk through heights having some shading. Note that
  ! 'Rad' is converted from MESH to meters.

  do u = 1, SLstLn

     v = ShdLst(u)

     do i = 1, Grid
        do j = 1, Grid
           G(i, j) = 0.
           800 continue
        end do
        700 continue
     end do

     do i = 1, ITRN

        Rad = DMRDMX(i, v, RADIUS) * FLOAT(MESH)

        if (Rad .gt.0.0) then

           ! Determine number of trees to place on grid; then choose for the
           ! fractional remainder.

           TNumbr = PROB(i) * SQM2AC * FLOAT(Grid) ** 2
           n = INT(TNumbr)
           Frac = TNumbr - n

           call DMRANN(RND)
           if (RND .le. Frac) n = n + 1

           ! Locate each simulated radius on the grid and fill up cells within
           ! the radius of each disc. ('xp' and 'yp' are actually not exactly at
           ! P(i)...; they are -0.5 from there. Since their comparison points
           ! 'x' and 'y' are also shifted, it makes no difference.) Note that
           ! the decision to overlay two disks replaces less dense "canopy
           ! foliage" with more opaque. I don't know if tree-tree competition
           ! really acts in this way.

           Opq = DMOPAQ(ISP(i))

           do j = 1, n

              call DMRANN(RND)
              x = FLOAT(INT(RND * FLOAT(Grid)) + 1)
              call DMRANN(RND)
              y = FLOAT(INT(RND * FLOAT(Grid)) + 1)

              do s = INT(x - Rad), INT(x + Rad)
                 do t = INT(y - Rad), INT(y + Rad)
                    if ((s .gt.0) .and. (s .le. Grid) .and.&
                         &(t .gt.0) .and. (t.le. Grid)) then
                       xp = FLOAT(s)
                       yp = FLOAT(t)
                       d = SQRT((xp - x) ** 2 + (yp - y) ** 2)
                       if ( (d .le. Rad) .and. (G(s,t) .lt. Opq) )&
                            &G(s, t) = Opq
                    end if
                    1200 continue
                 end do
                 1100 continue
              end do
              1000 continue
           end do

        end if

        900 continue
     end do

     !  Walk through the inner grid, adding up areas containing tree
     ! radii. The mean value is taken as the shading. Following that,
     ! 'Shade()' is converted to meter units, then to MESH units.

     sum = 0.0
     do i = LowIn, HighIn
        do j = LowIn, HighIn
           sum = sum + G(i, j)
        end do
     end do

     Shade(v) = sum
     Shade(v) = Shade(v) / (Cells ** 2)
     Shade(v) = 1.0 - ((1.0 - Shade(v)) ** MESH)

     if (Shade(v) .gt.1.0) Shade(v) = 1.0
     if (Shade(v) .lt.0.0) Shade(v) = 0.0

     500 continue
  end do

  ! Define 'Light()' extinction as the cumulative effect of shading
  ! levels above the current level.

  Light(MXHT) = 1.0 - Shade(MXHT)
  do i = MXHT-1,1,-1
     Light(i) = Light(i + 1) * (1.0 - Shade(i))
  end do

  return
end
