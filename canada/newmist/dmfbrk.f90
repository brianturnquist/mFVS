!=============================!
!   canada/newmist : dmfbrk   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMFBRK
  implicit none

  !  **DMFBRK -- NISI  Date of last revision April 7 1994

  ! Purpose:
  !   This routine locates the four breakpoints that define the crown
  ! thirds of each tree record. Crown thirds are the basic vertical
  ! spatial unit used by the model, and are drawn from Hawksworth's
  ! rating system. BrkPnt(1) is the height at the TOP of the crown;
  ! BrkPnt(4) is the BOTTOM of the crown. BrkPnt(2) and BrkPnt(3)
  ! divide the crown thirds. The BrkPnt() units are MESH.

  ! Called by:

  !     DMTREG

  ! Other routines called:

  !     [none]

  ! Argument list definitions:

  !     [none]

  ! Local variable definitions:

  !     INTEGER i           loop counter for tree records
  !     INTEGER j           loop counter for crown thirds
  !     REAL    x           scalar constant
  !     REAL    y           scalar constant
  !     REAL    z           MESH units per crown third

  ! Common block variables and parameters:

  !     ITRN    CONTRL
  !     HT      ARRAYS
  !     ICR     ARRAYS
  !     FPM     DMCOM
  !     MESH    DMCOM
  !     CRTHRD  DMCOM
  !     BPCNT   DMCOM
  !     BrkPnt  DMCOM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'DMCOM.f90'

  ! Local variables.

  ! Variables
  integer :: i,j
  real    :: x,y,z

  ! These conversions are required because 'HT()' is measured in feet and
  ! 'ICR()' is a percentage. The model requires MESH units (usually 2
  ! meters.

  y = 1.0 / (FPM * MESH)
  x = 0.01 * y / FLOAT(CRTHRD)

  do i = 1, ITRN
     z = HT(i) * FLOAT(ICR(i)) * x
     BrkPnt(i, 1) = HT(i) * y
     do j = 2, BPCNT
        BrkPnt(i, j) = BrkPnt(i, j - 1) - z
        200 continue
     end do
     100 continue
  end do

  return
end
