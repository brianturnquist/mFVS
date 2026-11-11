!===================!
!   rd : RDPARMCA   !
!===================!
! RD $Id$

!  Parameters for the root disease model are :

! Variables
integer :: DSO,DSII,DSIU

parameter (DSO = 1)
parameter (DSII = 2)
parameter (DSIU = 3)

parameter (IRRTRE = 500)
parameter (IRRTP1 = IRRTRE+1)
parameter (ITOTSP = 46)
parameter (ITOTRR = 4)

!     DSO    - Code for Dead Standing Outside (includes non-standing)
!     DSII   - Code for Dead Standing Inside Infected (includes
!              non-standing trees)
!     DSIU   - Code for Dead Standing Inside Uninfected (includes
!              non-standing)
!     IRRTRE - The max number of tree records that root disease can
!              process
!     IRRTP1 - The max number of tree records plus 1.
!     ITOTSP - Total number of species used by all variants.
!     ITOTRR - Total possible number of root diseases represented by
!              the model
!                 1 = P-TYPE ANNOSUS
!                 2 = S-TYPE ANNOSUS
!                 3 = ARMILLARIA
!                 4 = PHELLINUS

!            **If this is changed, the data statements in ANBLK1 must
!              also change.
