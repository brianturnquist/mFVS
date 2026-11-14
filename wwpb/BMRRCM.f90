!===================!
!   wwpb : BMRRCM   !
!===================!
! WWPB $Id$
!.... Code segment BMRRCM

!  **BMRR  Date of last revision:  JUNE 22, 1994

!  Purpose:
!     Common block variables related to the Stand level Root Disease and
!     Stem Rust embedded in Westwide Pine Beetle Model.

!     BMRRSR:   Decadal spread rate of RR
!     BMSRSR:   Decadal spread rate of SR
!     BMRR:     Proportion of stems infected by RR.
!     BMSR:     Proportion of stems infected by SR.

! Variables
logical :: LRRON,LSRON
real    :: BMRRSR,BMSRSR
real    :: BMRR(MAXTRE),BMSR(MAXTRE)

common /BMRRCM/ LRRON, LSRON, BMRRSR, BMSRSR, BMRR, BMSR

!.... End segment.
