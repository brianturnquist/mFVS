!===================!
!   wwpb : bmagdw   !
!===================!
! WWPB $Id$
subroutine BMAGDW (ISTD,IYR)

  !  Purpose:
  !     Transfer some SDWP() to DDWP (falldown)
  !     Age 'SDWP()' and 'DDWP()' compartments by one year, using decay rates
  !     taken from the fire model
  !     Zero out DWPHOS()

  !  Local variable definitions:
  !     I:      Loop counter for host/non=host species
  !     J:      Loop counter for size classes
  !     K:      Loop counter for dead woody pool age classes
  !     PTRANS: Array containing proportions of dead woody pool stuff
  !             surviving to next age class.

  !  Common block variables and parameters:
  !     DDWP:   From BMCOM;  Downed Dead Woody Pool (TONS)
  !     DWPHOS: From BMCOM;  Dead Woody Pool (Host Trees only) (TPA)
  !     SDWP:   From BMCOM;  Standing Dead Woody Pool (TOT CU FT)
  !     V2T:    From BMPRM;  Conversion factor from volume to tons

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: I,J,K
  real    :: FALL
  real    :: X
  real    :: PTRANS(MXDWAG - 1)

  !.... Data statements.

  !.... Initializations

  !.... Check for debug.

  if (LBMDEB) write(JBMBPR,10) IYR, ISTD
  10 format(' Begin BMAGDW: Year = ',I5, 'Stand = ', I6)

  !.... Begin Routine

  !.... The inverse is (roughly) the annual transfer out of each pool

  PTRANS(1) = 1.0 / 1.0
  PTRANS(2) = 1.0 / 1.0
  PTRANS(3) = 1.0 / 3.0
  PTRANS(4) = 1.0 / 5.0

  !.... Decay dead pools and calculate falldown
  !         JJ references small/large size classes in SDECRT and DDWP

  do J = 1,(MXDWHZ+1)
     JJ = MIN0(J,MXDWSZ)

     do K = 1,MXDWAG
        FALL = 0.0
        do I = 1,MXDWPC
           SDWP(ISTD,I,J,K) = SDWP(ISTD,I,J,K) * SDECRT(JJ)
           FALL = FALL + SDWP(ISTD,I,J,K) * FALLRT(I)
           !  Added 10/01.ajm.  I'm 95% certain this next line is needed.  Heretofore, felled
           !  trees were added to DDWP but never subtracted from SDWP!
           SDWP(ISTD,I,J,K) = SDWP(ISTD,I,J,K) * (1-FALLRT(I))
           222 continue
        end do
        if (J .le. MXDWSZ)&
             &DDWP(ISTD,J,K) = DDWP(ISTD,J,K) * DDECRT(J)
        DDWP(ISTD,JJ,K) = DDWP(ISTD,JJ,K) + FALL * V2T
        220 continue
     end do
     200 continue
  end do

  !     Age dead pools

  do J = 1,(MXDWHZ+1)
     do K = (MXDWAG-1),1,-1
        do I = 1,MXDWPC

           X = SDWP(ISTD,I, J, K) * PTRANS(K)
           SDWP(ISTD,I, J, K) = SDWP(ISTD,I, J, K) - X
           SDWP(ISTD,I, J, K + 1) = SDWP(ISTD,I, J, K + 1) + X

           322 continue
        end do
        if (J .le. MXDWSZ) then
           X = DDWP(ISTD,J, K) * PTRANS(K)
           DDWP(ISTD,J,K) = DDWP(ISTD,J,K) - X
           DDWP(ISTD,J,K + 1) = DDWP(ISTD,J,K + 1) + X
        end if
        320 continue
     end do
     300 continue
  end do

  !.... Zero out the fresh host pool (Note that fresh host is now also added to the
  !     other dead woody pools when it is created since everything uses slightly
  !     different array subscripts)

  do I = 1,MXDWHC
     do J = 1,MXDWHZ
        DWPHOS(ISTD,I, J) = 0.0
        520 continue
     end do
     500 continue
  end do

  if (LBMDEB) write(JBMBPR,99) IYR, ISTD
  99 format(' End BMAGDW: Year = ',I5, 'Stand = ', I6)

  return
end
