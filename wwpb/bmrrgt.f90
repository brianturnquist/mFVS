!===================!
!   wwpb : bmrrgt   !
!===================!
! WWPB $Id$
subroutine RRPPGT (WK3, IPNT, ILIMIT)

  !  Purpose:
  !     Get the quick root rot and stem rust model data for a given stand.
  !  This is part of the Parallel Processing Extension, and freely
  !  adapted from MSPPGT.FOR.

  !  Call list definitions:
  !     ILIMIT: (I)  Size of print buffer WK3.
  !     IPNT:   (IO) Pointer to curent element in print buffer WK3.
  !     WK3:    (IO) Work array used as a print buffer.

  !  Local variable definitions:
  !     I:      Loop counter.

  !  Common block variables and parameters:
  !     LRRON:  From RRBMCM; TRUE if RR spread rate is > 0
  !     LSRON:  From RRBMCM; TRUE if SR spread rate is > 0
  !     BMRRSR: From RRBMCM; Periodic RR spread rate (0-1)
  !     BMSRSR: From RRBMCM; Periodic SR spread rate (0-1)
  !     BMRR:   From RRBMCM; Proportion RR infection of record (0-1)
  !     BMSR:   From RRBMCM; Proportion SR infection of record (0-1)

  !.... Parameter statements.

  parameter (MXL = 2, MXR = 2, MXI = 0)

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'BMRRCM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: LOGICS (MXL)
  real    :: WK3(MAXTRE),REALS(MXR)

  !.... Get the logical scalars.

  call LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)
  LRRON = LOGICS (1)
  LSRON = LOGICS (2)

  !.... Get the real scalars.

  call BFREAD (WK3, IPNT, ILIMIT, REALS, MXR, 2)
  BMRRSR = REALS (1)
  BMSRSR = REALS (2)

  !.... Get the real arrays, if active.

  if (LRRON) call BFREAD (WK3, IPNT, ILIMIT, BMRR, ITRN, 2)
  if (LSRON) call BFREAD (WK3, IPNT, ILIMIT, BMSR, ITRN, 2)

  return
end
