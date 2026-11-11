!=================!
!   rd : rdestb   !
!=================!
! RD $Id$
subroutine RDESTB(N,ANS)
  implicit none

  !  SUBROUTINE FOR PUTTING REGENERATION ONTO ROOT DISEASE ROOT DISEASE PATCHES

  !  CALLED BY :
  !     ESTAB   [PROGNOSIS]

  !  CALLS     :
  !     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]

  !  PARAMETERS :
  !     N      -
  !     ANS    -

  !  Revision History :
  !   03/07/95 - Last revision date.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/01/2016 Lance R. David (FMSC)
  !     Moved one condition to exit to top.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'RDADD.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: IDI,IT,ISPI,J,N
  real    :: ANS,TPAREA,XXX

  if (IROOT .eq.0) return

  TPAREA = 0.0
  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     20 continue
  end do
  if (TPAREA .eq.0.0) return

  !.....PUT TREES INTO OUTSIDE DENSIITY ARRAY (INCLUDING THE FRINGE ARRAY)

  FPROB(N) = ANS
  FFPROB(N,2) = ANS

  do IT = 1,ISTEP
     PROBI(N,IT,1) = 0.0
     PROBI(N,IT,2) = 0.0
     40 continue
  end do

  IDI = MAXRR
  if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(ISP(N)))

  PROBL(N) = ANS
  PROBIU(N) = ANS * PAREA(IDI)
  WK22(N) = 0.0
  ISPI = ISP(N)
  call RDROOT(ISPI,0.1,XXX,PROOT(IRTSPC(ISPI)),RSLOP(IRTSPC(ISPI)),&
       &HT(N))
  RROOTT(N) = XXX

  do J = 2,4
     XMTH(J,N) = -1.0
     ROOTH(J,N) = -1.0
     1000 continue
  end do

  call RDSUM(ITRN,PROBIT,PROBI,ISTEP)

  return
end
