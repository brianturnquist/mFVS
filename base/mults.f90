!=============================!
!   Apply model multipliers   !
!=============================!
! BASE $Id$
subroutine MULTS (KIND,IDT,XMULT)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  !     GETS THE MULTIPLIERS - NL CROOKSTON - JAN 1981 - MOSCOW.

  !     KIND  = THE TYPE OF MULTIPLIER, WHERE:
  !             1 IS BAIMULT    (FROM **DGDRIV** )
  !             2 IS HTGMULT    (FROM **HTGF**   )
  !             3 IS REGHMULT   (FROM **REGENT** )
  !             4 IS MORTMULT
  !             5 IS ESTBMULT
  !             6 IS REGDMULT   (FROM **REGENT** )
  !             7 IS MANAGED    (FROM **DGDRIV** )

  !     IDT   = THE CURRENT YEAR=IY(ICYC).
  !     XMULT = SPECIES SPECIFIC MULTIPLIERS.

  ! Variables
  real    :: XMULT(MAXSP)
  integer :: MYACTS(7)
  real    :: PRM(2)
  integer :: IDT,KIND,NTODO,I,NP,IACTK,IDATE,ISP,IGRP,IULIM,IG,ISPG
  data MYACTS/91,92,93,94,95,96,82/

  !     IF KIND IS OUT-OF-RANGE; RETURN

  if (KIND.gt.7 .or. KIND.lt.1) return

  !     FIND OUT IF THERE ARE ANY MULTIPLIERS.

  call OPFIND (1,MYACTS(KIND),NTODO)

  !     IF THERE ARE NONE;
  !     THEN: RETURN

  if (NTODO .eq. 0) return

  !     ELSE: GET THE MULTIPLIERS AND LOAD THEM INTO THE XMULT ARRAY.

  do I = 1,NTODO
     call OPGET(I,2,IDATE,IACTK,NP,PRM)
     if (IACTK.lt.0) goto 50

     !     SET THE STATUS OF THE OPTION AS ACCOMPLISHED IN THE YEAR 'IDT'

     call OPDONE(I,IDT)

     !  PROCESS MANAGED KEYWORD

     if (KIND .eq. 7) then
        XMULT(1) = PRM(1)
        go to 50
     end if

     !     IF SPECIES CODE IS:
     !        < 0 THEN IT APPLIES TO SPECIES IN A GROUP
     !        = 0 THEN IT APPLIES TO ALL SPECIES
     !        > 0 THEN IT APPLIES TO JUST THIS SPECIES

     ISP = IFIX(PRM(1))
     if (ISP .lt. 0) then
        IGRP = -ISP
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           ISPG = ISPGRP(IGRP,IG)
           XMULT(ISPG) = PRM(2)
           30 continue
        end do
     else if (ISP.eq.0) then
        do ISP = 1,MAXSP
           XMULT(ISP) = PRM(2)
           40 continue
        end do
     else
        XMULT(ISP) = PRM(2)
     end if
     50 continue
  end do
  return
end subroutine MULTS
