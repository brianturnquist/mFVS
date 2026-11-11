!=================!
!   rd : rdcent   !
!=================!
! RD $Id$
subroutine RDCENT(PROBIN,PCENTS,NCENTS,PROBD,SAREA,&
     &IRFLAG,NNCENT,PISIZE,PAREA,IRRSP)
  implicit none

  !  THIS SUBROUTINE CREATES NEW INFECTION CENTERS BASED ON THE TOTAL
  !  NUMBER OF INFECTED STUMPS, THE PROBABILITY OF INITIATION, AND
  !  THE DISTRIBUTION OF THOSE STUMPS (IN THE OLD DISEASE CENTERS).

  !  CALLED BY :
  !     RDCNTL  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDRANN  (FUNCTION)     [ROOT DISEASE]

  !  PARAMETERS :
  !     PROBIN -
  !     PCENTS -
  !     NCENTS -
  !     PROBD  -
  !     SAREA  -
  !     IRFLAG -
  !     NNCENT -
  !     PISIZE -
  !     PAREA  -

  !  Revision History :
  !   12/15/87 - Last revision date.
  !   08/27/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'RDPARM.f90'
  include 'RDADD.f90'

  ! Variables
  integer :: I,INP,IRFLAG,IRRSP,IT,J,NCENTS(ITOTRR),NCELLS,NHASH,NNCENT,NPCELL,NSTUMP

  real    :: ARRAY(75,75),DIMEN,DIST,PAREA(ITOTRR),PCELL,PPCELL,PCENTS(ITOTRR,100,3),PISIZE(ITOTRR),&
       &PROBD(ITOTRR,2,5),PROBIN(ITOTRR,2,5),R,RDRANN,SAREA,TSTUMP,XP,YP,WSTUMP

  logical :: DEBUG

  data NHASH/75/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'RDCENT',6,ICYC)

  if (DEBUG) write (JOSTND,69) PROBIN
  69 format (' IN RDCENT : PROBIN = ',(4E15.7))
  if (DEBUG) write (JOSTND,79) PROBD
  79 format (' IN RDCENT : PROBD = ',(4E15.7))

  DIMEN = SQRT(SAREA) * 208.7

  !     SET UP THE LOCATION OF THE INFECTED AREAS IN THE LOCATION ARRAY

  NSTUMP = 0
  TSTUMP = 0.
  do I = 1, 2
     do J = 1, 5
        TSTUMP = TSTUMP + PROBD(IRRSP,I,J)
        NSTUMP = INT(real(NSTUMP)+PROBD(IRRSP,I,J)*PROBIN(IRRSP,I,J))
        100 continue
     end do
     200 continue
  end do

  WSTUMP = FLOAT(NSTUMP) / (TSTUMP+1E-6)
  NCELLS = 0
  do I = 1, NHASH
     do J = 1, NHASH
        ARRAY(I,J) = 0
        XP = (DIMEN/NHASH) * (I-0.5)
        YP = (DIMEN/NHASH) * (J-0.5)
        if (NCENTS(IRRSP) .eq.0) goto 400

        do IT = 1, NCENTS(IRRSP)
           DIST = SQRT((XP - PCENTS(IRRSP,IT,1))**2 +&
                &(YP-PCENTS(IRRSP,IT,2))**2)
           if (DIST .gt. PCENTS(IRRSP,IT,3)) goto 300
           NCELLS = NCELLS + 1
           ARRAY(I,J) = 1
           goto 400
           300 continue
        end do
        400 continue
     end do
     500 continue
  end do

  if (DEBUG) write(JOSTND,777) NCELLS
  777 format(' IN RDCENT : NCELLS=',I5)

  !     ESTIMATE MEAN NUMBER OF NEW CENTERS PER INFECTED CELL

  if (IRFLAG .eq.1) goto 650
  if (NSTUMP .gt.100) NSTUMP = 100
  PCELL = FLOAT(NSTUMP) / (FLOAT(NCELLS) + 1E-6)
  goto 700

  650 continue
  PCELL = FLOAT(NNCENT) / (FLOAT(NCELLS) + 1E-6)

  700 continue

  !     CALCULATE THE SIZE OF EACH NEW PATCH

  PISIZE(IRRSP) = SQRT(PAREA(IRRSP) * WSTUMP /&
       &(3.14159 * NSTUMP + 1E-6))
  PISIZE(IRRSP) = PISIZE(IRRSP) * 208.7

  if (DEBUG) write(JOSTND,778) IRRSP,PCELL,PISIZE(IRRSP)
  778 format(' RDCENT : IRRSP PCELL PISIZE=',I2,2E15.8)
  if (DEBUG) write(JOSTND,785) NSTUMP,NNCENT,WSTUMP,PAREA(IRRSP)
  785 format(' RDCENT : NSTUMP NNCENT WSTUMP PAREA',2I5,2F15.8)

  !     RANDOMLY ASSIGN DISEASE CENTERS TO CELLS TO OBTAIN COORDINATES

  NCENTS(IRRSP) = 0
  SPPROP(IRRSP) = 0.0

  do I = 1, NHASH
     do J = 1, NHASH
        if (ARRAY(I,J) .eq.0) goto 800
        NPCELL = IFIX(PCELL)
        R = RDRANN(0)
        PPCELL = PCELL - IFIX(PCELL)
        if (R .gt. PPCELL) goto 725
        NPCELL = NPCELL + 1
        if (DEBUG) write(JOSTND,779) NPCELL
        779 format(' RDCENT : NPCELL=',I5)

        725 continue
        if (NPCELL .lt.1) goto 800
        do INP = 1, NPCELL
           if (NCENTS(IRRSP) .ge.100) goto 1000
           NCENTS(IRRSP) = NCENTS(IRRSP) + 1
           R = RDRANN(0)
           PCENTS(IRRSP,NCENTS(IRRSP),1) = (DIMEN/NHASH) * (I-R)
           R = RDRANN(0)
           PCENTS(IRRSP,NCENTS(IRRSP),2) = (DIMEN/NHASH) * (J-R)
           PCENTS(IRRSP,NCENTS(IRRSP),3) = PISIZE(IRRSP)
           ICENSP(IRRSP,NCENTS(IRRSP)) = 0
           750 continue
        end do
        800 continue
     end do
     900 continue
  end do

  if (DEBUG) write(JOSTND,780) NCENTS(IRRSP)
  780 format(' RDCENT : NCENTS=',I5)

  1000 continue
  return
end
