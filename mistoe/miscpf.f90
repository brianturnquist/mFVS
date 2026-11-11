!=====================!
!   mistoe : miscpf   !
!=====================!
! MISTOE $Id$
subroutine MISCPF(PRFCUT)

  !  Purpose:
  !     Processes the MISTPREF mistletoe cutting preference keyword.

  !  Call list definitions:
  !     PRFCUT: (O) Array containing cutting preference values by species
  !                and mistletoe rating, as input on MISTPREF keyword.

  !  Local variable definitions:
  !     DEBUG:  Logical flag to turn debug on or off.
  !     I:      Loop counter.
  !     IACTK:  Passed back from OPGET (unused).
  !     IDATE:  Passed back from OPGET (unused).
  !     IDMR:   Loop index for DM rating.
  !     ISPC:   Current species number.
  !     NP:     Passed back from OPGET (unused).
  !     NTODO:  Number of actions to perform in a cycle.
  !     PRMS:   Array containing MISPREF keyword values.

  !  Common block variables and parameters:
  !     ICYC:   From CONTRL; cycle index number.
  !     IY:     From CONTRL; inventory year.
  !     JOSTND: From CONTRL; unit number of stand output.
  !     MAXSP:  From PRGPRM; maximum number of species.
  !     MISFIT: From MISCOM; which species affected by DM (see MISINT).
  !     PRFMST: From MISCOM; mistletoe cutting preference values.

  ! Revision History:
  !     02-AUG-99; Lance R. David (FHTET-FC)
  !        Added definition and data statements for MYACTS array
  !        and replaced the activity code literal in the CALL OPFIND
  !        statements.
  !        This change was provided by Bob Havis (FMSC) to eliminate
  !        LF95 FORTRAN compiler warnings.

  implicit none

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'MISCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: DEBUG
  real    :: PRFCUT(MAXSP,6),PRMS(7)
  integer :: MYACTS(1)
  integer :: I,IACTK,IDATE,IDMR,ISPC,NP,NTODO

  !.... Data statements.

  data MYACTS(1) / 2002 /

  !.... Check for debug.

  call DBCHK(DEBUG,'MISCPF',6,ICYC)

  if (DEBUG) write(JOSTND,100) ICYC
  100 format(' Begin MISCPF: Cycle = ',I5)

  !.... Process MISTPREF keyword (activity code 2002) if scheduled.

  call OPFIND(1,MYACTS(1),NTODO)
  if (NTODO.gt.0) then
     do I = 1,NTODO
        call OPGET(I,7,IDATE,IACTK,NP,PRMS)
        call OPDONE(I,IY(ICYC))

        !....       Check for a particular species number.
        !....       MISFIT insures that only species of this variant that
        !....        are affected by mistletoe will get pref. multipliers -
        !....        others will be set to 0.

        ISPC = IFIX(PRMS(1))
        if (ISPC.ne.0) then
           do IDMR = 1,6
              PRFMST(ISPC,IDMR) = PRMS(IDMR+1)*MISFIT(ISPC)
              120 continue
           end do
        else

           !....          Otherwise defaults to all species.

           do ISPC = 1,MAXSP
              do IDMR = 1,6
                 PRFMST(ISPC,IDMR) = PRMS(IDMR+1)*MISFIT(ISPC)
                 130 continue
              end do
              140 continue
           end do
        end if
        150 continue
     end do
  end if

  !.... Copy values from PRFMST to PRFCUT; PRFMST is saved in a common
  !.... block so that cutting preference values remain in effect until
  !.... a subsequent MISTPREF keyword changes the values and therefore
  !.... subroutine CUTS can use PRFCUT for every cycle.

  if (DEBUG) write(JOSTND,9010) (I, I = 1,6)
  9010 format(' Species/DMR ', 6I10)

  do ISPC = 1,MAXSP
     do IDMR = 1,6
        PRFCUT(ISPC,IDMR) = PRFMST(ISPC,IDMR)
        160 continue
     end do

     if (DEBUG) write(JOSTND,9020) ISPC,(PRFCUT(ISPC,I), I = 1,6)
     9020 format(I5, 9X, 6F10.2)

     170 continue
  end do

  !.... Common return.

  if (DEBUG) write(JOSTND,9040) ICYC
  9040 format(' End MISCPF: Cycle = ',I5)

  return
end
