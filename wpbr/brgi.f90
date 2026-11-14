!=================!
!   wpbr : brgi   !
!=================!
! WPBR $Id$
subroutine BRGI(IIAG,HHT,GIBR,TBSUM)
  implicit none

  !  Purpose:
  !  BRGI calculates the growth index and sum target for individual white
  !  pine trees (currently used for all species).
  !  Minimum age of tree is set at 2 years for GI calculation.

  !  IIAG - Tree age
  !  HHT  - Tree height in meters
  !  GIBR - Growth Index function result
  !  TBSUM - Target area

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  14-SEP-2000 Lance David (FHTET)
  !     No changes actually made to this routine that calculates growth
  !     index for white pine trees. The July, 2000 update to expand the
  !     model to function on additional pine species did not address the
  !     differences between species; therefore, it will likely be
  !     necessary to develop species-specific sections and add species to
  !     the argument list. FVS variant-specific versions of this may also
  !     be necessary or desirable for future development.
  !  28-FEB-2001 Lance R. David (FHTET)
  !     Minimum tree age used in GI calculation set at 2 years.
  !     Explicitly defined variables.
  !  22-MAR-2001 Lance R. David (FHTET)
  !     Added debug statment. Removed "0.0" preceding negative values in
  !     GI functions. Don't know why they were there in the first place,
  !     but may have been needed for some earlier version compiler.
  !  03-MAY-2001 Lance R. David (FHTET)
  !     Disabled stand target area summing because it was already being
  !     done in BRTREG and BRTARG.
  !  07-NOV-2002 Lance R. David (FHTET)
  !     Added max and min limits of 50 and 125 on growth index GIBR.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: IIAG,GIAGE,K
  real    :: HHT,GIBR,GIBRK,HITE,SVAL,TBSUM

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRGI',4,ICYC)
  if (DEBUG) write(JOSTND,111) ICYC,IIAG,HHT
  111 format('Entering subroutine BRGI: cycle = ',I2,/,&
       &27X,'IIAG=',I3,' HHT=',F6.2)

  TBSUM = 0.0

  !.... Calculate growth index.  This equation taken from the abstract:
  !.... "Measuring Early Performance of Second Generation Resistance to
  !.... Blister Rust in White Pine"; Geral McDonald, et.al.
  !.... Per Geral's recommendation, minimum age is set at 2 years.

  if (IIAG .le.2) then
     GIAGE = 2
  else
     GIAGE = IIAG
  end if

  GIBR = 0.466*(HHT-0.05)*&
       &((1-(1.024494*EXP((-0.024202)*GIAGE)))**(-2.071822))

  !.... Min (15.24m = 50ft*.3048) and max 38.10m = 125ft limits imposed
  !.... on growth index. feet to meter conversion is m=ft*0.3048.

  if (GIBR .lt.15.24) GIBR = 15.24
  if (GIBR .gt.38.10) GIBR = 38.10

  if (DEBUG) write(JOSTND,*) ' IN BRGI: GIBR=',GIBR
  !.... Calculate stand target area.

  do K = 1,IIAG
     GIBRK = 0.466*((1-(1.024494*EXP((-0.024202)*K)))**&
          &(-2.071822))
     HITE = 0.05+GIBR/GIBRK

     call BRSTAR(HITE,SVAL)
     !****    STSUM=STSUM+SVAL        !!! already occuring in BRTREG and BRTARG
     TBSUM = TBSUM+SVAL
     40 continue
  end do

  !.... Common return.

  if (DEBUG) write(JOSTND,113) ICYC
  113 format('Leaving subroutine BRGI: cycle = ',I2)
  return
end
