!===================!
!   wpbr : brestb   !
!===================!
! WPBR $Id$
subroutine BRESTB(TIME,ITYP,ISSP)
  implicit none

  !  Purpose:
  !  BRESTB sets the Blister Rust variables for new trees that are
  !  added by the FVS REGENeration system. It performs the following
  !  operations:
  !     1) Assigns initial stock type from parameter ITYP which
  !        is a hard-coded value in the CALL statement in the FVS
  !        subroutine ESTAB.
  !     2) Sets tree age from parameter TIME.
  !     3) Sets all other tree-specific Blister Rust variables,
  !        except "actual" stock type which is determined in
  !        subroutine BRSTYP.  Trees assigned stock type 5 in
  !        this routine, will be assigned stock type 1, 2, 3 or 4
  !        in BRSTYP based on the current stocking mix.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  14-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Implemented blister rust host species index array, ISPBR.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: IIAGO,ITYP,ISSP
  real    :: BRDBH,BRHT,CRLEN,GIBR,TBSUM,TIME

  !.... Is debug requested?

  call DBCHK(DEBUG,'BRESTB',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format('Entering subroutine BRESTB: cycle = ',I2)

  !.... If tree species is not host, no processing is performed.
  !.... If Stock Type is 5, it will be reset in BRSTYP.

  if (BRSPM(ISSP) .gt.0) then
     UPMARK(ITRN) = 10000.0
     RI(ITRN) = 0.0
     BRPB(ITRN) = 0.0
     ESTCAN(ITRN) = 0.0
     ITCAN(ITRN) = 0
     ILCAN(ITRN) = 0
     BRAGE(ITRN) = TIME
     IIAGO = IFIX(TIME)
     ISTOTY(ITRN) = ITYP
     IBRTID(ITRN) = IDTREE(ITRN)
     ICRED(ITRN) = 0
     IBRSTAT(ITRN) = 0
     BRHT = HT(ITRN)*0.3048
     BRDBH = DBH(ITRN)*2.54

     !....    The tree's ground diameter is calculated if the tree
     !....    height is > or = 2.0 meters, otherwise ground diameter
     !....    is DBH is used.

     if (BRHT.lt.2.0) then
        BRGD(ITRN) = BRDBH
     else
        BRGD(ITRN) = (100*BRHT*BRDBH)/(100*(BRHT-1.14))
        if (BRGD(ITRN).lt.BRDBH) BRGD(ITRN) = BRDBH
     end if

     if (ICR(ITRN).gt.0) then

        !....       Crown ratio is supplied; calculate height to base of crown.

        CRLEN = BRHT*(FLOAT(ICR(ITRN))/100.0)
        BRHTBC(ITRN) = (BRHT-CRLEN)*100.0
     else

        !....       Assume the base of crown is at ground level.

        BRHTBC(ITRN) = 0.0
     end if

     !....    Call BRGI to calculate growth index and sum target up to the
     !....    current cycle.

     call BRGI(IIAGO,BRHT,GIBR,TBSUM)
     GI(ITRN) = GIBR
     TSTARG(ITRN) = TBSUM
  end if

  !.... Common return.

  if (DEBUG) write(JOSTND,300) ICYC
  300 format('Leaving subroutine BRESTB: cycle = ',I2)
  return
end
