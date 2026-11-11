!===================!
!   wpbr : brsetp   !
!===================!
! WPBR $Id$
subroutine BRSETP
  implicit none

  !  Purpose:
  !  BRSETP initializes the Blister Rust Model tree-specific variables
  !  after FVS tree data and blister rust canker data has been read.
  !  It is called from MAIN.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  14-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 110) and species temp index variable (I3)
  !     are new.
  !  02-APR-2001 Lance R. David (FHTET)
  !     Deactivated initialization of stock type (ISTOTY) because it is
  !     now a canker input file variable.
  !  02-MAY-2001 Lance R. David (FHTET)
  !     Added species dimension to BRNTRECS variable.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM. Instead of just being and indicator of a
  !     species being a host, BRSPM holds the array index value for that
  !     species and is used to access all species-specific BR arrays.

  !.... Common include files

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !.... Local variable declarations

  ! Variables
  integer :: I1,I2,I3,I4,J,K
  real    :: BRDBH,BRHT
  logical :: DEBUG,BRGO

  !.... See if we need to do some debug

  call DBCHK (DEBUG,'BRSETP',6,ICYC)
  if (DEBUG) write(JOSTND,111) ICYC
  111 format('Entering subroutine BRSETP: cycle = ',I2)

  !.... Call BRATV to see if the Blister Rust model is being used in this
  !.... simulation. Return if not active or there are no trees.

  call BRATV(BRGO)
  if (.not. BRGO .or. ITRN .eq.0) go to 115

  !.... Process host pines. If none then return.
  !.... Start species loop

  do I3 = 1, MAXSP
     if (BRSPM(I3) .eq.0) go to 110
     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 110
     I2 = ISCT(I3,2)

     !.... Set blister rust species index
     I4 = BRSPM(I3)
     BRNTRECS(I4) = I2-I1+1

     do J = I1,I2
        K = IND1(J)

        !....    Change height to meters, DBH to centimeters, calculate ground
        !....    level diameter, and calculate height to base of crown in cm.
        !....    If tree age not recorded, then assign stand age as tree age.
        !....
        !....    Assign stock type of existing inventory trees to 5 so they
        !....    will be treated as planted trees during the assignment of
        !....    stock types in subroutine BRSTYP.
        !....
        !....    Stock type is now a variable in the canker list input file.
        !....    but still must be initialized here for the host trees that
        !....    do not have a corresponding record in the canker file.
        !....    (LRD 04/02/01)

        BRHT = HT(K)*0.3048
        BRDBH = DBH(K)*2.54
        BRGD(K) = (100*BRHT*BRDBH)/(100*(BRHT-1.14))
        if (BRGD(K).lt.BRDBH) BRGD(K) = BRDBH
        if (BRAGE(K).eq.0.0) BRAGE(K) = IAGE
        if (ISTOTY(K).eq.0) ISTOTY(K) = 5
        BRHTBC(K) = (BRHT-(BRHT*(FLOAT(ICR(K))/100.0)))*100.0

        !....    Initialize canker and infection conditions for each tree
        !....    based on canker counts read from canker data if requested
        !....    on the CANKDATA keyword.

        if (CKINIT) call BRCINI(K,BRHT)
        100 continue
     end do

     !.... End of species loop
     110 continue
  end do

  !.... Assign stock types to trees.
  !.... Any trees that were not assigned a stock type via the canker
  !.... input file will be rectified.

  call BRSTYP

  !.... Assign status to cankers.

  call BRCSTA

  !.... Calculate statistics.

  call BRSTAT

  !.... Reduce crowns of trees that have been girdled by cankers.

  call BRCRED

  !.... Calculate STAND RUST INDEX using basal area if specified by
  !.... the rust index assignment method.

  if (RIMETH.ne.0) call BRIBA

  !.... Calculate STAND TARGET, individual tree GROWTH INDEX,
  !.... individual tree RUST INDEX, and STAND DEVIATION FACTOR.

  call BRTARG

  !.... Common return.

  115 continue
  if (DEBUG) write(JOSTND,120) ICYC
  120 format('Leaving subroutine BRSETP: cycle = ',I2)
  return
end
