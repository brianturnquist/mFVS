!===================!
!   wpbr : brcred   !
!===================!
! WPBR $Id$
subroutine BRCRED
  implicit none

  !  Purpose:
  !  BRCRED reduces the crowns of white pines that have been girdled
  !  by Blister Rust at initialization from canker data provided or
  !  during the current growth cycle.
  !  Reduction in crowns during the initialization phase of FVS tree data
  !  is accomplished in CROWN.
  !  This subroutine is called by:
  !     BRSETP
  !     BRTREG

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  11-MAY-1999 Lance David
  !     Added debug for calculation of new crown ratio.
  !  13-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 95) and species temp index variable (I3)
  !     are new.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: I,I1,I2,I3,ICCR,IICI,K
  real    :: CL,HD,HN
  logical :: DEBUG,BRGO

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRCRED',6,ICYC)
  if (DEBUG) write(JOSTND,111) ICYC
  111 format('Entering subroutine BRCRED: cycle = ',I2)

  !.... If the Blister Rust model is not active, then return.

  call BRATV(BRGO)
  if (.not.BRGO) go to 100

  !.... Process host species. If no trees, return.

  if (ITRN .eq.0) go to 100

  !.... Start species loop

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 95

     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 95
     I2 = ISCT(I3,2)
     do K = I1,I2
        I = IND1(K)
        ICCR = ICR(I)

        !....    If tree is dead, go to the next one.

        if (IBRSTAT(I).eq.7) go to 90

        !....    If tree is not girdled, go to the next one.

        if (ITRUNC(I).eq.0.or.ICRED(I).eq.0) go to 90

        !....    Reduce crowns of trees flagged as top-killed.
        !....    NORMHT and ITRUNC are integer representing 1/100s feet.
        !....    Min and Max crown ratio is set to 5 and 95 percent.
        !....    HN is normal height (feet)
        !....    HD is length of top that is dead (feet)

        HN = NORMHT(I)/100.0
        HD = HN-(ITRUNC(I)/100.0)
        CL = (FLOAT(ICCR)/100.)*HN-HD
        IICI = IFIX((CL*100./HN)+.5)
        if (IICI.lt.5) IICI = 5
        if (IICI.gt.95) IICI = 95
        ICR(I) = IICI
        ICRED(I) = 0

        !....    Write debug information.
        if (DEBUG) then
           write (JOSTND,85) I,IDTREE(I),NORMHT(I),ITRUNC(I),CL,&
                &ICCR,ICR(I)
           85 format ('I=',I4,' IDTREE=',I5,' NORMHT=',I5,' ITRUNC=',I5,&
                &' CL=',F5.1,' OLDCR=',I3,' NEWCR=',I3)
        end if
        90 continue
     end do

     !.... End species loop
     95 continue
  end do

  !.... Common return.

  100 continue
  if (DEBUG) write(JOSTND,114) ICYC
  114 format('Leaving subroutine BRCRED: cycle = ',I2)
  return
end
