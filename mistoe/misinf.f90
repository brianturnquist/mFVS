!=====================!
!   mistoe : misinf   !
!=====================!
! MISTOE $Id$
subroutine MISINF

  !  Purpose:
  !     This routine adds dwarf mistletoe infections to a proportion of
  !  trees (by entire tree record) based on parameters input on the
  !  MISTPINF keyword (species, proportion of trees, infection level and
  !  randomness of dispersement). Trees that are already infected or
  !  trees that are not susceptible to dwarf mistletoe will not be
  !  changed by this process.

  !  Call list definitions:
  !     None.

  !  Local variable definitions:
  !     DEBUG:  Logical flag to turn debug on or off.
  !     HTINDX: Temporary storage array for sorting by tree height.
  !     I,J:    Loop counters.
  !     IACTK:  Passed back from OPGET (unused).
  !     IDATE:  Passed back from OPGET (unused).
  !     IDMPTR: A pointer that cycles thru DM ratings from 1 to NEWLEV.
  !     IDMR:   Same as IMIST.
  !     INDX:   Current tree index number from TRINDX.
  !     ISPC:   Species loop counter.
  !     ITREE:  Tree record loop counter.
  !     NEWCNT: Keeps count of TPA with new DM infections by species.
  !     NEWFLG: Log. flag implies at least 1 species w/new DM infection.
  !     NEWINF: Total TPA to have new forced DM infection by species.
  !     NEWLEV: Level of infection to introduce (DMR 1-6) by species.
  !     NEWPRP: Proportion of species to have new forced infection.
  !     NEWTYP: Method of infection to introduce (0=random, 1=hi-lo,
  !                2=lo-hi) by species.
  !     NP:     Passed back from OPGET (unused).
  !     NTODO:  Number of actions to perform in a cycle.
  !     NUTYPE: Method of infection used in this cycle (from NEWTYP).
  !     P:      Same as PROB.
  !     PRM:    Array containing MISPINF keyword values.
  !     SPTPAT: Array containing total # TPA by species.
  !     TEMPTR: Temporary array used to reverse order of TRINDX.
  !     TRINDX: Temporary index used to sort or randomize trees.

  !  Common block variables and parameters:
  !     HT:     From ARRAYS; individual tree record current height.
  !     ICYC:   From CONTRL; cycle index number.
  !     IMIST:  From MISCOM; individual tree record mistletoe rating.
  !     IND1:   From ARRAYS; tree list access.
  !     ISP:    From ARRAYS; current tree species.
  !     ITRN:   From CONTRL; actual number of trees in stand.
  !     IY:     From CONTRL; inventory year.
  !     JOSTND: From CONTRL; unit number of stand output.
  !     MAXSP:  From PRGPRM; maximum number species.
  !     MAXTRE: From PRGPRM; maximum number tree records.
  !     MISFIT: From MISCOM; which species affected by DM (see MISINT).
  !     PROB:   From ARRAYS; trees per acre per tree record.

  ! Revision History:
  !    02-AUG-99; Lance R. David (FHTET-FC)
  !       Added definition and data statements for MYACTS array
  !       and replaced the activity code literal in the CALL OPFIND
  !       statements.
  !       This change was provided by Bob Havis (FMSC) to eliminate
  !       LF95 FORTRAN compiler warnings.
  !    10-MAR-2008  Lance R. David (FHTET)
  !       DMFLAG removed from argument list because it is now in common.

  implicit none

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'MISCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: DEBUG,NEWFLG
  integer :: TRINDX(MAXTRE),TEMPTR(MAXTRE),NEWLEV(MAXSP),NEWTYP(MAXSP),IDMPTR(MAXSP)
  real    :: HTINDX(MAXTRE),NEWPRP(MAXSP),NEWINF(MAXSP),NEWCNT(MAXSP),P,PRM(4),SPTPAT(MAXSP)
  integer :: MYACTS(1)
  integer :: I,I1,I2,I3,IACTK,IDATE,IDMR,INDX,ISPC,J,NP,NTODO,ITREE,NUTYPE

  !.... Data statements.
  data MYACTS(1) / 2006 /

  !.... Check for debug.

  call DBCHK(DEBUG,'MISINF',6,ICYC)

  if (DEBUG) write(JOSTND,100) ICYC
  100 format(' Begin MISINF: Cycle = ',I5)

  !.... Initializations.

  NEWFLG = .false.
  NUTYPE = -1

  !.... These are initializations for the MISPINF keyword values. Since
  !.... MISTPINF is a single cycle type keyword (doesn't carry over to
  !.... subsequent cycles), these values are not in a common block and
  !.... are not initialized in MISINT.

  do I = 1,MAXSP
     NEWPRP(I) = 0.0
     NEWINF(I) = 0.0
     NEWCNT(I) = 0.0
     NEWLEV(I) = 0
     NEWTYP(I) = 0
     SPTPAT(I) = 0.0
     110 continue
  end do

  !.... Process MISTPINF keyword - get species to infect, proportion of
  !.... trees of that species to infect, level of infection, and method
  !.... of infection. NOTE: there can be multiple MISTPINF keywords with
  !.... different species and proportions for a given cycle BUT all must
  !.... use the SAME level and method of infection in a single cycle and,
  !.... if multiple keywords, will default to the first one encountered.
  !.... This is activity code 2006.

  NTODO = 0
  call OPFIND(1,MYACTS(1),NTODO)
  if (NTODO.ne.0) then
     do I = 1,NTODO
        call OPGET(I,4,IDATE,IACTK,NP,PRM)
        call OPDONE(I,IY(ICYC))

        !....       Check for particular species number.
        !....       MISFIT insures that only species of this variant that
        !....        are affected by mistletoe will get MISTPINF values -
        !....        others will be set to 0.

        ISPC = IFIX(PRM(1))
        if (ISPC.ne.0) then
           NEWPRP(ISPC) = PRM(2)*MISFIT(ISPC)
           NEWLEV(ISPC) = INT(PRM(3))*MISFIT(ISPC)
           NEWTYP(ISPC) = INT(PRM(4))*MISFIT(ISPC)
        else

           !....          Otherwise defaults to all species.

           do ISPC = 1,MAXSP
              NEWPRP(ISPC) = PRM(2)*MISFIT(ISPC)
              NEWLEV(ISPC) = INT(PRM(3))*MISFIT(ISPC)
              NEWTYP(ISPC) = INT(PRM(4))*MISFIT(ISPC)
              120 continue
           end do
        end if
        130 continue
     end do
  end if

  do ISPC = 1,MAXSP

     !....    Calculate species mistletoe infection totals.

     I1 = ISCT(ISPC,1)
     if (I1.gt.0) then
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           SPTPAT(ISPC) = SPTPAT(ISPC)+PROB(IND1(I3))
           135 continue
        end do
     end if

     !....    The proportion of trees to infect cannot be less than 0.

     if (NEWPRP(ISPC).lt.0.0) NEWPRP(ISPC) = 0.0

     !....    Check to see if any species will have new infections introduced
     !....    and, if so, translate proportions into TPA by species. Set
     !....    NEWFLG for this routine and DMFLAG to be returned to MISTOE.

     if (NEWPRP(ISPC).gt.0.0) then
        NEWFLG = .true.
        DMFLAG = .true.

        !....       The proportion of trees to infect cannot be greater than 1.

        if (NEWPRP(ISPC).gt.1.0) NEWPRP(ISPC) = 1.0
        NEWINF(ISPC) = SPTPAT(ISPC)*NEWPRP(ISPC)

        !....       Check that the level of infection is between 1 and 6 where:
        !....          1 => all trees infected with DMR 1 (default)
        !....          2 => all trees infected with DMR 1 or 2
        !....          3 => all trees infected with DMR 1, 2, or 3
        !....          4 => all trees infected with DMR 1, 2, 3, or 4
        !....          5 => all trees infected with DMR 1, 2, 3, 4, or 5
        !....          6 => all trees infected with DMR 1, 2, 3, 4, 5, or 6

        if (NEWLEV(ISPC).lt.1.or.NEWLEV(ISPC).gt.6) NEWLEV(ISPC) = 1

        !....       Check that the method of infection is between 0 and 2 where:
        !....          0 => random (default)
        !....          1 => hi to lo (tallest to shortest tree)
        !....          2 => lo to hi (shortest to tallest tree)
        !....       NUTYPE will be set to only the first NEWTYP encountered if
        !....       there is more than one species to be infected this cycle
        !....       (can have only one method of infection per cycle because it
        !....       involves sorting the entire treelist - not by species).

        if (NEWTYP(ISPC).lt.0.or.NEWTYP(ISPC).gt.2) NEWTYP(ISPC) = 0
        if (NUTYPE.eq.-1) NUTYPE = NEWTYP(ISPC)
     end if
     140 continue
  end do

  !.... If NEWFLG is FALSE then no infections to be introduced for any
  !.... species so skip out.

  if (.not.NEWFLG) go to 9000

  !.... Loop through entire tree list to create temporary tree and height
  !.... index to be sorted or randomized (obviously can't sort original
  !.... tree index).

  do ITREE = 1,ITRN
     TRINDX(ITREE) = IND1(ITREE)
     HTINDX(ITREE) = HT(ITREE)
     150 continue
  end do

  if (NUTYPE.eq.1.or.NUTYPE.eq.2) then

     !....    Sort index by tree height from tallest to shortest.

     call RDPSRT(ITRN,HTINDX,TRINDX,.true.)

     if (NUTYPE.eq.2) then

        !....       If shortest to tallest is desired then reverse TRINDX order.

        J = 1
        do I = ITRN,1,-1
           TEMPTR(J) = TRINDX(I)
           J = J+1
           160 continue
        end do

        !....       Copy reverse order temporary array back into TRINDX.

        do I = 1,ITRN
           TRINDX(I) = TEMPTR(I)
           165 continue
        end do
     end if
  else

     !....    This routine will return the first ITRN slots of TRINDX in
     !....    random order.

     call MISRAN(TRINDX,ITRN)
  end if

  !.... Initialize a pointer to cycle through DM ratings from 1 to NEWLEV
  !.... for each species.

  do I = 1,MAXSP
     IDMPTR(I) = 1
     170 continue
  end do

  !.... Loop through trees based on sorted or randomized index (TRINDX).
  !.... Update mistletoe rating if three conditions are satisfied:
  !....    1. that species was targeted for infection
  !....    2. current DMR is equal to 0
  !....    3. total TPA newly infected has not yet exceeded desired
  !....       proportion of TPA to be infected.

  do I = 1,ITRN
     INDX = TRINDX(I)
     ISPC = ISP(INDX)
     P = PROB(INDX)
     IDMR = IMIST(INDX)
     if ((IDMR.eq.0).and.(NEWPRP(ISPC).gt.0.0).and.&
          &((NEWCNT(ISPC)+P).le.NEWINF(ISPC))) then
        IMIST(INDX) = IDMPTR(ISPC)
        NEWCNT(ISPC) = NEWCNT(ISPC)+P

        !....       Increment IDMPTR or set it back to 1 if we've passed NEWLEV.

        IDMPTR(ISPC) = IDMPTR(ISPC)+1
        if (IDMPTR(ISPC).gt.NEWLEV(ISPC)) IDMPTR(ISPC) = 1
     end if
     200 continue
  end do

  !.... Common return.

  9000 continue

  if (DEBUG) write(JOSTND,9010) ICYC
  9010 format(' End MISINF: Cycle = ',I5)

  return
end
