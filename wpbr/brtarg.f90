!===================!
!   wpbr : brtarg   !
!===================!
! WPBR $Id$
subroutine BRTARG
  implicit none

  !  Purpose:
  !  BRTARG calculates STAND TARGET, individual tree GROWTH INDEX,
  !  individual tree RUST INDEX, and STAND DEVIATION FACTOR and a
  !  RUST INDEX ADJUSTMENT FACTOR.  Called during initialization.

  !  * * * Special note regarding 15-MAR-2001 update * * *
  !  Given the current method in which a new deviation factor is
  !  calculated, only species is isolated and the new DFACT value is
  !  calculated using all stock types for the species and assigned to
  !  stock type 1 (wild stock) of that species, stock types 2-4 remain
  !  unchanged.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  20-APR-1999 Lance David
  !     Added debug statement to code that calculates the stand deviation
  !     factor (variable DFACT).
  !     The Rust Index Adjustment Factor (RIAF) calculation did not allow
  !     for the possibility of no cankers resulting in a adjustment factor
  !     of zero which caused the individual tree RI to be set to zero.
  !     Added check that set RIAF to 1.0 if calculated RIAF is 0.0.
  !  14-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 90) and species temp index variable (I3)
  !     are new.
  !  14-DEC-2000 Lance David (FHTET)
  !     Added code to skip calculation of RIAF when RIMETH = 0 and
  !     initial cankers are present at initialization.
  !  15-MAR-2001 Lance R. David (FHTET)
  !     Stand Deviation Factor variable (DFACT) updated to array by
  !     species and stock type. Given the method that a new deviation
  !     factor is calculated at this time, only species is isolated and
  !     the new DFACT value is assigned to stock type 1 (wild stock),
  !     stock types 2-4 remain unchanged.
  !  01-MAY-2001 Lance R. David (FHTET)
  !     Change stand level/averages variables to arrays for handling of
  !     multiple host species treated independently.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM. Instead of just being and indicator of a
  !     species being a host, BRSPM holds the array index value for that
  !     species and is used to access all species-specific BR arrays.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: I1,I2,I3,I4,IIAG,ICLS,L,LL,M,MM
  real    :: ACTRI,ARI,ARISUM,DFD,DFN,GIBR,GISUM,HHT,RISUM,TBSUM
  logical :: DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRTARG',6,ICYC)
  if (DEBUG) write(JOSTND,11) ICYC
  11 format('Entering subroutine BRTARG: cycle = ',I2)

  !.... If no tree records, then return.

  if (ITRN.eq.0) go to 100

  !.... Process hosts trees in the treelist. If none, then return.

  !.... Start species loop

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 90
     !.... Set blister rust species index
     I4 = BRSPM(I3)

     !.... Initializations.

     RISUM = 0.0
     GISUM = 0.0
     ARISUM = 0.0

     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 90
     I2 = ISCT(I3,2)
     do L = I1,I2
        M = IND1(L)

        !....    skip trees with no age.
        if (BRAGE(M) .eq.0) go to 30
        IIAG = IFIX(BRAGE(M))
        HHT = HT(M)*0.3048

        !....    Call BRGI to calculate growth index and sum target.

        call BRGI(IIAG,HHT,GIBR,TBSUM)
        GI(M) = GIBR
        GISUM = GISUM+GI(M)
        TSTARG(M) = TBSUM
        STSUM(I4) = STSUM(I4)+TBSUM

        !....    Calculate an actual rust index at initialization based on
        !....    # cankers/target area for each tree.  This value will be
        !....    used as an adjustment factor to the model rust index (RIDEF).

        if (TSTARG(M).gt.0.0) then
           ARI = ITCAN(M)/TSTARG(M)
        else
           ARI = 0.0
        end if
        ARISUM = ARISUM+ARI
        30 continue
     end do

     !.... Calculate the average actual rust index (cankers/target area)
     !.... at initialization and the rust index adjustment factor.
     !.... If there were no canker provided at initialization, this value
     !.... can not be calculated and will be set to 1.0 (no adjustment).
     !.... Do not calculate the adjustment factor when the user specifies
     !.... a rust index value to use, even if there are initial cankers.

     if (RIMETH .eq.0.and. ICYC .eq.0) then
        continue
     else
        ACTRI = ARISUM/FLOAT(BRNTRECS(I4))
        RIAF(I4) = ACTRI/RIDEF
        if (RIAF(I4) .eq.0.0) RIAF(I4) = 1.0
     end if

     !.... Loop through the trees again to apply the rust index adjustment
     !.... factor.

     do LL = I1,I2
        MM = IND1(LL)

        !....    skip trees with no age.
        !+++++++ IF (BRAGE(MM) .EQ. 0) GO TO 35

        !....    Calculate rust index. Modify default value by resistance factor
        !....    for the stock type of this tree and multiply by the rust index
        !....    adjustment factor.

        ICLS = ISTOTY(MM)
        RI(MM) = RIDEF*RESIST(I4,ICLS)*RIAF(I4)
        RISUM = RISUM+RI(MM)
        35 continue
     end do

     !.... Calculate average rust index, growth index, and sum target.

     AVGRI(I4) = RISUM/FLOAT(BRNTRECS(I4))
     AVGGI(I4) = GISUM/FLOAT(BRNTRECS(I4))
     AVGSTS(I4) = STSUM(I4)/FLOAT(BRNTRECS(I4))

     !.... Calculate stand deviation factor.
     !.... If a stand deviation factor was supplied by keyword, that
     !.... value will be used.
     !.... If stand contains no cankers or if stand age < 10 years,
     !.... the default stand deviation factor will be used.
     !.... Otherwise, a stand deviation factor will be calculated.
     !.... Stand deviation factor cannot be less than 0.01.

     if (LDFACT) then

        !....    The deviation factor was supplied by keyword. Use it.

        if (DEBUG) then
           write(JOSTND,40)
           40 format('BRTARG: deviation factor supplied by keyword.')
        end if

     else if (INCAN.gt.0.or.IAGE.ge.10) then

        !....    Calculate stand deviation factor.
        !....    If a new deviation factor can not be calculated due to a zero
        !....    denominator, DFACT will remain unchanged.
        !....    Variable PITCA has a 0.999 upper limit imposed in subroutine BRSTAT
        !....    so this log function will not error.

        DFN = AVTCPT(I4)-(-ALOG(1.0-PITCA(I4)))
        DFD = AVTCPT(I4)*(-ALOG(1.0-PITCA(I4)))

        if (DFD.gt.0.0) then
           DFACT(I4,1) = DFN/DFD
        end if

        if (DEBUG) then
           write(JOSTND,50) DFN, DFD, DFACT(I4,1), I4
           50 format('BRTARG: deviation factor calculated. DFN=',F12.6,&
                &' DFD=',F12.6,' DFACT=',F12.6,' SP=',I4)
        end if
     end if

     !.... Minimum stand deviation factor allowed regardless of being provided
     !.... by keyword, or calculation.

     if (DFACT(I4,1).lt.0.01) then
        DFACT(I4,1) = 0.01
        if (DEBUG) write(JOSTND,*) ' RESET MIN DFACT=', DFACT(I4,1)
     end if

     !.... End species loop
     90 continue
  end do

  !.... Common return.

  100 continue
  if (DEBUG) write(JOSTND,101) ICYC
  101 format('Leaving subroutine BRTARG: cycle = ',I2)
  return
end
