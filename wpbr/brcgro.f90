!===================!
!   wpbr : brcgro   !
!===================!
! WPBR $Id$
subroutine BRCGRO(K,PROP,BRHT,BRHTG,BRDG,NLCAN,HNEW,DNEW)
  implicit none

  !  Purpose:
  !  BRCGRO grows cankers for one year and determines if any active
  !  cankers will become inactive based on the specified inactivation
  !  rate and a random number.  Cankers are also inactivated when:
  !     - a branch or bole canker is located above the actual height
  !       of the tree (current height or topkill height). If new cankers,
  !       in addition to pre-existing cankers, are to be processed,
  !       the actual height should be the height at end of cycle.
  !     - a branch canker is located below the tree's base of crown,
  !       because natural pruning can occur in Prognosis.

  !  New cankers added during the current cycle are not processed
  !  and this is accomplished with the parameter NLCAN.  If you want
  !  to grow and inactivate newly added cankers, the common variable
  !  ILCAN(K) could be used in place of NLCAN.

  !  The processing order in BRCGRO is as follows:
  !     - If canker is inactive, skip to next canker.
  !     - Random number is drawn to determine if the current canker
  !       will be inactivated.  If the canker is inactivated, set
  !       status code and skip to next canker.
  !     - Grow canker.
  !     - Assign canker's status code.
  !     - If canker status is topkill or tree kill, adjust tree
  !       attributes: for tree kill - adjust BRPB(K), WK2(K);
  !       for topkill - adjust UPMARK(K), ITRUNC(K), NORMHT(K), ICRED(K);
  !       (If tree has more than one canker which has caused topkill,
  !       the lowest one is used for the final calculation.)

  !  Note: BRCGRO is not called if a tree is dead already.

  !  Parameters passed:

  !     K      - (integer) holds value from IND1 array which identifies
  !              the appropriate array index for the current "tree"
  !     PROP   - (real) proportion of full cycle represented at current
  !              year.  This is used because current year values are
  !              calculated from values at start of cycle and adding
  !              the appropriate proportion of the growths (height,
  !              diameter). PROP values for yearly processing of a
  !              5 year cycle would be: 1/5, 2/5, 3/5, 4/5, 5/5.
  !     BRHT   - (real) tree height in meters at start of cycle.
  !              If tree is top killed before or during the cycle,
  !              this value is calculated based on the truncation
  !              height in BRTREG.
  !     BRHTG  - (real) tree height growth in meters for this year.
  !              If tree is top killed before or during the cycle,
  !              BRHTG is set to 0 in BRTREG.
  !     BRDG   - (real) tree diameter growth in cm for cycle.
  !     NLCAN  - (integer) number of lethal cankers for a single tree
  !              before new cankers are added.
  !     HNEW   - (real) tree height in feet for this year.  A proportion
  !              of the height growth predicted by FVS has been added.
  !              Top kill is not reflected.
  !     DNEW   - (real) tree DBH in inches for this year.  A proportion
  !              of the diameter growth predicted by FVS has been added.

  !  Local Variables:

  !     BCL    - beginning crown length (i.e. before topkill) in cm.
  !     BRGDY  - tree ground diameter in cm this year.
  !     BRHYR  - tree height in cm this year.
  !     DGPROP - tree diameter growth (cm) for this year.
  !     EXHT   - absolute maximum excise height (cm)
  !     EXPCT  - percent used to calculate excise hgt for each tree
  !     GIRAMT - circumference of tree in cm at canker height.
  !     GIRD   - percent of tree CANKER HAS GIRDLED. value 0.0-100.0
  !     GROBOL - the cankers circumference growth this year.
  !     HNEWCM - (real) value of HNEW converted to centimeters.
  !     DNEWCM - (real) value of DNEW converted to centimeters.
  !     HTBCR  - height (cm) to base of crown for current tree.
  !     HTGPRP - height growth in cm for this year.
  !     JCSTAT - holds current canker's status code.
  !     NCAN   - (integer) used to cycle through cankers.
  !     OUT    - holds current canker's distance out on the branch in
  !              centimeters. loaded from DOUT array.
  !     PCTREM - percent crown remaining after topkill has occurred.
  !     POTST  - this is the maximum distance out (in cm) that a canker
  !              can be on a branch to avoid natural pruning when below
  !              the base of the crown.
  !     PRHT   - absolute maximum prune height (cm)
  !     PRPCT  - percent used to calculate pruning hgt for each tree
  !     PHTST  - max prune height (cm) for current tree.
  !     RCL    - remaining crown length (i.e. after topkill) in cm.
  !     UP     - holds current canker's distance up the tree in
  !              centimeters. loaded from DUP array.
  !     XRAN   - holds random number used to determine if a canker
  !              will become inactive.
  !     EXCNCT - this is a counter for the number of excisable cankers
  !              on the current tree in the current cycle.

  !  Conversion equations:
  !     centimeters = inches * 2.54
  !     centimeters = feet * 30.48

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  16-APR-1999 Lance R. David (FHTET)
  !     Corrected canker inactivation logic that put bole cankers in
  !     double jeopardy.
  !  06-MAR-2001 Lance R. David (FHTET)
  !     Constant growth rate values in growth equations for branch and
  !     bole cankers (5.0 and 4.5 respectively) has been replaced by
  !     variables BRGRTH and BOGRTH.
  !  11-MAY-2001 Lance R. David (FHTET)
  !     Added accumulator for total BR historical mortality (TBRHMR).
  !  16-MAY-2001 Lance R. David (FHTET)
  !     Expanded canker growth rate variables by species and stock type.
  !  07-NOV-2002 Lance R. David (FHTET)
  !     Added condition so that circumference of the stem at the canker
  !     must be greater than the bole canker growth rate with additional
  !     buffer of 25% before bole canker growth is applied.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: JCSTAT,K,NCAN,NLCAN,EXCNCT,I4,I5
  real    :: BRDG,BRHT,BRHTG,HNEW,HNEWCM,DNEW,DNEWCM,PROP,BRGDY,BRHYR,DGPROP,EXHT,GIRAMT,GIRD,GROBOL,&
       &HTBCR,HTGPRP,OUT,PRHT,PHTST,UP,XRAN,BCL,RCL,PCTREM,POTST
  logical :: DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRCGRO',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format ('Entering subroutine BRCGRO: cycle = ',I2)
  if (DEBUG) write(JOSTND,*) ' PARAMETERS=',&
       &K,PROP,BRHT,BRHTG,BRDG,NLCAN,HNEW,DNEW

  !.... Set Blister rust host species and Stock type indices.

  I4 = BRSPM(ISP(K))
  I5 = ISTOTY(K)

  !.... Calculate height thresholds for pruning this tree. Maximum height
  !.... to prune is the lesser value of a maximum absolute height and a
  !.... percentage of the tree's height.
  !.... Heights are in centimeters.

  PRHT = HTMAX(1)
  PHTST = HTPRPR*HNEW*30.48
  if (PHTST.gt.PRHT) PHTST = PRHT

  !.... Set maximum height threshold for excising this tree.  Convert the
  !.... current DBH to centimeters (excising also has a minimum DBH
  !.... threshold).

  EXHT = HTMAX(2)
  DNEWCM = DNEW*2.54

  !.... Set height to base of crown guideline for other excise and prune
  !.... limits used with the heights calculated above to determine canker
  !.... status.

  HTBCR = BRHTBC(K)

  !.... Set pruning distance out guideline (in cm) for whether a branch
  !.... canker below the base of the crown gets naturally pruned; i.e. if
  !.... a branch canker is about to be naturally pruned because it's below
  !.... the base of the crown, if it's within 12 inches of the bole it
  !.... will be changed to a bole canker so it will not be lost.

  POTST = 12*2.54

  !.... Calculate yearly height and ground diameter.

  HTGPRP = BRHTG*PROP*100.0
  BRHYR = (BRHT*100.0)+HTGPRP
  DGPROP = BRDG*PROP
  BRGDY = BRGD(K)+DGPROP

  !.... Calculate beginning crown length this year; if topkill occurs
  !.... in this year then we will use this value to determine the
  !.... remaining crown length - if 75% or greater of the crown gets
  !.... topkilled then the tree will be considered dead.  Beginning
  !.... crown length is the tree height minus height to base of crown.

  BCL = BRHYR-HTBCR

  !.... Initialize the exciseable canker counter and the logical flag
  !.... for multiple excisable cankers on the current tree in the
  !.... current cycle.

  EXCNCT = 0
  LEXMLT(K) = .false.

  !.... Process pre-existing cankers for the current tree.
  !.... New cankers are not grown in the cycle they are added by
  !.... subroutine BRECAN.F77. That is why the canker count is passed
  !.... to BRCGRO as a parameter (NLCAN) to represent number of cankers
  !.... at the start of the current cycle.  Only active cankers are
  !.... are grown.  Active cankers have status codes 0 - 4.

  do NCAN = 1,NLCAN
     JCSTAT = ISTCAN(NCAN,K)
     UP = DUP(NCAN,K)
     OUT = DOUT(NCAN,K)
     GIRD = GIRDL(NCAN,K)

     if (JCSTAT.ne.-1) then
        if (UP.gt.UPMARK(K).or.UP.gt.BRHYR) then

           !....          This canker is located above the actual height of the
           !....          tree.  The status is set to -1 so it will be removed.

           ISTCAN(NCAN,K) = -1
           JCSTAT = -1
        else if (OUT.gt.0.0.and.UP.lt.HTBCR) then

           !....          This branch canker is below the base of crown.

           if (OUT.le.POTST) then

              !....             This branch canker is close enough to the bole that
              !....             we don't want to lose it to natural pruning (the FVS
              !....             maximum of 85% for crown ratio is very hard on low
              !....             branches which is where many of the cankers live in
              !....             real life) so change the close branch canker to a bole
              !....             canker to keep it.  Cankers out beyond this distance
              !....             (12 inches) will be pruned as usual.

              OUT = 0
              DOUT(NCAN,K) = 0
              GIRD = 0
              GIRDL(NCAN,K) = 0
           else

              !....             Natural pruning has occured.  Status is set to -1 for
              !....             removal.

              ISTCAN(NCAN,K) = -1
              JCSTAT = -1
           end if
        end if
     end if

     if (JCSTAT.ge.0.and.JCSTAT.le.4) then

        !....       This is an active canker.  Continue processing.
        !....       Draw a random number to determine if canker will
        !....       become inactive.  Inactivation rate is different for
        !....       branch cankers than for bole cankers.

        call BRANN(XRAN)
        if (OUT.gt.0.0.and.XRAN.lt.RATINV(1)) then

           !....          Branch canker has been inactivated.  Set canker's status
           !....          code and continue to next active canker.

           ISTCAN(NCAN,K) = -1
        else if (OUT.eq.0.0.and.XRAN.lt.RATINV(2)) then

           !....          Bole canker has been inactivated.  Set canker's status
           !....          code and continue to next active canker.

           ISTCAN(NCAN,K) = -1
        else

           !....          Canker is active.  Growth for branch or bole canker
           !....          canker must be calulated.

           if (OUT.gt.0.0) then

              !....             This is a branch canker. Growth is calculated
              !....             and applied as a reduction in the distance out.
              !....             Uncertain how the 5.0 constant was derived, but
              !....             according to Geral Mcdonald and his notes, this
              !....             value is correct.  The distance out is calculated
              !....             based on the canker growing in to meet the bole,
              !....             and the bole growing out to meet the canker.
              !....             Calculate new distance from bole.
              !....             The growth rate constant of 5.0 has been replaced by
              !....             variable BRGRTH.

              OUT = OUT-(BRGRTH(I4,I5)-(0.5*DGPROP))
              if (OUT.lt.0.0) OUT = 0.0
              DOUT(NCAN,K) = OUT
           else

              !....             This is a bole canker. Growth is calculated and
              !....             applied as an increase in the canker's girdle
              !....             percentage.
              !....             GIRAMT is circumference of tree in centimeters
              !....             at height of canker, based on current year
              !....             diameter at ground and normal height (no top kill).
              !....             GROBOL is amount of that circumference which
              !....             will be invaded by one year of canker growth.
              !....             Growth rate constant of 4.5 has been replaced by
              !....             variable BOGRTH.

              if (DEBUG) write(JOSTND,*) ' GROW BOLE CANKER'

              HNEWCM = HNEW*30.48
              GIRAMT = BRPI*BRGDY*((HNEWCM-UP)/HNEWCM)

              !....             07-NOV-2002
              !....             If current stem circumference is less than canker
              !....             growth rate with 25% buffer, do not grow the canker.

              if (GIRAMT .lt. (BOGRTH(I4,I5)*1.25)) then
                 GROBOL = 0.0
              else
                 GROBOL = BOGRTH(I4,I5)-(DGPROP*BRPI)
              end if

              if (GROBOL.lt.0.0) GROBOL = 0.0
              if (GROBOL.gt.GIRAMT) GROBOL = GIRAMT

              !....             Add canker growth percent to canker girdle percent
              !....             for new value of percent girdled. The value can not
              !....             be greater the 100 percent.

              if (DEBUG) write(JOSTND,*) ' GIRD,GROBOL,GIRAMT=',&
                   &GIRD,GROBOL,GIRAMT
              if (GROBOL .eq.0.0.or. GIRAMT .eq.0.0) then
                 continue
              else
                 GIRD = GIRD+((GROBOL/GIRAMT)*100.0)
              end if

              if (GIRD.gt.100.0) GIRD = 100.0
              GIRDL(NCAN,K) = GIRD
           end if

           !....          Assign canker status code.
           !....          NOTE: This same code is used in subroutine BRCSTA to
           !....          assign status codes to cankers after they have been
           !....          grown. Some variable names differ, but the logic is the
           !....          same.  DON'T change this code without changing
           !....          BRCSTA.F77 as well.

           if (OUT.eq.0.0) then

              !....             This is a bole canker.

              if (UP.le.EXHT.and.UP.ge.HTMIN.and.&
                   &DNEWCM.ge.EXDMIN) then

                 !....                Canker is within excisable height range and DBH
                 !....                of the tree is above the minimum for excising.

                 if (GIRD.le.GIRMAX) then

                    !....                   Canker is excisable.

                    ISTCAN(NCAN,K) = 3
                 else if (GIRD.ge.GIRMRT) then

                    !....                   This canker has girdled enough to kill.

                    if (UP.le.HTBCR) then

                       !....                      The canker is below the crown which kills
                       !....                      the tree.

                       ISTCAN(NCAN,K) = 7
                    else

                       !....                      Top kill.

                       ISTCAN(NCAN,K) = 5
                    end if
                 else

                    !....                   Canker is too large to excise, but has not
                    !....                   reached the lethal limit GIRMRT.
                    !....                   Status is non-salvable.

                    ISTCAN(NCAN,K) = 4
                 end if
              else

                 !....                Canker is not within the excisable height and
                 !....                DBH thresholds.

                 if (GIRD.ge.GIRMRT) then

                    !....                   Girdling has reached lethal level.

                    if (UP.le.HTBCR) then

                       !....                      Canker is below crown. Kill the tree.

                       ISTCAN(NCAN,K) = 7
                    else

                       !....                      Top kill.

                       ISTCAN(NCAN,K) = 5
                    end if
                 else

                    !....                   Canker status non-salvable.

                    ISTCAN(NCAN,K) = 4
                 end if
              end if
           else if (OUT.ge.OUTNLD) then

              !....             Canker far enough out on the branch to be non-lethal.

              ISTCAN(NCAN,K) = 1

           else if (OUT.ge.OUTDST) then

              !....             Canker is far enough out on the branch to be
              !....             pruned, but must also pass pruning height test.

              if (UP.le.PHTST) then

                 !....                Canker is within pruning height - prunable.

                 ISTCAN(NCAN,K) = 2
              else

                 !....                Canker is too high on the tree to prune -
                 !....                non-salvable.

                 ISTCAN(NCAN,K) = 4
              end if
           else

              !....             This is a branch canker, but it is too close to the
              !....             bole for pruning. So, it will be tested against
              !....             excising specifications as if it were a bole canker.
              !....             Branch cankers have no girdling, so only height and
              !....             minimum DBH are checked.

              if (UP.le.EXHT.and.UP.ge.HTMIN.and.&
                   &DNEWCM.ge.EXDMIN) then

                 !....                Canker is within excising height and tree DBH
                 !....                threshold; excisable.

                 ISTCAN(NCAN,K) = 3
              else

                 !....                Canker status non-salvable.

                 ISTCAN(NCAN,K) = 4
              end if
           end if

           !....          End of code in common with BRCSTA.F77.
           !....          Adjust tree attributes as indicated by new status code.

           if (ISTCAN(NCAN,K).eq.5) then

              !....             Adjust for topkill.
              !....             Determine if this is the lowest status 5 canker
              !....             on the tree.  The lowest topkilling canker is
              !....             used to calculate ITRUNC.  ITRUNC equation adds 1.0
              !....             to force rounding up because the real number is
              !....             truncated when stored in the integer variable.
              !....             NORMHT is calculated as the normal height of the
              !....             tree if topkill had not occurred.  FVS variables
              !....             ITRUNC, NORMHT are in 1/100s of feet, HT is in feet.
              !....             Setting ICRED to 1 signals that this tree's crown
              !....             needs to be reduced in subroutine BRCRED.

              if (UP.lt.UPMARK(K)) then
                 UPMARK(K) = UP
                 ITRUNC(K) = ((UP/30.48)*100.0)+1.0
                 if (NORMHT(K).le.0.0) NORMHT(K) = (HT(K)*100.0)+0.5
                 ICRED(K) = 1
              end if

              !....             Determine if this topkill canker has left the tree
              !....             with 25% or less of the beginning crown length; i.e.
              !....             if 75% of the crown is lost to topkill, then kill the
              !....             tree. Remaining crown length is: height to the lowest
              !....             topkill canker minus height to base of crown.  Percent
              !....             remaining crown is: remaining crown length divided by
              !....             beginning crown length.

              RCL = UP-HTBCR
              PCTREM = RCL/BCL*100.0
              if (PCTREM.le.25.0) then

                 !....                Kill the tree.
                 !....                Adjust mortality arrays.
                 !....                Add to accumulator for total historical mortality
                 !....                for the host species.
                 !....                Set canker and tree status codes.
                 !....                Skip out of the loop.

                 WK2(K) = PROB(K)*0.99999
                 BRPB(K) = BRPB(K)+PROB(K)
                 TBRHMR(I4) = TBRHMR(I4)+PROB(K)
                 ISTCAN(NCAN,K) = 7
                 IBRSTAT(K) = 7
                 go to 500
              end if
           else if (ISTCAN(NCAN,K).eq.7) then

              !....             Adjust for tree kill.
              !....             The canker has girdled the tree below the base
              !....             of the crown. When a status code 7 canker is
              !....             encountered for the tree, the loop for processing
              !....             all cankers for the current tree is exited.
              !....             There is no need to process remaining cankers
              !....             for a tree that has already been killed.
              !....             Load WK2 with new mortality value and BRPB
              !....             with blister rust mortality.
              !....             Add to accumulator for total historical mortality
              !....             for the host species.
              !....             Set tree status to 7 indicate its death so it
              !....             will not be processed for following years, if any,
              !....             for the current cycle.

              if (UP.le.HTBCR) then
                 WK2(K) = PROB(K)*0.99999
                 BRPB(K) = BRPB(K)+PROB(K)
                 TBRHMR(I4) = TBRHMR(I4)+PROB(K)
                 IBRSTAT(K) = 7
                 go to 500
              end if
           end if
        end if
     end if

     !....    Determine if there are multiple excisable cankers on the
     !....    current tree for the current cycle.  If so, set LEXMLT
     !....    to TRUE.  This will be used in the routine BRCREM; if
     !....    excising is scheduled and there is more than 1 excisable
     !....    canker on the tree, then no excise will happen at all.

     if (ISTCAN(NCAN,K).eq.3) then
        EXCNCT = EXCNCT+1
        LEXMLT(K) = (EXCNCT.gt.1)
     end if
     400 continue
  end do

  !.... Common return.

  500 continue
  if (DEBUG) write(JOSTND,501) ICYC
  501 format ('Leaving subroutine BRCGRO: cycle = ', I2)
  return
end
