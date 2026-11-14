!===================!
!   wpbr : brcsta   !
!===================!
! WPBR $Id$
subroutine BRCSTA
  implicit none

  !  Purpose:
  !  BRCSTA determines the status of cankers on each tree during the
  !  initialization cycle.  Status of cankers during cycling is
  !  determined in BRCGRO.

  !  This routine assigns status codes to each canker which is tracked
  !  by the White Pine Blister Rust Model (a maximum of ten cankers/tree).
  !  Descriptions of common variables are in BRCOM.F77.
  !  The canker status codes are:
  !     0 - inactive or no cankers
  !     1 - non-lethal
  !     2 - prunable
  !     3 - excisable
  !     4 - non-salvable
  !     5 - canker has top-killed the tree
  !     6 - (this code is not currently used)
  !     7 - canker has killed the tree

  !  This routine was coded from discussions between John Schwandt,
  !  Jan Savidge, and Lance David during John's visit to MAG for
  !  initial investigation of the White Pine Blister Rust Model.
  !  John drew the canker status assignment flow chart from which
  !  Lance David wrote this code (1990).

  !  Local Variables:

  !     BRDBH  - current tree DBH in centimeters
  !     EXHT   - absolute maximum excise height (cm)
  !     GIRD   - percent of tree CANKER HAS GIRDLED. value 0.0-100.0
  !     HTBCR  - height (cm) to base of crown for current tree.
  !     I1     - (integer) counter
  !     I2     - (integer) counter
  !     I3     - (integer) FVS species index
  !     J      - (integer) counter used to cycle through index array,
  !              IND1, for wpbr host species.
  !     K      - (integer) holds value from IND1 array which identifies
  !              the appropriate array index for the current "tree"
  !     L      - (integer) counter used to cycle through all cankers
  !              for the current tree.
  !     NLCAN  - (integer) number of lethal cankers for a single tree
  !              loaded from array ILCAN.
  !     OUT    - holds current canker's distance out on the branch in
  !              centimeters. loaded from DOUT array.
  !     PRHT   - absolute maximum prune height (cm)
  !     PHTST  - max prune height (cm) for current tree.
  !     UP     - holds current canker's distance up the tree in
  !              centimeters. loaded from DUP array.
  !     EXCNCT - this is a counter for the number of excisable cankers
  !              on the current tree.

  !  Conversion equations:
  !     centimeters = inches * 2.54
  !     centimeters = feet * 30.48

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  14-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 475) and species temp index variable (I3)
  !     are new.
  !  07-MAY-2001 Lance R. David (FHTET)
  !     Added debug code.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'

  !.... Local variable declarations.

  ! Variables
  real    :: BRDBH,EXHT,GIRD,HTBCR,OUT,PHTST,PRHT,UP
  integer :: I1,I2,I3,J,K,L,NLCAN,EXCNCT
  logical :: DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRCSTA',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format('Entering subroutine BRCSTA: cycle = ',I2)

  !.... Set prune and excise maximum height guidelines.  Heights are in
  !.... centimeters.

  PRHT = HTMAX(1)
  EXHT = HTMAX(2)

  if (DEBUG) write(JOSTND,*) ' IN BRCSTA: PRHT=',PRHT,' EXHT=',&
       &EXHT,' GIRMAX=',GIRMAX
  !.... Process host trees. If no trees, return.

  if (ITRN .eq.0) go to 500

  !.... Start species loop

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 475

     if (DEBUG) write(JOSTND,*) ' IN BRCSTA: PROCESSING SPECIES: ',I3

     I1 = ISCT(1,1)
     if (I1.eq.0) go to 475
     I2 = ISCT(1,2)

     !.... Top of tree loop.

     do J = I1,I2
        K = IND1(J)
        NLCAN = ILCAN(K)

        !....    If this tree has no cankers, go to bottom of tree loop.

        if (NLCAN.eq.0) go to 450

        !....    Set local variables:
        !....    Height to base of crown (HTBCR) from Blister Rust
        !....    tree variable (BRHTBC) and trees per acre.

        HTBCR = BRHTBC(K)

        !....    If tree was input as top killed, set value of
        !....    UPMARK to top kill height in centimeters.
        !....    UPMARK is centimeters, ITRUNC is in 1/100s of feet.

        if (ITRUNC(K).gt.0) UPMARK(K) = (FLOAT(ITRUNC(K))/100.0)*30.48

        !....    Set prune height for the tree; prune height is the lesser of
        !....    the absolute maximum pruning height and the proportion of total
        !....    pruning height value.

        PHTST = HTPRPR*HT(K)*30.48
        if (PHTST.gt.PRHT) PHTST = PRHT

        !....    Get the current tree DBH; used in testing the tree for
        !....    excising; convert it to centimeters.

        BRDBH = DBH(K)*2.54

        !....    Initialize the exciseable canker counter and the logical flag
        !....    for multiple excisable cankers on the current tree.

        EXCNCT = 0
        LEXMLT(K) = .false.

        !....    Process all cankers for the tree. Top of canker loop.

        do L = 1,NLCAN
           UP = DUP(L,K)
           OUT = DOUT(L,K)
           GIRD = GIRDL(L,K)

           !....       Assign a status code to the canker.
           !....       NOTE: This same code is used in subroutine BRCGRO to
           !....       assign status codes to cankers after they have been grown.
           !....       Some variable names differ, but the logic is the same.
           !....       DON'T change this code without changing BRCGRO.F77 as well.

           if (OUT.eq.0.0) then

              !....          This is a bole canker.

              if (UP.le.EXHT.and.UP.ge.HTMIN.and.BRDBH.ge.EXDMIN) then

                 !....             Canker is within excisable height range and
                 !....             minimum tree DBH excise threshold.

                 if (GIRD.le.GIRMAX) then

                    !....                Canker is excisable.

                    ISTCAN(L,K) = 3
                 else if (GIRD.ge.GIRMRT) then

                    !....                This canker has girdled enough to kill.

                    if (UP.le.HTBCR) then

                       !....                   The canker is below the crown which kills
                       !....                   the tree.

                       ISTCAN(L,K) = 7
                    else

                       !....                   Top kill.

                       ISTCAN(L,K) = 5
                    end if
                 else

                    !....                Canker is too large to excise, but has not
                    !....                reached the lethal limit GIRMRT.
                    !....                Status is non-salvable.

                    ISTCAN(L,K) = 4
                 end if
              else

                 !....             Canker is not within the excisable height range.

                 if (GIRD.ge.GIRMRT) then

                    !....                Girdling has reached lethal level

                    if (UP.le.HTBCR) then

                       !....                   Canker is below crown; kill tree.

                       ISTCAN(L,K) = 7
                    else

                       !....                   Top kill.

                       ISTCAN(L,K) = 5
                    end if
                 else

                    !....                Canker status non-salvable.

                    ISTCAN(L,K) = 4
                 end if
              end if
           else if (OUT.ge.OUTNLD) then

              !....          Canker far enough out on the branch to be non-lethal.

              ISTCAN(L,K) = 1
           else if (OUT.ge.OUTDST) then

              !....          Canker is far enough out on the branch to be
              !....          pruned, but must also pass pruning height test.

              if (UP.le.PHTST) then

                 !....             Canker is within pruning height - prunable.

                 ISTCAN(L,K) = 2
              else

                 !....             Canker is too high on the tree to prune -
                 !....             non-salvable.

                 ISTCAN(L,K) = 4
              end if
           else

              !....          This is a branch canker, but it is too close to the
              !....          bole for pruning. So, it will be tested against
              !....          excising specifications as if it were a bole canker.
              !....          Branch cankers have no girdling, so only height and
              !....          tree DBH are checked.

              if (UP.le.EXHT.and.UP.ge.HTMIN.and.BRDBH.ge.EXDMIN) then

                 !....             Canker is within excising height; excisable.

                 ISTCAN(L,K) = 3
              else

                 !....             Canker status non-salvable.

                 ISTCAN(L,K) = 4
              end if
           end if

           !....       Determine if there are multiple excisable cankers on the
           !....       current tree.  If so, set LEXMLT to TRUE.  This will be
           !....       used in the routine BRCREM; if excising is scheduled and
           !....       there is more than 1 excisable canker on the tree, then
           !....       no excise will happen at all.

           if (ISTCAN(L,K).eq.3) then
              EXCNCT = EXCNCT+1
              LEXMLT(K) = (EXCNCT.gt.1)
           end if

           !....    End of code in common with BRCGRO.F77.
           !....    Bottom of canker loop.

           if (DEBUG) write(JOSTND,*) ' IN BRCSTA: TREE:',K,' CANKER:',L,&
                &'UP,OUT,GIRD,STAT:',UP,OUT,GIRD,ISTCAN(L,K)
           400 continue
        end do

        !.... Bottom of tree loop.

        450 continue
     end do

     !.... End species loop
     475 continue
  end do

  !.... Common return.

  500 continue
  if (DEBUG) write(JOSTND, 501) ICYC
  501 format('Leaving subroutine BRCSTA: cycle = ',I2)
  return
end
