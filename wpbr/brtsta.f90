!===================!
!   wpbr : brtsta   !
!===================!
! WPBR $Id$
subroutine BRTSTA
  implicit none

  !  Purpose:
  !  BRTSTA determines the status of each tree based on the worst
  !  canker for the tree.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  15-SEP-2000 Lance R. David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 53) and species temp index variable (I3)
  !     are new.
  !  16-MAR-2001 Lance R. David (FHTET)
  !     Corrected tree status (ITSTAT) assignment so that lowest status
  !     for trees that have a total canker count (ITCAN) greater than 0
  !     is 1 (non-lethal).
  !  21-MAR-2001 Lance R. David (FHTET)
  !     Added handling of escape tree status code 9. Once a tree is tagged
  !     as an escape/reserve tree, it will never become infected.
  !  01-MAY-2001 Lance R. David (FHTET)
  !     Expanded tree category scalar variables to arrays.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM. Instead of just being and indicator of a
  !     species being a host, BRSPM holds the array index value for that
  !     species and is used to access all species-specific BR arrays.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: I1,I2,I3,I4,II,J,KT,MAXNO,NTIM
  logical :: DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRTSTA',6,ICYC)
  if (DEBUG) write(JOSTND,20) ICYC
  20 format('Entering subroutine BRTSTA: cycle = ',I2)

  !.... If no trees, return.
  if (ITRN .eq.0) go to 55

  !.... Process host pines in the treelist.
  !.... Start species loop

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 53
     !.... Set blister rust species index
     I4 = BRSPM(I3)

     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 53
     I2 = ISCT(I3,2)
     do J = I1,I2
        KT = IND1(J)

        !....    Initialize worst canker variable.
        !....    Minimum status for trees with any cankers is 1 non-lethal.

        if (ITCAN(KT) .gt.0) then
           MAXNO = 1
        else
           MAXNO = 0
        end if

        !....    Loop through cankers for the tree and find the worst one.
        !....    Canker categories: 0=clean, 1=non-lethal, 2=prunable,
        !....    3=excisable, 4=non-salvable or lethal, 5=topkilled,
        !....    7=tree killed.

        do NTIM = 1,ILCAN(KT)
           if (ISTCAN(NTIM,KT).gt.MAXNO) then
              MAXNO = ISTCAN(NTIM,KT)
           end if
           30 continue
        end do

        !....    Trees are not killed/topkilled on initialization.
        !....    Highest status code possible is 4 (non-salvable).
        !....    Status codes of 9 for reserved excape trees are not changed.
        if (ICYC.eq.0.and.MAXNO.gt.4) MAXNO = 4
        if (IBRSTAT(KT).ne.9) IBRSTAT(KT) = MAXNO

        !....    Add tree count to total number in that category. If tree has
        !....    lethal cankers then reset tree class code.

        if (IBRSTAT(KT).eq.0.or. IBRSTAT(KT).eq.9) then

           !....       Tree is clean (no cankers) or reserved escape, add count

           TBRCLN(I4) = TBRCLN(I4)+PROB(KT)

           !....       Loop through 2-inch DBH categories; add to the total for
           !....       clean trees; if DBH not < 19", must be => 19"

           do II = 1,9
              if (DBH(KT).lt.(FLOAT(II*2+1))) then
                 D2CLN(II) = D2CLN(II)+PROB(KT)
                 go to 32
              end if
              31 continue
           end do
           D2CLN(10) = D2CLN(10)+PROB(KT)
           32 continue
        else if (IBRSTAT(KT).eq.1) then

           !....       Status of worst canker on tree is non-lethal, add count

           TBRNOL(I4) = TBRNOL(I4)+PROB(KT)

           !....       Loop through 2-inch DBH categories; add to the total for
           !....       non-lethal trees; if DBH not < 19", must be > 19"

           do II = 1,9
              if (DBH(KT).lt.(FLOAT(II*2+1))) then
                 D2NOL(II) = D2NOL(II)+PROB(KT)
                 go to 34
              end if
              33 continue
           end do
           D2NOL(10) = D2NOL(10)+PROB(KT)
           34 continue
        else if (IBRSTAT(KT).eq.2) then

           !....       Status of worst canker on tree is prunable, add count

           TBRPRN(I4) = TBRPRN(I4)+PROB(KT)

           !....       Loop through 2-inch DBH categories; add to the total for
           !....       prunable trees; if DBH not < 19", must be > 19"

           do II = 1,9
              if (DBH(KT).lt.(FLOAT(II*2+1))) then
                 D2PRN(II) = D2PRN(II)+PROB(KT)
                 go to 36
              end if
              35 continue
           end do
           D2PRN(10) = D2PRN(10)+PROB(KT)
           36 continue
        else if (IBRSTAT(KT).eq.3) then

           !....       Status of worst canker on tree is excisable, add count

           TBREXC(I4) = TBREXC(I4)+PROB(KT)

           !....       Loop through 2-inch DBH categories; add to the total for
           !....       excisable trees; if DBH not < 19", must be > 19"

           do II = 1,9
              if (DBH(KT).lt.(FLOAT(II*2+1))) then
                 D2EXC(II) = D2EXC(II)+PROB(KT)
                 go to 38
              end if
              37 continue
           end do
           D2EXC(10) = D2EXC(10)+PROB(KT)
           38 continue
        else if (IBRSTAT(KT).eq.4) then

           !....       Status of worst canker on tree is non-salvable, add count

           TBRNOS(I4) = TBRNOS(I4)+PROB(KT)

           !....       Loop through 2-inch DBH categories; add to the total for
           !....       non-salvable trees; if DBH not < 19", must be > 19"

           do II = 1,9
              if (DBH(KT).lt.(FLOAT(II*2+1))) then
                 D2NOS(II) = D2NOS(II)+PROB(KT)
                 go to 40
              end if
              39 continue
           end do
           D2NOS(10) = D2NOS(10)+PROB(KT)
           40 continue
        else if (IBRSTAT(KT).eq.5) then

           !....       Status of worst canker on tree is topkilled, add count

           TBRGIR(I4) = TBRGIR(I4)+PROB(KT)
           IMC(KT) = 3

           !....       Loop through 2-inch DBH categories; add to the total for
           !....       topkill trees; if DBH not < 19", must be > 19"

           do II = 1,9
              if (DBH(KT).lt.(FLOAT(II*2+1))) then
                 D2GIR(II) = D2GIR(II)+PROB(KT)
                 go to 42
              end if
              41 continue
           end do
           D2GIR(10) = D2GIR(10)+PROB(KT)
           42 continue
        end if

        !....    After a compression, it is possible for a record to represent
        !....    both live and dead trees; therefore, BRPB must be added for
        !....    all trees.  BRPB will be 0 if the tree is not dead.

        TBRMRT(I4) = TBRMRT(I4)+BRPB(KT)

        !....    Loop through 2-inch DBH categories; add to the total for
        !....    dead trees; if DBH not < 19" or less, must be > 19"

        do II = 1,9
           if (DBH(KT).lt.(FLOAT(II*2+1))) then
              D2DED(II) = D2DED(II)+BRPB(KT)
              go to 44
           end if
           43 continue
        end do
        D2DED(10) = D2DED(10)+BRPB(KT)
        44 continue

        !....    Calculate total number of trees per acre that are white pine.
        !....    This should be the sum of all the canker status categories
        !....    listed above excluding trees killed by blister rust this cycle.

        TBRHST(I4) = TBRHST(I4)+PROB(KT)+BRPB(KT)

        !....    Loop through 2-inch DBH categories; add to the total for
        !....    all white pines; if DBH not < 19" or less, must be > 19"

        do II = 1,9
           if (DBH(KT).lt.(FLOAT(II*2+1))) then
              D2WP(II) = D2WP(II)+PROB(KT)
              go to 46
           end if
           45 continue
        end do
        D2WP(10) = D2WP(10)+PROB(KT)
        46 continue
        50 continue
     end do

     !.... End species loop.
     53 continue
  end do

  !.... Common return.

  55 continue
  if (DEBUG) write(JOSTND,60) ICYC
  60 format('Leaving subroutine BRTSTA: cycle = ',I2)
  return
end
