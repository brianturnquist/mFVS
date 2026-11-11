!===================!
!   wpbr : brcrem   !
!===================!
! WPBR $Id$
subroutine BRCREM
  implicit none

  !  Purpose:
  !  BRCREM removes cankers that have been classified as inactive
  !  or non-lethal.  Also removes excisable cankers when excising
  !  is scheduled and prunable cankers when pruning is scheduled.
  !  The tree's height to base of crown is reset for trees that
  !  have been pruned.
  !  Crown ratio carried by Prognosis is not changed.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  24-MAY-1999 Lance David
  !     Added debug code to track pruning.
  !  14-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 200) and species temp index variable (I3)
  !     are new.
  !  05-MAR-2001 Lance David (FHTET)
  !     Added pathological pruning control so that crown base height
  !     will remain unchanged when a branch canker is pruned off.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM.
  !  09-MAY-2006 Lance R. David (FHTET)
  !     Changed IND2 to ICNDX.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: I1,I2,I3,ICNDX(10),ITSTAT,IVAC,J,K,M,NLCAN,ICANT
  real    :: HITE,PRHT,PRHTST,VALUE
  logical :: DEBUG,PRUNED

  !.... Is debug requested?

  call DBCHK(DEBUG,'BRCREM',6,ICYC)
  if (DEBUG) write(JOSTND,111) ICYC
  111 format('Entering subroutine BRCREM: cycle = ',I2)

  !.... If there are no trees, exit subroutine.

  if (ITRN .eq.0) go to 300

  !.... Process host species, if any.
  !.... Start species loop

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 200

     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 200
     I2 = ISCT(I3,2)

     do K = I1,I2
        J = IND1(K)
        PRUNED = .false.
        IVAC = 0
        HITE = HT(J)
        NLCAN = ILCAN(J)

        !....    Get tree's status, based on the worst canker on the tree.

        ITSTAT = IBRSTAT(J)
        if (ITSTAT.eq.7) then

           !....       Tree status is dead (7).  No treatment is performed.

           go to 30
        else if (ITSTAT.eq.0.and.LPRGO.and.LCLEN) then

           !....       Prune clean tree.

           PRUNED = .true.
        else

           !....       Process cankers for this tree.
           !....       Tree status is anything from clean (0) to non-salvable (5).
           !....       Clear array that will hold canker indexes to be removed.

           do M = 1,10
              ICNDX(M) = 0
              10 continue
           end do

           do M = 1,NLCAN
              ICNDX(M) = M

              !....          Get this canker's status.

              ICANT = ISTCAN(M,J)
              if (ICANT.eq.-1.or.ICANT.eq.1) then

                 !....             Canker is inactive or non-lethal, signal vacancy
                 !....             in array.  Remove inactive or non-lethal cankers for
                 !....             all tree statuses (except dead, of course) because
                 !....             even on non-salvable (4) and topkill (5) trees the
                 !....             non-lethal cankers continue to grow and soon become
                 !....             "not" non-lethal.

                 IVAC = IVAC+1
                 ICNDX(M) = -M
              else if (ICANT.eq.3.and.LEXGO.and.ITSTAT.le.3) then

                 !....             This is an excisable canker and excising is scheduled.
                 !....             Don't excise non-salvable (4) and topkill (5) trees.
                 !....             If random number is less than excising success rate,
                 !....             the canker will be removed.  If random number is
                 !....             greater than or equal to the success rate, the canker
                 !....             has escaped the excising process and the percent
                 !....             girdle is reset to 1 percent.

                 !....             First check to see if there are multiple excisable
                 !....             cankers on the tree in which case no excise will
                 !....             happen at all (LEXMLT is determined in BRCGRO and
                 !....             and BRTSTA).

                 if (.not.LEXMLT(J)) then

                    !....                This must be the only excisable canker on this
                    !....                tree; go ahead and test against a random number.

                    call BRANN(VALUE)
                    if (VALUE.lt.SRATE(2)) then
                       IVAC = IVAC+1
                       ICNDX(M) = -M
                    else
                       GIRDL(M,J) = 1.0
                    end if
                 end if

                 if (LPRGO.and.LCLEN) then

                    !....                Pruning is scheduled and pruning of clean
                    !....                trees is also specified. Because pruning is
                    !....                done on cankerless trees, it will also be
                    !....                done on trees that have excisable cankers.

                    PRUNED = .true.
                 end if
              else if (ICANT.eq.2.and.LPRGO.and.ITSTAT.le.3) then

                 !....             This is a prunable canker and pruning is scheduled.
                 !....             Don't prune non-salvable (4) and topkill (5) trees.
                 !....             If random number is less than pruning success rate,
                 !....             the canker will be removed.

                 PRUNED = .true.
                 call BRANN(VALUE)
                 if (VALUE.lt.SRATE(1)) then
                    IVAC = IVAC+1
                    ICNDX(M) = -M
                 end if
              end if
              40 continue
           end do
        end if

        if (PRUNED .and..not. LPATPR) then

           !....       Pathological pruning (LPATPR = true) will not change
           !....       the height to base of crown for a tree. If set false,
           !....       reset the tree's height to base of crown, if it is
           !....       not already higher than the absolute pruning height.
           !....       Absolute pruning height for this tree must be
           !....       calculated.
           !....       NOTE: units are in centimeters

           PRHT = HTMAX(1)
           PRHTST = HITE*HTPRPR*30.48
           if (PRHTST.gt.PRHT) PRHTST = PRHT
           if (DEBUG) then
              write(JOSTND,*)&
                   &' tree pruned: BRHTBC old=',BRHTBC(J),&
                   &' new(if>)=',PRHTST
           end if
           if (BRHTBC(J).lt.PRHTST) BRHTBC(J) = PRHTST
        end if

        if (IVAC.gt.0) then

           !....       Vacancies have been created in this tree's canker
           !....       arrays by removing cankers.
           !....       Call BRCDEL to reorganize arrays.

           call BRCDEL(IVAC,ICNDX,J)
        end if
        30 continue
     end do

     !.... End of species loop
     200 continue
  end do

  !.... Common return.

  300 continue
  if (DEBUG) write (JOSTND,333) ICYC
  333 format('Leaving subroutine BRCREM: cycle = ',I2)
  return
end
