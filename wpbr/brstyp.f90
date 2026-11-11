!===================!
!   wpbr : brstyp   !
!===================!
! WPBR $Id$
subroutine BRSTYP
  implicit none

  !  Purpose:
  !  BRSTYP assigns stock types to individual tree records based on
  !  parameters specified with the STOCK keyword.
  !  Called from BRSETP and BRTREG.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  20-APR-1999 Lance David
  !     --(this is not a problem for LF95 and was removed 18-APR-01)--
  !     Added the function INT() to IF statements checking equality just
  !     to eliminate the warning messages from Lahey FORTRAN 90 (LF90).
  !  21-APR-1999 Lance David
  !     Changed some GOTO code to block IF statments. Added some comments
  !     and reworked the stock type assignment code because too many
  !     records would be assigned to the highest type included in a mixed
  !     stock type situation.
  !  14-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 100) and species temp index variable (I3)
  !     are new.
  !  17-APR-2001 Lance David (FHTET)
  !     Added debug statements.
  !     Change stock type proportions to all stock type 1 if sum of
  !     proportions is >1.0.
  !     Reworked the stock type assignment method for mixed stock types.
  !  24-APR-2001 Lance R. David (FHTET)
  !     Added species dimension to PRPSTK array and specie DO loop around
  !     stock proportion validation process.
  !  01-MAY-2001 Lance R. David (FHTET)
  !     Added TBRHST for accumulation of host trees in the stand.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM. Instead of just being and indicator of a
  !     species being a host, BRSPM holds the array index value for that
  !     species and is used to access all species-specific BR arrays.

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
  integer :: STOCK,I1,I2,I3,I4,J,K,L,M
  real    :: SSUM,TXP,TXP1,TXP2,TXP3,TXP4,TXPS1,TXPS2,TXPS3,TXPS4,X

  !.... Is debug requested?

  call DBCHK(DEBUG,'BRSTYP',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format('Entering subroutine BRSTYP: cycle = ', I2)

  !.... If no trees, then exit.
  if (ITRN .eq.0) go to 120

  !.... Planted trees have been assigned an initial value of 5 for
  !.... stocking type in BRESTB. This is so that the planted trees
  !.... can be identified in this routine and assigned valid stock types
  !.... according to the proportions specified by keyword. If planted
  !.... trees have been introduced into the model but no stock mix has
  !.... been specified, then change all the newly planted trees to type 1
  !.... (wild).  Make sure that the sum of stock type proportions equals 1.0
  !.... If the sum of the mix is less than 1.0, the difference is added
  !.... to the "wild" category.  If the sum is greater than 1.0, the
  !.... the proportions revert back to default of all type 1.

  do I3 = 1,MAXSP

     if (BRSPM(I3) .eq.0) go to 20

     !....   Set blister rust species index
     I4 = BRSPM(I3)
     SSUM = 0.0

     do M = 1,4
        SSUM = SSUM+PRPSTK(I4,M)
        16 continue
     end do

     if (SSUM.gt.1.0) then
        PRPSTK(I4,1) = 1.0
        PRPSTK(I4,2) = 0.0
        PRPSTK(I4,3) = 0.0
        PRPSTK(I4,4) = 0.0
     else if (SSUM.lt.1.0) then
        PRPSTK(I4,1) = PRPSTK(I4,1)+(1.0-SSUM)
     end if
     if (DEBUG) write(JOSTND,*) ' SP=',I4,' PRPSTK 1=',PRPSTK(I4,1),&
          &' 2=',PRPSTK(I4,2),' 3=',PRPSTK(I4,3),' 4=',PRPSTK(I4,4)

     20 continue
  end do

  !.... Process host species.
  !.... Start species loop

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 100

     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 100
     I2 = ISCT(I3,2)

     !.... Set blister rust species index
     I4 = BRSPM(I3)

     if (PRPSTK(I4,1).eq.1.0.or. PRPSTK(I4,2).eq.1.0.or.&
          &PRPSTK(I4,3).eq.1.0.or. PRPSTK(I4,4).eq.1.0) then

        !....    No mixing of stock types is specified.

        if (PRPSTK(I4,1).eq.1.0) then
           STOCK = 1
        else if (PRPSTK(I4,2).eq.1.0) then
           STOCK = 2
        else if (PRPSTK(I4,3).eq.1.0) then
           STOCK = 3
        else
           STOCK = 4
        end if

        if (DEBUG) write(JOSTND,*) ' ALL STOCK SET TO: ',STOCK

        !....    Set stock type of planted trees to type specified.

        do J = I1,I2
           K = IND1(J)
           if (DEBUG) write(JOSTND,*) ' IDTREE:',IDTREE(K),' IBRTID:',&
                &IBRTID(K),' ISTOTY:',ISTOTY(K)

           if (ISTOTY(K).eq.5) then
              !....          Add this new tree to the total TPA of this host species.
              !+++++++++++++ TBRHST(I4) = TBRHST(I4) + PROB(K)
              ISTOTY(K) = STOCK
           end if

           110 continue
        end do
     else

        !....    Assign stock types based on specified mix.
        !....    All natural trees ('NATURAL' and excess) have already
        !....    been assigned stock type 1 and will not be changed.
        !....
        !....    Loop through index for this host specie and sum the
        !....    PROB (number of trees) for all records that need
        !....    a stock type assignment.

        TXP = 0.0
        do L = I1,I2
           K = IND1(L)
           if (ISTOTY(K).eq.5) then
              TXP = TXP + PROB(K)
           end if
           30 continue
        end do

        !....    Calculate the target TPA for each stock type.
        TXP1 = PRPSTK(I4,1) * TXP
        TXP2 = PRPSTK(I4,2) * TXP
        TXP3 = PRPSTK(I4,3) * TXP
        TXP4 = PRPSTK(I4,4) * TXP

        !....    Initialize summing variables for stock type TPA.
        TXPS1 = 0.0
        TXPS2 = 0.0
        TXPS3 = 0.0
        TXPS4 = 0.0

        !....    loop through index to make stock type assignments.
        !....    In this simplistic method, the accuracy of the proportional
        !....    stock type assignments depends on the TPAs of the individual
        !....    tree records and number of tree records in the simulation.
        !....    Excess goes to stock type 1, wild stock. Since the sum of
        !....    proportions is tested to be exactly 1.0, this should not
        !....    be a possibility. But... if all conditions fail, stock is 1.
        !....    If random number drawn indicates a stock type for which the
        !....    TPA target has already been met, another random number will
        !....    be drawn.

        do L = I1,I2
           K = IND1(L)

           if (ISTOTY(K).eq.5) then

              !....          Add this new tree to the total TPA of this host species.
              !+++++++++++++ TBRHST(I4) = TBRHST(I4) + PROB(K)

              !....          draw a random number
              35 call BRANN(X)

              if (X.le.PRPSTK(I4,1)) then
                 if (TXPS1.lt.TXP1) then
                    J = 1
                    TXPS1 = TXPS1 + PROB(K)
                 else
                    J = 0
                 end if
              else if (X.le.(PRPSTK(I4,1)+PRPSTK(I4,2))) then
                 if (TXPS2.lt.TXP2) then
                    J = 2
                    TXPS2 = TXPS2 + PROB(K)
                 else
                    J = 0
                 end if
              else if (X.le.(PRPSTK(I4,1)+PRPSTK(I4,2)+&
                   &PRPSTK(I4,3))) then
                 if (TXPS3.lt.TXP3) then
                    J = 3
                    TXPS3 = TXPS3 + PROB(K)
                 else
                    J = 0
                 end if
              else if (X.le.(PRPSTK(I4,1)+PRPSTK(I4,2)+PRPSTK(I4,3)&
                   &+PRPSTK(I4,4))) then
                 if (TXPS4.lt.TXP4) then
                    J = 4
                    TXPS4 = TXPS4 + PROB(K)
                 else
                    J = 0
                 end if
              else
                 J = 1
              end if

              if (J.eq.0) then
                 !....             Need to draw another random number because stock type
                 !....             indicated already has TPA target met.
                 if (DEBUG) write(JOSTND,*) ' RANDOM NUMBER REDRAW'
                 goto 35
              else
                 if (DEBUG) write(JOSTND,*) ' IDTREE:',IDTREE(K),&
                      &' IBRTID:',IBRTID(K),' ISTOTY:',ISTOTY(K),&
                      &' RESET TO ',J,' X=',X
                 ISTOTY(K) = J
              end if
           end if
           40 continue
        end do
     end if

     !.... End of species loop
     100 continue
  end do

  !.... Common return.

  120 continue
  if (DEBUG) write (JOSTND, 300) ICYC
  300 format('Leaving subroutine  BRSTYP: cycle = ', I2)
  return
end
