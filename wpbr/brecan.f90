!===================!
!   wpbr : brecan   !
!===================!
! WPBR $Id$
subroutine BRECAN(IBRN,HITE,SSTAR,SSTHT,PROP,PIMX,EXPC)
  implicit none

  !  Purpose:
  !  BRECAN calculates the number of new cankers expected for a
  !  tree for one year.  When a new canker is placed into the
  !  arrays which hold the information of cankers tracked by the
  !  model, distance up (DUP array) is randomly placed within the
  !  crown and distance out on branch (DOUT) is calulated based
  !  on the distance up.

  !  Parameters Passed:
  !        IBRN - index for current tree
  !        HITE  - total tree height at end of cycle in meters for
  !                current tree. (current height  +  height growth)
  !        RINDX - rust index value for current tree
  !        SSTAR - summed target area for current tree
  !        SSTHT - this years height in meters for current tree
  !                (current height  +  proportion of height growth)
  !        PROP  - proportion of full cycle represented as of current year
  !        PIMX  - proportion trees infected maximum for the stand
  !     Returned:
  !        EXPC  - expected number of cankers for current tree (record)
  !                this year.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  19-MAY-1999 Lance David
  !     Added debug code.
  !  15-MAR-2001 Lance R. David (FHTET)
  !     Updated DFACT variable to array by species and stock type.
  !  22-MAR-2001 Lance R. David (FHTET)
  !     Added PIMX parameter and control for assigning escape status
  !     code 9 to trees. Once a tree is tagged as escape, it will never
  !     become infected.
  !  03-MAY-2001 Lance R. David (FHTET)
  !     Added species dimension to variables (arrays).
  !  10-MAY-2006 Lance R. David (FHTET)
  !     Added debug.
  !  14-MAY-2014 Lance R. David (FMSC)
  !     Removed RINDX parmeter (third in list) because it is in common.
  !     RINDX changed to RI(IBRN) in processing.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations.
  ! Variables
  integer :: IBRN,NUMTIM,I3,ICANB,J
  real    :: HITE,SSTAR,SSTHT,PROP,PIMX,EXPC,RITEM,TNEWC,CRLEN,PLI,TOUT,TUP,PLETH,XBRAN,PIEXP
  logical :: DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRECAN',6,ICYC)
  if (DEBUG) write(JOSTND,22) ICYC,&
       &IBRN,ISP(IBRN),HITE,RI(IBRN),SSTAR,SSTHT,PROP,&
       &PIMX,IBRSTAT(IBRN),ILCAN(IBRN),ITCAN(IBRN)
  22 format('Entering subroutine BRECAN: cycle = ',I2,/,&
       &'IBRN=',I4,' ISP=',I2,' HITE=',F10.7,' RI=',F10.7,&
       &' SSTAR=',F10.4,' SSTHT=',F10.7,' PROP=',F10.7,&
       &' PIMX=',F10.7,' IBRSTAT=',I2,' ILCAN=',I2,' ITCAN=',I3)

  EXPC = 0.0

  !.... set species code for index.
  I3 = BRSPM(ISP(IBRN))
  if (DEBUG) write(JOSTND,*) 'I3=',I3,' PROB=',PROB(IBRN),&
       &' PITCA=',PITCA(I3),' TRETN=',TRETN(I3),' THPROB=',THPROB(I3)

  if (IBRSTAT(IBRN).eq.0) then
     !....    If the current tree is a clean tree and stand infection max has
     !....    not been met, need to update proportion of infected trees.
     !....    Set tree status code so that this happens only once.
     if (PITCA(I3) .lt. PIMX) then
        IBRSTAT(IBRN) = 1
        TRETN(I3) = TRETN(I3) + PROB(IBRN)
        PITCA(I3) = TRETN(I3)/THPROB(I3)
        if (DEBUG) write(JOSTND,*) ' NEW TREE - ',IDTREE(IBRN),&
             &'PITCA=',PITCA(I3),'TRETN=',TRETN(I3),'PROB=',PROB(IBRN),&
             &'THPROB=',THPROB
     else if (PITCA(I3) .ge. PIMX) then
        !....       Infection level has been met.
        !....       Reserve this escape tree with IBRSTAT code 9.
        !....       During previous Years of cycle, expected cankers may
        !....       have accumulated for this tree. Zero them out.
        IBRSTAT(IBRN) = 9
        ESTCAN(IBRN) = 0.0
        ITCAN(IBRN) = 0
        if (DEBUG) write(JOSTND,*) ' ESCAPE TREE - ',&
             &IDTREE(IBRN),ITCAN(IBRN)
        go to 35
     end if
  end if

  !.... Calculate temporary rust index variable based on height at the
  !.... end of the cycle.

  if (HITE.gt.25.0) then
     RITEM = RI(IBRN)*0.1
  else if (HITE.gt.15.0) then
     RITEM = RI(IBRN)*(1.0-(0.09*(HITE-15.0)))
  else
     RITEM = RI(IBRN)
  end if
  if (DEBUG) write(JOSTND,*) 'RITEM=',RITEM,' RI=',RI(IBRN)

  !.... Calculate number of expected cankers for current year
  !.... (total and "potentially lethal") based on tree's target area.

  TNEWC = RITEM*SSTAR
  EXPC = TNEWC
  if (DEBUG) write(JOSTND,*) 'EXPC=',EXPC

  !.... Calculate crown length in centimeters for this year and the
  !.... probability of infection for the tree.

  CRLEN = (SSTHT*100.0)-BRHTBC(IBRN)
  PIEXP = EXP(-(TNEWC/(1+TNEWC*DFACT(I3,ISTOTY(IBRN)))))
  PLI = 1.0-PIEXP
  NUMTIM = INT(TNEWC)+1
  if (DEBUG) write(JOSTND,*) 'PLI=',PLI,' NUMTIM=',NUMTIM

  !.... Loop through for number of expected cankers.

  do J = 1,NUMTIM

     !....    If probability of infection >= random number then add canker.

     call BRANN(XBRAN)
     if (DEBUG) write(JOSTND,*) 'XBRAN=',XBRAN

     if (PLI.ge.XBRAN) then

        !....       Add a canker to total number of cankers for this tree.
        !....       Running total number of cankers is kept but only "lethal"
        !....       ones are processed during cycling.

        ITCAN(IBRN) = ITCAN(IBRN)+1

        !....       Generate up and out positions for added canker.

        call BRANN(XBRAN)
        TUP = (100*SSTHT-CRLEN)+CRLEN*XBRAN
        TOUT = (35*SQRT(SSTHT)*(100*SSTHT-TUP))/CRLEN

        if (DEBUG) write(JOSTND,*) 'XBRAN=',XBRAN,&
             &' TUP=',TUP,' TOUT=',TOUT

        !....       If out < 50 then canker possibly lethal (different
        !....       probability than if the canker is farther out than 50 cm).

        if (TOUT.lt.50.0) then
           PLETH = 0.97-0.0158*TOUT
        else
           PLETH = 35.4/TOUT**(1+(0.35*TOUT/50))
        end if
        if (PLETH.lt.0.0) PLETH = 0.0

        if (DEBUG) write(JOSTND,*) 'PLETH=',PLETH

        !....       Call random number generator. If PLETH >= number then create
        !....       a bole canker, otherwise create a branch canker. If array is
        !....       full then skip adding a canker altogether.

        call BRANN(XBRAN)
        if (DEBUG) write(JOSTND,*) 'XBRAN=',XBRAN

        if (ILCAN(IBRN).lt.10) then
           ILCAN(IBRN) = ILCAN(IBRN)+1
           ICANB = ILCAN(IBRN)
           if (PLETH.ge.XBRAN) then
              DOUT(ICANB,IBRN) = 0.0
           else
              DOUT(ICANB,IBRN) = TOUT
           end if
           DUP(ICANB,IBRN) = TUP
           GIRDL(ICANB,IBRN) = 0.0
           ISTCAN(ICANB,IBRN) = 0

           if (DEBUG) write(JOSTND,*)&
                &' DOUT=',DOUT(ICANB,IBRN),' DUP=',DUP(ICANB,IBRN)
        end if
     end if
     30 continue
  end do
  35 continue

  !.... Common return.
  if (DEBUG) then
     write (JOSTND,38) IBRN,HITE,RI(IBRN),SSTAR,SSTHT,PROP,EXPC
     38 format(' IBRN=',I4,' HITE=',F5.1,' RI=',F7.4,&
          &' SSTAR=',F7.1,' SSTHT=',F5.1,' PROP=',F4.1,' EXPC=',F3.1)
  end if

  if (DEBUG) write(JOSTND,40) ICYC
  40 format('Leaving subroutine BRECAN: cycle = ',I2)
  return
end
