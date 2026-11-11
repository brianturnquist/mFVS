!=================!
!   rd : rdtreg   !
!=================!
! RD $Id$
subroutine RDTREG
  implicit none

  !  Purpose :
  !     This root disease model subroutine is called from FVS
  !     GRADD every cycle.  It calls the other agents model,
  !     then calls the master root disease model.  Finally,
  !     it calls various subroutines to produce output files.

  !  Called By :
  !     GRADD  [FVS]

  !  Calls :
  !     DBCHK   (SUBROUTINE)   [FVS]
  !     RDLOAD  (SUBROUTINE)   [ROOT DISEASE]
  !     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
  !     RDOAGM  (SUBROUTINE)   [ROOT DISEASE]
  !     RDTIM   (SUBROUTINE)   [ROOT DISEASE]
  !     RDPUSH  (SUBROUTINE)   [ROOT DISEASE]
  !     RDCNTL  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
  !     RDEND   (SUBROUTINE)   [ROOT DISEASE]
  !     RDRDEL  (SUBROUTINE)   [ROOT DISEASE]
  !     RDGROW  (SUBROUTINE)   [ROOT DISEASE]
  !     RDINOC  (SUBROUTINE)   [ROOT DISEASE]
  !     ANWRITE (SUBROUTINE)   [ROOT DISEASE]

  !  Revision History :
  !    03-APR-97 Matt Thompson (FHTET)
  !              Changed the code that tests to see if a cut occurred
  !              so that if the disease area is 0 but there is a chance
  !              of spore infection, Annosus can occur.
  !    14-JAN-00 Lance David (FHTET)
  !              Replaced the literals in option processor calls with
  !              references to array MYACT.
  !    06-AUG-01 Lance R. David (FHTET)
  !              Added initialization of array DIFF.
  !    29-JUL-02 Lance R. David (FHTET)
  !              Modified and added debug statements. Changed two return
  !              statements to "GOTO 300" so routine has single point of exit.
  !   09/04/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/01/16 Lance R. David (FMSC)
  !     Moved check to exit routine if RD not in use. IF (IROOT .EQ. 0) GOTO 300

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDADD.f90'
  include 'RDARRY.f90'
  include 'RDCRY.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: I,IACTK,IDI,ITR,J,KDT,MYACT(1),NPS,NTODO
  real    :: ANS,DIFF(ITOTRR),PRMS(2),TPAREA

  data MYACT / 2430 /

  !.... See if we need to do some debug.

  call DBCHK (DEBUG,'RDTREG',6,ICYC)

  if (DEBUG) write (JOSTND,*) 'ENTER RDTREG'

  if (IROOT .eq.0) goto 300
  do IDI = 1,ITOTRR
     DIFF(IDI) = 0.0
     10 continue
  end do

  TPAREA = 0.0
  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     850 continue
  end do

  !.... Changed the following two lines so that the annosus model will be run if
  !.... there is no area but a cut did occur (LSPFLG(1..3) = .TRUE.).
  !.... mt 04-03-97

  if ((TPAREA .eq.0) .and.&
       &(.not. LSPFLG(1) .and.&
       &.not. LSPFLG(2) .and.&
       &.not. LSPFLG(3))) goto 300

  !.... Check for time to death and infection probability.

  if (IY(ICYC) .ge. IRGEN(8)) IRHAB = 2
  if (IY(ICYC) .ne. IRGEN(9)) goto 1000

  IDI = MAXRR
  do I = 1,MAXSP
     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(I))
     PNINF(IRTSPC(I),IDI) = SSSFAC(IRTSPC(I),IDI) *&
          &PNINF(IRTSPC(I),IDI)
     900 continue
  end do

  1000 continue

  !.... Copy the borax option processing information into the next
  !.... cycle so it will be there if there is a cut.
  !     (Activity Code 2430)

  if (LBORAX(1)) then
     if (.not. LBORAX(2)) then

        if (LBORAX(1)) call OPFIND (1,MYACT(1),NTODO)

        if (NTODO .gt.0) then

           call OPGET (1,2,KDT,IACTK,NPS,PRMS)

           BOTRT = PRMS(1)
           BODBH = PRMS(2)

           call OPDONE (1,IY(ICYC))
        end if

     else
        LBORAX(2) = .false.
     end if

  end if

  if (ITRN .le.0) goto 9999

  !.... Calculate outside density.

  do IDI = MINRR,MAXRR
     DIFF(IDI) = SAREA - PAREA(IDI)
     if (DIFF(IDI) .gt.0.0) goto 841
     839 continue
  end do
  goto 844

  841 continue
  IDI = MAXRR

  do I = 1,ITRN

     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(ISP(I)))

     !     Exit loop if not a host species (RNH 28May98)

     if (IDI .le.0) go to 843

     if (DIFF(IDI) .ge.1E-6) then
        FPROB(I) = (PROB(I) * SAREA - PROBIU(I) - PROBIT(I)) /&
             &DIFF(IDI)
        if (FPROB(I) .le.1E-6) FPROB(I) = 0.0
     else
        FPROB(I) = 0.0
     end if

     843 continue
  end do

  844 continue

  !.... Load PROB into PROBL (Density of all live trees in stand is PROB).

  call RDLOAD (PROBL,PROB,ITRN)

  !.... Calculate root radii.

  do ITR = 1,ITRN
     I = ISP(ITR)
     call RDROOT (I,DBH(ITR), ANS, PROOT(IRTSPC(I)),&
          &RSLOP(IRTSPC(I)), HT(ITR))
     ROOTL(ITR) = ANS
     if (DEBUG) write (JOSTND,775) ITR, PROB(ITR), PROBI(ITR,1,1),&
          &PROBIU(ITR), FPROB(ITR),ITR,HT(ITR),DBH(ITR),ANS
     775 format (' IN RDTREG: I PROB PROBI PROBIU FPROB',I5,4(1X,F8.2),&
          &' ITR HT DBH ROOTL ', I4,3(1X,F7.3))
     1001 continue
  end do

  !.... Call the other agents submodels.

  call RDOAGM

  call RDTIM

  9999 continue

  !.... Call subroutine to push stumps.

  call RDPUSH
  call RDCNTL
  if (ITRN .le.0) goto 10000

  !.... Call subroutine to update stump list with natural,
  !.... other agent, and root disease killed trees.
  !....
  !.... Root disease extensions simulated, redefine PROB
  !.... after losses to other agents

  call RDSUM(ITRN,PROBIT,PROBI,ISTEP)
  call RDEND

  !.... New code added.  Call RDRDEL to delete tree records which
  !.... have all prob values equal to zero, IE PROBI, PROBIU, PROBIT
  !.... and FPROB.  RDRDEL called only if model initialized from treelist.

  if (RRTINV) call RDRDEL

  !.... Call routine which reduces the growth factor due to root disease.

  call RDGROW

  !.... Update growing space factor based on root disease patches.

  10000 continue
  if (INFLAG .eq.0) goto 260
  if (INFLAG .gt.1) goto 260
  INFLAG = 2

  do IDI = MINRR,MAXRR
     do I = 1,2
        do J = 1,5
           CRNSTO(IDI,I,J) = PROBD(IDI,I,J)
           205 continue
        end do
        210 continue
     end do
     212 continue
  end do

  260 continue

  !.... Call subroutine to decay the stump root radius
  !.... (decrease inoculum).

  call RDINOC (.false.)

  300 continue
  if (DEBUG) write (JOSTND,*) 'EXIT RDTREG'
  return
end
