!=================!
!   rd : rdinsd   !
!=================!
! RD $Id$
subroutine RDINSD
  implicit none

  !  Purpose :
  !     This subroutine models the infection of new trees on the
  !     inside of infection centers by simulating all the inoculum
  !     in a small area explicitly and obtaining the probability of
  !     contact by simulation. Trees are infected at some point along
  !     their root systems (before the center - hence they will have
  !     a negative radius of infection)

  !  Called By :
  !     RDCNTL  [ROOT DISEASE]

  !  Calls :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDRANN  (FUNCTION)     [ROOT DISEASE]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]

  !  Local Variables :
  !     RRNINF - REAL
  !              New infections, by tree record, totalled over the number
  !              of simulations and averaged at the end.
  !     POLP   - REAL
  !              Proportion of root system infected, totalled over the
  !              simulations and averaged at the end. Positive POLP means
  !              negative root infection.
  !     NINSIM - REAL
  !              Number of infection simulations.

  !  Revision History :
  !     06/12/96 - Matthew K. Thompson
  !                Moved the declaration of DSO, DSII, and DSIU to the
  !                parameter include file RDPARM.

  !     25/MAR/98  RNH (FHTET)
  !                Modified loop 1000 to skip operations on non-host
  !                annosus species
  !    21-MAR-00 Lance David (FHTET)
  !                Reduced RETURN statements to 1 at the end of routine.
  !                Added Debug code.
  !    03-AUG-2012 Lance David (FMSC)
  !                Array dimension of RRNINF and POLP changed from 500 to
  !                parameter IRRTRE.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: I,I1,I2,IDI,IN,IP,IRINCS,IT,ITROLP,J,K,KSP,NSIMS,NUMOLP

  real    :: AVGINF,DENNEW,DISOLP,DIST,OAMOVE(3),OVERLP,PNIOLP,PNSP,POLP(IRRTRE),PREVKL,PROPN,R,&
       &RDRANN,RRNINF(IRRTRE),SMBI,SMIU,TEMP,XTRY,YTRY

  !.... See if we need to do some debug.

  call DBCHK (DEBUG,'RDINSD',6,ICYC)

  if (DEBUG) write(JOSTND,*)' ENTER RDINSD: ICYC=',ICYC

  !.... Set up inoculum test plot.

  if (ITRN .eq.0.or. PAREA(IRRSP) .eq.0.0) goto 2000 !RETURN

  SMBI = 0.0
  SMIU = 0.0

  IDI = IRRSP
  do I = 1,ITRN
     if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(ISP(I)))
     if (IDI .ne. IRRSP) goto 443

     POLP(I) = 0.0
     RRNINF(I) = 0.0

     SMBI = SMBI + PROBIT(I)

     SMIU = SMIU + PROBIU(I)
     443 continue
  end do

  if (DEBUG) write(JOSTND,888) SMIU, SMBI, PAREA
  888 format (' IN RDINSD:  SMIU SMBI PAREA=',3E15.7)

  DENNEW = (SMIU + SMBI) / PAREA(IRRSP)

  !.... Get linear dimensions in feet.

  do I = 1, 2
     do J = 1, 5
        DENNEW = DENNEW+(PROBD(IRRSP,I,J)/(PAREA(IRRSP)+1E-9))
        45 continue
     end do
     50 continue
  end do

  if (DENNEW .le.0.0) goto 2000 !RETURN

  RRIARE = RRGEN(IRRSP,9) / (DENNEW + 1E-9)
  RRIDIM = SQRT(RRIARE) * 208.7

  if (DEBUG) write(JOSTND,51) RRIARE,RRIDIM,RRGEN(IRRSP,9),DENNEW
  51 format(' RDINSD: RRIARE RRIDIM RRGEN(9) DENNEW=',4F12.3)

  !.... Begin the simulation loop for the calculation of inside center
  !.... infection.

  do NSIMS = 1,NINSIM

     ANUINF(IRRSP,NSIMS) = 0.0

     !....    Select live infected trees for the area to be simulated.

     IRINCS = 0
     IDI = MAXRR

     do KSP = 1, MAXSP
        if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))
        if ((ISCT(KSP,1) .eq.0) .or. (IDI .ne. IRRSP)) goto 100

        I1 = ISCT(KSP,1)
        I2 = ISCT(KSP,2)

        do J = I1, I2
           I = IND1(J)

           do IT = 1, ISTEP
              do IP = 1,2

                 !....                Calculate number of trees of class in simulated
                 !....                area. Ignore trees which are infected but are not
                 !....                yet inoculum.

                 if (PROPI(I,IT,IP) .le.0.0) goto 85

                 RRIMEN = PROBI(I,IT,IP) * RRIARE / (PAREA(IRRSP) +&
                      &1E-6)
                 if (RRIMEN .eq.0.0) goto 85

                 NUMTRE = INT(RRIMEN)
                 PTRE = RRIMEN - NUMTRE
                 R = RDRANN(0)

                 if (R .lt. PTRE) NUMTRE = NUMTRE + 1

                 if (DEBUG) write(JOSTND,86) NUMTRE
                 86 format(' RDINSD:  NUMTRE=',I6)

                 if (NUMTRE .le.0) goto 85

                 !....                Get tree information from the tree list.

                 do K = 1, NUMTRE
                    IRINCS = IRINCS + 1

                    if (DEBUG) write(JOSTND,87) IRINCS
                    87 format (' RDINSD:  IRINCS=',I6)

                    if (IRINCS .le. IRRTRE) goto 75

                    write(6,70)
                    70 format(' TOO MANY TREES IN INSIDE SIMULATION')
                    IRINCS = IRRTRE
                    goto  110

                    75 continue
                    RRIRAD(IRINCS) = ROOTL(I) * PROPI(I,IT,IP)
                    80 continue
                 end do
                 85 continue
              end do
              90 continue
           end do
           95 continue
        end do
        100 continue
     end do

     110 continue

     !....    Select infected dead trees and stumps for simulation.

     do I = 1, 2
        do J = 1,5

           !....          Estimate number of stumps in simulated area.

           RRIMEN = PROBD(IRRSP,I,J) * RRIARE /&
                &(PAREA(IRRSP) + 1E-6)
           if (RRIMEN .eq.0.0) goto 600
           NUMTRE = INT(RRIMEN)
           PTRE = RRIMEN - NUMTRE
           R = RDRANN(0)

           if (R .lt. PTRE) NUMTRE = NUMTRE + 1
           if (NUMTRE .le.0) goto 600

           !....          Get radii of inoculum.

           do K = 1, NUMTRE
              IRINCS = IRINCS + 1
              if (IRINCS .le. IRRTRE) goto 580

              write(JOSTND,570)
              570 FORMAT(' TOO MANY TREES AND STUMPS IN INSIDE          &
                   &                   SIMULATION')
              IRINCS = IRRTRE
              goto 610

              580 continue
              RRIRAD(IRINCS) = ROOTD(IRRSP,I,J)
              590 continue
           end do
           600 continue
        end do
        605 continue
     end do

     610 continue

     INFISD(IRRSP,NSIMS) = IRINCS

     !....    If IRINCS = 0 then we have no sources of inoculum, so there is
     !....    no point in doing the rest of this simulation.

     if (IRINCS .eq.0) goto 1010

     !....    Set up inoculum positions randomly.

     do IT = 1, IRINCS
        XRRI(IT) = RDRANN(0) * RRIDIM
        YRRI(IT) = RDRANN(0) * RRIDIM
        650 continue
     end do

     !....    Process tree list and infect trees inside infection zones.
     !....    ------------------------------------------------------------
     !....
     !....    Loop over tree classes in tree list.

     RRGEN(IRRSP,4) = 0
     IDI = MAXRR

     do KSP = 1, MAXSP
        if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))

        !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
        !     If non-host species then IDI = 0, and loop should be skipped to
        !     prevent array out of bounds error

        if (IDI .le.0) go to 1000

        PNSP = PNINF(IRTSPC(KSP),IDI)

        !....       First, modify probability of infection to account for the
        !....       number of years in a cycle.

        PNSP = 1 - ((1 - PNSP) ** (FINT/PINT))

        !....       Modify propability of infection for root to root contact
        !....       based on the proportion of centers that are spore initiated
        !....       (SPPROP).

        PNSP = PNSP * ((SPPROP(IDI) * SPTRAN(IDI)) +&
             &(1 - SPPROP(IDI)))
        if ((ISCT(KSP,1) .le.0) .or. (IDI .ne. IRRSP)) goto 1000

        I1 = ISCT(KSP,1)
        I2 = ISCT(KSP,2)

        do J = I1, I2
           I = IND1(J)

           !....          Loop over uninfected trees in simulation.
           !....          IRINIT (=13500) seems to be used to set a possible
           !....          maximum on the number of trees used in the simulation.

           NUMTRE = INT(real(IRINIT) / (real(ITRN) + 1E-6))
           if (NUMTRE .gt. PROBIU(I)) NUMTRE = INT(PROBIU(I))
           if (NUMTRE .le.0) goto 900

           NUMOLP = 0
           DISOLP = 0.0
           PNIOLP = 0.0

           do IT = 1, NUMTRE
              XTRY = RDRANN(0) * RRIDIM
              YTRY = RDRANN(0) * RRIDIM

              !....             Loop over inoculum sources to see if any contact.
              !....             ITROLP -  counts number of overlaps for a single tree.
              !....             NUMOLP -  counts total number of places of overlap
              !....                       (sum of ITROLP'S).
              !....             DISOLP -  adds up the amount of overlap (used in an
              !....                       average later).
              !....             PNIOLP -  is total probability of no infection from
              !....                       overlap (will be averaged).

              ITROLP = 0
              do IN = 1, IRINCS
                 DIST = SQRT((XTRY - XRRI(IN))**2 +&
                      &(YTRY - YRRI(IN))**2)
                 if (DIST .gt. (RRIRAD(IN) + ROOTL(I))) goto 750

                 ITROLP = ITROLP + 1
                 OVERLP = RRIRAD(IN) + ROOTL(I) - DIST
                 if (OVERLP .gt. ROOTL(I)) OVERLP = ROOTL(I)
                 DISOLP = DISOLP + OVERLP
                 750 continue
              end do

              if (ITROLP .gt.0) then
                 PNIOLP = PNIOLP + (1 - PNSP) ** ITROLP
              else
                 PNIOLP = PNIOLP + 1
              end if

              NUMOLP = NUMOLP + ITROLP
              800 continue
           end do

           !....          Old way of calculating infection.
           !....
           !....          RRICON = REAL(IRIHIT) / (REAL(NUMTRE) + 1E-6)
           !....          RRICON = RRICON * PNSP
           !....
           !....          Calculate total number of new infections for tree class.
           !....
           !....          RRIMEN = RRICON * PROBIU(I)
           !....
           !....          End of old way.

           !....          Calculate average probability of infection for tree
           !....          class.

           AVGINF = 1 - (PNIOLP / (real(NUMTRE) + 1E-6))

           RRNINF(I) = RRNINF(I) + PROBIU(I) * AVGINF
           ANUINF(IRRSP,NSIMS) = ANUINF(IRRSP,NSIMS) +&
                &PROBIU(I) * AVGINF

           if (NUMOLP .le.0) goto 900

           !...           DISOLP / NUMOLP is the average distance of overlap
           !....          so POLP is the average proportion of overlap
           !....          then take 1-POLP so POLP is measured from the center.

           TEMP = 1 - (DISOLP / NUMOLP) / (ROOTL(I) + 1E-6)
           if (TEMP .le.0.0) TEMP = -.001
           POLP(I) = POLP(I) + TEMP

           900 continue
        end do
        1000 continue
     end do

     1010 continue
  end do

  !.... Now that simulation loop is over, apply the average results to
  !.... each record.

  IDI = MAXRR
  do KSP = 1, MAXSP
     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))
     if ((ISCT(KSP,1) .le.0) .or. (IDI .ne. IRRSP)) goto 1050

     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)

     do J = I1, I2
        I = IND1(J)

        !....       Note: When we use the integer variable NUMTRE, we could
        !....             end up with more trees infected then there were
        !....             uninfected. There is no reason why PROBI has to be
        !....             given integer numbers. (sb: nov 30/93)

        !....       NUMTRE = INT(RRIMEN)
        !....       PTRE = RRIMEN - NUMTRE
        !....       R = RDRANN(0)
        !....       IF (R .LT. PTRE) NUMTRE = NUMTRE + 1
        !....       IF (NUMTRE .LE. 0) GOTO 900
        !....       PROBIU(I) = PROBIU(I) - NUMTRE
        !....       IF (PROBIU(I) .LT. 0) PROBIU(I) = 0
        !....       PROBI(I,ISTEP) = PROBI(I,ISTEP) + NUMTRE

        !....       For printing, add # of uninfected trees (before infection
        !....       occurs).

        CORINF(IDI,2) = CORINF(IDI,2) + PROBIU(I)

        !....       Find average of RRNINF and POLP

        if (RRNINF(I) .le.1E-4) goto 1030

        RRNINF(I) = RRNINF(I) / (real(NINSIM) + 1E-6)
        POLP(I) = POLP(I) / (real(NINSIM) + 1E-6)

        PROBIU(I) = PROBIU(I) - RRNINF(I)
        if (PROBIU(I) .lt.0) PROBIU(I) = 0

        PROBI(I,ISTEP,1) = PROBI(I,ISTEP,1) + RRNINF(I)

        !....       Add the new infection resulting inside the core of the
        !....       center.

        CORINF(IDI,1) = CORINF(IDI,1) + RRNINF(I)

        !....       Determine the initial infection level, from a distribution
        !....       about the mean infection of 0.001.

        !....       IF (PROPI(I,ISTEP) .LE. 0.0) PROPI(I,ISTEP) = RDRANP(0.001)

        PROPI(I,ISTEP,1) = -POLP(I)

        RRGEN(IRRSP,4) = RRGEN(IRRSP,4) + RRNINF(I)

        if (OAKL(DSIU,I) .gt.0.0) then

           !....          OAKL must be re-arranged because some of the inside
           !....          uninfected trees that have just been infected may have
           !....          been previously killed by other agents. eg. If 10% of
           !....          iu trees were killed previously then assume that 10% of
           !....          the newly infected trees are some of those that were
           !....          killed.

           PROPN = OAKL(DSIU,I) / (PROBIU(I) + RRNINF(I) + 1E-6)
           PREVKL = PROPN * RRNINF(I)

           OAMOVE(DSO) = 0.0
           OAMOVE(DSII) = PREVKL
           OAMOVE(DSIU) = -PREVKL

           call RDMREC (1,I,KSP,OAMOVE)

        end if

        1030 continue
     end do
     1050 continue
  end do

  !.... Call RDSUM to calculate new value of PROBIT.

  call RDSUM(ITRN,PROBIT,PROBI,ISTEP)

  2000 continue
  if (DEBUG) write(JOSTND,*) ' EXIT RDINSD'

  return
end
