!=================!
!   rd : rdsprd   !
!=================!
! RD $Id$
subroutine RDSPRD
  implicit none

  !  THIS SUBROUTINE CALCULATES THE RATE OF SPREAD OF DISEASE CENTER
  !  BY SIMULATING THE SPREAD THROUGH A SMALL STAND IN WHICH THE
  !  TREES ARE REPRESENTED EXPLICITLY.

  !  CALLED BY :
  !     RDCNTL  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDPSRT  (SUBROUTINE)   [PROGNOSIS]
  !     RDSLP   (FUNCTION)     [ROOT DISEASE]
  !     RDRANN  (FUNCTION)     [ROOT DISEASE]

  !  NOTES:
  !    PNINF is here taken to be the probability of infection PER infected
  !       tree contacted FOR PINT years. (where PINT is currently set to
  !       10 years)
  !    While the infection is growing into the tree center, the radius of
  !       infection - RADINF - is negative and indicates how far the
  !       infection is from the center.  RADINF is positive once the
  !       infections starts spreading outwards.  RADINF will be positive
  !       at the time of infection. Infection is from a neighbour whose
  !       radius of infection overlaps the stump of the focal tree.

  ! SOME VARIABLES:
  !   DISTNC(i,j)  the distance between the centers of each pair of trees
  !   EFFSDI(1)    the effect of Stand Density Index for tree species 1,
  !                equal to the proportion of the species-typical root
  !                radius actually realized, given the total stand density
  !   FINT         growth cycle used in the Annosus model
  !   IRRSP        RR species to be modelled
  !   IRSNYR       number of time periods for which to model RR spread
  !   IRSTYP       1 = place trees randomly on sample plot
  !                0 = regularly space trees on sample plot
  !   IRTSPC(1)    maps Annosus tree species codes to FVS species codes
  !   MCRATE(NMONT)  the spread rates determined by each Monte Carlo
  !                simulation
  !   NMONT        number of times to repeat MonteCarlo simulation of
  !   NOW, NEW     pointers that indicate the present and future RADINF
  !   NRSTEP       number of years in each time period
  !   NTREES       actual number of trees used in the simulation
  !                spread rate
  !   NUMPAL(I), IDPAL(I, J)  Respectively, the number of trees with
  !                which each focal tree 'i' has root contact ("pals"),
  !                and the identity of those contact trees, for each
  !                focal tree
  !   PAREA(1)     area of the stand infected with RR species 1, in acres
  !   PCOLO(1)     proportion of roots colonized after death of tree of
  !                species1
  !   PKILLS(1)    proportion of roots infected at death of tree of
  !                species 1
  !   PNINF(1)     probability of infection of tree species 1
  !   RKILLS(i)    the radius of infection at which tree 'i' will die
  !   ROOM         0/1 to indicate whether infection has crossed the
  !                sample plot.  If it has, the rate of spread is
  !                determined from the extent of infection at the end of
  !                the previous period
  !   RRGEN(IRRSP,2) target number of trees to put on sample plot
  !   RRRATE(IRRSP), SDRATE(IRRSP)  the average and the s.d. of the
  !   RRSARE       area of the sample plot used in the simulation
  !   RRSFRN       the amount of random variation when trees are evenly
  !                spaced
  !   RRSMEN -
  !   RRSRAD(i)    the species-typical root radius for a tree with the
  !                DBH of tree 'i'
  !   SAREA        stand area in acres
  !   SFPROB       the density of uninfected trees outside desease
  !                centers for the stand as a whole
  !   SICK(i)      0/1 to indicate whether tree 'i' is infected
  !   SINE(i)      the SIN of the angle of contact with infection for
  !                each tree
  !   TRURAD(i)    the realized root radius of each tree 'i', given EFFSDI
  !   UPDATE       1 = update RADINF when infection from a neighbour has
  !                    spread beyond the infection of the focal tree
  !                0 = don't update
  !   UPLTD        (UPDATE LIMITED) 1 = RADINF cannot be increased past
  !                a tree's center by later contact with an infected
  !                neighbour, 0 = it can be.  Where the increase in
  !                RADINF is limited to the center, an increment of
  !                between 0 and NRSTEP-1 years internal spread is added
  !                (based on a random number).  This is because infection
  !                will have been present for this long, on average,
  !                before it is noted by the program
  !                spread rates
  !   YTK(i)       the years required for rr to kill a tree with the DBH
  !                of tree 'i'.  We don't know whether this is time from
  !                first infection, of time from infection of the center
  !   YTKX         a multiplier of the YTK values calculated from a
  !                linear equation

  !  Revision History:
  !    21-MAR-00 Lance R. David (FHTET)
  !       Reduced RETURN statements to 1 at the end of routine.
  !       Added Debug code.
  !    07-AUG-01 Lance R. David (FHTET)
  !       Initialization of variable YFORWD near statement lable 300.
  !    19-AUG-14 Lance R. David (FMSC)
  !       Variable NTREES declared locally is in RDCOM.F77. Local
  !       declaration removed.
  !   09/12/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables. The implicit type for
  !     variable NEWDEN was not consistent with its use, declared REAL.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: DEBUG
  integer :: GCENTS,I,I1,I2,ICEN,IDI,IDPAL(50,50),II,IN,IRSSP(50),IRSTEP,IRSTP,IT,ITM,ITREES,J,JT,&
       &JTIME,JYEARS,K,KSP,KT,NEW,NOW,NUMPAL(50),ROOM,SICK(50),UPDATE,UPLTD
  real    :: DIFF,DISTNC(50,50),EFFSDI(ITOTSP),HABSP,NEWDEN,PNIN,PNSP,R,RDRANN,RDSLP,RKILLS(50),RPINT,&
       &RRSDIM,RRSMEN,SFPROB,SINE(50),TRURAD(50),YDMAX,YFORWD,YTK(50),YTKX

  !.... SEE IF WE NEED TO DO SOME DEBUG.

  call DBCHK (DEBUG,'RDSPRD',6,ICYC)

  if (DEBUG) write (JOSTND,*) 'ENTER RDSPRD'

  !.... INITIALIZE CONSTANTS THAT MAY ONE DAY DEPEND ON USER INPUTS
  !....
  !.... IF YTKX = .5 THEN YEARS TO KILL IS FROM INFECTION TO DEATH
  !.... IF YTKX = 1 THEN IT IS TIME FROM CENTER TO DEATH

  YTKX = 1

  do I = 1,ITOTSP
     EFFSDI(I) = 1.0
     12 continue
  end do

  UPDATE = 1
  UPLTD = 1
  NOW = 1
  NEW = 2

  !.... ZERO INFECTION ARRAYS

  RRRATE(IRRSP) = 0.0
  SDRATE(IRRSP) = 0.0

  do JTIME = 1,NMONT
     MCRATE(IRRSP,JTIME) = 0.0
     MCTREE(IRRSP,JTIME) = 0
     SPRQMD(IRRSP,JTIME) = 0.0
     5 continue
  end do

  GCENTS = NCENTS(IRRSP) - NSCEN(IRRSP)

  if (GCENTS .le.0.or. PAREA(IRRSP) .eq.0.0) then

     !....    don't need to zero out rrates any more since if GCENTS=0
     !....    rrates should be filled with shrinking values.

     if (NCENTS(IRRSP) .gt.0.0) then
        do ICEN = 1,NCENTS(IRRSP)
           RRRATE(IRRSP) = RRRATE(IRRSP) + RRATES(IRRSP,ICEN)
           10 continue
        end do

        RRRATE(IRRSP) = RRRATE(IRRSP) / NCENTS(IRRSP)
     end if

     goto 2000 !RETURN
  end if

  DIFF = SAREA - PAREA(IRRSP)
  if (DIFF .le.1E-3) goto 2000 !RETURN

  !.... IF STAND IS RUN AS ONE CENTER THEN THERE IS NO SPREAD TO
  !.... OUTSIDE AREAS. IF MANUAL INITIALIZATION CALCULATE SPREAD.

  if (LONECT(IRRSP) .eq.1) goto 2000 !RETURN

  if (IY(ICYC) .ge. IRGEN(4)) then

     !....    CHANGE THE TREE SPATIAL DISTRIBUTION TO THE OTHER TYPE
     !....    (RANDOM <-> REGULAR)

     ITM = IRSTYP
     if (ITM .eq.0) IRSTYP = 1
     if (ITM .eq.1) IRSTYP = 0
  end if

  !.... THIS CALCULATION IS BASED ON DENSITY OUTSIDE OF DISEASED AREAS
  !.... ONLY.  NOTE THAT ALL CACULATIONS ARE BASED ON THE LESSER OF LAST
  !.... YEAR'S FRINGE DENSITY AFTER BARK BEETLES AND WINDTHROW HAVE BEEN
  !.... APPLIED AND THIS YEAR'S OUTSIDE DENSITY. THE COMPARISON IS DONE
  !.... IN RDOAGM EVERY YEAR (EVEN IF THERE ARE NO BARK BEETLES ACTIVE)

  SFPROB = 0.0
  IDI = IRRSP

  do I = 1,ITRN
     if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(ISP(I)))
     if (IDI .eq. IRRSP) SFPROB = SFPROB + FFPROB(I,1)
     30 continue
  end do

  NEWDEN = SFPROB

  !.... GET LINEAR DIMENSIONS IN FEET

  if (IRSTYP .eq.0) then
     RRSARE = RRGEN(IRRSP,2) / (NEWDEN + 1E-9)
  else
     ITREES = IFIX(SQRT(RRGEN(IRRSP,2)))
     RRSARE = (ITREES ** 2) / (NEWDEN + 1E-9)
  end if

  if (NEWDEN .le.0) goto 1020

  RRSDIM = SQRT(RRSARE) * 208.7

  if (DEBUG)&
       &write (JOSTND,884) RRSARE,RRGEN(IRRSP,2),RRGEN(IRRSP,1)
  884 format (' RRSARE RRGEN(IRRSP,2),RRGEN(IRRSP,1)',4F10.2)

  do JTIME = 1,NMONT

     !....    SELECT TREES FOR THE AREA TO BE SIMULATED

     NTREES = 0
     IDI = IRRSP

     do KSP = 1,MAXSP
        if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(KSP))

        !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
        !     If non-host species then IDI = 0, and loop should be skipped to
        !     prevent array out of bounds error

        if (IDI .le.0) go to 100

        HABSP = HABFAC(IRTSPC(KSP),IDI,IRHAB)

        !....       MODIFY THE TIME TO DEATH MULTIPLIER (HABSP) BASED ON THE
        !....       PROPORTION OF CENTERS THAT ARE SPORE INITIATED (SPPROP).

        HABSP = HABSP * ((SPPROP(IDI) * SPYTK(IDI)) +&
             &(1 - SPPROP(IDI)))

        if ((ISCT(KSP,1) .eq.0) .or. (IDI .ne. IRRSP)) goto 100

        I1 = ISCT(KSP,1)
        I2 = ISCT(KSP,2)

        do J = I1,I2
           I = IND1(J)

           !....          CALCULATE NUMBER OF TREES OF CLASS IN A
           !....          IF TREE IS NOT OUTSIDE INFECTED AREA THEN DELETE

           if (DEBUG) write (JOSTND,778) PROBI(I,1,1), PROBIU(I),&
                &PROBIT(I), FFPROB(I,1)
           778 format(' PROBI PROBIU PROBIT FFPROB',4F10.2)

           if (FFPROB(I,1) .eq.0.0) goto 90

           RRSMEN = FFPROB(I,1) * RRSARE
           NUMTRE = IFIX(RRSMEN)
           PTRE = RRSMEN - NUMTRE
           R = RDRANN(0)
           if (R .le. PTRE) NUMTRE = NUMTRE + 1

           if (DEBUG) write (JOSTND,885) FFPROB(I,1), NUMTRE
           885 format (' FFPROB=',F10.2,'NUMTRE=',I4)

           if (NUMTRE .eq.0) goto 90

           !....          GET TREE INFORMATION FROM TREE LIST

           do K = 1,NUMTRE
              NTREES = NTREES + 1

              if (NTREES .gt.50) then
                 NTREES = 50
                 goto 90
              end if

              RRSDBH(NTREES) = DBH(I)
              RRSRAD(NTREES) = ROOTL(I)
              IRSSP(NTREES) = KSP
              YTK(NTREES) = RDSLP(RRSDBH(NTREES),XXINF,YYINF,NNINF)

              if (DEBUG) write (JOSTND,328) RRSDBH(NTREES),&
                   &YTK(NTREES)
              328 format(' RRSDBH,YTK=',2F10.2)

              YTK(NTREES) = (YTK(NTREES) - XMINKL(IRRSP)) * HABSP *&
                   &RRPSWT(IRTSPC(KSP)) + XMINKL(IRRSP)

              80 continue
           end do
           90 continue
        end do
        100 continue
     end do

     !....    MODIFY THE ROOT RADIUS TO ACCOUNT FOR CROWDING (SDI)
     !....    AND (POSSIBLY) CHANGE THE YTK TO REFLECT # YEARS FROM
     !....    INFECTION TO DEATH ALSO CALCULATE THE QUADRATIC MEAN DIAMETER
     !....    OF THE TREES INVOLVED IN THE SIMULATION

     do I = 1,NTREES
        TRURAD(I) = RRSRAD(I) * EFFSDI(IRTSPC(IRSSP(I)))
        RKILLS(I) = TRURAD(I) * PKILLS(IRTSPC(IRSSP(I)),IRRSP)
        YTK(I) = YTKX * YTK(I)

        SPRQMD(IRRSP,JTIME) = SPRQMD(IRRSP,JTIME) + RRSDBH(I) ** 2.0
        105 continue
     end do

     SPRQMD(IRRSP,JTIME) = SQRT(SPRQMD(IRRSP,JTIME) / FLOAT(NTREES))
     MCTREE(IRRSP,JTIME) = NTREES

     !....    SET UP TREE POSITIONS

     if (IRSTYP .ne.1) then

        !....       RANDOM

        do IT = 1,NTREES
           XRRS(IT) = RDRANN(0) * RRSDIM
           YRRS(IT) = RDRANN(0) * RRSDIM
           150 continue
        end do

     else

        !....       REGULAR

        ITREES = IFIX(SQRT(FLOAT(NTREES)))
        KT = 0

        do IT = 1,ITREES
           do JT = 1,ITREES
              KT = KT + 1
              XRRS(KT) = (IT - 0.5) * RRSDIM / FLOAT(ITREES) +&
                   &(RDRANN(0) * 2 - 1) * RRSFRN
              YRRS(KT) = (JT - 0.5) * RRSDIM / FLOAT(ITREES) +&
                   &(RDRANN(0) * 2 - 1) * RRSFRN
              170 continue
           end do
           175 continue
        end do

        NTREES = KT
     end if

     !....    DETERMINE DISTANCE BETWEEN EVERY PAIR OF TREES

     do I = 1, NTREES
        do J = (I + 1), NTREES
           DISTNC(I,J) = SQRT((XRRS(I) - XRRS(J)) ** 2 +&
                &(YRRS(I) - YRRS(J)) ** 2)
           DISTNC(J,I) = DISTNC(I, J)
           200 continue
        end do
        205 continue
     end do

     !....    (RE)INITIALIZE TREE VARIABLES

     do I = 1, NTREES
        SICK(I) = 0
        RADINF(I, 1) = -TRURAD(I)
        RADINF(I, 2) = -TRURAD(I)
        NUMPAL(I) = 0
        210 continue
     end do

     !....    FIND ALL POSSIBLE CONTACT-TREES OF EACH TREE

     do I = 1, NTREES
        do J = (I + 1), NTREES
           if (TRURAD(I) + TRURAD(J) .ge. DISTNC(I, J)) then
              NUMPAL(I) = NUMPAL(I) + 1
              NUMPAL(J) = NUMPAL(J) + 1
              IDPAL(I, NUMPAL(I)) = J
              IDPAL(J, NUMPAL(J)) = I
           end if
           220 continue
        end do
        225 continue
     end do

     !....    LOOP OVER TIME STEPS OF SIMULATION

     ROOM = 1
     JYEARS = 0

     RPINT = PINT

     do IRSTEP = 1, IRSNYR
        if (ROOM .gt.0) then
           JYEARS = JYEARS + NRSTEP

           do IN = 1, NTREES
              KSP = IRSSP(IN)

              if (RADINF(IN, NOW) .lt. RKILLS(IN)) then
                 PNSP = PNINF(IRTSPC(KSP),IRRSP)

                 !....                DETERMINE WHETHER UNINFECTED TREES ON THE BASELINE
                 !....                HAVE BECOME INFECTED

                 if ((SICK(IN) .eq.0) .and.&
                      &(YRRS(IN) .le. TRURAD(IN))) then
                    R = RDRANN(0)

                    if (IRSTEP .eq.1) then
                       PNIN = PNSP
                    else
                       PNIN = 1 - ((1 - PNSP) ** (NRSTEP / RPINT))
                    end if

                    if (R .le. PNIN) then
                       SICK(IN) = 1
                       RADINF(IN, NEW) = -YRRS(IN)
                       SINE(IN) = 1
                    end if
                 end if

                 !....                DETERMINE WHETHER UNINFECTED TREES BECOME INFECTED,
                 !....                AND, IF UPDATE = 1, WHETHER INFECTED TREES BECOME
                 !....                MORE INFECTED, THROUGH CONTACT WITH NEIGHBOURS
                 !....                WHOSE INFECTION IS SPREADING.  IF UPTLD=1, THEN THE
                 !....                INCREASE IN INFECTION IS LIMITED TO THE CENTER
                 !....                OF THE TREE, WITH AN INCREMENT FOR SPREAD DURING
                 !....                THE 0 TO (NRSTEP-1) YEARS SINCE INFECTION AT THE
                 !....                CENTER WILL HAVE OCCURRED.

                 if ((IRSTEP .gt.1) .and.&
                      &((SICK(IN) .eq.0) .or.&
                      &((UPDATE .eq.1) .and.&
                      &((UPLTD .eq.0) .or.&
                      &(RADINF(IN, NOW) .lt.0))))) then

                    do II = 1, NUMPAL(IN)
                       IT = IDPAL(IN, II)

                       if (RADINF(IT, NOW) .gt.0) then
                          if (RADINF(IN, NEW) .lt.&
                               &-(DISTNC(IT, IN) -&
                               &RADINF(IT, NOW))) then

                             R = RDRANN(0)

                             PNIN = (1 - (1 - PNSP) **&
                                  &(NRSTEP / FINT))

                             if (R .le. PNIN) then
                                SICK(IN) = 1
                                RADINF(IN,NEW) = -(DISTNC(IT,IN) -&
                                     &RADINF(IT,NOW))

                                if (DISTNC(IN, IT) .gt.0) then
                                   SINE(IN) = (YRRS(IN) -&
                                        &YRRS(IT)) /&
                                        &DISTNC(IT, IN)
                                else
                                   SINE(IN) = 1
                                end if
                             end if

                          end if
                       end if
                       320 continue
                    end do

                    if ((UPLTD .eq.1) .and.&
                         &(RADINF(IN,NEW) .gt.0)) then

                       IRSTP = IFIX(RDRANN(0) * FLOAT(NRSTEP))

                       if (IRSTP .eq. NRSTEP) IRSTP = NRSTEP - 1
                       RADINF(IN, NEW) = FLOAT(IRSTP) * RRSRAD(IN) *&
                            &PKILLS(IRTSPC(KSP),IRRSP) /&
                            &YTK(IN)
                    end if

                 end if

                 !....                SPREAD ROT THROUGH INFECTED ROOTS

                 if (SICK(IN) .eq.1) then
                    RADINF(IN,NEW) = RADINF(IN, NEW) + NRSTEP *&
                         &RRSRAD(IN) *&
                         &PKILLS(IRTSPC(KSP),IRRSP) /&
                         &YTK(IN)

                    if (RADINF(IN, NEW) .ge. RKILLS(IN)) then
                       RADINF(IN,NEW) = PCOLO(IRTSPC(KSP),IRRSP) *&
                            &TRURAD(IN)
                    end if
                 end if

                 if ((YRRS(IN) + RADINF(IN,NEW)) .gt. RRSDIM)&
                      &ROOM = 0
              end if

              290 continue
           end do
        end if

        !....       UPDATE THE PRESENT RADII OF INFECTION, AND BEGIN NEXT
        !....       TIME STEP

        if (ROOM .gt.0) then
           do I = 1, NTREES
              RADINF(I, NOW) = RADINF(I, NEW)
              295 continue
           end do
        end if

        300 continue
     end do

     !....    DETERMINE MAXIMUM SPREAD OF INFECTION.  IF INFECTION HAS
     !....    CROSSED THE WHOLE SAMPLE PLOT, CALCULATE THE RATE FROM ITS
     !....    EXTENT IN THE PREVIOUS TIME PERIOD (SUBSTITUTING THE PLOT
     !....    HEIGHT IF INFECTION CROSSED THE SAMPLE PLOT IN THE FIRST
     !....    TIME PERIOD).

     YDMAX = 0.0
     YFORWD = 0.0

     do IN = 1, NTREES
        if (SICK(IN) .gt.0) then

           if (RADINF(IN, NOW) .gt.0) then
              YFORWD = YRRS(IN) + RADINF(IN, NOW)
           else
              if (SINE(IN) .gt.0) then
                 YFORWD = YRRS(IN) - (SINE(IN)*(-RADINF(IN,NOW)))
              end if
           end if

           if (YFORWD .gt. YDMAX) YDMAX = YFORWD

        end if
        330 continue
     end do

     if (ROOM .gt.0) then
        MCRATE(IRRSP,JTIME) = YDMAX / JYEARS
     else
        if (JYEARS .gt. NRSTEP) then
           MCRATE(IRRSP,JTIME) = YDMAX / (JYEARS - NRSTEP)
        else
           MCRATE(IRRSP,JTIME) = RRSDIM / JYEARS
        end if
     end if

     RRRATE(IRRSP) = RRRATE(IRRSP) + MCRATE(IRRSP,JTIME)

     1000 continue
  end do

  RRRATE(IRRSP) = RRRATE(IRRSP) / (FLOAT(NMONT))

  do I = 1, NMONT
     SDRATE(IRRSP) = SDRATE(IRRSP) + (MCRATE(IRRSP,I) -&
          &RRRATE(IRRSP)) ** 2
     1010 continue
  end do

  SDRATE(IRRSP) = SQRT(SDRATE(IRRSP) / FLOAT(NMONT - 1))

  1020 continue

  if (IRSPTY .eq.1) then

     !....    DETERMINE WHAT SPREAD RATE TO ASSIGN TO EACH CENTER.
     !....    AVERAGE (rrrate) IS RECALCULATED FROM APPLIED RATES.

     call RDRATE (NMONT)
  end if

  2000 continue
  if (DEBUG) write (JOSTND,2) RRRATE(IRRSP)
  2 format (' EXIT RDSPRD:  RRRATE=',F9.2)

  return
end
