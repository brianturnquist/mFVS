!=============================!
!   canada/newmist : mistoe   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine MISTOE
  implicit none

  !  **MISTOE--MS  Date of last revision:  03/10/05

  !  Purpose:
  !     This is the main dwarf mistletoe processing routine. It reads the
  !  MISTMULT keyword, calculates changes in individual tree record
  !  mistletoe ratings (intensification and spread) using the Hawksworth
  !  6 point rating scheme, and keeps track of species/stand infection
  !  levels. It also calls routines that determine mistletoe mortality,
  !  and that automatically infect a stand with mistletoe.  It no longer
  !  calls the routine that prints mistletoe output tables.

  !  Call list definitions:

  !  Local variable definitions:
  !     BCONST: Constant for probability of increase equation.
  !     BDMR:   Coefficients for prob(+); depends on DMR.
  !     BHTG:   Multiplier for probability(+) for height growth.
  !     BTPA:   Multiplier for probability(+) for TPA.
  !     DCONST: Constant for probability of decrease equation.
  !     DDMR:   Multiplier for probability(-); based on DMR.
  !     DEBUG:  Logical flag to turn debug on or off.
  !     DHTG:   Multiplier for probability(-) for height growth.
  !     DTPA:   Multiplier for probability(-) for TPA.
  !     HTGR10: 10 year height growth (or whatever the cycle length).
  !     I1:     Beginning tree record pointer for a species.
  !     I2:     Ending tree record pointer for a species.
  !     I3:     Tree record loop counter.
  !     IACTK:  Passed back from OPGET (unused).
  !     IDATE:  Passed back from OPGET (unused).
  !     IDMR:   Current tree mistletoe rating.
  !     IPLT:   Plot (point) loop counter.
  !     ISPC:   Species loop counter.
  !     ITEMP:  Temporary storage for sorting top 4 most infected species.
  !     NEWSI:  Logical flag, true if new S+I model is to be used.
  !     NP:     Passed back from OPGET (unused).
  !     NTODO:  Number of actions to perform in a cycle.
  !     PMINUS: Probability of a rating decreasing.
  !     PPLUS:  Probability of a rating increasing.
  !     PRM:    Array containing MISTMULT keyword values.
  !     SMR:    Array to hold stand average mistletoe ratings by species.
  !     TEMP:   Temporary storage for sorting top 4 most infected species.
  !     TOTTPA: Total trees per acre for the species.
  !     XNUM:   Random number.

  ! Common block variables and parameters:
  !     DMRATE: From DMCOM;
  !     DMTALL: From MISCOM; height of tallest infected tree by spec and plot.
  !     FINT:   From PLOT;   current cycle length.
  !     HTG:    From ARRAYS; predicted height growth this cycle.
  !     ICYC:   From CONTRL; cycle index number.
  !     IMC:    From ARRAYS; management code.
  !     IMIST:  From MISCOM; individual tree record mistletoe rating.
  !     IND1:   From ARRAYS; tree list access.
  !     ISCT:   From CONTRL; species ordered list pointer.
  !     ITRE:   From CONTRL; individual tree record plot (point) number.
  !     ITRN:   From CONTRL; current number of tree records in stand.
  !     IVARNT: From MISCOM; current variant number (see MISINT).
  !     IY:     From CONTRL; inventory year.
  !     JOSTND: From CONTRL; unit number for stand output.
  !     MAXSP:  From PRGPRM; maximum number species.
  !     MAXPLT: From PRGPRM; maximum number of sample plots (points).
  !     MAXTRE: From PRGPRM; maximum number tree records.
  !     MISFIT: From MISCOM; which species affected by DM (see MISINT).
  !     PROB:   From ARRAYS; (P) trees per acre per tree record.
  !     SPPDMI: From MISCOM; DMI by species and point.
  !     SPPTPA: From MISCOM; Trees per acre by species and point.
  !     USEMRT: From MISCOM; true if using this model's mortality calcs.
  !     YNGMLT: From MISCOM; multiplier for probability of DMR decrease.
  !     YPLMLT: From MISCOM; multiplier for probability of DMR increase.

  ! Revision History:
  !     25-JUN-96; Julie C. Williams-Cipriani (FHTET-FC)
  !        Added a check of the MISFIT array at the top of the species
  !        processing loop; we don't want to spread and intensify DM
  !        infections on non-valid species for this variant (non-host);
  !        so if a non-host species does contain DM infection from damage
  !        codes from the treelist, it will remain at the input DMR for
  !        the entire FVS run without changing.
  !     20-JUN-97; Lance R. David (FHTET)
  !        Added spread of mistletoe to trees when a uninfected tree is
  !        overstoried by an infected tree of the same species on the point.
  !        A uninfected tree that is overstoried has a 20% chance (by draw
  !        of random number) of being infected each cycle. Infection begins
  !        with a DMR of 1.
  !     14-OCT-97; Lance R. David (FHTET)
  !        This update is an attempt to provide a more rapid intensification
  !        in smaller trees when overstoried by infected trees on the point.
  !        The DMR for a tree will be increased by 2 instead of 1 when the
  !        current tree is overtoried its DMR is more than 2 ratings below
  !        the DMI of the sample point. The probability of DMR increase is
  !        still in effect for this situation. So, if a tree's DMR is to
  !        increase, it may increase by 2 instead of 1.
  !     27-FEB-1998; Lance R. David (FHTET)
  !        This update replaces (14-OCT-97).
  !        Intensification and spread of mistletoe when infected
  !        overstory is present on the plot (sample point) is addressed
  !        in this modification.
  !        Overstoried is defined as the height of a tree being less
  !        than 70% the height of the tallest infected tree of the
  !        same species on the same plot.
  !        Intensification in overstoried trees:
  !          The DMR on individual tree records will be increased by
  !          0, 1, 2, or 3 based on its current DMR and a distribution
  !          determined by Helen Maffei (R6 FHP) from currently available
  !          data. A random number is drawn for determining placement
  !          in the magnitude-of-DMR-increase distribution.
  !        Spread when overstoried:
  !          Trees have a 55% chance of becoming infected when
  !          overstoried. Probability of infection is based on a random
  !          number draw. Overstoried trees that are selected for
  !          infection will begin with a DMR of 1 (69%), 2 (18%) or
  !          3 (13%).
  !        Additional note:
  !          All affected tree species use the same distributions for
  !          spread and intensification, when overstoried, at this
  !          time. It is expected that species-specific distributions
  !          will be developed when possible.
  !     09-MAR-1998; Lance R. David (FHTET)
  !          The intensification distribution for DMR 2 and 3 have been
  !          set evenly between increase of 1, 2 or 3 ratings.
  !     02-AUG-99; Lance R. David (FHTET-FC)
  !        Added definition and data statements for MYACTS array
  !        and replaced the activity code literal in the CALL OPFIND
  !        statements.
  !        This change was provided by Bob Havis (FMSC) to eliminate
  !        LF95 FORTRAN compiler warnings.
  !     23-DEC-2004  Lance R. David (FHTET)
  !        Removed inclusion of MISPRM.F77 parameter file.
  !     04-AUG-2005  Lance R. David (FHTET)
  !        Corrected code dealing with spread to trees not influenced
  !        by overstory. Initial infections (change off DMR=0 to DMR=1)
  !        did not take place on trees without an overstory source.
  !     10-MAR-2008  Lance R. David (FHTET)
  !        Removed local declaration of DMFLAG, variable is now in common.
  !        Also removed it as argument in call to MISINF.

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'MISCOM.f90'
  include 'PLOT.f90'
  include 'DMCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: DEBUG,NEWSI
  real    :: BDMR(7),BCONST,BTPA,BHTG,DDMR,DCONST,DTPA,DHTG
  real    :: PRM(4),SMR(MAXSP)
  real    :: HTGR10,P,PMINUS,PPLUS,TOTTPA,XNUM
  integer :: I,I1,I2,I3,IACTK,IDATE,IDMR,IPLT,ISPC,MYACTS(1),NP,NTODO

  !.... Data statements.
  data MYACTS(1) / 2001 /

  !.... Probability +/- equation coefficients.  Presently the same for
  !.... all variants and all species.

  data (BDMR(I),I = 1,7)&
       &/0.0, 2.45047, 2.30723, 1.8809, 2.11457, 1.43293, 0.0/

  data BCONST   /-1.67226/
  data BTPA     /-0.0012397/
  data BHTG     /-0.0747205/

  data DDMR     /0.0983757/
  data DCONST   /-5.59798/
  data DTPA     /-1.15053E-4/
  data DHTG     /0.013267/

  !.... Check for debug.

  call DBCHK(DEBUG,'MISTOE',6,ICYC)

  if (DEBUG) write(JOSTND,100)ICYC
  100 format(' Begin MISTOE: Cycle = ',I5)

  !.... If there are no trees in the stand skip out.

  if (ITRN.eq.0.0) go to 9000

  !.... Initializations.

  DMFLAG = .false.

  !.... If the new S+I model is in effect and if the MISTOFF
  !.... keyword is not active, then NEWSI is true.

  NEWSI = .false.
  if (NEWMOD .and. MISFLG) NEWSI = .true.

  do ISPC = 1, MAXSP
     do IPLT = 1, MAXPLT
        DMTALL(ISPC,IPLT) = 0.0
        SPPDMI(ISPC,IPLT) = 0.0
        SPPTPA(ISPC,IPLT) = 0.0
        105 continue
     end do
     110 continue
  end do

  !.... Process MISTMULT keyword - get probability multipliers.
  !.... Skip this if the new S+I model is in use.

  if (.not.NEWSI) then
     NTODO = 0
     call OPFIND(1,MYACTS(1),NTODO)
     if (NTODO.ne.0) then
        do I = 1,NTODO
           call OPGET(I,3,IDATE,IACTK,NP,PRM)
           call OPDONE(I,IY(ICYC))

           !....       Check for particular species number.
           !....       MISFIT insures that only species of this variant that
           !....        are affected by mistletoe will get prob. multipliers -
           !....        others will be set to 0.

           ISPC = IFIX(PRM(1))
           if (ISPC.ne.0) then
              YPLMLT(ISPC) = PRM(2)*MISFIT(ISPC)
              YNGMLT(ISPC) = PRM(3)*MISFIT(ISPC)
           else

              !....          Otherwise defaults to all species.

              do ISPC = 1,MAXSP
                 YPLMLT(ISPC) = PRM(2)*MISFIT(ISPC)
                 YNGMLT(ISPC) = PRM(3)*MISFIT(ISPC)
                 125 continue
              end do
           end if
           130 continue
        end do
     end if
  end if

  !.... Select method of calculation.
  !....  New model calls DMTREG, then maps results back to IMIST.
  !....  (ELSE) Interim model works directly.

  if (NEWSI) then

     call DMTREG
     call DMMDMR(DMRATE,IMIST)

  else

     !.... Top of species loop.

     do ISPC = 1,MAXSP
        I1 = ISCT(ISPC,1)

        !....     If there are no trees of this species, skip out.

        if (I1.eq.0) go to 300
        I2 = ISCT(ISPC,2)

        !....     If this species (in this variant) is not affected by mistletoe,
        !....     skip out.

        if (MISFIT(ISPC).eq.0) go to 300

        !....     Get total TPA for this species and check for any mistletoe.

        TOTTPA = 0.0
        SMR(ISPC) = 0.0
        do I3 = I1,I2
           I = IND1(I3)
           P = PROB(I)
           TOTTPA = TOTTPA+P
           SMR(ISPC) = SMR(ISPC)+IMIST(I)*P
           160 continue
        end do

        !....     If the stand is mistletoe free, skip the infection probability
        !....     calculations.

        SMR(ISPC) = SMR(ISPC)/TOTTPA
        if (SMR(ISPC).eq.0.0) go to 250

        !....     At least one species has mistletoe; set flag.

        DMFLAG = .true.

        !....     Loop through tree records of the current species.
        !....     -- Save height of tallest infected tree on each point.
        !....     -- Accumulate DMR*TPA and TPA for infected trees for calculation
        !....        of DMI on each point.
        !....        Note: The SPPDMI calculation does not seem to be needed per
        !....              discussion with Tommy Gregg on 6/18/97 and may want to
        !....              remove the code. But what I've done so far might not
        !....              be the right or permanent solution for spreading
        !....              infections to smaller trees so will leave for now.
        !....              -Lance David

        do I3 = I1, I2
           I = IND1(I3)
           IPLT = ITRE(I)
           if (IMIST(I) .gt.0) then
              SPPDMI(ISPC,IPLT) = SPPDMI(ISPC,IPLT)+(IMIST(I)*PROB(I))
              SPPTPA(ISPC,IPLT) = SPPTPA(ISPC,IPLT)+PROB(I)
              if (HT(I) .gt. DMTALL(ISPC,IPLT)) then
                 DMTALL(ISPC,IPLT) = HT(I)
              end if
           end if
           180 continue
        end do

        !....     Now loop through the points and divide the accumulated DMIs
        !....     (DMRs of infected trees only) by the total number of infected
        !....     trees.

        do IPLT = 1, MAXPLT
           if (SPPDMI(ISPC,IPLT) .gt.0.0&
                &.and. SPPTPA(ISPC,IPLT) .gt.0.0) then
              SPPDMI(ISPC,IPLT) = SPPDMI(ISPC,IPLT)/SPPTPA(ISPC,IPLT)
           end if
           185 continue
        end do

        !....     Loop for each tree record of the current species.

        do I3 = I1,I2
           I = IND1(I3)
           IDMR = IMIST(I)
           IPLT = ITRE(I)
           HTGR10 = HTG(I)
           PPLUS = 0.0
           PMINUS = 0.0

           !....       If mistletoe rating is already at 6 (highest) skip the
           !....       calculation for an increase in the rating probability.

           if (IDMR.ge.6) then
              IDMR = 6
              go to 200
           end if

           !....       Probability of an increase in rating, over 10 years.
           !....       HTGR10 is adjusted to give 1 decade's growth.

           PPLUS = BCONST+BDMR(IDMR+1)+BHTG*(HTGR10*10.0/FINT)&
                &+BTPA*TOTTPA

           !....       Make sure trees which are not affected by DM (i.e. PPLUS=0)
           !....       don't get thrown in here anyway.

           if (PPLUS.ne.0.0) then
              PPLUS = (1.0/(1.0+EXP(-PPLUS)))*YPLMLT(ISPC)
           end if

           !....       Scale the 10-year probability to FINT cycle length.

           if (PPLUS.ge.1.0) then
              PPLUS = 1.0
           else
              PPLUS = 1.0-(1.0-PPLUS)**(FINT/10.0)
           end if

           !....       If mistletoe rating is already at 0 (lowest) skip the
           !....       calculation for a decrease in the rating probability.

           200 continue
           if (IDMR.ne.0) then

              !....         An update to the DMR (IMIST) needs to be made.
              !....
              !....         Probability of a decrease in rating over 10 years.
              !....         HTGR10 is adjusted to give 1 decade's growth.

              PMINUS = DCONST+DDMR*IDMR+DHTG*(HTGR10*10.0/FINT)&
                   &+DTPA*TOTTPA

              !....         Same as PPLUS - see above.

              if (PMINUS.ne.0.0)&
                   &PMINUS = (1.0/(1.0+EXP(-PMINUS)))*YNGMLT(ISPC)

              !....         Scale the 10-year probability to FINT cycle length.

              if (PMINUS.ge.1.0) then
                 PMINUS = 1.0
              else
                 PMINUS = 1.0-(1.0-PMINUS)**(FINT/10.0)
              end if

              !....         Grab a uniform random number (between 0 and 1).

              call RANN(XNUM)

              !....         Update mistletoe ratings - NOTE: PPLUS or PPMINUS = 0 should
              !....         not affect IMIST because XNUM should always be 0 or greater.
              !....
              !....         27-FEB-1998
              !....         Intensification in smaller trees when overstoried by
              !....         infected trees of the same species on the point. The DMR
              !....         for individual tree records will be increased by 0, 1, 2,
              !....         or 3 based on its current DMR and a distribution.
              !....         Each current DMR value is treated differently, once it has
              !....         been selected to be increased, as to how many ratings the
              !....         DMR will change. A seperate random number is drawn for
              !....         determining placement in the magnitude-of-DMR-increase
              !....         distribution.
              !....
              !....         start of original intensification code
              !....         IF(IDMR.NE.6) THEN
              !....            IF(PPLUS.GT.XNUM) IMIST(I)=IMIST(I)+1
              !....         ENDIF
              !....         IF(IDMR.NE.0) THEN
              !....            IF(PMINUS.GT.XNUM) IMIST(I)=IMIST(I)-1
              !....         ENDIF
              !....         end of original intensification code

              if (IDMR.ne.6) then
                 if (PPLUS.gt.XNUM) then

                    !....           Use of point DMI to influence intensification
                    !....           is not used. 27-FEB-98
                    !*              IF (SPPDMI(ISPC,IPLT) .GT. (IDMR + 1) .AND.
                    !*   &              (DMTALL(ISPC,IPLT) * 0.7) .GT. HT(I)) THEN
                    !*                IMIST(I) = IMIST(I) + 2
                    !*              ELSE
                    !*                IMIST(I) = IMIST(I) + 1
                    !*              ENDIF

                    !....           The tree has been selected for an increase in DMR.
                    !....           If it is overstoried by an infected tree, another
                    !....           random number is drawn to determine the magnitude
                    !....           of the increase (1, 2 or 3).

                    if (DMTALL(ISPC,IPLT) * 0.7.gt. HT(I)) then

                       call RANN(XNUM)
                       if (IMIST(I) .eq.1) then
                          if (XNUM .lt.0.61) then
                             IMIST(I) = IMIST(I) + 1
                          else if (XNUM .lt.0.83) then
                             IMIST(I) = IMIST(I) + 2
                          else
                             IMIST(I) = IMIST(I) + 3
                          end if
                       else if (IMIST(I) .eq.2) then
                          !.... 09-MAR-98 LRD        IF (XNUM .LT. 0.45) THEN
                          if (XNUM .lt.0.34) then
                             IMIST(I) = IMIST(I) + 1
                             !.... 09-MAR-98 LRD       ELSEIF (XNUM .LT. 0.73) THEN
                          else if (XNUM .lt.0.67) then
                             IMIST(I) = IMIST(I) + 2
                          else
                             IMIST(I) = IMIST(I) + 3
                          end if
                       else if (IMIST(I) .eq.3) then
                          !.... 09-MAR-98 LRD   IF (XNUM .LT. 0.23) THEN
                          if (XNUM .lt.0.34) then
                             IMIST(I) = IMIST(I) + 1
                             !.... 09-MAR-98 LRD        ELSEIF (XNUM .LT. 0.79) THEN
                          else if (XNUM .lt.0.67) then
                             IMIST(I) = IMIST(I) + 2
                          else
                             IMIST(I) = IMIST(I) + 3
                          end if
                       else if (IMIST(I) .eq.4) then
                          if (XNUM .lt.0.55) then
                             IMIST(I) = IMIST(I) + 1
                          else
                             IMIST(I) = IMIST(I) + 2
                          end if
                       else
                          IMIST(I) = IMIST(I) + 1
                       end if
                    else
                       IMIST(I) = IMIST(I) + 1
                    end if
                    if (IMIST(I) .gt.6) IMIST(I) = 6
                 end if
              end if

              !....         Check to see if decrease in DMR will occur.
              if (IDMR.ne.0) then
                 if (PMINUS.gt.XNUM) IMIST(I) = IMIST(I) - 1
              end if

           else
              !....          The tree is not infected (DMR=0), introduce infection.
              !....          Trees have a 55% chance of being infected when overstoried.
              !....          Probability of infection is based on a random number draw.
              !....          A tree is considered overstoried when it is 70% or less the
              !....          height of the tallest infected tree of the same species on
              !....          the plot. Overstoried tree's that are selected for infection
              !....          will begin with a DMR of 1 (69%), 2 (18%) or 3 (13%).
              !....          Trees not influenced by infected overstory, use the original
              !....          even-age spread probability.

              call RANN(XNUM)

              if ((DMTALL(ISPC,IPLT) * 0.7) .gt. HT(I)) then
                 !....           55% chance that uninfected tree will becom infected.
                 if (XNUM .lt.0.55) then
                    call RANN(XNUM)

                    if (XNUM .lt.0.69) then
                       IMIST(I) = 1
                    else if (XNUM .lt.0.87) then
                       IMIST(I) = 2
                    else
                       IMIST(I) = 3
                    end if
                 end if
              else
                 if (PPLUS .gt. XNUM) IMIST(I) = 1
              end if
           end if

           !....    Bottom of tree record loop.

           240 continue
        end do

        !....    Set the management code to 3 for highly infected trees.

        250 continue
        do I3 = I1,I2
           I = IND1(I3)
           IDMR = IMIST(I)
           if (IDMR.ge.4) IMC(I) = 3
           260 continue
        end do

        !.... Bottom of species loop.

        300 continue
     end do

  end if

  !.... See if any forced mistletoe infection is desired and, if so,
  !.... add new infections to the stand. Also update the stand and
  !.... species infection totals if this was done.

  call MISINF

  !.... Check DMFLAG; if still FALSE then no mistletoe in any species in
  !.... this stand, this cycle so skip DM mortality routine.

  if (DMFLAG) call MISMRT(USEMRT)

  !.... Common return.

  9000 continue

  if (DEBUG) write(JOSTND,9010)ICYC
  9010 format(' End MISTOE: Cycle = ',I5)

  return
end
