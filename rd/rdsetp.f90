!=================!
!   rd : rdsetp   !
!=================!
! RD $Id$
subroutine RDSETP
  implicit none

  !  SUBROUTINE FOR CALCULATING VALUES OF ROOT DISEASE VARIABLES
  !  IN INITIALIZATION PHASE. ALSO CALLS RDAREA TO CALCULATE INITIAL
  !  ROOT DISEASE AREA AND RDPR TO PRINT REPORT OF ROOT DISEASE
  !  CONDITIONS. VALUES FOR THESE VARIABLES ARE ZEROED IN RDINIT.
  !  PROGNOSIS VARIABLE PROB MAY BE SET IN THIS ROUTINE IF SAMPLING
  !  ROOT DISEASE FROM TWO POPULATIONS (IE DISEASED AND NON-DISEASED
  !  SUB-PLOTS.) ROOT DISEASE MODEL VARIABLE PROBL IS THEN LOADED
  !  FROM PROB.

  !  Called By :
  !     RDMN1   [ROOT DISEASE]

  !  Calls :
  !     DBCHK   (SUBROUTINE)   [FVS]
  !     RDCLOC  (SUBROUTINE)   [ROOT DISEASE]
  !     RDAREA  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
  !     RDINOC  (SUBROUTINE)   [ROOT DISEASE]
  !     RDIPRP  (SUBROUTINE)   [ROOT DISEASE]

  !  Local Variables :
  !  <incomplete>

  !  Common Block Variables Used :
  !     DIMEN  - (RDCOM)   (O)
  !     FPROB  - (RDARRY)  (O)
  !     I1, I2 - (RDCOM)   (O)
  !     ICYC   - (CONTRL)  (I)
  !     IND1   - (ARRAYS)  (I)
  !     IPCFLG - (RDCOM)   (I)
  !     IRDPLT - (RDADD)   (I)
  !     IREC1  - (CONTRL)  (I)
  !     ISCT   - (CONTRL)  (I)
  !     JOSTND - (CONTRL)  (I)
  !     LONECT - (RDADD)   (O)
  !     MAXSP  - (PRGPRM)  (I)
  !     PAREA  - (RDCOM)   (I)
  !     PI     - (PLOT)    (I)
  !     PRINF  - (RDCOM)   (O)
  !     PRKILL - (RDCOM)   (I)
  !     PROB   - (PROB)    (I/O)
  !     PROBI  - (RDARRY)  (O)
  !     PROBIT - (RDARRY)  (I)
  !     PROBIU - (RDARRY)  (O)
  !     PROBL  - (RDARRY)  (O)
  !     PROPI  - (RDARRY)  (O)
  !     PRPTOT - (RDCOM)   (O)
  !     PRUN   - (RDCOM)   (I)
  !     RRGEN  - (RDCOM)   (O)
  !     RRINCS - (RDCOM)   (I)
  !     RRTINV - (RDADD)   (I)
  !     SAREA  - (RDCOM)   (I)
  !     <incomplete>

  !  Revision History :
  !     05/03/97 - Matthew K. Thompson (FHTET)
  !                Substituted the variable IREC1 for ITRN everywhere
  !                in the subroutine.  When RDSETP is called the value
  !                of ITRN (number of tree records) still includes
  !                recently dead trees.  IREC1 is the number of
  !                projectable records.

  !     05/13/97 - Matthew K. Thompson (FHTET)
  !                Changed code so that if number of plots equals number
  !                of disease plots than patch area equals stand area.
  !                Re-numbered statement labels.  Fixed call to RDIPRP,
  !                an element of the array TMP was being passed to RDIPRP
  !                not the whole array.  Removed the section of code that
  !                recalculates PROB.
  !     06/02/98 - Robert N. Havis (FHTET)
  !                Branched out of tree loop in several places
  !                when non-host tree record was beeing analysed
  !     17-JUL-2002 Lance R. David (FHTET)
  !                Modified/added debug code.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !.... Local variable declarations

  ! Variables
  logical :: DEBUG
  integer :: I,I1,I2,IDI,IIPI,IK,J,K,KSP,NDPLTS(ITOTRR)
  real    :: PROPN(MAXTRE),RDRANP,TMP(ITOTRR),TPROP

  !.... See if we need to do some debug.

  call DBCHK (DEBUG,'RDSETP',6,ICYC)
  if (DEBUG) write (JOSTND,100) ICYC,MINRR,MAXRR
  100 format(' ENTER RDSETP: CYCLE=',I4,' MINRR=',I1,' MAXRR=',I1)

  TPROP = 0.0
  do IDI = 1,ITOTRR
     NDPLTS(IDI) = 0
     TMP(IDI) = 0.0
  end do

  !.... Calculate number of diseased sub-plots.  If user has specified
  !.... diseased and non-diseased sub-plots and number of diseased plots
  !.... equals the number of total plots in the inventory then stand is
  !.... considered as one center.

  do IDI = MINRR,MAXRR
     do K = 1,50
        if (IRDPLT(IDI,K) .ne.0) then
           NDPLTS(IDI) = NDPLTS(IDI) + 1
        end if
        200 continue
     end do
     300 continue
  end do

  IIPI = INT(PI)
  do IDI = MINRR,MAXRR
     if (IIPI .eq. NDPLTS(IDI)) then
        LONECT(IDI) = 1
        PAREA(IDI) = SAREA
     end if

     if (LONECT(IDI) .eq.1) NDPLTS(IDI) = 1
     303 continue
  end do

  !.... Initialize the variables for the SDI effect on roots.

  if (SDISLP .eq.0.0.or. SDNORM .eq.0.0) then
     YINCPT = 1.0
  else
     YINCPT = 1.0 - (SDNORM * SDISLP)
  end if

  !.... Set square dimension of the stand.

  DIMEN = 0.0
  if (SAREA .ne.0.0) then
     DIMEN = SQRT(SAREA) * 208.7
  end if

  !.... Calculate the area of the disease patches.

  do IRRSP = MINRR,MAXRR
     if (IPCFLG(IRRSP) .eq.1) then

        !....       Disease centers are assigned by the user.

        call RDAREA
     else

        !....       DISEASE CENTERS ARE ASSIGNED RANDOMLY.

        if (PAREA(IRRSP) .gt.0.0) then
           call RDCLOC
           call RDAREA
        else
           PAREA(IRRSP) = SAREA * NDPLTS(IRRSP) / PI
           call RDCLOC
        end if
     end if

     if (DEBUG) write(JOSTND,400) PAREA(1), PAREA(2), PAREA(3),&
          &PAREA(4)
     400 format (' IN RDSETP: PAREA=',4F10.2)

     SPPROP(IRRSP) = 0.0

     !....    If not initialized from the tree list, use manual esitmate of
     !....    number of infected and uninfected trees in disease pockets??
     !....    PRKILL and PRUN are densities of trees inside centers. They
     !....    are changed into proportions here, using RRGEN, which is
     !....    then set to 1.0.

     if (.not. RRTINV .and..not. LPLINF) then
        RRGEN(IRRSP,1) = PRKILL(IRRSP) + PRUN(IRRSP)

        if (RRGEN(IRRSP,1) .gt.0.0) then
           PRKILL(IRRSP) = PRKILL(IRRSP) / RRGEN(IRRSP,1)
           PRUN(IRRSP) = PRUN(IRRSP) / RRGEN(IRRSP,1)
           RRGEN(IRRSP,1) = 1.0
        end if

     else if (.not. RRTINV .and. LPLINF) then
        RRGEN(IRRSP,1) = 1.0
     end if

     if (DEBUG) write (JOSTND,500) PI, NDPLTS(1), NDPLTS(2),&
          &NDPLTS(3), NDPLTS(4)
     500 format (' IN RDSETP: PI NDPLTS=',F3.0,4I5)
     600 continue
  end do

  !.... Figure out what proportion of each record becomes infected.
  !.... PROPN is by tree record.  If the PLOTINF keyword is used, the
  !.... same species may have different infection rates.

  if (.not. RRTINV .and..not. LPLINF) then
     call RDIPRP(PROPN,0,PRKILL)
  end if

  if (.not. RRTINV .and. LPLINF) then

     !....    Initialized via the PLOTINF keyword

     do IDI = MINRR,MAXRR
        TMP(IDI) = 0.0
        700 continue
     end do

     do IDI = MINRR,MAXRR
        if (IDI .gt. MINRR) TMP(IDI-1) = 0.0

        do IK = 1,50
           if (IANPLT(IDI,IK) .le.0) goto 900
           TMP(IDI) = PLPROP(IDI,IK)

           call RDIPRP(PROPN,IANPLT(IDI,IK),TMP)

           800 continue
        end do
        900 continue
     end do
  end if

  !.... Load the initial tree list from FVS into the root disease
  !.... tree list arrays that characterize the trees as inside and
  !.... outside of patches  (also infected and uninfected).

  !     Note

  IDI = MAXRR
  do I = 1, IREC1

     !.....   If Annosus then select S-type or P-type.

     !         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(I))) ! changed 11/24/2015

     !....    If both P and S type Annosus is being used, set disease area
     !....    to that of the disease type of the host tree species; otherwise,
     !....    use the disease area of the one disease being simulated even though
     !....    current tree may not be specified as host species.

     if (MAXRR .lt.3.and. MINRR .ne. MAXRR) then
        IDI = IDITYP(IRTSPC(ISP(I)))
     end if

     !     Branch outof loop for non-host tree ((RNH June98)

     if (IDI .le.0) go to 2100

     if (RRTINV .and..not. LPLINF) then

        !....       The model is being initialized from the tree list.

        if (IPRFL(I) .eq.1) then

           !....          This tree record has a severity code 1, tree within
           !....          30 feet of infected tree.  This tree is given an infection
           !....          level around 10% of the tree species infection level at
           !....          death.

           if (DEBUG) write (JOSTND,*) '# Severity Code 1.'

           TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.1
           PROPI(I,1,1) = RDRANP(TPROP)

        else if (IPRFL(I) .eq.2) then

           !....          This tree record has a severity code 2, pathogen or
           !....          diagnostic symptoms detected.  This tree is given an
           !....          infection level around 65% of the tree species infection
           !....          level at death.

           if (DEBUG) write (JOSTND,*) '# Severity Code 2.'

           TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.65
           PROPI(I,1,1) = RDRANP(TPROP)

        else if (IPRFL(I) .eq.3) then

           !....          This tree record has a severity code 3, crown
           !....          deterioration detected.  This tree is given an infection
           !....          level around 75% of the tree species infection level at
           !....          death.

           if (DEBUG) write (JOSTND,*) '# Severity Code 3.'

           TPROP = PKILLS(IRTSPC(ISP(I)),IDI) * 0.75
           PROPI(I,1,1) = RDRANP(TPROP)

        else

           !....          Model is initialized from the tree list.  This tree
           !....          record does not show infection (no root disease damage
           !....          code).  The tree record is given a random proportion
           !....          of roots infected for new infection. MEAN = 0.001.
           !....
           !....          I don't see why we initialize root disease level in
           !....          a tree with no root disease. It doesn't matter at any
           !....          point but seems useless.
           !              PROPI(I,1,1) = RDRANP(0.001)

           if (DEBUG) write (JOSTND,*) '# No Infection.'

        end if

        !....       Scale PROB variables based on how patches are being
        !....       defined.

        if (LONECT(IDI) .eq.1.or.&
             &(SAREA .eq. PAREA(IDI))) then

           !....          Stand being run as one disease center so scale PROB
           !....          variables by value of patch area.

           if (DEBUG) write (JOSTND,1400)
           1400 format(' # Run as 1 center, set PROBIU and PROBI.')

           PROBIU(I) = PROBIU(I) * PAREA(IDI)
           PROBI(I,1,1) = PROBI(I,1,1) * PAREA(IDI)
           FPROB(I) = 0.0

        else if (LONECT(IDI) .eq.0) then

           !....          Stand being run without knowledge of the patches so
           !....          scale PROB variables by value of patch area.

           if (DEBUG) write (JOSTND,1500)
           1500 format(' # No knowledge of patches, ',&
                &'set PROBIU and PROBI.')

           PROBIU(I) = PROBIU(I) * PAREA(IDI)
           PROBI(I,1,1) = PROBI(I,1,1) * PAREA(IDI)

           if (PAREA(IDI) .eq. SAREA) then

              !....             If patch area equals stand area then there are no
              !....             trees outside patches.

              if (DEBUG) write (JOSTND,1525)
              1525 format(' # No knowledge of patches, PAREA = SAREA.')

              FPROB(I) = 0.0

           else

              !....             Calculate trees/acre outside of the patches.

              if (DEBUG) write (JOSTND,1550)
              1550 format(' # No knowledge of patches, PAREA /= SAREA.')

              FPROB(I) = (PROB(I) * SAREA - (PROBIU(I) +&
                   &PROBI(I,1,1))) / (SAREA - PAREA(IDI))
           end if

        else if (LONECT(IDI) .eq.2) then

           !....          Stand being run as multiple plots so scale PROB
           !....          variables by value of the stand area.

           if (DEBUG) write (JOSTND,1600)
           1600 format(' # Run as multiple plots, ',&
                &'set PROBIU, PROBI, and FPROB.')

           PROBIU(I) = PROBIU(I) * SAREA
           PROBI(I,1,1) = PROBI(I,1,1) * SAREA
           FPROB(I) = FPROB(I) * (SAREA / (SAREA - PAREA(IDI)))
        end if

        if (DEBUG) write (JOSTND,1650) I, ISP(I), PROB(I),&
             &PROBI(I,1,1), PROBIU(I),&
             &FPROB(I)
        1650 format (' IN RDSETP: BEFORE-1 SCALE: I ISP PROB PROBI ',&
             &'PROBIU FPROB ', I4, I4, 4(2X,F10.2))

     else

        !....       Model is initialized manually from the RRINIT keyword
        !....       and/or the model is initialized through the PLOTINF keyword.
        !....

        if ((.not. LPLINF .or. IDPLOT(I) .gt.0) .and.&
             &IDI .ne.0) then

           !....          If (not using PLOTINF or tree records are inside
           !....          infected plots) and the tree species is a host then
           !....
           !....          The proportion of infection for each tree record is taken
           !....          from the random proportion function with the mean being
           !....          the value entered by the user in field 5 of the RRINIT
           !....          keyword.

           if (DEBUG) write (JOSTND,1700)
           1700 format(' # Using only RRINIT, or tree record inside ',&
                &'infected plot specified by PLOTINF ',&
                &'set PROBIU, PROBI, and FPROB.')

           if (PAREA(IDI) .gt.0.0)&
                &PROPI(I,1,1) = RDRANP(RRINCS(IDI))

           PROBI(I,1,1) = PROPN(I) * PROB(I) * PAREA(IDI) /&
                &(RRGEN(IDI,1) + 0.00001)
           PROBIU(I) = (1.0 - PROPN(I)) * PROB(I) *&
                &PAREA(IDI) / (RRGEN(IDI,1) + 0.00001)
           FPROB(I) = PROB(I)

        else

           !....          Initialized by PLOTINF keyword, but this record
           !....          belongs to a plot which is outside disease centers

           !     If non-host species exit loop (RNH May98)

           if (IDI .le.0) go to 2100

           if (DEBUG) write (JOSTND,1800)
           1800 format(' # Using PLOTINF, this tree is in plot which ',&
                &'is outside disease centers, set FPROB.')

           PROBI(I,1,1) = 0.0
           PROBIU(I) = 0.0
           FPROB(I) = (PROB(I) * SAREA - PROBIU(I) -&
                &PROBI(I,1,1)) / (SAREA - PAREA(IDI))

        end if

        if (DEBUG) write (JOSTND,1900) I, ISP(I), PROB(I),&
             &PROBI(I,1,1), PROBIU(I),&
             &FPROB(I)
        1900 format (' IN RDSETP: BEFORE-2 SCALE: I ISP PROB PROBI ',&
             &'PROBIU FPROB ', I4, I4, 4(2X,F10.2))

     end if

     !....    If stand is being modeled as diseased and non-diseased
     !....    subplots then modify PROB to recognize the effect of stand
     !....    being the sum of two populations.

     !M       IF (LONECT(IDI) .EQ. 2) THEN
     !M          PROB(I) = (PROBI(I,1,1) + PROBIU(I) + FPROB(I) *
     !M   &                (SAREA - PAREA(IDI))) / (SAREA + 1E-6)
     !M       ENDIF

     !....    Now load PROBL after PROB has been modified.

     PROBL(I) = PROB(I)

     if (DEBUG) write(JOSTND,2000) I,ISP(I),PROB(I),PROBI(I,1,1),&
          &PROBIU(I),FPROB(I)
     2000 format (' IN RDSETP: AFTER    SCALE: I ISP PROB PROBI ',&
          &'PROBIU FPROB ', I4, I4, 4(2X,F10.2))

     2100 continue
  end do

  !.... End tree loop.

  call RDSUM (IREC1,PROBIT,PROBI,ISTEP)

  !.... Decay the root system of stumps initialized through STREAD or
  !.... the treelist since the stumps may have been there for longer than
  !.... just the inventory year.

  call RDINOC (.true.)

  !.... Accumulate proportion of root systems infected inside patches
  !.... for output in inventory year.
  !....
  !.... (Remember that PRINF & PRPTOT contain RR info for first ITOTRR
  !.... slots and tree species info after that.)

  do IDI = 1,ITOTRR
     PRPTOT(IDI) = 0.0
     PRINF(IDI) = 0.0
     2200 continue
  end do

  IDI = MAXRR

  do KSP = 1,MAXSP
     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))

     !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
     !     If non-host species then IDI = 0, and loop should be skipped to
     !     prevent array out of bounds error

     if (IDI .le.0) go to 2400

     if (ISCT(KSP,1) .eq.0) goto 2400
     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)

     PRINF(KSP+ITOTRR) = 0.0
     PRPTOT(KSP+ITOTRR) = 0.0

     do J = I1,I2
        I = IND1(J)

        !           If IND1(j) is pointing to a dead tree (bottom part of tree
        !           list), then skip it. Also skip if PROPI < 0

        if (I .gt. IREC1) goto 2300

        if (PROPI(I,1,1) .gt.0.0) then
           PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) + PROBI(I,1,1) *&
                &PROPI(I,1,1)
           PRINF(IDI) = PRINF(IDI) + PROBI(I,1,1) * PROPI(I,1,1)
        end if

        PRPTOT(KSP+ITOTRR) = PRPTOT(KSP+ITOTRR) + PROBI(I,1,1)
        PRPTOT(IDI) = PRPTOT(IDI) + PROBI(I,1,1)
        2300 continue
     end do

     PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) /&
          &(PRPTOT(KSP+ITOTRR) + 1E-6)

     2400 continue
  end do

  do IDI = MINRR,MAXRR
     PRINF(IDI) = PRINF(IDI) / (PRPTOT(IDI) + 1E-6)
     2500 continue
  end do

  if (DEBUG) write (JOSTND,*) 'EXIT RDSETP'

  return
end
