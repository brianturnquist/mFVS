!=================!
!   rd : rdiprp   !
!=================!
! RD $Id$
subroutine RDIPRP(PROPN,NOPLOT,PRAN)
  implicit none

  !  Purpose :
  !     This subroutine calculates multipliers for each species for
  !     distributing the user-defined number of infected trees in a
  !     center.  The proportion of each species is defined by the
  !     infection probability and the average time to death for the
  !     species.

  !  Called By :
  !     RDSETP  (SUBROUTINE)  [ROOT DISEASE]

  !  Calls :
  !     RDSLP  (FUNCTION)   [ROOT DISEASE]

  !  Arguments :
  !     PROPN  -

  !     NOPLOT -

  !     PRAN   -

  !  Local Variables :
  !     <incomplete>

  !  Common Block Variables Used :
  !     <incomplete>

  !  Revision History :
  !  10/16/98 (RNH)
  !    MOdificaion to more accurately calculate the number of trees
  !    in each species to be inititially infected with root disease
  !    with RRINIT keyword
  !  02-AUG-01 Lance R. David (FHTET)
  !    Changed initialization control loop from MINRR,MAXRR to 1,ITOTRR.
  !  08/29/14 Lance R. David (FMSC)
  !    Added implicit none and declared variables.

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

  !.... Argument variable declarations.

  !.... Local variable declarations.

  ! Variables
  logical :: YESSP(MAXSP),FULL(MAXSP)
  integer :: I,IDI,II,INUMSP(ITOTRR),KSP,NOPLOT,NUMFUL(ITOTRR)
  real    :: ADDON,AVGYTK,HABSP,OVER,PRAN(ITOTRR),PROPN(MAXTRE),RDSLP,RELVAL(MAXSP),SSTVAL(ITOTRR),&
       &TMPPRP(MAXSP),TOTINF(ITOTRR),TOTNUM,TOTREE(ITOTRR),TOTSP(MAXSP),TOTVAL(ITOTRR),TOTYTK(MAXSP),&
       &TREENO(MAXSP)

  !     the array SSTVAL(MAXSP) is used in the calculation of initial
  !     root disease in each species (RNH NOV98)

  !.... Initializations.

  AVGYTK = 0.0
  OVER = 0.0

  do IDI = 1,ITOTRR
     TOTVAL(IDI) = 0.0
     TOTINF(IDI) = 0.0
     TOTREE(IDI) = 0.0
     INUMSP(IDI) = 0
     SSTVAL(IDI) = 0.0
     NUMFUL(IDI) = 0
     99 continue
  end do

  do KSP = 1, MAXSP
     YESSP(KSP) = .false.
     FULL(KSP) = .false.
     TOTSP(KSP) = 0.0
     TOTYTK(KSP) = 0.0
     TREENO(KSP) = 0.0
     RELVAL(KSP) = 0.0
     TMPPRP(KSP) = 0.0
     100 continue
  end do

  !.... Total the number of trees in center by species (TOTSP).
  !.... Figure out the "TOTAL YEARS TO KILL" trees so we can get an
  !.... average for the species in the next set of loops.

  IDI = MAXRR

  do I = 1, ITRN
     if (((IDPLOT(I) .eq. NOPLOT) .or. (NOPLOT .eq.0)) .and.&
          &(ISP(I) .ne.0)) then
        KSP = ISP(I)
        if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))

        !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
        !     If non-host species then IDI = 0, and loop should be skipped to
        !     prevent array out of bounds error

        if (IDI .le.0) go to 1500

        TOTNUM = PROB(I) * PAREA(IDI)
        TOTSP(KSP) = TOTSP(KSP) + TOTNUM

        HABSP = HABFAC(IRTSPC(KSP),IDI,IRHAB)
        YTKILL = RDSLP(DBH(I),XXINF,YYINF,NNINF)

        if (YTKILL .gt. XMINKL(IDI)) then
           YTKILL = (YTKILL - XMINKL(IDI)) * HABSP *&
                &RRPSWT(IRTSPC(KSP)) + XMINKL(IDI)
        end if

        TOTYTK(KSP) = TOTYTK(KSP) + YTKILL * TOTNUM

        if (.not. YESSP(KSP) .and. TOTSP(KSP) .gt.0.0)&
             &YESSP(KSP) = .true.
     end if
     1500 continue
  end do

  !.... Calculate the average year to kill of a particular species
  !.... (AVGYTK).  Use the AVGTYK and the probability of infection for
  !.... that species to find a relative value for the species (for
  !.... calculating relative proportions of species killed)

  !.... **Note that we are currently only using the defined probability
  !....   and not correcting it for the number of years in a cycle.
  !....   That would give slightly different results, but the cycle length
  !....   should not affect the initialization (I Think).
  !....   (Sarah, march 20/95)

  IDI = MAXRR

  do KSP = 1, MAXSP
     if (TOTSP(KSP) .gt.0.0) then

        if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))

        !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
        !     If non-host species then IDI = 0, and loop should be skipped to
        !     prevent array out of bounds error

        if (IDI .le.0) go to 1600

        AVGYTK = TOTYTK(KSP) / TOTSP(KSP)
        RELVAL(KSP) = PNINF(IRTSPC(KSP),IDI) * AVGYTK

        TOTVAL(IDI) = TOTVAL(IDI) + RELVAL(KSP)

        TOTREE(IDI) = TOTREE(IDI) + TOTSP(KSP)

        if (YESSP(KSP)) INUMSP(IDI) = INUMSP(IDI) + 1

     end if
     1600 continue
  end do

  !.... Calculate number to be infected in each species.
  !.... TOTINF is the total number of trees to be infected in each
  !.... disease type.  TREENO is the total number if trees to be infected
  !.... by species, based on the relative likelihoods (calculated from
  !.... YTK and PROB infection)
  !.... PRAN is the proportion of infection in stand or plot.

  do IDI = MINRR,MAXRR
     TOTINF(IDI) = PRAN(IDI) * TOTREE(IDI)
     !++++++
     do KSP = 1, MAXSP

        !     The following calculation was nested in this loop to for the
        !     new initial ization method used in loop 1700
        !     New array (RNH NOV98)

        if (TOTVAL(IDI) .gt.0.0)&
             &SSTVAL(IDI) = SSTVAL(IDI) + RELVAL(KSP)*TOTSP(KSP)/&
             &TOTVAL(IDI)

        1698 continue
     end do
     !++++++

     1699 continue
  end do

  IDI = MAXRR

  do KSP = 1, MAXSP

     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))

     !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
     !     If non-host species then IDI = 0, and loop should be skipped to
     !     prevent array out of bounds error

     if (IDI .le.0) go to 1700

     !     Commented out the following 2 lines and added a new formula to
     !     calculate the total number of trees infected per species
     !     (RNH Nov 98)

     !            IF (TOTSP(KSP) .GT. 0.0 .AND. TOTVAL(IDI) .GT. 0.0)
     !     &      TREENO(KSP) = TOTINF(IDI) * (RELVAL(KSP) / TOTVAL(IDI))

     if (TOTSP(KSP) .gt.0.0.and. TOTVAL(IDI) .gt.0.0.and.&
          &(SSTVAL(IDI) .gt.0.0))&

          &TREENO(KSP) = TOTINF(IDI)*TOTSP(KSP)*RELVAL(KSP)/SSTVAL(IDI)/&
          &TOTVAL(IDI)

     1700 continue
  end do

  !.... Turn number of trees to be infected of each species into a
  !.... proportion of the available trees within the center. This
  !.... proportion may be greater than one if there are few trees but
  !.... the probability of being infected is high.  If greater than one,
  !.... then set to one and increase the others (If not also one) by
  !.... dividing the remainder equally.

  1750 continue

  do IRRSP = MINRR,MAXRR
     if (INUMSP(IRRSP) .le. NUMFUL(IRRSP)) goto 1850
     ADDON = OVER / (INUMSP(IRRSP) - NUMFUL(IRRSP))
     OVER = 0.0

     IDI = IRRSP
     do KSP = 1, MAXSP
        if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(KSP))
        if (IDI .ne. IRRSP) goto 1800

        if (YESSP(KSP) .and..not. FULL(KSP)) then
           TREENO(KSP) = TREENO(KSP) + ADDON
           TMPPRP(KSP) = TREENO(KSP) / TOTSP(KSP)

           if (TMPPRP(KSP) .gt.1.0) then
              TMPPRP(KSP) = 1.0
              FULL(KSP) = .true.
              NUMFUL(IRRSP) = NUMFUL(IRRSP) + 1

              OVER = OVER + TREENO(KSP) - TOTSP(KSP)
           end if
        end if
        1800 continue
     end do

     if (NUMFUL(IRRSP) .eq. INUMSP(IRRSP)) goto 1850
     if (OVER .gt.0.0.and. NUMFUL(IRRSP) .lt. INUMSP(IRRSP))&
          &goto 1750

     1850 continue
     OVER = 0.0
     1900 continue
  end do

  do II = 1,ITRN
     if ((IDPLOT(II) .eq. NOPLOT) .or. (NOPLOT .eq.0)) then
        KSP = ISP(II)
        PROPN(II) = TMPPRP(KSP)
     end if
     2000 continue
  end do

  return
end
