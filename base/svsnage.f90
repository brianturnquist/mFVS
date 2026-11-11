!==================================!
!   Stand visualization: SVSNAGE   !
!==================================!
! BASE $Id$
subroutine SVSNAGE(IYEAR,IDEAD,SNCRDI,SNCRTO,SNHT,SNDI)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     J.J.MARCHINEK -- RMRS MOSCOW -- MAY 1999
  !     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
  !     L.R. DAVID    -- FHTET FORT COLLINS-- JULY 2005
  !     S.N.SCHAROSCH -- ABACUS -- MARCH 2008

  !     USED FOR PROCESSING SNAG AGING INFORMATION

  !     Called from:
  !        SVSNAD to age newly-added snags.
  !        SVOUT  to age snags prior to writing visualization data.

  !     IYEAR = CURRENT YEAR
  !     IDEAD = CURRENT SNAG BEING AGED
  !     SNCRDI= CURRENT SNAG CROWN DIAMETER
  !     SNCRTO= CURRENT SNAG CROWN RATIO
  !     SNHT  = CURRENT SNAG HEIGHT
  !     SNDI  = CURRENT SNAG DIAMETER

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'PLOT.f90'
  include 'SVDEAD.f90'
  include 'SVDATA.f90'

  ! Variables
  integer :: IDEAD,ISTMLT,ITIDIF,ITSTYR,ICURYR,IYEAR,J,KSP,NFALLD
  real    :: DFALLN,DKTIME,EXPFALL,FALLNUM,FALRATE,HTSNEW,PFALL,RSMAL,RSOFT,SNCRDI,SNCRTO,SNDI,SNHT,X, &
       & X1,XHMOD,XPROB
  double precision :: SAVESO
  logical :: DEBUG,LDOWN
  data ITSTYR / -999 /
  save ITSTYR

  !  Check for debug, initialize variables:

  call DBCHK (DEBUG,'SVSNAGE',7,ICYC)

  if (NDEAD.eq.0) return

  call SVHABT(XHMOD)

  !  If this is an open slot, zero out snag attributes and return.

  if (ISTATUS(IDEAD) .eq. 0) then
     SNCRDI = 0.
     SNCRTO = 0.
     SNHT = 0.
     SNDI = 0.
     return
  end if

  !  This is an active snag record--calculate elapsed yrs since tree death.

  if (IYEAR .gt. IYRCOD(IDEAD)) then
     ITIDIF = IYEAR - IYRCOD(IDEAD)
  else
     ITIDIF = 0
  end if

  !  Update snag crown diameter, and snag crown ratio

  SNHT = SNGLEN(IDEAD)
  SNDI = SNGDIA(IDEAD)
  SNCRDI = CRNDIA(IDEAD)*(0.90**(ITIDIF))
  if (ICYC .eq. 0) then
     SNCRTO = CRNRTO(IDEAD)
  else
     if (SNHT .gt. 0.5) then
        SNCRTO = (OLEN(IDEAD)/SNHT)*(CRNRTO(IDEAD)*.01-1)+1
        SNCRTO = SNCRTO * 100.
     else
        SNCRTO = 0.
     end if
  end if
  if (SNCRTO .le. 0. .or. SNCRDI .le. 0.) then
     SNCRDI = 0.
     SNCRTO = 0.
  end if

  !  If snag predictions are already up-to-date, then
  !    Return
  !  Else
  !    Initialize snag variables and proceed.

  if (IYEAR .le. ILYEAR .or. ITIDIF .le. 0) then
     return
  end if

  NFALLD = 0
  KSP = ISNSP(IDEAD)
  if (FALLDIR(IDEAD).eq.-1) then
     LDOWN = .false.
  else
     LDOWN = .true.
  end if

  !  Status codes will be negative for snags that have been flagged for
  !  salvage, but are being temporarily held for display in the post
  !  thinning/salvage SVS outputs. For such snags, temporarily reverse
  !  the sign on the status code--it will be set back at the end of this
  !  routine.

  if ( ISTATUS(IDEAD) .lt. 0 ) then
     ISTMLT = -1
  else
     ISTMLT = 1
  end if
  ISTATUS(IDEAD) = ISTATUS(IDEAD) * ISTMLT

  if ( .not. LDOWN .or. SPROBS(IDEAD,3) .gt. 0.0 ) then

     !  Predict annual snag fall rate, and apply--stochastically.
     !  This loop is executed if:
     !     1) the snag is still standing, or,
     !     2) the snag has previously fallen, but we need to continue
     !        decrementing the SPROBS(IDEAD,3) value for the expected
     !        snag expansion factor.
     !  Loop by years since last snag update to predict snagfall.
     !  If current year is prior to snag death year, skip falldown predictions.

     do ICURYR = ILYEAR+1,IYEAR
        if ( IYRCOD(IDEAD).ge.ICURYR ) cycle
        if ( FALLDIR(IDEAD).ne.-1 .and. SPROBS(IDEAD,3).eq.0.0) exit

        !  Call FMSFALL to:
        !    1) Compute special post-burn fall rates if this is the first year
        !       after a fire.
        !    2) Calculate the density of snags in this record that would fall
        !       under normal conditions. This depends on species, dbh and
        !       whether 5% are left.

        !  If the snag has already fallen, and we are calling FMSFALL just to
        !  update the SPROBS(IDEAD,3) value (the expected snag expansion factor
        !  if deterministic snagfall were used), then pass in 1 as the number
        !  of snags still standing.

        if ( FALLDIR(IDEAD).ne.-1 .and. SPROBS(IDEAD,2).eq.0.0 ) then
           call FMSFALL(ICURYR,KSP,SNGDIA(IDEAD),SPROBS(IDEAD,1), &
                & 1.0,2,RSOFT,RSMAL,DFALLN)
        else
           call FMSFALL(ICURYR,KSP,SNGDIA(IDEAD),SPROBS(IDEAD,1), &
                & SPROBS(IDEAD,2),2,RSOFT,RSMAL,DFALLN)
        end if

        if (DEBUG) then
           write(JOSTND,1020) IDEAD, RSOFT, RSMAL, &
                & DFALLN, SPROBS(IDEAD,1)
           1020 format(' ','IN SVSNAGE, IDEAD=',I4,', RSOFT=',F6.3, &
                & ', RSMAL=',F6.3,', DFALLN=',F6.3, &
                & ', ORIGDEN=',F6.3)
        end if

        !  If applicable, store the post-burn fall rate (PBFALL) that will be
        !  in effect for the next PBYEAR years.
        !  Different rates apply to small snags and snags that are soft
        !  AT TIME OF FIRE (whether initially hard or soft): if both apply,
        !  use whichever rate is greater. Fires do not affect the fall rate of
        !  large snags that are hard at the time of the fire.

        if ((ICURYR - BURNYR) .eq. 1) then
           if ( ISTATUS(IDEAD) .eq. 4 ) then
              PBFALL(IDEAD) = RSOFT
           else
              PBFALL(IDEAD) = 0.0
           end if

           if (SNGDIA(IDEAD) .lt. PBSIZE) then
              PBFALL(IDEAD) = RSMAL
              if ( ISTATUS(IDEAD) .eq. 4 ) then
                 PBFALL(IDEAD) = MAX(RSOFT,RSMAL)
              end if
           end if
        end if

        !  Set FALLNUM according to whichever density is largest - the density
        !  falling under normal conditions, or the density to fall under
        !  post-burn conditions if these are applicable.

        FALLNUM = DFALLN
        if (((ICURYR-BURNYR) .le. PBTIME) .and. &
             & (IYRCOD(IDEAD) .le. BURNYR)) then
           if (FALLNUM .lt. PBFALL(IDEAD)*SPROBS(IDEAD,2)) then
              FALLNUM = PBFALL(IDEAD)*SPROBS(IDEAD,2)
           end if
        end if

        !  FALLNUM is, at this point, the number of snags that need to
        !  fall in the current year, across all snag records that still
        !  remain standing from the original source tree.
        !  For predicting fall of each individual snag, scale FALLNUM
        !  down to a single stem probability.

        if ( SPROBS(IDEAD,2).gt.0.0 ) then
           FALRATE = FALLNUM/SPROBS(IDEAD,2)
        else
           FALRATE = 0.0
        end if

        !  Compute expected tree/acre representation of current snag, if
        !  fallrate was applied in a deterministic fashion instead of stochastic.
        !  SPROBS(IDEAD,3) is not adjusted for remaining snag cohort count
        !  since it's meant to tally expected remaing snag tpa--independent
        !  of stochastic falling.

        !  Note: the fall probabilities computed by year within each cycle are not
        !  entirely correct. That's because we loop by year, by snag, and the number
        !  of snags fallen so far influences the probability of additional snags
        !  falling. So snag 1 has already been updated to the end of the cycle
        !  before snag 2 has its fall probability predicted for the first year
        !  of the current cycle. The end result is that snags at the start of the
        !  list have a slightly higher fall probability than those at the end.
        !  The impact is probably negligible; looping sequence can't be easily
        !  changed since SVSNAGE is called when looping by SVS object in SVSOUT.

        if ( SPROBS(IDEAD,3) .gt. 0.0 ) then
           EXPFALL = FALLNUM / SPROBS(IDEAD,1)
           if ( SPROBS(IDEAD,3) .gt. EXPFALL ) then
              SPROBS(IDEAD,3) = SPROBS(IDEAD,3) - EXPFALL
           else
              SPROBS(IDEAD,3) = 0.0
           end if
        end if
        PFALL = FALRATE

        !  Now actually fall the snag.
        !  Snag records each represent one tree/ac; but FALRATE is fractional.
        !  Therefore, we need to stochastically fell snag records using a
        !  random number generation/test.

        if ( FALRATE .gt. 0.0 .and. FALLDIR(IDEAD) .eq. -1 ) then
           call SVRANN(X)
           if (X .lt. PFALL) then
              X1 = X
              call SVRANN(X)
              FALLDIR(IDEAD) = IFIX(360. *X +.5)
              if (DEBUG) write(JOSTND,1070) ICURYR, X1, IDEAD
              1070 format(' ','IN SVSNAGE (1070), ICURYR:',I4, &
                   & ', X1=',F5.3,', FELLED SNAG:',I4)
           end if

           !  If snag fell, update "remaining snag" count.

           if (FALLDIR(IDEAD) .ge. 0) then
              NFALLD = 1
              XPROB = 0.0
              do J = 1,NDEAD
                 if ( OIDTRE(J)  .eq. OIDTRE(IDEAD) .and. &
                      & IYRCOD(J)  .eq. IYRCOD(IDEAD) .and. &
                      & FALLDIR(J) .eq. -1 ) XPROB = XPROB+1
              end do
              do J = 1,NDEAD
                 if ( OIDTRE(J) .eq. OIDTRE(IDEAD) .and. &
                      & IYRCOD(J) .eq. IYRCOD(IDEAD) ) then
                    SPROBS(J,2) = XPROB
                 end if
              end do
           end if
        end if
     end do
  end if

  !  For standing snags, loop by year to predict annual height loss.
  !  Update hard/soft status in process, since height loss predictions
  !  depend on hard/soft status.
  !  Rannget and Rannput are used to not disrupt the random number
  !  sequence, since rann can be called in the snag height loss
  !  routine (fmR6htls).

  if (FALLDIR(IDEAD) .eq. -1) then
     call RANNGET(SAVESO)
     do ICURYR = MAX(ILYEAR+1,IYRCOD(IDEAD)+1),IYEAR
        if ( ISTATUS(IDEAD) .eq. 4 ) then
           call FMSNGHT(VARACD,KSP,OLEN(IDEAD),SNGLEN(IDEAD), &
                & 0,HTSNEW)
        else
           call FMSNGHT(VARACD,KSP,OLEN(IDEAD),SNGLEN(IDEAD), &
                & 1,HTSNEW)
        end if
        SNHT = HTSNEW
        SNGLEN(IDEAD) = HTSNEW

        !  Adjust snag status for normal aging (may be subsequently overridden).

        select case (ISTATUS(IDEAD))
        case(1,2) !Green/red hard snags
           if (ITIDIF .lt. 2) then

              !               Tree is moving from green to red snag, or staying red.
              ISTATUS(IDEAD) = 2
           else

              !               Tree is moving from green/red snag to hard grey snag.
              ISTATUS(IDEAD) = 3
           end if
        case (90,91,92)
           if (ITIDIF .lt. 2) then

              !               WWPB kill, one year or less old--remains red snag.
              continue
           else if (ITIDIF .lt. 3) then

              !               Advance to dark beetle kill.
              ISTATUS(IDEAD) = 91

           else if (ITIDIF .lt. 5) then

              !               Advance to grey WWPB kill.
              ISTATUS(IDEAD) = 92

           else if (ITIDIF .lt. 6) then

              !               Advance to grey hard snag.
              ISTATUS(IDEAD) = 3
           else

              !               Advance to grey soft snag.
              ISTATUS(IDEAD) = 4
           end if
        case (5)
           if (ITIDIF .ge. 2) then

              !               This snag is progressing from a recently burned snag
              !               to an older burned snag with no crown.
              ISTATUS(IDEAD) = 6
           end if
        end select

        !  Call FMSNGDK to predict years, since death, for snag to become soft.
        !  If time since tree death exceeds hard-to-soft decay time, change
        !  status to soft snag.
        !  (Includes beetle-caused and fire-caused snags)

        if ( ISTATUS(IDEAD) .ne. 4 ) then
           call FMSNGDK(VARACD,KSP,SNGDIA(IDEAD),DKTIME)
           if ((ICURYR - IYRCOD(IDEAD)) .ge. DKTIME) then
              ISTATUS(IDEAD) = 4
           end if
        end if
     end do
     call RANNPUT(SAVESO)

     !  No diameter change for standing snags.

     SNGDIA(IDEAD) = ODIA(IDEAD)
     SNDI = ODIA(IDEAD)
  end if

  if (DEBUG) then
     if ( ITSTYR .ne. IYEAR ) then
        write (JOSTND,1040) ICYC, IYEAR, &
             & NDEAD, NSVOBJ
        1040 format (' ','IN SVSNAGE, ICYC=',I2,', IYEAR=',I4,':', / , &
             & ' ',T5,'NDEAD=',I4,', NSVOBJ=',I5,//, &
             & ' ',T5,'SNAG LIST AND FALL PROBABILITIES:',//, &
             & ' ',T5,'IDEAD SPP ODIA OLEN IYRCOD ST ODEN CDEN -EXP- ', &
             & 'PBFALL FRATE FALLDIR',/, &
             & ' ',T5,'----- --- ---- ---- ------ -- ---- ---- ----- ', &
             & '----- ------ -------')
        !                       XXXXX XXX XX.X XXX. XXXXXX XX XXX. XXX. X.XXX
        !                       X.XXX X.XXXX XXXXX.X
        ITSTYR = IYEAR
     end if
     write(JOSTND,1050) IDEAD, ISNSP(IDEAD), ODIA(IDEAD), &
          & OLEN(IDEAD), IYRCOD(IDEAD), &
          & ISTATUS(IDEAD), SPROBS(IDEAD,1), &
          & SPROBS(IDEAD,2), SPROBS(IDEAD,3), &
          & PBFALL(IDEAD), FALRATE, &
          & FALLDIR(IDEAD)
     1050 format(' ',T5,I5,1X,I3,1X,F4.1, &
          & 1X,F4.0,1X,I6, &
          & 1X,I2,1X,F4.0, &
          & 1X,F4.0,1X,F5.3, &
          & 1X,F5.3,1X,F6.4, &
          & 1X,F7.1)
  end if

  !  Snag is down.
  !  If age of snag is greater than zero, progress status, diameter and height;
  !  otherwise, just adjust diameter and height.

  if (FALLDIR(IDEAD) .ge. 0) then

     !       Force all downed snags to be grey, unless they are salvage snags.

     if ( ISTMLT .eq. 1 ) then
        if ( ISTATUS(IDEAD) .le.  2 ) ISTATUS(IDEAD) = 3
        if ( ISTATUS(IDEAD) .eq. 90 ) ISTATUS(IDEAD) = 91
     end if

     if (ISTATUS(IDEAD) .eq. 1 .or. ISTATUS(IDEAD) .eq. 2) then
        if (ITIDIF .lt. 2) then
           ISTATUS(IDEAD) = 2
        else
           ISTATUS(IDEAD) = 3
        end if
     end if

     !       FOR FIRE-CAUSED SNAGS ONLY.  PROGRESS FROM ISTATUS OF 5
     !       TO ISTATUS OF 6.

     if (ISTATUS(IDEAD).eq.5 .and. ITIDIF.ge.2) then

        !         THIS SNAG IS PRORESSING FROM A RECENTLY BURNED SNAG
        !         TO AN OLDER BURNED SNAG WITH NO CROWN.

        ISTATUS(IDEAD) = 6
     else if (ISTATUS(IDEAD) .ge. 90 .and. &
          & ISTATUS(IDEAD) .le. 92) then
        if (ITIDIF .lt. 2) then
           !           THIS IS A NEW WWPB KILL ONE OR LESS YEARS OLD.
           !           NO CHANGE IN STATUS, REMAINS RED.
           continue
        else if (ITIDIF .lt. 3) then
           !           ADVANCE TO DARK WWPB KILL.
           ISTATUS(IDEAD) = 91
        else if (ITIDIF .lt. 5) then
           !           ADVANCE TO GREY WWPB KILL.
           ISTATUS(IDEAD) = 92
        else if (ITIDIF .lt. 6) then
           !           ADVANCE TO GREY HARD SNAG.
           ISTATUS(IDEAD) = 3
        else
           !           ADVANCE TO GREY SOFT SNAG.
           ISTATUS(IDEAD) = 4
        end if
     end if

     !  For downed snags:
     !    1) Predict diameter decrease using fixed rate.
     !       Don't allow negative diameter--snags with diam lt 1 get
     !       removed from snag arrays in SVOUT
     !    2) Predict current length, using ratio of original dia to current.
     !       Don't allow length to increase from what it was before snag fell.

     if (ISTATUS(IDEAD) .eq. 4) then
        SNDI = ODIA(IDEAD) - .11*ITIDIF
        if ( SNDI .lt. 0.0 ) SNDI = 0.0
        SNGDIA(IDEAD) = SNDI
        SNHT = OLEN(IDEAD) * SNDI/ODIA(IDEAD)
        if ( SNHT .gt. SNGLEN(IDEAD) ) SNHT = SNGLEN(IDEAD)
        SNGLEN(IDEAD) = SNHT
     else
        SNDI = ODIA(IDEAD) - .1*ITIDIF
        if ( SNDI .lt. 0.0 ) SNDI = 0.0
        SNGDIA(IDEAD) = SNDI
        SNHT = OLEN(IDEAD) * SNDI/ODIA(IDEAD)
        if ( SNHT .gt. SNGLEN(IDEAD) ) SNHT = SNGLEN(IDEAD)
        SNGLEN(IDEAD) = SNHT
     end if
  end if

  !  Return the snag status code to a negative value for salvaged snags.

  ISTATUS(IDEAD) = ISTATUS(IDEAD) * ISTMLT

  return
end subroutine SVSNAGE
