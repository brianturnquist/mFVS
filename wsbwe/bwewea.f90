!====================!
!   wsbwe : bwewea   !
!====================!
! WSBWE $Id$
subroutine BWEWEA
  implicit none

  ! GET WEATHER PARAMETERS FOR THE CURRENT YEAR

  !  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR

  !   CALLED FROM: BWELIT

  !   SUBROUTINES AND FUNCTIONS CALLED:

  !      BWENOR - CALC. AND SCALE A NORMALLY DISTRIB. DEVIATE
  !      BWEMUL - SCALE A RANDOM NUMBER TO GET A MULTIPLIER

  !   PARAMETERS:

  !   BWEATH - ARRAY THAT STORES WEATHER PARAMETERS FOR 1 STATION
  !            (X,1)=MEAN,(X,2)=S.D., (X,3)=MINIMUM, (X,4)=MAX. FOR X=
  !            1=NO. OF DAYS FROM L2 EMERGENCE TO MID-L4
  !            2=NO. OF DAYS FROM MID-L4 THROUGH PUPAE
  !            3=NO. OF DAYS AS PUPAE
  !            4=WARM DEGREE-DAYS AFTER ADULT FLIGHT IN FALL
  !            5=NO. OF DAYS FROM L2 EMERGENCE TO BUD-FLUSHING
  !            6=TREE DEGREE-DAYS ACCUMULATED AT MID-L4
  !            7=MEAN PPT. FOR SMALL LARVAE
  !            8=MEAN PPT. FOR LARGE LARVAE
  !            9=MEAN PPT. FOR PUPAE
  !            10=MEAN PPT. DURING L2 EMERGENCE
  !   AMIN   - MINIMUM VALUE FOR WEATHER PARAMETER MULTIPLIERS (NOW = 0.8)
  !   AMULT  - CURRENT VALUE FOR MULTIPLIERS
  !   IEVENT(250,5) - BW SPECIAL EVENTS SUMMARY ARRAY
  !   IWYR   - WEATHER YEAR COUNTER
  !   LP4    - TABLE 4 OUTPUT FLAGS (TRUE=PRINT, FALSE=NO PRINT) [BWEBOX]
  !   NEVENT - NUMBER OF BW SPECIAL EVENTS TO DATE
  !   TREEDD - DEGREE-DAYS THAT HAVE ACCUMULATED FOR DF AT MID-L4
  !   WCOLDW - EFFECT OF EXTREME WINTER WEATHER
  !   WRAINA - EFFECT OF HEAVY PPT. ON ANT PREDATION (BY LIFE STAGE)
  !   WRAINB - EFFECT OF HEAVY PPT. ON BIRD PREDATION (BY LIFE STAGE)
  !   WRAIN1 - EFFECT OF HEAVY PPT. ON OTHER1 PREDATION (BY LIFE STAGE)
  !   WRAIN2 - EFFECT OF HEAVY PPT. ON OTHER2 PREDATION (BY LIFE STAGE)
  !   WRAIN3 - EFFECT OF HEAVY PPT. ON OTHER3 PREDATION (BY LIFE STAGE)
  !   WRAIND - EFFECT OF HEAVY PPT. ON BW SURVIVAL DURING L2 EMERGENCE

  !  SET WEATHER-RELATED PARAMETERS

  ! Revision History:
  !   17-MAY-2005 Lance R. David (FHTET)
  !      Added FVS parameter file PRGPRM.F77.
  !   30-AUG-2006 Lance R. David (FHTET)
  !      Changed array orientation of IEVENT from (4,250) to (250,4).
  !   14-JUL-2010 Lance R. David (FMSC)
  !      Added IMPLICIT NONE and declared variables as needed.
  !   28-AUG-2013 Lance R. David (FMSC)
  !      Added weather year (if using RAWS) to special events table.
  !   19-SEP-2014 Lance R. David (FMSC)
  !      Changed variable named INDEX to INDX.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'BWECM2.f90'
  include 'BWEBOX.f90'
  include 'BWECOM.f90'

  ! Variables
  integer :: I,INDX,IOS,N
  real    :: AMIN,AMULT,BWENOR,PICK,RAIN(3)
  logical :: DBUG

  !.... Check for DEBUG

  call DBCHK(DBUG,'BWEWEA',6,ICYC)

  if (DBUG) write (JOSTND,*) 'ENTER BWEWEA: ICYC = ',ICYC

  !  SET THE ARRAY THAT STORES EVENT SUMMARIES (FOR TABLE 6) TO ZERO

  do I = 1,3
     IOUT6A(I) = '   '
     10 continue
  end do

  if (DBUG) write (JOSTND,*) 'IN BWEWEA: BWEATH=',BWEATH

  !  IF IWOPT=1, GENERATE WEATHER VALUES FROM THE MEANS AND STANDARD
  !    DEVIATIONS FROM THE WEATHER STATION SELECTED BY THE USER

  !  GET THE WEATHER RANDOM NO. SEED (WSEED).  CHECK TO SEE IF >1 YR
  !    HAS PASSED SINCE THIS SUBR. WAS CALLED -- IF SO, MAKE DUMMY
  !    CALLS TO THE RN GENERATOR SO THAT THE SAME SEED WILL BE USED IN
  !    A GIVEN YEAR (TO COMPARE ACROSS YEARS). EACH YEAR WHEN BUDLITE
  !    IS CALLED, THERE ARE 20 CALLS TO THE WEATHER SEED (10 CALLS TO
  !    BWERAN, 2 USES OF THE SEED PER CALL).
  ! IF IWSRC=2, USER HAS SUPPLIED ACTUAL DATA (1 LINE PER YEAR). READ
  ! CURRENT YEAR & STORE IN BWEATH(X,1); IWOPT IS SET TO 2 TEMPORARILY.

  !     RESTORE SEED VALUE FOR WEATHER RANDOM NUMBER SERIES USED IN FUNCTION BWENOR
  call BWERPT(WSEED)

  if (DBUG) write (JOSTND,*)&
       &'IN BWEWEA: IWOPT, IWSRC, WSEED=',IWOPT,IWSRC,WSEED

  if (IWOPT .eq.1.or. IWSRC .eq.2) then

     AMIN = 0.8
     if (IYRCUR .gt. IWYR+1.and. IWYR .lt.3000) then
        20 IWYR = IWYR+1
        if (DBUG) write (JOSTND,*)&
             &'IN BWEWEA: IYRCUR, IWYR=',IYRCUR,IWYR
        if (IWSRC .ne.2) then
           TREEDD = BWENOR(BWEATH(6,1),BWEATH(6,2))
           PICK = BWENOR(BWEATH(4,1),BWEATH(4,2))
           PICK = BWENOR(BWEATH(10,1),BWEATH(10,2))
           DFLUSH = BWENOR(BWEATH(5,1),BWEATH(5,2))
           DAYS(1) = BWENOR(BWEATH(1,1),BWEATH(1,2))
           DAYS(2) = BWENOR(BWEATH(2,1),BWEATH(2,2))
           DAYS(3) = BWENOR(BWEATH(3,1),BWEATH(3,2))
           PICK = BWENOR(BWEATH(7,1),BWEATH(7,2))
           PICK = BWENOR(BWEATH(8,1),BWEATH(8,2))
           PICK = BWENOR(BWEATH(9,1),BWEATH(9,2))
        else
           !               WRITE (16,*) 'IN BWEWEA: DUMMY READ JOWE'         ! TEMP DEBUG
           read (JOWE,30,IOSTAT = IOS)
           30 format (1X)
           if (IOS .eq. -1) rewind (JOWE)
           !               WRITE (16,*) 'IN BWEWEA: REWIND JOWE'             ! TEMP DEBUG
        end if
        if (IDEFPR .ne.0) write (JOBWP3,40) IWYR,(IDEF(IBUDYR,N),&
             &N = 1,NUMCOL)
        40 format (I4,4X,5(I4,6X))
        IBUDYR = IBUDYR+1
        if (DBUG) write (JOSTND,*)'IN BWEWEA: IBUDYR=',IBUDYR
        if (IWYR .lt. IYRCUR-1) goto 20
     end if
     IWYR = IWYR+1

     ! IF IWSRC=2, READ IN DATA FROM FILE SUPPLIED BY USER, THEN SKIP REST
     !   OF THIS SECTION

     if (IWSRC .eq.2) then
        44 read (JOWE,45,IOSTAT = IOS) DAYS(1),DAYS(2),DAYS(3),WHOTF,&
             &DFLUSH,TREEDD,RAIN(1),RAIN(2),RAIN(3),WRAIND
        45 format(5X,10F7.1)
        if (IOS .eq. -1) then
           rewind (JOWE)
           goto 44
        end if
        AMIN = 0.8
        AMULT = 1.0
        if (WHOTF .gt. WHOTM)&
             &call BWEMUL(WHOTM,WHOTSD,WHOTF,AMIN,AMULT)
        WHOTF = AMULT
        AMULT = 1.0
        if (WRAIND .gt. RAINDM)&
             &call BWEMUL(RAINDM,RAINDS,WRAIND,AMIN,AMULT)
        WRAIND = AMULT
        do I = 1,3
           AMULT = 1.0
           if (RAIN(I) .gt. RAINM(I))&
                &call BWEMUL(RAINM(I),RAINS(I),RAIN(I),AMIN,AMULT)
           WRAINA(I) = AMULT
           WRAINB(I) = AMULT
           WRAIN1(I) = AMULT
           WRAIN2(I) = AMULT
           WRAIN3(I) = AMULT
           46 continue
        end do
        WCOLDW = 1.0
     end if
     if (IWSRC .eq.2) go to 300 ! RETURN

     TREEDD = BWENOR(BWEATH(6,1),BWEATH(6,2))
     PICK = BWENOR(BWEATH(4,1),BWEATH(4,2))

     !        TEMP DEBUG
     !        WRITE (16,*) 'IN BWEWEA: PICK, BWEATH(4,1)=',PICK,BWEATH(4,1)

     if (PICK .gt. BWEATH(4,1)) then
        call BWEMUL(BWEATH(4,1),BWEATH(4,2),PICK,AMIN,AMULT)
     else
        AMULT = 1.0
     end if
     WHOTF = AMULT
     if (LP4.and. AMULT-AMIN .le.0.02) then
        NEVENT = NEVENT+1
        if (NEVENT.gt.250) then
           write (JOBWP4,8250)
           LP4 = .false.
        else
           IEVENT(NEVENT,1) = IYRCUR
           IEVENT(NEVENT,2) = 7
           IEVENT(NEVENT,3) = 0
           IEVENT(NEVENT,4) = 8
           !             weather year is reported only if RAWS data is in use
           !             so IEVENT(x,5) is set to 0 here.
           IEVENT(NEVENT,5) = 0
        end if
     end if
     if (AMULT-AMIN.le.0.02) IOUT6A(3) = ' 95'

     PICK = BWENOR(BWEATH(10,1),BWEATH(10,2))
     if (PICK .gt. BWEATH(10,1)) then
        call BWEMUL(BWEATH(10,1),BWEATH(10,2),PICK,&
             &AMIN,AMULT)
     else
        AMULT = 1.0
     end if
     WRAIND = AMULT
     if (LP4.and. AMULT-AMIN .le.0.02) then
        NEVENT = NEVENT+1
        if (NEVENT.gt.250) then
           write (JOBWP4,8250)
           LP4 = .false.
        else
           IEVENT(NEVENT,1) = IYRCUR
           IEVENT(NEVENT,2) = 7
           IEVENT(NEVENT,3) = 0
           IEVENT(NEVENT,4) = 4
           !             weather year is reported only if RAWS data is in use
           !             so IEVENT(x,5) is set to 0 here.
           IEVENT(NEVENT,5) = 0
        end if
     end if
     if (AMULT-AMIN .le.0.02) IOUT6A(3) = ' 95'

     DFLUSH = BWENOR(BWEATH(5,1),BWEATH(5,2))
     DAYS(1) = BWENOR(BWEATH(1,1),BWEATH(1,2))
     DAYS(2) = BWENOR(BWEATH(2,1),BWEATH(2,2))
     DAYS(3) = BWENOR(BWEATH(3,1),BWEATH(3,2))
     do I = 1,3
        INDX = I+6
        PICK = BWENOR(BWEATH(INDX,1),BWEATH(INDX,2))
        if (PICK .gt. BWEATH(INDX,1)) then
           call BWEMUL(BWEATH(INDX,1),BWEATH(INDX,2),PICK,&
                &AMIN,AMULT)
        else
           AMULT = 1.0
        end if
        WRAINA(I) = AMULT
        WRAINB(I) = AMULT
        WRAIN1(I) = AMULT
        WRAIN2(I) = AMULT
        WRAIN3(I) = AMULT
        if (LP4.and. AMULT-AMIN .le.0.02) then
           NEVENT = NEVENT+1
           if (NEVENT.gt.250) then
              write (JOBWP4,8250)
              8250 format ('********   ERROR - WSBW: MORE THAN 250 ENTRIES!')
              LP4 = .false.
           else
              IEVENT(NEVENT,1) = IYRCUR
              IEVENT(NEVENT,2) = 7
              IEVENT(NEVENT,3) = 0
              IEVENT(NEVENT,4) = I+4
              !             weather year is reported only if RAWS data is in use
              !             so IEVENT(x,5) is set to 0 here.
              IEVENT(NEVENT,5) = 0
           end if
        end if
        50 continue
     end do

     ! STILL NEED TO FIGURE OUT HOW TO CALC. EXTREME WINTER WEATHER MULTIPLIER
     !   SET TO 1.0 (NO EFFECT) FOR NOW.

     WCOLDW = 1.0

     ! IF WOPT NE 1, SET WEATHER PARAMETERS TO AVERAGE VALUES (NO STOCH.)

  else
     TREEDD = BWEATH(6,1)
     DFLUSH = BWEATH(5,1)
     DAYS(1) = BWEATH(1,1)
     DAYS(2) = BWEATH(2,1)
     DAYS(3) = BWEATH(3,1)
     AMULT = 1.0
     do I = 1,3
        WRAINA(I) = AMULT
        WRAINB(I) = AMULT
        WRAIN1(I) = AMULT
        WRAIN2(I) = AMULT
        WRAIN3(I) = AMULT
        250 continue
     end do
     WRAIND = AMULT
     WCOLDW = AMULT
     WHOTF = AMULT
  end if

  300 continue

  !     RETRIEVE SEED VALUE FOR WEATHER RANDOM NUMBER SERIES.
  call BWERGT(WSEED)

  if (DBUG) write (JOSTND,*) 'EXIT BWEWEA: NEVENT=',NEVENT

  return
end

function BWENOR(AVE,SD)

  ! **BWENOR      LAST REVISED: 2/25/97

  !  THIS FUNCTION FIRST CALCULATES A NORMALLY DISTRIBUTED DEVIATE WITH
  !  ZERO MEAN AND UNIT VARIANCE, USING BWERAN AS THE SOURCE FOR A UNIFORM
  !  DEVIATE.  SOURCE: PRESS, WILLIAM H.; TEUKOLSKY, SAUL A.; VETTERLING,
  !  WILLIAM T.; FLANNERY, BRIAN P.  1992.  NUMERICAL RECIPES IN FORTRAN,
  !  2ND EDITION.  CAMBRIDGE UNIVERSITY PRESS, PORT CHESTER, NEW YORK. 963 P.

  !  NEXT, THE DEVIATE IS SCALED TO THE AVERAGE (AVE) AND STANDARD DEVIATION
  !  (SD), WITH THE RESULTING VALUE STORED IN BWENOR.

  !  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR

  !   CALLED FROM: BWEWEA

  !   SUBROUTINES AND FUNCTIONS CALLED:

  !      BWERAN

  !   PARAMETERS:

  data ISET/0/

  !  FIRST, GET THE NORMALLY DISTRIBUTED DEVIATE

  !     WRITE (16,*) 'BWEWEA (BWENOR): ISET=',ISET                 ! TEMP DEBUG

  if (ISET.eq.0) then

     1 call BWERAN(X)
     !       WRITE (16,*) 'BWEWEA (BWENOR): RANDOM #1: ', X           ! TEMP DEBUG
     V1 = 2.*X-1.
     call BWERAN(X)
     !       WRITE (16,*) 'BWEWEA (BWENOR): RANDOM #2: ', X           ! TEMP DEBUG
     V2 = 2.*X-1.
     RSQ = V1**2+V2**2
     if (RSQ.ge.1..or.RSQ.eq.0.) goto 1
     FAC = SQRT(-2.*LOG(RSQ)/RSQ)
     GSET = V1*FAC
     GASDEV = V2*FAC
     ISET = 1
  else
     GASDEV = GSET
     ISET = 0
  end if

  ! NEXT, SCALE THE DEVIATE

  BWENOR = AVE+(GASDEV*SD)

  !     TEMP DEBUG
  !     WRITE (16,*) 'IN BWEWEA BWENOR: BWENOR, AVE, GASDEV, SD, ISET=',
  !    &                                BWENOR, AVE, GASDEV, SD, ISET
  return
end

subroutine BWEMUL(AVE,SD,PICK,AMIN,AMULT)

  ! **BWEMUL      LAST REVISED: 2/25/97

  !  THIS SUBROUTINE IS USED TO SCALE A NORMALY-DISTR. RANDOM
  !  NUMBER (PICK) FOR A MULTIPLIER THAT IS SET TO A MAXIMUM OF
  !  1.0 AND A MINIMUM OF AMIN.

  !  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR

  !   CALLED FROM: BWEWEA

  !   SUBROUTINES AND FUNCTIONS CALLED: NONE

  DIFF = ABS(AVE-PICK)
  TOP = SD*2.0
  if (DIFF.gt.TOP) DIFF = TOP
  if (TOP.gt.0.0) then
     PROP = DIFF/TOP
  else
     PROP = 0.0
  end if
  AMULT = 1.0-((1.0-AMIN)*PROP)
  if (AMULT.lt.0.0.or.AMULT.gt.1.0) then
     write (*,100) AVE,SD,PICK,AMIN,AMULT
     100 format ('********   WARNING (BWEMUL): BAD AMULT VALUE!',5F9.2)
     AMULT = 1.0
  end if

  !     WRITE (16,*) 'BWEWEA (BWEMUL): ',AVE,SD,PICK,AMIN,AMULT     ! TEMP DEBUG

  return
end
