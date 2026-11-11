!===================!
!   wsbwe : bwedr   !
!===================!
! WSBWE $Id$
subroutine BWEDR
  implicit none

  !     PROCESSES ONE STAND IN ONE YEAR OF A BUDWORM MODEL OUTBREAK.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
  !     FEB. 1984. - FORESTRY SCIENCES LAB - MOSCOW, ID.

  !     DESCRIPTION :

  !     BWEDR DRIVES THE BUDWORM MODEL IN ONE STAND FOR ONE
  !     YEAR.   NO STAND SWAPS OR YEAR LOOPS OCCUR.

  !     major modification by K.Sheehan to strip out BW Pop.Dyn.stuff
  !        July 1996+

  !     CALLED FROM :

  !       BWECUP - SINGLE STAND BUDWORM MODEL LINK TO PROGNOSIS.

  !     SUBROUTINES CALLED :

  !     BWEDEF - COMPUTE MANUAL DEFOLIATION.
  !     BWELIT - BUD-LITE MODEL.
  !     BWEAGE - AGES FOLIAGE ONE YEAR, OLD NEEDLES FALL OFF.
  !     BWEDAM - COMPUTES ANNUAL DAMAGE TO THE STAND.
  !     OPNEW

  !   PARAMETERS:

  !   LCALBW = TRUE IF BUDLITE MODEL IS CALLED
  !   LDEFOL = TRUE IF USER SUPPLIES ANNUAL DEFOLIATION RATES
  !   LREGO  - TRUE IF A REGIONAL OUTBREAK IS ACTIVE, FALSE IF NOT
  !   IYRCUR = CURRENT YEAR
  !   IYRST - YEAR THAT NEXT REGIONAL OUTBREAK STARTS  [BWECM2]
  !   IYREND - YEAR THAT NEXT REGIONAL OUTBREAK ENDS [BWECM2]
  !   IYRECV - YEAR THAT TREES WILL BE COMPLETELY RECOVERED [BWECM2]
  !                 FROM NEXT OUTBREAK

  ! Revision History:
  !   18-MAY-00 Lance David (FHTET)
  !      .Added debug handling.
  !      .Added variable IACTC for activity codes due to problems
  !       experienced with passing literals using LF95 compiler.
  !   30-AUG-2006 Lance R. David (FHTET)
  !      Changed array orientation of IEVENT from (4,250) to (250,4) for
  !      efficiency purposes of the PPE processes in bwepppt and bweppgt.
  !   06-OCT-2006 Lance R. David
  !      Corrected incompatible literal parameter in OPNEW call to PRMS.
  !   02-JUN-2009 Lance R. David (FMSC)
  !      Added Stand ID and comma delimiter to output tables, some header
  !      and column labels modified.
  !   14-JUL-2010 Lance R. David (FMSC)
  !      Added IMPLICIT NONE and declared variables as needed.
  !   28-AUG-2013 Lance R. David (FMSC)
  !      Added loading of weather year into IEVENT array.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'OPCOM.f90'
  include 'PLOT.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'
  include 'BWECM2.f90'
  include 'BWEBOX.f90'

  ! Variables
  integer :: I,IACTC,IC,ICROWN,IFAGE,IHOST,IR,ISZI,IT,KEND,KODE

  real    :: TOTP(6,3),TOTR(6,3),AVPRBO(6,3,2),PRMS(1)

  logical :: LBWEGO
  logical :: DEBUG

  !.... Check for DEBUG

  call DBCHK(DEBUG,'BWEDR',5,ICYC)

  if (DEBUG) write (JOSTND,*) 'ENTER BWEDR: NPLT,ICYC=',NPLT(1:8),&
       &ICYC
  if (DEBUG) write (JOSTND,*)&
       &' LCALBW=',LCALBW,' LDEFOL =',LDEFOL,' LREGO  =',LREGO,&
       &' IYRCUR =',IYRCUR,' IYRST =',IYRST,' IYREND =',IYREND,&
       &' IYRECV =',IYRECV

  !     ************************ EXECUTION BEGINS ************************

  !     WRITE A MSG INDICATING WHICH STAND IS BEING PROCESSED.  DON'T
  !     COUNT LBWDAM (DAMAGE) AS NEEDING A HEADING.

  !      WRITE (JOWSBW,117) IYRCUR,ISTAND,MGMID
  !  117 FORMAT (/,'PROCESSING:  YEAR=',I5,'; STAND NO=',I3,
  !     >         '; MANAGEMENT ID= ',A4)

  !     ZERO OUT THE TOTAL DEFOLIATION ARRAYS.

  do IR = 1,6
     do IC = 1,3
        TOTP(IR,IC) = 0.0
        TOTR(IR,IC) = 0.0
        CDEF(IR,IC) = 0.0
        do IT = 1,2
           AVPRBO(IR,IC,IT) = 0.0
           130 continue
        end do
     end do
  end do

  !     SUM THE TOTAL FOLIAGE BY TREE SIZE.

  !.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: IFHOST= ',IFHOST
  !.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: FOLPOT= ',FOLPOT
  !.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: FOLADJ= ',FOLADJ
  !.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: FREM = ',FREM
  do IHOST = 1,NHOSTS
     if (IFHOST(IHOST).eq.0) go to 160
     do ICROWN = 1,NCROWN
        ISZI = (ICROWN+2)/3
        do IFAGE = 1,3
           TOTP(IHOST,ISZI) = TOTP(IHOST,ISZI)&
                &+(FOLPOT(IHOST,ICROWN,IFAGE)*.6)&
                &+(FOLADJ(IHOST,ICROWN,IFAGE)*.4)
           140 continue
        end do
        TOTP(IHOST,ISZI) = TOTP(IHOST,ISZI)+FREM(ICROWN,IHOST)
        150 continue
     end do
     160 continue
  end do

  !.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: TOTP= ',TOTP

  !     IF CALLING THE BUDWORM MODEL, BRANCH TO CALL.

  if (LDEFOL) then

     !        MANUAL DEFOLIATION IS BEING APPLIED.

     if (DEBUG) write (JOSTND,*) 'IN BWEDR: CALL BWEDEF'
     call BWEDEF

  else if (LREGO) then

     !        CHECK TO SEE IF THE OUTBREAK SCHEDULED FOR THIS CYCLE
     !        HAS STARTED YET

     KEND = 0
     if (DEBUG) write (JOSTND,*) 'IN BWEDR: KRECVR= ',KRECVR,&
          &' IYRCUR, IYRST, IYREND= ',IYRCUR,IYRST,IYREND

     !        IF (KRECVR.EQ.0) KEND=0

     !        IF (IYRCUR.LT.IYRST) THEN
     !          GO TO 100

     !           CHECK TO SEE IF THE CURRENT OUTBREAK HAS ENDED.

     if (IYRCUR.gt.IYREND .and. IYREND.gt.0) then
        KEND = 1
        LCALBW = .false.

        !           CHECK TO SEE IF POPULATIONS HAVE CRASHED (NON-SCHEDULED ENDINGS)

     else if (IYREND.eq.-1.and. LOWYRS.ge.3.and. IOBDUR.gt.5&
          &.and. IYRCUR.ge.IYRST) then
        KEND = 1
        IYREND = IYRCUR-1
        LCALBW = .false.

        !           CALL THE BUDWORM DEFOLIATION (BUD-LITE) MODEL.

     else if (IYRCUR.ge.IYRST) then
        if (DEBUG) write (JOSTND,*) 'IN BWEDR: CALL BWELIT'
        call BWELIT
        IOBDUR = IOBDUR+1
     end if

     !        IF THE OUTBREAK HAS ENDED, SCHEDULE THE NEXT ONE

     if (KEND.eq.1) then
        if (LP5) write (JOBWP5,180)
        180 format (4X,6('----------'))
        NEVENT = NEVENT+1
        if (NEVENT.gt.250) then
           write (JOBWP5,8250)
           8250 format ('********   ERROR - WSBW: MORE THAN 250 ENTRIES!')
           LP5 = .false.
        else
           IEVENT(NEVENT,1) = IYREND
           IEVENT(NEVENT,2) = 7
           IEVENT(NEVENT,3) = 0
           IEVENT(NEVENT,4) = 2
           !              weather year is reported only if RAWS data is in use
           if (IWSRC .eq.3) then
              IEVENT(NEVENT,5) = BWPRMS(11,IWYR)
           else
              IEVENT(NEVENT,5) = 0
           end if
        end if
        if (LP6) write (JOBWP6,8600) NPLT,IYRCUR,IYRCUR
        8600 format (A26,', ',I5,',',7X,'0,',7X,'0,',3(5X,'   ,'),&
             &1X,I5,',',6(7X,'0,'))

        if (DEBUG) write (JOSTND,*) 'IN BWEDR: CALL BWEOB'
        call BWEOB

        if (IYRST.lt.(IY(ICYC)+BWFINT)) then
           LBWEGO = .true.
           LREGO = .true.
           LCALBW = .true.
        end if
        KEND = 2
     end if

     !        IF THE RECOVERY PERIOD IS OVER, RESET THE REGIONAL OB FLAG
     !        AND THE COUNTER FOR RECOVERY YEARS (KRECVR)

     if (KEND.eq.2) then
        KRECVR = KRECVR+1
        if (KRECVR.gt.IYRSRC) then
           LREGO = .false.
           if (IYRCUR.ge.IYRST) LREGO = .true.
           if (IYRST.lt.(IY(ICYC)+BWFINT)) LREGO = .true.
           KRECVR = 0
           KEND = 0
        end if
     end if

  end if

  !     AGE THE FOLIAGE ONE YEAR.

  100 continue
  if (DEBUG) write (JOSTND,*) 'IN BWEDR: CALL BWEAGE'
  call BWEAGE (TOTR)

  !     COMPUTE ONE YEARS WORTH OF DAMAGE.

  if (DEBUG) write (JOSTND,*) 'IN BWEDR: CALL BWEDAM'
  call BWEDAM (TOTP,TOTR,AVPRBO)

  ! IF MANUAL DEFOLIATION, THEN RETURN. IF BUDLITE IS ACTIVE,
  ! TELL THE ACTIVITY SUMMARY WHAT'S HAPPENED

  if (LDEFOL) go to 9000 ! RETURN

  if (LCALBW.and.IYRCUR.ge.IYRST) then
     if (DEBUG) write (JOSTND,*) 'IN BWEDR: OPNEW 2150 YR=',IYRCUR
     I = IMGL
     !                                           Changed activity codes (RNH jan99)
     IACTC = 2150
     call OPNEW (KODE,IYRCUR,IACTC,0,PRMS)
     IACT(I,4) = IYRCUR
  else if (KRECVR.ne.0) then
     if (DEBUG) write (JOSTND,*) 'IN BWEDR: OPNEW 2154 YR=',IYRCUR
     I = IMGL
     !        IACTC = 2154
     !        CALL OPNEW (KODE,IYRCUR,IACTC,0,0)
     IACT(I,4) = IYRCUR
  end if

  9000 continue

  if (DEBUG) write (JOSTND,*) 'EXIT BWEDR: ICYC= ',ICYC
  return
end
