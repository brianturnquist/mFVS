!===================!
!   wsbwe : bweob   !
!===================!
! WSBWE $Id$
subroutine BWEOB
  implicit none

  !  CALCULATE THE YEAR THE NEXT REGIONAL OUTBREAK STARTS & ENDS

  !  PART OF THE BUDWORM DEFOLIATION MODEL (BUDLITE)

  !  K.A. SHEEHAN, USDA-FS, R6 NATURAL RESOURCES, PORTLAND, OR

  !  CALLED FROM:  BWEIN (FIRST OUTBREAK)
  !                BWEGO (SUBSEQUENT OUTBREAKS)

  !  PARAMETERS:
  !   IYROBL - YEAR THAT LAST REGIONAL OUTBREAK STARTED [BWECM2]
  !   IOBLOC - GEOGRAPHIC LOCATION INDEX  [BWECM2]
  !                1 = SOUTHWEST, 2 = NORTHWEST, 3 = MONTANA
  !   IYRST - YEAR THAT NEXT REGIONAL OUTBREAK STARTS  [BWECM2]
  !   IYREND - YEAR THAT NEXT REGIONAL OUTBREAK ENDS [BWECM2]
  !   IYRECV - YEAR THAT TREES WILL BE COMPLETELY RECOVERED [BWECM2]
  !                 FROM NEXT OUTBREAK
  !   IYRSRC - NUMBER OF YEARS AFTER END OF OUTBREAK THAT TREES [BWECM2]
  !                 WILL BE COMPLETELY RECOVERED
  !   OBTABL(8,3) - OUTBREAK PARAMETER TABLE (PARAMETER=IOBLOC) [BWECM2]
  !       PARAMETERS:
  !          1=MEAN OUTBREAK INTERVAL, 2=INT. DEVIATION,
  !          3=INT.LOW RANGE, 4=INT.HIGH RANGE, 5=MEAN
  !          OUTBREAK DURATION, 6=DUR. DEVIATION, 7=DUR.
  !          LOW RANGE, 8=DUR.HIGH RANGE
  !   NEVENT - NO. OF BW SPECIAL EVENTS TO DATE                                   [BWEBOX]
  !   IEVENT(250,5) - BW SPECIAL EVENTS SUMMARY TABLE [BWEBOX]
  !   IOBOPT - 1 = ONLY OB STARTS SCHEDULED, 2=STARTS & ENDS SCHEDULED
  !            3=USER SPECIFIES OB STARTS AND ENDS

  ! Revision History:
  !   30-AUG-2006 Lance R. David (FHTET)
  !      Changed array orientation of IEVENT from (4,250) to (250,4).
  !   11-OCT-2006 Lance R. David (FHTET)
  !      Change variable name RANGE to OBRANG. Added debug.
  !   02-JUN-2009 Lance R. David (FMSC)
  !      Added Stand ID and comma delimiter to output tables, some header
  !      and column labels modified.
  !   14-JUL-2010 Lance R. David (FMSC)
  !      Added IMPLICIT NONE and declared variables as needed.
  !   28-AUG-2013 Lance R. David (FMSC)
  !      Added weather year (if using RAWS) to special events table.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'BWECM2.f90'
  include 'BWEBOX.f90'

  ! Variables
  integer :: I,IH
  real    :: OBINT,OBDUR,OBRANG,X
  logical :: DEBUG

  !.... Check for DEBUG

  call DBCHK(DEBUG,'BWEOB',5,ICYC)

  if (DEBUG) write (JOSTND,*) 'ENTER BWEOB: NPLT=',NPLT(1:8),&
       &' ICYC=',ICYC,' IOBOPT=',IOBOPT,' NOBDON,NOBSCH=',NOBDON,NOBSCH

  ! SEE IF THE USER HAS SCHEDULED SPECIFIC YEARS FOR OUTBREAKS

  if (IOBOPT.eq.3) then
     if (NOBDON.lt.NOBSCH) then
        NOBDON = NOBDON+1
        IYRST = IOBSCH(NOBDON,1)
        IYREND = IOBSCH(NOBDON,2)
     else
        IYRST = 0
        IYREND = 0
        IYRECV = 0
     end if
     if (DEBUG) write (JOSTND,*) 'IN BWEOB: IYRST,IYREND,IYRECV=',&
          &IYRST,IYREND,IYRECV

     ! OTHERWISE, FIRST CALC. THE NEXT INTERVAL
     !   ALWAYS MAKE 2 CALLS TO BWERAN (EVEN IF ONLY THE FIRST CALL
     !   WILL BE USED) TO ALLOW COMPARISONS BETWEEN OUTBREAK SCHEDULING
     !   OPTIONS.

  else if (IOBOPT.ne.3) then
     OBRANG = OBTABL(4,IOBLOC)-OBTABL(3,IOBLOC)
     X = 0.0
     if (DEBUG) write (JOSTND,*) 'IN BWEOB: OBRANG=',OBRANG
     !        PUT SEED VALUE FOR OUTBREAK RANDOM NUMBER SERIES.
     call BWERPT(OBSEED)
     call BWERAN(X)
     if (OBRANG.lt.1.0) then
        OBINT = OBTABL(1,IOBLOC)
        if (OBINT.lt.1.0) OBINT = 27.3
     else
        OBINT = OBTABL(3,IOBLOC)+ (OBRANG * X)
     end if
     IYRST = IYROBL + IFIX(OBINT)
     if (DEBUG) write (JOSTND,*)&
          &'IN BWEOB: IYRST,IYROBL,OBINT=',IYRST,IYROBL,OBINT
  end if

  ! NEXT CALC. THE NEXT DURATION

  if (IOBOPT.ne.3) then
     if (IOBOPT.eq.1) IYREND = -1
     if (IOBOPT.eq.4) IYREND = -2
     X = 0.0
     call BWERAN(X)
     !        GET SEED VALUE FOR OUTBREAK RANDOM NUMBER SERIES.
     call BWERGT(OBSEED)
     if (IOBOPT.eq.2) then
        OBRANG = OBTABL(8,IOBLOC)-OBTABL(7,IOBLOC)
        if (OBRANG.lt.1.0) then
           OBDUR = OBTABL(5,IOBLOC)
           if (OBDUR.lt.1.0) OBINT = 14.0
        else
           OBDUR = OBTABL(7,IOBLOC) + (OBRANG * X)
        end if
        IYREND = IYRST  + IFIX(OBDUR) - 1
        if (DEBUG) write (JOSTND,*)'IN BWEOB: OBRANG=',OBRANG,&
             &' OBDUR,IYREND=',OBDUR,IYREND
     end if
  end if

  IYROBL = IYRST
  if (IOBOPT.eq.1) then
     IYRECV = -1
  else if (IOBOPT.eq.4) then
     IYRECV = -2
  else
     IYRECV = IYREND+IYRSRC
  end if

  NEVENT = NEVENT+1

  if (DEBUG) write (JOSTND,*)&
       &'IN BWEOB: IYROBL,IYRSRC,IYRECV=',IYROBL,IYRSRC,IYRECV,&
       &' NEVENT=',NEVENT

  if (NEVENT.gt.250) then
     write (JOBWP4,8250)
     8250 format ('********   ERROR - WSBW: MORE THAN 250 ENTRIES!')
     LP4 = .false.
  else
     IEVENT(NEVENT,1) = IYRST
     IEVENT(NEVENT,2) = 7
     IEVENT(NEVENT,3) = 0
     IEVENT(NEVENT,4) = 1
     !        weather year is reported only if RAWS data is in use.
     !        If cycle is zero, weather data year is unknown.
     if (IWSRC .eq.3.and. ICYC .ne.0) then
        IEVENT(NEVENT,5) = BWPRMS(11,IWYR)
     else
        IEVENT(NEVENT,5) = 0
     end if
     I = IYRST-1
     if (LP6.and. NEVENT.gt.2) write (JOBWP6,8600) NPLT,I,I
     8600 format (A26,', ',I5,',',7X,'0,',7X,'0,',3(5X,'   ,'),&
          &1X,I5,',',6(7X,'0,'))
  end if

  !  SET LFIRST TO TRUE TO INDICATE THAT THE NEXT CALL TO BUDLITE
  !  WILL BE THE START OF A NEW OUTBREAK.  ALSO SET THE INSECTICIDE
  !  APPLICATION COUNTER (NO. OF APPLICATIONS MADE DURING THE CURRENT
  !  OUTBREAK), THE NO. OF YEARS OF DEFOLIATION, THE NO. OF CONSECUTIVE
  !  YEARS OF LOW DEFOLIATION, AND THE DURATION OF THE CURRENT OUTBREAK
  !  TO ZERO.

  LFIRST = .true.
  NUMAPP = 0
  do IH = 1,6
     DEFYRS(IH) = 0.0
     50 continue
  end do
  LOWYRS = 0
  IOBDUR = 0

  if (DEBUG) write (JOSTND,*) 'EXIT BWEOB: ICYC=',ICYC
  return
end
