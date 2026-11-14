!===================!
!   wsbwe : bwego   !
!===================!
! WSBWE $Id$
subroutine BWEGO(LBWEGO)
  implicit none

  !     DETERMINE IF BUDWORM MODEL IS TO BE CALLED THIS CYCLE.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB., MOSCOW, ID.--MAY 1983.

  !     minor changes by K.Sheehan 7/96 to remove LBWDEB
  !     major changes by K.Sheehan 8/96+ to add Defoliation Model

  !     CALLED FROM :

  !       GRINCR -

  !     SUBROUTINES CALLED :

  !       OPFIND - FIND OPTION IN OPTION LIST.
  !       EVUST4 - UNSET EVENT MONITOR VARIABLES.
  !       BWEOB  - SCHEDULE THE NEXT BUDWORM OUTBREAK

  !     PARAMETERS :

  !   BWFINT = NUMBER OF YEARS IN THE BUDWORM PORTION OF A CYCLE
  !   ICYC -- INDEX TO CURRENT CYCLE.  VALUE SET IN **MAIN**.
  !   IY -- IY(1)=INVENTORY DATE, IY(2)=ENDPOINT OF FIRST CYCLE,
  !          IY(3)=ENDPOINT OF SECOND CYCLE,...,IY(MAXCY1)=ENDPOINT
  !           OF FORTIETH CYCLE.  KEYWORD CONTROLLED.
  !   IYREND - YEAR THAT NEXT REGIONAL OUTBREAK ENDS [BWECM2]
  !   IYRECV - YEAR THAT TREES WILL BE COMPLETELY RECOVERED [BWECM2]
  !                 FROM NEXT OUTBREAK
  !   IYRSRC - NUMBER OF YEARS AFTER END OF OUTBREAK THAT TREES [BWECM2]
  !                 WILL BE COMPLETELY RECOVERED
  !   IYRST - YEAR THAT NEXT REGIONAL OUTBREAK STARTS  [BWECM2]
  !   LBUDL  - FLAG SET TO TRUE IF A BW OUTBREAK IS (WILL BE) ACTIVE [BWECM2]
  !   LREGO  - TRUE IF A REGIONAL OUTBREAK IS ACTIVE, FALSE IF NOT
  !   LBWEGO  - RETURNED AS TRUE IF THE BUDWORM MODEL IS TO BE CALLED
  !   LCALBW = TRUE IF BUDLITE MODEL IS CALLED
  !   LDEFOL = TRUE IF USER SUPPLIES ANNUAL DEFOLIATION RATES
  !              IN THIS CYCLE...FALSE IF NOT.

  !  REVISION HISTORY
  !    27-APR-2000 Lance David (FHTET)
  !       Minor restructuring of IF blocks and removed extra RETURN
  !       statements. Removed code that had been commented-out by
  !       someone else, probably Bob Havis or Kathy.
  !       Added FVS PLOT.F77 common file for FINT (cycle length)
  !       variable that I removed from the subroutine argument list
  !       because it was not in the call statement in FVS GRINCR.F
  !       routine.
  !   30-AUG-2006 Lance R. David (FHTET)
  !      Changed array orientation of IEVENT from (4,250) to (250,4) for
  !      efficiency purposes of the PPE processes in bwepppt and bweppgt.
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
  include 'PLOT.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'
  include 'BWECM2.f90'
  include 'BWEBOX.f90'

  ! Variables
  integer :: I,MYACT(4)
  logical :: LBWEGO

  data MYACT/2150,2151,2152,2153/

  !     CHECK FOR MANUAL DEFOL.  ACTIVITIES

  LBWEGO = .false.
  call OPFIND (1,MYACT(2),I)
  LDEFOL = I.gt.0

  !     THIS SECTION DEALS WITH USER-SUPPLIED DEFOLIATION (DEFOL KEYWORD)

  if (LDEFOL) then
     LCALBW = .false.
     LBWEGO = .true.
     call EVUST4 (5)

     !       THIS SECTION DEALS WITH THE DEFOLIATION MODEL

  else if (LCALBW.or.LBUDL) then

     !       CHECK THE REGIONAL OUTBREAK FLAG (LREGO).  IF TRUE, THEN
     !       CONTINUE THE ONGOING OUTBREAK (WILL CHECK FOR END OF OUTBREAK
     !       AT START OF EACH YEAR IN BWEDR).
     !       OTHERWISE, CHECK TO SEE IF A NEW REG. OUTBREAK WILL START DURING
     !       THIS CYCLE.  IYRST (YR THAT NEXT OUTBREAK STARTS) & IYREND (YR
     !       THAT NEXT OUTBREAK ENDS) WERE SET WHEN THE SIMULATION WAS
     !       INITIALIZED OR WHEN THE MOST RECENT OUTBREAK ENDED.
     !       IF IYREND=-1, THEN BUDLITE ENDS THE OUTBREAK WHEN DEFOLIATION
     !       FALLS BELOW A THRESHOLD.
     !       IF IYREND=-2, THEN ONCE AN OUTBREAK STARTS, BUDLITE CONTINUES
     !       SIMULATING BW POPULATIONS UNTIL THE END OF THE SIMULATION.

     LCALBW = .false.
     if (LREGO) then
        LBWEGO = .true.

        if (IYREND.ne.-1) then
           if (IY(ICYC).le.IYREND) then
              LCALBW = .true.
           else
              LCALBW = .false.
           end if
        else if (IYREND.eq.-2) then
           LCALBW = .true.
        else
           if (LOWYRS.lt.3.or.IOBDUR.lt.5) then
              LCALBW = .true.
           else
              LCALBW = .false.
              IYREND = IY(ICYC)-1
              NEVENT = NEVENT+1

              if (NEVENT.gt.250) then
                 write (JOBWP4,8250)
                 8250 format ('********   ERROR - WSBW: ',&
                      &'MORE THAN 250 ENTRIES!')
                 LP4 = .false.
              else
                 IEVENT(NEVENT,1) = IYREND
                 IEVENT(NEVENT,2) = 7
                 IEVENT(NEVENT,3) = 0
                 IEVENT(NEVENT,4) = 2
                 !                weather year is reported only if RAWS data is in use
                 if (IWSRC .eq.3) then
                    IEVENT(NEVENT,5) = BWPRMS(11,IWYR)
                 else
                    IEVENT(NEVENT,5) = 0
                 end if
                 if (LP6) write (JOBWP6,8600) NPLT,IY(ICYC),IY(ICYC)
                 8600 format (A26,', ',I5,',',7X,'0,',7X,'0,',3(5X,'   ,'),&
                      &1X,I5,',',6(7X,'0,'))
              end if
           end if
        end if

        !       THIS OUTBREAK HAS ENDED, CALC. TIMING OF NEXT OUTBREAK

     else if (IYRST.lt.(IY(ICYC)+FINT).and.IYRST.ne.0) then
        LBWEGO = .true.
        LREGO = .true.
        LCALBW = .true.
     else
        LBWEGO = .true.
     end if
  end if

  ! IF NEITHER LDEFOL OR LCALBW IS TRUE, THEN SOMETHING'S AMISS...

  return
end
