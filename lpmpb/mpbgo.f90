!===================!
!   lpmpb : mpbgo   !
!===================!
! LPMPB $Id$
subroutine MPBGO(L)
  implicit none

  !     L     = TRUE WHEN THE MOUNTAIN PINE BEETLE MODEL IS TO BE CALLED

  !     EVENTS CONTROLED BY THIS ROUTINE:

  !     1) DECIDES IF AN OUTBREAK WILL TAKE PLACE DURING THIS CYCLE
  !     2) SETS NUMBER OF CLASSES TO NUMBER 'MIN0(POSSABLE,REQUESTED)'

  ! Revision History:
  !   02/26/88 Last noted revision date.
  !   08/06/01 Lance R. David (FHTET)
  !     Initialization of logical variable L.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'

  ! Variables
  logical :: L,NOERR
  integer :: I,MYACTS(1),NTODO
  real    :: PROTBK,X

  data MYACTS /555/

  L = .false.

  if (.not.LMPB1) return
  NACLAS = 0
  PRBMPB(ICYC) = 0.0
  if (LEPI) PRBMPB(ICYC) = EPIPRB

  if (DEBUIN) then
     write (JOMPB,10) ICYC,MPBON,MPBYR,WORKIN
     10 format (/,'IN MPBGO:  ICYC= ',I5,' MPBON= ',I4,' MPBYR= ',I5/&
          &'  WORKIN= ',40L2)
     write (JOMPB,15) (IY(I),I = 1,NCYC)
     15 format ('IY =',T10,20I5,/,T10,20I5)
  end if

  !     CALL THE EVENT MONITOR TO 'UNSET' THE PREVIOUS MPB MORTALITY.

  call EVUST4 (4)

  !     CALL MPBER TO SEE IF MINIMUM CONDITIONS MET FOR OUTBREAK.

  call MPBER (NOERR)
  L = NOERR

  !     CALL OPFIND TO DETERMINE IF OUTBREAK SCHEDULED BY USER THIS CYCLE

  call OPFIND (1,MYACTS,NTODO)

  !     IF MINIMUM CONDITIONS MET BRANCH TO STMT 100 FOR POSSIBLE
  !     OUTBREAK.

  if (L) go to 100

  !     CONDITIONS NOT MET FOR OUTBREAK. NONE WILL OCCUR. IF NO OUTBREAK
  !     OCCURS BUT USER ASKED FOR ONE WRITE ERROR MESSAGE.

  if ( NTODO .gt.0) write (JOMPB,95)
  95 format (/,'********   WARNING:  A REQUESTED MOUNTAIN',&
       &' PINE BEETLE OUTBREAK CAN NOT BE SIMULATED.')

  !     WRITE REQUESTED GRAPHIC OUTPUT IF A OUTBREAK WAS IN PROGRESS.

  if ( MPBYR .gt.0.and. MPBGRF) call MPLOTW
  return
  100 continue

  !     AN OUTBREAK CAN  BE CONTINUED OR SIMULATED.
  !     SET NUMBER OF ACTUAL CLASSES. ALSO CALL OPDONE TO SIGNAL OPTION
  !     DONE.

  if (NTODO .gt.0) call OPDONE (1,IY(ICYC))
  NACLAS = MIN0 (ILP,NCLASS)
  if ( LEPI ) go to 110

  !     CALCULATE THE PROBABILITY OF AN OUTBREAK

  call MPOTPR ( PROTBK )

  !     SCALE THE PROBABILITY AND SAVE IT FOR FUTURE PRINTING

  PRBMPB(ICYC) = PROTBK*PRBSCL
  110 continue
  PROTBK = PRBMPB(ICYC)

  !     STORE THE PROBABILITY OF OUTBREAK IN THE EVENT MONITOR.

  call EVSET4 (6,PROTBK)

  !     MINIMUM CONDITIONS MET FOR OUTBREAK. NOW OUTBREAK WILL
  !     OCCUR IF THIS IS THE YEAR THE OUTBREAK HAS BEEN SCHEDULED
  !     OR IF IT IS ALREADY IN PROGRESS.

  if (NTODO .gt.0.or. MPBYR .gt.0) then
     L = .true.
     WORKIN(ICYC) = .true.
  else

     !        AT THIS POINT, NTODO AND MPBYR ARE BOTH ZERO.
     !        NO OUTBREAK WILL OCCUR UNLSSS THE RANSTART KEYWORD WAS
     !        WAS SPECIFIED.

     if (LRANST) then
        call MPRANN (X)
        if (X.lt.PROTBK) then

           !              AN OUTBREAK IS OCCURING.  SCHEDULE THE ACTIVITY,
           !              AND RETURN L AS TRUE.

           L = .true.
           WORKIN(ICYC) = .true.

           call OPADD (IY(ICYC),MYACTS(1),IY(ICYC),0,WK6,I)
        else
           L = .false.
           WORKIN(ICYC) = .false.
        end if
     else
        L = .false.
        WORKIN(ICYC) = .false.
     end if
  end if

  if (DEBUIN) then
     write (JOMPB,225) CNTLP,ILP,NACLAS,NCLASS
     225 format (/,'DUMP OF CONTROLING VARIABLES:',/&
          &T14,'TREES/ACRE  NUMBER OF TREE  NUMBER OF CLASSES',&
          &/,T29,'RECORDS      ACTUAL    REQUESTED'/&
          &'LODGEPOLE:   ',F10.3,I11,2I13)

     write (JOMPB,250) L
     250 format (/,'T=SIMULATION WILL GO; F=NO GO   " ',L1,' "')
  end if

  !     IF AN OUTBREAK IS TO BE SIMULATED;
  !     THEN: SAVE PAST DIAMETER GROWTH ESTIMATES.

  if (L) call MPSVDG
  return
end
