!=================!
!   dfb : dfbgo   !
!=================!
! DFB $Id$
subroutine DFBGO(L)
  implicit none

  !  THIS ROUTINE DECIDES IF A STAND OUTBREAK WILL TAKE PLACE DURING THIS
  !  CYCLE.

  !  CALLED BY :
  !     GRINCR  [PROGNOSIS]

  !  CALLS :
  !     DFBER   (SUBROUTINE)   [DFB]
  !     OPFIND  (SUBROUTINE)   [PROGNOSIS]
  !     OPDONE  (SUBROUTINE)   [PROGNOSIS]
  !     DFBPRB  (SUBROUTINE)   [DFB]
  !     DFBRAN  (SUBROUTINE)   [DFB]

  !  PARAMETERS :
  !     L      - SET TO .TRUE. WHEN THE DOUGLAS-FIR BEETLE MODEL IS
  !              TO BE CALLED.

  !  LOCAL VARIABLES :
  !     LMIN   - SET TO .TRUE. IN SUBROUTINE DFBER IF MINIMUM CONDITIONS
  !              FOR OUTBREAK ARE MET.
  !     IACTK  - HOLDS THE OPTION PROCESSOR CODE FOR A REGIONAL DFB
  !              OUTBREAK (SET TO 2208 OR 2209).
  !     NTODO  - HOLDS NUMBER OF ACTIVITIES TO DO THIS CYCLE.  RETURNED
  !              FROM THE SUBROUTINE OPFIND.
  !     PROTBK - HOLDS THE PROBABILITY OF AN OUTBREAK.
  !     RAND   - RANDOM NUMBER FROM RANDOM NUMBER GENERATOR.

  !  COMMON BLOCK VARIABLES USED :
  !     DEBUIN - (DFBCOM)  INPUT
  !     EPIPRB - (DFBCOM)  INPUT
  !     ICYC   - (CONTRL)  INPUT
  !     IDFBYR - (DFBCOM)  OUTPUT
  !     ISMETH - (DFBCOM)  INPUT
  !     IY     - (CONTRL)  INPUT
  !     JODFB  - (DFBCOM)  INPUT
  !     LDFBON - (DFBCOM)  INPUT
  !     LEPI   - (DFBCOM)  INPUT
  !     PRBDFB - (DFBCOM)  OUTPUT
  !     WORKIN - (DFBCOM)  OUTPUT

  ! REVISION HISTORY:
  !   02-AUG-01 Lance R. David (FHTET)
  !     (no previous revision date was listed)
  !     Changed scalar variable IACTK to array with dimension of (1) due to
  !     error detected by LF95.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'DFBCOM.f90'

  ! Variables
  logical :: L,LMIN

  integer :: IACTK(1),NTODO

  real    :: PROTBK,RAND

  if (DEBUIN) write (JODFB,*) ' ** ENTERING SUBROUTINE DFBGO'

  !.... INITIALIZE L AND WORKIN(ICYC)

  L = .false.
  WORKIN(ICYC) = .false.

  !.... IF THE DFB MODEL IS NOT BEING USED THEN GO TO THE END OF THIS
  !.... SUBROUTINE AND RETURN.

  if (.not. LDFBON) goto 1000

  !.... INITIALIZE THE PROBABILITY OF THE STAND BEING INCLUDED IN THE
  !.... REGIONAL OUTBREAK TO 0.0.

  PRBDFB(ICYC) = 0.0

  !.... CALL DFBER TO CALCULATE STAND STATISTICS AND TO CHECK FOR MINIMUM
  !.... CONDITIONS FOR OUTBREAK.

  call DFBER (LMIN)

  !.... SET THE PROBABILITY OF THE STAND BEING INCLUDED IN THE REGIONAL
  !.... OUTBREAK TO EPIPRB IF THE USER USED THE STOPROB KEYWORD.

  if (LEPI) then
     PRBDFB(ICYC) = EPIPRB

  else

     !....    IF STOPROB KEYWORD NOT USED THEN CALCULATE PROBABILITY OF A
     !....    STAND OUTBREAK.

     call DFBPRB (PROTBK)

     !....    SAVE THE PROBABILITY FOR FUTURE PRINTING.

     PRBDFB(ICYC) = PROTBK
  end if

  !.... SET PROTBK TO THE PROBABILITY OF THIS STAND BEING INCLUDED IN
  !.... THE REGIONAL OUTBREAK.

  PROTBK = PRBDFB(ICYC)

  !.... CALL OPFIND TO SEE IF A REGIONAL OUTBREAK IS OCCURING.

  IACTK(1) = 2208
  call OPFIND (1,IACTK(1),NTODO)
  if (NTODO .eq.0) then
     IACTK(1) = 2209
     call OPFIND (1,IACTK(1),NTODO)
  end if

  !.... IF REGIONAL OUTBREAK OCCURED THEN TEST TO SEE IF THIS STAND
  !.... IS INCLUDED IN THE OUTBREAK.

  if (NTODO .gt.0) then

     !....    SET IDFBYR TO SHOW THAT A REGIONAL OUTBREAK OCCURED.

     IDFBYR(ICYC) = IY(ICYC)

     !....    CHECK LMIN TO SEE IF MINIMUM CONDITIONS MET FOR OUTBREAK.

     if (LMIN) then

        !....       MINIMUM CONDITIONS FOR OUTBREAK WERE MET.  TEST TO SEE IF
        !....       STAND IS INCLUDED IN REGIONAL OUTBREAK.

        if (ISMETH .eq.1) then

           !....          MANSTART; A STAND OUTBREAK WILL HAPPEN.

           L = .true.
           WORKIN(ICYC) = .true.
           call OPDONE (NTODO,IY(ICYC))

        else if (ISMETH .eq.2) then

           !....          RANSTART USED AND STAND OUTBREAK WILL HAPPEN
           !....          STOCASTICALLY.

           call DFBRAN (RAND)

           if (RAND .lt. PROTBK) then

              !....             A STAND OUTBREAK WILL OCCUR. SET L TO .TRUE. AND
              !....             SIGNAL THAT OUTBREAK HAS OCCURED.

              L = .true.
              WORKIN(ICYC) = .true.
              call OPDONE (NTODO,IY(ICYC))
           end if
        end if

     end if
  end if

  1000 continue
  if (DEBUIN) write (JODFB,*) ' ** LEAVING SUBROUTINE DFBGO'

  return
end
