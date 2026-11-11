!=================!
!   dfb : dfber   !
!=================!
! DFB $Id$
subroutine DFBER (NOER)
  implicit none

  !  TESTS TO MAKE SURE MINIMUM CONDITIONS ARE MET FOR AN OUTBREAK TO
  !  OCCUR.  CALCULATES BASAL AREA FOR DOUGLAS-FIR.

  !  CALLED BY :
  !     DFBGO  [DFB]

  !  CALLS :
  !     NONE

  !  PARAMETERS :
  !     NOER   - SET TO .TRUE. WHEN MINIMUM CONDITIONS MET FOR AN
  !              OUTBREAK.

  !  LOCAL VARIABLES :
  !     BADF   - BASAL AREA FOR EACH DOUGLAS-FIR RECORD.
  !     D      - DBH FOR TREE RECORD (SET TO DBH(TREE #)).
  !     I      - INDEX FOR TREE RECORD (SET TO IND1(TREE PTR)).
  !     II     - COUNTER INDEX.
  !     I1,I2  - INDEXES FOR THE ARRAY IND1.  I1 IS THE STARTING
  !              INDEX FOR A SPECIES IN ARRAY IND1 (DF = IDFSPC).
  !              I2 IS THE ENDING INDEX FOR A SPECIES IN ARRAY IND1
  !              (DF = IDFSPC).
  !     P      - PROB FOR TREE RECORD (SET TO PROB(TREE #)).
  !     TDF45  - NUMBER OF DOUGLAS-FIR (TREES/ACRE) THAT HAVE A DBH >= 4.5
  !              INCHES.
  !     BADF45 - BASAL AREA FOR ALL DOUGLAS-FIR TREES WITH DBH >= 4.5
  !              INCHES.

  !  COMMON BLOCK VARIABLES USED :
  !     A45DBH - (DFBCOM)  OUTPUT
  !     BA     - (PLOT)    INPUT
  !     BA9    - (DFBCOM)  OUTPUT
  !     BADF9  - (DFBCOM)  OUTPUT
  !     DBH    - (ARRAYS)  INPUT
  !     DEBUIN - (DFBCOM)  INPUT
  !     IDFSPC - (DFBCOM)  INPUT
  !     IND1   - (ARRAYS)  INPUT
  !     ISCT   - (CONTRL)  INPUT
  !     ITRN   - (CONTRL)  INPUT
  !     JODFB  - (DFBCOM)  INPUT
  !     PBADF4 - (DFBCOM)  OUTPUT
  !     PROB   - (ARRAYS)  INPUT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'DFBCOM.f90'

  ! Variables
  logical :: NOER

  integer :: I,II,I1,I2

  real    :: BADF,BADF45,TDF45,D,P

  if (DEBUIN) write (JODFB,*) ' ** ENTERING SUBROUTINE DFBER'

  !.... INITIALIZE VARIABLES.

  NOER = .true.
  BADF45 = 0.0
  BADF9 = 0.0
  A45DBH = 0.0
  TDF45 = 0.0
  PBADF4 = 0.0
  BA9 = 0.0

  !.... CALCULATE THE BASAL AREA FOR ALL TREES IN THE STAND WITH A DBH
  !.... GREATER THEN OR EQUAL TO 9.0 INCHES.

  do II = 1,ITRN
     if (DBH(II) .ge.9.0) then
        BA9 = BA9 + 0.005454154 * DBH(II) * DBH(II) * PROB(II)
     end if
     100 continue
  end do

  !.... CALCULATE THE BASAL AREA, AVERAGE DBH VALUES, AND TREES/ACRE FOR
  !.... DIFFERENT SIZE GROUPS IN DOUGLAS-FIR.

  I1 = ISCT(IDFSPC,1)
  if (I1.eq.0) goto 300
  I2 = ISCT(IDFSPC,2)

  do II = I1, I2
     I = IND1(II)
     P = PROB(I)
     D = DBH(I)
     BADF = 0.005454154 * D * D * P

     if (D .ge.4.5) then
        TDF45 = TDF45 + P
        A45DBH = A45DBH + D * P
        BADF45 = BADF45 + BADF
     end if

     if (D .ge.9.0) then
        BADF9 = BADF9 + BADF
     end if
     200 continue
  end do

  !.... NOT ENOUGH DF FOR OUTBREAK TO OCCUR.  GO TO THE END.

  if (TDF45.lt.1.0) goto 300

  !.... PROPORTION BASAL AREA IN DF GREATER THEN 4.5 INCHES

  PBADF4 = BADF45 / BA

  !.... AVERAGE DBH FOR DOUGLAS-FIR TREES WITH A DBH >= 4.5 INCHES.

  A45DBH = A45DBH  / TDF45
  goto 400

  300 continue

  !.... SET FLAG TO SAY NO OUTBREAK WILL OCCUR.

  NOER = .false.

  400 continue
  if (DEBUIN) write (JODFB,*) ' ** LEAVING SUBROUTINE DFBER'

  return
end
