!====================!
!   wsbwe : bwedef   !
!====================!
! WSBWE $Id$
subroutine BWEDEF
  implicit none

  !     APPLIES DEFOLIATION RATES AS SPECIFIED BY A USER USING DEFOL
  !     OPTION.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JANUARY 1984

  !     minor changes by K.Sheehan 7/96 to remove LBWDEB

  !     CALLED FROM :

  !       BWEDR - DRIVE THE BUDWORM MODEL FOR ONE STAND IN ONE YEAR.

  !     SUBROUTINES CALLED :

  !       OPGET  - GET NEXT OPTION.
  !       OPDEL1 - DELETE OPTION.
  !       OPDONE - SIGNAL THAT OPTION IS DONE.
  !       BWESIN - SET DO LOOP INDICES FOR SETPRBIO AND DEFOL OPTIONS.

  ! REVISION HISTORY
  !   05-OCT-2006 Lance R. David
  !     Previous date noted was 2/25/97 by Kathy Sheehan.
  !     Added debug handling.
  !   14-JUL-2010 Lance R. David (FMSC)
  !     Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'

  ! Variables
  integer :: IACT,ICRC1,ICRC2,ICRC3,ICROWN,IDT,IHOST,IS1,IS2,NP
  real    :: PARMS(6)
  logical :: DEBUG

  !.... Check for DEBUG

  call DBCHK(DEBUG,'BWEDEF',5,ICYC)

  if (DEBUG) write (JOSTND,*) 'ENTER BWEDEF: ICYC=',ICYC,&
       &' ITODO=',ITODO,' NTODO=',NTODO,' IYRCUR=',IYRCUR

  10 continue

  !     IF ALL OF THE OPTIONS HAVE BEEN PROCESSED, THEN:  RETURN.

  if (ITODO.gt.NTODO) goto 1000
  call OPGET (ITODO,6,IDT,IACT,NP,PARMS)
  if (DEBUG) write (JOSTND,*) 'IN BWEDEF: OPGET RETURN: ',&
       &'ITODO, IDT, IACT, PARMS= ', ITODO, IDT, IACT, PARMS

  !     IF THE DATE OF THE OPTION IS GREATER OR EQUAL TO THE CURRENT YEAR,
  !     THEN: KEEP THE OPTION.

  if (IDT.ge.IYRCUR) goto 20

  !     ELSE: THE OPTION IS OLDER THAN THE YEAR, DELETE IT AND GET THE
  !     NEXT ONE.

  call OPDEL1 (ITODO)
  ITODO = ITODO+1
  goto 10
  20 continue

  !     IF THE DATE OF THIS OPTION IS GREATER THAN THE CURRENT DATE, THEN:
  !     RETURN.

  if (IDT.gt.IYRCUR) goto 1000

  !     SIGNAL THAT THE OPTION IS DONE.

  call OPDONE (ITODO,IYRCUR)
  ITODO = ITODO+1

  !     LOAD INDEXING BOUNDS.

  call BWESIN (PARMS,IBWSPM,IS1,IS2,ICRC1,ICRC2,ICRC3)
  if (DEBUG) write (JOSTND,*)&
       &'IN BWEDEF: IBWSPM, IS1, IS2, ICRC1, ICRC2, ICRC3',&
       &IBWSPM, IS1, IS2, ICRC1, ICRC2, ICRC3

  !     IF THE SPECIES MAPED TO NON-HOST, THEN DELETE OPTION.

  if (IS1.le.6) goto 30
  call OPDEL1 (ITODO-1)
  goto 10
  30 continue

  !     APPLY MANUAL DEFOLIATION.

  do IHOST = IS1,IS2
     if (IFHOST(IHOST).eq.0) goto 80
     do ICROWN = ICRC1,ICRC2,ICRC3
        FNEW(ICROWN,IHOST) = FNEW(ICROWN,IHOST)*(1.-(PARMS(3)/100.))
        FOLD1(ICROWN,IHOST) = FOLD1(ICROWN,IHOST)*(1.-(PARMS(4)/100.))
        FOLD2(ICROWN,IHOST) = FOLD2(ICROWN,IHOST)*(1.-(PARMS(5)/100.))
        FREM (ICROWN,IHOST) = FREM (ICROWN,IHOST)*(1.-(PARMS(6)/100.))
        70 continue
     end do
     80 continue
  end do

  !     GET ANOTHER OPTION.

  goto 10
  1000 continue
  if (DEBUG) write (JOSTND,*)'EXIT BWEDEF: ICYC=', ICYC
  return
end
