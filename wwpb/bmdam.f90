!==================!
!   wwpb : bmdam   !
!==================!
! WWPB $Id$
subroutine BMDAM (II,ICODES)

  !  THIS ROUTINE PROCESSES THE MOUNTAIN PINE BEETLE DAMAGE CODES.
  !  FOR THE WESTWIDE PINE BEETLE MODEL. THE ASSUMPTION IS THAT
  !  ATTACKS ARE ON LODGEPOLE PINE ONLY (THIS IS NOT CHECKED) THAT
  !  TREES MUST BE ALIVE, AND THAT THE ATTACK MUST HAVE HAPPENED
  !  WITHIN THE LAST (DEFAULT MODIFIABLE BY KEYWORD BMDAMAGE) TWO
  !  YEARS PRIOR TO THE FIRST PPE MASTER CYCLE.

  !  CALLED BY :
  !     DAMCDS  [PROGNOSIS]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]

  !  PARAMETERS :
  !     II     - TREE POINTER FOR LIVE AND DEAD TREES.
  !     ICODES - DISEASE AND DAMAGE CODES ARRAY.

  !  LOCAL VARIABLES :

  !  COMMON BLOCK VARIABLES USED :

  !  Revision History :
  !  07/28/00 AJM:
  !    Reading of damage codes changed so that damage codes 1, 2, 5
  !    are used and recognized instead of 3.
  !  07/10/07 - Lance R. David (FHTET)
  !    Time at which damage codes are processed is now at the end of
  !    keyword processing instead of during the reading of tree data.
  !    So, tree data items that were passed as arguments are now
  !    available from the FVS common area. Original arguments were:
  !    (II,IITH,ICODES)
  !    FVS array IMC(II) is used as replacement for input tree history
  !    code (IITH).
  !    No need for special handling of dead tree index (IREC2) because
  !    dead trees are already at the end of the arrays. Argument II is
  !    correct index value for both live and dead.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PPCNTL.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'

  ! Variables
  logical :: DEBUG
  dimension ICODES(6)

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'BMDAM',5,ICYC)
  if (DEBUG) write (JOSTND,*) ' ** IN BMDAM'

  LBMDAM(II) = .false.

  !     SEE IF THE INVENTORY IS WANTED FOR INITIALIZING THE MODEL.
  !     OTHERWISE BRANCH TO END

  if (.not. LINVON) goto 9000

  !     SEE IF THE INVENTORY YEAR IS RECENT ENOUGH TO USE THIS INVENTORY FOR
  !     INITIALIZING THE MODEL. OTHERWISE BRANCH TO END.

  if ((MIY(1) - IY(1)) .gt. DEDYRS) goto 9000

  !     PROCESS TREE RECORD. DEAD TREES ARE EXCLUDED, AND ONLY
  !     BOLE ATTACKS (OR NOTHING RECORDED) ARE VALID. LOADING VALUES
  !     INTO THE ARRAY WORKS BECAUSE *ALL* INVENTORIED STANDS ARE PART
  !     OF THE DISPERSAL PROCESS. THEREFORE WHEN THE DISPERSAL STANDS
  !     ARE RECORDED DURING THE READING OF THE INVENTORY, THERE WILL
  !     BE A PROPER CORRESPONDENCE BETWEEN 'ISTND', WHICH IS
  !     INCREMENTED IN PPMAIN, AND IS USED TO ASSIGN THE STAND NUMBER
  !     VIA A CALL TO GPADSD IN INSTND. THE LOGICAL FLAG IS USED IN
  !     BM??? TO CONVERT A TREES/ACRE ATTACK FOR THE BMKILL ARRAY.

  if (IMC(II) .lt.7.or. IMC(II) .gt.9) then
     do J = 1,5,2
        K1 = ICODES(J)
        K2 = ICODES(J+1)
        if ((K1.eq.1.or. K1.eq.2.or. K1.eq.5.or.&
             &K1.eq.6) .and. (K2.eq.0.or. K2.eq.3)) then
           LBMDAM(II) = .true.
        end if
        100 continue
     end do
  end if

  9000 continue

  return
end
