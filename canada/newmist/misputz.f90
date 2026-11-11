!==============================!
!   canada/newmist : misputz   !
!==============================!
! CANADA-NEWMIST $Id$
subroutine MISPUTZ(ITREE,IDMR)
  implicit none

  !  **MISPUTZ--MS  Date of last revision:  03/06/01

  !  Purpose:
  !     Sets dwarf mistletoe rating (stored in array IMIST) for the
  !  current tree record to the value passed in IDMR, and zeros
  !  elements in 3 DM arrays.
  !     Entry point MISPUT omits zeroing DM arrays, and is the same
  !  as the previous (before Sept. 1993) MISPUT code.

  !  Call list definitions:
  !     ITREE:  (I) Current tree record number.
  !     IDMR:   (I) Mistletoe rating to put into IMIST for current tree.

  !  Local variable definitions:
  !     DEBUG:  Logical flag to turn debug on or off.

  !  Common block variables and parameters:
  !     ACTIVE: From DMCOM; parameter.
  !     BPCNT:  From DMCOM; parameter.
  !     BRKPNT: From DMCOM;
  !     CRTHRD: From DMCOM; parameter.
  !     DMINF:  From DMCOM;
  !     DMKLDG: From DMCOM;
  !     ICYC:   From CONTRL; cycle index number.
  !     IMIST:  From MISCOM; array containing tree record DMR's.
  !     JOSTND: From CONTRL; unit number of stand output.
  !     PBRKPT: From DMCOM;

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'MISCOM.f90'
  include 'DMCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: I,J,K,KCOUNT,ITREE,IDMR

  !.... Data statements.

  !.... Check for debug.

  call DBCHK(DEBUG,'MISPUTZ',7,ICYC)

  if (DEBUG) write(JOSTND,100) ICYC
  100 format(' Begin MISPUTZ: Cycle = ',I5)

  !.... Set zeros for initial values of DM variables.

  do I = 1,BPCNT
     BRKPNT(ITREE,I) = 0.0
     PBRKPT(ITREE,I) = 0.0
  end do
  do I = 1,CRTHRD
     do J = 1,DEAD_BC
        DMINF(ITREE,I,J) = 0.0
     end do
  end do
  do I = 1,CRTHRD
     do J = 1,ACTIVE
        do K = 1,MAXBC
           DMINF_BC(ITREE,I,J,K) = 0.0
        end do
     end do
  end do

  entry MISPUT(ITREE,IDMR)

  !.... Check for debug.

  call DBCHK(DEBUG,'MISPUT',6,ICYC)

  if (DEBUG) write(JOSTND,110) ICYC
  110 format(' Begin MISPUT: Cycle = ',I5)

  !.... Check for valid dwarf mistletoe rating.

  if (IDMR .ge.0.and. IDMR .le.6) then

     !....    Put value into IMIST for current tree record number.

     IMIST(ITREE) = IDMR
     DMRATE(ITREE) = IDMR
     KCOUNT = 0

     do I = 1,BPCNT
        KCOUNT = KCOUNT + 1
        BRKPNT(ITREE,I) = DMKLDG(KCOUNT)
        KCOUNT = KCOUNT + 1
        PBRKPT(ITREE,I) = DMKLDG(KCOUNT)
     end do

     do I = 1,CRTHRD
        do J = 1,DEAD_BC
           KCOUNT = KCOUNT + 1
           DMINF(ITREE,I,J) = DMKLDG(KCOUNT)
        end do
     end do

     do I = 1,CRTHRD
        do J = 1,ACTIVE
           do K = 1,MAXBC
              KCOUNT = KCOUNT + 1
              DMINF_BC(ITREE,I,J,K) = DMKLDG(KCOUNT)
           end do
        end do
     end do

  else

     !....    Print error message and return.

     write(JOSTND,900) ITREE,IDMR
     900 format (/1X,' *** MISPUT. Invalid DM rating input.  ',&
          &'ITREE=',I5,'; IDMR=',I3,'.  0 used.')
     go to 9000
  end if

  !.... Common return.

  9000 continue

  if (DEBUG) write(JOSTND,9010) ICYC
  9010 format(' End MISPUT(Z): Cycle = ',I5)

  return
end
