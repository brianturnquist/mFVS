!=================!
!   rd : rdzero   !
!=================!
! RD $Id$
subroutine RDZERO
  implicit none

  !  THIS SUBROUTINE TAKES THE CENTERS, CHECKS IF THE RADIUS OF ANY IS ZERO
  !  (OR SMALL, LESS THAN 0,01)
  !  AND REMOVES IT FROM CONSIDERATION. WHEN A CENTER IS REMOVED, ALL VARIABLES
  !  REFERENCING A CENTER BY CENTER NUMBER MUST BE UPDATED.

  !  CALLED BY :
  !     RDCNTL  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]

  !  DEFINITIONS:

  !  Revision History:
  !   10/04/93 - Last revision date.
  !   09/04/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,ICEN,ICENT,JCEN

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'RDZERO',6,ICYC)

  if (DEBUG) write (JOSTND,1)
  1 format (' STARTING RDZERO')

  !     BEGIN PROGRAM
  ICENT = NCENTS(IRRSP)

  do ICEN = ICENT,1,-1
     if (PCENTS(IRRSP,ICEN,3) .le.0.01) then

        !         REDUCE THE NUMBER OF CENTERS
        NCENTS(IRRSP) = NCENTS(IRRSP) - 1
        if (SHCENT(IRRSP,ICEN,2) .gt.0.0)&
             &NSCEN(IRRSP) = NSCEN(IRRSP) - 1

        !         MOVE EVERYTHING UP ONE PLACE
        do JCEN = ICEN,ICENT-1
           do I = 1,3
              PCENTS(IRRSP,JCEN,I) = PCENTS(IRRSP,JCEN+1,I)
              SHCENT(IRRSP,JCEN,I) = SHCENT(IRRSP,JCEN+1,I)
              150 continue
           end do
           ICENSP(IRRSP,JCEN) = ICENSP(IRRSP,JCEN+1)
           RRATES(IRRSP,JCEN) = RRATES(IRRSP,JCEN+1)
           200 continue
        end do

        !         IF THIS IS THE FIRST CENTER TO BE REMOVED, PUT A ZERO IN THE LAST PLACE
        if (NCENTS(IRRSP) .eq. (ICENT-1)) then
           ICENSP(IRRSP,ICENT) = 0
           RRATES(IRRSP,ICENT) = 0.0
           do I = 1,3
              PCENTS(IRRSP,ICENT,I) = 0.0
              SHCENT(IRRSP,ICENT,I) = 0.0
              100 continue
           end do
        end if
     end if
     300 continue
  end do

  return
end
