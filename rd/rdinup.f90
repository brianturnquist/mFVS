!=================!
!   rd : rdinup   !
!=================!
! RD $Id$
subroutine  RDINUP
  implicit none

  !  THIS SUBROUTINE FORMS A WEIGHTED AVERAGE OF INFECTED ROOT
  !  CHARACTERISTICS FOR THE ROOT DISEASE MODEL

  !  CALLED BY :
  !     RDCNTL  [ROOT DISEASE]
  !     RDSPOR  [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  Revision History :
  !   11/06/89 - Last revision date.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: I,IDI,J,K

  !*    WRITE(6,1)
  !*  1 FORMAT(' STARTING RDINUP')

  IDI = IRRSP

  do I = 1, 2
     do J = 1,5
        PROBD(IDI,I,J) = 0.0
        DBHD(IDI,I,J) = 0.0
        ROOTD(IDI,I,J) = 0.0

        do K = 1, ISTEP
           if (PROBDA(IDI,I,J,K) .le.0) goto 300
           PROBD(IDI,I,J) = PROBD(IDI,I,J) + PROBDA(IDI,I,J,K)
           DBHD(IDI,I,J) = DBHD(IDI,I,J) + DBHDA(IDI,I,J,K) *&
                &PROBDA(IDI,I,J,K)
           ROOTD(IDI,I,J) = ROOTD(IDI,I,J) + ROOTDA(IDI,I,J,K) *&
                &PROBDA(IDI,I,J,K)
           300 continue
        end do

        DBHD(IDI,I,J) = DBHD(IDI,I,J) / (PROBD(IDI,I,J) + 1E-6)
        ROOTD(IDI,I,J) = ROOTD(IDI,I,J) / (PROBD(IDI,I,J) + 1E-6)
        400 continue
     end do
     500 continue
  end do

  !*    WRITE(6,2)
  !*  2 FORMAT(' ENDING RDINUP')

  return
end
