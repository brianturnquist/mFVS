!=================!
!   rd : rdpush   !
!=================!
! RD $Id$
subroutine RDPUSH
  implicit none

  !  PROCESSES THE PSTUMP KEYWORD.

  !  CALLED BY :
  !     RDTREG  [ROOT DISEASE]

  !  CALLS     :
  !     OPFIND  (SUBROUTINE)   [PROGNOSIS]
  !     OPGET   (SUBROUTINE)   [PROGNOSIS]
  !     OPDONE  (SUBROUTINE)   [PROGNOSIS]

  !  PARAMETERS :
  !     NONE

  !  COMMON BLOCK VARIABLES :
  !     xxxxx:   From ANCOM;

  !  LOCAL VARIABLES :

  !  Revision History :
  !   03/24/93 - Last revision date.
  !   09/02/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDADD.f90'

  !.... DATA statements

  ! Variables
  integer :: I,IACTK,IDI,J,K,KDT,MYACT(1),NPS,NTODO
  real    :: DIAM,PROP,PRMS(2)

  data MYACT /2403/

  call OPFIND (1,MYACT,NTODO)
  if (NTODO .le.0) return

  call OPGET (NTODO,2,KDT,IACTK,NPS,PRMS)
  call OPDONE (NTODO,IY(ICYC))
  PROP = PRMS(1)
  DIAM = PRMS(2)

  do IDI = MINRR,MAXRR
     do I = 1,2
        do J = 1,5
           do K = 1,ISTEP

              !                 PUSH STUMPS

              !                 WRITE (IRUNIT,999) IDI, I, J, K, PROBDA(IDI,I,J,K),
              !    &                  DBHDA(IDI,I,J,K),DIAM,PROP
              ! 999             FORMAT ('PUSHSTUMPS: ',4I5,2F10.5,2F10.5)

              if (DBHDA(IDI,I,J,K) .ge. DIAM)&
                   &PROBDA(IDI,I,J,K) = PROBDA(IDI,I,J,K) * (1 - PROP)
              if (PROBDA(IDI,I,J,K) .ne.0.0) goto 115

              !                 IF ALL STUMPS WERE REMOVED, ZERO ARRAYS ASSOCIATED
              !                 WITH STUMPS

              DBHDA(IDI,I,J,K) = 0.0
              ROOTDA(IDI,I,J,K) = 0.0
              JRAGED(IDI,I,J,K) = 0

              !*                DO 1010 J = 1,NCENTS(IDI)
              !*                   PCENTS(IDI,J,3) = 0.0
              !1010             CONTINUE
              !*                NCENTS(IDI)=0

              115 continue
           end do
           110 continue
        end do
        100 continue
     end do
     95 continue
  end do

  return
end
