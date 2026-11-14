!==================!
!   dfb : dfbint   !
!==================!
! DFB $Id$
subroutine DFBINT
  implicit none

  !  INITIALIZES DFB VARIABLES AND OPENS FILES.

  !  CALLED BY :
  !     INITRE   [PROGNOSIS]

  !  CALLS :
  !     MYOPEN  (SUBROUTINE)  [PROGNOSIS]

  !  LOCAL VARIABLES :
  !     I      - COUNTER INDEX.
  !     KODE   - ERROR CODE SENT BACK FROM SUBROUTINE MYOPEN.

  !  COMMON BLOCK VARIABLES USED :
  !     DBEVNT - (DFBCOM)  OUTPUT
  !     DEBUIN - (DFBCOM)  OUTPUT
  !     EPIPRB - (DFBCOM)  OUTPUT
  !     EXPCTD - (DFBCOM)  OUTPUT
  !     EXSTDV - (DFBCOM)  OUTPUT
  !     IDBSCH - (DFBCOM)  OUTPUT
  !     IDFBYR - (DFBCOM)  OUTPUT
  !     ILENTH - (DFBCOM)  OUTPUT
  !     IPAST  - (DFBCOM)  OUTPUT
  !     ISMETH - (DFBCOM)  OUTPUT
  !     IWAIT  - (DFBCOM)  OUTPUT
  !     JODFB  - (DFBCOM)  OUTPUT
  !     JODFBX - (DFBCOM)  OUTPUT
  !     LBAMOD - (DFBCOM)  OUTPUT
  !     LDFBON - (DFBCOM)  OUTPUT
  !     LEPI   - (DFBCOM)  OUTPUT
  !     LINPRG - (DFBCOM)  OUTPUT
  !     LINV   - (DFBCOM)  OUTPUT
  !     MINDEN - (DFBCOM)  OUTPUT
  !     MWINHT - (DFBCOM)  OUTPUT
  !     NDAMS  - (DFBCOM)  OUTPUT
  !     OKILL  - (DFBCOM)  OUTPUT
  !     ORSEED - (DFBCOM)  OUTPUT
  !     PRPWIN - (DFBCOM)  OUTPUT
  !     WORKIN - (DFBCOM)  OUTPUT

  ! REVISION HISTORY:
  !   22-MAR-2002 Lance R. David (FHTET)
  !     Added variable LSET and call to DFNSD to (re)set random number generator.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DFBCOM.f90'

  ! Variables
  integer :: I,KODE
  logical :: LSET

  do I = 1,40
     WORKIN(I) = .false.
     IDFBYR(I) = 0
     100 continue
  end do
  IDFBYR(41) = 0

  DEBUIN = .false.
  LBAMOD = .false.
  LDFBON = .false.
  LEPI = .false.
  LINPRG = .false.
  LINV = .false.

  DBEVNT = 0.05
  EPIPRB = 0.5
  EXPCTD = 6.0
  EXSTDV = 2.0
  IDBSCH = 2
  ILENTH = 4
  IPAST = 1950
  ISMETH = 2
  IWAIT = 10
  JODFB = 19
  JODFBX = 0
  MINDEN = 0.0
  MWINHT = 20.0
  NDAMS = 0
  OKILL = 0.0
  ORSEED = 55329.0
  PRPWIN = 0.8

  !.... Initialized random number generator with default seed value.

  LSET = .true.
  call DFBNSD (LSET, ORSEED)

  !.... OPEN THE SUMMARY OUTPUT FILE FOR DFB.

  call MYOPEN (JODFB, ' ', 4, 133, 0, 1, 1, 0, KODE)

  return
end
