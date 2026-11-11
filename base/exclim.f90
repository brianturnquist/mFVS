!=====================================!
!   Provide climate extension stubs   !
!=====================================!
! BASE $Id$
subroutine EXCLIM
  implicit none

  !     EXTRA EXTERNAL REFERENCES FOR CLIMATE EXTENSION CALLS

  ! Common blocks
  include 'PRGPRM.F77'

  ! Variables
  logical :: DEBUG,LKECHO,L
  integer :: KEY,IPNT,ILIMIT,ISP,IRC
  character(len=8) :: KEYWRD
  character(len=8) :: NOCLIM
  real    :: SDIDEF(MAXSP),XMAX,TREEMULT(MAXTRE),WK3(MAXTRE),VIA
  real    :: RDANUW
  integer :: IDANUW
  logical :: LDANUW

  data NOCLIM/'*NO CLIM'/

  !  ENTRY CLINIT

  entry CLINIT
  return

  !  ENTRY CLACTV

  entry CLACTV (L)
  L = .false.
  return

  !  ENTRY CLSETACTV

  entry CLSETACTV (L)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = L
  return

  !  ENTRY CLPUT

  entry CLPUT (WK3,IPNT,ILIMIT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = WK3(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
  return

  !  ENTRY CLGET

  entry CLGET (WK3,IPNT,ILIMIT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = WK3(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
  return

  !  ENTRY CLIN

  entry CLIN  (DEBUG,LKECHO)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = DEBUG
  LDANUW = LKECHO
  return

  !  ENTRY CLKEY

  entry CLKEY(KEY,KEYWRD)
  KEYWRD = NOCLIM

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

  !  ENTRY CLGMULT

  entry CLGMULT(TREEMULT)
  TREEMULT = 1.
  return

  !  ENTRY CLMORTS

  entry CLMORTS
  return

  !  ENTRY CLMAXDEN

  entry CLMAXDEN (SDIDEF,XMAX)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = SDIDEF(1)
  RDANUW = XMAX
  return

  !  ENTRY CLAUESTB

  entry CLAUESTB
  return

  !  ENTRY CLSPVIAB

  entry CLSPVIAB (ISP,VIA,IRC)
  IRC = 1

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = VIA
  IDANUW = ISP
  return

end subroutine EXCLIM
