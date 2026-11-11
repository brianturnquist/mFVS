!==========================================!
!   Provide regeneration extension stubs   !
!==========================================!
! BASE $Id$
subroutine EXESTB
  implicit none

  !     EXTRA REFERENCES FOR REGENERATION ESTABLISHMENT

  ! Common blocks
  include 'PRGPRM.F77'
  include 'PLOT.F77'
  include 'CALDEN.F77'

  ! Variables
  character(len=8) :: KEYWRD
  character(len=8) :: NOESTB
  character(len=8) :: PASKEY
  real    :: ARRAY(1)
  dimension LNOTBK(1)
  integer :: IPVARS(1)
  integer :: KEY,NPNVRS,IMC1,ITREI,IPTKNT,JOSTND,I,ID,ISHAG,JPLOT
  integer :: JSSP,ICALL,LENGTH
  real    :: DBH,PREM
  logical :: LACTV,LNOTBK,LFG,LKECHO
  real    :: RDANUW
  integer :: IDANUW
  character(len=8) :: CDANUW
  logical :: LDANUW

  data NOESTB/'*NO ESTB'/

  !  ENTRY ESIN

  entry ESIN(PASKEY,ARRAY,LNOTBK,LKECHO)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW(1:8) = PASKEY(1:8)
  RDANUW = ARRAY(1)
  LDANUW = LNOTBK(1)
  LDANUW = LKECHO
  return

  !  ENTRY ESNUTR

  entry ESNUTR
  return

  !  ENTRY ESKEY

  entry ESKEY(KEY,KEYWRD)
  KEYWRD = NOESTB

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

  !  ENTRY ESNOAU

  entry ESNOAU (KEYWRD)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW(1:8) = KEYWRD(1:8)
  return

  !  ENTRY ESINIT

  entry ESINIT
  return

  !  ENTRY ESOUT

  entry ESOUT(LFG)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = LFG
  return

  !  ENTRY ESPLT1

  entry ESPLT1(ITREI,IMC1,NPNVRS,IPVARS)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ITREI
  IDANUW = IMC1
  IDANUW = NPNVRS
  IDANUW = IPVARS(1)
  return

  !  ENTRY ESPLT2

  entry ESPLT2(JOSTND,IPTKNT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = JOSTND
  IDANUW = IPTKNT
  return

  !  ENTRY ESEZCR

  entry ESEZCR(JOSTND)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = JOSTND
  return

  !  ENTRY ESFLTR

  entry ESFLTR
  do I = 1,MAXPLT
     BAAINV(I) = 0.0
     10 continue
  end do
  TPACRE = 0.0
  return

  !  ENTRY ESXTRA

  entry ESXTRA
  return

  !  ENTRY ESHTS

  entry ESHTS
  return

  !  ENTRY ESDTLS

  entry ESDTLS (ID)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ID
  return

  !  ENTRY ESTUMP

  entry ESTUMP (JSSP,DBH,PREM,JPLOT,ISHAG)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = JSSP
  RDANUW = DBH
  RDANUW = PREM
  IDANUW = JPLOT
  IDANUW = ISHAG
  return

  !  ENTRY ESADDT

  entry ESADDT (ICALL)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ICALL
  return

  !     EXTRA REFERENCES FOR THE PARALLEL PROCESSING SYSTEM.

  !  ENTRY ESGET

  entry ESGET
  return

  !  ENTRY ESPUT

  entry ESPUT
  return

  !  ENTRY ESSPRQ

  entry ESSPRQ (LENGTH)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = LENGTH
  return

  !  ENTRY ESACTV

  entry ESACTV (LACTV)
  LACTV = .false.
  return

end subroutine EXESTB
