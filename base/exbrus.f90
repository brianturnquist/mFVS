!==========================================!
!   Provide blister rust extension stubs   !
!==========================================!
! BASE $Id$
subroutine EXBRUS
  implicit none

  !     ENTRY POINTS FOR PROGNOSIS EXTERNAL REFERENCES TO
  !     THE WHITE PINE BLISTER RUST MODEL SUBROUTINES WHEN
  !     WHITE PINE BLISTER RUST IS NOT INCLUDED.

  ! Variables
  logical :: LNOTBK(12),LKECHO
  character(len=8) :: KEYWRD
  character(len=8) :: NOBR
  real    :: ARRAY(12),PROB(*),WEIGHT,TIME
  integer :: IND(*),IND1(*)
  integer :: I3(6),NCLAS,I1,I2,KEY,IREC,IVAC,I,ITFN
  real    :: RDANUW
  integer :: IDANUW
  character(len=8) :: CDANUW
  logical :: LDANUW

  data NOBR/'*NO BRUS'/

  !  ENTRY BRCMPR   CALLED FROM COMPRS

  entry BRCMPR (NCLAS,PROB,IND,IND1)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = NCLAS
  RDANUW = PROB(1)
  IDANUW = IND(1)
  IDANUW = IND1(1)
  return

  !  ENTRY BRDAM   CALLED FROM DAMCDS

  entry BRDAM (I1, I3)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = I1
  IDANUW = I3(1)
  return

  !  ENTRY BRESTB   CALLED FROM ESTAB

  entry BRESTB (TIME, I1, I2)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = TIME
  IDANUW = I1
  IDANUW = I2
  return

  !  ENTRY BRTREG   CALLED FROM GRADD

  entry BRTREG
  return

  !  ENTRY BRINIT   CALLED FROM INITRE

  entry BRINIT
  return

  !  ENTRY BRIN

  entry BRIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  call ERRGRO (.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = ARRAY(1)
  CDANUW(1:8) = KEYWRD(1:8)
  LDANUW = LNOTBK(1)
  LDANUW = LKECHO
  return

  !  ENTRY BRSETP   CALLED FROM MAIN

  entry BRSETP
  return

  !  ENTRY BRPR

  entry BRPR
  return

  !  ENTRY BRROUT

  entry BRROUT
  return

  !  ENTRY BRKEY    CALLED FROM OPLIST

  entry BRKEY (KEY,KEYWRD)
  KEYWRD = NOBR

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

  !  ENTRY BRTDEL    CALLED FROM TREDEL

  entry BRTDEL (IVAC,IREC)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = IVAC
  IDANUW = IREC
  return

  !  ENTRY BRSOR    CALLED FROM TRESOR

  entry BRSOR
  return

  !  ENTRY BRTRIP   CALLED FROM TRIPLE

  entry BRTRIP (ITFN,I,WEIGHT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ITFN
  IDANUW = I
  RDANUW = WEIGHT
  return

end subroutine EXBRUS
