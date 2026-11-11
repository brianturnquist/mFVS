!=======================================!
!   Provide mistletoe extension stubs   !
!=======================================!
! BASE $Id$
subroutine EXMIST
  implicit none

  !  Purpose:
  !     Dummy external references for dwarf mistletoe model routines. A
  !  dummy version of function MISDGF is at the end of this file.

  !  Call list definitions (for entry point routines):
  !     ARRAY:  (I)  Array containing keyword entries.
  !     DMFLAG: (IO) Logical flag; TRUE if theres any DM in the stand.
  !     IARRAY: (IO) Array containing random integers between 1 and ISIZE.
  !     ICODE:  (I)  Array of mistletoe damage codes.
  !     IDMR:   (IO) Current tree dwarf mistletoe rating.
  !     IPNT:   (IO) Pointer to current element in print buffer WK3.
  !     ILIMIT: (I)  Size of print buffer WK3.
  !     ISIZE:  (I)  Range of integers in IARRAY.
  !     ITREE:  (I)  Current tree record number.
  !     KEY:    (I)  Keyword number used in MISKEY call list.
  !     KEYWRD: (IO) Character buffer containing keyword name.
  !     LNOTBK: (O)  Logical array depicting blank keyword fields.
  !     MFLAG:  (I)  Logical flag to update WK2 with mistletoe mortality.
  !     MSPCNT: (O)  Array of number of DM infected records by species.
  !     NFLAG:  (O)  Logical flag indicating presence of mistletoe code.
  !     PRFCUT: (O)  Array of mistletoe cutting preferences by species.
  !     WK3:    (IO) Work array used as a print buffer.

  !  Local variable definitions:
  !     NOMIS:  Character buffer used to tell MISKEY this is a no DM run.

  !  Common block variables and parameters:
  !     MAXSP:  From PRGPRM; maximum number species.
  !     MAXTRE: From PRGPRM; maximum number tree records.

  !  PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.F77'

  !  VARIABLE DECLARATIONS.

  ! Variables
  integer :: I,ITREE,IDMR,KEY,IDMR1,ISIZE,ILIMIT,IPNT
  logical :: LNOTBK(12),DMFLAG,MFLAG,NFLAG,LKECHO
  real    :: ARRAY(12),PRFCUT(MAXSP),WK3(MAXTRE)
  integer :: ICODE(6),MSPCNT(MAXSP),IARRAY(MAXTRE)
  character(len=8) :: KEYWRD
  character(len=8) :: NOMIS
  character(len=1) :: CBUFF
  real    :: RDANUW
  integer :: IDANUW
  character(len=8) :: CDANUW
  logical :: LDANUW

  ! Data statements
  data NOMIS/'*NO MIST'/

  !  ENTRY MISCNT

  entry MISCNT(MSPCNT)
  do I = 1,MAXSP
     MSPCNT(I) = 0
     100 continue
  end do
  return

  !  ENTRY MISCPF

  entry MISCPF(PRFCUT)
  do I = 1,MAXSP
     PRFCUT(I) = 0.0
     150 continue
  end do
  return

  !  ENTRY MISDAM

  entry MISDAM(ITREE,ICODE)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ITREE
  RDANUW = ICODE(1)
  return

  !  ENTRY MISGET

  entry MISGET(ITREE,IDMR)
  IDMR = 0

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ITREE
  return

  !  ENTRY MISIN

  entry MISIN(KEYWRD,ARRAY,LNOTBK,LKECHO)
  call ERRGRO(.true.,11)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW = KEYWRD
  RDANUW = ARRAY(1)
  LDANUW = LNOTBK(1)
  LDANUW = LKECHO
  return

  !  ENTRY MISINF

  entry MISINF(DMFLAG)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = DMFLAG
  return

  !  ENTRY MISIN0

  entry MISIN0
  return

  !  ENTRY MISINT

  entry MISINT
  return

  !  ENTRY MISKEY (ENTRY POINT IN MISIN)

  entry MISKEY(KEY,KEYWRD)
  KEYWRD = NOMIS

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = KEY
  return

  !  ENTRY MISMRT

  entry MISMRT(MFLAG)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  LDANUW = MFLAG
  return

  !  ENTRY MISPRT

  entry MISPRT
  return

  !  ENTRY MISPUT

  entry MISPUT(ITREE,IDMR)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ITREE
  IDANUW = IDMR
  return

  !  ENTRY MISPUTZ

  entry MISPUTZ(ITREE,IDMR1)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ITREE
  IDANUW = IDMR1
  return

  !  ENTRY MISRAN

  entry MISRAN(IARRAY,ISIZE)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ISIZE
  IDANUW = IARRAY(1)
  return

  !  ENTRY MISTOE

  entry MISTOE
  return

  !  ENTRY MSPPPT - PPE -

  entry MSPPPT (WK3,IPNT,ILIMIT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = WK3(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
  return

  !  ENTRY MSCHPUT

  entry MSCHPUT (CBUFF, IPNT, ILIMIT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW(1:1) = CBUFF(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
  return

  !  ENTRY MSPPGT - PPE -

  entry MSPPGT (WK3,IPNT,ILIMIT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  RDANUW = WK3(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
  return

  !  ENTRY MSCHGET

  entry MSCHGET (CBUFF, IPNT, ILIMIT)

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  CDANUW(1:1) = CBUFF(1)
  IDANUW = IPNT
  IDANUW = ILIMIT
  return

  !  ENTRY MISACT

  entry MISACT (NFLAG)
  NFLAG = .false.
  return

end subroutine EXMIST

  ! Variables
real function MISDGF(ITREE,ISPC)

  !  **MISDGF--MS  Date of last revision:  04/09/91

  !  Purpose:
  !     This is a dummy version of the mistletoe infection diameter
  !  growth function. It returns MISDGF as 1.0 always (i.e. 100 percent
  !  potential diameter growth).

  !  Call list definitions:
  !     ISPC:   (I) Current tree species.
  !     ITREE:  (I) Current tree record number.
  !     MISDGF: (O) Returns the 10 year proportion of potential diameter
  !                growth due to mistletoe infection.

  MISDGF = 1.0

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ITREE
  IDANUW = ISPC
  return

end function MISDGF

  ! Variables
real function MISHGF(ITREE,ISPC)

  !  **MISHGF--MS  Date of last revision:  04/01/11

  !  Purpose:
  !     This is a dummy version of the mistletoe infection height
  !  growth function. It returns MISHGF as 1.0 always (i.e. 100 percent
  !  potential height growth).

  !  Call list definitions:
  !     ISPC:   (I) Current tree species.
  !     ITREE:  (I) Current tree record number.
  !     MISHGF: (O) Returns the 10 year proportion of potential height
  !                growth due to mistletoe infection.

  MISHGF = 1.0

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  if (.true.)return
  IDANUW = ITREE
  IDANUW = ISPC
  return

end function MISHGF
