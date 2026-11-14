!==========================================!
!   Provide blister rust extension stubs   !
!==========================================!
! WPBR $Id$
subroutine EXBRUS
  implicit none

  !     ENTRY POINTS FOR PROGNOSIS EXTERNAL REFERENCES TO
  !     THE WHITE PINE BLISTER RUST MODEL SUBROUTINES WHEN
  !     WHITE PINE BLISTER RUST IS NOT INCLUDED.

  dimension I3(6)

  ! Variables
  logical :: LNOTBK
  character(len=8) :: KEYWRD
  character(len=8) :: NOBR
  dimension ARRAY(7),LNOTBK(7),PROB(*),IND(*),IND1(*)
  data NOBR/'*NO BRUS'/

  !     CALLED FROM COMPRS

  entry BRCMPR (NCLAS,PROB,IND,IND1)
  return

  !     CALLED FROM DAMCDS

  !     RNH
  entry BRDAM (I1, I3)
  return

  !     CALLED FROM ESTAB

  entry BRESTB (TIME, I1, I2)
  return

  !     CALLED FROM GRADD

  entry BRTREG
  return

  !     CALLED FROM INITRE

  entry BRINIT
  return

  entry BRIN (KEYWRD,ARRAY,LNOTBK)
  call ERRGRO (.true.,11)
  return

  !     CALLED FROM MAIN

  entry BRSETP
  return

  entry BRPR
  return

  entry BRROUT
  return

  !     CALLED FROM OPLIST

  entry BRKEY (KEY,KEYWRD)
  KEYWRD = NOBR
  return

  !     CALLED FROM TREDEL

  entry BRTDEL (IVAC,IREC)
  return

  !     CALLED FROM TRESOR

  entry BRSOR
  return

  !     CALLED FROM TRIPLE

  entry BRTRIP (ITFN,I,WEIGHT)
  return

end
