!=============================!
!   Read fertilizer options   !
!=============================!
! METRIC-BASE $Id$
subroutine FFIN (JOSTND,IRECNT,KEYWRD,ARRAY,LNOTBK,KARD,LKECHO)
  implicit none

  ! Common blocks
  include  'METRIC.f90'

  !     PART OF THE FERTILIZER OPTION.  CALLED BY INITRE TO ENTER
  !     FERTILIZER KEYWORD.
  !     KEYWRD=THE KEYWORD READ.
  !     ARRAY =THE PARAMETER ARRAY ON THE KEYWORD RECORD.
  !     LNOTBK=TRUE IF THE CORRESPONDING MEMBER OF ARRAY WAS ENTERED.
  !     JOSTND=THE PRINTER DATA SET REFERENCE NUMBER.

  ! Variables
  logical :: LNOTBK(7),LKECHO
  real    :: ARRAY(7)
  integer :: IRECNT,JOSTND,IDT,KODE,I
  character(len=8) :: KEYWRD
  character(len=10) :: KARD(7)

  !     200 LBS = 91 KG (TO THE NECESSARY PRECISION)

  if (.not.LNOTBK(2)) ARRAY(2) = 91.0
  if (ARRAY(2).ne.91.0) then
     if (LKECHO)write(JOSTND,5)
     5 format (T13,'ONLY TREATMENTS WITH 91 KG NITROGEN ',&
          &'CAN BE REPRESENTED.  PARAMETER CHANGED ACCORDINGLY.')
     ARRAY(2) = 200.0
  end if
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (.not.LNOTBK(5)) ARRAY(5) = 1.0
  call OPNEW (KODE,IDT,260,4,ARRAY(2))
  if (KODE.gt.0) return
  write (JOSTND,10) KEYWRD,IDT,(ARRAY(I),I = 2,5)
  10 format(/1X,A8,'   DATE/CYCLE=',I5,'; APPLY ',F6.0,' KG ',&
       &'NITROGEN, ',F6.0,' KG PHOSPHORUS, AND ',F6.0,&
       &' KG POTASSIUM PER ACRE.'/&
       &T13,'THE EFFECT OF THIS APPLICATION IS MULTIPLIED BY ',&
       &F10.4)
  if (ARRAY(3).gt.0..or. ARRAY(4).gt.0) write (JOSTND,20)
  20 format (T13,'TREATMENTS WITH PHOSPHORUS AND POTASSIUM CAN NOT ',&
       &'BE REPRESENTED.  THESE VALUES WILL BE IGNORED.')
  if (ARRAY(2).ne.200.0) write (JOSTND,30)
  30 format (T13,'TREATMENTS WITH OTHER THAN 91 KG NITROGEN ',&
       &'CAN NOT BE REPRESENTED; 91 KG IS ASSUMED.')
  return
end
