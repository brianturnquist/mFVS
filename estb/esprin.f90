!===================!
!   estb : esprin   !
!===================!
! ESTB $Id$
subroutine ESPRIN (IACTK,IDSDAT,IRECNT,KEYWRD,ARRAY,&
     &LNOTBK,JOSTND,KARD)
  implicit none

  !     PART OF THE ESTABLISHMENT SUBSYSTEM.  CALLED BY ESIN TO
  !     ENTER SITE PREP OPTIONS INTO THE ACTIVITY SCHEDULE.
  !     IACTK =THE ACTIVITY CODE ASSOCIATED WITH THE SITE PREP.
  !     IDSDAT=THE DATE OF DISTURBANCE.
  !     IRECNT=THE KEYWORD RECORD COUNT.
  !     KEYWRD=THE KEYWORD READ.
  !     ARRAY =THE PARAMETER ARRAY ON THE KEYWORD RECORD.
  !     LNOTBK=TRUE IF THE CORRESPONDING MEMBER OF ARRAY WAS ENTERED.
  !     JOSTND=THE PRINTER DATA SET REFERENCE NUMBER.

  ! Variables
  integer :: JOSTND,IRECNT,IDSDAT,IACTK,IDT,KODE
  real    :: ARRAY(12)
  logical :: LNOTBK(12)
  character(len=8) :: KEYWRD
  character(len=10) :: KARD(12)
  if (LNOTBK(1)) go to 25
  IDT = IDSDAT
  if (IDT.lt.0) go to 26
  go to 30
  25 continue
  IDT = IFIX(ARRAY(1))
  if (IDSDAT.le.0) go to 30
  if (IDT.le.IDSDAT+19) go to 30
  26 continue
  call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  return
  30 continue
  if (ARRAY(2).ge.0.0.and. ARRAY(2).le.100.0) go to 40
  call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  return
  40 continue
  call OPNEW(KODE,IDT,IACTK,1,ARRAY(2))
  if (KODE.gt.0) return
  write(JOSTND,50) KEYWRD,IDT,ARRAY(2)
  50 format(/A8,'   DATE/CYCLE=',I5,'; % PLOTS=',F6.1)
  return
end
