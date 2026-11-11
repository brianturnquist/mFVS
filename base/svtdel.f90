!=================================!
!   Stand visualization: SVTDEL   !
!=================================!
! BASE $Id$
subroutine SVTDEL(INDEX,IVACT)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     DELETE REFERENCES FROM THE OBJECT LIST TO TREES THAT ARE
  !     BEING DELETED.

  !     CALLED FROM TREDEL.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'SVDATA.f90'
  include 'CONTRL.f90'
  include 'WORKCM.f90'

  ! Variables
  integer :: IVACT,I,II,ISVOBJ,IPT
  integer :: INDEX(MAXTRE)
  integer :: IWKCM1(MAXTRE)
  equivalence (IWKCM1,WORK1)

  if (JSVOUT.eq.0) return
  if (IVACT.eq.0) return

  do I = 1,ITRN
     IWKCM1(I) = 0
  end do
  do I = 1,IVACT
     II = -INDEX(I)
     IWKCM1(II) = 1
  end do
  do ISVOBJ = 1,NSVOBJ
     I = IS2F(ISVOBJ)
     if (I.gt.0 .and. IOBJTP(ISVOBJ).eq.1) then
        IPT = IWKCM1(I)
        if (IPT.eq.1) IS2F(ISVOBJ) = 0
     end if
  end do
  return
end subroutine SVTDEL
