!=================================!
!   Stand visualization: SVCMP3   !
!=================================!
! BASE $Id$
subroutine SVCMP3
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998

  !     CALLED FROM COMPRS AND TREDEL.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'SVDATA.F77'
  include 'CONTRL.F77'
  include 'ARRAYS.F77'
  include 'WORKCM.F77'

  ! Variables
  integer :: ISVOBJ,IPT
  integer :: IWKCM1(MAXTRE)
  equivalence (IWKCM1,WORK1)

  if (JSVOUT.eq.0) return
  if (NSVOBJ.eq.0) return

  !     IF THERE ARE NO LIVE TREES LEFT, THEN REMOVE REFERENCES
  !     TO LIVE TREES.

  if (ITRN.eq.0) then
     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ).eq.1) IOBJTP(ISVOBJ) = 0
     end do
     return
  end if

  !     IPT POINTS TO TREE RECORDS.  IF ZERO, THEN THE POINTER
  !     POINTS TO A RECORD THAT WILL BE REMOVED.

  do ISVOBJ = 1,NSVOBJ
     if (IOBJTP(ISVOBJ).eq.1) then
        IPT = IS2F(ISVOBJ)
        if (IPT.gt.0) then
           IPT = IWKCM1(IPT)
           if (IPT.gt.0) IS2F(ISVOBJ) = IPT
        end if
     end if
  end do
  return
end subroutine SVCMP3
