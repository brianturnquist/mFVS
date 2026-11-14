!=====================!
!   base : intrscrn   !
!=====================!
! BASE $Id$
subroutine INTRSCRN (I,ISDSP,SDLO,SDHI,LDELTR)
  implicit none

  !  CALLED FROM **INTREE** AND **ORG_INTREE**.

  !  SETS LDELTR TO TRUE IF TREE RECORD I IS TO BE DELETED: ITS DBH IS
  !  ZERO, OR ITS SPECIES IS IN THE SPECIES/DBH SCREEN AND ITS DBH IS
  !  OUTSIDE [SDLO,SDHI).

  !  ISDSP  -- SPECIES SCREENED: 0 = ALL SPECIES, >0 = SPECIES ISDSP,
  !            <0 = SPECIES GROUP -ISDSP
  !  SDLO   -- LOWER DBH LIMIT OF THE SCREEN
  !  SDHI   -- UPPER DBH LIMIT OF THE SCREEN

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: I,ISDSP,IG,IULIM,IGRP
  real    :: SDLO,SDHI
  logical :: LDELTR,LINCL

  !  SET LINCL

  LINCL = .false.
  if (ISDSP.eq.0 .or. ISDSP.eq.ISP(I)) then
     LINCL = .true.
  else if (ISDSP.lt.0) then
     IGRP = -ISDSP
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
           LINCL = .true.
           go to 710
        end if
        705 continue
     end do
  end if
  710 continue

  LDELTR = (DBH(I) .lt. 0.0001) .or. &
       & (LINCL .and. (DBH(I) .lt. SDLO .or. DBH(I) .ge. SDHI))

  return
end subroutine INTRSCRN
