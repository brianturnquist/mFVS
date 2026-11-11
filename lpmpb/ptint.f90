!===================!
!   lpmpb : ptint   !
!===================!
! LPMPB $Id$
subroutine PTINT(IPLTNO, NINC, XAXIS, ANEW)
  implicit none

  ! Revision History
  !   02/08/88 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.
  !   08/22/14 Lance R. David (FMSC)
  !     Function names were used as variable names.
  !     changed variable INT to INC
  !     changed variable AINT to AINC
  !     changed variable NINT to NINC

  ! Variables
  integer :: IP,IPLTNO,NINC,NINT1,NP

  real    :: ANEW(10),OLD(10),DIFF(10),AINC,DIFAX,INC,OLDAX,XAXIS

  NP = 10
  if (NINC .le.1) go to 420
  AINC = FLOAT(NINC)

  !     ** COMPUTE DIFFERENCES
  do IP = 1,NP
     DIFF(IP) = (ANEW(IP) - OLD(IP))/AINC
     390 continue
  end do
  DIFAX = (XAXIS - OLDAX)/AINC

  !     **  INTERPOLATE PLOT VALUES
  NINT1 = NINC - 1
  do INC = 1,NINT1
     OLDAX = OLDAX + DIFAX
     do IP = 1,NP
        OLD(IP) = OLD(IP) + DIFF(IP)
        400 continue
     end do
     call PTARY (IPLTNO, OLDAX, OLD)
     410 continue
  end do
  420 continue

  !     ** SAVE OLD VALUES FOR PLOT
  do IP = 1,NP
     OLD(IP) = ANEW(IP)
     430 continue
  end do
  OLDAX = XAXIS

  !     ** SPOOL NEW VALUES
  call PTARY(IPLTNO, XAXIS, ANEW)

  return
end
