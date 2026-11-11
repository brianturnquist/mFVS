!================!
!   sn : dgbnd   !
!================!
! SN $Id$
subroutine DGBND (ISPC,DBH,DDG)
  implicit none

  !  THIS SUBROUTINE IS INSURES THAT A MAXIMUM VALUE FOR DIAMETER
  !  GROWTH IS NOT EXCEEDED. THE DIAMETER GROWTH IS ADJUSTED LINEARLY
  !  BETWEEN THE A RANGE OF DBH VALUES BY SPECIES.  BELOW THE RANGE
  !  THERE IS NO NET ADJUSTMENT (ADJUSTMENT FACTOR = 1.) ABOVE THE
  !  RANGE THE ADJUSTMENT IS CONSTANT (ADJUSTMENT FACTOR= 0.10)
  !  THE RANGE OVER WHICH THE DG ADJUSTMENT IS PERFORMED IS
  !  CONTAINED IN THE DLODHI ARRAY.
  !  THIS ROUTINE IS CALLED BY DGDRIV.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  !  DLODHI - (I,J) I IS SPECIES AND J=1 IS THE LOOWER BOUND FOR DG
  !           ADJUSTMENT AND J=2 IS THE UPPER BOUND FOR DG ADJUSTMENT.

  ! Variables
  real    :: DLODHI(90,2),DDG,DBH
  integer :: ISPC,I,J

  data ((DLODHI(I,J),J = 1,2),I = 1,15)/&
       &26.0,     34.0,&
       &998.0,    999.0,&
       &38.0,     52.0,&
       &18.9,     24.0,&
       &998.0,    999.0,&
       &998.0,    999.0,&
       &32.6,     48.0,&
       &998.0,    999.0,&
       &18.7,     28.0,&
       &24.2,     40.0,&
       &28.7,     40.0,&
       &998.0,    999.0,&
       &998.0,    999.0,&
       &998.0,    999.0,&
       &79.8,    144.0/
  data ((DLODHI(I,J),J = 1,2),I = 16,30)/&
       &998.0,    999.0,&
       &39.3,     84.0,&
       &26.1,     34.0,&
       &26.7,     60.0,&
       &998.0,    999.0,&
       &998.0,    999.0,&
       &998.0,    999.0,&
       &998.0,    999.0,&
       &38.4,     54.0,&
       &998.0,    999.0,&
       &17.3,     27.0,&
       &998.0,    999.0,&
       &46.5,    144.0,&
       &32.9,     60.0,&
       &11.3,     13.4/
  data ((DLODHI(I,J),J = 1,2),I = 31,45)/&
       &9.7,     12.0,&
       &22.4,     27.0,&
       &42.8,     60.0,&
       &30.7,     60.0,&
       &33.4,     84.0,&
       &36.0,     48.0,&
       &37.0,     48.0,&
       &33.2,     72.0,&
       &28.1,     33.4,&
       &20.5,     36.0,&
       &998.0,    999.0,&
       &30.0,     36.0,&
       &32.9,     96.0,&
       &39.6,     60.0,&
       &998.0,    999.0/
  data ((DLODHI(I,J),J = 1,2),I = 46,60)/&
       &32.5,     43.0,&
       &27.0,     72.0,&
       &36.5,     84.0,&
       &32.5,     43.0,&
       &36.5,     84.0,&
       &21.2,     22.0,&
       &23.6,     30.0,&
       &63.8,     89.0,&
       &998.0,    999.0,&
       &33.0,     60.0,&
       &18.6,     24.0,&
       &16.7,     24.0,&
       &19.2,     36.0,&
       &56.6,    125.0,&
       &46.5,    144.0/
  data ((DLODHI(I,J),J = 1,2),I = 61,75)/&
       &48.0,     60.0,&
       &26.9,     84.0,&
       &998.0,    999.0,&
       &34.5,     48.0,&
       &42.3,     84.0,&
       &46.2,     84.0,&
       &17.2,     26.0,&
       &48.1,     84.0,&
       &48.0,     60.0,&
       &22.7,     27.0,&
       &47.2,    108.0,&
       &37.2,     72.0,&
       &47.6,     72.0,&
       &998.0,    999.0,&
       &998.0,    999.0/
  data ((DLODHI(I,J),J = 1,2),I = 76,90)/&
       &40.6,     96.0,&
       &38.9,     52.0,&
       &998.0,    999.0,&
       &58.8,     69.0,&
       &30.8,     60.0,&
       &38.8,     60.0,&
       &25.6,     31.6,&
       &998.0,    999.0,&
       &31.4,     38.0,&
       &23.9,     27.0,&
       &46.7,    130.0,&
       &35.8,     80.0,&
       &24.1,     29.0,&
       &998.0,    999.0,&
       &20.5,     25.0/

  ! MAX DG CHECK.

  if (DBH .le. DLODHI(ISPC,1)) then
     DDG = 1.0 * DDG
  else if (DBH .gt. DLODHI(ISPC,2)) then
     DDG = 0.048
  else
     DDG = DDG*(1.0 + 0.90*((DBH - DLODHI(ISPC,1))/&
          &(DLODHI(ISPC,1) - DLODHI(ISPC,2))))
     if (DDG .lt.0.048) DDG = 0.048
  end if

  ! CHECK FOR SIZE CAP COMPLIANCE.

  if ((DBH+DDG).gt.SIZCAP(ISPC,1) .and. SIZCAP(ISPC,3).lt.1.5) then
     DDG = SIZCAP(ISPC,1)-DBH
     if (DDG .lt.0.01) DDG = 0.01
  end if

  return
end
