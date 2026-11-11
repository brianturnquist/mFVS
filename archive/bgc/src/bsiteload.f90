!=================================!
!   archive/bgc/src : bsiteload   !
!=================================!
subroutine BSITELOAD(FVS_SLOPE,FVS_ASPECT,FVS_ELEV,FVS_TLAT,&
     &FVS_KODTYP,FVS_KODFOR)

  !  **SITELOAD  BGC--DATE OF LAST REVISION: 10/16/99

  !     LOADS FVS SITE ATTRIBUTES INTO BGC MEMORY

  !     CALLED FROM: BGCGROW

  ! Common blocks
  include 'ENTITY.f90'
  include 'SITE.f90'
  ! Variables
  integer :: FVS_KODTYP,FVS_KODFOR

  !      print *,'in siteload'

  ! GET FVS SITE VARIABLES

  !  FVS_SLOPE =   slope in % scaled to 0-1
  !  FVS_ASPEC =   aspect in degrees
  !  FVS_ELEV   =  elevation in 100's feet
  !  FVS_TLAT =    latitude
  !  FVS_KODTYP =  habitat type code
  !  FVS_KODFOR =  Nat. Forest code

  SLOPE = FVS_SLOPE
  ASPECT = FVS_ASPECT*(360./6.28) !convert radians to degrees
  LAT = FVS_TLAT
  HTYPE = FVS_KODTYP
  NFLOC = FVS_KODFOR
  ELEV = INT((FVS_ELEV*100.)/3.28) ! Converts to meters

  write(*,*)
  write(*,*) 'HTYPE= ',HTYPE
  write(*,*) 'SLOPE= ',SLOPE
  write(*,*) 'ASPECT= ',ASPECT
  write(*,*) 'ELEV= ',ELEV
  write(*,*) 'NFLOC= ',NFLOC
  write(*,*) 'FVS_TLAT= ',FVS_TLAT
  write(*,*) 'LAT= ',LAT
  return
end
