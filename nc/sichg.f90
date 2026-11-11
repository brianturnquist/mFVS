!================!
!   nc : sichg   !
!================!
! NC $Id$
subroutine SICHG(ISISP,SSITE,SIAGE,JOSTND)
  implicit none

  ! Variables
  character(len=1) :: ISILOC
  character(len=1) :: REFLOC(12)
  integer :: IREFAG(12)
  real    :: A(12),B(12),SIAGE(12)
  real    :: SIMAX(12),SIMIN(12)
  integer :: JOSTND,ISISP,I,IDIFF
  real    :: SSITE,TEMSI,AGE2BH,SPREAD,RELSI
  integer :: IDANUW

  data B/-0.08,-0.05,-0.08,-0.07,-0.02,-0.05,-0.05,-0.03,&
       &-0.06,-0.05,-0.03,-0.08/
  data A/10.0,12.0,10.0,10.0,3.0,10.0,6.0,4.0,&
       &10.0,12.0,4.0,10.0/
  data SIMIN/50.,40.,50.,30.,50.,30.,30.,50.,30.,40.,50.,50./
  data SIMAX/150.,120.,150.,130.,100.,130.,70.,90.,130.,120.,90.,&
       &150./
  data REFLOC/&
       &'B','T',7*'B','T','B','B'/
  data IREFAG/&
       &12*50/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = JOSTND

  !  ISILOC IS THE PLACE THE AGE FOR THE SSITE FROM STDINFO IS TAKEN

  TEMSI = SSITE
  ISILOC = REFLOC(ISISP)
  do I = 1,12

     !  SET UP THE ARRAY TO TELL WHETHER YOU NEED TO SLIDE UP OR DOWN THE SIT
     !  LINE TO ADJUST FOR TOTAL AGE OR BREAST HIGH AGE

     if (ISILOC .eq. 'T' .and. REFLOC(I) .eq. 'B')IDIFF = -1
     if (ISILOC .eq. REFLOC(I))IDIFF = 0
     if (ISILOC .eq. 'B' .and. REFLOC(I) .eq. 'T')IDIFF = 1
     AGE2BH = 0.0
     if (IDIFF .lt.0.or. IDIFF .gt.0) then
        if (TEMSI .lt. SIMIN(ISISP))TEMSI = SIMIN(ISISP)
        if (TEMSI .gt. SIMAX(ISISP))TEMSI = SIMAX(ISISP)
        SPREAD = SIMAX(ISISP)-SIMIN(ISISP)
        RELSI = 100.0*(TEMSI-SIMIN(ISISP))/SPREAD
        AGE2BH = A(I) + B(I)*RELSI
     end if
     SIAGE(I) = IREFAG(I) + AGE2BH*IDIFF
     100 continue
  end do
  return
end
