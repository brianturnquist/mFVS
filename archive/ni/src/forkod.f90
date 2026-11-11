!============================!
!   Translate forest codes   !
!============================!
subroutine FORKOD
  implicit none

  !  **FORKOD--NI DATE OF LAST REVISION:  04/09/08

  !     TRANSLATES FOREST CODE INTO A SUBSCRIPT, IFOR, AND IF
  !     KODFOR IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  !  NATIONAL FORESTS:
  !  103 = BITTERROOT
  !  104 = IDAHO PANHANDLE
  !  105 = CLEARWATER
  !  106 = COEUR D'ALENE
  !  621 = COLVILLE
  !  110 = FLATHEAD
  !  113 = KANIKSU
  !  114 = KOOTENAI
  !  116 = LOLO
  !  117 = NEZPERCE
  !  118 = ST. JOE
  !  613 = KANIKSU ADMINISTERED BY COLVILLE (MAPPED TO KANIKSU 113)
  !  102 = BEAVERHEAD (MAPPED TO BITTERROOT 103)
  !  109 = DEERLODGE (MAPPED TO BITTERROOT 103)
  !  112 = HELENA (MAPPED TO LOLO 116)

  ! Variables
  integer :: JFOR(15),KFOR(15),NUMFOR,I
  data JFOR/103,104,105,106,621,110,113,114,116,117,118,613,&
       &102,109,112/,&
       &NUMFOR /15/
  data KFOR/1,1,3,2,1,1,1,1,1,3,2,1,1,1,1 /

  if (KODFOR .eq.0) goto 30
  do I = 1,NUMFOR
     if (KODFOR .eq. JFOR(I)) goto 20
     10 continue
  end do
  call ERRGRO (.true.,3)
  write(JOSTND,200) JFOR(IFOR)
  200 format(T12,'FOREST CODE USED FOR THIS PROJECTION IS',I4)
  goto 30
  20 continue

  if (I .eq.12) then
     write(JOSTND,22)
     22 format(T12,'KANIKSU NF (613) BEING MAPPED TO ',&
          &'KANIKSU (113) FOR FURTHER PROCESSING.')
     I = 7
  else if (I .eq.13) then
     write(JOSTND,24)
     24 format(T12,'BEAVERHEAD (102) BEING MAPPED TO ',&
          &'BITTERROOT (103) FOR FURTHER PROCESSING.')
     I = 1
  else if (I .eq.14) then
     write(JOSTND,26)
     26 format(T12,'DEERLODGE (109) BEING MAPPED TO ',&
          &'BITTERROOT (103) FOR FURTHER PROCESSING.')
     I = 1
  else if (I .eq.15) then
     write(JOSTND,28)
     28 format(T12,'HELENA (112) BEING MAPPED TO ',&
          &'LOLO (116) FOR FURTHER PROCESSING.')
     I = 9
  end if
  IFOR = I
  IGL = KFOR(I)
  30 continue
  KODFOR = JFOR(IFOR)
  return
end
