!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! CI $Id$
subroutine HABTYP (KARD2,ARRAY2)
  implicit none

  !     TRANSLATES HABITAT TYPE  CODE INTO SUBSCRIPTS:
  !        ITYPE = INDEX TO NI ORIGINAL 30 HABITAT TYPES
  !        ICINDX = INDEX TO CI TYPE
  !     IF KODTYP IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'CICOM.f90'

  ! Variables
  logical :: LPVCOD,LPVREF,LPVXXX
  character(len=10) :: KARD2
  integer :: NIHMAP(130),I
  real    :: ARRAY2
  real    :: RDANUW

  !  DATA STATEMENTS

  data NIHMAP/ 11*1, 3*2, 3*3, 2*2, 3, 4*4, 5, 6, 3*7, 4*8,&
       &24*9, 10, 2*11, 5*12, 13, 12, 2*13, 6*17, 2*18, 7*19, 7*20,&
       &4*21, 3*22, 3*23, 2*24, 2*25, 2*26, 11*27, 4*28, 7*29, 30/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = ARRAY2

  !  INITIALIZATIONS

  LPVREF = .false.
  LPVCOD = .false.
  LPVXXX = .false.

  !  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
  !  FVS HABITAT TYPE/ECOCLASS CODE IN THE KODTYP VARIABLE.
  !  THEN PROCESS KODTYP CODE (KODTYP IS PASSED BACK TO HABTYP
  !  THROUGH COMMON)

  if (CPVREF.ne.'          ') then
     ICL5 = 0
     call PVREF4(KARD2,LPVCOD,LPVREF)
     if ((LPVCOD.and.LPVREF).and.&
          &(KODTYP.lt.10.or.KODTYP.gt.999)) then
        call ERRGRO(.true.,34)
        LPVXXX = .true.
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        LPVXXX = .true.
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        LPVXXX = .true.
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        LPVXXX = .true.
     end if
  end if

  if (((KODTYP .lt.10).or.(KODTYP .gt.999)).and.(.not.LPVXXX)) then
     call ERRGRO (.true.,14)
  else if (.not.LPVXXX) then
     do I = 1,130
        if (KODTYP .lt. ICITYP(I)) go to 40
        20 continue
     end do
     I = 131
     40 continue
     if (I .eq.1) then
        ICINDX = 1
        ITYPE = 1
     else
        ICINDX = I-1
        ITYPE = NIHMAP(I-1)
     end if
  end if
  KODTYP = ICITYP(ICINDX)
  if (LSTART)write(JOSTND,31) KODTYP
  31 format(/,T12,'HABITAT TYPE MAPPED TO',I4,' FOR THIS PROJECTION.')

  return
end
