!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! KT $Id$
subroutine HABTYP (KARD2,ARRAY2)
  implicit none

  !     TRANSLATES HABITAT TYPE CODE INTO A SUBSCRIPT, ITYPE, AND IF
  !     KODTYP IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.   THIS
  !     ROUTINE ALSO TRANSLATES HABITAT TYPE INTO A SUBSCRIPT FOR THE
  !     KOOTENAI CODES WHICH IS USED IN DGF.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'KOTCOM.f90'

  ! Variables
  real    :: ARRAY2
  character(len=10) :: KARD2
  integer :: KTYPE(95),MTYPE(30),K
  logical :: LPVCOD,LPVREF,LPVXXX
  real    :: RDANUW

  data KTYPE/&
       &5*1, 4*2, 4, 3*1, 3,4,5,6,7,8,9,8,8,9,7,3, 3*10, 4,11,20,29,11,&
       &11,13,14,17, 4*12, 3*13, 14,15,14,16,14,16, 3*17, 24,12,24,18,&
       &19,21,19,20,20,21,22,19,23,19,24,27,25,25,26,27,22,24,27,24,&
       &27,28,28,29,28,28, 4*29, 27,9,20,24,21,27,24,30/
  data MTYPE/&
       &130,170,250,260,280,290,310,320,330,420,&
       &470,510,520,530,540,550,570,610,620,640,&
       &660,670,680,690,710,720,730,830,850,999/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = ARRAY2

  LPVREF = .false.
  LPVCOD = .false.
  LPVXXX = .false.

  !  IF KODTYP IS OUTSIDE THE ALLOWABLE RANGE, USE DEFAULT VALUE.

  !  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
  !  FVS HABITAT TYPE/ECOCLASS CODE IN THE KODTYP VARIABLE.
  !  THEN PROCESS KODTYP CODE (KODTYP IS PASSED BACK TO HABTYP
  !  THROUGH COMMON)

  KARD2 = ADJUSTL(KARD2)
  if (LEN_TRIM(KARD2).le.2) then
     if ((ICHAR(KARD2(1:1)).ge.48).and.&
          &(ICHAR(KARD2(1:1)).le.57).and.&
          &(ICHAR(KARD2(2:2)).ge.48).and.&
          &(ICHAR(KARD2(2:2)).le.57)) then
        KARD2 = '0'//KARD2(1:3)
     end if
  end if

  if (CPVREF.ne.'          ') then
     ICL5 = 0
     call PVREF1(KARD2,LPVCOD,LPVREF)
     if ((LPVCOD.and.LPVREF).and.&
          &(KODTYP.lt.10.or.KODTYP.gt.999)) then
        call ERRGRO(.true.,34)
        KKTYPE = 97
        LPVXXX = .true.
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        KKTYPE = 97
        LPVXXX = .true.
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        KKTYPE = 97
        LPVXXX = .true.
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        KKTYPE = 97
        LPVXXX = .true.
     end if
  end if
  if ((KODTYP .lt.10.or. KODTYP .gt.999).and.(.not.LPVXXX)) then
     call ERRGRO (.true.,14)
     KKTYPE = 97
  else

     !  FIND HABITAT CODE IN KOOTENAI HABITAT TYPE TABLE.

     do K = 1,175
        if (KODTYP .lt. KOTHAB(K)) go to 130
        120 continue
     end do
     KKTYPE = 97
     go to 140
     130 continue
     KKTYPE = K-1
     if (KKTYPE .eq.0)KKTYPE = 97
     140 continue
  end if
  KODTYP = KOTHAB(KKTYPE)

  !  NOW FIND THE CORRESPONDING NORTH IDAHO HABITAT TYPE.
  !  IF KODTYP IS OUTSIDE THE ALLOWABLE RANGE, USE DEFAULT VALUE.

  do K = 1,95
     if (KODTYP .lt. JTYPE(K)) go to 110
     100 continue
  end do
  K = 96
  110 continue
  ITYPE = KTYPE(K-1)
  KODTYP = MTYPE(ITYPE)

  if (LSTART)write (JOSTND,300) KOTHAB(KKTYPE)
  300 format(/,T12,'KOOTENAI HABITAT TYPE IS',I4,' FOR THIS PROJECTION.'&
       &)
  if (LSTART)write (JOSTND,200) KODTYP
  200 format(T12,'INLAND EMPIRE HABITAT TYPE IS',I4,' FOR THIS ',&
       &'PROJECTION.')
  return
end
