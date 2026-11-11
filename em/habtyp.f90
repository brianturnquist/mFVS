!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! EM $Id$
subroutine HABTYP (KARD2,ARRAY2)
  implicit none

  !     TRANSLATES HABITAT-TYPE CODE INTO A SUBSCRIPT, IEMTYP, AND IF
  !     KODTYP IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.
  !     MAPS HABITAT-TYPE SUBSCRIPT INTO A SUBSCRIPT (ITYPE) REPRESENTING
  !     ONE OF THE ORIGINAL 30 NI-VARIANT HABITAT TYPES

  !     IEMTYP - SUBSCRIPT USED IN **SITSET
  !     ITYPE  - SUBSCRIPT USED IN **ESTAB

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'EMCOM.f90'

  ! Variables
  real    :: ARRAY2
  character(len=10) :: KARD2
  integer :: NIHMAP(118),J
  logical :: LPVCOD,LPVREF,LPVXXX
  real    :: RDANUW

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  !  DATA STATEMENTS

  data NIHMAP/&
       &1, 1, 1, 1, 1, 1, 1, 1, 1, 1,&
       &1, 1, 1, 1, 1, 2, 2, 2, 2, 2,&
       &2, 2, 4, 1, 1, 1, 1, 3, 4, 4,&
       &4, 5, 5, 5, 5, 6, 6, 6, 6, 7,&
       &7, 7, 7, 7, 8, 8, 8, 8, 9, 9,&
       &9, 8, 8, 9, 7, 7,10,10, 4,11,&
       &20,29,29,11,11,12,18,19,19,19,&
       &21,21,20,20,20,20,20,20,20,20,&
       &21,21,21,21,22,22,24,24,24,27,&
       &25,26,27,27,27,27,22,24,24,27,&
       &24,27,27,27,28,29,28,28,29,29,&
       &29,27, 9,20,21,27,24,30/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = ARRAY2

  LPVREF = .false.
  LPVCOD = .false.
  LPVXXX = .false.

  !  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
  !  FVS HABITAT TYPE/ECOCLASS CODE IN THE KODTYP VARIABLE.
  !  THEN PROCESS KODTYP CODE (KODTYP IS PASSED BACK TO HABTYP
  !  THROUGH COMMON)

  !  The below ADJSUTL is used to add a leading zero to KODTYPs
  !  (PV Code) that are two or less digits. For example, 18
  !  would become 018
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
     call PVREF1(KARD2,LPVCOD,LPVREF)
     if ((LPVCOD.and.LPVREF).and.&
          &(KODTYP.le.0)) then
        call ERRGRO(.true.,34)
        LPVXXX = .true.
        goto 21
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        LPVXXX = .true.
        goto 21
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        LPVXXX = .true.
        goto 21
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        LPVXXX = .true.
        goto 21
     end if
  end if

  if (KODTYP .eq.0) goto 21
  if (KODTYP.eq.0.or. KODTYP.lt.JTYPE(1)) goto 21

  do J = 1,118
     if (KODTYP .lt. JTYPE(J)) go to 100
     20 continue
  end do
  21 continue
  if (.not.LPVXXX)call ERRGRO(.true.,14)
  IEMTYP = ITYPE
  KODTYP = JTYPE(IEMTYP)
  ITYPE = NIHMAP(IEMTYP)
  if (LSTART)write(JOSTND,23) KODTYP
  23 format(/,T12,'HABITAT TYPE CODE USED IN THIS PROJECTION IS',I4)
  return
  100 continue
  IEMTYP = J-1
  if (IEMTYP.lt.1)IEMTYP = 1
  KODTYP = JTYPE(IEMTYP)
  ITYPE = NIHMAP(IEMTYP)
  if (LSTART)write(JOSTND,30)KODTYP
  30 format(/,T12,'HABITAT TYPE MAPPED TO',I4,' FOR THIS PROJECTION.')
  return
end
