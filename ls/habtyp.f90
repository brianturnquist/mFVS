!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! LS $Id$
subroutine HABTYP (KARD2,ARRAY2)
  implicit none

  !     TRANSLATES HABITAT TYPE  CODE INTO A SUBSCRIPT, ITYPE, AND IF
  !     KODTYP IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'VARCOM.f90'

  ! Variables
  integer :: NPA,I,IHB
  parameter (NPA = 63)
  real    :: ARRAY2
  character(len=10) :: KARD2
  character(len=8) :: LSNPC(NPA)
  logical :: DEBUG
  logical :: LPVCOD,LPVREF,LPVXXX

  data (LSNPC(I),I = 1,63)/&
       &'FDN12   ','FDN22   ','FDN32   ','FDN33   ','FDN43   ',&
       &'FDC12   ','FDC23   ','FDC24   ','FDC25   ','FDC34   ',&
       &'MHN35   ','MHN44   ','MHN45   ','MHN46   ','MHN47   ',&
       &'MHC26   ','MHC36   ','MHC37   ','MHC47   ','FFN57   ',&
       &'FFN67   ','WFN53   ','WFN55   ','WFN64   ','WFS57   ',&
       &'WFW54   ','FPN62   ','FPN63   ','FPN71   ','FPN72   ',&
       &'FPN73   ','FPN81   ','FPN82   ','FPS63   ','FPW63   ',&
       &'APN80   ','APN81   ','APN90   ','APN91   ','CTN11   ',&
       &'CTN12   ','CTN24   ','CTN32   ','CTN42   ','CTU22   ',&
       &'RON12   ','RON23   ','LKI32   ','LKI43   ','LKI54   ',&
       &'LKU32   ','LKU43   ','RVX32   ','RVX43   ','RVX54   ',&
       &'OPN81   ','OPN91   ','OPN92   ','OPN93   ','WMN82   ',&
       &'MRN83   ','MRN93   ','MRU94   '/

  LPVREF = .false.
  LPVCOD = .false.
  LPVXXX = .false.

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'HABTYP',6,ICYC)
  if (DEBUG) write(JOSTND,*)&
       &'ENTERING HABTYP CYCLE,KODTYP,KODFOR,KARD2,ARRAY2= ',&
       &ICYC,KODTYP,KODFOR,KARD2,ARRAY2

  !  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
  !  FVS HABITAT TYPE/ECOCLASS CODE. THEN PROCESS FVS CODE

  if (CPVREF.ne.'          ') then
     call PVREF9(KARD2,ARRAY2,LPVCOD,LPVREF)
     ICL5 = 0
     if (DEBUG)write(JOSTND,*)'AFTER PVREF KARD2,ARRAY2= ',&
          &KARD2,ARRAY2
     if ((LPVCOD.and.LPVREF).and.&
          &(KARD2.eq.'          ')) then
        call ERRGRO(.true.,34)
        ITYPE = 1
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        ITYPE = 1
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        ITYPE = 1
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        ITYPE = 1
        LPVXXX = .true.
        go to 30
     end if
  end if

  !  DECODE HABITAT TYPE/PLANT ASSOCIATION FIELD.

  call HBDECD (KODTYP,LSNPC(1),NPA,ARRAY2,KARD2)
  if (DEBUG)write(JOSTND,*)'AFTER HAB DECODE,KODTYP= ',KODTYP
  if (KODTYP .le.0) go to 20

  PCOM = LSNPC(KODTYP)
  ITYPE = KODTYP
  if (LSTART)write(JOSTND,10) PCOM
  10 format(/,T12,'PLANT ASSOCIATION CODE USED IN THIS',&
       &' PROJECTION IS ',A8)
  go to 40

  !  NO MATCH WAS FOUND, TREAT IT AS A SEQUENCE NUMBER.

  20 continue
  if (DEBUG)write(JOSTND,*)'EXAMINING FOR INDEX, ARRAY2= ',ARRAY2
  IHB = IFIX(ARRAY2)
  if (IHB.gt.0.and. IHB.le.NPA) then
     KODTYP = IHB
     ITYPE = IHB
     PCOM = LSNPC(KODTYP)
     go to 40
  else

     !  DEFAULT CONDITIONS --- PLANT COMMUNITY = FDn12

     ITYPE = 1
     go to 30
  end if

  30 continue
  if (.not.LPVXXX)call ERRGRO(.true.,14)
  KODTYP = ITYPE
  PCOM = LSNPC(KODTYP)
  if (LSTART)write(JOSTND,10) PCOM

  40 continue
  ICL5 = KODTYP
  KARD2 = PCOM

  if (DEBUG)write(JOSTND,*)'LEAVING HABTYP KODTYP,ITYPE,ICL5,KARD2',&
       &' PCOM =',KODTYP,ITYPE,ICL5,KARD2,PCOM

  return
end
