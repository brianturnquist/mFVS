!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! PN $Id$
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
  integer :: NPA,IHB
  parameter (NPA = 75)
  real    :: ARRAY2
  character(len=10) :: KARD2
  character(len=8) :: PCOML(NPA)
  logical :: DEBUG
  logical :: LPVCOD,LPVREF,LPVXXX

  data PCOML/&
       ! 1-25
       &'CDS221  ', 'CDS255  ', 'CDS651  ', 'CEF321  ', 'CES212  ',&
       &'CES321  ', 'CES621  ', 'CFF111  ', 'CFF211  ', 'CFF311  ',&
       &'CFF611  ', 'CFF612  ', 'CFF911  ', 'CFS156  ', 'CFS211  ',&
       &'CFS212  ', 'CFS213  ', 'CFS214  ', 'CFS215  ', 'CFS217  ',&
       &'CFS218  ', 'CFS219  ', 'CFS311  ', 'CFS611  ', 'CFS612  ',&
       ! 26-50
       &'CHF112  ', 'CHF121  ', 'CHF122  ', 'CHF131  ', 'CHF132  ',&
       &'CHF211  ', 'CHF511  ', 'CHF911  ', 'CHM111  ', 'CHS121  ',&
       &'CHS122  ', 'CHS123  ', 'CHS131  ', 'CHS132  ', 'CHS133  ',&
       &'CHS134  ', 'CHS136  ', 'CHS137  ', 'CHS138  ', 'CHS139  ',&
       &'CHS221  ', 'CHS222  ', 'CHS321  ', 'CHS322  ', 'CHS323  ',&
       ! 51-75
       &'CHS324  ', 'CHS331  ', 'CHS332  ', 'CHS333  ', 'CHS334  ',&
       &'CHS335  ', 'CHS421  ', 'CHS422  ', 'CHS423  ', 'CHS512  ',&
       &'CHS521  ', 'CHS610  ', 'CHS621  ', 'CHS622  ', 'CHS623  ',&
       &'CHS624  ', 'CMS242  ', 'CSF111  ', 'CSF121  ', 'CSF321  ',&
       &'CSS221  ', 'CSS321  ', 'CSS521  ', 'CSS522  ', 'CSS621  '/

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
     call PVREF6(KARD2,ARRAY2,LPVCOD,LPVREF)
     ICL5 = 0
     if (DEBUG)write(JOSTND,*)'AFTER PVREF KARD2,ARRAY2= ',&
          &KARD2,ARRAY2
     if ((LPVCOD.and.LPVREF).and.&
          &(KARD2.eq.'          ')) then
        call ERRGRO(.true.,34)
        ITYPE = 40
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        ITYPE = 40
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        ITYPE = 40
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        ITYPE = 40
        LPVXXX = .true.
        go to 30
     end if
  end if

  !  DECODE HABITAT TYPE/PLANT ASSOCIATION FIELD.

  call HBDECD (KODTYP,PCOML(1),NPA,ARRAY2,KARD2)
  if (DEBUG)write(JOSTND,*)'AFTER HAB DECODE,KODTYP= ',KODTYP
  if (KODTYP .le.0) go to 20

  PCOM = PCOML(KODTYP)
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
     PCOM = PCOML(KODTYP)
     go to 40
  else

     !  DEFAULT CONDITIONS --- PA = CHS133

     ITYPE = 40
     go to 30
  end if

  30 continue
  if (.not.LPVXXX)call ERRGRO(.true.,14)
  KODTYP = ITYPE
  PCOM = PCOML(KODTYP)
  if (LSTART)write(JOSTND,10) PCOM

  40 continue
  ICL5 = KODTYP
  KARD2 = PCOM

  if (DEBUG)write(JOSTND,*)'LEAVING HABTYP KODTYP,ITYPE,ICL5,KARD2',&
       &' PCOM =',KODTYP,ITYPE,ICL5,KARD2,PCOM

  return
end
