!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! WC $Id$
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
  parameter (NPA = 139)
  real    :: ARRAY2
  character(len=10) :: KARD2
  character(len=8) :: PCOML(NPA)
  logical :: DEBUG
  logical :: LPVCOD,LPVREF,LPVXXX

  data (PCOML(I),I = 1,75)/&
       ! 1-25
       &'CAF211  ', 'CAF311  ', 'CAG211  ', 'CAG311  ', 'CAG312  ',&
       &'CAS211  ', 'CAS411  ', 'CDC711  ', 'CDC712  ', 'CDC713  ',&
       &'CDS211  ', 'CDS212  ', 'CDS213  ', 'CDS641  ', 'CFC251  ',&
       &'CFC311  ', 'CFF152  ', 'CFF153  ', 'CFF154  ', 'CFF250  ',&
       &'CFF253  ', 'CFF312  ', 'CFF450  ', 'CFM111  ', 'CFS110  ',&
       ! 26-50
       &'CFS151  ', 'CFS152  ', 'CFS154  ', 'CFS216  ', 'CFS221  ',&
       &'CFS222  ', 'CFS223  ', 'CFS224  ', 'CFS225  ', 'CFS226  ',&
       &'CFS229  ', 'CFS230  ', 'CFS231  ', 'CFS251  ', 'CFS252  ',&
       &'CFS253  ', 'CFS254  ', 'CFS255  ', 'CFS256  ', 'CFS257  ',&
       &'CFS258  ', 'CFS259  ', 'CFS260  ', 'CFS351  ', 'CFS352  ',&
       ! 51-75
       &'CFS550  ', 'CFS551  ', 'CFS552  ', 'CFS554  ', 'CFS555  ',&
       &'CFS651  ', 'CFS652  ', 'CFS653  ', 'CFS654  ', 'CHC212  ',&
       &'CHC213  ', 'CHF111  ', 'CHF123  ', 'CHF124  ', 'CHF125  ',&
       &'CHF133  ', 'CHF134  ', 'CHF135  ', 'CHF151  ', 'CHF221  ',&
       &'CHF222  ', 'CHF250  ', 'CHF321  ', 'CHF421  ', 'CHM121  '/
  data (PCOML(I),I = 76,139)/&
       ! 76-100
       &'CHS111  ', 'CHS113  ', 'CHS114  ', 'CHS124  ', 'CHS125  ',&
       &'CHS126  ', 'CHS127  ', 'CHS128  ', 'CHS129  ', 'CHS130  ',&
       &'CHS135  ', 'CHS140  ', 'CHS141  ', 'CHS223  ', 'CHS224  ',&
       &'CHS251  ', 'CHS325  ', 'CHS326  ', 'CHS327  ', 'CHS328  ',&
       &'CHS351  ', 'CHS352  ', 'CHS353  ', 'CHS354  ', 'CHS355  ',&
       ! 101-125
       &'CHS511  ', 'CHS513  ', 'CHS522  ', 'CHS523  ', 'CHS524  ',&
       &'CHS611  ', 'CHS612  ', 'CHS613  ', 'CHS614  ', 'CHS615  ',&
       &'CHS625  ', 'CHS626  ', 'CMF250  ', 'CMF251  ', 'CMS114  ',&
       &'CMS210  ', 'CMS216  ', 'CMS218  ', 'CMS221  ', 'CMS223  ',&
       &'CMS241  ', 'CMS244  ', 'CMS245  ', 'CMS246  ', 'CMS250  ',&
       ! 126-139
       &'CMS251  ', 'CMS252  ', 'CMS253  ', 'CMS254  ', 'CMS255  ',&
       &'CMS350  ', 'CMS351  ', 'CMS352  ', 'CMS353  ', 'CMS450  ',&
       &'CMS612  ', 'CWF211  ', 'CWS521  ', 'CWS522  '/

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
     ICL5 = 0
     if (DEBUG)write(JOSTND,*)'AFTER PVREF KARD2,ARRAY2= ',&
          &KARD2,ARRAY2
     if ((LPVCOD.and.LPVREF).and.&
          &(KARD2.eq.'          ')) then
        call ERRGRO(.true.,34)
        ITYPE = 52
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        ITYPE = 52
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        ITYPE = 52
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        ITYPE = 52
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

     !  DEFAULT CONDITIONS --- PA = CFS551

     ITYPE = 52
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
