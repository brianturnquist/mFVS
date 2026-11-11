!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! EC $Id$
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
  integer :: NPA
  parameter (NPA = 155)
  logical :: DEBUG
  logical :: LPVCOD,LPVREF,LPVXXX
  character(len=10) :: KARD2
  character(len=8) :: PCOML(NPA)
  integer :: IHB,I
  real    :: ARRAY2

  data (PCOML(I),I = 1,75)/&
       ! 1-25
       &'CAG112  ', 'CAS311  ', 'CCF211  ', 'CCF212  ', 'CCF221  ',&
       &'CCF222  ', 'CCS211  ', 'CCS311  ', 'CDF411  ', 'CDG123  ',&
       &'CDG131  ', 'CDG132  ', 'CDG134  ', 'CDG141  ', 'CDG311  ',&
       &'CDG321  ', 'CDG322  ', 'CDG323  ', 'CDS231  ', 'CDS241  ',&
       &'CDS411  ', 'CDS412  ', 'CDS631  ', 'CDS632  ', 'CDS633  ',&
       ! 26-50
       &'CDS636  ', 'CDS637  ', 'CDS638  ', 'CDS639  ', 'CDS640  ',&
       &'CDS653  ', 'CDS654  ', 'CDS655  ', 'CDS661  ', 'CDS662  ',&
       &'CDS673  ', 'CDS674  ', 'CDS675  ', 'CDS715  ', 'CDS716  ',&
       &'CDS811  ', 'CDS813  ', 'CDS814  ', 'CDS831  ', 'CDS832  ',&
       &'CDS833  ', 'CEF111  ', 'CEF211  ', 'CEF222  ', 'CEF421  ',&
       ! 51-75
       &'CEF422  ', 'CEF423  ', 'CEF424  ', 'CEG121  ', 'CEG310  ',&
       &'CEG311  ', 'CEM211  ', 'CES111  ', 'CES113  ', 'CES210  ',&
       &'CES211  ', 'CES213  ', 'CES312  ', 'CES313  ', 'CES342  ',&
       &'CES412  ', 'CES413  ', 'CES422  ', 'CES423  ', 'CES424  ',&
       &'CES425  ', 'CES426  ', 'CFF162  ', 'CFF254  ', 'CFS232  '/
  data (PCOML(I),I = 76,150)/&
       ! 76-100
       &'CFS233  ', 'CFS234  ', 'CFS542  ', 'CFS553  ', 'CFS556  ',&
       &'CFS558  ', 'CFS621  ', 'CHC311  ', 'CHF223  ', 'CHF311  ',&
       &'CHF312  ', 'CHF313  ', 'CHF422  ', 'CHF521  ', 'CHS142  ',&
       &'CHS143  ', 'CHS144  ', 'CHS225  ', 'CHS226  ', 'CHS227  ',&
       &'CHS411  ', 'CHS711  ', 'CLS521  ', 'CMF131  ', 'CMG221  ',&
       ! 101-125
       &'CMS121  ', 'CMS122  ', 'CMS256  ', 'CMS257  ', 'CMS258  ',&
       &'CMS259  ', 'CMS354  ', 'CMS355  ', 'CMS356  ', 'CPG141  ',&
       &'CPG231  ', 'CPH211  ', 'CPH212  ', 'CPS241  ', 'CWC511  ',&
       &'CWF321  ', 'CWF444  ', 'CWF521  ', 'CWF522  ', 'CWF523  ',&
       &'CWF524  ', 'CWG121  ', 'CWG122  ', 'CWG123  ', 'CWG124  ',&
       ! 126-150
       &'CWG125  ', 'CWS214  ', 'CWS221  ', 'CWS222  ', 'CWS223  ',&
       &'CWS224  ', 'CWS225  ', 'CWS226  ', 'CWS331  ', 'CWS332  ',&
       &'CWS335  ', 'CWS336  ', 'CWS337  ', 'CWS338  ', 'CWS421  ',&
       &'CWS422  ', 'CWS531  ', 'CWS532  ', 'CWS533  ', 'CWS534  ',&
       &'CWS535  ', 'CWS536  ', 'CWS537  ', 'CWS551  ', 'CWS552  '/
  data (PCOML(I),I = 151,155)/&
       ! 151-155
       &'CWS553  ', 'CWS554  ', 'CWS821  ', 'HQG111  ', 'HQS211  '/

  LPVREF = .false.
  LPVCOD = .false.
  LPVXXX = .false.

  !  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
  !  FVS HABITAT TYPE/ECOCLASS CODE. THEN PROCESS FVS CODE

  if (CPVREF.ne.'          ') then
     call PVREF6(KARD2,ARRAY2,LPVCOD,LPVREF)
     ICL5 = 0
     if ((LPVCOD.and.LPVREF).and.&
          &(KARD2.eq.'          ')) then
        call ERRGRO(.true.,34)
        ITYPE = 114
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        ITYPE = 114
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        ITYPE = 114
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        ITYPE = 114
        LPVXXX = .true.
        go to 30
     end if
  end if

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'HABTYP',6,ICYC)
  if (DEBUG) write(JOSTND,*)&
       &'ENTERING HABTYP CYCLE,KODTYP,KODFOR,KARD2,ARRAY2= ',&
       &ICYC,KODTYP,KODFOR,KARD2,ARRAY2

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

     !  DEFAULT CONDITIONS --- PA = CPS241   SITE SPECIES=PP   SITE INDEX=75

     ITYPE = 114
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
