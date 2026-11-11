!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
! BM $Id$
subroutine HABTYP (KARD2,ARRAY2)
  implicit none

  !     TRANSLATES HABITAT TYPE  CODE INTO A SUBSCRIPT, ITYPE, AND IF
  !     KODTYP IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'

  ! Variables
  logical :: DEBUG,LPVCOD,LPVREF,LPVXXX

  integer :: NPA
  parameter (NPA = 92)

  character(len=8) :: PCOML(NPA)
  character(len=10) :: KARD2

  integer :: I,IHB

  real    :: ARRAY2

  !  DATA STATEMENTS:

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
  !  17=OS, 18=OH

  !  SPECIES EXPANSION:
  !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
  !  LM USES UT LM
  !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
  !  YC USES WC YC
  !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
  !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
  !  OS USES BM PP BARK COEFFICIENT
  !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

  data (PCOML(I),I = 1,75) /&
       ! 1-25
       &'CAG111  ','CAG4    ','CDG111  ','CDG112  ','CDG121  ',&
       &'CDS611  ','CDS622  ','CDS623  ','CDS624  ','CDS634  ',&
       &'CDS711  ','CDS722  ','CDS821  ','CEF221  ','CEF311  ',&
       &'CEF331  ','CEF411  ','CEM111  ','CEM221  ','CEM222  ',&
       &'CEM311  ','CEM312  ','CES131  ','CES221  ','CES311  ',&
       ! 26-50
       &'CES314  ','CES315  ','CES411  ','CES414  ','CES415  ',&
       &'CLF211  ','CLG211  ','CLM112  ','CLM113  ','CLM114  ',&
       &'CLM312  ','CLM313  ','CLM314  ','CLM911  ','CLS411  ',&
       &'CLS415  ','CLS416  ','CLS5    ','CLS511  ','CLS515  ',&
       &'CLS6    ','CMS131  ','CMS231  ','CPG111  ','CPG112  ',&
       ! 51-75
       &'CPG131  ','CPG132  ','CPG221  ','CPG222  ','CPM111  ',&
       &'CPS131  ','CPS221  ','CPS222  ','CPS226  ','CPS232  ',&
       &'CPS233  ','CPS234  ','CPS511  ','CPS522  ','CPS523  ',&
       &'CPS524  ','CPS525  ','CWC811  ','CWC812  ','CWF311  ',&
       &'CWF312  ','CWF421  ','CWF431  ','CWF512  ','CWF611  '/
  data (PCOML(I),I = 76,NPA) /&
       ! 76-92
       &'CWF612  ','CWG111  ','CWG112  ','CWG113  ','CWG211  ',&
       &'CWS211  ','CWS212  ','CWS321  ','CWS322  ','CWS412  ',&
       &'CWS541  ','CWS811  ','CWS812  ','CWS912  ','HQM121  ',&
       &'HQM411  ','HQS221  '/

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
        ITYPE = 79
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        ITYPE = 79
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        ITYPE = 79
        LPVXXX = .true.
        go to 30
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        ITYPE = 79
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
  IHB = INT(ARRAY2)
  if (IHB.gt.0.and. IHB.le.NPA) then
     KODTYP = IHB
     ITYPE = IHB
     PCOM = PCOML(KODTYP)
     go to 40
  else

     !  DEFAULT CONDITIONS --- PA = CWG113

     ITYPE = 79
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
