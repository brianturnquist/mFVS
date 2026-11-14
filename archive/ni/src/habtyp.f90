!=======================================!
!   Handle unused habitat type inputs   !
!=======================================!
subroutine HABTYP (KARD2,ARRAY2)
  implicit none

  !  **HABTYP--NI   DATE OF LAST REVISION:  01/21/11

  !     TRANSLATES HABITAT TYPE  CODE INTO A SUBSCRIPT, ITYPE, AND IF
  !     KODTYP IS ZERO, THE ROUTINE RETURNS THE DEFAULT CODE.

  !     FOR THE COLVILLE (R6), TRANSLATES PLANT ASSOCIATION INTO A
  !     CORRESPONDING NI HABITAT TYPE CODE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: NPA,MTYPE,KTYPE,I,I1,I2,INDEX,NITYPE,K
  parameter (NPA = 40)
  character(len=8) :: PCOML(NPA)
  integer :: MAPR6(NPA)
  logical :: LPVCOD,LPVREF,LPVXXX

  real    :: ARRAY2
  character(len=10) :: KARD2
  dimension KTYPE(95),MTYPE(30)

  data KTYPE/&
       &5*1, 4*2, 4, 3*1, 3,4,5,6,7,8,9,8,8,9,7,3, 3*10, 4,11,20,29,11,&
       &11,13,14,17, 4*12, 3*13, 14,15,14,16,14,16, 3*17, 24,12,24,18,&
       &19,21,19,20,20,21,22,19,23,19,24,27,25,25,26,27,22,24,27,24,&
       &27,28,28,29,28,28, 4*29, 27,9,20,24,21,27,24,30/
  data MTYPE/&
       &130,170,250,260,280,290,310,320,330,420,&
       &470,510,520,530,540,550,570,610,620,640,&
       &660,670,680,690,710,720,730,830,850,999/
  data (PCOML(I),I = 1,NPA)/&
       ! 1-25
       &'CCF221  ', 'CCF222  ', 'CCS311  ', 'CDG131  ', 'CDG311  ',&
       &'CDS632  ', 'CDS633  ', 'CDS637  ', 'CDS715  ', 'CDS716  ',&
       &'CDS813  ', 'CDS814  ', 'CEF111  ', 'CEF211  ', 'CEF421  ',&
       &'CEF422  ', 'CEF423  ', 'CEG311  ', 'CEM211  ', 'CES210  ',&
       &'CES211  ', 'CES312  ', 'CES313  ', 'CES412  ', 'CES422  ',&
       ! 26-40
       &'CHF311  ', 'CHF312  ', 'CHF422  ', 'CHF521  ', 'CHS711  ',&
       &'CLS521  ', 'CWF411  ', 'CWS214  ', 'CWS421  ', 'CWS422  ',&
       &'CWS821  ', 'CCS211  ', 'CHS411  ', 'CAG112  ', 'CDG123  '/
  data MAPR6/&
       &45, 45, 45, 19,  4, 25, 18, 18, 15, 17, 14, 16, 68, 63, 58,&
       &60, 58, 75, 27, 64, 64, 72, 72, 73, 61, 52, 52, 51, 52, 54,&
       &88, 43, 43, 39, 43, 55, 48, 51, 84, 22/
  LPVREF = .false.
  LPVCOD = .false.
  LPVXXX = .false.

  !     IF THE HABITAT CODE IS 6 CHARS LONG, THEN ASSUME THAT IT
  !     IS A PLANT ASSOCIATION CODE AND MAP IT TO THE HABITAT CODES

  I1 = 0
  I2 = 0
  do I = 1,10
     if (KARD2(I:I).ne.' ') then
        if (I1.eq.0) I1 = I
     else
        if (I1.gt.0.and. I2.eq.0) I2 = I-1
     end if
     20 continue
  end do
  if (I2.eq.0.and. I1.gt.0) I2 = 10

  !  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
  !  FVS HABITAT TYPE/ECOCLASS CODE. THEN PROCESS FVS CODE

  if (CPVREF.ne.'          ') then
     call PVREF1(KARD2,ARRAY2,I1,I2,LPVCOD,LPVREF)
     ICL5 = 0
     if ((LPVCOD.and.LPVREF).and.&
          &(KODTYP.lt.10.or.KODTYP.gt.999).and.(KARD2.eq.'          ')) then
        call ERRGRO(.true.,34)
        LPVXXX = .true.
        !          K=96
     else if ((.not.LPVCOD).and.(.not.LPVREF)) then
        call ERRGRO(.true.,33)
        call ERRGRO(.true.,32)
        LPVXXX = .true.
        !          K=96
     else if ((.not.LPVREF).and.LPVCOD) then
        call ERRGRO(.true.,32)
        LPVXXX = .true.
        !          K=96
     else if ((.not.LPVCOD).and.LPVREF) then
        call ERRGRO(.true.,33)
        LPVXXX = .true.
        !          K=96
     end if
  end if

  !  EVALUATE CROSSWALK RESULTS AND WRITE ERROR REPORTS IF NEEDED

  if (I2-I1.eq.5.or.&
       &((IFOR.eq.5.or. IFOR.eq.12).and. KODTYP.le.40)) then
     call HBDECD(KODTYP,PCOML(1),NPA,ARRAY2,KARD2)
     if (KODTYP .le.0) go to 50
     INDEX = KODTYP
     NITYPE = MAPR6(KODTYP)
     KODTYP = JTYPE(NITYPE)
     ICL5 = KODTYP
     if (LSTART)write(JOSTND,30) PCOML(INDEX),KODTYP
     30 format(/,T12,'PLANT COMMUNITY CODE ',A8,&
          &' CONVERTED TO NORTH IDAHO HABITAT TYPE CODE ',I4,'.')
  end if
  50 continue

  !  IF KODTYP IS OUTSIDE THE ALLOWABLE RANGE, USE DEFAULT VALUE.

  if (KODTYP .lt.10.or. KODTYP .gt.999) then
     if (.not.LPVXXX)call ERRGRO (.true.,14)
  else

     !  FIND HABITAT CODE IN HABITAT TYPE TABLE.

     do K = 1,95
        if (KODTYP .lt. JTYPE(K)) go to 110
        100 continue
     end do
     K = 96
     110 continue
     ITYPE = KTYPE(K-1)
  end if
  KODTYP = MTYPE(ITYPE)
  if (LSTART)write (JOSTND,200) KODTYP
  200 format(/,T12,'HABITAT TYPE WILL BE MAPPED TO',I4,&
       &' FOR THIS PROJECTION.')
  return
end
