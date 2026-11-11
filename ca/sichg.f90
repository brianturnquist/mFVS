!================!
!   ca : sichg   !
!================!
! CA $Id$
subroutine SICHG(ISISP,SSITE,SIAGE)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'

  ! THIS ROUTINE TAKES THE SITE INDEX OF THE SITE SPECIES AND CALCULATES
  ! AN AGE FOR THAT CURVE, GIVEN THE SITE, WHERE A NEW HT (OR PROXY AGE)
  ! MAY BE LOOKED UP.

  !   SITE CURVES ARE LOADED (BLKDAT BB_ COEFFS) IN THE FOLLOWING ORDER:
  !   (ALL CURVES ARE BASE AGE 50)
  !       FIRST   POSITION ARE KING DOUGLAS-FIR; WEYERH FOR PAP 8
  !       SECOND  POSITION ARE DOLPH WHITE FIR PSW 185;
  !       THIRD   POSITION ARE HANN-SCRIVANI DOUGLAS-FIR
  !       FOURTH  POSITION ARE HANN-SCRIVANI PONDEROSA PINE
  !       FIFTH   POSITION ARE DOLPH RED FIR PSW 206
  !       SIXTH   POSITION ARE DAHMS LODGEPOLE RP-PNW-8
  !       SEVENTH POSITION ARE POWERS BLACK OAK RES NOTE PSW-262
  !       EIGHTH  POSITION ARE PORTER & WIANT TANOAK JOF 4/95 286-287
  !       NINTH   POSITION ARE PORTER & WIANT MADRONE  (DITTO)
  !       TENTH   POSITION ARE PORTER & WIANT RED ALDER  (DITTO)

  !   INDEX = ARRAY HOLDING THE INDEX FOR ACCESSING THE APPROPRIATE SITE
  !           CURVE COEFFICIENTS IN THE BB_ ARRAYS.

  ! Variables
  integer :: DIFF,REFAGE(10)
  character(len=1) :: ISILOC
  character(len=1) :: REFLOC(10)
  real    :: SIAGE(MAXSP)
  integer :: INDEX(MAXSP)
  real    :: SSITE,AGE2BH
  integer :: ISISP,INDX,I,JNDX

  ! SPECIES ORDER IN CA VARIANT:
  !  1=PC  2=IC  3=RC  4=WF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
  ! 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
  ! 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
  ! 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=FL
  ! 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH 50=RW

  data INDEX/ 3,  3,  3,  3,  5,  5,  3,  3,  5,  6,&
       &6,  6,  6,  6,  4,  3,  4,  4,  4,  4,&
       &6,  3,  3,  7,  3,  9,  9,  9,  9,  7,&
       &7,  7,  9, 10,  7, 10,  9,  9,  7,  7,&
       &10,  8, 10, 10, 10, 10, 10, 10, 10,  3/

  data REFLOC/'B','B','B','B','B','T','B','T','T','T'/
  data REFAGE/ 50, 50, 50, 50, 50, 50, 50, 50, 50, 50/

  ! DETERMINE AGE TO BREAST HEIGHT

  INDX = INDEX(ISISP)
  go to (10,20,30,40,50,60,70,80,90,100),INDX

  ! KING DF

  10 continue
  if (SSITE .gt.135.) then
     AGE2BH = 6.
  else if (SSITE.gt.115.and. SSITE.le.135.) then
     AGE2BH = 7.
  else if (SSITE.gt.95.and. SSITE.le.115) then
     AGE2BH = 8.
  else if (SSITE.gt.75.and. SSITE.le.95) then
     AGE2BH = 9.
  else
     AGE2BH = 10.
  end if
  go to 110

  ! DOLPH WHITE FIR

  20 continue
  if (SSITE .gt.100.) then
     AGE2BH = 5.
  else if (SSITE.gt.60.and. SSITE.le.100.) then
     AGE2BH = 7.
  else
     AGE2BH = 10.
  end if
  go to 110

  ! HANN-SCRIVANI DOUGLAS-FIR

  30 continue
  AGE2BH = 16.0 - 0.1*SSITE
  if (AGE2BH .lt.1.) AGE2BH = 1.
  go to 110

  ! HANN-SCRIVANI PONDEROSA PINE

  40 continue
  AGE2BH = 10.4 - 0.06*SSITE
  if (AGE2BH .lt.1.) AGE2BH = 1.
  go to 110

  ! DOLPH RED FIR

  50 continue
  AGE2BH = 18.
  go to 110

  ! DAHMS LODGEPOLE PINE

  60 continue
  AGE2BH = 10.
  go to 110

  ! POWERS BLACK OAK

  70 continue
  AGE2BH = 2.
  go to 110

  ! PORTER & WIANT TANOAK

  80 continue
  AGE2BH = 10.820513 - 0.102564*SSITE
  if (AGE2BH .lt.1.) AGE2BH = 1.
  if (AGE2BH .gt.7.) AGE2BH = 7.
  go to 110

  ! PORTER & WIANT MADRONE

  90 continue
  AGE2BH = 12.309524 - 0.119048*SSITE
  if (AGE2BH .lt.1.) AGE2BH = 1.
  if (AGE2BH .gt.7.) AGE2BH = 7.
  go to 110

  ! PORTER & WIANT RED ALDER

  100 continue
  AGE2BH = 4.0 - 0.026316*SSITE
  if (AGE2BH .lt.1.) AGE2BH = 1.
  if (AGE2BH .gt.3.) AGE2BH = 3.

  110 continue

  ! ISILOC IS THE PLACE THE AGE FOR THE SITE FROM STDINFO IS TAKEN

  ISILOC = REFLOC(INDX)

  !  SET UP THE ARRAY TO TELL WHETHER YOU NEED TO SLIDE UP OR DOWN THE SIT
  ! LINE TO ADJUST FOR TOTAL AGE OR BREAST HIGH AGE

  do I = 1,MAXSP
     JNDX = INDEX(I)
     if (ISILOC .eq. 'T' .and. REFLOC(JNDX) .eq. 'B')DIFF = -1
     if (ISILOC .eq. REFLOC(JNDX))DIFF = 0
     if (ISILOC .eq. 'B' .and. REFLOC(JNDX) .eq. 'T')DIFF = 1
     SIAGE(I) = REFAGE(JNDX) + AGE2BH*DIFF
     120 continue
  end do
  return
end
