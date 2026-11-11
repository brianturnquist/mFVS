!=========================================!
!   Compute stand mean annual increment   !
!=========================================!
! CR $Id$
subroutine MAICAL
  implicit none

  !  THIS SUBROUTINE CALCULATES THE MAI FOR THE STAND. IT IS CALLED
  !  FROM CRATET.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'HTCAL.f90'

  ! Variables
  logical :: DEBUG
  real    :: ADJMAI,SSSI
  integer :: ISPNUM(11),IMAP,ISICD,IERR
  integer :: MAP1(MAXSP),MAP2(MAXSP),MAP3(MAXSP),MAP4(MAXSP),MAP5(MAXSP)

  !  SPECIES ORDER:
  !   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
  !  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
  !  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
  !  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH

  !  SPECIES EXPANSION:
  !  UJ,AJ,RM,OJ,ER USE CR JU
  !  NC,PW USE CR CO
  !  GO,AW,EM,BK,SO USE CR OA
  !  PB USES CR AS
  !  PM,PD,AZ USE CR PI
  !  CI USES CR PP

  data MAP1/&
       &9,  9,  3,  9,  4,  2,  2,  2,  1,  1,&
       &2, 11, 10,  1,  1, 11,  7,  8,  8,  6,&
       &5,  5,  5,  5,  5,  5,  5,  6, 11, 11,&
       &11, 11, 11, 11, 11, 10,  2,  5/
  data MAP2/&
       &4,  4,  3,  4,  4,  2,  2,  2,  1,  1,&
       &2,  9, 10,  1,  1, 11,  7,  7,  7,  6,&
       &5,  5,  8,  8,  8,  8,  8,  6, 11, 11,&
       &11, 11,  9,  9,  9, 10,  2,  5/
  data MAP3/&
       &2,  2,  3,  2,  2,  2,  2,  2,  2,  2,&
       &7,  2, 10,  2,  2, 11,  8,  8,  8,  6,&
       &1,  1,  5,  5,  5,  5,  5,  6, 11, 11,&
       &11, 11,  2,  2,  2, 10,  2,  5/
  data MAP4/&
       &9,  9,  3,  9,  9,  2,  2,  2,  2,  2,&
       &7,  2,  2,  2,  2,  2,  8,  8,  8,  6,&
       &5,  5,  5,  5,  5,  5,  5,  6,  2,  2,&
       &2,  2,  2,  2,  2,  2,  2,  5/
  data MAP5/&
       &9,  9,  3,  9,  9,  2,  2,  2,  2,  2,&
       &7,  2, 10,  2,  2,  2,  8,  8,  8,  6,&
       &5,  5,  5,  5,  5,  5,  5,  6,  2,  2,&
       &2,  2,  2,  2,  2, 10,  2,  5/

  !  INITIALIZE INTERNAL VARIABLES:

  data ISPNUM/101,101,202,015,101,101,108,093,019,122,101/

  !     THE SPECIES ORDER IS AS FOLLOWS:
  !     1 = WHITE BARK PINE (WB)
  !     2 = LIMBER PINE (LM) USE WB
  !     3 = DOUGLAS-FIR (DF)
  !     4 = WHITE FIR (WF)
  !     5 = DUMMY
  !     6 = ASPEN (AS) USE WB
  !     7 = LODGEPOLE PINE (LP)
  !     8 = ENGLEMAN SPRUCE (ES)
  !     9 = SUBALPINE FIR (AF)
  !    10 = PONDEROSA PINE (PP)
  !    11 = OTHER ()

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'MAICAL',6,ICYC)

  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE MAICAL  CYCLE =',I5)

  if (ISISP .eq.0) ISISP = 3
  SSSI = SITEAR(ISISP)
  if (SSSI .eq.0.) SSSI = 140.0

  !  SET UP MAPPING INDEX BY  MODEL TYPE.

  if (IMODTY .eq.1) then
     IMAP = MAP1(ISISP)
  else if (IMODTY .eq.2) then
     IMAP = MAP2(ISISP)
  else if (IMODTY .eq.3) then
     IMAP = MAP3(ISISP)
  else if (IMODTY .eq.4) then
     IMAP = MAP4(ISISP)
  else
     IMAP = MAP5(ISISP)
  end if

  !   RMAI IS FUNCTION TO CALCULATE ADJUSTED MAI.

  ISICD = ISPNUM(IMAP)
  RMAI = ADJMAI(ISICD,SSSI,10.0,IERR)
  if (RMAI .gt.128.0)RMAI = 128.0
  return
end
