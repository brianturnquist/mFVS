!=======================================================!
!   Compute individual tree crown competition factors   !
!=======================================================!
! CR $Id$
subroutine CCFCAL(ISPC,D,H,JCR,P,LTHIN,CCFT,CRWDTH,MODE)
  implicit none

  !  THIS ROUTINE COMPUTES CROWN WIDTH AND CCF FOR INDIVIDUAL TREES.
  !  CALLED FROM DENSE, PRTRLS, SSTAGE, AND CVCW.

  !  ARGUMENT DEFINITIONS:
  !    ISPC = NUMERIC SPECIES CODE
  !       D = DIAMETER AT BREAST HEIGHT
  !       H = TOTAL TREE HEIGHT
  !     JCR = CROWN RATIO IN PERCENT (0-100)
  !       P = TREES PER ACRE
  !   LTHIN = .TRUE. IF THINNING HAS JUST OCCURRED
  !         = .FALSE. OTHERWISE
  !    CCFT = CCF REPRESENTED BY THIS TREE
  !  CRWDTH = CROWN WIDTH OF THIS TREE
  !    MODE = 1 IF ONLY NEED CCF RETURNED
  !           2 IF ONLY NEED CRWDTH RETURNED

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'

  !  DIMENSION AND DATA STATEMENTS FOR INTERNAL VARIABLES.

  !     CCF COEFFICIENTS FOR TREES THAT ARE GREATER THAN 10.0 IN. DBH:
  !      RD1 -- CONSTANT TERM IN CROWN COMPETITION FACTOR EQUATION,
  !             SUBSCRIPTED BY SPECIES
  !      RD2 -- COEFFICIENT FOR SUM OF DIAMETERS TERM IN CROWN
  !             COMPETITION FACTOR EQUATION,SUBSCRIPTED BY SPECIES
  !      RD3 -- COEFFICIENT FOR SUM OF DIAMETER SQUARED TERM IN
  !             CROWN COMPETITION EQUATION, SUBSCRIPTED BY SPECIES

  !     CCF COEFFICIENTS FOR TREES THAT ARE LESS THAN 10.0 IN. DBH:
  !      RDA -- MULTIPLIER.
  !      RDB -- EXPONENT.  CCF(I) = RDA*DBH**RDB

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

  !  CCF EQUATIONS ORDER:
  !     1 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: LODGEPOLE PINE
  !     2 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: DOUGLAS-FIR
  !     3 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: GRAND FIR
  !     4 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN HEMLOCK
  !     5 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: WESTERN RED CEDAR
  !     6 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: ENGELMANN SPRUCE
  !     7 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: SUBALPINE FIR
  !     8 = NORTHERN IDAHO VARIANT INT-133 TABLE 8: PONDEROSA PINE

  !      WYKOFF, CROOKSTON, STAGE, 1982. USER'S GUIDE TO THE STAND
  !        PROGNOSIS MODEL. GEN TECH REP INT-133. OGDEN, UT:
  !        INTERMOUNTAIN FOREST AND RANGE EXP STN. 112P.

  ! Variables
  logical :: LTHIN
  real    :: RD1(8),RD2(8),RD3(8),RDA(8),RDB(8)
  integer :: MAP1(MAXSP),MAP2(MAXSP),MAP3(MAXSP),MAP4(MAXSP)
  integer :: MAP5(MAXSP),MODE,ISPC,IMAP,JCR
  real    :: CRWDTH,CCFT,P,H,D
  real    :: RDANUW
  integer :: IDANUW
  logical :: LDANUW

  data MAP1/&
       &7,  7,  2,  7,  3,  1,  1,  1,  1,  1,&
       &1,  1,  8,  1,  1,  1,  1,  6,  6,  5,&
       &4,  4,  4,  4,  4,  4,  4,  5,  1,  1,&
       &1,  1,  1,  1,  1,  8,  1,  4/
  data MAP2/&
       &3,  3,  2,  3,  3,  1,  1,  1,  1,  1,&
       &1,  7,  8,  1,  1,  1,  1,  1,  1,  5,&
       &4,  4,  6,  6,  6,  6,  6,  5,  1,  1,&
       &1,  1,  7,  7,  7,  8,  1,  4/
  data MAP3/&
       &1,  1,  2,  1,  1,  1,  1,  1,  1,  1,&
       &1,  1,  8,  1,  1,  1,  6,  6,  6,  5,&
       &1,  1,  4,  4,  4,  4,  4,  5,  1,  1,&
       &1,  1,  1,  1,  1,  8,  1,  4/
  data MAP4/&
       &7,  7,  2,  7,  7,  1,  1,  1,  1,  1,&
       &1,  1,  1,  1,  1,  1,  6,  6,  6,  5,&
       &4,  4,  4,  4,  4,  4,  4,  5,  1,  1,&
       &1,  1,  1,  1,  1,  1,  1,  4/
  data MAP5/&
       &7,  7,  2,  7,  7,  1,  1,  1,  1,  1,&
       &1,  1,  8,  1,  1,  1,  6,  6,  6,  5,&
       &4,  4,  4,  4,  4,  4,  4,  5,  1,  1,&
       &1,  1,  1,  1,  1,  8,  1,  4/

  data RD1 /.01925, .11, .04, .03, .03, .03, .03, .03/
  data RD2 /.01676, .0333, .0270, .0215, .0238, .0173, .0216, .0180/
  data RD3 /0.00365, 0.00259, 0.00405, 0.00363,&
       &0.00490, 0.00259, 0.00405, 0.00281/
  data RDA/ 0.009187, 0.017299, 0.015248, 0.011109,&
       &0.008915, 0.007875, 0.011402, 0.007813/
  data RDB/ 1.7600, 1.5571,  1.7333,  1.7250,&
       &1.7800, 1.7360,  1.7560,  1.7680/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = JCR
  IDANUW = MODE
  RDANUW = H
  LDANUW = LTHIN

  !  INITIALIZE RETURN VARIABLES.

  CCFT = 0.
  CRWDTH = 0.

  !  SET UP MAPPING INDEX BY  MODEL TYPE.

  if (IMODTY .eq.1) then
     IMAP = MAP1(ISPC)
  else if (IMODTY .eq.2) then
     IMAP = MAP2(ISPC)
  else if (IMODTY .eq.3) then
     IMAP = MAP3(ISPC)
  else if (IMODTY .eq.4) then
     IMAP = MAP4(ISPC)
  else
     IMAP = MAP5(ISPC)
  end if

  !  COMPUTE CCF

  if (D .ge.10.0) then
     CCFT = RD1(IMAP) + D*RD2(IMAP) + D*D*RD3(IMAP)
  else if (D .gt.0.1) then
     CCFT = RDA(IMAP) * (D**RDB(IMAP))
  else
     CCFT = 0.001
  end if

  !  COMPUTE CROWN WIDTH

  CRWDTH = SQRT(CCFT/0.001803)
  if (CRWDTH .gt.99.9) CRWDTH = 99.9

  CCFT = CCFT * P

  return
end
