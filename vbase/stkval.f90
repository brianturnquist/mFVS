!====================!
!   vbase : stkval   !
!====================!
! VBASE $Id$
subroutine STKVAL(S)
  implicit none

  !     THIS ROUTINE CALCULATES THE STOCKING VALUES FOR EACH
  !     TREE IN THE STAND BASED ON ARNER 2001, NATIONAL ALGORITHMS
  !     FOR STOCKING, STAND SIZE, AND FOREST TYPE FOR FOREST INVENTORY
  !     AND ANALYSIS PLOTS.  TO PROVIDE ADEQUATE CODE DOCUMENTATION,
  !     STANDARD FVS CODING FORMAT HAS BEEN RELAXED TO ALLOW
  !     THE USE OF EXCLAMATION MARKS (!) TO PLACE COMMENTS AT THE
  !     END OF VALID FORTRAN STATEMENTS.

  !     CALLED FROM FORTYP

  !     S      - THIS ARRAY IS LOADED IN THIS ROUTINE WITH STOCKING VALUES
  !              ACCORDING TO INITIAL FOREST TYPE.

  !  INTERNAL VARIABLE DEFINITIONS

  !     DMXSTD - D(IND(1)) THE MAXIMUM DBH IN THE STAND
  !     DMXSS  - DIAMETER MAXIMUM FOR DIA < 5.O" TREES
  !     TOTSTK - TOTAL STAND STOCKING FOR ALL TREES
  !     TTST51 - SUM OF TOTAL STAND STOCKING FOR TREES
  !              GREATER THAN 5 IN DBH, PROCESS STEP 1
  !     TOTST5 - SUM OF TOTAL STAND STOCKING FOR TREES
  !              GREATER THAN 5 IN DBH
  !     TOTST3 - SUM OF TOTAL STAND STOCKING FOR TREES
  !              > 0.1" & < 5.0" IN DBH (SAPLINGS)
  !     TOTST1 - SUM OF TOTAL STAND STOCKING FOR TREES
  !              EQUAL TO 0.1 IN DBH (SEEDLINGS)
  !     SZCL   - SIZE CLASS ARRAY (3) SMALL/MEDIUM/LARGE
  !     ISZCL  - SIZE CLASS CODE
  !     ISTCL  - STOCKING CLASS CODE
  !     SS     - SS(MAXSP) THE STOCKING VALUE FOR EACH SPECIES CALCULATED
  !              IN THIS ROUTINE
  !     TAB2   - COEFFICIENTS FOR THE 36 STOCKING EQUATIONS
  !     TAB3   - REPRESENTS POTENTIALLY 1000 SPECIES BY FIA NUMBER.
  !              TAB3(FIA,1)= INITIAL TYPE GROUP NUMBER
  !              TAB3(FIA,2)= STOCKING EQUATION NUMBER

  !OMMON BLOCKS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !  DECLARATIONS

  ! Variables
  integer :: IS,IFIA,I3,I2,I1,ISPC
  real    :: PRE,PRA,PRG,UE,UA,EI,AI,GI,UG,STKTR2,CF,Q,D,B1,B0
  real    :: DMXSS,DMXSTD,TOTST1,TOTST3,TOTST5,TTST52,TTST51,TOTSTK,DMAX
  real    :: STKTR1
  integer :: I,J
  real    :: TAB2(36,2)
  integer :: TAB3(1000,2)
  real    :: SZCL(3)
  real    :: SS(MAXSP),S(210)
  logical :: DEBUG

  !  DATA STATEMENTS

  data ((TAB2(I,J),J = 1,2),I = 1,15) /&
       &0.00869, 1.48,&
       &0.00454, 1.73,&
       &0.01691, 1.05,&
       &0.00946, 1.59,&
       &0.00422, 1.70,&
       &0.00509, 1.81,&
       &0.00458, 1.91,&
       &0.00335, 1.73,&
       &0.01367, 1.44,&
       &0.00250, 2.00,&
       &0.00609, 1.67,&
       &0.00914, 1.67,&
       &0.00900, 1.51,&
       &0.00680, 1.72,&
       &0.00769, 1.54/
  data ((TAB2(I,J),J = 1,2),I = 16,30) /&
       &0.00433, 1.80,&
       &0.00313, 2.11,&
       &0.00427, 1.67,&
       &0.00333, 1.68,&
       &0.00000, 1.00,&
       &0.00000, 1.00,&
       &0.00000, 1.00,&
       &0.00000, 1.00,&
       &0.00000, 1.00,&
       &0.01105, 1.53,&
       &0.01671, 1.41,&
       &0.00694, 1.86,&
       &0.00635, 1.89,&
       &0.01119, 1.63,&
       &0.01546, 1.50/
  data ((TAB2(I,J),J = 1,2),I = 31,36) /&
       &0.00429, 1.87,&
       &0.01429, 1.46,&
       &0.02197, 1.13,&
       &0.00000, 1.00,&
       &0.00442, 2.02,&
       &0.00688, 1.86/
  data ((TAB3(I,J),J = 1,1),I = 1,100)/&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,  55,&
       &1,  55,   0,   2,   2,  55,   3,   4,   4,   5,&
       &5,   7,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &8,   9,  59,   0,   0,   0,   0,   0,   0,  12,&
       &12,  12,  12,  12,  12,   0,  64, 161, 161, 161,&
       &161, 161, 161,  38, 161,  63,  64,  64, 161,  70,&
       &65,  40,  13,   0,   0,   0,   0,   0,   0,   0,&
       &10,   0,   0,   0,   0,   0,   0,   0,   0,  16,&
       &70,  40,  14,  16,  17,  15,  58,  18,  16,  42/
  data ((TAB3(I,J),J = 1,1),I = 101,200)/&
       &19,  20,  21,  20,  41, 162,  44,  23,  24,  45,&
       &46,  22,  25,  22,  47,  36,  27,  22,  28,   6,&
       &48,  26,  49,  29,  42,  50, 163,  51,  53,  71,&
       &52,  54, 162, 162,  26,  70,  22,  22,  22, 162,&
       &22,  20, 162,  72,  70,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0/
  data ((TAB3(I,J),J = 1,1),I = 201,300)/&
       &30,  31,   0,   0,   0,   0,   0,   0,   0,   0,&
       &32,  33,   0,   0,   0,   0,   0,   0,   0,   0,&
       &61,  61,  61,   0,   0,   0,   0,   0,   0,   0,&
       &40,  40,   0,   0,   0,   0,   0,   0,   0,  60,&
       &60,  11,   0,   0,   0,   0,   0,   0,   0,   0,&
       &40,  40,   0,   0,   0,   0,   0,   0,   0,  66,&
       &66,  66,  34,  35,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0, 170, 160/
  data ((TAB3(I,J),J = 1,1),I = 301,400)/&
       &0,   0,   0,   0,   0,   0,   0,   0,   0, 152,&
       &151, 130, 208,  96, 152,  95,  97,  96, 152, 152,&
       &159, 159, 159, 159, 159,   0,   0,   0,   0, 153,&
       &153, 153, 153, 153, 153, 153, 153,   0,   0,   0,&
       &151,   0,   0,   0, 153, 153,   0,   0,   0, 153,&
       &131, 153, 153,   0, 146, 152,   0,   0,   0,   0,&
       &132, 132, 132,   0,   0,   0, 152,   0,   0,  98,&
       &98,  98, 129, 151,  99,  99,  99,  99,  99,   0,&
       &151,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &152,   0,   0,   0,   0,   0,   0,   0,   0,  92/
  data ((TAB3(I,J),J = 1,1),I = 401,500)/&
       &90,  92,  92,  91,  90,  92,  92,  92,  92,  92,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &152, 152, 152,   0,   0,   0,   0,   0,   0, 133,&
       &133,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0, 152,&
       &152, 152,   0,   0,   0,   0,   0,   0,   0, 115,&
       &100, 115, 115,   0,   0,   0,   0,   0,   0,   0,&
       &152,   0,   0,   0, 156, 156, 156, 156, 156,   0,&
       &153,   0,   0,   0,   0,   0,   0,   0,   0, 153,&
       &153, 153,   0,   0,   0,   0,   0,   0,   0, 101/
  data ((TAB3(I,J),J = 1,1),I = 501,600)/&
       &101, 101,   0,   0,   0,   0,   0,   0,   0, 148,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &93,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &102,   0,   0,   0,   0,   0,   0,   0,   0, 153,&
       &103, 135, 104, 105, 153, 153, 153, 153, 153,   0,&
       &151, 101,   0,   0, 127,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &101,   0,   0,   0,   0,   0,   0,   0,   0, 153,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &106,   0,   0,   0,   0,   0,   0,   0,   0, 153/
  data ((TAB3(I,J),J = 1,1),I = 601,700)/&
       &107, 108, 153, 153, 153, 153,   0,   0,   0,   0,&
       &109,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &110,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &136,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &101,   0,   0,   0,   0,   0,   0,   0,   0, 152,&
       &152, 152, 111, 152, 152, 152, 152, 152,   0, 152,&
       &152, 152, 152, 152, 152,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0, 152,&
       &152, 152, 152, 152,   0,   0,   0,   0,   0, 113,&
       &112, 151, 113, 114,   0,   0,   0,   0,   0,   0/
  data ((TAB3(I,J),J = 1,1),I = 701,800)/&
       &152, 152,   0,   0,   0,   0,   0,   0,   0,   0,&
       &152, 144,   0,   0,   0,   0,   0,   0,   0,   0,&
       &127, 151,   0,   0,   0,   0,   0,   0,   0, 153,&
       &116, 153,   0,   0,   0,   0,   0,   0,   0, 118,&
       &117, 118, 119, 118, 118, 119, 137, 118, 118,   0,&
       &0, 118,   0,   0, 157, 157, 157, 157,   0, 152,&
       &152, 121, 152, 152, 152, 152,   0, 152,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0, 158/
  data ((TAB3(I,J),J = 1,1),I = 801,900)/&
       &138,  81, 210, 151, 142,  82, 134,  89,  86, 210,&
       &210,  88,  87, 158, 140,  89, 202, 139,  89, 201,&
       &158, 128,  83, 206,  87, 207, 204, 143, 210, 205,&
       &125,  84,  85,  87,  86,  87, 120, 203, 142,  86,&
       &89,  89, 210,   0,   0,   0,   0,   0,   0, 210,&
       &152,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0, 152/
  data ((TAB3(I,J),J = 1,1),I = 901,1000)/&
       &122, 160,   0,   0,   0,   0,   0,   0,   0,   0,&
       &147,   0,   0,   0,   0,   0,   0,   0, 158, 123,&
       &123, 123, 123, 123,   0,   0, 123,   0, 123,   0,&
       &93,   0,   0,   0, 152, 152, 152, 152, 152,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0, 124,&
       &124, 124, 124,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0, 152,&
       &209,  94, 209, 146,  94, 152,  94,   0,   0,   0,&
       &141,   0,   0,   0,   0,   0,   0,   0, 149, 160,&
       &153, 145, 146, 146, 146, 153, 153, 180, 190,   0/
  data ((TAB3(I,J),J = 2,2),I = 1,100)/&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   1,&
       &18,   1,   0,  18,  15,   1,  18,   1,   1,  18,&
       &18,  18,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &18,  18,  16,   0,   0,   0,   0,   0,   0,  10,&
       &10,  10,  10,  10,  10,   0,   4,  10,  10,   3,&
       &3,  10,  10,  10,  10,  10,   4,   4,  10,   1,&
       &1,   2,   2,   0,   0,   0,   0,   0,   0,   0,&
       &18,   0,   0,   0,   0,   0,   0,   0,   0,   1,&
       &1,  18,   1,   1,   3,   1,   1,  18,   1,  11/
  data ((TAB3(I,J),J = 2,2),I = 101,200)/&
       &10,  10,   8,   8,   4,  10,   4,   5,  10,   6,&
       &7,  10,  10,  10,   4,  10,  10,  10,   8,  10,&
       &9,  10,   4,  10,  11,   4,  10,  12,  13,   4,&
       &14,   4,  10,  10,  10,  11,  10,  10,  10,  10,&
       &10,  10,  10,   4,   4,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0/
  data ((TAB3(I,J),J = 2,2),I = 201,300)/&
       &15,  15,   0,   0,   0,   0,   0,   0,   0,   0,&
       &19,  19,   0,   0,   0,   0,   0,   0,   0,   0,&
       &31,  31,  31,   0,   0,   0,   0,   0,   0,   0,&
       &18,  18,   0,   0,   0,   0,   0,   0,   0,  16,&
       &16,  18,   0,   0,   0,   0,   0,   0,   0,   0,&
       &18,  18,   0,   0,   0,   0,   0,   0,   0,  17,&
       &17,  17,  18,  18,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,  13,   0,  25/
  data ((TAB3(I,J),J = 2,2),I = 301,400)/&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,  25,&
       &25,  25,  36,  27,  27,  25,  25,  27,  25,  25,&
       &10,  10,  10,  10,  10,   0,   0,   0,   0,  27,&
       &27,  27,  27,  27,  27,  27,  27,   0,   0,   0,&
       &25,   0,   0,   0,  36,  36,   0,   0,   0,  26,&
       &26,  26,  26,   0,  26,  25,   0,   0,   0,   0,&
       &29,  29,  29,   0,   0,   0,  25,   0,   0,  27,&
       &27,  27,  28,  28,  28,  28,  28,  28,  28,   0,&
       &25,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &25,   0,   0,   0,   0,   0,   0,   0,   0,  29/
  data ((TAB3(I,J),J = 2,2),I = 401,500)/&
       &29,  29,  29,  29,  29,  29,  29,  29,  29,  29,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &25,  29,  29,   0,   0,   0,   0,   0,   0,  29,&
       &29,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,  27,&
       &27,  27,   0,   0,   0,   0,   0,   0,   0,  36,&
       &36,  36,  36,   0,   0,   0,   0,   0,   0,   0,&
       &25,   0,   0,   0,  10,  33,  33,  33,  33,   0,&
       &25,   0,   0,   0,   0,   0,   0,   0,   0,  25,&
       &25,  26,   0,   0,   0,   0,   0,   0,   0,  29/
  data ((TAB3(I,J),J = 2,2),I = 501,600)/&
       &29,  29,   0,   0,   0,   0,   0,   0,   0,  15,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &29,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &27,   0,   0,   0,   0,   0,   0,   0,   0,  33,&
       &33,  33,  33,  36,  33,  33,  33,  33,  33,   0,&
       &25,  27,   0,   0,  25,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &25,   0,   0,   0,   0,   0,   0,   0,   0,  25,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &25,   0,   0,   0,   0,   0,   0,   0,   0,  30/
  data ((TAB3(I,J),J = 2,2),I = 601,700)/&
       &30,  30,  30,  30,  30,  30,   0,   0,   0,   0,&
       &31,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &33,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &25,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &29,   0,   0,   0,   0,   0,   0,   0,   0,  33,&
       &33,  33,  25,  33,  33,  33,  33,  33,   0,  29,&
       &29,  29,  29,  29,  29,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,  25,&
       &25,  25,  25,  25,   0,   0,   0,   0,   0,  31,&
       &31,  31,  31,  31,   0,   0,   0,   0,   0,   0/
  data ((TAB3(I,J),J = 2,2),I = 701,800)/&
       &25,  25,   0,   0,   0,   0,   0,   0,   0,   0,&
       &25,  27,   0,   0,   0,   0,   0,   0,   0,   0,&
       &33,  33,   0,   0,   0,   0,   0,   0,   0,  36,&
       &36,  36,   0,   0,   0,   0,   0,   0,   0,  36,&
       &32,  36,  32,  36,  36,  32,  36,  36,  36,   0,&
       &0,  36,   0,   0,  10,  10,  10,  10,   0,  25,&
       &25,  33,  25,  25,  25,  25,   0,  25,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,  10/
  data ((TAB3(I,J),J = 2,2),I = 801,900)/&
       &29,  29,  10,  29,  29,  29,  29,  29,  29,  10,&
       &10,  29,  29,  10,  29,  29,  29,  29,  29,  29,&
       &25,  29,  10,  29,  29,  10,  29,  29,  10,  29,&
       &29,  29,  29,  29,  29,  29,  29,  29,  29,  29,&
       &29,  29,  10,   0,   0,   0,   0,   0,   0,  10,&
       &25,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,  29/
  data ((TAB3(I,J),J = 2,2),I = 901,1000)/&
       &29,  10,   0,   0,   0,   0,   0,   0,   0,   0,&
       &29,   0,   0,   0,   0,   0,   0,   0,  25,  25,&
       &25,  25,  25,  25,   0,   0,  25,   0,  25,   0,&
       &29,   0,   0,   0,  25,  25,  25,  25,  25,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,  35,&
       &35,  35,  35,   0,   0,   0,   0,   0,   0,   0,&
       &0,   0,   0,   0,   0,   0,   0,   0,   0,  36,&
       &36,  36,  36,  36,  36,  36,  36,   0,   0,   0,&
       &29,   0,   0,   0,   0,   0,   0,   0,  25,  10,&
       &25,   1,  33,  25,  25,  25,  25,  25,  25,   0/

  ! REDEFINE TAB3 STOCK EQ. COEF. FOR WESTERN VARIANTS

  select case (VARACD)
  case ('CS','LS','NE','SN','ON')

  case default
     TAB3(299,2) = 08 ! WEST OTHER SOFTWOOD
     TAB3(998,2) = 26 ! WEST OTHER HARDWOOD
     TAB3(999,2) = 26 ! WEST OTHER UNKNOWN
  end select

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'STKVAL',6,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC, ITRN
  3 format(' ENTERING SUBROUTINE STKVAL  CYCLE =',I5,' ITRN= ',I5)
  DMAX = 0.

  !  INITIALIZE VARIABLES

  do I = 1,MAXSP
     SS(I) = 0.0
     5 continue
  end do
  TOTSTK = 0.0
  TTST51 = 0.0
  TTST52 = 0.0
  TOTST5 = 0.0
  TOTST3 = 0.0
  TOTST1 = 0.0
  do I = 1,3
     SZCL(I) = 0.
  end do
  ISZCL = 0
  ISTCL = 0
  if (ITRN.le.0) return

  ! --- FIND DBH OF LARGEST TREE IN THE STAND
  !      IF (ICYC.LE.1) THEN
  DMXSTD = 0.0
  DMXSS = 0.0
  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.gt.0) then
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           I = IND1(I3)
           if ((IMC(I).ge.6).and.(IMC(I).le.9))go to 7
           if (DBH(I).gt.DMXSTD) DMXSTD = DBH(I)
           if (DBH(I).lt.5.0.and.DBH(I).gt.DMXSS) DMXSS = DBH(I)
           7 continue
        end do
     end if
     8 continue
  end do
  ! --- STEP 1: ASSIGN INITIAL STOCKING VALUE FOR EACH TREE
  do ISPC = 1,MAXSP !SPECIES LOOP
     I1 = ISCT(ISPC,1)
     if (I1.gt.0) then
        I2 = ISCT(ISPC,2)
        if (FIAJSP(ISPC).eq.'   ') then
           IFIA = 998
        else
           read (FIAJSP(ISPC),'(I4)') IFIA !FIA SPECIES CODE
        end if
        if (IFIA.eq.0) IFIA = 999
        B0 = TAB2(TAB3(IFIA,2),1) !STOCKING EQ. b0 COEFFICIENT
        B1 = TAB2(TAB3(IFIA,2),2) !STOCKING EQ. b1 COEFFICIENT
        if (B0.eq.0.or.B1.eq.0) then
           B0 = TAB2(TAB3(999,2),1)
           B1 = TAB2(TAB3(999,2),2)
        end if
        do I3 = I1,I2 !TREE LOOP
           I = IND1(I3)
           if ((IMC(I).ge.6).and.(IMC(I).le.9))go to 11
           D = DBH(I)
           if (D.gt.0.0) then
              Q = 1.0 !STOCKABILITY PROPORTION FROM DESIGN KEYWORD, PARAMETER FIELD 7
              STKTR1 = (B0*D**B1)*PROB(I)/Q !STEP 1 STOCKING VALUE
           end if
           if (D.ge.5.0) TTST51 = TTST51+STKTR1 !STEP 1 STOCKING TREES >= 5.0"
           11 continue
        end do
     end if
     21 continue
  end do
  if (DEBUG)write(JOSTND,*)' TTST51,STKTR1,PROB(1)= ',&
       &TTST51,STKTR1,PROB(1)

  ! --- STEP 2: ADJUST INITIAL STOCKING TO REFLECT COMPETITIVE POSITION
  do ISPC = 1,MAXSP !SPECIES LOOP
     I1 = ISCT(ISPC,1)
     if (I1.gt.0) then
        I2 = ISCT(ISPC,2)
        if (FIAJSP(ISPC).eq.'   ') then
           IFIA = 998
        else
           read(FIAJSP(ISPC),'(I4)')IFIA
        end if
        if (IFIA.eq.0) IFIA = 999
        B0 = TAB2(TAB3(IFIA,2),1)
        B1 = TAB2(TAB3(IFIA,2),2)
        if (B0.eq.0.or.B1.eq.0) then
           B0 = TAB2(TAB3(999,2),1)
           B1 = TAB2(TAB3(999,2),2)
        end if
        do I3 = I1,I2 !TREE LOOP
           I = IND1(I3)
           if ((IMC(I).ge.6).and.(IMC(I).le.9))go to 12
           D = DBH(I)
           if (D.gt.0.0) then
              if (D.ge.5.0) then
                 CF = 1.0 !TABLE 4 (ARNER 2/13/2001) CROWN POSITION ADJUSTMENT FACTORS (CF)
              else !         CROWN CLASS                 CF FOR TREES >= 5.0"
                 if (TTST51.ge.10) then !  -- OPEN GROWN, DOMINANT, CODOM            1.0
                    DMAX = 5.0 !  -- INTERMEDIATE                           0.5
                 else !  -- OVERTOPPED/SUPPRESSED                  0.1
                    DMAX = DMXSS !
                 end if ! DEFAULT CF VALUE SET TO 1.0 FOR TREES WITHOUT CROWN CLASS CODED
                 CF = D/DMAX
              end if
              Q = 1.0
              STKTR2 = ((B0*D**B1)*PROB(I)/Q)*CF !STEP 2 STOCKING VALUE
           end if
           if (D.ge.5.0) TTST52 = TTST52+STKTR2 !STEP 2 STOCKING TREES >= 5.0"
           if (D.ge.5.0) TOTST5 = TOTST5+STKTR2 !ACCUMMULATE STOCKING TREES >= 5.0"
           if (D.gt.0.1.and.D.lt.5.0) TOTST3 = TOTST3+STKTR2 !ACCUMMULATE STOCKING SAPLINGS
           if (D.eq.0.1) TOTST1 = TOTST1+STKTR2 !ACCUMMULATE STOCKING SEEDLINGS
           TOTSTK = TOTSTK+STKTR2 !ACCUMMULATE TOTAL STOCKING
           SS(ISPC) = SS(ISPC)+STKTR2 !SUM TREE STOCKING PER SPECIES
           12 continue
        end do
     end if
     22 continue
  end do
  if (DEBUG) write(JOSTND,*)' IN STKVAL_1: DMXSS,DMAX,CF,TTST51,',&
       &'TOTST5,TOTSTK - ',DMXSS,DMAX,CF,TTST51,TOTST5,TOTSTK

  ! --- STEP 3 & 4: DETERMINE 'FUTURE STAND' OR STANDARD VALUES FOR SEEDS/SAPS
  if (TTST52.lt.20.0) then !"FUTURE STAND" VALUES FOR SEEDS/SAPS
     do I = 1,MAXSP
        SS(I) = 0.0
        25 continue
     end do
     TOTSTK = 0.0
     TOTST5 = 0.0
     TOTST3 = 0.0
     TOTST1 = 0.0
     do ISPC = 1,MAXSP !SPECIES LOOP
        I1 = ISCT(ISPC,1)
        if (I1.gt.0) then
           I2 = ISCT(ISPC,2)
           if (FIAJSP(ISPC).eq.'   ') then
              IFIA = 998
           else
              read(FIAJSP(ISPC),'(I4)')IFIA
           end if
           if (IFIA.eq.0) IFIA = 999
           B0 = TAB2(TAB3(IFIA,2),1)
           B1 = TAB2(TAB3(IFIA,2),2)
           if (B0.eq.0.or.B1.eq.0) then
              B0 = TAB2(TAB3(999,2),1)
              B1 = TAB2(TAB3(999,2),2)
           end if
           do I3 = I1,I2 !TREE LOOP
              I = IND1(I3)
              if ((IMC(I).ge.6).and.(IMC(I).le.9))go to 30
              D = DBH(I)
              if (D.gt.0.0) then
                 if (D.ge.5.0) then
                    CF = 1.0
                 else
                    if (TTST51.ge.10) then
                       DMAX = 5.0
                    else
                       DMAX = DMXSS
                    end if
                    CF = D/DMAX
                 end if
                 if (D.lt.5.0) D = 5.0
                 Q = 1.0
                 STKTR2 = ((B0*D**B1)*PROB(I)/Q)*CF
              end if
              D = DBH(I)
              if (D.ge.5.0) TOTST5 = TOTST5+STKTR2 !ACCUMMULATE STOCKING TREES >= 5.0"
              if (D.gt.0.1.and.D.lt.5.0) TOTST3 = TOTST3+STKTR2 !ACCUMMULATE STOCKING SAPLINGS
              if (D.eq.0.1) TOTST1 = TOTST1+STKTR2 !ACCUMMULATE STOCKING SEEDLINGS
              TOTSTK = TOTSTK+STKTR2 !ACCUMMULATE TOTAL STOCKING
              SS(ISPC) = SS(ISPC)+STKTR2 !SUM TREE STOCKING PER SPECIES
              30 continue
           end do
        end if
        40 continue
     end do
  end if
  if (DEBUG) write(JOSTND,*)' IN STKVAL_2: DMXSS,DMAX,CF,TTST51,',&
       &'TOTST5,TOTSTK - ',DMXSS,DMAX,CF,TTST51,TOTST5,TOTSTK

  ! --- STEP 5: ASSURE SEEDS/SAPS DO NOT REDUCE STOCK VALUES OF LARGER TREES
  UG = 120
  GI = TOTST5
  AI = TOTST3
  EI = TOTST1
  UA = MAX(UG-GI,0.0)
  UE = MAX(UA-AI,0.0)
  PRG = 1.0
  PRA = 1.0
  PRE = 1.0
  if (UG.gt.0.and.GI.gt.0) PRG = MIN(UG/GI,1.0)
  if (UA.gt.0.and.AI.gt.0) PRA = MIN(UA/AI,1.0)
  if (UE.gt.0.and.EI.gt.0) PRE = MIN(UE/EI,1.0)
  do I = 1,MAXSP
     SS(I) = 0.0
  end do
  TOTSTK = 0.0
  TOTST5 = 0.0
  TOTST3 = 0.0
  TOTST1 = 0.0
  do ISPC = 1,MAXSP !SPECIES LOOP
     I1 = ISCT(ISPC,1)
     if (I1.gt.0) then
        I2 = ISCT(ISPC,2)
        if (FIAJSP(ISPC).eq.'   ') then
           IFIA = 998
        else
           read(FIAJSP(ISPC),'(I4)')IFIA
        end if
        if (IFIA.eq.0) IFIA = 999
        B0 = TAB2(TAB3(IFIA,2),1)
        B1 = TAB2(TAB3(IFIA,2),2)
        if (B0.eq.0.or.B1.eq.0) then
           B0 = TAB2(TAB3(999,2),1)
           B1 = TAB2(TAB3(999,2),2)
        end if
        do I3 = I1,I2 !TREE LOOP
           I = IND1(I3)
           if ((IMC(I).ge.6).and.(IMC(I).le.9))go to 80
           D = DBH(I)
           if (D.gt.0.0) then
              if (D.ge.5.0) then
                 CF = 1.0
              else
                 if (TTST51.ge.10) then
                    DMAX = 5.0
                 else
                    DMAX = DMXSS
                 end if
                 CF = D/DMAX
              end if
              if (TTST52.lt.20.0.and.D.lt.5.0) D = 5.0
              Q = 1.0
              STKTR2 = ((B0*D**B1)*PROB(I)/Q)*CF
           end if
           D = DBH(I)
           if (D.ge.5.0) TOTST5 = TOTST5+STKTR2 !ACCUMMULATE STOCKING TREES >= 5.0"
           if (D.gt.0.1.and.D.lt.5.0) TOTST3 = TOTST3+STKTR2 !ACCUMMULATE STOCKING SAPLINGS
           if (D.eq.0.1) TOTST1 = TOTST1+STKTR2 !ACCUMMULATE STOCKING SEEDLINGS
           TOTSTK = TOTSTK+STKTR2 !ACCUMMULATE TOTAL STOCKING
           SS(ISPC) = SS(ISPC)+STKTR2 !SUM TREE STOCKING PER SPECIES
           if (D.lt.5) then !ACCUMMULATE STOCKING BY SIZE CLASS
              SZCL(1) = SZCL(1)+STKTR2 !SMALL
           else if (IFIA.lt.300.and.D.ge.5.and.D.lt.9) then
              SZCL(2) = SZCL(2)+STKTR2 !MEDIUM
           else if (IFIA.ge.300.and.D.ge.5.and.D.lt.11) then
              SZCL(2) = SZCL(2)+STKTR2
           else if (IFIA.lt.300.and.D.ge.9) then
              SZCL(3) = SZCL(3)+STKTR2 !LARGE
           else if (IFIA.ge.300.and.D.ge.11) then
              SZCL(3) = SZCL(3)+STKTR2
           end if
           80 continue
        end do
     end if
  end do
  if (DEBUG) write(JOSTND,*)' IN STKVAL_3: DMXSS,DMAX,TTST51,TTST',&
       &'52,TOTST5,TOTSTK - ',DMXSS,DMAX,TTST51,TTST52,TOTST5,TOTSTK

  ! --- DETERMINE STAND SIZE CLASS
  ISZCL = 0
  if (TOTSTK.lt.10) then
     ISZCL = 5 !NON-STOCKED
  else if (SZCL(1).gt.(TOTSTK*0.50)) then
     ISZCL = 3 !SEEDLING-SAPLING
  else if (SZCL(2).gt.SZCL(3)) then
     ISZCL = 2 !POLETIMBER
  else if (SZCL(3).ge.SZCL(2)) then
     ISZCL = 1 !SAWTIMBER
  end if

  ! --- DETERMINE STOCKING CLASS
  ISTCL = 0
  if (TOTSTK.gt.100) then
     ISTCL = 1 !OVERSTOCKED
  else if (TOTSTK.ge.60.and.TOTSTK.lt.100) then
     ISTCL = 2 !FULLY STOCKED
  else if (TOTSTK.ge.35.and.TOTSTK.lt.60) then
     ISTCL = 3 !MODERATELY STOCKED
  else if (TOTSTK.ge.10.and.TOTSTK.lt.35) then
     ISTCL = 4 !POORLY STOCKED
  else if (TOTSTK.ge.00.and.TOTSTK.lt.10) then
     ISTCL = 5 !NON-STOCKED
  end if

  ! --- LOAD INITIAL FOREST TYPE (S ARRAY) BY SPECIES (SS ARRAY)
  !     FIRST LOOP THROUGH THE ITG GROUPS
  !     THEN LOOP THROUGH THE SPECIES LIST
  !     READ THE FIA NUMBER TO LOCATE SPECIES IN TAB3 ARRAY
  !     IF SPECIES IS IN ITG GROUP THEN ADD INTO STOCKING FOR THE ITG GROUP
  do IS = 1,210
     do ISPC = 1,MAXSP
        if (FIAJSP(ISPC) .eq. '   ')go to 99
        read (FIAJSP(ISPC),'(I4)') IFIA
        if (TAB3(IFIA,1).eq.IS) then
           S(IS) = S(IS)+SS(ISPC)
           if (DEBUG) then
              write(JOSTND,*)' IN STKVAL_LOAD, IS,S(IS)= ',IS,S(IS)
           end if
        end if
        99 continue
     end do
  end do

  ! --- DEBUG
  if (DEBUG) then
     write(JOSTND,*)' IN STKVAL, TOTST5= ',TOTST5
     write(JOSTND,*)' ISZCL,ISTCL= ',ISZCL,ISTCL
  end if

  return
end
