!=================================================!
!   Compute subsequent and planted tree heights   !
!=================================================!
! UT $Id$
subroutine ESSUBH (I,HHT,EMSQR,DILATE,DELAY,ELEV2,ISER,GENTIM,&
     &TRAGE)
  implicit none

  !     ASSIGNS HEIGHTS TO SUBSEQUENT AND PLANTED TREE RECORDS
  !     CREATED BY THE ESTABLISHMENT MODEL.

  !     COMING INTO ESSUBH, TRAGE IS THE AGE OF THE TREE AS SPECIFIED ON
  !     THE PLANT OR NATURAL KEYWORD.  LEAVING ESSUBH, TRAGE IS THE NUMBER
  !     BETWEEN PLANTING (OR NATURAL REGENERATION) AND THE END OF THE
  !     CYCLE.  AGE IS TREE AGE UP TO THE TIME REGENT WILL BEGIN GROWING
  !     THE TREE.
  !     CALLED FORM **ESTAB

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'PLOT.f90'

  !  DECLARATIONS

  ! Variables
  integer :: I,ISER,N,ITIME
  real    :: HHT,DELAY,DILATE,ELEV2,EMSQR,GENTIM,TRAGE,AGE
  real    :: RDANUW
  integer :: IDANUW

  ! SPECIES ORDER FOR UTAH VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
  ! 21=BI, 22=BE, 23=OS, 24=OH

  ! VARIANT EXPANSION:
  ! GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
  ! NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM UT

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = DILATE
  RDANUW = ELEV2
  RDANUW = EMSQR
  IDANUW = ISER

  N = INT(DELAY+0.5)
  if (N.lt.-3) N = -3
  DELAY = FLOAT(N)
  ITIME = INT(TIME+0.5)
  if (N.gt.ITIME) DELAY = TIME
  AGE = TIME-DELAY-GENTIM+TRAGE
  if (AGE.lt.1.0) AGE = 1.0
  TRAGE = TIME-DELAY

  select case (I)

     !     HEIGHT OF TALLEST SUBSEQUENT SPECIES 1 (WB)

  case (1)
     HHT = 1.0

     !     HEIGHT OF TALLEST SUBS. SPECIES 2 (LM)

  case (2)
     HHT = 0.5

     !     HT OF TALLEST SUBS. SPECIES 3 (DF)

  case (3)
     HHT = 2.0

     !     HT OF TALLEST SUBS. SPECIES 4 (WF)

  case (4)
     HHT = 2.0

     !     HT OF TALLEST SUBS. SPECIES 5 (BS)

  case (5)
     HHT = 1.0

     !     HT OF TALLEST SUBS. SPECIES 6 (AS)

  case (6)
     HHT = 5.0

     !     HT OF TALLEST SUBS. SPECIES 7 (LP)

  case (7)
     HHT = 3.0

     !     HT OF TALLEST SUBS. SPECIES 8 (ES)

  case (8)
     HHT = 1.5

     !     HT OF TALLEST SUBS. SPECIES 9 (AF)

  case (9)
     HHT = 0.75

     !     HEIGHT OF TALLEST SUBS. SPECIES 10 (PP)

  case (10)
     HHT = 3.0

     !     HT OF TALLEST SUBS. SPECIES 11 (PI), 14 (PM), 17 (GB)

  case (11,14,17)
     HHT = 0.5

     !     HT OF TALLEST SUBS. SPECIES 12 (WJ), 15 (RM), 16 (UJ)

  case (12,15,16)
     HHT = 0.5

     !     HT OF TALLEST SUBS. SPECIES 13 (GO)

  case (13)
     HHT = 5.0

     !     HT OF TALLEST SUBS. SPECIES 18 (NC), 19 (FC), 22 (BE)

  case (18,19,22)
     HHT = 10.0

     !     HT OF TALLEST SUBS. SPECIES 20 (MC), 21 (BI)

  case (20,21)
     !      SI= SITEAR(I)
     !      HHT = ((1.47043 + 0.23317*SI)/(31.56252 - 0.05586*SI))*AGE
     HHT = 1.0

     !     HT OF TALLEST SUBS. SPECIES 23 (OS)

  case (23)
     HHT = 1.0

     !     HT OF TALLEST SUBS. SPECIES 24 (OH)

  case (24)
     HHT = 5.0

  end select

  return
end
  !**END OF CODE SEGMENT
