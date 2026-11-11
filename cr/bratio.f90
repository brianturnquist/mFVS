!=========================!
!   Compute bark ratios   !
!=========================!
! CR $Id$
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BASE MODEL. COEFFICIENTS FOR BARK1 AND BARK2
  ! ARE SET IN BLKDAT AND SITSET.  IF BARK1 AND BARK2 ARE BOTH ZERO THEN
  ! ITS THE BLACK HILL MODEL TYPE AND USE CORMIER'S FMSC MODEL.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'GGCOM.f90'

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

  ! Variables
  integer :: IMAP(MAXSP),IEQN,IS
  real    :: H,D,BRATIO,TEMD
  real    :: RDANUW

  data IMAP/ 2, 2, 2, 2, 2, 2, 2, 3, 3, 3,&
       &3, 1, 1, 3, 2, 1, 3, 3, 3, 2,&
       &3, 3, 3, 3, 3, 3, 3, 2, 1, 1,&
       &1, 1, 1, 1, 1, 1, 1, 3/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

  !  PI, PP, UJ, AJ, RM, OJ, ER AND OS USE PP BARK EQUATION.
  !  PP EQN IS DIFFERENT FOR BHPP, S-F, AND LP MODEL TYPES.
  !  BARKi() COEFFICIENTS SET TO ZERO IN SITSET TO TRIGGER
  !  DIFFERENT EQUATION IN THESE MODEL TYPES.
  !  PP EQN FOR BLACK HILLS WAS FIT BY CORMIER, FMSC
  !  PP FOR MODEL TYPES 1 & 2 USES EQUATION FROM CA VARIANT
  !  AF, CB, GF, WF USE THE CONSTANT RATIO FROM NI VARIANT

  IEQN = IMAP(IS)
  TEMD = D
  if (TEMD.lt.1.)TEMD = 1.

  if (IEQN .eq.1) then
     if (BARK1(IS).eq.0.0.and. BARK2(IS).eq.0.0) then
        if (TEMD.gt.19.)TEMD = 19.
        BRATIO = 0.9002 - 0.3089*(1/TEMD)
     else
        BRATIO = BARK1(IS)+BARK2(IS)*(1/TEMD)
     end if

  else if (IEQN .eq.2) then
     BRATIO = BARK1(IS)

  else if (IEQN .eq.3) then
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end
