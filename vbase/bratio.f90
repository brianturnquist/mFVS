!=========================!
!   Compute bark ratios   !
!=========================!
! IE, EM, BC $Id$
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN NI VARIANT.

  ! Common blocks
  include 'PRGPRM.f90'

  !  ONE BARK-RATIO CALCULATION USES THE ORIGINAL NATIVE TABLES.
  !  KEEP EACH BANK'S SPECIES ORDER AND EXTENT INDEPENDENT OF THE
  !  MAXSP OF THE VARIANT BEING COMPILED.

  ! Variables
  real    :: BARK1,BARK2,H,D,BRATIO,TEMD,RDANUW
  real    :: BARK1IE(23),BARK2IE(23),BARK1EM(19),BARK2EM(19)
  real    :: BARK1BC(15),BARK2BC(15)
  integer :: IMAPIE(23),IMAPEM(19),IMAPBC(15),IS,IEQN

  !  IE COEFFICIENT AND EQUATION BANK.

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=WH,  6=RC,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=MH, 12=WB, 13=LM, 14=LL, 15=PI, 16=JU,
  !  17=PY, 18=AS, 19=CO, 20=MM, 21=PB, 22=OH, 23=OS

  data BARK1IE/0.964, 0.851, 0.867, 0.915, 0.934, 0.950, 0.969,&
       &0.956, 0.937, 0.890, 0.934, 0.851, 0.969, 0.937, 0.000,&
       &0.000, 0.969, 0.950, 0.892, 0.950, 0.950, 0.892, 0.934/
  data BARK2IE/0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, -.086, 0.000, 0.000, -.086, 0.000/
  data IMAPIE/     2,     2,     2,     2,     2,     2,     2,&
       &2,     2,     2,     2,     2,     2,     2,     1,&
       &1,     2,     2,     3,     2,     2,     3,     2/

  !  EM COEFFICIENT AND EQUATION BANK.

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  OS USES EM WB BARK COEFFICIENT

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  data BARK1EM/0.934, 0.934, 0.867, 0.969, 0.937, 0.000, 0.969,&
       &0.956, 0.937, 0.890, 0.892, 0.950, 0.892, 0.892, 0.892,&
       &0.892, 0.950, 0.934, 0.892/
  data BARK2EM/0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, -.086, 0.000, -.086, -.086, -.086,&
       &-.086, 0.000, 0.000, -.086/
  data IMAPEM/     2,     2,     2,     2,     2,     1,     2,&
       &2,     2,     2,     3,     2,     3,     3,     3,&
       &3,     2,     2,     3/

  !  BC COEFFICIENT AND EQUATION BANK.

  !  BIRCH, ASPEN AND COTTONWOOD FACTORS ARE FROM B. SNOWDON.

  data BARK1BC/0.964, 0.851, 0.867, 0.915, 0.934, 0.950, 0.969,&
       &0.956, 0.937, 0.890, 0.939, 0.916, 0.897, 0.867, 0.939/
  data BARK2BC/15*0.0/
  data IMAPBC/     2,     2,     2,     2,     2,     2,     2,&
       &2,     2,     2,     2,     2,     2,     2,     2/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

  !  PI, JU USE PP BARK EQUATION FROM CR VARIANT.
  !  WB USE COEFFICIENTS FOR WL
  !  LM AND PY USE COEFFICIENTS FROM TT FOR LM
  !  LL USE COEFFICIENTS FOR AF
  !  AS, MM, PB USE COEFFIECIENTS FOR AS FROM UT
  !  CO, OH USE COEFFIECIENTS FOR OH FROM CR
  !  OS USE COEFFEICIENTS FOR MH

  !  SELECT ONLY THE CURRENT VARIANT'S NATIVE COEFFICIENTS.

  select case (VARIANTCD)
  case ('IE')
     IEQN = IMAPIE(IS)
     BARK1 = BARK1IE(IS)
     BARK2 = BARK2IE(IS)
  case ('EM')
     IEQN = IMAPEM(IS)
     BARK1 = BARK1EM(IS)
     BARK2 = BARK2EM(IS)
  case ('BC')
     IEQN = IMAPBC(IS)
     BARK1 = BARK1BC(IS)
     BARK2 = BARK2BC(IS)
  end select
  TEMD = D
  if (TEMD.lt.1.)TEMD = 1.

  if (IEQN .eq.1) then
     if (BARK1.eq.0.0.and. BARK2.eq.0.0) then
        if (TEMD.gt.19.)TEMD = 19.
        BRATIO = 0.9002 - 0.3089*(1/TEMD)
     else
        BRATIO = BARK1+BARK2*(1/TEMD)
     end if

  else if (IEQN .eq.2) then
     BRATIO = BARK1

  else if (IEQN .eq.3) then
     BRATIO = BARK1+BARK2*(1.0/TEMD)
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end
