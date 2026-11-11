!=========================!
!   Compute bark ratios   !
!=========================!
! IE $Id$
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN NI VARIANT.

  ! Common blocks
  include 'PRGPRM.f90'

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=WH,  6=RC,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=MH, 12=WB, 13=LM, 14=LL, 15=PI, 16=JU,
  !  17=PY, 18=AS, 19=CO, 20=MM, 21=PB, 22=OH, 23=OS

  ! Variables
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,TEMD
  integer :: IMAP(MAXSP),IS,IEQN
  real    :: RDANUW

  data BARK1/0.964, 0.851, 0.867, 0.915, 0.934, 0.950, 0.969,&
       &0.956, 0.937, 0.890, 0.934, 0.851, 0.969, 0.937, 0.000,&
       &0.000, 0.969, 0.950, 0.892, 0.950, 0.950, 0.892, 0.934/
  data BARK2/0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, -.086, 0.000, 0.000, -.086, 0.000/
  data IMAP/     2,     2,     2,     2,     2,     2,     2,&
       &2,     2,     2,     2,     2,     2,     2,     1,&
       &1,     2,     2,     3,     2,     2,     3,     2/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

  !  PI, JU USE PP BARK EQUATION FROM CR VARIANT.
  !  WB USE COEFFICIENTS FOR WL
  !  LM AND PY USE COEFFICIENTS FROM TT FOR LM
  !  LL USE COEFFICIENTS FOR AF
  !  AS, MM, PB USE COEFFIECIENTS FOR AS FROM UT
  !  CO, OH USE COEFFIECIENTS FOR OH FROM CR
  !  OS USE COEFFEICIENTS FOR MH

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
