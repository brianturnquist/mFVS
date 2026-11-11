!=========================!
!   Compute bark ratios   !
!=========================!
! EM $Id$
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BLKDAT.

  ! Common blocks
  include 'PRGPRM.f90'

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

  ! Variables
  real    :: BARK1(MAXSP),BARK2(MAXSP),H,D,BRATIO,TEMD
  integer :: IMAP(MAXSP),IS,IEQN
  real    :: RDANUW

  data BARK1/0.934, 0.934, 0.867, 0.969, 0.937, 0.000, 0.969,&
       &0.956, 0.937, 0.890, 0.892, 0.950, 0.892, 0.892, 0.892,&
       &0.892, 0.950, 0.934, 0.892/
  data BARK2/0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,&
       &0.000, 0.000, 0.000, -.086, 0.000, -.086, -.086, -.086,&
       &-.086, 0.000, 0.000, -.086/
  data IMAP/     2,     2,     2,     2,     2,     1,     2,&
       &2,     2,     2,     3,     2,     3,     3,     3,&
       &3,     2,     2,     3/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

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
