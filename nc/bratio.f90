!=========================!
!   Compute bark ratios   !
!=========================!
! NC $Id$
function BRATIO(IS,D,H)
  implicit none

  !  FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  !  REPLACES ARRAY BKRAT IN BASE MODEL.

  !  SPECIES LIST FOR KLAMATH MOUNTAINS VARIANT.
  !  1=OC,2=SP,3=DF,4=WF,5=M,6=IC,7=BO,8=TO,9=RF,10=PP,11=OH, 12=RW

  ! Variables
  real    :: BRKRAT(4,12),H,D,BRATIO,DBT,DIB
  integer :: IS,J,I
  real    :: RDANUW

  data ((BRKRAT(I,J),I = 1,4),J = 1,12)/&
       &1.,  0.1429,  0.1137,    1.0,&
       &2.,  0.1429,  0.1137,    1.0,&
       &3.,  0.1045,  0.1661,    1.0,&
       &4.,  0.1593,  0.1089,    1.0,&
       &5., -0.01348, 0.98155,   2.0,&
       &6., -0.0549,  0.1626,    1.0,&
       &7., -0.26824, 0.95767,   2.0,&
       &8., -0.26824, 0.95354,   2.0,&
       &9.,  0.1593,  0.1089,    1.0,&
       &10., 0.4448,  0.1033,    1.0,&
       &11.,-0.26824, 0.95767,   2.0,&
       &12., 0.70120, 1.04862,   3.0/

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

  ! Equation Types:
  ! 1 DBT= a+b*DOB
  ! 2 DIB= a+b*DOB
  ! 3 DIB= a*DOB^b

  if (D .gt.0) then
     if (BRKRAT(4,IS) .eq.1.) then
        DBT = BRKRAT(2,IS) + BRKRAT(3,IS)*D
        BRATIO = (D - DBT) / D
     else if (BRKRAT(4,IS) .eq.2.) then
        DIB = BRKRAT(2,IS) + BRKRAT(3,IS)*D
        BRATIO = DIB/D
     else if (BRKRAT(4,IS) .eq.3.) then
        DIB = BRKRAT(2,IS) * D ** BRKRAT(3,IS)
        BRATIO = DIB/D
     end if
  else
     BRATIO = 0.99
  end if

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end
