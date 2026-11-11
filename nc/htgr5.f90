!================!
!   nc : htgr5   !
!================!
! NC $Id$
subroutine HTGR5(ISP,SSITE,BAA,YHTG,RELHT,CR,H,DEBUG)
  implicit none

  ! SUBROUTINE TO CALCULATE 5-YEAR HEIGHT GROWTH
  ! SPECIES INDEX 1-OC, 2-WP, 3-DF, 4-WF, 5-M , 6-IC, 7-BO,
  !               8-TO, 9-RF, 10-PP, 11-OTHER, 12 - RW

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  logical :: DEBUG
  real    :: HRELHT(12),HCRSQ(12),HHT(12),HBA(12),HSITE(12),HCON(12)
  real    :: H,CR,RELHT,YHTG,BAA,SSITE,HTGR
  real    :: HTMAX,AGE1,AGE2,H1,H2
  integer :: IMETH(12),ISP
  data HRELHT/12*4.292/
  data HCRSQ/12*0.0566/
  data HHT/12*0.1699/
  data HBA/4*-0.00828,-0.54648,-0.00828,-0.78296,-0.58984,&
       &2*-0.00828,-0.54984, -0.00828/
  data HSITE/12*0.00768/
  data IMETH/1,1,1,1,2,1,2,2,1,1,2,3/
  data HCON/4*-2.193,3.560,-2.193,3.817,3.385,2*-2.193,&
       &3.385, -2.193/

  !  CALCULATE HTGR FOR SPECIES GROUP 2
  if (IMETH(ISP) .eq.2) then
     if (BAA .le.5.0)BAA = 5.0
     HTGR = EXP(HCON(ISP) + HBA(ISP)*ALOG(BAA))

     !  CALCULATE HTGR FOR SPECIES GROUP 1
  else if (IMETH(ISP) .eq.1) then
     HTGR = HCON(ISP) + RELHT*HRELHT(ISP) + HCRSQ(ISP)*CR*CR&
          &+ HHT(ISP) * H + HBA(ISP)*BAA + HSITE(ISP)*SSITE

     !  CALCULATE HTGR FOR COAST REDWOOD
  else
     HTMAX = (2.242202*SSITE)
     if (HTMAX-H.le.1.) then
        HTGR = 0.0

        !  CALCULATE CURRENT AGE AND AGE 5 YEARS HENCE
     else
        AGE1 = 1/-0.010742*&
             &LOG(1-((H)/2.242202/SSITE)**(1/0.919076))
        AGE2 = AGE1 + 5

        ! ESTIMATE H1 BASED ON AGE1 AND H2 BASED ON AGE2
        H1 = 2.242202*SSITE*(1-EXP(-0.010742*AGE1))**0.919076
        H2 = 2.242202*SSITE*(1-EXP(-0.010742*AGE2))**0.919076

        ! COMPUTE HEIGHT INCREMENT BASED ON SUBTRACTION OF H2 AND H1
        HTGR = H2 - H1
     end if
     if (DEBUG)write(JOSTND,*) "RW DEBUG"," H=", H, ' AGE1=', AGE1,&
          &" AGE2=", AGE2, " H1=", H1, " H2=", H2," SI=", SSITE,&
          &" HTGR=", HTGR
  end if

  !  ROUTINE FOR HARDWOODS

  if (HTGR .le.0.0)HTGR = 0.01
  YHTG = HTGR
  return
end
