!================================================!
!   Predict the number of regenerating species   !
!================================================!
! ESTB $Id$
subroutine ESNSPE (PSPE,TPPLN,ISER,ITPP,TPP)
  implicit none

  !     PREDICT PROBS FOR NUMBER OF SPECIES ON STOCKED PLOTS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'ESCOM2.f90'
  include 'PLOT.f90'

  ! Variables
  integer :: ITPP,ISER
  real    :: TPP,PSPE(6),PN,TPPLN

  !     P(1 SPECIES ON STOCKED PLOTS) (PLANTING COEFS NOT INCLUDED.)

  PN = 1.399594 +.002162*BAA +.0213903*ELEV -.022173*REGT&
       &-.039405*BWAF -1.017904*TPPLN +SPEHAB(ISER,1)
  PSPE(1) = 1.0/ (1.0+EXP(-PN))

  !     P(2 SPECIES ON STOCKED PLOTS)

  if (ITPP.lt.2) go to 78
  PN = -.441879+.577760*XCOS+.294070*XSIN -.128766*SLO -.010128*TPP&
       &-.011040*ELEV +.016568*REGT -.000247*BWAF +SPEHAB(ISER,2)
  PSPE(2) = 1.0/ (1.0+EXP(-PN))

  !     P(3 SPECIES ON STOCKED PLOTS)

  if (ITPP.lt.3) go to 78
  PN = -2.052740 +.075063*XCOS -.471162*XSIN -.840378*SLO&
       &-.018585*ELEV +.076352*BWAF +.004745*REGT&
       &+SPEHAB(ISER,3) +.054661*TPP -.000467*TPP*TPP
  PSPE(3) = 1.0/ (1.0+EXP(-PN))

  !     P(4 SPECIES ON STOCKED PLOTS)

  if (ITPP.lt.4) go to 78
  PN = -6.24551 -1.277404*XCOS -.329897*XSIN +.425239*SLO&
       &+.071878*BWAF +.001371*REGT&
       &+SPEHAB(ISER,4) +.056784*TPP -.000239*TPP*TPP
  PSPE(4) = 1.0/ (1.0+EXP(-PN))

  !     P(5 SPECIES ON STOCKED PLOTS)

  if (ITPP.lt.5) go to 78
  PN = -2.043228 -.052907*ELEV +.021884*TPP
  PSPE(5) = 1.0/ (1.0+EXP(-PN))
  if (ITPP.lt.6) go to 78
  PN = -5.938035 +.020070*TPP
  PSPE(6) = 1.0/ (1.0+EXP(-PN))
  78 continue
  return
end
