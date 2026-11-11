!=================!
!   cr : badist   !
!=================!
! CR $Id$
subroutine BADIST(DEBUG)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'GGCOM.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,J,ISPC,ICLS,K
  real    :: TOTBA,TDBH,TREEBA

  ! Initialize variables
  do I = 1,MAXSP
     TBA(I) = 0.0
     do J = 1,41
        if (I .eq.1) BAU(J) = 0.0
        BCLAS(I,J) = 0.0
        TCLAS(I,J) = 0.0
        5 continue
     end do
     10 continue
  end do
  TOTBA = 0.0
  SEEDS = 0.0
  TPAT = 0.0

  !  COMPUTE BA BY DBH CLASS, BA BY SPECIES, AND BA BY DBH CLASS BY
  !  SPECIES.

  do I = 1,ITRN
     ISPC = ISP(I)
     if (LSTART) then
        TDBH = WK3(I)
     else
        TDBH = DBH(I)
     end if
     ICLS = IFIX(TDBH + 1.0)
     if (ICLS .gt.41) ICLS = 41
     if (TDBH .lt.1.0) TDBH = 1.0
     if (HT(I) .lt.4.5) then
        SEEDS = SEEDS + PROB(I)
        go to 100
     end if
     TREEBA = 0.0054542 * TDBH * TDBH * PROB(I)
     BCLAS(ISPC,ICLS) = BCLAS(ISPC,ICLS) + TREEBA
     TOTBA = TOTBA + TREEBA
     TBA(ISPC) = TBA(ISPC) + TREEBA
     BAU(ICLS) = BAU(ICLS) + TREEBA
     TCLAS(ISPC,ICLS) = TCLAS(ISPC,ICLS) + PROB(I)
     TPAT = TPAT + PROB(I)
     100 continue
  end do

  ! BAU NOW CONTAINS BY BY CLASS, CONVERT TO BA ABOVE CLASS.

  BAU(1) = TOTBA - BAU(1)
  if (BAU(1) .lt.0.0) BAU(1) = 0.0
  do I = 2,41
     K = I-1
     BAU(I) = BAU(K) - BAU(I)
     if (BAU(I) .lt.0.0) BAU(I) = 0.0
     150 continue
  end do
  if (DEBUG) then
     write(JOSTND,*)' IN BADIST TOTBA =',TOTBA
     write(JOSTND,*)' I,BCLAS(I, ),BAU(I) I=1,MAXSP BY DBH CLASS ='
     do I = 1,41
        write(JOSTND,155)I,(BCLAS(J,I),J = 1,MAXSP),BAU(I)
        155 format(' ',I3,4(12F10.3,/' '))
        160 continue
     end do
     write(JOSTND,165)TBA
     165 format(' TBA BY SPECIES =',10(11F10.4,/' ',16X))
  end if

  return
end
