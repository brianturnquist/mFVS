!=====================!
!   vbase : mortalloc !
!=====================!
subroutine MORTALLOC(TOKILL,DEBUG,SUMKIL,PASS1,TEMKIL,JPASS,EFFTR,TEMWK2)
  implicit none

  ! Distribute mortality using efficiencies prepared by the variant caller.
  ! The caller retains background mortality and initialization in their
  ! original order. Preserve the allocation arithmetic and stopping rules.
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  real    :: TOKILL,SUMKIL,PASS1,TEMKIL,EFFTR(MAXTRE),TEMWK2(MAXTRE)
  real    :: SHORT,ADJUST,TPALFT,OTEM2,TEMSUM,XKILL
  integer :: I,IG,JPASS,NPASS,MINSTP,ISWTCH
  logical :: DEBUG

  ! PASS1 is the caller's first-pass mortality total. ADJUST scales the
  ! current guess; SHORT carries mortality left after a record saturates.
  ! TPALFT is the record's remaining TPA. Debug formats are kept verbatim.

  NPASS = IFIX(TOKILL/PASS1)+1
  if (DEBUG)write(JOSTND,*)' APPROXIMATE NUMBER OF PASSES NEEDED = ',&
       &NPASS

  !  LOOP THROUGH THE TREES AND LOAD THE FIRST GUESS AT TREE RECORD
  !  MORTALITY BASED ON THE MORTALITY EFFICIENCY (r) AND APPROXIMATE NUMBER
  !  OF PASSES NEEDED (n). THIS IS A GEOMETRIC PROGRESSION WHERE THE RATE
  !  IS THE MORTALITY EFFICIENCY (r) AND THE STARTING VALUE IS THE INITIAL
  !  PROB VALUE (a). THE Nth TERM IN THE PROGRESSION IS a*r*(1-r)**(n-1)
  !  AND THE SUM OF N TERMS IS -a*((1-r)**n - 1).
  !  ACCUMULATE THE MORTALITY ACHIEVED WITH THIS FIRST PASS.

  100 continue
  JPASS = JPASS+1
  if (JPASS .gt.1)TEMKIL = SHORT
  ISWTCH = 0
  105 continue
  TEMSUM = 0.
  do I = 1,ITRN
     TPALFT = PROB(I)-WK2(I)
     if (TPALFT .gt.0.) then
        OTEM2 = TEMWK2(I)
        TEMWK2(I) = (-TPALFT*((1.0-EFFTR(I))**NPASS - 1.0))
        if (DEBUG)write(JOSTND,*)' I,PROB,WK2,TPALFT,EFFTR,TEMWK2,',&
             &'OTEM2= ',I,PROB(I),WK2(I),TPALFT,EFFTR(I),TEMWK2(I),OTEM2
        TEMSUM = TEMSUM+TEMWK2(I)
     end if
  end do
  if (DEBUG)write(JOSTND,*)' AFTER GUESS ',JPASS,' TEMSUM= ',TEMSUM,&
       &'  TOKILL= ',TOKILL

  ! ADJUST MORTALITY VALUES TO ACHIEVE DESIRED MORTALITY.
  ! IF AN ENTIRE TREE RECORD PROB GETS KILLED, ADJUST PASS1 VALUE FOR
  ! THE NEXT PASS.

  if (NPASS .gt.50) then
     MINSTP = 5
  else if (NPASS .gt.20) then
     MINSTP = 2
  else
     MINSTP = 1
  end if
  ADJUST = TEMKIL/TEMSUM
  if (ADJUST .lt.0.8) then
     if (ISWTCH .eq.2) go to 110
     if (DEBUG)write(JOSTND,*)' TEMKIL,TEMSUM,PASS1,NPASS= ',&
          &TEMKIL,TEMSUM,PASS1,NPASS
     NPASS = NPASS-MAX(MINSTP,IFIX((TEMSUM-TEMKIL)/PASS1))
     if (DEBUG)write(JOSTND,*)' ADJUST= ',ADJUST,'  IS TO SMALL,',&
          &' MIN STEP= ',MINSTP,' NEW NPASS= ',NPASS
     ISWTCH = 1
     if (NPASS .le.0)go to 110
     go to 105
  else if (ADJUST .gt.1.2) then
     if (ISWTCH .eq.1) go to 110
     NPASS = NPASS+MAX(MINSTP,IFIX((TEMKIL-TEMSUM)/PASS1))
     if (DEBUG)write(JOSTND,*)' ADJUST= ',ADJUST,'  IS TO BIG,',&
          &' MIN STEP= ',MINSTP,' NEW NPASS= ',NPASS
     ISWTCH = 2
     go to 105
  end if
  110 continue
  SHORT = 0.
  if (DEBUG)write(JOSTND,*)' TEMKIL= ',TEMKIL,'  TEMSUM= ',&
       &TEMSUM,'  ADJUSTMENT= ',ADJUST
  do I = 1,ITRN
     TPALFT = PROB(I)-WK2(I)
     if (TPALFT .lt.0.00001)go to 150
     XKILL = TEMWK2(I)*ADJUST
     if ((PROB(I)-WK2(I)-XKILL) .le.0.00001) then
        TEMWK2(I) = PROB(I)-WK2(I)
        if (DEBUG)write(JOSTND,*)' SHORT,I,XKILL,PROB,WK2= ',&
             &SHORT,I,XKILL,PROB(I),WK2(I)
        SHORT = SHORT+(XKILL-PROB(I)+WK2(I))
        if (DEBUG)write(JOSTND,*)' SHORT= ',SHORT
        PASS1 = PASS1-EFFTR(I)
        go to 140
     end if
     TEMWK2(I) = XKILL
     140 continue
     WK2(I) = WK2(I)+TEMWK2(I)
     SUMKIL = SUMKIL+TEMWK2(I)
     150 continue
  end do
  if (DEBUG)write(JOSTND,23)ITRN,(WK2(IG),IG = 1,ITRN)
  23 format(' ADJUSTED MORTALITY VALUES, ITRN = ',I7,&
       &(/10F10.5))
  if (DEBUG)write(JOSTND,*)' SHORT = ',SHORT
  if (SHORT .gt.0.) then
     NPASS = IFIX(SHORT/PASS1)+1
     if (DEBUG)write(JOSTND,*)' SHORT,PASS1, ADJUSTED PASSES NEEDED',&
          &'= ',SHORT,PASS1,NPASS
     go to 100
  end if

  return
end
