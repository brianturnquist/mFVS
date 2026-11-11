!=============================!
!   Apply growth increments   !
!=============================!
! BASE $Id$
subroutine UPDATE
  implicit none

  !  UPDATE:
  !     ADDS GROWTH INCREMENT
  !     DEDUCTS MORTALITY
  !     COMPUTES VOLUME STATISTICS
  !     EXECUTES SOME OF THE SUMMARY LOGIC.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'
  include 'OUTCOM.F77'
  include 'PLOT.F77'

  ! Variables
  logical :: DEBUG
  real    :: SPCMO(MAXSP,3),WKI,BRATIO
  integer :: I,IS,I1,I2,J,IM

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'UPDATE',6,ICYC)

  !  ZERO OUT MORTALITY ACCUMULATOR ARRAYS

  do I = 1,7
     OMORT(I) = 0.0
     5 continue
  end do
  do I = 1,MAXSP
     SPCMO(I,1) = 0.0
     SPCMO(I,2) = 0.0
     SPCMO(I,3) = 0.0
     10 continue
  end do

  !  DO FOR ALL TREES BY SPECIES.

  do IS = 1,MAXSP
     I1 = ISCT(IS,1)
     if (I1 .eq. 0) go to 100
     I2 = ISCT(IS,2)
     do J = I1,I2
        I = IND1(J)

        !  ADD THE HEIGHT INCREMENT TO THE HEIGHTS

        HT(I) = HT(I)+HTG(I)
        if (NORMHT(I).gt.0) &
             & NORMHT(I) = INT(real(NORMHT(I))+(HTG(I)*100.+.5))

        !  DEDUCT THE MORTALITY FROM PROB AND PREPARE THE SUMMARY.

        IM = IMC(I)
        WKI = WK2(I)
        if (WKI.gt.PROB(I)) WKI = PROB(I)
        WK6(I) = WKI*CFV(I)/FINT
        SPCMO(IS,IM) = SPCMO(IS,IM)+WK6(I)
        PROB(I) = MAX(0.,PROB(I)-WKI)

        !  END OF TREE WITHIN SPECIES LOOP.

        90 continue
     end do
     if (.not.DEBUG) go to 100
     write(JOSTND,9001)  IS,(SPCMO(IS,J),J = 1,3)
     9001 format(' IN UPDATE,  SPECIES=',I3, &
          & ',  CUM. MORT. VOL. BY TREE CLASS=',3(F8.3,','))

     !  END OF SPECIES LOOP.

     100 continue
  end do

  !  CALL **PCTILE** TO LOAD WK3 WITH PERCENTILES IN THE DISTRIBUTION
  !  OF VOLUME MORTALITY IN DESCENDING ORDER OF DIAMETER.

  call PCTILE(ITRN,IND,WK6,WK3,OMORT(7))

  !  FIND THE PERCENTILE POINTS IN THE DISTRIBUTION OF MORTALITY
  !  VOLUME.

  call DIST(ITRN,OMORT,WK3)

  !  COMPUTE SPECIES TREE CLASS COMPOSITION CORRESPONDING TO OMORT.

  call COMP(OSPMO,IOSPMO,SPCMO)

  !  COMPUTE TREE AND STAND VOLUME STATISTICS.

  if (DEBUG) write(JOSTND,9017) ICYC
  9017 format(' CALLING VOLS, CYCLE=',I2)
  call VOLS

  !  UPDATE DIAMETERS TO END OF CYCLE VALUES.

  if (ITRN.eq.0) return
  do I = 1,ITRN
     IS = ISP(I)
     DBH(I) = DBH(I)+DG(I)/BRATIO(IS,DBH(I),HT(I))
     110 continue
  end do
  return
end subroutine UPDATE
