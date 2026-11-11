!===================!
!   dftm : gfcomp   !
!===================!
! DFTM $Id$
subroutine GFCOMP
  implicit none

  !     DFTM MODEL SUBROUTINE - JIM COLBERT - JAN 1978.

  !  REVISION HISTORY:
  !    01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'LOWER.f90'
  include 'LIMITS.f90'

  ! Variables
  integer :: I,N,NN
  real    :: ALPHA,UV1,ETA,ETAPLS,UV2,G19TOT,G17TOT,PHI

  G1(6) = 0.0
  if (KP .gt.0.and. KP .le.6) G1(6) = (1.0 - R1(KP)) *&
       &(1.0 - R1(KP + 6)) * (1.0 - B1(KP))

  G1(7) = 0.0
  if (KP .gt.0.and. KP .le.6) G1(7) = 1.0 - R1(KP + 12)

  G1(12) = 0.0
  !     IF (X1(3) .LE. 0.0 .OR. X1(1) + X1(2) .LE. 0.0) GO TO 991
  if (KP .ge.1.and. KP .lt.4) G1(12) = B1(22)
  if (KP .eq.4) G1(12) = B1(23)
  if (KP .gt.4.and. KP .le.6) G1(12) = B1(24)

  991 continue
  G1(14) = AMAX1(2.0 * X1(4) / B1(26) - 1.0, 0.0)
  if (KP .eq.0.or. KP .gt.6) go to 15
  if (X1(1) .le.0.0) go to 15

  do I = 1,11
     ALPHA = FLOAT(11-I)
     G1(15) = ALPHA
     if (UV1(KP, X1, B1, ALPHA, G1) .lt. X1(1)) goto 20
     10 continue
  end do
  go to 20

  15 G1(15) = 0.0

  20 continue
  G1(16) = 0.0
  if (KP .eq.0.or. KP .gt.6) goto 30
  ALPHA = AMIN1(G1(15) + 1.0, 10.0)
  if (X1(1) .gt.0.0) G1(16) = UV1(KP,X1,B1,ALPHA,G1)

  30 continue
  if ((X1(1) + X1(2)) .le.0.0) goto 920
  if (KP .eq.0.or. KP .gt.6) goto 920
  G1(17) = 10.0 - G1(15)
  ALPHA = G1(15)
  N = IFIX (10.0 - G1(15))
  NN = N + 1

  do I = 1,NN
     ETA = FLOAT(I-1)
     ETAPLS = ETA + 1.
     if ((UV2(G1, KP, X1, B1, ALPHA, ETAPLS) - (X1(1) -&
          &UV1(KP, X1, B1, ALPHA, G1))) .gt. X1(2)) go to 915
     910 continue
  end do
  go to 40

  915 G1(17) = ETA
  go to 40

  920 G1(17) = 0.0

  40 continue
  G1(18) = 0.0
  if (KP .eq.0.or. KP .gt.6) go to 50
  ALPHA = G1(15)
  ETA = 10.0 - ALPHA
  G1(18) = UV2(G1, KP, X1, B1, ALPHA, ETA) -&
       &(X1(1) - UV1(KP, X1, B1, ALPHA, G1))
  if (G1(18) .lt.0.0) G1(18) = 0.0

  50 continue
  if (KP .eq.0.0) G19TOT = 0.0
  if (KP .gt.0.and. KP .le.6) G19TOT = G19TOT +&
       &(10.0 - (G1(15) + G1(17)))
  G1(19) = G19TOT

  G1(1) = 0.0
  if (KP .eq.8) G1(1) = (Z1(2) * Z1(1) / 100.0) *&
       &((X1(1) + X1(2) + Z1(2)) / (2.0 * Z1(2)) - G1(19) * B1(27))

  G1(3) = 0.0
  if (KP .eq.8) G1(3) = X1(1)

  G1(4) = 0.0
  if (KP .gt.0.and. KP .le.6) G1(4) = AMIN1(G1(16), X1(1))

  G1(5) = 0.0
  if (KP .ge.1.and. KP .le.6) G1(5) = AMIN1(G1(18), X1(2))
  G1(8) = 0.0

  if (KP .eq.0.or. KP .gt.6) go to 99
  G1(8) = G1(6)**10.0 - 1.0
  if (G1(15) .ne.10.0.and. G1(7) .ne.0.0) G1(8) = (G1(6)**10.0) *&
       &(G1(7)**(10.0 - G1(15))) - 1.0
  if (G1(7) .eq.0.0.and. G1(15) .ne.10.0) G1(8) = -1.0

  99 continue
  if (KP .eq.0.or. KP .eq.9) G17TOT = 0.0
  G17TOT = G17TOT + G1(17)
  G1(9) = G17TOT

  G1(10) = 0.0
  if (KP .eq.0) G1(10) = -B1(36)
  if (KP .eq.5) G1(10) = -(1.0 + G1(8)) * B1(30)
  if (KP .eq.7) G1(10) = -B1(34)
  if (KP .eq.8) G1(10) = (B1(35) * G1(14) * (1.0 - B1(28) *&
       &G1(9)) - 1.0)

  G1(11) = (G1(8) + G1(10)) * X1(3)

  G1(13) = 0.0
  if (KP .eq.0) G1(13) = B1(25)

  !     IF (G1(4) .LE. 0.0 .OR. G1(18) .LE. 0.0) PRINT 997, KP,
  !    +   G1(4), G1(5), G1(18)
  ! 997 FORMAT(' DEBUG 997:',I5,3(2X,G10.4))

  if ((G1(4) + G1(18)) .gt.0.0) PHI = (G1(4) + G1(5)) /&
       &(G1(4) + G1(18))
  if (X1(3) .le.0.0.or. X1(1) + X1(2) .le.0.0) G1(12) = 0.0
  if (KP .gt.0.and. KP .le.6) G1(13) = X1(4) *&
       &(EXP(10.0 * G1(12) * PHI) - 1.0)

  F1(1) = (G1(1) - G1(4) - G1(3))
  F1(2) = (G1(3) - G1(5))
  F1(3) = G1(11)
  F1(4) = G1(13)

  return
end
