!============================================!
!   Sort indexed values in ascending order   !
!============================================!
! BASE $Id$
subroutine IQRSRT(LIST,N)
  implicit none

  !     THIS ASCENDING SORT IS AN FORTRAN ADAPTATION OF THE SORT
  !     DESCRIBED IN:

  !       SCOWEN, R.A. 1965. ALGORITHM 271; QUICKERSORT. COMM ACM.
  !                    8(11) 669-670.

  !     (THIS IS THE SAME TECHNIQUE USED IN RDPSRT)
  !     (THE CODE WAS SUPPLIED BY WSUCSC, PULLMAN WA.)

  ! Variables
  integer :: T,TT
  integer :: LIST(N)
  integer :: IU(33),IL(33)
  integer :: N,M,I,J,K,IJ,L
  M = 1
  I = 1
  J = N
  5 if (I.ge.J) goto 70
  10 K = I
  IJ = (I+J)/2
  T = LIST(IJ)
  if (LIST(I).le.T) goto 20
  LIST(IJ) = LIST(I)
  LIST(I) = T
  T = LIST(IJ)
  20 L = J
  if (LIST(J).ge.T) goto 40
  LIST(IJ) = LIST(J)
  LIST(J) = T
  T = LIST(IJ)
  if (LIST(I).le.T) goto 40
  LIST(IJ) = LIST(I)
  LIST(I) = T
  T = LIST(IJ)
  goto 40
  30 LIST(L) = LIST(K)
  LIST(K) = TT
  40 L = L-1
  if (LIST(L).gt.T) goto 40
  TT = LIST(L)
  50 K = K+1
  if (LIST(K).lt.T) goto 50
  if (K.le.L) goto 30
  if (L-I.le.J-K) goto 60
  IL(M) = I
  IU(M) = L
  I = K
  M = M+1
  goto 80
  60 IL(M) = K
  IU(M) = J
  J = L
  M = M+1
  goto 80
  70 M = M-1
  if (M.le.0) return
  I = IL(M)
  J = IU(M)
  80 if (J-I.ge.11) goto 10
  if (I.eq.1) goto 5
  I = I-1
  90 I = I+1
  if (I.eq.J) goto 70
  T = LIST(I+1)
  if (LIST(I).le.T) goto 90
  K = I
  100 LIST(K+1) = LIST(K)
  K = K-1
  if (T.lt.LIST(K)) goto 100
  LIST(K+1) = T
  goto 90
end subroutine IQRSRT
