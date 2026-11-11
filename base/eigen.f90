!==========================================!
!   Compute eigenvalues and eigenvectors   !
!==========================================!
! BASE $Id$
subroutine EIGEN(A,R,N,MV)
  implicit none

  !     FROM: SYSTEM/360 SCIENTIFIC SUBROUTINE PACKAGE VERSION III
  !     PROGRAMMERS MANUAL. IBM 1966.

  !     PARTS RECODED TO COMPLY TO REMOVED FORTRAN FEATURES BY
  !     NCROOKSTON JAN 2020

  !     ..................................................................

  !        SUBROUTINE EIGEN

  !        PURPOSE
  !           COMPUTE EIGENVALUES AND EIGENVECTORS OF A REAL SYMMETRIC
  !           MATRIX

  !        USAGE
  !           CALL EIGEN(A,R,N,MV)

  !        DESCRIPTION OF PARAMETERS
  !           A - ORIGINAL MATRIX (SYMMETRIC), DESTROYED IN COMPUTATION.
  !               RESULTANT EIGENVALUES ARE DEVELOPED IN DIAGONAL OF
  !               MATRIX A IN DESCENDING ORDER.
  !           R - RESULTANT MATRIX OF EIGENVECTORS (STORED COLUMNWISE,
  !               IN SAME SEQUENCE AS EIGENVALUES)
  !           N - ORDER OF MATRICES A AND R
  !           MV- INPUT CODE
  !                   0   COMPUTE EIGENVALUES AND EIGENVECTORS
  !                   1   COMPUTE EIGENVALUES ONLY (R NEED NOT BE
  !                       DIMENSIONED BUT MUST STILL APPEAR IN CALLING
  !                       SEQUENCE)

  !        REMARKS
  !           ORIGINAL MATRIX A MUST BE REAL SYMMETRIC (STORAGE MODE=1)
  !           MATRIX A CANNOT BE IN THE SAME LOCATION AS MATRIX R

  !        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED
  !           NONE

  !        METHOD
  !           DIAGONALIZATION METHOD ORIGINATED BY JACOBI AND ADAPTED
  !           BY VON NEUMANN FOR LARGE COMPUTERS AS FOUND IN 'MATHEMATICAL
  !           METHODS FOR DIGITAL COMPUTERS', EDITED BY A. RALSTON AND
  !           H.S. WILF, JOHN WILEY AND SONS, NEW YORK, 1962, CHAPTER 7

  !     ..................................................................

  ! Variables
  integer :: MV,N,IQ,J,I,IJ,IA,IND,L,M,MQ,LQ,LM,LL,MM,ILQ,IMQ,IM,IL
  integer :: ILR,IMR,JQ,K

  !        ...............................................................

  !        IF A DOUBLE PRECISION VERSION OF THIS ROUTINE IS DESIRED, THE
  !        C IN COLUMN 1 SHOULD BE REMOVED FROM THE DOUBLE PRECISION
  !        STATEMENT WHICH FOLLOWS.

  double precision :: A(*),R(*),ANORM,ANRMX,THR,X,Y,SINX,SINX2,COSX,COSX2,SINCS,RANGE,DN,ONE,TWO

  !        THE C MUST ALSO BE REMOVED FROM DOUBLE PRECISION STATEMENTS
  !        APPEARING IN OTHER ROUTINES USED IN CONJUNCTION WITH THIS
  !        ROUTINE.

  !        THE DOUBLE PRECISION VERSION OF THIS SUBROUTINE MUST ALSO
  !        CONTAIN DOUBLE PRECISION FORTRAN FUNCTIONS.  SQRT IN STATEMENTS
  !        40, 68, 75, AND 78 MUST BE CHANGED TO DSQRT.  ABS IN STATEMENT
  !        62 MUST BE CHANGED TO DABS. THE CONSTANT IN STATEMENT 5 SHOULD
  !        BE CHANGED TO 1.0D-12.

  !        ...............................................................
  data ONE/1.0D00/,TWO/2.0D00/

  !        GENERATE IDENTITY MATRIX

  !   5
  RANGE = 1.0D-12
  !   6
  DN = FLOAT(N)
  if (MV-1 .eq. 0) goto 25
  IQ = -N
  do J = 1,N
     IQ = IQ+N
     do I = 1,N
        IJ = IQ+I
        R(IJ) = 0.0
        if (I-J .eq. 0) R(IJ) = ONE
     end do
  end do

  !        COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)

  25 continue
  ANORM = 0.0
  do I = 1,N
     do J = I,N
        if (I-J .eq. 0) cycle
        IA = I+(J*J-J)/2
        ANORM = ANORM+A(IA)*A(IA)
     end do
  end do
  if (ANORM.le.0) goto 165
  ANORM = DSQRT(ANORM*TWO)
  ANRMX = ANORM*RANGE/DN

  !        INITIALIZE INDICATORS AND COMPUTE THRESHOLD, THR

  IND = 0
  THR = ANORM
  45 THR = THR/DN
  50 L = 1
  55 M = L+1

  !        COMPUTE SIN AND COS

  60 MQ = (M*M-M)/2
  LQ = (L*L-L)/2
  LM = L+MQ
  !  62
  if (DABS(A(LM))-THR .lt. 0) goto 130
  IND = 1
  LL = L+LQ
  MM = M+MQ
  X = (A(LL)-A(MM))/TWO
  !  68
  Y = -A(LM)/ DSQRT(A(LM)*A(LM)+X*X)
  if (X.le.0.) Y = -Y
  SINX = Y/ DSQRT(TWO*(ONE+( DSQRT(ONE-Y*Y))))
  SINX2 = SINX*SINX
  !  78
  COSX = DSQRT(ONE-SINX2)
  COSX2 = COSX*COSX
  SINCS = SINX*COSX

  !        ROTATE L AND M COLUMNS

  ILQ = N*(L-1)
  IMQ = N*(M-1)
  do I = 1,N
     IQ = (I*I-I)/2
     if (I-L .eq. 0 .or. I-M .eq. 0) goto 115
     if (I-M .gt. 0) goto 90
     IM = I+MQ
     go to 95
     90 IM = M+IQ
     95 if (I-L .ge. 0) goto 105
     IL = I+LQ
     go to 110
     105 IL = L+IQ
     110 X = A(IL)*COSX-A(IM)*SINX
     A(IM) = A(IL)*SINX+A(IM)*COSX
     A(IL) = X
     115 if (MV-1 .eq. 0) cycle
     ILR = ILQ+I
     IMR = IMQ+I
     X = R(ILR)*COSX-R(IMR)*SINX
     R(IMR) = R(ILR)*SINX+R(IMR)*COSX
     R(ILR) = X
  end do
  X = TWO*A(LM)*SINCS
  Y = A(LL)*COSX2+A(MM)*SINX2-X
  X = A(LL)*SINX2+A(MM)*COSX2+X
  A(LM) = (A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
  A(LL) = Y
  A(MM) = X

  !        TESTS FOR COMPLETION

  !        TEST FOR M = LAST COLUMN

  130 if (M-N .eq. 0) goto 140
  M = M+1
  go to 60

  !        TEST FOR L = SECOND FROM LAST COLUMN

  140 if (L-(N-1) .eq. 0) goto 150
  L = L+1
  go to 55
  150 if (IND-1 .ne. 0) goto 160
  IND = 0
  go to 50

  !        COMPARE THRESHOLD WITH FINAL NORM

  160 if (THR-ANRMX .gt. 0.) goto 45

  !        SORT EIGENVALUES AND EIGENVECTORS

  165 continue
  IQ = -N
  do I = 1,N
     IQ = IQ+N
     LL = I+(I*I-I)/2
     JQ = N*(I-2)
     do J = I,N
        JQ = JQ+N
        MM = J+(J*J-J)/2
        if (A(LL)-A(MM) .ge. 0) cycle
        X = A(LL)
        A(LL) = A(MM)
        A(MM) = X
        if (MV-1 .eq. 0) cycle
        do K = 1,N
           ILR = IQ+K
           IMR = JQ+K
           X = R(ILR)
           R(ILR) = R(IMR)
           R(IMR) = X
        end do
     end do
  end do
  return
end subroutine EIGEN
