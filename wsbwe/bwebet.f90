!====================!
!   wsbwe : bwebet   !
!====================!
! WSBWE $Id$
subroutine BWEBET (A0,B0,X,N,KODE)
  implicit none

  !     A0   = FIRST PARAMETER OF THE BETA DISTRIBUTION: A>0.
  !     B0   = SECOND PARAMETER: B>0.
  !     X    = A VECTOR OF BETA RANDOM VARIATES.
  !     N    = THE LENGTH OF X AND THE NUMBER OF RANDOM VARIATES REQUESTED.
  !     KODE = -1 FOR MIN(A,B)<0, OTHERWISE, KODE IS EQUAL TO THE NUMBER
  !            OF PASSES TO FIND ALL OF THE MEMBERS OF X.

  !     RETURNS N BETA RANDOM NUMBERS.

  !     REFERENCE: R.C.H. CHENG, 1978 ACM 21(4):317-322.

  !     N.L. CROOKSTON, FORESTRY SCIENCES LABORATORY, MOSCOW, ID, 4/1989.
  !     PART OF THE SPRUCE BUDWORM MODELING SYSTEM.

  !     TESTING THIS GENERATOR INDICATES A BIAS...A DIFFERENT GENERATOR
  !     NEEDS TO BE FOUND.

  ! Revision History:
  !  14-JUL-2010 Lance R. David (FMSC)
  !     Added IMPLICIT NONE and declared variables as needed.

  ! Variables
  integer :: I,KODE,N
  real    :: A0,B0,X(N)
  real    :: A,ALF,B,BET,C1,C2,DEL,GAM,R,S,T,U1,U2,V,W,Y,Z

  if (A0.le.0.00001.or. B0.le.0.00001) then
     KODE = -1
     goto 70
  else
     KODE = 0
     I = 0
  end if

  !     USE TWO DIFFERENT METHODS, DEPENDING ON THE VALUES OF A0 AND B0.

  if (AMIN1(A0,B0).gt.1.0) then
     A = AMIN1(A0,B0)
     B = AMAX1(A0,B0)
     ALF = A+B
     BET = SQRT((ALF-2.)/(2.*A*B-ALF))
     GAM = A+(1./B)
     10 continue
     KODE = KODE+1
     call BWERAN(U1)
     !        write (16,*) 'in bwebet: damage random number: ',u1    ! TEMP DEBUG
     call BWERAN(U2)
     !        write (16,*) 'in bwebet: damage random number: ',u2    ! TEMP DEBUG
     V = BET*ALOG(U1/(1.-U1))

     !        THE FOLLOWING LIMIT ON V INSERTED BY N.CROOKSTON

     if (V.gt.20.0) V = 20.0
     W = A*EXP(V)
     Z = U1*U1*U2
     R = GAM*V-1.3862944
     S = A+R-W
     if (S+2.609438.ge.5.*Z) goto 15
     T = ALOG(Z)
     if (S.ge.T) goto 15
     if (R+ALF*ALOG(ALF/(B+W)) .lt. T) goto 10
     15 continue
     I = I+1
     if (A.eq.A0) then
        X(I) = W/(B+W)
     else
        X(I) = B/(B+W)
     end if
     if (I.eq.N) goto 70
     goto 10

  else

     A = AMAX1(A0,B0)
     B = AMIN1(A0,B0)
     ALF = A+B
     BET = 1./B
     DEL = 1.+A-B
     C1 = DEL*(0.0138889+0.0416667*B)/(A*BET-0.777778)
     C2 = 0.25+(0.5+0.25/DEL)*B
     20 continue
     KODE = KODE+1
     call BWERAN(U1)
     !        write (16,*) 'in bwebet: damage random number: ',u1     ! TEMP DEBUG
     call BWERAN(U2)
     !        write (16,*) 'in bwebet: damage random number: ',u2     !TEMP DEBUG
     if (U1.ge.0.5) goto 30
     Y = U1*U2
     Z = U1*Y
     if (0.25*U2+Z-Y .ge. C1) then
        goto 20
     else
        goto 50
     end if
     30 continue
     Z = U1*U1*U2
     if (Z.le.0.25) then
        V = BET*ALOG(U1/(1.-U1))

        !           THE FOLLOWING LIMIT ON V INSERTED BY N.CROOKSTON

        if (V.gt.20.0) V = 20.0
        W = A*EXP(V)
        goto 60
     end if
     if (Z.ge.C2) goto 20
     50 continue
     V = BET*ALOG(U1/(1.-U1))

     !        THE FOLLOWING LIMIT ON V INSERTED BY N.CROOKSTON

     if (V.gt.20.0) V = 20.0
     W = A*EXP(V)
     if (ALF*(ALOG(ALF/(B+W))+V)-1.3862944.lt. ALOG(Z)) goto 20
     60 continue
     I = I+1
     if (A.eq.A0) then
        X(I) = W/(B+W)
     else
        X(I) = B/(B+W)
     end if
     if (I.eq.N) goto 70
     goto 20
  end if
  70 continue
  !     WRITE (16,*) 'IN BWEBET: KODE=',KODE,' X=',X(1),' U1,U2=',U1,U2
  return
end
