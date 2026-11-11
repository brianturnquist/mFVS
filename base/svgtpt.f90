!=================================!
!   Stand visualization: SVGTPT   !
!=================================!
! BASE $Id$
subroutine SVGTPT (IPGLEM,X1R1,X2R2,Y1A1,Y2A2,X,Y,IMETRIC)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999
  !     D. ROBINSON   -- ESSA        -- MAY 2005

  !     GET A RANDOM POINT IN THE REGION DEFINED BY X1R1,X2R2,Y1A1,Y2A2

  ! Variables
  integer :: IPGLEM,IMETRIC
  real    :: X1R1,X2R2,Y1A1,Y2A2,X,Y,A,R

  if (IPGLEM.lt.2) then
     call SVRANN (X)
     X = X1R1 + ((X2R2-X1R1)*X)
     call SVRANN (Y)
     Y = Y1A1 + ((Y2A2-Y1A1)*Y)
  else
     call SVRANN (A)
     A = Y1A1 + ((Y2A2-Y1A1)*A)
     call SVRANN (R)
     R = SQRT(X1R1*X1R1 + R*(X2R2*X2R2-X1R1*X1R1))

     if (IMETRIC.eq.0) then
        X = COS(A)*R + 117.7522
        Y = SIN(A)*R + 117.7522
     else
        X = COS(A)*R + 56.42
        Y = SIN(A)*R + 56.42
     end if

  end if
  return
end subroutine SVGTPT
