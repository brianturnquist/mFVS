!====================!
!   lpmpb : grclas   !
!====================!
! LPMPB $Id$
subroutine GRCLAS (NRECS,IPT,ATR,ISC,NCLAS,NATR,CLAS,&
     &PROB,IMPROB,IPOS,MAXCLS,ITWO)
  implicit none

  !     CALLED FROM 'GARBEL'   N.L. CROOKSTON  JAN 1978 & JAN 1981

  !          NRECS = LENGTH OF POINTER ARRAY
  !          IPT   = POINTER ARRAY
  !          NCLAS = NUMBER OF CLASSES
  !          CLAS  = CLASSIFIED ARRAY (ONE WHICH HOLDS CLASSIFIED DATA)
  !          PROB  = WEIGHT OF EACH MEMBER OF ATR
  !          IPOS  = THE POSITION WITHIN 'CLAS' OF THIS ATTRIBUTE
  !          IMPROB= POSITION WITHIN CLAS OF PROB
  !          NATR  = NUMBER OF ATTRIBUTES (NEEDED FOR OBJECT
  !                  TIME DIMENSIONING OF CLAS.)
  !          ISC   = SECTOR POINTERS TO BE USED WITH 'IPT'

  ! Revision History
  !   04/03/02 Lance R. David (FHTET)
  !     Arrays dimensioned "1" changed to "*" so that array size is
  !     inherited from calling routine.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  integer :: IPOS,IMPROB,NCLAS,NRECS,MAXCLS,ITWO,ISC(MAXCLS,ITWO),NATR,IPT(*),I3,J,I1,I2,I,II
  real    :: CLAS(MAXCLS,NATR),PROB(*),ATR(*),XP

  I3 = IPOS
  do J = 1, NCLAS
     I1 = ISC(J,1)
     I2 = ISC(J,2)
     CLAS(J,I3) = 0.0

     do II = I1, I2
        I = IPT(II)
        CLAS (J,I3) = CLAS (J,I3) + ATR(I) * PROB(I)
        10 continue
     end do

     XP = CLAS (J,IMPROB)
     if ( XP .gt.1.E-30 ) CLAS (J,I3) = CLAS (J,I3) / XP
     20 continue
  end do

  return
end
