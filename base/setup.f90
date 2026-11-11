!======================================!
!   Initialize tree species pointers   !
!======================================!
! BASE $Id$
subroutine SETUP
  implicit none

  !     SET UP THE INDEX POINTERS FOR IND1( SPECIES INDICES ).
  !     THE DEFINITION OF MOST VARIABLES OCCURES IN **INTREE**

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'

  ! Variables
  integer :: IX,LAST,I,IR,KNT,ISTART,NEXT,J

  if (IREC1.gt.0) goto 10
  ITRN = 0
  return
  10 continue
  IX = 1
  LAST = 0

  do I = 1, MAXSP

     IR = IREF(I)
     if ( IR .eq. 0 ) go to 300
     KNT = KOUNT(IR)
     ISCT(I,1) = LAST + 1
     LAST = LAST + KNT
     ISCT(I,2) = LAST
     ISTART = IBEGIN(I)
     IND1(IX) = ISTART
     IX = IX + 1
     if ( KNT .eq. 1 ) go to  300
     NEXT = IND2( ISTART )

     do J = 2, KNT

        IND1(IX) = NEXT
        IX = IX + 1
        if (J.eq. KNT) goto 299
        NEXT = IND2( NEXT )
        299 continue
     end do
     300 continue
  end do

  ITRN = LAST

  return
end subroutine SETUP
