!==================================!
!   Realign tree record pointers   !
!==================================!
! BASE $Id$
subroutine REASS
  implicit none

  !  **REASS** REALLIGNS THE POINTERS VECTORS IND1 AND IND, AND
  !  THE ARRAY ISCT FOLLOWING RECORD TRIPLING.  THE VECTOR IND2
  !  IS USED FOR WORK SPACE.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'

  ! Variables
  integer :: J,ISPC,I1,I2,I,K,ITFN,LAST,IKNT

  !  COPY IND1 INTO IND2 IN SPECIES ORDER.

  J = 0
  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 20
     I2 = ISCT(ISPC,2)
     do I = I1,I2
        J = J+1
        IND2(J) = IND1(I)
        10 continue
     end do
     20 continue
  end do

  !  REALLIGN THE POINTERS IN IND1.

  J = 1
  do K = 1,ITRN,3
     I = IND2(J)
     ITFN = IREC1+2*I-1
     IND1(K) = ITFN
     IND1(K+1) = I
     IND1(K+2) = ITFN+1
     J = J+1
     30 continue
  end do

  !  REALLIGN POINTERS TO THE SPECIES SEGMENTS IN IND1.

  LAST = 0
  do ISPC = 1,MAXSP
     if (ISCT(ISPC,1).eq.0) go to 40
     IKNT = ISCT(ISPC,2)-ISCT(ISPC,1)+1
     ISCT(ISPC,1) = LAST+1
     LAST = LAST+IKNT*3
     ISCT(ISPC,2) = LAST
     40 continue
  end do

  !  COPY IND INTO IND2.

  do I = 1,IREC1
     IND2(I) = IND(I)
     50 continue
  end do

  !  REALLIGN POINTERS IN IND.

  J = 1
  do K = 1,ITRN,3
     I = IND2(J)
     ITFN = IREC1+2*I-1
     IND(K) = ITFN
     IND(K+1) = I
     IND(K+2) = ITFN+1
     J = J+1
     60 continue
  end do
  return
end subroutine REASS
