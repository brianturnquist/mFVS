!============================================!
!   Process tree damage and severity codes   !
!============================================!
! BASE $Id$
subroutine DAMPRO
  implicit none

  !  THIS SUBROUTINE CONTROLS THE PROCESSING DAMAGE AND SEVERITY CODES
  !  STORED WHILE READING THE INPUT TREE RECORDS. DAMAGE CODE PROCESSING
  !  ROUTINES OF EXTENSIONS THAT UTILIZE DAMAGE AND SEVERITY CODES ARE
  !  CALLED HERE (PREVIOUSLY THEY WERE CALLED FROM SUBROUTINE INTRE).
  !  CREATED: 07/06/2007 LANCE R. DAVID

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'CONTRL.F77'

  ! Variables
  integer :: I1,I2,II,I
  integer :: ICODES(6)

  continue

  !      WRITE(JOSTND,*) ' IN DAMPRO: IRECRD=',IRECRD,' ITRN=',ITRN,
  !     > ' IREC1=',IREC1,' IREC2=',IREC2

  !     Set looping indices to process live tree records.
  !     Indices for dead trees will be set and processed next.

  I1 = 1
  I2 = IREC1

  100 continue
  !      WRITE(JOSTND,*) ' IN DAMPRO: I1=',I1,' I2=',I2

  do II = I1, I2
     do I = 1,6
        ICODES(I) = DAMSEV(I,II)
     end do
     !       DAMAGE CODE 54 IS USED TO SET ACADIAN FORM AND RISK CODE
     do I = 1,5,2
        if (ICODES(I).eq.54) ISPECL(II) = ICODES(I+1)
     end do

     !        WRITE(JOSTND,*) 'IN DAMPRO: ICODES(I) =    ',(ICODES(I),I=1,6),
     !     &  ' IDTREE, ISP, DBH =',IDTREE(II),ISP(II),DBH(II),
     !     &  ' IMC = ',IMC(II)

     call MISDAM (II,ICODES)
     call RDDAM  (II,ICODES)
     call TMDAM  (II,ICODES)
     call MPBDAM (II,ICODES)
     call DFBDAM (II,ICODES)
     call BRDAM  (II,ICODES)
     call BMDAM  (II,ICODES)
  end do

  !     If dead trees are present, set indices and process.

  if (I1 .eq. 1 .and. IREC2 .ne. (MAXTRE + 1)) then
     I1 = IREC2
     I2 = MAXTRE
     go to 100
  end if

  !     write(JOSTND,*) 'EXIT ROUTINE DAMPRO'

  return
end subroutine DAMPRO
