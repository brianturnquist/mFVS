!===================!
!   dftm : dftmod   !
!===================!
! DFTM $Id$
subroutine DFTMOD (ISTART,IFIN)
  implicit none

  !     *** DOUGLAS-FIR TUSSOCK MOTH INSECT AND BRANCH MODEL ***
  !     DEVELOPED BY SCOTT OVERTON, JIM COLBERT AND CURTIS WHITE
  !     AT OREGON STATE UNIVERSITY, CORVALLIS, OREGON.

  !     MODIFIED FOR USE IN THE PROGNOSIS MODEL BY JIM COLBERT,
  !     JAN. 1978

  !  REVISION HISTORY:
  !    01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'TMCOM1.f90'
  include 'UPPER.f90'
  include 'LOWER.f90'
  include 'LIMITS.f90'
  include 'DFOL.f90'
  include 'ICOND.f90'
  include 'GPASS.f90'

  ! Variables
  integer :: IFIN,ISTART,I
  integer :: K1,K2,IC1,IC2,K3,IC3,IX

  !                  K(1):  BEGINNING PHASE
  !                  K(2):  LAST PHASE
  !                  K(3):  PHASE COUNTER
  !                  IC(1): FIRST TREE CLASS
  !                  IC(2): LAST TREE CLASS
  !                  IC(3): TREE CLASS COUNTER

  !                 SET INITIAL K AND IC VAR.

  K(1) = 1
  K(2) = 3
  IC(1) = ISTART
  IC(2) = IFIN
  ICOUNT = IC(2)-IC(1)+1

  !     SUPPRESS REDISTRIBUTION IF LREDIS IS FALSE

  B0(57) = ICOUNT

  !     WRITE PARAMETER FILE TO JOTMDK

  if (.not. LPUNCH) go to 67
  write (JOTMDK,65)
  65 format (' LIST OF DFTMOD PARAMETERS:')
  write (JOTMDK,66) B0,R0,(B1(I),I = 1,30)
  66 format (6F12.7)

  67 continue

  !                  START SIMULATION

  do I = 1,100
     DPCENT(I,1) = 0.0
     DPCENT(I,2) = 0.0
     3 continue
  end do

  K1 = K(1)
  K2 = K(2)
  IC1 = IC(1)
  IC2 = IC(2)

  do K3 = K1,K2
     K(3) = K3

     !       PROCESS G FUNCTIONS FOR UPPER MODULE

     call G0COMP

     !       PROCESS LOWER MODULE FOR DESIRED TREE CLASSES

     do IC3 = IC1,IC2
        IC(3) = IC3
        ICOUNT = IC3 - IC1 + 1

        !         CALL ROUTINE TO SET UP PARAMETERS

        call Z1COMP

        !         DO LOWER LEVEL PROCESSING IN MODULE S(1)

        do I = 1,10

           !           SET LOWER LEVEL TIME INDEX, KP

           KP = I - 1

           !           PROCESS G AND F FUNCTIONS IN MODULE S(1)

           call GFCOMP

           !           UPDATE STATE VARIABLES IN S(1)

           do IX = 1,4
              X1(IX) = X1(IX) + F1(IX)

              !             CHECK FOR NEGATIVE X VALUES

              if (X1(IX) .ge.0.0) go to 6

              !             PRINT MESSAGE AND RESET X

              if (TMDEBU) write (JODFTM,1000) IX,X1(IX),I,IC(3),K(3)
              1000 format ('0X(',I1,')=',F14.6,' AT OCCASION ',I2,&
                   &' FOR TREE CLASS',I3,' IN PHASE',I2/&
                   &' RESET TO 0.0')
              X1(IX) = 0.0
              6 continue
           end do
           10 continue
        end do

        !         FINISH UPPER LEVEL PROCESSING IN MODULE S(1)

        call Y1COMP
        50 continue
     end do
     if (K3.eq.2.or. K3.eq.3) call DFOLE8
     100 continue
  end do

  !     FINISH UPPER LEVEL PROCESSING IN MODULE S(0)

  call Y0COMP

  if (.not. TMDEBU) return
  write (JODFTM,9992)
  9992 format('0   ***PERCENT BRANCH  DEFOLIATION FROM DFTMOD***'//&
       &'     JCLASS       IZ6   DPCENT1   DPCENT2'//)

  do I = 1,ICOUNT
     write (JODFTM,9991) I, IZ6(I), DPCENT(I,1), DPCENT(I,2)
     999 continue
  end do
  9991 format(1X,2I10,2F10.4)

  return
end
