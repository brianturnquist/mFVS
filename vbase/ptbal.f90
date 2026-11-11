!===================!
!   vbase : ptbal   !
!===================!
! VBASE $Id: ptbal.f 2438 2018-07-05 16:54:21Z gedixon $
subroutine PTBAL
  implicit none

  !     COMPUTES THE POINT BASAL AREA IN LARGER TREES.  THIS IS A
  !     TREE ATTRIBUTE.  CALLED FROM DENSE.

  !     N.L.CROOKSTON, INT-MOSCOW, MARCH 1991.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'

  ! Variables
  logical :: DEBUG

  integer :: I,II,IP,N,NP

  integer :: IPNDX(MAXPLT)

  real    :: XBALT

  equivalence (WK6,IPNDX)

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'PTBAL',5,ICYC)
  if (DEBUG) write (JOSTND,1) ICYC,ITRN,VARACD
  1 format(' ENTERING SUBROUTINE PTBAL CYCLE=',I5,&
       &'; ITRN=',I5,'; VARACD=',A2)

  select case (VARACD)

     !  EASTERN TWIGS AND CANADIAN ONTARIO VARIANTS
     !  POINT BASAL AREA NOT USED; SET ARRAYS TO ZERO.

  case ('CS','LS','NE','ON')
     PTBALT = 0.
     PTBAA = 0.
     if (DEBUG) write(JOSTND,10)
     10 format(' EASTERN TWIGS VARIANT, PBA VALUES ZERO, LEAVING PTBAL')
     return

     !  WESTERN, SOUTHERN, AND CANADIAN BRITISH COLUMBIA VARIANTS

  case default
     if (ITRN.le.0) then
        PTBALT = 0.
        PTBAA = 0.
        if (DEBUG) write(JOSTND,20)
        20 format(' WESTERN/SOUTHERN VARIANT, NO TREES, PBA VALUES ',&
             &'ZERO, LEAVING PTBAL')
        return
     end if

     !  FIND THE MAXIMUM VALUE OF ITRE...THAT WILL BE TAKEN AS THE
     !  ACTUAL POINT COUNT.

     NP = -1
     do I = 1,ITRN
        II = IND(I)
        if (ITRE(II).gt.NP) NP = ITRE(II)
     end do
     if (DEBUG) write (JOSTND,30) NP,IREC2,PI,GROSPC
     30 format(' ENTERING SUBROUTINE PTBAL NP=',I4,&
          &'; IREC2=',I5,'; PI=',F6.1,'; GROSPC=',F8.3)

     !  IF THERE IS ONLY ONE POINT, THE PROBLEM IS SIMPLE...SAVE IT
     !  FOR A SPECIAL CASE.

     if (NP.gt.1) then

        !  BUILD THE INDICES TO POINTS.  ZERO OUT IPNDX, AND LOAD WITH
        !  THE NUMBER OF RECORDS ON EACH POINT.

        do IP = 1,NP
           IPNDX(IP) = 0
        end do
        do I = 1,ITRN
           II = IND(I)
           IPNDX(ITRE(II)) = IPNDX(ITRE(II))+1
        end do

        !  RUN A CUMULATIVE.  THESE POINTERS "RESERVE" ROOM IN IND2.

        do IP = 2,NP
           IPNDX(IP) = IPNDX(IP)+IPNDX(IP-1)
        end do

        !  BUILD A LIST OF POINTERS IN SORTED ORDER. THEY ARE SORTED
        !  BECAUSE IND IS SORTED OVER DBH.

        do I = ITRN,1,-1
           II = IND(I)
           IND2(IPNDX(ITRE(II))) = II
           IPNDX(ITRE(II)) = IPNDX(ITRE(II))-1
        end do
     else

        !  BUILD THE LIST BY COPYING THE IND INTO THE IND2 LISTS.

        do I = 1,ITRN
           IND2(I) = IND(I)
        end do
        IPNDX(1) = 0
     end if

     !  COMPUTE THE BASAL AREA IN LARGER TREES WITHIN EACH POINT.

     do IP = 1,NP
        if (IP.eq.NP) then
           N = ITRN-IPNDX(IP)
        else
           N = IPNDX(IP+1)-IPNDX(IP)
        end if

        !  LOOP OVER TREES WITHIN POINTS.

        XBALT = 0.0
        do I = IPNDX(IP)+1,IPNDX(IP)+N
           PTBALT(IND2(I)) = XBALT

           !  WK5 IS COMPUTED IN DENSE AS DBH(I)*DBH(I)*PROB(I)

           XBALT = XBALT+(WK5(IND2(I))*.005454154*PI/GROSPC)
        end do

        !  XBALT IS NOW THE POINT ESTIMATE OF STAND BASAL AREA.

        PTBAA(IP) = XBALT
     end do

     !  WRITE DEBUG, IF REQUESTED.

     if (DEBUG) then
        do II = 1,ITRN
           I = IND2(II)
           write (JOSTND,40) I,ITRE(I),DBH(I),WK5(I),PTBALT(I),PROB(I),&
                &PTBAA(ITRE(I))
           40 format (' IN PTBAL: I,ITRE,DBH,WK5,PTBALT,PROB,PTBAA=',&
                &2I5,5F10.3)
        end do
     end if
     if (DEBUG) write(JOSTND,50)
     50 format(' WESTERN/SOUTHERN VARIANT, PBA VALUES SET, LEAVING ',&
          &'PTBAL.')

  end select

  return
end
