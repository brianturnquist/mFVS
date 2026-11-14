!====================!
!   lpmpb : mpsdlp   !
!====================!
! LPMPB $Id$
subroutine MPSDLP
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
  !     COMPUTES THE SURFACE AREA OF THE DEAD LPP.

  ! Revision History
  !   08/07/01 Lance R. David (FHTET)
  !     Initialization of LPRT to false. It was uninitialized before.
  !     (previous date of last revision was 02/08/88)
  !   08/06/07 Lance R. David (FHTET)
  !     Added PLOT.F77 common file. Added code for UNexpansion factor
  !     on dead trees to address MPB mortality TPA being 2x the values
  !     of input TPA and that reported in the cycle 0 treelist. An
  !     explanation of the TPA expansion is in base\src\notre.f.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'
  include 'COLCOM.f90'

  ! Variables
  logical :: LPRT
  integer :: I,II,ISIZ
  real    :: P,SURFLP
  character(len=5) :: DBHCLS(10)

  data DBHCLS / ' 1-3 ',' 3-5 ',' 5-7 ',' 7-9 ',' 9-11',&
       &'11-13','13-15','15-17','17-19',' 19+ ' /

  LPRT = .false.

  !     IF LSADLP IS TRUE THE USER SPECIFIED THE SURFACE AREA...

  if (.not. LSADLP) then

     !        IF THERE ARE DEAD TREES, SUM UP SURFACE AREA OF DEAD LPP.

     if (IREC2.le. MAXTRE) then
        do I = IREC2,MAXTRE
           if (ISP(I) .eq.7) SADLPP = SADLPP + SURFLP(DBH(I)) *&
                &PROB(I)
           100 continue
        end do
     end if

     !        IF THERE ARE 'NEW-HITS', ADD THEM TOO.

     if (NDAMS .gt.0) then
        do II = 1,NDAMS
           I = IPT(II)
           SADLPP = SADLPP + SURFLP(DBH(I)) * PROB(I)
           200 continue
        end do
     end if
  end if

  !     IF LINVMR IS TRUE, THEN SUM UP THE CURRENT ATTACKS

  if (NDAMS .eq.0) LDAM = .false.

  if (LINVMR) then
     if (.not. LPRT .and. LDAM) then
        write(JOMPB,310)
        write(JOMPB,*)
        write(JOMPB,320)
        write(JOMPB,330)
        write(JOMPB,340)
        LPRT = .true.
        310 format (//,'MOUNTAIN PINE BEETLE DAMAGE CODE ',&
             &'OUTPUT TABLE')
        320 format (21X,'DBH',16X,'INF')
        330 format (' TREE ID     DBH    CLASS   TR/ACRE    STATUS ')
        340 format ('----------------------------------------------')
     end if

     if (NDAMS .gt.0) then
        do I = 1,10
           CURRMR(I) = 0.0
           GREINF(I) = 0.0
           400 continue
        end do

        do II = 1,NDAMS
           I = IPT(II)
           call COLIND(DBH(I),ISIZ)

           !              IF THE POINTER I, IS GE IREC2, THEN THE TREE IS DEAD,
           !              OTHERWISE, THE TREE IS LIVE.
           !              Dead trees need the TPA adjusted in order for the MPB tables
           !              to match the input data set and FVS output treelist. See
           !              notre.f for complete explanation.

           if (I .ge. IREC2) then
              P = (PROB(I) / GROSPC) / (FINT/FINTM)
              CURRMR(ISIZ) = CURRMR(ISIZ) + P
              if (LDAM) write (JOMPB,600) IDTREE(I), DBH(I),&
                   &DBHCLS(ISIZ), P, 'DEAD'
           else
              GREINF(ISIZ) = GREINF(ISIZ) + PROB(I)
              if (LDAM) write (JOMPB,600) IDTREE(I), DBH(I),&
                   &DBHCLS(ISIZ), PROB(I), 'INFEST'
           end if
           500 continue
        end do
        if (LDAM) write (JOMPB,*)
     end if
     600 format(I8,3X,F6.1,3X,A5,4X,F6.2,4X,A6)
  end if

  return
end
