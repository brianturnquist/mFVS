!=======================================!
!   Estimate tree age and site height   !
!=======================================!
! CI $Id$
subroutine FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,&
     &HTMAX2,DEBUG)
  implicit none

  !  THIS ROUTINE SETS EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
  !  SUCH AS TREE HEIGHT.  IN CI, AGE IS CALCULATED FOR ASPEN ONLY
  !  SUCH AS TREE HEIGHT.
  !  CALLED FROM **COMCUP
  !  CALLED FROM **CRATET
  !  CALLED FROM **REGENT**
  !  CALLED FROM **HTGF**

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  !  LOCAL VARIABLE DEFINITIONS:

  !  INCRNG = HAS A VALUE OF 1 WHEN THE SITE CURVE HAS BEEN MONOTONICALLY
  !           INCREASING IN PREVIOUS ITERATIONS.
  !   OLDHG = HEIGHT GUESS FROM PREVIOUS ITERATION.

  ! Variables
  logical :: DEBUG
  integer :: I,ISPC,INCRNG
  real    :: D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,HTMAX2
  real    :: TOLER,SINDX,HGUESS,AG,DIFF
  real    :: RDANUW,OLDHG

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = D1
  RDANUW = D2
  RDANUW = HTMAX2

  select case (ISPC)

     !  COMPUTE AGE FOR ASPEN. EQUATION FORMULATED BY
     !  WAYNE SHEPPERD, ROCKY MTN FOREST EXP STATION, FT COLLINS, CO.

  case(13)
     SITAGE = (H*2.54*12.0/26.9825)**(1.0/1.1752)

     !  CURLLEAF MOUNTAIN-MAHOGANY

  case(15)
     AGMAX1 = 50.
     HTMAX1 = 20.
     TOLER = 2.0
     SINDX = SITEAR(ISPC)

     !  CRATET CALLS FINDAG AT THE BEGINING OF THE SIMULATION TO
     !  CALCULATE THE AGE OF INCOMMING TREES.  AT THIS POINT ABIRTH(I)=0.
     !  THE AGE OF INCOMMING TREES HAVING H>=HMAX IS CALCULATED BY
     !  ASSUMEING A GROWTH RATE OF 0.10FT/YEAR FOR THE INTERVAL H-HMAX.
     !  TREES REACHING HMAX DURING THE SIMULATION ARE IDENTIFIED IN HTGF.

     if (H .ge. HTMAX1) then
        SITAGE = AGMAX1 + (H - HTMAX1)/0.10
        SITHT = H
        if (DEBUG)write(JOSTND,*)' I,ISPC,AGMAX1,H,HTMAX1= ',I,ISPC,&
             &AGMAX1,H,HTMAX1
        go to 29
     end if

     AG = 2.0

     INCRNG = 0
     HGUESS = 0.
     75 continue
     OLDHG = HGUESS

     HGUESS = (SINDX - 4.5) / ( 0.6192 - 5.3394/(SINDX - 4.5)&
          &+ 240.29 * AG**(-1.4) +(3368.9/(SINDX - 4.5))*AG**(-1.4))
     HGUESS = HGUESS + 4.5
     if (DEBUG)write(JOSTND,91200)I,ISPC,AG,HGUESS,H
     91200 format(' FINDAG,I,ISPC,AGE,HGUESS,H ',2I5,3F10.2)

     !  AVOID NEGATIVE PREDICTED HEIGHTS AT SMALL AGES FROM SOME SI CURVES
     !  GED 4/20/18

     if (HGUESS .lt.1.)go to 175

     DIFF = ABS(HGUESS-H)
     if (DIFF .le. TOLER .or. H .lt. HGUESS) then
        SITAGE = AG
        SITHT = HGUESS
        if (DEBUG) then
           write(JOSTND, *)' DIFF,TOLER,H,HGUESS,AG,SITAGE,SITHT= ',&
                &DIFF,TOLER,H,HGUESS,AG,SITAGE,SITHT
        end if
        go to 29
     end if

     !  SOME SITE CURVES DECREASE AT THE START BEFORE INCREASING. IF DECREASING,
     !  KEEP GOING; IF SITE CURVE WAS INCREASING, BUT NOW HAS FLATTENED OFF, STOP
     !  THE ITERATION GED 04/19/18

     DIFF = (HGUESS-OLDHG)
     if (OLDHG.ne.0.0.and. DIFF.ge.0.05) INCRNG = 1
     if (DEBUG)write(JOSTND,*)' IN FINDAG OLDHG,DIFF,INCRNG= ',&
          &OLDHG,DIFF,INCRNG
     if (INCRNG.eq.1.and. DIFF .lt.0.05) then
        SITAGE = AG
        SITHT = HGUESS
        if (DEBUG) then
           write(JOSTND, *)' SITE CURVE FLAT OLDHG,AG,HGUESS,SITAGE,',&
                &'SITHT= ',OLDHG,AG,HGUESS,SITAGE,SITHT
        end if
        go to 29
     end if

     175 continue
     AG = AG + 2.

     if (AG .gt. AGMAX1) then

        !  H IS TOO GREAT AND MAX AGE IS EXCEEDED

        SITAGE = AGMAX1
        SITHT = H
        go to 29
     else
        go to 75
     end if

     !  REMAINING SPECIES FOR WHICH AGE DOESN'T MATTER.

  case default
     SITAGE = 0.
     SITHT = H

  end select

  29 continue
  if (DEBUG)write(JOSTND,50)ISPC,I,SITAGE,SITHT
  50 format(' LEAVING SUBROUTINE FINDAG  ISPC,I,SITAGE,SITHT =',&
       &2I5,2F10.3)

  return
end
