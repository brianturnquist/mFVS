!=======================================!
!   Estimate tree age and site height   !
!=======================================!
! UT $Id$
subroutine FINDAG(I,ISPC,D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,&
     &HTMAX2,DEBUG)
  implicit none

  !  THIS ROUTINE SET EFFECTIVE TREE AGE BASED ON INPUT VARIABLE(S)
  !  SUCH AS TREE HEIGHT.  IN UT AGE IS CALCULATED FOR
  !  CALLED FROM **COMCUP**
  !  CALLED FROM **CRATET**
  !  CALLED FROM **REGENT**
  !  CALLED FROM **HTGF**

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'GGCOM.f90'

  !  LOCAL VARIABLE DEFINITIONS:

  !  INCRNG = HAS A VALUE OF 1 WHEN THE SITE CURVE HAS BEEN MONOTONICALLY
  !           INCREASING IN PREVIOUS ITERATIONS.
  !   OLDHG = HEIGHT GUESS FROM PREVIOUS ITERATION.

  ! Variables
  logical :: DEBUG
  integer :: I,ISPC,ICLS,INCRNG
  real    :: BAUTBA,SITE,EFFAGE
  real    :: D1,D2,H,SITAGE,SITHT,AGMAX1,HTMAX1,HTMAX2
  real    :: AG,DIFF,HGUESS,SINDX,TOLER
  real    :: AGMAX(MAXSP),HTMAX(MAXSP)
  real    :: RDANUW,OLDHG

  ! SPECIES ORDER FOR UTAH VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
  ! 21=BI, 22=BE, 23=OS, 24=OH

  ! VARIANT EXPANSION:
  ! GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
  ! NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM UT

  !  DATA STATEMENTS

  data AGMAX/&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,  50.,&
       &100.,   0.,   0.,   0./

  data HTMAX/&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,&
       &0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,   0.,  20.,&
       &100.,   0.,   0.,   0./

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = HTMAX2

  !  PRINT DEBUG INFORMATION

  if (DEBUG)write(JOSTND,*)' ENTERING FINDAG I,ISPC,D1,D2,H= ',&
       &I,ISPC,D1,D2,H

  !  INITIALIZATIONS

  AGMAX1 = AGMAX(ISPC)
  HTMAX1 = HTMAX(ISPC)

  select case (ISPC)

     !  COMPUTE AGE FOR ASPEN.  THIS EQUATION IS ALSO USED FOR OAK.
     !  EQUATIONS FORMULATED BY
     !  WAYNE SHEPPERD, ROCKY MTN FOREST EXP STATION, FT COLLINS, CO.

  case(6,13,24)
     SITAGE = (H*2.54*12.0/26.9825)**(1.0/1.1752)
     SITHT = H

     !  SPECIES USING SURROGATE EQUATIONS FROM CR VARIANT
     !  17 (GB), 18 (NC), 19 (FC), 22 (BE)

  case(17:19,22)
     ICLS = IFIX(D1+1.0)
     if (ICLS .gt.41) ICLS = 41
     BAUTBA = BAU(ICLS)/BA
     if (BAUTBA.lt.0.0)BAUTBA = 0.0
     SITE = SITEAR(ISPC)
     call FNDAG(EFFAGE,SITE,H,BAUTBA,DEBUG)
     if (DEBUG)write(JOSTND,*)' IN FINDAG EFFAGE,SITE,H,',&
          &'BAUTBA= ',EFFAGE,SITE,H,BAUTBA
     SITAGE = EFFAGE
     if (SITAGE .le.0.0) SITAGE = 1.0

     !  MC AND BI USE LOGIC FROM THE SO VARIANT

  case(20,21)
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

  !   END OF TREE LOOP.  PRINT DEBUG INFO IF DESIRED.

  29 continue
  IF(DEBUG)WRITE(JOSTND,*)' IN FINDAG I,ISPC,H,SITAGE,SITHT =       &
       & ',I,ISPC,H,SITAGE,SITHT

  if (DEBUG)write(JOSTND,50)
  50 format(' LEAVING SUBROUTINE FINDAG')

  return
end
  !**END OF CODE SEGMENT
