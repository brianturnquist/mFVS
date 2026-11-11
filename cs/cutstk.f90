!=================!
!   cs : cutstk   !
!=================!
! CS $Id$
subroutine CUTSTK
  implicit none

  !  THIS SUBROUTINE CONTAINS ENTRY POINTS FOR CALCULATING STOCKING
  !  LEVELS FOR VARIOUS THINNING OPTIONS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG,LINCL
  real    :: BASP(5),A1(5),A2(5),A3(5),TPRED(5)
  integer :: JJSP(MAXSP)
  real    :: FSTOCK,TOTBA,STTPA,HU,HL,DU,DL,CSTOCK,D,H,TPA
  integer :: I,IISPG,JPNUM,JSPCUT,JTYP,IC,IGRP,IULIM,IG
  data A1/-.5779,-.5373,-.3430,-.1433,-.1879/
  data A2/16.9246,23.1713,10.7172,7.1389,9.0272/
  data A3/45.7353,60.8299,43.0417,34.3018,46.7800/

  !  PUT SPECIES IN ONE OF 5 STOCKING CATEGORIES

  !  SPECIES GROUP 1 - SHORTLEAF PINE
  !  SPECIES GROUP 2 - EASTERN WHITE PINE
  !  SPECIES GROUP 3 - OAKS
  !  SPECIES GROUP 4 - NORTHERN HARDWOOD
  !  SPECIES GROUP 5 - NORTHERN HARDWOODS  50-100% BASSWOOD

  data JJSP/5*1,2*2,2*3,18*4,3,3*4,3,8*4,3,5,4*4,21*3,10*4,19*3/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'CUTSTK',6,ICYC)

  !  ENTRY AUTSTK CALCULATES NORMAL STOCKING FOR AUTOMATIC THINS.

  !  VARIABLE DEFINITIONS:
  !  A1,A2,A3  COEFFICIENTS TO PREDICT TPA BY STOCKING CATEGORY
  !  TPRED     TREES PER ACRE FOR THE STAND AT 100% STOCKING-
  !            BY STOCKING CATEGORY.
  !  STTPA     NUMBER OF TREES PER ACRE PREDICTED FOR THE NEW STAND AT
  !            100% STOCKING--INCLUDES ALL SIX STOCKING CATEGORIES

  entry AUTSTK (FSTOCK)
  do I = 1,5
     BASP(I) = 0.0
     TPRED(I) = 0.0
     10 continue
  end do
  TOTBA = 0.0

  !  DETERMINE TOTAL NUMBER OF TREES PER ACRE IN EACH SPECIES GROUP

  do I = 1,ITRN
     IISPG = JJSP(ISP(I))
     BASP(IISPG) = BASP(IISPG) + .0054542*DBH(I)*DBH(I)*PROB(I)
     20 continue
  end do

  !  RECALCULATE STOCKING PERCENT FOR LIVE TREES ONLY (ISTATUS = 1).

  do I = 1,5
     if (BASP(I) .eq.0.0) then
        TPRED(I) = 1.0
        go to 30
     end if
     TOTBA = BASP(I)+TOTBA

     !  CALCULATE PREDICTED TREES PER ACRE FOR EACH SPECIES GROUP

     TPRED(I) = (A1(I)*RMSQD*RMSQD+A2(I)*RMSQD+A3(I))/&
          &(.0054542*RMSQD*RMSQD)
     30 continue
  end do

  !  WEIGHT PREDICTED TPA FOR EACH STOCKING CATEGORY BY PERCENT BA IN
  !  EACH STOCKING CATEGORY

  STTPA = (BASP(1)*TPRED(1)+BASP(2)*TPRED(2)+BASP(3)*TPRED(3)&
       &+BASP(4)*TPRED(4)+BASP(5)*TPRED(5))/TOTBA
  FSTOCK = STTPA
  return

  !  ENTRY CLSSTK COMPUTES BA OR TPA STOCKING IN A SPECIFIED
  !  DBH/HT/SPECIES CLASS.

  !  CSTOCK = CLASS STOCKING
  !  JTYP   = 1 IF STOCKING IS IN TPA,  2 IF STOCKING IS IN BA
  !  JSPCUT = SPECIES WHICH WILL BE CUT
  !  LINCL  = LOGICAL VARIABLE USED TO INCLUDE SPECIES OR NOT.
  !  DL,DU  = LOWER AND UPPER DIAMETER LIMITS RESPECTIVELY (GE,LT)
  !  HL,HU  = LOWER AND UPPER HEIGHT LIMITS RESPECTIVELY (GE,LT)
  !  JPNUM = POINT NUMBER (IN FVS SEQUENCE FORMAT), 0 = ALL POINTS

  entry CLSSTK (CSTOCK,JTYP,JSPCUT,DL,DU,HL,HU,JPNUM)
  CSTOCK = 0.0
  do IC = 1,ITRN
     if (JPNUM.gt.0.and. ITRE(IC).ne.JPNUM)go to 100

     LINCL = .false.
     if ((JSPCUT.eq.0.or. JSPCUT.eq.ISP(IC)).and.&
          &.not.LEAVESP(ISP(IC))) then
        LINCL = .true.
     else if (JSPCUT.lt.0) then
        IGRP = -JSPCUT
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           if ((ISP(IC) .eq. ISPGRP(IGRP,IG)).and..not.LEAVESP(ISP(IC))) then
              LINCL = .true.
              go to 61
           end if
           60 continue
        end do
     end if
     61 continue

     if (LINCL) then
        D = DBH(IC)
        H = HT(IC)
        TPA = WK4(IC)
        if (JPNUM.gt.0)TPA = TPA*(PI-FLOAT(NONSTK))
        if (D.lt.DL .or. D.ge.DU) go to 100
        if (H.lt.HL .or. H.ge.HU) go to 100
        if (JTYP.eq.1) then
           CSTOCK = CSTOCK + TPA
        else
           CSTOCK = CSTOCK + TPA*(D*D*0.005454154)
        end if
     end if
     100 continue
  end do
  return

end
