!=================!
!   cr : cutstk   !
!=================!
! CR $Id$
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
  integer :: I,IC,IGRP,IULIM,IG,JPNUM,JSPCUT,JTYP
  real    :: FSTOCK,SLP,TMPMAX,TEMP,HU,HL,DU,DL,CSTOCK,D,H,TPA
  logical :: LINCL

  !  ENTRY AUTSTK CALCULATES NORMAL STOCKING FOR AUTOMATIC THINS.

  !  USES EDMINSTERS SDI MAXIMUMS. IF USER SPECIFIED MAX
  !  IS PRESENT USE IT (1ST ONE IN SDIDEF ARRAY).  LIMIT FULSTK CALC
  !  TO QMD GE 2.0 INCHES FOR REASONABLE MAXIMUMS (APPROX 6000 TPA).

  entry AUTSTK (FSTOCK)

  !  SET EXPONENT BY MODEL TYPE.

  SLP = 1.70008
  if (IMODTY.eq.2.or. IMODTY.eq.3) then
     SLP = 1.65992
  else if (IMODTY.eq.4) then
     SLP = 1.66143
  else if (IMODTY.eq.5) then
     SLP = 1.68265
  end if

  !  SET SDI MAXIMUM BY USER ENTERED SDI, OR MODEL TYPE DEFAULT

  do I = 1,MAXSP
     if (SDIDEF(I) .eq.0.0) go to 210
     TMPMAX = SDIDEF(I)
     go to 220
     210 continue
  end do
  TMPMAX = 473.
  if (IMODTY.eq.2.or. IMODTY.eq.3) then
     TMPMAX = 408.
  else if (IMODTY.eq.4) then
     TMPMAX = 625.
  else if (IMODTY.eq.5) then
     TMPMAX = 573.
  end if
  220 continue

  !  CALCULATE FULL STOCKING

  TEMP = 10.0 ** (-SLP)

  FSTOCK = 1./((TEMP/TMPMAX) * 2.0**SLP)
  if (RMSQD.gt.2.0)&
       &FSTOCK = 1./((TEMP/TMPMAX) *  RMSQD**SLP)
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
