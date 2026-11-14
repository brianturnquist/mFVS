!=============================!
!   archive/ni/src : cutstk   !
!=============================!
subroutine CUTSTK
  implicit none

  !  NI  $ID

  !  THIS SUBROUTINE CONTAINS ENTRY POINTS FOR CALCULATING STOCKING
  !  LEVELS FOR VARIOUS THINNING OPTIONS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: LINCL
  real    :: TPA
  real    :: FSTOCK,TOTBA,TEMBA,TBA,TMPMAX,HU,HL,DU,DL,CSTOCK,H,D
  integer :: I,ISPC,JSPCUT,JTYP,IC,IGRP,IULIM,IG,JPNUM

  !  ENTRY AUTSTK CALCULATES NORMAL STOCKING FOR AUTOMATIC THINS.

  entry AUTSTK (FSTOCK)

  !  CALCULATE MAXIMUM SDI WEIGHTED BY BASAL AREA
  !  NOTE: 10.0 ** -1.605 = 0.02483133

  TOTBA = 0.0
  TEMBA = 0.0

  do I = 1,ITRN
     ISPC = ISP(I)
     TBA = 0.0054542 * DBH(I) * DBH(I) * PROB(I)
     TEMBA = TEMBA + TBA * SDIDEF(ISPC)
     TOTBA = TOTBA + TBA
     210 continue
  end do

  if (TOTBA.le.1..or. TEMBA.le.1) then
     FSTOCK = 0.
     go to 50
  else
     TMPMAX = TEMBA / TOTBA
  end if

  FSTOCK = 1./((0.02483133/TMPMAX) * 2.0**1.605)
  if (RMSQD.gt.2.0)&
       &FSTOCK = 1./((0.02483133/TMPMAX) *  RMSQD**1.605)
  50 continue
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
