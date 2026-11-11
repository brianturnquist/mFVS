!================================================!
!   Read volume standards and equation options   !
!================================================!
! BASE $Id$
subroutine VOLKEY(DEBUG)
  implicit none

  !  THIS SUBROUTINE PROCESSES THE KEYWORDS USED TO MODIFY VOLUME
  !  STANDARDS AND EQUATIONS.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'ARRAYS.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'
  include 'OUTCOM.F77'
  include 'PLOT.F77'
  include 'VOLSTD.F77'

  ! Variables
  logical :: DEBUG
  integer :: MYACTS(4)
  real    :: PRMS(10)
  integer :: NTODO,I,NP,IACTK,KDT,INUM,ISPC,IGRP,IULIM,IG,IGSP,J

  !  INITIALIZE ACTIVITY VECTORS.
  !    215 = MCDEFECT
  !    216 = BFDEFECT
  !    217 = VOLUME
  !    218 = BFVOLUME

  data MYACTS/215,216,217,218/

  !  IF CYCLE EQUAL ZERO, THEN THIS STUFF "CAN NOT" BE DONE.

  if (ICYC.eq.0) go to 9960

  !  DETERMINE IF VOLUME STANDARDS CHANGE THIS CYCLE.

  call OPFIND(4,MYACTS,NTODO)
  if (NTODO.le.0) go to 9960
  do I = 1,NTODO
     call OPGET(I,12,KDT,IACTK,NP,PRMS)

     !  IF THE PARAMETER COUNT IS TOO LOW, DELETE THE ACTIVITY.

     if (NP.lt.6) then
        call OPDEL1 (I)
        goto 7
     end if
     INUM = 219-IACTK
     go to (9905,9915,9925,9935),INUM

     !  CHANGE MERCHANTABILITY STANDARDS FOR THE BOARD FOOT VOLUME
     !  EQUATIONS.  EMPIRICAL ADJUSTMENT DEVELOPED BY J. E. BRICKELL,
     !  REGION ONE, TM.
     !  BFVOLUME KEYWORD.

     9905 continue
     call OPDONE(I,IY(ICYC))
     ISPC = IFIX(PRMS(1))

     if (ISPC .lt. 0) then
        IGRP = -ISPC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           BFMIND(IGSP) = PRMS(2)
           BFTOPD(IGSP) = PRMS(3)
           BFSTMP(IGSP) = PRMS(4)
           FRMCLS(IGSP) = PRMS(5)
           METHB(IGSP) = IFIX(PRMS(6))
           9906 continue
        end do
     else if (ISPC .eq. 0) then
        do J = 1,MAXSP
           BFMIND(J) = PRMS(2)
           BFTOPD(J) = PRMS(3)
           BFSTMP(J) = PRMS(4)
           FRMCLS(J) = PRMS(5)
           METHB(J) = IFIX(PRMS(6))
           9907 continue
        end do
     else
        BFMIND(ISPC) = PRMS(2)
        BFTOPD(ISPC) = PRMS(3)
        BFSTMP(ISPC) = PRMS(4)
        FRMCLS(ISPC) = PRMS(5)
        METHB(ISPC) = IFIX(PRMS(6))
     end if
     go to 7

     !  CHANGE MERCHANTABILITY STANDARDS FOR MERCHANTABLE CUBIC
     !  FOOT VOLUME EQUATIONS.  ADJUSTMENTS BASED ON BEHRE HYPERBOLA.
     !  VOLUME KEYWORD.

     9915 continue
     if (LFIANVB) go to 7
     call OPDONE(I,IY(ICYC))
     ISPC = IFIX(PRMS(1))
     if (ISPC .lt. 0) then
        IGRP = -ISPC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           DBHMIN(IGSP) = PRMS(2)
           TOPD(IGSP) = PRMS(3)
           STMP(IGSP) = PRMS(4)
           FRMCLS(IGSP) = PRMS(5)
           METHC(IGSP) = IFIX(PRMS(6))
           SCFMIND(IGSP) = PRMS(7)
           SCFTOPD(IGSP) = PRMS(8)
           SCFSTMP(IGSP) = PRMS(9)
           9916 continue
        end do
     else if (ISPC .eq. 0) then
        do J = 1,MAXSP
           DBHMIN(J) = PRMS(2)
           TOPD(J) = PRMS(3)
           STMP(J) = PRMS(4)
           FRMCLS(J) = PRMS(5)
           METHC(J) = IFIX(PRMS(6))
           SCFMIND(J) = PRMS(7)
           SCFTOPD(J) = PRMS(8)
           SCFSTMP(J) = PRMS(9)
           9917 continue
        end do
     else
        DBHMIN(ISPC) = PRMS(2)
        TOPD(ISPC) = PRMS(3)
        STMP(ISPC) = PRMS(4)
        FRMCLS(ISPC) = PRMS(5)
        METHC(ISPC) = IFIX(PRMS(6))
        SCFMIND(ISPC) = PRMS(7)
        SCFTOPD(ISPC) = PRMS(8)
        SCFSTMP(ISPC) = PRMS(9)
     end if
     go to 7

     !  CHANGE DEFECT STANDARDS FOR BOARD FOOT VOLUME. ADJUSTMENTS ARE
     !  BASED ON LINE SEGMENT DATA.
     !  BFDEFECT KEYWORD.

     9925 continue
     call OPDONE(I,IY(ICYC))
     ISPC = IFIX(PRMS(1))
     if (ISPC .lt. 0) then
        IGRP = -ISPC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           BFDEFT(2,IGSP) = PRMS(2)
           BFDEFT(3,IGSP) = PRMS(3)
           BFDEFT(4,IGSP) = PRMS(4)
           BFDEFT(5,IGSP) = PRMS(5)
           BFDEFT(6,IGSP) = PRMS(6)
           BFDEFT(7,IGSP) = PRMS(6)
           BFDEFT(8,IGSP) = PRMS(6)
           BFDEFT(9,IGSP) = PRMS(6)
           9926 continue
        end do
     else if (ISPC .eq. 0) then
        do J = 1,MAXSP
           BFDEFT(2,J) = PRMS(2)
           BFDEFT(3,J) = PRMS(3)
           BFDEFT(4,J) = PRMS(4)
           BFDEFT(5,J) = PRMS(5)
           BFDEFT(6,J) = PRMS(6)
           BFDEFT(7,J) = PRMS(6)
           BFDEFT(8,J) = PRMS(6)
           BFDEFT(9,J) = PRMS(6)
           9927 continue
        end do
     else
        BFDEFT(2,ISPC) = PRMS(2)
        BFDEFT(3,ISPC) = PRMS(3)
        BFDEFT(4,ISPC) = PRMS(4)
        BFDEFT(5,ISPC) = PRMS(5)
        BFDEFT(6,ISPC) = PRMS(6)
        BFDEFT(7,ISPC) = PRMS(6)
        BFDEFT(8,ISPC) = PRMS(6)
        BFDEFT(9,ISPC) = PRMS(6)
     end if
     go to 7

     !  CHANGE DEFECT STANDARDS FOR CUBIC FOOT VOLUME. ADJUSTMENTS ARE
     !  BASED ON SEGMENTED LINED DATA.
     !  MCDEFECT KEYWORD.

     9935 continue
     if (LFIANVB) go to 7
     call OPDONE(I,IY(ICYC))
     ISPC = IFIX(PRMS(1))
     if (ISPC .lt. 0) then
        IGRP = -ISPC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           CFDEFT(2,IGSP) = PRMS(2)
           CFDEFT(3,IGSP) = PRMS(3)
           CFDEFT(4,IGSP) = PRMS(4)
           CFDEFT(5,IGSP) = PRMS(5)
           CFDEFT(6,IGSP) = PRMS(6)
           CFDEFT(7,IGSP) = PRMS(6)
           CFDEFT(8,IGSP) = PRMS(6)
           CFDEFT(9,IGSP) = PRMS(6)
           9936 continue
        end do
     else if (ISPC .eq. 0) then
        do J = 1,MAXSP
           CFDEFT(2,J) = PRMS(2)
           CFDEFT(3,J) = PRMS(3)
           CFDEFT(4,J) = PRMS(4)
           CFDEFT(5,J) = PRMS(5)
           CFDEFT(6,J) = PRMS(6)
           CFDEFT(7,J) = PRMS(6)
           CFDEFT(8,J) = PRMS(6)
           CFDEFT(9,J) = PRMS(6)
           9937 continue
        end do
     else
        CFDEFT(2,ISPC) = PRMS(2)
        CFDEFT(3,ISPC) = PRMS(3)
        CFDEFT(4,ISPC) = PRMS(4)
        CFDEFT(5,ISPC) = PRMS(5)
        CFDEFT(6,ISPC) = PRMS(6)
        CFDEFT(7,ISPC) = PRMS(6)
        CFDEFT(8,ISPC) = PRMS(6)
        CFDEFT(9,ISPC) = PRMS(6)
     end if
     7 continue
  end do

  9960 continue
  if (DEBUG) write(JOSTND,9970) (BFMIND(I),BFTOPD(I),BFSTMP(I), &
       & I,I = 1,MAXSP)
  9970 format(' BFMIND=',F10.2,' BFTOPD=',F10.2,' BFSTMP=', &
       & F10.2,' SPECIES=',I3)

  return
end subroutine VOLKEY
