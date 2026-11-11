!===================================!
!   Read stand definition options   !
!===================================!
! BASE $Id$
subroutine SDEFET (LNOTBK,ARRAY,KEYWRD, &
     & LOPEVN,IACTK,KARD,IPRMPT)
  implicit none

  ! Common blocks
  include 'PRGPRM.F77'
  include 'PLOT.F77'
  include 'COEFFS.F77'
  include 'CONTRL.F77'

  !     PROCESSES BFDEFECT OR MCDEFECT KEYWORDS - CALLED FROM INITRE.

  ! Variables
  logical :: LNOTBK(12),LOPEVN
  character(len=8) :: KEYWRD
  character(len=10) :: KARD(12)
  real    :: ARRAY(12),XX(6),YY(6)
  integer :: IPRMPT,IACTK,IDT,KODE,IS,N,I,ILEN,IGRP,IULIM,IG,IGSP
  integer :: I1,I2,IRTNCD
  real    :: ALGSLP

  !     IF IPRMPT IS GT 0, THEN THE PARMS OPTION IS BEING USED.

  if (IPRMPT.gt.0) then
     if (LNOTBK(1)) then
        IDT = IFIX(ARRAY(1))
     else
        IDT = 1
     end if
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,IACTK,KEYWRD,KARD, &
             & IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else

     !        CHECK THE SPECIES CODE.

     call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD, &
          & ARRAY,KARD)
     if (IS.eq.-999) return

     !        FIX UP MISSING NUMBERS.

     XX(1) = 0.
     YY(1) = 0.
     N = 1
     do I = 3,7
        if (LNOTBK(I)) then
           N = N+1
           XX(N) = FLOAT(I-2)*5.0
           YY(N) = ARRAY(I)
        end if
        10 continue
     end do
     if (N.le.1) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,4)
        return
     end if
     if (N.lt.6) then
        do I = 3,7
           if (.not.LNOTBK(I)) ARRAY(I) =  &
                & ALGSLP(FLOAT(I-2)*5.0,XX,YY,N)
           20 continue
        end do
     end if

     !        IF THE DATE IS DEFINED OR AN EVENT IS OPENED (KEYWORD
     !        IS BETWEEN A THEN AND AN ENDIF, THEN SCHEDULE THE
     !        DEFECT TERM CHANGE.

     if (LNOTBK(1).or.LOPEVN) then
        IDT = 1
        if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
        call OPNEW (KODE,IDT,IACTK,6,ARRAY(2))
        write (KARD(2)(7:10),'(I4)') IDT
        ILEN = 3
        if (IS.lt.0)ILEN = ISPGRP(-IS,92)
        write (JOSTND,30) KEYWRD,'DATE/CYCLE= ' // KARD(2)(7:10), &
             & KARD(2)(1:ILEN),IS,(ARRAY(I),I = 3,7)
        30 format(/1X,A8,T13,A,'; SPECIES=',A,' (CODE=', &
             & I2,'); 5 INCH TREES=',F6.2,'; 10 INCH TREES=',F6.2, &
             & '; 15 INCH TREES=',F6.2,/T13,'20 INCH TREES=',F6.2, &
             & ';  25 INCH AND LARGER TREES=',F6.2)
     else
        ILEN = 3
        if (IS.lt.0)ILEN = ISPGRP(-IS,92)
        write (JOSTND,30) KEYWRD,'DEFECT CHANGED', &
             & KARD(2)(1:ILEN),IS,(ARRAY(I),I = 3,7)

        !           THE DATE IS NOT DEFINED, CHANGE THE DEFECT TERMS NOW.
        !           IF THE SPECIES CODE IS:
        !                <0 CHANGE ALL SPECIES IN THE GROUP,
        !                =0 CHANGE ALL SPECIES,
        !                >0 CHANGE THE ONE POINTED TO.

        if (IS .lt. 0.) then
           IGRP = -IS
           IULIM = ISPGRP(IGRP,1)+1
           do IG = 2,IULIM
              IGSP = ISPGRP(IGRP,IG)
              if (IACTK .eq. 215) then
                 CFDEFT(2,IGSP) = ARRAY(3)
                 CFDEFT(3,IGSP) = ARRAY(4)
                 CFDEFT(4,IGSP) = ARRAY(5)
                 CFDEFT(5,IGSP) = ARRAY(6)
                 CFDEFT(6,IGSP) = ARRAY(7)
                 CFDEFT(7,IGSP) = ARRAY(7)
                 CFDEFT(8,IGSP) = ARRAY(7)
                 CFDEFT(9,IGSP) = ARRAY(7)
              else if (IACTK .eq. 216) then
                 BFDEFT(2,IGSP) = ARRAY(3)
                 BFDEFT(3,IGSP) = ARRAY(4)
                 BFDEFT(4,IGSP) = ARRAY(5)
                 BFDEFT(5,IGSP) = ARRAY(6)
                 BFDEFT(6,IGSP) = ARRAY(7)
                 BFDEFT(7,IGSP) = ARRAY(7)
                 BFDEFT(8,IGSP) = ARRAY(7)
                 BFDEFT(9,IGSP) = ARRAY(7)
              end if
              4321 continue
           end do
           go to 100
        end if

        if (IS.eq.0) then
           I1 = 1
           I2 = MAXSP
        else
           I1 = IS
           I2 = IS
        end if

        !           LOAD THE COEFFICIENTS.

        do IS = I1,I2
           do I = 2,6
              if (IACTK.eq.216) BFDEFT(I,IS) = ARRAY(I+1)
              if (IACTK.eq.215) CFDEFT(I,IS) = ARRAY(I+1)
              50 continue
           end do
           BFDEFT(7,IS) = ARRAY(7)
           BFDEFT(8,IS) = ARRAY(7)
           BFDEFT(9,IS) = ARRAY(7)
           CFDEFT(7,IS) = ARRAY(7)
           CFDEFT(8,IS) = ARRAY(7)
           CFDEFT(9,IS) = ARRAY(7)
           60 continue
        end do
        100 continue
     end if
  end if
  return
end subroutine SDEFET
