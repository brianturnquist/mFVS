!==================================!
!   Read stand definition labels   !
!==================================!
! BASE $Id$
subroutine SDEFLN (LNOTBK,ARRAY,KEYWRD,B0,B1,KARD,IS)
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  !     PROCESSES BFFDLN OR MCFDLN KEYWORDS - CALLED FROM INITRE.
  !     NL CROOKSTON - INT. MOSCOW - 4/80.

  ! Variables
  logical :: LNOTBK(12)
  character(len=8) :: KEYWRD
  character(len=10) :: KARD(12)
  real    :: ARRAY(12),B0(MAXSP),B1(MAXSP)
  integer :: IS,IGRP,IULIM,IG,IGSP,ILEN,I,JS

  !     SPECIES CODE PROCESSING.

  call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD, &
       & ARRAY,KARD)
  if (IS.eq.-999)return

  write (JOSTND,20) KEYWRD
  20 format (/1X,A8,'   COEFFICIENTS FOR LOG-LINEAR FORM AND', &
       & ' DEFECT CORRECTION EQUATION')

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

  if (IS .lt. 0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        if (LNOTBK(2)) B0(IGSP) = ARRAY(2)
        if (LNOTBK(3)) B1(IGSP) = ARRAY(3)
        25 continue
     end do
     ILEN = ISPGRP(-IS,92)
     write (JOSTND,30) KARD(1)(1:ILEN),IS,B0(IGSP),B1(IGSP)

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq. 0) then
     do IS = 1,MAXSP
        if (LNOTBK(2)) B0(IS) = ARRAY(2)
        if (LNOTBK(3)) B1(IS) = ARRAY(3)
        60 continue
     end do
     write (JOSTND,70)
     70 format (T13,'FOR ALL SPECIES THE FOLLOWING COEFFICIENTS HAVE ', &
          & 'BEEN SPECIFIED:')
     do I = 2,3
        JS = I-2
        if (LNOTBK(I)) write (JOSTND,80) JS,ARRAY(I)
        80 format (T23,'B',I1,'= ',F8.5)
        100 continue
     end do

     !     SPECIES CODE IS SPECIFIED (IS>0).

  else
     if (LNOTBK(2)) B0(IS) = ARRAY(2)
     if (LNOTBK(3)) B1(IS) = ARRAY(3)
     write (JOSTND,30) KARD(1)(1:3),IS,B0(IS),B1(IS)
     30 format (T13,'SPECIES= ',A,' (CODE= ',I2,'); B0=',F8.5,'; B1=', &
          & F8.5)
  end if

  return
end subroutine SDEFLN
