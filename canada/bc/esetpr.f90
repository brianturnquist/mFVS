!========================!
!   canada/bc : esetpr   !
!========================!
! CANADA-BC $Id$
subroutine ESETPR (METH,ZMECH,ZBURN,ZBRSH,&
     &PNONE,PMECH,PBURN,IALN,&
     &IDSDAT,KDT,IP)
  implicit none

  !     SET SITE PREPARATION FOR THE REGENERATION MODEL.

  ! Variables
  real    :: PRMS(3)
  integer :: IALN(3)
  integer :: IP,KDT,IDSDAT,METH,I,KODE,NPRMS,IDT
  real    :: PBURN,PMECH,PNONE,ZBURN,ZMECH,ZBRSH

  METH = 0
  ZMECH = 0.0
  ZBURN = 0.0
  ZBRSH = 0.0
  PNONE = 0.0
  PMECH = 0.0
  PBURN = 0.0
  do I = 1,3
     IALN(I) = 0
     10 continue
  end do

  !     PROCESS BURNPREP

  I = 0
  80 continue
  I = I+1
  call OPGET2(491,IDT,IDSDAT,KDT,I,3,NPRMS,PRMS,KODE)
  if (KODE.gt.0) goto 90
  IP = 0
  ZBURN = FLOAT(IDT)
  if (NPRMS.gt.0) goto 85
  METH = 3
  call OPDON2(491,IDT,IDSDAT,KDT,I,KODE)
  return
  85 continue
  PBURN = PRMS(1)/100.
  IALN(3) = 1
  goto 80
  90 continue
  I = I-1
  if (I.eq.0) goto 110

  !     FLAG THE LAST BURNPREP ACCOMPLISHED.

  call OPDON2(491,IDT,IDSDAT,KDT,I,KODE)
  100 continue

  !     FLAG THE REST OF THE BURNPREPS AS CANCELLED.

  I = I-1
  if (I.le.0) goto 110
  call OPDON2(491,-1,IDSDAT,KDT,I,KODE)
  goto 100
  110 continue

  !     PROCESS MECHPREP

  I = 0
  120 continue
  I = I+1
  call OPGET2(493,IDT,IDSDAT,KDT,I,3,NPRMS,PRMS,KODE)
  if (KODE.gt.0) goto 130
  IP = 0
  ZMECH = FLOAT(IDT)
  if (NPRMS.gt.0) goto 125
  METH = 2
  call OPDON2(493,IDT,IDSDAT,KDT,I,KODE)
  goto 130
  125 continue
  PMECH = PRMS(1)/100.
  IALN(2) = 1
  goto 120
  130 continue
  I = I-1
  if (I.eq.0) goto 150

  !     FLAG THE LAST MECHPREP ACCOMPLISHED.

  call OPDON2(493,IDT,IDSDAT,KDT,I,KODE)
  140 continue

  !     FLAG THE REST OF THE MECHPREPS AS CANCELLED.

  I = I-1
  if (I.le.0) goto 150
  call OPDON2(493,-1,IDSDAT,KDT,I,KODE)
  goto 140
  150 continue

  !     PROCESS BRUSHPREP

  I = 0
  200 continue
  I = I+1
  call OPGET2(492,IDT,IDSDAT,KDT,I,3,NPRMS,PRMS,KODE)
  if (KODE.gt.0) goto 290
  IP = 0
  ZBRSH = FLOAT(IDT)
  if (NPRMS.gt.0) goto 285
  METH = 0 ! no method
  call OPDON2(492,IDT,IDSDAT,KDT,I,KODE)
  return
  285 continue
  !      PBURN=PRMS(1)/100.
  !      IALN(3)=1
  goto 200
  290 continue
  I = I-1
  if (I.eq.0) goto 310

  !     FLAG THE LAST BRUSHPREP ACCOMPLISHED.

  call OPDON2(492,IDT,IDSDAT,KDT,I,KODE)
  300 continue

  !     FLAG THE REST OF THE BRUSHPREPS AS CANCELLED.

  I = I-1
  if (I.le.0) goto 310
  call OPDON2(492,-1,IDSDAT,KDT,I,KODE)
  goto 300
  310 continue

  return
end
