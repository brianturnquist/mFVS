!=================================!
!   Process height growth steps   !
!=================================!
! BASE $Id$
subroutine HTGSTP
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'

  external RANN
  ! Variables
  integer :: MYACT(2)
  integer :: NEW,IOD,ITRC1,ITRC2,I,K,IG,IULIM,IGRP,I1,I2,IS,IS1,IS2
  integer :: ISPC,IDT,IACT,NP,ITODO,NTODO
  real    :: STDPBR,AVEPRB,PRB,HT1,HT2,H,BRK,BRATIO,X,PKIL,BACHLO
  real    :: TOPK,TOPH,D,AF,DTK,CN
  equivalence (WK6(2),HT1),(WK6(3),HT2),(WK6(4),PRB), &
       & (WK6(5),AVEPRB),(WK6(6),STDPBR)
  data MYACT/110,111/

  !     FIND OPTIONS

  call OPFIND(2,MYACT,NTODO)
  if (NTODO .eq. 0) return

  !     ENTER LOOP TO PROCESS ALL OPTIONS SPECIFIED.

  do ITODO = 1,NTODO
     call OPGET(ITODO,6,IDT,IACT,NP,WK6)
     if (IACT.lt.0) goto 200
     call OPDONE (ITODO,IY(ICYC))

     !     LOAD PARAMETERS.  NOTE THAT SEVERAL PARAMETERS ARE EQUIVALENCED
     !     TO WK6 THUS THEY ARE LOADED AUTOMATICALLY.

     ISPC = IFIX(WK6(1))

     !     SET INDICIES TO SPECIES COUNTERS.

     if (ISPC.le.0) goto 10
     IS1 = ISPC
     IS2 = ISPC
     goto 20
     10 continue
     IS1 = 1
     IS2 = MAXSP
     20 continue

     !     ENTER SPECIES LOOP

     do IS = IS1,IS2
        I1 = ISCT(IS,1)

        !     IF THERE ARE NO TREES OF THIS SPECIES, THEN BRANCH.

        if (I1.le.0) goto 110

        !     LOAD TREE INDEX.

        I2 = ISCT(IS,2)

        !  IF PROCESSING A SPECIES GROUP AND THIS SPECIES IS NOT IN THE GROUP,
        !  THEN SKIP THIS SPECIES.

        if (ISPC .lt. 0) then
           IGRP = -ISPC
           IULIM = ISPGRP(IGRP,1)+1
           do IG = 2,IULIM
              if (IS .eq. ISPGRP(IGRP,IG))go to 91
              90 continue
           end do
           go to 110
           91 continue
        end if

        !     ENTER TREE LOOP

        do K = I1,I2
           I = IND1(K)

           !     LOAD HEIGHT AND CHECK HEIGHT RANGE.  IF OUT-OF-RANGE, THEN SKIP
           !     TREE RECORD.

           H = HT(I)
           BRK = BRATIO(IS,DBH(I),H)
           if (H.le.HT1 .or. H.gt.HT2) goto 100

           !     IF A RANDOM NUMBER IS GREATER THAN THE PROBABILITY OF DAMAGE,
           !     THEN: THE TREE ESCAPES.

           if (PRB.gt. 0.99999) goto 30
           call RANN(X)
           if (X.gt.PRB) goto 100

           !     ELSE: THE TREE WILL BE DAMAGED.

           30 continue

           !     COMPUTE PROPORTION REDUCTION IN HEIGHT OR HIEGHT GROWTH.

           PKIL = BACHLO(AVEPRB,STDPBR,RANN)
           if (PKIL.le.0.0) goto 100

           !     IF SIMULATING HTGSTOP.

           if (IACT.eq.110) then

              !        SET THE HEIGHT GROWTH AND BIRTH YEAR.

              if (PKIL.ge.1.0) PKIL = 1.0
              HTG(I) = HTG(I)*PKIL
              ABIRTH(I) = 0.0
              goto 100
           end if
           if (PKIL.gt.0.8) PKIL = .8

           !     COMPUTE ABSOLUTE NUMBER OF FEET TOP-KILLED.

           TOPK = H*PKIL
           TOPH = H-TOPK
           ITRC2 = IFIX(TOPH*100.+.5)

           !     IF THE TREE IS NOT ALREADY TOPKILLED, THEN: BRANCH TO TOPKILL.

           ITRC1 = ITRUNC(I)
           if (ITRC1.le.0) goto 40

           !     IF THE NEW POINT IS BELOW THE POINT OF TRUNCATION, THEN:
           !     REDUCE THE POINT OF TRUNCATION.

           if (ITRC1.gt.ITRC2) ITRUNC(I) = ITRC2
           HT(I) = TOPH
           goto 100
           40 continue

           !     ELSE: THE TREE HAS NOT BEEN TOP KILLED.  IF THE HEIGHT IS LESS
           !     THAN 25 OR THE DBH IS LESS THAN 6, THEN:  THE TREE WILL BE
           !     TEMPORARILY KILLED.

           D = DBH(I)*BRK
           if (H.lt. 25 .or. D.lt.6.0) goto 60

           !     ELSE:
           !     IF THE DIAMETER AT THE POINT OF TOPKILL IS LESS THAN A CRITICAL
           !     VALUE, THEN TEMPORARILY TOPKILL THE TREE.
           !     COMPUTE THE DIAMETER AT POINT OF TOPKILL USING THE BEHRE HYPER-
           !     BOLOID (MONSERUD 1981 FOREST SCI. 27:253/265.)

           AF = CFV(I)/(.00545415*D*D*H)
           AF = .44244-(.99167/AF)-(1.43237*ALOG(AF))+(1.68581*SQRT(AF)) &
                & -(.13611*AF*AF)
           DTK = TOPK/H
           DTK = (DTK/((AF*DTK)+(1-AF)))*D
           if (DTK.lt. 4.0) goto 60

           !     PERMANENTLY TOPKILL THE TREE.

           ITRUNC(I) = ITRC2
           NORMHT(I) = IFIX(H*100.+.5)

           !     SET THE MANAGEMENT CODE FOR PERMANENTLY TOPKILLED TREES
           !     EQUAL TO 3

           IMC(I) = 3
           60 continue
           HT(I) = TOPH

           !     IF THE CROWN RATIO HAS NOT BEEN ADJUSTED BY THE BUG MODELS
           !     (THE CROWN RATIO IS NEG IF CROWN HAS BEEN ADJUSTED BY BUG MODELS),
           !     THEN: REDUCE THE CROWN RATIO TO ACCOUNT FOR THE DAMAGE.

           IOD = ICR(I)
           if (IOD.lt.0) goto 100
           CN = (IOD/100.*H)-H+TOPH
           NEW = IFIX(CN/TOPH*100.+.5)

           !     TOPKILL WILL NOT REDUCE THE CROWN RATIO TO LESS THAN 5 PERCENT.

           if (NEW.lt.5) NEW = 5
           ICR(I) = -NEW

           !     MAKE SURE THAT THE MANAGEMENT CODE FOR TOP DAMAGED TREES
           !     IS GREATER THAN 1 (WHICH IS A LOWER SCORE THUS NORMALLY
           !     INDICATING HIGHER PRIORITY FOR CUTTING).

           if (IMC(I).eq.1) IMC(I) = 2
           100 continue
        end do
        110 continue
     end do
     200 continue
  end do
  return
end subroutine HTGSTP
