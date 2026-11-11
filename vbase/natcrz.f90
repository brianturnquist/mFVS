!====================!
!   vbase : natcrz   !
!====================!
! VBASE $Id$
subroutine NATCRZ (IWHO)
  implicit none

  !     CREATE A FILE FOR PROCESSING THROUGH THE NATIONAL CRUISE SYSTEM
  !     VOLUME ROUTINE

  !     IWHO=1 IF CALLED FROM MAIN, IWHO=2 IF CALLED FROM CUTS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'HTCAL.f90'
  include 'PLOT.f90'
  include 'ESTREE.f90'
  include 'VARCOM.f90'
  include 'WORKCM.f90'
  include 'STDSTK.f90'
  include 'SUMTAB.f90'
  include 'OUTCOM.f90'
  include 'ECON.f90'
  include 'OPCOM.f90'

  ! Variables
  integer :: IFC,IBDF,ICDF,I3,I2,I1,ISPC,ISITE,KFOR,KREG,ITMFOR
  integer :: I,ITPLAB,IP,JYR,ISTLNB,KODE,KOLIST,IBA,IDT,IACTK
  integer :: NPRMS,ITODO,NTODO,IOPEN,IWHO,KDST
  integer :: MYACT(1)
  real    :: TEM(6),XXWT,TEMBA,P,DP,HTLL,TDIBB,TDIBC,FC,BKRAT,BRATIO
  character(len=11) :: CISN
  character(len=8) :: TIM
  character(len=10) :: DAT
  character(len=8) :: TID
  character(len=4) :: CLAB1(2)
  character(len=10) :: REV
  character(len=11) :: EQNC
  character(len=11) :: EQNB
  integer*4 :: IDCMP1,IDCMP2
  data MYACT/205/
  data IDCMP1,IDCMP2/10000000,20000000/
  data CLAB1/'TREE','CUT '/

  !  SET FILE OPEN STATUS TO ZERO IF THIS IS CYCLE 0

  if (LSTART) then
     IOPEN = 0
  end if

  !  FIND OUT IF THERE IS A FVSSTAND OPTION.

  call OPFIND (1,MYACT(1),NTODO)

  !  IF THERE ARE NONE TO DO, RETURN

  if (NTODO.eq.0) go to 220

  !  SET OLD PPE VARIABLE TO BLANK, BUT KEEP TO PRESERVE HEADER FORMAT

  CISN = '           '

  !  ELSE: DO THE OPTIONS PRESENT.

  do ITODO = 1,NTODO
     call OPGET (ITODO,2,IDT,IACTK,NPRMS,TEM)
     if (IACTK.lt.0) goto 200

     !  IF THE OUTPUT FILE IS NOT OPEN, OPEN IT.

     if (IOPEN .eq.0) then
        KOLIST = 30
        call MYOPEN(KOLIST,KWDFIL(1:ISTLNB(KWDFIL))//'.ncp',&
             &5,170,0,1,1,0,KODE)
        if (KODE .gt.0) then
           write(JOSTND,*) ' FVSSTAND FILE DID NOT OPEN'
           call RCDSET (2,.true.)
           go to 200
        end if
        IOPEN = 1
     end if

     !  LET IP BE THE RECORD OUTPUT COUNT
     !  SET THE OUTPUT REPORTING YEAR.

     if (IWHO .eq.1) then
        JYR = IY(ICYC+1)
        IP = ITRN
        ITPLAB = 1
     else
        JYR = IY(ICYC)
        IP = 0
        do I = 1,ITRN
           if (WK3(I) .gt.0.) IP = IP+1
           1 continue
        end do
        ITPLAB = 2
     end if

     !  IF AT THE PRE-PROJECTION TIME, FIND OUT IF A FVSSTAND HAS BEEN
     !  SUPPRESSED. IF YES, BRANCH TO NOT ACCOMPLISH THE REQUEST.
     !  ONLY MARK THE OPTION DONE IF THIS IS CALLED FROM MAIN.

     if (LSTART) then
        if (NPRMS.ge.1.and. TEM(1).eq.1.) goto 200
        if (NPRMS.ge.1.and. TEM(1).eq.2)call OPDONE(ITODO,JYR)
     else
        if (NPRMS.ge.1.and. TEM(1).eq.2.) goto 200
        if (IWHO .eq.1) then
           call OPDONE (ITODO,JYR-1)
        else
           !         CALL OPDONE(ITODO,JYR)
        end if
     end if

     !  WRITE A MARKER AND THE NUMBER OF RECORDS WHICH FOLLOW.

     XXWT = SAMWT
     call REVISE (VARACD,REV)
     call GRDTIM (DAT,TIM)
     select case (VARACD)
     case ('KT')
        ITMFOR = KODFOR/100000
     case ('SN')
        ITMFOR = KODFOR/100
     case default
        ITMFOR = KODFOR
     end select
     KREG = ITMFOR/100
     KFOR = ITMFOR-KREG*100
     select case (VARACD)
     case ('SN')
        KDST = KODFOR-ITMFOR*100
     case default
        KDST = 0
     end select
     IBA = NINT(BA/GROSPC)
     if (IWHO.ne.1) then
        TEMBA = 0.
        do I = 1,ITRN
           TEMBA = TEMBA+0.0054542*DBH(I)*DBH(I)*PROB(I)
           40 continue
        end do
        IBA = NINT(TEMBA/GROSPC)
     end if
     ISITE = IFIX(SITEAR(ISISP))
     write (KOLIST,2) IP,ICYC,JYR,NPLT,MGMID,VARACD,DAT,TIM,&
          &CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN,FIAJSP(ISISP),ISITE,IBA
     2 format (' -999',3I5,6(1X,A),I3,E14.7,2(1X,A),1X,A3,I5,I5)

     !  PROCESS TREELIST

     do ISPC = 1,MAXSP
        I1 = ISCT(ISPC,1)
        if (I1.eq.0) go to 60
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           I = IND1(I3)
           if (ITPLAB .eq.1) then
              P = PROB(I) / GROSPC
              if (ICYC.gt.0) then
                 DP = WK2(I)/ GROSPC
              else
                 DP = 0.0
              end if
           else
              P = WK3(I)/GROSPC
              DP = 0.
              if (P .le.0.) go to 50
           end if

           !   TRANSLATE TREE IDS FOR TREES THAT HAVE BEEN COMPRESSED OR
           !   GENERATED THROUGH THE ESTAB SYSTEM.

           if (IDTREE(I) .gt. IDCMP1) then
              if (IDTREE(I) .gt. IDCMP2) then
                 write(TID,'(''CM'',I6.6)') IDTREE(I)-IDCMP2
              else
                 write(TID,'(''ES'',I6.6)') IDTREE(I)-IDCMP1
              end if
           else
              write(TID,'(I8)') IDTREE(I)
           end if

           !  DECODE DEFECT AND COMPUTE CROWN BASE HT.

           ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
           IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
           HTLL = HT(I)-HT(I)*(FLOAT(ICR(I))*0.01)

           !  GET VOLUME EQUATION NUMBER, TOP DIB AND FORM CLASS FOR THIS TREE

           call GETEQN(ISP(I),DBH(I),HT(I),EQNC,EQNB,TDIBC,TDIBB)
           call FORMCL(ISP(I),IFOR,DBH(I),FC)
           IFC = IFIX(FC)

           !  GET THE BARK RATIO FOR THIS TREE

           BKRAT = BRATIO(ISPC,DBH(I),HT(I))

           select case (VARACD)
           case ('SN')
              if (P.lt.9999.9995.and. DP.lt.9999.9995) then
                 write(KOLIST,21) KREG,KFOR,KDST,ICYC,TID,I,FIAJSP(ISPC),&
                      &ITRE(I),P,DP,DBH(I),HT(I),ICR(I),ICDF,IBDF,IFC,HTLL,EQNC,&
                      &EQNB,TDIBC,TDIBB,BKRAT
                 21 format(1X,I2,',',1X,I2,',',1X,I2,',',1X,I2,', 01,',1X,A8,&
                      &',',1X,I4,',',1X,A4,',',1X,I4,',',1X,F7.2,',',1X,F7.2,',',&
                      &1X,F5.1,',',1X,F5.1,',',1X,I2,',',1X,I2,',',1X,I2,',',1X,&
                      &I2,',',1X,F5.1,',',1X,A10,',',1X,A10,',',1X,F5.2,',',1X,&
                      &F5.2,',',1X,F6.4)
              else
                 write(KOLIST,20) KREG,KFOR,KDST,ICYC,TID,I,FIAJSP(ISPC),&
                      &ITRE(I),P,DP,DBH(I),HT(I),ICR(I),ICDF,IBDF,IFC,HTLL,&
                      &EQNC,EQNB,TDIBC,TDIBB,BKRAT
                 20 format(1X,I2,',',1X,I2,',',1X,I2,',',1X,I2,', 01,',1X,A8,&
                      &',',1X,I4,',',1X,A4,',',1X,I4,',',1X,F7.1,',',1X,F7.1,',',&
                      &1X,F5.1,',',1X,F5.1,',',1X,I2,',',1X,I2,',',1X,I2,',',1X,I2,&
                      &',',1X,F5.1,',',1X,A10,',',1X,A10,',',1X,F5.2,',',1X,F5.2,&
                      &',',1X,F6.4)
              end if
           case default
              if (P.lt.9999.9995.and. DP.lt.9999.9995) then
                 write(KOLIST,31) KREG,KFOR,KDST,ICYC,TID,I,&
                      &NSP(ISP(I),1)(1:2),ITRE(I),P,DP,DBH(I),HT(I),ICR(I),ICDF,&
                      &IBDF,IFC,HTLL,EQNC,EQNB,TDIBC,TDIBB,BKRAT
                 31 format(1X,I2,',',1X,I2,',',1X,I2,',',1X,I2,', 01,',1X,A8,&
                      &',',1X,I4,',',1X,A2,',',1X,I4,',',1X,F7.2,',',1X,F7.2,',',&
                      &1X,F5.1,',',1X,F5.1,',',1X,I2,',',1X,I2,',',1X,I2,',',1X,I2,&
                      &',',1X,F5.1,',',1X,A10,',',1X,A10,',',1X,F5.2,',',1X,F5.2,&
                      &',',1X,F6.4)
              else
                 write(KOLIST,30) KREG,KFOR,KDST,ICYC,TID,I,&
                      &NSP(ISP(I),1)(1:2),ITRE(I),P,DP,DBH(I),HT(I),ICR(I),ICDF,&
                      &IBDF,IFC,HTLL,EQNC,EQNB,TDIBC,TDIBB,BKRAT
                 30 format(1X,I2,',',1X,I2,',',1X,I2,',',1X,I2,', 01,',1X,A8,&
                      &',',1X,I4,',',1X,A2,',',1X,I4,',',1X,F7.1,',',1X,F7.1,',',&
                      &1X,F5.1,',',1X,F5.1,',',1X,I2,',',1X,I2,',',1X,I2,',',1X,I2,&
                      &',',1X,F5.1,',',1X,A10,',',1X,A10,',',1X,F5.2,',',1X,F5.2,&
                      &',',1X,F6.4)
              end if
           end select

           50 continue
        end do
        60 continue
     end do

     !  FOR CYCLE 0 LIST, PRINT DEAD TREES WHICH WERE PRESENT IN
     !  THE INVENTORY DATA AT THE BOTTOM OF THE FVSSTAND LIST

     if (IREC2.gt.MAXTRE .or. ITPLAB.eq.2) go to 200
     do I = IREC2,MAXTRE
        P = (PROB(I) / GROSPC) / (FINT/FINTM)
        write(TID,'(I8)') IDTREE(I)

        !  DECODE DEFECT AND COMPUTE CROWN BASE HT.

        ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
        IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
        HTLL = HT(I)-HT(I)*(FLOAT(ICR(I))*0.01)

        !  GET VOLUME EQUATION NUMBER, TOP DIB AND FORM CLASS FOR THIS TREE

        call GETEQN(ISP(I),DBH(I),HT(I),EQNC,EQNB,TDIBC,TDIBB)
        call FORMCL(ISP(I),IFOR,DBH(I),FC)
        IFC = IFIX(FC)

        !  PUT PROB IN MORTALITY COLUMN

        DP = P
        P = 0.

        !  GET THE BARK RATIO FOR THIS TREE

        BKRAT = BRATIO(ISP(I),DBH(I),HT(I))

        select case (VARACD)
        case ('SN')

           if (P.lt.9999.9995.and. DP.lt.9999.9995) then
              write(KOLIST,21) KREG,KFOR,KDST,ICYC,TID,I,FIAJSP(ISP(I)),&
                   &ITRE(I),P,DP,DBH(I),HT(I),ICR(I),ICDF,IBDF,IFC,HTLL,EQNC,&
                   &EQNB,TDIBC,TDIBB,BKRAT
           else
              write(KOLIST,20) KREG,KFOR,KDST,ICYC,TID,I,FIAJSP(ISP(I)),&
                   &ITRE(I),P,DP,DBH(I),HT(I),ICR(I),ICDF,IBDF,IFC,HTLL,EQNC,&
                   &EQNB,TDIBC,TDIBB,BKRAT
           end if

        case default

           if (P.lt.9999.9995.and. DP.lt.9999.9995) then
              write(KOLIST,31) KREG,KFOR,KDST,ICYC,TID,I,&
                   &NSP(ISP(I),1)(1:2),ITRE(I),P,DP,DBH(I),HT(I),ICR(I),ICDF,&
                   &IBDF,IFC,HTLL,EQNC,EQNB,TDIBC,TDIBB,BKRAT
           else
              write(KOLIST,30) KREG,KFOR,KDST,ICYC,TID,I,&
                   &NSP(ISP(I),1)(1:2),ITRE(I),P,DP,DBH(I),HT(I),ICR(I),ICDF,&
                   &IBDF,IFC,HTLL,EQNC,EQNB,TDIBC,TDIBB,BKRAT
           end if
        end select

        150 continue
     end do

     200 continue
  end do

  220 continue
  return
end
