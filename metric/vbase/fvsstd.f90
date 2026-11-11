!===========================!
!   metric/vbase : fvsstd   !
!===========================!
! METRIC-VBASE $Id$
subroutine FVSSTD (IWHO)
  implicit none

  !     CREATE A FILE FOR FVSTAND POST-PROCESSOR INPUT.

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
  include 'FVSSTDCM.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IWHO,MYACT(1),KSDI,IOPEN,I,NTODO,ITODO,NPRMS,IACTK,IDT
  integer :: KODE,ISTLNB,JYR,IP,ITPLAB,ITMFOR,IBA,IKNT,ISITE
  integer :: KAGE,ISPC,I1,I2,I3,IICR,IDMR,ICDF,IBDF,IPTBAL
  real    :: TEM(6),TEMSDI,STAGEA,STAGEB,XXWT,TEMBA,P,DP,CW,DGI
  real    :: PDBHI,BRATIO,PHTI,DUMSDI
  dimension CLAB1(2)
  character(len=11) :: CISN
  character(len=8) :: TIM
  character(len=10) :: DAT
  character(len=8) :: TID
  character(len=4) :: CLAB1
  character(len=10) :: REV
  integer*4 :: IDCMP1,IDCMP2
  data MYACT/204/
  data IDCMP1,IDCMP2/10000000,20000000/
  data CLAB1/'TREE','CUT '/

  !  COMPUTE CURRENT SDI FOR PRINTING

  TEMSDI = 0.
  call SDICLS(0,0.,999.,1,TEMSDI,DUMSDI,STAGEA,STAGEB,0)
  KSDI = INT((TEMSDI/ACRtoHA)/GROSPC + 0.5)

  !  SET OLD VOLUME VARIABLES TO ZERO IF THIS IS CYCLE 0

  if (LSTART) then
     IOPEN = 0
     do I = 1,ITRN
        PTOCFV(I) = 0.
        PMRCFV(I) = 0.
        PMRBFV(I) = 0.
        NCFDEF(I) = 0
        NBFDEF(I) = 0
        10 continue
     end do
  end if

  !  SET OLD PPE VARIABLE TO BLANK, BUT KEEP TO PRESERVE HEADER FORMAT

  CISN = '           '

  !  FIND OUT IF THERE IS A FVSSTAND OPTION.

  call OPFIND (1,MYACT(1),NTODO)

  !  IF THERE ARE NONE TO DO, RETURN

  if (NTODO.eq.0) go to 220

  !  ELSE: DO THE OPTIONS PRESENT.

  do ITODO = 1,NTODO
     call OPGET (ITODO,2,IDT,IACTK,NPRMS,TEM)
     if (IACTK.lt.0) goto 200

     !  IF THE OUTPUT FILE IS NOT OPEN, OPEN IT.

     if (.not.FSTOPEN) then
        call MYOPEN(KOLIST,KWDFIL(1:ISTLNB(KWDFIL))//'.fst',&
             &5,170,0,1,1,0,KODE)
        if (KODE .gt.0) then
           write(JOSTND,*) ' FVSSTAND FILE DID NOT OPEN'
           call RCDSET (2,.true.)
           go to 200
        end if
        FSTOPEN = .true.
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
     case default
        ITMFOR = KODFOR
     end select
     IBA = NINT((BA*FT2pACRtoM2pHA)/GROSPC)
     if (IWHO.ne.1) then
        TEMBA = 0.
        do I = 1,ITRN
           TEMBA = TEMBA+0.0054542*DBH(I)*DBH(I)*PROB(I)
           40 continue
        end do
        IBA = NINT((TEMBA*FT2pACRtoM2pHA)/GROSPC)
     end if
     IKNT = ICYC+1
     ISITE = IFIX(SITEAR(ISISP)*FTtoM)
     if (LSTART) then
        KAGE = IAGE
     else
        if (IWHO .eq.1) then
           KAGE = IAGE + IY(IKNT) - IY(1)
        else
           KAGE = IAGE + IY(ICYC) - IY(1)
        end if
     end if
     write (KOLIST,2) IP,ICYC,JYR,NPLT,MGMID,VARACD,DAT,TIM,&
          &CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN,&
          &KAGE,ITMFOR,FIAJSP(ISISP),ISITE,IPTINV,IBA,KSDI
     2 format (' -999',3I5,3(1X,A),5X,3(1X,A),I3,E14.7,2(1X,A),&
          &5X,I4,12X,I9,1X,A3,I5,I2,I3,I5)

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
           IICR = ((ICR(I)-1)/10)+1
           if (IICR.gt.9) IICR = 9

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

           !  GET MISTLETOE RATING FOR CURRENT TREE RECORD.

           call MISGET(I,IDMR)

           !  SET CROWN WIDTH.

           CW = CRWDTH(I)

           !  DECODE DEFECT AND ROUND OFF POINT BAL.

           ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
           IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
           NCFDEF(I) = ICDF
           NBFDEF(I) = IBDF
           IPTBAL = NINT(PTBALT(I)*FT2pACRtoM2pHA)

           !  CYCLE 0, PRINT INPUT DG ONLY

           DGI = DG(I)
           if (LSTART) DGI = WORK1(I)

           if (IDG.eq.0.or. IDG.eq.2) then
              if (DGI .gt.0..and.&
                   &((DGI/BRATIO(ISP(I),DBH(I)-DG(I),HT(I)).lt.DBH(I)))) then
                 PDBHI = DBH(I)-(DG(I)/BRATIO(ISP(I),DBH(I)-DG(I),HT(I)))
              else
                 PDBHI = 0.
              end if
           else
              PDBHI = PDBH(I)
           end if
           PDBH(I) = 0.

           if (IHTG.eq.0.or. IHTG.eq.2) then
              if (HTG(I) .gt.0.) then
                 PHTI = HT(I)-HTG(I)
              else
                 PHTI = 0.
              end if
           else
              PHTI = PHT(I)
           end if
           PHT(I) = 0.

           if (P.lt.9999.9995.and. DP.lt.9999.9995) then
              write(KOLIST,21)&
                   &TID,&
                   &I,&
                   &NSP(ISP(I),1)(1:2),&
                   &ISP(I),&
                   &IMC(I),&
                   &ISPECL(I),&
                   &ITRE(I),&
                   &P/ACRtoHA,&
                   &DP/ACRtoHA,&
                   &DBH(I)*INtoCM,&
                   &DGI*INtoCM,&
                   &HT(I)*FTtoM,&
                   &HTG(I)*FTtoM,&
                   &ICR(I),&
                   &CW*FTtoM,&
                   &IDMR,&
                   &PCT(I),&
                   &IPTBAL,&
                   &CFV(I)*FT3toM3,&
                   &WK1(I)*FT3toM3,&
                   &BFV(I)*FT3toM3,&
                   &ICDF,&
                   &IBDF,&
                   &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),&
                   &PDBHI*INtoCM,&
                   &PHTI*FTtoM,&
                   &PTOCFV(I)*FT3toM3,&
                   &PMRCFV(I)*FT3toM3,&
                   &PMRBFV(I)*FT3toM3,&
                   &NCFDEF(I),&
                   &NBFDEF(I),&
                   &YRDLOS(I),&
                   &NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),&
                   &ABIRTH(I),&
                   &IPVEC(ITRE(I))
              21 format(1X,A8,1X,I4,1X,A2,I3,2(1X,I2),1X,I3,1X,F8.3,1X,F8.3,&
                   &1X,F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
                   &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                   &1X,I3,&
                   &1X,F5.1,1X,F5.1,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                   &1X,F7.5,1X,I5,1X,F6.0,1X,I8)
           else
              write(KOLIST,20)&
                   &TID,&
                   &I,&
                   &NSP(ISP(I),1)(1:2),&
                   &ISP(I),&
                   &IMC(I),&
                   &ISPECL(I),&
                   &ITRE(I),&
                   &P/ACRtoHA,&
                   &DP/ACRtoHA,&
                   &DBH(I)*INtoCM,&
                   &DGI*INtoCM,&
                   &HT(I)*FTtoM,&
                   &HTG(I)*FTtoM,&
                   &ICR(I),&
                   &CW*FTtoM,&
                   &IDMR,&
                   &PCT(I),&
                   &IPTBAL,&
                   &CFV(I)*FT3toM3,&
                   &WK1(I)*FT3toM3,&
                   &BFV(I)*FT3toM3,&
                   &ICDF,&
                   &IBDF,&
                   &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),&
                   &PDBHI*INtoCM,&
                   &PHTI*FTtoM,&
                   &PTOCFV(I)*FT3toM3,&
                   &PMRCFV(I)*FT3toM3,&
                   &PMRBFV(I)*FT3toM3,&
                   &NCFDEF(I),&
                   &NBFDEF(I),&
                   &YRDLOS(I),&
                   &NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),&
                   &ABIRTH(I),&
                   &IPVEC(ITRE(I))
              20 format(1X,A8,1X,I4,1X,A2,I3,2(1X,I2),1X,I3,1X,F8.2,1X,F8.2,&
                   &1X,F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
                   &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                   &1X,I3,&
                   &1X,F5.1,1X,F5.1,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                   &1X,F7.5,1X,I5,1X,F6.0,1X,I8)
           end if
           50 continue
        end do
        60 continue
     end do

     !  FOR CYCLE 0 LIST, PRINT DEAD TREES WHICH WERE PRESENT IN
     !  THE INVENTORY DATA AT THE BOTTOM OF THE FVSSTAND LIST

     if (IREC2.gt.MAXTRE .or. ITPLAB.eq.2) go to 200
     do I = IREC2,MAXTRE
        P = (PROB(I) / GROSPC) / (FINT/FINTM)
        IICR = ((ICR(I)-1)/10)+1
        if (IICR.gt.9) IICR = 9
        write(TID,'(I8)') IDTREE(I)

        !     GET MISTLETOE RATING FOR CURRENT TREE RECORD.

        call MISGET(I,IDMR)

        !     SET CROWN WIDTH.

        CW = CRWDTH(I)

        !     DECODE DEFECT AND ROUND OFF POINT BAL.

        ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
        IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
        NCFDEF(I) = ICDF
        NBFDEF(I) = IBDF
        IPTBAL = NINT(PTBALT(I)*FT2pACRtoM2pHA)

        !  CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.

        DGI = DG(I)
        if (ICYC.eq.0.and. TEM(6).eq.0) DGI = WORK1(I)
        PDBHI = 0.
        PHTI = 0.

        !  PUT PROB IN MORTALITY COLUMN

        DP = P
        P = 0.

        if (P.lt.9999.9995.and. DP.lt.9999.9995) then
           write(KOLIST,21)&
                &TID,&
                &I,&
                &NSP(ISP(I),1)(1:2),&
                &ISP(I),&
                &IMC(I),&
                &ISPECL(I),&
                &ITRE(I),&
                &P/ACRtoHA,&
                &DP/ACRtoHA,&
                &DBH(I)*INtoCM,&
                &DGI*INtoCM,&
                &HT(I)*FTtoM,&
                &HTG(I)*FTtoM,&
                &ICR(I),&
                &CW*FTtoM,&
                &IDMR,&
                &PCT(I),&
                &IPTBAL,&
                &CFV(I)*FT3toM3,&
                &WK1(I)*FT3toM3,&
                &BFV(I)*FT3toM3,&
                &ICDF,&
                &IBDF,&
                &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),&
                &PDBHI*INtoCM,&
                &PHTI*FTtoM,&
                &PTOCFV(I)*FT3toM3,&
                &PMRCFV(I)*FT3toM3,&
                &PMRBFV(I)*FT3toM3,&
                &NCFDEF(I),&
                &NBFDEF(I),&
                &YRDLOS(I),&
                &NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),&
                &ABIRTH(I),&
                &IPVEC(ITRE(I))
        else
           write(KOLIST,20)&
                &TID,&
                &I,&
                &NSP(ISP(I),1)(1:2),&
                &ISP(I),&
                &IMC(I),&
                &ISPECL(I),&
                &ITRE(I),&
                &P/ACRtoHA,&
                &DP/ACRtoHA,&
                &DBH(I)*INtoCM,&
                &DGI*INtoCM,&
                &HT(I)*FTtoM,&
                &HTG(I)*FTtoM,&
                &ICR(I),&
                &CW*FTtoM,&
                &IDMR,&
                &PCT(I),&
                &IPTBAL,&
                &CFV(I)*FT3toM3,&
                &WK1(I)*FT3toM3,&
                &BFV(I)*FT3toM3,&
                &ICDF,&
                &IBDF,&
                &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM),&
                &PDBHI*INtoCM,&
                &PHTI*FTtoM,&
                &PTOCFV(I)*FT3toM3,&
                &PMRCFV(I)*FT3toM3,&
                &PMRBFV(I)*FT3toM3,&
                &NCFDEF(I),&
                &NBFDEF(I),&
                &YRDLOS(I),&
                &NINT(FLOAT(NORMHT(I)+5)*.01*FTtoM),&
                &ABIRTH(I),&
                &IPVEC(ITRE(I))
        end if
        150 continue
     end do

     200 continue
  end do

  220 continue
  return
end
