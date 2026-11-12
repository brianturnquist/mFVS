!====================!
!   vbase : prtrls   !
!====================!
! VBASE $Id$
subroutine PRTRLS (IWHO)
  implicit none

  !     PRINT THE TREE LIST.

  !     IWHO = 1 IF CALLED NORMALLY, AND 2 OR 3 IF CALLED FROM CUTS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESTREE.f90'
  include 'VARCOM.f90'
  include 'WORKCM.f90'
  include 'METRIC.f90'

  ! Variables
  logical :: LFORMT,LHD,LOK,LRC,LTREE

  character(len=11) :: CISN
  character(len=10) :: DAT
  character(len=10) :: REV
  character(len=8) :: TID
  character(len=8) :: TIM

  character(len=4) :: CLAB1(4)
  character(len=3) :: CLAB2(3)

  integer*4 :: DBSKODE,IDCMP1,IDCMP2

  integer :: I,I1,I2,I3,IACTK,IBDF,ICDF,IDMR,IDT,IICR,IP,IPTBAL,ITODO
  integer :: ITPLAB,ISPC,IWHO,J,JYR,NPRMS,NTODO,NUMREQ,KNTREC,KOLIST

  integer :: MYACT(3)

  real    :: CW,DGI,DP,P,XXWT

  real    :: DUPCHK(5,5),TEM(6)

  !  DATA STATEMENTS:

  data MYACT/80,199,198/
  data IDCMP1,IDCMP2/10000000,20000000/
  data CLAB1/'TREE','DEAD','CUT','ATRT'/
  data CLAB2/'END','CUT','CUT'/

  !  INITIALIZATION

  do I = 1,6
     TEM(I) = 0.
     500 continue
  end do

  !  SET OLD PPE VARIABLE TO BLANK, BUT KEEP TO PRESERVE HEADER FORMAT

  CISN = '           '

  !  ARRAY DUPCHK STORES UP TO 5 REQUESTS PER CYCLE AND ELIMINATES
  !  DUPLICATES OF THESE 5 REQUESTS.

  do I = 1,5
     do J = 1,5
        DUPCHK(I,J) = 0.
        501 continue
     end do
     502 continue
  end do
  NUMREQ = 0

  !     FIND OUT IF THERE IS A TREELIST OPTION.

  call OPFIND (1,MYACT(IWHO),NTODO)

  !     IF THERE ARE NONE TO DO, THEN: RETURN.

  if (NTODO.eq.0) return

  !     ELSE: DO THE OPTIONS PRESENT.

  do ITODO = 1,NTODO
     call OPGET (ITODO,6,IDT,IACTK,NPRMS,TEM)
     if (IACTK.lt.0) goto 200

     !  STORE THE REQUEST IF IT ISN'T A DUPLICATE. IGNORE THE REQUEST
     !  IF IT IS A DUPLICATE. SKIP THE DUPLICATE LOGIC IF THERE IS ONLY
     !  ONE REQUEST.

     if (NTODO .eq.1)go to 510
     if (ITODO .eq.1) then
        DUPCHK(1,1) = TEM(1)
        DUPCHK(1,2) = TEM(2)
        DUPCHK(1,3) = TEM(3)
        DUPCHK(1,4) = TEM(4)
        DUPCHK(1,5) = TEM(6)
        NUMREQ = 1
     else
        do I = 1,NUMREQ
           if (TEM(1).eq.DUPCHK(I,1) .and. TEM(2).eq.DUPCHK(I,2) .and.&
                &TEM(3).eq.DUPCHK(I,3) .and. TEM(4).eq.DUPCHK(I,4) .and.&
                &TEM(6).eq.DUPCHK(I,5)) go to 200
        end do
        if (NUMREQ.ge.5)go to 510
        NUMREQ = NUMREQ+1
        DUPCHK(NUMREQ,1) = TEM(1)
        DUPCHK(NUMREQ,2) = TEM(2)
        DUPCHK(NUMREQ,3) = TEM(3)
        DUPCHK(NUMREQ,4) = TEM(4)
        DUPCHK(NUMREQ,5) = TEM(6)
     end if

     !     SET THE TREELIST TYPE FLAG (LET IP BE THE RECORD OUTPUT COUNT).
     !     AND THE OUTPUT REPORTING YEAR.
     !     ITPLAB: 1=STANDARD COMPLETE LIVE TREE LIST, 2=DEAD TREELIST
     !             3=CUT TREE LIST, 4=AFTER TREATMENT TREE LIST.

     510 continue
     if (IWHO.eq.1) then
        JYR = IY(ICYC+1)
        IP = ITRN
        ITPLAB = 1
        if (NPRMS.ge.4.and. TEM(4).gt.0.)ITPLAB = 2
     else if (IWHO.eq.2) then
        JYR = IY(ICYC)
        IP = 0
        do I = 1,ITRN
           if (WK3(I).gt.0) IP = IP+1
           1 continue
        end do
        ITPLAB = 3
     else
        JYR = IY(ICYC)
        IP = 0
        do I = 1,ITRN
           if (PROB(I).gt.0)IP = IP+1
        end do
        ITPLAB = 4
     end if
     LTREE = .false.

     !     IF AT THE PRE-PROJECTION TIME, FIND OUT IF A TREELIST HAS BEEN
     !     SUPPRESSED OR IF A DEAD TREE LIST IS REQUESTED. IF YES, THEN
     !     BRANCH TO NOT ACCOMPLISH THE REQUEST.

     if (LSTART) then
        if ((NPRMS.ge.3.and. TEM(3).eq.1.)) goto 200
        if (NPRMS.ge.3.and. TEM(3).eq.2.)call OPDONE(ITODO,JYR)
     else
        if (NPRMS.ge.3.and. TEM(3).eq.2) go to 200

        !        SIGNAL THE OPTION DONE.

        if (IWHO.eq.1) then
           call OPDONE (ITODO,JYR-1)
        else if (IWHO.eq.2) then
           call OPDONE (ITODO,JYR)
        else
           call OPDONE (ITODO,JYR)
        end if
     end if

     !     CALL THE DATABASE TREELIST OUTPUT FUNCTION

     DBSKODE = 1
     call DBSTRLS(IWHO,DBSKODE,TEM(6))
     !     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
     if (DBSKODE.eq.0) return

     !     CALL THE DATABASE AFTER TREATMENT TREELIST OUTPUT FUNCTION

     DBSKODE = 1
     call DBSATRTLS(IWHO,DBSKODE,TEM(6))
     !     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
     if (DBSKODE.eq.0) return

     !     CALL THE DATABASE CUTLIST OUTPUT FUNCTION

     DBSKODE = 1
     call DBSCUTS(IWHO,DBSKODE)
     !     RETURN IF WE ARE REDIRECTING OUTPUT EXCLUSIVELY TO THE DATABASE
     if (DBSKODE.eq.0) return

     !     SET VARIABLES AND SWITCHES

     LHD = TEM(2).eq.0.0
     LRC = TEM(2).ge.0.0
     KOLIST = IFIX(TEM(1))
     LFORMT = KOLIST.gt.0
     if (.not.LFORMT) then
        LRC = .true.
        LHD = .false.
        KOLIST = IABS(KOLIST)
     end if

     !     MAKE SURE THE OUTPUT FILE IS OPENNED

     call openIfClosed (KOLIST,"trl",LOK)
     if (.not.LOK) return

     !     IF NO HEADING IS BEING WRITTEN, WRITE A MARKER AND THE NUMBER OF
     !     RECORDS WHICH FOLLOW.

     if (.not.LHD .and. LRC) then
        XXWT = SAMWT
        call REVISE (VARACD,REV)
        call GRDTIM (DAT,TIM)
        if (LFORMT) then
           write (KOLIST,2) IP,ICYC,JYR,NPLT,MGMID,VARACD,DAT,TIM,&
                &CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN
           2 format ('-999',3I5,6(1X,A),I3,E14.7,2(1X,A))
        else
           write (KOLIST,*) IP,ICYC,JYR,NPLT,MGMID,VARACD,DAT,TIM,&
                &CLAB1(ITPLAB)(1:1),IFINT,XXWT,REV,CISN
        end if
     end if
     IP = 0
     KNTREC = 0

     !     PROCESS TREELIST

     do ISPC = 1,MAXSP
        I1 = ISCT(ISPC,1)
        if (I1.eq.0) go to 60
        I2 = ISCT(ISPC,2)
        do I3 = I1,I2
           LTREE = .true.
           I = IND1(I3)
           if (.not.LHD) goto 120
           if (KNTREC.ne.0) go to 110
           IP = IP+1
           if (LFORMT) call GROHED (KOLIST)
           if (LFORMT) then

              select case (VARACD)

                 !  EASTERN VARIANTS

              case ('CS','LS','NE','SN','ON')
                 if (LMETRICVAR) then
                    write (KOLIST,9010) CLAB1(ITPLAB),NPLT,MGMID,&
                         &CLAB2(IWHO),ICYC,IFINT,JYR,IP
                    9010 format(/'COMPLETE ',A4,' LIST -- STAND: ',A26,T58,'MGMTID: ',&
                         &A4,T72,A3,' CYCLE: ',I2,T87,'CYCLE LENGTH: ',I2,' YRS',&
                         &T109,'YEAR: ',I4,T121,'PAGE: ',I3/&
                         &'  TREE   TREE SP SP TR SS PNT  ',&
                         &'TREES    MORTAL   CURR  DIB   CURR  HT',5X,'MAX',7X,'BA   ',&
                         &'POINT TOT MCH SAW CU SAW BD PW SL TRC',/,&
                         &' NUMBER  INDX CD NO CL CD NUM PER HA   PER HA    ',&
                         &'DIAM  INCR   HT  INCR CR  CW  MS ',&
                         &'%-TILE  BAL CU M  VL M  VOL M  VL  DF DF  HT',/,&
                         &'-------- ---- -- -- -- -- --- -------- -------- ----- ',&
                         &'----- ----- ---- -- ---- -- ------ ----- ------ ',&
                         &'------ ------- -- -- ---')
                 else
                    write (KOLIST,10) CLAB1(ITPLAB),NPLT,MGMID,&
                         &CLAB2(IWHO),ICYC,IFINT,JYR,IP
                    10 format(/'COMPLETE ',A4,' LIST -- STAND: ',A26,T58,'MGMTID: ',&
                         &A4,T72,A3,' CYCLE: ',I2,T87,'CYCLE LENGTH: ',I2,' YRS',&
                         &T109,'YEAR: ',I4,T121,'PAGE: ',I2/&
                         &'  TREE   TREE SP SP TR SS PNT  ',&
                         &'TREES    MORTAL   CURR  DIB   CURR  HT',5X,'MAX',7X,'BA   ',&
                         &'POINT TOT CU MCH CU SAW CU SAW BD PW SL TRC',/,&
                         &' NUMBER  INDX CD NO CL CD NUM PER ACRE PER ACRE  ',&
                         &'DIAM  INCR   HT  INCR CR  CW  MS ',&
                         &'%-TILE  BAL CU FT VL FT VOL FT VL  DF DF  HT',/,&
                         &'-------- ---- -- -- -- -- --- -------- -------- ----- ',&
                         &'----- ----- ---- -- ---- -- ------ ----- ------ ',&
                         &'------ ------- -- -- ---')
                 end if

                 !  WESTERN VARIANTS

              case default
                 if (LMETRICVAR) then
                    write (KOLIST,9011) CLAB1(ITPLAB),NPLT,MGMID,&
                         &CLAB2(IWHO),ICYC,IFINT,JYR,IP
                    9011 format(/'COMPLETE ',A4,' LIST -- STAND: ',A26,T58,'MGMTID: ',&
                         &A4,T72,A3,' CYCLE: ',I2,T87,'CYCLE LENGTH: ',I2,' YRS',&
                         &T109,'YEAR: ',I4,T121,'PAGE: ',I3/&
                         &'    TREE TREE SP SP TR SS PNT  ',&
                         &'  TREES   MORTAL  CURR  DIAM  CURR   HT',5X,'MAX',7X,' BA ',&
                         &'POINT TOT CU MCH CU         MC BF TRC',/,&
                         &'  NUMBER INDX CD NO CL CD NUM   PER HA   PER HA  ',&
                         &'DIAM  INCR    HT INCR CR   CW MS ',&
                         &'%-TILE    BA  M VOL  M VOL     N/A DF DF  HT',/,&
                         &'-------- ---- -- -- -- -- --- -------- -------- ----- ',&
                         &'----- ----- ---- -- ---- -- ------ ----- ------ ',&
                         &'------ ------- -- -- ---')
                 else
                    write (KOLIST,11) CLAB1(ITPLAB),NPLT,MGMID,&
                         &CLAB2(IWHO),ICYC,IFINT,JYR,IP
                    11 format(/'COMPLETE ',A4,' LIST -- STAND: ',A26,T58,'MGMTID: ',&
                         &A4,T72,A3,' CYCLE: ',I2,T87,'CYCLE LENGTH: ',I2,' YRS',&
                         &T109,'YEAR: ',I4,T121,'PAGE: ',I2/&
                         &'  TREE   TREE SP SP TR SS PNT  ',&
                         &'TREES    MORTAL   CURR  DIAM  CURR  HT',5X,'MAX',7X,'BA   ',&
                         &'POINT TOT CU MCH CU SAW CU MCH BD MC BF TRC',/,&
                         &' NUMBER  INDX CD NO CL CD NUM PER ACRE PER ACRE  ',&
                         &'DIAM  INCR   HT  INCR CR  CW  MS ',&
                         &'%-TILE  BAL  FT VOL FT VOL  FT VOL FT VOL DF DF  HT',/,&
                         &'-------- ---- -- -- -- -- --- -------- -------- ----- ',&
                         &'----- ----- ---- -- ---- -- ------ ----- ------ ',&
                         &'------ ------- ------- -- -- ---')
                 end if
              end select

           end if
           KNTREC = 1
           110 continue
           if (ITPLAB.eq.3) then
              if (WK3(I).gt.0) KNTREC = KNTREC+1
           else if (ITPLAB.eq.4) then
              if (PROB(I).gt.0) KNTREC = KNTREC+1
           else
              KNTREC = KNTREC+1
           end if
           if (KNTREC.gt.50) KNTREC = 0
           120 continue

           !      ITPLAB: 1=STANDARD COMPLETE LIVE TREE LIST, 2=DEAD TREELIST
           !              3=CUT TREE LIST, 4=AFTER TREATMENT TREE LIST.

           if (ITPLAB.eq.1) then
              P = PROB(I) / GROSPC
              if (ICYC.gt.0) then
                 DP = WK2(I)/ GROSPC
              else
                 DP = 0.0
              end if
           else if (ITPLAB.eq.2) then
              if (ICYC.gt.0) then
                 DP = WK2(I)/ GROSPC
                 P = 0.
              else
                 DP = 0.
                 P = 0.
              end if
           else if (ITPLAB.eq.3) then
              P = WK3(I)/GROSPC
              DP = 0.0
              if (P.le.0.0) goto 50
           else
              P = PROB(I)/GROSPC
              DP = 0.0
              if (P.le.0.0) goto 50
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

           !     GET MISTLETOE RATING FOR CURRENT TREE RECORD.

           call MISGET(I,IDMR)

           !     SET CROWN WIDTH.

           CW = CRWDTH(I)

           !     DECODE DEFECT AND ROUND OFF POINT BAL.

           ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
           IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
           if (LMETRICVAR) then
              IPTBAL = NINT(PTBALT(I)*FT2pACRtoM2pHA)
           else
              IPTBAL = NINT(PTBALT(I))
           end if

           !  CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.

           DGI = DG(I)
           if (ICYC.eq.0.and. TEM(6).eq.0) DGI = WORK1(I)

           select case (VARACD)

              !  EASTERN VARIANTS

           case ('CS','LS','NE','SN','ON')
              if (LFORMT) then
                 if (P.lt.9999.9995.and. DP.lt.9999.9995) then
                    if (LMETRICVAR) then
                       write(KOLIST,9021) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                            &ISPECL(I),IPVEC(ITRE(I)),&
                            &P/ACRtoHA,&
                            &DP/ACRtoHA,&
                            &DBH(I)*INtoCM,&
                            &DGI*INtoCM,&
                            &HT(I)*FTtoM,&
                            &HTG(I)*FTtoM,&
                            &ICR(I),&
                            &CW*FTtoM,&
                            &IDMR,PCT(I),IPTBAL,&
                            &CFV(I)*FT3toM3,&
                            &WK1(I)*FT3toM3,&
                            &BFV(I)*FT3toM3,&
                            &ICDF, IBDF,&
                            &NINT(real(ITRUNC(I)+5)*.01*FTtoM)
                       9021 format(A8,1X,I4,1X,A2,I3,2(1X,I2),1X,I3,1X,F8.3,1X,F8.3,1X,&
                            &F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
                            &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                            &1X,I3)
                    else
                       write(KOLIST,21) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                            &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI  ,HT(I),HTG(I),&
                            &ICR(I),CW,&
                            &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                            &((ITRUNC(I)+5)/100)
                       21 format(A8,1X,I4,1X,A2,I3,2(1X,I2),1X,I3,1X,F8.3,1X,F8.3,1X,&
                            &F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
                            &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F6.1,1X,F7.1,&
                            &1X,I2,1X,I2,1X,I3)
                    end if
                 else
                    if (LMETRICVAR) then
                       write(KOLIST,9021) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                            &ISPECL(I),IPVEC(ITRE(I)),&
                            &P/ACRtoHA,&
                            &DP/ACRtoHA,&
                            &DBH(I)*INtoCM,&
                            &DGI*INtoCM,&
                            &HT(I)*FTtoM,&
                            &HTG(I)*FTtoM,&
                            &ICR(I),&
                            &CW*FTtoM,&
                            &IDMR,PCT(I),IPTBAL,&
                            &CFV(I)*FT3toM3,&
                            &WK1(I)*FT3toM3,&
                            &BFV(I)*FT3toM3,&
                            &ICDF, IBDF,&
                            &NINT(real(ITRUNC(I)+5)*.01*FTtoM)
                       9020 format(A8,1X,2I4,I5,F9.3,F8.2,3F7.2,I3,I4,F9.3,I4,F9.2,&
                            &F9.3,F9.2,F9.2,I4,I7,I4)
                    else
                       write(KOLIST,20) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                            &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI  ,HT(I),HTG(I),&
                            &ICR(I),CW,&
                            &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                            &((ITRUNC(I)+5)/100)
                       20 format(A8,1X,2I4,I5,F9.3,F8.2,3F7.2,I3,I4,F9.3,I4,F9.2,&
                            &F9.3,F9.2,F9.2,F9.2,I4,I7,I4)
                    end if
                 end if
              else
                 if (LMETRICVAR) then
                    write(KOLIST,*) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                         &ISPECL(I),IPVEC(ITRE(I)),&
                         &P/ACRtoHA,&
                         &DP/ACRtoHA,&
                         &DBH(I)*INtoCM,&
                         &DGI*INtoCM,&
                         &HT(I)*FTtoM,&
                         &HTG(I)*FTtoM,&
                         &ICR(I),&
                         &CW*FTtoM,&
                         &IDMR,PCT(I),IPTBAL,&
                         &CFV(I)*FT3toM3,&
                         &WK1(I)*FT3toM3,&
                         &BFV(I)*FT3toM3,&
                         &ICDF, IBDF,&
                         &NINT(real(ITRUNC(I)+5)*.01*FTtoM)
                 else
                    write(KOLIST,*) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                         &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI,HT(I),HTG(I),&
                         &ICR(I),CW,&
                         &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                         &((ITRUNC(I)+5)/100)
                 end if
              end if

              !  WESTERN VARIANTS

           case default
              if (LFORMT) then
                 if (P.lt.9999.9995.and. DP.lt.9999.9995) then
                    if (LMETRICVAR) then
                       write(KOLIST,9023) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                            &ISPECL(I),IPVEC(ITRE(I)),&
                            &P/ACRtoHA,&
                            &DP/ACRtoHA,&
                            &DBH(I)*INtoCM,&
                            &DGI*INtoCM,&
                            &HT(I)*FTtoM,&
                            &HTG(I)*FTtoM,&
                            &ICR(I),&
                            &CW*FTtoM,&
                            &IDMR,PCT(I),IPTBAL,&
                            &CFV(I)*FT3toM3,&
                            &WK1(I)*FT3toM3,&
                            &BFV(I)*FT3toM3,&
                            &ICDF, IBDF,&
                            &NINT(real(ITRUNC(I)+5)*.01*FTtoM)
                       9023 format(A8,1X,I4,1X,A2,3(1X,I2),1X,I3,1X,F8.3,1X,F8.3,1X,&
                            &F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
                            &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                            &1X,I3)
                    else
                       write(KOLIST,23) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                            &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI  ,HT(I),HTG(I),&
                            &ICR(I),CW,&
                            &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                            &((ITRUNC(I)+5)/100)
                       23 format(A8,1X,I4,1X,A2,3(1X,I2),1X,I3,1X,F8.3,1X,F8.3,1X,&
                            &F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
                            &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                            &1X,I3)
                    end if
                 else
                    if (LMETRICVAR) then
                       write(KOLIST,9024) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                            &ISPECL(I),IPVEC(ITRE(I)),&
                            &P/ACRtoHA,&
                            &DP/ACRtoHA,&
                            &DBH(I)*INtoCM,&
                            &DGI*INtoCM,&
                            &HT(I)*FTtoM,&
                            &HTG(I)*FTtoM,&
                            &ICR(I),&
                            &CW*FTtoM,&
                            &IDMR,PCT(I),IPTBAL,&
                            &CFV(I)*FT3toM3,&
                            &WK1(I)*FT3toM3,&
                            &BFV(I)*FT3toM3,&
                            &ICDF, IBDF,&
                            &NINT(real(ITRUNC(I)+5)*.01*FTtoM)
                       9024 format(A8,1X,I4,1X,A2,3(1X,I2),1X,I3,1X,F8.2,1X,F8.2,1X,&
                            &F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
                            &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                            &1X,I3)
                    else
                       write(KOLIST,24) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                            &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI  ,HT(I),HTG(I),&
                            &ICR(I),CW,&
                            &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                            &((ITRUNC(I)+5)/100)
                       24 format(A8,1X,I4,1X,A2,3(1X,I2),1X,I3,1X,F8.2,1X,F8.2,1X,&
                            &F5.1,1X,F5.2,1X,F5.1,1X,F4.1,1X,I2,1X,F4.1,1X,I2,&
                            &1X,F6.2,1X,I5,1X,F6.1,1X,F6.1,1X,F6.1,1X,F7.1,1X,I2,1X,I2,&
                            &1X,I3)
                    end if
                 end if
              else
                 if (LMETRICVAR) then
                    write(KOLIST,*) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                         &ISPECL(I),IPVEC(ITRE(I)),&
                         &P/ACRtoHA,&
                         &DP/ACRtoHA,&
                         &DBH(I)*INtoCM,&
                         &DGI*INtoCM,&
                         &HT(I)*FTtoM,&
                         &HTG(I)*FTtoM,&
                         &ICR(I),&
                         &CW*FTtoM,&
                         &IDMR,PCT(I),IPTBAL,&
                         &CFV(I)*FT3toM3,&
                         &WK1(I)*FT3toM3,&
                         &BFV(I)*FT3toM3,&
                         &ICDF, IBDF,&
                         &NINT(real(ITRUNC(I)+5)*.01*FTtoM)
                 else
                    write(KOLIST,*) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                         &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI  ,HT(I),HTG(I),&
                         &ICR(I),CW,&
                         &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                         &((ITRUNC(I)+5)/100)
                 end if
              end if

           end select

           50 continue
        end do
        60 continue
     end do

     !  FOR CYCLE 0 TREELIST, PRINT DEAD TREES WHICH WERE PRESENT IN
     !  THE INVENTORY DATA AT THE BOTTOM OF THE TREELIST.

     if ((IREC2.ge.MAXTP1).or.(ITPLAB.eq.3).or.(ICYC.ge.1))go to 200

     do I = IREC2,MAXTRE

        !  WRITE HEADER FOR CASE WHERE NO LIVE TREES IN INVENTORY

        if ((.not.LTREE).and.LFORMT.and.LHD) then
           if (LFORMT) call GROHED (KOLIST)
           if (LMETRICVAR) then
              write (KOLIST,9011) CLAB1(ITPLAB),NPLT,MGMID,&
                   &CLAB2(IWHO),ICYC,IFINT,JYR,IP
           else
              write (KOLIST,11) CLAB1(ITPLAB),NPLT,MGMID,&
                   &CLAB2(IWHO),ICYC,IFINT,JYR,IP
           end if
           LTREE = .true.
        end if
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
        if (LMETRICVAR) then
           IPTBAL = NINT(PTBALT(I)*FT2pACRtoM2pHA)
        else
           IPTBAL = NINT(PTBALT(I))
        end if

        !  CYCLE 0, PRINT INPUT DG ONLY, UNLESS DIRECTED TO PRINT ESTIMATES.

        DGI = DG(I)
        if (ICYC.eq.0.and. TEM(6).eq.0) DGI = WORK1(I)

        !  PUT PROB IN MORTALITY COLUMN

        DP = P
        P = 0.

        if (LFORMT) then
           if (P.lt.9999.9995.and. DP.lt.9999.9995) then
              if (LMETRICVAR) then
                 write(KOLIST,9021) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                      &ISPECL(I),IPVEC(ITRE(I)),&
                      &P/ACRtoHA,&
                      &DP/ACRtoHA,&
                      &DBH(I)*INtoCM,&
                      &DGI*INtoCM,&
                      &HT(I)*FTtoM,&
                      &HTG(I)*FTtoM,&
                      &ICR(I),&
                      &CW*FTtoM,&
                      &IDMR,PCT(I),IPTBAL,&
                      &CFV(I)*FT3toM3,&
                      &WK1(I)*FT3toM3,&
                      &BFV(I)*FT3toM3,&
                      &ICDF, IBDF,&
                      &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM)
              else
                 write(KOLIST,21) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                      &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI  ,HT(I),HTG(I),&
                      &ICR(I),CW,&
                      &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                      &((ITRUNC(I)+5)/100)
              end if
           else
              if (LMETRICVAR) then
                 write(KOLIST,9020) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                      &ISPECL(I),IPVEC(ITRE(I)),&
                      &P/ACRtoHA,&
                      &DP/ACRtoHA,&
                      &DBH(I)*INtoCM,&
                      &DGI*INtoCM,&
                      &HT(I)*FTtoM,&
                      &HTG(I)*FTtoM,&
                      &ICR(I),&
                      &CW*FTtoM,&
                      &IDMR,PCT(I),IPTBAL,&
                      &CFV(I)*FT3toM3,&
                      &WK1(I)*FT3toM3,&
                      &BFV(I)*FT3toM3,&
                      &ICDF, IBDF,&
                      &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM)
              else
                 write(KOLIST,20) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                      &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI  ,HT(I),HTG(I),&
                      &ICR(I),CW,&
                      &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                      &((ITRUNC(I)+5)/100)
              end if
           end if
        else
           if (LMETRICVAR) then
              write(KOLIST,*) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                   &ISPECL(I),IPVEC(ITRE(I)),&
                   &P/ACRtoHA,&
                   &DP/ACRtoHA,&
                   &DBH(I)*INtoCM,&
                   &DGI*INtoCM,&
                   &HT(I)*FTtoM,&
                   &HTG(I)*FTtoM,&
                   &ICR(I),&
                   &CW*FTtoM,&
                   &IDMR,PCT(I),IPTBAL,&
                   &CFV(I)*FT3toM3,&
                   &WK1(I)*FT3toM3,&
                   &BFV(I)*FT3toM3,&
                   &ICDF, IBDF,&
                   &NINT(FLOAT(ITRUNC(I)+5)*.01*FTtoM)
           else
              write(KOLIST,*) TID,I,NSP(ISP(I),1)(1:2),ISP(I),IMC(I),&
                   &ISPECL(I),IPVEC(ITRE(I)),P,DP,DBH(I),DGI  ,HT(I),HTG(I),&
                   &ICR(I),CW,&
                   &IDMR,PCT(I),IPTBAL,CFV(I),MCFV(I),SCFV(I),BFV(I),ICDF,IBDF,&
                   &((ITRUNC(I)+5)/100)
           end if
        end if
        150 continue
     end do

     200 continue
  end do
  return
end
