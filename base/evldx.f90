!=======================================!
!   Load model values for expressions   !
!=======================================!
! BASE $Id$
subroutine EVLDX (XLDREG,NXLDX,INSTR,IRC)
  implicit none

  !     CALLED FROM ALGEVL

  !     LOADS X VALUES OWNED BY THE PROGNOSIS MODEL.

  !     N.L.CROOKSTON - APR 87 - FORESTRY SCIENCES LAB - MOSCOW, ID

  !     XLDREG= THE VALUE WANTED IS RETURNED AS XLDREG(1).
  !     NXLDX = THE LENGTH OF THE XLDREG ARRAY.
  !     INSTR = THE CODE THAT SAYS WHICH VALUE IS REQUESTED.
  !     IRC   = RETURN CODE, 0=OK, 1=VARIABLE IS CURRENTLY UNDEFINED,
  !             2=INSTRUCTION CODE COULD NOT BE DECIPHERED.

  !     IN THE METRIC VARIANTS (BC, ON) THE VALUES ARE RETURNED IN METRIC
  !     UNITS AND DBH/HEIGHT ARGUMENTS ARE GIVEN IN METRIC UNITS
  !     (LMETRICVAR BRANCHES).

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'
  include 'ARRAYS.f90'
  include 'OUTCOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'STDSTK.f90'
  include 'WORKCM.f90'
  include 'SSTGMC.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: M,I1,I2,I3,I4,INDEX(MAXTRE),ISRTI
  logical :: LDEB,LACTV,LINCL,LPARM
  real    :: XLDREG(NXLDX),DUMSDI
  integer :: ITMPDX(1)
  integer :: IHI,ILO,ISPC,IDMR,IG,IULIM,ILIM,NTREES,IDMI,K
  integer :: IRC,INSTR,NXLDX,I,J,JARGS,MYSTR,L,IGRP
  real    :: STAGEB,STAGEA,XSDI,CUT,DEAD,RES,XLDBH,XHDBH,SUMP,TPA,TREERD
  real    :: TEMSUM,RVAL,XLHT,XHHT,HTPCT,P,SUMPIN,X
  integer :: JPNUM,IGSP
  external RANN
  real    :: TPA3,DCM,ADIV,BADJ,ACRN
  integer :: JPT,JPTGRP
  real    :: CLSD2,CLSTPA,XCRD,CRDTFAC,CRDDFAC

  ITMPDX(1) = 0

  call DBCHK (LDEB,'EVLDX',5,ICYC)
  if (LDEB) write (JOSTND,5) INSTR,NXLDX
  5 format (/' IN EVLDX, INSTR=',I6,'; NXLDX=',I3)

  !     DECODE THE INSTRUCTION AND EXECUTE: LOAD A CONSTANT.
  !     THE METRIC TWIN STOPPED AT 7000 INSTEAD OF 9000 (PRESERVED
  !     DELIBERATELY).

  if (LMETRICVAR) then
     LPARM = INSTR.gt.1000.and. INSTR.lt.7000
  else
     LPARM = INSTR.gt.1000 .and. INSTR.lt. 9000
  end if
  if (LPARM) then
     XLDREG(1) = PARMS(INSTR-1000)
     if (LDEB) write (JOSTND,*) &
          & 'IN EVLDX, XLDREG(1)=PARMS(',INSTR-1000,')',PARMS(INSTR-1000)
     goto 1000
  end if

  !     DECODE THE INSTRUCTION AND EXECUTE: LOAD A SAVED VARIABLE.

  if (INSTR.lt.1000) then
     I = MOD(INSTR,100)
     J = INSTR/100
     if (LDEB) write (JOSTND,'('' IN EVLDX, I, J='',2I4)') I,J
     goto (1001,11,12,13,14,15,16,17,18,19,1001),(J+1)

     !     IN THE METRIC VARIANTS, A COMPUTED GOTO ON THE VARIABLE NUMBER
     !     SELECTS THE UNIT CONVERSION. THE CONVERSION LABELS (11NN, 12NN,
     !     13NN, 14NN OTHER THAN THE FIRST OF EACH SET) ARE REACHED ONLY
     !     FROM THESE GOTOS.

     11 continue
     if (LMETRICVAR) then
        goto (1100,1100,1101,1102,1102,1100,1103,1101,1104,1105,&
             &1100,1100,1100,1100,1101,1101,1101,1101,1101,1101,&
             &1101,1101,1101,1101,1101,1100,1100,1100,1104,1100,&
             &1100,1100,1100,1100,1104,1100,1101,1100,1100,1100) I
     end if
     1100 continue
     XLDREG(1) = TSTV1(I)
     goto 1000
     1101 continue
     XLDREG(1) = TSTV1(I) / ACRtoHA
     goto 1000
     1102 continue
     XLDREG(1) = TSTV1(I) * FT3pACRtoM3pHA
     goto 1000
     1103 continue
     XLDREG(1) = TSTV1(I) * FT2pACRtoM2pHA
     goto 1000
     1104 continue
     XLDREG(1) = TSTV1(I) * FTtoM
     goto 1000
     1105 continue
     XLDREG(1) = TSTV1(I) * INtoCM
     goto 1000

     12 continue
     if (IPHASE.lt.2) goto 1001
     if (LMETRICVAR) then
        goto (1201,1202,1202,1200,1203,1200,1204,1205,1201,1202,&
             &1202,1200,1200,1200,1200,1200,1200,1200,1200,1200,&
             &1200,1200,1200,1201,1200) I
     end if
     1200 continue
     XLDREG(1) = TSTV2(I)
     goto 1000
     1201 continue
     XLDREG(1) = TSTV2(I) / ACRtoHA
     goto 1000
     1202 continue
     XLDREG(1) = TSTV2(I) * FT3pACRtoM3pHA
     goto 1000
     1203 continue
     XLDREG(1) = TSTV2(I) * FT2pACRtoM2pHA
     goto 1000
     1204 continue
     XLDREG(1) = TSTV2(I) * FTtoM
     goto 1000
     1205 continue
     XLDREG(1) = TSTV2(I) * INtoCM
     goto 1000

     13 continue
     if (ICYC.lt.2) goto 1001
     if (LMETRICVAR) then
        goto (1302,1302,1302,1302,1301,1300,1303,1300,1300,1300,&
             &1300,1300,1300,1300,1300,1300,1300,1300,1300,1300) I
     end if
     1300 continue
     XLDREG(1) = TSTV3(I)
     goto 1000
     1301 continue
     XLDREG(1) = TSTV3(I) / ACRtoHA
     goto 1000
     1302 continue
     XLDREG(1) = TSTV3(I) * FT3pACRtoM3pHA
     goto 1000
     1303 continue
     XLDREG(1) = TSTV3(I) * FT2pACRtoM2pHA
     goto 1000

     14 continue
     if (.not.LTSTV4(I)) goto 1001
     if (LMETRICVAR) then
        goto (1400,1400,1400,1400,1400,1400,1400,1400,1400,1400,&
             &1400,1400,1400,1400,1400,1400,1400,1401,1401,1400,&
             &1400,1402,1400,1400,1400) I
     end if
     1400 continue
     XLDREG(1) = TSTV4(I)
     goto 1000
     1401 continue
     XLDREG(1) = TSTV4(I) * INtoCM
     goto 1000
     1402 continue
     XLDREG(1) = TSTV4(I) * FTtoM
     goto 1000

     15 continue ! OPCODE 501 to 599
     if (.not.LTSTV5(I)) goto 1001
     XLDREG(1) = TSTV5(I)
     goto 1000
     16 continue ! OPCODE 600 to 699
     if (.not.LTSTV5(I+100)) goto 1001
     XLDREG(1) = TSTV5(I+100)
     goto 1000
     17 continue ! OPCODE 700 to 799
     if (.not.LTSTV5(I+200)) goto 1001
     XLDREG(1) = TSTV5(I+200)
     goto 1000
     18 continue ! OPCODE 800 to 899
     if (.not.LTSTV5(I+300)) goto 1001
     XLDREG(1) = TSTV5(I+300)
     goto 1000
     19 continue ! OPCODE 900
     if (I.eq.0) then
        call RANN (XLDREG(1))
     else
        XLDREG(1) = FLOAT(I)
     end if
     goto 1000
  end if

  !     IF THE INSTRUCTION CODE IS LT 10000, THEN IT CAN NOT BE
  !     PROCESSED BY THIS PART OF THIS SUBROUTINE.

  if (INSTR.le.10000) goto 1002

  !     THE INSTRUCTION CODE IS GT 10000, THEN SET THE NUMBER OF
  !     ARGUMENTS TO THE OPCODE (JARGS) AND SET MYSTR TO THE INSTRUCTION
  !     LESS THE ARGUMENT COUNT.

  JARGS = MOD(INSTR,100)
  MYSTR = INSTR/100*100

  !  DECODE INSTRUCTION AND EXECUTE.    SPMCDBH:
  !   1ST ARGUMENT:
  !      1 = TREES PER ACRE
  !      2 = BASAL AREA PER ACRE
  !      3 = TOTAL CUBIC FOOT VOLUME PER ACRE
  !      4 = TOTAL MERCH BOARD FOOT VOLUME PER ACRE
  !      5 = QUADRATIC MEAN DIAMETER
  !      6 = AVERAGE HEIGHT
  !      7 = PERCENT COVER
  !      8 = AVERAGE DWARF MISTLETOE RATING
  !      9 = TOTAL MERCH CUBIC FOOT VOLUME PER ACRE (WESTERN VARIANTS)
  !        = TOTAL SAWTIMBER CUBIC FOOT VOLUME PER ACRE (EASTERN VARIANTS)
  !     10 = AVERAGE DIAMETER GROWTH INSIDE BARK
  !     11 = STAND DENSITY INDEX
  !     12 = SILVAH RELATIVE DENSITY
  !     13 = ZEIDE STAND DENSITY INDEX
  !     14 = CURTIS RELATIVE DENSITY
  !     15 = CUBIC SAWLOG VOLUME (NOT IN THE METRIC VARIANTS)
  !   2ND ARGUMENT:
  !      0 = ALL SPECIES
  !     -X = SPECIES GROUP X

  !  VARIABLE LINCL IS USED TO INDICATE WHETHER A TREE GETS INCLUDED IN
  !  THE CALCULATION OR NOT.

  if (MYSTR.eq.10600) then
     if (JARGS.lt.3) goto 1002
     L = IFIX(XLDREG(1)+.5)
     if (XLDREG(2).ge. 0.) then
        J = IFIX(XLDREG(2)+.5)
     else
        J = IFIX(XLDREG(2)-.5)
     end if
     K = IFIX(XLDREG(3)+.5)

     !       IF THE ATTRIBUTE (L) IS OUT OF RANGE OR UNDEFINED IN THE
     !       PHASE, THEN: ISSUE ERROR CODE.

     if (LMETRICVAR) then
        if (L.le.0.or. L.gt.14) goto 1002
     else
        if (L.le.0 .or. L.gt.15) goto 1002
     end if

     !       IF THE SPECIES (J) IS OUT OF RANGE, THEN: ISSUE ERROR CODE.
     !       NOTE: A NEGATIVE SPECIES NUMBER INDICATES A SPECIES GROUP

     if (J.gt.MAXSP) goto 1002
     if (J.lt.0 .and. NSPGRP.lt.-J) goto 1002

     !       IF THE MANAGEMENT CODE (K) IS OUT OF RANGE, THEN: ISSUE ERROR
     !       CODE.

     if (K.lt.0 .or. K.gt.3) goto 1002

     !       IF CALCULATING STAGE'S SDI, GET THE STAGE PARAMETERS

     if (L.eq.11)call SDICLS(0,0.,999.,1,XSDI,DUMSDI,STAGEA,STAGEB,0)

     !       IF CALCULATING CURTIS'S RD, GET THE BRYAN LU PARAMETERS
     !       CRDTFAC - Curtis Relative Density TPA factor
     !       CRDDFAC - Curtis Relative Density diameter factor

     if (L.eq.14)call RDCLS(0,0.,999.,1,CLSD2,CLSTPA,XCRD,0, &
          & CRDTFAC,CRDDFAC)

     !       FIND THE DBH RANGE AND THE HT RANGE.

     CUT = 0.0
     DEAD = 0.0
     RES = 0.0
     IDMI = 0
     XLDBH = 0.0
     XHDBH = 1E30
     XLHT = 0.0
     XHHT = 1E30
     JPNUM = 0
     JPTGRP = 0
     if (LMETRICVAR) then
        if (JARGS.ge.4) XLDBH = XLDREG(4) * CMtoIN
        if (JARGS.ge.5) XHDBH = XLDREG(5) * CMtoIN
        if (JARGS.ge.6) XLHT = XLDREG(6) * MtoFT
        if (JARGS.ge.7) XHHT = XLDREG(7) * MtoFT
     else
        if (JARGS.ge.4) XLDBH = XLDREG(4)
        if (JARGS.ge.5) XHDBH = XLDREG(5)
        if (JARGS.ge.6) XLHT = XLDREG(6)
        if (JARGS.ge.7) XHHT = XLDREG(7)
     end if
     if (JARGS.ge.8 .and. XLDREG(8).eq.1) DEAD = XLDREG(8)
     if (JARGS.ge.8 .and. XLDREG(8).eq.2) CUT = XLDREG(8)
     if (JARGS.ge.8 .and. XLDREG(8).eq.3) RES = XLDREG(8)
     if (JARGS.ge.8 .and. XLDREG(8).eq.4) IDMI = IFIX(XLDREG(8)+0.5)
     if (JARGS.ge.9) then
        if (XLDREG(9).lt.0.) then
           JPNUM = IFIX(XLDREG(9)-0.5)
           JPTGRP = -JPNUM
        else
           JPNUM = IFIX(XLDREG(9)+0.5)
        end if
     end if

     ! IF THE POINT NUMBER IS PRESENT, AND VALID, AND A POINT NUMBER FROM
     ! THE INVENTORY DATA, CONVERT IT TO THE CORRESPONDING FVS SEQUENTIAL
     ! POINT NUMBER

     XLDREG(1) = 0.0
     if (JPNUM.gt.0) then
        if (ITHNPI .le. 0 .or. ITHNPI.gt.2) then
           go to 1000
        else if (ITHNPI .eq. 1) then
           do I = 1,IPTINV
              if (JPNUM .eq. IPVEC(I))go to 89
           end do
           go to 1000
           89 continue
           JPNUM = I
        else if (ITHNPI .eq. 2) then
           if (JPNUM .gt. IPTINV) then
              go to 1000
           end if
        end if
     end if
     if (LDEB)write(JOSTND,*)' AFTER POINT PROCESSING JPNUM= ',JPNUM

     SUMP = 0.0
     NTREES = 0

     if (CUT.gt.0. .and. IPHASE.lt.2) go to 1001
     if (RES.gt.0. .and. IPHASE.lt.2) go to 1001
     ILIM = ITRN
     if (CUT .ne. 0) ILIM = MAXTRE
     if (ILIM.gt.0) then
        do I = 1,ILIM
           LINCL = .false.
           if (J.eq.0 .or. J.eq.ISP(I)) then
              LINCL = .true.
           else if (J.lt.0) then
              IGRP = -J
              IULIM = ISPGRP(IGRP,1)+1
              do IG = 2,IULIM
                 if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                    LINCL = .true.
                    go to 91
                 end if
                 90 continue
              end do
           end if
           91 continue

           !  CHECK TO SEE IF TREE IS ON A POINT IN A POINT GROUP

           if (JPTGRP.gt.0) then
              if (ITHNPI .le. 0 .or. ITHNPI.gt.2)go to 1000
              do JPT = 2,IPTGRP(JPTGRP,1)+1
                 if (ITHNPI .eq. 1) then
                    if (IPTGRP(JPTGRP,JPT).eq.IPVEC(ITRE(I))) then
                       JPNUM = ITRE(I)
                       go to 95
                    end if
                 else if (ITHNPI .eq. 2) then
                    if (IPTGRP(JPTGRP,JPT).eq.ITRE(I)) then
                       JPNUM = ITRE(I)
                       if (JPNUM .gt. IPTINV)go to 1000
                       goto 95
                    end if
                 end if
              end do
              LINCL = .false.
           end if
           95 continue
           if (JPNUM.gt.0 .and. JPNUM.ne.ITRE(I)) LINCL = .false.
           if (LINCL .and. &
                & (K.eq.0  .or. K.eq.IMC(I)) .and. &
                & (DBH(I).ge.XLDBH)          .and. &
                & (DBH(I).lt.XHDBH)          .and. &
                & (HT(I).ge.XLHT)            .and. &
                & (HT(I).lt.XHHT)) then

              TPA = PROB(I)
              if (DEAD.ne.0.) then
                 if (ICYC.le.1) then
                    TPA = 0.
                 else
                    TPA = WK2(I)
                 end if
              else if (CUT.ne.0.) then
                 TPA = WK4(I)
              else if (IDMI.ne.0.) then
                 call MISGET(I,IDMR)
                 if (IDMR .eq. 0)TPA = 0.
              end if
              if (JPNUM.gt.0)TPA = TPA*(PI-FLOAT(NONSTK))

              SUMP = SUMP+TPA
              goto (111,112,113,114,115,116,117,118,119,120,121,122,123, &
                   & 124,125),L

              111 continue
              XLDREG(1) = XLDREG(1)+TPA
              goto 190
              112 continue
              XLDREG(1) = XLDREG(1)+(TPA*DBH(I)*DBH(I)*.005454154)
              goto 190
              113 continue
              if (DEAD.ne.0.) then
                 XLDREG(1) = XLDREG(1)+(TPA*PTOCFV(I))
              else
                 XLDREG(1) = XLDREG(1)+(TPA*CFV(I))
              end if
              goto 190
              114 continue
              if (DEAD.ne.0.) then
                 XLDREG(1) = XLDREG(1)+(TPA*PMRBFV(I))
              else
                 XLDREG(1) = XLDREG(1)+(TPA*BFV(I))
              end if
              goto 190
              115 continue
              XLDREG(1) = XLDREG(1)+(TPA*DBH(I)*DBH(I))
              goto 190
              116 continue
              XLDREG(1) = XLDREG(1)+(TPA*HT(I))
              goto 190
              117 continue
              NTREES = NTREES+1
              WORK1(NTREES) = CRWDTH(I)

              !           NOTE:  0.785398 DEALS WITH PI*TRECW/2*TRECW/2

              WORK1(NTREES) = WORK1(NTREES)*WORK1(NTREES)*TPA*0.785398
              goto 190
              118 continue
              call MISGET(I,IDMR)
              XLDREG(1) = XLDREG(1)+(FLOAT(IDMR)*TPA)
              goto 190
              119 continue
              if (DEAD.ne.0.) then
                 XLDREG(1) = XLDREG(1)+(TPA*PMRCFV(I))
              else
                 if (LMETRICVAR) then
                    XLDREG(1) = XLDREG(1)+(TPA*WK1(I))
                 else
                    XLDREG(1) = XLDREG(1)+(TPA*MCFV(I))
                 end if
              end if
              goto 190
              120 continue
              XLDREG(1) = XLDREG(1)+(TPA*DG(I))
              goto 190
              121 continue
              if (DBH(I).lt.DBHSTAGE)go to 190 ! BRANCH IF D IS LT MIN DBH
              XLDREG(1) = XLDREG(1)+(STAGEA + STAGEB*(DBH(I)**2.0))*TPA
              goto 190
              122 continue
              call RDSLTR(ISP(I),I,TREERD)
              XLDREG(1) = XLDREG(1)+(TPA*TREERD)/GROSPC
              goto 190
              123 continue
              if (DBH(I).lt.DBHZEIDE)go to 190 ! BRANCH IF D IS LT MIN DBH
              XLDREG(1) = XLDREG(1)+((DBH(I)/10)**1.605)*TPA
              goto 190
              124 continue

              !           CURTIS RELATIVE DENSITY

              if (DBH(I).lt.DBHSTAGE)go to 190 ! BRANCH IF D IS LT MIN DBH
              XLDREG(1) = XLDREG(1)+(CRDTFAC + CRDDFAC*(DBH(I)**2.0))*TPA
              goto 190

              !           NEW SPMCDBH OPTIONS ADDED AS PART OF FIANVB INCLUSION, 2024 (DW)

              !           CUBIC SAWLOG VOLUME
              125 continue
              if (DEAD.ne.0.) then
                 XLDREG(1) = XLDREG(1)+(TPA*PSCFV(I))
              else
                 XLDREG(1) = XLDREG(1)+(TPA*SCFV(I))
              end if
              goto 190

           end if
           190 continue
        end do

        !  NORMALIZE GROUP POINT STATISTICS BASED ON GROUP AREA,
        !  I.E. NUMBER OF POINTS IN GROUP

        if (((L.ge.1).and.(L.le.4)).or.(L.eq.9).or. &
             & ((L.ge.11).and.(L.le.14))) then
           if (JPTGRP.gt.0) then
              if (IPTGRP(JPTGRP,1).gt.0) then
                 XLDREG(1) = XLDREG(1)/FLOAT(IPTGRP(JPTGRP,1))
              end if
           end if
        end if

        if (L.eq.5 .or. L.eq.6 .or. L.eq.8 .or. L.eq.10) then
           if (SUMP.gt.0.0001) then
              if (L.eq.5) XLDREG(1) = SQRT(XLDREG(1)/SUMP)
              if (L.eq.6 .or. L.eq.8 .or. L.eq.10) &
                   & XLDREG(1) = XLDREG(1)/SUMP
           else
              XLDREG(1) = 0.0
           end if
        else if (L.eq.7) then
           call COVOLP (LDEB,JOSTND,NTREES,ITMPDX,WORK1,XLDREG(1), &
                & CCCOEF)
        else if ((L.ge.1 .and. L.le.4) .or. L.eq.9) then
           XLDREG(1) = XLDREG(1)/GROSPC
        end if

        if (LMETRICVAR) then

           !  CONVERT THE RESULT TO METRIC UNITS.

           goto (911,912,913,914,915,916,917,918,919,920,921,922,923,&
                &924), L

           911 continue
           XLDREG(1) = XLDREG(1) / ACRtoHA
           goto 1000
           912 continue
           XLDREG(1) = XLDREG(1) * FT2pACRtoM2pHA
           goto 1000
           913 continue
           XLDREG(1) = XLDREG(1) * FT3pACRtoM3pHA
           goto 1000
           914 continue
           XLDREG(1) = XLDREG(1)
           goto 1000
           915 continue
           XLDREG(1) = XLDREG(1) * INtoCM
           goto 1000
           916 continue
           XLDREG(1) = XLDREG(1) * FTtoM
           goto 1000
           917 continue
           XLDREG(1) = XLDREG(1)
           goto 1000
           918 continue
           XLDREG(1) = XLDREG(1)
           goto 1000
           919 continue
           XLDREG(1) = XLDREG(1) * FT3pACRtoM3pHA
           goto 1000
           920 continue
           XLDREG(1) = XLDREG(1) * INtoCM
           goto 1000
           921 continue
           XLDREG(1) = XLDREG(1) / ACRtoHA
           goto 1000
           922 continue
           XLDREG(1) = XLDREG(1) / ACRtoHA
           goto 1000
           923 continue
           XLDREG(1) = XLDREG(1) / ACRtoHA
           goto 1000
           924 continue
           XLDREG(1) = XLDREG(1) / ACRtoHA
           goto 1000
        end if
     end if

     !  WITH NO TREES (ILIM.LE.0) THE METRIC TWIN DOES NOT BRANCH TO 1000
     !  HERE; IT FALLS THROUGH THE REMAINING (NON-MATCHING) MYSTR TESTS TO
     !  1000. PRESERVED AS IS.

     if (.not. LMETRICVAR) goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    ACCFSP & BCCFSP:

  if (MYSTR.eq.11200.or.MYSTR.eq.11300) then
     if (JARGS.ne.1) goto 1002
     if (XLDREG(1).ge. 0.) then
        I = IFIX(XLDREG(1)+.5)
     else
        I = IFIX(XLDREG(1)-.5)
     end if
     if (I.gt.MAXSP) goto 1002
     if (I.lt.0 .and. NSPGRP.lt.-I) goto 1002
     if (MYSTR.eq.11300 .and. IPHASE.lt.2) goto 1001
     if (I .lt. 0) then
        IGRP = -I
        IULIM = ISPGRP(IGRP,1)+1
        TEMSUM = 0.
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           if (MYSTR.eq.11200) then
              TEMSUM = TEMSUM + BCCFSP(IGSP)
           else
              TEMSUM = TEMSUM + ACCFSP(IGSP)
           end if
           191 continue
        end do
        XLDREG(1) = TEMSUM
     else if (I .eq. 0) then
        TEMSUM = 0.
        do ISPC = 1,MAXSP
           if (MYSTR.eq.11200) then
              TEMSUM = TEMSUM + BCCFSP(ISPC)
           else
              TEMSUM = TEMSUM + ACCFSP(ISPC)
           end if
           195 continue
        end do
        XLDREG(1) = TEMSUM
     else
        if (MYSTR.eq.11200) then
           XLDREG(1) = BCCFSP(I)
        else
           XLDREG(1) = ACCFSP(I)
        end if
     end if
     go to 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    DBHDIST:

  if (MYSTR.eq.10500) then
     if (JARGS.lt.2) goto 1002
     I = IFIX(XLDREG(1)+.5)
     J = IFIX(XLDREG(2)+.5)

     !       IF THE ATTRIBUTE (I) IS OUT OF RANGE OR UNDEFINED IN THE
     !       PHASE, THEN: ISSUE ERROR CODE.

     if (LMETRICVAR) then
        if (I.le.0.or. I.gt.11) goto 1002
     else
        if (I.le.0 .or. I.gt.13) goto 1002
     end if
     if (I.ge.7 .and. IPHASE.eq.1) goto 1001
     if (J.le.0 .or. J.gt. 7) goto 1002
     goto (201,202,203,204,205,206,207,208,209,210,211,212,213),I
     201 continue
     if (LMETRICVAR) then
        XLDREG(1) = OACC(J)   * INtoCM
     else
        XLDREG(1) = OACC(J)
     end if
     goto 1000
     202 continue
     if (LMETRICVAR) then
        XLDREG(1) = OMORT(J)  * INtoCM
     else
        XLDREG(1) = OMORT(J)
     end if
     goto 1000
     203 continue
     if (LMETRICVAR) then
        XLDREG(1) = ONTCUR(J) * INtoCM
     else
        XLDREG(1) = ONTCUR(J)
     end if
     goto 1000
     204 continue
     if (LMETRICVAR) then
        XLDREG(1) = OCVCUR(J) * INtoCM
     else
        XLDREG(1) = OCVCUR(J)
     end if
     goto 1000
     205 continue
     if (LMETRICVAR) then
        XLDREG(1) = OMCCUR(J) * INtoCM
     else
        XLDREG(1) = OMCCUR(J)
     end if
     goto 1000
     206 continue
     if (LMETRICVAR) then
        XLDREG(1) = OBFCUR(J) * INtoCM
     else
        XLDREG(1) = OBFCUR(J)
     end if
     goto 1000
     207 continue
     if (LMETRICVAR) then
        XLDREG(1) = ONTREM(J) * INtoCM
     else
        XLDREG(1) = ONTREM(J)
     end if
     goto 1000
     208 continue
     if (LMETRICVAR) then
        XLDREG(1) = OCVREM(J) * INtoCM
     else
        XLDREG(1) = OCVREM(J)
     end if
     goto 1000
     209 continue
     if (LMETRICVAR) then
        XLDREG(1) = OMCREM(J) * INtoCM
     else
        XLDREG(1) = OMCREM(J)
     end if
     goto 1000
     210 continue
     if (LMETRICVAR) then
        XLDREG(1) = OBFREM(J) * INtoCM
     else
        XLDREG(1) = OBFREM(J)
     end if
     goto 1000
     211 continue
     if (LMETRICVAR) then
        XLDREG(1) = ONTRES(J) * INtoCM
     else
        XLDREG(1) = ONTRES(J)
     end if
     goto 1000
     212 continue
     XLDREG(1) = OSCCUR(J)
     goto 1000
     213 continue
     XLDREG(1) = OSCREM(J)
     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    SUMSTAT:

  if (MYSTR.eq.10400) then
     if (LMETRICVAR) then
        if (LDEB) write(JOSTND,310) (J,(IOSUM(J,I),J = 1,17),I = 1,ICYC+1)
     else
        if (LDEB) write(JOSTND,310) (J,(IOSUM(J,I),J = 1,22),I = 1,ICYC+1)
     end if
     310 format (' IN EVLDX: CURRENT IOSUM:'/((1X,I3,17I7)))
     if (JARGS.lt.2) goto 1002
     I = IFIX(XLDREG(1)+.5)
     J = IFIX(XLDREG(2)+.5)
     if (I.le.0 .or. I.gt.MAXCY1) goto 1001
     if (LMETRICVAR) then
        if (J.le.0.or. J.gt.17) goto 1002
     else
        if (J.le.0 .or. J.gt.22) goto 1002
     end if
     if (I.gt.ICYC+1) goto 1001
     if (LMETRICVAR) then
        goto(3000,3000,3001,3002,3002,3000,3001,3002,3002,3000,&
             &3003,3000,3004,3000,3002,3002,3000), J
     end if
     3000 continue
     XLDREG(1) = IOSUM(J,I)
     goto 1000
     3001 continue
     XLDREG(1) = IOSUM(J,I) / ACRtoHA
     goto 1000
     3002 continue
     XLDREG(1) = IOSUM(J,I) * FT3pACRtoM3pHA
     goto 1000
     3003 continue
     XLDREG(1) = IOSUM(J,I) * FT2pACRtoM2pHA
     goto 1000
     3004 continue
     XLDREG(1) = IOSUM(J,I) * FTtoM
     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    FUELLOAD:

  if (MYSTR.eq.11700) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.2) goto 1002

     !       ONE OR TWO ARGS CAN BE GIVEN

     ILO = IFIX(XLDREG(1)+.5)
     if (JARGS.eq.1) then
        IHI = ILO
     else
        IHI = IFIX(XLDREG(2)+.5)
     end if

     !       CHECK RANGE AND VALUES: MUST BE 1-11 (SEE DIMENSIONING OF 2ND
     !       DIMENSION OF CWD ARRAY IN FMCOM.F77)

     if (IHI.lt.ILO) goto 1002
     if (ILO.lt.1 .or. IHI.gt.11) goto 1002

     call FMEVCWD(RVAL, ILO, IHI, I)
     if (I.eq.1) goto 1001
     if (LMETRICVAR) then
        XLDREG(1) = RVAL * TItoTM / ACRtoHA
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    SNAGS:

  if (MYSTR.eq.11800) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002

     if (JARGS.lt.3) goto 1002
     L = IFIX(XLDREG(1)+.5)
     if (XLDREG(2).ge. 0.) then
        J = IFIX(XLDREG(2)+.5)
     else
        J = IFIX(XLDREG(2)-.5)
     end if
     K = IFIX(XLDREG(3)+.5)

     !  CHECK BOUNDARIES FOR
     !  (L=1:3)  TPA,BA OR VOL
     !  (J=0:MAXSP) ALL,SPP; <0=SPECIES GROUP
     !  (K=0:2)  ALL,HARD,SOFT

     if (L.lt.1 .or. L.gt.3) goto 1002
     if (J.gt.MAXSP) goto 1002
     if (J.lt.0 .and. NSPGRP.lt.-J) goto 1002
     if (K.lt.0 .or. K.gt.2) goto 1002

     !       FIND THE DBH RANGE AND THE HT RANGE.

     XLDBH = 0.0
     XHDBH = 1E30
     XLHT = 0.0
     XHHT = 1E30
     M = 0
     if (JARGS.ge.4) XLDBH = XLDREG(4)
     if (JARGS.ge.5) XHDBH = XLDREG(5)
     if (JARGS.ge.6) XLHT = XLDREG(6)
     if (JARGS.ge.7) XHHT = XLDREG(7)
     if (JARGS.ge.8) M = INT(XLDREG(8))

     if ((M.eq.1) .and. (IPHASE.lt.2)) goto 1001

     if (LMETRICVAR) then
        XLDBH = XLDBH * CMtoIN
        XHDBH = XHDBH * CMtoIN
        XLHT = XLHT * MtoFT
        XHHT = XHHT * MtoFT
     end if

     call FMEVSNG(RVAL, L, J, K, XLDBH, XHDBH, XLHT, XHHT, I)
     if (I.eq.1) goto 1001
     if (LMETRICVAR) then
        XLDREG(1) = RVAL * FT3pACRtoM3pHA
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    POTFLEN:
  !     1 = SEVERE (WILDFIRE) FLAME LENGTH
  !     2 = MODERATE (PRESCRIBED) FLAME LENGTH

  if (MYSTR.eq.11900) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.2) goto 1002

     !       ONE ARG CAN BE GIVEN

     J = IFIX(XLDREG(1)+.5)

     !       CHECK RANGE AND VALUES: MUST BE 1-8 (SEE DIMENSIONING OF 2ND
     !       DIMENSION OF CWD ARRAY IN FMCOM.F77)

     if (J.lt. 1 .or. J .gt. 4) goto 1002

     call FMEVFLM(RVAL, J, I)
     if (I.eq.1) goto 1001
     if (LMETRICVAR) then
        XLDREG(1) = RVAL * FTtoM
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    POTFMORT:

  if (MYSTR.eq.12100) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.1) goto 1002

     !       ONE ARG CAN BE GIVEN

     J = IFIX(XLDREG(1)+.5)

     !       CHECK RANGE AND VALUES: MUST BE 1-4

     if (J.lt. 1 .or. J .gt. 4) goto 1002

     call FMEVMRT(RVAL, J, I)
     if (I.eq.1) goto 1001
     if (LMETRICVAR) then
        if (J.le.2) then
           XLDREG(1) = RVAL
        else
           XLDREG(1) = RVAL * FT3pACRtoM3pHA
        end if
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    FUELMODS:

  if (MYSTR.eq.12200) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.2) goto 1002

     !       TWO ARGS CAN BE GIVEN

     L = IFIX(XLDREG(1)+.5)
     J = IFIX(XLDREG(2)+.5)

     !       CHECK RANGE AND VALUES: ARG1 MUST BE 1-4, ARG2 MUST BE 1-2

     if (L .lt. 1 .or. L .gt. 4) goto 1002
     if (J .lt. 1 .or. J .gt. 2) goto 1002
     call FMEVFMD(RVAL, L, J, I)
     if (I.eq.1) goto 1001
     XLDREG(1) = RVAL

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    SALVVOL:

  if (MYSTR.eq.12300) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.3) goto 1002

     !       CHECK BOUNDARIES FOR
     !       (J=0:11) ALL,SPP

     if (XLDREG(1).ge. 0.) then
        J = IFIX(XLDREG(1)+.5)
     else
        J = IFIX(XLDREG(1)-.5)
     end if
     if (J.gt.MAXSP) goto 1002
     if (J.lt.0 .and. NSPGRP.lt.-J) goto 1002

     !       FIND THE DBH RANGE

     if (LMETRICVAR) then
        XLDBH = XLDREG(2) * CMtoIN
        XHDBH = XLDREG(3) * CMtoIN
     else
        XLDBH = XLDREG(2)
        XHDBH = XLDREG(3)
     end if

     call FMEVSAL(RVAL, J, XLDBH, XHDBH, I)
     if ((I.eq.1) .or. (IPHASE.lt.2)) goto 1001
     if (LMETRICVAR) then
        XLDREG(1) = RVAL * FT3pACRtoM3pHA
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    POINTID:

  if (MYSTR.eq.12400) then
     if (JARGS.ne.1) goto 1002
     I = IFIX(XLDREG(1)+.5)
     if (I.le.0 .or. I.gt.IPTINV) goto 1001
     XLDREG(1) = IPVEC(I)
     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    STRSTAT:

  if (MYSTR.eq.12500) then
     if (.not.LCALC) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.2) goto 1002

     !       TWO ARGS CAN BE GIVEN, FIRST IS MANDATORY

     J = 0
     L = IFIX(XLDREG(1)+.5)
     if (JARGS.eq.2) J = IFIX(XLDREG(2)+.5)

     !       CHECK RANGE AND VALUES: ARG1 MUST BE 1-33, ARG2 MUST BE 0-1

     if (L .lt. 1 .or. L .gt. 33) goto 1002
     if (J .lt. 0 .or. J .gt. 1) goto 1002
     if (J.eq.1 .and. IPHASE.lt.2) go to 1001
     if (LMETRICVAR) then
        X = 1.0
        select case (L)
        case (1,11,21)
           X = INtoCM
        case (2:5,12:15,22:25)
           X = FTtoM
        case (10,20,30)
           X = HAtoACR
        end select
        XLDREG(1) = OSTRST(L,J+1) * X
     else
        XLDREG(1) = OSTRST(L,J+1)
     end if
     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    POTFTYPE:

  if (MYSTR.eq.12600) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.1) goto 1002

     !       ONE ARG CAN BE GIVEN

     J = IFIX(XLDREG(1)+.5)

     !       CHECK RANGE AND VALUES: MUST BE 1-2

     if (J.lt. 1 .or. J .gt. 2) goto 1002

     call FMEVTYP(RVAL, J, I)
     if (I.eq.1) goto 1001
     XLDREG(1) = RVAL

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    POTSRATE:

  if (MYSTR.eq.12700) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.1) goto 1002

     !       ONE ARG CAN BE GIVEN

     J = IFIX(XLDREG(1)+.5)

     !       CHECK RANGE AND VALUES: MUST BE 1-4

     if (J.lt. 1 .or. J .gt. 4) goto 1002

     call FMEVSRT(RVAL, J, I)
     if (I.eq.1) goto 1001
     if (LMETRICVAR) then
        XLDREG(1) = RVAL * FTtoM ! ft/min -> m/min
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    POTREINT:

  if (MYSTR.eq.12800) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.1) goto 1002

     !       ONE ARG CAN BE GIVEN

     J = IFIX(XLDREG(1)+.5)

     !       CHECK RANGE AND VALUES: MUST BE 1-2

     if (J.lt. 1 .or. J .gt. 2) goto 1002

     call FMEVRIN(RVAL, J, I)
     if (I.eq.1) goto 1001
     if (LMETRICVAR) then
        ! btu/ft**2/min -> kJ/m**2/min
        XLDREG(1) = RVAL * BTUtoKJ / FT2toM2
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    TREEBIO:
  !     NO ARGUMENTS ARE REQUIRED

  if (MYSTR.eq.12900) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.3) goto 1002

     !  SET DEFAULTS

     I1 = 1 ! INCLUDE STANDING AND REMOVED
     I2 = 1 ! INCLUDE LIVE AND DEAD
     I3 = 1 ! INCLUDE STEM AND CROWN
     I4 = 0 ! ICLUDE ALL SPECIES

     !  ARG1-FIND TREE STATUS -  STANDING(<0), REMOVED(0),OR BOTH(>0)

     if (XLDREG(1).ge. 0.) then
        I1 = IFIX(XLDREG(1)+.5)
     else if (XLDREG(1).eq. 0.) then
        I1 = IFIX(XLDREG(1))
     else
        I1 = IFIX(XLDREG(1)-.5)
     end if

     !  ARG2-FIND TREE TYPE -  DEAD(<0), LIVE(0),OR BOTH(>0)

     if (XLDREG(2).ge. 0.) then
        I2 = IFIX(XLDREG(2)+.5)
     else if (XLDREG(2).eq. 0.) then
        I2 = IFIX(XLDREG(2))
     else
        I2 = IFIX(XLDREG(2)-.5)
     end if

     !  ARG3-FIND TREE PART -  STEM(<0), CROWN(0),OR BOTH-WHOLE TREE(>0)

     if (XLDREG(3).ge. 0.) then
        I3 = IFIX(XLDREG(3)+.5)
     else if (XLDREG(3).eq. 0.) then
        I3 = IFIX(XLDREG(3))
     else
        I3 = IFIX(XLDREG(3)-.5)
     end if

     !  ARG4-FIND SPECIES
     !       (J=0:MAXSP) ALL,SPP

     if (XLDREG(4).ge. 0.) then
        I4 = IFIX(XLDREG(4)+.5)
     else
        I4 = IFIX(XLDREG(4)-.5)
     end if
     if (I4.gt.MAXSP) goto 1002
     if (I4.lt.0 .and. NSPGRP.lt.-I4) goto 1002

     !       FIND THE DBH RANGE AND THE HT RANGE.

     XLDBH = 0.0
     XHDBH = 1E30
     XLHT = 0.0
     XHHT = 1E30
     M = 0
     if (LMETRICVAR) then
        if (JARGS.ge.5) XLDBH = XLDREG(5) * CMtoIN
        if (JARGS.ge.6) XHDBH = XLDREG(6) * CMtoIN
        if (JARGS.ge.7) XLHT = XLDREG(7) * MtoFT
        if (JARGS.ge.8) XHHT = XLDREG(8) * MtoFT
     else
        if (JARGS.ge.5) XLDBH = XLDREG(5)
        if (JARGS.ge.6) XHDBH = XLDREG(6)
        if (JARGS.ge.7) XLHT = XLDREG(7)
        if (JARGS.ge.8) XHHT = XLDREG(8)
     end if

     call FMEVTBM(RVAL,I1,I2,I3,I4,XLDBH,XHDBH,XLHT,XHHT,I)
     if (I.eq.1) goto 1001
     if (LMETRICVAR) then
        XLDREG(1) = RVAL * TItoTM / ACRtoHA
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    CARBSTAT:

  if (MYSTR.eq.13000) then

     !       RETURN IF FIRE MODEL IS INACTIVE

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.lt.1 .or. JARGS.gt.1) goto 1002

     !       ONE ARG CAN BE GIVEN

     J = IFIX(XLDREG(1)+.5)

     !       CHECK RANGE AND VALUES: MUST BE 1-17

     if (J.lt. 1 .or. J .gt. 17) goto 1002

     call FMEVCARB(RVAL, J, I)
     if (I.eq.1) goto 1001
     XLDREG(1) = RVAL

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    HTDIST:

  if (MYSTR.eq.13100) then
     if (JARGS.lt.1 .or. JARGS.gt.1) goto 1002

     !       ONE ARG CAN BE GIVEN

     J = IFIX(XLDREG(1)+.5)

     !       CHECK RANGE AND VALUES: MUST BE 1-100

     if (J.lt. 1 .or. J .gt. 100) goto 1002
     HTPCT = 1-(FLOAT(J)/100)

     do I = 1,MAXTRE
        if (I .le. ITRN) then
           INDEX(I) = I
        else
           INDEX(I) = 0
        end if
        400 continue
     end do
     call RDPSRT(ITRN,HT,INDEX,.false.)

     SUMPIN = 0.
     XLDREG(1) = 0.
     do I = 1,ITRN
        ISRTI = INDEX(I)
        P = PROB(ISRTI)
        if (HT(ISRTI) .ge. XLDREG(1)) then
           SUMPIN = SUMPIN + P
        end if
        if (LMETRICVAR) then
           if (SUMPIN.gt.TPROB*HTPCT .and. XLDREG(1).eq.0.) &
                & XLDREG(1) = HT(ISRTI) * FTtoM
        else
           if (SUMPIN.gt.TPROB*HTPCT .and. XLDREG(1).eq.0.) &
                & XLDREG(1) = HT(ISRTI)
        end if
        410 continue
     end do
     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    HERBSHRB:

  if (MYSTR.eq.13200) then

     !       RETURN IF FIRE MODEL IS INACTIVE
     !       RETURN IF THE WRONG NUMBER OF ARGUMENTS ARE PRESENT

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.ne.1) goto 1002

     !       CHECK RANGE OF VALUES: MUST BE 1-3

     ILO = IFIX(XLDREG(1)+.5)
     if (ILO.lt.1 .or. ILO.gt.3) goto 1002

     call FMEVLSF(RVAL, ILO, I) ! tons/acre
     if (I.eq.1) goto 1001
     if (LMETRICVAR) then
        XLDREG(1) = RVAL * TItoTM / ACRtoHA
     else
        XLDREG(1) = RVAL
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    DWDVAL:

  if (MYSTR.eq.13300) then

     !       RETURN IF FIRE MODEL IS INACTIVE
     !       RETURN IF THE WRONG NUMBER OF ARGUMENTS ARE PRESENT

     call FMATV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.ne.4) goto 1002

     L = IFIX(XLDREG(1)+.5)
     J = IFIX(XLDREG(2)+.5)
     K = IFIX(XLDREG(3)+.5)
     M = IFIX(XLDREG(4)+.5)

     !  CHECK BOUNDARIES FOR
     !  (L=1:2)  VOLUME OR COVER
     !  (J=0:2)  ALL,HARD,SOFT
     !  (K=1:7)  LOWER SIZE CLASS: 1=0-3", 2=3-6:, 3=6-12", 4=12-20", 5=20-35",
     !                             6=35-50", 7=50"+
     !  (M=1:7)  UPPER SIZE CLASS: 1=0-3", 2=3-6:, 3=6-12", 4=12-20", 5=20-35",
     !                             6=35-50", 7=50"+

     if (L.lt.1 .or. L.gt.2) goto 1002
     if (J.lt.0 .or. J.gt.2) goto 1002
     if (K.lt.1 .or. K.gt.7) goto 1002
     if (M.lt.1 .or. M.gt.7) goto 1002
     if (K.gt.M) goto 1002

     call FMDWD(RVAL, L, J, K, M, I)
     if (I.eq.1) goto 1001
     XLDREG(1) = RVAL
     if (LMETRICVAR) then
        if (L .eq.1) XLDREG(1) = RVAL * FT3pACRtoM3pHA
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    ACORNS:

  !      1ST ARUGMENT:
  !         1 = NUMBER OF ACORNS PER ACRE
  !         2 = LBS OF ACORNS PER ACRE
  !      2ND ARGUMENT: SPECIES
  !         0 = ALL SPECIES
  !        -X = SPECIES GROUP X

  !  VARIABLE LINCL IS USED TO INDICATE WHETHER A TREE GETS INCLUDED IN
  !  THE CALCULATION OR NOT.
  if (MYSTR.eq.13400) then

     !       RETURN IF THE WRONG NUMBER OF ARGUMENTS ARE PRESENT

     if (JARGS.ne.2) goto 1002
     L = IFIX(XLDREG(1)+.5)
     if (XLDREG(2).ge. 0.) then
        J = IFIX(XLDREG(2)+.5)
     else
        J = IFIX(XLDREG(2)-.5)
     end if

     !       IF THE ATTRIBUTE (L) IS OUT OF RANGE OR UNDEFINED IN THE
     !       PHASE, THEN: ISSUE ERROR CODE.

     if (L.le.0 .or. L.gt.2) goto 1002

     !       IF THE SPECIES (J) IS OUT OF RANGE, THEN: ISSUE ERROR CODE.
     !       NOTE: A NEGATIVE SPECIES NUMBER INDICATES A SPECIES GROUP

     if (J.gt.MAXSP) goto 1002
     if (J.lt.0 .and. NSPGRP.lt.-J) goto 1002

     !       COMPUTE ACORNS

     ACRN = 0.
     XLDREG(1) = 0.
     ILIM = ITRN
     if (ILIM.gt.0) then
        do I = 1,ILIM
           LINCL = .false.
           if (FIAJSP(ISP(I)).eq."802" .or. FIAJSP(ISP(I)).eq."806" .or. &
                & FIAJSP(ISP(I)).eq."832" .or. FIAJSP(ISP(I)).eq."833" .or. &
                & FIAJSP(ISP(I)).eq."837") then
              if (J.eq.0 .or. J.eq.ISP(I)) then
                 LINCL = .true.
              else if (J.lt.0) then
                 IGRP = -J
                 IULIM = ISPGRP(IGRP,1)+1
                 do IG = 2,IULIM
                    if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                       LINCL = .true.
                       go to 452
                    end if
                    451 continue
                 end do
              end if
              452 continue
           end if
           if (LINCL .and. (DBH(I).ge.5.0)) then
              DCM = DBH(I)*2.54
              TPA3 = PROB(I)
              ACRN = 0.

              !       SET RETURN VALUE TO # IF L=1, LBS IF L=2

              select case (L)
              case(1)
                 ADIV = 1.
              case(2)
                 select case (FIAJSP(ISP(I)))
                 case("802")
                    ADIV = 140.
                 case("806")
                    ADIV = 180.
                 case("832")
                    ADIV = 115.
                 case("833")
                    ADIV = 100.
                 case("837")
                    ADIV = 160.
                 end select
              end select
              select case (FIAJSP(ISP(I)))
              case("802")
                 BADJ = 0.6**2*(1-0.6**2)/2
                 ACRN = (0.71155+0.06346*DCM -0.00034290*DCM*DCM)
                 ACRN = (TPA3*(10**(ACRN+BADJ)-1))/ADIV
              case("806")
                 BADJ = 0.5**2*(1-0.5**2)/2
                 ACRN = (1.16744+0.05158*DCM -0.00026797*DCM*DCM)
                 ACRN = (TPA3*(10**(ACRN+BADJ)-1))/ADIV
              case("832")
                 BADJ = 0.6**2*(1-0.6**2)/2
                 ACRN = (0.20984+0.06029*DCM-0.00039431*DCM*DCM)
                 ACRN = (TPA3*(10**(ACRN+BADJ)-1))/ADIV
              case("833")
                 BADJ = 0.6**2*(1-0.6**2)/2
                 ACRN = (-0.14836+0.07539*DCM-0.00039950*DCM*DCM)
                 ACRN = (TPA3*(10**(ACRN+BADJ)-1))/ADIV
              case("837")
                 BADJ = 0.4**2*(1-0.4**2)/2
                 ACRN = (TPA3*(10**(1.06367+0.03123*DCM+BADJ)-1))/ADIV
              end select
              XLDREG(1) = XLDREG(1)+(ACRN)
           end if
           450 continue
        end do
        if (LMETRICVAR) then
           !       convert acorn lb/ac to acorn kg/ha
           XLDREG(1) = XLDREG(1) * LBtoKG / ACRtoHA
        end if
     end if

     goto 1000
  end if

  !     DECODE INSTRUCTION AND EXECUTE.    CLSPVIAB

  if (MYSTR.eq.13500) then

     !       RETURN IF CLIMATE MODEL IS INACTIVE, ALSO RETURN IF THE NUMBER
     !       OF ARGUMENTS IS NOT 1.

     call CLACTV(LACTV)
     if (.not.LACTV) goto 1002
     if (JARGS.ne.1) goto 1002

     !       FETCH THE VIABILITY SCORE FOR SPECIES CODE STORED IN XLDREG(1)
     !       AND LOAD THE SCORE INTO XLDREG(1). IF THE SPECIES CODE IS NOT
     !       WITHIN RANGE, THEN SIGNAL THAT THE FUNCTION RESULTS IN AN UNDEFINED
     !       VALUE (SAME ACTION AS WHEN THE CLIMATE MODEL IS NOT ACTIVE

     call CLSPVIAB(IFIX(XLDREG(1)),XLDREG(1),IRC)
     if (IRC.ne.0) goto 1002
     goto 1000
  end if

  !  **INSERT ADDITIONAL FUNCTIONS HERE**

  1000 continue
  IRC = 0
  goto 1010
  1001 continue
  IRC = 1
  goto 1010
  1002 continue
  IRC = 2
  1010 continue
  if (LDEB) write (JOSTND,1020) IRC,XLDREG(1)
  1020 format (' IN EVLDX: IRC= ',I2,' XLDREG=',E14.5)
  return
end subroutine EVLDX
