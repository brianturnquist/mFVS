!=================!
!   estb : esin   !
!=================!
! ESTB $Id$
subroutine ESIN (PASKEY,ARRAY,LNOTBK,KARD,LKECHO)
  implicit none

  !     OPTION PROCESSOR FOR ESTABLISHMENT MODEL

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESWSBW.f90'

  !     INTERNAL STORAGE

  ! Variables
  integer :: IKYSIZ,KODE,IPRMPT,NUMBER,NP,IACTK,IDT,IS,ILEN,I,JRGRDR
  integer :: N,IPR,ITO,IHB,IAS,ISL,ID,IYR1,IMET,KEY
  integer :: IULIM,IG,IGSP,IGRP,NSPCNT,IRTNCD
  real    :: TH1,TH2,ARRAY(12)
  parameter (IKYSIZ = 29)
  logical :: DEBUG,LNOTBK(12),LTALLY,LMODE,LKECHO
  integer :: IPPR(MAXPLT)
  equivalence (PNN,IPPR)
  character(len=1) :: IBK
  character(len=1) :: ISLBK
  character(len=8) :: TABLE(IKYSIZ)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=10) :: KARD(12)
  character(len=150) :: RECORD
  data IBK/' '/
  data TABLE/&
       &'END','PLANT','NATURAL','BURNPREP','MECHPREP','OUTPUT',&
       &'SPECMULT','BUDWORM','EZCRUISE','PLOTINFO','TALLYONE',&
       &'TALLYTWO','STOCKADJ','HABGROUP','HTADJ','TALLY','ESRANSD',&
       &'PASSALL','RESETAGE','MINPLOTS','INGROW','NOINGROW',&
       &'NOAUTALY','AUTALLY','THRSHOLD','SPROUT','NOSPROUT',&
       &'ESTAB','ADDTREES'/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'ESIN',4,ICYC)

  !     LOAD THE KEYWORD INTO 'KEYWRD' AND BRANCH TO THE FNDKEY ROUTINE.

  KEYWRD = PASKEY
  LTALLY = .false.
  call OPMODE (LMODE)
  if (LMODE) then
     if (LNOTBK(1)) then
        call RCDSET (10,.true.)
        write(JOSTND,1)
        1 format(/'********   WARNING:  DATE OF DISTURBANCE',&
             &' IS IGNORED.')
     end if
     IDSDAT = 0
  else
     if (LNOTBK(1)) then
        IDSDAT = IFIX(ARRAY(1))
        if (LKECHO)write(JOSTND,5) IDSDAT
        5 format (T12,'DATE OF DISTURBANCE=',I5)
     else
        IDSDAT = -1
     end if
  end if

  !     PROCESS MORE KEYWORDS.

  10 continue
  call KEYRDR (IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF
  !     LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.

  if (KODE.lt.0) then
     IPRMPT = -KODE
  else
     IPRMPT = 0
  end if

  if (KODE.le.0) go to 30
  if (KODE.eq.2) call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  call ERRGRO (.true.,6)
  goto 10
  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,IKYSIZ,KODE,DEBUG,JOSTND)

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND.

  if (KODE.eq.0) go to 90
  if (KODE.eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if

  !     SPECIAL END-OF-FILE TARGET (USED IF YOU READ PARAMETER CARDS).

  80 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  90 continue

  !     PROCESS OPTIONS

  go to( 1000,1200,1300,1400,1500,1600,1700,1800,1900,2000,&
       &2100,2200,2300,2400,2500,2600,2700,2800,2900,3000,&
       &3100,3200,3300,3400,3500,3600,3700,1000,3800),NUMBER
  1000 continue

  !     OPTION NUMBER 1 -- END & ESTAB                            END & ESTAB

  !     IF A TALLY KEYWORD WAS PRESENT IN THE ESTAB PACKET
  !     THEN DO NOT SCHEDULE ANY TALLYS.

  if (LTALLY) goto 1080

  !     SCHEDULE A TALLY AT THE DATE OF DISTURBANCE.  IF WE ARE
  !     INSIDE AN IF-ENDIF ACTIVITY GROUP, THEN DO NOT SET THE
  !     DATE OF DISTURBANCE ON THE KEYWORD.

  if (LMODE) then
     NP = 0
     if (IDSDAT.eq.0) IDSDAT = 1
     call OPNEW(KODE,IDSDAT,427,NP,ARRAY)
     if (LKECHO)write(JOSTND,1020) KEYWRD,' ',IDSDAT,&
          &' YEARS AFTER THE EVENT IS OCCURS.'
     1020 format (/A8,'   REGENERATION TALLY SEQUENCE SCHEDULED TO',&
          &' START',A,I4,A)
  else
     if (IDSDAT.eq.-1) goto 1080
     NP = 1
     ARRAY(7) = FLOAT(IDSDAT)
     call OPNEW(KODE,MAX0(1,IDSDAT),427,NP,ARRAY(7))
     if (KODE.gt.0) goto 1080
     if (LKECHO)write(JOSTND,1020) KEYWRD,' IN ',IDSDAT,' '
  end if
  if (NUMBER.eq.1) then
     if (LKECHO)write(JOSTND,1021)
     1021 format (T12,'END OF ESTABLISHMENT KEYWORDS')

     !        MAKE SURE DATE OF DISTURBANCE IS -9999 THEREBY SIGNALING ESNUTR
     !        THAT THE DATE OF DISTURBANCE HAS NEVER BEEN SET.

     IDSDAT = -9999
     return
  end if
  1080 continue
  if (NUMBER.ne.1) then
     LTALLY = .false.
     if (LMODE) then
        if (LNOTBK(1)) then
           call RCDSET (10,.true.)
           write(JOSTND,1)
        end if
        IDSDAT = 0
     else

        !           IF THE NEWEST ESTAB KEYWORD CONTAINS A DATE OF DIST, THEN
        !           USE IT. IF NOT, DON'T CHANGE THE STATUS OF THE DATE.

        if (LNOTBK(1)) IDSDAT = IFIX(ARRAY(1))
        if (IDSDAT.gt.-1) then
           if (LKECHO)write(JOSTND,1085) KEYWRD,IDSDAT
        else
           if (LKECHO)write(JOSTND,1085) KEYWRD
           1085 format (/A8,'   MORE ESTABLISHMENT KEYWORDS':&
                &'; DATE OF DISTURBANCE=',I5)
        end if
     end if
     goto 10
  else
     if (LKECHO)write(JOSTND,1090) KEYWRD
     1090 format (/A8,'   END OF ESTABLISHMENT KEYWORDS')
     IDSDAT = -1
     return
  end if
  1200 continue

  !     OPTION NUMBER 2 -- PLANT                                   PLANT

  IACTK = 430
  1205 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE PARMS FEATURE IS BEING USED PROCESS OPTION USING
  !     OPNEWC, OTHERWISE, PROCESS "NORMALLY".

  if (IPRMPT.gt.0) then
     call OPNEWC (KODE,JOSTND,IREAD,IDT,IACTK,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     goto 10
  end if

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  if (ARRAY(3).gt.0.0) go to 1215
  call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  go to 10
  1215 continue
  if (ARRAY(4).lt.0.001.or.ARRAY(4).gt.100.0) ARRAY(4) = 100.0
  call OPNEW(KODE,IDT,IACTK,6,ARRAY(2))
  if (KODE.gt.0) goto 10
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,1220) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
       &(ARRAY(I),I = 3,7)
  1220 format(/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,' (CODE= ',I3,&
       &'); TREES/ACRE=',F6.0,'; % SURVIVAL=',F6.2,/,T12,&
       &'AGE=',F5.1,'; AVE. HEIGHT=',F5.1,'; SHADE CODE=',F5.1)
  if ((IACTK.eq.431).and.LKECHO)write(JOSTND,1230)
  1230 format(11X,'NATURAL IMPLIES: STOCKADJ = 0.0, NOAUTALY, ',&
       &'AND NOINGROW.')
  goto 10
  1300 continue

  !     OPTION NUMBER 3 -- NATURAL                               NATURAL

  IACTK = 431
  STOADJ = 0.0
  LAUTAL = .false.
  LINGRW = .false.
  goto 1205
  1400 continue

  !     OPTION NUMBER 4 -- BURNPREP                             BURNPREP

  call ESPRIN(491,IDSDAT,IRECNT,KEYWRD,ARRAY,LNOTBK,JOSTND,KARD)
  goto 10
  1500 continue

  !     OPTION NUMBER 5 -- MECHPREP                             MECHPREP

  call ESPRIN(493,IDSDAT,IRECNT,KEYWRD,ARRAY,LNOTBK,JOSTND,KARD)
  goto 10
  1600 continue

  !     OPTION NUMBER 6 -- OUTPUT                                 OUTPUT

  if (LNOTBK(1)) then
     IPRINT = IFIX(ARRAY(1))
     if (IPRINT.lt.0) IPRINT = 0
  end if
  if (LKECHO)write(JOSTND,1610) KEYWRD,IPRINT
  1610 format (/A8,'   OUTPUT OPTION=',I2,' (0=NO OUTPUT,',&
       &' 1=NORMAL OUTPUT).')
  if (LNOTBK(2)) then
     JOREGT = IFIX(ARRAY(2))
     if (LKECHO)write(JOSTND,'(T12,''OUTPUT UNIT = '',I2)') JOREGT
  end if
  goto 10
  1700 continue

  !     OPTION NUMBER 7 -- SPECMULT                             SPECMULT

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  if (IPRMPT.gt.0) then
     call OPNEWC (KODE,JOSTND,IREAD,IDT,95,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     goto 10
  end if

  if (.not.LNOTBK(3)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  if (ARRAY(3).lt.0.0) ARRAY(3) = 0.0

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  call OPNEW(KODE,IDT,95,2,ARRAY(2))
  if (KODE.gt.0) goto 10
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,1730) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
       &ARRAY(3)
  1730 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,' (CODE= ',&
       &I3,'); MULTIPLIER=',F10.4)
  goto 10
  1800 continue

  !     OPTION NUMBER 8 -- BUDWORM                               BUDWORM

  if (NBWHST.ge.20) then
     if (LKECHO)write(JOSTND,1810) NBWHST
     1810 format (/T12,'EXCEEDED MAX. #BUDWORM RECS=',I3)
     call ERRGRO (.true.,1)
     goto 10
  else
     if (ARRAY(2).eq.0.0) ARRAY(2) = ARRAY(1)
     if (ARRAY(1).eq.0.0.or. ARRAY(1).gt.ARRAY(2)) then
        call ERRGRO (.true.,4)
        goto 10
     else
        NBWHST = NBWHST+1
        IBWHST(1,NBWHST) = IFIX(ARRAY(1))
        IBWHST(2,NBWHST) = IFIX(ARRAY(2))
        if (LKECHO)write(JOSTND,1820) KEYWRD,IBWHST(1,NBWHST),&
             &IBWHST(2,NBWHST)
        1820 format (/A8,'   BUDWORM ACTIVE FROM ',I4,' TO ',I4)
     end if
  end if
  go to 10
  1900 continue

  !     OPTION NUMBER 9 -- EZCRUISE                             EZCRUISE

  INADV = 1
  if (LKECHO)write(JOSTND,1930) KEYWRD
  1930 format (/A8,'   MODEL PREDICTS REGENERATION AT TIME OF ',&
       &'DISTURBANCE OR INVENTORY.')
  goto 10
  2000 continue

  !     OPTION NUMBER 10 -- PLOTINFO                            PLOTINFO

  JRGRDR = IREAD
  I = IFIX(ARRAY(1))
  if (I.ge.1) JRGRDR = I

  !     SET THE NUMBER OF PLOTS DEFINED EQUAL TO ZERO.

  N = 0

  !     SIGNAL THAT PLOT INFO WAS READ USING PLOTINFO OPTION.

  IPINFO = 1
  2010 continue
  read(JRGRDR,2020,end = 80) ID,ISL,ISLBK,IAS,IHB,ITO,IPR
  2020 format(I10,I2,T11,A2,2I3,2I1)
  IRECNT = IRECNT+1
  if (ID.lt.0) goto 2040
  N = N+1
  if (N.gt.MAXPLT) goto 2030

  !     IF TREEDATA HAS ALREADY BEEN READ, CHECK THE PLOTID TO SEE IF
  !     THIS IS A STOCKABLE PLOT.  IPPREP IS -1 FOR NONSTOCKABLE PLOTS

  if (NPTIDS.le.0) goto 2028
  do I = 1,NPTIDS
     if (IPVEC(I).eq.ID) goto 2025
     2022 continue
  end do
  goto 2026
  2025 continue
  if (IPPREP(I).eq.-1) IPR = -1
  2026 continue
  IPPR(N) = IPR
  goto 2029
  2028 continue
  IPPREP(N) = IPR
  2029 continue
  IPTIDS(N) = ID
  PSLO(N) = FLOAT(ISL)
  if (ISLBK.eq.IBK) PSLO(N) = -1.
  PASP(N) = FLOAT(IAS)
  IPHAB(N) = IHB
  IPHYS(N) = ITO
  goto 2010
  2030 continue

  !     BRANCH HERE IF OVER 'MAXPLT' PLOTS WERE READ, ERRGRO ISSUES
  !     MSG AND ABORTS THE RUN.

  LSTKNT = N
  call ERRGRO(.false.,13)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  2040 continue

  !     WRITE KEYWORD AND NUMBER OF PLOTS READ.

  if (LKECHO)write(JOSTND,2050) KEYWRD,N
  2050 format(/A8,3X,I3,' INDIVIDUAL PLOT RECORDS WERE READ.')

  !     IF TREEDATA ARE ALREADY READ, THEN THE SITE PREP VECTOR NEEDS
  !     TO BE MOVED TO PERMANENT STORAGE.

  if (NPTIDS.le.0) goto 2060
  do I = 1,N
     IPPREP(I) = IPPR(I)
     2055 continue
  end do
  goto 10
  2060 continue

  !     NOW SET THE NUMBER OF PLOTS DEFINED.

  NPTIDS = N
  goto 10
  2100 continue

  !     OPTION NUMBER 11 -- TALLYONE                            TALLYONE

  I = 428
  2105 continue

  !     IF THE DATE FIELD IS BLANK, SIGNAL AN ERROR.

  if (.not.LNOTBK(1)) goto 2150
  IDT = IFIX(ARRAY(1))
  if (LMODE) then
     NP = 0
  else
     if (.not.LNOTBK(2)) ARRAY(2) = FLOAT(IDSDAT)
     if (ARRAY(2).lt.0.) goto 2150
     NP = 1
  end if
  call OPNEW(KODE,IDT,I,NP,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (LMODE) then
     if (LKECHO)write(JOSTND,2140) KEYWRD,IDT
  else
     if (LKECHO)write(JOSTND,2140) KEYWRD,IDT,ARRAY(2)
  end if
  2140 format(/A8,'   DATE/CYCLE=',I5,:,&
       &'; DATE OF DISTURBANCE=',F5.0)
  LTALLY = .true.
  goto 10
  2150 continue
  call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  2200 continue

  !     OPTION NUMBER 12 -- TALLYTWO                            TALLYTWO

  I = 429
  goto 2105
  2300 continue

  !     OPTION NUMBER 13 -- STOCKADJ                            STOCKADJ

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (IPRMPT.gt.0) then
     call OPNEWC (KODE,JOSTND,IREAD,IDT,440,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  else
     call OPNEW (KODE,IDT,440,1,ARRAY(2))
     if (KODE.gt.0) goto 10
     if (LKECHO)write(JOSTND,2310) KEYWRD,IDT,ARRAY(2)
     2310 format(/A8,'   DATE/CYCLE=',I5,'; MULTIPLIER=',F6.2)
     if ((ARRAY(2).le.0.).and.LKECHO)write(JOSTND,2320)
     2320 format (T12,'PREDICTION OF NATURAL REGENERATION IS CANCELLED.')
  end if
  goto 10
  2400 continue

  !     OPTION NUMBER 14 -- HABGROUP                            HABGROUP

  if (LKECHO)write(JOSTND,2410) KEYWRD
  2410 format(/A8,'   HABITAT TYPE GROUPS WILL BE PRINTED')
  call ESMSGS (JOREGT)
  goto 10
  2500 continue

  !     OPTION NUMBER 15 -- HTADJ                                  HTADJ

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  if (IPRMPT.gt.0) then
     call OPNEWC (KODE,JOSTND,IREAD,IDT,442,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     goto 10
  end if

  if (.not.LNOTBK(3)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  call OPNEW(KODE,IDT,442,2,ARRAY(2))
  if (KODE.gt.0) goto 10
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,2530) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
       &ARRAY(3)
  2530 format(/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,' (CODE= ',&
       &I3,'); ADJUSTMENT VALUE=',F10.4)
  goto 10
  2600 continue

  !     OPTION NUMBER 16 -- TALLY                                 TALLY

  I = 427
  goto 2105
  2700 continue

  !     OPTION NUMBER 17 -- RANNSEED                            RANNSEED

  if (LNOTBK(1).and.ARRAY(1).eq.0.0) call GETSED (ARRAY(1))
  call ESRNSD (LNOTBK(1),ARRAY(1))
  if (LKECHO)write(JOSTND,2710) KEYWRD,ARRAY(1)
  2710 format(/A8,'   RANDOM SEED=',F14.1)
  goto 10
  2800 continue

  !     OPTION NUMBER 18 -- PASSALL                              PASSALL

  !     IF IBLK EQUALS 1, THEN EXCESS TREES ARE TO BE PASSED

  IBLK = 1
  CONFID = ARRAY(1)
  if (CONFID.lt.1.0) CONFID = 1.0
  if (LKECHO)write(JOSTND,2810) KEYWRD,CONFID
  2810 format(/A8,'   MAXIMUM NUMBER OF EXCESS TREES',&
       &' PASSED PER PLOT PER SPECIES=',F10.2)
  goto 10
  2900 continue

  !     OPTION NUMBER 19 -- RESETAGE                            RESETAGE

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (IPRMPT.gt.0) then
     call OPNEWC (KODE,JOSTND,IREAD,IDT,443,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  else
     call OPNEW (KODE,IDT,443,1,ARRAY(2))
     if ((KODE.eq.0).and.LKECHO)write(JOSTND,2910) KEYWRD,IDT,&
          &ARRAY(2)
     2910 format(/A8,'   DATE/CYCLE=',I5,'; NEW AGE=',F6.0)
  end if
  goto 10
  3000 continue

  !     OPTION NUMBER 20 -- MINPLOTS                            MINPLOTS

  !     MINPLOTS SETS THE MINIMUM NUMBER OF PLOTS TO PROJECT.
  !     DEFAULT=50 (SET IN ESINIT).  MINIMUM IS 20 PLOTS.

  MINREP = IFIX(ARRAY(1))
  if (MINREP.lt.20) MINREP = 20
  if (LKECHO)write(JOSTND,3010) KEYWRD,MINREP
  3010 format(/A8,'   MINIMUM NUMBER OF PLOTS TO PROJECT=',I5)
  goto 10
  3100 continue

  !     OPTION NUMBER 21 -- INGROW                                INGROW

  LINGRW = .true.
  if (LKECHO)write(JOSTND,3110) KEYWRD,'INGROWTH',' '
  3110 format (/A8,3X,A,' WILL',A,'BE ADDED AUTOMATICALLY.')
  goto 10
  3200 continue

  !     OPTION NUMBER 22 -- NOINGROW                            NOINGROW

  LINGRW = .false.
  if (LKECHO)write(JOSTND,3110) KEYWRD,'INGROWTH',' NOT '
  goto 10
  3300 continue

  !     OPTION NUMBER 23 -- NOAUTALY                            NOAUTALY

  LAUTAL = .false.
  if (LKECHO)write(JOSTND,3110) KEYWRD,'TALLIES',' NOT '
  goto 10
  3400 continue

  !     OPTION NUMBER 24 -- AUTALLY                              AUTALLY

  LAUTAL = .true.
  if (LKECHO)write(JOSTND,3110) KEYWRD,'TALLIES',' '
  goto 10
  3500 continue

  !     OPTION NUMBER 25 -- THRSHOLD                            THRSHOLD

  if (LNOTBK(1)) then
     THRES1 = ARRAY(1)/100.0
     if (THRES1.lt.0.025) THRES1 = 0.025
     if (THRES1.gt.0.950) THRES1 = 0.950
  end if
  if (LNOTBK(2)) then
     THRES2 = ARRAY(2)/100.0
     if (THRES2.lt.0.050) THRES2 = 0.050
     if (THRES2.gt.0.975) THRES2 = 0.975
  end if
  TH1 = THRES1*100.0
  TH2 = THRES2*100.0
  if (LKECHO)write(JOSTND,3510) KEYWRD,TH1,TH2
  3510 format(/A8,'   THRESHOLD VALUES FOR AUTOMATIC TALLIES ',&
       &'FOLLOWING THINNINGS:',/,T12,'LOWER PERCENT=',F6.1,&
       &'   UPPER PERCENT=',F6.1)
  goto 10
  3600 continue

  !     OPTION NUMBER 26 -- SPROUT                               SPROUT

  LSPRUT = .true.
  IACTK = 450
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE PARMS FEATURE IS BEING USED PROCESS OPTION USING
  !     OPNEWC, OTHERWISE, PROCESS "NORMALLY".

  if (IPRMPT.gt.0) then
     call OPNEWC (KODE,JOSTND,IREAD,IDT,IACTK,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     goto 10
  end if

  if (LNOTBK(7)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (DEBUG)write(JOSTND,*)'IS,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD',&
       &'ARRAY,KARD= ',IS,NSP(1,1),MAXSP,JOSTND,IRECNT,KEYWRD,ARRAY,KARD

  if (IS.eq.-999) then
     LSPRUT = .false.
     go to 10
  end if

  !  CHECK TO MAKE SURE THAT INPUT SPECIES WILL SPROUT

  if (IS.lt.0) then
     NSPCNT = 0
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        do I = 1,NSPSPE
           if (IGSP.eq.ISPSPE(I))NSPCNT = NSPCNT+1
        end do
     end do
     if (NSPCNT.eq.ISPGRP(IGRP,1))goto 3604
  else if (IS.eq.0) then
     goto 3604
  else
     do I = 1,NSPSPE
        if (IS.eq.ISPSPE(I))goto 3604
     end do
  end if

  !  NO VALID SPROUTING SPECIES CODE WAS FOUND.  SIGNAL AN ERROR.

  call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  LSPRUT = .false.
  goto 10
  3604 continue
  if (DEBUG)write(JOSTND,*)'IS= ',IS

  if (.not.LNOTBK(3))ARRAY(3) = 1.
  if (.not.LNOTBK(4))ARRAY(4) = 1.
  if (.not.LNOTBK(5))ARRAY(5) = 0.
  if (.not.LNOTBK(6))ARRAY(6) = 999.

  call OPNEW(KODE,IDT,IACTK,5,ARRAY(2))
  if (KODE.gt.0) goto 10
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,3606) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
       &(ARRAY(I),I = 3,6)
  3606 format(/A8,'   DATE/CYCLE=',I5,'; SPROUTING WILL BE SIMULATED.'&
       &,'ADJUSTMENTS FOR SPECIES= ',A,' (CODE= ',I3,') ARE;',/T12,&
       &'SPROUT NUMBER MULTIPLIER VALUE= ',F10.4,';  HEIGHT MULTIPLIER',&
       &' VALUE= ',F10.4,';',/T12,'LOWER DBH LIMIT= ',F10.2,';  UPPER',&
       &' DBH LIMIT= ',F10.2)
  goto 10
  3700 continue

  !     OPTION NUMBER 27 -- NOSPROUT                           NOSPROUT

  LSPRUT = .false.
  if (LKECHO)write(JOSTND,3710) KEYWRD
  3710 format (/A8,'   SPROUTING WILL NOT BE SIMULATED.')
  goto 10
  3800 continue

  !     OPTION NUMBER 28 -- ADDTREES                           ADDTREES

  IACTK = 432
  IDT = 1
  IYR1 = 0
  IMET = 0
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (LNOTBK(2)) IYR1 = IFIX(ARRAY(2))
  if (LNOTBK(3)) IMET = IFIX(ARRAY(3))
  ARRAY(2) = FLOAT(IYR1)
  ARRAY(3) = FLOAT(IMET)

  !     METHOD=1; READ EXTERNAL PROGRAM THAT WILL GENERATE PLANT COMMANDS

  if (IMET .eq.1) then
     read(IREAD,'(A)',end = 80) RECORD
     IRECNT = IRECNT+1
     if (RECORD.eq.' ') then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,4)
        goto 10
     end if
     if (IPRMPT.gt.0) then
        if (IPRMPT.ne.2) then
           call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
           call ERRGRO (.true.,25)
        else
           call OPNEWC (KODE,JOSTND,IREAD,IDT,IACTK,KEYWRD,KARD,&
                &IPRMPT,IRECNT,ICYC)
           call fvsGetRtnCode(IRTNCD)
           if (IRTNCD.ne.0) return
        end if
     else
        call OPNEW (KODE,IDT,IACTK,2,ARRAY(2))
     end if
     if (KODE.eq.0) call OPCACT (KODE,RECORD)
     if (LKECHO)write(JOSTND,3830) KEYWRD,IDT,INT(ARRAY(2)),&
          &INT(ARRAY(3))
     3830 format(/A8,'   DATE/CYCLE=',I5,' SCHEDULE PLANTING=', I5,&
          &' YRS LATER; METHOD=',I2)
  end if
  goto 10

  !     SPECIAL ENTRY TO TURN OFF AUTOMATIC TALLIES AND INGROWTH.

  entry ESNOAU (PASKEY,LKECHO)

  LAUTAL = .false.
  LINGRW = .false.
  LSPRUT = .false.
  STOADJ = 0.0
  if (LKECHO)write(JOSTND,3110) PASKEY,'TALLIES AND INGROWTH',' NOT '
  if (LKECHO)write(JOSTND,9010)
  9010 format (T12,'STOCKADJ IS SET TO ZERO, AND NO SPROUTING ',&
       &'WILL BE SIMULATED.')
  return

  !     SPECIAL ENTRY TO RETRIEVE KEYWORDS.

  entry ESKEY (KEY,PASKEY)
  PASKEY = TABLE(KEY)
  return
end
