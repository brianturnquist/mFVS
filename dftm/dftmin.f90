!===================!
!   dftm : dftmin   !
!===================!
! DFTM $Id$
subroutine DFTMIN(LKECHO)
  implicit none

  !     OPTION PROCESSOR FOR DOUGLAS FIR TUSSOCK MOTH MODEL

  !     NICK CROOKSTON, PROGRAMMER           JAN 1978  & JULY 1981

  ! REVISION HISTORY:
  !   06-AUG-01 LANCE R. DAVID (FHTET)
  !     MOVED LOGICAL VARIABLES LNPV2, LNPV3, LRANSD, LTMPRM, LCHEM TO TMCOM1
  !     AND INITIALIZE IN TMINIT.
  !     (PREVIOUSE REVISION DATE WAS 01/06/89)
  !   10-NOV-2003 - Lance R. David (FHTET)
  !     Added LFLAG to KEYRDR call statement argument list.
  !   01-APR-2013 Lance R. David (FMSC)
  !     A few variables defined locally were already defined
  !     in a common block. Local declaration removed. Other declarations
  !     corrected.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'TMEGGS.f90'
  include 'TMCOM1.f90'
  include 'UPPER.f90'
  include 'LOWER.f90'

  ! Variables
  character(len=4) :: SPEC(2)
  character(len=4) :: PRMTYP(3)
  character(len=8) :: TABLE(50)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=10) :: KARD(12)
  character(len=80) :: PNAME
  integer :: IDT,I,I1,I2,ISP,J,IPH,K1,INSTAR
  integer :: K2,K3,KEY,ISIZE,KODE,NUMBER
  real    :: REGG(6),DEGG(6),PRMS(1),ARRAY(12),EFFIC
  real    :: RSEED
  logical :: LNOTBK(12),LFIRST,LKECHO
  logical :: DEBUG

  !     This equivalence enables arrays DFEGG and GFEGG to be referenced
  !     by DEGG, and arrays DFREGG and GFREGG to be referenced by REGG.
  !     This requires that the order of the variables DFEGG, GFEGG, DFREGG,
  !     and GFREGG remain in the order listed here.  The results of this
  !     statement are:
  !                    DEGG(1)-(3) = DFEGG(1)-(3)
  !                    DEGG(4)-(6) = GFEGG(1)-(3)
  !                    REGG(1)-(3) = DFREGG(1)-(3)
  !                    REGG(4)-(6) = GFREGG(1)-(3)

  equivalence (DEGG(1), DFEGG(1)), (REGG(1), DFREGG(1))

  data SPEC / ' (DF', ' (GF' /
  data LFIRST / .true. /
  data PRMTYP / ' B0(', ' R0(', ' B1(' /
  data ISIZE / 50 /
  data DEBUG /.false./

  data TABLE /&
       &'END     ','REPORT  ','DATELIST','NODFRUN ','NOGFRUN ',&
       &'MANSCHED','DEBUG   ','RANNSEED','WEIGHT  ','NUMCLASS',&
       &'RANLARVA','DETLARVA','DFTMECHO','REDIST  ','NOREDIST',&
       &'        ','        ','        ','        ','NPV2    ',&
       &'NPV3    ','        ','        ','        ','        ',&
       &'TMPARMS ','CHEMICAL','DEBUTREE','        ','PUNCH   ',&
       &'        ','BIOMASS ','DFBIOMAS','GFBIOMAS','        ',&
       &'RANSCHED','MANSTART','ASHDEPTH','        ','CRTSTART',&
       &'RANSTART','PROBMETH','TOPO    ','        ','SALVAGE ',&
       &5 * ' ' /

  if (.not. LFIRST) goto 10
  LFIRST = .false.
  LRANSD = .false.
  !06aug01      LNPV2  = .FALSE.
  !06aug01      LNPV3  = .FALSE.
  LTMPRM = .false.
  LCHEM = .false.

  !     OPEN JODFTM FILE.

  call MYOPEN (10, ' ', 4, 132, 0, 1, 1, 0, KODE)

  !     CALL CH8SRT TO CREATE A SORTED INDEX FOR TABLE.

  !     CALL CH8SRT (ISIZE,TABLE,INDEX,.TRUE.)

  10 continue

  !     READ KEYWORDS

  call KEYRDR (IREAD, JOSTND, DEBUG, KEYWRD, LNOTBK, ARRAY,&
       &IRECNT, KODE, KARD, LFLAG,LKECHO)

  !     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF

  if (KODE .eq.0) go to 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call ERRGRO (.true.,6)
  goto 10

  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

  if (KODE .eq.0) go to 40
  if (KODE .eq.1) call ERRGRO (.true., 1)
  if (KODE .eq.1) goto 10
  call ERRGRO (.true., 5)

  40 continue

  !     GO TO 60

  !     SPECIAL END OF FILE TARGET (CURRENTLY NOT USED).

  !  50 CONTINUE
  !     CALL ERRGRO (.FALSE.,2)
  !  60 CONTINUE

  !     PROCESS OPTIONS

  go to( 1000,1200,1300,1400,1500,1600,1700,1800,1900,2000&
       &,2100,2200,2300,2400,2500,2600,2700,2800,2900,3000&
       &,3100,3200,3300,3400,3500,3600,3700,3800,3900,4000&
       &,4100,4200,4300,4400,4500,4600,4700,4800,4900,5000&
       &,5100,5200,5300,5400,5500,5600,5700,5800,5900,6000&
       &),  NUMBER

  900 format (/A8)

  1000 continue

  !======================  OPTION NUMBER 1: END  ==================

  if (LKECHO)write(JOSTND,1010) KEYWRD
  1010 format (/A8,'   END OF DFTM OPTIONS.')
  return

  1200 continue

  !======================  OPTION NUMBER 2: REPORT  ===============

  if (LNOTBK(1)) ITMREP = IFIX(ARRAY(1))
  if (LKECHO)write(JOSTND,1220) KEYWRD,ITMREP
  1220 format (/A8,'   REPORT LEVEL=',I3,&
       &' (0=NONE,1=SUMMARY ONLY,2=NORMAL)')
  go to 10

  1300 continue

  !======================  OPTION NUMBER 3: DATELIST  =============

  if (LKECHO)write(JOSTND,2) KEYWRD
  call TMDTLS (JOSTND)
  go to 10

  1400 continue

  !======================  OPTION NUMBER 4: NODFRUN  ==============

  LDF = .false.
  if (LKECHO)write(JOSTND,1410) KEYWRD
  1410 format (/A8,'   EXCLUDE DOUGLAS-FIR')
  go to 10

  1500 continue

  !======================  OPTION NUMBER 5: NOGFRUN  ==============

  LGF = .false.
  if (LKECHO)write(JOSTND,1510) KEYWRD
  1510 format (/A8,'   EXCLUDE GRAND FIR')
  go to 10

  1600 continue

  !======================  OPTION NUMBER 6: MANSCHED  =============

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  ITMSCH = 1

  call OPNEW (KODE,IDT,810,0,PRMS)
  if (KODE.gt.0) goto 10

  if (LKECHO)write(JOSTND,1610) KEYWRD,IDT
  1610 format (/A8,'   DATE/CYCLE=',I5,&
       &'; REGIONAL OUTBREAKS SCHEDULED BY USER.')

  !     KEEP TRACK OF OUTBREAKS SCHEDULED BY DATE.

  if (IDT .le.40) goto 10
  TMYRS(41) = TMYRS(41) + 1
  I = TMYRS(41)
  TMYRS(I) = IDT
  go to 10

  1700 continue

  !======================  OPTION NUMBER 7: DEBUG  ================

  TMDEBU = .true.
  ITMREP = 2
  JODFTM = JOSTND
  if (LKECHO)write(JOSTND,900) KEYWRD
  go to 10

  1800 continue

  !======================  OPTION NUMBER 8  RANNSEED  =============

  ! BECAUSE OF PROBLEMS WITH THE DFTM RANDOM NUMBER GENERATOR WHEN RUNNING
  ! ON A PC, IT HAS BEEN REPLACED BY CODE FROM THE GENERATOR USED IN THE
  ! MOUNTAIN PINE BEETLE MODEL, WHICH USES ONLY ONE SEED VALUE.
  ! LANCE R. DAVID 4/4/90

  LRANSD = .true.

  !     IF (LNOTBK(1)) IR1=ARRAY(1)
  !     IF (LNOTBK(2)) IR2=ARRAY(2)
  !     IF (LNOTBK(3)) IR3=ARRAY(3)

  !     CALL TMRNSD(IR1, IR2, IR3)

  !     IF(LKECHO)WRITE(JOSTND,1810) KEYWRD,IR1,IR2,IR3
  !1810 FORMAT (/1X,A8,'   DFTM RANDOM NUMBER GENERATOR WAS RESEEDED:',
  !    >        3I13)

  RSEED = real(ARRAY(1))
  call TMRNSD (LNOTBK(1), RSEED)
  if (LKECHO)write(JOSTND,1810) KEYWRD, RSEED
  1810 format (/A8,'   DFTM RANDOM NUMBER SEED IS: ',F10.0)
  go to 10

  1900 continue

  !======================  OPTION NUMBER 9: WEIGHT  ===============

  if (LNOTBK(1)) WEIGHT(1) = ARRAY(1)
  if (LNOTBK(2)) WEIGHT(2) = ARRAY(2)

  if (LKECHO)write(JOSTND,1920) KEYWRD,WEIGHT(1),WEIGHT(2)
  1920 format (/A8,'   THE CLASSIFICATION WEIGHTING ',&
       &'FACTORS ARE: ',F6.2,' FOR % NEW FOLIAGE, AND ',&
       &F6.2,' FOR FOLIAGE BIOMASS.')
  go to 10

  2000 continue

  !======================  OPTION NUMBER 10: NUMCLASS  ============

  I1 = NCLAS(1)
  I2 = NCLAS(2)
  if (LNOTBK(1)) I1 = ARRAY(1)
  if (LNOTBK(2)) I2 = ARRAY(2)
  if ((I1 + I2) .le.100) goto 2010

  call KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)
  call ERRGRO (.true., 4)
  goto 10

  2010 continue
  NCLAS(1) = I1
  NCLAS(2) = I2
  if (LNOTBK(3)) TMPN1 = ARRAY(3)

  if (LKECHO)write(JOSTND,2020) KEYWRD, NCLAS, TMPN1
  2020 format (/A8,'   NUMBER OF REQUESTED CLASSES OF ',&
       &'DOUGLAS-FIR= ',I3,'; GRAND FIR=',I3/T12,'PROPORTION OF ',&
       &'CLASSES DEFINED BY FINDING DIFFERENCES BETWEEN',&
       &' TREES = ',F5.2/T12,'THE REMAINING CLASSES ARE FOUND',&
       &' BY HALVING THE CLASSES WITH THE MOST TREE RECORDS.')
  goto 10

  2100 continue

  !======================  OPTION NUMBER 11: RANLARVA  ============

  ISP = IFIX(ARRAY(1))
  if (ISP .ge.1.and. ISP .le.2) goto 2110

  call KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)
  call ERRGRO (.true., 4)
  goto 10

  2110 continue
  IEGTYP = 1

  do I = 2,4
     if (LNOTBK(I)) REGG(3 * ISP + I - 4) = ARRAY(I)
     2120 continue
  end do

  if (LKECHO)write(JOSTND,2130) KEYWRD, ISP, SPEC(ISP),&
       &(REGG(3 * ISP + I - 3), I = 1,3)
  2130 format (/A8,'   RANDOM FIRST INSTAR LARVAE ASSIGNMENT ',&
       &'FOR SPECIES',I3,A4,')'/T12,'AVERAGE =',F7.2,&
       &'; WITHIN-OUTBREAK STANDARD DEVIATION =',F6.2,&
       &'; BETWEEN-OUTBREAK STANDARD DEVIATION =',F6.2)
  goto 10

  2200 continue

  !======================  OPTION NUMBER 12: DETLARVA  ============

  ISP = IFIX(ARRAY(1))
  if (ISP .ge.1.and. ISP .le.2) goto 2210
  call KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)
  call ERRGRO (.true.,4)
  goto 10

  2210 continue
  IEGTYP = 2

  do I = 2,4
     if (LNOTBK(I)) DEGG(3 * ISP + I - 4) = ARRAY(I)
     2220 continue
  end do

  if (LKECHO)write(JOSTND,2230) KEYWRD, ISP, SPEC(ISP),&
       &(DEGG(3 * ISP + I - 3), I = 1,3)
  2230 format(/A8,'   FIRST INSTAR LARVAE ALLOCATION BY THIRDS OF ',&
       &'AVERAGE DBH DISTRIBUTION.'/T12,&
       &'THREE LEVELS ON SPECIES',I3,A4,') ARE:',3F7.1)
  goto 10

  2300 continue

  !======================  OPTION NUMBER 13: DFTMECHO  ============

  !     SET JODFEC TO THE UNIT NUMBER FOR THE POST PROCESSOR OUTPUT.

  JODFEC = 11

  !     READ IN THE FILE NAME FROM THE SUPPLEMENTAL RECORD.

  read (IREAD,2310) PNAME
  2310 format (A)
  if (PNAME .eq. ' ') PNAME = 'DFTMOUT'

  !     TRY TO OPEN THE FILE.

  call MYOPEN (JODFEC, PNAME, 1, 133, 0, 1, 1, 0, KODE)

  !     PRINT OUT KEYWORD MESSAGE BASED ON IF THE FILE OPENS OR NOT.

  if (KODE .eq.1) then
     write(JOSTND,2320) KEYWRD, PNAME
     2320 format (/A8,'   POST PROCESSOR OUTPUT FILE ',A80,/,&
          &11X,'WAS NOT OPENED DUE TO AN ERROR!!!!')
     JODFEC = 0
  else
     if (LKECHO)write(JOSTND,2330) KEYWRD, PNAME
     2330 format (/A8,'   POST PROCESSOR OUTPUT FILE ',A80,/,&
          &11X,'WAS OPENED.')
  end if

  go to 10

  2400 continue

  !======================  OPTION NUMBER 14: REDIST  ==============

  if (LNOTBK(1)) B0(62) = ARRAY(1)
  if (LKECHO)write(JOSTND,2410) KEYWRD, B0(62)
  2410 format (/A8,'   ANNUAL TUSSOCK MOTH REDISTRIBUTION ',&
       &'RATE = ',F6.2)
  go to 10

  2500 continue

  !======================  OPTION NUMBER 15: NOREDIST  ============

  B0(62) = 0.0
  if (LKECHO)write(JOSTND,2410) KEYWRD, B0(62)
  go to 10

  2600 continue

  !======================  OPTION NUMBER 16:  =====================

  go to 10

  2700 continue

  !======================  OPTION NUMBER 17:  =====================

  go to 10

  2800 continue

  !======================  OPTION NUMBER 18:  =====================

  go to 10

  2900 continue

  !======================  OPTION NUMBER 19:  =====================

  go to 10

  3000 continue

  !======================  OPTION NUMBER 20: NPV2  ================

  LNPV2 = .true.
  B0(9) = 0.036
  if (LNOTBK(1)) B0(9) = ARRAY(1)
  B0(10) = 0.039
  if (LNOTBK(2)) B0(10) = ARRAY(2)
  B0(11) = 0.042
  if (LNOTBK(3)) B0(11) = ARRAY(3)
  B0(12) = 0.072
  if (LNOTBK(4)) B0(12) = ARRAY(4)

  if (LKECHO)write(JOSTND,3010) KEYWRD, (PRMTYP(1), I, B0(I), I = 9,12)
  3010 format (/A8,'   OUTBREAK MODEL PARAMETER VALUE(S):  ',&
       &4(A4,I2,')= ',F10.4))
  goto 10

  3100 continue

  !======================  OPTION NUMBER 21: NPV3  ================

  LNPV3 = .true.
  B0(15) = 0.036
  if (LNOTBK(1)) B0(15) = ARRAY(1)
  B0(16) = 0.039
  if (LNOTBK(2)) B0(16) = ARRAY(2)
  B0(17) = 0.042
  if (LNOTBK(3)) B0(17) = ARRAY(3)
  B0(18) = 0.072
  if (LNOTBK(4)) B0(18) = ARRAY(4)

  if (LKECHO)write(JOSTND,3010) KEYWRD, (PRMTYP(1), I, B0(I),I = 15,18)
  go to 10

  3200 continue

  !======================  OPTION NUMBER 22:  =====================

  go to 10

  3300 continue

  !======================  OPTION NUMBER 23:  =====================

  go to 10

  3400 continue

  !======================  OPTION NUMBER 24:  =====================

  go to 10

  3500 continue

  !======================  OPTION NUMBER 25:  =====================

  go to 10

  3600 continue

  !======================  OPTION NUMBER 26: TMPARMS  =============

  I = IFIX(ARRAY(1))
  J = IFIX(ARRAY(2))
  if (LNOTBK(1) .and. LNOTBK(2) .and. LNOTBK(3)) go to 3610

  3602 continue
  if (LKECHO)write(JOSTND,3605) I, J, ARRAY(3)
  3605 format (/T12,'I=',I2,' J=',I2,' VALUE=',F5.2)
  call KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)
  call ERRGRO (.true., 4)
  goto 10

  3610 continue
  LTMPRM = .true.
  if (J .eq.0.or. J .gt.6) go to 3602
  if (I .ge.2.and. I .le.12 ) go to 3620
  if (I .ge.19.and. I .le.21 ) go to 3630
  if (I .ge.22.and. I .le.25 ) go to 3640
  go to 3602

  3620 continue
  IPH = 1
  K1 = (I - 2) * 6 + J
  B0(K1) = ARRAY(3)
  goto 3650

  3630 continue
  IPH = 2
  K1 = (I - 19) * 6 + J
  R0(K1) = ARRAY(3)
  goto 3650

  3640 continue
  IPH = 3
  K1 = (I - 21) * 6 + J
  B1(K1) = ARRAY(3)

  3650 continue
  if (LKECHO)write(JOSTND,3060) KEYWRD, I, J, PRMTYP(IPH),&
       &K1, ARRAY(3)
  3060 format(/A8,'   RECORD NUMBER= ',I2,' COLUMN= ',I2,&
       &' MODEL PARAMETER',A4,I2,')= ',F10.4)
  go to 10

  3700 continue

  !======================  OPTION NUMBER 27: CHEMICAL  ============

  LCHEM = .true.
  IPH = 3
  I = ARRAY(1)
  if (I .ge.1.and. I .le.4) IPH = I
  INSTAR = 4
  I = ARRAY(2)
  if (I .ge.1.and. I .le.6) INSTAR = I
  EFFIC = 0.95
  if (ARRAY(3) .ge.0.0.and. ARRAY(3) .le.1.0.and. LNOTBK(3))&
       &EFFIC = ARRAY(3)

  K1 = (IPH - 1) * 6 + INSTAR
  K2 = K1 + 24
  K3 = INSTAR + 12
  B0(K2) = 1.0 - (((1.0 - EFFIC)**0.1) / ((1.0 - R0(K3)) *&
       &(1.0 - B0(K1))))

  if (LKECHO)write(JOSTND,3750)KEYWRD,IPH,INSTAR,EFFIC,K2,B0(K2)
  3750 format (/A8,'   PHASE=',I2,'; INSTAR=',I2,' EFFICACY=',F6.2,&
       &/T12,'MODEL PARAMETER B0(',I2,') IS NOW=',F10.4)
  go to 10

  3800 continue

  !======================  OPTION NUMBER 28: DEBUTREE  ============

  TMDTRE = .true.
  if (LKECHO)write(JOSTND,900) KEYWRD
  go to 10

  3900 continue

  !======================  OPTION NUMBER 29  ======================

  go to 10

  4000 continue

  !======================  OPTION NUMBER 30: PUNCH  ===============

  LPUNCH = .true.
  JOTMDK = IFIX(ARRAY(1))
  if (JOTMDK .gt.0) goto 4005

  call KEYDMP (JOSTND, IRECNT, KEYWRD, ARRAY, KARD)
  call ERRGRO (.true., 4)
  LPUNCH = .false.
  goto 10

  4005 continue
  if (LKECHO)write(JOSTND,4010) KEYWRD, JOTMDK
  4010 format (/A8,'   DATA SET REFERENCE NUMBER=',I3)
  go to 10

  4100 continue

  !======================  OPTION NUMBER 31  ======================

  go to 10

  4200 continue

  !======================  OPTION NUMBER 32: BIOMASS  =============

  I = IFIX(ARRAY(1))
  if (I .ge.1.and. I .le.4) IBMTYP = I
  go to (4211,4212,4213,4214), IBMTYP

  4211 if (LKECHO)write(JOSTND,4215) KEYWRD
  go to 10
  4212 if (LKECHO)write(JOSTND,4216) KEYWRD
  go to 10
  4213 if (LKECHO)write(JOSTND,4217) KEYWRD
  go to 10
  4214 if (LKECHO)write(JOSTND,4218) KEYWRD
  goto 10

  4215 format (/A8,T12,'RANDOM BIOMASS ASSIGNMENT (METHOD 1)')
  4216 format (/A8,T12,'DETERMINISTIC ASSIGNMENT USING TREE',&
       &', SITE AND STAND VARIABLES (METHOD 2)')
  4217 format (/A8,T12,'DETERMINSTIC ASSIGNMENT USING BASAL',&
       &' AREA PERCENTILE AND SPECIES (METHOD 3)')
  4218 format (/A8,T12,'ASSIGNMENT USING BASAL AREA ',&
       &'PERCENTILE AND SPECIES, WITH ADDITIVE RANDOM',&
       &' VARIATION (METHOD 4)')

  4300 continue

  !======================  OPTION NUMBER 33: DFBIOMAS  ============

  if (LNOTBK(1)) DFFBIO(1) = ARRAY(1)
  if (LNOTBK(2)) DFFBIO(2) = ARRAY(2)
  if (LNOTBK(3)) DFPNEW(1) = ARRAY(3)
  if (LNOTBK(4)) DFPNEW(2) = ARRAY(4)

  if (LKECHO)write(JOSTND,4310) KEYWRD, DFFBIO, DFPNEW
  4310 format(/A8,T12,'MEAN BIOMASS =',F8.2,&
       &'; STD DEV =',F7.2,'; MEAN %NEW FOLIAGE =',F7.2,&
       &'; STD DEV =',F7.2)
  go to 10

  4400 continue

  !======================  OPTION NUMBER 34: GFBIOMAS  ============

  if (LNOTBK(1)) GFFBIO(1) = ARRAY(1)
  if (LNOTBK(2)) GFFBIO(2) = ARRAY(2)
  if (LNOTBK(3)) GFPNEW(1) = ARRAY(3)
  if (LNOTBK(4)) GFPNEW(2) = ARRAY(4)
  if (LKECHO)write(JOSTND,4310) KEYWRD, GFFBIO, GFPNEW
  go to 10

  4500 continue

  !======================  OPTION NUMBER 35:  =====================

  go to 10

  4600 continue

  !======================  OPTION NUMBER 36: RANSCHED  ============

  ITMSCH = 2
  if (LNOTBK(1)) TMWAIT = IFIX(ARRAY(1))
  if (LNOTBK(2)) TMEVNT = ARRAY(2)
  if (LNOTBK(3)) TMPAST = IFIX(ARRAY(3))

  if (LKECHO)write(JOSTND,4610) KEYWRD, TMWAIT, TMEVNT, TMPAST
  4610 format (/A8,'   REGIONAL OUTBREAKS AUTOMATICALLY ',&
       &'SCHEDULED.'/T12,'MINIMUM WAITING PERIOD IS',I5,&
       &' YEARS; EVENT PROBABILITY IS ',F6.3 / T12,&
       &'LAST RECORDED TUSSOCK MOTH OUTBREAK WAS IN YEAR: ',I5)
  go to 10

  4700 continue

  !======================  OPTION NUMBER 37: MANSTART  ============

  ITMETH = 1
  if (LKECHO)write(JOSTND,4710) KEYWRD
  4710 format (/A8,'   STAND IS INCLUDED IN ALL REGIONAL OUTBREAKS.')
  go to 10

  4800 continue

  !======================  OPTION NUMBER 38: ASHDEPTH  ============

  if (LNOTBK(1)) TMASHD = ARRAY(1)
  if (LKECHO)write(JOSTND,4910) KEYWRD, TMASHD
  4910 format (/A8,'   SOIL ASH DEPTH IN INCHES =',F9.3)
  go to 10

  4900 continue

  !======================  OPTION NUMBER 39:  =====================

  go to 10

  5000 continue

  !======================  OPTION NUMBER 40: CRTSTART  ============

  ITMETH = 2
  if (LKECHO)write(JOSTND,900) KEYWRD
  go to 10

  5100 continue

  !======================  OPTION NUMBER 41: RANSTART  ============

  ITMETH = 3
  if (LKECHO)write(JOSTND,5110) KEYWRD
  5110 format (/A8,'   STAND INCLUSION IN REGIONAL OUTBREAKS IS ',&
       &'STOCHASTICALLY DETERMINED (SEE PROBMETH).')
  go to 10

  5200 continue

  !======================  OPTION NUMBER 42: PROBMETH  ============

  I = IFIX(ARRAY(1))
  if (I .gt.0.and. I .le.3) IPRBMT = I
  if (LNOTBK(2)) PRBSCL = ARRAY(2)

  if (LKECHO)write(JOSTND,5230) KEYWRD
  5230 format(/A8,T12,'CONDITIONAL PROBABILITY OF STAND',&
       &' BEING INCLUDED IN A REGIONAL OUBREAK IS A FUNCTION OF:')
  go to (5231,5232,5233), IPRBMT

  5231 if (LKECHO)write(JOSTND,5234)
  goto 5250
  5232 if (LKECHO)write(JOSTND,5235)
  goto 5250
  5233 if (LKECHO)write(JOSTND,5236)

  5234 format (T12,'ELEV, SLOPE, ASPECT, TOPO, CROWN CLOSURE, CROWN ',&
       &'WIDTH, AND %HOST (METHOD 1)')
  5235 format (T12,'TOPO, BASAL AREA, %GRAND FIR, ASHDEPTH ',&
       &'(METHOD 2)')
  5236 format (T12,'TOPO, BASAL AREA, %GRAND FIR (METHOD 3)')

  5250 continue
  if (LKECHO)write(JOSTND,5260) PRBSCL
  5260 format (T12,'PROBABILITY SCALING FACTOR =',F6.3)
  go to 10

  5300 continue

  !======================  OPTION NUMBER 43: TOPO  ================

  if (LNOTBK(1) .and. ARRAY(1) .le.3.0) TOPO = ARRAY(1)
  if (LKECHO)write(JOSTND,5310) KEYWRD, TOPO
  5310 format (/A8,'   TOPOGRAPHIC POSITION CODE = ',&
       &F6.2,';  1=RIDGETOP,2=SIDEHILL,3=BOTTOM.')
  go to 10

  5400 continue

  !======================  OPTION NUMBER 44  ======================

  go to 10

  5500 continue

  !======================  OPTION NUMBER 45: SALVAGE  =============

  ITMSLV = 1
  if (LNOTBK(1)) TMDEFL = ARRAY(1)
  if (LKECHO)write(JOSTND,5510) KEYWRD, TMDEFL
  5510 format (/A8,'   SALVAGE SURVIVORS AFTER EVERY OUTBREAK.',&
       &'  CRITICAL TREE DEFOLITION LEVEL = ',F5.1,'%')
  go to 10

  5600 continue

  !======================  OPTION NUMBER 46:  =====================

  go to 10

  5700 continue

  !======================  OPTION NUMBER 47:  =====================

  go to 10

  5800 continue

  !======================  OPTION NUMBER 48:  =====================

  go to 10

  5900 continue

  !======================  OPTION NUMBER 49:  =====================

  go to 10

  6000 continue

  !======================  OPTION NUMBER 50:  =====================

  go to 10

  entry TMOPS

  if (.not. LFIRST) goto 110
  ITMREP = 0
  return

  110 continue
  LFIRST = .true.
  if (IBMTYP .eq.2) LDUBDG = .true.

  !===  SCHEDULE THE OUTBREAKS  ===================================
  !     TMSCHD IS USED TO SCHEDULE OUTBREAKS
  !     IF 'RANSCHED' WAS SPECIFIED OR TO INSURE A CYCLE EXISTS WITHIN
  !     2 YRS OF AN OUTBREAK IF DATES ARE USED ON THE MANSCHED OPTION.

  call TMSCHD

  if (ITMREP .lt.2) return

  write (JODFTM,1)
  1 format (/46('-'),'  DFTM OPTIONS AND INPUT TABLE  ',53('-')&
       &//'KEYWORD    KEYWORD DISCRIPTION AND PARAMETER VALUES ',&
       &'USED'/'--------   ',120('-')/)
  2 format (/A8)

  !===  PROGRAM EXECUTION OPTIONS  ================================

  if (LPUNCH) write (JODFTM,131) TABLE(30), JOTMDK
  131 format (/A8,'   DATA SET REFERENCE NUMBER= ',I2)

  if (.not. LDF) write (JODFTM,2) TABLE(04)
  if (.not. LGF) write (JODFTM,2) TABLE(05)

  !===  OUTBREAK TIMING OPTIONS.  SCHEDULING:  ====================

  if (ITMSCH .eq.1 ) write (JODFTM,118) TABLE(06)
  118 format(/A8,'   REGIONAL OUTBREAKS SCHEDULED ',&
       &'BY USER.')

  if (ITMSCH .eq.2) write (JODFTM,117) TABLE(36), TMWAIT,&
       &TMEVNT, TMPAST
  117 format (/A8,'   REGIONAL OUTBREAKS AUTOMATICALLY ',&
       &'SCHEDULED.'/T13,'MINIMUM WAITING PERIOD IS',I5,&
       &' YEARS; EVENT PROBABILITY IS ',F6.3 / T12,&
       &'LAST RECORDED TUSSOCK MOTH OUTBREAK WAS IN YEAR: ',I5)

  !===  OUTBREAK TIMING OPTIONS.  STARTING:  ======================

  go to (221,222,223), ITMETH
  221 write(JODFTM,224) TABLE(37)
  go to 227
  222 write(JODFTM,2) TABLE(40)
  go to 227
  223 write(JODFTM,226) TABLE(41)
  224 format(/A8,'   STAND IS INCLUDED IN ALL ',&
       &'REGIONAL OUTBREAKS.')
  226 format(/A8,'   STAND INCLUSION IN REGIONAL ',&
       &'OUTBREAKS IS STOCHASTICALLY DETERMINED (SEE PROBMETH).')

  227 continue

  !===  OUTBREAK TIMING OPTIONS.  PROBABILITY CALCULATION:  =======

  write(JODFTM,230) TABLE(42)
  230 format(/A8,'   CONDITIONAL PROBABILITY OF STAND',&
       &' BEING INCLUDED IN A REGIONAL OUBREAK IS A FUNCTION OF:')

  go to (231,232,233), IPRBMT
  231 write (JODFTM,5234)
  go to 240
  232 write (JODFTM,5235)
  go to 240
  233 write (JODFTM,5236)

  240 continue
  write (JODFTM,241) PRBSCL
  241 format(T12,'PROBABILITY SCALING FACTOR =',F6.3)

  if (IPRBMT .eq.1.or. TOPO .le.2.) go to 148
  write (JODFTM,147)
  147 format (/'********   WARNING:  TOPO POSITION CODE WAS SET',&
       &' TO 2; CODES GREATER THAN 2 ARE INVALID WHEN PROBMETH ',&
       &'IS NOT EQUAL TO 1.')
  TOPO = 2.0

  148 continue
  write (JODFTM,150) TABLE(43), TOPO
  150 format (/A8,'   TOPOGRAPHIC POSITION CODE = ',&
       &F6.2,';  1=RIDGETOP,2=SIDEHILL,3=BOTTOM.')
  write (JODFTM,155) TABLE(38), TMASHD
  155 format (/A8,'   SOIL ASH DEPTH IN INCHES = ',F9.3)

  !===  OUTBREAK INITIAL CONDITIONS.  EGG ALLOCATION:  ============

  do ISP = 1,2
     if (IEGTYP .eq.2) goto 203
     write (JODFTM,2130) TABLE(11), ISP, SPEC(ISP),&
          &(REGG(3 * ISP + I - 3), I = 1,3)
     goto 204
     203 continue
     write (JODFTM,2230) TABLE(12), ISP, SPEC(ISP),&
          &(DEGG(3 * ISP + I - 3), I = 1,3)
     204 continue
  end do

  140 continue

  !===  OUTBREAK INITIAL CONDITIONS.  BIOMASS ALLOCATION:  ========

  go to (211,212,213,214), IBMTYP
  211 write (JODFTM,215) TABLE(32)
  write (JODFTM,220) TABLE(33), DFFBIO, DFPNEW
  write (JODFTM,220) TABLE(34), GFFBIO, GFPNEW
  go to 219
  212 write (JODFTM,216) TABLE(32)
  go to 219
  213 write (JODFTM,217) TABLE(32)
  go to 219
  214 write (JODFTM,218) TABLE(32)

  215 format (/A8,'   RANDOM BIOMASS ASSIGNMENT (METHOD 1)')
  216 format (/A8,'   DETERMINISTIC ASSIGNMENT USING TREE',&
       &', SITE AND STAND VARIABLES (METHOD 2)')
  217 format (/A8,'   DETERMINSTIC ASSIGNMENT USING BASAL',&
       &' AREA PERCENTILE AND SPECIES (METHOD 3)')
  218 format (/A8,'   ASSIGNMENT USING BASAL AREA ',&
       &'PERCENTILE AND SPECIES, WITH ADDITIVE RANDOM',&
       &' VARIATION (METHOD 4)')
  220 format (/A8,10X,A3,T12,'MEAN BIOMASS =',F8.2,&
       &'; STD DEV =',F7.2,'; MEAN %NEW FOLIAGE =',F7.2,&
       &'; STD DEV =',F7.2)

  219 continue

  !===  OUTBREAK INITIAL CONDITIONS.  NUMCLASS, WEIGHT, REDIST:  ==

  write (JODFTM,134) TABLE(10), NCLAS, TMPN1
  134 format (/A8,'   NUMBER OF REQUESTED CLASSES OF ',&
       &'DOUGLAS-FIR= ',I3,'; GRAND FIR=',I3/T12,'PROPORTION OF ',&
       &'CLASSES DEFINED BY FINDING DIFFERENCES BETWEEN',&
       &' TREES = ',F5.2/T12,'THE REMAINING CLASSES ARE FOUND',&
       &' BY HALVING THE CLASSES WITH THE MOST TREE RECORDS.')

  write (JODFTM,135) TABLE(09), (WEIGHT(I),I = 1,2)
  135 format (/A8,'   THE CLASSIFICATION WEIGHTING ',&
       &'FACTORS ARE: ',F6.2,' FOR % NEW FOLIAGE, AND ',&
       &F6.2,' FOR FOLIAGE BIOMASS.')

  if (.not. LREDIS) write (JODFTM,2) TABLE(15)
  if (LREDIS) write (JODFTM,144) TABLE(14), B0(62)

  144 format (/A8,'   ANNUAL TUSSOCK MOTH REDISTRIBUTION RATE =',&
       &F6.2)

  !===  CONTROL AND SALVAGE OPTIONS:  =============================

  170 format (/A8,'   OUTBREAK MODEL PARAMETER VALUES: ',&
       &6F10.4)

  if (LNPV2) write (JODFTM,170) TABLE(20), (B0(I),I = 9,12)
  if (LNPV3) write (JODFTM,170) TABLE(21), (B0(I),I = 15,18)
  if (ITMSLV .gt.0) write (JODFTM,180) TABLE(45), TMDEFL

  180 format (/A8,'   SALVAGE SURVIVORS AFTER EVERY OUTBREAK.',&
       &'  CRITICAL TREE DEFOLITION LEVEL = ',F5.1,'%')
  if (LTMPRM) write (JODFTM,190) TABLE(26)
  190 format (/A8,'   AT LEAST 1 DFTM SUB-MODEL PARAMETER WAS ',&
       &'REDEFINED.  SEE: OPTIONS SELECTED BY INPUT.')
  if (LCHEM) write (JODFTM,190) TABLE(27)
  if (LRANSD) write (JODFTM,250) TABLE(08), RSEED
  !     IF (LRANSD) WRITE (JODFTM,250) TABLE(08), IR1, IR2, IR3
  250 format (/A8,'   DFTM RANDOM NUMBER GENERATOR ',&
       &'WAS RESEEDED: ',F10.0)

  write (JODFTM,4)
  4 format (/131('-'))

  return

  !===  SPECIAL ENTRY TO RETURN A KEYWORD TO SUBROUTINE OPLIST.  ===

  entry TMKEY(KEY,PASKEY)
  PASKEY = TABLE(KEY)

  return
end
