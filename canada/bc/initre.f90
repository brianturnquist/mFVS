!========================!
!   canada/bc : initre   !
!========================!
! CANADA-BC $Id$
subroutine INITRE
  implicit none

  !  THIS ROUTINE PROCESSES OPTIONS AND INITIATES THE
  !  PROGNOSIS RUN.  CALLED FROM **MAIN**.

  ! Common blocks
  include  'PRGPRM.f90'

  include  'ARRAYS.f90'

  include  'COEFFS.f90'

  include  'CONTRL.f90'

  include  'PLOT.f90'

  include  'OUTCOM.f90'

  include  'HTCAL.f90'

  include  'ECON.f90'

  include  'KEYCOM.f90'

  include  'MULTCM.f90'

  include  'VOLSTD.f90'

  include  'SCREEN.f90'

  include  'SNCOM.f90' !?

  include  'VARCOM.f90'

  include  'CWDCOM.f90'

  include  'CALCOM.f90'

  include  'WORKCM.f90'

  include  'BCPLOT.f90'

  include  'METRIC.f90'

  ! Variables
  integer :: IDUM1,IDUM2,IDUM3,IHAB,NEWYR,ISPEC,IFOREST,INTDIST,IFORST
  integer :: IFSP,IREGN,IRDUM,INUM,IDFLG,IDTYPE,IXF,IXTMP,ITB,ITC,JJTAB
  real    :: CAPHT,CAPFLG,CAPPRP,CAPDBH,DUM1,XTMP,X,TB,XXG
  real    :: TC,SDHI,SDLO,ARRAY(7),PRMS(10),XBAF
  integer :: ITOPD8,IACT,IRC,IDBCYC,NP,K,I1,IGSP,IG,IULIM,IGRP,ISETQFA
  integer :: ISPC,ILEN,IS,IDT,JRECNT,ISDSP,IGNDEF,IPSI,KODE,IPRMPT
  integer :: NUMBER,IPTKNT,I,IISP,J,I2
  integer :: ERRFLAG,IRTNCD
  integer :: POINTNO,IFLAG
  logical :: LTRERD,LFIRST,LNOTBK(7),LNOTRE
  logical :: DEBUG,LOPEVN,LKECHO
  logical :: RRGO,RRT
  character(len=250) :: RECORD
  character(len=10) :: KARD(7)
  character(len=10) :: PVRDUM
  character(len=2) :: FORST
  character(len=2) :: FORDUM
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=3) :: ONOFF
  character(len=4) :: IEND
  character(len=8) :: KEYWRD
  character(len=8) :: IAGERF
  character(len=8) :: IPLTRF
  character(len=8) :: TPCOM
  character(len=11) :: CTARGET
  character(len=20) :: KARD12

  data IEND/'END '/

  !  KEYWRD= THE KEYWORD OF INTEREST
  !  NUMBER= OPTION NUMBER AS DETERMINED BY ORDER IN 'TABLE' VECTOR.
  !  ISIZE = THE TABLE SIZE
  !  IPTKNT= LSTKNT-1, THE NUMBER OR POINTS COUNTED BY **INTREE**
  !    ICL1= TEMPORARY STORAGE FOR REFERENCE NUMBER FOR PRIMARY
  !          KEYWORD FILE WHEN REFERENCE NUMBER FOR SECONDARY KEYWORD
  !          FILE IS INPUT ON ADDFILE RECORD.
  !  NONSTK= NUMBER OF NON-STOCKABLE POINTS
  !  LFIRST= TRUE IF NO KEYWORDS HAVE BEEN READ FOR THIS STAND
  !  LTRERD= TRUE IF TREEFMT CARD WAS READ.
  !  LECON = TRUE IF OUTPUTS FOR CHEAPO ANALYSIS ARE DESIRED
  !  LKECHO= TRUE IF KEYWORDS AND PARAMETER VALUES ARE ECHOED
  !          TO THE MAIN OUTPUT FILE. BY DEFAULT = .TRUE.
  !          CONTROLED WITH ECHOK AND NOECHO KEYWORDS.
  !  JOSUME= LOGICAL UNIT NUMBER FOR CHEAPO ANALYSIS OUTPUT
  !  LNOTBK= TRUE IF CORRESPONDING KEYWORD CARD NUMERIC FIELD IS
  !          NON-BLANK.
  !  ARRAY = HOLDS VALUES READ FROM KEYWORD CARD NUMERIC FIELDS.
  !  RECORD= HOLDS AN EIGHTY COLUMN RECORD.
  !  LNOTRE= TRUE IF NOTREES OPTION SPECIFIED.
  !    SDLO= LOWER DIAMETER LIMIT SCREEN FOR INPUT TREE RECORDS (ge)
  !    SDHI= UPPER DIAMETER LIMIT SCREEN FOR INPUT TREE RECORDS (lt)
  !   ISDSP= SPECIES SCREEN FOR INPUT RECORDS

  !     **********          EXECUTION BEGINS          **********

  LKECHO = .true.
  JRECNT = 0
  DEBUG = .false.
  SDLO = 0.0
  SDHI = 999.0*CMtoIN
  ISDSP = 0
  IGNDEF = 0

  !  READ KEYWORDS

  LFIRST = .true.
  IPSI = 0
  10 continue
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  call KEYRDR (IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
  !               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.

  if (KODE.lt.0) then
     IPRMPT = -KODE
  else
     IPRMPT = 0
  end if
  if (KODE.eq.3) then
     call fvsSetRtnCode (2)
     return
  end if
  if (KODE.le.0) go to 30
  if (KODE.ne.2) goto 20
  !     NO STOP KEYWORD, BUT EOF WAS REACHED.
  if (ICL1.eq.0) then
     call ERRGRO(.false.,2)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  IREAD = ICL1
  ICL1 = 0
  goto 10
  20 continue
  call ERRGRO (.true.,6)
  goto 10
  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

  !  RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND

  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if

  !  IF VARIABLES ARE INITIALIZED FOR THE CURRENT STAND, THEN:
  !  BRANCH TO OPTION PROCESSING

  if (.not.LFIRST) go to 90
  LFIRST = .false.

  !  ELSE: INITIALIZE VARIABLES AND, IF LFLAG IS TRUE,
  !  WRITE A HEADER.

  LNOTRE = .false.
  call GRINIT
  call OPINIT
  call RANSED(.false.,WK6(1))
  call ESINIT
  call MPBINT
  call DFBINT
  call TMINIT
  call BWEINT
  call CVINIT
  call RDINIT
  call MISIN0
  call MISINT
  call BRINIT
  call ISSTAG
  call FMINIT
  call SVINIT
  call ECINIT
  call DBSINIT
  call CLINIT

  !     CALL TO ROOT DISEASE MODEL SUBROUTINE TO CHANGE DEFAULT
  !     VALUE OF MINREP WHEN ROOT DISEASE ACTIVE

  call RDATV (RRGO,RRT)
  if (RRGO) call RDESIN

  !  INITIALIZE INTERNAL VARIABLES

  ITITLE = ' '
  LTRERD = .false.
  go to 90

  !  SPECIAL END-OF-FILE TARGET

  80 continue
  call ERRGRO (.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  90 continue

  !  PROCESS OPTIONS

  go to(100, 1200, 1300, 1400, 1500, 1600, 1700, 1800, 1900, 2000,&
       &2100, 2200, 2300, 2400, 2500, 2600, 2700, 2800, 2900, 3000,&
       &3100, 3200, 3300, 3400, 3500, 3600, 3700, 3800, 3900, 4000,&
       &4010, 4020, 4030, 4040, 4050, 4060, 4070, 4080, 4100, 4200,&
       &4300, 4400, 4500, 4600, 4700, 4800, 4900, 5000, 5100, 5200,&
       &5300, 5400, 5500, 5600, 5700, 5800, 5900, 6000, 6100, 6200,&
       &6300, 6400, 6500, 6600, 6700, 6800, 6900, 7000, 7100, 7200,&
       &7300, 7400, 7500, 7600, 7700, 7800, 7900, 8000, 8100, 8200,&
       &8300, 8400, 8500, 8600, 8700, 8800, 8900, 9000, 9100, 9200,&
       &9300, 9400, 9500, 9600, 9700, 9800, 9900, 9950, 9975, 9985,&
       &9995,10000,10100,10200,10300,10400,10500,10600,10900,11000,&
       &11100,11200,11300,11400,11500,11600,11700,11800,11900,12000,&
       &12100,12200,12300,12400,12500,12600,12700,12800,12900,13000,&
       &13100,13200,13300,13400,13500,13600,13700,13800,13900,14000,&
       &14100,14200,14300,14400,14500,14600)&
       &, NUMBER

  !  ==========  OPTION NUMBER 1: PROCESS  ============================PROCESS

  100 continue
  if (.not.LKECHO) then
     if (IRECNT.gt.JRECNT) then
        JRECNT = IRECNT-JRECNT-1
     else
        JRECNT = IRECNT
     end if
  end if
  if (.not.LKECHO)write(JOSTND,95)JRECNT
  95 format(/T12,'THERE WERE',I8,' KEYWORD RECORDS PROCESSED.')
  write(JOSTND,105) KEYWRD
  105 format (/A8,'   PROCESS THE STAND.')

  !  MAKE SURE THAT NO EVENTS WERE LEFT OPEN

  IPRMPT = -1
  call EVEND (DEBUG,JOSTND,IRECNT,KEYWRD,ARRAY,LNOTBK,KARD,IPRMPT,&
       &LKECHO)

  !  IF TREES ARE NOT EXPECTED, AND IF NO TREE RECORDS WERE READ,
  !  THEN SET THE POINT COUNTS EQUAL TO 0.

  if (.not.MORDAT .and. LNOTRE) then
     LSTKNT = 1
     NSTKNT = 0
     MORDAT = .true.
  end if

  !  ENSURE INTREE WAS CALLED AT LEAST ONCE

  if (.not.MORDAT) then
     call INTREE (RECORD,0,ISDSP,SDLO,SDHI,LKECHO)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     MORDAT = .true.
  end if

  !  ENSURE THAT GT 1 PROJECTABLE TREE RECORD HAS BEEN READ.

  if (IREC1.lt.1.and..not.LNOTRE) then

     !        TOO FEW PROJECTABLE TREES; THEN: WRITE WARNING MSG,
     !        SET LNOTRE ON, CONTINUE PROCESSING.

     call ERRGRO (.true.,8)
     write(JOSTND,106)
     106 format (T12,'(NOTREES OPTION IS AUTOMATICALLY INVOKED.)')
     LNOTRE = .true.
     call ESEZCR(JOSTND,LKECHO)
     IREC1 = 0
     ITRN = 0
     LSTKNT = 1
     NSTKNT = 0
  end if

  !  PROCESS DAMAGE CODES STORED AT READING OF INPUT TREE DATA.

  call DAMPRO

  !     CALL TRESOR TO MATCH TREE ID NUMBERS WITH TREE INDEX NUMBERS
  !     ASSIGNED IN INTREE. THIS IS NEEDED TO INSURE THAT TREE DATA
  !     NEEDED BY ANY EXTENSIONS THAT MAY BE ACTIVE ARE MAPPED TO THE
  !     APPROPRIATE TREE RECORDS.

  call TRESOR

  !     IF DATA CONFLICTS ARE PRESENT CONCERNING PLOT COUNTS
  !     ASSUME THAT THE DESIGN CARD IS CORRECT AND WRITE A WARNING

  !  ADDITIONAL TESTING AND VALIDATION OF PLOT COUNTS SPECIFIED ON DESIGN
  !  KEYWORD OR NUM_PLOTS OF INPUT DATABASE STANDINIT TABLE TO ENSURE THAT
  !  TREE RECORD PLOT ID CAN BE USED IN PROCESSES UTILITZING PLOT LEVEL
  !  VARIABLES, SUCH AS DENSITIES, CAN BE USED AND TPA EXPANSIONS ARE CORRECT:
  !    1 - IF NUM_PLOTS IS 1 ALL TREE RECORD PLOT IDS WILL BE SET TO 1.
  !        (THIS IS HANDLED IN SUBROUTINE INTREE.F)
  !    2 - IF NUM_PLOTS IS >1 AND COUNTED TREE RECORD PLOT IDS IS LESS THAN
  !        THAT VALUE, PRESENCE OF NONSTOCKED PLOTS IS ASSUMED.
  !        (THIS IS A DEFAULT BEHAVIOR)
  !    3   IF NUM_PLOTS IS >1 AND COUNTED TREE RECORD PLOT IDS IS GREATER THAN
  !        THAT VALUE, THE RUN WILL BE TERMINATED BECAUSE ACCURATE TPA VALUES
  !        CAN NOT BE CALCULATED.

  IPTKNT = LSTKNT - 1
  if ( IPTINV .eq. -9999 ) IPTINV = IPTKNT
  if ( NONSTK .eq. -9999 ) NONSTK = NSTKNT
  if (IPTINV.le.0) IPTINV = 1
  if (IPTINV.gt.MAXPLT) IPTINV = IPTKNT

  !     MORE THAN 1 PLOT SPECIFIED, BUT ADDITIONAL PLOTS FOUND IN TREEDATA

  if ((IPTINV.ne.IPTKNT.or.NONSTK.ne.NSTKNT).and..not.LNOTRE)&
       &call ERRGRO (.true.,9)

  PI = FLOAT ( IPTINV )
  if (SAMWT .lt.0.) SAMWT = FLOAT(IPTINV)

  !  SET UP A STAND DENSITY AMPLIFIER. GROSPC IS USED TO ADJUST STAND
  !  DENSITY ESTIMATES BY REMOVING THE AREA ASSOCIATED WITH NON-STOCKABLE
  !  PLOTS FROM THE STAND AREA ESTIMATES.  FOR FIXED AREA PLOTS, THE NON-
  !  STOCKABLE AREA SHOULD BE REMOVED FROM THE STAND AREA ESTIMATE PRIOR
  !  TO INPUT.  IF GROSPC IS LT 0, COMPUTE IT HERE...ELSE ITS READ IN.

  if (GROSPC.lt.0.0) then
     GROSPC = (PI-FLOAT(NONSTK))/PI
     if (GROSPC.gt.1.0) GROSPC = 1.0
     if ((PI-FLOAT(NONSTK)).le.0.) then
        GROSPC = 1.
        call ERRGRO (.true.,35)
     end if
  end if

  !     WRITE THE END DELIMITER TO THE INPUT OPTIONS TABLE

  write(JOSTND,120)
  120 format (130('-'))

  !     WRITE THE STATUS OF THE DEFAULT OPTIONS.

  write(JOSTND,130)
  130 format (/T49,'OPTIONS SELECTED BY DEFAULT'//130('-'))
  if (.not.LTRERD)write(JOSTND,133) TREFMT(1:80),&
       &TREFMT(81:160)
  133 format (/'TREEFMT    ',A80/T12,A80)
  if (BAF .lt.0.0) then
     XBAF = BAF / ACRtoHA
  else
     XBAF = BAF * FT2pACRtoM2pHA
  end if
  write(JOSTND,2010) 'DESIGN',XBAF,FPA/ACRtoHA,&
       &BRK*INtoCM,IPTINV,NONSTK,SAMWT,GROSPC
  if (GROSPC.ne.1.0)write(JOSTND,134) GROSPC
  134 format(T13,'STAND ATTRIBUTES ARE CALCULATED PER HA OF ',&
       &'STOCKABLE AREA.  STAND STATISTICS'/&
       &T13,'IN SUMMARY TABLE ARE MULTIPLIED BY ',F5.3,&
       &'TO INCLUDE TOTAL STAND AREA.')

  !     ACTUALLY, WE DIVIDE THE STATISTICS BY THE INVERSE!...

  GROSPC = 1./GROSPC

  KODFOR = 0
  if (KODFOR .eq.0)call FORKOD
  if (KODTYP .eq.0) then
     ARRAY(2) = 0
     KARD12 = '                    '
     if (VARACD .ne. 'BC')&
          &call HABTYP (KARD12,ARRAY(2))
  end if

  select case (VARACD)
  case('CR','UT','TT','WS')
     write (JOSTND,139) KODFOR,KARD(2),KODTYP,IAGE,&
          &ASPECT,SLOPE,ELEV,ADJUSTL(CPVREF)
     139 format (/'STDINFO    FOREST-LOCATION CODE=',I6,&
          &'; HABITAT TYPE=',A10,' (CODE',I3,')',&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &';',/T12,' SLOPE= ' ,F4.0,'%',&
          &'; ELEVATION (100''S FEET)=',F5.1,'; REFERENCE CODE= ',A4)
  case('AK')
     write (JOSTND,131) KODFOR,KODTYP,IAGE,ASPECT,&
          &SLOPE,ELEV,ADJUSTL(CPVREF)
     131 format (/'STDINFO    FOREST-LOCATION CODE=',I8,&
          &'; HABITAT TYPE=',I3,&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &'; SLOPE= ' ,F4.0,'%'/&
          &T12,'ELEVATION (10''S FEET)=',F5.1,'; REFERENCE CODE=',A4)
  case ('SN')
     write(JOSTND,135) KODFOR,PCOM,&
          &IAGE,ASPECT,SLOPE,ELEV,ADJUSTL(CPVREF)
     135 format (/'STDINFO    FOREST-LOCATION CODE=',I8,&
          &'; ECOLOGICAL UNIT=',A10,&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &'; SLOPE= ',F4.0,'%'/&
          &T12,'ELEVATION(100''S FEET)=',F5.1,';  REFERENCE CODE= ',A4)

  case ('BC')
     write (JOSTND,132) BEC%PrettyName,IAGE,ASPECT,SLOPE,&
          &ELEV*100.*FTtoM, ADJUSTL(CPVREF)
     132 format (/'STDINFO    BEC=',A15,&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &'; SLOPE= ' ,F4.0,'%'/&
          &T12,'ELEVATION (M)=',F7.1,'; REFERENCE CODE= ',A4)
  case default
     write (JOSTND,138) KODFOR,KODTYP,IAGE,ASPECT,&
          &SLOPE,ELEV,ADJUSTL(CPVREF)
     138 format (/'STDINFO    FOREST-LOCATION CODE=',I8,&
          &'; HABITAT TYPE=',I3,&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &'; SLOPE= ' ,F4.0,'%'/&
          &T12,'ELEVATION(100''S FEET)=',F5.1,';  REFERENCE CODE= ',A4)

  end select

  !     SAVE INPUT SLOPE AND ASPECT VALUES IN NON-DECODED VALUES.

  IASPEC = IFIX(ASPECT)
  ISLOP = IFIX(SLOPE)

  !   CALL SITSET TO SET CONTROLLING VALUES THAT HAVE NOT BEEN SET
  !   USING KEYWORDS. THEN DUMP SITE INDEX ARRAY.

  call SITSET
  if (ISISP.gt.0.and. ISISP.le.MAXSP) then
     STNDSI = SITEAR(ISISP)
  else
     STNDSI = 0.
  end if

  !  WIPE OUT DEFECT PERCENTAGES READ ON INPUT DATA IF REQUESTED.

  if (IGNDEF .gt.0) then
     do I = 1,MAXTRE
        DEFECT(I) = 0
        209 continue
     end do
  end if

  !  WRITE SITE INDEX CROSS REFERENCE TABLE

  if (IPSI.gt.0.or. ISISP.gt.0) then
     write(JOSTND,210) TABLE(94)
     210 format(A8,'   SITE INDEX INFORMATION:')
     write(JOSTND,211) (NSP(I,1)(1:2),FTtoM*SITEAR(I),I = 1,MAXSP)
     211 format ((T12,8(A3,'=',F6.1,:,'; '),A,'=',F6.1))
     if (ISISP.gt.0) then
        write(JOSTND,212) NSP(ISISP,1)(1:2),ISISP
        212 format(T12,'SITE SPECIES=',A,' CODE=',I5)
     else
        write(JOSTND,213) ISISP
        213 format(T12,'SITE SPECIES NOT SPECIFIED.  CODE=',I5)
     end if
     if (VARACD.eq.'NI' .or. VARACD.eq.'KT'&
          &.or. VARACD.eq.'BC') then
        write(JOSTND,214)
        214 format(T12,'NOTE: SITE INDEX IS NOT USED IN THIS VARIANT.',&
             &'  VALUES ARE PRINTED FOR INFORMATIONAL PURPOSES.' )
     end if
     if (NSITET.gt.0) then
        write(JOSTND,2141)
        2141 format(/,T12,'SITE INDEX TREE INPUT SUMMARY TABLE:',/,&
             &T12,'SPECIES',2X,'DIAMETER',4X,'HEIGHT',6X,'AGE',5X,'AGE REF',&
             &4X,'PLOT REF',/,T12,'-------',2X,'--------',4X,'------',6X,&
             &'---',5X,'-------',4X,'--------')
        do I = 1,NSITET
           IAGERF = '        '
           IPLTRF = '        '
           if (SITETR(I,5).eq.1)IAGERF = '   TOTAL'
           if (SITETR(I,5).eq.2)IAGERF = '  BREAST'
           if (SITETR(I,6).eq.1)IPLTRF = '      ON'
           if (SITETR(I,6).eq.2)IPLTRF = '     OFF'
           IISP = INT(SITETR(I,1))
           write(JOSTND,2142)IISP,NSP(IISP,1)(1:2),&
                &SITETR(I,2) * INtoCM,&
                &SITETR(I,3) * FTtoM,&
                &SITETR(I,4),&
                &IAGERF,IPLTRF
           2142 format(T11,I3,' ('A2,')',2F10.1,F10.0,3X,A8,2X,A8)
           2143 continue
        end do
     end if
  end if

  !  WRITE INVENTORY POINT CROSS REFERENCE TABLE

  write(JOSTND,215)
  215 format(/,' INVENTORY POINT CROSS REFERENCE ',&
       &'(FVS SEQUENTIAL POINT NUMBER = POINT NUMBER AS ENTERED IN THE',&
       &' INPUT DATA):')
  write(JOSTND,216) (I,IPVEC(I),I = 1,IPTINV)
  216 format ((8(I3,'=',I8,:,'; '),I3,'=',I8))

  !     WRITE THE END DELIMITER TO THE DEFAULT OPTIONS TABLE

  call LBDSET (JOSTND,LKECHO)
  if (LKECHO)write(JOSTND,120)

  !  CALL TRNSLO TO TRANSFORM SLOPE CODE TO A RATIO.

  call TRNSLO

  !  CALL TRNASP TO TRANSFORM ASPECT TO RADIANS.

  call TRNASP
  !C----------
  !C  CALL SDICAL TO CALCULATE MAX SDI FOR STAND AND PLOTS
  !C     RECENT MORTALITY IS INCLUDED FOR THIS ONE.
  !C----------
  !      CALL SDICAL(1,SDIMAX)

  !     CALL ESTAB PLOT-SPECIFIC PROCESSING.

  call ESPLT2 (IPTKNT)
  return

  !  **********          SET OPTIONS          **********

  9999 format (/A8)

  !  ==========  OPTION NUMBER 2: TIMEINT  ============================TIMEINT

  1200 continue
  I2 = 10
  if (LNOTBK(2)) I2 = IFIX(ARRAY(2))
  if (.not.LNOTBK(1)) go to 1220
  I = IABS(IFIX(ARRAY(1))) + 1
  if (I.eq.1) goto 1220
  if (I.gt.MAXCY1) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  IY(I) = I2
  I = I-1
  if (LKECHO)write(JOSTND,1215) KEYWRD,I,I2
  1215 format(/A8,3X,'CYCLE=',I3,'; PERIOD LENGTH=',I3)
  go to 10
  1220 continue
  do I = 2,MAXCY1
     IY(I) = I2
     1230 continue
  end do
  if (LKECHO)write(JOSTND,1235) KEYWRD,I2
  1235 format(/A8,3X,'ALL CYCLES;  PERIOD LENGTH=',I3)
  go to 10

  !  ==========  OPTION NUMBER 3: FIXCW     =============================FIXCW

  1300 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  ARRAY(4) = ARRAY(4) * CMtoIN
  ARRAY(5) = ARRAY(5) * CMtoIN

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,90,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else

     !        SPECIES CODE PROCESSING.

     call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)
     if (IS.eq.-999) goto 10
     if (.not.LNOTBK(3))ARRAY(3) = 1.
     if (.not.LNOTBK(4))ARRAY(4) = 0.
     if (.not.LNOTBK(5))ARRAY(5) = 999.*CMtoIN
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,1330) KEYWRD,IDT,&
          &KARD(2)(1:ILEN),IS,ARRAY(3),ARRAY(4)*INtoCM,&
          &ARRAY(5)*INtoCM
     1330 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES=',A,' (CODE=',&
          &I3,'); CROWN WIDTH MULTIPLIER=',F10.4/&
          &T12,'ONLY TREES GREATER THAN OR EQUAL TO ',F7.2,&
          &' AND LESS THAN ',&
          &F7.2,' CM DBH ARE AFFECTED.')
     call OPNEW (KODE,IDT,90,4,ARRAY(2))
     if (KODE.gt.0) goto 10
  end if
  goto 10

  !  ==========  OPTION NUMBER 4: TREEDATA  ===========================TREEDATA

  1400 continue
  if ( LNOTBK(1) ) ISTDAT = IFIX (ARRAY(1))
  I = 0
  if (LNOTBK(2)) I = 1
  if (LNOTBK(3)) SDLO = ARRAY(3)*CMtoIN
  if (LNOTBK(4)) SDHI = ARRAY(4)*CMtoIN

  !     SPECIES CODE PROCESSING.

  IS = 0
  if (LNOTBK(5)) then
     call SPDECD (5,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)
     if (IS.eq.-999) goto 10
     ISDSP = IS
  end if

  if (LKECHO)write(JOSTND,1410) KEYWRD,ISTDAT
  1410 format (/A8,'   DATA SET REFERENCE NUMBER=',I3)
  if (IS.ne.0) then
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO .and. LNOTBK(5))write(JOSTND,1411) 'SPECIES= ',&
          &KARD(5)(1:ILEN),IS,'IS'
     1411 format (T12,A,A,' (CODE= ',I3,') ',A,&
          &' TARGETED FOR SCREENING.')
  else
     if (LKECHO .and. LNOTBK(5))write(JOSTND,1411) 'ALL',&
          &' SPECIES',IS,'ARE'
  end if
  if ((LNOTBK(3) .or. LNOTBK(4)).and.LKECHO)&
       &write(JOSTND,1405) SDLO*INtoCM,SDHI*INtoCM
  1405 format(T12,'ONLY RECORDS WITH DIAMETERS GE',F6.1,' CM OR ',&
       &'LT',F6.1,' CM WILL BE READ FROM THE DATA.',&
       &/,T12,'ALL OTHER INPUT RECORDS WILL BE SCREENED OUT.')
  if ((I.eq.1).and.LKECHO)write(JOSTND,1420)
  1420 format (T12,'PLOT SPECIFIC SITE DATA READ FROM TREE RECORDS.')
  call INTREE (RECORD,I,ISDSP,SDLO,SDHI,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  MORDAT = .true.
  go to 10

  !  ==========  OPTION NUMBER 5: TREEFMT  ============================TREEFMT

  1500 continue
  LTRERD = .true.
  if (LKECHO)write(JOSTND,9999) KEYWRD
  read (IREAD,1550,end = 80) TREFMT(1:80),TREFMT(81:160)
  1550 format (A80/A80)
  IRECNT = IRECNT + 2
  if (LKECHO)write(JOSTND,1560) TREFMT (1:80), TREFMT (81:160)
  1560 format (T11,A80/T11,A80)
  if (MORDAT) call ERRGRO (.true.,7)
  go to 10

  !  ==========  OPTION NUMBER 6: MPB  ================================MPB

  1600 continue
  if (LKECHO)write(JOSTND,1610) KEYWRD
  1610 format (/A8,'   MOUNTAIN PINE BEETLE OPTIONS:')
  call MPBIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  go to 10

  !  ==========  OPTION NUMBER 7: DFTM  ===============================DFTM

  1700 continue
  if (LKECHO)write(JOSTND,1710) KEYWRD
  1710 format (/A8,'   DOUGLAS-FIR TUSSOCK MOTH OPTIONS:')
  call DFTMIN(LKECHO)
  go to 10

  !  ==========  OPTION NUMBER 8: WSBW  ===============================WSBW

  1800 continue
  if (LKECHO)write(JOSTND,1810) KEYWRD
  1810 format (/A8,'   WESTERN SPRUCE BUDWORM OPTIONS:')

  !  IF BUDLITE MODEL IS BEING RUN, THE FOLLOWING CALL IS USED

  call BWEIN(LKECHO)
  go to 10

  !  ==========  OPTION NUMBER 9: CWEQN  ============================CWEQN

  1900 continue

  !     READ COEFFICIENTS FROM A SUPPLEMENTAL RECORD.

  read(IREAD,1905,end = 80) (WK6(I),I = 1,8)
  1905 format(8F10.0)
  IRECNT = IRECNT+1

  !   SPECIES CODE PROCESSING.

  call SPDECD (1,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPC.eq.-999) go to 10
  TC = ARRAY(2)*CMtoIN

  !     IF (ISPC<0) CHANGE ALL SPECIES IN THE GROUP

  if (ISPC .lt.0) then
     IGRP = -ISPC
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        LSPCWE(IGSP) = .true.
        CWTDBH(IGSP) = TC
        CWDS0(IGSP) = WK6(1)
        CWDS1(IGSP) = WK6(2)
        CWDS2(IGSP) = WK6(3)
        CWDS3(IGSP) = WK6(4)
        CWDL0(IGSP) = WK6(5)
        CWDL1(IGSP) = WK6(6)
        CWDL2(IGSP) = WK6(7)
        CWDL3(IGSP) = WK6(8)
        1906 continue
     end do
     ILEN = ISPGRP(-ISPC,92)
     if (LKECHO)write(JOSTND,1918) KEYWRD,KARD(1)(1:ILEN),ISPC

     !     IF (ISPC=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (ISPC .eq.0) then
     do I = 1,MAXSP
        LSPCWE(I) = .true.
        CWTDBH(I) = TC
        CWDS0(I) = WK6(1)
        CWDS1(I) = WK6(2)
        CWDS2(I) = WK6(3)
        CWDS3(I) = WK6(4)
        CWDL0(I) = WK6(5)
        CWDL1(I) = WK6(6)
        CWDL2(I) = WK6(7)
        CWDL3(I) = WK6(8)
        1907 continue
     end do
     if (LKECHO)write(JOSTND,1908) KEYWRD
     1908 format(/A8,T12,'DEFAULT COEFFICIENTS FOR CROWN WIDTH',&
          &' EQUATIONS REPLACED FOR ALL SPECIES.')

     !     SPECIES CODE IS SPECIFIED (IS>0).

  else
     LSPCWE(ISPC) = .true.
     CWTDBH(ISPC) = TC
     CWDS0(ISPC) = WK6(1)
     CWDS1(ISPC) = WK6(2)
     CWDS2(ISPC) = WK6(3)
     CWDS3(ISPC) = WK6(4)
     CWDL0(ISPC) = WK6(5)
     CWDL1(ISPC) = WK6(6)
     CWDL2(ISPC) = WK6(7)
     CWDL3(ISPC) = WK6(8)
     ILEN = 3
     if (LKECHO)write(JOSTND,1918) KEYWRD,KARD(1)(1:ILEN),ISPC
     1918 format(/A8,T12,'DEFAULT COEFFICIENTS FOR CROWN WIDTH',&
          &' EQUATIONS REPLACED FOR SPECIES= ',A,' (CODE= ',I3,').' )
  end if
  if (LKECHO)write(JOSTND,1925) TC*INtoCM,(WK6(I),I = 1,8)
  1925 format(T12,'TRANSITION SIZE = ',F10.2,' CM DBH.',&
       &'   EQUATION FORM: C0 + C1*DBH +C2*DBH**C3'/&
       &T12,'COEFFICIENTS FOR TREES THAT ARE SMALLER THAN',&
       &' TRANSITION SIZE:'/T12,' C0= ',F12.5,'  C1= ',F12.5,'  C2= ',&
       &F12.5,'  C3= ',F12.5,/&
       &T12,'COEFFICIENTS FOR TREES THAT ARE LARGER THAN',&
       &' TRANSITION SIZE:'/T12,' C0= ',F12.5,'  C1= ',F12.5,'  C2= ',&
       &F12.5,'  C3= ',F12.5)
  go to 10

  !  ==========  OPTION NUMBER 10: DESIGN  ============================DESIGN

  2000 continue
  if (ARRAY(1) .lt.0.0) then
     ARRAY(1) = ARRAY(1) / HAtoACR
  else
     ARRAY(1) = ARRAY(1) * M2pHAtoFT2pACR
  end if
  ARRAY(2) = ARRAY(2) / HAtoACR
  ARRAY(3) = ARRAY(3) * CMtoIN

  if ( LNOTBK(1) ) BAF = ARRAY(1)
  if ( LNOTBK(2) ) FPA = ARRAY(2)
  if ( LNOTBK(3) ) BRK = ARRAY(3)
  if ( LNOTBK(4) ) IPTINV = IFIX( ARRAY(4) )
  if ( LNOTBK(5) ) NONSTK = IFIX( ARRAY(5) )
  if ( LNOTBK(6) ) SAMWT = ARRAY(6)
  if (SAMWT.le.0..and. IPTINV.gt.0) SAMWT = FLOAT(IPTINV)
  if (ARRAY(7).gt.1.0.and. ARRAY(7).le.100.) ARRAY(7) = ARRAY(7)*.01
  if (ARRAY(7).gt.0.0.and. ARRAY(7).le.1.0)  GROSPC = ARRAY(7)
  if (GROSPC.lt.0.) then
     XXG = 1.0
     if (IPTINV.gt.0.and. NONSTK.gt.0.and.&
          &IPTINV-NONSTK.gt.0) XXG = (FLOAT(IPTINV)-FLOAT(NONSTK))/&
          &FLOAT(IPTINV)
  else
     XXG = GROSPC
  end if

  if (BAF .lt.0.0) then
     XBAF = BAF/ACRtoHA
  else
     XBAF = BAF*FT2pACRtoM2pHA
  end if
  if (LKECHO)write(JOSTND,2010) KEYWRD,XBAF,FPA/ACRtoHA,BRK*INtoCM,&
       &MAX(0,IPTINV),MAX(0,NONSTK),MAX(0.,SAMWT),XXG
  2010 format (/A,T12,'BASAL AREA FACTOR= ',F6.1,&
       &'; INVERSE OF FIXED PLOT AREA= ',F6.1,&
       &'; BREAK DBH= ',F6.1/T12,'NUMBER OF PLOTS= ',I5,&
       &'; NON-STOCKABLE PLOTS= ',I5,&
       &'; STAND SAMPLING WEIGHT=',F12.5/T12,&
       &'PROPORTION OF STAND CONSIDERED STOCKABLE= ',F6.3)
  if (LKECHO)write(JOSTND,2030)
  2030 format (T12,'SEE "OPTIONS SELECTED BY DEFAULT" FOR FINAL ',&
       &'DESIGN VALUES.')
  go to 10

  !  ==========  OPTION NUMBER 11: NUMCYCLE  ==========================NUMCYCLE

  2100 continue
  if (ARRAY(1).ge.1..and. IFIX(ARRAY(1)).le.MAXCYC) goto 2110
  call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  2110 continue
  NCYC = IFIX ( ARRAY(1) )
  if (LKECHO)write(JOSTND,2120) KEYWRD,NCYC
  2120 format (/A8,'   NUMBER OF CYCLES=',I3)
  go to 10

  !  ==========  OPTION NUMBER 12: TFIXAREA  ==========================TFIXAREA

  2200 continue
  ARRAY(1) = ARRAY(1) * HAtoACR
  if (LNOTBK(1)) TFPA = ARRAY(1)
  if (LKECHO)write(JOSTND,2210) KEYWRD,TFPA*ACRtoHA
  2210 format (/A8,'   TOTAL FIXED PLOT AREA=',F10.3)
  go to 10

  !  ==========  OPTION NUMBER 13: GROWTH  ============================GROWTH

  2300 continue
  if (LNOTBK(1)) IDG = IFIX(ARRAY(1))
  if (LNOTBK(2).and.ARRAY(2).gt.0.) IFINT = IFIX(ARRAY(2))
  FINT = FLOAT(IFINT)
  if (LNOTBK(3)) IHTG = IFIX(ARRAY(3))
  if (LNOTBK(4).and.ARRAY(4).gt.0.) IFINTH = IFIX(ARRAY(4))
  FINTH = FLOAT(IFINTH)
  if (LNOTBK(5).and.ARRAY(5).gt.0.) FINTM = ARRAY(5)
  I = IFIX(FINTM)
  if (LKECHO)write(JOSTND,2310) KEYWRD,IDG,IFINT,IHTG,IFINTH,I
  2310 format (/A8,'   DIAMETER GROWTH DATA TYPE CODE= ',I1,&
       &', ',I3,' YEAR MEASUREMENT PERIOD;' /&
       &T12,'HEIGHT GROWTH DATA TYPE CODE  = ',I1,&
       &', ',I3,' YEAR MEASUREMENT PERIOD;'/&
       &T12,'MORTALITY MEASUREMENT PERIOD  =',I2)
  go to 10

  !  ==========  OPTION NUMBER 14: STDINFO  ===========================STDINFO

  2400 continue

  ARRAY(6) = ARRAY(6) * MtoFT / 100.

  KODFOR = IFIX(ARRAY(1))
  call FORKOD
  if ( LNOTBK(7) ) then
     write(CPVREF,'(I10)')IFIX(ARRAY(7))
  else
     CPVREF = '          '
  end if
  if (lnotbk(2)) then
     KODTYP = IFIX(ARRAY(2))
     ICL5 = KODTYP
     ! concat fld1+2 to accommodate long BEC strings
     KARD12 = KARD(1)//KARD(2)
     call HABTYP(KARD12,ARRAY(2))
     if (ICL5.le.0) ICL5 = KODTYP
  end if
  if ( LNOTBK(3) ) IAGE = IFIX( ARRAY(3) )
  if ( LNOTBK(4) ) ASPECT = ARRAY(4)
  if ( LNOTBK(5) ) SLOPE = ARRAY(5)
  if ( LNOTBK(6) .and. ARRAY(6).gt.0) ELEV = ARRAY(6)

  select case (VARACD)

  case ('CR','UT','TT','WS')
     if (LKECHO)write (JOSTND,2411) KEYWRD,KODFOR,KARD(2),KODTYP,&
          &IAGE,ASPECT,SLOPE,ELEV,ADJUSTL(CPVREF)
     2411 format (/A8,'   FOREST-LOCATION CODE=',I6,&
          &'; HABITAT TYPE=',A10,' (CODE ',I3,')',&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &';',/T12,' SLOPE= ' ,F4.0,'%',&
          &';  ELEVATION(100''S FEET)=',F5.1,'; REFERENCE CODE= ',A4)

  case ('AK')
     if (LKECHO)write (JOSTND,2412) KEYWRD,KODFOR,KODTYP,&
          &IAGE,ASPECT,SLOPE,ELEV,ADJUSTL(CPVREF)
     2412 format (/A8,'   FOREST-LOCATION CODE=',I8,&
          &'; HABITAT TYPE=',I3,&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &'; SLOPE= ',F4.0,'%'/&
          &T12,'ELEVATION(10''S FEET)=',F5.1,'; REFERENCE CODE= ',A4)

  case ('SN')
     if (LKECHO)write(JOSTND,2413) KEYWRD,KODFOR,PCOM,&
          &IAGE,ASPECT,SLOPE,ELEV,ADJUSTL(CPVREF)
     2413 format (/A8,'   FOREST-LOCATION CODE=',I8,&
          &'; ECOLOGICAL UNIT=',A10,&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &'; SLOPE= ',F4.0,'%'/&
          &T12,'ELEVATION(100''S FEET)=',F5.1,'; REFERENCE CODE= ',A4)

  case('BC')
     if (LKECHO)write(JOSTND,24101) KEYWRD,BEC%PrettyName,IAGE,&
          &ASPECT,SLOPE,ELEV*100.*FTtoM,ADJUSTL(CPVREF)
     24101 format (/A8,'   BEC=',A15,&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &'; SLOPE= ',F4.0,'%'/&
          &T12,'ELEVATION (M)=',F7.1,'; REFERENCE CODE= ',A4)

  case default
     if (LKECHO)write (JOSTND,2410) KEYWRD,KODFOR,KODTYP,&
          &IAGE,ASPECT,SLOPE,ELEV,ADJUSTL(CPVREF)
     2410 format (/A8,'   FOREST-LOCATION CODE=',I8,&
          &'; HABITAT TYPE=',I3,&
          &'; AGE=',I5,'; ASPECT AZIMUTH IN DEGREES= ',F4.0,&
          &'; SLOPE= ',F4.0,'%'/&
          &T12,'ELEVATION(100''S FEET)=',F5.1,'; REFERENCE CODE= ',A4)

  end select
  go to 10

  !  ==========  OPTION NUMBER 15: STDIDENT  ==========================STDIDENT

  2500 continue
  if (LKECHO)write(JOSTND,9999) KEYWRD
  read (IREAD,'(A)',end = 80) RECORD
  I1 = 0
  do K = 1,26
     if (RECORD(K:K).ne.' ') then
        I1 = K
        go to 2505
     end if
  end do
  2505 continue
  I2 = 26
  if (I1.gt.0) then
     do K = I1,26
        if (RECORD(K:K).eq.' ') then
           I2 = K
           go to 2506
        end if
     end do
     2506 continue
     NPLT = RECORD(I1:I2)
  else
     NPLT = ' '
  end if
  ITITLE = RECORD(I2+1:)
  IRECNT = IRECNT + 1
  if (LKECHO)write(JOSTND,2510) NPLT,ITITLE
  2510 format (T12,'STAND ID= ',A,8X,A)
  go to 10

  !  ==========  OPTION NUMBER 16: INVYEAR  ===========================INVYEAR

  2600 continue
  if ( LNOTBK(1) ) IY(1) = IFIX(ARRAY(1))
  if (LKECHO)write(JOSTND,2610) KEYWRD,IY(1)
  2610 format (/A8,'   INVENTORY YEAR=',I5)
  go to 10

  !  ==========  OPTION NUMBER 17: TREELIST  ==========================TREELIST

  2700 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (.not.LNOTBK(2)) ARRAY(2) = FLOAT(JOLIST)
  NP = 2
  if (LNOTBK(4)) NP = 3
  if (LNOTBK(5) .and. ARRAY(5).gt.0) then
     NP = 4
  end if
  if (LNOTBK(6) .and. ARRAY(6).gt.0) NP = 5
  if (LNOTBK(7).and.ARRAY(7).gt.0.and.(IDT.eq.0.or.IDT.eq.1)&
       &.and.(.not.(LNOTBK(4).and.ARRAY(4).eq.1)))NP = 6
  call OPNEW (KODE,IDT,80,NP,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,2710) KEYWRD,IDT,ARRAY(2),ARRAY(3)
  2710 format (/A8,'   DATE/CYCLE=',I5,'; DATA SET REFERENCE ',&
       &'NUMBER =',F4.0,'; HEADING SUPPRESSION CODE =',F3.0/&
       &T12,'(0=WITH HEADING, OTHER VALUES=SUPPRESS HEADING).')
  if ((NP.ge.3).and.(ARRAY(4).eq.1.).and.LKECHO)write(JOSTND,2720)
  2720 format (T12,'CYCLE ZERO TREELIST SUPPRESSED.')
  if ((NP.ge.3).and.(ARRAY(4).eq.2.).and.LKECHO)write(JOSTND,2721)
  2721 format (T12,'CYCLE ONE TREELIST SUPPRESSED.')
  if ((ARRAY(5).gt.0).and.LKECHO)write(JOSTND,2740)
  2740 format(T12,'TREELIST WILL REPORT DEAD TREE STATISTICS.')
  if ((NP.eq.6).and.LKECHO)write(JOSTND,2745)
  2745 format(T12,'ESTIMATED DIAMETER GROWTHS WILL BE PRINTED ON THE',&
       &' CYCLE 0 TREELIST FOR TREES WITH NO MEASURED DIAMETER GROWTH.')
  goto 10

  !  ==========  OPTION NUMBER 18: REWIND  ============================REWIND

  2800 continue
  I = ISTDAT
  if ( ARRAY(1) .ge.1. ) I = IFIX ( ARRAY(1) )
  if (LKECHO)write(JOSTND,1410) KEYWRD,I
  rewind I
  go to 10

  !  ==========  OPTION NUMBER 19: NOSUM  =============================NOSUM

  2900 continue
  if (LKECHO)write(JOSTND,2910) KEYWRD
  2910 format (/A8,'   NO SUMMARY OUTPUT WILL BE WRITTEN.')
  LSUMRY = .false.
  go to 10

  !  ==========  OPTION NUMBER 20: DEBUG  =============================DEBUG

  3000 continue
  if (LNOTBK(3)) JOSTND = IFIX(ARRAY(3))
  IDBCYC = IFIX(ARRAY(1))
  if ((IDBCYC.lt.0).or.(IDBCYC.gt.MAXCYC)) IDBCYC = 0
  if (IDBCYC.eq.0) then
     if (LKECHO)write(JOSTND,3010) KEYWRD
     3010 format (/A8,'   DEBUG, ALL CYCLES. ')
  else
     if (LKECHO)write(JOSTND,3020) KEYWRD,IDBCYC
     3020 format (/A8,'   DEBUG, CYCLE=',I2,'. ')
  end if
  if (LNOTBK(2)) then
     call DBPRSE (IREAD,RECORD,JOSTND,IDBCYC,IRC)
     if (IRC.eq.1) goto 80
  else
     call DBALL (IDBCYC)
  end if

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'INITRE',6,0)
  go to 10

  !  ==========  OPTION NUMBER 21: ECHOSUM  ===========================ECHOSUM

  3100 continue
  LSUMRY = .true.
  if (ARRAY(1) .ge.1.) JOSUM = IFIX(ARRAY(1))
  if (LKECHO)write(JOSTND,3110) KEYWRD,JOSUM
  3110 format (/A8,'   SUMMARY OUTPUT WILL BE WRITTEN TO ',&
       &'FILE REFERENCED BY NUMBER ',I2)
  go to 10

  !  ==========  OPTION NUMBER 22: ADDFILE  ===========================ADDFILE

  3200 continue
  if (LNOTBK(1)) goto 3210
  call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  3210 continue
  ICL1 = IREAD
  IREAD = IFIX(ARRAY(1))
  if (LKECHO)write(JOSTND,1410) KEYWRD,IREAD
  go to 10

  !  ==========  OPTION NUMBER 23: THINAUTO  ==========================THINAUTO

  3300 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,222,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if
  PRMS(1) = AUTMIN
  PRMS(2) = AUTMAX
  PRMS(3) = EFF
  if (LNOTBK(2))PRMS(1) = ARRAY(2)
  if (LNOTBK(3))PRMS(2) = ARRAY(3)
  if (LNOTBK(4))PRMS(3) = ARRAY(4)
  call OPNEW (KODE,IDT,222,3,PRMS)
  if (KODE .gt.0) goto 10
  if (LKECHO)write(JOSTND,3310) KEYWRD,IDT,(PRMS(I),I = 1,3)
  3310 format (/A8,'   DATE/CYCLE=',I5,'; MIN=',F8.1,'; MAX=',&
       &F8.1,' PERCENT OF FULL STOCKING',&
       &';  PROPORTION OF SELECTED TREES REMOVED=',F6.3)
  goto 10

  !  ==========  OPTION NUMBER 24: THINBTA  ===========================THINBTA

  3400 continue
  ARRAY(2) = ARRAY(2)/HAtoACR
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoIN
  ARRAY(6) = ARRAY(6)*MtoFT
  ARRAY(7) = ARRAY(7)*MtoFT
  ICFLAG = 223
  go to 4090

  !  ==========  OPTION NUMBER 25: THINATA  ===========================THINATA

  3500 continue
  ARRAY(2) = ARRAY(2)/HAtoACR
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoIN
  ARRAY(6) = ARRAY(6)*MtoFT
  ARRAY(7) = ARRAY(7)*MtoFT
  ICFLAG = 224
  go to 4090

  !  ==========  OPTION NUMBER 26: THINBBA  ===========================THINBBA

  3600 continue
  ARRAY(2) = ARRAY(2)*M2pHAtoFT2pACR
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoIN
  ARRAY(6) = ARRAY(6)*MtoFT
  ARRAY(7) = ARRAY(7)*MtoFT
  ICFLAG = 225
  go to 4090

  !  ==========  OPTION NUMBER 27: THINABA  ===========================THINABA

  3700 continue
  ARRAY(2) = ARRAY(2)*M2pHAtoFT2pACR
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoIN
  ARRAY(6) = ARRAY(6)*MtoFT
  ARRAY(7) = ARRAY(7)*MtoFT
  ICFLAG = 226
  go to 4090

  !  ==========  OPTION NUMBER 28: THINPRSC  ==========================THINPRSC

  3800 continue
  ICFLAG = 227
  3805 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if
  PRMS(1) = EFF
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (PRMS(1).lt.0.) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if
  if (LNOTBK(3)) then
     PRMS(2) = ARRAY(3)
     I = 2
  else
     I = 1
  end if
  call OPNEW (KODE,IDT,ICFLAG,I,PRMS)
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,3810) KEYWRD,IDT,PRMS(1)
  3810 format (/A8,'   DATE/CYCLE=',I5,&
       &'; PROPORTION OF SELECTED TREES REMOVED=',F6.3)
  if ((I.eq.2).and.LKECHO)write(JOSTND,3820) IFIX(PRMS(2))
  3820 format (T12,'TREES CODED ',I4,' ARE SELECTED.')
  go to 10

  !  ==========  OPTION NUMBER 29: THINDBH  ===========================THINDBH

  3900 continue
  ARRAY(2) = ARRAY(2)*CMtoIN
  ARRAY(3) = ARRAY(3)*CMtoIN
  ARRAY(6) = ARRAY(6)/HAtoACR
  ARRAY(7) = ARRAY(7)*M2pHAtoFT2pACR
  ICFLAG = 228
  3901 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if
  if (.not.LNOTBK(3)) ARRAY(3) = 999.
  if (.not.LNOTBK(4)) ARRAY(4) = EFF

  !     SPECIES CODE PROCESSING.

  call SPDECD (5,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10

  if (ARRAY(7).lt.0.0.or. ARRAY(6).gt.0.0) ARRAY(7) = 0.0
  if (ARRAY(6).lt.0.0) ARRAY(6) = 0.0
  call OPNEW(KODE,IDT,ICFLAG,6,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (ICFLAG.eq.228) then
     if (LKECHO)write(JOSTND,3910)KEYWRD,IDT,&
          &'DBH=',ARRAY(2)*INtoCM,&
          &'DBH=',ARRAY(3)*INtoCM,&
          &ARRAY(4)
  else
     if (LKECHO)write(JOSTND,3910)KEYWRD,IDT,&
          &'HEIGHT=',ARRAY(2)*FTtoM,&
          &'HEIGHT=',ARRAY(3)*FTtoM,&
          &ARRAY(4)
  end if
  3910 format(/A8,'   DATE/CYCLE=',I5,'; MINIMUM ',A,F8.1,&
       &'; MAXIMUM ',A,F8.1,&
       &'; PROPORTION OF SELECTED TREES REMOVED=',F6.3)
  if (IS.ne.0) then
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,3920) 'SPECIES= ',KARD(5)(1:ILEN),&
          &IS,'IS'
     3920 format(T12,A,A,' (CODE= ',I3,') ',A,&
          &' TARGETED FOR THIS CUT.')
  else
     if (LKECHO)write(JOSTND,3920) 'ALL',' SPECIES',IS,'ARE'
  end if
  if (((ARRAY(6).gt.0.0).or.(ARRAY(7).gt.0.0))&
       &.and.LKECHO)write(JOSTND,3930) ARRAY(6)/ACRtoHA,&
       &ARRAY(7)*FT2pACRtoM2pHA
  3930 format(T12,'RESIDUAL CLASS TPH =',F8.1,&
       &' RESIDUAL CLASS BA =',F8.1)
  goto 10

  !  ==========  OPTION NUMBER 30: xSALVAGE  ===========================xSALVAGE
  ! Taken out of service to avoid accidental use as same keyword in FFE extension
  4000 continue
  ICFLAG = 229
  go to 3805

  !  ==========  OPTION NUMBER 31:  SPLABEL ===========================SPLABEL

  4010 continue
  call LBSPLR (KEYWRD,JOSTND,IREAD,IRECNT,RECORD,KODE,LKECHO)
  if (KODE.ge.1) goto 80
  go to 10

  !  ==========  OPTION NUMBER 32:  AGPLABEL ==========================AGPLABEL

  4020 continue
  call LBAGLR (KEYWRD,JOSTND,IREAD,IRECNT,RECORD,KODE)
  if (KODE.ge.1) goto 80
  go to 10

  !  ==========  OPTION NUMBER 33:  COMPUTE   =========================COMPUTE

  4030 continue
  call EVUSRV (RECORD,KEYWRD,ARRAY,LNOTBK,IREAD,JOSTND,&
       &DEBUG,IRECNT)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  goto 10

  !  ==========  OPTION NUMBER 34:  FERTILIZE  ========================FERTILIZE

  4040 continue
  ARRAY(2) = ARRAY(2)*KGtoLB/HAtoACR
  ARRAY(3) = ARRAY(3)*KGtoLB/HAtoACR
  ARRAY(4) = ARRAY(4)*KGtoLB/HAtoACR
  call FFIN (JOSTND,IRECNT,KEYWRD,ARRAY,LNOTBK,KARD,IPRMPT,&
       &ICYC,IREAD,LKECHO)
  go to 10

  !  ==========  OPTION NUMBER 35:  THINHT  ===========================THINHT

  4050 continue
  ARRAY(2) = ARRAY(2)*CMtoIN
  ARRAY(3) = ARRAY(3)*CMtoIN
  ARRAY(6) = ARRAY(6)/HAtoACR
  ARRAY(7) = ARRAY(7)*M2pHAtoFT2pACR
  ICFLAG = 232
  goto 3901

  !  ==========  OPTION NUMBER 36:  STATS =============================STATS

  4060 continue
  LSTATS = .true.
  if (LNOTBK(1))   ALPHA = ARRAY(1)
  if (ALPHA.le.0.0.or.ALPHA.ge.1.0) go to 4068
  4067 if (LKECHO)write(JOSTND,4062) KEYWRD,ALPHA
  4062 format(/A8,'   STATISTICAL DESCRIPTION OF INPUT DATA WILL',&
       &' BE PROVIDED; CONFIDENCE LIMITS AT ',F4.2,' LEVEL.')
  go to 10
  4068 continue
  ALPHA = 0.05
  if (LKECHO)write(JOSTND,4069)
  4069 format(11X,'SPECIFIED PROBABILITY LEVEL IS INVALID; 0.05 WILL',&
       &' BE USED.')
  go to 4067

  !  ==========  OPTION NUMBER 37:  TOPKILL  ==========================TOPKILL

  4070 continue
  ARRAY(3) = ARRAY(3)*MtoFT
  ARRAY(4) = ARRAY(4)*MtoFT
  IACT = 111
  goto 4081

  !  ==========  OPTION NUMBER 38:  HTGSTOP   =========================HTGSTOP

  4080 continue
  ARRAY(3) = ARRAY(3)*MtoFT
  ARRAY(4) = ARRAY(4)*MtoFT
  IACT = 110
  4081 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,IACT,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else

     !        SPECIES CODE PROCESSING.

     call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)
     if (IS.eq.-999) go to 10

     !        SPECIAL CASE FOR FIELD 6 AND HTGSTOP.

     if (IACT.eq.110.and..not.LNOTBK(6)) ARRAY(6) = 1.0

     !        LOAD THE ACTIVITY CODE.

     call OPNEW (KODE,IDT,IACT,6,ARRAY(2))
     if (KODE.gt.0) goto 10

     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,4082)KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
          &ARRAY(3)*FTtoM,&
          &ARRAY(4)*FTtoM,&
          &KEYWRD,ARRAY(5)
     4082 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,' (CODE= ',&
          &I3,'); LOWER TREE-HEIGHT LIMIT=',F5.1,'; UPPER LIMIT=',F5.1/&
          &T12,'PROBABILITY (',A8,') FOR TREES WITHIN ',&
          &'THE HEIGHT LIMITS=',F10.3)
     if ((IACT.ge.110).and.LKECHO)write(JOSTND,4083) 'HEIGHT GROWTH',&
          &ARRAY(6),ARRAY(7)
     if ((IACT.ge.111).and.LKECHO)write(JOSTND,4083) 'HEIGHT',&
          &ARRAY(6),ARRAY(7)
     4083 format (T12,'AVERAGE PROPORTION OF ',A,' LOST=',F6.3,&
          &'; STD DEV=',F6.3)
  end if
  goto 10

  !     STANDARD THINNING PROCESSING SECTION:

  4090 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.le.0) goto 4091
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  4091 continue
  if (LNOTBK(2)) then
     PRMS(1) = ARRAY(2)
  else

     !        REQUIRED RESIDUAL IS MISSING.

     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if
  PRMS(2) = EFF
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  PRMS(3) = 0.0
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  PRMS(4) = 999.0
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  PRMS(5) = 0.0
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)
  PRMS(6) = 999.0
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)
  call OPNEW (KODE,IDT,ICFLAG,6,PRMS)
  if (KODE.gt.0) goto 10
  if (ICFLAG .eq.223.or. ICFLAG .eq.224) then
     if (LKECHO)write(JOSTND,4095) KEYWRD,IDT,&
          &PRMS(1)/ACRtoHA,&
          &PRMS(2),&
          &PRMS(3)*INtoCM,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*FTtoM,&
          &PRMS(6)*FTtoM
  else
     if (LKECHO)write(JOSTND,4095) KEYWRD,IDT,&
          &PRMS(1)*FT2pACRtoM2pHA,&
          &PRMS(2),&
          &PRMS(3)*INtoCM,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*FTtoM,&
          &PRMS(6)*FTtoM
  end if
  4095 format (/A8,'   DATE/CYCLE=',I5,'; RESIDUAL=',F8.2,&
       &'; PROPORTION OF SELECTED TREES REMOVED=',F6.3/&
       &T12,'DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
       &F5.1,' CMS, AND'/&
       &T12,'HEIGHT OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
       &F5.1,' METERS.')
  go to 10

  !  ==========  OPTION NUMBER 39: MCFDLN  ============================MCFDLN

  4100 continue
  LCVOLS = .true.
  call SDEFLN(LNOTBK,ARRAY,KEYWRD,CFLA0,CFLA1,KARD,IS)
  if (IS .eq. -999) go to 10

  select case (VARACD)

  case ('CS','LS','NE','SN','ON')
     if (LKECHO)write(JOSTND,4110)
     4110 format(T12,'DEFECT CORRECTIONS WILL BE APPLIED TO ',&
          &'PULPWOOD VOLUME.')

  case default
     if (LKECHO)write(JOSTND,4120)
     4120 format(T12,'DEFECT CORRECTIONS WILL BE APPLIED TO ',&
          &'MERCHANTABLE CUBIC METER VOLUME.')

  end select
  go to 10

  !  ==========  OPTION NUMBER 40: BFFDLN  ============================BFFDLN

  4200 continue
  LBVOLS = .true.
  call SDEFLN(LNOTBK,ARRAY,KEYWRD,BFLA0,BFLA1,KARD,IS)
  if (IS .eq. -999) go to 10

  select case (VARACD)

  case ('CS','LS','NE','SN','ON')
     if (LKECHO)write(JOSTND,4210)
     4210 format(T12,'DEFECT CORRECTIONS WILL BE APPLIED TO ',&
          &'SAWLOG VOLUME.')

  case default
     if (LKECHO)write(JOSTND,4220)
     4220 format(T12,'DEFECT CORRECTIONS WILL BE APPLIED TO ',&
          &'METRIC BOARD VOLUME.')

  end select
  go to 10

  !  ==========  OPTION NUMBER 41:  MCDEFECT  =========================MCDEFECT

  4300 continue
  LCVOLS = .true.

  !     IF THE DATE WAS NOT SPECIFIED (LNOTBK(1)=F), OR = 0;
  !     THEN: CHANGE THE DEFAULT STANDARDS (DO NOT 'SCHEDULE' THE
  !     REQUEST TO CHANGE THE STANDARDS).

  if (LNOTBK(1) .and. ARRAY(1).gt.0.) go to 4350

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        if (LNOTBK(3)) CFDEFT(2,IGSP) = ARRAY(3)
        if (LNOTBK(4)) CFDEFT(3,IGSP) = ARRAY(4)
        if (LNOTBK(5)) CFDEFT(4,IGSP) = ARRAY(5)
        if (LNOTBK(6)) CFDEFT(5,IGSP) = ARRAY(6)
        if (LNOTBK(7)) then
           CFDEFT(6,IGSP) = ARRAY(7)
           CFDEFT(7,IGSP) = ARRAY(7)
           CFDEFT(8,IGSP) = ARRAY(7)
           CFDEFT(9,IGSP) = ARRAY(7)
        end if
        4321 continue
     end do
     ILEN = ISPGRP(-IS,92)

     select case (VARACD)

     case ('CS','LS','NE','SN','ON')
        if (LKECHO)write(JOSTND,4332) KEYWRD,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)

     case default
        if (LKECHO)write(JOSTND,4340) KEYWRD,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)

     end select
     go to 10

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq.0) then
     do IS = 1,MAXSP
        if (LNOTBK(3)) CFDEFT(2,IS) = ARRAY(3)
        if (LNOTBK(4)) CFDEFT(3,IS) = ARRAY(4)
        if (LNOTBK(5)) CFDEFT(4,IS) = ARRAY(5)
        if (LNOTBK(6)) CFDEFT(5,IS) = ARRAY(6)
        if (LNOTBK(7)) then
           CFDEFT(6,IS) = ARRAY(7)
           CFDEFT(7,IS) = ARRAY(7)
           CFDEFT(8,IS) = ARRAY(7)
           CFDEFT(9,IS) = ARRAY(7)
        end if
        4310 continue
     end do

     select case (VARACD)
     case ('CS','LS','NE','SN','ON')
        if (LKECHO)write(JOSTND,4312) KEYWRD,(ARRAY(I),I = 3,7)
        4312 format(/A8,T12,'PULPWOOD VOLUME DEFECT',&
             &' PROPORTIONS HAVE BEEN CHANGED FOR ALL SPECIES:',&
             &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
             &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
             &'; 63.5 CM AND LARGER TREES=',F6.2)
     case default
        if (LKECHO)write(JOSTND,4320) KEYWRD,(ARRAY(I),I = 3,7)
        4320 format(A8,T12,'CUBIC METER VOLUME DEFECT',&
             &' PROPORTIONS HAVE BEEN CHANGED FOR ALL SPECIES:',&
             &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
             &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
             &'; 63.5 CM AND LARGER TREES=',F6.2)

     end select
     goto 10

     !     SPECIES CODE IS SPECIFIED (IS>0).

  else
     if (LNOTBK(3)) CFDEFT(2,IS) = ARRAY(3)
     if (LNOTBK(4)) CFDEFT(3,IS) = ARRAY(4)
     if (LNOTBK(5)) CFDEFT(4,IS) = ARRAY(5)
     if (LNOTBK(6)) CFDEFT(5,IS) = ARRAY(6)
     if (LNOTBK(7)) then
        CFDEFT(6,IS) = ARRAY(7)
        CFDEFT(7,IS) = ARRAY(7)
        CFDEFT(8,IS) = ARRAY(7)
        CFDEFT(9,IS) = ARRAY(7)
     end if

     select case (VARACD)

     case ('CS','LS','NE','SN','ON')
        ILEN = 3
        if (LKECHO)write(JOSTND,4332) KEYWRD,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)
        4332 format(/A8,T12,'PULPWOOD VOLUME DEFECT PROPORTIONS HAVE ',&
             &'BEEN CHANGED FOR SPECIES:',A,' (CODE=',I3,')',&
             &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
             &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
             &'; 63.5 CM AND LARGER TREES=',F6.2)

     case default
        ILEN = 3
        if (LKECHO)write(JOSTND,4340) KEYWRD,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)
        4340 format(/A8,T12,'CUBIC METER VOLUME DEFECT PROPORTIONS HAVE ',&
             &'BEEN CHANGED FOR SPECIES:',A,' (CODE=',I3,')',&
             &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
             &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
             &'; 63.5 CM AND LARGER TREES=',F6.2)

     end select
     goto 10
  end if

  ! SCHEDULE THE ACTIVITY

  4350 continue
  call SDEFET (LNOTBK,ARRAY,KEYWRD,LOPEVN,215,KARD,IPRMPT)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  go to 10

  !  ==========  OPTION NUMBER 42:  BFDEFECT  =========================BFDEFECT

  4400 continue
  LBVOLS = .true.

  !     IF THE DATE WAS NOT SPECIFIED (LNOTBK(1)=F), OR = 0;
  !     THEN: CHANGE THE DEFAULT STANDARDS (DO NOT 'SCHEDULE' THE
  !     REQUEST TO CHANGE THE STANDARDS).

  if (LNOTBK(1) .and. ARRAY(1).gt.0.) go to 4450

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        if (LNOTBK(3)) BFDEFT(2,IGSP) = ARRAY(3)
        if (LNOTBK(4)) BFDEFT(3,IGSP) = ARRAY(4)
        if (LNOTBK(5)) BFDEFT(4,IGSP) = ARRAY(5)
        if (LNOTBK(6)) BFDEFT(5,IGSP) = ARRAY(6)
        if (LNOTBK(7)) then
           BFDEFT(6,IGSP) = ARRAY(7)
           BFDEFT(7,IGSP) = ARRAY(7)
           BFDEFT(8,IGSP) = ARRAY(7)
           BFDEFT(9,IGSP) = ARRAY(7)
        end if
        4021 continue
     end do
     ILEN = ISPGRP(-IS,92)

     select case (VARACD)

     case ('CS','LS','NE','SN','ON')
        if (LKECHO)write(JOSTND,4432) KEYWRD,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)

     case default
        if (LKECHO)write(JOSTND,4440) KEYWRD,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)

     end select
     go to 10

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq.0) then
     do IS = 1,MAXSP
        if (LNOTBK(3)) BFDEFT(2,IS) = ARRAY(3)
        if (LNOTBK(4)) BFDEFT(3,IS) = ARRAY(4)
        if (LNOTBK(5)) BFDEFT(4,IS) = ARRAY(5)
        if (LNOTBK(6)) BFDEFT(5,IS) = ARRAY(6)
        if (LNOTBK(7)) then
           BFDEFT(6,IS) = ARRAY(7)
           BFDEFT(7,IS) = ARRAY(7)
           BFDEFT(8,IS) = ARRAY(7)
           BFDEFT(9,IS) = ARRAY(7)
        end if
        4410 continue
     end do

     select case (VARACD)

     case ('CS','LS','NE','SN','ON')
        if (LKECHO)write(JOSTND,4412) KEYWRD,(ARRAY(I),I = 3,7)
        4412 format(/A8,T12,'SAWLOG VOLUME DEFECT',&
             &' PROPORTIONS HAVE BEEN CHANGED FOR ALL SPECIES:',&
             &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
             &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
             &'; 63.5 CM AND LARGER TREES=',F6.2)

     case default
        if (LKECHO)write(JOSTND,4420) KEYWRD,(ARRAY(I),I = 3,7)
        4420 format(/A8,T12,'BOARD FOOT VOLUME DEFECT',&
             &' PROPORTIONS HAVE BEEN CHANGED FOR ALL SPECIES:',&
             &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
             &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
             &'; 63.5 CM AND LARGER TREES=',F6.2)

     end select
     goto 10

     !     SPECIES CODE IS SPECIFIED (IS > 0).

  else
     if (LNOTBK(3)) BFDEFT(2,IS) = ARRAY(3)
     if (LNOTBK(4)) BFDEFT(3,IS) = ARRAY(4)
     if (LNOTBK(5)) BFDEFT(4,IS) = ARRAY(5)
     if (LNOTBK(6)) BFDEFT(5,IS) = ARRAY(6)
     if (LNOTBK(7)) then
        BFDEFT(6,IS) = ARRAY(7)
        BFDEFT(7,IS) = ARRAY(7)
        BFDEFT(8,IS) = ARRAY(7)
        BFDEFT(9,IS) = ARRAY(7)
     end if

     select case (VARACD)

     case ('CS','LS','NE','SN','ON')
        ILEN = 3
        if (LKECHO)write(JOSTND,4432) KEYWRD,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)
        4432 format(/A8,T12,'SAWLOG VOLUME DEFECT PROPORTIONS HAVE ',&
             &'BEEN CHANGED FOR SPECIES:',A,' (CODE=',I3,')',&
             &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
             &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
             &'; 63.5 CM AND LARGER TREES=',F6.2)

     case default
        ILEN = 3
        if (LKECHO)write(JOSTND,4440) KEYWRD,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)
        4440 format(/A8,T12,'BOARD FOOT VOLUME DEFECT PROPORTIONS ',&
             &'HAVE BEEN CHANGED FOR SPECIES:',A,' (CODE=',I3,')',&
             &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
             &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
             &'; 63.5 CM AND LARGER TREES=',F6.2)

     end select
     goto 10
  end if

  !  SCHEDULE THE ACTIVITY

  4450 continue
  call SDEFET (LNOTBK,ARRAY,KEYWRD,LOPEVN,216,KARD,IPRMPT)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  go to 10

  !  ==========  OPTION NUMBER 43:  VOLUME  ===========================VOLUME

  4500 continue
  LCVOLS = .true.

  ARRAY(3) = ARRAY(3)*CMtoIN
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoFT

  !  SET ITOPD8=1 TO DISSALLOW MODIFICATION OF TOP MERCH. SPEC.
  !  IN R8 VARIANTS

  select case (VARACD)

  case ('SN')
     ITOPD8 = 1

  case default
     ITOPD8 = 0

  end select

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10

  !     IF THE DATE WAS NOT SPECIFIED (LNOTBK(1)=F), OR = 0;
  !     THEN: CHANGE THE DEFAULT STANDARDS (DO NOT 'SCHEDULE' THE
  !     REQUEST TO CHANGE THE STANDARDS).

  if (LNOTBK(1) .and. ARRAY(1).gt.0.) goto 4540

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        if (LNOTBK(3)) DBHMIN(IGSP) = ARRAY(3)
        if (LNOTBK(4).and.(ITOPD8.le.0)) then
           TOPD(IGSP) = ARRAY(4)
        end if
        if (LNOTBK(5)) STMP(IGSP) = ARRAY(5)
        if (LNOTBK(6)) FRMCLS(IGSP) = ARRAY(6)
        if (LNOTBK(7)) METHC(IGSP) = INT(ARRAY(7))
        ILEN = ISPGRP(IGRP,92)
        if (VARACD .eq. 'AK') then
           if (LKECHO)write(JOSTND,4534) KEYWRD,KARD(2)(1:ILEN),&
                &IGSP,DBHMIN(IGSP),TOPD(IGSP),STMP(IGSP),FRMCLS(IGSP)
        else if (ITOPD8.gt.0) then
           write(JOSTND,4537) KEYWRD,KARD(2)(1:ILEN),&
                &IGSP,DBHMIN(IGSP)*INtoCM,STMP(IGSP)*FTtoCM,&
                &FRMCLS(IGSP)
        else
           if (LKECHO)write(JOSTND,4535) KEYWRD,KARD(2)(1:ILEN),&
                &IGSP,DBHMIN(IGSP)*INtoCM,TOPD(IGSP)*INtoCM,&
                &STMP(IGSP)*FTtoCM,FRMCLS(IGSP)
        end if
        if (LNOTBK(7).and.LKECHO)write(JOSTND,4536)METHC(IGSP)
        4505 continue
     end do
     goto 10

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq.0) then
     do IS = 1,MAXSP
        if (LNOTBK(3)) DBHMIN(IS) = ARRAY(3)
        if (LNOTBK(4).and.(ITOPD8.le.0))&
             &TOPD(IS) = ARRAY(4)
        if (LNOTBK(5)) STMP(IS) = ARRAY(5)
        if (LNOTBK(6)) FRMCLS(IS) = ARRAY(6)
        if (LNOTBK(7)) METHC(IS) = INT(ARRAY(7))
        4510 continue
     end do
     if (LKECHO)write(JOSTND,4520) KEYWRD
     4520 format (/A8,'   THE FOLLOWING MERCHANTABILITY ',&
          &'STANDARDS HAVE BEEN CHANGED FOR ALL SPECIES:')
     if (LNOTBK(3).and.LKECHO)write(JOSTND,4521) ARRAY(3)*INtoCM
     if (ITOPD8.gt.0) then
        write(JOSTND,4527)
     else if (LNOTBK(4).and.LKECHO) then
        write(JOSTND,4528)
     else if (LNOTBK(4).and.LKECHO) then
        write(JOSTND,4522) ARRAY(4)*INtoCM
     end if
     if (LNOTBK(5).and.LKECHO)write(JOSTND,4523) ARRAY(5)*FTtoCM
     if (LNOTBK(6)) then

        select case (VARACD)

        case ('AK')
           if (LKECHO)write(JOSTND,4526)ARRAY(6)

        case default
           if (LKECHO)write(JOSTND,4524) ARRAY(6)

        end select
     end if
     if (LNOTBK(7).and.LKECHO)write(JOSTND,4525) ARRAY(7)
     4521 format (T22,'MINIMUM DBH =',F6.2)
     4522 format (T22,'TOP DIAMETER=',F6.2)
     4523 format (T22,'STUMP HEIGHT=',F6.2)
     4524 format (T22,'FORM CLASS  =',F6.2)
     4525 format (T22,'METHOD OF VOLUME CALCULATION IS ',F5.0)
     4526 format (T22,'LOG LENGTH  =',F6.2)
     4527 format (T22,'TOP DIAMETER CAN NOT BE MODIFIED. IT WILL BE ',&
          &'SET TO DEFAULT VALUE')
     4528 format (T22,'TOP DIAMETER CAN NOT BE MODIFIED. IT WILL BE ',&
          &'SET TO DEFAULT VALUE BASED ON NATIONAL FOREST AND SPECIES')
     goto 10
  else

     !     SPECIES CODE IS SPECIFIED (IS>0).

     if (LNOTBK(3)) DBHMIN(IS) = ARRAY(3)
     if (LNOTBK(4).and.(ITOPD8.le.0)) then
        TOPD(IS) = ARRAY(4)
     end if
     if (LNOTBK(5)) STMP(IS) = ARRAY(5)
     if (LNOTBK(6)) FRMCLS(IS) = ARRAY(6)
     if (LNOTBK(7)) METHC(IS) = INT(ARRAY(7))
     ILEN = 3
     if (VARACD .eq. 'AK') then
        if (LKECHO)write(JOSTND,4534) KEYWRD,KARD(2)(1:ILEN),IS,&
             &DBHMIN(IS),TOPD(IS),STMP(IS),FRMCLS(IS)
        4534 format (/A8,'   MERCHANTABILITY STANDARDS FOR ',&
             &'SPECIES= ',A,' (CODE=',I3,') ARE:  MINIMUM DBH=',F6.2,&
             &'; TOP DIAMETER=',F6.2,'; STUMP HEIGHT=',F6.2,/T12,&
             &'LOG LENGTH=',F6.2)
     else if (ITOPD8.gt.0) then
        if (LKECHO)write(JOSTND,4537) KEYWRD,KARD(2)(1:ILEN),&
             &IS,DBHMIN(IS)*INtoCM,STMP(IS)*FTtoCM,FRMCLS(IS)
     else
        if (LKECHO)write(JOSTND,4535) KEYWRD,KARD(2)(1:ILEN),IS,&
             &DBHMIN(IS)*INtoCM,&
             &TOPD(IS)*INtoCM,&
             &STMP(IS)*FTtoCM,&
             &FRMCLS(IS)
        4535 format (/A8,'   MERCHANTABILITY STANDARDS FOR ',&
             &'SPECIES= ',A,' (CODE=',I3,') ARE:  MINIMUM DBH=',F6.2,&
             &'; TOP DIAMETER=',F6.2,'; STUMP HEIGHT=',F6.2,/T12,&
             &'FORM CLASS=',F6.2)
     end if
     if (LNOTBK(7).and.LKECHO)write(JOSTND,4536)METHC(IS)
     4536 format (T12,'METHOD OF VOLUME CALCULATION IS ',I6)
     4537 format (/A8,'   MERCHANTABILITY STANDARDS FOR ',&
          &'SPECIES= ',A,' (CODE=',I3,') ARE:  MINIMUM DBH=',F6.2,&
          &'; TOP DIAMETER'/,T12,'CAN NOT BE MODIFIED. IT WILL BE ',&
          &'SET TO DEFAULT VALUE; STUMP HEIGHT=',F6.2,&
          &' FORM CLASS=',F6.2)
     goto 10
  end if

  !     DATE WAS SPECIFIED;  SCHEDULE THE CHANGING OF MERCH-
  !     ABILITY STANDARDS.

  4540 continue
  IDT = IFIX(ARRAY(1))

  !     IF (IS<0) SCHEDULE CHANGE FOR THE SPECIES GROUP

  !     SET DEFAULTS TO THOSE FOR THE FIRST SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IFSP = ISPGRP(IGRP,2)
     if (.not.LNOTBK(3)) ARRAY(3) = DBHMIN(IFSP)
     if (LNOTBK(4).and.(ITOPD8.gt.0))&
          &ARRAY(4) = TOPD(IFSP)
     if (.not.LNOTBK(5)) ARRAY(5) = STMP(IFSP)
     if (.not.LNOTBK(6)) ARRAY(6) = FRMCLS(IFSP)
     if (.not.LNOTBK(7)) ARRAY(7) = METHC(IFSP)
     call OPNEW(KODE,IDT,217,6,ARRAY(2))
     if (KODE.gt.0) goto 10
     ILEN = ISPGRP(IGRP,92)
     if (VARACD .eq. 'AK') then
        if (LKECHO)write(JOSTND,4544) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM, ARRAY(4)*INtoCM, ARRAY(5)*FTtoCM,&
             &(ARRAY(I),I = 6,7)
     else if (ITOPD8.gt.0) then
        write(JOSTND,4547) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM, ARRAY(5)*FTtoCM,(ARRAY(I),I = 6,7)
     else
        if (LKECHO)write(JOSTND,4545) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM,ARRAY(4)*INtoCM,ARRAY(5)*FTtoCM,&
             &(ARRAY(I),I = 6,7)
     end if

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

     !     SPECIES CODE IS BLANK; SET ALL SPECIES DEFAULTS TO DEFAULT
     !     WHICH IS CURRENTLY IN FORCE FOR SPECIES 1.

  else if (IS .eq.0) then
     if (.not.LNOTBK(3)) ARRAY(3) = DBHMIN(1)
     if (LNOTBK(4).and.(ITOPD8.gt.0))&
          &ARRAY(4) = TOPD(1)
     if (.not.LNOTBK(5)) ARRAY(5) = STMP(1)
     if (.not.LNOTBK(6)) ARRAY(6) = FRMCLS(1)
     if (.not.LNOTBK(7)) ARRAY(7) = METHC(1)
     ARRAY(2) = 0.
     call OPNEW(KODE,IDT,217,6,ARRAY(2))
     if (KODE.gt.0) goto 10
     if (VARACD.eq.'AK') then
        if (LKECHO)write(JOSTND,4574) KEYWRD,IDT,(ARRAY(I),I = 3,7)
        4574 format(/A8,'   DATE/CYCLE=',I5,'; ALL SPECIES (CODE= 0)',&
             &'; MINIMUM DBH=',F6.2,'; TOP DIAMETER=',F6.2,&
             &'; STUMP HEIGHT=',F6.2,/T12,'LOG LENGTH=',F6.2,&
             &'; METH OF VOL CALC=',F6.0)
     else if (ITOPD8.gt.0) then
        write(JOSTND,4547) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM, ARRAY(5)*FTtoCM,(ARRAY(I),I = 6,7)
     else
        if (LKECHO)write(JOSTND,4575) KEYWRD,IDT,&
             &ARRAY(3)*INtoCM,&
             &ARRAY(4)*INtoCM,&
             &ARRAY(5)*FTtoCM,&
             &(ARRAY(I),I = 6,7)
        4575 format(/A8,'   DATE/CYCLE=',I5,'; ALL SPECIES (CODE= 0)',&
             &'; MINIMUM DBH=',F6.2,'; TOP DIAMETER=',F6.2,&
             &'; STUMP HEIGHT=',F6.2,/T12,' FORM CLASS=',F6.2,&
             &'; METH OF VOL CALC=',F6.0)
     end if
  else

     !     SPECIES CODE IS SPECIFIED (IS>0).

     if (.not.LNOTBK(3)) ARRAY(3) = DBHMIN(IS)
     if (LNOTBK(4).and.(ITOPD8.gt.0))&
          &ARRAY(4) = TOPD(IS)
     if (.not.LNOTBK(5)) ARRAY(5) = STMP(IS)
     if (.not.LNOTBK(6)) ARRAY(6) = FRMCLS(IS)
     if (.not.LNOTBK(7)) ARRAY(7) = METHC(IS)
     call OPNEW(KODE,IDT,217,6,ARRAY(2))
     if (KODE.gt.0) goto 10
     ILEN = 3
     if (VARACD .eq. 'AK') then
        if (LKECHO)write(JOSTND,4544) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)
        4544 format(/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,&
             &' (CODE= ',I3,&
             &'); MINIMUM DBH=',F6.2,'; TOP DIAMETER=',F6.2,&
             &'; STUMP HEIGHT=',F6.2,/T12,'LOG LENGTH=',F6.2,&
             &';  METH OF VOL CALC=',F6.0)
     else if (ITOPD8.gt.0) then
        write(JOSTND,4547) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM, ARRAY(5)*FTtoCM,(ARRAY(I),I = 6,7)
     else
        if (LKECHO)write(JOSTND,4545) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM,&
             &ARRAY(4)*INtoCM,&
             &ARRAY(5)*FTtoCM,&
             &ARRAY(6),ARRAY(7)
        4545 format(/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,&
             &' (CODE= ',I3,&
             &'); MINIMUM DBH=',F6.2,'; TOP DIAMETER=',F6.2,&
             &'; STUMP HEIGHT=',F6.2,/T12,'FORM CLASS=',F6.2,&
             &';  METH OF VOL CALC=',F6.0)

        4547 format(/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,&
             &' (CODE= ',I3,&
             &'); MINIMUM DBH=',F6.2,'; TOP DIAMETER CAN NOT BE ',&
             &'MODIFIED.'/T12,'IT WILL BE SET TO DEFAULT VALUE',&
             &'; STUMP HEIGHT=',F6.2,' FORM CLASS=',F6.2,&
             &';  METH OF VOL CALC=',F6.0)
     end if
  end if
  goto 10

  !  ==========  OPTION NUMBER 44: BFVOLUME ===========================BFVOLUME

  4600 continue
  LBVOLS = .true.

  ARRAY(3) = ARRAY(3)*CMtoIN
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoFT

  !  SET ITOPD8=1 TO DISSALLOW MODIFICATION OF TOP MERCH. SPEC.
  !  IN R8 VARIANTS RESPECTIVELY

  select case (VARACD)

  case ('SN')
     ITOPD8 = 1

  case default
     ITOPD8 = 0

  end select

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10

  !     IF THE DATE WAS NOT SPECIFIED (LNOTBK(1)=F), OR = 0;
  !     THEN: CHANGE THE DEFAULT STANDARDS (DO NOT 'SCHEDULE' THE
  !     REQUEST TO CHANGE THE STANDARDS).

  if (LNOTBK(1) .and. ARRAY(1).gt.0.) goto 4640

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        if (LNOTBK(3)) BFMIND(IGSP) = ARRAY(3)
        if (LNOTBK(4).and.(ITOPD8.le.0)) then
           BFTOPD(IGSP) = ARRAY(4)
        end if
        if (LNOTBK(5)) BFSTMP(IGSP) = ARRAY(5)
        if (LNOTBK(6)) FRMCLS(IGSP) = ARRAY(6)
        if (LNOTBK(7)) METHB(IGSP) = INT(ARRAY(7))
        ILEN = ISPGRP(IGRP,92)
        if (VARACD .eq. 'AK') then
           if (LKECHO)write(JOSTND,4634) KEYWRD,KARD(2)(1:ILEN),IGSP,&
                &BFMIND(IGSP),BFTOPD(IGSP),BFSTMP(IGSP),FRMCLS(IGSP)
        else if (ITOPD8.gt.0) then
           write(JOSTND,4537) KEYWRD,KARD(2)(1:ILEN),IGSP,&
                &BFMIND(IGSP)*INtoCM,BFSTMP(IGSP)*FTtoCM,FRMCLS(IGSP)
        else
           if (LKECHO)write(JOSTND,4635) KEYWRD,KARD(2)(1:ILEN),IGSP,&
                &BFMIND(IGSP),BFTOPD(IGSP),BFSTMP(IGSP),FRMCLS(IGSP)
        end if
        if (LNOTBK(7).and.LKECHO)write(JOSTND,4536)METHB(IGSP)
        4605 continue
     end do
     goto 10

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq.0) then
     do IS = 1,MAXSP
        if (LNOTBK(3)) BFMIND(IS) = ARRAY(3)
        if (LNOTBK(4).and.(ITOPD8.le.0))&
             &BFTOPD(IS) = ARRAY(4)
        if (LNOTBK(5)) BFSTMP(IS) = ARRAY(5)
        if (LNOTBK(6)) FRMCLS(IS) = ARRAY(6)
        if (LNOTBK(7)) METHB(IS) = INT(ARRAY(7))
        4615 continue
     end do
     if (LKECHO)write(JOSTND,4620) KEYWRD
     4620 format (/1X,A8,'   THE FOLLOWING MERCHANTABILITY ',&
          &'STANDARDS HAVE BEEN CHANGED FOR ALL SPECIES:')
     if (LNOTBK(3).and.LKECHO)write(JOSTND,4621) ARRAY(3)*INtoCM
     if (LNOTBK(4).and.(ITOPD8.gt.0)) then
        write(JOSTND,4527)
     else if (LNOTBK(4).and.LKECHO) then
        write(JOSTND,4522) ARRAY(4)*INtoCM
     end if
     if (LNOTBK(5).and.LKECHO)write(JOSTND,4623) ARRAY(5)*FTtoCM
     if (LNOTBK(6)) then
        if (VARACD .eq. 'AK') then
           if (LKECHO)write(JOSTND,4626) ARRAY(6)
        else
           if (LKECHO)write(JOSTND,4624) ARRAY(6)
        end if
     end if
     if (LNOTBK(7).and.LKECHO)write(JOSTND,4625) ARRAY(7)
     4621 format (T22,'MINIMUM DBH =',F6.2)
     4623 format (T22,'STUMP HEIGHT=',F6.2)
     4624 format (T22,'FORM CLASS  =',F6.2)
     4625 format (T22,'METHOD OF VOLUME CALCULATION IS ',F5.0)
     4626 format (T22,'LOG LENGTH  =',F6.2)
     goto 10
  else

     !     SPECIES CODE IS SPECIFIED (IS>0).

     if (LNOTBK(3)) BFMIND(IS) = ARRAY(3)
     if (LNOTBK(4).and.(ITOPD8.le.0)) then
        BFTOPD(IS) = ARRAY(4)
     end if
     if (LNOTBK(5)) BFSTMP(IS) = ARRAY(5)
     if (LNOTBK(6)) FRMCLS(IS) = ARRAY(6)
     if (LNOTBK(7)) METHB(IS) = INT(ARRAY(7))
     ILEN = 3
     if (VARACD .eq. 'AK') then
        if (LKECHO)write(JOSTND,4634) KEYWRD,KARD(2)(1:ILEN),IS,&
             &BFMIND(IS),BFTOPD(IS),BFSTMP(IS),FRMCLS(IS)
        4634 format (/A8,'   MERCHANTABILITY STANDARDS FOR ',&
             &'SPECIES= ',A,' (CODE=',I3,') ARE:  MINIMUM DBH=',F6.2,&
             &'; TOP DIAMETER=',F6.2,'; STUMP HEIGHT=',F6.2/T12,&
             &'LOG LENGTH=',F6.2)
     else if (ITOPD8.gt.0) then
        if (LKECHO)write(JOSTND,4537) KEYWRD,KARD(2)(1:ILEN),&
             &IS,BFMIND(IS)*INtoCM,STMP(IS)*FTtoCM,FRMCLS(IS)
     else
        if (LKECHO)write(JOSTND,4635) KEYWRD,KARD(2)(1:ILEN),IS,&
             &BFMIND(IS)*INtoCM,&
             &BFTOPD(IS)*INtoCM,&
             &BFSTMP(IS)*FTtoCM,&
             &FRMCLS(IS)
        4635 format (/A8,'   MERCHANTABILITY STANDARDS FOR ',&
             &'SPECIES= ',A,' (CODE=',I3,') ARE:  MINIMUM DBH=',F6.2,&
             &'; TOP DIAMETER=',F6.2,'; STUMP HEIGHT=',F6.2/T12,&
             &'FORM CLASS=',F6.2)
     end if
     if (LNOTBK(7).and.LKECHO)write(JOSTND,4536)METHB(IS)
     goto 10
  end if

  !     DATE WAS SPECIFIED;  SCHEDULE THE CHANGING OF MERCH-
  !     ABILITY STANDARDS.

  4640 continue
  IDT = IFIX(ARRAY(1))

  !     IF (IS<0) SCHEDULE CHANGE FOR THE SPECIES GROUP

  !     SET DEFAULTS TO THOSE FOR THE FIRST SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IFSP = ISPGRP(IGRP,2)
     if (.not.LNOTBK(3)) ARRAY(3) = BFMIND(IFSP)
     if (LNOTBK(4).and.(ITOPD8.gt.0))&
          &ARRAY(4) = BFTOPD(IFSP)
     if (.not.LNOTBK(5)) ARRAY(5) = BFSTMP(IFSP)
     if (.not.LNOTBK(6)) ARRAY(6) = FRMCLS(IFSP)
     if (.not.LNOTBK(7)) ARRAY(7) = METHB (IFSP)
     call OPNEW(KODE,IDT,218,6,ARRAY(2))
     if (KODE.gt.0) goto 10
     ILEN = ISPGRP(IGRP,92)
     if (VARACD.eq.'AK') then
        if (LKECHO)write(JOSTND,4645) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)
     else if (ITOPD8.gt.0) then
        write(JOSTND,4547) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM,ARRAY(5)*FTtoCM,(ARRAY(I),I = 6,7)
     else
        if (LKECHO)write(JOSTND,4646) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)
     end if

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

     !     SPECIES CODE IS BLANK; SET ALL SPECIES DEFAULTS TO DEFAULT
     !     WHICH IS CURRENTLY IN FORCE FOR SPECIES 1.

  else if (IS .eq.0) then
     if (.not.LNOTBK(3)) ARRAY(3) = BFMIND(1)
     if (LNOTBK(4).and.(ITOPD8.gt.0))&
          &ARRAY(4) = BFTOPD(1)
     if (.not.LNOTBK(5)) ARRAY(5) = BFSTMP(1)
     if (.not.LNOTBK(6)) ARRAY(6) = FRMCLS(1)
     if (.not.LNOTBK(7)) ARRAY(7) = METHB(1)
     ARRAY(2) = 0.
     call OPNEW(KODE,IDT,218,6,ARRAY(2))
     if (KODE.gt.0) goto 10
     if (VARACD.eq.'AK') then
        if (LKECHO)write(JOSTND,4674) KEYWRD,IDT,(ARRAY(I),I = 3,7)
        4674 format(/A8,'   DATE/CYCLE=',I5,'; ALL SPECIES (CODE= 0)',&
             &'; MINIMUM DBH=',F6.2,'; TOP DIAMETER=',F6.2,&
             &'; STUMP HEIGHT=',F6.2/T12,'LOG LENGTH=',F6.2,&
             &'; METH OF VOL CALC=',F6.0)
     else if (ITOPD8.gt.0) then
        write(JOSTND,4547) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM,ARRAY(5)*FTtoCM,(ARRAY(I),I = 6,7)
     else
        if (LKECHO)write(JOSTND,4675) KEYWRD,IDT,&
             &ARRAY(3)*INtoCM,&
             &ARRAY(4)*INtoCM,&
             &ARRAY(5)*FTtoCM,&
             &ARRAY(6),ARRAY(7)
        4675 format(/A8,'   DATE/CYCLE=',I5,'; ALL SPECIES (CODE= 0)',&
             &'; MINIMUM DBH=',F6.2,'; TOP DIAMETER=',F6.2,&
             &'; STUMP HEIGHT=',F6.2/T12,'FORM CLASS=',F6.2,&
             &'; METH OF VOL CALC=',F6.0)
     end if
  else

     !     SPECIES CODE IS SPECIFIED (IS>0).

     if (.not.LNOTBK(3)) ARRAY(3) = BFMIND(IS)
     if (LNOTBK(4).and.(ITOPD8.le.0.)) then
        BFTOPD(IS) = ARRAY(4)
     end if
     if (.not.LNOTBK(5)) ARRAY(5) = BFSTMP(IS)
     if (.not.LNOTBK(6)) ARRAY(6) = FRMCLS(IS)
     if (.not.LNOTBK(7)) ARRAY(7) = METHB (IS)
     call OPNEW(KODE,IDT,218,6,ARRAY(2))
     if (KODE.gt.0) goto 10
     ILEN = 3
     if (VARACD.eq.'AK') then
        if (LKECHO)write(JOSTND,4645) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &(ARRAY(I),I = 3,7)
        4645 format(/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,&
             &' (CODE= ',I3,&
             &'); MINIMUM DBH=',F6.2,'; TOP DIAMETER=',F6.2,&
             &'; STUMP HEIGHT=',F6.2,/T12,'LOG LENGTH=',F6.2,&
             &'; METH OF VOL CALC=',F6.0)
     else if (ITOPD8.gt.0) then
        if (LKECHO)write(JOSTND,4537) KEYWRD,KARD(2)(1:ILEN),&
             &IS,BFMIND(IS)*INtoCM,STMP(IS)*FTtoCM,FRMCLS(IS)
     else
        if (LKECHO)write(JOSTND,4646) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
             &ARRAY(3)*INtoCM,&
             &ARRAY(4)*INtoCM,&
             &ARRAY(5)*FTtoCM,&
             &ARRAY(6),ARRAY(7)
        4646 format(/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,&
             &' (CODE= ',I3,&
             &'); MINIMUM DBH=',F6.2,'; TOP DIAMETER=',F6.2,&
             &'; STUMP HEIGHT=',F6.2,/T12,'FORM CLASS=',F6.2,&
             &'; METH OF VOL CALC=',F6.0)
     end if
  end if
  goto 10

  !  ==========  OPTION NUMBER 45: REGDMULT ===========================REGDMULT

  4700 continue
  I = 96
  goto 6005

  !  ==========  OPTION NUMBER 46:  COVER =============================COVER

  4800 continue
  call CVIN  (KEYWRD,ARRAY,LNOTBK,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  go to 10

  !  ==========  OPTION NUMBER 47:  ESTAB  ============================ESTAB

  4900 continue
  if (LKECHO)write(JOSTND,4910) KEYWRD
  4910 format (/A8,'   REGENERATION ESTABLISHMENT OPTIONS:')
  call ESIN (KEYWRD,ARRAY,LNOTBK,KARD,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  go to 10

  !  ==========  OPTION NUMBER 48:   MINHARV  =========================MINHARV

  5000 continue
  ARRAY(2) = ARRAY(2)*M3pHAtoFT3pACR
  ARRAY(3) = 0.0 ! bd feet not used
  ARRAY(4) = ARRAY(4)*M2pHAtoFT2pACR
  ARRAY(5) = ARRAY(5)*M3pHAtoFT3pACR
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,200,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if
  call OPNEW(KODE,IDT,200,4,ARRAY(2))
  if (KODE.gt.0) goto 10

  select case (VARACD)

  case ('CS','LS','NE','SN','ON')
     if (LKECHO)write(JOSTND,5010) KEYWRD,IDT,&
          &ARRAY(2)*FT3pACRtoM3pHA,&
          &ARRAY(3),&
          &ARRAY(4)*FT2pACRtoM2pHA,&
          &ARRAY(5)*FT3pACRtoM3pHA
     5010 format (/A8,'   DATE/CYCLE=',I5,'; CUTTING MUST EXCEED ',&
          &'THE FOLLOWING:',/,29X,F8.1,' SAWLOG M3/HA,',F8.1,&
          &' SAWLOG BDFT,',F6.1,' M2/HA BASAL AREA, AND',&
          &F8.1,' TOTAL MERCH M3/HA.')

  case default
     if (LKECHO)write(JOSTND,5011) KEYWRD,IDT,&
          &ARRAY(2)*FT3pACRtoM3pHA,&
          &ARRAY(3),&
          &ARRAY(4)*FT2pACRtoM2pHA,&
          &ARRAY(5)*FT3pACRtoM3pHA
     5011 format (/A8,'   DATE/CYCLE=',I5,'; CUTTING MUST EXCEED ',&
          &'THE FOLLOWING:',/,29X,F8.1,' MERCH M3/HA,',F8.1,&
          &' MERCH BDFT,',F6.1,' M2/HA BASAL AREA, AND',&
          &F8.1,' TOTAL M3/HA.')
  end select
  go to 10

  !  ==========  OPTION NUMBER 49: SPECPREF  ==========================SPECPREF

  5100 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,201,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10

  !     IF THE SELECTION VALUE IS NOT PRESENT;
  !     THEN:  WRITE ERROR MSG AND SKIP PROCESSING KEYWORD CARD.

  if (.not.LNOTBK(3)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !     SCHEDULE THE ACTIVITY

  call OPNEW(KODE,IDT,201,2,ARRAY(2))
  if (KODE .gt.0) goto 10
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,5160) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
       &ARRAY(3)
  5160 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,' (CODE=',&
       &I3,'); THINNING SELECTION PRIORITY=',F6.0)
  goto 10

  !  ==========  OPTION NUMBER 50: SPCODES  ===========================SPCODES

  5200 continue

  !     SPECIES CODE PROCESSING.

  call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.lt.0) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     go to 10
  end if
  if (IS.eq.0) then
     if (LKECHO)write(JOSTND,5210) KEYWRD,JSP
     5210 format(/A8,'   PREVIOUS CODES:',&
          &((T32,10('''',A4,''';'),'''',A4,''''))/)
     read (IREAD,5250,end = 80) JSP
     5250 format  (20A4)
     IRECNT = IRECNT + 1
     if (LKECHO)write(JOSTND,5260) JSP
     5260 format (T12,'NEW CODES:',&
          &((T32,10('''',A4,''';'),'''',A4,''''))/)
     if (MORDAT) call ERRGRO (.true.,7)
     go to 10
  end if
  if (LKECHO)write(JOSTND,5275) KEYWRD,KARD(1)(1:3),IS,JSP(IS)
  5275 format(/A8,'   PREVIOUS CODE FOR SPECIES= ',A,' (CODE= ',I3,&
       &') IS= ''',A4,'''')
  read (IREAD,5250,end = 80) JSP(IS)
  IRECNT = IRECNT+1
  if (LKECHO)write(JOSTND,5280) KARD(1)(1:3),IS,JSP(IS)
  5280 format(T17,'NEW CODE FOR SPECIES= ',A,' (CODE= ',I3,&
       &') IS= ''',A4,'''')
  if (MORDAT) call ERRGRO (.true.,7)
  goto 10

  !  ==========  OPTION NUMBER 51: NODEBUG  ===========================NODEBUG

  5300 continue
  call DBINIT
  DEBUG = .false.
  if (LKECHO)write(JOSTND,9999) KEYWRD
  go to 10

  !  ==========  OPTION NUMBER 52: CUTEFF  ============================CUTEFF

  5400 continue
  if ( LNOTBK(1) ) EFF = ARRAY(1)
  if (LKECHO)write(JOSTND,5410) KEYWRD,EFF
  5410 format (/A8,'   PROPORTION OF SELECTED TREES REMOVED=',&
       &F6.3,' (REPLACES THE DEFAULT)')
  go to 10

  !  ==========  OPTION NUMBER 53: NOTRIPLE  ==========================NOTRIPLE

  5500 continue
  NOTRIP = .true.
  if (LKECHO)write(JOSTND,9999) KEYWRD
  go to 10

  !  ==========  OPTION NUMBER 54: READCORD  ==========================READCORD

  5600 continue
  LDCOR2 = .true.
  if (LKECHO)write(JOSTND,5610) KEYWRD
  5610 format (/A8,'   LARGE TREE DIAMETER GROWTH MODELS ',&
       &'ARE MODIFIED WITH CORRECTION TERMS PRIOR TO CALIBRATION.')
  read (IREAD,5650,end = 80) COR2
  5650 format (8F10.0)
  I = MAXSP/8
  if (MOD(MAXSP,8).gt.0) I = I+1
  IRECNT = IRECNT + I
  if (LKECHO)write(JOSTND,5660)
  5660 format (T12,'LARGE TREE DIAMETER GROWTH MODEL ',&
       &'CORRECTION TERMS BY SPECIES:')
  if (LKECHO)write(JOSTND,5670) (NSP(I,1),COR2(I),I = 1,MAXSP)
  5670 format((T12,7(A2,' =',F5.2,:,'; '),A2,' =',F5.2))
  go to 10

  !  ==========  OPTION NUMBER 55: REUSCORD ===========================REUSCORD

  5700 continue
  if (LKECHO)write(JOSTND,5610) KEYWRD
  LDCOR2 = .true.
  go to 10

  !  ==========  OPTION NUMBER 56: NOCALIB  ===========================NOCALIB

  5800 continue

  !   SPECIES CODE PROCESSING.

  call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        LDGCAL(IGSP) = .false.
        LHTCAL(IGSP) = .false.
        5801 continue
     end do
     ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,5808) KEYWRD,KARD(1)(1:ILEN),IS

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq.0) then
     do J = 1,MAXSP
        LDGCAL(J) = .false.
        LHTCAL(J) = .false.
        5803 continue
     end do
     if (LKECHO)write(JOSTND,5804) KEYWRD
     5804 format(/A8,'   CALIBRATION USING GROWTH DATA FROM TREE',&
          &' RECORDS HAS BEEN SUPPRESSED FOR ALL SPECIES')

     !     SPECIES CODE IS SPECIFIED (IS>0).

  else
     LDGCAL(IS) = .false.
     LHTCAL(IS) = .false.
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,5808) KEYWRD,KARD(1)(1:ILEN),IS
     5808 format (/A8,'   CALIBRATION USING GROWTH DATA FROM TREE',&
          &' RECORDS WILL BE SUPPRESSED FOR SPECIES= ',A,' (CODE= ',I3,')')
  end if
  go to 10

  !  ==========  OPTION NUMBER 57: DGSTDEV  ===========================DGSTDEV

  5900 continue
  if ( LNOTBK(1) ) DGSD = ARRAY(1)
  if (LKECHO)write(JOSTND,5910) KEYWRD,DGSD
  5910 format (/A8,'   RANDOM VARIATION THAT IS ADDED TO PREDICTED ',&
       &'DIAMETER GROWTH IS BOUNDED TO ',F4.1,' STANDARD DEVIATIONS.')
  if ((DGSD.lt.1.0).and.LKECHO)write(JOSTND,5920)
  5920 format (T12,'NO RANDOM VARIATION IS ADDED TO DIAMETER ',&
       &'GROWTH PREDICTIONS.')
  go to 10

  !  ==========  OPTION NUMBER 58: BAIMULT  ===========================BAIMULT

  6000 continue
  I = 91
  6005 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.le.0) goto 6010
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,I,KEYWRD,KARD,&
          &IPRMPT,IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  6010 continue
  if (.not.LNOTBK(3)) ARRAY(3) = 1.0

  !   SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  call OPNEW(KODE,IDT,I,2,ARRAY(2))
  if (KODE.gt.0) goto 10
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,6030) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
       &ARRAY(3)
  6030 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES=',A,' (CODE= ',&
       &I3,'); MULTIPLIER=',F10.4)
  goto 10

  !  ==========  OPTION NUMBER 59: MORTMULT  ==========================MORTMULT

  6100 continue
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoIN
  LMORT = .true.
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,94,KEYWRD,KARD,IPRMPT,&
             &IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10
  if (.not.LNOTBK(3)) ARRAY(3) = 1.0
  if (.not.LNOTBK(5)) ARRAY(5) = 999.0
  ARRAY(6) = 0.
  if (ARRAY(4).ge.ARRAY(5)) then
     call ERRGRO (.true.,4)
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  else
     call OPNEW (KODE,IDT,94,4,ARRAY(2))
     if (KODE.gt.0) goto 10
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,6131) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
          &ARRAY(3)
     6131 format(/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,&
          &' (CODE= ',I3,'); MULTIPLIER=',F10.2)
     if ((ARRAY(3) .gt.0.0).and.LKECHO)write(JOSTND,6130)&
          &ARRAY(4)*INtoCM,&
          &ARRAY(5)*INtoCM
     6130 format (T12,'ONLY TREES GREATER THAN ',F7.2,&
          &' AND LESS THAN ',F7.2,' CM DBH ARE AFFECTED.')
  end if
  goto 10

  !  ==========  OPTION NUMBER 60: NOHTDREG ===========================NOHTDREG

  6200 continue

  !     PROCESS BASED ON SPECIES CODE.

  call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        if (LNOTBK(2).and.ARRAY(2).gt.0.) then
           LHTDRG(IGSP) = .true.
        else
           LHTDRG(IGSP) = .false.
        end if
        6201 continue
     end do
     ILEN = ISPGRP(-IS,92)
     if (LNOTBK(2).and.ARRAY(2).gt.0.) then
        if (LKECHO)write(JOSTND,6215) KEYWRD,KARD(1)(1:ILEN),IS
     else
        if (LKECHO)write(JOSTND,6230) KEYWRD,KARD(1)(1:ILEN),IS
     end if

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq.0) then

     !        IF FIELD 2 IS BLANK OR ZERO SUPPRESS CALIBRATION FOR ALL
     !        SPECIES, OTHERWISE INVOKE CALIBRATION FOR ALL SPECIES.

     if (LNOTBK(2).and.ARRAY(2).gt.0) then
        do J = 1,MAXSP
           LHTDRG(J) = .true.
           6205 continue
        end do
        if (LKECHO)write(JOSTND,6206) KEYWRD
        6206 format(/A8,'   CALIBRATION OF HEIGHT-DIAMETER',&
             &' RELATIONSHIP WILL BE  INVOKED   FOR ALL SPECIES')
     else
        do J = 1,MAXSP
           LHTDRG(J) = .false.
           6210 continue
        end do
        if (LKECHO)write(JOSTND,6220) KEYWRD
        6220 format(/A8,'   CALIBRATION OF HEIGHT-DIAMETER',&
             &' RELATIONSHIP WILL BE SUPPRESSED FOR ALL SPECIES')
     end if

     !     SPECIES CODE IS SPECIFIED (IS>0).

  else

     !        IF FIELD 2 IS BLANK OR ZERO ONLY ONE SPECIES GETS SUPPRESSED;
     !        OTHERWISE ONE SPECIES GETS INVOKED.

     ILEN = 3
     if (LNOTBK(2).and.ARRAY(2).gt.0.) then
        LHTDRG(IS) = .true.
        if (LKECHO)write(JOSTND,6215) KEYWRD,KARD(1)(1:ILEN),IS
        6215 format (/A8,'   CALIBRATION OF HEIGHT-DIAMETER ',&
             &'RELATIONSHIP WILL BE  INVOKED   FOR SPECIES= ',A,&
             &' (CODE= ',I3,')')
     else
        LHTDRG(IS) = .false.
        if (LKECHO)write(JOSTND,6230) KEYWRD,KARD(1)(1:ILEN),IS
        6230 format (/A8,'   CALIBRATION OF HEIGHT-DIAMETER ',&
             &'RELATIONSHIP WILL BE SUPPRESSED FOR SPECIES= ',A,&
             &' (CODE= ',I3,')')
     end if
  end if
  go to 10

  !  ==========  OPTION NUMBER 61: RANNSEED  ==========================RANNSEED

  6300 continue
  if (LNOTBK(1).and.ARRAY(1).eq.0.0) call GETSED (ARRAY(1))
  call RANSED (LNOTBK(1),ARRAY(1))
  if (LKECHO)write(JOSTND,6310) KEYWRD,ARRAY(1)
  6310 format (/A8,'   RANDOM SEED IS:',F14.1)
  go to 10

  !  ==========  OPTION NUMBER 62: HTGMULT  ===========================HTGMULT

  6400 continue
  I = 92
  goto 6005

  !  ==========  OPTION NUMBER 63: CHEAPO  ============================CHEAPO

  6500 continue
  if ( LNOTBK(1) ) JOSUME = IFIX(ARRAY(1))
  LECBUG = LNOTBK(6)
  LECON = .true.
  if (LKECHO)write(JOSTND,1410) KEYWRD,JOSUME
  LECON = .true.
  if (LECBUG.and.LKECHO)write(JOSTND,6510) LECBUG
  6510 format (T12,'ECONOMIC SUMMARY DEBUG REQUESTED.  LECBUG=',L2)
  call ECAVAL
  go to 10

  !  ==========  OPTION NUMBER 64: NUMTRIP  ===========================NUMTRIP

  6600 continue
  if (LNOTBK(1)) ICL4 = IFIX ( ARRAY(1) )
  if (LKECHO)write(JOSTND,6610) KEYWRD,ICL4
  6610 format (/A8,'   NUMBER OF TRIPLES LIMITED TO',I3)
  go to 10

  !  ==========  OPTION NUMBER 65: ENDFILE  ===========================ENDFILE

  6700 continue
  I = JOSUM
  if ( ARRAY(1) .ge.1. ) I = IFIX ( ARRAY(1) )
  end FILE I
  if (LKECHO)write(JOSTND,1410) KEYWRD,I
  go to 10

  !  ==========  OPTION NUMBER 66: BAMAX  =============================BAMAX

  6800 continue
  ARRAY(1) = ARRAY(1)*M2pHAtoFT2pACR
  LMORT = .true.
  if (ARRAY(1).gt.0.0) then
     BAMAX = ARRAY(1)
     LBAMAX = .true.
  end if
  if (LKECHO)write(JOSTND,6810) KEYWRD,BAMAX*FT2pACRtoM2pHA
  6810 format (/A8,'   MAXIMUM BASAL AREA=',F10.2)
  go to 10

  !  ==========  OPTION NUMBER 67: READCORH  ==========================READCORH

  6900 continue
  LHCOR2 = .true.
  if (LKECHO)write(JOSTND,6910) KEYWRD
  6910 format (/A8,'   LARGE TREE HEIGHT GROWTH MODELS ',&
       &'ARE MODIFIED WITH CORRECTION TERMS PRIOR TO CALIBRATION.')
  read (IREAD,5650,end = 80) HCOR2
  I = MAXSP/8
  if (MOD(MAXSP,8).gt.0) I = I+1
  IRECNT = IRECNT + I
  if (LKECHO)write(JOSTND,6920)
  6920 format (T12,'LARGE TREE HEIGHT GROWTH MODEL ',&
       &'CORRECTION TERMS BY SPECIES:')
  if (LKECHO)write(JOSTND,5670) (NSP(I,1),HCOR2(I),I = 1,MAXSP)
  go to 10

  !  ==========  OPTION NUMBER 68: REUSCORH  ==========================REUSCORH

  7000 continue
  LHCOR2 = .true.
  if (LKECHO)write(JOSTND,6910) KEYWRD
  go to 10

  !  ==========  OPTION NUMBER 69: MGMTID    ==========================MGMTID

  7100 continue
  if (LKECHO)write(JOSTND,9999) KEYWRD
  read (IREAD,7105,end = 80) MGMID
  7105 format (A4)
  IRECNT = IRECNT+1
  if (LKECHO)write(JOSTND,7110) MGMID
  7110 format (T12,'MANAGEMENT ID= ',A4)
  go to 10

  !  ==========  OPTION NUMBER 70: REGHMULT  ==========================REGHMULT

  7200 continue
  I = 93
  goto 6005

  !  ==========  OPTION NUMBER 71: TCONDMLT  ==========================TCONDMLT

  7300 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,202,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if
  if (LNOTBK(2).or.LNOTBK(3).or.LNOTBK(4).or.LNOTBK(5).or.&
       &LNOTBK(6)) goto 7320
  call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  7320 continue
  call OPNEW(KODE,IDT,202,5,ARRAY(2))
  if (KODE .gt.0) goto 10
  if (LKECHO)write(JOSTND,7330) KEYWRD,IDT,ARRAY(2),ARRAY(3),&
       &ARRAY(4),ARRAY(5),ARRAY(6)
  7330 format (/A8,'   DATE/CYCLE=',I5,&
       &'; TREE SUBCLASS MULTIPLIER=',F6.1,&
       &'; SPECIAL TREE STATUS MULTIPLIER=',F8.1,&
       &';',/T12,'POINT BASAL AREA MULTIPLIER=',F8.1,&
       &'; POINT CCF MULTIPLIER=',F8.1,&
       &'; POINT TPH MULTIPLIER=',F8.1)
  goto 10

  !  ==========  OPTION NUMBER 72: NOAUTOES  ==========================NOAUTOES

  7400 continue

  !     CALL ESNOAU (ENTRY IN ESIN) TO SET NO AUTOMATIC ESTAB.

  call ESNOAU (KEYWRD,LKECHO)
  goto 10

  !  ==========  OPTION NUMBER 73: READCORR  ==========================READCORR

  7500 continue
  LRCOR2 = .true.
  if (LKECHO)write(JOSTND,7510) KEYWRD
  7510 format (/A8,'   SMALL TREE HEIGHT GROWTH MODELS ',&
       &'ARE MODIFIED WITH CORRECTION TERMS PRIOR TO CALIBRATION.')
  read (IREAD,5650,end = 80) RCOR2
  I = MAXSP/8
  if (MOD(MAXSP,8).gt.0) I = I+1
  IRECNT = IRECNT + I
  if (LKECHO)write(JOSTND,7520)
  7520 format (T12,'SMALL TREE HEIGHT GROWTH MODEL ',&
       &'CORRECTION TERMS BY SPECIES:')
  if (LKECHO)write(JOSTND,5670) (NSP(I,1),RCOR2(I),I = 1,MAXSP)
  go to 10

  !  ==========  OPTION NUMBER 74: REUSCORR  ==========================REUSCORR

  7600 continue
  LRCOR2 = .true.
  if (LKECHO)write(JOSTND,7510) KEYWRD
  go to 10

  !  ==========  OPTION NUMBER 75: BRUST     ==========================BRUST

  7700 continue
  if (LKECHO)write(JOSTND,7710) KEYWRD
  7710 format(/A8,'   BLISTER RUST KEYWORDS:')
  call BRIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  go to 10

  !  ==========  OPTION NUMBER 76: IF        ==========================IF

  7800 continue
  call EVIF (KEYWRD,ARRAY,LNOTBK,IRECNT,IREAD,RECORD,KARD,&
       &JOSTND,DEBUG,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  go to 10

  !  ==========  OPTION NUMBER 77: SCREEN    ==========================SCREEN

  7900 continue
  LSCRN = .true.
  if (LNOTBK(1)) JOSCRN = INT(ARRAY(1))
  if (LKECHO)write(JOSTND,7910) KEYWRD,JOSCRN
  7910 format (/A8,'   SUMMARY TABLE WILL BE PRINTED TO DATA SET ',&
       &'REFERENCE NUMBER ',I5,' AS RUN PROGRESSES.')
  go to 10

  !  ==========  OPTION NUMBER 78: COMPRESS  ==========================COMPRESS

  8000 continue
  if (.not.LNOTBK(2)) ARRAY(2) = MAXTRE/2
  if (.not.LNOTBK(3)) ARRAY(3) = 50.
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  call OPNEW (KODE,IDT,250,2,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,8020) KEYWRD,IDT,(ARRAY(I),I = 2,3)
  8020 format(/A8,'   DATE/CYCLE=',I5,'; TARGET TREE RECORDS=',F5.0,&
       &'; FIND ',F5.0,' PERCENT OF THE NEW RECORDS BY DEFINING'/&
       &T12,'TREE-CLASS BOUNDARIES WHERE THE LARGEST ',&
       &'DIFFERENCES ARE BETWEEN TREES.  DEFINE THE REMAINING'/&
       &T12,'CLASSES BY SPLITTING THOSE WHICH HAVE THE LARGEST ',&
       &'RANGES.')
  go to 10

  !  ==========  OPTION NUMBER 79:  THEN     ==========================THEN

  8100 continue
  call EVTHEN (DEBUG,JOSTND,IREAD,IRECNT,KEYWRD,ARRAY,LNOTBK,&
       &KARD,IPRMPT,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  goto 10

  !  ==========  OPTION NUMBER 80:  ALSOTRY  ==========================ALSOTRY

  8200 continue
  call EVALSO (DEBUG,JOSTND,IREAD,IRECNT,KEYWRD,ARRAY,LNOTBK,&
       &KARD,IPRMPT)
  goto 10

  !  ==========  OPTION NUMBER 81:  ENDIF    ==========================ENDIF

  8300 continue
  IPRMPT = 0
  call EVEND (DEBUG,JOSTND,IRECNT,KEYWRD,ARRAY,LNOTBK,KARD,IPRMPT,&
       &LKECHO)
  goto 10

  !  ==========  OPTION NUMBER 82:  NOTREES  ==========================NOTREES

  8400 continue
  LNOTRE = .true.
  if (LKECHO)write(JOSTND,8410) KEYWRD
  8410 format (/A8,'   NO PROJECTABLE TREE RECORDS EXPECTED.')
  call ESEZCR(JOSTND,LKECHO)
  goto 10

  !  ==========  OPTION NUMBER 83:  CALBSTAT ==========================CALBSTAT

  8500 continue
  JOCALB = JOSUME
  if (LNOTBK(1)) JOCALB = IFIX(ARRAY(1))
  if (LNOTBK(2)) FNMIN = ARRAY(2)
  if (FNMIN.lt.3.)FNMIN = 3.
  if (LNOTBK(3)) NCALHT = IFIX(ARRAY(3))
  if (NCALHT.lt.3) NCALHT = 3
  if (LKECHO)write(JOSTND,8510) KEYWRD,JOCALB,FNMIN,NCALHT
  8510 format (/A8,'   CALIBRATION STATISTICS WILL BE ',&
       &'WRITTEN TO FILE REFERENCED BY NUMBER ',I2/&
       &T12,'MINIMUM NUMBER OF DIA. GROWTH OBSERVATIONS PER ',&
       &'SPECIES TO CALIBRATE '/&
       &T12,'THE LARGE TREE DIAMETER INCREMENT ',&
       &'MODEL FOR LOCAL CONDITIONS= ',F6.0/&
       &T12,'MINIMUM NUMBER OF HEIGHT GROWTH OBSERVATIONS PER ',&
       &'SPECIES TO CALIBRATE '/&
       &T12,'THE SMALL TREE HEIGHT INCREMENT ',&
       &'MODEL FOR LOCAL CONDITIONS= ',I6)
  goto 10

  !  ==========  OPTION NUMBER 84: OPEN      ==========================OPEN

  8600 continue
  call KEYOPN (IREAD,RECORD,JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  goto 10

  !  ==========  OPTION NUMBER 85: CLOSE     ==========================CLOSE

  8700 continue
  if (ARRAY(1).gt.0.0) goto 8710
  call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  call ERRGRO (.true.,4)
  goto 10
  8710 continue
  I = INT(ARRAY(1))
  if (LKECHO)write(JOSTND,8720) KEYWRD,I
  8720 format (/A8,'   DATA SET REFERENCE NUMBER = ',I5)
  close (UNIT = I)
  goto 10

  !  ==========  OPTION NUMBER 86: NOSCREEN  ==========================NOSCREEN

  8800 continue
  LSCRN = .false.
  if (LKECHO)write(JOSTND,8810) KEYWRD
  8810 format (/A8,'   SCREEN OUTPUT DEACTIVATED.')
  goto 10

  !  ==========  OPTION NUMBER 87: RRIN      ==========================RRIN

  8900 continue
  if (LKECHO)write(JOSTND,8910) KEYWRD
  8910 format (/A8,'   ROOT DISEASE MODEL IS OBSOLETE, USE',/,&
       &' WESTERN ROOT DISEASE MODEL VER. 3.0:')
  goto 10

  !  ==========  OPTION NUMBER 88:  FIXMORT  ==========================FIXMORT

  9000 continue
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoIN
  LMORT = .true.
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,97,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else

     !        SPECIES CODE PROCESSING.

     call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)
     if (IS.eq.-999) goto 10
     if (.not.LNOTBK(3))ARRAY(3) = 0.
     if (.not.LNOTBK(4))ARRAY(4) = 0.
     if (.not.LNOTBK(5))ARRAY(5) = 999.
     if ((ARRAY(3) .gt.1) .and. (ARRAY(6) .lt.3)) ARRAY(3) = 1.0
     if ((ARRAY(3) .lt.0) .and. (ARRAY(6) .lt.3)) ARRAY(3) = 0.0
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,9030) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
          &ARRAY(3),&
          &ARRAY(4)*INtoCM,&
          &ARRAY(5)*INtoCM,&
          &ARRAY(6)
     9030 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES=',A,' (CODE=',&
          &I3,'); MORTALITY RATE=',F10.4/&
          &T12,'ONLY TREES GREATER THAN ',F7.2,' AND LESS THAN ',&
          &F7.2,' CM DBH ARE AFFECTED.'/T12,'TYPE CODE = ',&
          &F3.0,'; (0=REPLACE MODEL PREDICTION, 1=ADD TO MODEL',&
          &' PREDICTION, 2=USE MAXIMUM RATE, 3=MULTIPLY MODEL BY).')
     if (LNOTBK(7).and.ARRAY(7).gt.0.) then
        if (ARRAY(7) .eq.1) then
           if (LKECHO)write(JOSTND,9032)ARRAY(7)
           9032 format(T12,'FIXMORT MORTALITY CONCENTRATION CODE = ',F4.0,&
                &'; (BY POINT)')
        else if (ARRAY(7).eq.10) then
           if (LKECHO)write(JOSTND,9033)ARRAY(7)
           9033 format(T12,'FIXMORT MORTALITY CONCENTRATION CODE = ',F4.0,&
                &'; (BY SIZE: SMALLEST TO LARGEST DBH)')
        else if (ARRAY(7).eq.11) then
           if (LKECHO)write(JOSTND,9034)ARRAY(7)
           9034 format(T12,'FIXMORT MORTALITY CONCENTRATION CODE = ',F4.0,&
                &'; (BY POINT BY SIZE: SMALLEST TO LARGEST DBH)')
        else if (ARRAY(7).eq.20) then
           if (LKECHO)write(JOSTND,9035)ARRAY(7)
           9035 format(T12,'FIXMORT MORTALITY CONCENTRATION CODE = ',F4.0,&
                &'; (BY SIZE: LARGEST TO SMALLEST DBH)')
        else if (ARRAY(7).eq.21) then
           if (LKECHO)write(JOSTND,9036)ARRAY(7)
           9036 format(T12,'FIXMORT MORTALITY CONCENTRATION CODE = ',F4.0,&
                &'; (BY POINT BY SIZE: LARGEST TO SMALLEST DBH)')
        else
           write(JOSTND,9037)ARRAY(7)
           9037 format(T12,'FIXMORT MORTALITY CONCENTRATION CODE = ',F4.0,&
                &'; (CODE NOT RECOGNIZED, IT WILL BE IGNORED)')
        end if
     end if
     call OPNEW (KODE,IDT,97,6,ARRAY(2))
     if (KODE.gt.0) goto 10
  end if
  goto 10

  !  ==========  OPTION NUMBER 89: SDIMAX    ==========================SDIMAX

  9100 continue
  ARRAY(2) = ARRAY(2)/HAtoACR

  !   SPECIES CODE PROCESSING.

  call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  LMORT = .true.
  if (LKECHO)write(JOSTND,9101) KEYWRD
  9101 format(/A8)

  !     PROCESS MAX SDI IF NON-ZERO VALUE ENTERED.

  if (ARRAY(2) .gt.0.0) then

     !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

     if (IS .lt.0) then
        IGRP = -IS
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           SDIDEF(IGSP) = ARRAY(2)
           MAXSDI(IGSP) = 1
           9102 continue
        end do
        ILEN = ISPGRP(-IS,92)
        if (LKECHO)write(JOSTND,9108) KARD(1)(1:ILEN),IS,&
             &ARRAY(2)/ACRtoHA

        !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

     else if (IS .eq.0) then
        do J = 1,MAXSP
           SDIDEF(J) = ARRAY(2)
           MAXSDI(J) = 1
           9103 continue
        end do
        if (LKECHO)write(JOSTND,9104) ARRAY(2)/ACRtoHA
        9104 format(/8X,'   MAXIMUM PERMISSABLE STAND DENSITY INDEX',&
             &' WILL BE CHANGED FOR ALL SPECIES ; MAX SDI=',F6.0)

        !     SPECIES CODE IS SPECIFIED (IS>0).

     else
        SDIDEF(IS) = ARRAY(2)
        MAXSDI(IS) = 1
        ILEN = 3
        if (LKECHO)write(JOSTND,9108) KARD(1)(1:ILEN),IS,&
             &ARRAY(2)/ACRtoHA
        9108 format (/8X,'   MAXIMUM PERMISSABLE STAND DENSITY INDEX',&
             &' WILL BE CHANGED FOR SPECIES= ',A,' (CODE= ',I3,&
             &'); MAX SDI=',F6.0)
     end if
  end if

  !   PROCESS OTHER PARAMETER FIELDS. FIELDS 3 & 4 NO LONGER USED.

  if (LNOTBK(5)) PMSDIL = ARRAY(5)
  if (PMSDIL .lt.10.) PMSDIL = 10.
  if (LNOTBK(6)) PMSDIU = ARRAY(6)
  if (PMSDIU .gt.95.) PMSDIU = 95.

  if (LKECHO)write(JOSTND,9109) PMSDIL,PMSDIU
  9109 format (T12,&
       &'PERCENT OF MAX SDI TO',&
       &' INVOKE DENSITY RELATED MORTALITY=',F5.1,'; PERCENT OF MAX',&
       &' SDI WHERE STAND REACHES MAX DENSITY=',F5.1)

  !   PROCESS STAGNATION INDICATOR (CENTRAL ROCKIES VARIANT)

  select case (VARACD)

  case ('CR','TT','UT')

     if (LNOTBK(7)) then
        if (IS .lt.0) then
           IGRP = -IS
           IULIM = ISPGRP(IGRP,1)+1
           do IG = 2,IULIM
              IGSP = ISPGRP(IGRP,IG)
              ISTAGF(IGSP) = INT(ARRAY(7))
              9112 continue
           end do
           ILEN = ISPGRP(-IS,92)
           if (LKECHO)write(JOSTND,9117) KARD(1)(1:ILEN),IS,ARRAY(7)
        else if (IS .eq.0) then
           do J = 1,MAXSP
              ISTAGF(J) = INT(ARRAY(7))
              9115 continue
           end do
           if (LKECHO)write(JOSTND,9116) ARRAY(7)
           9116 format(8X,'   STAGNATION EFFECTS INDICATOR',&
                &' WILL BE CHANGED FOR ALL SPECIES ; INDICATOR =',F4.0)
        else

           !   ONLY ONE SPECIES GETS CHANGED

           ISTAGF(IS) = INT(ARRAY(7))
           ILEN = 3
           if (LKECHO)write(JOSTND,9117) KARD(1)(1:ILEN),IS,ARRAY(7)
           9117 format (8X,'   STAGNATION EFFECTS INDICATOR',&
                &' WILL BE CHANGED FOR SPECIES= ',A,' (CODE= ',I3,&
                &'); INDICATOR =',F4.0)
        end if
     end if

  case default
     if (LNOTBK(7)) then
        if (LKECHO)write(JOSTND,9120)
        9120 format(8X,'   STAGNATION EFFECTS NOT USED IN THIS VARIANT.')
     end if

  end select
  goto 10

  !  ==========  OPTION NUMBER 90: DELOTAB   ==========================DELOTAB

  9200 continue
  JJTAB = 0
  if (LNOTBK(1)) JJTAB = IFIX(ARRAY(1))

  if (JJTAB .eq.0) then
     do I = 1,5
        ITABLE(I) = 1
        9210 continue
     end do
     if (LKECHO)write(JOSTND,9215)KEYWRD
     9215 format(/A8,'   NO TABLE SPECIFIED, ALL OPTIONAL TABLES',&
          &' WILL BE DELETED.')
     go to 10
  end if

  if (JJTAB .gt.4) then
     if (LKECHO)write(JOSTND,9220) KEYWRD,JJTAB
     9220 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. THIS IS',&
          &' AN INVALID TABLE NUMBER. KEYWORD IGNORED.')
     go to 10
  end if

  ITABLE(JJTAB) = 1
  if ((JJTAB .eq.1).and.LKECHO)write(JOSTND,9226)KEYWRD,JJTAB
  9226 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
       &' STAND COMPOSITION TABLE WILL BE DELETED.')
  if ((JJTAB .eq.2).and.LKECHO)write(JOSTND,9225)KEYWRD,JJTAB
  9225 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
       &' SAMPLE TREE TABLE WILL BE DELETED.')
  if ((JJTAB .eq.3).and.LKECHO)write(JOSTND,9227)KEYWRD,JJTAB
  9227 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
       &' SUMMARY OUTPUT TABLE WILL BE DELETED.')
  if ((JJTAB .eq.4).and.LKECHO)write(JOSTND,9228)KEYWRD,JJTAB
  9228 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
       &' ACTIVITY SCHEDULE TABLE WILL BE DELETED.')
  if ((JJTAB .eq.5).and.LKECHO)write(JOSTND,9229)KEYWRD,JJTAB
  9229 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
       &' THIS IS NOT FUNCTIONAL AT THIS TIME.')
  go to 10

  !  ==========  OPTION NUMBER 91: SERLCORR  ==========================SERLCORR

  9300 continue
  if (LNOTBK(1)) BJPHI = ARRAY(1)
  if (LNOTBK(2)) BJTHET = ARRAY(2)
  if (LKECHO)write(JOSTND,9310) KEYWRD,BJPHI,BJTHET
  9310 format (/A8,'   ARMA(1,1) SERIAL CORRELATION PARAMETERS:',&
       &' PHI=',F5.2,'; THETA=',F5.2)
  goto 10

  !  ==========  OPTION NUMBER 92: CUTLIST   ==========================CUTLIST

  9400 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  NP = 2
  if (LNOTBK(4)) NP = 3
  if (LNOTBK(6)) NP = 5
  if (.not.LNOTBK(2)) ARRAY(2) = FLOAT(JOLIST)
  call OPNEW (KODE,IDT,199,NP,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,9410) KEYWRD,IDT,ARRAY(2),ARRAY(3)
  9410 format (/A8,'   DATE/CYCLE=',I5,'; DATA SET REFERENCE ',&
       &'NUMBER =',F4.0,'; HEADING SUPPRESSION CODE =',F3.0/&
       &T12,'(0=WITH HEADING, OTHER VALUES=SUPPRESS HEADING).')
  if ((NP.eq.3).and.(ARRAY(4).eq.2).and.LKECHO)write(JOSTND,9411)
  9411 format(T12,'CYCLE ONE CUTLIST SUPRESSED')
  if ((NP.eq.5).and.(ARRAY(6).gt.0).and.LKECHO)write(JOSTND,9412)
  9412 format(T12,'CUTLIST WILL BE IN OLD FORMAT')
  goto 10

  !  ==========  OPTION NUMBER 93: RESETAGE  ==========================RESETAGE

  9500 continue
  IDT = 1

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,443,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  call OPNEW (KODE,IDT,443,1,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,9510) KEYWRD,IDT,ARRAY(2)
  9510 format (/A8,'   DATE/CYCLE=',I5,'; NEW AGE=',F6.0)
  goto 10

  !  ==========  OPTION NUMBER 94: SITECODE  ==========================SITECODE

  9600 continue

  !   SITE SPECIES IS OBTAINED FROM THE FIRST SITECODE CARD WHICH
  !   CONTAINS A VALUE IN FIELD 1 BETWEEN 1 AND MAXSP, OR CAN BE
  !   SPECIFIED USING FIELD 3.  IF NONE OF THE SITECODE KEYWORDS HAVE A
  !   SITE SPECIES SPECIFIED, THE DEFAULT IN **SITSET** WILL BE USED.
  !   MAKE SURE THAT SITE SPECIES IS SET BEFORE LSITE IS SET TO TRUE.

  !   SPECIES CODE PROCESSING.

  ARRAY(2) = ARRAY(2) * MtoFT

  call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  IPSI = 1

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP
  !     IF SITE SPECIES HAS NOT BEEN SET, SET IT TO THE FIRST SPECIES
  !     IN THE GROUP.

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     if (ISISP .eq.0) ISISP = ISPGRP(IGRP,2)
     LSITE = .true.
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        if (LNOTBK(2) .and. ARRAY(2) .gt.5.) SITEAR(IGSP) = ARRAY(2)
        9605 continue
     end do
     ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,9610) KEYWRD,KARD(1)(1:ILEN),IS,&
          &ARRAY(2)*FTtoM

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq.0) then
     if (LNOTBK(2)) then
        if (ARRAY(2) .le.5.) go to 9617
        do K = 1,MAXSP
           SITEAR(K) = ARRAY(2)
           9616 continue
        end do
        9617 continue
        if (LKECHO)write(JOSTND,9618) KEYWRD,ARRAY(2)*FTtoM
        9618 format(/A8,'   SITE CODE CHANGED FOR ALL SPECIES; ',&
             &' SITE CODE FOR LARGE AND SMALL TREES=',F8.1)
     else
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,4)
        goto 10
     end if

     !     SPECIES CODE IS SPECIFIED (IS>0).

  else
     if (ISISP .eq.0) ISISP = IS
     LSITE = .true.

     !   LOAD SITE ARRAY ONLY IF DUNNING CODES NOT BEING USED IE VALUES
     !   GREATER THAN 7. THIS IS NECESSARY SO CALLS TO DUNN WILL SET ALL
     !   SITE INDEX VALUES PROPERLY.

     if (LNOTBK(2) .and. ARRAY(2) .gt.7.) SITEAR(IS) = ARRAY(2)

     ILEN = 3
     if (LKECHO)write(JOSTND,9610) KEYWRD,KARD(1)(1:ILEN),IS,&
          &ARRAY(2)*FTtoM
     9610 format(/A8,'   SITE INDEX SPECIES= ',A,' (CODE= ',I3,&
          &');  SITE CODE FOR LARGE AND SMALL TREES=',F8.1)

     !   FIELD 3 INDICATES USE THIS SPECIES FOR SITE SPECIES. LSITE SET ABOVE

     if (LNOTBK(3).and.ARRAY(3).gt.0) then
        ISISP = IS
        if (LKECHO)write(JOSTND,9611)
        9611 format(8X,'   SITE INDEX SPECIES WILL BE USED AS THE SITE',&
             &' SPECIES.')
     end if
  end if

  !     CALL DUNN TO PROCESS ANY DUNNING SITE CODE INFORMATION.

  if (LNOTBK(2) .and. ARRAY(2) .le.7) then
     select case (VARACD)
     case('CA','NC','SO','WS','OC')
        if (LKECHO)write(JOSTND,9620)
        9620 format(8X,'   FIELD 2 OF THE SITECODE KEWORD IS LESS',&
             &' THAN 8 AND WILL BE INTERPRETED AS A DUNNING CODE.')
     end select
     call DUNN(ARRAY(2))
  end if

  go to 10

  !  ==========  OPTION NUMBER 95:  MISTOE    =========================MISTOE

  9700 continue
  if (LKECHO)write(JOSTND,9710) KEYWRD
  9710 format(/A8,'   MISTLETOE KEYWORDS:')
  call MISIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  goto 10

  !  ==========  OPTION NUMBER 96:  CRNMULT ===========================CRNMULT

  9800 continue
  ARRAY(4) = ARRAY(4)*CMtoIN
  ARRAY(5) = ARRAY(5)*CMtoIN
  IDT = 1
  if (LNOTBK(1))IDT = IFIX(ARRAY(1))
  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,81,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     go to 10
  end if

  !    SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  call OPNEW (KODE,IDT,81,5,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (ARRAY(5) .le.0.0) ARRAY(5) = 99.0
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,9854)KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
       &ARRAY(3),&
       &ARRAY(4)*INtoCM,&
       &ARRAY(5)*INtoCM, ARRAY(6)
  9854 format(/A8,'   DATE/CYCLE=',I5,';  SPECIES=',A,' (CODE= ',I3,&
       &');  MULTIPLIER=',F7.2,';  LOWER DBH=',F6.1,';  UPPER DBH=',F6.1,&
       &';  DUB FLAG=',F6.0)
  goto 10

  !  ==========  OPTION NUMBER 97   CFVOLEQU  =========================CFVOLEQU

  9900 continue

  !     READ COEFFICIENTS FROM 2 SUPPLEMENTAL RECORDS.

  read(IREAD,9901,end = 80) (WK6(I),I = 1,14)
  9901 format(7F10.0/7F10.0)
  IRECNT = IRECNT+2
  LCVOLS = .true.

  !   SPECIES CODE PROCESSING.

  call SPDECD (1,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPC.eq.-999) go to 10
  ITC = IFIX(ARRAY(2))
  TC = ARRAY(3)

  !     IF (ISPC<0) CHANGE ALL SPECIES IN THE GROUP

  if (ISPC .lt.0) then
     IGRP = -ISPC
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        ICTRAN(IGSP) = ITC
        CTRAN(IGSP) = TC
        do K = 1,7
           J = K+7
           CFVEQS(K,IGSP) = WK6(K)
           CFVEQL(K,IGSP) = WK6(J)
           9904 continue
        end do
        9905 continue
     end do
     ILEN = ISPGRP(-ISPC,92)
     if (LKECHO)write(JOSTND,9968) KEYWRD,KARD(1)(1:ILEN),ISPC

     !     IF (ISPC=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (ISPC .eq.0) then
     do I = 1,MAXSP
        ICTRAN(I) = ITC
        CTRAN(I) = TC
        do K = 1,7
           J = K+7
           CFVEQS(K,I) = WK6(K)
           CFVEQL(K,I) = WK6(J)
           9906 continue
        end do
        9907 continue
     end do
     if (LKECHO)write(JOSTND,9908) KEYWRD
     9908 format(/A8,T12,'DEFAULT COEFFICIENTS FOR CUBIC METER VOLUME'/&
          &T12,'EQUATIONS REPLACED FOR ALL SPECIES.')

     !     SPECIES CODE IS SPECIFIED (ISPC>0).

  else
     ICTRAN(ISPC) = ITC
     CTRAN(ISPC) = TC
     do K = 1,7
        J = K+7
        CFVEQS(K,ISPC) = WK6(K)
        CFVEQL(K,ISPC) = WK6(J)
        9915 continue
     end do
     ILEN = 3
     if (LKECHO)write(JOSTND,9918) KEYWRD,KARD(1)(1:ILEN),ISPC
     9918 format(/A8,T12,'DEFAULT COEFFICIENTS FOR CUBIC METER VOLUME'/&
          &T12,'EQUATIONS REPLACED FOR SPECIES= ',A,' (CODE= ',I3,').' )
  end if

  if (LKECHO)write(JOSTND,9925) ITC,TC,(WK6(I),I = 1,14)
  9925 format(T12,'TRANSITION SIZE CODE = ',I4,';   TRANSITION SIZE = ',&
       &F10.2/T12,'COEFFICIENTS FOR TREE THAT IS SMALLER THAN',&
       &' TRANSITION SIZE:'/T12,7F12.5/&
       &T12,'COEFFICIENTS FOR TREE THAT IS LARGER THAN',&
       &' TRANSITION SIZE:'/T12,7F12.5)
  go to 10

  !  ==========  OPTION NUMBER 98: BFVOLEQU   =========================BFVOLEQU

  9950 continue

  !     READ COEFFICIENTS FROM 2 SUPPLEMENTAL RECORDS.

  read(IREAD,9901,end = 80) (WK6(I),I = 1,14)
  IRECNT = IRECNT+2
  LBVOLS = .true.

  !   SPECIES CODE PROCESSING.

  call SPDECD (1,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPC.eq.-999) go to 10
  ITB = IFIX(ARRAY(2))
  TB = ARRAY(3)

  !     IF (ISPC<0) CHANGE ALL SPECIES IN THE GROUP

  if (ISPC .lt.0) then
     IGRP = -ISPC
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        IBTRAN(IGSP) = ITB
        BTRAN(IGSP) = TB
        do K = 1,7
           J = K+7
           BFVEQS(K,IGSP) = WK6(K)
           BFVEQL(K,IGSP) = WK6(J)
           9954 continue
        end do
        9955 continue
     end do
     ILEN = ISPGRP(-ISPC,92)
     if (LKECHO)write(JOSTND,9968) KEYWRD,KARD(1)(1:ILEN),ISPC

     !     IF (ISPC=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (ISPC .eq.0) then
     do I = 1,MAXSP
        IBTRAN(I) = ITB
        BTRAN(I) = TB
        do K = 1,7
           J = K+7
           BFVEQS(K,I) = WK6(K)
           BFVEQL(K,I) = WK6(J)
           9956 continue
        end do
        9957 continue
     end do
     if (LKECHO)write(JOSTND,9958) KEYWRD
     9958 format(/A8,T12,'DEFAULT METRIC COEFFICIENTS FOR BOARD ',&
          &'VOLUME'/&
          &T12,'EQUATIONS REPLACED FOR ALL SPECIES.')

     !     SPECIES CODE IS SPECIFIED (ISPC>0).

  else
     IBTRAN(ISPC) = ITB
     BTRAN(ISPC) = TB
     do K = 1,7
        J = K+7
        BFVEQS(K,ISPC) = WK6(K)
        BFVEQL(K,ISPC) = WK6(J)
        9965 continue
     end do
     ILEN = 3
     if (LKECHO)write(JOSTND,9968) KEYWRD,KARD(1)(1:ILEN),ISPC
     9968 format(/A8,T12,'DEFAULT METRIC COEFFICIENTS FOR BOARD ',&
          &'VOLUME'/&
          &T12,'EQUATIONS REPLACED FOR SPECIES= ',A,' (CODE= ',I3,').' )
  end if

  if (LKECHO)write(JOSTND,9972) ITB,TB,(WK6(I),I = 1,14)
  9972 format(T12,'TRANSITION SIZE CODE = ',I4,';   TRANSITION SIZE = ',&
       &F10.2/T12,'COEFFICIENTS FOR TREE THAT IS SMALLER THAN',&
       &' TRANSITION SIZE:'/T12,7F12.5/&
       &T12,'COEFFICIENTS FOR TREE THAT IS LARGER THAN',&
       &' TRANSITION SIZE:'/T12,7F12.5)
  go to 10

  !  ==========  OPTION NUMBER 99: ANIN      ==========================ANIN

  9975 continue
  if (LKECHO)write(JOSTND,9977) KEYWRD
  9977 format (/A8,'   ANNOSUS ROOT DISEASE MODEL IS OBSOLETE, USE',/&
       &' WESTERN ROOT DISEASE MODEL VER. 3.0:')
  goto 10

  !  ==========  OPTION NUMBER 100: DFB      ==========================DFB

  9985 continue
  if (LKECHO)write(JOSTND,9987) KEYWRD
  9987 format (/A8,'   DOUGLAS-FIR BEETLE OPTIONS:')
  call DFBIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  goto 10

  !  ==========  OPTION NUMBER 101: RDIN     ==========================RDIN

  9995 continue
  if (LKECHO)write(JOSTND,9997) KEYWRD
  9997 format (/A8,'   WESTERN ROOT DISEASE MODEL OPTIONS:')
  call RDIN (KEYWRD,ARRAY,LNOTBK,LKECHO)
  goto 10

  !  ==========  OPTION NUMBER 102: MANAGED  ==========================MANAGED

  10000 continue
  IDT = 0
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (IDT .gt.0) then
     call OPNEW (KODE,IDT,82,1,ARRAY(2))
     if (KODE.gt.0) go to 10
     if (LKECHO)write(JOSTND,10010) KEYWRD,IDT,ARRAY(2)
     10010 format(/A8,'   DATE/CYCLE=',I5,'; MANAGED = ',F5.0)
  else
     if (LNOTBK(2) .and. ARRAY(2).eq.0.) then
        if (LKECHO)write(JOSTND,10015) KEYWRD,IDT
        10015 format(/A8,'   DATE/CYCLE=',I5,'; MANAGED = 0 (NO)')
     else
        if (LKECHO)write(JOSTND,10020) KEYWRD,IDT
        10020 format(/A8,'   DATE/CYCLE=',I5,'; MANAGED = 1 (YES)')
        ARRAY(2) = 1.
     end if
  end if
  if (IDT .le.0) MANAGD = IFIX(ARRAY(2))
  go to 10

  !  ==========  OPTION NUMBER 103: YARDLOSS  =========================YARDLOSS

  10100 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,203,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  !     PROPORTION OF HARVEST NOT REMOVED FROM STAND

  X = 0.
  if (LNOTBK(2)) then
     X = AMAX1(0.,ARRAY(2))
     X = AMIN1(1.,X)
  end if
  ARRAY(2) = X

  !     PROPORTION OF NON-REMOVED HARVEST THAT IS DOWN

  X = 0.
  if (LNOTBK(3)) then
     X = AMAX1(0.,ARRAY(3))
     X = AMIN1(1.,X)
  end if
  ARRAY(3) = X

  !     PROPORTION OF HARVESTED CROWNS LEFT IN STAND

  X = 1.
  if (LNOTBK(4)) then
     X = AMAX1(0.,ARRAY(4))
     X = AMIN1(1.,X)
  end if
  ARRAY(4) = X

  call OPNEW(KODE,IDT,203,3,ARRAY(2))
  if (KODE .gt.0) goto 10
  if (LKECHO)write(JOSTND,10110) KEYWRD,IDT,ARRAY(2),ARRAY(3),&
       &ARRAY(4)
  10110 format (/A8,'   DATE/CYCLE=',I5,&
       &'; PROPORTION OF HARVESTED STEMS LEFT IN STAND = ',F6.4/T12,&
       &'PROPORTION OF NON-REMOVED HARVEST THAT IS DOWN = ',F6.4/T12,&
       &'PROPORTION OF CROWNS REMAINING IN STAND FROM ',&
       &'REMOVED STEMS =',F6.4)
  goto 10

  !  ==========  OPTION NUMBER 104: FMIN      =========================FMIN

  10200 continue
  if (LKECHO)write(JOSTND,10210) KEYWRD
  10210 format (/A8,'   FIRE MODEL KEYWORDS:')
  call FMIN (1,NSP,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  goto 10

  !  ==========  OPTION NUMBER 105: STRCLASS  =========================STRCLASS

  10300 continue
  call KSSTAG (JOSTND,KEYWRD,LNOTBK,ARRAY,LKECHO)
  goto 10

  !  ==========  OPTION NUMBER 106: MODTYPE   =========================MODTYPE

  10400 continue

  !  VALUES ARE STORED AND THEN DIGESTED IN SITSET.
  !  CENTRAL ROCKIES AND ORGANON VARIANTS, USE MODEL TYPE (FIELD 1).
  !  FIELD 2 IS NOT USED.
  !  ALL VARIANTS HAVE FOREST TYPE (FIELD 3) AVAILABLE, BUT MAY HAVE NO EFFECT.

  select case (VARACD)

  case ('CR')
     if (LNOTBK(1)) IMODTY = IFIX(ARRAY(1))
     if (LKECHO)write(JOSTND,10430) KEYWRD,IMODTY
     10430 format(/A8,'   MODEL TYPE =',I5)
     if (LKECHO)write(JOSTND,10450)
     10450 format(T12,'(SEE MODTYPE KEYWORD DISCUSSION FOLLOWING',&
          &' --- PROCESS --- KEYWORD.)')

  case default
     continue
  end select

  if (LNOTBK(3)) IFORTP = IFIX(ARRAY(3))
  if (IFORTP .gt.999) then

     !  THE LAST 3 CHARACTERS INDICATE THE FOREST TYPE AND THE FIRST
     !  CHARACTER INDICATES THAT THE USER SET THE FOREST TYPE TO BE
     !  CONSTANT FOR ALL CYCLES.  THE FIELD 3 INPUT IS DECODED
     !  AND LFLAGV IS SET TO TRUE TO INDICATE CONSTANT FOREST TYPE

     XTMP = FLOAT(IFORTP)
     XTMP = XTMP/1000. + 0.00001
     IXTMP = INT(XTMP)
     IFORTP = INT((XTMP-real(IXTMP))*1000.)
     LFLAGV = .true.
  end if

  !  CALL FORTYP TO CHECK FOR VALID USER INPUT OF IFORTP VALUE

  DUM1 = 0.
  IXF = 1
  call FORTYP(IXF,DUM1)

  if (LKECHO)write(JOSTND,10455) KEYWRD,IFORTP
  10455 format(/A8,T12,'FOREST TYPE =',I5)

  go to 10

  !  ==========  OPTION NUMBER 107: FVSSTAND  =========================FVSSTAND

  10500 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  NP = 1
  if (LNOTBK(2)) NP = 2
  call OPNEW (KODE,IDT,204,NP,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (NP.eq.2.and. ARRAY(2).eq.1) then
     if (LKECHO)write(JOSTND,10510) KEYWRD,IDT
     10510 format (/A8,'   DATE/CYCLE=',I5,&
          &'; CYCLE 0 FVSSTAND SUPPRESSED; CYCLE 1 FVSSTAND DONE')
  else if (NP.eq.2.and. ARRAY(2).eq.2) then
     if (LKECHO)write(JOSTND,10515) KEYWRD,IDT
     10515 format (/A8,'   DATE/CYCLE=',I5,&
          &'; CYCLE 0 FVSSTAND DONE; CYCLE 1 FVSSTAND SUPPRESSED')
  else
     if (LKECHO)write(JOSTND,10520) KEYWRD,IDT
     10520 format (/A8,'   DATE/CYCLE=',I5)
  end if
  !     NOTRIP = .TRUE.
  !     IF(LKECHO)WRITE(JOSTND,10530)
  !0530 FORMAT(11X,'** RECORD TRIPLING TURNED OFF **')
  goto 10

  !  ==========  OPTION NUMBER 108: PRUNE   ===========================PRUNE

  10600 continue
  IDT = 1
  if (LNOTBK(1))IDT = IFIX(ARRAY(1))
  if (ARRAY(2) .le.0.) ARRAY(2) = 2.
  if (ARRAY(4) .le.0.) ARRAY(4) = 0.5
  if (ARRAY(4) .gt.1.) ARRAY(4) = 1.
  if (ARRAY(7) .le.0.) ARRAY(7) = 99.0
  ARRAY(3) = ARRAY(3) * MtoFT
  ARRAY(6) = ARRAY(6) * CMtoIN
  ARRAY(7) = ARRAY(7) * CMtoIN
  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,249,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     go to 10
  end if

  !   SPECIES CODE PROCESSING.

  call SPDECD (5,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  call OPNEW (KODE,IDT,249,6,ARRAY(2))
  if (KODE.gt.0) go to 10
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,10605)KEYWRD,IDT,(ARRAY(J),J = 2,4),&
       &KARD(5)(1:ILEN),IS,(ARRAY(J),J = 6,7)
  10605 format(/A8,'   DATE/CYCLE=',I5,';  METHOD=',F5.0,&
       &';  M=',F7.2,';  MAX CR REMOVAL PROPORTION=',F6.1,&
       &';'/T12,'SPECIES=',A,' (CODE= ',I3,&
       &');  LOWER DBH=',F6.1,';  UPPER DBH=',F6.1)
  goto 10

  !  ==========  OPTION NUMBER 109: SVS   =============================SVS

  10900 continue
  call SVKEY (KEYWRD,LNOTBK,ARRAY)
  goto 10

  !  ==========  OPTION NUMBER 110:  FIXDG   ==========================FIXDG

  11000 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  ARRAY(4) = ARRAY(4) * CMtoIN
  ARRAY(5) = ARRAY(5) * CMtoIN
  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,98,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else

     !        SPECIES CODE PROCESSING.

     call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)
     if (IS.eq.-999) goto 10
     if (.not.LNOTBK(3))ARRAY(3) = 1.
     if (.not.LNOTBK(4))ARRAY(4) = 0.
     if (.not.LNOTBK(5))ARRAY(5) = 999.
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,11030) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
          &ARRAY(3),&
          &ARRAY(4)*INtoCM,&
          &ARRAY(5)*INtoCM
     11030 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES=',A,' (CODE=',&
          &I3,'); DIAMETER GROWTH MULTIPLIER=',F10.4/&
          &T12,'ONLY TREES GREATER THAN OR EQUAL TO ',F7.2,&
          &' AND LESS THAN ',&
          &F7.2,' CM DBH ARE AFFECTED.')

     call OPNEW (KODE,IDT,98,4,ARRAY(2))
     if (KODE.gt.0) goto 10
  end if
  goto 10

  !  ==========  OPTION NUMBER 111:  FIXHTG   ==========================FIXHTG

  11100 continue
  IDT = 1
  ARRAY(4) = ARRAY(4) * CMtoIN
  ARRAY(5) = ARRAY(5) * CMtoIN
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,99,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else

     !        SPECIES CODE PROCESSING.

     call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)
     if (IS.eq.-999) goto 10
     if (.not.LNOTBK(3))ARRAY(3) = 1.
     if (.not.LNOTBK(4))ARRAY(4) = 0.
     if (.not.LNOTBK(5))ARRAY(5) = 999.
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,11130) KEYWRD,IDT,KARD(2)(1:ILEN),IS,&
          &ARRAY(3),&
          &ARRAY(4)*INtoCM,&
          &ARRAY(5)*INtoCM
     11130 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES=',A,' (CODE=',&
          &I3,'); HEIGHT GROWTH MULTIPLIER=',F10.4/&
          &T12,'ONLY TREES GREATER THAN OR EQUAL TO ',F7.2,&
          &' AND LESS THAN ',&
          &F7.2,' CM DBH ARE AFFECTED.')
     call OPNEW (KODE,IDT,99,4,ARRAY(2))
     if (KODE.gt.0) goto 10
  end if
  goto 10

  !  ==========  OPTION NUMBER 112: THINSDI  ==========================THINSDI

  11200 continue
  ICFLAG = 230
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.le.0) goto 11210
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  11210 continue
  if (LNOTBK(2)) then
     PRMS(1) = ARRAY(2)
  else

     !        REQUIRED RESIDUAL IS MISSING.

     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !   PROCESS REMAINING PARAMETER FIELDS

  PRMS(2) = EFF
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)

  !   SPECIES CODE PROCESSING.

  call SPDECD (4,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  PRMS(3) = IS
  PRMS(4) = 0.0
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  PRMS(5) = 999.0
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)
  if (PRMS(5) .le. PRMS(4)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if
  PRMS(4) = PRMS(4) * CMtoIN
  PRMS(5) = PRMS(5) * CMtoIN
  PRMS(6) = 0.
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)
  if (PRMS(6).eq.0..and. PRMS(2).gt.0.)PRMS(2) = 0.
  call OPNEW (KODE,IDT,ICFLAG,6,PRMS)
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,11220) KEYWRD,IDT,PRMS(1)
  11220 format (A8,'   DATE/CYCLE=',I5,'; RESIDUAL=',F8.2)
  if (PRMS(2).eq.0.) then
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,11221)KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*INtoCM
     11221 format(T12,&
          &'PROPORTION OF SELECTED TREES REMOVED WILL BE ',&
          &'COMPUTED BY THE MODEL'/,&
          &T12,'SPECIES=',A,' (CODE=',I3,');',&
          &' DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
          &F5.1,' CM.')
  else
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,11222)PRMS(2),KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*INtoCM
     11222 format(T12,&
          &'PROPORTION OF SELECTED TREES REMOVED= ',F6.3,';',/,&
          &T12,'SPECIES=',A,' (CODE=',I3,');',&
          &' DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
          &F5.1,' CM.')
  end if
  if (PRMS(6) .eq.0) then
     if (LKECHO)write(JOSTND,11223)
     11223 format(T12,'CUTTING WILL BE DISTRIBUTED THROUGHOUT THE ',&
          &'SPECIFIED DIAMETER CLASS.')
  else if (PRMS(6) .eq.1) then
     if (LKECHO)write(JOSTND,11224)
     11224 format(T12,'CUTTING WILL BE FROM BELOW IN THE SPECIFIED ',&
          &'DIAMETER CLASS.')
  else
     if (LKECHO)write(JOSTND,11225)
     11225 format(T12,'CUTTING WILL BE FROM ABOVE IN THE SPECIFIED ',&
          &'DIAMETER CLASS.')
  end if
  goto 10

  !  ==========  OPTION NUMBER 113: LOCATE  ============================LOCATE

  11300 continue
  if (LNOTBK(1)) TLAT = ARRAY(1)
  if (LNOTBK(2)) TLONG = ARRAY(2)
  if (LNOTBK(3)) ISTATE = IFIX(ARRAY(3))
  if (LNOTBK(4)) ICNTY = IFIX(ARRAY(4))
  if (LKECHO)write(JOSTND,11310) KEYWRD,TLAT,TLONG,ISTATE,ICNTY
  11310 format (/A8,'   LATITUDE =',F9.4,';  LONGITUDE = ',F9.4,&
       &';  STATE CODE = ',I2,';  COUNTY CODE = ',I3)
  goto 10

  !  ==========  OPTION NUMBER 114: available =======================available

  11400 continue

  !     PROCESS OPTION 114 CODE HERE.
  !     BGCIN KEYWORD WAS HERE AND INITIATED CALL TO SUBROUTINE BGCIN.
  !     BGC EXTENSION HAS BEEN RETIRED.

  goto 10

  !  ==========  OPTION NUMBER 115: THINCC  ===========================THINCC

  11500 continue
  ICFLAG = 231
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.le.0) goto 11510
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  11510 continue
  if (LNOTBK(2)) then
     PRMS(1) = ARRAY(2)
  else

     !        REQUIRED RESIDUAL IS MISSING.

     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !   PROCESS REMAINING PARAMETER FIELDS

  PRMS(2) = EFF
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)

  !   SPECIES CODE PROCESSING.

  call SPDECD (4,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  PRMS(3) = IS
  PRMS(4) = 0.0
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)*CMtoIN
  PRMS(5) = 999.0
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)*CMtoIN
  if (PRMS(5) .le. PRMS(4)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if
  PRMS(6) = 0.
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)
  if (PRMS(6).eq.0..and. PRMS(2).gt.0.)PRMS(2) = 0.
  call OPNEW (KODE,IDT,ICFLAG,6,PRMS)
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,11520) KEYWRD,IDT,PRMS(1)
  11520 format (/A8,'   DATE/CYCLE=',I5,'; RESIDUAL=',F8.2)
  if (PRMS(2).eq.0.) then
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,11521)KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*INtoCM
     11521 format(T12,&
          &'PROPORTION OF SELECTED TREES REMOVED WILL BE ',&
          &'COMPUTED BY THE MODEL'/,&
          &T12,'SPECIES=',A,' (CODE=',I3,');',&
          &' DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
          &F5.1,' CM.')
  else
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,11522)PRMS(2),KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*INtoCM
     11522 format(T12,&
          &'PROPORTION OF SELECTED TREES REMOVED= ',F6.3,';',/,&
          &T12,'SPECIES=',A,' (CODE=',I3,');',&
          &' DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
          &F5.1,' CM.')
  end if
  if (PRMS(6) .eq.0) then
     if (LKECHO)write(JOSTND,11523)
     11523 format(T12,'CUTTING WILL BE DISTRIBUTED THROUGHOUT THE ',&
          &'SPECIFIED DIAMETER CLASS.')
  else if (PRMS(6) .eq.1) then
     if (LKECHO)write(JOSTND,11524)
     11524 format(T12,'CUTTING WILL BE FROM BELOW IN THE SPECIFIED ',&
          &'DIAMETER CLASS.')
  else
     if (LKECHO)write(JOSTND,11525)
     11525 format(T12,'CUTTING WILL BE FROM ABOVE IN THE SPECIFIED ',&
          &'DIAMETER CLASS.')
  end if
  goto 10

  !  ==========  OPTION NUMBER 116: ECON    ============================ECON

  11600 continue
  if (LKECHO)write(JOSTND,11610) KEYWRD
  11610 format (/A8,'   ECONOMIC EXTENSION KEYWORDS:')
  call ECIN(IRECNT,IREAD,JOSTND,NSP,ICYC,LKECHO,ISPGRP)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  goto 10

  !  ==========  OPTION NUMBER 117: DATABASE ============================DATABASE

  11700 continue
  if (LKECHO)write(JOSTND,11710) KEYWRD
  11710 format (/A8,'   DATABASE KEYWORDS:')
  call DBSIN (KEYWRD,ARRAY,ISDSP,SDLO,SDHI,LNOTBK,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  goto 10

  !  ==========  OPTION NUMBER 118: available  =========================available

  11800 continue

  !     PROCESS OPTION 118 CODE HERE. PREVIOUS OPTION WAS AN ACTIVITY AND USED
  !     ACTIVITY CODE 100.

  goto 10

  !  ==========  OPTION NUMBER 119: DEFECT  =========================DEFECT

  11900 continue

  !     READ COEFFICIENTS FROM A SUPPLEMENTAL RECORD.

  read(IREAD,1905,end = 80) (WK6(I),I = 1,8)
  IRECNT = IRECNT+1

  !     DETERMINE THE TYPE OF DEFALUT FACTOR SPECIFIED: 0 OR BLANK FOR
  !     BOTH TYPES, 1 FOR WEST CUBIC OR EAST PULPWOOD, 2 FOR WEST BOARDS
  !     OR EAST SAWTIMBER.

  IDTYPE = 0
  if (LNOTBK(1) .and. ARRAY(1).eq.1.) then
     IDTYPE = 1
     LCVOLS = .true.
  else if (LNOTBK(1) .and. ARRAY(1).eq.2.) then
     IDTYPE = 2
     LBVOLS = .true.
  else if (LNOTBK(1) .and. (ARRAY(1).gt.2..or. ARRAY(1).lt.0.)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
     go to 10
  else
     LCVOLS = .true.
     LBVOLS = .true.
  end if
  if (LNOTBK(3) .and. ARRAY(3).gt.0.)IGNDEF = 1

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10

  !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
           CFDEFT(2,IGSP) = WK6(1)
           CFDEFT(3,IGSP) = WK6(2)
           CFDEFT(4,IGSP) = WK6(3)
           CFDEFT(5,IGSP) = WK6(4)
           CFDEFT(6,IGSP) = WK6(5)
           CFDEFT(7,IGSP) = WK6(6)
           CFDEFT(8,IGSP) = WK6(7)
           CFDEFT(9,IGSP) = WK6(8)
        end if
        if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
           BFDEFT(2,IGSP) = WK6(1)
           BFDEFT(3,IGSP) = WK6(2)
           BFDEFT(4,IGSP) = WK6(3)
           BFDEFT(5,IGSP) = WK6(4)
           BFDEFT(6,IGSP) = WK6(5)
           BFDEFT(7,IGSP) = WK6(6)
           BFDEFT(8,IGSP) = WK6(7)
           BFDEFT(9,IGSP) = WK6(8)
        end if
        11901 continue
     end do
     ILEN = ISPGRP(-IS,92)

     select case (VARACD)

     case ('CS','LS','NE','SN','ON')
        if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
           if (LKECHO)write(JOSTND,11932) KEYWRD,KARD(2)(1:ILEN),&
                &IS,(WK6(I),I = 1,8)
        end if
        if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
           if (LKECHO)write(JOSTND,11933) KEYWRD,KARD(2)(1:ILEN),&
                &IS,(WK6(I),I = 1,8)
        end if

     case default
        if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
           if (LKECHO)write(JOSTND,11934) KEYWRD,KARD(2)(1:ILEN),&
                &IS,(WK6(I),I = 1,8)
        end if
        if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
           if (LKECHO)write(JOSTND,11935) KEYWRD,KARD(2)(1:ILEN),&
                &IS,(WK6(I),I = 1,8)
        end if

     end select

     !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (IS .eq.0) then
     do IS = 1,MAXSP
        if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
           CFDEFT(2,IS) = WK6(1)
           CFDEFT(3,IS) = WK6(2)
           CFDEFT(4,IS) = WK6(3)
           CFDEFT(5,IS) = WK6(4)
           CFDEFT(6,IS) = WK6(5)
           CFDEFT(7,IS) = WK6(6)
           CFDEFT(8,IS) = WK6(7)
           CFDEFT(9,IS) = WK6(8)
        end if
        if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
           BFDEFT(2,IS) = WK6(1)
           BFDEFT(3,IS) = WK6(2)
           BFDEFT(4,IS) = WK6(3)
           BFDEFT(5,IS) = WK6(4)
           BFDEFT(6,IS) = WK6(5)
           BFDEFT(7,IS) = WK6(6)
           BFDEFT(8,IS) = WK6(7)
           BFDEFT(9,IS) = WK6(8)
        end if
        11910 continue
     end do

     select case (VARACD)

     case ('CS','LS','NE','SN','ON')
        if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
           if (LKECHO)write(JOSTND,11912) KEYWRD,(WK6(I),I = 1,8)
           11912 format(/A8,T12,'PULPWOOD VOLUME DEFECT',&
                &' PROPORTIONS HAVE BEEN CHANGED FOR ALL SPECIES:',&
                &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
                &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
                &'; 63.5 CM TREES=',F6.2,'; 76.2 CM TREES=',F6.2,/T12,&
                &'88.9 CM TREES=',F6.2,'; 101.6 CM AND LARGER TREES=',F6.2)
           if (LKECHO .and. IGNDEF.gt.0)write(JOSTND,11909)
           11909 format(/T12,'DEFECT PERCENTAGES READ AS PART OF THE TRE',&
                &'E RECORD INPUT FOR ALL SPECIES WILL BE IGNORED.')
        end if
        if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
           if (LKECHO)write(JOSTND,11913) KEYWRD,(WK6(I),I = 1,8)
           11913 format(/A8,T12,'SAWLOG VOLUME DEFECT',&
                &' PROPORTIONS HAVE BEEN CHANGED FOR ALL SPECIES:',&
                &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
                &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
                &'; 63.5 CM TREES=',F6.2,'; 76.2 CM TREES=',F6.2,/T12,&
                &'88.9 CM TREES=',F6.2,'; 101.6 CM AND LARGER TREES=',F6.2)
           if (LKECHO .and. IGNDEF.gt.0)write(JOSTND,11909)
        end if

     case default
        if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
           if (LKECHO)write(JOSTND,11920) KEYWRD,(WK6(I),I = 1,8)
           11920 format(/A8,T12,'CUBIC METER VOLUME DEFECT',&
                &' PROPORTIONS HAVE BEEN CHANGED FOR ALL SPECIES:',&
                &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
                &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
                &'; 63.5 CM TREES=',F6.2,'; 76.2 CM TREES=',F6.2,/T12,&
                &'88.9 CM TREES=',F6.2,'; 101.6 CM AND LARGER TREES=',F6.2)
           if (LKECHO .and. IGNDEF.gt.0)write(JOSTND,11909)
        end if
        if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
           if (LKECHO)write(JOSTND,11921) KEYWRD,(WK6(I),I = 1,8)
           11921 format(/A8,T12,'BOARD VOLUME DEFECT',&
                &' PROPORTIONS HAVE BEEN CHANGED FOR ALL SPECIES:',&
                &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
                &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
                &'; 63.5 CM TREES=',F6.2,'; 76.2 CM TREES=',F6.2,/T12,&
                &'88.9 CM TREES=',F6.2,'; 101.6 CM AND LARGER TREES=',F6.2)
           if (LKECHO .and. IGNDEF.gt.0)write(JOSTND,11909)
        end if

     end select

     !     SPECIES CODE IS SPECIFIED (IS > 0).

  else
     if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
        CFDEFT(2,IS) = WK6(1)
        CFDEFT(3,IS) = WK6(2)
        CFDEFT(4,IS) = WK6(3)
        CFDEFT(5,IS) = WK6(4)
        CFDEFT(6,IS) = WK6(5)
        CFDEFT(7,IS) = WK6(6)
        CFDEFT(8,IS) = WK6(7)
        CFDEFT(9,IS) = WK6(8)
     end if
     if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
        BFDEFT(2,IS) = WK6(1)
        BFDEFT(3,IS) = WK6(2)
        BFDEFT(4,IS) = WK6(3)
        BFDEFT(5,IS) = WK6(4)
        BFDEFT(6,IS) = WK6(5)
        BFDEFT(7,IS) = WK6(6)
        BFDEFT(8,IS) = WK6(7)
        BFDEFT(9,IS) = WK6(8)
     end if
     ILEN = 3

     select case (VARACD)

     case ('CS','LS','NE','SN','ON')
        if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
           if (LKECHO)write(JOSTND,11932) KEYWRD,KARD(2)(1:ILEN),&
                &IS,(WK6(I),I = 1,8)
           11932 format(/A8,T12,'PULPWOOD VOLUME DEFECT PROPORTIONS ',&
                &'HAVE BEEN CHANGED FOR SPECIES:',A,' (CODE=',I3,')',&
                &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
                &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
                &'; 63.5 CM TREES=',F6.2,'; 76.2 CM TREES=',F6.2,/T12,&
                &'88.9 CM TREES=',F6.2,'; 101.6 CM AND LARGER TREES=',F6.2)
           if (LKECHO .and. IGNDEF.gt.0)write(JOSTND,11909)
        end if
        if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
           if (LKECHO)write(JOSTND,11933) KEYWRD,KARD(2)(1:ILEN),&
                &IS,(WK6(I),I = 1,8)
           11933 format(/A8,T12,'SAWLOG VOLUME DEFECT PROPORTIONS HAVE ',&
                &'BEEN CHANGED FOR SPECIES:',A,' (CODE=',I3,')',&
                &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
                &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
                &'; 63.5 CM TREES=',F6.2,'; 76.2 CM TREES=',F6.2,/T12,&
                &'88.9 CM TREES=',F6.2,'; 101.6 CM AND LARGER TREES=',F6.2)
           if (LKECHO .and. IGNDEF.gt.0)write(JOSTND,11909)
        end if

     case default
        if (IDTYPE.eq.0.or. IDTYPE.eq.1) then
           if (LKECHO)write(JOSTND,11934) KEYWRD,KARD(2)(1:ILEN),&
                &IS,(WK6(I),I = 1,8)
           11934 format(/A8,T12,'CUBIC METER VOLUME DEFECT PROPORTIONS ',&
                &'HAVE BEEN CHANGED FOR SPECIES:',A,' (CODE=',I3,')',&
                &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
                &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
                &'; 63.5 CM TREES=',F6.2,'; 76.2 CM TREES=',F6.2,/T12,&
                &'88.9 CM TREES=',F6.2,'; 101.6 CM AND LARGER TREES=',F6.2)
           if (LKECHO .and. IGNDEF.gt.0)write(JOSTND,11909)
        end if
        if (IDTYPE.eq.0.or. IDTYPE.eq.2) then
           if (LKECHO)write(JOSTND,11935) KEYWRD,KARD(2)(1:ILEN),&
                &IS,(WK6(I),I = 1,8)
           11935 format(/A8,T12,'BOARD VOLUME DEFECT PROPORTIONS ',&
                &'HAVE BEEN CHANGED FOR SPECIES:',A,' (CODE=',I3,')',&
                &/T12,' 12.7 CM TREES=',F6.2,'; 25.4 CM TREES=',F6.2,&
                &'; 38.1 CM TREES=',F6.2,/T12,'50.8 CM TREES=',F6.2,&
                &'; 63.5 CM TREES=',F6.2,'; 76.2 CM TREES=',F6.2,/T12,&
                &'88.9 CM TREES=',F6.2,'; 101.6 CM AND LARGER TREES=',F6.2)
           if (LKECHO .and. IGNDEF.gt.0)write(JOSTND,11909)
        end if

     end select
  end if
  go to 10

  !  ==========  OPTION NUMBER 120: available ==========================available

  12000 continue

  !     PROCESS OPTION 120 CODE HERE. PREVIOUS OPTION WAS AN ACTIVITY AND USED
  !     ACTIVITY CODE 205.
  !     CRUZFILE KEYWORD HAD BEEN HERE BUT WAS DEFUNCT AND REMOVED 7/23/2021

  goto 10

  !  ==========  OPTION NUMBER 121: STANDCN  ================================ STANDCN

  12100 continue
  read (IREAD,'(A)',end = 80) DBCN
  IRECNT = IRECNT + 1
  DBCN = TRIM(ADJUSTL(DBCN))
  if (LKECHO)write(JOSTND,12110) KEYWRD,TRIM(DBCN)
  12110 format(/A8,'   DATA BASE CONTROL NUMBER=',A)
  goto 10

  !  ==========  OPTION NUMBER 122: THINMIST  =========================THINMIST

  12200 continue
  ICFLAG = 233
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  ARRAY(3) = ARRAY(3) * CMtoIN
  ARRAY(4) = ARRAY(4) * CMtoIN
  if (IPRMPT.le.0) goto 12210
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10

  12210 continue
  PRMS(1) = 0.
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (PRMS(1).lt.0.0.or. PRMS(1).gt.6.0)PRMS(1) = 0.

  !   PROCESS REMAINING PARAMETER FIELDS

  PRMS(2) = 0.0
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  if (PRMS(2).lt.0.0)PRMS(2) = 0.0

  PRMS(3) = 999.0
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)

  if (PRMS(3) .le. PRMS(2)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  PRMS(4) = EFF
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)

  call OPNEW (KODE,IDT,ICFLAG,4,PRMS)
  if (KODE.gt.0) goto 10
  if (PRMS(1).eq.0.0) then
     if (LKECHO)write(JOSTND,12215) KEYWRD,IDT
     12215 format (/A8,'   DATE/CYCLE=',I5,'; TREES WITH A DMR OF 1-6',&
          &' WILL BE REMOVED.')
  else
     if (LKECHO)write(JOSTND,12220) KEYWRD,IDT,PRMS(1)
     12220 format (/A8,'   DATE/CYCLE=',I5,'; TREES WITH A DMR OF',F5.0,&
          &' WILL BE REMOVED.')
  end if
  if (LKECHO)write(JOSTND,12225)PRMS(2)*INtoCM,PRMS(3)*INtoCM,PRMS(4)
  12225 format(T12,&
       &'DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
       &F5.1,' CM.',&
       &' PROPORTION OF SELECTED TREES REMOVED= ',F6.3)
  goto 10

  !  ==========  OPTION NUMBER 123  TREESZCP  =========================TREESZCP

  12300 continue

  !   SPECIES CODE PROCESSING.

  call SPDECD (1,ISPC,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPC.eq.-999) go to 10
  LMORT = .true.
  CAPDBH = 999.
  if (LNOTBK(2))CAPDBH = ARRAY(2)*CMtoIN
  CAPPRP = 1.0
  if (LNOTBK(3))CAPPRP = ARRAY(3)
  CAPFLG = 0.
  if (LNOTBK(4))CAPFLG = ARRAY(4)
  IDFLG = IFIX(CAPFLG)
  CAPHT = 999.
  if (LNOTBK(5))CAPHT = ARRAY(5)*MtoFT
  if (CAPPRP.lt.0..or. CAPPRP.gt.1.0.or. CAPDBH.lt.0..or.&
       &CAPHT.lt.0..or. CAPFLG.lt.0..or. CAPFLG.gt.2.) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     go to 10
  end if

  !     IF (ISPC<0) CHANGE ALL SPECIES IN THE GROUP

  if (ISPC .lt.0) then
     IGRP = -ISPC
     IULIM = ISPGRP(IGRP,1)+1
     do IG = 2,IULIM
        IGSP = ISPGRP(IGRP,IG)
        SIZCAP(IGSP,1) = CAPDBH
        SIZCAP(IGSP,2) = CAPPRP
        SIZCAP(IGSP,3) = CAPFLG
        SIZCAP(IGSP,4) = CAPHT
        12301 continue
     end do
     ILEN = ISPGRP(-ISPC,92)
     if (LKECHO)write(JOSTND,12320) KEYWRD,KARD(1)(1:ILEN),ISPC,&
          &CAPDBH*INtoCM,CAPPRP,IDFLG,CAPHT*FTtoM

     !     IF (ISPC=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

  else if (ISPC .eq.0) then
     do I = 1,MAXSP
        SIZCAP(I,1) = CAPDBH
        SIZCAP(I,2) = CAPPRP
        SIZCAP(I,3) = CAPFLG
        SIZCAP(I,4) = CAPHT
        12310 continue
     end do
     if (LKECHO)write(JOSTND,12315) KEYWRD,CAPDBH*INtoCM,CAPPRP,&
          &IDFLG,CAPHT*FTtoM
     12315 format(/A8,'   SPECIES= ALL (CODE=  0',&
          &');  DBH=',F6.1,';  MORTALITY PROPORTION=',F7.4,&
          &';  DBH USE CODE=',I2,';  HT=',F6.1)

     !     SPECIES CODE IS SPECIFIED (ISPC > 0).

  else
     SIZCAP(ISPC,1) = CAPDBH
     SIZCAP(ISPC,2) = CAPPRP
     SIZCAP(ISPC,3) = CAPFLG
     SIZCAP(ISPC,4) = CAPHT
     ILEN = 3
     if (LKECHO)write(JOSTND,12320) KEYWRD,KARD(1)(1:ILEN),ISPC,&
          &CAPDBH*INtoCM,CAPPRP,IDFLG,CAPHT*FTtoM
     12320 format(/A8,'   SPECIES= ',A,' (CODE= ',I3,&
          &');  DBH=',F6.1,';  MORTALITY PROPORTION=',F7.4,&
          &';  DBH USE CODE=',I2,';  HT=',F6.1)
  end if
  go to 10

  !  ==========  OPTION NUMBER 124: THINRDEN ==========================THINRDEN

  12400 continue
  ICFLAG = 234
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.le.0) goto 12410
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  12410 continue
  if (LNOTBK(2)) then
     PRMS(1) = ARRAY(2)*M2pHAtoFT2pACR
  else

     !        REQUIRED RESIDUAL IS MISSING.

     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !   PROCESS REMAINING PARAMETER FIELDS

  PRMS(2) = EFF
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)

  !   SPECIES CODE PROCESSING.

  call SPDECD (4,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  PRMS(3) = IS
  PRMS(4) = 0.0
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)*CMtoIN
  PRMS(5) = 999.0
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)*CMtoIN
  if (PRMS(5) .le. PRMS(4)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if
  PRMS(6) = 0.
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)
  if (PRMS(6).eq.0..and. PRMS(2).gt.0.)PRMS(2) = 0.
  call OPNEW (KODE,IDT,ICFLAG,6,PRMS)
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,12420) KEYWRD,IDT,PRMS(1)/M2pHAtoFT2pACR
  12420 format (/A8,'   DATE/CYCLE=',I5,'; RESIDUAL=',F8.2)
  if (PRMS(2).eq.0.) then
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,12421)KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*INtoCM
     12421 format(T12,&
          &'PROPORTION OF SELECTED TREES REMOVED WILL BE ',&
          &'COMPUTED BY THE MODEL'/,&
          &T12,'SPECIES=',A,' (CODE=',I3,');',&
          &' DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
          &F5.1,' CM.')
  else
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,12422)PRMS(2),KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*INtoCM
     12422 format(T12,&
          &'PROPORTION OF SELECTED TREES REMOVED= ',F6.3,';',/,&
          &T12,'SPECIES=',A,' (CODE=',I3,');',&
          &' DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
          &F5.1,' CM.')
  end if
  if (PRMS(6) .eq.0) then
     if (LKECHO)write(JOSTND,12423)
     12423 format(T12,'CUTTING WILL BE DISTRIBUTED THROUGHOUT THE ',&
          &'SPECIFIED DIAMETER CLASS.')
  else if (PRMS(6) .eq.1) then
     if (LKECHO)write(JOSTND,12424)
     12424 format(T12,'CUTTING WILL BE FROM BELOW IN THE SPECIFIED ',&
          &'DIAMETER CLASS.')
  else
     if (LKECHO)write(JOSTND,12425)
     12425 format(T12,'CUTTING WILL BE FROM ABOVE IN THE SPECIFIED ',&
          &'DIAMETER CLASS.')
  end if
  goto 10

  !  ==========  OPTION NUMBER 125: SPGROUP  ==========================SPGROUP

  12500 continue

  !     FIRST CHECK TO SEE THAT THE MAXIMUM NUMBER OF SPECIES GROUPS HAS NOT
  !     ALREADY BEEN DEFINED.  IF SO, IGNORE THIS REQUEST.

  if (NSPGRP .ge.30) then
     if (LKECHO)write(JOSTND,12502) IRECNT,KEYWRD,KARD(1)
     12502 format (/,' CARD NUM =',I5,'; KEYWORD FIELD = ',A8,&
          &'   GROUP NAME: ',A10)
     call ERRGRO(.true.,28)
     12503 continue
     IRECNT = IRECNT+1
     read(IREAD,'(A)',end = 80) RECORD
     I = LEN_TRIM(RECORD)
     if (LKECHO)write(JOSTND,'(" SKIPPED RECORD=",A)') RECORD(:I)
     if (RECORD(I:I).eq.'&') goto 12503
     go to 10
  end if

  !     PROCESS THE SPECIES GROUP REQUEST. DELETE LEADING BLANKS
  !     FROM THE SPECIES GROUP NAME AND STORE THE LENGTH OF THE
  !     NAME

  NSPGRP = NSPGRP+1
  if (LNOTBK(1)) then ! if there is a name, then use it
     NAMGRP(NSPGRP) = ADJUSTL(KARD(1))
     ILEN = LEN_TRIM(NAMGRP(NSPGRP))
     do J = 1,ILEN
        call UPCASE(NAMGRP(NSPGRP)(J:J))
     end do
  else ! otherwise build one
     write(NAMGRP(NSPGRP),'("GROUP",I2.2,2X)') NSPGRP
     ILEN = 7
  end if
  ISPGRP(NSPGRP,92) = ILEN ! save the group name length
  INUM = 0 ! initialize the number of codes in the group
  KARD = ' ' ! blank out and zero out kard and arrays
  ARRAY = 0
  12504 continue
  IRECNT = IRECNT+1
  read(IREAD,'(A)',end = 80) RECORD
  ILEN = LEN_TRIM(RECORD)
  J = 0 ! j points to the first char for the new token...
  ! when j is zero, no new token is being built.
  do I = 1,ILEN ! loop over the chars in the record
     if (RECORD(I:I).eq.' ') then ! hit a blank
        if (J.eq.0) cycle ! if no token being built, go on to next char
     end if
     if (RECORD(I:I).eq.'&') goto 12504 ! read another record
     if (J.eq.0) J = I
     call UPCASE(RECORD(I:I))
     if (RECORD(I:I).eq.' '.or.I.eq.ILEN) then ! end of token (a blank or end of record)
        INUM = INUM+1 ! so, we have a token.

        !  DON'T SAVE OVER 90, COULD DO AN ERROR MSG, BUT DIDN'T

        if (INUM.gt.90) INUM = 90
        if (I.eq.ILEN) then ! store the token in kard(1)
           KARD(1) = RECORD(J:I)
        else
           KARD(1) = RECORD(J:I-1)
        end if
        J = 0
        ARRAY(1) = 0.
        read(KARD(1),'(F10.0)',ERR = 12505) ARRAY(1) ! make a number, if we can.
        12505 continue
        if (KARD(1).eq.'ALL') then ! ALL is illegal
           INUM = INUM-1
           cycle
        end if

        call SPDECD(1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
             &ARRAY,KARD)
        if (ARRAY(1).eq.0) then ! zero is illegal
           INUM = INUM-1
           cycle
        end if
        if (IS.eq.-999) then ! species decode failed, don't keep the species
           INUM = INUM-1
           cycle
        end if
        if (INUM.gt.1) then ! check for duplicates, save unique codes
           do J = 2,INUM
              if (ISPGRP(NSPGRP,J).eq.IS) then
                 IS = -999 ! if it is a dup, then set flag
                 exit
              end if
           end do
           J = 0
           if (IS.eq.-999) then ! skip dups
              INUM = INUM-1
              cycle
           end if
        end if
        ISPGRP(NSPGRP,INUM+1) = IS
     end if
  end do
  ISPGRP(NSPGRP,1) = INUM
  if (LKECHO)write(JOSTND,12535) KEYWRD,-NSPGRP,NAMGRP(NSPGRP),INUM
  12535 format(/A8,'   GROUP NUMBER:',I4,'  GROUP NAME: ',A10,&
       &'  NUMBER OF SPECIES IN THIS GROUP:',I3,'  SPECIES:')
  if (LKECHO)write(JOSTND,12540) (ISPGRP(NSPGRP,J),&
       &NSP(ISPGRP(NSPGRP,J),1),J = 2,INUM+1)
  12540 format((T12,9(I3,' =',A2,:,'; '),I3,' =',A2))
  go to 10

  !  ==========  OPTION NUMBER 126: BMIN  ============================BMIN

  12600 continue
  if (LKECHO)write(JOSTND,12610) KEYWRD
  12610 format (/A8,'   INDIVIDUAL STAND WWPB MODEL OPTIONS:')
  call BMIN(LKECHO)
  goto 10

  !  ==========  OPTION NUMBER 127: DATASCRN ========================DATASCRN

  12700 continue
  if (LNOTBK(1)) SDLO = ARRAY(1)*CMtoIN
  if (LNOTBK(2)) SDHI = ARRAY(2)*CMtoIN

  !     SPECIES CODE PROCESSING.

  call SPDECD (3,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10

  ISDSP = IS
  if (LKECHO) then
     write(JOSTND,12708)KEYWRD
     12708 format(/A8)
     if (IS.ne.0) then
        ILEN = 3
        if (IS.lt.0)ILEN = ISPGRP(-IS,92)
        if (LKECHO)write(JOSTND,12709) 'SPECIES= ',KARD(3)(1:ILEN),&
             &IS,'IS'
        12709 format (T12,A,A,' (CODE= ',I3,') ',A,&
             &' TARGETED FOR SCREENING.')
     else
        if (LKECHO)write(JOSTND,1411) 'ALL',' SPECIES',IS,'ARE'
     end if
     write(JOSTND,12710) SDLO*INtoCM,SDHI*INtoCM
     12710 format(T12,'ONLY RECORDS WITH DIAMETERS GE',F6.1,&
          &' CM OR LT',F6.1,' CM WILL BE READ FROM THE DATA.',/,&
          &T12,'ALL OTHER INPUT RECORDS WILL BE SCREENED OUT.')
  end if
  go to 10

  !  ==========  OPTION NUMBER 128: SETPTHIN ========================SETPTHIN

  12800 continue
  ICFLAG = 248
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.le.0) goto 12810
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  12810 continue

  !   PROCESS REMAINING PARAMETER FIELDS

  PRMS(1) = FLOAT(ITHNPN)
  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  POINTNO = INT(PRMS(1))

  !  CHECK FOR POINT GROUP

  call PTGDECD(POINTNO,KARD(2),IFLAG)
  if (IFLAG.ge.1) then
     ITHNPN = POINTNO
     PRMS(1) = FLOAT(ITHNPN)
     ILEN = IPTGRP(ITHNPN,52)
     if (LKECHO)write(JOSTND,12812) 'POINT= ',KARD(2)(1:ILEN),ITHNPN
     12812 format (T12,A,A,I3' TARGETED FOR THIS CUT.')
  end if

  PRMS(2) = FLOAT(ITHNPA)
  if (LNOTBK(3).and.ARRAY(3).gt.0.) PRMS(2) = ARRAY(3)

  if (PRMS(2).le.0..or. PRMS(2).gt.6) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     if (LKECHO)write(JOSTND,12816)
     12816 format(T12,'POINT THINNING RESIDUAL ATTRIBUTE IS MISSING ',&
          &'OR INVALID.')
     go to 10
  end if

  call OPNEW (KODE,IDT,ICFLAG,2,PRMS)
  if (KODE.gt.0) goto 10
  if (PRMS(2).eq.1.) then
     if (LKECHO)write(JOSTND,12824)KEYWRD,IDT,PRMS(1),PRMS(2)
     12824 format(/A8,'   DATE/CYCLE=',I5,&
          &'; POINT NUMBER=',F5.0,'; POINT THINNING RESIDUAL ATTRIBUTE=',&
          &F5.0,' (TREES PER HA).')
  else if (PRMS(2).eq.2.) then
     if (LKECHO)write(JOSTND,12825)KEYWRD,IDT,PRMS(1),PRMS(2)
     12825 format(/A8,'   DATE/CYCLE=',I5,&
          &'; POINT NUMBER=',F5.0,'; POINT THINNING RESIDUAL ATTRIBUTE=',&
          &F5.0,' (BASAL AREA PER HA).')
  else if (PRMS(2).eq.3.) then
     if (LKECHO)write(JOSTND,12826)KEYWRD,IDT,PRMS(1),PRMS(2)
     12826 format(/A8,'   DATE/CYCLE=',I5,&
          &'; POINT NUMBER=',F5.0,'; POINT THINNING RESIDUAL ATTRIBUTE=',&
          &F5.0,' (STAND DENSITY INDEX).')
  else if (PRMS(2).eq.4.) then
     if (LKECHO)write(JOSTND,12827)KEYWRD,IDT,PRMS(1),PRMS(2)
     12827 format(/A8,'   DATE/CYCLE=',I5,&
          &'; POINT NUMBER=',F5.0,'; POINT THINNING RESIDUAL ATTRIBUTE=',&
          &F5.0,' (PERCENT CANOPY COVER).')
  else if (PRMS(2).eq.5.) then
     if (LKECHO)write(JOSTND,12828)KEYWRD,IDT,PRMS(1),PRMS(2)
     12828 format(/A8,'   DATE/CYCLE=',I5,&
          &'; POINT NUMBER=',F5.0,'; POINT THINNING RESIDUAL ATTRIBUTE=',&
          &F5.0,' (CURTIS RELATIVE DENSITY).')
  else
     if (LKECHO)write(JOSTND,12829)KEYWRD,IDT,PRMS(1),PRMS(2)
     12829 format(/A8,'   DATE/CYCLE=',I5,&
          &'; POINT NUMBER=',F5.0,'; POINT THINNING RESIDUAL ATTRIBUTE=',&
          &F5.0,' (SILVAH RELATIVE DENSITY).')
  end if
  go to 10

  !  ==========  OPTION NUMBER 129: THINPT   ========================THINPT

  12900 continue
  ICFLAG = 235
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.le.0) goto 12910
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  12910 continue
  if (LNOTBK(2)) then
     PRMS(1) = ARRAY(2)
  else

     !        REQUIRED RESIDUAL IS MISSING.

     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !   PROCESS REMAINING PARAMETER FIELDS

  PRMS(2) = EFF
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)

  !   SPECIES CODE PROCESSING.

  call SPDECD (4,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  PRMS(3) = IS
  PRMS(4) = 0.0
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  PRMS(5) = 999.0
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)
  if (PRMS(5) .le. PRMS(4)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if
  PRMS(6) = 0.
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)
  if (PRMS(6).eq.0..and. PRMS(2).gt.0.)PRMS(2) = 0.
  PRMS(4) = PRMS(4) * CMtoIN
  PRMS(5) = PRMS(5) * CMtoIN
  call OPNEW (KODE,IDT,ICFLAG,6,PRMS)
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,12920) KEYWRD,IDT,PRMS(1)
  12920 format (/A8,'   DATE/CYCLE=',I5,'; RESIDUAL=',F8.2)
  if (PRMS(2).eq.0.) then
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,12921)KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM, PRMS(5)*INtoCM
     12921 format(T12,&
          &'PROPORTION OF SELECTED TREES REMOVED WILL BE ',&
          &'COMPUTED BY THE MODEL'/,&
          &T13,'SPECIES=',A,' (CODE=',I3,');',&
          &' DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
          &F5.1,' CM.')
  else
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,12922)PRMS(2),KARD(4)(1:ILEN),&
          &IS,PRMS(4)*INtoCM, PRMS(5)*INtoCM
     12922 format(T12,&
          &'PROPORTION OF SELECTED TREES REMOVED= ',F6.3,';',/,&
          &T12,'SPECIES=',A,' (CODE=',I3,');',&
          &' DBH OF REMOVED TREES WILL RANGE FROM ',F5.1,' TO ',&
          &F5.1,' CM.')
  end if
  if (PRMS(6) .eq.0) then
     if (LKECHO)write(JOSTND,12923)
     12923 format(T12,'CUTTING WILL BE DISTRIBUTED THROUGHOUT THE ',&
          &'SPECIFIED DIAMETER CLASS,')
  else if (PRMS(6) .eq.1) then
     if (LKECHO)write(JOSTND,12924)
     12924 format(T12,'CUTTING WILL BE FROM BELOW IN THE SPECIFIED ',&
          &'DIAMETER CLASS,')
  else
     if (LKECHO)write(JOSTND,12925)
     12925 format(T12,'CUTTING WILL BE FROM ABOVE IN THE SPECIFIED ',&
          &'DIAMETER CLASS,')
  end if
  if (LKECHO)write(JOSTND,12926)
  12926 format(T12,'ON THE POINT SPECIFIED WITH THE SETPTHIN ',&
       &'KEYWORD PRIOR TO THIS THINNING REQUEST.')
  goto 10

  !  ==========  OPTION NUMBER 130: VOLEQNUM ========================VOLEQNUM

  13000 continue

  !  IF WE DONT HAVE A KODFOR VALUE YET SET REGION BASED ON VRAIANT 'VAR'

  if (KODFOR.le.0) then
     select case(VARACD)
     case('EM','KT','NI','IE','BC')
        IRDUM = 1
     case( 'CR')
        IRDUM = 2
     case('CI','TT','UT')
        IRDUM = 4
     case('CA','NC','SO','WS')
        IRDUM = 5
     case('BM','PN','WC','EC')
        IRDUM = 6
     case('OC','OP')
        IRDUM = 7
     case('SN')
        IRDUM = 8
     case('CS','LS','NE','ON')
        IRDUM = 9
     case('AK')
        IRDUM = 10
     end select
  else

     !   FOREST CODE PROCESSING

     if ((VARACD.eq.'SN').and.&
          &(KODFOR.ge.1000)) then
        IREGN = KODFOR/10000
        IRDUM = IREGN
        IFORST = KODFOR/100-IREGN*100
        write(FORST,'(I2)')IFORST
        if (IFORST.lt.10)FORST(1:1) = '0'
        INTDIST = KODFOR - (KODFOR/100)*100
        write(DIST,'(I2)')INTDIST
        if (INTDIST.lt.10)DIST(1:1) = '0'
     else if (VARACD.eq.'AK') then
        IREGN = 10
        FORST = '02'
     else
        IREGN = KODFOR/100
        IRDUM = IREGN
        IFOREST = KODFOR-900
        write(FORST,'(I2)')IFOREST
        if (IFOREST.lt.10)FORST(1:1) = '0'
     end if

     !  USE SPECIAL FORESTS FOR BIA AND PRIVATE VOLUME EQS.
     !  PN-QUINAULT IR (FC=800 USES OLYMIC 609)
     !  NC SIMPSOM TIMBER (FC=800 USES R5 SAY 510)
     !  SO INDUSTRY LANDS (FC=701 USES R5 SAY 505)
     !  SO WARM SPRINGS (FC=799 USES R6 FC=601)

     if (VARACD.eq.'PN' .or. VARACD.eq.'OP') then
        if (IREGN.eq.8) then
           IRDUM = 6
           FORDUM = '09'
        else
           IRDUM = IREGN
           FORDUM = FORST
        end if
     else if (VARACD.eq.'NC') then
        if (IREGN.eq.8) then
           IRDUM = 5
           FORDUM = '10'
        else
           IRDUM = IREGN
           FORDUM = FORST
        end if
     else if (VARACD.eq.'SO') then
        if (IREGN.eq.7) then
           select case (KODFOR)
           case (701)
              IRDUM = 5
              FORDUM = '05'
           case (799)
              IRDUM = 6
              FORDUM = '01'
           end select
        else
           IRDUM = IREGN
           FORDUM = FORST
        end if
     else
        IRDUM = IREGN
        FORDUM = FORST
     end if
  end if

  !   SPECIES CODE PROCESSING.

  call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS .eq. -999) go to 10

  !  SPECIES GROUP PROCESSING

  if (IS .lt.0) then
     IGRP = -IS
     IULIM = ISPGRP(IGRP,1)+1
     if (LNOTBK(2)) then
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           VEQNNC(IGSP) = KARD(2)
        end do

        !  CHECK FOR VALID CUBIC FT EQUATION NUMBER

        ISPEC = 9999
        PROD = '02'
        call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
             &VEQNNC(IGSP),ERRFLAG)

        !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

        if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
           select case(IRDUM)
           case(1)
              if (VARACD.eq.'IE') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(IGSP),ERRFLAG)
              end if
           case(2)
              if (VARACD.eq.'CR') then
                 IRDUM = 3
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(IGSP),ERRFLAG)
              end if
           case(5)
              if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                   &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(IGSP),ERRFLAG)
              end if
           case(7)
              if (VARACD.eq.'OC') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(IGSP),ERRFLAG)
                 if (ISPEC.ne.8888) then
                    IRDUM = 5
                    call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                         &VEQNNC(IGSP),ERRFLAG)
                 end if
              end if
           end select
        end if

        if (ISPEC.ne.8888) then
           do IG = 2,IULIM
              IGSP = ISPGRP(IGRP,IG)
              VEQNNC(IGSP) = '          '
           end do
           ILEN = ISPGRP(-IS,92)
           write(JOSTND,13003)KEYWRD,KARD(1)(1:ILEN),IS
        else
           ILEN = ISPGRP(-IS,92)
           if (LKECHO)write(JOSTND,13008)KEYWRD,KARD(1)(1:ILEN),IS
        end if
     end if
     if (LNOTBK(3)) then
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           VEQNNB(IGSP) = KARD(3)
        end do

        !  CHECK FOR VALID BOARD FT EQUATION NUMBER

        ISPEC = 9999
        PROD = '01'
        call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
             &VEQNNB(IGSP),ERRFLAG)

        !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

        if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
           select case(IRDUM)
           case(1)
              if (VARACD.eq.'IE') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(IGSP),ERRFLAG)
              end if
           case(2)
              if (VARACD.eq.'CR') then
                 IRDUM = 3
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(IGSP),ERRFLAG)
              end if
           case(5)
              if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                   &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(IGSP),ERRFLAG)
              end if
           case(7)
              if (VARACD.eq.'OC') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(IGSP),ERRFLAG)
                 if (ISPEC.ne.8888) then
                    IRDUM = 5
                    call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                         &VEQNNB(IGSP),ERRFLAG)
                 end if
              end if
           end select
        end if

        if (ISPEC.ne.8888) then
           do IG = 2,IULIM
              IGSP = ISPGRP(IGRP,IG)
              VEQNNB(IGSP) = '           '
           end do
           ILEN = ISPGRP(-IS,92)
           if (LNOTBK(2))KEYWRD = '        '
           write(JOSTND,13004)KEYWRD,KARD(1)(1:ILEN),IS
        else
           ILEN = ISPGRP(-IS,92)
           if (LNOTBK(2))KEYWRD = '        '
           if (LKECHO)write(JOSTND,13009)KEYWRD,KARD(1)(1:ILEN),IS
        end if
     end if
     goto 10
     13003 format(/A8,'   INVALID VOLUME EQUATION NUMBER: THE DEFAULT ',&
          &'CUBIC FT EQUATION NUMBER WILL BE USED FOR SPECIES GROUP=',A,&
          &' (CODE=',I3,');')
     13004 format(/A8,'   INVALID VOLUME EQUATION NUMBER: THE DEFAULT ',&
          &'BOARD FT EQUATION NUMBER WILL BE USED FOR SPECIES GROUP=',A,&
          &' (CODE=',I3,');')
     13008 format(/A8,'   NATIONAL CRUISE SYSTEM CUBIC FT VOLUME EQUA',&
          &'TION NUMBERS CHANGED FOR SPECIES GROUP=',A,' (CODE=',I3,');')
     13009 format(/A8,'   NATIONAL CRUISE SYSTEM BOARD FT VOLUME EQUA',&
          &'TION NUMBERS CHANGED FOR SPECIES GROUP=',A,' (CODE=',I3,');')

     !  ALL SPECIES PROCESSING

  else if (IS.eq.0) then
     if (LNOTBK(2)) then
        do I = 1,MAXSP
           VEQNNC(I) = KARD(2)
        end do

        !  CHECK FOR VALID CUBIC FT EQUATION NUMBER

        ISPEC = 9999
        PROD = '02'
        call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
             &VEQNNC(1),ERRFLAG)

        !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

        if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
           select case(IRDUM)
           case(1)
              if (VARACD.eq.'IE') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(1),ERRFLAG)
              end if
           case(2)
              if (VARACD.eq.'CR') then
                 IRDUM = 3
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(1),ERRFLAG)
              end if
           case(5)
              if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                   &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(1),ERRFLAG)
              end if
           case(7)
              if (VARACD.eq.'OC') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(1),ERRFLAG)
                 if (ISPEC.ne.8888) then
                    IRDUM = 5
                    call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                         &VEQNNC(1),ERRFLAG)
                 end if
              end if
           end select
        end if

        if (ISPEC.ne.8888) then
           do I = 1,MAXSP
              VEQNNC(I) = '          '
           end do
           write(JOSTND,13006)KEYWRD
        else
           if (LKECHO)write(JOSTND,13020)KEYWRD
        end if
     end if
     if (LNOTBK(3)) then
        do I = 1,MAXSP
           VEQNNB(I) = KARD(3)
        end do

        !  CHECK FOR VALID BOARD FT EQUATION NUMBER

        ISPEC = 9999
        PROD = '01'
        call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
             &VEQNNB(1),ERRFLAG)

        !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

        if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
           select case(IRDUM)
           case(1)
              if (VARACD.eq.'IE') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(1),ERRFLAG)
              end if
           case(2)
              if (VARACD.eq.'CR') then
                 IRDUM = 3
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(1),ERRFLAG)
              end if
           case(5)
              if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                   &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(1),ERRFLAG)
              end if
           case(7)
              if (VARACD.eq.'OC') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(1),ERRFLAG)
                 if (ISPEC.ne.8888) then
                    IRDUM = 5
                    call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                         &VEQNNB(1),ERRFLAG)
                 end if
              end if
           end select
        end if

        if (ISPEC.ne.8888) then
           do I = 1,MAXSP
              VEQNNB(I) = '           '
           end do
           if (LNOTBK(2))KEYWRD = '        '
           write(JOSTND,13016)KEYWRD
        else
           if (LNOTBK(2))KEYWRD = '        '
           if (LKECHO)write(JOSTND,13021)KEYWRD
        end if
     end if
     goto 10
     13006 format(/A8,'   INVALID VOLUME EQUATION NUMBER: THE',&
          &' DEFAULT CUBIC FT EQUATION NUMBERS WILL BE USED FOR ALL ',&
          &'SPECIES:')
     !       IF(LKECHO)WRITE(JOSTND,13021)KEYWRD
     13020 format(/A8,'   NATIONAL CRUISE SYSTEM CUBIC FT ',&
          &'VOLUME EQUATION NUMBERS CHANGED FOR ALL SPECIES:')
     13016 format(/A8,'   INVALID VOLUME EQUATION NUMBER: THE',&
          &' DEFAULT BOARD FT EQUATION NUMBERS WILL BE USED FOR ALL ',&
          &'SPECIES:')
     !       IF(LKECHO)WRITE(JOSTND,13021)KEYWRD
     13021 format(/A8,'   NATIONAL CRUISE SYSTEM BOARD FT VOLUME ',&
          &'EQUATION NUMBERS CHANGED FOR ALL SPECIES:')

     !  SINGLE SPECIES PROCESSING

  else

     !  CHECK FOR VALID EQUATION NUMBER CUBIC FT

     if (LNOTBK(2)) then
        VEQNNC(IS) = KARD(2)
        ISPEC = 9999
        PROD = '02'
        call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
             &VEQNNC(IS),ERRFLAG)

        !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

        if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
           select case(IRDUM)
           case(1)
              if (VARACD.eq.'IE') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(IS),ERRFLAG)
              end if
           case(2)
              if (VARACD.eq.'CR') then
                 IRDUM = 3
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(IS),ERRFLAG)
              end if
           case(5)
              if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                   &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(IS),ERRFLAG)
              end if
           case(7)
              if (VARACD.eq.'OC') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNC(IS),ERRFLAG)
                 if (ISPEC.ne.8888) then
                    IRDUM = 5
                    call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                         &VEQNNC(IS),ERRFLAG)
                 end if
              end if
           end select
        end if

        if (ISPEC.ne.8888) then
           VEQNNC(IS) = '          '
           KARD(2) = '***INVALID'
           write(JOSTND,13030)KEYWRD,KARD(1)(1:3),IS
        else
           if (LKECHO)write(JOSTND,13030)KEYWRD,KARD(1)(1:3),IS
        end if
     end if

     !  CHECK FOR VALID EQUATION NUMBER BOARD FT

     if (LNOTBK(3)) then
        VEQNNB(IS) = KARD(3)
        ISPEC = 9999
        PROD = '01'
        call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
             &VEQNNB(IS),ERRFLAG)

        !  CHECK FOR CASES WHERE VARIANTS CROSS REGIONAL BOUNDARIES

        if ((KODFOR.le.0).and.(ISPEC.ne.8888)) then
           select case(IRDUM)
           case(1)
              if (VARACD.eq.'IE') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(IS),ERRFLAG)
              end if
           case(2)
              if (VARACD.eq.'CR') then
                 IRDUM = 3
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(IS),ERRFLAG)
              end if
           case(5)
              if ((VARACD.eq.'CA').or.(VARACD.eq.'NC').or.&
                   &(VARACD.eq.'SO').or.(VARACD.eq.'OC')) then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(IS),ERRFLAG)
              end if
           case(7)
              if (VARACD.eq.'OC') then
                 IRDUM = 6
                 call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                      &VEQNNB(IS),ERRFLAG)
                 if (ISPEC.ne.8888) then
                    IRDUM = 5
                    call VOLEQDEF(VARACD,IRDUM,FORDUM,DIST,ISPEC,PROD,&
                         &VEQNNB(IS),ERRFLAG)
                 end if
              end if
           end select
        end if

        if (ISPEC.ne.8888) then
           VEQNNB(IS) = '           '
           KARD(3) = '***INVALID'
           if (.not.LNOTBK(2))write(JOSTND,13030)KEYWRD,KARD(1)(1:3),IS
        else
           if (.not.LNOTBK(2).and.LKECHO)&
                &write(JOSTND,13030)KEYWRD,KARD(1)(1:3),IS
        end if
     end if
     13030 format(/A8,'   NATIONAL CRUISE SYSTEM VOLUME EQUATION ',&
          &'NUMBERS CHANGED FOR SPECIES=',A,' (CODE=',I3,');')
  end if
  if (LNOTBK(2).and.LNOTBK(3)) then
     if (LKECHO)write(JOSTND,13040)KARD(2),KARD(3)
     13040 format(T9,'   CUBIC FOOT = ',A10,'   BOARD FOOT = ',A10)
  else if (LNOTBK(2)) then
     if (LKECHO)write(JOSTND,13041)KARD(2)
     13041 format(T9,'   CUBIC FOOT = ',A10)
  else if (LNOTBK(3)) then
     if (LKECHO)write(JOSTND,13042)KARD(3)
     13042 format(T9,'   BOARD FOOT = ',A10)
  else
     write(JOSTND,13043)
     13043 format(T9,' EQUATION NUMBERS NOT SPECIFIED.',&
          &'  REQUEST CANCELLED.')
  end if
  go to 10

  !  ==========  OPTION NUMBER 131: POINTREF ==========================POINTREF

  13100 continue
  if (LNOTBK(1)) ITHNPI = IFIX(ARRAY(1)+0.5)
  if (ITHNPI.lt.0.or. ITHNPI.gt.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     write(JOSTND,13105)
     13105 format(T12,'INVALID POINT INDICATOR. POINT INDICATOR BEING ',&
          &'RESET TO 1 (POINT NUMBERS FROM DATA) FOR FURTHER PROCESSING.')
     ITHNPI = 1
     go to 10
  end if
  if (ITHNPI.eq.1.) then
     if (LKECHO)write(JOSTND,13110)KEYWRD
     13110 format(/A8,'   POINT NUMBERS ARE AS INPUT IN THE TREE DATA.')
  else
     if (LKECHO)write(JOSTND,13111)KEYWRD
     13111 format(/A8,'   POINT NUMBERS ARE THE FVS SEQUENTIAL POINT ',&
          &'NUMBERS.')
  end if
  go to 10

  !  ==========  OPTION NUMBER 132: ECHO  ============================ECHO

  13200 continue
  LKECHO = .true.
  if (IRECNT.gt.JRECNT) then
     JRECNT = IRECNT-JRECNT-1
  else
     JRECNT = IRECNT
  end if
  write(JOSTND,13210)KEYWRD,JRECNT
  13210 format(/A8,'   THERE WERE',I8,' KEYWORD RECORDS PROCESSED.',&
       &' KEYWORD ECHO WILL RESUME.')
  go to 10

  !  ==========  OPTION NUMBER 133: NOECHO ===========================NOECHO

  13300 continue
  LKECHO = .false.
  JRECNT = IRECNT
  write(JOSTND,13310)KEYWRD
  13310 format(/A8,'   KEYWORDS WILL **NOT** BE ECHOED TO THE MAIN',&
       &' OUTPUT FILE.')
  go to 10

  !  ==========  OPTION NUMBER 134: CYCLEAT ===========================CYCLEAT

  13400 continue
  NEWYR = IFIX(ARRAY(1))
  if (NEWYR.le.0) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
  else
     if (IWORK1(1).eq.0) then
        IWORK1(1) = 1
        IWORK1(2) = NEWYR
     else
        I = IWORK1(1)+1
        J = 0
        do K = 2,I
           if (IWORK1(K).eq.NEWYR) then
              J = K
              exit
           end if
        end do
        if (J.eq.0) then
           IWORK1(1) = I
           IWORK1(I+1) = NEWYR
        end if
     end if
     if (LKECHO)write(JOSTND,13410) KEYWRD,NEWYR
     13410 format(/A8,'   REQUESTED YEAR FOR A CYCLE =',I4)
  end if
  go to 10

  !  ==========  OPTION NUMBER 135: ATRTLIST  ==========================ATRTLIST

  13500 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  NP = 2
  if (LNOTBK(4)) NP = 3
  if (.not.LNOTBK(2)) ARRAY(2) = FLOAT(JOLIST)
  call OPNEW (KODE,IDT,198,NP,ARRAY(2))
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,13510) KEYWRD,IDT,ARRAY(2),ARRAY(3)
  13510 format (/A8,'   DATE/CYCLE=',I5,'; DATA SET REFERENCE ',&
       &'NUMBER =',F4.0,'; HEADING SUPPRESSION CODE =',F3.0/&
       &T12,'(0=WITH HEADING, OTHER VALUES=SUPPRESS HEADING).')
  goto 10

  !  ==========  OPTION NUMBER 136: THINRDSL ==========================THINRDSL

  13600 continue
  if (VARACD.ne.'NE') then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,13601)
     13601 format(/' ********   ','THIS KEYWORD IS NOT VALID FOR THIS ',&
          &'VARIANT. KEYWORD IGNORED.')
     go to 10
  end if
  ICFLAG = 236
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.le.0) goto 13610
  if (IPRMPT.ne.2) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,25)
  else
     call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,IPRMPT,&
          &IRECNT,ICYC)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
  end if
  goto 10
  13610 continue
  if (LNOTBK(2)) then
     PRMS(1) = ARRAY(2)*M2pHAtoFT2pACR
  else

     !        REQUIRED RESIDUAL IS MISSING.

     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !   PROCESS REMAINING PARAMETER FIELDS

  PRMS(2) = EFF
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)

  !   SPECIES CODE PROCESSING.

  call SPDECD (4,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) go to 10
  PRMS(3) = IS
  PRMS(4) = 0.0
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)*CMtoIN
  PRMS(5) = 999.0
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)*CMtoIN
  if (PRMS(5) .le. PRMS(4)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if
  PRMS(6) = 0.
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)
  if (PRMS(6).eq.0..and. PRMS(2).gt.0.)PRMS(2) = 0.
  call OPNEW (KODE,IDT,ICFLAG,6,PRMS)
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,12420) KEYWRD,IDT,PRMS(1)/M2pHAtoFT2pACR
  if (PRMS(2).eq.0.) then
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,12421)KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*INtoCM
  else
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,12422)PRMS(2),KARD(4)(1:ILEN),IS,&
          &PRMS(4)*INtoCM,&
          &PRMS(5)*INtoCM
  end if
  if (PRMS(6) .eq.0) then
     if (LKECHO)write(JOSTND,12423)
  else if (PRMS(6) .eq.1) then
     if (LKECHO)write(JOSTND,12424)
  else
     if (LKECHO)write(JOSTND,12425)
  end if
  goto 10

  !  ==========  OPTION NUMBER 137: MORTMSB  ===========================MORTMSB

  13700 continue
  if (LNOTBK(1)) then
     if (ARRAY(1).gt.0.) then
        QMDMSB = ARRAY(1)*CMtoIN
     else
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,4)
        goto 13714
     end if
  end if
  if (LNOTBK(2)) then
     if (ARRAY(2).le. -1.605.and. ARRAY(2).gt. -10.0) then
        SLPMSB = ARRAY(2)
     else
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,4)
        goto 13714
     end if
  end if
  if (LNOTBK(3)) then
     if (ARRAY(3).gt.0..and. ARRAY(3).le.1.0) then
        EFFMSB = ARRAY(3)
     else
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,4)
        goto 13714
     end if
  end if
  if (LNOTBK(4)) then
     if (ARRAY(4) .ge.0.) DLOMSB = ARRAY(4)*CMtoIN
  end if
  if (LNOTBK(5)) then
     if (ARRAY(5) .ge.0.) DHIMSB = ARRAY(5)*CMtoIN
  end if
  if (DLOMSB .ge. DHIMSB) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 13714
  end if
  if (LNOTBK(6)) then
     if (ARRAY(6).ge.1.and. ARRAY(6).le.3) then
        MFLMSB = IFIX(ARRAY(6))
     else
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,4)
        goto 13714
     end if
  end if
  if (LKECHO)write(JOSTND,13710) KEYWRD,QMDMSB*INtoCM,SLPMSB,&
       &EFFMSB,DLOMSB*INtoCM,DHIMSB*INtoCM,MFLMSB
  13710 format (/A8,'   MATURE STAND BOUNDARY: QMD =',F9.4,&
       &';  SLOPE = ',F9.4,';  MORTALITY EFFICIENCY = ',F9.4,';',/11X,&
       &'LOWER DBH (GE) = ',F6.1,';  UPPER DBH (LT) = ',F6.1,&
       &';  MORTALITY FLAG = ',I1)
  if (LKECHO .and. MFLMSB .eq.1) then
     write(JOSTND,13711)
  else if (LKECHO .and. MFLMSB .eq.2) then
     write(JOSTND,13712)
  else
     write(JOSTND,13713)
  end if
  13711 format(11X,'(MORTALITY WILL BE FROM ABOVE WITHIN THE SPECIFIED',&
       &' DBH RANGE.)')
  13712 format(11X,'(MORTALITY WILL BE FROM BELOW WITHIN THE SPECIFIED',&
       &' DBH RANGE.)')
  13713 format(11X,'(MORTALITY WILL BE SPREAD THROUGHOUT THE SPECIFIED',&
       &' DBH RANGE.)')
  goto 10

  ! RESTORE DEFAULTS

  13714 continue
  QMDMSB = 999.
  SLPMSB = 0.
  CEPMSB = 0.
  EFFMSB = 0.90
  DLOMSB = 0.
  DHIMSB = 999.
  MFLMSB = 1
  go to 10

  !  ==========  OPTION NUMBER 138: SETSITE  ===========================SETSITE

  13800 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.
  !     NOTE: A NUMERIC VALUE MUST BE PASSED OR THERE WILL BE AN ERROR
  !     COMPILING THE EXPRESSION IN GRINCR. SO IN PASSING A PLANT ASSOCIATION
  !     CODE, USERS SHOULD ENTER THE NUMERIC SEQUENCE NUMBER OF THE PA CODE.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,120,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  !  IF HABITAT TYPE WAS ENTERED, TRANSLATE IT FOR THIS VARIANT, AND
  !  PASS THE INDEX OF THE TRANSLATED VALUE.

  IHAB = 0
  if (LNOTBK(2)) then
     IDUM1 = ICL5
     IDUM2 = ITYPE
     IDUM3 = KODTYP
     PVRDUM = CPVREF
     KODTYP = IFIX(ARRAY(2))
     ICL5 = KODTYP
     CPVREF = '          '
     TPCOM = PCOM
     KARD12 = KARD(1)//KARD(2)
     call HABTYP (KARD12,ARRAY(2))
     ARRAY(2) = FLOAT(KODTYP)
     IHAB = KODTYP
     ICL5 = IDUM1
     ITYPE = IDUM2
     KODTYP = IDUM3
     CPVREF = PVRDUM
     PCOM = TPCOM
  end if

  !  BASAL AREA MAXIMUM PROCESSING

  if (.not.LNOTBK(3))ARRAY(3) = 0.

  !  SPECIES CODE PROCESSING.

  call SPDECD (4,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10

  !  SITE INDEX AND SITE INDEX FLAG PROCESSING

  if (.not.LNOTBK(5))ARRAY(5) = 0.
  if (.not.LNOTBK(6))ARRAY(6) = 0.

  !  STAND DENSITY INDEX MAXIMUM PROCESSING

  if (.not.LNOTBK(7))ARRAY(7) = 0.

  !  WRITE KEYWORD TO THE OUTPUT FILE AND SEND TO THE OPTION PROCESSOR.

  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (LKECHO)write(JOSTND,13830)KEYWRD,IDT
  13830 format (/A8,'   DATE/CYCLE=',I5)

  if (ARRAY(2).ne.0.and. LKECHO) then
     write(JOSTND,13832)IHAB
     13832 format (T12,'HABITAT TYPE=',I5)
  end if

  if (ARRAY(3).ne.0.and. LKECHO) then
     write(JOSTND,13836)ARRAY(3)
     13836 format (T12,'BASAL AREA MAXIMUM=',F5.0)
  end if

  if (ARRAY(5).ne.0.and. LKECHO) then
     write(JOSTND,13833)KARD(4)(1:ILEN),IS,ARRAY(5)
     13833 format (T12,'SPECIES=',A,' (CODE=',I3,'); SITE INDEX=',F10.1)
     if (ARRAY(6).eq.0) then
        write(JOSTND,13834)
        13834 format(T12,'SITE INDEX VALUE IS ENTERED DIRECTLY')
     else
        write(JOSTND,13835)
        13835 format(T12,'SITE INDEX VALUE ENTERED AS A PERCENT CHANGE')
     end if
  end if

  if (ARRAY(7).ne.0.and. LKECHO) then
     write(JOSTND,13837)ARRAY(7)
     13837 format (T12,'STAND DENSITY INDEX=',F5.0)
  end if

  call OPNEW (KODE,IDT,120,6,ARRAY(2))
  goto 10

  !  ==========  OPTION NUMBER 139: CLIMATE  ===========================CLIMATE

  13900 continue
  if (LKECHO)write(JOSTND,13910) KEYWRD
  13910 format (/A8,'   CLIMATE EXTENSION KEYWORDS:')
  call CLIN (DEBUG,LKECHO)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  goto 10

  !  ==========  OPTION NUMBER 1400 SDICALC ===========================SDICALC

  14000 continue
  DBHSTAGE = 0.
  if (LNOTBK(1))DBHSTAGE = ARRAY(1)*CMtoIN
  DBHZEIDE = 0.
  if (LNOTBK(2))DBHZEIDE = ARRAY(2)*CMtoIN
  if (ARRAY(3).ge.1) then
     LZEIDE = .true.
     CALCSDI = "ZEIDE"
  else
     LZEIDE = .false.
     CALCSDI = "REINEKE"
  end if
  if (LKECHO)write(JOSTND,14010) KEYWRD,CALCSDI,&
       &DBHSTAGE*INtoCM,DBHZEIDE*INtoCM
  14010 format (/A8,'   THE ',A7,' METHOD WILL BE ',&
       &'USED TO CALCULATE SDI.'/&
       &8X,'   THE MINIMUM DIAMETER USED IN REINEKE SDI CALCULATIONS = '&
       &,F6.2,' CM'/&
       &8X,'   THE MINIMUM DIAMETER USED IN ZEIDE SDI CALCULATIONS = ',&
       &F6.2,' CM')
  go to 10
  call ERRGRO (.true.,16)
  go to 10

  !  ==========  OPTION NUMBER 141: THINQFA  ===========================THINQFA

  14100 continue
  ICFLAG = 237
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  ARRAY(2) = ARRAY(2) * CMtoIN
  ARRAY(3) = ARRAY(3) * CMtoIN
  ARRAY(6) = ARRAY(6) * CMtoIN

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,ICFLAG,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if
  call SPDECD (4,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10
  PRMS(3) = IS

  !  DEFAULT PARAMETERS

  PRMS(1) = 0. ! LOWER DBH
  PRMS(2) = 24. ! UPPER DBH
  PRMS(4) = 1.4 ! Q FACTOR
  PRMS(5) = 2. ! DIA. CLASS WIDTH
  PRMS(6) = 0. ! BA, SDI, OR TPA TARGET VALUE

  if (LNOTBK(2))PRMS(1) = ARRAY(2) ! LOWER DBH
  if (LNOTBK(3))PRMS(2) = ARRAY(3) ! UPPER DBH
  if (LNOTBK(5))PRMS(4) = ARRAY(5) ! Q FACTOR
  if (LNOTBK(6))PRMS(5) = ARRAY(6) ! DIA. CLASS WIDTH
  if (LNOTBK(7))PRMS(6) = ARRAY(7) ! TARGET TPA, BA, or SDI

  !  THE SWITCH TO SET THE UNITS FOR THE TARGET ARE ON A SECOND RECORD
  !  OF THE THINQFA KEYWORD
  !  <= 0 - TPHA TARGET (TPH)
  !  <= 1 - BA TARGET
  !  >  1 - SDI TARGET

  ISETQFA = 0
  read(IREAD,'(I1)')ISETQFA
  IRECNT = IRECNT+1

  if (ISETQFA.le.0) then ! BA TARGET
     CTARGET = 'TARGET BA='
     PRMS(7) = 0
  else if (ISETQFA.le.1) then ! TPA TARGET (TPH)
     CTARGET = 'TARGET TPH='
     PRMS(7) = 1
  else if (ISETQFA.gt.1) then ! SDI TARGET
     CTARGET = 'TARGET SDI='
     PRMS(7) = 2
  end if

  call OPNEW(KODE,IDT,ICFLAG,7,PRMS(1))
  if (KODE.gt.0) goto 10
  if (LKECHO)write(JOSTND,41110)KEYWRD,IDT,&
       &PRMS(1)*INtoCM,PRMS(2)*INtoCM
  41110 format(/A8,'   DATE/CYCLE=',I5,'; MINIMUM ',F8.1,&
       &'; MAXIMUM ',F8.1)
  if (IS.ne.0) then
     ILEN = 3
     if (IS.lt.0)ILEN = ISPGRP(-IS,92)
     if (LKECHO)write(JOSTND,41114) ' SPECIES= ',KARD(4)(1:ILEN),&
          &IS,'IS'
     41114 format (T11,A,A,' (CODE= ',I3,') ',A,&
          &' TARGETED FOR THIS CUT.')
  else
     if (LKECHO)write(JOSTND,41114) ' ALL',' SPECIES',IS,'ARE'
  end if
  if (LKECHO)write(JOSTND,41117)PRMS(4),PRMS(5)*INtoCM,CTARGET,&
       &PRMS(6)
  41117 format(T12,'Q FACTOR= ',F8.3,/&
       &T12,'DIA. CLASS WIDTH= ',F6.1,/&
       &T12,A,F6.1)

  goto 10

  !  ==========  OPTION NUMBER 142: PTGROUP ===========================PTGROUP

  14200 continue

  !     FIRST CHECK TO SEE THAT THE MAXIMUM NUMBER OF POINT GROUPS HAS NOT
  !     ALREADY BEEN DEFINED.  IF SO, IGNORE THIS REQUEST.

  if (NPTGRP .ge.30) then
     if (LKECHO)write(JOSTND,14210) IRECNT,KEYWRD,KARD(1)
     14210 format (/,'CARD NUM =',I5,'; KEYWORD FIELD = ',A8,&
          &'   GROUP NAME: ',A10)
     call ERRGRO(.true.,37)
     14220 continue
     IRECNT = IRECNT+1
     read(IREAD,'(A)',end = 80) RECORD
     I = LEN_TRIM(RECORD)
     if (LKECHO)write(JOSTND,'(" SKIPPED RECORD=",A)') RECORD(:I)
     if (RECORD(I:I).eq.'&') goto 14220
     go to 10
  end if

  !     PROCESS THE POINT GROUP REQUEST. DELETE LEADING BLANKS
  !     FROM THE POINT GROUP NAME AND STORE THE LENGTH OF THE
  !     NAME

  NPTGRP = NPTGRP+1
  if (LNOTBK(1)) then ! if there is a name, then use it
     PTGNAME(NPTGRP) = ADJUSTL(KARD(1))
     ILEN = LEN_TRIM(PTGNAME(NPTGRP))
     do J = 1,ILEN
        call UPCASE(PTGNAME(NPTGRP)(J:J))
     end do
  else ! otherwise build one
     write(PTGNAME(NPTGRP),'("PTGROUP",I2.2,2X)') NPTGRP
     ILEN = 9
  end if
  IPTGRP(NPTGRP,52) = ILEN ! save the group name length
  INUM = 0 ! initialize the number of codes in the group
  KARD = ' ' ! blank out and zero out kard and arrays
  ARRAY = 0
  14230 continue
  IRECNT = IRECNT+1
  read(IREAD,'(A)',end = 80) RECORD
  ILEN = LEN_TRIM(RECORD)
  J = 0 ! j points to the first char for the new token...
  ! when j is zero, no new token is being built.
  do I = 1,ILEN ! loop over the chars in the record
     if (RECORD(I:I).eq.' ') then ! hit a blank
        if (J.eq.0) cycle ! if no token being built, go on to next char
     end if
     if (RECORD(I:I).eq.'&') goto 14230 ! read another record
     if (J.eq.0) J = I
     !        CALL UPCASE(RECORD(I:I))
     if (RECORD(I:I).eq.' '.or.I.eq.ILEN) then ! end of token (a blank or end of record)
        INUM = INUM+1 ! so, we have a token.

        !  DON'T SAVE OVER 50, COULD DO AN ERROR MSG, BUT DIDN'T

        if (INUM.gt.50) INUM = 50
        if (I.eq.ILEN) then ! store the token in kard(1)
           KARD(1) = RECORD(J:I)
        else
           KARD(1) = RECORD(J:I-1)
        end if
        J = 0
        ARRAY(1) = 0.
        read(KARD(1),'(F10.0)',ERR = 14240) ARRAY(1) ! make a number, if we can.
        14240 continue
        if (ARRAY(1).eq.0) then ! zero is illegal
           INUM = INUM-1
           cycle
        end if
        if (INUM.gt.1) then ! check for duplicates, save unique codes
           do J = 2,INUM
              if (IPTGRP(NPTGRP,J).eq.ARRAY(1)) then
                 ARRAY(1) = -999 ! if it is a dup, then set flag
                 exit
              end if
           end do
           J = 0
           if (ARRAY(1).eq.-999) then ! skip dups
              INUM = INUM-1
              cycle
           end if
        end if
        IPTGRP(NPTGRP,INUM+1) = INT(ARRAY(1))
     end if
  end do
  IPTGRP(NPTGRP,1) = INUM

  if (LKECHO)write(JOSTND,14250) KEYWRD,-NPTGRP,PTGNAME(NPTGRP),INUM,&
       &(IPTGRP(NPTGRP,J),J = 2,INUM+1)
  14250 format(/A8,'   GROUP NUMBER:',I4,'  GROUP NAME: ',A10,&
       &'  NUMBER OF POINTS IN THIS GROUP:',I3,'  POINTS:'/5(T12,10I10/))
  go to 10

  !  ==========  OPTION NUMBER 143: ORGANON  ===========================ORGANON

  14300 continue

  !  CALL THE SUBROUTINE THAT READS ORGANON CONTROL PARAMETERS FROM THE
  !  KEYWORD FILE

  select case (VARACD)

  case ('OC','OP')
     if (LKECHO)write(JOSTND,14310) KEYWRD
     14310 format (/,A8,'   START OF ORGANON KEYWORDS:')
     call ORIN (DEBUG,LKECHO,LNOTRE)

  case default
     if (LKECHO)write(JOSTND,14315) KEYWRD
     14315 format (/,A8,'   ORGANON KEYWORDS NOT RECOGNIZED IN THIS',&
          &' VARIANT')

  end select
  goto 10

  !  ==========  OPTION NUMBER 144: SPLEAVE ============================SPLEAVE

  14400 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,206,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  !     IF THE SELECTION VALUE IS NOT PRESENT;
  !     THEN:  WRITE ERROR MSG AND SKIP PROCESSING KEYWORD CARD.

  if (.not.LNOTBK(2)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !     SPECIES CODE PROCESSING.

  call SPDECD (2,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (IS.eq.-999) goto 10
  PRMS(1) = IS

  !  DEFAULT IS LEAVE THE SPECIES PRMS(2)=1.

  PRMS(2) = 1.
  if (LNOTBK(3))PRMS(2) = ARRAY(3)

  !     SCHEDULE THE ACTIVITY

  call OPNEW(KODE,IDT,206,2,PRMS)
  if (KODE .gt.0) goto 10
  ILEN = 3
  if (IS.lt.0)ILEN = ISPGRP(-IS,92)
  if (PRMS(2).gt.0) then
     if (LKECHO)write(JOSTND,14410) KEYWRD,IDT,KARD(2)(1:ILEN),IS
  else
     if (LKECHO)write(JOSTND,14420) KEYWRD,IDT,KARD(2)(1:ILEN),IS
  end if
  14410 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,' (CODE=',&
       &I3,'); WILL NOT BE INCLUDED IN THE FOLLOWING',&
       &' THINNING ACTIVITIES')
  14420 format (/A8,'   DATE/CYCLE=',I5,'; SPECIES= ',A,' (CODE=',&
       &I3,'); WILL BE INCLUDED IN THE FOLLOWING',&
       &' THINNING ACTIVITIES')
  goto 10

  !  ==========  OPTION NUMBER 145: CCADJ ============================CCADJ

  14500 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !    IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !    TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,444,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  !    IF THE FIRST PARAMETER IS NOT PRESENT THEN SET OR JUST OUTPUT
  !    THE CURRENT CORRECTION FACTOR.

  if (.not.LNOTBK(1)) then
     if (.not.LNOTBK(2)) then
        ARRAY(2) = CCCOEF
     else
        CCCOEF = ARRAY(2)
     end if
     if (LKECHO)write(JOSTND,14505) KEYWRD,ARRAY(2)
     14505 format (/A8,'   OVERLAP COEFFICIENT=',F8.6)
  else
     PRMS(1) = ARRAY(2)

     !       SCHEDULE THE ACTIVITY

     call OPNEW (KODE,IDT,444,1,PRMS)
     if (KODE.gt.0) goto 10
     if (LKECHO)write(JOSTND,14510)KEYWRD,IDT,ARRAY(2)
     14510 format (/A8,'   DATE/CYCLE=',I5,'; OVERLAP COEFFICIENT=',F8.6)
  end if
  goto 10

  !  ==========  OPTION NUMBER 146: PRMFROST  ==========================PRMFROST

  14600 continue
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,445,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  !     IF THE SELECTION VALUE IS NOT PRESENT;
  !     THEN:  WRITE ERROR MSG AND SKIP PROCESSING KEYWORD CARD.

  if (.not.LNOTBK(2)) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO (.true.,4)
     goto 10
  end if

  !     SCHEDULE THE ACTIVITY

  call OPNEW(KODE,IDT,445,1,ARRAY(2))
  if (KODE .gt.0) goto 10
  ONOFF = 'OFF'
  if (ARRAY(2) .eq.1) ONOFF = 'ON '
  if (LKECHO)write(JOSTND,14620) KEYWRD,IDT,ONOFF
  14620 format (/A8,'   DATE/CYCLE=',I5,'; PERMAFROST EFFECT ',A3)
  goto 10

end
