!======================!
!   canada/on : cuts   !
!======================!
! CANADA-ON $Id$
subroutine CUTS
  implicit none

  !  THIS SUBROUTINE IS CALLED ONCE PER CYCLE.  IT INITIALIZES THE
  !  THINNING SUMMARY ARRAYS AND THEN EXECUTES THE THINNING STRATEGY
  !  INDICATED.  CALLS **CLSSTK** AND **AUTSTK** WHICH ARE ENTRY
  !  POINTS IN SUBROUTINE **CUTSTK**.  ALSO CALLS **SDICLS**, **CCCLS**,
  !  AND **RDCLS** WHICH ARE ENTRY POINTS IN **SDICAL**

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PDEN.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'ECON.f90'
  include 'ESHOOT.f90'

  !  TYPE DECLARATIONS FOR INTERNAL VARIABLES.

  ! Variables
  real    :: SPCRT(MAXSP,3),SPCRC(MAXSP,3),SPCRES(MAXSP,3),SPCBR(MAXSP,3),SPCMR(MAXSP,3),SSNG(MAXTRE),&
       &DSNG(MAXTRE),CTCRWN(MAXTRE),TKCRWN(MAXTRE)
  real    :: BASNEW,CUTMAX,FTCUT,CRLEN,CRI,HTI,DHI,DLOW,PPROP
  real    :: FEET,PREM2,P,DMAX,REMTPA,DIFF,ORGWK4,TREERD,TEMP,CWDI
  real    :: CRBASE,CUT,PREM,XMORE,XLEFT,THIS,TOTCUT,XSZ,V,D,SUMD2,CLKNT
  integer :: IOLDCR,ISPEC,IMETH,IPRUN,NPRUN,IM,INDX,ISPG,ISPC,IDMR
  integer :: INEG,IPOS,IG,IULIM,IGRP,IP,IS,IDMCUT,ICUT,JSPCUT,JTYP
  real    :: SDIC,STAGEA,STAGEB,CUTEF1,CSDI,CBA,CTPA,VALMAX,VALMIN,CSTOCK
  real    :: HTHI,HTLO,DBHHI,DBHLO,RSTOCK,REMOVE,FULSTK,STOCK,CUTEFF
  real    :: TARGET,TREES,CMCUT,TSUMD2,TCLKNT,PRLOST,PRDSNG,PRCRWN
  real    :: BACUT,BFCUT,CFCUT,TCUT,ZSDI
  integer :: IPCHK,NPS,KUT,JPNUM,KDT,IACTK,NP,NTODO,IC,J,ISPCUT
  integer :: KUTNOW,IT,IVAC,I
  logical :: LBAREA,LBELOW,LSPECL,LNOAUT,DEBUG,LPDBH,LNOCUT(MAXTRE)
  logical :: LSDI,LPRTND
  real    :: PRFMIS(MAXSP,6),LOSS,SALVTPA
  integer :: ISHAG
  logical :: LYARD,LINCL,LPTALL

  !  ACTIVITY ARRAYS

  integer :: MYACTS(21)
  real    :: PRMS(7)
  data MYACTS/200,201,202,203,&
       &222,223,224,225,226,227,228,229,230,231,232,233,234,&
       &235,236,&
       &248,249/

  !  DEFINITIONS OF IMPORTANT VARIABLES:

  !  ACTIVITIES:
  !     200 = MINHARV
  !     201 = SPECPREF
  !     202 = TCONDMLT
  !     203 = YARDING   (CURRENTLY TIED TO THE FIRE MODEL)
  !     222 = THINAUTO  (221 ALSO INDICATES THINAUTO)

  !     223 = THINBTA
  !     224 = THINATA
  !     225 = THINBBA
  !     226 = THINABA
  !     227 = THINPRSC
  !     228 = THINDBH
  !     229 = SALVAGE (ABANDONED, CAN REUSE THIS SLOT. GED 11.23.05)
  !     230 = THINSDI
  !     231 = THINCC
  !     232 = THINHT
  !     233 = THINMIST
  !     234 = THINRDEN
  !     235 = THINPT
  !     236 = THINRDSL

  !     248 = SETPTHIN
  !     249 = PRUNE

  !  LAUTON = LOGICAL FLAG TO DETECT PRESENCE OF THINAUTO KEYWORD IN
  !           THE RUNSTREAM.
  !           .TRUE.  IF THINAUTO KEYWORD IS PRESENT
  !           .FALSE. OTHERWISE
  !  LBAREA = LOGICAL FLAG IF BASAL AREA THINNING IS BEING ATTEMPTED
  !           .TRUE.  IF THINABA, THINBBA, OR THINDBH TO A BA TARGET
  !           .FALSE. OTHERWISE
  !  LBELOW = LOGICAL FLAG IF THINNING FROM BELOW IS BEING ATTEMPTED
  !           .TRUE.  IF THINAUTO, THINBTA, OR THINBBA
  !           .TRUE.  IF THINSDI, THINCC, THINRDEN, OR THINPT FROM BELOW
  !           .FALSE. OTHERWISE
  !   LINCL = LOGICAL VARIABLE USED TO INDICATE WHETHER THE TREE, BASED
  !           ON SPECIES, GETS CONSIDERED IN THE CUT OR NOT.
  !           .TRUE.  IF THE TREE SHOULD BE CONSIDERED BASED ON SPECIES
  !           .FALSE. OTHERWISE
  !  LSPECL = LOGICAL FLAG FOR THINNINGS WHICH DO NOT REQUIRE A SORTED
  !           REMOVAL PRIORITY LIST
  !           .TRUE.  IF THINPRSC, SALVAGE, THINDBH, THINHT, OR
  !                   THINSDI, THINCC, OR THINRDEN THAT IS THROUGHOUT
  !                   THE STAND, OR THINPT THROUGHOUT THE POINT.
  !           .FALSE. OTHERWISE
  !  LNOAUT = LOGICAL FLAG TO SUPPRESS AUTOMATIC THINNINGS WHEN OTHER
  !           THINNINGS ARE PRESENT
  !           .TRUE.  IF AUTOMATIC THINNING IS TO BE SUPRESSED
  !           .FALSE. OTHERWISE
  !  LPDBH  = LOGICAL FLAG TO INDICATE SIZE TERM IN REMOVAL PRIORITY
  !           .TRUE.  IF DBH IS TO BE USED AS THE SIZE TERM
  !           .FALSE. IF HT  IS TO BE USED AS THE SIZE TERM
  !  LNOCUT = LOGICAL FLAG TO INDICATE TREE RECORD IS OUTSIDE THE
  !           SPECIFIED CUTTING CLASS DBH/HT/SPECIES RANGE
  !           .TRUE.  IF THE TREE RECORD IS EXEMPT FROM CUTTING
  !           .FALSE. IF THE TREE RECORD CAN BE CONSIDERED FOR CUTTING
  !  LSDI   = LOGICAL FLAG TO INDICATE THAT AN SDI, CC, RD OR PT THINNING
  !           IS BEING ATTEMPTED.
  !           .TRUE.  IF THINSDI, THINCC, THINRDEN, OR THINPT IS BEING ATTEMPTED
  !           .FALSE. OTHERWISE
  !  LYARD  = LOGICAL FLAG TO INDICATE THAT YARDING KEYWORD IS ACTIVE
  !           .TRUE.  IF YARDING ENABLED; LOSSES MAY OCCUR
  !           .FALSE. IF YARDING ABSENT; REMOVALS ARE PERFECT
  !  LPRTND = LOGICAL FLAG WHICH INDICATES WHETHER 'PRETEND' MODE IS
  !           ACTIVE. THIS FEATURE IS PART OF THE ECON EXTENSION.
  !  LPTALL = LOGICAL FLAG TO INDICATE THAT ALL POINTS ARE BEING THINNED
  !           WITH THE THINPT KEYWORD (SETPTHIN, PRMS(1)=0)

  !  CFMIN  = MINIMUM HARVEST IN MERCHANTABLE CUBIC FEET
  !  BFMIN  = MINIMUM HARVEST IN MERCHANTABLE BOARD FEET
  !  BAMIN  = MINIMUM HARVEST IN SQUARE FEET OF BASAL AREA
  ! TCFMIN  = MINIMUM HARVEST IN TOTAL CUBIC FEET

  !  THE FOLLOWING VARIABLES ARE SUMS FOR ALL THINS IN THE CYCLE
  !   CFCUT = TOTAL CUBIC FEET CUT
  !   BFCUT = TOTAL BOARD FEET CUT
  !   BACUT = TOTAL BASAL AREA CUT
  !   TCUT  = TOTAL TREES PER ACRE CUT
  !   CMCUT = TOTAL MERCHANTABLE CUBIC FEET CUT

  !  TREES  = TOTAL TREES PER ACRE BEFORE ANY THINNINGS
  !  ICFLAG = CUTTING FLAG SET TO ACTIVITY NUMBER - 220
  !  RSTOCK = RESIDUAL STOCKING AFTER THIN
  !  STOCK  = AMOUNT OF STOCKING IN TPA OR BA ON A STAND LEVEL
  !           WHICH IS AVAILABLE TO BE CUT
  !  CSTOCK = AMOUNT OF STOCKING IN TPA OR BA IN A SPECIFIED CLASS
  !           WHICH IS AVAILABLE TO BE CUT
  !  TARGET = AFTER THINNING TARGET IN TPA OR BA
  !  REMOVE = AMOUNT OF STOCKING IN TPA OR BA WHICH NEEDS TO BE
  !           REMOVED IN THE THIN
  !  CUTEFF = CUTTING EFFICIENCY
  !  KUTNOW = CODE FROM THINPRSC KEYWORD WHICH INDICATES WHICH TREES
  !           ARE BEING REMOVED DURING CURRENT PRESCRIPTION THIN
  !  DBHLO  = LOWER DBH CLASS BOUNDARY FOR THIN (GE)
  !  DBHHI  = UPPER DBH CLASS BOUNDARY FOR THIN (LT)
  !  HTLO   = LOWER HT  CLASS BOUNDARY FOR THIN (GE)
  !  HTHI   = UPPER HT  CLASS BOUNDARY FOR THIN (LT)
  !  ISPCUT = SPECIES BEING CUT, OR SPECIES CLASS FOR THIN
  !  IDMCUT = DWARF MISTLETOE RATING BEING CUT FOR THINMIST
  !  VALMIN = LOWER DBH OR HT CLASS BOUNDARY (GE)
  !  VALMAX = UPPER DBH OR HT CLASS BOUNDARY (LT)
  !  CTPA   = RESIDUAL CLASS TARGET TPA FOR THINDBH OR THINHT
  !  CBA    = RESIDUAL CLASS TARGET BA  FOR THINDBH OR THINHT

  !  AUTMIN = THINAUTO LOWER LIMIT
  !  AUTMAX = THINAUTO UPPER LIMIT
  !  AUTEFF = THINAUTO CUTTING EFFICIENCY

  !  WORK ARRAYS:
  !     WK2 = STORES REMOVAL PRIORITY DURING TRIAL THINNING, AND
  !           STORES TOTAL CF VOLUME REMOVED IN FINAL PASS
  !     WK3 = STORES REMOVED TREES PER ACRE
  !     WK4 = STORES CURRENT TREES PER ACRE REPRESENTATION OF TREE
  !           RECORD, ADJUSTED AS THINNINGS ARE DONE
  !     WK5 = STORES REMOVED MERCH CF VOLUME
  !     WK6 = STORES REMOVED MERCH BF VOLUME

  !  THE FOLLOWING ARRAYS STORE VALUES BY SPECIES BY IMC CLASS
  !    SPCRT  = REMOVED TREES PER ACRE
  !    SPCRC  = REMOVED TOTAL CUBIC FOOT VOLUME
  !    SPCRES = RESIDUAL TREES PER ACRE FOLLOWING THINNINGS
  !    SPCBR  = REMOVED MERCH BOARD FOOT VOLUME
  !    SPCMR  = REMOVED MERCH CUBIC FOOT VOLUME

  !  THE FOLLOWING ARRAYS STORE VALUES BY THE NOMINAL PERCENTILE
  !  DISTRIBUTION VALUES (10,30,50,70,90,100, & TOTAL)
  !    ONTREM = REMOVED TREES PER ACRE
  !    OCVREM = REMOVED TOTAL CUBIC FOOT VOLUME
  !    ONTRES = RESIDUAL TREES PER ACRE FOLLOWING THINNINGS
  !    OBFREM = REMOVED MERCH BOARD FOOT VOLUME
  !    OMCREM = REMOVED MERCH CUBIC FOOT VOLUME

  !  ARRAYS RELATED TO YARDLOSS AND PRUNE KEYWORDS
  !     SSNG  = SNAGS/ACRE CREATED BY CUTTING, BUT LEFT STANDING
  !     DSNG  = SNAGS/ACRE CREATED BY CUTTING, DOWN LEFT IN STAND
  !     CTCRWN= TREES/ACRE REMOVED BUT WHOSE CROWNS ARE STILL IN THE
  !             STAND. LATER, IN THE PRUNE PROCESSING, THIS IS
  !             THE PROPORITON OF THE CROWN THAT HAS BEEN PRUNED.
  !     TKCRWN= TREES/ACRE CROWNS TAKEN OUT OF THE STAND

  !  SEE IF WE NEED TO DO SOME DEBUG. SET VARIANT.

  call DBCHK (DEBUG,'CUTS',4,ICYC)
  if (DEBUG) write (JOSTND,9000)
  9000 format (/' IN CUTS: VALUES OF PROB (15/LINE):'/)
  if (DEBUG) write (JOSTND,9010) (PROB(I),I = 1,ITRN)
  9010 format (1X,12F10.4)

  !  IF TREES GET HERE MARKED FOR DELETION, OR THEY GET HERE WITH ZERO
  !  PROB FROM PREVIOUS CYCLE MORTALITY, DELETE THOSE TREE RECORDS.

  IVAC = 0
  if (ITRN .gt.0) then
     call RDPSRT(ITRN,PROB,IND2,.true.)
     do I = 1,ITRN
        IT = IND2(I)
        if (IT.le.0) then
           IVAC = IVAC+1
        else if (PROB(IT).le.1E-10) then
           IVAC = IVAC+1
           IND2(I) = -IT
        end if
        5 continue
     end do
     if (DEBUG) write (JOSTND,9091) IVAC,ITRN
     9091 format (/' IN CUTS, TOTALLY DEAD TREES=',I5,'; TOT TREES=',I5)
     if (IVAC.le.0) go to 6
     call TREDEL (IVAC,IND2)
     call SPESRT
     IFST = 1
     if (ITRN.le.0) then

        !  CHECK FOR REQUEST OF SALVAGE ACTION FOR THE SPECIAL CASE WHERE ALL
        !  TREES IN THE STAND WERE KILLED

        call FMSALV (IY(ICYC),SALVTPA)
        if ( DEBUG ) then
           write (JOSTND,9013) SALVTPA
           9013 format(' ','IN CUTS, FOLLOWING CALL TO FMSALV',&
                &' FOR SPECIAL CASE WHERE ALL TREES IN STAND ARE KILLED:',&
                &' SALVTPA=',F7.2)
           go to 1900
        end if
     end if
     IND(1) = 0
     call RDPSRT(ITRN,DBH,IND,.true.)
     6 continue
  end if

  !  INITIALIZE REMOVAL SUMMARY ARRAYS AND LOGICAL FLAGS:

  TSUMD2 = 0.
  TCLKNT = 0.
  ASTPAR = 0.
  ASBAR = 0.
  KUTNOW = 0
  LPDBH = .true.
  LNOAUT = .false.
  ISPCUT = 0
  do I = 1,7
     ONTREM(I) = 0.0
     OCVREM(I) = 0.0
     ONTRES(I) = 0.0
     OBFREM(I) = 0.0
     OMCREM(I) = 0.0
     PRMS(I) = 0.
     10 continue
  end do
  do I = 1,MAXSP
     do J = 1,3
        SPCRT(I,J) = 0.0
        SPCRC(I,J) = 0.0
        SPCRES(I,J) = 0.0
        SPCBR(I,J) = 0.0
        SPCMR(I,J) = 0.0
        20 continue
     end do
     30 continue
  end do

  ! ZERO OUT THE REMOVED TREES (WK3), SNAG ARRAYS, YARDLOSS ARRAY

  do IC = 1,MAXTRE
     WK3(IC) = 0.
     SSNG(IC) = 0.
     DSNG(IC) = 0.
     CTCRWN(IC) = 0.
     TKCRWN(IC) = 0.
     YRDLOS(IC) = 0.
     31 continue
  end do

  !  SET YARDLOSS ACTIVE WITH DEFAULTS IF FIRE IS MODEL ACTIVE

  call FMATV(LYARD)
  PRLOST = 0.
  PRDSNG = 0.
  PRCRWN = 1.

  !  IF CHEAPO WAS REQUESTED CALL **ECVOLS**

  if (LECON) call ECVOLS

  !  CALL ECON EXTENSION ROUTINE TO LOAD ECON RELATED HARVEST/THIN
  !  INFORMATION. THIS INFO IS PRE-HARVEST. ALSO CALL ROUTINE TO
  !  DETERMINE WHETHER ECON 'PRETEND' HARVEST MODE IS ACTIVE THIS CYCLE.
  !  CHECK ECON EXTENSION FOR "PRETEND" MODE - ENTRY IN ECSTATUS

  call GETISPRETENDACTIVE(LPRTND)
  if (DEBUG) write (JOSTND,*) 'ECON PRETEND MODE= ',LPRTND

  !  PROCESS SALVAGE CUTS

  call FMSALV (IY(ICYC),SALVTPA)
  if (DEBUG) then
     write (JOSTND,9015) SALVTPA
     9015 format(' ','IN CUTS, FOLLOWING FMSALV CALL: SALVTPA=',F7.2)
  end if

  !  SUPPRESS THINAUTO OPTION IF OTHER THINNINGS ARE SCHEDULED.
  !  THIS DOES NOT INCLUDE SETPTHIN OR PRUNING.
  !  ACTIVITIES 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233,
  !             234, 235, 236,

  call OPFIND(14,MYACTS(6),NTODO)
  if (NTODO.gt.0) LNOAUT = .true.

  !  CHECK FOR UPDATE OF MINHARV PARAMETERS.
  !  ACTIVITY 200

  call OPFIND (1,MYACTS(1),NTODO)
  if (NTODO.le.0) go to 40
  call OPGET (NTODO,4,KDT,IACTK,NP,PRMS)
  if (IACTK.lt.0) go to 40
  call OPDONE(NTODO,IY(ICYC))
  CFMIN = PRMS(1)
  BFMIN = PRMS(2)
  BAMIN = PRMS(3)
  TCFMIN = PRMS(4)
  40 continue

  !  PROCESS MISTLETOE CUTTING PREFERENCE KEYWORD.

  call MISCPF(PRFMIS)

  !  DETERMINE IF THINNING RELATED ACTIVITIES ARE SCHEDULED THIS CYCLE.
  !  ACTIVITIES 201,202,203,222,223,224,225,226,227,228,229,230,231,232,
  !             233,234,235,236,248

  call OPFIND (19,MYACTS(2),NTODO)
  if (DEBUG) write (JOSTND,9020) NTODO,(MYACTS(I),I = 2,18)
  9020 format (' IN CUTS:  NTODO=',I4,'; MYACTS=',18I5)
  if (NTODO.le.0) go to 1950
  if (DEBUG) then
     do I = 1,MAXPLT
        if (PTBAA(I).eq.0..and. PCCF(I).eq.0..and. PTPA(I).eq.0.)&
             &go to 701
        write(JOSTND,*)' I,IPVEC(I),PTBAA,PCCF,PTPA = ',&
             &I,IPVEC(I),PTBAA(I),PCCF(I),PTPA(I)
        701 continue
     end do
  end if

  !  INITIALIZE TRIAL THINNING VECTOR AND CONTROL VARIABLES.

  IACTK = 0
  50 continue
  if (ITRN.le.0) go to 100
  do I = 1,ITRN
     WK4(I) = PROB(I)
     90 continue
  end do
  100 continue
  CFCUT = 0.0
  BFCUT = 0.0
  BACUT = 0.0
  TCUT = 0.0
  CMCUT = 0.0
  TREES = ONTCUR(7)
  JPNUM = 0
  LPTALL = .false.

  !  BEGIN PROCESSING THINNING OPTIONS.

  do KUT = 1,NTODO
     LBELOW = .false.
     LSPECL = .false.
     LBAREA = .false.
     LSDI = .false.
     do I = 1,7
        PRMS(I) = 0.
        101 continue
     end do

     !  BYPASS OPTION ASSIGNMENTS IF AUTOMATIC THINNING IS IN EFFECT.
     !  NOTE: THINAUTO ASSIGNED ACTIVITY 221 AFTER 1950 CONTINUE

     if (IACTK.eq.221) go to 150
     call OPGET (KUT,7,KDT,IACTK,NPS,PRMS)
     if (DEBUG) write (JOSTND,9030) KDT,IACTK,NPS,(PRMS(I),I = 1,NPS)
     9030 format (' IN CUTS:  KDT=',I5,'; IACTK=',I4,'; NPS=',I2,&
          &'; PRMS=',10F8.2)
     if (IACTK.lt.0) go to 1400

     !  PROCESS SETPTHIN KEYWORD. IF POINT NUMBER IS INVALID, CANCEL
     !  KEYWORD REQUEST. IF IT IS VALID, FIND THE FVS
     !  SEQUENTIAL POINT NUMBER FOR FURTHER PROCESSING.

     if (IACTK.eq.248) then
        JPNUM = IFIX(PRMS(1)+0.5)
        if (JPNUM.le.0) then
           LPTALL = .true.
           goto 12813
        end if
        do IPCHK = 1,IPTINV
           if (ITHNPI .eq.1) then
              if (JPNUM .eq. IPVEC(IPCHK)) then
                 JPNUM = IPCHK
                 go to 12813
              end if
           else
              if (JPNUM .le. IPTINV) go to 12813
           end if
        end do
        call OPDEL1(KUT)
        go to 1400
        12813 continue
        if (PRMS(1).ge.0.)ITHNPN = IFIX(PRMS(1))
        if (PRMS(2).gt.0.)ITHNPA = IFIX(PRMS(2))
        call OPDONE(KUT,IY(ICYC))
        if (DEBUG)write(JOSTND,*)' SETPTHIN PROCESSING ITHNPN,ITHNPA,',&
             &'JPNUM,IPTINV= ',ITHNPN,ITHNPA,JPNUM,IPTINV
        go to 1400
     end if

     !  RESET THE DATE, SET THE CUTTING FLAG, SET THE CUTTING
     !  EFFICIENCY, AND THE TARGET.  IF THE AUTO THIN WAS SPECIFIED,
     !  AUTMAX, AUTMIN, AND AUTEFF, ARE IN PRMS(1-3).

     ICFLAG = IACTK-220
     if (ICFLAG .ne.15)JPNUM = 0

     !  BRANCH TO END OF THINNING LOOP TO PROCESS SPECPREF OR
     !  TCONDMLT REQUESTS.

     if (ICFLAG.le.0) go to 1150

     !  BRANCH TO PRUNING SECTION; NOTE: LNOAUT IS .TRUE.

     if (ICFLAG.gt.28) go to 1950

     !  SAVE THE PARAMETERS FOR THE THINAUTO OPTION.

     if (ICFLAG.eq.2) then
        AUTMIN = PRMS(1)
        AUTMAX = PRMS(2)
        AUTEFF = PRMS(3)
        LAUTON = .true.
        call OPDONE(KUT,IY(ICYC))
        go to 1400
     end if

     !  IF TREE LIST IS EMPTY, CANCEL THE CUTTING OPTION.

     if (ITRN.gt.0) go to 125
     call OPDEL1 (KUT)
     go to 1400

     125 continue
     TARGET = PRMS(1)
     if (TARGET.lt.0.)TARGET = 0.
     if (NPS.gt.1)CUTEFF = PRMS(2)
     STOCK = TREES-TCUT
     go to (150,200,200,225,250,275,300,350,300,400,400,350,450,400,&
          &475,400 ),ICFLAG
     150 continue
     ICFLAG = 1
     STOCK = TREES-TCUT

     !  AUTOMATIC THINNING.

     CUTEFF = AUTEFF

     !  CALL AUTSTK TO CALCULATE FULSTK FOR AUTO THIN.
     !  FULSTK IS NORMAL FULL STOCKING IN STEMS PER ACRE.

     call AUTSTK (FULSTK)

     !  THINNING WILL NOT BE EXECUTED IF STOCK IS LESS THAN AUTMAX
     !  PERCENT OF NORMAL.

     REMOVE = 0.0
     if (STOCK.lt.(AUTMAX/100.0)*FULSTK) go to 2000
     RSTOCK = (AUTMIN/100.0)*FULSTK
     REMOVE = STOCK-RSTOCK
     if (REMOVE .le.0.) go to 2000
     LBELOW = .true.
     if (DEBUG) write (JOSTND,9040) ICFLAG,STOCK,AUTMIN,AUTMAX,CUTEFF,&
          &FULSTK,RMSQD,REMOVE,RSTOCK,LBELOW
     9040 format (' ICFLAG  STOCK  AUTMIN  AUTMAX  CUTEFF  FULSTK  RMSQD ',&
          &' REMOVE  RSTOCK  LBELOW'/1X,I5,8F8.2,L8)
     go to 700

     !  SET REMOVE AND RSTOCK FOR OPTIONS THINBTA AND THINATA. IF
     !  THINBTA SET LBELOW TRUE.
     !  BRANCHING HERE FOR THINAUTO IN COMPUTED "GO TO" WON'T HAPPEN

     200 continue
     LBELOW = .true.
     225 continue
     DBHLO = PRMS(3)
     DBHHI = PRMS(4)
     HTLO = PRMS(5)
     HTHI = PRMS(6)
     LPDBH = DBHHI-DBHLO .le. HTHI-HTLO
     JTYP = 1
     JSPCUT = 0
     call CLSSTK(CSTOCK,JTYP,JSPCUT,DBHLO,DBHHI,HTLO,HTHI,0)
     RSTOCK = TARGET
     REMOVE = CSTOCK-RSTOCK
     go to 550

     !  SET STOCK, REMOVE, RSTOCK, AND LBAREA FOR OPTIONS THINBBA AND
     !  THINABA. IF THINBBA SET LBELOW TRUE.

     250 continue
     LBELOW = .true.
     275 continue
     LBAREA = .true.
     DBHLO = PRMS(3)
     DBHHI = PRMS(4)
     HTLO = PRMS(5)
     HTHI = PRMS(6)
     JTYP = 2
     JSPCUT = 0
     call CLSSTK(CSTOCK,JTYP,JSPCUT,DBHLO,DBHHI,HTLO,HTHI,0)
     RSTOCK = TARGET
     REMOVE = CSTOCK-RSTOCK
     go to 550

     !  SET LSPECL, REMOVE, AND RSTOCK FOR PRESCRIPTION AND DIAMETER
     !  LIMIT AND THINHT THINNINGS. VALMAX AND VALMIN ARE LOADED WITH
     !  MAX AND MIN DBH WHEN THINDBH IS CALLED FOR AND THEY ARE LOADED WITH
     !  MAX AND MIN HEIGHT WHEN THINHT IS CALLED FOR.

     300 continue

     !   THINPRSC OPTION.

     CUTEFF = PRMS(1)
     RSTOCK = 0.0
     REMOVE = 99999.0
     LSPECL = .true.
     if (NPS.gt.1)KUTNOW = IFIX(PRMS(2))
     go to 550

     !   THINDBH AND THINHT OPTIONS.

     350 continue
     VALMIN = PRMS(1)
     VALMAX = PRMS(2)
     if (VALMAX .lt.VALMIN) VALMAX = 9999.0
     CUTEFF = PRMS(3)
     ISPCUT = PRMS(4)
     CTPA = PRMS(5)
     if (CTPA.lt.0.)CTPA = 0.
     CBA = PRMS(6)
     if (CBA.lt.0.)CBA = 0.
     RSTOCK = 0.0
     REMOVE = 99999.0
     LSPECL = .true.

     !  COMPUTE TOTAL REMOVAL AND CUTTING EFFICIENCY FOR A DIAMETER-
     !  SPECIES CLASS WHEN RESIDUAL TPA OR BA IS SPECIFIED FOR THE CLASS.
     !  CTPA AND CBA ARE SET TO .GE. 0 IN INITRE; CBA IS ALSO SET TO 0
     !  WHEN CTPA GT 0.

     if (CTPA.eq.0.0.and. CBA.eq.0.0) go to 360
     CSTOCK = 0.0
     JTYP = 1
     if (CBA.gt.0.)JTYP = 2
     if (ICFLAG.eq.8) then
        call CLSSTK(CSTOCK,JTYP,ISPCUT,VALMIN,VALMAX,0.,999.,0)
     else if (ICFLAG.eq.12) then
        call CLSSTK(CSTOCK,JTYP,ISPCUT,0.,999.,VALMIN,VALMAX,0)
     end if
     if (CSTOCK.le.0.0) go to 360
     RSTOCK = CTPA+CBA
     REMOVE = CSTOCK-RSTOCK
     if (CBA.gt.0.0) LBAREA = .true.
     CUTEFF = REMOVE/CSTOCK
     if (CUTEFF.gt.1.0) CUTEFF = 1.0

     !  RESET THE STORED PARAMETERS FOR CUTEFF.

     PRMS(3) = MAX(0.0,CUTEFF)
     call OPCHPR (KUT,6,PRMS)
     360 continue
     if (DEBUG) write(JOSTND,9050) CTPA,CBA,CSTOCK,REMOVE,CUTEFF,&
          &VALMIN,VALMAX
     9050 format(' IN CUTS: CTPA=',F8.2,' CBA=',F8.2,' CSTOCK=',F8.2,&
          &' REMOVE=',F8.2,' CUTEFF=',F8.2,' VALMIN=',F8.2,&
          &' VALMAX=',F8.2)
     go to 550

     !   THINSDI  OPTION (ICFLAG=10)
     !   THINCC   OPTION (ICFLAG=11)
     !   THINRDEN OPTION (ICFLAG=14)
     !   THINPT   OPTION (ICFLAG=15)
     !   THINRDSL OPTION (ICFLAG=16)

     400 continue

     if (DEBUG)write(JOSTND,*)' AFTER STATEMENT 400-JPNUM= ',JPNUM
     !  IF THIS IS A THINRDSL AND THE SILVAH FOREST TYPE IS NOT ONE WHERE THE
     !  SILVAH RELATIVE DENSITY IS VALID, THEN CANCEL THE THINNING.
     !  IF THIS IS A POINT THINNING TO A SILVAH RD TARGET AND THE FOREST TYPE
     !  IS NOT A DEFINED SILVAH FOREST TYPE, THEN CANCEL THE THINNING.

     if ((ICFLAG.eq.16.and. ISILFT.eq.0) .or. (ICFLAG.eq.15.and.&
          &ITHNPA.eq.6.and. ISILFT.eq.0)) then
        call OPDEL1(KUT)
        go to 1400
     end if

     CSDI = PRMS(1)
     if (CSDI.lt.0.)CSDI = 0.
     CUTEF1 = PRMS(2)
     ISPCUT = PRMS(3)
     VALMIN = PRMS(4)
     DBHLO = PRMS(4)
     VALMAX = PRMS(5)
     DBHHI = PRMS(5)
     ICUT = PRMS(6)
     HTLO = 0.
     HTHI = 999.
     RSTOCK = 0.
     REMOVE = 99999.
     LSDI = .true.
     LPDBH = .true.
     LSPECL = .true.
     if (ICUT .gt.0) LSPECL = .false.
     if (ICUT .eq.1) LBELOW = .true.

     !  IF CLASS RESIDUAL TARGET IS GREATER THAN ZERO, DETERMINE CURRENT
     !  STOCKING IN THE CLASS.

     if (CSDI .gt.0.) then
        if (ICFLAG.eq.10.or. (ICFLAG.eq.15.and. ITHNPA.eq.3)) then
           call SDICLS(ISPCUT,VALMIN,VALMAX,2,SDIC,ZSDI,&
                &STAGEA,STAGEB,JPNUM)
           if (LZEIDE)SDIC = ZSDI
        else if (ICFLAG.eq.11.or. (ICFLAG.eq.15.and. ITHNPA.eq.4)) then
           if (CSDI.lt.100.) then
              CSDI = (43560.0*ALOG(1.0-(CSDI/100.))/0.785398)*(-1.0)
              call CCCLS(ISPCUT,VALMIN,VALMAX,2,SDIC,JPNUM)
           else
              call OPDEL1(KUT)
              goto 1400
           end if
        else if (ICFLAG.eq.14.or. (ICFLAG.eq.15.and. ITHNPA.eq.5)) then
           call RDCLS(ISPCUT,VALMIN,VALMAX,2,SUMD2,CLKNT,SDIC,JPNUM)
           TSUMD2 = SUMD2
           TCLKNT = CLKNT
        else if (ICFLAG.eq.16.or. (ICFLAG.eq.15.and. ITHNPA.eq.6)) then
           call RDCLS2(ISPCUT,VALMIN,VALMAX,2,SDIC,JPNUM)
        else if (ICFLAG.eq.15.and. ITHNPA.eq.1) then
           call CLSSTK(SDIC,1,ISPCUT,VALMIN,VALMAX,0.,999.,JPNUM)
        else if (ICFLAG.eq.15.and. ITHNPA.eq.2) then
           call CLSSTK(SDIC,2,ISPCUT,VALMIN,VALMAX,0.,999.,JPNUM)
           LBAREA = .true.
        end if
        IF(DEBUG)WRITE(JOSTND,*)' ISPCUT,VALMIN,VALMAX,SDIC,CSDI,ICFLAG,&
             &  JPNUM= ',ISPCUT,VALMIN,VALMAX,SDIC,CSDI,ICFLAG,JPNUM

        !  IF CURRENT STOCKING EXCEEDS THE TARGET, SET TARGET RESIDUAL STOCKING,
        !  AMOUNT TO REMOVE, AND CUTTING EFFICIENCY.

        if (SDIC .gt. CSDI) then
           RSTOCK = CSDI
           REMOVE = SDIC-CSDI
           if (DEBUG)write(JOSTND,*)' RSTOCK,REMOVE,ICUT= ',&
                &RSTOCK,REMOVE,ICUT
           CUTEFF = REMOVE/SDIC
           if (CUTEFF .gt.1.) CUTEFF = 1.
           if (CUTEF1.lt. CUTEFF) then
              PRMS(2) = MAX(0.,CUTEFF)
              call OPCHPR(KUT,6,PRMS)
           else
              if (ICUT.gt.0)CUTEFF = CUTEF1
           end if
        else
           REMOVE = 0.
        end if

        !  IF CLASS RESIDUAL TARGET IS ZERO, THEN SET TARGET RESIDUAL STOCKING
        !  TO ZERO, SET AMOUNT TO REMOVE TO THE CURRENT STOCKING IN THE CLASS,
        !  AND SET THE CUTTING EFFICIENCY TO 1.

     else
        if (ICFLAG.eq.10.or. (ICFLAG.eq.15.and. ITHNPA.eq.3)) then
           call SDICLS(ISPCUT,VALMIN,VALMAX,2,SDIC,ZSDI,&
                &STAGEA,STAGEB,JPNUM)
           if (LZEIDE)SDIC = ZSDI
        else if (ICFLAG.eq.11.or. (ICFLAG.eq.15.and. ITHNPA.eq.4)) then
           call CCCLS(ISPCUT,VALMIN,VALMAX,2,SDIC,JPNUM)
        else if (ICFLAG.eq.14.or. (ICFLAG.eq.15.and. ITHNPA.eq.5)) then
           call RDCLS(ISPCUT,VALMIN,VALMAX,2,SUMD2,CLKNT,SDIC,JPNUM)
           TSUMD2 = SUMD2
           TCLKNT = CLKNT
        else if (ICFLAG.eq.16.or. (ICFLAG.eq.15.and. ITHNPA.eq.6)) then
           call RDCLS2(ISPCUT,VALMIN,VALMAX,2,SDIC,JPNUM)
        else if (ICFLAG.eq.15.and. ITHNPA.eq.1) then
           call CLSSTK(SDIC,1,ISPCUT,VALMIN,VALMAX,0.,999.,JPNUM)
        else if (ICFLAG.eq.15.and. ITHNPA.eq.2) then
           call CLSSTK(SDIC,2,ISPCUT,VALMIN,VALMAX,0.,999.,JPNUM)
           LBAREA = .true.
        end if
        REMOVE = SDIC
        RSTOCK = 0.
        CUTEFF = 1.
     end if
     if (DEBUG)write(JOSTND,*)' CUTTING EFFICIENCY =',CUTEFF
     go to 550

     !   THINMIST OPTION (ICFLAG=12)

     450 continue
     IDMCUT = IFIX(PRMS(1))
     DBHLO = PRMS(2)
     DBHHI = PRMS(3)
     if (DBHHI .lt.DBHLO) DBHHI = 9999.0
     CUTEFF = PRMS(4)
     RSTOCK = 0.0
     REMOVE = 99999.0
     LSPECL = .true.
     go to 550

     !   THINPT OPTION (ICFLAG=15)

     475 continue

     !  IF THE POINT NUMBER OR THINNING ATTRIBUTE HAS NOT BEEN ENTERED,
     !  CANCEL THE REQUEST.

     if (((ITHNPN.eq.0).and.(.not.LPTALL)).or.(ITHNPA.eq.0)) then
        call OPDEL1(KUT)
        goto 1400
     end if

     ! PROCESS THE THINPT REQUEST:

     if (ITHNPA .le.5) then
        if (LPTALL)JPNUM = JPNUM+1
        go to 400
     else
        call OPDEL1(KUT)
        goto 1400
     end if

     ! 500 CONTINUE

     !   ** FREE SPOT **

     !     REMOVE=0.0

     !  WRITE DEBUG OUTPUT IF DESIRED.

     550 continue
     if (.not.DEBUG) go to 600
     write(JOSTND,9060) ICYC,ICFLAG,STOCK,RSTOCK,REMOVE,LBAREA,&
          &LBELOW,LSPECL,KUTNOW,JPNUM
     9060 format(' IN CUTS, CYCLE=',I3,' ICFLAG=',I2,' STOCK=',F8.1,&
          &' RSTOCK=',F8.1,' REMOVE=',F8.1,' LBAREA=',L1,&
          &' LBELOW=',L1,' LSPECL=',L1,' KUTNOW=',I2,' JPNUM=',I5)
     600 continue

     ! IF REMOVE LESS THAN OR EQUAL TO 0.0, DELETE AND PROCESS THE NEXT.

     if ((REMOVE.gt.0.0).or.(LPTALL.and.(JPNUM.lt.IPTINV))) go to 650
     call OPDEL1(KUT)
     go to 1400
     650 continue

     !  SET THE CUTTING OPTION STATUS TO ACCOMPLISHED.  IF THE MIN.
     !  HARVEST RESULTS IN THE TOTAL CUT NOT BEING ACCOMPLISHED, THEN
     !  THE STATUS IS RESET TO 'DELETED'.

     call OPDONE(KUT,IY(ICYC))
     700 continue

     !  LOAD ARRAY WK2 WITH PRIORITIES FOR REMOVAL.

     do I = 1,ITRN
        IS = ISP(I)
        D = DBH(I)
        IP = ITRE(I)
        V = D
        if (ICFLAG .eq.12) V = HT(I)
        LNOCUT(I) = .false.

        !  SET LINCL

        LINCL = .false.
        if (JPNUM.gt.0.and. ITRE(I).ne.JPNUM)go to 710
        if (ISPCUT.eq.0.or. ISPCUT.eq.ISP(I)) then
           LINCL = .true.
        else if (ISPCUT.lt.0) then
           IGRP = -ISPCUT
           IULIM = ISPGRP(IGRP,1)+1
           do IG = 2,IULIM
              if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                 LINCL = .true.
                 go to 710
              end if
              705 continue
           end do
        end if
        710 continue

        !  GET MISTLETOE RATING FOR CURRENT TREE RECORD.

        call MISGET(I,IDMR)

        !  SET REMOVAL PRIORITY FOR THINMIST REQUESTS

        if (ICFLAG.eq.13) then
           LNOCUT(I) = D.lt.DBHLO.or.D.ge.DBHHI .or.&
                &(IDMR.ne.IDMCUT .and. IDMCUT.ne.0) .or.&
                &IDMR.eq.0
           WK2(I) = 1.0
           if (LNOCUT(I))WK2(I) = 0.0
           go to 850
        end if

        !  GO TO STATEMENT 750 TO PROCESS PRESCRIPTIONS AND DIAMETER LIMIT
        !  THINNINGS.

        if (LSPECL) go to 750

        !  IF THINNING FROM BELOW, REVERSE THE SIGN OF THE TREE SIZE TERM
        !  IN THE THINNING PRIORITY FORMULA.

        if (LPDBH) then
           XSZ = DBH(I)
        else
           XSZ = HT(I)
        end if
        if (LBELOW) XSZ = -XSZ
        WK2(I) = XSZ + IORDER(IS) + TCWT * IMC(I) + SPCLWT*ISPECL(I)&
             &+ PBAWT*PTBAA(IP) + PCCFWT*PCCF(IP) + PTPAWT*PTPA(IP)
        if (DEBUG)write(JOSTND,*)' I,WK2,XSZ,IORDER,IS,TCWT,IMC,SPCLWT,',&
             &'ISPECL,IP,PBAWT,PTBAA,PCCFWT,PCCF,PTPAWT,PTPA= ',I,WK2(I),XSZ,&
             &IORDER(IS),IS,TCWT,IMC(I),SPCLWT,ISPECL(I),IP,PBAWT,PTBAA(IP),&
             &PCCFWT,PCCF(IP),PTPAWT,PTPA(IP)
        if (IDMR.gt.0) WK2(I) = WK2(I)+PRFMIS(IS,IDMR)

        if (ICFLAG.eq.1) go to 850
        LNOCUT(I) = DBH(I).lt.DBHLO.or.DBH(I).ge.DBHHI .or.&
             &HT (I).lt.HTLO .or.HT (I).ge.HTHI

        !  IF THINNING BY SDI, CC, RDEN, RDSL, OR PT, AND THE TREE IS NOT OF THE
        !  RIGHT SPECIES OR ON THE RIGHT POINT, FLAG TREE AS A NO CUT TREE.

        if ((ICFLAG.eq.10.or. ICFLAG.eq.11.or. ICFLAG.eq.14.or.&
             &ICFLAG.eq.15.or. ICFLAG.eq.16) .and..not.LINCL)&
             &LNOCUT(I) = .true.

        if (DEBUG)write(JOSTND,*)' I,WK2(),LNOCUT()= ',I,WK2(I),LNOCUT(I)
        go to 850
        750 continue
        WK2(I) = 1.0
        if (ICFLAG.eq.8.or. ICFLAG .eq.12) go to 800
        if (LSDI) go to 800

        !  THINNING PRIORITY FROM PRESCRIPTION. NPS IS THE NUMBER OF
        !  PARAMETERS SPECIFIED IN THE THINPRSC KEYWORD.  IF NPS IS 2
        !  THEN CHECK KUTKOD FOR VALUES EQUAL TO KUTNOW.  IF NPS IS 1
        !  THEN CHECK KUTKOD FOR VALUES GREATER OR EQUAL TO 2.

        if (NPS.eq.1) then
           if (KUTKOD(I).lt.2) WK2(I) = 0.0
        else
           if (KUTNOW.ne.KUTKOD(I)) WK2(I) = 0.0
        end if
        go to 850

        !  DIAMETER LIMIT OR HEIGHT LIMIT THINNING.

        800 continue
        if (V.lt.VALMIN.or.V.ge.VALMAX) WK2(I) = 0.0
        if (.not.LINCL) WK2(I) = 0.0
        850 continue
     end do

     !  SORT REMOVAL PRIORITY ARRAY IN DESCENDING ORDER.  THE ARRAY IND2
     !  CONTAINS THE SUBSCRIPTS TO THE SORTED ORDER.  BYPASS THE SORT
     !  IF LSPECL IS TRUE.

     if (LSPECL) go to 900
     if (ITRN .gt.0) call RDPSRT(ITRN,WK2,IND2,.true.)
     900 continue

     !  LOOP THROUGH THE TREE LIST TO COMPUTE TRIAL THINNING STATISTICS.
     !  ADJUSTED PROBS ARE STORED IN THE VECTOR WK4 UNTIL ALL
     !  SCHEDULED THINNINGS ARE COMPLETE.  QUANTITIES OF BF VOLUME,
     !  CF VOLUME, AND BASAL AREA REMOVED PER ACRE ARE ACCUMULATED ACROSS
     !  ALL THINNINGS TO TEST FOR OVERALL COMPLIANCE WITH MINIMUM HARVEST
     !  STANDARDS.

     TOTCUT = 0.0
     THIS = 0.0
     XLEFT = 999999.
     do I = 1,ITRN

        !    IT -- SUBSCRIPT TO RECORD WITH NEXT HIGHEST PRIORITY FOR
        !          REMOVAL.  IF LSPECL IS TRUE, IT IS EQUAL TO I.  IF
        !          LSPECL IS FALSE, IT IS EQUAL TO IND2(I).
        !  PREM -- NUMBER OF TREES PER ACRE REMOVED BY THINNING TREE IT.
        !   CUT -- STOCKING REMOVED BY THINNING TREE IT, STATED IN THE
        !          APPROPRIATE UNITS (BASAL AREA OR TREES PER ACRE).
        !  DMAX -- DIAMETER OF LARGEST TREE RECORD FOR WHICH SOME PORTION
        !          WAS REMOVED.

        IT = IND2(I)
        if (.not.LSPECL) go to 950
        IT = I
        IND2(I) = I
        950 continue
        D = DBH(IT)
        XMORE = 0.

        !  IF REMOVE-TOTCUT IS LESS THAN OR EQUAL TO ZERO, THINNING IS COMPLETE.
        !  ZERO OUT WK2 FOR REMAINING TREES.

        if (DEBUG)write(JOSTND,*)' LSDI,ICFLAG,XLEFT,REMOVE,TOTCUT= ',&
             &LSDI,ICFLAG,XLEFT,REMOVE,TOTCUT
        if (LSDI .and. (ICFLAG.eq.10.or. ICFLAG.eq.14.or. ICFLAG.eq.16&
             &.or. (ICFLAG.eq.15.and. (ITHNPA.eq.3.or. ITHNPA.eq.5.or.&
             &ITHNPA.eq.6)))) then
           if (XLEFT .gt.0.)go to 1000
        else
           if (REMOVE-TOTCUT.gt.0.0) go to 1000
        end if
        WK2(IT) = 0.0
        go to 1100

        !  BRANCH TO END OF LOOP IF LNOCUT IS TRUE OR IF WK2(IT)=0
        !  AND LSPECL IS TRUE.

        1000 if (WK2(IT).le.0.0.and.LSPECL) go to 1100
        if (LNOCUT(IT)) go to 1100

        !  DETERMINE THE AMOUNT OF EACH TREE CUT AND COLLECT REMOVAL
        !  SUMMARY INFORMATION.

        PREM = WK4(IT)*CUTEFF
        if (PREM.gt.WK4(IT)) PREM = WK4(IT)
        if (PREM.le.0.0) go to 1100
        CUT = PREM
        if (LBAREA) CUT = PREM*D*D*0.005454154
        if (ICFLAG.eq.15.and. (ITHNPA.eq.1.or. ITHNPA.eq.2))&
             &CUT = CUT*(PI-FLOAT(NONSTK))
        XLEFT = REMOVE-(TOTCUT+CUT)
        if (LSDI) then
           if (ICFLAG.eq.10.or. (ICFLAG.eq.15.and. ITHNPA.eq.3)) then
              call SDICLS(ISPCUT,VALMIN,VALMAX,2,SDIC,ZSDI,&
                   &STAGEA,STAGEB,JPNUM)
              if (LZEIDE)SDIC = ZSDI
              if (DEBUG)write(JOSTND,*)' SDI THINNING, TREE= ',IT,' DBH= ',&
                   &D,' RANGE= ',VALMIN,VALMAX,' WK4= ',WK4(IT),' CUTEFF= ',&
                   &CUTEFF,' PREM= ',PREM,' EXISTING SDI IN CLASS= ',SDIC,&
                   &' PARAMETERS= ',STAGEA,STAGEB
              CUT = PREM*(STAGEA+STAGEB*D*D)
              XLEFT = SDIC-(CSDI+CUT)
           else if (ICFLAG.eq.11.or. (ICFLAG.eq.15.and. ITHNPA.eq.4)) then
              CWDI = CRWDTH(IT)
              CUT = PREM*CWDI*CWDI
              XLEFT = REMOVE-(TOTCUT+CUT)
           else if (ICFLAG.eq.14.or. (ICFLAG.eq.15.and. ITHNPA.eq.5)) then
              TEMP = TSUMD2-PREM*DBH(IT)*DBH(IT)
              CUT = SDIC-(TEMP*0.0054542)/(TEMP/(TCLKNT-PREM))**0.25
              XLEFT = SDIC-(CSDI+CUT)
           else if (ICFLAG.eq.16.or. (ICFLAG.eq.15.and. ITHNPA.eq.6)) then
              call RDSLTR(ISP(IT),IT,TREERD)
              CUT = PREM*TREERD
              XLEFT = SDIC-(CSDI+CUT)
           end if
        end if
        if (DEBUG)write(JOSTND,*)' IT,WK4,CUTEFF,PREM,REMOVE,TOTCUT,CUT,',&
             &'XLEFT= ',IT,WK4(IT),CUTEFF,PREM,REMOVE,TOTCUT,CUT,XLEFT
        if (LSDI .and. (ICFLAG.eq.10.or. ICFLAG.eq.14.or. ICFLAG.eq.16&
             &.or. (ICFLAG.eq.15.and. (ITHNPA.eq.3.or. ITHNPA.eq.5.or.&
             &ITHNPA.eq.6)))) then
           XMORE = SDIC-CSDI
           if (DEBUG)write(JOSTND,*)' IT,SDIC,CSDI,XMORE= ',&
                &IT,SDIC,CSDI,XMORE
           if (XMORE.gt.CUT)go to 1050
           go to 955
        end if
        if (XLEFT.ge.0.0) go to 1050

        !  THIS IS THE LAST TREE TO REMOVE.
        !  THINSDI OR THINRDEN PREM NEEDS TO BE DETERMINED ITERATIVELY

        955 continue
        if (LSDI .and. (ICFLAG.eq.10.or. (ICFLAG.eq.15.and. ITHNPA.eq.3))&
             &) then
           PREM = XMORE/(STAGEA+STAGEB*D*D)
           if (PREM .gt. WK4(IT))PREM = WK4(IT)
           if (DEBUG)write(JOSTND,*)' IT,D,XMORE,CUT,STAGEA,STAGEB,EST PRE',&
                &'M= ',IT,D,XMORE,CUT,STAGEA,STAGEB,PREM
           IPOS = 0
           INEG = 0
           ORGWK4 = WK4(IT)
           1045 continue
           if (IPOS.gt.0.and. INEG.gt.0)go to 1046
           WK4(IT) = ORGWK4-PREM
           call SDICLS(ISPCUT,VALMIN,VALMAX,2,SDIC,ZSDI,&
                &STAGEA,STAGEB,JPNUM)
           if (LZEIDE)SDIC = ZSDI
           DIFF = SDIC-CSDI
           if (DEBUG)write(JOSTND,*)' ITERATIVE THINSDI IT,SDIC WITH EST PR'&
                &,'EM,CSDI,DIFF,',&
                &'PREM,ORGWK4,WK4,STAGEA,STAGEB,IPOS,INEG,JPNUM= ',&
                &IT,SDIC,CSDI,DIFF,PREM,ORGWK4,WK4(IT),STAGEA,STAGEB,IPOS,INEG,&
                &JPNUM
           if (ABS(DIFF).gt.0.5) then
              if (DIFF.lt.0.) then
                 PREM = PREM-0.05
                 INEG = 1
                 go to 1045
              else
                 PREM = PREM+0.05
                 IPOS = 1
                 go to 1045
              end if
           end if
           1046 continue
           WK4(IT) = ORGWK4
        else if (LSDI .and. (ICFLAG.eq.14.or.&
             &(ICFLAG.eq.15.and. ITHNPA.eq.5))) then
           PREM = (XMORE/CUT)*PREM
           if (PREM .gt. WK4(IT))PREM = WK4(IT)
           if (DEBUG)write(JOSTND,*)' IT,D,XMORE,CUT,PREM= ',&
                &IT,D,XMORE,CUT,PREM
           IPOS = 0
           INEG = 0
           ORGWK4 = WK4(IT)
           1047 continue
           if (IPOS.gt.0.and. INEG.gt.0)go to 1048
           WK4(IT) = ORGWK4-PREM
           call RDCLS(ISPCUT,VALMIN,VALMAX,2,SUMD2,CLKNT,SDIC,JPNUM)
           DIFF = SDIC-CSDI
           if (DEBUG)write(JOSTND,*)' ITERATIVE THINRDEN IT,SDIC WITH EST P'&
                &,'REM,CSDI,DIFF,',&
                &'PREM,ORGWK4,WK4,IPOS,INEG,JPNUM= ',&
                &IT,SDIC,CSDI,DIFF,PREM,ORGWK4,WK4(IT),IPOS,INEG,JPNUM
           if (ABS(DIFF).gt.0.01) then
              if (DIFF.lt.0.) then
                 PREM = PREM-0.2
                 INEG = 1
                 go to 1047
              else
                 PREM = PREM+0.2
                 IPOS = 1
                 go to 1047
              end if
           end if
           1048 continue
           WK4(IT) = ORGWK4
        else
           PREM = ((XLEFT+CUT)/CUT)*PREM
        end if
        CUT = REMOVE-TOTCUT

        !  INCREMENT THE VALUE OF TCUT.

        1050 TOTCUT = TOTCUT+CUT
        TSUMD2 = TSUMD2-PREM*DBH(IT)*DBH(IT)
        TCLKNT = TCLKNT-PREM
        if (DEBUG)write(JOSTND,*)'IT,CUT,TOTCUT,TSUMD2,TCLKNT= ',&
             &IT,CUT,TOTCUT,TSUMD2,TCLKNT

        !  IF YARDING LOSSES ARE BEING ACCOUNTED FOR, SOME OF THE
        !  CUT IS PLACED IN SEPARATE POOLS AND DOES
        !  NOT BECOME PART OF THE USUAL REMOVAL STATISTICS.

        if (LYARD) then
           LOSS = PREM * PRLOST
           DSNG(IT) = DSNG(IT) + LOSS * PRDSNG
           SSNG(IT) = SSNG(IT) + LOSS * (1.0 - PRDSNG)
           CTCRWN(IT) = CTCRWN(IT) + (PRCRWN * (PREM - LOSS)) +&
                &(LOSS * PRDSNG)
           TKCRWN(IT) = TKCRWN(IT) + ((1.0 - PRCRWN) * (PREM - LOSS))
        else
           CTCRWN(IT) = CTCRWN(IT) + PREM
        end if
        WK4(IT) = WK4(IT)-PREM
        YRDLOS(IT) = YRDLOS(IT)+PREM*PRLOST
        if (VARACD.eq.'ON' .or.&
             &VARACD.eq.'CS' .or. VARACD.eq.'LS' .or.&
             &VARACD.eq.'NE' .or.&
             &VARACD.eq.'SN') then
           CFCUT = CFCUT+PREM*(1.-PRLOST)*CFV(IT)
        else
           CFCUT = CFCUT+PREM*CFV(IT)
        end if
        CMCUT = CMCUT+PREM*(1.-PRLOST)*WK1(IT)
        BFCUT = BFCUT+PREM*(1.-PRLOST)*BFV(IT)
        BACUT = BACUT+PREM*D*D*0.005454154
        TCUT = TCUT+PREM
        THIS = THIS+PREM
        if (DEBUG) write (JOSTND,9070) IT,TOTCUT,CUT,WK4(IT),CUTEFF,PREM,&
             &PRLOST,PRDSNG,CFCUT,BFCUT,DSNG(IT),SSNG(IT),CTCRWN(IT),YRDLOS(IT)
        9070 format (' IT=',I4,' TOTCUT=',F10.3,' CUT=',F10.3,' WK4(IT)=',F8.3,&
             &' CUTEFF=',F8.3,' PREM=',F8.3,' PRLOST=',F5.3,' PRDSNG=',F5.3/8X,&
             &' CFCUT=',F9.2,' BFCUT=',F9.2,' DSNG=',F7.2,' SSNG=',F7.2,&
             &' CTCRWN=',F7.2,' YRDLOS=',F10.5)
        1100 continue
     end do

     !   END TREE LOOP

     go to 1350
     1150 continue

     !  PROCESS SPECPREF AND TCONDMLT REQUESTS.

     call OPDONE(KUT,IY(ICYC))
     if (IACTK.eq.201) go to 1200
     if (IACTK.eq.203) go to 1325

     !     OPTION 202... TCONDMLT

     TCWT = PRMS(1)
     SPCLWT = PRMS(2)
     PBAWT = PRMS(3)
     PCCFWT = PRMS(4)
     PTPAWT = PRMS(5)
     go to 1400

     !     OPTION 201... SPECPREF

     1200 continue

     !     IF SPECIES CODE IS:
     !        < 0 THEN IT APPLIES TO SPECIES IN A GROUP
     !        = 0 THEN IT APPLIES TO ALL SPECIES
     !        > 0 THEN IT APPLIES TO JUST THIS SPECIES

     ISPC = IFIX(PRMS(1))
     if (ISPC .lt.0) then
        IGRP = -ISPC
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           ISPG = ISPGRP(IGRP,IG)
           IORDER(ISPG) = PRMS(2)
           1210 continue
        end do
     else if (ISPC.eq.0) then
        do IS = 1,MAXSP
           IORDER(IS) = PRMS(2)
           1220 continue
        end do
     else
        IORDER(ISPC) = PRMS(2)
     end if
     go to 1400

     !     OPTION 203... YARDLOSS

     1325 continue
     LYARD = .true.
     PRLOST = PRMS(1)
     PRDSNG = PRMS(2)
     PRCRWN = PRMS(3)
     go to 1400

     1350 continue

     !  THE FOLLOWING IF-THEN CONSTRUCT MANAGES THINING ALL POINTS (PRM(I)=0
     !  WITH THE THINPT /SETPTHIN KEYWORDS

     if (DEBUG)write(JOSTND,*)'LPTALL,JPNUM,IPTINV=',LPTALL,JPNUM,IPTINV
     if (LPTALL.and.(JPNUM.lt.IPTINV)) then
        JPNUM = JPNUM+1
        goto 400
     else
        LPTALL = .false.
     end if

     if (THIS.le.0.0) call OPDEL1(KUT)

     1400 continue
  end do

  !  IF THERE WERE NO REMOVALS, BYPASS THE CALCULATION OF
  !  DISTRIBUTION STATISTICS.

  if (TCUT.le.0.0)  go to 1950

  !  IF TRIAL THINNINGS RESULT IN A TRIVIAL TPA REMAINING IN THE STAND,
  !  THEN ADJUST TRIAL THINNINGS SO ALL TPA IN THE STAND ARE REMOVED.
  !  ADDITIONAL YARDING LOSSES ARE IGNORED SINCE THE TPA ARE SO SMALL.

  REMTPA = TREES - TCUT
  if (REMTPA .gt.0..and. REMTPA .lt.0.01) then
     do INDX = 1,ITRN
        if (WK4(INDX) .gt.0.) then
           CFCUT = CFCUT+WK4(INDX)*CFV(INDX)
           CMCUT = CMCUT+WK4(INDX)*WK1(INDX)
           BFCUT = BFCUT+WK4(INDX)*BFV(INDX)
           BACUT = BACUT+WK4(INDX)*DBH(INDX)*DBH(INDX)*0.005454154
           TCUT = TCUT+WK4(INDX)
           WK4(INDX) = 0.
        end if
        1405 continue
     end do
  end if

  !  TRIAL THINNINGS COMPLETED.  TEST FOR COMPLIANCE WITH THE MINIMUM
  !  HARVEST STANDARDS. IF ECON 'PRETEND' MODE IN EFFECT THEN DO
  !  NOT ACTUALLY PERFORM THE THINNING.

  if (DEBUG) write (JOSTND,9080) BACUT,BAMIN,CMCUT,CFMIN,&
       &BFCUT,BFMIN,CFCUT,TCFMIN
  9080 format (' IN CUTS: BACUT,BAMIN=',2F10.3,&
       &'; CMCUT,CFMIN=',2F10.2,';'/,10x,'BFCUT,BFMIN=',2F10.2,&
       &'; CFCUT,TCFMIN=',2F10.2)

  if (BACUT.ge.BAMIN .and. CMCUT.ge.CFMIN .and. BFCUT.ge.BFMIN&
       &.and. CFCUT.ge.TCFMIN .and..not. LPRTND) go to 1500

  !  FLAG THINNINGS AS DELETED (NO COMPLIANCE WITH MINIMUM).  CUT WILL ALSO
  !  BE CANCELED IF ECON 'PRETEND' MODE IS ACTIVE.

  if (ICFLAG.eq.1) go to 2000
  do I = 1,NTODO
     call OPGET (I,7,KDT,IACTK,NPS,PRMS)
     if (IABS(IACTK).gt.222) call OPDEL1(I)
     1450 continue
  end do

  ! RESET YARD LOSS ACCUMULATOR TO 0.

  do I = 1,MAXTRE
     YRDLOS(I) = 0.
     1455 continue
  end do

  !    INSURE THAT REMOVAL INFO IS NOT LOADED INTO THE ECON EXTENSION
  !    IF REMOVAL WAS CANCELED BECAUSE MINIMUM REQUIREMENTS WERE NOT
  !    MET.

  if (.not. LPRTND .or. BACUT.lt.BAMIN .or. CMCUT.lt.CFMIN .or.&
       &BFCUT.lt.BFMIN .or. CFCUT.lt.TCFMIN) go to 1950

  1500 continue

  !  TRIAL THINNING IS ACCEPTABLE.  ONE FINAL PASS THROUGH THE TREE
  !  LIST IS NEEDED TO EFFECT REMOVALS AND SUMMARIZE THE RESULTS.

  DMAX = 0.0
  IVAC = 0
  do I = 1,ITRN
     IT = IND2(I)
     IS = ISP(IT)
     IM = IMC(IT)
     WK2(IT) = 0.0
     WK5(IT) = 0.0
     WK6(IT) = 0.0
     D = DBH(IT)

     !  PREM IS THE DIFFERENCE BETWEEN ORIGINAL DENSITY AND THE TOTAL OF
     !  MERCH REMOVALS AND ALL YARDING LOSSES. IF YARDING IS INACTIVE
     !  THEN THE DSNG/SSNG POOLS WILL BE ZERO.

     PREM = PROB(IT)-WK4(IT)
     if (LPRTND) goto 1640

     !  IF THE RESIDUAL FOR THE TREE RECORD IS NEARLY ZERO, THEN:
     !  SIGNAL THAT THE TREE BE DELETED AND CUT THE ENTIRE TREE.

     P = PROB(IT)-PREM
     if (P .gt.0.0005) go to 1550
     PREM = PROB(IT)
     PROB(IT) = 0.0
     if (LYARD)YRDLOS(IT) = YRDLOS(IT)+P*PRLOST
     IVAC = IVAC+1
     IND2(I) = -IT
     go to 1600
     1550 continue

     !  IF THE PROBABILITY OF REMOVAL IS TRIVIAL, DON'T CUT THE TREE

     if (PREM.lt.0.00001) go to 1650
     PROB(IT) = P
     1600 continue
     if (D.gt.DMAX) DMAX = D

     !  IF YARDING LOSSES ARE ACCOUNTED FOR, REMOVE THE AMOUNT
     !  SUCCESSFULLY REMOVED FROM THE STAND BY THE PROPORTIONAL
     !  LOSS. NOTE THAT PREM2 IS THE REDEFINED PREM: THE TPA
     !  ACTUALLY REMOVED FROM THE STAND. THIS IS A BIT OF A KLUDGE
     !  SO THAT THE MERCH CF AND BF VOLUMES ARE THE AMOUNTS TAKEN
     !  FROM THE STAND, AND WK2 AND WK3 ACCOUNT FOR THE AMOUNTS KILLED

     1640 continue
     PREM2 = AMAX1(0.0,PREM - DSNG(IT) - SSNG(IT))

     !  CALL ECON EXTENSION TO ACCUMULATE REMOVAL BFV
     !  INFORMATION TO BE USED IN BFV REVENUE CALCULATIONS.
     !  IF WE ARE IN ECON PRETEND MODE DO NOT ACCUMULATE STATISTICS
     !  FOR THE CUT.

     if (PREM2.gt.0.00001) then
        call ECHARV (BFV(IT),D,WK1(IT),GROSPC,PREM2,IS,IT,ICYC,IY)
     end if
     if (LPRTND) goto 1700

     if (DEBUG)write(JOSTND,*)' IT,PREM,PREM2,DSNG,SSNG,YRDLOS(IT)= ',&
          &IT,PREM,PREM2,DSNG(IT),SSNG(IT),YRDLOS(IT)

     if (PREM.le.0.) then
        YRDLOS(IT) = 0.
     else
        YRDLOS(IT) = YRDLOS(IT)/PREM
     end if
     if (DEBUG)write(JOSTND,*)' IT,YRDLOS FINAL= ',IT,YRDLOS(IT)
     WK2(IT) = PREM*CFV(IT)
     WK3(IT) = PREM
     PREM2 = AMAX1(0.0,PREM - DSNG(IT) - SSNG(IT))
     WK5(IT) = PREM2*WK1(IT)
     WK6(IT) = PREM2*BFV(IT)
     SPCBR(IS,IM) = SPCBR(IS,IM)+WK6(IT)
     SPCMR(IS,IM) = SPCMR(IS,IM)+WK5(IT)
     SPCRT(IS,IM) = SPCRT(IS,IM)+WK3(IT)
     SPCRC(IS,IM) = SPCRC(IS,IM)+WK2(IT)

     !     ADD STUMPS TO LIST FOR REGENERATION SPROUTS.

     ISHAG = IFINT
     call ESTUMP (IS,D,AMAX1(0.0,PREM-SSNG(IT)),ITRE(IT),ISHAG)

     !  CALL WESTERN ROOT DISEASE MODEL VER. 3.0 SUBROUTINE TO ADD STUMPS
     !  TO STUMP LIST

     call RDSTR (IT,PREM,P+PREM)

     !  CALL WESTWIDE BEETLE MODEL SUBROUTINE TO CREATE SLASH FOR IPS

     call BMSLSH (IS,PREM,CFV(IT),0)
     1650 continue

     SPCRES(IS,IM) = SPCRES(IS,IM)+PROB(IT)

     1700 continue
  end do

  !  CALL ECON EXTENSION ROUTINE TO LOAD HARVEST/THIN ECON INFORMATION
  !  FOR REMOVED TREES. IF WE ARE IN 'PRETEND' MODE SKIP ACCUMULATION OF REMOVAL
  !  STATISTICS.

  if (LPRTND) goto 1950

  !  CALL PRTRLS TO PROCESS THE CUTLIST (ARGUMENT=2) OR AFTER TREATEMENT
  !  (ARGUMENT=3) OPTION.

  call PRTRLS (2)
  call PRTRLS (3)

  !  CALL FVSSTD TO PROCESS THE CUTLIST FOR STAND TABLE DEVELOPMENT.

  call FVSSTD (2)

  ! RESET YARD LOSS ACCUMULATOR TO 0.

  do I = 1,MAXTRE
     YRDLOS(I) = 0.
     1710 continue
  end do

  !  CALL NATCRZ TO PROCESS THE CUTLIST FOR NATIONAL CRUISE SYSTEM.

  call NATCRZ (2)

  !  PROCESS THE VISUALIZATION FOR CUTS

  call SVCUTS (IVAC,SSNG,DSNG,CTCRWN)

  !  IF CHEAPO WAS REQUESTED CALL **ECREMS**

  if (LECON) call ECREMS

  !  DETERMINE SPECIES-TREE CLASS COMPOSITION FOR REMOVED TREES,
  !  REMOVED VOLUME, AND RESIDUAL TREES.

  call COMP(OSPTT,IOSPTT,SPCRT)
  call COMP(OSPTV,IOSPTV,SPCRC)
  call COMP(OSPBR,IOSPBR,SPCBR)
  call COMP(OSPMR,IOSPMR,SPCMR)

  !  COMPUTE PERCENTILE POINTS IN THE DISTRIBUTION OF REMOVED TREES.

  call PCTILE(ITRN,IND,WK3,PCT,ONTREM(7))
  call DIST(ITRN,ONTREM,PCT)

  !  COMPUTE PERCENTILE POINTS IN THE DISTRIBUTION OF REMOVED VOLUME.

  call PCTILE(ITRN,IND,WK2,PCT,OCVREM(7))
  call DIST(ITRN,OCVREM,PCT)
  if (CMCUT.le.0.0) go to 1750
  call PCTILE(ITRN,IND,WK5,PCT,OMCREM(7))
  call DIST(ITRN,OMCREM,PCT)
  1750 continue
  if (BFCUT.le.0.0) go to 1800
  call PCTILE(ITRN,IND,WK6,PCT,OBFREM(7))
  call DIST(ITRN,OBFREM,PCT)
  1800 continue

  !  REPLACE MAXIMUM DIAMETER IN DISTRIBUTIONS FOR REMOVED TREES AND
  !  REMOVED VOLUME WITH DMAX.

  ONTREM(6) = DMAX
  OCVREM(6) = DMAX
  OMCREM(6) = DMAX
  OBFREM(6) = DMAX

  !  CALL FIRE MODEL TO UPDATE SNAGS before TREES RECORDS ARE DELETED

  if (DEBUG) write (JOSTND,'('' CUTS: CALLING FMSCUT.'')')
  call FMSCUT(SPCRC,MAXSP,3,SSNG,DSNG,CTCRWN,TKCRWN)

  !     SAVE REMOVALS FOR PROCESSING WITH THE TREEBIO EM FUNCTION.

  call FMTREM(DSNG,SSNG,TKCRWN)

  !  IF SOME OR ALL OF THE TREES ARE COMPLETELY CUT, THEN:
  !  DELETE THE CUT TREE RECORDS.

  if (DEBUG) write (JOSTND,9090) IVAC,ITRN
  9090 format (/' IN CUTS, TOTALLY CUT TREES=',I5,'; TOT TREES=',I5)
  if (IVAC.le.0) go to 1850
  call TREDEL (IVAC,IND2)
  call SPESRT
  IFST = 1
  if (ITRN.le.0) go to 1900
  IND(1) = 0
  call RDPSRT(ITRN,DBH,IND,.true.)
  1850 continue

  !  COMPUTE PERCENTILE POINTS IN THE DISTRIBUTION OF RESIDUAL TREES.

  call PCTILE(ITRN,IND,PROB,PCT,ONTRES(7))
  1900 continue
  call DIST(ITRN,ONTRES,PCT)
  call COMP(OSPRT,IOSPRT,SPCRES)
  1950 continue
  if (ITRN.le.0.or. (.not.LAUTON.or.LNOAUT)) go to 2000
  LNOAUT = .true.
  IACTK = 221
  NTODO = 1
  if (DEBUG) write (JOSTND,9100)
  9100 format (' CUTS: THINAUTO IS BEING ATTEMPTED')
  go to 50
  2000 continue

  ! PROCESS ANY PRUNING REQUESTS.

  call OPFIND(1,MYACTS(21),NTODO)
  if (DEBUG) write (JOSTND,2010) NTODO,ITRN
  2010 format (' CUTS: PRUNE, NTODO=',I4,' ITRN=',I4)
  if (NTODO .le.0)go to 2500
  do NPRUN = 1,NTODO
     call OPGET(NPRUN,7,KDT,IACTK,NP,PRMS)
     if (IACTK.lt.0)go to 2490

     ! IF NO TREES REMAIN AFTER THINNINGS, CANCEL PRUNING.
     ! OTHERWISE, INITIALIZE PRUNING PARAMETERS.

     if (ITRN.le.0) then
        call OPDEL1(NPRUN)
        go to 2490
     else
        IPRUN = 0
        IMETH = IFIX(PRMS(1))
        FEET = PRMS(2)
        PPROP = PRMS(3)
        ISPEC = PRMS(4)
        DLOW = PRMS(5)
        DHI = PRMS(6)
        if (DEBUG) write (JOSTND,2020) IMETH,FEET,PPROP,ISPEC,DLOW,DHI
        2020 format (' CUTS: PRUNE, IMETH=',I2,' FEET=',F7.2,' PPROP=',&
             &F7.2,' ISPEC=',I3,' DLOW=',F7.2,' DHI=',F7.2)
     end if

     ! ZERO OUT THE CTCRWN VECTOR...IT KEEPS TRACK OF THE PROPORTION OF THE
     ! CROWN THAT IS BEING PRUNED AND REPORTS THIS TO THE FUELS&FIRE MODEL.

     do I = 1,ITRN
        CTCRWN(I) = 0.
     end do

     ! LOOP THROUGH TREES AND ATTEMPT PRUNING.

     do I = 1,ITRN

        !  SET LINCL

        LINCL = .false.
        if (ISPEC.eq.0.or. ISPEC.eq.ISP(I)) then
           LINCL = .true.
        else if (ISPEC.lt.0) then
           IGRP = -ISPEC
           IULIM = ISPGRP(IGRP,1)+1
           do IG = 2,IULIM
              if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                 LINCL = .true.
                 go to 2022
              end if
              2021 continue
           end do
        end if
        2022 continue

        ! SKIP TREE IF IT DOESN'T MATCH SPECIFIED SPECIES OR SIZE

        if (.not.LINCL) go to 2480
        if (DBH(I).lt.DLOW .or. DBH(I).ge.DHI) go to 2480

        !  COMPUTE CROWN LENGTH AND HEIGHT TO BASE OF CROWN.

        IOLDCR = ICR(I)
        HTI = HT(I)
        CRI = FLOAT(ICR(I))
        CRLEN = HTI*CRI/100.
        CRBASE = HTI-CRLEN

        ! METHOD 1: PRUNE TO A SPECIFIED HEIGHT.

        if (IMETH.eq.1.and. FEET.gt.0.) then
           if (FEET .gt. CRBASE) then
              ICR(I) = IFIX( ((HTI-FEET)/HTI)*100. + 0.5 )
              if (ICR(I).lt.5.and. IOLDCR.gt.5) ICR(I) = 5
              IPRUN = IPRUN+1
           end if

           ! METHODS 2 & 3: PRUNE TO A SPECIFIED HEIGHT SUBJECT TO A MAXIMUM
           ! CROWN RATIO REMOVAL PERCENTAGE.  IF IN PRUNING TO THE SPECIFIED
           ! HEIGHT, THE CR REMOVAL PERCENTAGE WOULD BE EXCEEDED:
           ! METHOD 2 -- ONLY REMOVE ENOUGH CROWN TO REACH THE REMOVAL PERCENT.
           ! METHOD 3 -- DO NOT REMOVE ANY CROWN FROM THE TREE.

        else if ((IMETH.eq.2.or. IMETH.eq.3) .and.&
             &FEET.gt.0..and. PPROP.gt.0.) then
           if (FEET .gt. CRBASE) then
              FTCUT = FEET-CRBASE
              CUTMAX = CRLEN*PPROP
              if (FTCUT .le. CUTMAX) then

                 ! CR REMOVAL PERCENTAGE NOT EXCEEDED

                 ICR(I) = IFIX( ((HTI-FEET)/HTI)*100. + 0.5 )
                 IPRUN = IPRUN+1
              else

                 ! CR REMOVAL PERCENTAGE EXCEEDED

                 if (IMETH .eq.2) then
                    ICR(I) = IFIX( ((CRLEN-CUTMAX)/HTI)*100. + 0.5 )
                    IPRUN = IPRUN+1
                 end if
              end if
           end if

           ! METHOD 4: REMOVE A SPECIFIED PROPORTION OF EVERY TREES CROWN.

        else if (IMETH.eq.4.and. PPROP.gt.0.) then
           ICR(I) = IFIX( CRI*(1.0-PPROP) + 0.5 )
           IPRUN = IPRUN+1

           ! METHOD 5: REMOVE A SPECIFIED LENGTH OF CROWN FROM EVERY TREE.

        else if (IMETH.eq.5.and. FEET.gt.0.) then
           ICR(I) = IFIX( ((CRLEN-FEET)/HTI)*100. + 0.5 )
           if (ICR(I).lt.5.and. IOLDCR.gt.5) ICR(I) = 5
           IPRUN = IPRUN+1

           ! METHODS 6 & 7: REMOVE A SPECIFIED LENGTH OF CROWN SUBJECT TO A
           ! MAXIMUM CR REMOVAL PERCENTAGE.  IF IN PRUNING THE SPECIFIED
           ! LENGTH, THE CR REMOVAL PERCENTAGE WOULD BE EXCEEDED:
           !  METHOD 6 -- ONLY REMOVE ENOUGH CROWN TO REACH THE REMOVAL PERCENT.
           !  METHOD 7 -- DO NOT REMOVE ANY CROWN FROM THE TREE.

        else if ((IMETH.eq.6.or. IMETH.eq.7) .and.&
             &FEET .gt.0..and. PPROP.gt.0.) then
           CUTMAX = CRLEN*PPROP
           if (FEET .le. CUTMAX) then

              ! CR REMOVAL PERCENTAGE NOT EXCEEDED.

              ICR(I) = IFIX( ((CRLEN-FEET)/HTI)*100. + 0.5 )
              IPRUN = IPRUN+1
           else

              ! CR REMOVAL PERCENTAGE WOULD BE EXCEEDED.

              if (IMETH .eq.6) then
                 ICR(I) = IFIX( ((CRLEN-CUTMAX)/HTI)*100. + 0.5 )
                 IPRUN = IPRUN+1
              end if
           end if
        end if
        BASNEW = (1.-(FLOAT(ICR(I))*.01))*HTI
        CTCRWN(I) = AMAX1(0.,1.-((HTI-BASNEW)/(HTI-CRBASE)))
        if (DEBUG)write(JOSTND,2040) I,IOLDCR,ICR(I),IOLDCR-ICR(I),&
             &CRBASE,BASNEW,CTCRWN(I)
        2040 format (' CUTS: PRUNE, I=',I4,' IOLDCR=',I3,' ICR=',I3,&
             &' DIFF=',I3,' OLD&NEW BASE HT=',2F7.1,' CTCRWN=',F6.3)
        2480 continue
     end do

     !  IF ANY PRUNING WAS DONE, MARK REQUEST DONE AND PASS THE PRUNED MATERIAL
     !  TO THE DEBRIS POOLS.
     !  IF NOT, DELETE THE REQUEST.

     if (IPRUN .gt.0) then
        call FMPRUN(CTCRWN)
        call OPDONE(NPRUN,IY(ICYC))
     else
        call OPDEL1(NPRUN)
     end if
     if (DEBUG)write(JOSTND,2050) IPRUN
     2050 format (' CUTS: PRUNE, IPRUN=',I4)
     2490 continue
  end do

  ! END OF PRUNING SEGMENT

  2500 continue

  !  IF CHEAPO WAS REQUESTED CALL **ECOUT**

  if (LECON) call ECOUT

  return
end
