!====================!
!   wsbwe : bwesit   !
!====================!
! WSBWE $Id$
subroutine BWESIT
  implicit none

  !     INITIALIZES ONE STAND AT THE BEGINNING OF A CYCLE.

  !     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JANUARY 1984

  !     THIS ROUTINE CONVERTS A PROGNOSIS MODEL STAND REPRESENTATION
  !     TO AN EQUIVALENT BUDWORM MODEL STAND REPRESENTATION.  IT IS
  !     THE ROUTINE WHICH PASSES MOST OF THE INFORMATION FROM PROGNOSIS
  !     TO BUDWORM.

  !   major revision by K.A.Sheehan to strip out Pop.Dyn.stuff 7/96+

  !     CALLED FROM :

  !       BWECUP - SINGLE STAND BUDWORM MODEL LINK TO PROGNOSIS.

  !     FUNCTIONS CALLED :

  !       BWESLP  - LINEAR INTERPOLATION FUNCTION.

  !     SUBROUTINES CALLED :

  !       OPFIND - FIND OPTION IN OPTION LIST.
  !       OPGET  - RETRIEVE OPTION.
  !       OPDONE - SIGNAL THAT OPTION HAS BEEN ACCOMPLISHED.
  !       OPDEL1 - DELETE OPTION FROM OPTION LIST.
  !       OPADD
  !       OPINCR
  !       BWESIN - SET DO LOOP INDICES FOR DEFOL AND SETPRBIO OPTIONS.
  !       BWEBMS - COMPUTE FOLIAGE BIOMASS FOR INDIVIDUAL TREES.
  !       BWECRC - SELECT THE HEIGHT AND CROWN CLASS INDICES FOR A TREE.
  !       BWEADV - COMPUTE SUCCESSIONAL STAGE INDEX AND WEATHER PARAMETERS

  !     PARAMETERS :

  !   LCALBW = TRUE IF BUDLITE MODEL IS CALLED
  !   LDEFOL = TRUE IF USER SUPPLIES ANNUAL DEFOLIATION RATES
  !   LREGO  - TRUE IF A REGIONAL OUTBREAK IS ACTIVE, FALSE IF NOT
  !   LSPRAY - TRUE IF INSECTICIDES ARE TO BE SPRAYED IN THE CURRENT YEAR[BWECM2]

  ! Revision History:
  !   25-MAY-00 Lance David (FHTET)
  !      .Added debug handling.
  !      .Initialize local variables.
  !   30-NOV-00 Lance David (FHTET)
  !      .Added debug statement for option processing.
  !      .Added call to opdone for setprbio between statement lables 80 & 90.
  !   14-JUL-2010 Lance R. David (FMSC)
  !       Added IMPLICIT NONE and declared variables as needed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'BWESTD.f90'
  include 'BWECOM.f90'
  include 'BWECM2.f90'

  ! Variables
  logical :: LSKBIO,LPRBIN,DEBUG
  integer :: I,I1,I2,IACTK,ICRC1,ICRC2,ICRC3,ICRI,ICROWN,IDT,IELEV,IFAGE,IHOST,IS1,IS2,ISPI,ISZI,&
       &ITREE,IYR1,J,K,KODE,MYACT(8),MYALL(9),NP,NSPYR
  real    :: BIO,BWESLP,CRM,DIV,HTI,PROBI,HTSCL(6),CRNMIN(6),CRT(7,3),HTT(7,3),PRMS(1)

  data HTSCL/0.,2.5,5.0,10.,20.,22./,CRNMIN/1.,.9,.7,.5,.3,.2/
  data MYACT/2150,2152,2153,2154,2155,2156,2157,2159/
  data MYALL/2150,2151,2152,2153,2154,2155,2156,2157,2159/

  !.... Check for DEBUG

  call DBCHK(DEBUG,'BWESIT',6,ICYC)

  if (DEBUG) write (JOSTND,*) 'ENTER BWESIT: ICYC = ',ICYC

  !     ********************** EXECUTION BEGINS **************************

  !     ****NOTE****NOTE****NOTE****NOTE****NOTE****NOTE****NOTE****
  !     FVS COMMON ARRAYS WK3, WK4 AND WK6 ARE USED HERE
  !     WK6 IS LOADED WITH VALUES AND NEEDS TO BE INITIALIZED FIRST, MAYBE(?)
  !     LANCE DAVID  25-MAY-00

  do I1 = 1, MAXTRE
     WK6(I1) = 0.0
  end do

  do I1 = 1,6
     HTSCL(I1) = 0.0
     CRNMIN(I1) = 0.0
  end do

  do I1 = 1,7
     do I2 = 1,3
        CRT(I1,I2) = 0.0
        HTT(I1,I2) = 0.0
        BWTPHA(I1,I2) = 0.0
     end do
  end do

  !     RESET INITIAL STAND CONDITIONS IF NOT
  !     IN THE MIDDLE OF A CONTINUING OUTBREAK.
  !     THIS INCLUDES ALL BUDWORM MODEL VARIABLES WHICH
  !     ARE ALTERED VIA THE PROGNOSIS OPTION PROCESSOR.

  IYR1 = IY(ICYC)

  !     SET LSKBIO TRUE IF THE RDDSM1, RHTGM1, AND PRBIO ARRAYS ARE
  !     INITIALIZED OR ARE BEING CARRIED OVER FROM THE PREVIOUS CYCLE.

  LSKBIO = IY(ICYC).le.IPRBYR

  !     INITIALIZE OPTION SWITCHES

  LPRBIN = .false.
  LSPRAY = .false.
  NSPYR = 0
  HOSTST = 0.0

  !     IF DEFOL EXISTS, THEN CALLBW IS ILLEGAL.  SET LDEFOL TRUE IF DEFOL
  !     EXISTS.

  call OPFIND (1,MYALL(2),I)
  LDEFOL = I.gt.0
  if (LDEFOL) then
     LREGO = .false.
     LCALBW = .false.
  end if

  !     PROCESS ALL ACTIVITIES EXCEPT DEFOL.

  call OPFIND (8,MYACT,NTODO)
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: OPFIND NTODO=',NTODO

  !     IF THERE ARE NO ACTIVITIES, BRANCH PAST ACTIVITY PROCESSING.

  if (NTODO.le.0) goto 100

  do ITODO = 1,NTODO

     !       GET THE ACTIVITY AND SIGNAL THAT IT IS ACCOMPLISHED.

     call OPGET (ITODO,6,IDT,IACTK,NP,WK3)
     if (DEBUG) write (JOSTND,*) 'IN BWESIT: OPGET ',&
          &'ITODO,IDT,IACTK,NP,WK3=',ITODO,IDT,IACTK,NP,WK3

     !       ACTIVITIES ARE:

     !       2150 - GENDEFOL
     !       2151 - DEFOL
     !       2153 - SETPRBIO
     !       2152, 2154, 2155, 2156 - NOT CURRENTLY USED
     !       2157 - SPRAY INSECTICIDE IN A GIVEN YEAR
     !       2159 - SPRAY INSECTICIDE WHEN TRIGGER CONDITIONS ARE MET

     if (IACTK.eq.2150) goto 10
     if (IACTK.eq.2153) goto 40
     if (IACTK.eq.2157) goto 20
     if (IACTK.eq.2159) goto 30

     !       ALL OTHER ACTIVITY CODES IN MYACT LISTED BELOW ARE NOT USED.
     !       IF (IACTK.EQ.2152) GOTO 90
     !       IF (IACTK.EQ.2154) GOTO 90
     !       IF (IACTK.EQ.2155) GOTO 90
     !       IF (IACTK.EQ.2156) GOTO 90

     goto 90

     10 continue

     !       GENDEFOL
     !       IF DEFOL OPTION EXISTS, THEN DELETE GENDEFOL.

     if (LDEFOL) then
        call OPDEL1 (ITODO)
     else

        !         SET MAX YEAR, TURN ON OPTION [THIS SECTION MAY NOT BE NEC.?]

        PRMS(1) = IYREND-IY(ICYC+1)+1
        if (PRMS(1).gt.0) then
           call OPADD (IY(ICYC+1),2150,0,1,PRMS(1),KODE)
           call OPINCR (IY,ICYC,NCYC)
        end if
     end if
     goto 90

     !       BWSPRAY - SPRAY OPTION 1, APPLY IN SPECIFIED YEAR.

     20 continue
     NSPYR = NSPYR+1
     ISPYR(NSPYR) = IDT
     SPEFFS(NSPYR) = WK3(2)
     SPINST(NSPYR) = WK3(1)
     call OPDONE(ITODO,IDT)
     goto 90

     !       BWSPRAY - SPRAY OPTION 2, APPLICATION TRIGGERED BY % DEFOLIATION.

     30 continue
     !        have not quite figured this out and am not sure if this activity
     !        can be set to done or deleted based on defoliation values at this
     !        point. This code is really just a placeholder, the 2159 activity
     !        code had no associated processing before. Lance David 8-dec-00
     !        NSPYR=NSPYR+1
     !        ISPYR(NSPYR)=IDT
     !        SPEFFS(NSPYR)=WK3(2)
     !        SPINST(NSPYR)=WK3(1)
     !        CALL OPDONE(ITODO,IDT)
     goto 90

     !       SET THE SETPRBIO OPTION.

     40 continue

     !       PRBIO ARRAY IS INITIALIZED FOR FIRST OCCURENCE OF SETPRBIO ONLY.

     if (LPRBIN) goto 60

     !       INITIALIZE PRBIO.

     do I = 1,6
        do J = 1,9
           do K = 1,4
              PRBIO(I,J,K) = 1.0
              50 continue
           end do
        end do
     end do
     60 continue
     IPRBYR = IYR1
     LPRBIN = .true.

     !       LOAD INDEXING BOUNDS FOR THE OPTION.

     call BWESIN (WK3,IBWSPM,IS1,IS2,ICRC1,ICRC2,ICRC3)

     !       IF THE SPECIES CODE MAPS TO NON-HOST, THEN DELETE OPTION.
     !       IF VALID HOST, LOAD PRBIO FROM WK3 AND SET OPTION COMPLETE.

     if (IS1.gt.6) then
        call OPDEL1 (ITODO)
     else
        do ICROWN = ICRC1,ICRC2,ICRC3
           do IHOST = IS1,IS2
              do IFAGE = 2,4
                 PRBIO(IHOST,ICROWN,IFAGE) = WK3(IFAGE+2)
                 80 continue
              end do
           end do
        end do
        call OPDONE(ITODO,IDT)
     end if

     90 continue
  end do
  100 continue

  !     END OPTION PROCESSING.

  !     RESET LSKBIO.  LSKBIO STAYS TRUE IF RDDSM1 AND RHTGM1 WERE SET
  !     BEFORE THE ROUTINE WAS CALLED AND IF PRBIO HAS NOT BEEN ALTERED.

  LSKBIO = LSKBIO .and..not.LPRBIN
  if (LSKBIO) goto 140
  do I = 1,6
     do J = 1,3
        RDDSM1(I,J) = 1.0
        RHTGM1(I,J) = 1.0
        117 continue
     end do
     do J = 1,9
        do K = 1,4
           POFPOT(I,J,K) = 1.0
           119 continue
        end do
     end do
     120 continue
  end do
  ICUMYR = 0
  NCUMYR = 0

  !     IF SETPRBIO HAS BEEN PROCESSED, SKIP INITIALIZATION OF PRBIO.

  if (LPRBIN) goto 140
  do I = 1,6
     do J = 1,9
        do K = 1,4
           PRBIO(I,J,K) = 1.0
           130 continue
        end do
     end do
  end do
  IPRBYR = IYR1
  140 continue

  !     ARE WE DONE?  RETURN IF THERE IS NOTHING ELSE TO DO.

  if (.not. (LREGO.or.LDEFOL)) goto 9000

  !     LOAD SPECIES PRESENT/ABSENCE ARRAY (IFHOST)

  do I = 1,7
     IFHOST(I) = 0
     150 continue
  end do
  do I = 1,MAXSP
     K = IBWSPM(I)
     if (ISCT(I,1).gt.0) IFHOST(K) = 1
     160 continue
  end do

  !     ZERO OUT ACCUMULATORS.

  do IHOST = 1,6
     do ITREE = 1,3
        PEDDS(IHOST,ITREE) = 0.0
        PEHTG(IHOST,ITREE) = 0.0
        BWMXCD(IHOST,ITREE) = 0.0
        AVYRMX(IHOST,ITREE) = 0.0
        164 continue
     end do
     165 continue
  end do

  do ICROWN = 1,9
     FOLNH(ICROWN) = 0.
     170 continue
  end do
  do I = 1,6
     do J = 1,9
        do K = 1,4
           FOLPOT(I,J,K) = 0.
           180 continue
        end do
     end do
  end do

  !LRD  MOVED TO TOP
  !LRD      DO 190 ISZI=1,3
  !LRD      DO 190 IHOST=1,7
  !LRD      BWTPHA(IHOST,ISZI)=0.
  !LRD  190 CONTINUE

  !     ****** CALL THE CROWN BIOMASS MODEL ****** (LOAD WK4).

  call BWEBMS (WK4,2)
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: bwebms WK4=',WK4

  !     INITIALIZE AVERAGE DBH, CROWNRATIO, AND HEIGHT.

  do IHOST = 1,7
     do ISZI = 1,3
        CRT(IHOST,ISZI) = 0.0
        HTT(IHOST,ISZI) = 0.0
        204 continue
     end do
     205 continue
  end do

  !     DO FOR ALL TREES.

  if (ITRN.le.0) goto 262
  do I = 1,ITRN
     HTI = HT(I)
     ICRI = ICR(I)

     !       FIND THE CROWN CLASS INDICIES

     call BWECRC (HTI,ISZI,ICRC1,ICRC2)

     !       LOAD TREE SAMPLING WEIGHT, CONVERT TO TREES PER HA.

     PROBI = PROB(I)*2.47103

     !       LOAD SPECIES CODE AND MAP TO HOST CODE.

     ISPI = ISP(I)
     IHOST = IBWSPM(ISPI)

     !       ACCUMULATE TREES PER HA BY TREE CLASS AND HOST SPECIES.

     BWTPHA(IHOST,ISZI) = BWTPHA(IHOST,ISZI)+PROBI
     WK6(ISZI) = WK6(ISZI)+PROBI

     !       ACCUMULATE THE HEIGHT AND CROWN RATIO OF THE TREES BY HOST
     !       AND CROWN LEVEL.

     HTT(IHOST,ISZI) = HTT(IHOST,ISZI)+HT(I)*PROBI
     CRT(IHOST,ISZI) = CRT(IHOST,ISZI)+ICRI*PROBI

     !       LOAD TREE BIOMASS IN GRAMS PER HA

     BIO = WK4(I)*453.6*PROBI

     !       IF IHOST IS LESS THAN 7, THE TREE IS HOST, BRANCH TO HOST
     !       FOLIAGE CALCULATIONS.

     if (IHOST.lt.7) goto 230

     !       ELSE: THE TREE IS NON-HOST, ACCUMULATE NON-HOST FOLIAGE.

     do ICROWN = ICRC1,ICRC2
        FOLNH(ICROWN) = FOLNH(ICROWN)+(PRCRN3(ICROWN)*BIO)
        220 continue
     end do
     goto 250
     230 continue

     !       ACCUMULATE THE BIOMASS OF HOST FOLIAGE.

     do ICROWN = ICRC1,ICRC2
        do IFAGE = 1,4
           FOLPOT(IHOST,ICROWN,IFAGE) = FOLPOT(IHOST,ICROWN,IFAGE)+&
                &(THEOFL(IFAGE,ICROWN)*PRCRN3(ICROWN)*BIO)
           240 continue
        end do
     end do
     250 continue
     260 continue
  end do
  262 continue

  !     COMPUTE THE AVERAGE CROWN LENGTH AND HEIGHT OF CROWN CLASSES.

  do IHOST = 1,7
     if (IFHOST(IHOST).eq.0) goto 267
     do ISZI = 1,3
        DIV = BWTPHA(IHOST,ISZI)

        !         CALC. THE NUMBER OF HOST STEMS PER ACRE. BWTPHA=NO. OF TREES
        !         PER HECTARE BY HOST.

        if (IHOST.ne.7) HOSTST = HOSTST+(BWTPHA(IHOST,ISZI)*0.4047)

        if (DIV.le.0.0) goto 265

        !         COMPUTE THE AVERAGE HEIGHT, DBH, AND CROWN RATIO OF TREES
        !         BY TREE SIZE AND HOST.  .3048 CONVERTS FEET TO METERS, AND
        !         .01 CONVERTS CROWN RATIO FROM 1 TO 90 TO CROWN RATIO OF
        !         .01 TO .9, WHICH IS USED TO PROPORTION OUT THE CROWN LEVELS.

        HTT(IHOST,ISZI) = HTT(IHOST,ISZI)*.3048/DIV
        CRT(IHOST,ISZI) = CRT(IHOST,ISZI)*.01/DIV

        !         CHECK TO SEE IF THE CROWN RATIO FALLS BELOW THE MINIMUM FOR THIS
        !         HEIGHT CLASS.  IF SO, USE THE MINIMUM AS THE CROWN RATIO.

        CRM = BWESLP(HTT(IHOST,ISZI),HTSCL,CRNMIN,6)
        if (CRT(IHOST,ISZI).lt.CRM) CRT(IHOST,ISZI) = CRM
        265 continue
     end do
     267 continue
  end do

  !     COMPUTE THE PER TREE (AVERAGE) POTENTIAL FOLIAGE.

  do IHOST = 1,6

     !     IF THERE IS NO HOST OF THIS SPECIES, THEN: SKIP CALCULATIONS.

     if (IFHOST(IHOST).eq.0) goto 330
     do ICROWN = 1,9
        ISZI = (ICROWN+2)/3
        DIV = BWTPHA(IHOST,ISZI)

        !     IF THE SAMPLING WEIGHT IS ZERO, BRANCH TO ZERO OUT THE
        !     FOLIAGE ARRAY.

        if (DIV.le.0.0) goto 300
        do IFAGE = 1,4
           FOLPOT(IHOST,ICROWN,IFAGE) = FOLPOT(IHOST,ICROWN,IFAGE)/DIV
           290 continue
        end do
        goto 320
        300 continue
        do IFAGE = 1,4
           FOLPOT(IHOST,ICROWN,IFAGE) = 0.
           310 continue
        end do
        320 continue
     end do
     330 continue
  end do

  !     COMPUTE THE PER TREE (AVERAGE) NON-HOST FOLIAGE.

  do ICROWN = 1,9
     ISZI = (ICROWN+2)/3
     DIV = BWTPHA(7,ISZI)
     if (DIV.le.0.0) goto 340
     FOLNH(ICROWN) = FOLNH(ICROWN)/DIV
     goto 350
     340 continue
     FOLNH(ICROWN) = 0.
     350 continue
  end do

  !     WRITE THE POTENTIAL FOLIAGE, TREES PER HA, AND CROWN DIMENSION
  !     ARRAYS.

  !     LOAD THE ADJUSTED FOLIAGE ARRAYS.  THE ADJUSTMENT IS DONE TO
  !     ACCOUNT FOR THE EFFECT DEFOLIATION HAS ON THE PREVIOUS YEARS TREE
  !     GROWTH.

  !     THE AMOUNT OF THE ADJUSTMENT IS A FUNCTION OF THE ACTUAL
  !     PROPORTION OF RETAINED BIOMASS IN THE PREVIOUS YEAR.  NOTE THAT
  !     THE ACTUAL BUDWORM MODEL FOLIAGE ARRAYS ARE LOADED IN BWADPV.

  do IHOST = 1,6
     if (IFHOST(IHOST).eq.0) goto 550
     do ICROWN = 1,9
        do IFAGE = 1,4
           FOLADJ(IHOST,ICROWN,IFAGE) = FOLPOT(IHOST,ICROWN,IFAGE)*&
                &POFPOT(IHOST,ICROWN,IFAGE)
           530 continue
        end do
        540 continue
     end do
     550 continue
  end do

  !     IF FOLIAGE PROFILE AND SITE DATA ARE REQUESTED, THEN:
  !     WRITE IT (80 CHAR, FIRST COL IS BLANK).

  IELEV = IFIX(ELEV+.5)
  call BWEADV(IYR1)

  !     SET TIMING OF MODEL. IBWYR1 AND IBWYR2
  !     ARE THE VARIABLES WHICH CONTROL HOW LONG THE BUDWORM MODEL
  !     WILL RUN ( YEAR 1 TO YEAR 2 ).  NTODO IS THE NUMBER OF DEFOLS,
  !     IF ANY, THAT THE BUDWORM MODEL WILL DO.  BWFINT IS THE CYCLE
  !     INTERVAL.

  IBWYR1 = IY(ICYC)
  IBWYR2 = IBWYR1+IFINT-1
  BWFINT = FINT
  if (BWFINT.lt.1.0) BWFINT = 1.0
  if (.not.LDEFOL) goto 700
  ITODO = 1
  call OPFIND(1,MYALL(2),NTODO)
  700 continue

  9000 continue
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: WK6=',WK6
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: FOLPOT=',FOLPOT
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: POFPOT=',POFPOT
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: FOLADJ=',FOLADJ
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: FOLNH=',FOLNH
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: BWTPHA=',BWTPHA
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: HTT=',HTT
  if (DEBUG) write (JOSTND,*) 'IN BWESIT: CRT=',CRT

  if (DEBUG) write (JOSTND,*) 'EXIT BWESIT: ICYC = ',ICYC
  return
end
