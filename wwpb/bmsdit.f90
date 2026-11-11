!===================!
!   wwpb : bmsdit   !
!===================!
! WWPB $Id$
subroutine BMSDIT

  !     SET UP ONE STAND FOR PINE BEETLE RUN.

  !     AN EXTENSION OF THE PARALLEL PROCESSING EXTENSION (PROGNOSIS)
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--MAY 1987

  !     CALLED FROM: PPMAIN

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'
  include 'PPCNTL.f90'
  include 'ARRAYS.f90'
  include 'OUTCOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MISCOM.f90'
  include 'BMCOM.f90'
  include 'BMFCOM.f90'
  include 'BMRRCM.f90'
  include 'BMPCOM.f90'

  dimension DWT(NSCL)

  ! Variables
  real    :: BATEMP
  real    :: DWT
  logical :: LX
  integer :: K,LCUT,IBMSTD,JYR,IACTK,NPRM,CASEID
  real    :: MAX,MAXH,PRMS
  dimension PRMS(2)
  real    :: PI24
  real    :: TOTBAS(MAXSP,2)
  integer :: MYACT(4),ITODO
  data    MYACT/2701,2702,2703,2704/ !MAINOUT,TREEOUT,BKPOUT,VOLOUT, RESPECTIVELY

  !     IF BMSTND IS ZERO, NO DISPERSAL WAS RUN, BRANCH TO EXIT.

  if (BMSTND.le.0) return

  !     FIND OUT IF THE CURRENT STAND IS IN THE DISPERSAL GROUP

  call OPBISR (BMSTND,BMSDIX,ISTND,IBMSTD)

  !     IF THERE ARE NO MPB STANDS, THEN RETURN.

  if ((IBMSTD .le.0) .or. (.not.STOCK(IBMSTD))) return

  !     LOAD THE STAND INTO THE BEETLE MODEL ARRAYS.

  do ISIZ = 1,NSCL
     BAH(IBMSTD,ISIZ) = 0.0
     BANH(IBMSTD,ISIZ) = 0.0
     CRS(IBMSTD,ISIZ,1) = 0.0
     CRS(IBMSTD,ISIZ,2) = 0.0
     DNSTD(IBMSTD,ISIZ) = 0.0
     DWT(ISIZ) = 0.0
     HGS(IBMSTD,ISIZ,1) = 0.0
     HGS(IBMSTD,ISIZ,1) = 0.0
     HTS(IBMSTD,ISIZ,1) = 0.0
     HTS(IBMSTD,ISIZ,2) = 0.0
     TREE(IBMSTD,ISIZ,1) = 0.0
     TREE(IBMSTD,ISIZ,2) = 0.0
     OTPA(IBMSTD,ISIZ,1) = 0.0
     OTPA(IBMSTD,ISIZ,2) = 0.0
     TVOL(IBMSTD,ISIZ,1) = 0.0
     TVOL(IBMSTD,ISIZ,2) = 0.0

     do K = 1,3
        TPBK(IBMSTD,ISIZ,1,K) = 0.0
        TPBK(IBMSTD,ISIZ,2,K) = 0.0
        101 continue
     end do

     SDMR(IBMSTD,ISIZ) = 0.0
     SRR(IBMSTD,ISIZ) = 0.0
     SSR(IBMSTD,ISIZ) = 0.0
     100 continue
  end do

  do ISPC = 1,MAXSP
     TOTBAS(ISPC,1) = 0.0
     TOTBAS(ISPC,2) = 0.0
     500 continue
  end do

  BAH(IBMSTD,NSCL+1) = 0.0
  BANH(IBMSTD,NSCL+1) = 0.0
  TREE(IBMSTD,NSCL+1,1) = 0.0 !HOST.  NEW AJM 9/05
  TREE(IBMSTD,NSCL+1,2) = 0.0 !NON-HOST.  NEW AJM 9/05

  MAX = 0.0
  MAXH = 0.0

  PI24 = PIE / (24.0 * 24.0)

  if (PBSPEC .eq.4) then
     LCUT = MIN0(ISCMIN(1),ISCMIN(2))
  else
     LCUT = ISCMIN(PBSPEC)
  end if

  !     STAND-LEVEL IDENTIFIERS FOR USE BY THE FIRE MODEL:

  HABTYP(IBMSTD) = KODTYP

  !     STCCF(IBMSTD) = ATCCF

  SLP(IBMSTD) = SLOPE

  do ISPC = 1,MAXSP
     if (ISCT(ISPC,1).eq.0) goto 20

     LX = .false.
     if ((((PBSPEC .eq.1) .or. (PBSPEC .eq.4))&
          &.and. (HSPEC(1,ISPC).eq.1))&
          &.or. (((PBSPEC .eq.2) .or. (PBSPEC .eq.4))&
          &.and. (HSPEC(2,ISPC) .eq.1))&
          &.or. ((PBSPEC .eq.2) .and. (HSPEC(2,ISPC) .eq.1))& ! i THINK THIS LINE IS REDUNDANT AJM 9/05
          &.or. ((PBSPEC .eq.3).and.(HSPEC(3,ISPC) .eq.1))) LX = .true.

     ! Compute the basal area present in each host and non-host size class.
     ! The final category (NSCL+1) is for summaries.

     ! Size-class level GRF for density is computed here, since the IND(II)
     ! array is already in use, and points to the individual GRF computed
     ! elsewhere.

     do II = ISCT(ISPC,1),ISCT(ISPC,2)

        I = IND1(II)
        BATEMP = DBH(I) * DBH(I) * PI24

        call BMDBHC(DBH(I), K)

        X = (BATEMP * PROB(I))

        !     BAH, BANH are total BA within the size class.

        if (LX) then
           BAH(IBMSTD,K) = BAH(IBMSTD,K) + X
           BAH(IBMSTD,NSCL+1) = BAH(IBMSTD,NSCL+1) + X

           TREE(IBMSTD,K,1) = TREE(IBMSTD,K,1) + PROB(I)

           TREE(IBMSTD,NSCL+1,1) = TREE(IBMSTD,NSCL+1,1)+PROB(I) ! AJM 9/05

           HTS(IBMSTD,K,1) = HTS(IBMSTD,K,1) + HT(I) * PROB(I)
           CRS(IBMSTD,K,1) = CRS(IBMSTD,K,1) + ICR(I) * PROB(I)

           HGS(IBMSTD,K,1) = HGS(IBMSTD,K,1) + HTG(I) * PROB(I)
           TVOL(IBMSTD,K,1) = TVOL(IBMSTD,K,1) + CFV(I) * PROB(I)

           SDMR(IBMSTD,K) = SDMR(IBMSTD,K) + IMIST(I) * X
           SRR(IBMSTD,K) = SRR(IBMSTD,K) + BMRR(I) * X
           SSR(IBMSTD,K) = SSR(IBMSTD,K) + BMSR(I) * X

           !   Size class level GRFs (i.e. Rating Values) will not be calculated.
           !   GRF due to stand density effects has been modified in routine
           !   BMCGRF.  Modified 6/25/99, AJM.  See also lines 237, below.

           !        DNSTD(IBMSTD,K)= DNSTD(IBMSTD,K) + AMIN1(GRFDEN(I), 1.0) * X
           !           DWT(K)= DWT(K) + X

           TOTBAS(ISPC,1) = TOTBAS(ISPC,1) + X
        else

           BANH(IBMSTD,K) = BANH(IBMSTD,K) + X
           BANH(IBMSTD,NSCL+1) = BANH(IBMSTD,NSCL+1) + X

           TREE(IBMSTD,K,2) = TREE(IBMSTD,K,2) + PROB(I)

           TREE(IBMSTD,NSCL+1,2) = TREE(IBMSTD,NSCL+1,2)+PROB(I) !AJM 9/05

           HTS(IBMSTD,K,2) = HTS(IBMSTD,K,2) + HT(I) * PROB(I)
           CRS(IBMSTD,K,2) = CRS(IBMSTD,K,2) + ICR(I) * PROB(I)
           HGS(IBMSTD,K,2) = HGS(IBMSTD,K,2) + HTG(I) * PROB(I)
           TVOL(IBMSTD,K,2) = TVOL(IBMSTD,K,2) + CFV(I) * PROB(I)

           TOTBAS(ISPC,2) = TOTBAS(ISPC,2) + X
        end if
        10 continue
     end do
     20 continue
  end do

  do ISPC = 1,MAXSP
     if (ISCT(ISPC,1).eq.0) goto 25

     if (TOTBAS(ISPC,2) .gt. MAX) then
        MAX = TOTBAS(ISPC,2)
        ISPH(IBMSTD,2) = ISPC
     end if
     if (TOTBAS(ISPC,1) .gt. MAXH) then
        MAXH = TOTBAS(ISPC,1)
        ISPH(IBMSTD,1) = ISPC
     end if

     25 continue
  end do

  do I = 1,2
     if (ISPH(IBMSTD,I) .gt.0) then
        IQPTYP(IBMSTD,I) = ISPFLL(ISPH(IBMSTD,I))
     else
        IQPTYP(IBMSTD,I) = 1
     end if
     26 continue
  end do

  !     Record the initial TPA for each size class of each stand. This could be
  !     done in the DA file rather than memory, I think.

  do ISCL = 1, NSCL
     OTPA(IBMSTD,ISCL,1) = TREE(IBMSTD,ISCL,1)
     OTPA(IBMSTD,ISCL,2) = TREE(IBMSTD,ISCL,2)
     30 continue
  end do

  !     Transform density-dependent GRF, HTS into size-class averages.

  do ISCL = 1,NSCL
     if (TREE(IBMSTD,ISCL,1) .gt.1.0E-9) then
        HTS(IBMSTD,ISCL,1) = HTS(IBMSTD,ISCL,1) / TREE(IBMSTD,ISCL,1)
        CRS(IBMSTD,ISCL,1) = CRS(IBMSTD,ISCL,1) / TREE(IBMSTD,ISCL,1)
        HGS(IBMSTD,ISCL,1) = HGS(IBMSTD,ISCL,1) / TREE(IBMSTD,ISCL,1)
        TVOL(IBMSTD,ISCL,1) = TVOL(IBMSTD,ISCL,1) / TREE(IBMSTD,ISCL,1)
     else
        HTS(IBMSTD,ISCL,1) = 0.0
        CRS(IBMSTD,ISCL,1) = 0.0
        HGS(IBMSTD,ISCL,1) = 0.0
        TVOL(IBMSTD,ISCL,1) = 0.0
     end if

     if (TREE(IBMSTD,ISCL,2) .gt.1.0E-9) then
        HTS(IBMSTD,ISCL,2) = HTS(IBMSTD,ISCL,2) / TREE(IBMSTD,ISCL,2)
        CRS(IBMSTD,ISCL,2) = CRS(IBMSTD,ISCL,2) / TREE(IBMSTD,ISCL,2)
        HGS(IBMSTD,ISCL,2) = HGS(IBMSTD,ISCL,2) / TREE(IBMSTD,ISCL,2)
        TVOL(IBMSTD,ISCL,2) = TVOL(IBMSTD,ISCL,2) / TREE(IBMSTD,ISCL,2)
     else
        HTS(IBMSTD,ISCL,2) = 0.0
        CRS(IBMSTD,ISCL,2) = 0.0
        HGS(IBMSTD,ISCL,2) = 0.0
        TVOL(IBMSTD,ISCL,2) = 0.0
     end if

     if (DWT(ISCL) .gt.1.0E-9) then
        !  see note above.  The following two lines commented out 6/25/99, AJM.

        !         DNSTD(IBMSTD,ISCL)= DNSTD(IBMSTD,ISCL) / DWT(ISCL)
        SDMR(IBMSTD,ISCL) = SDMR(IBMSTD,ISCL) / DWT(ISCL)
        SRR(IBMSTD,ISCL) = SRR(IBMSTD,ISCL) / DWT(ISCL)
        SSR(IBMSTD,ISCL) = SSR(IBMSTD,ISCL) / DWT(ISCL)
     else
        !         DNSTD(IBMSTD,ISCL)= 1.0
        SDMR(IBMSTD,ISCL) = 0.0
        SRR(IBMSTD,ISCL) = 0.0
        SSR(IBMSTD,ISCL) = 0.0
     end if
     40 continue
  end do

  !     IF THIS IS THE FIRST MASTER CYCLE (MICYC=2), THEN INITIALIZE
  !     ANY DAMAGE READ FROM INVENTORY (ASSUMING THE USER HAS GIVEN
  !     THE RIGHT KEYWORDS.

  if (MICYC .eq.2) then
     do ISPC = 1, MAXSP

        if (ISCT(ISPC,1) .eq.0) goto 1000

        LX = .false.
        if ((((PBSPEC .eq.1) .or. (PBSPEC .eq.4))&
             &.and. (HSPEC(1,ISPC) .eq.1))&
             &.or. (((PBSPEC .eq.2) .or. (PBSPEC .eq.4))&
             &.and. (HSPEC(2,ISPC) .eq.1))&
             &.or. ((PBSPEC .eq.2) .and. (HSPEC(2,ISPC) .eq.1))& !REDUNDANT?
             &.or. ((PBSPEC .eq.3) .and. (HSPEC(3,ISPC) .eq.1)))&
             &LX = .true.

        if (.not. LX) goto 1000

        do II = ISCT(ISPC,1), ISCT(ISPC,2)
           I = IND1(II)
           if (LBMDAM(I)) then
              if (PBSPEC .lt.3) PBKILL(IBMSTD,K) = PBKILL(IBMSTD,K)&
                   &+ PROB(I)
              if (PBSPEC .eq.3) ALLKLL(IBMSTD,K) = ALLKLL(IBMSTD,K)&
                   &+ PROB(I)
           end if
           1010 continue
        end do
        1000 continue
     end do
  end if

  ! GET THE ACTIVITY INFO PERTAINING TO OUTPUT WRITING.
  ! ADDED 9/1/05 AJM
  ! NOTE IBEG, IEND,ISTP ARE INITIALIZED IN BMSETP IN FIRST MASTER CYCLE

  ! FIND OUT IF STAND-LEVEL OUTPUT--IS REQUESTED FOR THIS STAND.
  ! CODE ADAPTED FROM FMDOUT AJM 8/05
  ! MYACT(1)=MAINOUT; MYACT(2)=TREEOUT;MYACT(3)=BKPOUT,MYACT(4)=VOLOUT
  ! EACH ACTIVITY AND EACH STAND GETS ITS OWN SPACE IN PARMETER ARRAYS
  ! IBEG, IEND, & ISTP.

  do I = 1,4
     call OPFIND(1,MYACT(I),ITODO)
     if (ITODO.gt.0) then
        call OPGET(ITODO,2,JYR,IACTK,NPRM,PRMS)
        IBEG(I,IBMSTD) = JYR !YEAR TO BEGIN REPORTING
        IEND(I,IBMSTD) = JYR + PRMS(1) !ENDING YEAR OF REPORTING
        ISTP(I,IBMSTD) = INT(PRMS(2)) !FREQUENCY INTERVAL TO REPORT
        if (ISTP(I,IBMSTD).eq.0) ISTP(I,IBMSTD) = 1
        call OPDONE (ITODO,JYR)
     end if
     2000 continue
  end do

  ! THE DBSLOGICAL FLAGGING WWPBM-DB WRITING (1=DB+STD OUTPUT; 2==>DB ONLY)

  ! NOTE THESE VARS ARE SET VIA DBS KEYWORDS & SUPPL RECORDS, WHICH
  ! INVOKE WRITING TO DB, BUT BY THEMSELVES WON'T ENSURE THEIR WRITING.
  ! THE APPROPRIATE WWPBM OUTPUT REQUEST MUST BE MADE, WITH YEARS
  ! SPECIFIED.  THE DBS OUTPUT REQUEST MERELY SAYS: IF WE REQUESTED THE
  ! WWPBM OUTPUT (VIA WWPBM KEYWORD) THEN WRITE IT TO DB, EITHER SOLELY
  ! (VAR = 2), OR IN ADDITION TO (VAR=1) THE STD OUTPUT.
  ! VARIABLES SET IN DBSIN, WHERE THIS DBSWW IS HAS AN ENTRY.
  ! RETURN FROM EXDBS RETURNS ZEROS.

  if (MICYC .eq.2) then
     call DBSWW(J1,J2,J3,J4) !ENTRY IN DBSIN AND EXDBS,
     JBMDB(1,IBMSTD) = J1 !DBSIN RETURNS A 1 OR 2, IF CORRESPONDING
     JBMDB(2,IBMSTD) = J2 !  DB TABLE WAS REQUESTED, ELSE -1 (AS INITIALIZED)
     JBMDB(3,IBMSTD) = J3 !EXDBS RETURNS ZERO.
     JBMDB(4,IBMSTD) = J4
  end if

  !     TEST: CALL DBSCASE TO CREATE THIS STAND'S CASEID

  if (MICYC .ge.2.and.&
       &(JBMDB(1,IBMSTD).ge.1.or.&
       &JBMDB(2,IBMSTD).ge.1.or.&
       &JBMDB(3,IBMSTD).ge.1.or.&
       &JBMDB(4,IBMSTD).ge.1)) call DBSCASE(1) !CREATE A CASE ID (Note: it'll bail if its already got one.)

  if (BMCASEID(IBMSTD) .lt.0) then
     call DBSWW2(CASEID) !FETCH A CASE ID (ENTRY IN DBSCASE)
     BMCASEID(IBMSTD) = CASEID !ASSIGN IT TO A WWPBM VARIABLE
  end if

  if (LBMDEB) write (JBMBPR,310) ISTND,IBMSTD,&
       &BAH(IBMSTD,NSCL+1),BANH(IBMSTD,NSCL+1)
  310 format (' IN BMSDIT: ISTND=',I4,'; IBMSTD=',I4,'; TOT BAH=',&
       &F10.3,'; TOT BANH=',F10.4)

  return
end
