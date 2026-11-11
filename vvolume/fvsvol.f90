!======================!
!   vvolume : fvsvol   !
!======================!
! VVOLUME $Id$
subroutine FVSVOL
  use MRULES_MOD
  implicit none

  !  THIS SUBROUTINE CALLS THE VOLINIT ROUTINE TO ACCESS THE
  !  THE NATIONAL CRUISE SYSTEM VOLUME LIBRARY FOR METHB OR METHC
  !  EQUAL TO 5 (GEVORKIANTZ) OR 6. IT ALSO CONTAINS ENTRY POINTS
  !  AT THE END FOR OTHER VARIANT-SPECIFIC SPECIAL VOLUME
  !  CALCULATION METHODZ (METHB OR METHC = 8)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VOLSTD.f90'

  common/FVSVOLCOM/VOLEQ,MTOPP,MTOPS,IREGN,FORST,PROD

  ! Variables
  character(len=1) :: CTYPE
  character(len=1) :: HTTYPE
  character(len=1) :: LIVEDEAD
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=4) :: CONSPEC
  character(len=4) :: FIASP
  character(len=11) :: EQNC
  character(len=11) :: EQNB
  character(len=11) :: VOLEQ
  integer :: CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
  integer :: IT,ITRNC,ISPC,INTFOR,IERR,IZERO,FIASPCD
  integer :: I1,IREGN,IFC
  integer :: I,II,I3,I7,I15,I20,I21
  integer :: ITD,BADUM,SIDUM,HTTDUM,IDIST,TLOGS
  integer :: HTLOG,HTREF
  real    :: VMAX,BFMAX,BARK,BRATIO,H,D,BBF,BBFV,BV,VM,VN,VVN
  real    :: FC,DBTBH,TVOL1,TVOL4,TVOL7,TDIBB,TDIBC
  real    :: NOLOGP,NOLOGS
  real    :: BOLHT(21),LOGLEN(20),TVOL(15)
  real    :: HT1PRD,HT2PRD,MTOPP,MTOPS,STUMP,TOPDIAM
  real    :: DRCOB,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,RCULL,PCULL
  real    :: TCF,MCF,SCF,CLMSTOP,RCRATIO,BRKHT
  logical :: TKILL,CTKFLG,BTKFLG,LCONE,DEBUG
  !----- New Variables added for 03/2023 Version of NVEL
  integer :: CRATIO,DECAY,MRULFLG,BRKTD,WDSTMS,PDECAY
  real    :: BIODRY(15),BIOGRN(15),BIODRYIN(15)
  TYPE(MERCHRULES)::MERRLS

  !  NATIONAL CRUISE SYSTEM ROUTINES (METHOD = 6)

  entry NATCRS (TCF,MCF,SCF,BBFV,ISPC,D,H,TKILL,&
       &CRATIO,BARK,ITRNC,VMAX,BFMAX,&
       &RCULL,DECAY,WDSTMS,BIODRYIN,&
       &LIVEDEAD,CTKFLG,BTKFLG,IT)

  ! Check whether debugging is enabled
  MRULFLG = 0
  BRKHT = 0
  BRKTD = 0
  TCF = 0
  MCF = 0
  SCF = 0
  BBFV = 0
  CLMSTOP = 0
  PCULL = 0
  PDECAY = 0
  IREGN = 0
  VMAX = 0
  BFMAX = 0
  RCRATIO = FLOAT(CRATIO)
  if (LFIANVB) then
     PCULL = RCULL
     PDECAY = DECAY
  end if
  IDIST = 1
  VOLEQ = VEQNNC(ISPC)
  call FORMCL(ISPC,IFOR,D,FC)
  IFC = IFIX(FC)
  read(FIAJSP(ISPC), '(I4)')FIASPCD
  if ((LFIANVB .or. VOLEQ(1:3) .eq. "NVB") .and.&
       &TKILL .and. LIVEDEAD .eq. 'L') then
     BRKHT = FLOAT(ITRNC)/100
  end if

  if (KODFOR.gt.10000) then
     IREGN = KODFOR/10000
     INTFOR = KODFOR/100-IREGN*100
  else
     IREGN = KODFOR/100
     INTFOR = KODFOR - (KODFOR/100)*100
  end if

  if (VARACD.eq.'PN' .or. VARACD.eq.'OP') then
     if (IREGN.eq.8) then
        IREGN = 6
        INTFOR = 9
     end if
  else if (VARACD.eq.'NC') then
     if (IREGN.eq.8) then
        IREGN = 5
        INTFOR = 10
     end if
  else if (VARACD.eq.'SO') then
     if (IREGN.eq.7) then
        select case (KODFOR)
        case (701)
           IREGN = 5
           INTFOR = 5
        case (799)
           IREGN = 6
           INTFOR = 1
        end select
     end if
  end if

  if (LFIANVB) then
     select case (FIASPCD)
     case (62,  63,  65,  66, 69, 106, 133, 134,&
          &143, 321, 322, 475, 803, 810, 814, 843)
        IFC = WDSTMS
        select case (ISTATE)
        case (4)
           IREGN = 3
        case (16, 32)
           IREGN = 4
        case (6)
           IREGN = 5
        case (41, 53)
           IREGN = 6
        end select
     end select
  end if

  call DBCHK (DEBUG,'FVSVOL',6,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC,IFOR
  3 format(' ENTERING SUBROUTINE FVSVOL CYCLE,IFOR =',I5,i4)
  if (DEBUG)write(JOSTND,*)' ENTERING NATCRS ISPC,D,H,TKILL,BARK,',&
       &'ITRNC,CTKFLG,BTKFLG,IT= ',ISPC,D,H,TKILL,BARK,ITRNC,CTKFLG,&
       &BTKFLG,IT

  if (VARACD.eq.'SN')IDIST = KODFOR-(KODFOR/100)*100

  write(FORST,'(I2)')INTFOR
  if (INTFOR.lt.10)FORST(1:1) = '0'
  write(FIASP,('(A)'))JSP(ISPC)
  HTTYPE = 'F'
  IERR = 0
  DBTBH = D*(1-BARK)
  if (DEBUG)write(JOSTND,*)' INTFOR, IREGN= ',INTFOR, IREGN
  do IZERO = 1,15
     TVOL(IZERO) = 0.
     BIODRY(IZERO) = 0.
     BIODRYIN(IZERO) = 0.
     BIOGRN(IZERO) = 0.
     100 continue
  end do

  DBTBH = D*(1-BARK)
  VOLEQ = VEQNNC(ISPC)
  STUMP = STMP(ISPC)
  PROD = '02'
  !  OLD COMMENT BELOW NEEDS TO BE DELETED
  !  REGION 9 INPUTS OUTSIDE-BARK TOP DIAMETERS TO VOLINIT
  !  REGION 8 DOES NOT CARE, TOP DIAMETERS ARE HARD WIRED
  !  WESTERN VARIANTS INPUT INSIDE-BARK TOP DIAMETERS
  !  MTOPP (primary product - CuFt, Sawlog)
  !  MTOPS (secondary product - Cuft, Pulpwood)
  !  OLD COMMENT ABOVE NEEDS TO BE DELETED

  !  SPRING 2026 -- DWAGNER
  !  WE NOW UNDERSTAND THAT TOP DIAMETERS, AS TRACKED BY FVS
  !  AND DESCRIBED IN THE VARIANT OVERVIEW DOCUMNENTATION
  !  CAN BE INPUT DIRECTLY INTO NVEL WITHOUT MODIFICATION FOR
  !  BARK THICKNESS.
  !  ALL VARIANTS OTHER THAN SN, ARE STORED AS TOP DIAMETER INSIDE BARK,
  !  THE SN VARIANT IS OUTSIDE BARK BUT BARK THICKNESS IS HANDLED IN NVEL.

  !  FIANVB EQUATIONS USE OUTSIDE BARK DIAMETERS, BUT NVEL IS DESIGNED WITH THAT INTENT
  !  THUS, ALL TOPDIAMETERS CAN BE PASSED TO NVEL AS IS, AND NOT MODIFIED FOR BARK THICKNESS,
  !  BUT NFS SPECS SHOULD NOT BE USED WITH CRUISE TYPE 'I', AND FIAVBC SPECS SHOULD NOT BE USED
  !  WITH CRUISETYPE 'F' SINCE THERE ARE DIFFENCE IN THE ASSUMPTIONS OF THE TOP DIAMETER INPUTS.
  !  'F' ASSUMES TOP DIAMETER OUTSIDE BARK
  !  'I' ASSUMES TOP DIAMETER INSIDE BARK

  !  WESTERN VARIANTS USING DEFAULT NFS EQUATIONS DO NOT COMPUTE SAWLOG CUBIC VOLUME,
  !  THUS MTOPS AND TOPDIAM BOTH EQUAL THE TOPD FOR THAT SPECIES

  MTOPS = TOPD(ISPC)
  TOPDIAM = TOPD(ISPC)

  if ((IREGN.eq.8.or.IREGN.eq.9.or.LFIANVB)&
       &.and.D.ge.SCFMIND(ISPC)) then
     STUMP = SCFSTMP(ISPC)
     TOPDIAM = SCFTOPD(ISPC)
     PROD = '01'
  end if

  !      I1=0     ! no longer in use a.o. 10/28/2022 to prevent address space pollution
  I3 = 3 ! second dimension of LOGDIA(,x) array
  I7 = 7 ! first dimension of LOGVOL(x,) array
  I15 = 15 ! Dimension of VOL(x) array
  I20 = 20 ! second dimension of LOGVOL(,x) array, dimension of LOGLEN(x)
  I21 = 21 ! first dimension of LOGDIA(x,) array, dimension of BOLHT(x)

  !     Initialize independant volinit variables to prevent shared address space
  HTLOG = 0 ! Replaces I1 in position 11 of volinit call
  HTREF = 0 ! Replaces I1 in position 18 of volinit call

  DRCOB = 0. ! Replaces X1 in position 08 of volinit call
  UPSHT1 = 0. ! Replaces X1 in position 14 of volinit call
  UPSHT2 = 0. ! Replaces X1 in position 15 of volinit call
  UPSD1 = 0. ! Replaces X1 in position 16 of volinit call
  UPSD2 = 0. ! Replaces X1 in position 17 of volinit call
  AVGZ1 = 0. ! Replaces X1 in position 19 of volinit call
  AVGZ2 = 0. ! Replaces X1 in position 20 of volinit call

  !     INITIALIZE ARRAYS LOADED BY VOLUME ROUTINES (NVEL)
  !     DIMENSIONS BASED ON VARIABLES SET DIRECTLY ABOVE
  do I = 1,I21
     do II = 1,I3
        LOGDIA(I,II) = 0.0
     end do
     BOLHT(I) = 0.0
  end do
  do I = 1,I20
     do II = 1,I7
        LOGVOL(II,I) = 0.0
     end do
     LOGLEN(I) = 0.0
  end do

  !  CONSTANT CHARACTER ARGUMENTS

  CTYPE = CFCTYPE

  !  PRODUCT FLAGS

  !  TOAL CUBIC
  CUTFLG = 1
  !  BF
  BFPFLG = 0
  if ((IREGN.eq.9.or. IREGN.eq.8) .and.&
       &(D.ge.BFMIND(ISPC)) .and.&
       &(VEQNNB(ISPC) .eq. VEQNNC (ISPC)) .and.&
       &(BFMIND(ISPC) .eq. SCFMIND(ISPC)) .and.&
       &(BFSTMP(ISPC) .eq. SCFSTMP(ISPC)) .and.&
       &(BFTOPD(ISPC) .eq. SCFTOPD(ISPC))) BFPFLG = 1
  !  MERCH CUBIC
  CUPFLG = 1
  !  CORDWOOD
  CDPFLG = 0
  !  SECONDARY PRODUCT
  SPFLG = 0
  if ((LFIANVB .or. IREGN.eq.9.or. IREGN.eq.8) .and.&
       &D.ge.SCFMIND(ISPC)) SPFLG = 1

  TLOGS = 0
  NOLOGP = 0.
  NOLOGS = 0.
  HT1PRD = 0.
  HT2PRD = 0.
  IERR = 0
  SIDUM = 0
  BADUM = 0
  HTTDUM = 0
  CONSPEC = '    '

  if (DEBUG)write(JOSTND,*)&
       &'CALLING VOLINIT CF ISPC,IREGN,FORST,VOLEQ = ',&
       &ISPC,IREGN,FORST,VOLEQ
  if (DEBUG)write(JOSTND,*)&
       &'  SCFTopD,MCFTopD,STUMP,D,DRCOB,HTTYPE,H,I1 = ',&
       &TOPDIAM,MTOPS,STUMP,D,DRCOB,HTTYPE,H,I1
  if (DEBUG)write(JOSTND,*)'  HT1PRD,HT2PRD,IFC,DBTBH,BARK = ',&
       &HT1PRD,HT2PRD,IFC,DBTBH,BARK
  if (DEBUG)write(JOSTND,*)'  CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG = ',&
       &CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
  if (DEBUG)write(JOSTND,*)&
       &'  CONSPEC=',CONSPEC,' PROD=',PROD,' HTTDUM=',HTTDUM,&
       &' LIVEDEAD=',LIVEDEAD,' BADUM=',BADUM,' SIDUM= ',SIDUM
  if (DEBUG)write(JOSTND,*)'  CTYPE,IERR,IDIST = ',&
       &CTYPE,IERR,IDIST

  !     It does not appear that the secondary product top diameter MTOPS,
  !     5th argument, functions. So it is used in primary product position on
  !     this call to compute the height to merch cubic top diameter. Region 5
  !     is a special case and so TOPDIAM is set above.

  call VOLINITNVB(IREGN,FORST,VOLEQ,TOPDIAM,MTOPS,STUMP,D,&
       &DRCOB,HTTYPE,H,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,&
       &HTREF,AVGZ1,AVGZ2,IFC,DBTBH,BARK*100,RCRATIO,PCULL,PDECAY,&
       &TVOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
       &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTDUM,LIVEDEAD,&
       &BADUM,SIDUM,CTYPE,IERR,IDIST,BRKHT,BRKTD,FIASPCD,BIODRY,&
       &BIOGRN,MRULFLG,MERRLS,CLMSTOP)

  if (DEBUG)write(JOSTND,*)&
       &'AFTER VOLINIT CF IERR,PROD,HT1PRD,HT2PRD = ',&
       &IERR,PROD,HT1PRD,HT2PRD
  if (DEBUG)write(JOSTND,*)'   TVOL = ',TVOL
  if (DEBUG)write(JOSTND,*)'   LOGVOL= ',LOGVOL
  if (DEBUG)write(JOSTND,*)'   LOGDIA= ',LOGDIA
  if (DEBUG)write(JOSTND,*)'   LOGLEN= ',LOGLEN
  if (DEBUG)write(JOSTND,*)'   BOLHT,TLOGS,NOLOGP,NOLOGS= ',&
       &BOLHT,TLOGS,NOLOGP,NOLOGS

  !  SAVE THE CUBIC FOOT LOGS FOR ECON BEFORE THE BOARD FOOT CALL BELOW
  !  OVERWRITES THEM.
  CFLDIA = LOGDIA
  CFLVOL = LOGVOL

  !  STORE CALCULATED MERCH CUBIC HEIGHT
  !                               West     East
  !  Primary product (HT1PRD):    BdFt     Sawlog
  !  Secondary Product (HT2PRD):  CuFt     Pulpwood

  !  While working on this routine 5/24/2017, using default volume equation
  !  assignments, Region 9 (Variants CS, LS, NE) is the only one that
  !  returns value in HT2PRD, others are captured from HT1PRD with the
  !  merch CuFt top diameter set for primary product (volinit 4th argument).

  !  Old R8, pulpwood process no longer needed, need to store pulp ht here
  !    -DW August 2022

  if (IT.gt.0.and. D.ge.DBHMIN(ISPC)) then
     HT2TD(IT,2) = MAX(HT1PRD, HT2PRD)
     if (BFPFLG.eq.1) HT2TD(IT,1) = HT1PRD

     !     Region 8 requires at least 10 feet of product from the tree
     !     for the tree volume to be included.
     if (IREGN.eq.8.and. PROD.eq.'01' .and. HT1PRD.lt.10.) then
        TVOL(4) = 0.
        TVOL(2) = 0.
     end if
  end if

  if (LFIANVB) BIODRYIN = BIODRY

  !  END OF CF SECTION

  !  IF THE BF VOLUME EQUATION IS DIFFERENT THAN THE CF VOLUME EQ OR
  !  THE TOP DIAMETER SPECIFICATION FOR THE CUFT AND BDFT PRODUCTS
  !  ARE DIFFERENT, THEN STORE CUBIC VOLUMES AND CALL  VOLINIT AGAIN
  !  TO CALCULATE BF VOLUMES ONLY

  if (BFPFLG.eq.0.and. D.ge.BFMIND(ISPC)) then
     TVOL1 = TVOL(1)
     TVOL4 = TVOL(4)
     TVOL7 = TVOL(7)
     VOLEQ = VEQNNB(ISPC)

     !       Region 8 Clark equation needs saw timber top diameter spec
     !       inserted into equation identification.
     !        IF(IREGN.EQ.8 .AND. VOLEQ(4:6).EQ.'CLK')THEN
     !          WRITE(BFTOP,'(I1)')NINT(BFTOPD(ISPC))
     !          VOLEQ(3:3)=BFTOP
     !        ENDIF
     do IZERO = 1,15
        TVOL(IZERO) = 0.
     end do

     STUMP = BFSTMP(ISPC)
     MTOPP = BFTOPD(ISPC)
     PROD = '01'
     !       I1=0 ! no longer in use a.o. 10/28/2022 to prevent address space pollution
     I3 = 3
     I7 = 7
     I15 = 15
     I20 = 20
     I21 = 21

     !     Initialize independant volinit variables to prevent shared address space
     HTLOG = 0 ! Replaces I1 in position 11 of volinit call
     HTREF = 0 ! Replaces I1 in position 18 of volinit call

     DRCOB = 0. ! Replaces X1 in position 08 of volinit call
     UPSHT1 = 0. ! Replaces X1 in position 14 of volinit call
     UPSHT2 = 0. ! Replaces X1 in position 15 of volinit call
     UPSD1 = 0. ! Replaces X1 in position 16 of volinit call
     UPSD2 = 0. ! Replaces X1 in position 17 of volinit call
     AVGZ1 = 0. ! Replaces X1 in position 19 of volinit call
     AVGZ2 = 0. ! Replaces X1 in position 20 of volinit call

     !  CONSTANT CHARACTER ARGUMENTS

     CTYPE = BFCTYPE

     !  PRODUCT FLAGS

     !  TOAL CUBIC
     CUTFLG = 1
     !  BF
     BFPFLG = 1
     !  MERCH CUBIC
     CUPFLG = 1
     !  CORDWOOD
     CDPFLG = 0
     !  SECONDARY PRODUCT
     SPFLG = 0
     !  CONSTANTS
     TLOGS = 0
     NOLOGP = 0.
     NOLOGS = 0.
     if ((IREGN.ne.8).and.(VOLEQ(4:6).ne.'DVE')) then
        HT1PRD = 0.
        HT2PRD = 0.
     end if
     if ((IREGN.eq.8).and.(VOLEQ(4:6).eq.'CLK')) then
        HT1PRD = 0.
        HT2PRD = 0.
     end if
     IERR = 0
     SIDUM = 0
     BADUM = 0
     HTTDUM = 0
     CONSPEC = '   '
     HT1PRD = 0.
     HT2PRD = 0.
     PCULL = 0
     PDECAY = 0

     !  GET FORM CLASS FOR THIS TREE.

     call FORMCL(ISPC,IFOR,D,FC)
     IFC = IFIX(FC)

     DBTBH = D*(1-BARK)

     if (DEBUG)write(JOSTND,*)&
          &'CALLING VOLINIT BF ISPC,IREGN,FORST,VOLEQ = ',&
          &ISPC,IREGN,FORST,VOLEQ
     if (DEBUG)write(JOSTND,*)&
          &'  BFTopD,MCFTopD,STUMP,D,DRCOB,HTTYPE,H,I1 = ',&
          &MTOPP,MTOPS,STUMP,D,DRCOB,HTTYPE,H,I1
     if (DEBUG)write(JOSTND,*)'  HT1PRD,HT2PRD,IFC,DBTBH,BARK = ',&
          &HT1PRD,HT2PRD,IFC,DBTBH,BARK
     if (DEBUG)write(JOSTND,*)'  CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG = '&
          &,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG
     if (DEBUG)write(JOSTND,*)&
          &'  CONSPEC=',CONSPEC,' PROD=',PROD,' HTTDUM=',HTTDUM,&
          &' LIVEDEAD=',LIVEDEAD,' BADUM=',BADUM,' SIDUM= ',SIDUM
     if (DEBUG)write(JOSTND,*)'  CTYPE,IERR,IDIST = ',&
          &CTYPE,IERR,IDIST

     call VOLINITNVB(IREGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,D,&
          &DRCOB,HTTYPE,H,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,&
          &HTREF,AVGZ1,AVGZ2,IFC,DBTBH,BARK*100,RCRATIO,PCULL,PDECAY,&
          &TVOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,&
          &BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTDUM,LIVEDEAD,&
          &BADUM,SIDUM,CTYPE,IERR,IDIST,BRKHT,BRKTD,FIASPCD,BIODRY,&
          &BIOGRN,MRULFLG,MERRLS,CLMSTOP)

     if (DEBUG)write(JOSTND,*)&
          &'AFTER VOLINIT BF IERR,PROD,HT1PRD,HT2PRD= ',&
          &IERR,PROD,HT1PRD,HT2PRD
     if (DEBUG)write(JOSTND,*)'   TVOL = ',TVOL
     if (DEBUG)write(JOSTND,*)'   LOGVOL= ',LOGVOL
     if (DEBUG)write(JOSTND,*)'   LOGDIA= ',LOGDIA
     if (DEBUG)write(JOSTND,*)'   LOGLEN= ',LOGLEN
     if (DEBUG)write(JOSTND,*)'   BOLHT,TLOGS,NOLOGP,NOLOGS= ',&
          &BOLHT,TLOGS,NOLOGP,NOLOGS

     if (D.ge.BFMIND(ISPC)) then
        if (IT.gt.0)HT2TD(IT,1) = HT1PRD
     else
        if (IT.gt.0)HT2TD(IT,1) = 0.0
        TVOL(2) = 0.0
     end if

     !  WHEN THERE IS NO CF VOL. EQ. NO. (E.G. CF METHC=2 OR 3, AND CFVOL
     !  OR OCFVOL IS CALLED FROM VOLS, MUST SET VMAX SO TRUNCATED VOLUMES
     !  ARE CALCULATED

     BFMAX = TVOL(1)
     TVOL(1) = TVOL1
     TVOL(4) = TVOL4
     TVOL(7) = TVOL7
     if ((IREGN.eq.8).and.(HT1PRD.lt.10.)) then
        TVOL(4) = 0.
        TVOL(2) = 0.
     end if
  end if ! END OF BF SECTION

  !  SAVE THE BOARD FOOT LOGS FOR ECON. IF THE BOARD FOOT CALL ABOVE WAS
  !  SKIPPED, THESE ARE STILL THE LOGS FROM THE CUBIC FOOT CALL.
  BFLDIA = LOGDIA
  BFLVOL = LOGVOL

  !  SET RETURN VALUES.

  TCF = TVOL(1)
  if (TCF.lt.0.)TCF = 0.
  VMAX = TCF
  if (BFMAX.eq.0) BFMAX = VMAX
  if (D .ge. DBHMIN(ISPC)) MCF = TVOL(4) + TVOL(7)
  if (MCF.lt.0.)MCF = 0.
  if ((IREGN.eq.8.or. IREGN.eq.9.or. LFIANVB)&
       &.and. (D .ge. SCFMIND(ISPC))) then
     SCF = TVOL(4)
  end if

  if (D.lt.BFMIND(ISPC)) then
     BBFV = 0.
  else
     if (METHB(ISPC).eq.9) then
        BBFV = TVOL(10)
     else
        BBFV = TVOL(2)
     end if
     if (BBFV.lt.0.)BBFV = 0.
  end if
  if (DEBUG)write(JOSTND,*)'  IN FVSVOL D, TVOL, MCF, SCF, BBFV = ',&
       &D, TCF, MCF, SCF, BBFV
  CTKFLG = .true.
  BTKFLG = .true.
  return

  !  ENTER ANY OTHER CUBIC HERE

  entry OCFVOL (VN,VM,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,&
       &CTKFLG,IT)
  call DBCHK (DEBUG,'FVSVOL',6,ICYC)
  if (DEBUG) write(JOSTND,13)ICYC,IFOR
  13 format(' ENTERING FVSVOL-OCFVOL CYCLE,IFOR =',I5,I4)
  IF(DEBUG)WRITE(JOSTND,*)'  ARGUMENTS: VN,VM,                      &
       &ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,CTKFLG,IT'
  if (DEBUG)write(JOSTND,*)'  OCFVOL IN:  ',&
       &VN,VM,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,CTKFLG,IT

  VVN = 0.0
  BBF = 0.0

  select case (VARACD)
  case ('AK')
     call FVSBRUCEDEMARS(VN,VM,VMAX,D,H,ISPC,BARK,LCONE,CTKFLG)

  case ('CR')
     call FVSHANNBARE(VN,VM,VMAX,ISPC,D,H,CTKFLG)

  case ('NC')
     call FVSSIERRALOG(VN,VM,VMAX,ISPC,D,H,BARK,LCONE,CTKFLG)

  case ('CS','LS','NE')
     VN = 0.
     VM = 0.
     if (IMC(IT) .ge.3.or. D .lt. DBHMIN(ISPC))goto 600
     call TWIGCF(ISPC,H,D,VN,VM,IT)
     600 continue

  case default
     VN = 0.
     VMAX = 0.
     VM = 0.
     CTKFLG = .false.

  end select

  !  SET RETURN VALUES HERE

  CTKFLG = .true.
  if (VN.le.0.) then
     VN = 0.
     CTKFLG = .false.
  end if
  if (VM.le.0.) then
     VM = 0.
  end if
  VMAX = VN
  if (DEBUG)write(JOSTND,*)'  OCFVOL OUT: ',&
       &VN,VM,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,CTKFLG,IT
  return

  !  ENTER ANY OTHER BOARD HERE.

  entry OBFVOL (BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,&
       &BTKFLG,IT)
  call DBCHK (DEBUG,'FVSVOL',6,ICYC)
  if (DEBUG) write(JOSTND,23)ICYC,IFOR
  23 format(' ENTERING FVSVOL-OBFVOL CYCLE,IFOR =',I5,I4)
  if (DEBUG)write(JOSTND,*)'  ARGUMENTS: ',&
       &'BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,BTKFLG,IT'
  if (DEBUG)write(JOSTND,*)'  OBFVOL IN:  ',&
       &BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,BTKFLG,IT

  select case (VARACD)
  case ('AK')
     VVN = 0.0
     BBF = 0.0
     if (D .ge.9.0.and. H .gt.40.0)&
          &call FVSOLDGRO(ISPC,VVN,D,H,BBF)
     BBFV = BBF

  case ('CR')
     call HANNBAREBF(BBFV,ISPC,D,H,VMAX,BTKFLG)

  case ('NC')
     ITD = INT(BFTOPD(ISPC)+0.5)
     if (ITD.gt.100) ITD = 100
     call LOGS(D,H,ITD,ITD,DBHMIN(ISPC),BFMIND(ISPC),ISPC,&
          &BFSTMP(ISPC),BV,JOSTND)
     BBFV = BV

  case ('CS','LS','NE')
     BBFV = 0.
     if (D .lt. BFMIND(ISPC) .or. IMC(IT) .gt.1)goto 700
     call TWIGBF(ISPC,H,D,VMAX,BBFV)
     700 continue

  case default
     BBFV = 0.

  end select

  BTKFLG = .true.
  if (BBFV .le.0.) then
     BBFV = 0.
     BTKFLG = .false.
  end if
  if (DEBUG)write(JOSTND,*)'  OBFVOL OUT: ',&
       &BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,BTKFLG,IT
  return

  !  ENTRY POINT FOR SENDING VOLUME EQN NUMBER TO THE FVS-TO-NATCRZ ROUTINE

  entry GETEQN(ISPC,D,H,EQNC,EQNB,TDIBC,TDIBB)
  call DBCHK (DEBUG,'FVSVOL',6,ICYC)
  if (DEBUG) write(JOSTND,33)ICYC,IFOR
  33 format(' ENTERING FVSVOL-GETEQN CYCLE,IFOR =',I5,I4)
  if (DEBUG)write(JOSTND,*)'  ARGUMENTS: ',&
       &'ISPC,D,H,EQNC,EQNB,TDIBC,TDIBB'
  if (DEBUG)write(JOSTND,*)'  GETEQN IN:  ',&
       &ISPC,D,H,EQNC,EQNB,TDIBC,TDIBB

  EQNC = VEQNNC(ISPC)
  EQNB = VEQNNB(ISPC)
  TDIBC = TOPD(ISPC)*BRATIO(ISPC,D,H)
  TDIBB = BFTOPD(ISPC)*BRATIO(ISPC,D,H)

  if (DEBUG)write(JOSTND,*)'  GETEQN OUT: ',&
       &ISPC,D,H,EQNC,EQNB,TDIBC,TDIBB
  return

end
