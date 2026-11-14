!=====================!
!   mistoe : misprt   !
!=====================!
! MISTOE $Id$
subroutine MISPRT

  !  Purpose:
  !     Produces dwarf mistletoe infection information and mortality
  !  summary output by cycle in table format to be appended to the
  !  Prognosis standard output listing. Also processes the MISTPRT and
  !  MISTABLE keywords.

  !  Call list definitions:

  !  Local variable definitions:
  !     CFMT:   Character array to hold write formats for DBH class loop.
  !     CSP:    Array to hold 2 char. rep. of top 4 infected species.
  !     DCDMI:  Array - average DMRs inf. trees only by 2" DBH class.
  !     DCDMR:  Array - average DMRs by 2" DBH classes.
  !     DCINF:  Array - sums of infected TPA by 2" DBH classes.
  !     DCMRT:  Array - sums of DM mortality (TPA) by 2" DBH classes.
  !     DCSUM:  Array - sums of DM ratings (IMIST) by 2" DBH classes.
  !     DCTPA:  Array - sums of TPA by 2" DBH classes.
  !     DCTPAX: Array - sums of TPA by 2" DBH classes, DBH >= DMRMIN.
  !     DEBUG:  Logical flag to turn debug on or off.
  !     DMFLAG: Logical flag, true if mistletoe is present in the stand.
  !     IJKLM:  Loop counters.
  !     I1:     Beginning tree record pointer for a species.
  !     I2:     Ending tree record pointer for a species.
  !     I3:     Tree record loop counter.
  !     IACTK:  Passed back from OPGET (unused).
  !     IDATE:  Passed back from OPGET (unused).
  !     IDCLAS: Loop counter for 2" DBH classes.
  !     IDMR:   Current tree mistletoe rating.
  !     INFNO:  Species number of current (infected) species.
  !     INFSPC: Infected species loop counter.
  !     IPRINT: Flag to turn mistletoe output on or off.
  !     ISPC:   Current species number and species loop counter.
  !     ISTAT:  Status used to open output temporary files.
  !     ITREE:  Tree loop counter.
  !     KODE:   Error code returned from MYOPEN.
  !     NAGE:   Current stand age.
  !     NP:     Passed back from OPGET (unused).
  !     NTODO:  Number of actions to perform in a cycle.
  !     NYEAR:  Current year in cycle.
  !     P:      Current trees per acre.
  !     PRM:    Array containing MISTPRT/MISTABLE keyword values.
  !     PRTREE: Logical flag to print DM tree-level output file.
  !     SDDMI:  2D array - average DMRs inf. only by species & DBH class.
  !     SDDMR:  2D array - average DMRs by species & DBH class.
  !     SDINF:  2D array - sums of infected TPA by species & DBH class.
  !     SDIN0:  2D array - sums of TPA with DMR 0 by species & DBH class.
  !     SDIN12: 2D array - sums of TPA with DMR 1-2 by species/DBH class.
  !     SDIN34: 2D array - sums of TPA with DMR 3-4 by species/DBH class.
  !     SDIN56: 2D array - sums of TPA with DMR 5-6 by species/DBH class.
  !     SDMRT:  2D array - sums of mortality TPA by species & DBH class.
  !     SDSUM:  2D array - sums of DM ratings (IMIST) by spec./DBH class.
  !     SDTPA:  2D array - sums of TPA by species and DBH class.
  !     SDTT0:  Array containing totals of SDIN0 by species.
  !     SDTT12: Array containing totals of SDIN12 by species.
  !     SDTT34: Array containing totals of SDIN34 by species.
  !     SDTT56: Array containing totals of SDIN56 by species.
  !     SORTSP: Array containing infection levels of top 4 most inf. spec.
  !     SPDMI:  Array containing species average DMI.
  !     SPDMI4: Array for species average DMI (top 4 only).
  !     SPDMR:  Array containing species average DMR.
  !     SPDMR4: Array for species average DMR (top 4 only).
  !     SPDMRS: Array containing sum of DMR's by species.
  !     SPINF4: Array for species TPA infected with DM (top 4 only).
  !     SPMRT4: Array for species TPA mortality due to DM (top 4 only).
  !     SPPIN4: Array for % of species TPA infected (top 4 only).
  !     SPPMR4: Array for % of species TPA mortality (top 4 only).
  !     SPPOC4: Array for % species occurance in stand total TPA (top 4).
  !     SPTPAI: Array containing total # infected TPA by species.
  !     SPTPAM: Array containing species TPA mortality.
  !     SPTPAT: Array containing total # TPA by species.
  !     SPTPAX: Array containing total # TPA by species, DBH >= DMRMIN.
  !     STBAI:  Stand basal area infected with DM.
  !     STBAM:  Stand basal area mortality due to DM.
  !     STDMI:  Stand average DMI (based on all trees).
  !     STDMR:  Stand average DMR (based on infected-only trees).
  !     STDMRS: Stand total sum of DMR's.
  !     STPIT:  Stand % of TPA infected with DM.
  !     STPIV:  Stand % of volume infected with DM.
  !     STPMT:  Stand % of TPA lost to DM mortality.
  !     STPMV:  Stand % of volume lost to DM mortality.
  !     STTPAI: Stand total # infected TPA.
  !     STTPAM: Stand TPA mortality due to DM.
  !     STTPAT: Stand total # TPA.
  !     STTPAX: Stand total # TPA, DBH >= DMRMIN.
  !     STVOL:  Stand total volume in cubic feet.
  !     STVOLI: Stand volume infected with DM.
  !     STVOLM: Stand volume mortality due to DM.

  !  Common block variables and parameters:
  !     BA:     From PLOT; current stand basal area.
  !     CFV:    From ARRAYS; current tree total cubic foot volume.
  !     CSPARR: From MISCOM; array to hold 2 char. rep. of all species.
  !     DBH:    From ARRAYS; current tree DBH.
  !     DMFLAG: From MISCOM; true if mistletoe is present in the stand.
  !     DMMTPA: From MISCOM; array containing mistletoe TPA mortality.
  !     DMRMIN: From MISCOM; minimum tree DBH for DMR/DMI statistics.
  !     FSTMIS: From MISCOM; to print summary headings first time only.
  !     FSTTBL: From MISCOM; to print detail headings first time only.
  !     IAGE:   From PLOT; original stand age.
  !     ICYC:   From CONTRL; current cycle index number.
  !     IMIST:  From MISCOM; array containing tree record DMRs.
  !     IND1:   From ARRAYS; tree list access.
  !     ISCT:   From CONTRL; species ordered list pointer.
  !     ISP:    From ARRAYS; current tree species.
  !     ISVSP4: From MISCOM; Array of top 4 most DM infected species by number.
  !     ITRN:   From CONTRL; current number of tree records.
  !     IY:     From CONTRL; inventory year.
  !     JOSTND: From CONTRL; unit number of stand output.
  !     LSORT4: From MISCOM; Logical flag to set species by infection.
  !     LSTART: From CONTRL; true during summarization of input.
  !     MAXSP:  From PRGPRM; maximum # species.
  !     MAXTRE: From PRGPRM; maximum # tree records.
  !     MGMID:  From PLTCHR; management code.
  !     MISFIT: From MISCOM; which species affected by DM (see MISINT).
  !     MISTBL: From MISCOM; Logical array - what species desired in detailed output.
  !     NPLT:   From PLTCHR; stand ID.
  !     PRTTBL: From MISCOM; Logical flag to print DM detailed output tables.
  !     PROB:   From ARRAYS; trees per acre per tree record.
  !     PRTMIS: From MISCOM; TRUE if DM output tables are to be printed.

  ! Revision History:
  !     24-FEB-98; Update Lance David
  !        Added warning on DMR column of Stand Average Summary Table
  !        (file 2) indicating inclusion of nonhost species.
  !     02-AUG-99; Lance R. David (FHTET-FC)
  !        Added definition and data statements for MYACTS array
  !        and replaced the activity code literal in the CALL OPFIND
  !        statements.
  !        This change was provided by Bob Havis (FMSC) to eliminate
  !        LF95 FORTRAN compiler warnings.
  !     23-DEC-99; Lance R. David (FHTET-FC)
  !        Updated for expansion of FVS stand id (variable NPLT)
  !        from 8 to 26 characters.
  !     21-DEC-00; Lance R. David (FHTET-FC)
  !        Added additional condition when setting variable MISTBL so
  !        that tables will not be generated for host species not present
  !        in the simulation. After processing of the tree list the
  !        condition of having infections present on the species was added
  !        before the specie/diameter class table is written (array SPTPAI).
  !     02-AUG-01; Lance R. David (FHTET-FC)
  !        Initialization of SORTSP array.
  !     23-JAN-02; Lance R. David (FHTET-FC)
  !        Moved if statements controling writing of tables 1, 2 and 3 so
  !        that tables 1 and 2 will print automatically when mistletoe damage
  !        codes are present in the data set and the MISTOFF keyword has not
  !        been issued.
  !        The MISTPRT keyword now controls printing of the DBH Class Tables
  !        (table 3) only.
  !     08-AUG-02; Lance R. David (FHTET-FC)
  !        Restricted some controling variables initialization to cycle 0
  !        so that once a table/species began to print, it would print for
  !        the duration of the simulation. This addresses truncation of
  !        tables that resulted from successful dwarf mistletoe sanitation
  !        management actions.
  !     29-AUG-02; Lance R. David (FHTET-FC)
  !        Variables for sorting and ordering of top 4 species, LSORT4 and
  !        ISVSP4, were initialized with data statements which resulted in
  !        these variables not being reset for the next stand when processing
  !        multiple stands in single execution. These variables moved to
  !        cycle 0 initialization section.
  !     13-JAN-04; Lance R. David (FHTET)
  !        Modifications provided by Don Robinson of ESSA to place Mistletoe
  !        summary tables into the standard FVS output file instead of
  !        directing them to external files. Capability to use the FVS
  !        Database extension the DM summaries was also added.
  !     14-MAR-05; Lance R. David (FHTET)
  !        Added calls to GETID for assignment of report ID Numbers at
  !        time of header writing. Assignments were originally made in
  !        subroutine MISIN0.
  !     07-MAR-08; Lance R. David (FHTET)
  !        Moved local variables to common, necessary for output table
  !        control that need to be maintained for each stand.
  !        Variables moved are: DMFLAG, PRTTBL, LSORT4, MISTBL and ISVSP4
  !  21-APR-09; Lance R. David (FMSC)
  !        Variables IMOUT_ moved to MISCOM.(thanks to Don Robinson)

  implicit none

  !     Parameter statements.

  !     Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !     Common include files.

  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MISCOM.f90'

  !     Variable declarations.

  ! Variables
  logical :: DEBUG
  integer :: MYACTS(1)
  integer :: I,II,I1,I2,I3,IACTK,IDATE,IDBH,IDBSKODE,IDCLAS,IDMR,INFNO,INFSPC,ISPC,ITEMP,ITREE,IVAL,J,&
       &K,L,M,N,NAGE,NP,NTODO,NYEAR
  real    :: P,STBAI,STBAM,STDMI,STDMR,STDMRS,STPIT,STPIV,STPMT,STPMV,STTPAI,STTPAM,STTPAT,STTPAX,&
       &STVOL,STVOLI,STVOLM,TEMP
  real    :: SPTPAM(MAXSP),SPDMR(MAXSP),SPDMI(MAXSP),SPDMR4(4),SPDMI4(4),SPINF4(4),SPMRT4(4),&
       &SPPIN4(4),SPPMR4(4),SPPOC4(4),PRM(2),DCTPA(20),DCINF(20),DCMRT(20),DCSUM(20),DCDMR(20),DCDMI(20),&
       &DCTPAX(20),SDTPA(MAXSP,20),SDINF(MAXSP,20),SDMRT(MAXSP,20),SDSUM(MAXSP,20),SDDMR(MAXSP,20),&
       &SDDMI(MAXSP,20),SDIN0(MAXSP,20),SDIN12(MAXSP,20),SDIN34(MAXSP,20),SDIN56(MAXSP,20),SDTT0(MAXSP),&
       &SDTT12(MAXSP),SDTT34(MAXSP),SDTT56(MAXSP),SORTSP(4),SPDMRS(MAXSP),SPTPAI(MAXSP),SPTPAT(MAXSP),&
       &SPTPAX(MAXSP)

  character(len=2) :: CSP(4)
  character(len=3) :: NLABS(5)
  character(len=42) :: CFMT(20)

  !     Data statements.

  data NLABS  /'TPA','INF','MRT','DMR','DMI'/

  !     These are formats for writing out the species/DBH class data
  !     to the DM detail output file within a DBH class loop.

  data (CFMT(I),I = 1,10)&
       &/'(1X,I5,''  0- 2.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,''  3- 4.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,''  5- 6.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,''  7- 8.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,''  9-10.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 11-12.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 13-14.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 15-16.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 17-18.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 19-20.9" '',7(6X,F7.1),2(4X,F3.1))'/
  data (CFMT(I),I = 11,20)&
       &/'(1X,I5,'' 21-22.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 23-24.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 25-26.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 27-28.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 29-30.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 31-32.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 33-34.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 35-36.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 37-38.9" '',7(6X,F7.1),2(4X,F3.1))',&
       &'(1X,I5,'' 39+    " '',7(6X,F7.1),2(4X,F3.1))'/

  data MYACTS /2007/

  !     Check for debug.

  call DBCHK(DEBUG,'MISPRT',6,ICYC)

  if (DEBUG) write(JOSTND,5)ICYC,LSTART
  5 format(' Begin MISPRT: Cycle, LSTART = ',I5,L8)

  !     Print control done in cycle 0 only so that once started, a table
  !     prints for full duration of the simulation.(LRD 08/09/02)
  !     LSORT4 and ISVSP4 initializations (LRD 08/29/02)

  if (ICYC .eq.0) then
     DMFLAG = .false.
     PRTTBL = .false.
     LSORT4 = .false.

     do I = 1,4
        ISVSP4(I) = 0
     end do

     do I = 1,MAXSP
        MISTBL(I) = .false.
     end do
  end if

  !     STAND TOTAL VARIABLE INITIALIZATIONS.

  STTPAM = 0.0
  STVOL = 0.0
  STVOLI = 0.0
  STVOLM = 0.0
  STBAI = 0.0
  STBAM = 0.0
  STDMR = 0.0
  STDMI = 0.0
  STPIT = 0.0
  STPIV = 0.0
  STPMT = 0.0
  STPMV = 0.0
  STTPAI = 0.0
  STTPAT = 0.0
  STTPAX = 0.0
  STDMRS = 0.0

  !     SPECIES VARIABLE INITIALIZATIONS.

  do I = 1,MAXSP
     SPTPAM(I) = 0.0
     SPDMR(I) = 0.0
     SPDMI(I) = 0.0
     SDTT0(I) = 0.0
     SDTT12(I) = 0.0
     SDTT34(I) = 0.0
     SDTT56(I) = 0.0
     SPTPAI(I) = 0.0
     SPTPAT(I) = 0.0
     SPTPAX(I) = 0.0
     SPDMRS(I) = 0.0
  end do

  !     SPECIES VARIABLE INITIALIZATIONS (TOP 4 MOST INFECTED SPECIES).

  do I = 1,4
     SORTSP(I) = 0.0
     SPDMR4(I) = 0.0
     SPDMI4(I) = 0.0
     SPINF4(I) = 0.0
     SPMRT4(I) = 0.0
     SPPIN4(I) = 0.0
     SPPMR4(I) = 0.0
     SPPOC4(I) = 0.0
     25 continue
  end do

  !     DBH class variable initializations.
  !     20 DBH classes at 2 inch intervals; 0-3, 3-5, 5-7,..., 37-39, 39+.

  do I = 1,20
     DCTPA(I) = 0.0
     DCTPAX(I) = 0.0
     DCINF(I) = 0.0
     DCMRT(I) = 0.0
     DCSUM(I) = 0.0
     DCDMR(I) = 0.0
     DCDMI(I) = 0.0
  end do

  !     Species/DBH class variable initializations.
  !     MAXSP species by 20 DBH classes at 2 inch intervals (see above).

  do I = 1,MAXSP
     do J = 1,20
        SDTPA(I,J) = 0.0
        SDINF(I,J) = 0.0
        SDIN0(I,J) = 0.0
        SDIN12(I,J) = 0.0
        SDIN34(I,J) = 0.0
        SDIN56(I,J) = 0.0
        SDMRT(I,J) = 0.0
        SDSUM(I,J) = 0.0
        SDDMR(I,J) = 0.0
        SDDMI(I,J) = 0.0
     end do
  end do

  if (DEBUG) write(JOSTND,50)
  50 format(' in MISPRT: Begin summary processing')

  !     Calculate species mistletoe infection totals.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)

     !        If there are no trees of this species, skip out.

     if (I1.eq.0) go to 90
     I2 = ISCT(ISPC,2)
     do I3 = I1,I2
        I = IND1(I3)
        P = PROB(I)
        IDMR = IMIST(I)

        if (IDMR .gt.0) DMFLAG = .true.

        if (DBH(I) .ge. DMRMIN) then
           if (IDMR .gt.0) SPTPAI(ISPC) = SPTPAI(ISPC) + P
           SPDMRS(ISPC) = SPDMRS(ISPC) + IDMR * P
           SPTPAX(ISPC) = SPTPAX(ISPC) + P
        end if
        SPTPAT(ISPC) = SPTPAT(ISPC) + P
        80 continue
     end do
     STTPAI = STTPAI + SPTPAI(ISPC)
     STTPAT = STTPAT + SPTPAT(ISPC)
     STTPAX = STTPAX + SPTPAX(ISPC)
     STDMRS = STDMRS + SPDMRS(ISPC)
     90 continue
  end do

  !     If DMFLAG is false, there is no mistletoe in any species
  !     in this cycle. Skip the printing.

  if (.not.DMFLAG) goto 9000

  !     Process MISTABLE keyword (activity code 2007).

  NTODO = 0
  call OPFIND(1,MYACTS(1),NTODO)

  if (NTODO.ne.0) then
     do I = 1,NTODO
        call OPGET(I,2,IDATE,IACTK,NP,PRM)

        !           Test for zero/first cycle.

        if (.not.LSTART) call OPDONE(I,IY(ICYC))

        !           Check for a particular species number.
        !           MISFIT insures that only species of this variant that
        !           are affected by mistletoe will be printed in detailed
        !           output - others will not be included in printing.
        !           The presence of trees (records) of the particular species
        !           is determined from the ISCT array and table will also not
        !           be generated it the species is not currently in the simulation.

        ISPC = IFIX(PRM(1))
        if (ISPC .ne.0) then
           PRTTBL = .true.
           MISTBL(ISPC) = (MISFIT(ISPC) .eq.1.and.&
                &ISCT(ISPC,1) .gt.0)
        else

           !              Otherwise defaults to all species.

           PRTTBL = .true.

           do ISPC = I,MAXSP
              MISTBL(ISPC) = (MISFIT(ISPC) .eq.1.and.&
                   &ISCT(ISPC,1) .gt.0)
              110 continue
           end do
        end if
        120 continue
     end do
  end if

  !     Sort for the top four most infected species only once. This may
  !     happen in the first cycle or it may happen later if infections
  !     were introduced. Anyway, LSORT4 will be FALSE if not sorted yet.

  if (.not.LSORT4) then
     LSORT4 = .true.

     do ISPC = 1,MAXSP

        !           Loop and save top 4 most infected species (based on SPTPAI).
        !           The species numbers are saved in ISVSP4 and sorted by
        !           infection level (saved in SORTSP) from highest to lowest.

        if (SPTPAI(ISPC).gt.SORTSP(4)) then
           SORTSP(4) = SPTPAI(ISPC)
           ISVSP4(4) = ISPC

           do I = 1,3
              do J = I+1,4
                 if (SORTSP(J).gt.SORTSP(I)) then
                    TEMP = SORTSP(I)
                    ITEMP = ISVSP4(I)
                    SORTSP(I) = SORTSP(J)
                    ISVSP4(I) = ISVSP4(J)
                    SORTSP(J) = TEMP
                    ISVSP4(J) = ITEMP
                 end if
                 160 continue
              end do
              170 continue
           end do
        end if
        180 continue
     end do
  end if

  !     Get current year and age.

  NYEAR = IY(ICYC+1)
  NAGE = IAGE+NYEAR-IY(1)

  !     Get mistletoe mortality rates.

  call MISMRT(.false.)

  !     Do stand related calculations.

  do ITREE = 1,ITRN
     ISPC = ISP(ITREE)

     !        Add up total, infected, and mortality TPA and sum DM ratings
     !        for: each DBH class (variables begin with 'DC') and each
     !        species/DBH class (variables begin with 'SD').
     !        Classification into DBH classes made consistent with
     !        other models; 0-2.9, 3.0-4.9, etc.

     IVAL = IFIX(DBH(ITREE))
     IDBH = IVAL / 2 + MOD(IVAL,2)
     IDBH = MAX(1,MIN(20,IDBH))
     P = PROB(ITREE)
     IDMR = IMIST(ITREE)

     DCTPA(IDBH) = DCTPA(IDBH) + P

     if (DBH(ITREE) .ge. DMRMIN) DCTPAX(IDBH) = DCTPAX(IDBH) + P

     DCMRT(IDBH) = DCMRT(IDBH) + DMMTPA(ITREE)
     SDTPA(ISPC,IDBH) = SDTPA(ISPC,IDBH) + P
     SDMRT(ISPC,IDBH) = SDMRT(ISPC,IDBH) + DMMTPA(ITREE)

     if (IDMR .gt.0.and. DBH(ITREE) .ge. DMRMIN) then
        DCINF(IDBH) = DCINF(IDBH) + P
        DCSUM(IDBH) = DCSUM(IDBH) + IDMR * P
        SDINF(ISPC,IDBH) = SDINF(ISPC,IDBH) + P
        SDSUM(ISPC,IDBH) = SDSUM(ISPC,IDBH) + IDMR * P

        if (IDMR .eq.1.or. IDMR .eq.2)&
             &SDIN12(ISPC,IDBH) = SDIN12(ISPC,IDBH) + P
        if (IDMR .eq.3.or. IDMR .eq.4)&
             &SDIN34(ISPC,IDBH) = SDIN34(ISPC,IDBH) + P
        if (IDMR .eq.5.or. IDMR .eq.6)&
             &SDIN56(ISPC,IDBH) = SDIN56(ISPC,IDBH) + P
     else
        SDIN0(ISPC,IDBH) = SDIN0(ISPC,IDBH) + P
     end if

     !        Calculate stand volumes, BAs, and TPAs at beginning of
     !        cycle, infected with DM, and mortality due to DM.

     STVOL = STVOL + CFV(ITREE) * P
     if (IMIST(ITREE) .gt.0.and. DBH(ITREE) .ge. DMRMIN) then
        STVOLI = STVOLI + CFV(ITREE) * P
     end if

     if (DMMTPA(ITREE) .gt.0.0) then
        STTPAM = STTPAM + DMMTPA(ITREE)
        STVOLM = STVOLM + CFV(ITREE) * DMMTPA(ITREE)
     end if
  end do

  if (STVOL .ne.0) then
     STBAI = BA / STVOL * STVOLI
     STBAM = BA / STVOL * STVOLM
  end if

  !     Lump the largest 10 DBH classes into the tenth class
  !     because the (file 3) table only prints the first 10.

  do IDCLAS = 11,20
     DCTPA(10) = DCTPA(10)  + DCTPA(IDCLAS)
     DCTPAX(10) = DCTPAX(10) + DCTPAX(IDCLAS)
     DCINF(10) = DCINF(10)  + DCINF(IDCLAS)
     DCMRT(10) = DCMRT(10)  + DCMRT(IDCLAS)
     DCSUM(10) = DCSUM(10)  + DCSUM(IDCLAS)
  end do

  !     Calculate average mistletoe ratings by 2 inch DBH classes
  !     using all trees and infected-only trees.

  do IDCLAS = 1,10
     if (DCTPAX(IDCLAS) .ne.0)&
          &DCDMR(IDCLAS) = DCSUM(IDCLAS) / DCTPAX(IDCLAS)
     if (DCINF(IDCLAS) .ne.0)&
          &DCDMI(IDCLAS) = DCSUM(IDCLAS) / DCINF(IDCLAS)
  end do

  !     Calculate average mistletoe ratings by species/DBH classes and
  !     total infected by DMR class for each species using all trees
  !     and infected-only trees.

  do ISPC = 1,MAXSP
     do IDCLAS = 1,20
        if (SDTPA(ISPC,IDCLAS) .ne.0) SDDMR(ISPC,IDCLAS) = &
             &SDSUM(ISPC,IDCLAS) / SDTPA(ISPC,IDCLAS)
        if (SDINF(ISPC,IDCLAS) .ne.0) SDDMI(ISPC,IDCLAS) = &
             &SDSUM(ISPC,IDCLAS) / SDINF(ISPC,IDCLAS)
        SDTT0(ISPC) = SDTT0(ISPC) + SDIN0(ISPC,IDCLAS)
        SDTT12(ISPC) = SDTT12(ISPC) + SDIN12(ISPC,IDCLAS)
        SDTT34(ISPC) = SDTT34(ISPC) + SDIN34(ISPC,IDCLAS)
        SDTT56(ISPC) = SDTT56(ISPC) + SDIN56(ISPC,IDCLAS)
     end do
  end do

  !     Calculate stand average mistletoe ratings and infection levels
  !     using all trees and infected-only trees.

  if (STTPAX .ne.0) then
     STDMR = STDMRS / STTPAX
     STPIT = STTPAI / STTPAT * 100.0
     STPMT = STTPAM / STTPAT * 100.0
  end if

  if (STTPAI .ne.0) STDMI = STDMRS / STTPAI
  if (STVOL .ne.0) then
     STPIV = STVOLI / STVOL * 100.0
     STPMV = STVOLM / STVOL * 100.0
  end if

  !     Do species specific calculations.

  do ISPC = 1,MAXSP

     !        Calculate species average mistletoe ratings and infection
     !        levels using all trees and infected-only trees.

     if (SPTPAX(ISPC) .ne.0) SPDMR(ISPC) = SPDMRS(ISPC)/SPTPAX(ISPC)
     if (SPTPAI(ISPC) .ne.0) SPDMI(ISPC) = SPDMRS(ISPC)/SPTPAI(ISPC)

     !        Add up the total TPA of this species that are infected by
     !        or died due to DM and calculate mortality levels.

     I1 = ISCT(ISPC,1)
     if (I1.eq.0) goto 265
     I2 = ISCT(ISPC,2)

     do I3 = I1,I2
        I = IND1(I3)
        SPTPAM(ISPC) = SPTPAM(ISPC) + DMMTPA(I)
     end do
     265 continue
  end do

  !     Pick off top 4 most infected species statistics for species
  !     summary table (top 4 saved in ISVSP4).

  do INFSPC = 1,4

     !        Get a current species number (i.e. between 1 & 11; INFNO=0
     !        implies there are less than 4 infected species in the stand)

     INFNO = ISVSP4(INFSPC)
     if (INFNO .eq.0) goto 270

     SPDMR4(INFSPC) = SPDMR(INFNO)
     SPDMI4(INFSPC) = SPDMI(INFNO)
     SPINF4(INFSPC) = SPTPAI(INFNO)
     SPMRT4(INFSPC) = SPTPAM(INFNO)

     if (SPTPAT(INFNO) .ne.0.0) then
        SPPIN4(INFSPC) = SPTPAI(INFNO) / SPTPAT(INFNO) * 100.0
        SPPMR4(INFSPC) = SPTPAM(INFNO) / SPTPAT(INFNO) * 100.0
     end if
     if (STTPAT .ne.0) SPPOC4(INFSPC) = SPTPAT(INFNO) / STTPAT *&
          &100.0
     270 continue
  end do

  !     TRANSLATE TOP 4 INFECTED SPECIES NUMBERS TO CHAR. STRINGS.

  do I = 1,4
     INFNO = ISVSP4(I)
     CSP(I) = '**'
     if (INFNO.ne.0) CSP(I) = CSPARR(INFNO)
  end do

  !     CALL THE DBS MODULE TO OUTPUT DMDATA TO A DATABASE

  IDBSKODE = 1
  call DBSMIS1(NYEAR,NPLT,CSP,&
       &SPDMR4,SPDMI4,SPINF4,SPMRT4,SPPIN4,SPPMR4,SPPOC4,&
       &IDBSKODE)

  call DBSMIS2(NYEAR,NPLT,NAGE,&
       &NINT(STTPAT),NINT(BA),&
       &NINT(STVOL),NINT(STTPAI),&
       &NINT(STBAI),NINT(STVOLI),&
       &NINT(STTPAM),NINT(STBAM),&
       &NINT(STVOLM),NINT(STPIT),&
       &NINT(STPIV),NINT(STPMT),NINT(STPMV),STDMR,STDMI,&
       &IDBSKODE)

  if (PRTMIS) then
     call DBSMIS3(NYEAR,NPLT,NLABS,&
          &DCTPA,DCINF,DCMRT,DCDMR,DCDMI,&
          &IDBSKODE)
  end if

  !     BRANCH AROUND MAIN OUTPUT IF THAT OUTPUT STREAM IS SILENCED

  if (IDBSKODE.eq.0) goto 280

  !     Check for first time through this routine for opening summary
  !     table files and printing summary table headings.
  !     Summary tables 1 and 2 are generated by default. Table 3 is
  !     generated when the MISTPRT keyword is used (PRTMIS = true).

  if (FSTMIS) then
     FSTMIS = .false.
     call GETID(IDMSOUT(1))
     call GETID(IDMSOUT(2))

     call GETLUN(IMOUT1)
     call GETLUN(IMOUT2)

     write(IMOUT1,'(2(/1X,I5))') IDMSOUT(1),IDMSOUT(1)
     write(IMOUT1,610) IDMSOUT(1)
     write(IMOUT1,600) IDMSOUT(1)
     write(IMOUT1,605) IDMSOUT(1)
     write(IMOUT1,610) IDMSOUT(1)
     write(IMOUT1,615) IDMSOUT(1)
     write(IMOUT1,620) IDMSOUT(1),NPLT,MGMID
     write(IMOUT1,615) IDMSOUT(1)
     write(IMOUT1,720) IDMSOUT(1),DMRMIN
     write(IMOUT1,625) IDMSOUT(1)
     write(IMOUT1,630) IDMSOUT(1)
     write(IMOUT1,615) IDMSOUT(1)
     write(IMOUT1,635) IDMSOUT(1)
     write(IMOUT1,640) IDMSOUT(1)
     write(IMOUT1,645) IDMSOUT(1)

     write(IMOUT1,650) IDMSOUT(1),((CSP(I),I = 1,4),J = 1,7)
     write(IMOUT1,655) IDMSOUT(1)

     write(IMOUT2,'(2(/1X,I5))') IDMSOUT(2),IDMSOUT(2)
     write(IMOUT2,610) IDMSOUT(2)
     write(IMOUT2,700) IDMSOUT(2)
     write(IMOUT2,705) IDMSOUT(2)
     write(IMOUT2,710) IDMSOUT(2)
     write(IMOUT2,615) IDMSOUT(2)
     write(IMOUT2,620) IDMSOUT(2),NPLT,MGMID
     write(IMOUT2,615) IDMSOUT(2)
     write(IMOUT2,720) IDMSOUT(2),DMRMIN
     write(IMOUT2,725) IDMSOUT(2)
     write(IMOUT2,730) IDMSOUT(2)
     write(IMOUT2,615) IDMSOUT(2)
     write(IMOUT2,735) IDMSOUT(2)
     write(IMOUT2,740) IDMSOUT(2)
     write(IMOUT2,745) IDMSOUT(2)
     write(IMOUT2,750) IDMSOUT(2)
     write(IMOUT2,755) IDMSOUT(2)

     if (PRTMIS) then

        call GETID(IDMSOUT(3))
        call GETLUN(IMOUT3)

        write(IMOUT3,'(2(/1X,I5))') IDMSOUT(3),IDMSOUT(3)
        write(IMOUT3,610) IDMSOUT(3)
        write(IMOUT3,800) IDMSOUT(3)
        write(IMOUT3,805) IDMSOUT(3)
        write(IMOUT3,810) IDMSOUT(3)
        write(IMOUT3,615) IDMSOUT(3)
        write(IMOUT3,620) IDMSOUT(3),NPLT,MGMID
        write(IMOUT3,615) IDMSOUT(3)
        write(IMOUT3,720) IDMSOUT(3),DMRMIN
        write(IMOUT3,825) IDMSOUT(3)
        write(IMOUT3,830) IDMSOUT(3)
        write(IMOUT3,615) IDMSOUT(3)
        write(IMOUT3,835) IDMSOUT(3)
        write(IMOUT3,840) IDMSOUT(3)
        write(IMOUT3,845) IDMSOUT(3)
        write(IMOUT3,850) IDMSOUT(3)
     end if
  end if

  write(IMOUT1,660) IDMSOUT(1),NYEAR,&
       &(SPDMR4(I),I = 1,4),(SPDMI4(J),J = 1,4),&
       &(NINT(SPINF4(K)),K = 1,4),(NINT(SPMRT4(L)),L = 1,4),&
       &(NINT(SPPIN4(M)),M = 1,4),(NINT(SPPMR4(N)),N = 1,4),&
       &(NINT(SPPOC4(II)),II = 1,4)

  write(IMOUT2,760) IDMSOUT(2),NYEAR,NAGE,&
       &NINT(STTPAT),NINT(BA),&
       &NINT(STVOL),NINT(STTPAI),NINT(STBAI),NINT(STVOLI),&
       &NINT(STTPAM),NINT(STBAM),NINT(STVOLM),NINT(STPIT),&
       &NINT(STPIV),NINT(STPMT),NINT(STPMV),STDMR,STDMI

  if (PRTMIS) then
     write(IMOUT3,855) IDMSOUT(3), NYEAR
     write(IMOUT3,860) IDMSOUT(3),NLABS(1),(DCTPA(I),I = 1,10)
     write(IMOUT3,860) IDMSOUT(3),NLABS(2),(DCINF(I),I = 1,10)
     write(IMOUT3,860) IDMSOUT(3),NLABS(3),(DCMRT(I),I = 1,10)
     write(IMOUT3,861) IDMSOUT(3),NLABS(4),(DCDMR(I),I = 1,10)
     write(IMOUT3,861) IDMSOUT(3),NLABS(5),(DCDMI(I),I = 1,10)
  end if

  !     CHECK FOR PRINTING THE DETAILED OUTPUT TABLES.

  280 if (PRTTBL) then

     !        CHECK FOR FIRST TIME HERE FOR OPENING DETAIL TABLE FILE
     !        AND PRINTING DETAIL TABLE HEADINGS.

     if (FSTTBL) then

        FSTTBL = .false.
        call GETID(IDMSOUT(4))
        call GETLUN(IMOUT4)

        write(IMOUT4,'(2(/1X,I5))') IDMSOUT(4),IDMSOUT(4)
        write(IMOUT4,610) IDMSOUT(4)
        write(IMOUT4,900) IDMSOUT(4)
        write(IMOUT4,905) IDMSOUT(4)
        write(IMOUT4,910) IDMSOUT(4)
        write(IMOUT4,615) IDMSOUT(4)
        write(IMOUT4,620) IDMSOUT(4),NPLT,MGMID
        write(IMOUT4,615) IDMSOUT(4)
        write(IMOUT4,720) IDMSOUT(4),DMRMIN
        write(IMOUT4,925) IDMSOUT(4)
        write(IMOUT4,930) IDMSOUT(4)
        write(IMOUT4,615) IDMSOUT(4)
     end if

     do ISPC = 1,MAXSP

        !         CHECK IF THIS PARTICULAR SPECIES WAS REQUESTED AND
        !         THE SPECIES HAS MISTLETOE INFECTIONS.

        if (MISTBL(ISPC).and. SPTPAI(ISPC) .gt.0.0) then

           write(IMOUT4,935) IDMSOUT(4),NYEAR
           write(IMOUT4,940) IDMSOUT(4),CSPARR(ISPC)

           !           WRITE SUBHEADINGS.

           write(IMOUT4,615) IDMSOUT(4)
           write(IMOUT4,945) IDMSOUT(4)
           write(IMOUT4,950) IDMSOUT(4)
           write(IMOUT4,955) IDMSOUT(4)

           !           WRITE DATA FOR EACH DBH CLASS FOR THIS SPECIES.

           do IDCLAS = 1,20
              if (SDINF(ISPC,IDCLAS) .lt.0.05) then
                 SDDMR(ISPC,IDCLAS) = 0.0
                 SDDMI(ISPC,IDCLAS) = 0.0
              end if

              write(IMOUT4,CFMT(IDCLAS)) IDMSOUT(4),&
                   &SDTPA(ISPC,IDCLAS),&
                   &SDINF(ISPC,IDCLAS),&
                   &SDIN0(ISPC,IDCLAS),&
                   &SDIN12(ISPC,IDCLAS),&
                   &SDIN34(ISPC,IDCLAS),&
                   &SDIN56(ISPC,IDCLAS),&
                   &SDMRT(ISPC,IDCLAS),&
                   &SDDMR(ISPC,IDCLAS),SDDMI(ISPC,IDCLAS)
           end do

           !           WRITE COLUMN TOTALS AND AVERAGES.

           write(IMOUT4,955) IDMSOUT(4)
           write(IMOUT4,960) IDMSOUT(4),SPTPAT(ISPC),&
                &SPTPAI(ISPC),SDTT0(ISPC),&
                &SDTT12(ISPC),SDTT34(ISPC),&
                &SDTT56(ISPC),SPTPAM(ISPC),&
                &SPDMR(ISPC),SPDMI(ISPC)
           write(IMOUT4,615) IDMSOUT(4)
        end if
     end do
  end if

  !     FORMAT STATEMENTS USED BY ALL FOUR OUTPUT STREAMS

  615 format(1X,I5,' ')
  620 format(1X,I5,' ','Stand ID: ',A26,3X,'Management Code: ',A4,3X,&
       &'Revision code: 991201')
  720 format(1X,I5,' ','DMRs and DMIs calculated for trees with DBH >=',&
       &F4.1)

  !     FORMAT STATEMENTS FOR SPECIES STATISTICS TABLE

  600 format(1X,I5,'1',30X,'DWARF MISTLETOE INFECTION AND MORTALITY ',&
       &'STATISTICS SUMMARY TABLE')
  605 format(1X,I5,' ',41X,'(BY SPECIES; TOP 4 MOST INFECTED SPECIES)')
  610 format(1X,I5,' ',124('-'))
  625 format(1X,I5,' ','MEAN DMR = Average dwarf mistletoe rating for',&
       &' all trees of that species.')
  630 format(1X,I5,' ','MEAN DMI = Average dwarf mistletoe rating for',&
       &' infected-only trees of that species.')
  635 format(1X,I5,' ',41X,'TPA INFECTED',7X,'TPA MORTALITY',9X,&
       &'% TPA',11X,'% TPA',8X,'COMPOSITION')
  640 format(1X,I5,' ','YEAR',4X,'MEAN DMR',8X,'MEAN DMI',11X,&
       &'WITH DM',13X,'FROM DM',11X,'INFECTED',7X,'MORTALITY',4X,&
       &'OF STAND (%TPA)')
  645 format(1X,I5,' ','----',1X,2(15('-'),1X),2(19('-'),1X),&
       &2(15('-'),1X),15('-'))
  650 format(1X,I5,' ',4X,9(2X,A),7(3X,A),1X,12(2X,A))
  655 format(1X,I5,' ',4X,8(1X,3('-')),8(1X,4('-')),12(1X,3('-')))
  660 format(1X,I5,' ',I4,8(1X,F3.1),8(1X,I4),12(1X,I3))

  !     FORMAT STATEMENTS FOR STAND STATISTICS TABLE

  700 format(1X,I5,'1',24X,'DWARF MISTLETOE INFECTION AND MORTALITY ',&
       &'STATISTICS SUMMARY TABLE')
  705 format(1X,I5,' ',42X,'(STAND COMPOSITE; PER ACRE)')
  710 format(1X,I5,' ',112('-'))
  725 format(1X,I5,' ','MEAN DMR = Average dwarf mistletoe rating for',&
       &' all trees in the stand.',14X,'* --WARNING-- Calculation may')
  730 format(1X,I5,' ','MEAN DMI = Average dwarf mistletoe rating for',&
       &' infected-only trees in the stand.',9X,&
       &'include nonhost species.')
  735 format(1X,I5,' ',15X,'START OF CYCLE',8X,'DM INFECTION',9X,'DM ',&
       &'MORTALITY')
  740 format(1X,I5,' ',10X,3(3X,18('-')),31X,'*')
  745 format(1X,I5,' ',9X,3(4X,'TREES',3X,'BA',4X,'VOL'),4X,2('%TPA',2X,&
       &'%VOL',3X),'MEAN',2X,'MEAN')
  750 format(1X,I5,' ','YEAR',3X,'AGE',3X,3('/ACRE',2X,'SQFT',2X,&
       &'CU FT',3X),1X,2('INF',3X),2('MORT',2X),2X,'DMR',3X,'DMI')
  755 format(1X,I5,' ','----',3X,'---',3X,&
       &3('-----',2X,'----',2X,'-----',3X),3('----',2X,'----',3X))
  760 format(1X,I5,' ',I4,3X,I3,3(3X,I5,2X,I4,2X,I5),2(3X,I4,2X,I4),&
       &4X,F3.1,3X,F3.1)

  !     FORMAT STATEMENTS FOR 2" DBH CLASS STATISTICS TABLE

  800 format(1X,I5,'1',28X,'DWARF MISTLETOE INFECTION AND MORTALITY ',&
       &'STATISTICS SUMMARY TABLE')
  805 format(1X,I5,' ',46X,'(BY 2 INCH DIAMETER CLASSES)')
  810 format(1X,I5,' ',120('-'))
  825 format(1X,I5,' ','MEAN DMR = Average dwarf mistletoe rating',&
       &' for all trees of that diameter class.')
  830 format(1X,I5,' ','MEAN DMI = Average dwarf mistletoe rating',&
       &' for infected-only trees of that diameter class.')
  835 format(1X,I5,' ','YEAR',51X,'2 INCH DIAMETER CLASSES')
  840 format(1X,I5,' ','----',9X,107('-'))
  845 format(1X,I5,' ',14X,'0-2.9"',5X,'3-4.9"',5X,'5-6.9"',5X,&
       &'7-8.9"',5X,'9-10.9"',3X,'11-12.9"',3X,'13-14.9"',3X,&
       &'15-16.9"',3X,'17-18.9"',4X,'19  +"')
  850 format(1X,I5,' ',13X,10(8('-'),3X))
  855 format(1X,I5,' ',I4)
  860 format(1X,I5,' ',7X,A3,10(4X,F7.1))
  861 format(1X,I5,' ',7X,A3,10(8X,F3.1))

  !     FORMAT STATEMENTS FOR SPECIES/DBH CLASS STATISTICS TABLE

  900 format(1X,I5,'1',25X,'DWARF MISTLETOE INFECTION AND MORTALITY ',&
       &'STATISTICS DETAIL TABLE')
  905 format(1X,I5,' ',32X,&
       &'(BY SPECIES, BY 2 INCH DIAMETER CLASS, BY CYCLE)')
  910 format(1X,I5,' ',114('-'))
  925 format(1X,I5,' ','MEAN DMR = Average DM rating for all trees of',&
       &' that species/DBH class.')
  930 format(1X,I5,' ','MEAN DMI = Average DM rating for infected-only',&
       &' trees of that species/DBH class.')
  935 format(1X,I5,' ','Year:    ',I4)
  940 format(1X,I5,' ','Species:   ',A2)
  945 format(1X,I5,' ',9X,7(3X,'TREES/ACRE'),2(3X,'MEAN'))
  950 format(1X,I5,' ','DBH CLASS',6X,'TOTAL',6X,'INFECTED',4X,&
       &'UNINFECTED',5X,'DMR 1-2',6X,'DMR 3-4',6X,'DMR 5-6',5X,&
       &'MORTALITY',4X,'DMR',4X,'DMI')
  955 format(1X,I5,' ',9('-'),7(3X,10('-')),2(3X,4('-')))
  960 format(1X,I5,' ',' SUM/AVG ',7(6X,F7.1),2(4X,F3.1))

  !     COMMON RETURN.

  9000 continue

  if (DEBUG) write(JOSTND,9010)ICYC
  9010 format(' End MISPRT: Cycle = ',I5)

  return
end
