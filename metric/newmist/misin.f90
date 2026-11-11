!============================!
!   metric/newmist : misin   !
!============================!
! METRIC-NEWMIST $Id$
subroutine MISIN(PASKEY,ARRAY,LNOTBK,LKECHO)
  implicit none

  !  Purpose:
  !     Reads in mistletoe keywords and processes the ones that aren't
  !  cycle dependent.

  !  Call list definitions:
  !     ARRAY:  (I) Array containing keyword entries.
  !     LNOTBK: (O) Logical array depicting blank keyword fields.
  !     PASKEY: (I) Character buffer containing keyword name.

  !  Local variable definitions:
  !     DEBUG:  Logical flag to turn debug on and off.
  !     IDT:    Temporary variable to hold date from keyword.
  !     ISIZE:  Size of a keyword entry (10 bytes).
  !     ISPL:   Numeric species code.
  !     KARD:   Character buffer returned from KEYRDR.
  !     KEY:    Keyword number used in MISKEY entry point call list.
  !     KEYWRD: Character buffer to hold local copy of PASKEY.
  !     KODE:   Error code passed back from keyword reading routines.
  !     NUMBER: Keyword number passed back from FNDKEY.
  !     TABLE:  Character array of possible keywords.

  !  Common block variables and parameters:

  !     DMFILE: From MISCOM; array to hold DM table perm. file names.
  !     DMDETH: From DMCOM;
  !     DMETUN: From DMCOM;
  !     DMFLWR: From DMCOM;
  !     DMITUN: From DMCOM;
  !     DMKTUN: From DMCOM;
  !     DMLTNP: From DMCOM;
  !     DMLTRX: From DMCOM;
  !     DMOPAQ: From DMCOM;
  !     DMOPQM: From DMCOM;
  !     DMSTUN: From DMCOM;
  !     DMRMIN: From MISCOM; minimum tree DBH for DMR/DMI statistics.
  !     ICYC:   From CONTRL; cycle index number.
  !     IREAD:  From CONTRL; logical unit number for keyword input.
  !     IRECNT: From CONTRL; number of keyword parameter records read.
  !     ITYPE:  From PLOT;   input stand habitat type code.
  !     JOSTND: From CONTRL; logical unit number for stand output.
  !     MAXCYC: From PRGPRM; maximum # cycles.
  !     MAXSP:  From PRGPRM; maximum # species.
  !     MISFLG: From MISCOM; logical flag for turning mistletoe on/off.
  !     NEWMOD: From DMCOM;
  !     NSP:    From PLOT;   valid species-tree class codes.
  !     PRTMIS: From MISCOM; log. flag for turning DM printing on/off.

  !  Revision History :
  !     03/19/96 - Matthew K. Thompson (MAG)
  !                Changed MISTGMOD keyword to use supplemental to provide
  !                growth modification proportions for 1 species DMRs 1-6.
  !                Field 2 of keyword record specifies species being
  !                modified.  This change was made for the Interim model
  !                by Lance R. David on 02/28/95.
  !     02/26/98 - Lance R. David (FHTET)
  !                Default value for MISTPRT field 1 (DMRMIN) set to 1.0
  !     12/05/01 - Lance R. David (MAG)
  !                Initialization of DMRMIN moved to MISIN0.f
  !                Slight modification to clean up MISPRT keyword process.
  !     10/20/03 - Bob Havis (FMSC)
  !                COMMENT keyword processing removed.
  !                Added LFLAG to KEYRDR call statement argument list.
  !     12/20/03 - Don Robinson (ESSA)
  !                Modified so that all Dwarf Mistletoe summaries are written
  !                to the FVS standard output file. Also added ability to
  !                utilize the FVS database extension.
  !                Subroutine (MISOUT) and parameter file (MISPRM) removed.
  !     03/14/05 - Lance David (FHTET)
  !                Removed manipulation of IDMSOUT(3) and IDMSOUT(4) at
  !                MISTPRT and MISTABLE keyword processing.
  !     07/13/05 - Lance David (FHTET)
  !                SPDECD invalid species return code changed to -999.
  !                Species format changed from I2 to I3.
  !     01/25/06 - Robert Havis (FMSC)
  !                Added conditional at writing of keywords to utilize new
  !                FVS keywords ECHO and NOECHO
  !     02/02/06 - Lance David (FHTET)
  !                Removed obsolete message "*** MISTMORT keyword ignored in
  !                CR variant". Modified keyword output for MISTMULT and
  !                MISTMORT keywords to be more descriptive.
  !     03/22/07 - Lance David (FHTET)
  !                Functionality of MistOff keyword was changed so that it
  !                is not required to be processed prior to tree data input.
  !                Added inclusion of ARRAYS.F77, which required change of
  !                local variable "ISP" to "ISPL".
  !                MistOff keyword text modified to reflect change in behavior.
  !     07/09/07 - Lance David (FHTET)
  !                With restructuring of damage code processing after reading
  !                of the input tree data, conditional re-zeroing of IMIST
  !                array added for previous update is no longer needed and
  !                was removed along with inclusion of common ARRAYS.F77.

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MISCOM.f90'
  include 'DMCOM.f90'
  include 'METRIC.f90'

  !.... Variable declarations.

  ! Variables
  integer :: ISIZE
  parameter (ISIZE = 30)

  logical :: LNOTBK(7),DEBUG,LCHK(2),LOK,LKECHO
  integer :: I,IDT,ISPL,KEY,KODE,NUMBR,IKD,IPRMPT
  integer :: LTNP(2),ERRT(2),J,K,M,N,P,IRTNCD
  integer :: DMT(5,3)
  real    :: DMLRX(2,2,4),X
  real    :: ARRAY(7)
  character(len=8) :: TABLE(ISIZE)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=8) :: LTNM
  character(len=10) :: KARD(7)
  character(len=8) :: RTYPE(2)

  !.... Data statements.

  data TABLE/&
       &'MISTMULT','MISTHMOD','END     ','MISTPREF','MISTMORT',&
       &'MISTPRT ','MISTGMOD','MISTOFF ','        ','MISTPINF',&
       &'MISTABLE','NEWSPRED','DMCRTHRD','DMSED   ','DMOPAQ  ',&
       &'DMKTUNE ','DMSTUNE ','DMITUNE ','DMETUNE ','DMLIGHT ',&
       &'DMDEATH ','DMCAP   ','DMCLUMP ','DMAUTO  ','DMTABLE ',&
       &'        ','DMOPQMLT','DMFLOWER','DMBCI   ','DMBCA   '/

  !.... Initialize variable NEWMOD.

  NEWMOD = .false.

  !.... See 'OPLIST' for arrays with OPNEW values and all that
  !.... weird stuff in arrays ITRSL1() and ITRSL2().

  !.... Check for debug.

  call DBCHK(DEBUG,'MISIN',5,ICYC)

  if (DEBUG)write(JOSTND,10)ICYC,ITYPE
  10 format(' Begin MISIN: Cycle, ITYPE = ',I5,I5)

  !.... Load the passed keyword into KEYWRD.

  KEYWRD = PASKEY

  !.... Call the keyword reader.

  50 continue
  call KEYRDR(IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,ARRAY,IRECNT,KODE,&
       &KARD,LFLAG,LKECHO)

  !.... Process errors; 0=no error, 1=first column blank, 2=EOF.

  if (KODE.ne.0) then
     if (KODE.eq.2) then
        call ERRGRO(.false.,2)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     else
        call ERRGRO(.true.,6)
     end if
     go to 50
  end if

  call FNDKEY(NUMBR,KEYWRD,TABLE,ISIZE,KODE,DEBUG,JOSTND)

  !.... Return codes; 0=no error, 1=keyword not found.

  if (KODE.eq.1) then
     call ERRGRO(.true.,1)
     go to 50
  end if
  go to 70

  !....Special EOF target.

  60 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  !.....Process the keyword.

  70 continue
  go to( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,&
       &1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,&
       &2100,2200,2300,2400,2500,2600,2700,2800,2900,3000),&
       &NUMBR

  !.... Option number 1: MISTMULT.

  100 continue

  !.... Check for mistletoe spread probability adjustment multipliers.
  !.... ARRAY(1): effective date, ARRAY(2): species number, ARRAY(3):
  !.... probability(+) multiplier, ARRAY(4): probability(-) multiplier.

  if (.not.LNOTBK(3)) ARRAY(3) = 1.0
  if (.not.LNOTBK(4)) ARRAY(4) = 1.0

  !.... Check for blank date.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !.... Get species code.

  call SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-999) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid multipliers.

     if (ARRAY(3).lt.0.0) then
        write(JOSTND,8100) ARRAY(3)
        ARRAY(3) = 1.0
     end if
     if (ARRAY(4).lt.0.0) then
        write(JOSTND,8100) ARRAY(4)
        ARRAY(4) = 1.0
     end if

     !....    Insert activity into schedule.

     call OPNEW(KODE,IDT,2001,3,ARRAY(2))
     if (KODE.gt.0) go to 50
     if (LKECHO)write(JOSTND,120) KEYWRD,IDT,KARD(2),ISPL,ARRAY(3),&
          &ARRAY(4)
     120 format(/A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',&
          &I3,'); DMR Increase Multiplier=',F6.2,&
          &'; DMR Decrease Multiplier=',F6.2)
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if
  go to 50

  !.... Option number 2: MISTHMOD.

  200 continue
  !.... Mistletoe height growth modification proportion values.
  !.... ARRAY(1): effective date, ARRAY(2): species number
  !.... Supplemental record: fields 1-6, height growth proportions
  !.... for DMRs 1-6.

  !.... Check for blank date.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !.... Get species code.

  call SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-999) goto 50

  !.... Check for invalid species code.

  if (ISPL.lt.0.or.ISPL.gt.MAXSP) then
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)

     !.... Process height growth modification proportions from
     !.... supplemental record

  else
     ARRAY(1) = ARRAY(2)
     read(IREAD,*,end = 60) (ARRAY(I+1),I = 1,6)

     !....    Insert activity into schedule.

     call OPNEW(KODE,IDT,2004,7,ARRAY(1))
     if (KODE.gt.0) go to 50
     if (LKECHO)write(JOSTND,250)KEYWRD,IDT,KARD(2),ISPL
     250 format(/A8,'   Date/Cycle=',I5,'; Species= ',A3,&
          &' (',I3,'):')
     do I = 1,6
        if (LKECHO)write(JOSTND,280) I,ARRAY(I+1)
        280 format(10X,' DMR=',I1,&
             &';  height growth modification proportion=',&
             &F10.2)
        290 continue
     end do
  end if
  go to 50

  !.... Option number 3: END.

  300 continue

  !.... End mistletoe keyword processing.

  if (LKECHO)write(JOSTND,310) KEYWRD
  310 format(/A8,'   End of mistletoe keywords.')
  go to 9000

  !.... Option number 4: MISTPREF.

  400 continue

  !.... Check for mistletoe cutting preference values.
  !.... ARRAY(1): effective date, ARRAY(2): species number
  !.... Supplemental record: fields 1-6, preferences for DMRs 1-6.

  !.... Check for blank date.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !.... Get species code.

  call SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-999) goto 50

  !.... Check for invalid species code.

  if (ISPL.lt.0.or.ISPL.gt.MAXSP) then
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)

     !.... Process preferences from supplemental record

  else
     ARRAY(1) = ARRAY(2)
     read(IREAD,*,end = 60) (ARRAY(I+1),I = 1,6)

     !....    Insert activity into schedule.

     call OPNEW(KODE,IDT,2002,7,ARRAY(1))
     if (KODE.gt.0) go to 50
     if (LKECHO)write(JOSTND,450)KEYWRD,IDT,KARD(2),ISPL
     450 format(/A8,'   Date/Cycle=',I5,'; Species= ',A3,&
          &' (',I3,'):')
     do I = 1,6
        if (LKECHO)write(JOSTND,480) I,ARRAY(I+1)
        480 format(' DMR=',I1,';  cutting preference=',F10.2)
        490 continue
     end do
  end if
  go to 50

  !.... Option number5: MISTMORT.

  500 continue

  !.... Check for mistletoe mortality percentage multiplier.
  !.... ARRAY(1): effective date, ARRAY(2): species number,
  !.... ARRAY(3): mortality percentage multiplier.

  if (.not.LNOTBK(3)) ARRAY(3) = 1.0

  !.... Check for blank date.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !.... Get species code.

  call SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-999) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid multipliers.

     if (ARRAY(3).lt.0.0) then
        write(JOSTND,8100) ARRAY(3)
        ARRAY(3) = 1.0
     end if

     !....    Insert activity into schedule.

     call OPNEW(KODE,IDT,2003,2,ARRAY(2))
     if (KODE.gt.0) go to 50
     if (LKECHO)write(JOSTND,520) KEYWRD,IDT,KARD(2),ISPL,ARRAY(3)
     520 format(/A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',&
          &I3,'); Mortality Multiplier=',F6.2)
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if
  go to 50

  !.... Option number 6: MISTPRT.

  600 continue

  !.... If this keyword is used, then print 3 statistical summary
  !.... tables.  This is separate from MISTABLE.
  !.... Check for DBH threshold for large trees.  Trees this size
  !.... and above will be counted in DMR/DMI statistics.

  PRTMIS = .true.

  if (LNOTBK(1).and.ARRAY(1).ge.0.0) DMRMIN = ARRAY(1)*CMtoIN
  if (LKECHO)write(JOSTND,620) KEYWRD,DMRMIN*INtoCM
  620 format(/A8,'   Print mistletoe summary output; ',&
       &'DMRs and DMIs calculated for trees with DBH >=',F4.1)

  go to 50

  !.... Option number7: MISTGMOD.

  700 continue

  !.... Mistletoe diameter growth modification proportion values.
  !.... ARRAY(1): effective date, ARRAY(2): species number
  !.... Supplemental record: fields 1-6, growth modification proportions
  !.... for DMRs 1-6.

  !.... Check for blank date.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !.... Get species code.

  call SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-999) goto 50

  !.... Check for invalid species code.

  if (ISPL .lt.0.or. ISPL .gt. MAXSP) then
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO (.true.,4)

     !.... Process growth modification proportions from supplemental record

  else
     ARRAY(1) = ARRAY(2)
     read (IREAD,*,end = 60) (ARRAY(I+1),I = 1,6)

     !....    Insert activity into schedule.

     call OPNEW (KODE,IDT,2005,7,ARRAY(1))
     if (KODE .gt.0) goto 50
     if (LKECHO)write (JOSTND,750) KEYWRD,IDT,KARD(2),ISPL
     750 format (/A8,'   Date/Cycle=',I5,'; Species= ',A3,&
          &' (',I3,'):')
     do I = 1,6
        if (LKECHO)write (JOSTND,780) I,ARRAY(I+1)
        780 format(' DMR=',I1,&
             &';  diameter growth modification proportion=',&
             &F10.2)
        790 continue
     end do
  end if
  goto 50

  !.... Option number 8: MISTOFF.

  800 continue

  !.... Check for mistletoe processing ON/OFF option.  If this keyword is
  !.... used, then ignore the dwarf mistletoe in the stand.  The default
  !.... is to run with mistletoe if any exists.

  MISFLG = .false.

  if (LKECHO)write(JOSTND,820) KEYWRD
  820 format(/A8,'   Mistletoe stand infection ignored.')

  go to 50

  !.... Option  number 9: ** OPEN SPOT

  900 continue

  go to 50

  !.... Option number 10: MISTPINF.

  1000 continue

  !.... Check for new (introduced) mistletoe infections option.
  !.... ARRAY(1): effective date, ARRAY(2): species number, ARRAY(3):
  !.... proportion of species to infect, ARRAY(4): infection level
  !.... (DMR 1-6), ARRAY(5): infection method (0-2).

  if (.not.LNOTBK(3)) ARRAY(3) = 0.0
  if (.not.LNOTBK(4)) ARRAY(4) = 1.0
  if (.not.LNOTBK(5)) ARRAY(5) = 0.0

  !.... Check for blank date.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !.... Get species code.

  call SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-999) goto 50

  !.... Check for valid species code.

  if (ISPL.lt.0.or.ISPL.gt.MAXSP) then
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)

     !.... Check for invalid proportion to be infected.

  else if (ARRAY(3).lt.0.or.ARRAY(3).gt.1) then
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8300) ARRAY(3)
     call ERRGRO(.true.,4)

     !.... Check for invalid DMR level of infection.

  else if (ARRAY(4).lt.1.or.ARRAY(4).gt.6) then
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8400) NINT(ARRAY(4))
     call ERRGRO(.true.,4)

     !.... Check for invalid method of infection.

  else if (ARRAY(5).lt.0.or.ARRAY(5).gt.2) then
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8500) NINT(ARRAY(5))
     call ERRGRO(.true.,4)
  else

     !....    Insert activity into schedule.

     call OPNEW(KODE,IDT,2006,4,ARRAY(2))
     if (KODE.gt.0) go to 50
     if (LKECHO)write(JOSTND,1020) KEYWRD,IDT,KARD(2),ISPL,ARRAY(3)
     1020 format(/A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',&
          &I3,'); Proportion to Infect=',F10.2,';')
     if (LKECHO)write(JOSTND,1021) ARRAY(4),ARRAY(5)
     1021 format(8X,'   Infection Level=',F10.2,'; Infection Method=',&
          &F10.2)
  end if
  go to 50

  !.... Option number 11: MISTABLE.

  1100 continue

  !.... Check for mistletoe detail (species/DBH) table output preference.
  !.... ARRAY(1): effective date, ARRAY(2): species number.

  !.... Check for blank date.

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !.... Get species code.

  call SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-999) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Insert activity into schedule.

     call OPNEW(KODE,IDT,2007,1,ARRAY(2))
     if (KODE.gt.0) go to 50
     if (LKECHO)write(JOSTND,1120) KEYWRD,IDT,KARD(2),ISPL
     1120 format(/A8,'   Date/Cycle=',I5,'; Species= ',A3,' (',&
          &I3,')')
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if
  go to 50

  !.... Option 12: NEWSPRED (USING NEW SPREAD AND INTENSIFICATION MODEL).

  1200 continue

  NEWMOD = .true.
  write(JOSTND,1220) KEYWRD
  1220 format(/A8,'   New Spread & Intensification Model in use')
  go to 50

  !.... Option 13: DMCRTHRD

  !     Change the default values for initial distribution of DM in the
  !     crown thirds.

  1300 continue

  write(JOSTND,1301) KEYWRD
  1301 format(/A8,' Default initial crownthird distribution changed')

  do I = 1,5
     read(IREAD,1303) DMT(I,1), DMT(I,2), DMT(I,3)
     1302 continue
  end do
  1303 format (3I10)

  !     Check that crownthird values sum to DMR.

  LCHK(1) = .true.
  do I = 1,5
     J = DMT(I,1) + DMT(I,2) + DMT(I,3)
     if (I .ne. J) then
        write(JOSTND,1311) I
        LCHK(1) = .false.
     end if
     1310 continue
  end do
  1311 format(/'** ERROR: DMR class:',I2,', Sum of thirds .NE. DMR,',&
       &'Change not made.')

  if (LCHK(1)) then
     write(JOSTND,1307)
     do I = 1,5
        write(JOSTND,1309) I, DMT(I,1), DMT(I,2), DMT(I,3)
        DMDMR(I,1) = DMT(I,1)
        DMDMR(I,2) = DMT(I,2)
        DMDMR(I,3) = DMT(I,3)
        1306 continue
     end do
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO(.true.,4)
  end if

  1307 format(T11,'  DMR    Upper    Mid   Lower')
  1309 format(T7,4I8)

  go to 50

  !     Option 14: DMSED
  !     Provide a new seed for the random number routine. If no seed
  !     is provided (or a negative number), the clock is used.

  1400 continue

  if (LNOTBK(1).and.ARRAY(1).eq.0.0) call GETSED (ARRAY(1))
  call DMRNSD (.true., ARRAY(1))
  write (JOSTND,1401) KEYWRD,ARRAY(1)
  1401 format (/A8,'   RANDOM SEED IS:',F14.1)
  go to 50

  !     Option 15: DMOPAQ
  !     Change the relative opacity of foliage, by species.

  1500 continue

  !.... ARRAY(1): species code,  ARRAY(2): relative opacity.
  !.... Set defaults.

  if (.not.LNOTBK(1)) ARRAY(1) = 0.0
  if (.not.LNOTBK(2)) ARRAY(2) = 1.0

  !.... Get species code.

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid opacity.

     if (ARRAY(2).lt.0.0) then
        write(JOSTND,8100) ARRAY(2)
        ARRAY(2) = 1.0
     end if

     write(JOSTND,1501) KEYWRD,KARD(1),ISPL,ARRAY(2)
     1501 format(/A8, 'Relative opacity (LP=1, PP=1) for ',&
          &'foliage of species: ', A3, ' (', I2,') changed to: ', F4.2)
     if (ISPL .gt.0) then
        DMOPAQ(ISPL) = ARRAY(2)
     else
        do J = 1, MAXSP
           DMOPAQ(J) = ARRAY(2)
           1502 continue
        end do
     end if
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 16: DMKTUNE
  !     Alter the critical DMR class that determines if an infected
  !     individual is to be a potential Source.

  1600 continue

  !.... ARRAY(1): species code,  ARRAY(2): threshold DMR.
  !.... Set defaults.

  if (.not.LNOTBK(1)) ARRAY(1) = 0.0
  if (.not.LNOTBK(2)) ARRAY(2) = 0.0

  !.... Get species code.

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid threshold DMR.

     if (ARRAY(2).lt.0.0.or.ARRAY(2).gt.6) then
        write(JOSTND,8150) ARRAY(2)
        ARRAY(2) = 0.0
     end if

     write(JOSTND,1601) KEYWRD,KARD(1),ISPL,NINT(ARRAY(2))
     1601 format(/A8, 'Critical DMR for spreading DM from a ',&
          &'tree of species: ', A3, ' (', I2, ') changed to ', I1)
     if (ISPL .gt.0) then
        DMKTUN(ISPL) = INT(ARRAY(2))
     else
        do J = 1, MAXSP
           DMKTUN(J) = INT(ARRAY(2))
           1602 continue
        end do
     end if
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 17: DMSTUNE
  !     Alter the spread multiplier

  1700 continue

  !.... ARRAY(1): species code,  ARRAY(2): multiplier.
  !.... Set defaults.

  if (.not.LNOTBK(1)) ARRAY(1) = 0.0
  if (.not.LNOTBK(2)) ARRAY(2) = 0.0

  !.... Get species code.

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid multiplier.

     if (ARRAY(2).lt.0.0) then
        write(JOSTND,8150) ARRAY(2)
        ARRAY(2) = 0.0
     end if

     write(JOSTND,1701) KEYWRD,KARD(1),ISPL,ARRAY(2)
     1701 format(/A8, 'Spread scaling factor for DM on ',&
          &'species: ', A3, ' (', I2, ') changed to ', F6.2)
     if (ISPL .gt.0) then
        DMSTUN(ISPL) = ARRAY(2)
     else
        do J = 1, MAXSP
           DMSTUN(J) = ARRAY(2)
           1702 continue
        end do
     end if
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 18: DMITUNE
  !     Alter the intensification multiplier.

  1800 continue

  !.... ARRAY(1): species code,  ARRAY(2): multiplier.
  !.... Set defaults.

  if (.not.LNOTBK(1)) ARRAY(1) = 0.0
  if (.not.LNOTBK(2)) ARRAY(2) = 0.0

  !.... Get species code.

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid multiplier.

     if (ARRAY(2).lt.0.0) then
        write(JOSTND,8150) ARRAY(2)
        ARRAY(2) = 0.0
     end if

     write(JOSTND,1801) KEYWRD,KARD(1),ISPL,ARRAY(2)
     1801 format(/A8, 'Intensification scaling factor for DM ',&
          &'on species: ', A3, ' (', I2, ') changed to ', F6.2)
     if (ISPL .gt.0) then
        DMITUN(ISPL) = ARRAY(2)
     else
        do J = 1, MAXSP
           DMITUN(J) = ARRAY(2)
           1802 continue
        end do
     end if
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 19: DMETUNE
  !     Alter the spread and intensification multipliers simultaneously.

  1900 continue

  !.... ARRAY(1): species code,  ARRAY(2): multiplier.
  !.... Set defaults.

  if (.not.LNOTBK(1)) ARRAY(1) = 0.0
  if (.not.LNOTBK(2)) ARRAY(2) = 0.0

  !.... Get species code.

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid multiplier.

     if (ARRAY(2).lt.0.0) then
        write(JOSTND,8150) ARRAY(2)
        ARRAY(2) = 0.0
     end if

     write(JOSTND,1901) KEYWRD,KARD(1),ISPL,ARRAY(2)
     1901 format(/A8, 'Total establishment scaling factor for DM ',&
          &'on species: ', A3, ' (', I2, ') changed to ', F6.2)
     if (ISPL .gt.0) then
        DMETUN(ISPL) = ARRAY(2)
     else
        do J = 1, MAXSP
           DMETUN(J) = ARRAY(2)
           1902 continue
        end do
     end if
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 20: DMLIGHT

  ! Modify the coefficients describing the latent -> immature forward
  ! and backward transitions.  The default values for the forward
  ! transition low and high (DMLT) are 0 and 1.  The default
  ! back-transition values are 1 and 0.  For the forward case these
  ! values mean that at 0% light, none of the latent will move to
  ! immature; at 100% light all the latent will move to immature. The
  ! back-transition values are similar, but govern the immature ->
  ! latent case.  The user-supplied values are the values of 'y' which
  ! result from the 0.0 and 1.0 values of 'x', where 'y' is the
  ! proportion making the transition and 'x' is the 0-1 light level.

  2000 continue

  ! Assign default values. Paste species-specific coefficients from
  ! species 1 default when 'all species' is indicated by a 0 in ARRAY(1).

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  ! Zero the matrix to hold coefficients for the forward and bacward functions

  do I = 1, 2
     do J = 1, 2
        do K = 1, 4
           DMLRX(I, J, K) = 0.
           2003 continue
        end do
        2002 continue
     end do
     2001 continue
  end do

  ! Check for valid species code. Pick up at most 4 points for
  ! forward and backward reactions.

  LTNM = KEYWRD

  if (ISPL .ge.0.and. ISPL .le. MAXSP) then
     LTNP(1) = 0
     LTNP(2) = 0
     LCHK(1) = .true.
     LCHK(2) = .true.

     do i = 1, 4
        call KEYRDR(IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,ARRAY,IRECNT,&
             &KODE,KARD,LFLAG,LKECHO)

        ! Errors; 0=no error, 1=first column blank (OK here), 2=EOF.

        if (KODE .eq.2) then
           call ERRGRO(.false., 2)
           go to 50
        end if

        ! If -999 is encountered, all values have been read; skip
        ! If all fields are blank all values have been read; skip

        if (INDEX(KEYWRD, '-999') .gt.0) goto 2006
        if (.not. LNOTBK(1) .and.&
             &.not. LNOTBK(2) .and.&
             &.not. LNOTBK(3) .and.&
             &.not. LNOTBK(4)) goto 2006

        ! Read Light (Forward reaction) point. If entry is absent, set LCHK(1).

        if (LCHK(1) .and. LNOTBK(1) .and. LNOTBK(2)) then
           LTNP(1) = LTNP(1) + 1
           DMLRX(1, 1, LTNP(1)) = ARRAY(1)
           DMLRX(1, 2, LTNP(1)) = ARRAY(2)
        else
           LCHK(1) = .false.
        end if

        ! Read Dark (Backward reaction) point. If entry is absent, set LCHK(2).

        if (LCHK(2) .and. LNOTBK(3) .and. LNOTBK(4)) then
           LTNP(2) = LTNP(2) + 1
           DMLRX(2, 1, LTNP(2)) = ARRAY(3)
           DMLRX(2, 2, LTNP(2)) = ARRAY(4)
        else
           LCHK(2) = .false.
        end if

        2005 continue
     end do
     2006 continue

     ! The forward reaction is indexed with 1; the backward reaction with 2.
     ! Possible errors are: (1) not enought points; (2) points not sorted in x;
     ! (3) points have x-values outside 0-1 range. After determining an error type,
     ! print out a message and the values.

     RTYPE(1) = 'FORWARD'
     RTYPE(2) = 'BACKWARD'

     do I = 1, 2

        ERRT(I) = 0
        LCHK(I) = .true.
        if (LTNP(I) .le.1) then
           LCHK(I) = .false.
           ERRT(I) = 1
           goto 2013
        end if

        do J = 2, LTNP(I)
           if (DMLRX(I, 1, J) .le. DMLRX(I, 1, J-1)) then
              LCHK(I) = .false.
              ERRT(I) = 2
              goto 2013
           end if
           2011 continue
        end do

        do J = 2, LTNP(I)
           if ( (DMLRX(I, 1, J) .lt.0.0) .or.&
                &(DMLRX(I, 1, J) .gt.1.0) .or.&
                &(DMLRX(I, 2, J) .lt.0.0) .or.&
                &(DMLRX(I, 2, J) .gt.1.0) ) then
              LCHK(I) = .false.
              ERRT(I) = 3
              goto 2013
           end if
           2012 continue
        end do

        2013 continue

        if (LCHK(I)) then
           write(JOSTND, 2020) LTNM, RTYPE(I), ISPL
           2020 format(/A8,A8, ' reaction function default',&
                &' values for species ',I2,' WILL be changed.')
        else if (.not.LCHK(I) .and. LTNP(I) .gt.0) then
           write(JOSTND,2021) LTNM, RTYPE(I), ISPL, ERRT(I)
           2021 format(/A8,'Error reading ', A8, ' reaction',&
                &' function for species: ',I2,'. Error is of',/,' type ',&
                &I1,'; where 1 means not enough points (<2), 2 means',&
                &' the x-values',/,' are not sorted in ascending order',&
                &' and 3 means the x or y values are',/,' not within',&
                &' the 0-1 range. Default values WILL NOT be changed.')
        end if

        ! Write out values if they were changed or in error.

        if (LTNP(I) .gt.0) then
           write(JOSTND,2025) RTYPE(I), LTNP(I)
           2025 format(/A8, ' reaction points = ',I1,/)
           write(JOSTND,2026)
           2026 format('            X         Y')
           do J = 1, LTNP(I)
              write(JOSTND,2041) J, DMLRX(I,1,J), DMLRX(I,2,J)
              2041 format(I2,':',2(3X,F7.3))
              2040 continue
           end do
        end if

        2010 continue
     end do

     ! Copy user-specified values over defaults if everything is OK.

     do I = 1,2

        if (ISPL .eq.0) then
           J = 1
           K = MAXSP
        else
           J = ISPL
           K = ISPL
        end if

        if (LCHK(I)) then
           do M = J, K
              do N = 1, 2
                 do P = 1, LTNP(I)
                    DMLTRX(M,I,N,P) = DMLRX(I,N,P)
                    2033 continue
                 end do
                 DMLTNP(M,I) = LTNP(I)
                 2032 continue
              end do
              2031 continue
           end do
        end if

        2030 continue
     end do
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8800) ISPL,MAXSP
  end if

  go to 50

  !     Option 21: DMDEATH
  !     Annual death rate of DM plants, regardless of latency or maturity.

  2100 continue

  !.... ARRAY(1): species code,  ARRAY(2): death rate.
  !.... Set defaults.

  if (.not.LNOTBK(1)) ARRAY(1) = 0.0
  if (.not.LNOTBK(2)) ARRAY(2) = 0.08

  !.... Get species code.

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid death rate.

     if (ARRAY(2).lt.0.0.or.ARRAY(2).gt.1.0) then
        write(JOSTND,8600) ARRAY(2)
        ARRAY(2) = 0.08
     end if

     write(JOSTND,2101) KEYWRD,KARD(1),ISPL,ARRAY(2)
     2101 format(/A8, 'Annual mortality for DM on',&
          &'species: ', A3, ' (', I2, ') changed to ', F5.3)
     if (ISPL .gt.0) then
        DMDETH(ISPL) = ARRAY(2)
     else
        do J = 1, MAXSP
           DMDETH(J) = ARRAY(2)
           2102 continue
        end do
     end if
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 22: DMCAP
  !     Cap the total density of infection.

  2200 continue

  !.... ARRAY(1): species code,  ARRAY(2): capping factor.
  !.... Set defaults.

  if (.not.LNOTBK(1)) ARRAY(1) = 0.0
  if (.not.LNOTBK(2)) ARRAY(2) = 0.0

  !.... Get species code.

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid capping factor.

     if (ARRAY(2) .lt.0.0) then
        write(JOSTND,8150) ARRAY(2)
        ARRAY(2) = 0.0
     end if

     write(JOSTND,2201) KEYWRD,KARD(1),ISPL,ARRAY(2)
     2201 format(/A8, 'Capping factor for DM on ',&
          &'species: ', A3, ' (', I2, ') changed to ', F6.2)
     if (ISPL .gt.0) then
        DMCAP(ISPL) = ARRAY(2)
     else
        do J = 1, MAXSP
           DMCAP(J) = ARRAY(2)
           2202 continue
        end do
     end if
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 23: DMCLUMP
  !     Select a spatial clumping term. The user supplies a year in
  !     which the clumping term is to be set, along with a non-zero
  !     value for the variance/mean ratio. By default, the system
  !     determines the variance/mean ratio from the simulated stems/ha
  !     of the sample points.

  2300 continue

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  if (LNOTBK(2) .and. (ARRAY(2) .gt. DMTINY)) then
     call OPNEW(KODE,IDT,2008,1,ARRAY(2))
     if (KODE.gt.0) go to 50
     write(JOSTND, 2301) KEYWRD, IDT, ARRAY(2)
     2301 format(/A8, '   Date/Cycle=', I5,&
          &': Stems/ha variance/mean ratio will be changed to', F7.3)
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO(.true.,4)
  end if
  go to 50

  !     Option 24: DMAUTO (DMALPHA and DMBETA)
  !     Select the decay term for the terms of the double exponential.
  !     If both are missing, an error occurs; if either is missing, a
  !     dummy value is substituted and detected in DMOPTS

  2400 continue

  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  if (LNOTBK(2)) then
     if (ARRAY(2) .gt.0.) ARRAY(2) = -999.
  else
     ARRAY(2) = -999.
  end if

  if (LNOTBK(3)) then
     if (ARRAY(3) .gt.0.) ARRAY(3) = -999.
  else
     ARRAY(3) = -999.
  end if

  call OPNEW(KODE,IDT,2009,2,ARRAY(2))
  if (KODE.gt.0) go to 50

  if (ARRAY(2)  .ne. -999.) then
     write(JOSTND, 2401) KEYWRD, IDT, ARRAY(2)
     2401 format(/A8, '   Date/Cycle=', I5,&
          &': Autocorrelation alpha is: ', F7.3)
  else
     write(JOSTND, 2402) KEYWRD, IDT
     2402 format(/A8, '   Date/Cycle=', I5, ': Autocorrelation alpha',&
          &' value is missing or greater than zero. Current value',&
          &' not changed.')
  end if

  if (ARRAY(3) .ne. -999.) then
     write(JOSTND, 2403) KEYWRD, IDT, ARRAY(3)
     2403 format(/A8, '   Date/Cycle=', I5,&
          &': Autocorrelation beta is: ', F7.3)
  else
     write(JOSTND, 2404) KEYWRD, IDT
     2404 format(/A8, '   Date/Cycle=', I5, ': Autocorrelation beta',&
          &' value is missing or greater than zero. Current value',&
          &' not changed.')
  end if

  go to 50

  !     Option 25: DMTABLE
  !     ENABLE PRINTING OF DM-TREELIST AND LIGHT TABLES TO DATABASE

  2500 continue
  LDETAIL = .true.
  write(JOSTND,2501) KEYWRD
  2501 format(/A8,'  DM-Treelist table printing is on')
  go to 50

  !     Option 26: SAREA
  !     The area of the stand, used for selecting the likelihood of being
  !     selected as a target record.

  2600 continue

  ! >> DEFUNCT <<

  go to 50

  !     Option 27: DMOPQMLT
  !     Change the relative default opacity of all species. Use this to
  !     calibrate the relative values of DMOPAQ().

  2700 continue

  if (LNOTBK(1) .and. (ARRAY(1) .gt.0.0)) then
     X = ARRAY(1)
     write(JOSTND, 2701) KEYWRD, X
     2701 format(/A8, ' Relative P(stopping DM seed) IN 1 M**3 of ',&
          &'avg. PP/LP foliage will be multiplied by: ', F4.2)
     DMOPQM = X
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 28: DMFLOWER
  !     Change the number of years required to become flowering, once
  !     latency is ended and immaturity begins.

  2800 continue

  !.... ARRAY(1): species code,  ARRAY(2): delay in years.
  !.... Set defaults.

  if (.not.LNOTBK(1)) ARRAY(1) = 0.0
  if (.not.LNOTBK(2)) ARRAY(2) = 4.0

  !.... Get species code.

  call SPDECD(1,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) goto 50

  !.... Check for valid species code.

  if (ISPL.ge.0.and.ISPL.le.MAXSP) then

     !....    Check for valid delay.

     if (ARRAY(2).lt.1.0) then
        write(JOSTND,8700) ARRAY(2)
        ARRAY(2) = 4.0
     end if

     write(JOSTND,2801) KEYWRD,KARD(1),ISPL,NINT(ARRAY(2))
     2801 format(/A8, ' Years to flowering for species: ',&
          &A3, ' (', I2, ') changed to ', I3, ' years.')
     if (ISPL .gt.0) then
        DMFLWR(ISPL) = NINT(ARRAY(2))
     else
        do J = 1,MAXSP
           DMFLWR(J) = NINT(ARRAY(2))
        end do
     end if
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write (JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)
  end if

  go to 50

  !     Option 29: DMBCI: Biological control initiation

  2900 continue

  LOK = .true.
  if (.not.LNOTBK(1)) LOK = .false.
  I = IFIX(ARRAY(1))
  if (I .lt.1.or. I .gt. MAXBC) LOK = .false.

  call SPDECD(2,ISPL,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (ISPL.eq.-1) LOK = .false.

  if (LOK) then
     read(IREAD,2901,end = 60) (BC(I)%Mort(K),  K = 1,ACTIVE)
     IRECNT = IRECNT+1
     read(IREAD,2901,end = 60) (BC(I)%Suprs(K), K = 1,ACTIVE)
     IRECNT = IRECNT+1
     read(IREAD,2901,end = 60) (BC(I)%Yr(K),    K = 1,ACTIVE)
     IRECNT = IRECNT+1
     do K = 1,ACTIVE ! bound the values
        BC(I)%Mort(K) = MIN(100.0, MAX(0.0, BC(I)%Mort(K))) ! 0,   100%
        BC(I)%Suprs(K) = MIN(100.0, MAX(0.0, BC(I)%Suprs(K))) ! 0,   100%
        BC(I)%Yr(K) = MIN(999.0, MAX(0.04, BC(I)%Yr(K))) ! 0.04,999yr
        if (BC(I)%Yr(K) .ge.0.04) then
           BC(I)%HfLf(K) = EXP(LOG(0.5)/BC(I)%Yr(K))
        end if
     end do
  end if
  2901 format(4F10.0)

  if (LOK) then
     BC(I)%Spp = ISPL
     write(JOSTND,2902) KEYWRD,I,KARD(2)(1:3),&
          &(BC(I)%Mort(K),  K = 1,ACTIVE),&
          &(BC(I)%Suprs(K), K = 1,ACTIVE),&
          &(BC(I)%Yr(K),    K = 1,ACTIVE)
     2902 format(/A8,&
          &/T8,'Definition of Biocontrol Agent: ',I2,&
          &/T8,'Host species: ',A,' (CODE= ',I2,')',&
          &/T8,'Immediate Mortality (%)',  4F10.1,&
          &/T8,'Suppression (%)        ',  4F10.1,&
          &/T8,'Suppr''n half-life (yr) ', 4F10.1)
  else
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     call ERRGRO(.true.,4)
  end if
  go to 50

  !     Option 30: DMBCA:      Biological control application

  3000 continue
  IDT = 1
  IKD = 2004
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,IKD,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
     end if
  else
     if (.not.LNOTBK(2)) ARRAY(2) = 1.0
     if (.not.LNOTBK(3)) ARRAY(3) = 1.0
     if (.not.LNOTBK(4)) ARRAY(4) = FLOAT(MXHT*MESH)

     ARRAY(2) = FLOAT(IFIX(MIN(FLOAT(MAXBC), MAX(1.0, ARRAY(2)))))
     ARRAY(3) = MIN(20.0,             MAX(0.0, ARRAY(3)))
     ARRAY(4) = MIN(FLOAT(MXHT*MESH), MAX(0.0, ARRAY(4)))

     write (JOSTND,3010) KEYWRD,IDT,IFIX(ARRAY(2)),&
          &ARRAY(3),ARRAY(4)
     3010 format (/A8,'   DATE/CYCLE=',I5,&
          &'; Application of Biocontrol Agent:', I2,&
          &/T13,'Strength: ',F7.2, '; Height (m): ', F7.2)
     call OPNEW (KODE,IDT,IKD,3,ARRAY(2))
     if (KODE.gt.0) then
        call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO(.true.,4)
     end if
  end if
  go to 50

  !.... Error messages.

  8000 format(/' *** Species specified,',I3,', outside range of',&
       &' valid species ( 0 -',I3,' )')
  8100 format(/' *** Value specified,',F10.4,', negative; set to',&
       &' 1.0.')
  8150 format(/' *** Value specified,',F10.4,', negative; set to',&
       &' 0.0.')
  !8200 FORMAT(/' *** DMR specified,',I3,', outside range of',
  !     &       ' valid values ( 0 - 6 )')
  8300 format(/' *** Proportion to be infected, ',F10.4,', outside',&
       &' range of valid values ( 0.0 - 1.0 )')
  8400 format(/' *** DMR infection specified,',I3,', outside range',&
       &' of valid values ( 1 - 6 )')
  8500 format(/' *** Method of infection specified,',I3,', outside',&
       &' range of valid values ( 0 - 2 )')
  8600 format(/' *** Death rate specified,',F6.2,', outside',&
       &' range of valid values ( 0 - 1 ).  Default 0.08 used.')
  8700 format(/' *** Years delay specified,',F6.2,', less than 1.',&
       &'  Default 4.0 used.')
  8800 format(/' *** Backwards coefficients for species: ', A3,&
       &' (', I2, ') - not used.')

  !.... Common return.

  9000 continue

  if (DEBUG)write(JOSTND,9010)ICYC
  9010 format(' End MISIN: Cycle = ',I5)
  go to 9020

  !.... Special entry to retrieve keywords.

  entry MISKEY(KEY,PASKEY)
  PASKEY = TABLE(KEY)

  9020 continue
  return
end
