!====================!
!   mistoe : misin   !
!====================!
! MISTOE $Id$
subroutine MISIN(PASKEY,ARRAY,LNOTBK,LKECHO)

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
  !     NUMBR:  Keyword number passed back from FNDKEY.
  !     TABLE:  Character array of possible keywords.

  !  Common block variables and parameters:

  !     DMRMIN: From MISCOM; minimum tree DBH for DMR/DMI statistics.
  !     ICYC:   From CONTRL; cycle index number.
  !     IREAD:  From CONTRL; logical unit number for keyword input.
  !     IRECNT: From CONTRL; number of keyword parameter records read.
  !     ITYPE:  From PLOT;   input stand habitat type code.
  !     JOSTND: From CONTRL; logical unit number for stand output.
  !     MAXCYC: From PRGPRM; maximum # cycles.
  !     MAXSP:  From PRGPRM; maximum # species.
  !     MISFLG: From MISCOM; logical flag for turning mistletoe on/off.
  !     NSP:    From PLOT;   valid species-tree class codes.
  !     PRTMIS: From MISCOM; log. flag for turning DM printing on/off.

  !  Revision History :
  !     02/28/95 - Lance R. David (MAG)
  !                Changed MISTGMOD keyword to use supplemental to provide
  !                growth modification proportions for 1 species DMRs 1-6.
  !                Field 2 of keyword record specifies species being
  !                modified.
  !     02/26/98 - Lance R. David (FHTET)
  !                Default value for MISTPRT field 1 (DMRMIN) set to 1.0
  !     12/05/01 - Lance R. David (MAG)
  !                Initialization of DMRMIN moved to MISIN0.f.
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

  implicit none

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MISCOM.f90'

  !.... Variable declarations.
  ! Variables
  integer :: ISIZE
  parameter (ISIZE = 11)

  logical :: LNOTBK(12),DEBUG,LKECHO
  real    :: ARRAY(12)
  integer :: I,IDT,ISPL,KEY,KODE,NUMBR,IRTNCD
  character(len=8) :: TABLE(ISIZE)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=10) :: KARD(12)

  !.... Data statements.

  data TABLE/&
       &'MISTMULT','MISTHMOD','END     ','MISTPREF','MISTMORT',&
       &'MISTPRT ','MISTGMOD','MISTOFF ','        ','MISTPINF',&
       &'MISTABLE'/

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

  !.... Special EOF target.

  60 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  !.....Process the keyword.

  70 continue
  go to(100,200,300,400,500,600,700,800,900,1000,1100),NUMBR

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

  !.... Option number 5: MISTMORT.

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

  if (LNOTBK(1).and.ARRAY(1).ge.0.0) DMRMIN = ARRAY(1)
  if (LKECHO)write(JOSTND,620) KEYWRD,DMRMIN
  620 format(/A8,'   Print mistletoe summary output; ',&
       &'DMRs and DMIs calculated for trees with DBH >=',F4.1)
  go to 50

  !.... Option number 7: MISTGMOD.

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

  if (ISPL.lt.0.or.ISPL.gt.MAXSP) then
     call KEYDMP(JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
     write(JOSTND,8000) ISPL,MAXSP
     call ERRGRO(.true.,4)

     !.... Process growth modification proportions from supplemental record

  else
     ARRAY(1) = ARRAY(2)
     read(IREAD,*,end = 60) (ARRAY(I+1),I = 1,6)

     !....    Insert activity into schedule.

     call OPNEW(KODE,IDT,2005,7,ARRAY(1))
     if (KODE.gt.0) go to 50
     if (LKECHO)write(JOSTND,750)KEYWRD,IDT,KARD(2),ISPL
     750 format(/A8,'   Date/Cycle=',I5,'; Species= ',A3,&
          &' (',I3,'):')
     do I = 1,6
        if (LKECHO)write(JOSTND,780) I,ARRAY(I+1)
        780 format(' DMR=',I1,&
             &';  diameter growth modification proportion=',&
             &F10.2)
        790 continue
     end do
  end if
  go to 50

  !.... Option number 8: MISTOFF.

  800 continue

  !.... Check for mistletoe processing ON/OFF option.  If this keyword is
  !.... used, then ignore the dwarf mistletoe in the stand.  The default
  !.... is to run with mistletoe if any exists.

  MISFLG = .false.

  if (LKECHO)write(JOSTND,820) KEYWRD
  820 format(/A8,'   Mistletoe stand infection ignored.')

  go to 50

  !.... Option number 9: ** OPEN SPOT

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

  !.... Error messages.

  8000 format(/' *** Species specified,',I3,', outside range of',&
       &' valid species ( 0 -',I3,' )')
  8100 format(/' *** Value specified,',F10.4,', negative; set to',&
       &' 1.0.')
  8300 format(/' *** Proportion to be infected, ',F10.4,', outside',&
       &' range of valid values ( 0.0 - 1.0 )')
  8400 format(/' *** DMR infection specified,',I3,', outside range',&
       &' of valid values ( 1 - 6 )')
  8500 format(/' *** Method of infection specified,',I3,', outside',&
       &' range of valid values ( 0 - 2 )')

  !.... Common return.

  9000 continue

  if (DEBUG)write(JOSTND,9010)ICYC
  9010 format('End MISIN: Cycle = ',I5)
  go to 9020

  !.... Special entry to retrieve keywords.

  entry MISKEY(KEY,PASKEY)
  PASKEY = TABLE(KEY)

  9020 continue
  return
end
