!==================================================!
!   Base-model keyword handlers shared by INITRE   !
!==================================================!
module INITREKEYS_MOD
  implicit none

  ! Keyword handlers that were identical in the US, ON and BC INITRE.
  ! Each takes INITRE's local variables by reference, so values that persist
  ! between keywords are unchanged; RETURN replaces INITRE's GOTO 10, and
  ! IKEXIT reports any other exit INITRE must take.

  private
  public :: INKTIMEINT, INKNUMCYCLE, INKTREELIST, INKDEBUG
  public :: INKADDFILE, INKTHINAUTO, INKNOCALIB, INKNOHTDREG
  public :: INKCHEAPO, INKCOMPRESS, INKCALBSTAT, INKDELOTAB
  public :: INKCUTLIST, INKRESETAGE, INKMANAGED, INKFVSSTAND
  public :: INKPOINTREF, INKECHO, INKCYCLEAT, INKATRTLIST
  public :: INKORGANON

contains

  subroutine INKTIMEINT(I2,LNOTBK,ARRAY,I,KEYWRD,KARD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: I2
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: I
    character(len=8) :: KEYWRD
    character(len=10) :: KARD(12)
    logical :: LKECHO

    I2 = 10
    if (LNOTBK(2)) I2 = IFIX(ARRAY(2))
    if (.not.LNOTBK(1)) go to 1220
    I = IABS(IFIX(ARRAY(1))) + 1
    if (I.eq.1) goto 1220
    if (I.gt.MAXCY1) then
       call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
       call ERRGRO (.true.,4)
       return
    end if
    IY(I) = I2
    I = I-1
    if (LKECHO)write(JOSTND,1215) KEYWRD,I,I2
    1215 format(/A8,3X,'CYCLE=',I3,'; PERIOD LENGTH=',I3)
    return
    1220 continue
    do I = 2,MAXCY1
       IY(I) = I2
       1230 continue
    end do
    if (LKECHO)write(JOSTND,1235) KEYWRD,I2
    1235 format(/A8,3X,'ALL CYCLES;  PERIOD LENGTH=',I3)
  end subroutine INKTIMEINT

  subroutine INKNUMCYCLE(ARRAY,KEYWRD,KARD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    real :: ARRAY(12)
    character(len=8) :: KEYWRD
    character(len=10) :: KARD(12)
    logical :: LKECHO

    if (ARRAY(1).ge.1..and. IFIX(ARRAY(1)).le.MAXCYC) goto 2110
    call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
    call ERRGRO (.true.,4)
    return
    2110 continue
    NCYC = IFIX ( ARRAY(1) )
    if (LKECHO)write(JOSTND,2120) KEYWRD,NCYC
    2120 format (/A8,'   NUMBER OF CYCLES=',I3)
  end subroutine INKNUMCYCLE

  subroutine INKTREELIST(IDT,LNOTBK,ARRAY,NP,KODE,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: NP
    integer :: KODE
    logical :: LKECHO
    character(len=8) :: KEYWRD

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
    if (KODE.gt.0) return
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
  end subroutine INKTREELIST

  subroutine INKDEBUG(LNOTBK,ARRAY,IDBCYC,LKECHO,KEYWRD,RECORD,IRC,DEBUG,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IDBCYC
    logical :: LKECHO
    character(len=8) :: KEYWRD
    character(len=250) :: RECORD
    integer :: IRC
    logical :: DEBUG
    integer :: IKEXIT

    IKEXIT = 0

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
       if (IRC.eq.1) then
          IKEXIT = 80
          return
       end if
    else
       call DBALL (IDBCYC)
    end if

    ! Check whether debugging is enabled
    call DBCHK (DEBUG,'INITRE',6,0)
  end subroutine INKDEBUG

  subroutine INKADDFILE(LNOTBK,KEYWRD,ARRAY,KARD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    logical :: LNOTBK(12)
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    logical :: LKECHO

    if (LNOTBK(1)) goto 3210
    call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
    call ERRGRO (.true.,4)
    return
    3210 continue
    ICL1 = IREAD
    IREAD = IFIX(ARRAY(1))
    if (LKECHO)write(JOSTND,1410) KEYWRD,IREAD
    return

    1410 format (/A8,'   DATA SET REFERENCE NUMBER=',I3)
  end subroutine INKADDFILE

  subroutine INKTHINAUTO(IDT,LNOTBK,ARRAY,IPRMPT,KEYWRD,KARD,KODE,IRTNCD,PRMS,&
       &LKECHO,I,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IPRMPT
    character(len=8) :: KEYWRD
    character(len=10) :: KARD(12)
    integer :: KODE
    integer :: IRTNCD
    real :: PRMS(10)
    logical :: LKECHO
    integer :: I
    integer :: IKEXIT

    IKEXIT = 0

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
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if
    PRMS(1) = AUTMIN
    PRMS(2) = AUTMAX
    PRMS(3) = EFF
    if (LNOTBK(2))PRMS(1) = ARRAY(2)
    if (LNOTBK(3))PRMS(2) = ARRAY(3)
    if (LNOTBK(4))PRMS(3) = ARRAY(4)
    call OPNEW (KODE,IDT,222,3,PRMS)
    if (KODE .gt.0) return
    if (LKECHO)write(JOSTND,3310) KEYWRD,IDT,(PRMS(I),I = 1,3)
    3310 format (/A8,'   DATE/CYCLE=',I5,'; MIN=',F8.1,'; MAX=',&
         &F8.1,' PERCENT OF FULL STOCKING',&
         &';  PROPORTION OF SELECTED TREES REMOVED=',F6.3)
  end subroutine INKTHINAUTO

  subroutine INKNOCALIB(IS,KEYWRD,ARRAY,KARD,IGRP,IULIM,IG,IGSP,ILEN,LKECHO,J)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'
    include 'HTCAL.f90'

    integer :: IS
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    integer :: IGRP
    integer :: IULIM
    integer :: IG
    integer :: IGSP
    integer :: ILEN
    logical :: LKECHO
    integer :: J

    !   SPECIES CODE PROCESSING.

    call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (IS.eq.-999) return

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
  end subroutine INKNOCALIB

  subroutine INKNOHTDREG(IS,KEYWRD,ARRAY,KARD,IGRP,IULIM,IG,IGSP,LNOTBK,ILEN,LKECHO,&
       &J)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'

    integer :: IS
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    integer :: IGRP
    integer :: IULIM
    integer :: IG
    integer :: IGSP
    logical :: LNOTBK(12)
    integer :: ILEN
    logical :: LKECHO
    integer :: J

    !     PROCESS BASED ON SPECIES CODE.

    call SPDECD (1,IS,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (IS.eq.-999) return

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
  end subroutine INKNOHTDREG

  subroutine INKCHEAPO(LNOTBK,ARRAY,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'ECON.f90'

    logical :: LNOTBK(12)
    real :: ARRAY(12)
    logical :: LKECHO
    character(len=8) :: KEYWRD

    if ( LNOTBK(1) ) JOSUME = IFIX(ARRAY(1))
    LECBUG = LNOTBK(6)
    LECON = .true.
    if (LKECHO)write(JOSTND,1410) KEYWRD,JOSUME
    LECON = .true.
    if (LECBUG.and.LKECHO)write(JOSTND,6510) LECBUG
    6510 format (T12,'ECONOMIC SUMMARY DEBUG REQUESTED.  LECBUG=',L2)
    call ECAVAL
    return

    1410 format (/A8,'   DATA SET REFERENCE NUMBER=',I3)
  end subroutine INKCHEAPO

  subroutine INKCOMPRESS(LNOTBK,ARRAY,IDT,KODE,LKECHO,KEYWRD,I)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IDT
    integer :: KODE
    logical :: LKECHO
    character(len=8) :: KEYWRD
    integer :: I

    if (.not.LNOTBK(2)) ARRAY(2) = MAXTRE/2
    if (.not.LNOTBK(3)) ARRAY(3) = 50.
    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
    call OPNEW (KODE,IDT,250,2,ARRAY(2))
    if (KODE.gt.0) return
    if (LKECHO)write(JOSTND,8020) KEYWRD,IDT,(ARRAY(I),I = 2,3)
    8020 format(/A8,'   DATE/CYCLE=',I5,'; TARGET TREE RECORDS=',F5.0,&
         &'; FIND ',F5.0,' PERCENT OF THE NEW RECORDS BY DEFINING'/&
         &T12,'TREE-CLASS BOUNDARIES WHERE THE LARGEST ',&
         &'DIFFERENCES ARE BETWEEN TREES.  DEFINE THE REMAINING'/&
         &T12,'CLASSES BY SPLITTING THOSE WHICH HAVE THE LARGEST ',&
         &'RANGES.')
  end subroutine INKCOMPRESS

  subroutine INKCALBSTAT(LNOTBK,ARRAY,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'ECON.f90'
    include 'CALCOM.f90'

    logical :: LNOTBK(12)
    real :: ARRAY(12)
    logical :: LKECHO
    character(len=8) :: KEYWRD

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
  end subroutine INKCALBSTAT

  subroutine INKDELOTAB(JJTAB,LNOTBK,ARRAY,I,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: JJTAB
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: I
    logical :: LKECHO
    character(len=8) :: KEYWRD

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
       return
    end if

    if (JJTAB .gt.4) then
       if (LKECHO)write(JOSTND,9220) KEYWRD,JJTAB
       9220 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. THIS IS',&
            &' AN INVALID TABLE NUMBER. KEYWORD IGNORED.')
       return
    end if

    ITABLE(JJTAB) = 1
    if ((JJTAB.eq.1).and.LKECHO)write(JOSTND,9226)KEYWRD,JJTAB
    9226 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
         &' STAND COMPOSITION TABLE WILL BE DELETED.')
    if ((JJTAB.eq.2).and.LKECHO)write(JOSTND,9225)KEYWRD,JJTAB
    9225 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
         &' SAMPLE TREE TABLE WILL BE DELETED.')
    if ((JJTAB.eq.3).and.LKECHO)write(JOSTND,9227)KEYWRD,JJTAB
    9227 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
         &' SUMMARY OUTPUT TABLE WILL BE DELETED.')
    if ((JJTAB.eq.4).and.LKECHO)write(JOSTND,9228)KEYWRD,JJTAB
    9228 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
         &' ACTIVITY SCHEDULE TABLE WILL BE DELETED.')
    if ((JJTAB.eq.5).and.LKECHO)write(JOSTND,9229)KEYWRD,JJTAB
    9229 format(/A8,'   TABLE NUMBER',I4,'  SPECIFIED. ',&
         &' THIS IS NOT FUNCTIONAL AT THIS TIME.')
  end subroutine INKDELOTAB

  subroutine INKCUTLIST(IDT,LNOTBK,ARRAY,NP,KODE,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: NP
    integer :: KODE
    logical :: LKECHO
    character(len=8) :: KEYWRD

    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
    NP = 2
    if (LNOTBK(4)) NP = 3
    if (LNOTBK(6)) NP = 5
    if (.not.LNOTBK(2)) ARRAY(2) = FLOAT(JOLIST)
    call OPNEW (KODE,IDT,199,NP,ARRAY(2))
    if (KODE.gt.0) return
    if (LKECHO)write(JOSTND,9410) KEYWRD,IDT,ARRAY(2),ARRAY(3)
    9410 format (/A8,'   DATE/CYCLE=',I5,'; DATA SET REFERENCE ',&
         &'NUMBER =',F4.0,'; HEADING SUPPRESSION CODE =',F3.0/&
         &T12,'(0=WITH HEADING, OTHER VALUES=SUPPRESS HEADING).')
    if ((NP.eq.3).and.(ARRAY(4).eq.2).and.LKECHO)write(JOSTND,9411)
    9411 format(T12,'CYCLE ONE CUTLIST SUPRESSED')
    if ((NP.eq.5).and.(ARRAY(6).gt.0).and.LKECHO)write(JOSTND,9412)
    9412 format(T12,'CUTLIST WILL BE IN OLD FORMAT')
  end subroutine INKCUTLIST

  subroutine INKRESETAGE(IDT,IPRMPT,KEYWRD,ARRAY,KARD,KODE,IRTNCD,LNOTBK,LKECHO,&
       &IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: IDT
    integer :: IPRMPT
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    integer :: KODE
    integer :: IRTNCD
    logical :: LNOTBK(12)
    logical :: LKECHO
    integer :: IKEXIT

    IKEXIT = 0

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
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
    call OPNEW (KODE,IDT,443,1,ARRAY(2))
    if (KODE.gt.0) return
    if (LKECHO)write(JOSTND,9510) KEYWRD,IDT,ARRAY(2)
    9510 format (/A8,'   DATE/CYCLE=',I5,'; NEW AGE=',F6.0)
  end subroutine INKRESETAGE

  subroutine INKMANAGED(IDT,LNOTBK,ARRAY,KODE,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'

    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: KODE
    logical :: LKECHO
    character(len=8) :: KEYWRD

    IDT = 0
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
    if (IDT .gt.0) then
       call OPNEW (KODE,IDT,82,1,ARRAY(2))
       if (KODE.gt.0) return
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
  end subroutine INKMANAGED

  subroutine INKFVSSTAND(IDT,LNOTBK,ARRAY,NP,KODE,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: NP
    integer :: KODE
    logical :: LKECHO
    character(len=8) :: KEYWRD

    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
    NP = 1
    if (LNOTBK(2)) NP = 2
    call OPNEW (KODE,IDT,204,NP,ARRAY(2))
    if (KODE.gt.0) return
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
  end subroutine INKFVSSTAND

  subroutine INKPOINTREF(LNOTBK,ARRAY,KEYWRD,KARD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    logical :: LNOTBK(12)
    real :: ARRAY(12)
    character(len=8) :: KEYWRD
    character(len=10) :: KARD(12)
    logical :: LKECHO

    if (LNOTBK(1)) ITHNPI = IFIX(ARRAY(1)+0.5)
    if (ITHNPI.lt.0.or. ITHNPI.gt.2) then
       call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
       call ERRGRO (.true.,4)
       write(JOSTND,13105)
       13105 format(T12,'INVALID POINT INDICATOR. POINT INDICATOR BEING ',&
            &'RESET TO 1 (POINT NUMBERS FROM DATA) FOR FURTHER PROCESSING.')
       ITHNPI = 1
       return
    end if
    if (ITHNPI.eq.1.) then
       if (LKECHO)write(JOSTND,13110)KEYWRD
       13110 format(/A8,'   POINT NUMBERS ARE AS INPUT IN THE TREE DATA.')
    else
       if (LKECHO)write(JOSTND,13111)KEYWRD
       13111 format(/A8,'   POINT NUMBERS ARE THE FVS SEQUENTIAL POINT ',&
            &'NUMBERS.')
    end if
  end subroutine INKPOINTREF

  subroutine INKECHO(LKECHO,JRECNT,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    logical :: LKECHO
    integer :: JRECNT
    character(len=8) :: KEYWRD

    LKECHO = .true.
    if (IRECNT.gt.JRECNT) then
       JRECNT = IRECNT-JRECNT-1
    else
       JRECNT = IRECNT
    end if
    write(JOSTND,13210)KEYWRD,JRECNT
    13210 format(/A8,'   THERE WERE',I8,' KEYWORD RECORDS PROCESSED.',&
         &' KEYWORD ECHO WILL RESUME.')
  end subroutine INKECHO

  subroutine INKCYCLEAT(NEWYR,ARRAY,KEYWRD,KARD,I,J,K,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'WORKCM.f90'

    integer :: NEWYR
    real :: ARRAY(12)
    character(len=8) :: KEYWRD
    character(len=10) :: KARD(12)
    integer :: I
    integer :: J
    integer :: K
    logical :: LKECHO

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
  end subroutine INKCYCLEAT

  subroutine INKATRTLIST(IDT,LNOTBK,ARRAY,NP,KODE,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: NP
    integer :: KODE
    logical :: LKECHO
    character(len=8) :: KEYWRD

    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
    NP = 2
    if (LNOTBK(4)) NP = 3
    if (.not.LNOTBK(2)) ARRAY(2) = FLOAT(JOLIST)
    call OPNEW (KODE,IDT,198,NP,ARRAY(2))
    if (KODE.gt.0) return
    if (LKECHO)write(JOSTND,13510) KEYWRD,IDT,ARRAY(2),ARRAY(3)
    13510 format (/A8,'   DATE/CYCLE=',I5,'; DATA SET REFERENCE ',&
         &'NUMBER =',F4.0,'; HEADING SUPPRESSION CODE =',F3.0/&
         &T12,'(0=WITH HEADING, OTHER VALUES=SUPPRESS HEADING).')
  end subroutine INKATRTLIST

  subroutine INKORGANON(LKECHO,KEYWRD,DEBUG,LNOTRE)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    logical :: LKECHO
    character(len=8) :: KEYWRD
    logical :: DEBUG
    logical :: LNOTRE

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
  end subroutine INKORGANON

end module INITREKEYS_MOD
