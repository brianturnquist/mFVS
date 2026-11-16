!================================================!
!   Fire-model keyword handlers shared by FMIN   !
!================================================!
module FMINKEYS_MOD
  implicit none

  ! Shared keyword handlers for the imperial and metric FMIN. Unit-specific
  ! statements stay behind LMETRICVAR, including the bug 24 report flags.
  ! Each takes FMIN's local variables by reference, so values that persist
  ! between keywords are unchanged; RETURN replaces FMIN's GOTO 10.

  private
  public :: FMKSALVSP, FMKMOISTURE, FMKPOTFIRE, FMKSNAGFALL
  public :: FMKSNAGBRK, FMKSNAGDCAY, FMKSNAGOUT, FMKFUELPOOL
  public :: FMKPILEBURN, FMKFUELMULT, FMKDROUGHT, FMKSNAGPSFT
  public :: FMKFUELMODL, FMKFUELFOTO, FMKFMODLIST
  public :: FMKFUELTRET
  public :: FMKFUELREPT
  public :: FMKMORTREPT
  public :: FMKSOILHEAT
  public :: FMKCARBREPT
  public :: FMKCARBCUT
  public :: FMKCANFPROF
  public :: FMKFUELINIT
  public :: FMKSNAGINIT
  public :: FMKPOTFWIND
  public :: FMKPOTFTEMP
  public :: FMKCARBCALC
  public :: FMKFIRECALC
  public :: FMKFUELSOFT
  public :: FMKFUELDCAY
  public :: FMKPOTFMOIS
  public :: FMKDUFFPROD

contains

  subroutine FMKSALVSP(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,PRMS,JSP,NSP,ILEN,LKECHO,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    integer :: MYACT
    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IPRMPT
    character(len=10) :: KARD(12)
    integer :: NVALS
    integer :: KODE
    integer :: IRTNCD
    integer :: NPARMS
    real :: PRMS(13)
    integer :: JSP
    character(len=4) :: NSP(MAXSP,3)
    integer :: ILEN
    logical :: LKECHO
    integer :: IKEXIT

    IKEXIT = 0

    !     SET THE SPECIES TO BE CUT, OR LEFT, IN A SALVAGE OPERATION. ONCE IN
    !     EFFECT IT STAYS IN EFFECT UNTIL RESET.

    if (ICALL .eq.2) then
       write(JOSTND,2504) KEYWRD
       return
    end if

    MYACT = 2501
    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

    if (IPRMPT.gt.0) then
       if (IPRMPT.ne.2) then
          call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          call ERRGRO (.true.,25)
       else
          call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
               &IPRMPT,IRECNT,ICYC)
          call fvsGetRtnCode(IRTNCD)
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    NPARMS = 2
    PRMS(1) = 0.0
    PRMS(2) = 0.0

    JSP = 0
    call SPDECD (2,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (JSP .eq. -999) return
    PRMS(1) = JSP

    if (LNOTBK(3)) PRMS(2) = ARRAY(3)

    if (PRMS(2) .lt.1.0) PRMS(2) = 0.0
    if (PRMS(2) .ge.1.0) PRMS(2) = 1.0

    ILEN = 3
    if (JSP.lt.0)ILEN = ISPGRP(-JSP,92)
    if (PRMS(2).lt.1.0) then
       if (LKECHO)write(JOSTND,110) KEYWRD,IDT,KARD(2)(1:ILEN),JSP
       110 format(/A8,'   DATE/CYCLE ',I4,' SPECIES=',A,' (CODE=',&
            &I3,') IS MARKED FOR CUTTING IN SUBSEQUENT SALVAGE',&
            &' OPERATIONS. ')
    else
       if (LKECHO) write(JOSTND,111) KEYWRD,IDT,KARD(2)(1:ILEN),JSP
       111 format(/A8,'   DATE/CYCLE ',I4,' SPECIES=',A,' (CODE=',&
            &I3,') IS MARKED TO BE LEFT IN SUBSEQUENT SALVAGE',&
            &' OPERATIONS. ')
    end if

    MYACT = 2501
    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
    return

    2504 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
         &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
  end subroutine FMKSALVSP

  subroutine FMKMOISTURE(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,II,PRMS,LKECHO,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    integer :: MYACT
    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IPRMPT
    character(len=10) :: KARD(12)
    integer :: NVALS
    integer :: KODE
    integer :: IRTNCD
    integer :: NPARMS
    integer :: II
    real :: PRMS(13)
    logical :: LKECHO
    integer :: IKEXIT

    IKEXIT = 0

    !     SET THE FUEL MOISTURE CONDITIONS FOR THE DIFFERENT FUEL CATEGORIES
    !     FOR VARIOUS STANDS IN A GIVEN YEAR.

    if (ICALL .eq.2) then
       write(JOSTND,504) KEYWRD
       504 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
       return
    end if

    MYACT = 2505

    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

    if (IPRMPT .gt.0) then
       if (IPRMPT.ne.2) then
          call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          call ERRGRO (.true.,25)
       else
          call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
               &IPRMPT,IRECNT,ICYC)
          call fvsGetRtnCode(IRTNCD)
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    !     READ THE USER-DEFINED VALUES

    NPARMS = 7
    do II = 1,NPARMS
       PRMS(II) = 0.0
       if (LNOTBK(II+1)) PRMS(II) = ARRAY(II+1)

       !     IF LIVE HERB MOISTURE NOT FILLED IN THEN ASSUME IT IS THE SAME AS
       !     THE LIVE WOODY MOISTURE.

       if ((II .eq.7) .and. (.not. LNOTBK(II+1))) then
          PRMS(7) = PRMS(6)
       end if
       505 continue
    end do

    if (LKECHO)write(JOSTND,510) KEYWRD, IDT, PRMS(1), PRMS(2),&
         &PRMS(3), PRMS(4), PRMS(5), PRMS(6), PRMS(7)
    510 format(/A8,'   IN DATE/CYCLE ', I4,' FUEL MOISTURE VALUES (%)',&
         &' WILL BE:',&
         &/T12,'1HR: ',F5.1,' 10HR: ',F5.1,' 100HR: ',F5.1,' 3+: ',&
         &F5.1,' DUFF: ',F5.1,' LIVE WOODY: ',F5.0,&
         &' LIVE HERB: ',F5.0)
    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  end subroutine FMKMOISTURE

  subroutine FMKPOTFIRE(ICALL,KEYWRD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    logical :: LKECHO

    !     GENERATE THE POTENTIAL FIRE (FLAME LENGTH) REPORT.
    !     RETREIVE AN ID NUMBER FOR THE REPORT.

    if (IDPFLM .eq.0) call GETID (IDPFLM)

    if (ICALL .eq.2) then
       write(JOSTND,804) KEYWRD
       804 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
       return
    end if

    IPFLMB = IY(1)
    IPFLME = IY(1) + 999

    if (LKECHO)write(JOSTND,810) KEYWRD
    810 format(/A8,'   THE POTENTIAL FIRE REPORT WILL BE PRINTED.')
  end subroutine FMKPOTFIRE

  subroutine FMKSNAGFALL(JSP,NSP,KEYWRD,ARRAY,KARD,LNOTBK,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'CONTRL.f90'

    integer :: JSP
    character(len=4) :: NSP(MAXSP,3)
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    logical :: LNOTBK(12)
    logical :: LKECHO

    !     SET THE NEW FALL RATE PARAMETERS FOR THE SNAG MODEL.
    !        NOTE THAT MORE THAN ONE SUPPLEMENTAL RECORD IS REQUIRED
    !           IF ENTERING MULTIPLE SPECIES.

    JSP = 0
    call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (JSP .eq. -999) return
    if (JSP .ne.0 ) then

       if (LNOTBK(2)) FALLX(JSP) = ARRAY(2)
       if (FALLX(JSP) .lt.0.001) FALLX(JSP) = 0.001

       if (LNOTBK(3)) ALLDWN(JSP) = ARRAY(3)
       if (ALLDWN(JSP) .lt.0.0) ALLDWN(JSP) = 0.0

    else

       do JSP = 1,MAXSP
          if (LNOTBK(2)) FALLX(JSP) = ARRAY(2)
          if (FALLX(JSP) .lt.0.001) FALLX(JSP) = 0.001
          if (LNOTBK(3)) ALLDWN(JSP) = ARRAY(3)
          if (ALLDWN(JSP) .lt.0.0) ALLDWN(JSP) = 0.0
       end do
       JSP = 1

    end if

    if (LKECHO)write(JOSTND,915) KEYWRD, KARD(1)(1:3), FALLX(JSP),&
         &ALLDWN(JSP)
    915 format(/A8,'   FOR SPECIES ',A,&
         &', THE RATE-OF-FALL CORRECTION MULTIPLIER IS: ',F6.3,&
         &/T12,'THE SNAG AGE BY WHICH THE LAST 5% FALL: ',&
         &F6.1)
  end subroutine FMKSNAGFALL

  subroutine FMKSNAGBRK(JSP,NSP,KEYWRD,ARRAY,KARD,YRS50,YRS30,LNOTBK,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'CONTRL.f90'

    integer :: JSP
    character(len=4) :: NSP(MAXSP,3)
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    real :: YRS50(2)
    real :: YRS30(2)
    logical :: LNOTBK(12)
    logical :: LKECHO

    !     THIS KEYWORD USED TO BE USED TO SET THE "HEIGHT-LOSS CORRECTION FACTOR"
    !     FOR EACH SPECIES, AND (FOR ALL SPECIES) THE PROPORTION OF SNAGS THAT ARE
    !     INITIALLY SOFT, THE MULTIPLIER FOR ADJUSTING THE HEIGHT-LOSS RATE OF
    !     INITIALLY SOFT SNAGS AND THE BASE RATE TO USE FOR ALL SPECIES FOR THE
    !     FIRST AND LAST 50% OF HEIGHT LOSS.

    !     THIS KEYWORD IS NOW CHANGED SO THAT USERS SPECIFY, FOR EVERY SPECIES AND
    !     HARD/SOFT, THE YEARS FOR THE FIRST 50% AND THE NEXT 30% OF HEIGHT TO BE
    !     LOST. THE PROGRAM THEN DETERMINES THE APPROPRIATE "HEIGHT-LOSS CORRECTION
    !     FACTOR" THAT WILL BE USED IN THE EQUATIONS. THE PROPORTION OF SNAGS THAT
    !     ARE INITIALLY SOFT HAS BEEN MOVED TO THE KEYWORD: SNAGPSFT. (FEB 2002)

    !     NOTE: THESE LOG-BASED EQUATIONS WILL STILL HAVE A PROBLEM IF HTX*HTR > 1

    JSP = 0
    call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (JSP .eq. -999) return

    YRS50(1) = INT(ARRAY(2))
    if (YRS50(1) .lt.1) YRS50(1) = 1
    YRS50(2) = INT(ARRAY(3))
    if (YRS50(2) .lt.1) YRS50(2) = 1

    YRS30(1) = INT(ARRAY(4))
    if (YRS30(1) .lt.1) YRS30(1) = 1
    YRS30(2) = INT(ARRAY(5))
    if (YRS30(2) .lt.1) YRS30(2) = 1

    if (JSP .ne.0 ) then
       !        DO THE CALCULATIONS FOR INITIALLY HARD SNAGS
       if (LNOTBK(2)) then
          HTX(JSP,1) = (1. - 0.5**(1./YRS50(1))) / HTR1
       else if (HTX(JSP,1) .ne.0.0) then
          YRS50(1) = LOG(0.5) / LOG(1.-MIN(0.9,HTX(JSP,1)*HTR1))
       else
          YRS50(1) = 999
       end if
       if (LNOTBK(4)) then
          if (YRS30(1) .le. YRS50(1)) YRS30(1) = YRS50(1) + 0.001
          HTX(JSP,2) = (1. - (0.3/0.5)**(1./(YRS30(1)-YRS50(1))))&
               &/ HTR2
       else if (HTX(JSP,2) .ne.0.0) then
          YRS30(1) = YRS50(1) + LOG(0.3/0.5) /&
               &LOG(1.-MIN(0.9,HTX(JSP,2)*HTR2))
       else
          YRS30(1) = 999
       end if

       !        AND INITIALLY SOFT SNAGS
       if (LNOTBK(3)) then
          HTX(JSP,3) = (1. - 0.5**(1./YRS50(2))) / (HTR1*HTXSFT)
       else if (HTX(JSP,3) .ne.0.0.and. HTXSFT .ne.0.0) then
          YRS50(2) = LOG(0.5) /&
               &LOG(1.-MIN(0.9,HTX(JSP,3)*HTR1*HTXSFT))
       else
          YRS50(2) = 999
       end if
       if (LNOTBK(5)) then
          if (YRS30(2) .le. YRS50(2)) YRS30(2) = YRS50(2) + 0.001
          HTX(JSP,4) = (1. - (0.3/0.5)**(1./(YRS30(2)-YRS50(2))))&
               &/ (HTR2*HTXSFT)
       else if (HTX(JSP,4) .ne.0.0.and. HTXSFT .ne.0.0) then
          YRS30(2) = YRS50(2) + LOG(0.3/0.5) /&
               &LOG(1.-MIN(0.9,HTX(JSP,4)*HTR2*HTXSFT))
       else
          YRS30(2) = 999
       end if

    else
       do JSP = 1,MAXSP
          !           DO THE CALCULATIONS FOR INITIALLY HARD SNAGS
          if (LNOTBK(2)) then
             HTX(JSP,1) = (1. - 0.5**(1./YRS50(1))) / HTR1
          else if (HTX(JSP,1) .ne.0.0) then
             YRS50(1) = LOG(0.5) / LOG(1.-MIN(0.9,HTX(JSP,1)*HTR1))
          else
             YRS50(1) = 999
          end if
          if (LNOTBK(4)) then
             if (YRS30(1) .le. YRS50(1)) YRS30(1) = YRS50(1) + 0.001
             HTX(JSP,2) = (1. - (0.3/0.5)**(1./(YRS30(1)-YRS50(1))))&
                  &/ HTR2
          else if (HTX(JSP,2) .ne.0.0) then
             YRS30(1) = YRS50(1) + LOG(0.3/0.5) /&
                  &LOG(1.-MIN(0.9,HTX(JSP,2)*HTR2))
          else
             YRS30(1) = 999
          end if

          !           AND INITIALLY SOFT SNAGS
          if (LNOTBK(3)) then
             HTX(JSP,3) = (1. - 0.5**(1./YRS50(2))) / (HTR1*HTXSFT)
          else if (HTX(JSP,3) .ne.0.0.and. HTXSFT .ne.0.0) then
             YRS50(2) = LOG(0.5) /&
                  &LOG(1.-MIN(0.9,HTX(JSP,3)*HTR1*HTXSFT))
          else
             YRS50(2) = 999
          end if
          if (LNOTBK(5)) then
             if (YRS30(2) .le. YRS50(2)) YRS30(2) = YRS50(2) + 0.001
             HTX(JSP,4) = (1. - (0.3/0.5)**(1./(YRS30(2)-YRS50(2))))&
                  &/ (HTR2*HTXSFT)
          else if (HTX(JSP,4) .ne.0.0.and. HTXSFT .ne.0.0) then
             YRS30(2) = YRS50(2) + LOG(0.3/0.5) /&
                  &LOG(1.-MIN(.9,HTX(JSP,4)*HTR2*HTXSFT))
          else
             YRS30(2) = 999
          end if

       end do
       JSP = 1

    end if

    if (LKECHO)write(JOSTND,1005) KEYWRD, KARD(1)(1:3), INT(YRS50(1)),&
         &INT(YRS50(2)),INT(YRS30(1)), INT(YRS30(2))
    1005 format(/A8,'   FOR SPECIES ',A,&
         &', THE YEARS TO 50% HT LOSS FOR INITIALLY HARD SNAGS IS: ',&
         &I4,/T12,' AND FOR INITIALLY SOFT SNAGS IS: ',I4,/T12,&
         &'YEARS TO THE NEXT 30% HT LOSS FOR INITIALLY HARD SNAGS: ',&
         &I4,/T12,' AND FOR INITIALLY SOFT SNAGS IS: ',I4)
  end subroutine FMKSNAGBRK

  subroutine FMKSNAGDCAY(JSP,NSP,KEYWRD,ARRAY,KARD,LNOTBK,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'CONTRL.f90'

    integer :: JSP
    character(len=4) :: NSP(MAXSP,3)
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    logical :: LNOTBK(12)
    logical :: LKECHO

    !     SET THE NEW DECAY RATE PARAMETERS FOR THE MODEL

    JSP = 0
    call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (JSP .eq. -999) return
    if (JSP .ne.0 ) then

       if (LNOTBK(2)) DECAYX(JSP) = ARRAY(2)
       if (DECAYX(JSP) .lt.0.0) DECAYX(JSP) = 0.0
    else

       do JSP = 1,MAXSP
          if (LNOTBK(2)) DECAYX(JSP) = ARRAY(2)
          if (DECAYX(JSP) .lt.0.0) DECAYX(JSP) = 0.0
       end do
       JSP = 1

    end if

    if (LKECHO)write(JOSTND,1115) KEYWRD, KARD(1)(1:3), DECAYX(JSP)
    1115 format(/A8,'   FOR SPECIES ',A,&
         &', THE RATE-OF-DECAY CORRECTION MULTIPLIER IS:',F5.3)
  end subroutine FMKSNAGDCAY

  subroutine FMKSNAGOUT(ICALL,KEYWRD,IDT,PRMS,LNOTBK,ARRAY,LKECHO,MYACT,NPARMS,KODE)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    integer :: IDT
    real :: PRMS(13)
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    logical :: LKECHO
    integer :: MYACT
    integer :: NPARMS
    integer :: KODE

    !     PRINT THE SNAG OUTPUT TABLE. USERS MAY SELECT ONE/SEVERAL/ALL STANDS
    !     IN ONE/PERIODIC/ALL YEARS

    if (ICALL .eq.2) then
       write(JOSTND,1204) KEYWRD
       1204 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
       return
    end if

    IDT = 1
    PRMS(1) = 200.
    PRMS(2) = -1. ! reporting interval no longer used with cycle boundary FFE
    PRMS(3) = real(JSNOUT)
    PRMS(4) = 0.

    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
    if (LNOTBK(2)) PRMS(1) = INT(ARRAY(2))
    !      IF (LNOTBK(3)) PRMS(2) = INT(ARRAY(3))  !reporting interval no longer used.
    if (LNOTBK(4)) PRMS(3) = INT(ARRAY(4))
    if (LNOTBK(5)) PRMS(4) = INT(ARRAY(5))

    if (LKECHO)write(JOSTND,1215) KEYWRD, IDT, INT(PRMS(1)),&
         &INT(PRMS(3))
    1215 format(/A8,'   THE SNAG LIST WILL BE OUTPUT',&
         &' STARTING IN DATE/CYCLE ',I4,','/T12,'FOR ',I4,&
         &' YEARS.'/&
         &T12,'OUTPUT WILL BE PRINTED TO UNIT ',I3)

    if (PRMS(4).gt.0) then
       if (LKECHO)write(JOSTND,1216) ' NOT '
    else
       if (LKECHO)write(JOSTND,1216) ' '
       1216 format(T12,'HEADINGS WILL',A,'BE PRINTED.')
    end if

    MYACT = 2512
    NPARMS = 4
    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  end subroutine FMKSNAGOUT

  subroutine FMKFUELPOOL(JSP,NSP,KEYWRD,ARRAY,KARD,LNOTBK,IDEC,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'CONTRL.f90'

    integer :: JSP
    character(len=4) :: NSP(MAXSP,3)
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    logical :: LNOTBK(12)
    integer :: IDEC
    logical :: LKECHO

    !     SET THE FUEL DECAY POOL THAT EACH SPECIES BELONGS TO.

    JSP = 0
    call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (JSP .eq. -999.or..not.LNOTBK(2)) return
    IDEC = INT(ARRAY(2))
    if (IDEC .lt.1.or. IDEC .gt.4) return

    if (JSP .ne.0) then
       DKRCLS(JSP) = IDEC
    else
       do JSP = 1,MAXSP
          DKRCLS(JSP) = IDEC
       end do
       JSP = 1
    end if

    if (LKECHO)write(JOSTND,1905) KEYWRD, KARD(1)(1:3), DKRCLS(JSP)
    1905 format(/A8,'   FOR SPECIES ',A,', THE FUEL DECAY CLASS ',&
         &' HAS BEEN CHANGED TO:',I1)
  end subroutine FMKFUELPOOL

  subroutine FMKPILEBURN(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,PRMS,LKECHO,I,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    integer :: MYACT
    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IPRMPT
    character(len=10) :: KARD(12)
    integer :: NVALS
    integer :: KODE
    integer :: IRTNCD
    integer :: NPARMS
    real :: PRMS(13)
    logical :: LKECHO
    integer :: I
    integer :: IKEXIT

    IKEXIT = 0

    !     SET UP JACKPOT AND PILE BURNS.

    if (ICALL .eq.2) then
       write(JOSTND,2304) KEYWRD
       2304 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
       return
    end if

    MYACT = 2523
    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
    if (IPRMPT.gt.0) then
       if (IPRMPT.ne.2) then
          call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          call ERRGRO (.true.,25)
       else
          call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
               &IPRMPT,IRECNT,ICYC)
          call fvsGetRtnCode(IRTNCD)
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    NPARMS = 5
    PRMS(1) = 1.0
    if (LNOTBK(2)) PRMS(1) = ARRAY(2)

    !     Now that we know what type of fuel burning it is, let's set the
    !     defaults for that type.

    if (PRMS(1) .eq.1.0) then
       PRMS(2) = 70.0
       PRMS(3) = 10.0
       PRMS(4) = 80.0
       PRMS(5) = 0.0
    else
       PRMS(2) = 100.0
       PRMS(3) = 30.0
       PRMS(4) = 60.0
       PRMS(5) = 0.0
    end if

    if (LNOTBK(3)) PRMS(2) = ARRAY(3)
    if (LNOTBK(4)) PRMS(3) = ARRAY(4)
    if (LNOTBK(5)) PRMS(4) = ARRAY(5)
    if (LNOTBK(6)) PRMS(5) = ARRAY(6)

    if (PRMS(1) .eq.1.0) then
       if (LKECHO)write(JOSTND,2310) KEYWRD,IDT,'PILE'
       2310 format(/A8,'   IN DATE/CYCLE ',I4,' A ',A,' BURN WILL',&
            &' OCCUR.')
    else
       if (LKECHO)write(JOSTND,2310) KEYWRD,IDT,'JACKPOT'
    end if

    if (LKECHO)write(JOSTND,2312) (PRMS(I),I = 2,5)
    2312 format(T12,&
         &'PERCENT OF THE STAND AREA FROM WHICH FUEL IS COLLECTED ',&
         &'(AFFECTED AREA):',T85,F4.0/T12,&
         &'PERCENT OF THE AFFECTED AREA WHERE THE FUEL IS ',&
         &'CONCENTRATED:',T85,F4.0/T12,&
         &'PERCENT OF THE FUEL FROM THE AFFECTED AREA THAT IS ',&
         &'COLLECTED:',T85,F4.0/T12,'PERCENT OF THE TREES IN THE ',&
         &'STAND WILL DIE AS A RESULT OF THE FIRE:',T85,F4.0)

    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  end subroutine FMKPILEBURN

  subroutine FMKFUELMULT(IDEC,LNOTBK,DKMULT,ARRAY,I,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'CONTRL.f90'

    integer :: IDEC
    logical :: LNOTBK(12)
    real :: DKMULT
    real :: ARRAY(12)
    integer :: I
    logical :: LKECHO
    character(len=8) :: KEYWRD

    !     CHANGE THE TOTAL DECAY RATE OF FUELS USING A MULTIPLIER

    do IDEC = 1,4
       if (LNOTBK(IDEC)) then
          DKMULT = ARRAY(IDEC)
          do I = 1,11
             DKR(I,IDEC) = DKR(I,IDEC) * DKMULT
             if (DKR(I,IDEC) .gt.1.0) DKR(I,IDEC) = 1.0
             if (I .le.10) TODUFF(I,IDEC) = DKR(I,IDEC)*PRDUFF(I,IDEC)
             2920 continue
          end do
       else
          ARRAY(IDEC) = 1.0
       end if
       2910 continue
    end do

    if (LKECHO)write(JOSTND,2950) KEYWRD, (ARRAY(IDEC),IDEC = 1,4)
    2950 format (/A8,'   THE MULTIPLIERS APPLIED TO THE TOTAL DECAY ',&
         &'RATES FOR DECAY RATE CLASS 1-4 ARE ',3(F5.3,",",2X),F5.3)
  end subroutine FMKFUELMULT

  subroutine FMKDROUGHT(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,PRMS,LKECHO,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    integer :: MYACT
    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IPRMPT
    character(len=10) :: KARD(12)
    integer :: NVALS
    integer :: KODE
    integer :: IRTNCD
    integer :: NPARMS
    real :: PRMS(13)
    logical :: LKECHO
    integer :: IKEXIT

    IKEXIT = 0

    !     DEFINE THE YEARS IN WHICH A DROUGHT OR DORMANT YEAR OCCURS

    if (ICALL .eq.2) then
       write(JOSTND,3304) KEYWRD
       3304 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
       return
    end if

    MYACT = 2529
    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

    if (IPRMPT.gt.0) then
       if (IPRMPT.ne.2) then
          call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          call ERRGRO (.true.,25)
       else
          call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
               &IPRMPT,IRECNT,ICYC)
          call fvsGetRtnCode(IRTNCD)
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    NPARMS = 1
    PRMS(1) = 1.0
    if (LNOTBK(2) .and. ARRAY(2) .gt.0.0) PRMS(1) = ARRAY(2)

    if (LKECHO)write(JOSTND,3305) KEYWRD,IDT,INT(PRMS(1))
    3305 format(/A8,'   IN DATE/CYCLE ',I4,' DROUGHT/DORMANCY IS',&
         &' SIMULATED FOR ', I3, ' YEARS.')

    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  end subroutine FMKDROUGHT

  subroutine FMKSNAGPSFT(JSP,NSP,KEYWRD,ARRAY,KARD,LNOTBK,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'CONTRL.f90'

    integer :: JSP
    character(len=4) :: NSP(MAXSP,3)
    character(len=8) :: KEYWRD
    real :: ARRAY(12)
    character(len=10) :: KARD(12)
    logical :: LNOTBK(12)
    logical :: LKECHO

    !     SET THE PROPORTION OF EACH SPECIES THAT IS INITIALLY SOFT
    !        NOTE THE TWO CHANGES: THIS USED TO BE PART OF THE SNAGBRK KEYWORD
    !        AND THIS DID NOT USE TO VARY BY SPECIES.

    JSP = 0
    call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (JSP .eq. -999) return
    if (JSP .ne.0 ) then
       if (LNOTBK(2)) then
          PSOFT(JSP) = ARRAY(2)
          if (PSOFT(JSP) .lt.0.0) PSOFT(JSP) = 0.0
          if (PSOFT(JSP) .gt.1.0) PSOFT(JSP) = 1.0
       end if
    else
       do JSP = 1,MAXSP
          if (LNOTBK(2)) PSOFT(JSP) = ARRAY(2)
          if (PSOFT(JSP) .lt.0.0) PSOFT(JSP) = 0.0
          if (PSOFT(JSP) .gt.1.0) PSOFT(JSP) = 1.0
       end do
       JSP = 1
    end if

    if (LKECHO)write(JOSTND,3705) KEYWRD, KARD(1)(1:3), PSOFT(JSP)
    3705 format(/A8,'   FOR SPECIES ',A,&
         &', THE PROPORTION OF SNAGS THAT ARE SOFT AT ',&
         &'THE TIME THEY ARE CREATED:',F4.2)
  end subroutine FMKSNAGPSFT

  subroutine FMKFUELMODL(MYACT,NPARMS,IDT,LNOTBK,ARRAY,IPRMPT,KEYWRD,KARD,NVALS,&
       &KODE,IRTNCD,I,PRMS,LKECHO,X,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'CONTRL.f90'

    integer :: MYACT
    integer :: NPARMS
    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IPRMPT
    character(len=8) :: KEYWRD
    character(len=10) :: KARD(12)
    integer :: NVALS
    integer :: KODE
    integer :: IRTNCD
    integer :: I
    real :: PRMS(13)
    logical :: LKECHO
    real :: X
    integer :: IKEXIT

    IKEXIT = 0

    !     SET THE FUEL MODELS TO BE USED INSTEAD OF THE MODEL AUTOMATICALLY
    !     DETERMINING THE APPROPRIATE ONES (ADDED FEB 2002)

    MYACT = 2538
    NPARMS = 8
    IDT = 1

    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

    if (IPRMPT.gt.0) then
       if (IPRMPT.ne.2) then
          call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          call ERRGRO (.true.,25)
       else
          call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
               &IPRMPT,IRECNT,ICYC)
          call fvsGetRtnCode(IRTNCD)
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    NPARMS = 0
    do I = 1,6
       PRMS(I) = ARRAY(I+1)
    end do
    read(IREAD,'(2F10.0)',ERR = 3801) PRMS(7),PRMS(8)
    IRECNT = IRECNT + 1

    !     FIND OUT HOW MANY VALID FUEL MODELS YOU HAVE.

    do I = 1,7,2
       if (PRMS(I).gt.0..and. PRMS(I).le.MXDFMD) then
          NPARMS = NPARMS+2
          if (PRMS(I+1).le.0.) PRMS(I+1) = 1.
       else
          exit
       end if
    end do

    if (NPARMS.eq.0) then
       if (LKECHO)write(JOSTND,3814) KEYWRD, IDT
       3814 format(/A8,'   IN DATE/CYCLE ',I4,' AUTOMATIC FUEL ',&
            &'MODEL SELECTION WILL BE USED.',/1X)
    else

       !        SCALE THE WEIGHTS SO THAT THEY ADD UP TO 1

       X = 0.
       do I = 2,NPARMS,2
          X = X+PRMS(I)
       end do
       X = 1./X
       do I = 2,NPARMS,2
          PRMS(I) = PRMS(I)*X
       end do
       if (LKECHO)write(JOSTND,3815) KEYWRD, IDT
       3815 format(/A8,'   IN DATE/CYCLE ',I4,' THE FUEL MODELS AND ',&
            &'WEIGHTS THAT WILL BE USED ARE:')
       do I = 1,NPARMS,2
          if (LKECHO)write(JOSTND,3816) INT(PRMS(I)), 100.0*PRMS(I+1)
          3816 format(T12,'MODEL ',I3,': ',F6.1,'%')
       end do

    end if

    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)

    return

    3801 continue
    call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
    call ERRGRO (.true.,4)
  end subroutine FMKFUELMODL

  subroutine FMKFUELFOTO(ICALL,KEYWRD,NPARMS,PRMS,IDT,LNOTBK,ARRAY,REF,PHOTOREF,J,K,&
       &CHARCODE,LKECHO,MYACT,KODE)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    integer :: NPARMS
    real :: PRMS(13)
    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    character(len=40) :: REF
    character(len=40) :: PHOTOREF(32)
    integer :: J
    integer :: K
    character(len=13) :: CHARCODE
    logical :: LKECHO
    integer :: MYACT
    integer :: KODE

    !     THIS KEYWORD SETS THE INITIAL FUEL VALUES USING FUELS PHOTO
    !     SERIES INFORMATION.

    if (ICALL .eq.2) then
       write(JOSTND,4804) KEYWRD
       4804 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
       return
    end if

    NPARMS = 2
    PRMS(1) = -1.0
    PRMS(2) = -1.0

    IDT = 1
    if (LNOTBK(1)) PRMS(1) = NINT(ARRAY(1))
    if (LNOTBK(2)) PRMS(2) = NINT(ARRAY(2))

    !     THE PHOTO REFERENCE CODE MUST BE BETWEEN 1 AND 32.
    !     4 AND 10 ARE NOT VALID REFERENCE CODES.

    if ((NINT(PRMS(1)) .eq.4) .or. (NINT(PRMS(1)) .eq.10) .or.&
         &(NINT(PRMS(1)) .lt.1) .or. (NINT(PRMS(1)) .gt.32)) then
       PRMS(1) = -1.0
    end if

    if (NINT(PRMS(2)) .lt.1) PRMS(2) = -1.0

    select case (NINT(PRMS(1)))
    case (1)
       if (NINT(PRMS(2)) .gt.22) PRMS(2) = -1.0

    case (2)
       if (NINT(PRMS(2)) .gt.59) PRMS(2) = -1.0

    case (3)
       if (NINT(PRMS(2)) .gt.66) PRMS(2) = -1.0

    case (5)
       if (NINT(PRMS(2)) .gt.17) PRMS(2) = -1.0

    case (6)
       if (NINT(PRMS(2)) .gt.27) PRMS(2) = -1.0

    case (7)
       if (NINT(PRMS(2)) .gt.56) PRMS(2) = -1.0

    case (8)
       if (NINT(PRMS(2)) .gt.86) PRMS(2) = -1.0

    case (9)
       if (NINT(PRMS(2)) .gt.26) PRMS(2) = -1.0

    case (11)
       if (NINT(PRMS(2)) .gt.26) PRMS(2) = -1.0

    case (12)
       if (NINT(PRMS(2)) .gt.90) PRMS(2) = -1.0

    case (13)
       if (NINT(PRMS(2)) .gt.42) PRMS(2) = -1.0

    case (14)
       if (NINT(PRMS(2)) .gt.29) PRMS(2) = -1.0

    case (15)
       if (NINT(PRMS(2)) .gt.29) PRMS(2) = -1.0

    case (16)
       if (NINT(PRMS(2)) .gt.41) PRMS(2) = -1.0

    case (17)
       if (NINT(PRMS(2)) .gt.35) PRMS(2) = -1.0

    case (18)
       if (NINT(PRMS(2)) .gt.43) PRMS(2) = -1.0

    case (19)
       if (NINT(PRMS(2)) .gt.34) PRMS(2) = -1.0

    case (20)
       if (NINT(PRMS(2)) .gt.26) PRMS(2) = -1.0

    case (21)
       if (NINT(PRMS(2)) .gt.25) PRMS(2) = -1.0

    case (22)
       if (NINT(PRMS(2)) .gt.36) PRMS(2) = -1.0

    case (23)
       if (NINT(PRMS(2)) .gt.26) PRMS(2) = -1.0

    case (24)
       if (NINT(PRMS(2)) .gt.27) PRMS(2) = -1.0

    case (25)
       if (NINT(PRMS(2)) .gt.14) PRMS(2) = -1.0

    case (26)
       if (NINT(PRMS(2)) .gt.16) PRMS(2) = -1.0

    case (27)
       if (NINT(PRMS(2)) .gt.30) PRMS(2) = -1.0

    case (28)
       if (NINT(PRMS(2)) .gt.30) PRMS(2) = -1.0

    case (29)
       if (NINT(PRMS(2)) .gt.16) PRMS(2) = -1.0

    case (30)
       if (NINT(PRMS(2)) .gt.16) PRMS(2) = -1.0

    case (31)
       if (NINT(PRMS(2)) .gt.10) PRMS(2) = -1.0

    case (32)
       if (NINT(PRMS(2)) .gt.39) PRMS(2) = -1.0

    end select

    if (NINT(PRMS(1)) .eq. -1) then
       REF = 'UNKNOWN'
    else
       REF = PHOTOREF(NINT(PRMS(1)))
    end if

    J = NINT(PRMS(1))
    K = NINT(PRMS(2))

    if ((J .ne. -1) .and. (K .ne. -1)) then
       call FMPHOTOCODE(J, CHARCODE, K, 0)
    else
       CHARCODE = 'UNKNOWN'
    end if

    if (LKECHO)write(JOSTND,4810) KEYWRD, NINT(PRMS(1)),&
         &REF, NINT(PRMS(2)), CHARCODE
    4810 format(/A8,'   PHOTO SERIES REFERENCE IS ',&
         &I4,' = ',A/T12,'PHOTO CODE IS ',I4,' = ',A)

    MYACT = 2548
    if ((J .ne. -1) .and. (K .ne. -1)) then
       call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
    else
       WRITE (JOSTND,"(/'*** FFE MODEL WARNING: INCORRECT ',       &
            &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ', &
            &      'REQUIRED.  KEYWORD IGNORED.',/1X)")
       call RCDSET (2,.true.)
    end if
  end subroutine FMKFUELFOTO

  subroutine FMKFMODLIST(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,PRMS,NPARMS,LKECHO,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    integer :: MYACT
    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IPRMPT
    character(len=10) :: KARD(12)
    integer :: NVALS
    integer :: KODE
    integer :: IRTNCD
    real :: PRMS(13)
    integer :: NPARMS
    logical :: LKECHO
    integer :: IKEXIT

    IKEXIT = 0

    if (ICALL .eq.2) then
       write(JOSTND,5001) KEYWRD
       5001 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
       return
    end if

    MYACT = 2550

    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

    if (IPRMPT.gt.0) then
       if (IPRMPT.ne.2) then
          call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          call ERRGRO (.true.,25)
       else
          call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
               &IPRMPT,IRECNT,ICYC)
          call fvsGetRtnCode(IRTNCD)
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    !     PRM1: fuel model
    !     PRM2: -1 = not set/default, 0 = on, 1 = off

    PRMS(1) = 1
    PRMS(2) = -1
    NPARMS = 2
    if (LNOTBK(2)) PRMS(1) = MAX(0,INT(ARRAY(2)))
    if (LNOTBK(3)) PRMS(2) = MAX(-1,MIN(1,INT(ARRAY(3))))

    if (LKECHO)write(JOSTND,5010) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2))

    5010 format(/A8,T12,'IN DATE/CYCLE ',I4,' FUEL MODEL ',I3,&
         &' WILL BE:',I2,' (-1=DEFAULT, 0=ON, 1=OFF)')

    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  end subroutine FMKFMODLIST

  subroutine FMKFUELTRET(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,PRMS,LKECHO,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'

    integer :: ICALL
    character(len=8) :: KEYWRD
    integer :: MYACT
    integer :: IDT
    logical :: LNOTBK(12)
    real :: ARRAY(12)
    integer :: IPRMPT
    character(len=10) :: KARD(12)
    integer :: NVALS
    integer :: KODE
    integer :: IRTNCD
    integer :: NPARMS
    real :: PRMS(13)
    logical :: LKECHO
    integer :: IKEXIT

    IKEXIT = 0

    !     SET THE TYPE OF FUEL TREATMENT AND HARVEST TYPE THAT WAS DONE.  NOTE
    !     THAT THIS KEYWORD WILL ONLY AFFECT FIRE INTENSITY AND ONLY WITHIN 5
    !     YEARS AFTER A STAND ENTRY.

    if (ICALL .eq.2) then
       write(JOSTND,2504) KEYWRD
       2504 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
       return
    end if

    MYACT = 2525
    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

    if (IPRMPT.gt.0) then
       if (IPRMPT.ne.2) then
          call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          call ERRGRO (.true.,25)
       else
          call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
               &IPRMPT,IRECNT,ICYC)
          call fvsGetRtnCode(IRTNCD)
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    NPARMS = 3
    PRMS(1) = 0.0
    PRMS(2) = 1.0
    PRMS(3) = -1.0

    if (LNOTBK(2)) PRMS(1) = ARRAY(2)
    if (LNOTBK(3)) PRMS(2) = ARRAY(3)
    if (LNOTBK(4)) PRMS(3) = ARRAY(4)

    if (PRMS(1) .lt.0.0) PRMS(1) = 0.0
    if (PRMS(1) .gt.2.0) PRMS(1) = 2.0
    if (PRMS(2) .lt.1.0) PRMS(2) = 1.0
    if (PRMS(2) .gt.3.0) PRMS(2) = 3.0

    if (LKECHO)write(JOSTND,2510) KEYWRD,IDT, INT(PRMS(1)),&
         &INT(PRMS(2))
    2510 format(/A8,'   IN DATE/CYCLE ',I4,' FUEL TREATMENT TYPE IS ',&
         &I2,' AND HARVEST TYPE ',I2,' WAS USED FOR THE STAND',&
         &' ENTRY.')

    if ((PRMS(3).ge.0.0).and.LKECHO)write(JOSTND,2511) PRMS(3)
    2511 format(T12,'MULTIPLIER FOR FUEL DEPTH WILL BE ',F5.1)

    MYACT = 2525
    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  end subroutine FMKFUELTRET

  subroutine FMKFUELREPT(KEYWRD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LKECHO


    !                        OPTION NUMBER 27 -- FUELREPT

    !     PRODUCE THE FUEL CONSUMPTION AND PHYSICAL EFFECTS REPORT. RETREIVE
    !     THE ID NUMBER FOR THE REPORT.

    if (IDFUL .eq.0) call GETID (IDFUL)

    ! Preserve the original metric sentinel and imperial year window (bug 24).
    if (LMETRICVAR) then
       IFMFLB = -1
       IFMFLE = -1
    else
       IFMFLB = IY(1)
       IFMFLE = IY(1) + 999
    end if

    if (LKECHO)write(JOSTND,2705) KEYWRD

    return

    2705 format(/A8,'   THE FUEL CONSUMPTION AND PHYSICAL EFFECTS',&
         &' REPORT WILL BE WRITTEN WHEN A FIRE OCCURS.')
  end subroutine FMKFUELREPT

  subroutine FMKMORTREPT(KEYWRD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LKECHO


    !                        OPTION NUMBER 28 -- MORTREPT

    !     PRODUCE THE TREE MORTALITY REPORT. RETREIVE THE ID NUMBER
    !     FOR THE REPORT.

    if (IDMRT .eq.0) call GETID (IDMRT)

    ! Preserve the original metric sentinel and imperial year window (bug 24).
    if (LMETRICVAR) then
       IFMMRB = -1
       IFMMRE = -1
    else
       IFMMRB = IY(1)
       IFMMRE = IY(1) + 999
    end if

    if (LKECHO)write(JOSTND,2805) KEYWRD

    return

    2805 format(/A8,'   THE TREE MORTALITY REPORT WILL BE WRITTEN',&
         &' WHEN A FIRE OCCURS.')
  end subroutine FMKMORTREPT

  subroutine FMKSOILHEAT(KEYWRD,LNOTBK,LKECHO,ARRAY)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    real :: ARRAY(12)


    !                        OPTION NUMBER 43 -- SOILHEAT

    if (IDSHEAT .eq.0) call GETID (IDSHEAT)

    ! Preserve the original metric sentinel and imperial year window (bug 24).
    if (LMETRICVAR) then
       ISHEATB = -1
       ISHEATE = -1
    else
       ISHEATB = -IY(1) ! WHEN NEGATIVE, A HEADING IS NEEDED.
       ISHEATE = IY(1) + 999
    end if
    SOILTP = 3
    if (LNOTBK(3)) SOILTP = INT(ARRAY(3))
    if (SOILTP .lt.1) SOILTP = 1
    if (SOILTP .gt.5) SOILTP = 5

    if (LKECHO)write(JOSTND,4315) KEYWRD, SOILTP

    return

    4315 format(/A8,'   SOIL HEATING WILL BE ESTIMATED AND',&
         &' REPORTED WHEN A FIRE OCCURS.'&
         &/T12,'SOIL TYPE IS SET TO ',I4)
  end subroutine FMKSOILHEAT

  subroutine FMKCARBREPT(KEYWRD,LKECHO,ICALL)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LKECHO
    integer :: ICALL

    !                        OPTION NUMBER 44 -- CARBREPT

    if (ICALL .eq.2) then
       write(JOSTND,4401) KEYWRD
       return
    end if

    if (IDCRPT .eq.0) call GETID (IDCRPT)

    ! Preserve the original metric sentinel and imperial year window (bug 24).
    if (LMETRICVAR) then
       ICRPTB = -1
       ICRPTE = -1
    else
       ICRPTB = IY(1)
       ICRPTE = IY(1) + 999
    end if

    if (LKECHO)write(JOSTND,4410) KEYWRD

    return

       4401 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
    4410 format(/A8,'   THE MAIN CARBON REPORT WILL BE PRINTED.')
  end subroutine FMKCARBREPT

  subroutine FMKCARBCUT(KEYWRD,LKECHO,ICALL)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LKECHO
    integer :: ICALL


    !                        OPTION NUMBER 45 -- CARBCUT

    if (ICALL .eq.2) then
       write(JOSTND,4501) KEYWRD
       return
    end if

    if (IDCHRV .eq.0) call GETID (IDCHRV)

    ! Preserve the original metric sentinel and imperial year window (bug 24).
    if (LMETRICVAR) then
       ICHRVB = -1
       ICHRVE = -1
    else
       ICHRVB = IY(1)
       ICHRVE = IY(1) + 999
    end if

    if (LKECHO)write(JOSTND,4510) KEYWRD

    return

       4501 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
    4510 format(/A8,T12,'THE HARVESTED PRODUCTS REPORT WILL BE PRINTED.')
  end subroutine FMKCARBCUT

  subroutine FMKCANFPROF(KEYWRD,LKECHO,ICALL,ICANPR)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LKECHO
    integer :: ICALL
    integer :: ICANPR

    !                        OPTION NUMBER 47 -- CANFPROF
    if (ICALL .eq.2) then
       write(JOSTND,4701) KEYWRD
       return
    end if

    ICANPR = 1
    call DBSFMLINK(ICANPR)

    ! Preserve the original metric sentinel and imperial year window (bug 24).
    if (LMETRICVAR) then
       ICFPB = -1
       ICFPE = -1
    else
       ICFPB = IY(1)
       ICFPE = IY(1) + 999
    end if

    if (LKECHO)write(JOSTND,4710) KEYWRD

    return

       4701 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
    4710 format(/A8,'   CANOPY FUELS PROFILE TABLE SENT ',&
         &'TO SPECIFIED DATABASE.')
  end subroutine FMKCANFPROF

  subroutine FMKFUELINIT(KEYWRD,LNOTBK,LKECHO,NPARMS,IDT,ICALL,PRMS,ARRAY,KODE,MYACT,I)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'
    include 'METRIC.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    integer :: NPARMS
    integer :: IDT
    integer :: ICALL
    real :: PRMS(13)
    real :: ARRAY(12)
    integer :: KODE
    integer :: MYACT
    integer :: I

    !                        OPTION NUMBER 21 -- FUELINIT

    !     THIS KEYWORD SETS THE INITIAL HARD/SOUND FUEL VALUES, PLUS LITTER AND DUFF
    !     IF THEY ARE TO BE DIFFERENT THAN THE DEFAULT ONES.

    if (ICALL .eq.2) then
       write(JOSTND,2104) KEYWRD
       return
    end if

    NPARMS = 12
    PRMS(1) = -1.0
    PRMS(2) = -1.0
    PRMS(3) = -1.0
    PRMS(4) = -1.0
    PRMS(5) = -1.0
    PRMS(6) = -1.0
    PRMS(7) = -1.0
    PRMS(8) = -1.0
    PRMS(9) = -1.0
    PRMS(10) = -1.0
    PRMS(11) = -1.0
    PRMS(12) = -1.0

    IDT = 1
    if (LNOTBK(1)) PRMS(1) = ARRAY(1)
    if (LNOTBK(2)) PRMS(2) = ARRAY(2)
    if (LNOTBK(3)) PRMS(3) = ARRAY(3)
    if (LNOTBK(4)) PRMS(4) = ARRAY(4)
    if (LNOTBK(5)) PRMS(5) = ARRAY(5)
    if (LNOTBK(6)) PRMS(6) = ARRAY(6)
    if (LNOTBK(7)) PRMS(7) = ARRAY(7)
    if (LNOTBK(8)) PRMS(8) = ARRAY(8)
    if (LNOTBK(9)) PRMS(9) = ARRAY(9)
    if (LNOTBK(10)) PRMS(10) = ARRAY(10)
    if (LNOTBK(11)) PRMS(11) = ARRAY(11)
    if (LNOTBK(12)) PRMS(12) = ARRAY(12)
    if (LMETRICVAR) then
       if (LKECHO)write(JOSTND,12110) KEYWRD,(PRMS(I),I = 1,12)

       do I = 1,NPARMS
          if (PRMS(I) .gt.0.0) PRMS(I) = PRMS(I) * TMtoTI / HAtoACR
          2115 continue
       end do
    else
       if (LKECHO)write(JOSTND,2110) KEYWRD,(PRMS(I),I = 1,12)
    end if

    MYACT = 2521
    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
    return

       2104 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
    2110 format(/A8,'   INITIAL HARD FUEL VALUES (TONS/ACRE) ARE',&
         &' (-1.0=NO VALUE SPECIFIED): FUELS <1"=',F5.1,&
         &'; FUELS 1-3"=',F5.1/T12,'FUELS 3-6"=',F5.1,&
         &'; FUELS 6-12"=',F5.1,'; FUELS 12-20"=',F5.1,'; LITTER=',&
         &F5.1,'; DUFF=',F5.1/T12,'FUELS <.25"=',F5.1,&
         &'; FUELS .25-1"=',F5.1/T12,'FUELS 20-35"=',F5.1,&
         &'; FUELS 35-50"=',F5.1,'; FUELS >50"=',F5.1)
    12110 format(/A8,'   INITIAL HARD FUEL VALUES (TONNES/HA) ARE',&
         &' (-1.0=NO VALUE SPECIFIED): FUELS <2.5cm=',F5.1,&
         &'; FUELS 2.5-7.6cm=',F5.1/T12,'FUELS 7.6-15.2cm=',F5.1,&
         &'; FUELS 15.2-30.5cm=',F5.1,'; FUELS 30.5-50.8cm=',F5.1,&
         &'; LITT=',F5.1,'; DUFF=',F5.1/T12,'FUELS <0.6cm=',F5.1,&
         &'; FUELS 0.6-2.5cm=',F5.1/T12,'FUELS 50.8-89.0cm=',F5.1,&
         &'; FUELS 89.0-127.1cm=',F5.1,'; FUELS >127.1cm=',F5.1)
  end subroutine FMKFUELINIT

  subroutine FMKSNAGINIT(NSP,KEYWRD,KARD,LNOTBK,LKECHO,NPARMS,IDT,ICALL,PRMS,ARRAY,KODE,&
       &MYACT,JSP,I)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'
    include 'METRIC.f90'

    character(len=4) :: NSP(MAXSP,3)
    character(len=8) :: KEYWRD
    character(len=10) :: KARD(12)
    logical :: LNOTBK(12)
    logical :: LKECHO
    integer :: NPARMS
    integer :: IDT
    integer :: ICALL
    real :: PRMS(13)
    real :: ARRAY(12)
    integer :: KODE
    integer :: MYACT
    integer :: JSP
    integer :: I

    !                        OPTION NUMBER 22 -- SNAGINIT

    !     THIS KEYWORD ALLOWS USERS TO ADD ADDITIONAL SNAGS TO ONE OR MORE STANDS

    if (ICALL .eq.2) then
       write(JOSTND,2204) KEYWRD
       return
    end if

    IDT = 1
    NPARMS = 6
    PRMS(1) = -1.0
    PRMS(2) = -1.0
    PRMS(3) = -1.0
    PRMS(4) = -1.0
    PRMS(5) = -1.0
    PRMS(6) = -1.0

    JSP = 0
    call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
         &ARRAY,KARD)
    if (JSP.eq.0.or. JSP.eq.-999) return
    PRMS(1) = JSP
    if (LNOTBK(2)) PRMS(2) = ARRAY(2)
    if (LNOTBK(3)) PRMS(3) = ARRAY(3)
    if (LNOTBK(4)) PRMS(4) = ARRAY(4)
    if (LNOTBK(5)) PRMS(5) = ARRAY(5)
    if (LNOTBK(6)) PRMS(6) = ARRAY(6)
    if (LMETRICVAR) then
       if (LKECHO)write(JOSTND,12210) KEYWRD,KARD(1)(1:3),(PRMS(I),I = 2,6)

       if (PRMS(2) .gt.0.0) PRMS(2) = PRMS(2) * CMtoIN
       if (PRMS(3) .gt.0.0) PRMS(3) = PRMS(3) * MtoFT
       if (PRMS(4) .gt.0.0) PRMS(4) = PRMS(4) * MtoFT
       if (PRMS(6) .gt.0.0) PRMS(6) = PRMS(6) / HAtoACR
    else
       if (LKECHO)write(JOSTND,2210) KEYWRD,KARD(1)(1:3),(PRMS(I),I = 2,6)
    end if

    MYACT = 2522
    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
    return

       2204 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
    2210 format(/A8,'   INITIAL SNAG CHARACTERISTICS ',&
         &'(-1.0=NO VALUE SPECIFIED): SPECIES: ',A,&
         &'; DBH AT DEATH (IN): ',F5.1,&
         &/T12,'HEIGHT AT DEATH (FT): ',F5.1,'; CURRENT HEIGHT: ',&
         &F5.1,'; AGE: ',F4.0,'; DENSITY (STEMS/ACRE): ',F6.1)
    12210 format(/A8,'   INITIAL SNAG CHARACTERISTICS ',&
         &'(-1.0=NO VALUE SPECIFIED): SPECIES: ',A,&
         &'; DBH AT DEATH (CM): ',F5.1,&
         &/T12,'HEIGHT AT DEATH (M): ',F5.1,'; CURRENT HEIGHT: ',&
         &F5.1,'; AGE: ',F4.0,'; DENSITY (STEMS/HA): ',F6.1)
  end subroutine FMKSNAGINIT

  subroutine FMKPOTFWIND(KEYWRD,LNOTBK,LKECHO,ARRAY)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'
    include 'METRIC.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    real :: ARRAY(12)

    if (LMETRICVAR) then
       !                        OPTION NUMBER 35 -- POTFWIND

       if (LNOTBK(1)) PREWND(1) = ARRAY(1) * KMtoMI
       if (LNOTBK(2)) PREWND(2) = ARRAY(2) * KMtoMI

       if (LKECHO)write(JOSTND,13550) KEYWRD, PREWND * MItoKM
    else
       !                        OPTION NUMBER 35 -- POTFWIND

       if (LNOTBK(1)) PREWND(1) = ARRAY(1)
       if (LNOTBK(2)) PREWND(2) = ARRAY(2)

       if (LKECHO)write(JOSTND,3550) KEYWRD, PREWND
    end if

    return

    3550 format (/A8,'   FIRE WIND SPEEDS USED FOR CALCULATING ',&
         &'POTENTIAL FLAME LENGTHS ARE',/T11,' FOR SEVERE FIRE: ',F5.0,&
         &' AND FOR MODERATE FIRE: ',F5.0,' MPH')
    13550 format (/A8,'   FIRE WIND SPEEDS USED FOR CALCULATING ',&
         &'POTENTIAL FLAME LENGTHS ARE',/T11,' FOR SEVERE FIRE: ',F5.0,&
         &' AND FOR MODERATE FIRE: ',F5.0,' KM/H')
  end subroutine FMKPOTFWIND

  subroutine FMKPOTFTEMP(KEYWRD,LNOTBK,LKECHO,ARRAY)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'
    include 'METRIC.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    real :: ARRAY(12)

    if (LMETRICVAR) then
       !                        OPTION NUMBER 36 -- POTFTEMP

       if (LNOTBK(1)) POTEMP(1) = ARRAY(1) * CtoF1 + CtoF2
       if (LNOTBK(2)) POTEMP(2) = ARRAY(2) * CtoF1 + CtoF2

       if (LKECHO)write(JOSTND,13650) KEYWRD, POTEMP * FtoC1 + FtoC2
    else
       !                        OPTION NUMBER 36 -- POTFTEMP

       if (LNOTBK(1)) POTEMP(1) = ARRAY(1)
       if (LNOTBK(2)) POTEMP(2) = ARRAY(2)

       if (LKECHO)write(JOSTND,3650) KEYWRD, POTEMP
    end if

    return

    3650 format (/A8,'   FIRE TEMPERATURES USED FOR CALCULATING ',&
         &'POTENTIAL FLAME LENGTHS ARE',/T11,' FOR SEVERE FIRE: ',F5.0,&
         &' AND FOR MODERATE FIRE: ',F5.0,' DEGREES F')
    13650 format (/A8,'   FIRE TEMPERATURES USED FOR CALCULATING ',&
         &'POTENTIAL FLAME LENGTHS ARE',/T11,' FOR SEVERE FIRE: ',F5.0,&
         &' AND FOR MODERATE FIRE: ',F5.0,' DEGREES C')
  end subroutine FMKPOTFTEMP

  subroutine FMKCARBCALC(KEYWRD,LNOTBK,LKECHO,ICALL,ARRAY)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'
    include 'METRIC.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    integer :: ICALL
    real :: ARRAY(12)


    !                        OPTION NUMBER 46 -- CARBCALC

    if (ICALL .eq.2) then
       write(JOSTND,4601) KEYWRD
       return
    end if

    !     DEFAULTS (*) SET IN FMINIT:
    !     FLD1: CARBON METHOD 0 = FFE (*), 1 = JENKINS
    !     FLD2: UNITS TYPE 0 = IMPERIAL (*), 1 = METRIC, 2 = METRIC TONS/AC
    !     FLD3: ROOT DECAY RATE = 0.0425 (*)
    !     FLD4: SOFTWOOD DIAMETER BREAKPOINT =  9.0 IN (*)
    !     FLD5: HARDWOOD DIAMETER BREAKPOINT = 11.0 IN (*)

    if (LNOTBK(1)) ICMETH = MAX(0,MIN(1,INT(ARRAY(1))))
    if (LNOTBK(2)) ICMETRC = MAX(0,MIN(2,INT(ARRAY(2))))
    if (LNOTBK(3)) CRDCAY = MAX(0.0,MIN(1.0,ARRAY(3)))
    if (LMETRICVAR) then
       if (LNOTBK(4)) CDBRK(1) = MAX(0.0,MIN(999.0,ARRAY(4)))*CMtoIN
       if (LNOTBK(5)) CDBRK(2) = MAX(0.0,MIN(999.0,ARRAY(5)))*CMtoIN

       if (LKECHO)write(JOSTND,14610) KEYWRD,ICMETH,ICMETRC,&
            &CRDCAY,CDBRK(1)*INtoCM,CDBRK(2)*INtoCM
    else
       if (LNOTBK(4)) CDBRK(1) = MAX(0.0,MIN(999.0,ARRAY(4)))
       if (LNOTBK(5)) CDBRK(2) = MAX(0.0,MIN(999.0,ARRAY(5)))

       if (LKECHO)write(JOSTND,4610) KEYWRD,ICMETH,ICMETRC,&
            &CRDCAY,CDBRK(1),CDBRK(2)
    end if

    return

       4601 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
    4610 format(/A8,T12,'CARBON REPORTS WILL BE BASED ON METHOD',&
         &I2, ' (0=FFE, 1=JENKINS)',/T12, 'REPORT UNITS WILL BE',&
         &I2, ' (0=US(TONS/ACRE), 1=METRIC(METRIC TONS/HA)',&
         &' 2=COMBINED(METRIC TONS/ACRE))',/T12,&
         &'PROPORTION OF DEAD ROOTS DECAYING ANNUALLY WILL BE: ',&
         &F7.4,' (<0 = NO DEAD ROOTS)',/T12,&
         &'SOFTWOOD DIAMETER BREAKPOINT: ', F5.1,/T12,&
         &'HARDWOOD DIAMETER BREAKPOINT: ', F5.1)
    14610 format(/A8,T12,'CARBON REPORTS WILL BE BASED ON METHOD',&
         &I2, ' (0=FFE, 1=JENKINS)',/T12, 'REPORT UNITS WILL BE',&
         &I2, ' (0=US(TONS/ACRE), 1=METRIC(METRIC TONS/HA)',&
         &' 2= METRIC TONS/ACRE))',/T12,&
         &'PROPORTION OF DEAD ROOTS DECAYING ANNUALLY WILL BE: ',&
         &F7.4,' (<0 = NO DEAD ROOTS)',/T12,&
         &'SOFTWOOD DIAMETER BREAKPOINT: ', F5.1,/T12,&
         &'HARDWOOD DIAMETER BREAKPOINT: ', F5.1)
  end subroutine FMKCARBCALC

  subroutine FMKFIRECALC(KEYWRD,KARD,LNOTBK,LKECHO,NPARMS,IDT,ICALL,NVALS,IRTNCD,PRMS,&
       &ARRAY,KODE,IPRMPT,MYACT,IKEXIT)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'
    include 'METRIC.f90'

    character(len=8) :: KEYWRD
    character(len=10) :: KARD(12)
    logical :: LNOTBK(12)
    logical :: LKECHO
    integer :: NPARMS
    integer :: IDT
    integer :: ICALL
    integer :: NVALS
    integer :: IRTNCD
    real :: PRMS(13)
    real :: ARRAY(12)
    integer :: KODE
    integer :: IPRMPT
    integer :: MYACT
    integer :: IKEXIT

    IKEXIT = 0


    !                        OPTION NUMBER 49 -- FIRECALC

    if (ICALL .eq.2) then
       write(JOSTND,4901) KEYWRD
       return
    end if

    MYACT = 2549

    IDT = 1
    if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

    if (IPRMPT.gt.0) then
       if (IPRMPT.ne.2) then
          call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
          call ERRGRO (.true.,25)
       else
          call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
               &IPRMPT,IRECNT,ICYC)
          call fvsGetRtnCode(IRTNCD)
          if (IRTNCD.ne.0) then
             IKEXIT = -1
             return
          end if
       end if
       return
    end if

    !     DEFAULTS (*) SET IN FMINIT:
    !     PRM1: FIRE CALCULATION METHOD: 0 = OLD FM LOGIC (*), 1 = NEW FM LOGIC, 2 = USE MODELLED LOADS
    !     PRM2: FUEL MODEL SET: 0 = 13, 1 = 40, 2 = 53 (*)
    !     PRM3: ONE-HR SAV (1/FT) = 2000 (*)
    !     PRM4: HERB SAV (1/FT) = 1800 (*)
    !     PRM5: LIVE WOODY SAV (1/FT) = 1500 (*)
    !     PRM6: BULK DENSITY - LIVE (LBS/FT3) = 0.10 (*)
    !     PRM7: BULK DENSITY - DEAD (LBS/FT3) = 0.75 (*)
    !     PRM8: DEAD AND LIVE HEAT CONTENT (BTU/LB) = 8000 (*)

    !     PRM 2 IS USED WITH THE NEW FM LOGIC OPTION
    !     PRMS 3 - 7 ARE USED WITH THE NEW FM LOGIC AND MODELLED LOADS OPTIONS
    !     PRM 8 IS USED WITH THE MODELLED LOAD OPTION ONLY

    NPARMS = 8
    PRMS(1) = IFLOGIC
    PRMS(2) = IFMSET
    PRMS(3) = USAV(1)
    PRMS(4) = USAV(2)
    PRMS(5) = USAV(3)
    PRMS(6) = UBD(1)
    PRMS(7) = UBD(2)
    PRMS(8) = ULHV

    if (LNOTBK(2)) PRMS(1) = MAX(0,MIN(2,INT(ARRAY(2))))
    if (LNOTBK(3)) PRMS(2) = MAX(0,MIN(2,INT(ARRAY(3))))
    if (LMETRICVAR) then
       if (LNOTBK(4)) PRMS(3) = MAX(0.0,ARRAY(4)) / FTtoM
       if (LNOTBK(5)) PRMS(4) = MAX(0.0,ARRAY(5)) / FTtoM
       if (LNOTBK(6)) PRMS(5) = MAX(0.0,ARRAY(6)) / FTtoM
       if (LNOTBK(7)) PRMS(6) = MAX(0.0,ARRAY(7)) * KGtoLB/M3toFT3
       if (LNOTBK(8)) PRMS(7) = MAX(0.0,ARRAY(8)) * KGtoLB/M3toFT3
       if (LNOTBK(9)) PRMS(8) = MAX(0.0,ARRAY(9)) * KJtoBTU/KGtoLB

       if (LKECHO)write(JOSTND,14910) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2)),&
            &PRMS(3)/MtoFT, PRMS(4)/MtoFT, PRMS(5)/MtoFT,&
            &PRMS(6)*(LBtoKG/FT3toM3), PRMS(7)*(LBtoKG/FT3toM3),&
            &PRMS(8)/(BTUtoKJ/LBtoKG)
    else
       if (LNOTBK(4)) PRMS(3) = MAX(0.0,ARRAY(4))
       if (LNOTBK(5)) PRMS(4) = MAX(0.0,ARRAY(5))
       if (LNOTBK(6)) PRMS(5) = MAX(0.0,ARRAY(6))
       if (LNOTBK(7)) PRMS(6) = MAX(0.0,ARRAY(7))
       if (LNOTBK(8)) PRMS(7) = MAX(0.0,ARRAY(8))
       if (LNOTBK(9)) PRMS(8) = MAX(0.0,ARRAY(9))

       if (LKECHO)write(JOSTND,4910) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2)),&
            &PRMS(3), PRMS(4), PRMS(5), PRMS(6), PRMS(7), PRMS(8)
    end if

    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)

    return

       4901 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
    4910 format(/A8,T12,'FIRE CALCULATIONS IN DATE/CYCLE ',&
         &I4,' WILL BE:'&
         &/T12, 'BASED ON METHOD',I2,' ',&
         &'(0=OLD FM LOGIC, 1=NEW FM LOGIC, 2=USE MODELLED LOADS DIRECTLY)',&
         &/T12, 'FUEL MODEL SET (IF USING NEW FM LOGIC) WILL BE',&
         &I2, ' (0=13, 1=40, 2=53)',/T12,&
         &'ONE-HOUR SAV (1/FT) WILL BE: ',F6.0,/T12,&
         &'HERB SAV (1/FT) WILL BE: ',F6.0,/T12,&
         &'LIVE WOODY SAV (1/FT) WILL BE: ',F6.0,/T12,&
         &'LIVE FUEL BULK DENSITY (LBS/FT3) WILL BE: ',F4.2,/T12,&
         &'DEAD FUEL BULK DENSITY (LBS/FT3) WILL BE: ',F4.2,/T12,&
         &'HEAT CONTENT (BTU/LB) WILL BE: ',F6.0)
    14910 format(/A8,T12,'FIRE CALCULATIONS IN DATE/CYCLE ',&
         &I4,' WILL BE:'&
         &/T12, 'BASED ON METHOD',I2,' ',&
         &'(0=OLD FM LOGIC, 1=NEW FM LOGIC, 2=USE MODELLED LOADS DIRECTLY)',&
         &/T12, 'FUEL MODEL SET (IF USING NEW FM LOGIC) WILL BE',&
         &I2, ' (0=13, 1=40, 2=53)',/T12,&
         &'ONE-HOUR SAV (1/M) WILL BE: ',F6.0,/T12,&
         &'HERB SAV (1/M) WILL BE: ',F6.0,/T12,&
         &'LIVE WOODY SAV (1/M) WILL BE: ',F6.0,/T12,&
         &'LIVE FUEL BULK DENSITY (KG/M3) WILL BE: ',F6.3,/T12,&
         &'DEAD FUEL BULK DENSITY (KG/M3) WILL BE: ',F6.3,/T12,&
         &'HEAT CONTENT (KJ/KG) WILL BE: ',F6.0)
  end subroutine FMKFIRECALC

  subroutine FMKFUELSOFT(KEYWRD,LNOTBK,LKECHO,NPARMS,IDT,ICALL,PRMS,ARRAY,KODE,MYACT,I)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'
    include 'METRIC.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    integer :: NPARMS
    integer :: IDT
    integer :: ICALL
    real :: PRMS(13)
    real :: ARRAY(12)
    integer :: KODE
    integer :: MYACT
    integer :: I

    !                        OPTION NUMBER 53 -- FUELSOFT

    !     THIS KEYWORD SETS THE INITIAL SOFT/ROTTEN FUEL VALUES, IF THEY ARE TO BE DIFFERENT
    !     THAN THE DEFAULT ONES.

    if (ICALL .eq.2) then
       write(JOSTND,5304) KEYWRD
       return
    end if

    NPARMS = 9
    PRMS(1) = -1.0
    PRMS(2) = -1.0
    PRMS(3) = -1.0
    PRMS(4) = -1.0
    PRMS(5) = -1.0
    PRMS(6) = -1.0
    PRMS(7) = -1.0
    PRMS(8) = -1.0
    PRMS(9) = -1.0

    IDT = 1
    if (LNOTBK(1)) PRMS(1) = ARRAY(1)
    if (LNOTBK(2)) PRMS(2) = ARRAY(2)
    if (LNOTBK(3)) PRMS(3) = ARRAY(3)
    if (LNOTBK(4)) PRMS(4) = ARRAY(4)
    if (LNOTBK(5)) PRMS(5) = ARRAY(5)
    if (LNOTBK(6)) PRMS(6) = ARRAY(6)
    if (LNOTBK(7)) PRMS(7) = ARRAY(7)
    if (LNOTBK(8)) PRMS(8) = ARRAY(8)
    if (LNOTBK(9)) PRMS(9) = ARRAY(9)
    if (LMETRICVAR) then
       if (LKECHO)write(JOSTND,15310) KEYWRD,(PRMS(I),I = 1,9)

       do I = 1,NPARMS
          if (PRMS(I) .gt.0.0) PRMS(I) = PRMS(I) * TMtoTI / HAtoACR
          5315 continue
       end do
    else
       if (LKECHO)write(JOSTND,5310) KEYWRD,(PRMS(I),I = 1,9)
    end if

    MYACT = 2553
    call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
    return

       5304 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
            &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
    5310 format(/A8,'   INITIAL SOFT FUEL VALUES (TONS/ACRE) ARE',&
         &' (-1.0=NO VALUE SPECIFIED): FUELS <.25"=',F5.1/T12,&
         &'FUELS .25-1"=',F5.1,'; FUELS 1-3"=',F5.1,&
         &'; FUELS 3-6"=',F5.1,'; FUELS 6-12"=',F5.1,&
         &'; FUELS 12-20"=',F5.1/T12,'FUELS 20-35"=',F5.1,&
         &'; FUELS 35-50"=',F5.1,'; FUELS >50"=',F5.1)
    15310 format(/A8,'   INITIAL SOFT FUEL VALUES (TONNES/HA) ARE',&
         &' (-1.0=NO VALUE SPECIFIED): FUELS <0.6cm=',F5.1/T12,&
         &'FUELS 0.6-2.5cm=',F5.1,'; FUELS 2.5-7.6cm=',F5.1,&
         &'; FUELS 7.6-15.2cm=',F5.1,'; FUELS 15.2-30.5cm=',F5.1,&
         &'; FUELS 30.5-50.8cm=',F5.1/T12,'FUELS 50.8-89.0cm=',F5.1,&
         &'; FUELS 89.0-127.1cm=',F5.1,'; FUELS >127.1cm=',F5.1)
  end subroutine FMKFUELSOFT

  subroutine FMKFUELDCAY(KEYWRD,LNOTBK,LKECHO,J,ARRAY,IDEC,ID,I)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    integer :: J
    real :: ARRAY(12)
    integer :: IDEC
    integer :: ID
    integer :: I

    !                        OPTION NUMBER 16 -- FUELDCAY

    !     CHANGE THE TOTAL DECAY RATE OF FUELS.  [LANDSCAPE-LEVEL]

    IDEC = 0
    if (LNOTBK(1)) then
       ID = IFIX(ARRAY(1))
       IDEC = ID
       if (ID .gt.4) IDEC = 4
       if (ID .le.0) IDEC = 1

       if (LNOTBK(2)) DKR(10,IDEC) = ARRAY(2)
       if (LNOTBK(3)) DKR(11,IDEC) = ARRAY(3)
       if (LNOTBK(4)) DKR(1,IDEC) = ARRAY(4)
       if (LNOTBK(5)) DKR(2,IDEC) = ARRAY(5)
       if (LNOTBK(6)) DKR(3,IDEC) = ARRAY(6)
       if (LNOTBK(7)) then
          DKR(4,IDEC) = ARRAY(7)
          DKR(5,IDEC) = ARRAY(7)
          DKR(6,IDEC) = ARRAY(7)
          DKR(7,IDEC) = ARRAY(7)
          DKR(8,IDEC) = ARRAY(7)
          DKR(9,IDEC) = ARRAY(7)
       end if

       !        set array SETDECAY so we know if the decay rates have been set by the user
       if (LNOTBK(2)) SETDECAY(10,IDEC) = ARRAY(2)
       if (LNOTBK(3)) SETDECAY(11,IDEC) = ARRAY(3)
       if (LNOTBK(4)) SETDECAY(1,IDEC) = ARRAY(4)
       if (LNOTBK(5)) SETDECAY(2,IDEC) = ARRAY(5)
       if (LNOTBK(6)) SETDECAY(3,IDEC) = ARRAY(6)
       if (LNOTBK(7)) then
          SETDECAY(4,IDEC) = ARRAY(7)
          SETDECAY(5,IDEC) = ARRAY(7)
          SETDECAY(6,IDEC) = ARRAY(7)
          SETDECAY(7,IDEC) = ARRAY(7)
          SETDECAY(8,IDEC) = ARRAY(7)
          SETDECAY(9,IDEC) = ARRAY(7)
       end if
       !        NOW RE-DETERMINE THE DECAY RATE TO DUFF
       if (ID .lt.5) then
          do I = 1,10
             if (DKR(I,IDEC) .gt.1.0) DKR(I,IDEC) = 1.0
             TODUFF(I,IDEC) = DKR(I,IDEC) * PRDUFF(I,IDEC)
             1620 continue
          end do
          if (LMETRICVAR) then
             if (LKECHO)write(JOSTND,11650) KEYWRD,IDEC,DKR(10,IDEC),&
                  &DKR(11,IDEC),(DKR(I,IDEC),I = 1,4)
          else
             if (LKECHO)write(JOSTND,1650) KEYWRD,IDEC,DKR(10,IDEC),&
                  &DKR(11,IDEC),(DKR(I,IDEC),I = 1,4)
          end if
       else
          do IDEC = 1,4
             do J = 1,11
                DKR(J,IDEC) = DKR(J,4)
                SETDECAY(J,IDEC) = MIN(SETDECAY(J,4),1.0)
             end do
             do I = 1,10
                if (DKR(I,IDEC) .gt.1.0) DKR(I,IDEC) = 1.0
                TODUFF(I,IDEC) = DKR(I,IDEC) * PRDUFF(I,IDEC)
                1630 continue
             end do
             1635 continue
          end do
          if (LMETRICVAR) then
             if (LKECHO)write(JOSTND,11655) KEYWRD,DKR(10,4),DKR(11,4),&
                  &(DKR(I,4),I = 1,4)
          else
             if (LKECHO)write(JOSTND,1655) KEYWRD,DKR(10,4),DKR(11,4),&
                  &(DKR(I,4),I = 1,4)
          end if
       end if

    else

       write(JOSTND,1660) KEYWRD
       call ERRGRO (.true.,1)
    end if

    return

       1650 format (/A8,'   THE TOTAL DECAY RATES FOR DECAY CLASS',I2,&
            &' WILL BE',/T12,'LITTER: ',F5.3,' DUFF: ',F5.3,' 0-.25: ',&
            &F5.3,' .25-1: ',F5.3,' 1-3: ',F5.3,' >3: ',F5.3)
       1655 format (/A8,'   THE TOTAL DECAY RATES FOR ALL DECAY CLAS',&
            &'SES WILL BE',/T12,'LITTER: ',F5.3,' DUFF: ',F5.3,' 0-.25: ',&
            &F5.3,' .25-1: ',F5.3,' 1-3: ',F5.3,' >3: ',F5.3)
       1660 format (/A8,'    **** NO DECAY CLASS WAS SPECIFIED. ',&
            &'KEYWORD WILL BE IGNORED!')
       11650 format (/A8,'   THE TOTAL DECAY RATES FOR DECAY CLASS',I2,&
            &' WILL BE',/T12,'LITTER: ',F5.3,' DUFF: ',F5.3,' 0-0.6: ',&
            &F5.3,' 0.6-2.5: ',F5.3,' 2.5-7.6: ',F5.3,' >7.6: ',F5.3)
       11655 format (/A8,'   THE TOTAL DECAY RATES FOR ALL DECAY CLAS',&
            &'SES WILL BE',/T12,'LITTER: ',F5.3,' DUFF: ',F5.3,' 0-0.6: ',&
            &F5.3,' 0.6-2.5: ',F5.3,' 2.5-7.6: ',F5.3,' >7.6: ',F5.3)
  end subroutine FMKFUELDCAY

  subroutine FMKPOTFMOIS(KEYWRD,LNOTBK,LKECHO,ARRAY,I,IFIRE,IARRY)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    real :: ARRAY(12)
    integer :: I
    integer :: IFIRE
    integer :: IARRY

    !                        OPTION NUMBER 30 -- POTFMOIS

    !     CHANGE THE WIND AND MOISTURE CONDITIONS FOR THE POTENTIAL FLAME REPORT

    IFIRE = 0
    if (LNOTBK(1)) IFIRE = INT(ARRAY(1))
    if (IFIRE .gt.2) IFIRE = 2
    if (IFIRE .lt.1) IFIRE = 1
    PRESVL(IFIRE,1) = 1

    !     GET THE DEFAULTS.

    I = 1
    if (IFIRE.eq.2) I = 3
    call FMMOIS(I,MOIS)

    !     LOAD THE VALUES CHANGED BY THE USER
    !     IF THE LIVE HERB MOISTURE IS LEFT BLANK, USE THE LIVE WOODY
    !     MOISTURE.  THIS WAS ASSUMED SO THAT NO ONES RESULTS WOULD CHANGE
    !     (THEY WOULDN'T HAVE TO REDO KEYWORD FILES) WHEN THE NEW MOISTURE
    !     CATEGORY WAS ADDED. SAR

    do IARRY = 2,8
       if (LNOTBK(IARRY)) then
          PRESVL(IFIRE,IARRY) = ARRAY(IARRY)*.01
       else
          if (IARRY.lt.7) then
             PRESVL(IFIRE,IARRY) = MOIS(1,IARRY-1)
          else if (IARRY .eq.7) then
             PRESVL(IFIRE,IARRY) = MOIS(2,1)
          else if (IARRY .eq.8) then
             PRESVL(IFIRE,IARRY) = PRESVL(IFIRE,7)
          end if
       end if
    end do

    if (IFIRE .eq.1) then
       if (LMETRICVAR) then
          if (LKECHO)write(JOSTND,13050) KEYWRD, 'SEVERE',&
               &(PRESVL(IFIRE,IARRY)*100.0,IARRY = 2,8)
       else
          if (LKECHO)write(JOSTND,3050) KEYWRD, 'SEVERE',&
               &(PRESVL(IFIRE,IARRY)*100.0,IARRY = 2,8)
       end if
    else
       if (LMETRICVAR) then
          if (LKECHO)write(JOSTND,13050) KEYWRD, 'MODERATE',&
               &(PRESVL(IFIRE,IARRY)*100.0,IARRY = 2,8)
       else
          if (LKECHO)write(JOSTND,3050) KEYWRD, 'MODERATE',&
               &(PRESVL(IFIRE,IARRY)*100.0,IARRY = 2,8)
       end if
    end if

    return

       3050 format (/A8,'   FIRE MOISTURE CONDITIONS FOR CALCULATING ',&
            &A,' POTENTIAL FLAME LENGTHS ARE: '&
            &/T12,'% MOISTURE FOR 0-.25"= ',F4.0,'; 0.25-1"= ',F4.0,&
            &'; 1-3"= ',F4.0,'; 3+"= ',F4.0,'; DUFF=',F4.0,&
            &'; LIVE WOODY =',F4.0,'; LIVE HERB =',F4.0)
       13050 format (/A8,'   FIRE MOISTURE CONDITIONS FOR CALCULATING ',&
            &A,' POTENTIAL FLAME LENGTHS ARE: '&
            &/T12,'% MOISTURE FOR 0-0.6cm= ',F4.0,'; 0.6-2.5cm= ',F4.0,&
            &'; 2.5-7.6cm= ',F4.0,'; 7.6+cm= ',F4.0,'; DUFF=',F4.0,&
            &'; LIVE WOODY =',F4.0,'; LIVE HERB =',F4.0)
  end subroutine FMKPOTFMOIS

  subroutine FMKDUFFPROD(KEYWRD,LNOTBK,LKECHO,ARRAY,IDEC,ID,I)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    character(len=8) :: KEYWRD
    logical :: LNOTBK(12)
    logical :: LKECHO
    real :: ARRAY(12)
    integer :: IDEC
    integer :: ID
    integer :: I

    !                        OPTION NUMBER 17 -- DUFFPROD

    !     CHANGE THE PROPORTION OF THE DECAY RATE THAT GOES TO DUFF (AS OPPOSED
    !     TO GOING TO THE AIR).  THIS CAN BE DONE FOR THE 3 SMALLEST FUEL
    !     CATEGORIES, EVERYTHING LARGER THAN 3" AND LITTER.

    IDEC = 0
    if (LNOTBK(1)) then
       ID = IFIX(ARRAY(1))
       IDEC = ID
       if (ID .gt.4) IDEC = 4
       if (ID .le.0) IDEC = 1

       if (LNOTBK(7)) then
          do I = 1,10
             PRDUFF(I,IDEC) = ARRAY(7)
             1710 continue
          end do
       end if
       if (LNOTBK(2)) PRDUFF(10,IDEC) = ARRAY(2)
       if (LNOTBK(3)) PRDUFF(1,IDEC) = ARRAY(3)
       if (LNOTBK(4)) PRDUFF(2,IDEC) = ARRAY(4)
       if (LNOTBK(5)) PRDUFF(3,IDEC) = ARRAY(5)
       if (LNOTBK(6)) then
          PRDUFF(4,IDEC) = ARRAY(6)
          PRDUFF(5,IDEC) = ARRAY(6)
          PRDUFF(6,IDEC) = ARRAY(6)
          PRDUFF(7,IDEC) = ARRAY(6)
          PRDUFF(8,IDEC) = ARRAY(6)
          PRDUFF(9,IDEC) = ARRAY(6)
       end if

       !        NOW RE-DETERMINE THE DECAY RATES TO DUFF AND TO AIR
       if (ID .le.4) then
          do I = 1,10
             if (PRDUFF(I,IDEC) .gt.1.0) PRDUFF(I,IDEC) = 1.0
             if (PRDUFF(I,IDEC) .lt.0.0) PRDUFF(I,IDEC) = 0.0
             TODUFF(I,IDEC) = PRDUFF(I,IDEC) * DKR(I,IDEC)
             1720 continue
          end do
          if (LMETRICVAR) then
             if (LKECHO)write(JOSTND,11750) KEYWRD,IDEC,&
                  &PRDUFF(10,IDEC), (PRDUFF(I,IDEC),I = 1,4)
          else
             if (LKECHO)write(JOSTND,1750) KEYWRD,IDEC,&
                  &PRDUFF(10,IDEC), (PRDUFF(I,IDEC),I = 1,4)
          end if
       else
          do I = 1,10
             do IDEC = 1,4
                PRDUFF(I,IDEC) = PRDUFF(I,4)
                if (PRDUFF(I,IDEC) .gt.1.0) PRDUFF(I,IDEC) = 1.0
                if (PRDUFF(I,IDEC) .lt.0.0) PRDUFF(I,IDEC) = 0.0
                TODUFF(I,IDEC) = PRDUFF(I,IDEC) * DKR(I,IDEC)
                1730 continue
             end do
             1735 continue
          end do
          if (LMETRICVAR) then
             if (LKECHO)write(JOSTND,11755) KEYWRD, PRDUFF(10,4),&
                  &(PRDUFF(I,4),I = 1,4)
          else
             if (LKECHO)write(JOSTND,1755) KEYWRD, PRDUFF(10,4),&
                  &(PRDUFF(I,4),I = 1,4)
          end if
       end if
    else
       write(JOSTND,1760) KEYWRD
    end if

    return

          1750 format (/A8,'   THE PROPORTION OF THE DECOMPOSING ',&
               &' MATERIAL WHICH GOES TO DUFF IN DECAY POOL ',I2,' IS:',&
               &/T12,'LITTER: ',F4.2,&
               &' 0-.25: ',F4.2,' .25-1: ',F4.2,' 1-3: ',F4.2,' >3: ',F4.2)
          1755 format (/A8,'   THE PROPORTION OF THE DECOMPOSING ',&
               &' MATERIAL WHICH GOES TO DUFF IS',/T12,'LITTER: ',F4.2,&
               &' 0-.25: ',F4.2,' .25-1: ',F4.2,' 1-3: ',F4.2,' >3: ',F4.2)
       1760 format (/A8,'    ****NO DECAY POOL WAS SPECIFIED. ',&
            &'KEYWORD WILL BE IGNORED!')
          11750 format (/A8,'   THE PROPORTION OF THE DECOMPOSING ',&
               &' MATERIAL WHICH GOES TO DUFF IN DECAY POOL ',I2,' IS:',&
               &/T12,'LITTER: ',F4.2,' 0-0.6: ',F4.2,&
               &' 0.6-2.5: ',F4.2,' 2.5-7.6: ',F4.2,' >7.6: ',F4.2)
          11755 format (/A8,'   THE PROPORTION OF THE DECOMPOSING ',&
               &' MATERIAL WHICH GOES TO DUFF IS',/T12,'LITTER: ',F4.2,&
               &' 0-0.6: ',F4.2,&
               &' 0.6-2.5: ',F4.2,' 2.5-7.6: ',F4.2,' >7.6: ',F4.2)
  end subroutine FMKDUFFPROD

end module FMINKEYS_MOD
