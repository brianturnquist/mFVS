!================================================!
!   Fire-model keyword handlers shared by FMIN   !
!================================================!
module FMINKEYS_MOD
  implicit none

  ! Keyword handlers that were identical in the imperial and metric FMIN.
  ! Each takes FMIN's local variables by reference, so values that persist
  ! between keywords are unchanged; RETURN replaces FMIN's GOTO 10.

  private
  public :: FMKSALVSP, FMKMOISTURE, FMKPOTFIRE, FMKSNAGFALL
  public :: FMKSNAGBRK, FMKSNAGDCAY, FMKSNAGOUT, FMKFUELPOOL
  public :: FMKPILEBURN, FMKFUELMULT, FMKDROUGHT, FMKSNAGPSFT
  public :: FMKFUELMODL, FMKFUELFOTO, FMKFMODLIST

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

end module FMINKEYS_MOD
