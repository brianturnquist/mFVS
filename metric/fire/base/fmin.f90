!=============================!
!   metric/fire/base : fmin   !
!=============================!
subroutine FMIN (ICALL,NSP,LKECHO)
  implicit none

  ! METRIC-FIRE-BASE

  !     FIRE - FIRE & SNAG MODEL

  !     OPTION PROCESSOR FOR FIRE MODEL

  !     CALLED FROM: INITRE [SINGLE-STAND VERSION]

  !     PARAMETER DEFINITIONS
  !     ICALL = 1 call is within a stand (called from INITRE)
  !             2 call is from outside (will always be 1 with deletion of PPE)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: KWCNT
  parameter (KWCNT = 56)

  character(len=4) :: NSP(MAXSP,3)
  character(len=8) :: TABLE(KWCNT)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=10) :: KARD(12)
  character(len=10) :: APRMS(13)
  character(len=40) :: PHOTOREF(32)
  character(len=40) :: REF
  character(len=13) :: CHARCODE
  logical :: LNOTBK(12),LOK,LKECHO
  integer :: NPARMS,IDT,ICALL,IFMD,NVALS,ILEN,ICANPR,J,K
  integer :: IRTNCD
  real    :: YRS50(2),YRS30(2)
  real    :: PRMS(13)
  real    :: ARRAY(12)

  data TABLE /&
       &'SALVSP  ','END     ','SVIMAGES','BURNREPT','MOISTURE',&
       &'SIMFIRE ','FLAMEADJ','POTFIRE ','SNAGFALL','SNAGBRK ',& !10
       &'SNAGDCAY','SNAGOUT ','SNAGCLAS','LANDOUT ','FUELOUT ',&
       &'FUELDCAY','DUFFPROD','MOREOUT ','FUELPOOL','SALVAGE ',& !20
       &'FUELINIT','SNAGINIT','PILEBURN','SNAGPBN ','FUELTRET',&
       &'STATFUEL','FUELREPT','MORTREPT','FUELMULT','POTFMOIS',& !30
       &'SNAGSUM', 'MORTCLAS','DROUGHT ','FUELMOVE','POTFWIND',&
       &'POTFTEMP','SNAGPSFT','FUELMODL','DEFULMOD','CANCALC ',& !40
       &'POTFSEAS','POTFPAB ','SOILHEAT','CARBREPT','CARBCUT ',&
       &'CARBCALC','CANFPROF','FUELFOTO','FIRECALC','FMODLIST',& !50
       &'DWDVLOUT','DWDCVOUT','FUELSOFT','FMORTMLT','USECFIM ',&
       &'CFIMCALC'/
  data PHOTOREF / 'Fischer INT-96                      ',&
       &'Fischer INT-97                      ',&
       &'Fischer INT-98                      ',&
       &'                                    ',&
       &'Koski and Fischer INT-46            ',&
       &'Maxwell and Ward PNW-52             ',&
       &'Blonski and Schramel PSW-56         ',&
       &'Maxwell and Ward PNW-105            ',&
       &'Ottmar and Hardy PNW-GTR-231        ',&
       &'                                    ',&
       &'Maxwell A-89-6-82                   ',&
       &'Southwestern region compilation     ',&
       &'Maxwell and Ward PNW-51             ',&
       &'Ottmar and others Volume I          ',&
       &'Ottmar and others Volume I          ',&
       &'Ottmar and Vihnanek Volume II / IIa ',&
       &'Ottmar and others Volume III        ',&
       &'Ottmar and others Volume V / Va     ',&
       &'Ottmar and others Volume VI / VIa   ',&
       &'Maxwell A-89-1-90                   ',&
       &'Ottmar and others Volume IV         ',&
       &'Wright and others PNW-GTR-545       ',&
       &'Ottmar and others PNW-GTR-258       ',&
       &'Lynch and Horton NA-FR-25           ',&
       &'Wilcox and others NA-FR-22          ',&
       &'Scholl and Waldrop GTR-SRS-26       ',&
       &'Ottmar and others Volume VII        ',&
       &'Maxwell and Ward PNW-95             ',&
       &'Sanders and Van Lear GTR-SE-49      ',&
       &'Wade and others GTR-SE-82           ',&
       &'Blank GTR-NC-77                     ',&
       &'Popp and Lundquist RMRS-GTR-172     ' /

  integer :: KODE,IPRMPT,NUMBER,MYACT,II,JSP,IHEAD,ICHNG,ICLS,IDEC,ID,I,IFIRE,IARRY,KEY
  real    :: DKMULT,X,XSUM

  !     **********          EXECUTION BEGINS          **********

  10 continue

  !     THIS WAS INCREASED TO 9, AND THEN 12, BECAUSE WE NEEDED TO ADD FIELDS TO
  !     THE POTFMOIS, MOISTURE, AND FUELINIT KEYWORDS

  NVALS = 12
  call FMKEYRDR (IREAD,JOSTND,.false.,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,NVALS)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  !  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
  !               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.

  if (KODE.lt.0) then
     IPRMPT = -KODE
  else
     IPRMPT = 0
  end if
  if (KODE .le.0) go to 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  call ERRGRO (.true.,6)
  goto 10
  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.false.,JOSTND)

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

  if (KODE .eq.0) goto 90
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if

  90 continue

  !     SIGNAL THAT THE FIRE MODEL IS NOW ACTIVE.

  LFMON = .true.

  !     PROCESS OPTIONS

  go to( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,&
       &1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,&
       &2100,2200,2300,2400,2500,2600,2700,2800,2900,3000,&
       &3100,3200,3300,3400,3500,3600,3700,3800,3900,4000,&
       &4100,4200,4300,4400,4500,4600,4700,4800,4900,5000,&
       &5100,5200,5300,5400,5500,5600), NUMBER

  100 continue
  !                        OPTION NUMBER 1 -- SALVSP

  !     SET THE SPECIES TO BE CUT, OR LEFT, IN A SALVAGE OPERATION. ONCE IN
  !     EFFECT IT STAYS IN EFFECT UNTIL RESET.

  if (ICALL .eq.2) then
     write(JOSTND,2504) KEYWRD
     goto 10
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  NPARMS = 2
  PRMS(1) = 0.0
  PRMS(2) = 0.0

  JSP = 0
  call SPDECD (2,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (JSP .eq. -999) goto 10
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
     if (LKECHO)write(JOSTND,111) KEYWRD,IDT,KARD(2)(1:ILEN),JSP
     111 format(/A8,'   DATE/CYCLE ',I4,' SPECIES=',A,' (CODE=',&
          &I3,') IS MARKED TO BE LEFT IN SUBSEQUENT SALVAGE',&
          &' OPERATIONS. ')
  end if

  MYACT = 2501
  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10

  200 continue
  !                        OPTION NUMBER 2 -- END
  if (LKECHO)write(JOSTND,210) KEYWRD
  210 format (/A8,'   END OF FIRE MODEL OPTIONS.')
  return

  300 continue
  !                        OPTION NUMBER 3 -- SVIMAGES
  if (LNOTBK(1)) NFMSVPX = IFIX(ARRAY(1))
  if (LKECHO)write(JOSTND,310) KEYWRD, NFMSVPX
  310 format(/A8,'   VISUALIZATION IMAGES PER FIRE= ',I4)
  goto 10
  400 continue
  !                        OPTION NUMBER 4 -- BURNREPT

  if (IDBRN .eq.0) call GETID (IDBRN)

  IFMBRB = IY(1)
  IFMBRE = IY(1) + 999

  if (LKECHO)write(JOSTND,415) KEYWRD
  415 format(/A8,'   THE BURN CONDITIONS REPORT WILL BE WRITTEN',&
       &' WHEN A FIRE OCCURS.')

  goto 10
  500 continue

  !                        OPTION NUMBER 5 -- MOISTURE

  !     SET THE FUEL MOISTURE CONDITIONS FOR THE DIFFERENT FUEL CATEGORIES
  !     FOR VARIOUS STANDS IN A GIVEN YEAR.

  if (ICALL .eq.2) then
     write(JOSTND,504) KEYWRD
     504 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
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
  goto 10
  600 continue
  !                        OPTION NUMBER 6 -- SIMFIRE (formerly FIRECOND)

  !     SET THE CONDITIONS UNDER WHICH THE FIRE WILL OCCUR: WIND, TEMPERATURE
  !     AND FUEL MOISTURES.  SET THE MORTALITY CODE (0 = TURN OFF FFE MORTALITY,
  !     1 = FFE ESTIMATES MORTALITY).  SET THE % OF THE STAND BURNED.
  !     SET THE SEASON OF THE BURN.
  !     1 = early spring (compact leaves), 2 = before greenup, 3 = after greenup, 4 = fall

  if (ICALL .eq.2) then
     write(JOSTND,604) KEYWRD
     604 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  MYACT = 2506

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
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  NPARMS = 6
  PRMS(1) = 20.0
  PRMS(2) = 1.0
  PRMS(3) = 70.0
  PRMS(4) = 1.0
  PRMS(5) = 100.0
  PRMS(6) = 1

  if (LNOTBK(2)) PRMS(1) = ARRAY(2) * KMtoMI
  if (LNOTBK(3)) PRMS(2) = ARRAY(3)
  if (LNOTBK(4)) PRMS(3) = ARRAY(4) * CtoF1 + CtoF2
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)
  if (PRMS(4) .lt.0) PRMS(4) = 0
  if (PRMS(4) .gt.1) PRMS(4) = 1
  if (PRMS(5) .lt.0) PRMS(5) = 0
  if (PRMS(5) .gt.100) PRMS(5) = 100
  if (PRMS(6) .lt.1) PRMS(6) = 1
  if (PRMS(6) .gt.4) PRMS(6) = 4

  if (LKECHO)write(JOSTND,610) KEYWRD, IDT, PRMS(1) * MItoKM,&
       &PRMS(2),PRMS(3)*FtoC1+FtoC2,PRMS(4),PRMS(5),PRMS(6)

  610 format(/A8,T12,'FIRE CONDITIONS IN DATE/CYCLE ',&
       &I4,' WILL BE: WIND: ',F5.1,' KM/HR'&
       &/T12,'FUEL MOISTURE VALUES WILL USE THE ',&
       &'PRESET MOISTURE CONDITION ',F3.0&
       &/T12,'TEMPERATURE: ',F5.0,' DEGREES C.'&
       &/T12,'MORTALITY CODE: ',F2.0,' (0 = TURN OFF FFE MORTALITY',&
       &', 1 = FFE ESTIMATES MORTALITY)'&
       &/T12,'PERCENTAGE OF THE STAND BURNED: ',F6.1&
       &/T12,'SEASON OF THE BURN: ',F2.0,' (1 = EARLY SPRING ',&
       &'(COMPACT LEAVES), 2 = BEFORE GREENUP, 3 = AFTER GREENUP,',&
       &' 4 = FALL)')

  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10

  700 continue
  !                        OPTION NUMBER 7 -- FLAMEADJ (formerly FIRETYPE)

  !     SELECT THE TYPE OF FIRE WHICH WILL OCCUR (INCLUDING FLAME LENGTH OR FLAME
  !     LENGTH MULTIPLIER, AND PERCENT OF TREES EXPERIENCING CROWNING).

  if (ICALL .eq.2) then
     write(JOSTND,704) KEYWRD
     704 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  MYACT = 2507
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  NPARMS = 3
  PRMS(1) = 1.0
  PRMS(2) = -1.0
  PRMS(3) = -1.0
  PRMS(4) = -1.0

  if (LNOTBK(2)) PRMS(1) = ARRAY(2)
  if (LNOTBK(3)) PRMS(2) = ARRAY(3) * MtoFT
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  if (LNOTBK(5)) then
     PRMS(4) = ARRAY(5) * MtoFT
     NPARMS = 4
  end if

  !     SET BOUNDS FOR SOME OF THE VALUES

  if (PRMS(1) .lt.0.0)   PRMS(1) = 1.0
  if (PRMS(2) .lt.0.0)   PRMS(2) = -1.0
  if (PRMS(3) .lt.0.0)   PRMS(3) = -1.0
  if (PRMS(3) .gt.100.0) PRMS(3) = 100.

  if (PRMS(2) .gt.0.0) then
     if (LKECHO)write(JOSTND,717) KEYWRD, IDT, PRMS(2) * FTtoM
     717 format (/A8,T12,'IN DATE/CYCLE ', I4,&
          &' FLAME LENGTH WILL BE: ',F6.1,' M.')
  else
     if (LKECHO)write(JOSTND,719) KEYWRD, IDT, PRMS(1)
     719 format (/A8,T12,'IN DATE/CYCLE ', I4,&
          &' FLAME LENGTH WILL BE CALCULATED BASED ON APPLICABLE',&
          &' CONDITIONS,',/T12,'AND THEN MULTIPLIED BY ',F7.3)
  end if
  if (PRMS(3).eq.-1.) then
     if (LKECHO)write(JOSTND,721)
     721 format (T12,'THE MODEL PREDICTS THE % CROWNING.')
  else
     if (LKECHO)write(JOSTND,722) PRMS(3)
     722 format (T12,F5.1,' % OF THE CROWN WILL UNDERGO CROWNING.')
  end if
  if (NPARMS.ge.4) then
     if (LKECHO)write(JOSTND,723) PRMS(4) * FTtoM
     723 format (T12,'SCORCH HEIGHT =',F10.2,' M.')
  end if

  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10
  800 continue
  !                        OPTION NUMBER 8 -- POTFIRE (formerly POTFLAME)

  !     GENERATE THE POTENTIAL FIRE (FLAME LENGTH) REPORT.
  !     RETREIVE AN ID NUMBER FOR THE REPORT.

  if (IDPFLM .eq.0) call GETID (IDPFLM)

  if (ICALL .eq.2) then
     write(JOSTND,804) KEYWRD
     804 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  IPFLMB = IY(1)
  IPFLME = IY(1) + 999

  if (LKECHO)write(JOSTND,810) KEYWRD
  810 format(/A8,'   THE POTENTIAL FIRE REPORT WILL BE PRINTED.')

  goto 10

  900 continue
  !                        OPTION NUMBER 9 -- SNAGFALL

  !     SET THE NEW FALL RATE PARAMETERS FOR THE SNAG MODEL.
  !        NOTE THAT MORE THAN ONE SUPPLEMENTAL RECORD IS REQUIRED
  !           IF ENTERING MULTIPLE SPECIES.

  JSP = 0
  call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (JSP .eq. -999) goto 10
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

  goto 10
  1000 continue
  !                        OPTION NUMBER 10 -- SNAGBRK

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
  if (JSP .eq. -999) goto 10

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

  goto 10

  1100 continue
  !                        OPTION NUMBER 11 -- SNAGDCAY

  !     SET THE NEW DECAY RATE PARAMETERS FOR THE MODEL

  JSP = 0
  call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (JSP .eq. -999) goto 10
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

  goto 10
  1200 continue
  !                        OPTION NUMBER 12 --  SNAGOUT

  !     PRINT THE SNAG OUTPUT TABLE. USERS MAY SELECT ONE/SEVERAL/ALL STANDS
  !     IN ONE/PERIODIC/ALL YEARS

  if (ICALL .eq.2) then
     write(JOSTND,1204) KEYWRD
     1204 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
  goto 10
  1300 continue
  !                        OPTION NUMBER 13 -- SNAGCLAS

  !     ENTER THE LOWER BOUND OF THE CLASSES BY WHICH TO GROUP THE
  !     SNAG LIST OUTPUT

  ICHNG = 0
  do ICLS = 1,6
     if (LNOTBK(ICLS)) then
        SNPRCL(ICLS) = ARRAY(ICLS) * CMtoIN
        ICHNG = ICLS
        if (SNPRCL(ICLS) .gt.36.0.and. SNPRCL(ICLS) .ne.999.0)&
             &SNPRCL(ICLS) = 36.0
     end if
     1320 continue
  end do
  if (ICHNG .gt.0) then
     do ICLS = ICHNG+1,6
        SNPRCL(ICLS) = 999.0
        1330 continue
     end do
  end if

  if (LKECHO)write(JOSTND,1350) KEYWRD,&
       &(ICLS,SNPRCL(ICLS)*INtoCM,ICLS = 1,6)
  1350 format (/A8,'   THE LOWER BOUNDARY FOR EACH DBH CLASS FOR ',&
       &'PRINTING THE SNAG OUTPUT (IN CM): ',&
       &/T12,6('CLASS',I2,'=',F7.1:'; '))
  goto 10
  1400 continue
  !                        OPTION NUMBER 14 -- LANDOUT

  !     REQUEST THAT THE LANDSCAPE OUTPUT TABLES BE PRINTED. THERE ARE THREE:
  !     1) FUEL LOADING LEVELS, 2) FUEL MODEL, 3) FIRE INFO
  !     (EFFECTS/POTENTIAL FLAMES).
  !     USING THE KEYWORD MEANS THAT THE OUTPUT FILES WILL BE PRINTED (EVEN IF
  !     NONE OF THE FIELDS ARE ENTERED) UNLESS THE USER ENTERS A VALUE <= 0.

  if (ICALL .eq.1) then
     write(JOSTND,1404) KEYWRD
     1404 format(/A8,'   ***KEYWORD IS A LANDSCAPE-LEVEL KEYWORD',&
          &' ONLY')
     goto 10
  end if

  JLOUT(1) = 36
  JLOUT(2) = 37
  JLOUT(3) = 38
  PLSIZ(1) = 4
  PLSIZ(2) = 8
  IHEAD = 0
  if (LNOTBK(1)) JLOUT(1) = IFIX(ARRAY(1))
  if (LNOTBK(2)) JLOUT(2) = IFIX(ARRAY(2))
  if (LNOTBK(3)) JLOUT(3) = IFIX(ARRAY(3))
  if (LNOTBK(4)) PLSIZ(1) = IFIX(ARRAY(4) * MtoFT)
  if (LNOTBK(5)) PLSIZ(2) = IFIX(ARRAY(5) * MtoFT)
  if (LNOTBK(6)) IHEAD = IFIX(ARRAY(6))
  if (IHEAD .ne.0) LANHED = .false.

  if (JLOUT(1) .gt.0.or. JLOUT(2) .gt.0.or. JLOUT(3).gt.0) then
     if (LKECHO)write(JOSTND, 1410) KEYWRD
     1410 format(/A8,'   THE FOLLOWING LANDSCAPE-LEVEL REPORTS WILL',&
          &' BE PRINTED EACH YEAR: ')

     if ((JLOUT(1) .gt.0).and.LKECHO)write(JOSTND,1420) JLOUT(1)
     if ((JLOUT(2) .gt.0).and.LKECHO)write(JOSTND,1430) JLOUT(2)
     if ((JLOUT(3) .gt.0).and.LKECHO)write(JOSTND,1440) JLOUT(3),&
          &PLSIZ(1), PLSIZ(2)

     1420 format(T17,'LOADING CATEGORIES FOR FUEL AND SNAGS, UNIT: ',I3)
     1430 format(T17,'PERCENT OF LANDSCAPE USING EACH FUEL MODEL, ',&
          &'UNIT: ',I3)
     1440 format(T17,'FIRE EFFECTS & POTENTIAL FLAME LENGTH INFO, ',&
          &'UNIT: ',I3,/T17,'THE AREA WILL BE GROUPED INTO ',&
          &'FLAME LENGTHS THAT ARE ABOVE OR BELOW ',I3,' M',&
          &' AND ABOVE ',I3,' M.')

     if ((.not. LANHED).and.LKECHO)write(JOSTND,1450)
     1450 format(T12,'TABLE HEADINGS WILL NOT BE PRINTED.')

  else
     if (LKECHO)write(JOSTND,1490) KEYWRD
     1490 format(/A8,'     NO LANDSCAPE-LEVEL REPORTS WILL BE PRINTED')
  end if

  goto 10
  1500 continue
  !                        OPTION NUMBER 15 -- FUELOUT

  if (ICALL .eq.2) then
     write(JOSTND,1504) KEYWRD
     1504 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  if (IDFLAL .eq.0) call GETID (IDFLAL)

  IFLALB = IY(1)
  IFLALE = IY(1) + 999

  if (LKECHO)write(JOSTND,1515) KEYWRD
  1515 format(/A8,'   THE ALL FUELS REPORT WILL BE PRINTED.')

  goto 10
  1600 continue
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
        if (LKECHO)write(JOSTND,1650) KEYWRD,IDEC,DKR(10,IDEC),&
             &DKR(11,IDEC),(DKR(I,IDEC),I = 1,4)
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

        if (LKECHO)write(JOSTND,1655) KEYWRD,DKR(10,4),DKR(11,4),&
             &(DKR(I,4),I = 1,4)
     end if

     1650 format (/A8,'   THE TOTAL DECAY RATES FOR DECAY CLASS',I2,&
          &' WILL BE',/T12,'LITTER: ',F5.3,' DUFF: ',F5.3,' 0-0.6: ',&
          &F5.3,' 0.6-2.5: ',F5.3,' 2.5-7.6: ',F5.3,' >7.6: ',F5.3)
     1655 format (/A8,'   THE TOTAL DECAY RATES FOR ALL DECAY CLAS',&
          &'SES WILL BE',/T12,'LITTER: ',F5.3,' DUFF: ',F5.3,' 0-0.6: ',&
          &F5.3,' 0.6-2.5: ',F5.3,' 2.5-7.6: ',F5.3,' >7.6: ',F5.3)

  else

     write(JOSTND,1660) KEYWRD
     1660 format (/A8,'    **** NO DECAY CLASS WAS SPECIFIED. ',&
          &'KEYWORD WILL BE IGNORED!')
     call ERRGRO (.true.,1)
  end if

  goto 10
  1700 continue
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

        if (LKECHO)write(JOSTND,1750) KEYWRD,IDEC,&
             &PRDUFF(10,IDEC), (PRDUFF(I,IDEC),I = 1,4)
        1750 format (/A8,'   THE PROPORTION OF THE DECOMPOSING ',&
             &' MATERIAL WHICH GOES TO DUFF IN DECAY POOL ',I2,' IS:',&
             &/T12,'LITTER: ',F4.2,' 0-0.6: ',F4.2,&
             &' 0.6-2.5: ',F4.2,' 2.5-7.6: ',F4.2,' >7.6: ',F4.2)
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

        if (LKECHO)write(JOSTND,1755) KEYWRD, PRDUFF(10,4),&
             &(PRDUFF(I,4),I = 1,4)
        1755 format (/A8,'   THE PROPORTION OF THE DECOMPOSING ',&
             &' MATERIAL WHICH GOES TO DUFF IS',/T12,'LITTER: ',F4.2,&
             &' 0-0.6: ',F4.2,&
             &' 0.6-2.5: ',F4.2,' 2.5-7.6: ',F4.2,' >7.6: ',F4.2)
     end if
  else
     write(JOSTND,1760) KEYWRD
     1760 format (/A8,'    ****NO DECAY POOL WAS SPECIFIED. ',&
          &'KEYWORD WILL BE IGNORED!')
  end if

  goto 10
  1800 continue
  !                        OPTION NUMBER 18 -- MOREOUT

  !     A TOP-SECRET KEYWORD THAT ALLOWS US MODELLERS TO GET SOME
  !     ADDITIONAL OUTPUT

  JCOUT = 30
  if (LKECHO)write(JOSTND,1860) KEYWRD
  1860 format (/A8)

  goto 10
  1900 continue
  !                        OPTION NUMBER 19 -- FUELPOOL

  !     SET THE FUEL DECAY POOL THAT EACH SPECIES BELONGS TO.

  JSP = 0
  call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (JSP .eq. -999.or..not.LNOTBK(2)) goto 10
  IDEC = INT(ARRAY(2))
  if (IDEC .lt.1.or. IDEC .gt.4) goto 10

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

  goto 10
  2000 continue
  !                        OPTION NUMBER 20 -- SALVAGE

  !     THIS KEYWORD ALLOWS USERS TO REMOVE SNAGS FROM THE SNAG LIST.

  if (ICALL .eq.2) then
     write(JOSTND,2004) KEYWRD
     2004 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  MYACT = 2520
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  NPARMS = 6
  PRMS(1) = 0.0
  PRMS(2) = 999.0
  PRMS(3) = 5.0
  PRMS(4) = 1.0
  PRMS(5) = 0.9
  PRMS(6) = 0.0

  if (LNOTBK(2)) PRMS(1) = ARRAY(2) * CMtoIN
  if (LNOTBK(3)) PRMS(2) = ARRAY(3) * CMtoIN
  if (LNOTBK(4)) PRMS(3) = ARRAY(4)
  if (LNOTBK(5)) PRMS(4) = ARRAY(5)
  if (LNOTBK(6)) PRMS(5) = ARRAY(6)
  if (LNOTBK(7)) PRMS(6) = ARRAY(7)

  !     SET THE BOUNDS FOR VARIOUS VARIABLES

  PRMS(4) = INT(PRMS(4))
  if (PRMS(4) .gt.2.0.or. PRMS(4) .lt.0.0) PRMS(4) = 0.0
  PRMS(5) = MIN(1.0,MAX(0.0,PRMS(5)))
  PRMS(6) = MIN(1.0,MAX(0.0,PRMS(6)))

  if (LKECHO)write(JOSTND,2010) KEYWRD, IDT,&
       &PRMS(1)*INtoCM, PRMS(2)*INtoCM, PRMS(3)
  2010 format(/A8,'   IN DATE/CYCLE ', I4,' THE PROPORTION OF SNAGS ',&
       &'THAT ARE BETWEEN ',F4.0,' AND ',F4.0,/T12,&
       &'CM DBH AND HAVE BEEN DEAD LESS THAN ',F4.0,' YEARS ',&
       &'AND ARE ')

  if (PRMS(4) .eq.1.0) then
     if (LKECHO)write(JOSTND,2011) 'STILL HARD',PRMS(5)
  else if (PRMS(4) .eq.2.0) then
     if (LKECHO)write(JOSTND,2011) 'SOFT',PRMS(5)
  else
     if (LKECHO)write(JOSTND,2011) 'EITHER HARD OR SOFT',PRMS(5)
  end if
  2011 format(T12,A,' THAT WILL BE REMOVED AS A SALVAGE ',&
       &'CUT IS ',F5.3)
  if (LKECHO)write(JOSTND,2012) PRMS(6)
  2012 format(T12,'THE PROPORTION OF TREATED SNAGS THAT WILL REMAIN IN ',&
       &'THE STAND IS ',F5.3)

  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10

  2100 continue
  !                        OPTION NUMBER 21 -- FUELINIT

  !     THIS KEYWORD SETS THE INITIAL HARD/SOUND FUEL VALUES, PLUS LITTER AND DUFF
  !     IF THEY ARE TO BE DIFFERENT THAN THE DEFAULT ONES.

  if (ICALL .eq.2) then
     write(JOSTND,2104) KEYWRD
     2104 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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

  if (LKECHO)write(JOSTND,2110) KEYWRD,(PRMS(I),I = 1,12)
  2110 format(/A8,'   INITIAL HARD FUEL VALUES (TONNES/HA) ARE',&
       &' (-1.0=NO VALUE SPECIFIED): FUELS <2.5cm=',F5.1,&
       &'; FUELS 2.5-7.6cm=',F5.1/T12,'FUELS 7.6-15.2cm=',F5.1,&
       &'; FUELS 15.2-30.5cm=',F5.1,'; FUELS 30.5-50.8cm=',F5.1,&
       &'; LITT=',F5.1,'; DUFF=',F5.1/T12,'FUELS <0.6cm=',F5.1,&
       &'; FUELS 0.6-2.5cm=',F5.1/T12,'FUELS 50.8-89.0cm=',F5.1,&
       &'; FUELS 89.0-127.1cm=',F5.1,'; FUELS >127.1cm=',F5.1)

  do I = 1,NPARMS
     if (PRMS(I) .gt.0.0) PRMS(I) = PRMS(I) * TMtoTI / HAtoACR
     2115 continue
  end do

  MYACT = 2521
  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10
  2200 continue
  !                        OPTION NUMBER 22 -- SNAGINIT

  !     THIS KEYWORD ALLOWS USERS TO ADD ADDITIONAL SNAGS TO ONE OR MORE STANDS

  if (ICALL .eq.2) then
     write(JOSTND,2204) KEYWRD
     2204 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
  if (JSP.eq.0.or. JSP.eq.-999) goto 10
  PRMS(1) = JSP
  if (LNOTBK(2)) PRMS(2) = ARRAY(2)
  if (LNOTBK(3)) PRMS(3) = ARRAY(3)
  if (LNOTBK(4)) PRMS(4) = ARRAY(4)
  if (LNOTBK(5)) PRMS(5) = ARRAY(5)
  if (LNOTBK(6)) PRMS(6) = ARRAY(6)

  if (LKECHO)write(JOSTND,2210) KEYWRD,KARD(1)(1:3),(PRMS(I),I = 2,6)
  2210 format(/A8,'   INITIAL SNAG CHARACTERISTICS ',&
       &'(-1.0=NO VALUE SPECIFIED): SPECIES: ',A,&
       &'; DBH AT DEATH (CM): ',F5.1,&
       &/T12,'HEIGHT AT DEATH (M): ',F5.1,'; CURRENT HEIGHT: ',&
       &F5.1,'; AGE: ',F4.0,'; DENSITY (STEMS/HA): ',F6.1)

  if (PRMS(2) .gt.0.0) PRMS(2) = PRMS(2) * CMtoIN
  if (PRMS(3) .gt.0.0) PRMS(3) = PRMS(3) * MtoFT
  if (PRMS(4) .gt.0.0) PRMS(4) = PRMS(4) * MtoFT
  if (PRMS(6) .gt.0.0) PRMS(6) = PRMS(6) / HAtoACR

  MYACT = 2522
  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10
  2300 continue
  !                        OPTION NUMBER 23 -- PILEBURN (formerly FUELBURN)

  !     SET UP JACKPOT AND PILE BURNS.

  if (ICALL .eq.2) then
     write(JOSTND,2304) KEYWRD
     2304 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
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
  goto 10
  2400 continue
  !                        OPTION NUMBER 24 -- SNAGPBN

  !     CHANGE THE PARAMETERS WHICH SET THE RATE AT WHICH SNAGS FALL
  !     AFTER A BURN.

  if (LNOTBK(1)) PBSOFT = ARRAY(1)
  if (LNOTBK(2)) PBSMAL = ARRAY(2)
  if (LNOTBK(3)) PBTIME = ARRAY(3)
  if (LNOTBK(4)) PBSIZE = ARRAY(4) * CMtoIN
  if (LNOTBK(5)) PBSCOR = ARRAY(5) * MtoFT

  if (PBSOFT .lt.0.0) PBSOFT = 0.0
  if (PBSOFT .gt.1.0) PBSOFT = 1.0
  if (PBSMAL .lt.0.0) PBSMAL = 0.0
  if (PBSMAL .gt.1.0) PBSMAL = 1.0
  if (PBTIME .lt.1.0) PBTIME = 1.0
  if (PBSIZE .lt.0.0) PBSIZE = 0.0
  if (PBSCOR .lt.0.0) PBSCOR = 0.0

  if (LKECHO)write(JOSTND,2420) KEYWRD,PBSOFT,PBSMAL,PBTIME,&
       &PBSIZE*INtoCM,PBSCOR*FTtoM
  2420 format(/A8,'   THE PROPORTIONS OF SOFT OR SMALL SNAGS TO ',&
       &'FALL AFTER A BURN ARE (RESPECTIVELY): ',F4.2,F5.2,&
       &/T12,'THE NUMBER OF YEARS IN WHICH THEY FALL IS: ',F4.0,&
       &' SMALL SNAGS ARE THOSE LESS THAN: ',F4.0,' CM DBH.',&
       &/T12,'THE THRESHOLD SCORCH HEIGHT FOR THE POST-BURN ',&
       &'FALLING IS: ',F5.1)

  goto 10
  2500 continue
  !                        OPTION NUMBER 25 -- FUELTRET

  !     SET THE TYPE OF FUEL TREATMENT AND HARVEST TYPE THAT WAS DONE.  NOTE
  !     THAT THIS KEYWORD WILL ONLY AFFECT FIRE INTENSITY AND ONLY WITHIN 5
  !     YEARS AFTER A STAND ENTRY.

  if (ICALL .eq.2) then
     write(JOSTND,2504) KEYWRD
     2504 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
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

  if ((PRMS(3) .ge.0.0).and.LKECHO)write(JOSTND,2511) PRMS(3)
  2511 format(T12,'MULTIPLIER FOR FUEL DEPTH WILL BE ',F5.1)

  MYACT = 2525
  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10

  2600 continue
  !                        OPTION NUMBER 26 -- STATFUEL

  !     DISABLE THE DYNAMIC FUEL MODEL FOR THE ENTIRE LANDSCAPE

  LDYNFM = .false.
  if (LKECHO)write(JOSTND,2610) KEYWRD
  2610 format(/A8,'   THE DYNAMIC FUEL MODEL IS DISABLED. ')

  goto 10
  2700 continue

  !                        OPTION NUMBER 27 -- FUELREPT

  !     PRODUCE THE FUEL CONSUMPTION AND PHYSICAL EFFECTS REPORT. RETREIVE
  !     THE ID NUMBER FOR THE REPORT.

  if (IDFUL .eq.0) call GETID (IDFUL)

  IFMFLB = -1
  IFMFLE = -1

  if (LKECHO)write(JOSTND,2705) KEYWRD
  2705 format(/A8,'   THE FUEL CONSUMPTION AND PHYSICAL EFFECTS',&
       &' REPORT WILL BE WRITTEN WHEN A FIRE OCCURS.')

  goto 10

  2800 continue

  !                        OPTION NUMBER 28 -- MORTREPT

  !     PRODUCE THE TREE MORTALITY REPORT. RETREIVE THE ID NUMBER
  !     FOR THE REPORT.

  if (IDMRT .eq.0) call GETID (IDMRT)

  IFMMRB = -1
  IFMMRE = -1

  if (LKECHO)write(JOSTND,2805) KEYWRD
  2805 format(/A8,'   THE TREE MORTALITY REPORT WILL BE WRITTEN',&
       &' WHEN A FIRE OCCURS.')

  goto 10
  2900 continue
  !                        OPTION NUMBER 29 -- FUELMULT

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

  goto 10
  3000 continue
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
     if (LKECHO)write(JOSTND,3050) KEYWRD, 'SEVERE',&
          &(PRESVL(IFIRE,IARRY)*100.0,IARRY = 2,8)
     3050 format (/A8,'   FIRE MOISTURE CONDITIONS FOR CALCULATING ',&
          &A,' POTENTIAL FLAME LENGTHS ARE: '&
          &/T12,'% MOISTURE FOR 0-0.6cm= ',F4.0,'; 0.6-2.5cm= ',F4.0,&
          &'; 2.5-7.6cm= ',F4.0,'; 7.6+cm= ',F4.0,'; DUFF=',F4.0,&
          &'; LIVE WOODY =',F4.0,'; LIVE HERB =',F4.0)
  else
     if (LKECHO)write(JOSTND,3050) KEYWRD, 'MODERATE',&
          &(PRESVL(IFIRE,IARRY)*100.0,IARRY = 2,8)
  end if

  goto 10
  3100 continue

  !                        OPTION NUMBER 31 -- SNAGSUM

  !     REQUEST A SNAG REPORT SUMMARY

  ISNGSM = -1
  if (ARRAY(1).ge.0.) ISNGSM = 0
  if (ISNGSM.ge.0) then
     if (LKECHO)write(JOSTND,3110) KEYWRD,'REQUESTED'
     3110 format (/A8,'   SNAG SUMMARY REPORT ',A)
  else
     if (LKECHO)write(JOSTND,3110) KEYWRD,'TURNED OFF'
  end if
  goto 10
  3200 continue

  !                        OPTION NUMBER 32 -- MORTCLAS

  !     SET-UP THE SIZE CLASSES FOR REPORTING FIRE-BASED MORTALITY

  do IARRY = 1,7
     if (LNOTBK(IARRY)) LOWDBH(IARRY) = ARRAY(IARRY) * CMtoIN
     3210 continue
  end do

  if (LKECHO)write(JOSTND,3250) KEYWRD,&
       &(IARRY, LOWDBH(IARRY)*INtoCM,IARRY = 1,7)
  3250 format (/A8,'   THE LOWER BOUND OF EACH SIZE CLASS USED ',&
       &'IN THE MORTALITY REPORT IS:',&
       &/T12,7('CLASS',I2,'=',F4.1:'; '))

  goto 10
  3300 continue

  !                        OPTION NUMBER 33 -- DROUGHT

  !     DEFINE THE YEARS IN WHICH A DROUGHT OR DORMANT YEAR OCCURS

  if (ICALL .eq.2) then
     write(JOSTND,3304) KEYWRD
     3304 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  NPARMS = 1
  PRMS(1) = 1.0
  if (LNOTBK(2) .and. ARRAY(2) .gt.0.0) PRMS(1) = ARRAY(2)

  if (LKECHO)write(JOSTND,3305) KEYWRD,IDT,INT(PRMS(1))
  3305 format(/A8,'   IN DATE/CYCLE ',I4,' DROUGHT/DORMANCY IS',&
       &' SIMULATED FOR ', I3, ' YEARS.')

  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10
  3400 continue

  !                        OPTION NUMBER 34 -- FUELMOVE

  !     TRANSFER FUELS FROM ONE SIZE CATEGORY TO ANOTHER. TRANSFERS
  !     MAY INCLUDE LARGER TO SMALLER, SMALLER TO LARGER. IMPORTING
  !     AND EXPORTING MAY OCCUR. NOTE THAT THE SEQUENCE OF CATEGORIES
  !     IS NOT ORDERED SMALLEST TO LARGEST: LITTER AND DUFF ARE IN THE
  !     HIGHEST POSITIONS

  if (ICALL .eq.2) then
     write(JOSTND,3404) KEYWRD
     3404 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  MYACT = 2530
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  NPARMS = 6
  II = 0

  !     *SOURCE* CWD POOL - MUST HAVE A VALID ENTRY (0,1-11)
  !     DEFAULT 6 (>12")
  !       0 = IMPORTED (EXPORTED IN *TARGET* CONTEXT)
  !       1 = <0.25"         (<0.6 CM)
  !       2 =  0.25 TO   1"  ( 0.6 TO  2.5 CM)
  !       3 =  1    TO   3"  ( 2.5 TO  7.6 CM)
  !       4 =  3    TO   6"  ( 7.6 TO 15.2 CM)
  !       5 =  6    TO  12"  (15.2 TO 30.5 CM)
  !       6 =  12   TO  20"
  !       7 =  20   TO  35"
  !       8 =  35   TO  50"
  !       9 =  > 50"
  !       10 = LITTER
  !       11 = DUFF

  PRMS(1) = 6.0
  if (LNOTBK(2)) then
     if ((ARRAY(2) .lt.0.0) .or. (ARRAY(2) .gt.11.0)) then
        II = II + 1
     else
        PRMS(1) = ARRAY(2)
     end if
  end if

  !     *TARGET* FUEL POOL (SAME CATEGORIES AS SOURCE); MUST HAVE A
  !     VALID ENTRY (0,1-11), DEFAULT 11 (DUFF)

  PRMS(2) = 11.0
  if (LNOTBK(3)) then
     if ((ARRAY(2) .lt.0.0) .or. (ARRAY(2) .gt.11.0)) then
        II = II + 1
     else
        PRMS(2) = ARRAY(3)
     end if
  end if

  !     INVALID TO HAVE SAME SOURCE AND TARGET POOL

  if (INT(PRMS(1)) .eq. INT(PRMS(2))) II = II + 1

  !     AMOUNT TO MOVE (TONNES/HA) - MUST BE >=0

  PRMS(3) = 0.0
  if (LNOTBK(4)) then
     if (ARRAY(4) .ge.0.0) then
        PRMS(3) = ARRAY(4) * TMtoTI / HAtoACR
     else
        II = II + 1
     end if
  end if

  !     PROPORTION TO MOVE  MUST BE 0-1

  PRMS(4) = 0.0
  if (LNOTBK(5)) then
     if (ARRAY(5) .ge.0.0.and. ARRAY(5) .le.1.0) then
        PRMS(4) = ARRAY(5)
     else
        II = II + 1
     end if
  end if

  !     RESIDUAL AMOUNT (TONNES/HA) - MUST BE >=0

  PRMS(5) = 9999.0
  if (LNOTBK(6)) then
     if (ARRAY(6) .ge.0.0) then
        PRMS(5) = ARRAY(6) * TMtoTI / HAtoACR
     else
        II = II + 1
     end if
  end if

  !     TARGET AMOUNT (TONNES/HA): UNSET= 0.0; ELSE >=0

  PRMS(6) = 0.0
  if (LNOTBK(7)) then
     if (ARRAY(7) .ge.0.0) then
        PRMS(6) = ARRAY(7) * TMtoTI / HAtoACR
     else
        II = II + 1
     end if
  end if

  if (II .eq.0) then
     if (LKECHO)write(JOSTND,3405) KEYWRD,IDT,INT(PRMS(1)),&
          &INT(PRMS(2))
     3405 format(/A8,'   IN DATE/CYCLE ',I4,' FUEL IN SIZE ',&
          &'CATEGORY',I3,' WILL BE MOVED TO',/T12,'SIZE CATEGORY ',&
          &I2,'. WHICHEVER OF THE FOLLOWING 4 CRITERIA',/T12,&
          &'MOVES THE MOST FUEL, WILL BE USED:')
     if (LKECHO)write(JOSTND, 3406) PRMS(3) * TItoTM / ACRtoHA
     if (LKECHO)write(JOSTND, 3407) PRMS(4)
     if (LKECHO)write(JOSTND, 3408) PRMS(5) * TItoTM / ACRtoHA
     if (LKECHO)write(JOSTND, 3409) PRMS(6) * TItoTM / ACRtoHA
     3406 format(T12,'TONNES/HA MOVED FROM SOURCE CATEGORY:    ',F12.3)
     3407 format(T12,'PROPORTION MOVED FROM SOURCE CATEGORY:   ',F12.3)
     3408 format(T12,'TONNES/HA REMAINING IN SOURCE CATEGORY:  ',F12.3)
     3409 format(T12,'FINAL TONNES/HA IN TARGET CATEGORY:      ',F12.3)
     call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  else
     call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
     call ERRGRO (.true.,4)
  end if

  goto 10
  3500 continue

  !                        OPTION NUMBER 35 -- POTFWIND

  if (LNOTBK(1)) PREWND(1) = ARRAY(1) * KMtoMI
  if (LNOTBK(2)) PREWND(2) = ARRAY(2) * KMtoMI

  if (LKECHO)write(JOSTND,3550) KEYWRD, PREWND * MItoKM
  3550 format (/A8,'   FIRE WIND SPEEDS USED FOR CALCULATING ',&
       &'POTENTIAL FLAME LENGTHS ARE',/T11,' FOR SEVERE FIRE: ',F5.0,&
       &' AND FOR MODERATE FIRE: ',F5.0,' KM/H')

  goto 10
  3600 continue

  !                        OPTION NUMBER 36 -- POTFTEMP

  if (LNOTBK(1)) POTEMP(1) = ARRAY(1) * CtoF1 + CtoF2
  if (LNOTBK(2)) POTEMP(2) = ARRAY(2) * CtoF1 + CtoF2

  if (LKECHO)write(JOSTND,3650) KEYWRD, POTEMP * FtoC1 + FtoC2
  3650 format (/A8,'   FIRE TEMPERATURES USED FOR CALCULATING ',&
       &'POTENTIAL FLAME LENGTHS ARE',/T11,' FOR SEVERE FIRE: ',F5.0,&
       &' AND FOR MODERATE FIRE: ',F5.0,' DEGREES C')

  goto 10
  3700 continue
  !                        OPTION NUMBER 37 -- SNAGPSFT

  !     SET THE PROPORTION OF EACH SPECIES THAT IS INITIALLY SOFT
  !        NOTE THE TWO CHANGES: THIS USED TO BE PART OF THE SNAGBRK KEYWORD
  !        AND THIS DID NOT USE TO VARY BY SPECIES.

  JSP = 0
  call SPDECD (1,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (JSP .eq. -999) goto 10
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

  goto 10

  3800 continue

  !                        OPTION NUMBER 38 -- FUELMODL

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
        if (IRTNCD.ne.0) return
     end if
     goto 10
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

  goto 10

  3801 continue
  call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
  call ERRGRO (.true.,4)
  goto 10
  3900 continue

  !                        OPTION NUMBER 39 -- DEFULMOD

  !     DEFINE NEW OR ALTER EXISTING FUEL MODELS (ADDED MARCH 2002)

  MYACT = 2539
  IDT = 1
  if (LNOTBK(1)) then
     I = IFIX(ARRAY(1))
     if (I .ge.0) IDT = I
  end if

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,MYACT,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  NPARMS = 13
  do I = 1,NPARMS
     PRMS(I) = -1.0
  end do

  !     READ VALUES FROM THE SUPPLEMENTAL RECORD
  !     READ AS CHARACTER STRINGS SO THAT THE -1 CAN BE USED
  !     TO SHOW THAT NO VALUE WAS ENTERED.

  IRECNT = IRECNT + 1
  read(IREAD,'(7A10)',ERR = 3901) (APRMS(I),I = 7,13)
  do I = 7,13
     if (APRMS(I) .ne. ' ')&
          &read(APRMS(I),'(F10.0)',ERR = 3901) PRMS(I)
  end do
  goto 3903

  3901 continue
  call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
  call ERRGRO (.true.,4)
  goto 10

  !     CHECK FOR VALID FUEL MODEL NUMBER

  3903 LOK = .true.
  if (LNOTBK(2)) then
     IFMD = INT(ARRAY(2))
     if (IFMD .ge.1.and. IFMD .le. MXDFMD) then
        PRMS(1) = FLOAT(IFMD)
     else
        LOK = .false.
     end if
  end if

  !     IF THE MODEL NUMBER IS VALID, READ THE REMAINING 10 PARAMETERS

  if (LOK) then

     do I = 2,4
        if (LNOTBK(I+1)) then
           PRMS(I) = ARRAY(I+1) / MtoFT
        else
           PRMS(I) = SURFVL(IFMD,1,I-1)
        end if
     end do

     I = 5
     if (LNOTBK(I+1)) then
        PRMS(I) = ARRAY(I+1) / MtoFT
     else
        PRMS(I) = SURFVL(IFMD,2,1)
     end if

     I = 6
     if (LNOTBK(I+1)) then
        PRMS(I) = ARRAY(I+1) * FT2toM2/LBtoKG
     else
        PRMS(I) = FMLOAD(IFMD,1,1)
     end if

     do I = 7,8
        if (PRMS(I) .lt.0.0) then
           PRMS(I) = FMLOAD(IFMD,1,I-5)
        else
           PRMS(I) = PRMS(I) * FT2toM2/LBtoKG
        end if
     end do

     I = 9
     if (PRMS(I) .lt.0.0) then
        PRMS(I) = FMLOAD(IFMD,2,1)
     else
        PRMS(I) = PRMS(I) * FT2toM2/LBtoKG
     end if

     I = 10
     if (PRMS(I) .lt.0.0) then
        PRMS(I) = FMDEP(IFMD)
     else
        PRMS(I) = PRMS(I) * MtoFT
     end if

     I = 11
     if (PRMS(I) .lt.0.0) then
        PRMS(I) = MOISEX(IFMD)
     end if

     I = 12
     if (PRMS(I) .lt.0.0) then
        PRMS(I) = SURFVL(IFMD,2,2)
     else
        PRMS(I) = PRMS(I) / MtoFT
     end if

     I = 13
     if (PRMS(I) .lt.0.0) then
        PRMS(I) = FMLOAD(IFMD,2,2)
     else
        PRMS(I) = PRMS(I) * FT2toM2/LBtoKG
     end if
  end if

  !     CHECK FOR VALID (>=0) S/V RATIO

  do I = 2,5
     if (PRMS(I) .lt.0.0) then
        LOK = .false.
        exit
     end if
  end do

  !     CHECK FOR NON-ZERO FUELS

  if (LOK) then
     XSUM = 0.0
     do I = 6,9
        XSUM = XSUM + PRMS(I)
     end do
     XSUM = XSUM + PRMS(13)
     if (XSUM .lt.0.0001) LOK = .false.
  end if

  !     CHECK FOR VALID (>=0) FUEL DEPTH

  if (LOK .and. PRMS(10) .le.0.0)  LOK = .false.

  !     CHECK FOR VALID (>=0) MOISTURE OF EXTINCTION

  if (LOK .and. PRMS(11) .lt.0.0.or. PRMS(11) .gt.1.0)&
       &LOK = .false.

  if (LOK) then
     if (LKECHO)write(JOSTND,3910) KEYWRD,IDT,INT(PRMS(1)),&
          &(PRMS(I) / FTtoM, I = 2,5),PRMS(12)/FTtoM,&
          &(PRMS(I) * LBtoKG/FT2toM2, I = 6,9),&
          &PRMS(13) * LBtoKG/FT2toM2,&
          &PRMS(10) * FTtoM, PRMS(11)
     3910 format(/A8,'   IN DATE/CYCLE ',I4,' THE VALUES FOR FUEL',&
          &' MODEL', I3,' WILL BE: ',&
          &/T12, 'SURFACE TO VOL RATIO (/M):',&
          &/T14, '    (<0.6cm) = ', F6.0,&
          &/T14, ' (0.6-2.5cm) = ', F6.0,&
          &/T14, ' (2.5-7.6cm) = ', F6.0,&
          &/T14, '(LIVE WOODY) = ', F6.0,&
          &/T14, ' (LIVE HERB) = ', F6.0,&
          &/T12,'FUEL LOADING (KG/SQ M):',&
          &/T14, '    (<0.6cm) = ', F7.4,&
          &/T14, ' (0.6-2.5cm) = ', F7.4,&
          &/T14, ' (2.5-7.6cm) = ', F7.4,&
          &/T14, '(LIVE WOODY) = ', F7.4,&
          &/T14, ' (LIVE HERB) = ', F7.4,&
          &/T12,'DEPTH = ', F6.3,&
          &/T12,'MOISTURE OF EXTINCTION =', F7.4)
     call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  else
     write(JOSTND,3911) KEYWRD,IRECNT-1,IRECNT
     3911 format(/A8,'   ERROR AT CARD ', I4, ' OR CARD ', I4,&
          &' MAKES IT INVALID. ')
     call FMKEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
     call ERRGRO (.true.,4)
  end if

  goto 10
  4000 continue
  !                        OPTION NUMBER 40 -- CANCALC

  !     SET OPTIONS FOR CALCULATION OF CANOPY BASE HEIGHT AND CANOPY BULK
  !     DENSITY (note: already kg/m3).

  if (ICALL .eq.2) then
     write(JOSTND,4004) KEYWRD
     4004 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  if (LNOTBK(1)) ICBHMT = IFIX(ARRAY(1))
  if (LNOTBK(2)) CANMHT = ARRAY(2) * MtoFT
  if (LNOTBK(3)) ICANSP = IFIX(ARRAY(3))
  if (LNOTBK(4)) CBHCUT = ARRAY(4)
  if (LNOTBK(5)) FOLMC = ARRAY(5)

  if (LKECHO)write(JOSTND,4010) KEYWRD, ICBHMT, CANMHT * FTtoM,&
       &ICANSP, CBHCUT, FOLMC
  4010 format(/A8,T12,'CALCULATION OF CANOPY BASE HEIGHT AND CANOPY ',&
       &'BULK DENSITY WILL USE METHOD ',I1,',',/T12,'TREES ',&
       &'AT LEAST ',F5.1,' M TALL, SPECIES CATEGORY ',I1,&
       &', A CUTOFF VALUE OF ',F5.1,',',/T12,'AND A FMC OF ',F5.1)

  goto 10
  4100 continue

  !                        OPTION NUMBER 41 -- POTFSEAS

  if (LNOTBK(1)) POTSEAS(1) = INT(ARRAY(1))
  if (LNOTBK(2)) POTSEAS(2) = INT(ARRAY(2))

  if (LKECHO)write(JOSTND,4150) KEYWRD, POTSEAS
  4150 format (/A8,'   SEASONS USED FOR CALCULATING POTENTIAL FIRE ',&
       &'BEHAVIOR ARE ',/T11,' FOR SEVERE FIRE: ',I2,&
       &' AND FOR MODERATE FIRE: ',I2)
  goto 10
  4200 continue

  !                        OPTION NUMBER 42 -- POTFPAB

  if (LNOTBK(1)) POTPAB(1) = ARRAY(1)
  if (LNOTBK(2)) POTPAB(2) = ARRAY(2)

  if (LKECHO)write(JOSTND,4250) KEYWRD, POTPAB
  4250 format (/A8,'   % AREA BURNED VALUES USED FOR CALCULATING ',&
       &'POTENTIAL FIRE EFFECTS ARE',/T11,' FOR SEVERE FIRE: ',F5.1,&
       &' AND FOR MODERATE FIRE: ',F5.1)
  goto 10
  4300 continue

  !                        OPTION NUMBER 43 -- SOILHEAT

  if (IDSHEAT .eq.0) call GETID (IDSHEAT)

  ISHEATB = -1
  ISHEATE = -1
  SOILTP = 3
  if (LNOTBK(3)) SOILTP = INT(ARRAY(3))
  if (SOILTP .lt.1) SOILTP = 1
  if (SOILTP .gt.5) SOILTP = 5

  if (LKECHO)write(JOSTND,4315) KEYWRD, SOILTP
  4315 format(/A8,'   SOIL HEATING WILL BE ESTIMATED AND',&
       &' REPORTED WHEN A FIRE OCCURS.'&
       &/T12,'SOIL TYPE IS SET TO ',I4)

  goto 10
  4400 continue
  !                        OPTION NUMBER 44 -- CARBREPT

  if (ICALL .eq.2) then
     write(JOSTND,4401) KEYWRD
     4401 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  if (IDCRPT .eq.0) call GETID (IDCRPT)

  ICRPTB = -1
  ICRPTE = -1

  if (LKECHO)write(JOSTND,4410) KEYWRD
  4410 format(/A8,'   THE MAIN CARBON REPORT WILL BE PRINTED.')

  goto 10
  4500 continue

  !                        OPTION NUMBER 45 -- CARBCUT

  if (ICALL .eq.2) then
     write(JOSTND,4501) KEYWRD
     4501 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  if (IDCHRV .eq.0) call GETID (IDCHRV)

  ICHRVB = -1
  ICHRVE = -1

  if (LKECHO)write(JOSTND,4510) KEYWRD
  4510 format(/A8,T12,'THE HARVESTED PRODUCTS REPORT WILL BE PRINTED.')

  goto 10
  4600 continue

  !                        OPTION NUMBER 46 -- CARBCALC

  if (ICALL .eq.2) then
     write(JOSTND,4601) KEYWRD
     4601 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
  if (LNOTBK(4)) CDBRK(1) = MAX(0.0,MIN(999.0,ARRAY(4)))*CMtoIN
  if (LNOTBK(5)) CDBRK(2) = MAX(0.0,MIN(999.0,ARRAY(5)))*CMtoIN

  if (LKECHO)write(JOSTND,4610) KEYWRD,ICMETH,ICMETRC,&
       &CRDCAY,CDBRK(1)*INtoCM,CDBRK(2)*INtoCM

  4610 format(/A8,T12,'CARBON REPORTS WILL BE BASED ON METHOD',&
       &I2, ' (0=FFE, 1=JENKINS)',/T12, 'REPORT UNITS WILL BE',&
       &I2, ' (0=US(TONS/ACRE), 1=METRIC(METRIC TONS/HA)',&
       &' 2= METRIC TONS/ACRE))',/T12,&
       &'PROPORTION OF DEAD ROOTS DECAYING ANNUALLY WILL BE: ',&
       &F7.4,' (<0 = NO DEAD ROOTS)',/T12,&
       &'SOFTWOOD DIAMETER BREAKPOINT: ', F5.1,/T12,&
       &'HARDWOOD DIAMETER BREAKPOINT: ', F5.1)

  goto 10
  4700 continue
  !                        OPTION NUMBER 47 -- CANFPROF
  if (ICALL .eq.2) then
     write(JOSTND,4701) KEYWRD
     4701 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  ICANPR = 1
  call DBSFMLINK(ICANPR)
  ICFPB = -1
  ICFPE = -1

  if (LKECHO)write(JOSTND,4710) KEYWRD
  4710 format(/A8,'   CANOPY FUELS PROFILE TABLE SENT ',&
       &'TO SPECIFIED DATABASE.')

  goto 10
  4800 continue
  !                        OPTION NUMBER 48 -- FUELFOTO

  !     THIS KEYWORD SETS THE INITIAL FUEL VALUES USING FUELS PHOTO
  !     SERIES INFORMATION.

  if (ICALL .eq.2) then
     write(JOSTND,4804) KEYWRD
     4804 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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

  goto 10
  4900 continue

  !                        OPTION NUMBER 49 -- FIRECALC

  if (ICALL .eq.2) then
     write(JOSTND,4901) KEYWRD
     4901 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if

  !     DEFAULTS (*) SET IN FMINIT:
  !     PRM1: FIRE CALCULATION METHOD: 0 = OLD FM LOGIC (*), 1 = NEW FM LOGIC, 2 = USE MODELLED LOADS
  !     PRM2: FUEL MODEL SET: 0 = 13, 1 = 40, 2 = 53 (*)

  !           INTERNAL UNITS                                     METRIC INPUT UNITS

  !     PRM3: ONE-HR SAV (1/FT) = 2000 (*)                   <- /m (m**2/m**3)
  !     PRM4: HERB SAV (1/FT) = 1800 (*)                     <- /m
  !     PRM5: LIVE WOODY SAV (1/FT) = 1500 (*)               <- /m
  !     PRM6: BULK DENSITY - LIVE (LBS/FT3) = 0.10 (*)       <- kg/m**3
  !     PRM7: BULK DENSITY - DEAD (LBS/FT3) = 0.75 (*)       <- kg/m**3
  !     PRM8: DEAD AND LIVE HEAT CONTENT (BTU/LB) = 8000 (*) <- kilojoules/kilogram

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
  if (LNOTBK(4)) PRMS(3) = MAX(0.0,ARRAY(4)) / FTtoM
  if (LNOTBK(5)) PRMS(4) = MAX(0.0,ARRAY(5)) / FTtoM
  if (LNOTBK(6)) PRMS(5) = MAX(0.0,ARRAY(6)) / FTtoM
  if (LNOTBK(7)) PRMS(6) = MAX(0.0,ARRAY(7)) * KGtoLB/M3toFT3
  if (LNOTBK(8)) PRMS(7) = MAX(0.0,ARRAY(8)) * KGtoLB/M3toFT3
  if (LNOTBK(9)) PRMS(8) = MAX(0.0,ARRAY(9)) * KJtoBTU/KGtoLB

  if (LKECHO)write(JOSTND,4910) KEYWRD,IDT,INT(PRMS(1)),INT(PRMS(2)),&
       &PRMS(3)/MtoFT, PRMS(4)/MtoFT, PRMS(5)/MtoFT,&
       &PRMS(6)*(LBtoKG/FT3toM3), PRMS(7)*(LBtoKG/FT3toM3),&
       &PRMS(8)/(BTUtoKJ/LBtoKG)

  4910 format(/A8,T12,'FIRE CALCULATIONS IN DATE/CYCLE ',&
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

  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)

  goto 10
  5000 continue

  !                        OPTION NUMBER 50 -- FMODLIST

  if (ICALL .eq.2) then
     write(JOSTND,5001) KEYWRD
     5001 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
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

  goto 10
  5100 continue
  !                        OPTION NUMBER 51 -- DWDVLOUT

  if (ICALL .eq.2) then
     write(JOSTND,5104) KEYWRD
     5104 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  if (IDDWRP .eq.0) call GETID (IDDWRP)

  IDWRPB = -1
  IDWRPE = -1

  if (LKECHO)write(JOSTND,5115) KEYWRD
  5115 format(/A8,'   THE DOWN WOOD VOLUME REPORT WILL BE PRINTED.')

  goto 10
  5200 continue
  !                        OPTION NUMBER 52 -- DWDCVOUT

  if (ICALL .eq.2) then
     write(JOSTND,5204) KEYWRD
     5204 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  if (IDDWCV .eq.0) call GETID (IDDWCV)

  IDWCVB = -1
  IDWCVE = -1

  if (LKECHO)write(JOSTND,5215) KEYWRD
  5215 format(/A8,'   THE DOWN WOOD COVER REPORT WILL BE PRINTED.')

  goto 10

  5300 continue
  !                        OPTION NUMBER 53 -- FUELSOFT

  !     THIS KEYWORD SETS THE INITIAL SOFT/ROTTEN FUEL VALUES, IF THEY ARE TO BE DIFFERENT
  !     THAN THE DEFAULT ONES.

  if (ICALL .eq.2) then
     write(JOSTND,5304) KEYWRD
     5304 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
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

  if (LKECHO)write(JOSTND,5310) KEYWRD,(PRMS(I),I = 1,9)
  5310 format(/A8,'   INITIAL SOFT FUEL VALUES (TONNES/HA) ARE',&
       &' (-1.0=NO VALUE SPECIFIED): FUELS <0.6cm=',F5.1/T12,&
       &'FUELS 0.6-2.5cm=',F5.1,'; FUELS 2.5-7.6cm=',F5.1,&
       &'; FUELS 7.6-15.2cm=',F5.1,'; FUELS 15.2-30.5cm=',F5.1,&
       &'; FUELS 30.5-50.8cm=',F5.1/T12,'FUELS 50.8-89.0cm=',F5.1,&
       &'; FUELS 89.0-127.1cm=',F5.1,'; FUELS >127.1cm=',F5.1)

  do I = 1,NPARMS
     if (PRMS(I) .gt.0.0) PRMS(I) = PRMS(I) * TMtoTI / HAtoACR
     5315 continue
  end do

  MYACT = 2553
  call OPNEW(KODE,IDT,MYACT,NPARMS,PRMS)
  goto 10

  5400 continue
  !                        OPTION NUMBER 54 -- FMORTMLT

  !     SET THE SPECIES-SPECIFIC FIRE-CAUSED MORTALITY MULTIPLIER

  MYACT = 2554
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
        if (IRTNCD.ne.0) return
     end if
     goto 10
  end if
  JSP = 0
  call SPDECD (3,JSP,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
       &ARRAY,KARD)
  if (JSP.eq.-999) goto 10
  ARRAY(3) = JSP

  NPARMS = 4
  PRMS(1) = 1.0
  PRMS(2) = JSP
  PRMS(3) = 0.0
  PRMS(4) = 999.0

  if (.not.LNOTBK(4)) PRMS(3) = ARRAY(4) * CMtoIN
  if (.not.LNOTBK(5)) PRMS(4) = ARRAY(5) * CMtoIN

  if (ARRAY(4).ge.ARRAY(5)) then
     call ERRGRO (.true.,4)
     call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
  else
     call OPNEW (KODE,IDT,MYACT,4,PRMS)
     if (KODE.gt.0) goto 10
     if (LKECHO)write(JOSTND,5410) KEYWRD,IDT,ARRAY(2),&
          &KARD(3)(1:3),JSP,ARRAY(4)*INtoCM,ARRAY(5)*INtoCM

     5410 format(/A8,'   DATE/CYCLE=',I5,&
          &'; FIRE-CAUSED MORTALITY MULTIPLIER=',F10.2'; SPECIES= ',A,&
          &' (CODE= ',I3,')'/T12,'ONLY TREES GREATER THAN OR EQUAL TO',&
          &F7.2,' AND LESS THAN ',F7.2,' DBH ARE AFFECTED.')
  end if
  goto 10

  5500 continue

  !                        OPTION NUMBER 55 -- USECFIM

  !     THIS KEYWORD TELLS THE MODEL TO USE THE CFIM FIRE CALCULATIONS INSTEAD
  !     OF THE DEFAULT FFE FIRE CALCULATIONS

  if (ICALL .eq.2) then
     write(JOSTND,5504) KEYWRD
     5504 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  NPARMS = 7
  !     BULK DENSITY
  PRMS(1) = 80.0
  !     DROUGHT CODE
  PRMS(2) = 300
  !     fuel_density(kg/m^3)---rho_surf                   398.0
  PRMS(3) = 398.0
  !     surface_area_to_volume_ratio(1/m)---sigma_surf   3092.0
  PRMS(4) = 3092
  !     surface_area_to_volume_ratio(1/m)---sigma_can    5401.0
  PRMS(5) = 5401.0
  !     canopy_fuel_particle_diameter(m)---diameter     0.00074
  PRMS(6) = 0.00074
  !     canopy_fuel_particle_length(m)---length            0.10
  PRMS(7) = 0.10

  IDT = 1
  if (LNOTBK(1)) PRMS(1) = ARRAY(1)
  if (LNOTBK(2)) PRMS(2) = ARRAY(2)
  if (LNOTBK(3)) PRMS(3) = ARRAY(3)
  if (LNOTBK(4)) PRMS(4) = ARRAY(4)
  if (LNOTBK(5)) PRMS(5) = ARRAY(5)
  if (LNOTBK(6)) PRMS(6) = ARRAY(6)
  if (LNOTBK(7)) PRMS(7) = ARRAY(7)

  do I = 1,NPARMS
     if (PRMS(I) .lt.0) PRMS(I) = 0
     5509 continue
  end do

  CFIM_ON = .true.

  !           BULK DENSITY
  CFIM_BD = PRMS(1)

  !           DROUGHT CODE
  CFIM_DC = PRMS(2)

  !           fuel_density(kg/m^3)---rho_surf                 398.0
  CFIM_INPUT(17) = PRMS(3)

  !           surface_area_to_volume_ratio(1/m)---sigma_surf          3092.0
  CFIM_INPUT(16) = PRMS(4)

  !           surface_area_to_volume_ratio(1/m)---sigma_can               5401.0
  CFIM_INPUT(19) = PRMS(5)

  !           canopy_fuel_particle_diameter(m)---diameter             0.00074
  CFIM_INPUT(21) = PRMS(6)

  !           canopy_fuel_particle_length(m)---length             0.10
  CFIM_INPUT(23) = PRMS(7)

  if (LKECHO)write(JOSTND,5510) KEYWRD,(PRMS(I),I = 1,7)
  5510 format(/A8,'   THE CFIM MODEL WILL BE USED FOR FIRE ',&
       &'CALCULATIONS.', /T12,&
       &'BULK DENSITY=',F4.0, '; DROUGHT CODE=',I3/T12,&
       &'FUEL DENS(KG/M3)=',F5.1,&
       &'; SAV SURF=',F5.0,'; SAV CANOPY=',F5.0/T12,&
       &'CAN. PARTICLE DIAM (M)=',F7.5,&
       &'; CAN. PARTICLE LENGTH(M)=',F7.3)

  goto 10

  5600 continue

  !                        OPTION NUMBER 56 -- CFIMCALC

  !     THIS KEYWORD SETS SOME PARAMETERS THAT CONTROL THE MINI-SIMULATION
  !     INSIDE THE CFIM MODEL.

  if (ICALL .eq.2) then
     write(JOSTND,5605) KEYWRD
     5605 format(/A8,'   KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
          &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
     goto 10
  end if

  NPARMS = 3
  !     time_step(s)---tstep                      1
  PRMS(1) = 1.0
  !     iterations---iters                        150
  PRMS(2) = 150.0
  !     x_starting_position(m)---xstart                   8.0
  PRMS(3) = 8.0

  IDT = 1
  if (LNOTBK(1)) PRMS(1) = ARRAY(1)
  if (LNOTBK(2)) PRMS(2) = ARRAY(2)
  if (LNOTBK(3)) PRMS(3) = ARRAY(3)

  !     time_step(s)---tstep                      1
  if (PRMS(1) .lt.1) PRMS(1) = 1
  CFIM_INPUT(7) = PRMS(1)

  !     iterations---iters                        150
  if (PRMS(2) .lt.1) PRMS(2) = 1
  CFIM_INPUT(8) = PRMS(2)

  !     x_starting_position(m)---xstart                   8.0
  if (PRMS(3) .lt.1) PRMS(3) = 1
  CFIM_INPUT(9) = PRMS(3)

  if (LKECHO)write(JOSTND,5610) KEYWRD,(PRMS(I),I = 1,3)
  5610 format(/A8,'   THE INTERNAL CFIM FIRE CALCULATION WILL USE THE'&
       &' FOLLOWING PARAMETERS:', /T13,&
       &'TIME STEP=',F3.0,' ITERATIONS=',F5.0,' START POS=',F5.0)

  goto 10

  !.... Special entry to retrieve keywords.

  entry FMKEY (KEY,PASKEY)
  PASKEY = TABLE(KEY)
  return

end

subroutine FMKEYDMP (IOUT,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
  implicit none

  !  **FMKEYDMP DATE OF LAST REVISION:   08/03/05

  ! Variables
  character(len=10) :: KARD(*)
  character(len=8) :: KEYWRD
  integer :: IOUT,IRECNT,I,NVALS
  real    :: ARRAY(NVALS)

  !  THIS CODE ASSUMES NVALS IS LESS THAN OR EQUAL TO 12.

  write (IOUT,70) IRECNT,KEYWRD,(ARRAY(I),I = 1,NVALS)
  write (IOUT,71) (KARD(I),I = 1,NVALS)
  70 format (/' CARD NUM =',I8,'; KEYWORD FIELD = ''',A8,''''/&
       &'      NUMBERS=',12F13.7)
  71 format ('      CHARS  =',12('''',A10,'''':','))

  return
end

subroutine FMKEYRDR (INUNIT,IOUT,LDEBUG,KEYWRD,LNOTBK,&
     &ARRAY,IRECNT,KODE,KARD,LFLAG,NVALS)
  implicit none

  !  **FMKEYRDR DATE OF LAST REVISION:  10/20/09

  !     KEYWORD CARD READER FOR THE STAND PROGNOSIS SYSTEM

  !     ARGUMENTS:
  !     INUNIT= READER REFERENCE NUMBER
  !     IOUT  = PRINTER REFERENCE NUMBER
  !     LDEBUG= TRUE IF DEBUGGING.
  !     KEYWRD= CHARACTER*8 KEYWORD.
  !     LNOTBK= VALUES ARE TRUE IF THE CORRESPONDING
  !             NUMERIC FIELD IS NOT BLANK, FALSE IF THEY ARE.
  !     ARRAY = VALUES READ FROM THE NUMERIC FIELDS.
  !     IRECNT= RECORD COUNTER.
  !     KODE  = RETURN CODE:
  !             0= NO ERRORS FOUND.
  !             1= FIRST COLUMN OF CARD WAS BLANK OR INVALID CHAR DATA
  !                WAS FOUND.
  !             2= END-OF-FILE WAS FOUND.
  !             LESS THAN ZERO:  PARMS STATEMENT IS ON THE RECORD START-
  !             ING IN FIELD IABS(KODE).
  !     KARD  = A CHARACTER IMAGE OF THE INPUT FIELDS
  !     LFLAG = .TRUE. IF THE HEADING NEEDS TO BE CREATED, FALSE OTHERWISE
  !     NVALS = THE NUMBER OF KEYWORD FIELDS, OFTEN 7, CAN BE DIFFERENT.

  !  DECLARATIONS AND DATA STATEMENTS:

  ! Variables
  integer :: NVALS
  character(len=130) :: RECORD
  character(len=10) :: KARD(NVALS)
  character(len=8) :: KEYWRD
  character(len=8) :: TMP
  logical :: LDEBUG,LNOTBK(NVALS),LCOM,LFLAG
  integer :: IOUT,IRECNT,INUNIT,KODE,NF,IP,I,J,K,ISTLNB
  real    :: ARRAY(NVALS)

  !  READ KEYWORD AND LOCATE BLANK PARAMETER FIELDS.

  LCOM = .false.
  5 continue
  read (INUNIT,'(A)',end = 30) RECORD

  IRECNT = IRECNT + 1
  if (RECORD(1:1).eq.'!') goto 5
  if (LFLAG .and. RECORD.eq.' ') goto 5

  !     CHECK FOR "COMMENT" KEYWORD AND PROCESS.

  TMP = RECORD(1:8)
  do I = 1,8
     call UPCASE (TMP(I:I))
  end do
  if (LFLAG) then
     call GROHED (IOUT)
     write (IOUT,12)
     12 format (/130('-')//T49,'OPTIONS SELECTED BY INPUT'//&
          &130('-')/' KEYWORD    PARAMETERS:'/&
          &'--------   ',119('-'))
     LFLAG = .false.
  end if
  if (RECORD(1:1).eq.'*'.or. RECORD.eq.' ') then
     if (.not.LCOM) then
        write (IOUT,'(/)')
        LCOM = .true.
     end if
     write (IOUT,'(T12,A)') RECORD(1:MAX(1,ISTLNB(RECORD)))
     goto 5
  else
     LCOM = .false.
  end if
  if (TMP.eq.'COMMENT') then
     write (IOUT,'(/A)') TMP
     14 continue
     read (INUNIT,'(A)',end = 30) RECORD
     IRECNT = IRECNT+1
     TMP = RECORD (1:4)
     do I = 1,4
        call UPCASE (TMP(I:I))
     end do
     if (TMP(1:4).eq.'END ') then
        write (IOUT,'(/A4)') TMP(1:4)
        goto 5
     else
        write (IOUT,'(T12,A)') RECORD(1:ISTLNB(RECORD))
        goto 14
     end if
  end if

  !     CHECK FOR THE PRESENCE OF A 'P' SO THAT THE PARMS STATEMENT
  !     MAY BE DETECTED.

  NF = NVALS
  IP = INDEX(RECORD(11:),'P')
  if (IP.gt.0) then

     !        BORROW THE USE OF KEYWRD TO HOLD "PARMS" IN MIXED, THEN UPPER
     !        CASE...

     KEYWRD(1:5) = RECORD(IP+10:IP+14)
     do I = 1,5
        call UPCASE (KEYWRD(I:I))
     end do
     if (KEYWRD(1:5).eq.'PARMS') then
        if (MOD(IP,10).eq.0) then
           NF = IP/10-1
        else
           NF = IP/10
        end if
     end if
  end if

  !     LOAD THE KEYWORD INTO KEYWRD...DECODE THE FIELDS.

  KEYWRD = RECORD(1:8)
  J = 1
  do I = 1,NF
     J = J+10
     KARD(I) = RECORD(J:J+9)
     ARRAY(I) = 0.0

     !     MAKE SURE ALL OF THE CHARACTERS IN KARD ARE NUMBERS...

     do K = 1,10
        if (INDEX(' .+-eE0123456789',KARD(I)(K:K)).eq.0) goto 25
        15 continue
     end do
     read (KARD(I),'(G10.0)',ERR = 25) ARRAY(I)
     25 continue
  end do
  goto 40

  30 continue
  KODE = 2
  return

  40 continue
  call UPKEY(KEYWRD)
  KODE = 0
  do I = 1,NF
     LNOTBK(I) = KARD(I).ne.' '
     50 continue
  end do
  if (NF.lt.NVALS) then
     KODE = -(NF+1)
     do I = NF+1,NVALS
        LNOTBK(I) = .false.
        ARRAY(I) = 0.0
        J = I*10+1
        KARD(I) = RECORD(J:J+9)
        55 continue
     end do
  end if

  if ( LDEBUG ) call FMKEYDMP (IOUT,IRECNT,KEYWRD,ARRAY,KARD,NVALS)
  return
end
