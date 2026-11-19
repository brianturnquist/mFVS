!=============================!
!   metric/fire/base : fmin   !
!=============================!
subroutine FMIN (ICALL,NSP,LKECHO)
  use FMDWDREQ_MOD, only: FMDWDREQ
  use FMDWCVREQ_MOD, only: FMDWCVREQ
  use FMFUELREQ_MOD, only: FMFUELREQ
  use FMINKEYS_MOD, only: FMKSALVSP, FMKMOISTURE, FMKPOTFIRE, FMKSNAGFALL,&
       &FMKSNAGBRK, FMKSNAGDCAY, FMKSNAGOUT, FMKFUELPOOL, FMKPILEBURN,&
       &FMKFUELMULT, FMKDROUGHT, FMKSNAGPSFT, FMKFUELMODL, FMKFUELFOTO,&
       &FMKFMODLIST, FMKFUELTRET, &
       &FMKFUELREPT, FMKMORTREPT, FMKSOILHEAT, FMKCARBREPT, &
       &FMKCARBCUT, FMKCANFPROF, FMKFUELINIT, FMKSNAGINIT, &
       &FMKPOTFWIND, FMKPOTFTEMP, FMKCARBCALC, FMKFIRECALC, &
       &FMKFUELSOFT, FMKFUELDCAY, FMKPOTFMOIS, FMKDUFFPROD
  implicit none

  ! Exit taken by a shared keyword handler (0: next keyword)
  integer :: IKEXIT

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

  call FMKSALVSP(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,PRMS,JSP,NSP,ILEN,LKECHO,IKEXIT)
  if (IKEXIT .lt. 0) return
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

  call FMKMOISTURE(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,II,PRMS,LKECHO,IKEXIT)
  if (IKEXIT .lt. 0) return
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

  call FMKPOTFIRE(ICALL,KEYWRD,LKECHO)
  goto 10

  900 continue
  !                        OPTION NUMBER 9 -- SNAGFALL

  call FMKSNAGFALL(JSP,NSP,KEYWRD,ARRAY,KARD,LNOTBK,LKECHO)
  goto 10
  1000 continue
  !                        OPTION NUMBER 10 -- SNAGBRK

  call FMKSNAGBRK(JSP,NSP,KEYWRD,ARRAY,KARD,YRS50,YRS30,LNOTBK,LKECHO)
  goto 10

  1100 continue
  !                        OPTION NUMBER 11 -- SNAGDCAY

  call FMKSNAGDCAY(JSP,NSP,KEYWRD,ARRAY,KARD,LNOTBK,LKECHO)
  goto 10
  1200 continue
  !                        OPTION NUMBER 12 --  SNAGOUT

  call FMKSNAGOUT(ICALL,KEYWRD,IDT,PRMS,LNOTBK,ARRAY,LKECHO,MYACT,NPARMS,KODE)
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

  call FMFUELREQ(ICALL,KEYWRD,LKECHO)

  goto 10
  1600 continue
  !                        OPTION NUMBER 16 -- FUELDCAY

  call FMKFUELDCAY(KEYWRD,LNOTBK,LKECHO,J,ARRAY,IDEC,ID,I)
  goto 10

  1700 continue
  !                        OPTION NUMBER 17 -- DUFFPROD

  call FMKDUFFPROD(KEYWRD,LNOTBK,LKECHO,ARRAY,IDEC,ID,I)
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

  call FMKFUELPOOL(JSP,NSP,KEYWRD,ARRAY,KARD,LNOTBK,IDEC,LKECHO)
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

  call FMKFUELINIT(KEYWRD,LNOTBK,LKECHO,NPARMS,IDT,ICALL,PRMS,ARRAY,KODE,MYACT,I)
  goto 10

  2200 continue
  !                        OPTION NUMBER 22 -- SNAGINIT

  call FMKSNAGINIT(NSP,KEYWRD,KARD,LNOTBK,LKECHO,NPARMS,IDT,ICALL,PRMS,ARRAY,KODE,MYACT,&
       &JSP,I)
  goto 10

  2300 continue
  !                        OPTION NUMBER 23 -- PILEBURN (formerly FUELBURN)

  call FMKPILEBURN(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,PRMS,LKECHO,I,IKEXIT)
  if (IKEXIT .lt. 0) return
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

  call FMKFUELTRET(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,PRMS,LKECHO,IKEXIT)
  if (IKEXIT .lt. 0) return
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

  call FMKFUELREPT(KEYWRD,LKECHO)
  goto 10

  2800 continue
  !                        OPTION NUMBER 28 -- MORTREPT

  call FMKMORTREPT(KEYWRD,LKECHO)
  goto 10

  2900 continue
  !                        OPTION NUMBER 29 -- FUELMULT

  call FMKFUELMULT(IDEC,LNOTBK,DKMULT,ARRAY,I,LKECHO,KEYWRD)
  goto 10
  3000 continue
  !                        OPTION NUMBER 30 -- POTFMOIS

  call FMKPOTFMOIS(KEYWRD,LNOTBK,LKECHO,ARRAY,I,IFIRE,IARRY)
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

  call FMKDROUGHT(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,NPARMS,PRMS,LKECHO,IKEXIT)
  if (IKEXIT .lt. 0) return
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

  call FMKPOTFWIND(KEYWRD,LNOTBK,LKECHO,ARRAY)
  goto 10

  3600 continue
  !                        OPTION NUMBER 36 -- POTFTEMP

  call FMKPOTFTEMP(KEYWRD,LNOTBK,LKECHO,ARRAY)
  goto 10

  3700 continue
  !                        OPTION NUMBER 37 -- SNAGPSFT

  call FMKSNAGPSFT(JSP,NSP,KEYWRD,ARRAY,KARD,LNOTBK,LKECHO)
  goto 10

  3800 continue

  !                        OPTION NUMBER 38 -- FUELMODL

  call FMKFUELMODL(MYACT,NPARMS,IDT,LNOTBK,ARRAY,IPRMPT,KEYWRD,KARD,NVALS,&
       &KODE,IRTNCD,I,PRMS,LKECHO,X,IKEXIT)
  if (IKEXIT .lt. 0) return
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

  call FMKSOILHEAT(KEYWRD,LNOTBK,LKECHO,ARRAY)
  goto 10

  4400 continue
  !                        OPTION NUMBER 44 -- CARBREPT

  call FMKCARBREPT(KEYWRD,LKECHO,ICALL)
  goto 10

  4500 continue
  !                        OPTION NUMBER 45 -- CARBCUT

  call FMKCARBCUT(KEYWRD,LKECHO,ICALL)
  goto 10

  4600 continue
  !                        OPTION NUMBER 46 -- CARBCALC

  call FMKCARBCALC(KEYWRD,LNOTBK,LKECHO,ICALL,ARRAY)
  goto 10

  4700 continue
  !                        OPTION NUMBER 47 -- CANFPROF

  call FMKCANFPROF(KEYWRD,LKECHO,ICALL,ICANPR)
  goto 10

  4800 continue
  !                        OPTION NUMBER 48 -- FUELFOTO

  call FMKFUELFOTO(ICALL,KEYWRD,NPARMS,PRMS,IDT,LNOTBK,ARRAY,REF,PHOTOREF,J,K,&
       &CHARCODE,LKECHO,MYACT,KODE)
  goto 10
  4900 continue
  !                        OPTION NUMBER 49 -- FIRECALC

  call FMKFIRECALC(KEYWRD,KARD,LNOTBK,LKECHO,NPARMS,IDT,ICALL,NVALS,IRTNCD,PRMS,ARRAY,KODE,&
       &IPRMPT,MYACT,IKEXIT)
  if (IKEXIT .lt. 0) return
  goto 10

  5000 continue

  !                        OPTION NUMBER 50 -- FMODLIST

  call FMKFMODLIST(ICALL,KEYWRD,MYACT,IDT,LNOTBK,ARRAY,IPRMPT,KARD,NVALS,KODE,&
       &IRTNCD,PRMS,NPARMS,LKECHO,IKEXIT)
  if (IKEXIT .lt. 0) return
  goto 10
  5100 continue
  !                        OPTION NUMBER 51 -- DWDVLOUT

  call FMDWDREQ(ICALL,KEYWRD,LKECHO)

  goto 10
  5200 continue
  !                        OPTION NUMBER 52 -- DWDCVOUT

  call FMDWCVREQ(ICALL,KEYWRD,LKECHO)

  goto 10

  5300 continue
  !                        OPTION NUMBER 53 -- FUELSOFT

  call FMKFUELSOFT(KEYWRD,LNOTBK,LKECHO,NPARMS,IDT,ICALL,PRMS,ARRAY,KODE,MYACT,I)
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
  5610 format(/A8,'   THE INTERNAL CFIM FIRE CALCULATION WILL USE THE',&
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
