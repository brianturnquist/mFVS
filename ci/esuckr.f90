!=================!
!   ci : esuckr   !
!=================!
! CI $Id$
subroutine ESUCKR
  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESPARM.f90'
  include 'ESHOOT.f90'
  include 'ESCOMN.f90'
  include 'ESCOM2.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'
  include 'ESTREE.f90'
  include 'STDSTK.f90'
  include 'VARCOM.f90'

  !     CREATE STUMP & ROOT SPROUTS FROM TREES CUT AT BEGINNING OF CYCLE.
  !     ASSUMPTION: THE TREE LIST HAS BEEN COMPRESSED TO ABOUT 1/2 THE
  !     VALUE OF MAXTRE. COMPRS IS CALLED IN ESNUTR.

  external ESRANN
  ! Variables
  logical :: DEBUG
  character(len=2) :: CLABEL
  integer*4 :: MDBH,INUMB,MSP
  integer :: ISHAG
  integer :: MYACTS(1),JJ,IDATE,ITODO,IT
  integer :: NTODO,KDT,NP,IACTK,IDT,IGRP
  integer :: IULIM,IG,IGSP
  integer :: NUMSPR,I,ICL,ISSP,IPLOT,ISPSTO,II,MXRR,J,ITRGT,MXTODO
  real    :: RANDEV,DSTMP,RSHAG,SPA
  real    :: PRMS(6),SPRMLT(NSPSPE,100),HTMSPR(NSPSPE,100),SMULT,HMULT
  real    :: DMIN(NSPSPE,100),DMAX(NSPSPE,100)
  real    :: COUNTR(NSPSPE),TPASUM(NSPSPE),HTAVE(NSPSPE)
  real    :: CW,CRDUM,AX,BX,BACHLO,PREM,TPATOT,COUNTR,TPASUM,HTAVE

  !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

  !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
  !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
  !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
  !     4 = GRAND FIR (GF)                   ABIES GRANDIS
  !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
  !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
  !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
  !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
  !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
  !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
  !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
  !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
  !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
  !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
  !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
  !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
  !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
  !    18 = OTHER SOFTWOODS (OS)
  !    19 = OTHER HARDWOODS (OH)

  !  DATA STATEMENTS:

  data MDBH/10000000/,MSP/10000/,NUMSPR/2/
  data MYACTS/450/

  call DBCHK (DEBUG,'ESUCKR',6,ICYC)
  if (ITRNRM.lt.1) go to 900
  do I = 1,NSPSPE
     COUNTR(I) = 0.0
     TPASUM(I) = 0.0
     HTAVE(I) = 0.0
     10 continue
  end do
  TPATOT = 0.0

  !  PROCESS SPROUT KEYWORD OPTIONS.
  !  INITIALIZE VARIABLES

  do I = 1,NSPSPE
     do J = 1,100
        SPRMLT(I,J) = 1.
        HTMSPR(I,J) = 1.
        DMIN(I,J) = 0.
        DMAX(I,J) = 0.
     end do
  end do
  HMULT = 1.
  SMULT = 1.

  call OPFIND (1,MYACTS(1),NTODO)
  if (DEBUG) write (JOSTND,5) NTODO,ITRNRM
  5 format (' IN ESUCKR: OPTS NTODO,ITRNRM=',I2,2X,I2)
  if (NTODO.le.0)go to 60

  do IT = 1,NTODO
     call OPGET (IT,5,IDATE,IACTK,NP,PRMS)
     IDT = IDATE
     KDT = IY(ICYC)
     J = IFIX(PRMS(1))

     !  SPECIES GROUP

     if (J .lt.0) then
        IGRP = -J
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           IGSP = ISPGRP(IGRP,IG)
           do JJ = 1,NSPSPE
              if (IGSP.eq.ISPSPE(JJ)) then
                 SPRMLT(JJ,IT) = PRMS(2)
                 HTMSPR(JJ,IT) = PRMS(3)
                 DMIN(JJ,IT) = PRMS(4)
                 DMAX(JJ,IT) = PRMS(5)
              end if
           end do
        end do

        !  ALL SPROUTABLE SPECIES

     else if (J .eq.0) then
        do JJ = 1,NSPSPE
           SPRMLT(JJ,IT) = PRMS(2)
           HTMSPR(JJ,IT) = PRMS(3)
           DMIN(JJ,IT) = PRMS(4)
           DMAX(JJ,IT) = PRMS(5)
        end do

        !  SINGLE SPECIES

     else
        do JJ = 1,NSPSPE
           if (J.eq.ISPSPE(JJ)) then
              SPRMLT(JJ,IT) = PRMS(2)
              HTMSPR(JJ,IT) = PRMS(3)
              DMIN(JJ,IT) = PRMS(4)
              DMAX(JJ,IT) = PRMS(5)
           end if
        end do
     end if
     call OPDONE(IT,IDT)
     50 continue
  end do

  60 continue
  do I = 1,ITRNRM
     INUMB = ISHOOT(I)
     ICL = INUMB/MDBH
     ISSP = INUMB/MSP - ICL*1000
     IPLOT = MOD(INUMB,MSP)
     PREM = PRBREM(I)
     ISHAG = JSHAGE(I)
     DSTMP = DSTUMP(I)

     !  IF AN INSIGNIFICANT AMOUNT OF A SPROUTING SPECIES WAS CUT,
     !  DO NOT GENERATE SPROUTS.  GED 4-8-97.

     if (PREM.lt.0.001)go to 500

     ISPSTO = ISSP
     do II = 1,NSPSPE
        if (ISSP.ne.II) go to 200
        ISSP = ISPSPE(II)
        go to 201
        200 continue
     end do
     write (JOREGT,1000) ISSP,ITRNRM
     1000 format(' IN ESPROT: NO SPECIES MATCH WHEN DECODING ISHOOT(',&
          &'ITRNRM). ISSP=',I5,'; ITRNRM=',I5)
     go to 900
     201 continue
     if (DEBUG) write(JOSTND,300) I,IPLOT,ISSP,ICL,PREM
     300 format(' IN ESUCKR: I=',I5,'; IPLOT=',I5,'; ISSP=',I5,&
          &'; ICL=',I5,'; PREM=',F6.2)

     !     MAKE SURE THAT THERE IS ROOM IN THE TREE LIST FOR SPROUTS.
     !     THE CALL TO RDESCP INSURES THAT THE SPACE REQUIRED IS BELOW THAT
     !     NEEDED BY THE ROOT DISEASE MODEL.  RDESCP RETURNS THE MAXIMUM
     !     NUMBER OF TREES THAT ROOT DISEASE CAN HANDLE.  IF RROT IS NOT
     !     BEING RUN, MXRR IS RETURNED AS MAXTRE.

     call RDESCP (MAXTRE, MXRR)

     !     INSERT CODE TO PREDICT NUMBER OF SPROUTS.  ICL IS DIAMETER CLASS
     !     IN THE ARRAY DBHMID.

     !     ASPEN SPROUTS PER ACRE (SPA) BASED ON POLYNOMIAL FORMULA FROM
     !     CROUCH DATA SET, MODIFIED BY PROPORTION OF ASPEN BA CUT.

     if (DEBUG)write(JOSTND,*)' I,ISHAG= ',I,ISHAG
     if (ISSP.eq.13) then
        RSHAG = FLOAT(ISHAG)
        SPA = 40100.45 - 3574.02*RSHAG**2.0 + 554.02*RSHAG**3.0&
             &- 3.5208*RSHAG**5.0 + 0.011797*RSHAG**7.0
        if (DEBUG)write(JOSTND,*)' AFTER CALC SPA= ',SPA
        if (SPA .lt.2608.)SPA = 2608.
        if (SPA .gt.30125.)SPA = 30125.
        SPA = SPA * ASBAR/198.
        PREM = (PREM/(ASTPAR*2.))*SPA
        if (DEBUG)write(JOSTND,*)' I,ISHAG,SPA,ASBAR,ASTPAR,PREM= ',&
             &I,ISHAG,SPA,ASBAR,ASTPAR,PREM
     end if

     !     SET SPROUT KEYWORD MULTIPLIERS BASED ON DBH RANGE

     SMULT = 1.
     HMULT = 1.
     do IT = 1,NTODO
        if ((DSTMP.ge.DMIN(ISPSTO,IT)).and.&
             &(DSTMP.lt.DMAX(ISPSTO,IT))) then
           SMULT = SPRMLT(ISPSTO,IT)
           HMULT = HTMSPR(ISPSTO,IT)
        end if
        450 continue
     end do

     !  IF SPROUT MULTIPLIER (SMULT) IS ZERO DO NOT ADD RECORDS TO TREE LIST

     if (SMULT.le.0.)goto 500

     !  PACIFIC YEW (12) SPROUT COUNT IS CUT IN HALF

     if (ISSP.eq.12) then
        NUMSPR = 1
     else
        NUMSPR = 2
     end if

     do J = 1,NUMSPR
        if (ITRN.lt.MXRR) go to 100
        ITRGT = ITRNRM-I
        MXTODO = FLOAT(MXRR)* 0.70
        if (MXTODO.gt.ITRGT) ITRGT = MXTODO
        call ESCPRS (ITRGT,DEBUG)
        100 continue
        ITRN = ITRN+1
        IMC(ITRN) = 2
        ISP(ITRN) = ISSP
        ITRE(ITRN) = IPLOT
        CFV(ITRN) = 0.0
        MCFV(ITRN) = 0.0
        SCFV(ITRN) = 0.0

        MCFV(ITRN) = 0.
        SCFV(ITRN) = 0.
        CULL(ITRN) = 0.
        DECAYCD(ITRN) = 0
        WDLDSTEM(ITRN) = 0
        ABVGRD_BIO(ITRN) = 0.
        ABVGRD_CARB(ITRN) = 0.
        MERCH_BIO(ITRN) = 0.
        MERCH_CARB(ITRN) = 0.
        CUBSAW_BIO(ITRN) = 0.
        CUBSAW_CARB(ITRN) = 0.
        FOLI_BIO(ITRN) = 0.
        FOLI_CARB(ITRN) = 0.
        CARB_FRAC(ITRN) = 0.

        ITRUNC(ITRN) = 0
        NORMHT(ITRN) = 0

        !     INSERT CODE TO CALCULATE DBH AND HEIGHT OF SPROUTS

        PROB(ITRN) = PREM*SMULT

        select case (ISSP)

        case(13,17,19)
           HT(ITRN) = ((1.0+SITEAR(ISSP)/5.)/10.)*ISHAG*HMULT

        case default
           HT(ITRN) = (0.5 + 0.5*ISHAG)*HMULT

        end select

        162 RANDEV = BACHLO(0.,0.5,ESRANN)
        if (RANDEV.lt.-1.0.or. RANDEV.gt.1.0) go to 162
        RANDEV = RANDEV*HT(ITRN)/5.5
        HT(ITRN) = HT(ITRN)+RANDEV

        if (HT(ITRN) .gt.4.5) then
           BX = HT2(ISSP)
           if (IABFLG(ISSP).eq.1) then
              AX = HT1(ISSP)
           else
              AX = AA(ISSP)
           end if
           DBH(ITRN) = (BX/(ALOG(HT(ITRN)-4.5)-AX))-1.0
           if (DBH(ITRN).lt.0.1) DBH(ITRN) = 0.1
        else
           DBH(ITRN) = 0.1
        end if

        ICR(ITRN) = 70

        !  CALCULATE A CROWN WIDTH FOR SPROUTS

        CRDUM = 1.
        call CWCALC(ISSP,PROB(ITRN),DBH(ITRN),HT(ITRN),CRDUM,&
             &ICR(ITRN),CW,0,JOSTND)
        CRWDTH(ITRN) = CW

        DG(ITRN) = 0.0
        HTG(ITRN) = 0.0
        PCT(ITRN) = 0.0
        OLDPCT(ITRN) = 0.0
        WK1(ITRN) = 0.0
        WK2(ITRN) = 0.
        WK4(ITRN) = 0.
        BFV(ITRN) = 0.0
        IESTAT(ITRN) = 0
        PTBALT(ITRN) = 0.
        IDTREE(ITRN) = 10000000+ICYC*10000+ITRN
        call MISPUTZ(ITRN,0)

        ABIRTH(ITRN) = ISHAG
        DEFECT(ITRN) = 0.
        ISPECL(ITRN) = 0
        OLDRN(ITRN) = 0.
        PTOCFV(ITRN) = 0.
        PMRCFV(ITRN) = 0.
        PSCFV(ITRN) = 0.
        PMRBFV(ITRN) = 0.
        NCFDEF(ITRN) = 0
        NBFDEF(ITRN) = 0
        PDBH(ITRN) = 0.
        PHT(ITRN) = 0.
        ZRAND(ITRN) = -999.

        COUNTR(ISPSTO) = COUNTR(ISPSTO) +1.0
        TPASUM(ISPSTO) = TPASUM(ISPSTO) +PREM*SMULT
        TPATOT = TPATOT+PREM*SMULT
        HTAVE(ISPSTO) = HTAVE(ISPSTO) +HT(ITRN)
        499 continue
     end do
     500 continue
  end do
  if (IPRINT.ne.0) then
     write(JOREGT,1100) NPLT,MGMID,IY(ICYC+1)-1
     1100 format(' ',54('-'),/,' REGENERATION FROM STUMP & ROOT SPROUTS',&
          &//,' STAND ID: ',A26,'  MANAGEMENT CODE: ',A4,'  YEAR: ',I5,//,&
          &T15,'TREES  AVERAGE',/,T6,'SPECIES  /ACRE  HEIGHT',/,T6,&
          &'-------  -----  -------')
     do I = 1,NSPSPE
        CLABEL = NSP(ISPSPE(I),1)
        HTAVE(I) = HTAVE(I)/(COUNTR(I)+.00001)
        if (TPASUM(I).gt.0.0)&
             &write(JOREGT,1200) CLABEL,TPASUM(I),HTAVE(I)
        1200 format(T9,A2,T14,F6.0,T22,F6.1)
        700 continue
     end do
     write(JOREGT,1300) TPATOT
     1300 format(T15,'-----',/,T14,F6.0,/,54('-') )
  end if
  900 continue
  ITRNRM = 0
  return
end
