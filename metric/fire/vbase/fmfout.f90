!================================!
!   metric/fire/vbase : fmfout   !
!================================!
! METRIC-FIRE-VBASE $Id$
subroutine FMFOUT(IYR, FLAME, FMD, IFIRE, CFTMP)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMBURN
  !  PURPOSE:
  !     TO PRINT THE THREE OUTPUT FILES WHICH MAY GET PRINTED WHEN A FIRE
  !     OCCURS.  THESE FILES ARE: BURN CONDITIONS, FUEL CONSUMPTION, AND
  !     TREE MORTALITY.

  !  CALL LIST DEFINITIONS:
  !     IFIRE:   FIRE 'TYPE': 0=FUEL TREATMENT FIRE ONLY,
  !                 1=STAND FIRE ONLY, 2=STAND FIRE & TREATMENT FIRE.
  !                -1=NONE (JUST CALLED TO CHECK OPTION PROCESSOR)
  !     IYR:     CURRENT YEAR
  !     FLAME:   FLAME LENGTH (FT)
  !     FMD:     CURRENT FUEL MODEL
  !     CFTMP:   LABEL FOR FIRE TYPE

  !  LOCAL VARIABLE DEFINITIONS:
  !     CLSKIL:  FIRE TREE MORTALITY, BY SPECIES AND 2" DBH SIZE CLASS
  !     LFIRST:  IS THIS THE FIRST LINE OF THE BLOCK TO BE PRINTED?
  !                 (note that we will need to remove this if want totally
  !                  machine readable output)
  !     TOTCLS:  TOTAL NUMBER OF TREES IN THE CLASS, BEFORE MORTALITY

  !  COMMON BLOCK VARIABLES AND PARAMETERS:

  !.... PARAMETER STATEMENTS.

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'METRIC.f90'

  !.... VARIABLE DECLARATIONS.
  !        note: if change MAXCL,must also change the formats 335,345!

  ! Variables
  integer :: MIDBH,MAXCL,MAXCL1,MXSP1

  parameter (MIDBH = 20)
  !      PARAMETER (MAXCL = 14)
  parameter (MAXCL = 7)
  parameter (MAXCL1 = MAXCL + 1)
  parameter (MXSP1 = MAXSP + 1)

  integer :: FMD,IFIRE
  logical :: LFIRST,DEBUG
  real    :: CLSKIL(MXSP1,MAXCL1),TOTCLS(MXSP1,MAXCL1),TOTBAK(MXSP1),TOTVOLK(MXSP1),BurnG12
  real    :: CONV

  character(len=*) :: CFTMP
  character(len=90) :: ALLINE
  integer :: WPOS1,WPOS2

  integer :: IYR,JROUT,K,I,J,L,II,IJ,IK,ICRB,KSP,ICLS
  integer :: DBSKODEM,DBSKODEB,DBSKODEF
  real    :: FLAME,SUML3,SUMG3,BLIVE,TOTG3,PDUFF,PGR3,BDTOT

  call DBCHK (DEBUG,'FMFOUT',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,IYR,FLAME,FMD,IFIRE,CFTMP
  7 format(' ENTERING FMFOUT CYCLE=',I3,' IYR=',I4,' FLAME=',F14.4,&
       &' FMD=',I2,' IFIRE=',I4,' CFTMP=',A)

  !     ROUTINE BEGINS

  !     BURN CONDITIONS REPORT: IFMBRB, IFMBRE ARE THE BEGINNING AND
  !     ENDING YEARS FOR THE REPORT.
  !     RETREIVE THE UNIT NUMBER FOR ANY REPORTS WHICH MAY BE WRITTEN.

  call GETLUN (JROUT)

  if (DEBUG) write(JOSTND,777) IFMBRB,IFMBRE,IDBRN,JROUT
  777 format(' IN FMFOUT, IFMBRB IFMBRE IDBRN JROUT: ',4I5)

  !     WRITE THE BURN CONDITIONS REPORT IF CURRENT CYCLE IS WITHIN THE
  !     TIME WINDOW FOR PRODUCING THE REPORT. WRITE REPORT HEADER
  !     IF REQUESTED.

  if (IFMBRB .lt.0.and. IFIRE .ge.0) then

     !     CALL THE DBS MODULE TO OUTPUT BURN CONDITIONS DATA TO A DATABASE

     if (IFIRE .ne.0) then
        DBSKODEB = 1
        call DBSFMBURN(IYR,NPLT,MOIS(1,1)*100,MOIS(1,2)*100,&
             &MOIS(1,3)*100,MOIS(1,4)*100,MOIS(1,5)*100,MOIS(2,1)*100,&
             &MOIS(2,2)*100,FWIND*MItoKM,INT(FMSLOP*100),FLAME*FTtoM,&
             &SCH*FTtoM,CFTMP,FMOD,FWT,DBSKODEB)
        if (DBSKODEB.eq.0) goto 105
     end if

     IBRPAS = IBRPAS + 1
     if (IBRPAS .eq.1) then
        write (JROUT,39) IDBRN,IDBRN
        write (JROUT,40) IDBRN
        write (JROUT,41) IDBRN
        write (JROUT,43) IDBRN
        write (JROUT,44) IDBRN,NPLT,MGMID
        write (JROUT,40) IDBRN
        write (JROUT,45) IDBRN
        write (JROUT,46) IDBRN
        write (JROUT,47) IDBRN
        write (JROUT,40) IDBRN
        39 format(2(/1X,I5))
        40 format (1X,I5,1X,113('-'))
        41 format (1X,I5,37X,'******  FIRE MODEL VERSION 1.0 ******')
        43 format (1X,I5,26X,'BURN CONDITIONS REPORT --',&
             &' CONDITIONS AT THE TIME OF THE FIRE')
        44 format (1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
        45 format (1X,I5,43X,'MIDFLAME       FLAME  SCORCH',21X,&
             &'FUEL MODELS')
        46 format (1X,I5,7X,'------------ % MOISTURE ------------- WIND',&
             &'  SLOPE  LENGTH HEIGHT',12X,31('-'))
        47 format (1X,I5,1X,'YEAR  1HR 10HR 100HR  3+ DUFF LIVE W LIVE',&
             &' H (KMH)  (%)    (M)    (M)  FIRE TYPE   ',4('MOD %WT '))
     end if

     if (IFIRE .ne.0) then
        if (FMD.eq.0) then
           write (JROUT,100) IDBRN,IYR,&
                &(MOIS(1,1)*100),(MOIS(1,2)*100),&
                &(MOIS(1,3)*100),(MOIS(1,4)*100),&
                &(MOIS(1,5)*100),(MOIS(2,1)*100),(MOIS(2,2)*100),&
                &FWIND*MItoKM, INT(FMSLOP*100), FLAME*FTtoM,&
                &SCH*FTtoM, CFTMP, 0,100
        else
           write (JROUT,100) IDBRN,IYR,&
                &(MOIS(1,1)*100),(MOIS(1,2)*100),&
                &(MOIS(1,3)*100),(MOIS(1,4)*100),&
                &(MOIS(1,5)*100),(MOIS(2,1)*100),(MOIS(2,2)*100),&
                &FWIND*MItoKM, INT(FMSLOP*100.), FLAME*FTtoM, SCH*FTtoM,&
                &CFTMP, (FMOD(I),INT((FWT(I)*100.)+0.5),&
                &I = 1,NFMODS)
           100 format(1X,2I5,5F5.0,F6.0,1X,F6.0,1X,F5.1,I6,2F7.1,2X,A,T89,&
                &8(1X,I3))
        end if
     end if
  end if
  105 continue

  !     FUEL CONSUMPTION REPORT: IFMFLB, IFMFLE ARE THE BEGINNING AND
  !     ENDING YEARS FOR THE REPORT.

  if (DEBUG) write(JOSTND,778) IFMFLB,IFMFLE,IDFUL,JROUT
  778 format(' IFMFLB IFMFLE IDFUL JROUT: ',4I5)

  !     WRITE THE FUEL CONSUMPTION REPORT IF CURRENT CYCLE IS WITHIN THE
  !     TIME WINDOW FOR PRODUCING THE REPORT. WRITE REPORT HEADER
  !     IF REQUESTED.

  if (IFMFLB .lt.0.and. IFIRE .ge.0) then

     SUML3 = 0.0
     SUMG3 = 0.0
     BLIVE = 0.0
     TOTG3 = 0.0
     PDUFF = 0.0

     !       First, sum up the CWD info:
     !       Zero the summation columns (index 1=3 and 4=5)

     do I = 1, MXFLCL
        do J = 1, 2
           CWD(3,I,J,5) = 0.0
        end do
     end do

     !       Calculate summation in Pile categories

     do I = 1, 2
        do J = 1, MXFLCL
           do K = 1, 2
              do L = 1, 4
                 CWD(3,J,K,5) = CWD(3,J,K,5) + CWD(I,J,K,L)
              end do
           end do
        end do
     end do

     do II = 1,3
        IJ = II + 3
        IK = II + 6
        SUML3 = SUML3 + BURNED(3,II)
        SUMG3 = SUMG3 + BURNED(3,IJ) + BURNED(3,IK)
        TOTG3 = TOTG3 + CWD(3,IJ,1,5) + CWD(3,IJ,2,5) +&
             &CWD(3,IK,1,5) + CWD(3,IK,2,5)
     end do
     BurnG12 = BURNED(3,6)+ BURNED(3,7)+ BURNED(3,8)+ BURNED(3,9)
     if (BURNED(3,11)+CWD(3,11,1,5)+CWD(3,11,2,5) .gt.0.0)&
          &PDUFF = 100. * (BURNED(3,11) /&
          &(BURNED(3,11) + CWD(3,11,1,5) + CWD(3,11,2,5)))
     if (SUMG3 + TOTG3.gt.0.0)&
          &PGR3 = 100.0 * (SUMG3 / (SUMG3 + TOTG3))
     SMOKE(1) = SMOKE(1) * P2T
     SMOKE(2) = SMOKE(2) * P2T

     BLIVE = BURNLV(1) + BURNLV(2)

     BDTOT = SUML3 + SUMG3 + BURNED(3,11) + BURNED(3,10)&
          &+ BLIVE + BURNCR

     ICRB = INT(CRBURN*100.0+.5)
     if (IFIRE.eq.0) ICRB = 0
     if (ICRB.lt.0)  ICRB = -1

     !     CALL THE DBS MODULE TO OUTPUT FUEL CONSUMPTION INFO TO A DATABASE

     DBSKODEF = 1
     CONV = TItoTM/ACRtoHA
     call DBSFMFUEL(IYR,NPLT,EXPOSR,&
          &BURNED(3,10)*CONV,BURNED(3,11)*CONV,&
          &SUML3*CONV,SUMG3*CONV,BURNED(3,4)*CONV,BURNED(3,5)*CONV,&
          &BurnG12*CONV,BLIVE*CONV,BURNCR*CONV,&
          &BDTOT*CONV,PDUFF,PGR3,ICRB,SMOKE(1)*CONV,SMOKE(2)*CONV,&
          &DBSKODEF)
     if (DBSKODEF.eq.0) goto 252

     IFLPAS = IFLPAS + 1

     !       WRITE THE HEADER IF REQUESTED.

     if (IFLPAS .eq.1) then
        write (JROUT,200) IDFUL,IDFUL
        write (JROUT,201) IDFUL
        write (JROUT,202) IDFUL
        write (JROUT,203) IDFUL
        write (JROUT,44) IDFUL,NPLT,MGMID
        write (JROUT,201) IDFUL
        write (JROUT,205) IDFUL
        write (JROUT,206) IDFUL
        write (JROUT,207) IDFUL
        write (JROUT,208) IDFUL
        write (JROUT,201) IDFUL
        200 format(2(/1X,I5))
        201 format(1X,I5,1X,104('-'))
        202 format(1X,I5,30X,'******  FIRE MODEL VERSION 1.0 ******')
        203 format(1X,I5,28X,'FUEL CONSUMPTION & PHYSICAL EFFECTS',&
             &' REPORT')
        205 format(1X,I5,6X,'PERCENT',11X,'FUEL CONSUMED ',&
             &'(TONNES/HA)',12X,12X,9X,6X,'%',2X,'     SMOKE')
        206 format(1X,I5,6X,'MINERAL',61('-'),9X,3X,'TREES',&
             &1X,'  PRODUCTION')
        207 format(1X,I5,7X,'SOIL',1X,45X,'HERB&',6X,&
             &' TOTAL',2X,'%CONSUME',1X,' WITH ',&
             &2X,' (TONNES/HA)')
        208 format(1X,I5,1X,'YEAR EXPOSR  LITR ',&
             &' DUFF  0-7.6   7.6+ -15.2 -30.5 ',&
             &'30.5+ SHRUB CRWNS CONS.  DUFF 7.6+',&
             &' CRWNG   <2.5   < 10')
     end if

     ICRB = INT(CRBURN*100.0+.5)
     CONV = TItoTM/ACRtoHA
     if (IFIRE.eq.0) ICRB = 0
     if (ICRB.lt.0) ICRB = -1
     write (JROUT,250) IDFUL,IYR,INT(EXPOSR),&
          &BURNED(3,10)*CONV, BURNED(3,11)*CONV, SUML3*CONV, SUMG3*CONV,&
          &BURNED(3,4)*CONV, BURNED(3,5)*CONV, BurnG12*CONV,&
          &BLIVE*CONV, BURNCR*CONV, BDTOT*CONV,&
          &INT(PDUFF), INT(PGR3), ICRB,&
          &SMOKE(1)*CONV, SMOKE(2)*CONV
     250 format(1X,I5,1X,I4,1X,I4,3X,2(F5.1,1X),2(F6.1,1X),&
          &5(F5.1,1X),F6.1,2X,I3,1X,I3,2X,I4,2X,F6.2,1X,F6.2)

  end if
  252 continue

  !     MORTALITY REPORT: IFMMRB, IFMMRE ARE THE BEGINNING AND
  !     ENDING YEARS FOR THE REPORT.

  if (DEBUG) write(JOSTND,779) IFMMRB,IFMMRE,IDMRT,JROUT
  779 format(' IFMMRB IFMMRE IDMRT JROUT: ',4I5)

  !     WRITE THE MORTALITY REPORT IF CURRENT CYCLE IS WITHIN THE
  !     TIME WINDOW FOR PRODUCING THE REPORT.

  if (IFMMRB .lt.0.and. IFIRE .ge.0) then

     !       FIRST, ACCUMULATE MORTALITY INTO SPECIES & SIZE CLASSES

     do KSP = 1,MXSP1
        TOTBAK(KSP) = 0.
        TOTVOLK(KSP) = 0.
        do ICLS = 1,MAXCL1
           CLSKIL(KSP,ICLS) = 0.
           TOTCLS(KSP,ICLS) = 0.
        end do
     end do

     do I = 1, ITRN
        KSP = ISP(I)
        if (KSP .gt.0) then

           !           ACCUMULATED THE BA AND VOL KILLED.

           TOTBAK(KSP) = TOTBAK(KSP)+(CURKIL(I)*DBH(I)*DBH(I)*.005454154)
           TOTVOLK(KSP) = TOTVOLK(KSP)+(CURKIL(I)*CFV(I))

           !           Find the first lower bound that this dbh is less than
           !           This means that the dbh is in the class just before
           !           If the dbh is less than the first class (ICLS=0) then
           !           this tree will not be included in the output.

           do ICLS = 1, 7
              if (DBH(I) .lt. LOWDBH(ICLS)) goto 302
              301 continue
           end do
           302 continue
           ICLS = ICLS - 1
           if (ICLS .eq.0) goto 310

           CLSKIL(KSP,ICLS) = CLSKIL(KSP,ICLS) + CURKIL(I)
           CLSKIL(KSP,MAXCL1) = CLSKIL(KSP,MAXCL1) + CURKIL(I)
           CLSKIL(MXSP1,ICLS) = CLSKIL(MXSP1,ICLS) + CURKIL(I)
           TOTCLS(KSP,ICLS) = TOTCLS(KSP,ICLS) + CURKIL(I)&
                &+ FMPROB(I)
           TOTCLS(KSP,MAXCL1) = TOTCLS(KSP,MAXCL1) + CURKIL(I)&
                &+ FMPROB(I)
           TOTCLS(MXSP1,ICLS) = TOTCLS(MXSP1,ICLS) + CURKIL(I)&
                &+ FMPROB(I)
           TOTCLS(MXSP1,MAXCL1) = TOTCLS(MXSP1,MAXCL1) +&
                &CURKIL(I) + FMPROB(I)
        end if
        310 continue
     end do

     do KSP = 1,MAXSP
        TOTBAK(MXSP1) = TOTBAK(MXSP1)+TOTBAK(KSP)
        TOTVOLK(MXSP1) = TOTVOLK(MXSP1)+TOTVOLK(KSP)
     end do

     !     CALL THE DBS MODULE TO OUTPUT THE MORTALITY REPORT TO A DATABASE

     DBSKODEM = 1
     call DBSFMMORT(IYR,CLSKIL/ACRtoHA,TOTCLS/ACRtoHA,&
          &TOTBAK*FT2pACRtoM2pHA,TOTVOLK*FT3pACRtoM3pHA,DBSKODEM)
     if (DBSKODEM.eq.0) return

     IMRPAS = IMRPAS + 1

     !       WRITE HEADER IF REQUESTED.

     if (IMRPAS .eq.1) then
        write (JROUT,315) IDMRT, IDMRT
        write (JROUT,316) IDMRT
        write (JROUT,317) IDMRT
        write (JROUT,318) IDMRT
        write (JROUT,44) IDMRT,NPLT,MGMID
        write (JROUT,316) IDMRT

        select case (VARACD)

        case ('CS','LS','NE','SN','ON')
           write (JROUT,420) IDMRT

        case default
           write (JROUT,320) IDMRT

        end select

        write (JROUT,321) IDMRT,&
             &(LOWDBH(I)*INtoCM,LOWDBH(I+1)*INtoCM,I = 1,MAXCL-1),&
             &LOWDBH(MAXCL)*INtoCM
        write (JROUT,316) IDMRT

        315 format (2(/1X,I5))
        316 format (1X,I5,1X,117('-'))
        317 format(1X,I5,33X,'******  FIRE MODEL VERSION 1.0 ******')
        318 format (1X,I5,43X,'MORTALITY REPORT '&
             &'(BASED ON STOCKABLE AREA)')
        320 format (1X,I5,21X,&
             &'   NUMBER KILLED / NUMBER BEFORE ',&
             &'(BY DIAMETER CLASS IN CM)  ',T111,'BASAL    TOTAL')
        420 format (1X,I5,21X,&
             &'NUMBER KILLED / NUMBER BEFORE ',&
             &'(BY DIAMETER CLASS IN CM) ',T111,'BASAL    MERCH')
        321 format (1X,I5,1X,'YEAR  SP  ',&
             &6(1X,F5.1,'-',F5.1,1X),4X,'>=',F5.1,T110,'  AREA     CU M')
     end if

     LFIRST = .true.
     do KSP = 1,MXSP1
        ALLINE = ' '
        WPOS1 = 2
        WPOS2 = 6
        do ICLS = 1,MAXCL
           if (TOTCLS(KSP,ICLS) .gt.0.0) then
              write (ALLINE(WPOS1:WPOS2),'(I5)')&
                   &INT(CLSKIL(KSP,ICLS)/ACRtoHA)
              WPOS1 = WPOS2 + 1
              WPOS2 = WPOS1
              write (ALLINE(WPOS1:WPOS2),'("/")')
              WPOS1 = WPOS1 + 1
              WPOS2 = WPOS2 + 5
              write (ALLINE(WPOS1:WPOS2),'(I5)')&
                   &INT(TOTCLS(KSP,ICLS)/ACRtoHA)
              WPOS1 = WPOS1 + 7
              WPOS2 = WPOS2 + 7
           else
              CLSKIL(KSP,ICLS) = -1.0
              WPOS1 = WPOS1 + 13
              WPOS2 = WPOS2 + 13
           end if
           335 continue
        end do

        !         PRINT ONLY THOSE SPECIES WHICH ARE PRESENT IN THE STAND
        !         (EVEN IF THEY HAD NO MORTALITY).

        if (TOTCLS(KSP,MAXCL1) .gt.0.0.and. KSP .lt. MXSP1) then

           if (LFIRST) then
              write (JROUT,340) IDMRT,IYR,JSP(KSP),&
                   &ALLINE,TOTBAK(KSP)*FT2pACRtoM2pHA,&
                   &INT(TOTVOLK(KSP)*FT3pACRtoM3pHA)

              340 format(1X,I5,1X,I4,2X,A3,1X,A90,T108,F8.2,I9)
              LFIRST = .false.
           else
              write (JROUT,345) IDMRT,JSP(KSP),ALLINE,&
                   &TOTBAK(KSP)*FT2pACRtoM2pHA,&
                   &INT(TOTVOLK(KSP)*FT3pACRtoM3pHA)
              345 format(1X,I5,7X,A3,1X,A90,T108,F8.2,I9)
           end if
        else if (TOTCLS(KSP,MAXCL1).gt.0.0.and. KSP.eq.MXSP1) then
           if (LFIRST) then
              write (JROUT,340) IDMRT,IYR,'ALL',ALLINE,&
                   &TOTBAK(KSP)*FT2pACRtoM2pHA,&
                   &INT(TOTVOLK(KSP)*FT3pACRtoM3pHA)
              LFIRST = .false.
           else
              write (JROUT,345) IDMRT,'ALL',ALLINE,&
                   &TOTBAK(KSP)*FT2pACRtoM2pHA,&
                   &INT(TOTVOLK(KSP)*FT3pACRtoM3pHA)
           end if
        end if
     end do
  end if

  return
end
