!========================!
!   fire/base : fmdout   !
!========================!
! FIRE-BASE $Id$
subroutine FMDOUT (IYR)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMMAIN
  !  PURPOSE:
  !     PRINT THE FUELS AND DEBRIS

  !  CALL LIST DEFINITIONS:
  !     IYR:     CURRENT YEAR

  !  LOCAL VARIABLE DEFINITIONS:

  !  COMMON BLOCK VARIABLES AND PARAMETERS:

  !.... PARAMETER STATEMENTS.

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  !.... VARIABLE DECLARATIONS.
  ! Variables
  real    :: TOTSNG(2),TOTLIV(2),TONREM
  logical :: DEBUG,LPRINT,LPRINT2,LPRINT3,LMERCH
  integer :: IYR,JROUT,I,J,K,L,ISZ,JSZ,KSZ,KSP,IDC,ITM,ITON,JS,II,DBSKODE,CRWNRTO
  real    :: TOTDUF,TOTLIT,SNVIS,SNVIH,ACRES,TOTFOL,TOTCON,D,H,X,VT,TOTSTD,TOTFUL,TOTSUR,SMALL2,&
       &LARGE2,LARGE12,CWDDEN(2,4),V1(16),V2(14)
  LPRINT = .false.
  LPRINT2 = .false.
  LPRINT3 = .false.

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMDOUT',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMDOUT CYCLE = ',I2)

  !     RETREIVE THE UNIT NUMBER TO BE USED FOR THE ALL FUELS AND CARBON REPORTS.

  call GETLUN (JROUT)

  if (DEBUG) write(JOSTND,40) IFLALB,IFLALE,IDFLAL,&
       &JROUT,NSNAG
  40 format(' FMDOUT: IFLALB=',I5,' IFLALE=',I5,&
       &' IDFLAL=',I5,' JROUT=',I3,' NSNAG=',I5)

  !     FIRST CHECK TO SEE IF WE NEED TO DO THIS FOR FUELS REPORTING
  !     IF THIS YEAR IS WITHIN THE REPORTING PERIOD, SET THE
  !     PRINT FLAG TO TRUE (DEFAULT IS FALSE)

  LPRINT = .true.
  if (IYR .eq.0.and. IYR.eq. IFLALE) goto 45

  if (IYR .lt. IFLALB .or. IYR .gt. IFLALE) LPRINT = .false.

  45 continue

  !     DO THIS AGAIN FOR THE DOWN WOOD VOLUME AND COVER REPORTS

  LPRINT2 = .true.
  if (IYR .eq.0.and. IYR.eq. IDWRPE) goto 46

  if (IYR .lt. IDWRPB .or. IYR .gt. IDWRPE) LPRINT2 = .false.

  46 continue

  LPRINT3 = .true.
  if (IYR .eq.0.and. IYR.eq. IDWCVE) goto 47

  if (IYR .lt. IDWCVB .or. IYR .gt. IDWCVE) LPRINT3 = .false.

  47 continue

  !     Zero the summation columns (index 1=3 and 4=5)

  do I = 1, MXFLCL
     do J = 1, 2
        CWD(3,I,J,5) = 0.0
     end do
  end do

  !     Calculate summation in Pile categories

  do I = 1, 2
     do J = 1, MXFLCL
        do K = 1, 2
           do L = 1, 4
              CWD(3,J,K,5) = CWD(3,J,K,5) + CWD(I,J,K,L)
           end do
        end do
     end do
  end do

  !     ACCUMULATE SURFACE FUELS FOR PRINTING

  SMALL2 = 0.0
  LARGE2 = 0.0
  LARGE12 = 0.0
  do ISZ = 1,3
     JSZ = ISZ + 3
     KSZ = ISZ + 6
     SMALL2 = SMALL2 + CWD(3,ISZ,1,5) + CWD(3,ISZ,2,5)
     LARGE2 = LARGE2 + CWD(3,JSZ,1,5) + CWD(3,JSZ,2,5)&
          &+ CWD(3,KSZ,1,5) + CWD(3,KSZ,2,5)
  end do
  LARGE12 = CWD(3,6,1,5)+CWD(3,6,2,5)+CWD(3,7,1,5)+CWD(3,7,2,5)&
       &+CWD(3,8,1,5)+CWD(3,8,2,5)+CWD(3,9,1,5)+CWD(3,9,2,5)

  TOTDUF = CWD(3,11,1,5) + CWD(3,11,2,5)
  TOTLIT = CWD(3,10,1,5) + CWD(3,10,2,5)
  TOTSUR = FLIVE(1) + FLIVE(2) + SMALL2 + LARGE2 + TOTDUF + TOTLIT

  !     ACCUMULATE SNAG INFORMATION

  TOTSNG(1) = 0.0
  TOTSNG(2) = 0.0
  do I = 1, NSNAG
     if ((DENIH(I)+DENIS(I)) .gt.0.0) then

        SNVIS = 0.0
        SNVIH = 0.0

        if (DENIH(I) .gt.0.0) then
           call FMSVOL (I, HTIH(I), SNVIH, DEBUG, JOSTND)
           SNVIH = SNVIH*DENIH(I)
        end if

        if (DENIS(I) .gt.0.0) then
           call FMSVOL (I, HTIS(I), SNVIS, DEBUG, JOSTND)
           SNVIS = SNVIS*DENIS(I)
        end if

        KSP = SPS(I)

        if (DBHS(I) .le.3.0) then
           TOTSNG(1) = TOTSNG(1) + (SNVIS+SNVIH) * V2T(KSP)
        else
           TOTSNG(2) = TOTSNG(2) + (SNVIS+SNVIH) * V2T(KSP)
        end if

        if (DEBUG) write(JOSTND,50) I,DENIH(I),HTIH(I),SNVIH,&
             &DENIS(I),HTIS(I),SNVIS,KSP,V2T(KSP),TOTSNG(1),TOTSNG(2)
        50 format(' FMDOUT: I=',I5,' DENIH,HTIH,SNVIH=',3F10.3,&
             &' DENIS,HTIS,SNVIS=',3F10.3,' KSP=',I3/T18,'V2T=',F6.3,&
             &' TOTSNG(1&2)=',2F10.4)

     end if
  end do

  !     ADD SNAG CROWNS TO THE APPROPRIATE SIZE CLASS
  !     INCLUDE LITTER IN THE 0-3 CLASS, JUST SO THAT IT IS REPORTED

  do ISZ = 0,3
     do IDC = 1,4
        do ITM = 1,TFMAX
           TOTSNG(1) = TOTSNG(1) + P2T *&
                &(CWD2B(IDC,ISZ,ITM) + CWD2B2(IDC,ISZ,ITM))
           if (ISZ .gt.0.and. ISZ .lt.3)&
                &TOTSNG(2) = TOTSNG(2) + P2T *&
                &(CWD2B(IDC,ISZ+3,ITM) + CWD2B2(IDC,ISZ+3,ITM))

           if (DEBUG)write(JOSTND,55)ISZ,IDC,ITM,CWD2B(IDC,ISZ,ITM),&
                &CWD2B2(IDC,ISZ,ITM),TOTSNG(1),TOTSNG(2)
           55 format(' FMDOUT: ISZ,IDC,ITM=',3I3,' CWD2B=',F10.2,&
                &' CWD2B2=',F10.2,' TOTSNG(1&2)=',2F10.4)

        end do
     end do
  end do

  !     GET AREA DISTRIBUTION FOR LANDSCAPE REPORT

  if (JLOUT(1) .gt.0) then

     ACRES = 0.0
     !ppe     CALL SPLAAR (ISTD, ACRES, IRC)

     ITON = INT((SMALL2/10) + 1)
     if (ITON .gt.4) ITON = 4
     FUAREA(1,ITON) = FUAREA(1,ITON) + ACRES

     ITON = INT((LARGE2/10) + 1)
     if (ITON .gt.4) ITON = 4
     FUAREA(2,ITON) = FUAREA(2,ITON) + ACRES

     ITON = INT(((SMALL2+LARGE2)/10) + 1)
     if (ITON .gt.4) ITON = 4
     FUAREA(3,ITON) = FUAREA(3,ITON) + ACRES

     ITON = INT(((CWD(3,11,1,5)+CWD(3,11,2,5))/10) + 1)
     if (ITON .gt.4) ITON = 4
     FUAREA(4,ITON) = FUAREA(4,ITON) + ACRES

     ITON = INT(((TOTSNG(1)+TOTSNG(2))/10) + 1)
     if (ITON .gt.4) ITON = 4
     FUAREA(5,ITON) = FUAREA(5,ITON) + ACRES
  end if

  !     ACCUMULATE LIVE TREE INFORMATION

  TOTLIV(1) = 0.0
  TOTLIV(2) = 0.0
  TOTFOL = 0.0
  TOTCON = 0.0

  do I = 1,ITRN

     TOTFOL = TOTFOL + CROWNW(I,0) * FMPROB(I) * P2T

     !        PUT CROWN INFO INTO THE RELATED SIZE CLASSES TOO

     do J = 1,3
        TOTLIV(1) = TOTLIV(1) + (CROWNW(I,J) + OLDCRW(I,J))&
             &* P2T * FMPROB(I)
        if (J .lt.3) TOTLIV(2) = TOTLIV(2) + P2T * FMPROB(I) *&
             &(CROWNW(I,J+3) + OLDCRW(I,J+3))
     end do

     !     GET THE VOLUME OF THE FM TREES.
     !     ****CHANGED MARCH 5\99 AND USES FMSVOL (FROM FMSVL2) SO THAT BASE MODE
     !         VOLUME CALL IS ONLY IN ONE PLACE*** (SB)

     JS = ISP(I)
     D = DBH(I)
     H = HT(I)
     X = -1.0
     CRWNRTO = ICR(I)
     LMERCH = .false.

     call FMSVL2(JS,D,H,X,VT,CRWNRTO,&
          &'L',LMERCH,DEBUG,JOSTND)

     if (DEBUG) write(JOSTND,60) I,FMPROB(I),ISP(I),D,H,VT
     60 format(' FMDOUT (LIVE): I=',I5,' FMPROB=',F10.3,' ISP=',I3,&
          &' D,H,VT=',3F10.3)

     if (D .le.3.0) then
        TOTLIV(1) = TOTLIV(1) + FMPROB(I) * VT * V2T(JS)
     else
        TOTLIV(2) = TOTLIV(2) + FMPROB(I) * VT * V2T(JS)
     end if
  end do

  TOTSTD = TOTLIV(1) + TOTLIV(2) + TOTFOL + TOTSNG(1) + TOTSNG(2)
  TOTFUL = TOTSTD + TOTSUR

  !     ADD UP THE TOTAL AMOUNT CONSUMED

  do II = 1,MXFLCL
     TOTCON = TOTCON + BURNED(3,II)
  end do
  TOTCON = TOTCON + BURNLV(1) + BURNLV(2) + BURNCR

  !     ADD VOLUME REMOVED BY SALVAGE, HARVEST, AND CWD TRANSFER.
  !     EXPRESSED AS TONS

  TONREM = TONRMS + TONRMH + TONRMC

  !     ACCUMULATE OUTPUT VARIABLES INTO THOSE NEEDED FOR THE
  !     CARBON REPORT

  BIOLIVE = TOTFOL + TOTLIV(1) + TOTLIV(2)
  BIOSNAG = TOTSNG(1) + TOTSNG(2)
  BIODDW = SMALL2 + LARGE2
  BIOFLR = TOTLIT + TOTDUF
  BIOSHRB = FLIVE(1) + FLIVE(2)
  BIOREM(1) = BIOREM(1) + TONRMS + TONRMC
  BIOREM(2) = TONREM
  BIOCON(1) = BURNED(3,10) + BURNED(3,11)
  BIOCON(2) = TOTCON - BIOCON(1)

  TONRMS = 0.
  TONRMH = 0.
  TONRMC = 0.

  !     CALCULATE DOWN WOOD VOLUME AND COVER FOR REPORT AND EVENT MONITOR
  !     FIRST CLEAR THE ARRAYS
  do I = 1, 3
     do J = 1, 10
        do K = 1, 2
           do L = 1, 5
              CWDVOL(I,J,K,L) = 0
              CWDCOV(I,J,K,L) = 0
           end do
        end do
     end do
  end do

  !     FILL IN DENSITY VALUES TO USE IN DOWN WOOD VOLUME CALCULATIONS.
  !     CWDDEN ARRAY HOLDS VALUES IN LBS/CUFT.  CURRENT VALUES ARE BASED
  !     ON SPECIFIC GRAVITY VALUE OF 0.4 FOR  HARD MATERIAL, AND 0.3 FOR SOFT MATERIAL
  !     FROM BROWN (1974), GTR-INT-16, HANDBOOK FOR INVENTORYING DOWNED WOODY MATERIAL

  do L = 1, 4
     CWDDEN(1,L) = 18.72 ! lbs/cuft for soft down wood (SG = 0.3)
     CWDDEN(2,L) = 24.96 ! lbs/cuft for hard down wood (SG = 0.4)
  end do

  do I = 1, 2
     do J = 1, 9
        do K = 1, 2
           do L = 1, 4
              CWDVOL(I,J,K,L) = CWD(I,J,K,L)*2000/CWDDEN(K,L)
           end do
        end do
     end do
  end do

  !                         total stuff up
  do I = 1, 2
     do J = 1, 9
        do K = 1, 2
           do L = 1, 4
              CWDVOL(3,J,K,L) = CWDVOL(3,J,K,L) + CWDVOL(I,J,K,L)
           end do
        end do
     end do
  end do

  do I = 1, 3
     do J = 1, 9
        do K = 1, 2
           do L = 1, 4
              CWDVOL(I,J,K,5) = CWDVOL(I,J,K,5) + CWDVOL(I,J,K,L)
           end do
        end do
     end do
  end do

  do I = 1, 3
     do J = 1, 9
        do K = 1, 2
           do L = 1, 5
              CWDVOL(I,10,K,L) = CWDVOL(I,10,K,L) + CWDVOL(I,J,K,L)
           end do
        end do
     end do
  end do

  do I = 3,3
     do J = 1, 9
        do K = 1, 2
           do L = 5,5
              select case (J)
              case (1,2,3)
                 CWDCOV(I,J,K,L) = 0
              case (4)
                 CWDCOV(I,J,K,L) = 0.0166*CWDVOL(I,J,K,L)**0.8715
              case (5)
                 CWDCOV(I,J,K,L) = 0.0092*CWDVOL(I,J,K,L)**0.8795
              case (6)
                 CWDCOV(I,J,K,L) = 0.0063*CWDVOL(I,J,K,L)**0.8728
              case (7)
                 CWDCOV(I,J,K,L) = 0.0069*CWDVOL(I,J,K,L)**0.8134
              case (8)
                 CWDCOV(I,J,K,L) = 0.0033*CWDVOL(I,J,K,L)**0.8617
              case (9)
                 CWDCOV(I,J,K,L) = 0.0949*CWDVOL(I,J,K,L)**0.5
              end select
              CWDCOV(I,10,K,L) = CWDCOV(I,10,K,L) + CWDCOV(I,J,K,L)
           end do
        end do
     end do
  end do

  !     STOP HERE IF WE ARE NOT PRINTING THE FUELS REPORT
  if (.not. LPRINT) goto 750

  !     CALL THE DBS MODULE TO OUTPUT FUEL DATA TO A DATABASE

  DBSKODE = 1
  if (DEBUG) write(JOSTND,150)
  150 format(' FMDOUT: DBSFUELS- TOTLIT,TOTDUF,SMALL2,LARGE2,',&
       &'CWD(3,4,1,5),CWD(3,4,2,5),CWD(3,5,1,5),CWD(3,5,2,5),',&
       &'LARGE12,FLIVE(1),FLIVE(2),TOTSUR,TOTSNG(1),TOTSNG(2),',&
       &'TOTFOL,TOTLIV(1),TOTLIV(2),TOTSTD,TOTFUL,TOTCON,TONREM')
  if (DEBUG) write(JOSTND,*) TOTLIT,TOTDUF,SMALL2,LARGE2,&
       &CWD(3,4,1,5),CWD(3,4,2,5),CWD(3,5,1,5),CWD(3,5,2,5),&
       &LARGE12,FLIVE(1),FLIVE(2),TOTSUR,TOTSNG(1),TOTSNG(2),&
       &TOTFOL,TOTLIV(1),TOTLIV(2),TOTSTD,TOTFUL,TOTCON,TONREM

  call DBSFUELS(IYR,NPLT,TOTLIT,TOTDUF,SMALL2,LARGE2,&
       &(CWD(3,4,1,5)+CWD(3,4,2,5)),(CWD(3,5,1,5)+CWD(3,5,2,5)),&
       &LARGE12,FLIVE(1),FLIVE(2),TOTSUR,&
       &TOTSNG(1),TOTSNG(2),TOTFOL,TOTLIV(1),NINT(TOTLIV(2)),&
       &NINT(TOTSTD),NINT(TOTFUL),NINT(TOTCON),NINT(TONREM),DBSKODE)
  if (DEBUG) write(JOSTND,*) ' FMDOUT: AFTER DBSFUELS'
  if (DBSKODE.eq.0) goto 750

  !     IF HEADER REQUESTED AND THIS IS THE FIRST OPPORTUNITY TO PRINT
  !     IT, THEN DO SO.

  IFAPAS = IFAPAS + 1
  if (IFAPAS .eq.1) then
     write(JROUT,699) IDFLAL,IDFLAL
     write(JROUT,700) IDFLAL
     write(JROUT,701) IDFLAL
     write(JROUT,702) IDFLAL
     write (JROUT,44) IDFLAL,NPLT,MGMID
     write(JROUT,700) IDFLAL
     write(JROUT,703) IDFLAL
     write(JROUT,704) IDFLAL
     write(JROUT,705) IDFLAL
     write(JROUT,710) IDFLAL
     write(JROUT,715) IDFLAL
     write(JROUT,720) IDFLAL
     write(JROUT,700) IDFLAL
     699 format(2(/1X,I5))
     700 format(1X,I5,1X,122('-'))
     701 format(1X,I5,1X,42X,'******  FIRE MODEL VERSION 1.0 ******')
     702 format(1X,I5,1X,52X,'ALL FUELS REPORT ',&
          &'(BASED ON STOCKABLE AREA)')
     44 format(1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
     703 format(1X,I5,1X,52X,'ESTIMATED FUEL LOADINGS')
     704 format(1X,I5,22X,'SURFACE FUEL (TONS/ACRE) ',&
          &26X,'STANDING WOOD (TONS/ACRE)')
     705 format(I5,7X,59('-'),2X,35('-'))
     710 format(1X,I5,21X,'DEAD FUEL ',21X,&
          &'LIVE',15X,'DEAD',12X,'LIVE')
     715 format(1X,I5,1X,5X,39('-'),&
          &'--  ---------- SURF   -----------   ---------------',&
          &'        TOTAL TOTAL BIOMASS')
     720 format(1X,I5,1X,'YEAR LITT.  DUFF  0-3"   >3"  3-6" ',&
          &'6-12"  >12"  HERB SHRUB TOTAL   0-3"   >3"   FOL  0-3" ',&
          &'  >3" TOTAL BIOMASS CONS REMOVED')
  end if

  write(JROUT,730) IDFLAL,IYR,TOTLIT,TOTDUF,&
       &SMALL2,LARGE2,&
       &(CWD(3,4,1,5)+CWD(3,4,2,5)),(CWD(3,5,1,5)+CWD(3,5,2,5)),&
       &LARGE12,(FLIVE(I),I = 1,2),TOTSUR,&
       &TOTSNG(1),TOTSNG(2),TOTFOL,TOTLIV(1),NINT(TOTLIV(2)),&
       &NINT(TOTSTD),NINT(TOTFUL),NINT(TOTCON),&
       &NINT(TONREM)

  730 format(1X,I5,1X,I4,1X,F5.2,1X,6(F5.1,1X),2(F5.2,1X),&
       &F5.1,1X,F6.2,1X,3(F5.1,1X),I5,1X,I5,1X,I5,I6,1X,I6)

  750 continue
  !     STOP HERE IF WE ARE NOT PRINTING THE DOWN WOOD VOLUME REPORT
  if (.not. LPRINT2) goto 850

  !     CALL THE DBS MODULE TO OUTPUT DOWN WOOD VOLUME REPORT TO A DATABASE

  DBSKODE = 1
  do I = 1,16
     V1(I) = 0.0
  end do

  V1(1) = CWDVOL(3,1,2,5)+CWDVOL(3,2,2,5)+CWDVOL(3,3,2,5)
  V1(2) = CWDVOL(3,4,2,5)
  V1(3) = CWDVOL(3,5,2,5)
  V1(4) = CWDVOL(3,6,2,5)
  V1(5) = CWDVOL(3,7,2,5)
  V1(6) = CWDVOL(3,8,2,5)
  V1(7) = CWDVOL(3,9,2,5)
  V1(8) = CWDVOL(3,10,2,5)
  V1(9) = CWDVOL(3,1,1,5)+CWDVOL(3,2,1,5)+CWDVOL(3,3,1,5)
  V1(10) = CWDVOL(3,4,1,5)
  V1(11) = CWDVOL(3,5,1,5)
  V1(12) = CWDVOL(3,6,1,5)
  V1(13) = CWDVOL(3,7,1,5)
  V1(14) = CWDVOL(3,8,1,5)
  V1(15) = CWDVOL(3,9,1,5)
  V1(16) = CWDVOL(3,10,1,5)

  call DBSFMDWVOL(IYR,NPLT,V1,16,DBSKODE)
  if (DBSKODE.eq.0) goto 850

  !     IF HEADER REQUESTED AND THIS IS THE FIRST OPPORTUNITY TO PRINT
  !     IT, THEN DO SO.

  IDWPAS = IDWPAS + 1
  if (IDWPAS .eq.1) then
     write(JROUT,799) IDDWRP,IDDWRP
     write(JROUT,800) IDDWRP
     write(JROUT,801) IDDWRP
     write(JROUT,802) IDDWRP
     write (JROUT,44) IDDWRP,NPLT,MGMID
     write(JROUT,800) IDDWRP
     write(JROUT,803) IDDWRP
     write(JROUT,804) IDDWRP
     write(JROUT,805) IDDWRP
     write(JROUT,820) IDDWRP
     write(JROUT,800) IDDWRP
     799 format(2(/1X,I5))
     800 format(1X,I5,1X,134('-'))
     801 format(1X,I5,1X,42X,'******  FIRE MODEL VERSION 1.0 ******')
     802 format(1X,I5,1X,46X,'DOWN DEAD WOOD VOLUME REPORT ',&
          &'(BASED ON STOCKABLE AREA)')
     !   44    FORMAT(1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
     803 format(1X,I5,1X,30X,'ESTIMATED DOWN WOOD VOLUME (CUFT/ACRE)',&
          &' BY SIZE CLASS (INCHES)')
     804 format(1X,I5,34X,'HARD ',61X,'SOFT')
     805 format(I5,8X,62('-'),4X,62('-'))
     820 format(1X,I5,1X,'YEAR    0-3    3-6   6-12   12-20   20-35',&
          &'   35-50     >=50      TOT       0-3    3-6   6-12   12-20',&
          &'   20-35   35-50     >=50      TOT ')
  end if

  write(JROUT,830) IDDWRP,IYR,(NINT(V1(I)),I = 1,16)
  830 format(1X,I5,1X,I4,1X,3(I6,1X),3(I7,1X),2(I8,1X),&
       &3X, 3(I6,1X),3(I7,1X),2(I8,1X))

  850 continue
  !     STOP HERE IF WE ARE NOT PRINTING THE DOWN WOOD COVER REPORT
  if (.not. LPRINT3) return

  !     CALL THE DBS MODULE TO OUTPUT DOWN WOOD COVER REPORT TO A DATABASE

  DBSKODE = 1
  do I = 1,14
     V2(I) = 0.0
  end do
  V2(1) = CWDCOV(3,4,2,5)
  V2(2) = CWDCOV(3,5,2,5)
  V2(3) = CWDCOV(3,6,2,5)
  V2(4) = CWDCOV(3,7,2,5)
  V2(5) = CWDCOV(3,8,2,5)
  V2(6) = CWDCOV(3,9,2,5)
  V2(7) = CWDCOV(3,10,2,5)
  V2(8) = CWDCOV(3,4,1,5)
  V2(9) = CWDCOV(3,5,1,5)
  V2(10) = CWDCOV(3,6,1,5)
  V2(11) = CWDCOV(3,7,1,5)
  V2(12) = CWDCOV(3,8,1,5)
  V2(13) = CWDCOV(3,9,1,5)
  V2(14) = CWDCOV(3,10,1,5)

  call DBSFMDWCOV(IYR,NPLT,V2,14,DBSKODE)
  if (DBSKODE.eq.0) return

  !     IF HEADER REQUESTED AND THIS IS THE FIRST OPPORTUNITY TO PRINT
  !     IT, THEN DO SO.

  IDCPAS = IDCPAS + 1
  if (IDCPAS .eq.1) then
     write(JROUT,899) IDDWCV,IDDWCV
     write(JROUT,900) IDDWCV
     write(JROUT,901) IDDWCV
     write(JROUT,902) IDDWCV
     write (JROUT,44) IDDWCV,NPLT,MGMID
     write(JROUT,900) IDDWCV
     write(JROUT,903) IDDWCV
     write(JROUT,904) IDDWCV
     write(JROUT,905) IDDWCV
     write(JROUT,920) IDDWCV
     write(JROUT,900) IDDWCV
     899 format(2(/1X,I5))
     900 format(1X,I5,1X,122('-'))
     901 format(1X,I5,1X,42X,'******  FIRE MODEL VERSION 1.0 ******')
     902 format(1X,I5,1X,46X,'DOWN DEAD WOOD COVER REPORT ',&
          &'(BASED ON STOCKABLE AREA)')
     !   44    FORMAT(1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
     903 format(1X,I5,1X,30X,'ESTIMATED DOWN WOOD PERCENT COVER (%)',&
          &' BY SIZE CLASS (INCHES)')
     904 format(1X,I5,31X,'HARD ',53X,'SOFT')
     905 format(I5,8X,56('-'),2X,58('-'))
     920 format(1X,I5,1X,'YEAR    3-6    6-12   12-20   20-35   35-50',&
          &'    >=50     TOT       3-6    6-12   12-20   20-35   35-50',&
          &'    >=50     TOT ')
  end if

  write(JROUT,930) IDDWCV,IYR,(V2(I),I = 1,14)
  930 format(1X,I5,1X,I4,1X,7(F6.1,2X),2X,7(F6.1,2X))

  return
end
