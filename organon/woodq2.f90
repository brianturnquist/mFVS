!======================!
!   organon : woodq2   !
!======================!
! ORGANON $Id$

subroutine WQ_FILL(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,NWQT,TDATAI,&
     &VERSION,WQINIT,SI_1,SI_2,PDEN,TDATAR,SCR,BRCNT,&
     &BRDIA,BRHT,JCORE)
  implicit none
  ! Variables
  real*4  :: SI_1,SI_2,PDEN,TDATAR(2000,8),SCR(2000,3)
  integer*4 :: BHAGE,STAGE,IB2,NTREES,NWQT,TDATAI(2000,3),VERSION,WQINIT,BRCNT(2000,3),BRDIA(2000,40),&
       &BRHT(2000,40),JCORE(2000,40)
  logical*2 :: JCALC,EVEN
  integer*4 :: II,III,WQAGE,ZBRDIA,ZJCORE,II1,JLAG,JJ,JK,JJJ,IKNT
  real*4  :: DINC,TOHT,HTCB,BDBH,RAT,HSH,HTCORE,X1,X2,A1,A2,B0,B1,B2,SITE,BBC,X50,AA1,BSH,BSHX,XAGE,&
       &SITE2,WHHLBH,WHHLBHX,XBRHT

  !  04/25/2014 - THERE ARE COMMON SUBROUTINE NAMES IN THE SOURCE CODE
  !               USED TO BUILD THE ORGANON DLLS. IN ORDER TO LINK THE
  !               ORGANON SOURCE CODE WITH THE FVS SOURCE CODE WE CHANGED
  !               THE DUPLICATED SUBROUTINE NAMES TO MAKE THEM UNIQUE.

  !  CHANGED THE NAME OF SUBROUTINE CALL - TAPER_RA TO TAPER_RA_WOOD

  !     SUBROUTINE WQ_FILL HAS ONE FUNCTION:INITIATION OF WHORL BRANCH
  !                    LOCATIONS, BRANCH DIAMETERS BELOW LIVE CROWN, AND
  !                    JUVENILE WOOD CORE DIAMETER (IF LIVE CROWN IS ABOVE
  !                    1 FT, OR IF BREAST HEIGHT AGE IS >20)

  !     ASSIGN WHORL BRANCH LOCATIONS, INITIAL BRANCH SIZES, AND
  !        JUVENILE WOOD CORE IF NOT PREVIOUSLY DONE

  !     START INITIALIZATION

  !       DETERMINE IF TREES BEING INITIALIZED ARE INGROWTH OR ORIGINAL
  !         TREE LIST

  if (WQINIT .lt.0) then
     II1 = 1
     III = 0
  else
     II1 = NTREES-WQINIT+1
     III = NWQT
  end if

  !     STEP THROUGH TREE LIST TO INITIALIZE

  do II = II1,NTREES
     if (TDATAI(II,2) .gt. IB2) cycle
     !         IF(VERSION .EQ. 4 .AND. TDATAI(II,2) .EQ. 1) CYCLE
     III = III+1
     ! Modified 10/27/94 for both
     select case(VERSION)
     case(1)
        if (TDATAI(II,1) .eq.202) then
           B0 = -6.21693
           B1 = 0.281176
           B2 = 1.14354
           SITE = SI_1
        else if (TDATAI(II,1) .eq.122) then
           B0 = -6.54707
           B1 = 0.288169
           B2 = 1.21297
           SITE = SI_2
        else
           B0 = -6.21693
           B1 = 0.281176
           B2 = 1.14354
           SITE = SI_1
           if (TDATAI(II,1) .eq.81) SITE = ((SITE+4.5)*0.7)-4.5
        end if
        if (EVEN .and. WQINIT .lt.0) then
           WQAGE = BHAGE
        else
           BBC = B0+B1*LOG(SITE)
           X50 = 1.0-EXP((-1.)*EXP(BBC+B2*3.912023))
           AA1 = 1.0-(TDATAR(II,2)-4.5)*(X50/SITE)
           if (AA1.lt.0.) then
              WQAGE = 500
           else
              XAGE = ((-1.0*LOG(AA1))/(EXP(B0)*SITE**B1))**(1.0/B2)
              WQAGE = (NINT(XAGE/5.0))*5
           end if
        end if
     case(2)
        SITE = SI_1+4.5
        X1 = 13.25-SITE/20.0
        X2 = 63.25-SITE/20.0
        B2 = -0.447762-0.894427*SITE/100.0+0.793548*(SITE/100.0)**2&
             &-0.171666*(SITE/100.0)**3
        B1 = ALOG(4.5/SITE)/(X1**B2-X2**B2)
        if (EVEN .and. WQINIT .lt.0) then
           WQAGE = BHAGE
        else
           XAGE = (ALOG(TDATAR(II,2)/SITE)/B1+X2**B2)**(1.0/B2)-X1
           WQAGE = (NINT(XAGE/5.0))*5
        end if
     case(3)
        SITE = SI_1+4.5
        X1 = 13.25-SITE/20.0
        X2 = 63.25-SITE/20.0
        B2 = -0.447762-0.894427*SITE/100.0+0.793548*(SITE/100.0)**2&
             &-0.171666*(SITE/100.0)**3
        B1 = ALOG(4.5/SITE)/(X1**B2-X2**B2)
        if (EVEN .and. WQINIT.lt.0) then
           WQAGE = BHAGE
        else
           XAGE = (ALOG(TDATAR(II,2)/SITE)/B1+X2**B2)**(1.0/B2)-X1
           WQAGE = (NINT(XAGE/5.0))*5
        end if
     case(4)
        SITE = SI_1+4.5
        call WHHLB_SI_UC_WOOD(SITE,PDEN,SITE2)
        !             IF(EVEN .AND. WQINIT.LT.0) THEN
        if (WQINIT.lt.0) then
           WQAGE = STAGE
           !             ELSE
           !                CALL WHHLB_GEA(TDATAR(II,2),SITE2,XAGE)
           !                WQAGE=NINT(XAGE)
        end if
     end select
     if (VERSION .eq.4) then
        if (WQAGE .lt.3) WQAGE = 3
     else
        if (WQAGE .lt.5) WQAGE = 5
     end if

     !                    JLAG = NUMBER OF YEARS INITIAL AGE IS PAST
     !                            A MULTIPLE OF 5

     if (VERSION .eq.4) then
        !            JLAG = MOD(WQAGE,1)
        JLAG = 0
        BRCNT(III,1) = INT(WQAGE)
     else
        JLAG = MOD(WQAGE,5)
        BRCNT(III,1) = INT(WQAGE/5)+2
     end if
     TOHT = TDATAR(II,2)
     BDBH = TDATAR(II,1)
     !         HTCB=(1.0-TDATAR(II,3))*TOHT
     if (SCR(II,1) .gt.0.0) then
        HTCB = (1.0-SCR(II,1))*TOHT
     else
        HTCB = (1.0-TDATAR(II,3))*TOHT
     end if

     !        ASSIGN LOWEST BRANCH HEIGHT
     !                    =  HEIGHT TO CROWN BASE IF HTCB < 4.5 FT
     !                    =  2.3 FT               IF HTCB >= 4.5 FT

     if (VERSION .le.3) then
        if (HTCB .lt.4.4) then
           BRHT(III,1) = INT(10*(HTCB+0.15))
           BRCNT(III,2) = 0
           if (JCALC) then
              BRCNT(III,3) = 0
           else
              if (WQAGE .lt.20) BRCNT(III,3) = 0
           end if
        else if (HTCB .eq.4.4) then
           BRHT(III,1) = 23
           BRCNT(III,2) = 1
           if (JCALC) then
              BRCNT(III,3) = 1
           else
              if (WQAGE .lt.20) BRCNT(III,3) = 0
           end if
        else
           BRHT(III,1) = 23
           BRCNT(III,2) = 2
           if (JCALC) then
              BRCNT(III,3) = 2
           else
              if (WQAGE .lt.20) BRCNT(III,3) = 0
           end if
        end if
        BRHT(III,2) = 45
     end if

     !         ASSIGN HEIGHTS TO INITIAL BRANCHES ABOVE DBH

     !  Modified 10/27/94 for both models

     select case(VERSION)
     case(1)

        !               SCALED HANN-SCRIVANI HEIGHT GROWTH APPROACH FOR
        !                    ASSIGNING WHORL HEIGHTS

        HSH = SITE*((1-EXP(-EXP(B0+B1*LOG(SITE)&
             &+B2*LOG(FLOAT(WQAGE)))))/&
             &(1-EXP(-EXP(B0+B1*LOG(SITE)+B2*LOG(50.0)))))
        RAT = (TDATAR(II,2)-4.5)/HSH
        do JJ = 3,BRCNT(III,1)
           BRHT(III,JJ) = INT(10*(4.5+RAT*SITE*&
                &((1-EXP(-EXP(B0+B1*LOG(SITE)+B2*LOG(5.*(FLOAT(JJ)-2.)))))/&
                &(1-EXP(-EXP(B0+B1*LOG(SITE)+B2*LOG(50.0)))))))
        end do
     case(2,3)

        !               SCALED BRUCE HEIGHT GROWTH APPROACH FOR
        !                    ASSIGNING WHORL HEIGHTS

        BSH = SITE*EXP(B1*((FLOAT(WQAGE)+X1)**B2-X2**B2))
        RAT = (TDATAR(II,2)-4.5)/(BSH-4.5)
        do JJ = 3,BRCNT(III,1)
           XAGE = 5.0*(FLOAT(JJ)-2.0)
           BSHX = SITE*EXP(B1*((XAGE+X1)**B2-X2**B2))
           BRHT(III,JJ) = INT(10*(4.5+RAT*(BSHX-4.5)))
        end do
     case(4)

        !               SCALED WEISKITTEL,HANN.HIBBS,LAM,BLUHM HEIGHT GROWTH
        !                    APPROACH FOR ASSIGNING WHORL HEIGHTS

        IKNT = 0
        A1 = -4.481266
        A2 = -0.658884
        WHHLBH = SITE2*EXP(A1*(FLOAT(WQAGE)**A2-20.0**A2))
        RAT = TDATAR(II,2)/WHHLBH
        do JJ = 1,BRCNT(III,1)
           XAGE = FLOAT(JJ)
           WHHLBHX = SITE2*EXP(A1*(XAGE**A2-20.0**A2))
           XBRHT = RAT*WHHLBHX
           if (XBRHT .lt. HTCB) then
              IKNT = IKNT+1
           end if
           BRHT(III,JJ) = INT(10*XBRHT)
        end do
        BRCNT(III,2) = IKNT
     end select

     !             CHECK TO SEE IF WHORLS ARE DEAD, ASSIGN BRANCH DIAMETER
     !                AND CROWN-DEFINED JUVENILE CORE IF SO

     if (VERSION .le.3.and. BRCNT(III,2) .gt.0) then
        DINC = TOHT-HTCB
        call BRANCH(TDATAI(II,1),DINC,BDBH,TOHT,HTCB,ZBRDIA)
        if (JCALC) then
           call CORE(VERSION,TDATAI(II,2),HTCB,TOHT,BDBH,HTCB,ZJCORE)
        end if
        do JJ = 2,BRCNT(III,1)
           if (FLOAT(BRHT(III,JJ))/10.ge. HTCB) then
              BRCNT(III,2) = JJ-1
              BRDIA(III,JJ-1) = ZBRDIA
              if (JCALC) then
                 JCORE(III,JJ-1) = ZJCORE
                 BRCNT(III,3) = JJ-1
              end if
              exit
           else if (JJ .eq. BRCNT(III,1)) then
              BRDIA(III,JJ) = ZBRDIA
              BRCNT(III,2) = JJ
              if (JCALC) then
                 JCORE(III,JJ) = ZJCORE
                 BRCNT(III,3) = JJ
              end if
              exit
           end if
        end do
        do JK = 1,BRCNT(III,2)-1
           BRDIA(III,JK) = BRDIA(III,BRCNT(III,2))
           if (JCALC) then
              JCORE(III,JK) = JCORE(III,BRCNT(III,2))
           end if
        end do
     else if (VERSION .eq.4.and. BRCNT(III,2) .gt.0) then
        DINC = TOHT-HTCB
        call BRANCH(TDATAI(II,1),DINC,BDBH,TOHT,HTCB,ZBRDIA)
        do JJ = 1,BRCNT(III,2)
           BRDIA(III,JJ) = ZBRDIA
        end do
     end if

     !             ASSIGN AGE-DEFINED JUVENILE CORE
     !                                   IF BREAST HEIGHT AGE >= 20 YRS

     if (.not. JCALC .and. VERSION .le.3) then
        if (5*(BRCNT(III,1)-2) .ge.20) then
           HTCORE = FLOAT(BRHT(III,BRCNT(III,1)-4))/10. +&
                &(1.0-FLOAT(JLAG)/5)*(FLOAT(BRHT(III,BRCNT(III,1)-3)-&
                &BRHT(III,BRCNT(III,1)-4))/10.)
           call CORE(VERSION,TDATAI(II,2),HTCORE,TOHT,BDBH,HTCB,&
                &JCORE(III,BRCNT(III,1)-4))
           BRCNT(III,3) = BRCNT(III,1) - 4
           do JJJ = 1,BRCNT(III,1)-5
              JCORE(III,JJJ) = JCORE(III,BRCNT(III,1)-4)
           end do
        end if
     end if
  end do

  !       ESTABLISH NEW NUMBER OF WOOD QUALITY TREES (NWQT)

  NWQT = III

  !          SET WQINIT TO 0 TO SIGNAL INITIALIZATION OF

  !                                INITIAL TREE LIST OR
  !                                         OR
  !                                 NEW INGROWTH TREES

  WQINIT = 0

  return
end

subroutine WQ_CALC(JCALC,EVEN,BHAGE,STAGE,IB2,NTREES,VERSION,&
     &TDATAI,GROWTH,TDATAR,SCR,BRCNT,BRDIA,BRHT,&
     &JCORE)
  implicit none
  ! Variables
  real*4  :: GROWTH(2000,4),TDATAR(2000,8),SCR(2000,3)
  integer*4 :: BHAGE,STAGE,IB2,NTREES,VERSION,TDATAI(2000,3),BRCNT(2000,3),BRDIA(2000,40),BRHT(2000,&
       &40),JCORE(2000,40),GP
  logical*2 :: JCALC,EVEN
  integer*4 :: II,III,JCOR1,JCOR2,WQAGE,JLAG,LL
  real*4  :: DINC,TOHT,HTCB,BDBH,FHCB,UD,UBH,UHC,SCALE,BHITE,UH

  !     SUBROUTINE WQ_CALC HAS ONE PRIMARY FUNCTION: UDATING MAXIMUM BRANCH
  !                  DIAMETER AND JUVENILE WOOD CORE AS TREES GROW AND
  !                  CROWNS RECEDE

  !           EVERY 5TH WHORL IS TRACKED, AND WHENEVER A TREE IS CUT
  !                EITHER BEFORE OR AFTER FINAL HARVEST, THE TREE SIZE
  !                AND WHORL SPECIFIC INFORMATION IS WRITTEN TO A
  !                SPECIFIED WOOD QUALITY OUTPUT FILE

  !     START OF END-OF-GROWTH-PERIOD UPDATES FOR WOOD QUALITY

  !          STEP THROUGH TREE LIST TO UPDATE:

  !                          MAXMIMUM BRANCH DIAMETER
  !                          JUVENILE WOOD CORE

  III = 0
  do II = 1,NTREES

     !         BYPASS WOOD QUALITY PREDICITIONS IF TREE
     !                                 IS NOT A BIG FIVE SPECIES

     if (TDATAI(II,2) .gt. IB2) cycle
     !        IF(VERSION .EQ. 4 .AND. TDATAI(II,2) .EQ. 1) CYCLE

     !          INCREMENT WOOD QUALITY ARRAY INDEX (III)

     III = III+1

     !          BYPASS WOOD QUALITY PREDICTIONS IF TREE HAS DIED

     if (TDATAR(II,4) .le.0.0) cycle

     !          BYPASS TREE IF CROWN HAS ALREADY RECEDED 200 WHORLS
     !                            (THAT IS, 40 SETS OF 5 WHORLS)

     if (BRCNT(III,2) .ge.40) cycle

     !         TDATAR(II,2) = ENDING HEIGHT
     !         GROWTH(II,1) = 5 YEAR HEIGHT GROWTH
     !         TDATAR(II,1) = ENDING DBH
     !         GROWTH(II,2) = 5 YEAR DBH GROWTH

     !         CALC BEGINNING TOHT, BDBH, AND HTCB

     !         TOHT = BEGINNING TOTAL HEIGHT
     !         BDBH = BEGINNING BREAST HT STEM DIAMETER
     !         HTCB = BEGINNING HEIGHT TO CROWN BASE
     !         FHCB = FINAL HEIGHT TO CROWN BASE

     !         DEFINE VARIABLES FOR WOOD QUALITY PREDICTIONS BASED ON
     !          CONDITIONS AT BEGINNING OF THE GROWTH PERIOD

     TOHT = TDATAR(II,2) - GROWTH(II,1)
     BDBH = TDATAR(II,1) - GROWTH(II,2)
     !        HTCB=TOHT*(1-TDATAR(II,7))
     !        FHCB=TDATAR(II,2)*(1.-TDATAR(II,3))
     if (SCR(II,3) .gt.0.0) then
        HTCB = (1.0-SCR(II,3))*TOHT
     else
        HTCB = (1.0-TDATAR(II,7))*TOHT
     end if
     if (SCR(II,1) .gt.0.0) then
        FHCB = (1.0-SCR(II,1))*TDATAR(II,2)
     else
        FHCB = (1.0-TDATAR(II,3))*TDATAR(II,2)
     end if

     !      INCREMENT ASSIGNED WHORL COUNT BY 1 AND ASSIGN WHORL HEIGHT

     if (EVEN) then
        if (VERSION .eq.4) then
           !             JLAG=MOD(STAGE,1)
           JLAG = 0
           GP = 1
        else
           JLAG = MOD(BHAGE,5)
           GP = 5
        end if
     else
        JLAG = 0
     end if
     if (BRCNT(III,1) .lt.40) then
        BRCNT(III,1) = BRCNT(III,1)+1
        if (JLAG .eq.0) then
           BRHT(III,BRCNT(III,1)) = INT(10*TDATAR(II,2))
        else
           BRHT(III,BRCNT(III,1)) = INT(10*( TDATAR(II,2) -&
                &(FLOAT(JLAG)/GP) * GROWTH(II,1) ))
        end if
     end if
     if (VERSION .le.3) then
        WQAGE = (BRCNT(III,1)-2)*GP
     else
        WQAGE = STAGE
     end if

     !      PREDICT AGE-DEFINED JUVENILE WOOD CORE FOR ANOTHER WHORL

     if (.not. JCALC .and. VERSION .le.3) then
        if (BRCNT(III,3) .lt.40) then
           if (WQAGE .ge.20) then
              BRCNT(III,3) = BRCNT(III,3) + 1
              if (BRCNT(III,3).eq.1) BRCNT(III,3) = BRCNT(III,3) + 1
              BHITE = FLOAT(BRHT(III,BRCNT(III,3)))/10.
              call CORE(VERSION,TDATAI(II,2),BHITE,TOHT,BDBH,HTCB,JCOR1)
              call CORE(VERSION,TDATAI(II,2),BHITE,TDATAR(II,2),&
                   &TDATAR(II,1),FHCB,JCOR2)
              if (JLAG.eq.0) then
                 JCORE(III,BRCNT(III,3)) = JCOR2
              else
                 JCORE(III,BRCNT(III,3)) = JCOR2 -&
                      &INT((FLOAT(JLAG)/GP)*FLOAT(JCOR2-JCOR1))
              end if

              !              ASSIGN JUVENILE WOOD CORE DOWN TO WHORL 1 IF IT
              !                   HAS NOT ALREADY BEEN DONE (WHEN INITIAL BREAST
              !                   HEIGHT AGE FIRST EXCEEDS 20 YRS)

              if (WQAGE .eq.20) then
                 JCORE(III,1) = JCORE(III,2)
              end if
           end if
        end if
     end if

     !        CHECK FOR CROWN RECESSION PAST LOWEST LIVE WHORL

     !        IF CROWN BASE HAS PASSED ANY PREVIOUSLY LIVE WHORLS,
     !          ESTIMATE MAXIMUM BRANCH DIAMETER AND CROWN WOOD CORE

     do LL = BRCNT(III,2)+1,BRCNT(III,1)
        if (FLOAT(BRHT(III,LL))/10.ge. FHCB) exit

        !           COMPUTE SCALING FACTOR FOR COMPUTING DBH, RD, AND HCB
        !             AT TIME OF CROWN RECESSION PAST SUBJECT WHORL

        SCALE = (FLOAT(BRHT(III,LL))/10.-HTCB)/(FHCB-HTCB)
        UHC = FLOAT(BRHT(III,LL))/10.
        UD = TDATAR(II,1) - (1.-SCALE)*GROWTH(II,2)
        UH = TDATAR(II,2) - (1.-SCALE)*GROWTH(II,1)
        UBH = FLOAT(BRHT(III,LL))/10.
        DINC = UH - FLOAT(BRHT(III,LL))/10.
        call BRANCH(TDATAI(II,1),DINC,UD,UH,UHC,BRDIA(III,LL))
        BRCNT(III,2) = LL
        if (JCALC .and. VERSION .le.3) then
           call CORE(VERSION,TDATAI(II,2),FLOAT(BRHT(III,LL))/10.,&
                &UH,UD,UBH,JCORE(III,LL))
           BRCNT(III,3) = LL
        end if
     end do
  end do

  !     END OF END-OF-GROWTH-PERIOD UPDATES FOR WOOD QUALITY

  return
end

subroutine BRANCH(SP,DINC,DBH,HT,HC,IBD)
  implicit none

  !     SUBROUTINE BRANCH CALCULATES MAXIMUM BRANCH DIAMETER
  !                                               NEAR CROWN BASE

  !     BRANCH DIAMETER EQUATIONS ARE SPECIES SPECIFIC

  ! Variables
  integer*4 :: SP,IBD
  real*4  :: DINC,DBH,HT,HC
  real*4  :: CL,CR,RDINC,Z,MCW,LCW,XBD
  CL = HT-HC
  CR = CL/HT
  RDINC = DINC/CL
  Z = 1.0-RDINC
  if (Z .lt.0.0) Z = 0.0
  if (Z .gt.1.0) Z = 1.0
  if (SP .eq.202) then

     !       DOUGLAS-FIR

     !       BRANCH DIAMETER MODEL 6c FROM MAGUIRE, JOHNSON AND CAHILL (1999)
     !              CANADIAN JOURNAL OF FOREST RESEARCH 29: 1829-1840

     MCW = 4.6198+1.8426*DBH-0.011311*DBH*DBH
     LCW = MCW*CR**(0.004363240*CL+0.6020020*(DBH/HT))
     if (Z .ge.1.0) then
        XBD = 0.0
     else
        XBD = (0.03691*LCW**1.1501)*((1.0-SQRT(Z))**(0.5569*Z**2.7096))
     end if
     IBD = INT(100*XBD)

  else if (SP .eq.15.or. SP .eq.17) then

     !       TRUE FIR (WHITE AND GRAND)

     !             BRANCH DIAMETER MODEL LAST CHANGED 10-20-92

     IBD = INT(100*(0.18840809*(DINC**0.406434)*EXP(0.029701*DBH)))
  else if (SP .eq.122) then

     !       PONDEROSA PINE

     !             BRANCH DIAMETER MODEL LAST CHANGED 10-20-92

     IBD = INT(100*(0.07762242*(DINC**(0.479889-0.001042*RDINC))*&
          &EXP(0.025218*DBH)*(HT**0.319545)))
  else if (SP .eq.117) then

     !       SUGAR PINE

     !             BRANCH DIAMETER MODEL LAST CHANGED 10-20-92

     IBD = INT(100*(0.05575434*(DINC**0.197098)*(HT**0.629666)))
  else if (SP .eq.81) then

     !       INCENSE CEDAR

     !             BRANCH DIAMETER MODEL LAST CHANGED 10-20-92

     IBD = INT(100*(0.11311752*(DINC**0.675786)*(CL**(-0.564461))))
  else if (SP .eq.351) then

     !       RED ALDER

     !             BRANCH DIAMETER MODEL LAST CHANGED 10-20-10

     IBD = INT(100*(0.160884735*(DINC**(0.747251187&
          &-0.132263075*RDINC))*EXP(0.024891787*DBH)))
  end if
  return
end

subroutine CORE(VERSION,SP,BHT,TOHT,DBH,HTCB,ICR)
  implicit none

  !     SUBROUTINE CORE

  !         CALCULATES INSIDE BARK DIAMETER FOR

  !                         JUVENILE WOOD CORE DIAMETER AND
  !                         FINAL DIBs FOR HARVESTED STEMS

  ! Variables
  integer*4 :: VERSION,SP,ICR
  real*4  :: BHT,TOHT,DBH,HTCB
  real*4  :: CR,DIB,D1,AA1,AA2,A3,A4,ALP,BBH,DCB,HD,RH,WLTX,PX1,PX2,JP1,JP2,I1,I2,A,B,C

  CR = 1.0-HTCB/TOHT
  HD = (TOHT-4.5)/DBH
  select case(VERSION)
  case(1) ! Southwest Oregon
     call SWO_DIB(SP,DBH,CR,DIB)
     call SWO_DIB1FT(SP,DBH,CR,D1)
     call SWO_TAPER(SP,AA1,AA2,A3,A4,ALP)
  case(2,3) ! Western Willamette Valley, SMC
     call NWO_DIB(SP,DBH,CR,DIB)
     call NWO_DIB1FT(SP,DBH,CR,D1)
     call NWO_TAPER(SP,AA1,AA2,A3,A4,ALP)
  case(4) !Red Alder Plantations
     call TAPER_RA_WOOD(DBH,TOHT,CR,BHT,DCB)
     go to 10
  end select
  if (BHT .le.4.5) then
     BBH = ((4.5-((DIB/D1)**(2./3.))-(BHT&
          &*(1.0-((DIB/D1)**(2./3.)))))/3.5)**(3./2.)
     DCB = BBH*D1
  else
     RH = (BHT-4.5)/(TOHT-4.5)
     WLTX = ((ALP*HTCB)-4.5)/(TOHT-4.5)
     PX1 = AA1+AA2*EXP(A3*HD**2)
     PX2 = A4
     JP1 = (RH-1.0)/(WLTX-1.0)
     JP2 = (WLTX-RH)/(WLTX-1.0)
     if (RH .ge.0.0.and. RH .le. WLTX) then
        I1 = 0.0
     else
        I1 = 1.0
     end if
     if (WLTX.le.0.0) then
        I2 = 0.0
     else
        I2 = 1.0
     end if
     A = 1.0-RH+I2*(RH+I1*(JP1*(1.0+JP2)-1.0))-(RH-1.0)*(RH-I2*RH)
     B = PX1*(I2*(RH+I1*(JP1*(RH+WLTX*JP2)-RH))-(RH-1.0)*(RH-I2*RH))
     C = PX2*I2*((RH**2)+I1*(JP1*WLTX*(2.0*RH-WLTX+WLTX*JP2)-RH**2))
     DCB = DIB*(A+B+C)
  end if
  10 ICR = NINT(100.0*DCB)
  return
end

subroutine WQ_END(FINAL,JCALC,NEXT,IB2,NTREES,VERSION,&
     &TDATAI,MGEXP,TDATAR,SCR,BRCNT,BRDIA,BRHT,JCORE,&
     &IDIB)
  implicit none
  ! Variables
  real*4  :: MGEXP(2000),TDATAR(2000,8),SCR(2000,3)
  integer*4 :: NEXT,IB2,NTREES,VERSION,TDATAI(2000,3),BRCNT(2000,3),BRDIA(2000,40),BRHT(2000,40),&
       &JCORE(2000,40),IDIB(2000,40)
  logical*2 :: FINAL,JCALC
  real*4  :: TOHT,BDBH,HTCB,DINC
  integer*4 :: III,LL,JJ,J,II,XDIB,IB3

  !   Calculates Branch data.  Used here, and in MANAGE1

  III = 0
  if (FINAL) then
     JJ = 8
  else
     JJ = 4
  end if
  IB3 = 1
  do LL = 1,NTREES
     if (TDATAI(LL,2) .ge. IB3.and. TDATAI(LL,2) .le. IB2) then
        III = III+1
        if (NEXT .eq.4) then
           if (MGEXP(LL) .le.0.0001) cycle
        else
           if (TDATAR(LL,JJ) .le.0.00001) cycle
        end if
     else
        cycle
     end if

     !          CALCULATE WOOD QUALITY PARAMETERS FOR CANOPY

     TOHT = TDATAR(LL,2)
     BDBH = TDATAR(LL,1)
     !        HTCB=(1-TDATAR(LL,3))*TOHT
     if (SCR(LL,1) .gt.0.0) then
        HTCB = (1.0-SCR(LL,1))*TOHT
     else
        HTCB = (1.0-TDATAR(LL,3))*TOHT
     end if

     !          CALCULATE JUVENILE CORE IF USING 20-YR DEFINITION

     if (.not. JCALC .and. VERSION .le.3) then
        do J = BRCNT(III,3)+1,BRCNT(III,1)
           call CORE(VERSION,TDATAI(LL,2),FLOAT(BRHT(III,J))/10.0,&
                &TOHT,BDBH,HTCB,XDIB)
           JCORE(III,J) = XDIB
        end do
     end if
     do II = BRCNT(III,2)+1,BRCNT(III,1)
        DINC = TOHT-FLOAT(BRHT(III,II))/10.0

        !          CALCULATE BRANCH DIAMETER

        call BRANCH(TDATAI(LL,1),DINC,BDBH,TOHT,HTCB,BRDIA(III,II))

        !          CALCULATE JUVENILE WOOD CORE IF DEFINED BY CROWN

        if (JCALC .and. VERSION .le.3) then
           call CORE(VERSION,TDATAI(LL,2),FLOAT(BRHT(III,II))/10.0,&
                &TOHT,BDBH,HTCB,XDIB)
           JCORE(III,II) = XDIB
        end if
     end do

     !             CALCULATE STEM DIAMETER AT EACH 5TH WHORL HEIGHT

     do J = 1,BRCNT(III,1)
        IDIB(III,J) = 0
     end do
     if (VERSION .le.3) then
        if (JCALC) then
           do J = 1,BRCNT(III,1)
              if (BRHT(III,J) .lt. (INT(10*HTCB))) then
                 call CORE(VERSION,TDATAI(LL,2),FLOAT(BRHT(III,J))/10.0,&
                      &TOHT,BDBH,HTCB,XDIB)
                 IDIB(III,J) = XDIB
              else
                 IDIB(III,J) = JCORE(III,J)
              end if
           end do
        else
           do J = 1,BRCNT(III,1)
              if (BRCNT(III,3).gt.0) then
                 if (BRHT(III,J) .le. BRHT(III,BRCNT(III,3))) then
                    call CORE(VERSION,TDATAI(LL,2),&
                         &FLOAT(BRHT(III,J))/10.0,TOHT,BDBH,HTCB,&
                         &XDIB)
                    IDIB(III,J) = XDIB
                 else
                    IDIB(III,J) = JCORE(III,J)
                 end if
              else
                 IDIB(III,J) = JCORE(III,J)
              end if
           end do
        end if
     else
        do J = 1,BRCNT(III,1)
           call CORE(VERSION,TDATAI(LL,2),FLOAT(BRHT(III,J))/10.0,&
                &TOHT,BDBH,HTCB,XDIB)
           IDIB(III,J) = XDIB
        end do
     end if
  end do
  return
end
