!=========================!
!   fire/vbase : fmpocr   !
!=========================!
! FIRE-VBASE $Id$
subroutine FMPOCR(IYR,ICALL)
  implicit none

  !     CALLED FROM: FMBURN

  !  PURPOSE:
  !     THIS SUBROUTINE FINDS THE BOTTOM OF THE LIVE CROWN (ACTCBH)
  !     THE CROWN BULK DENSITY (CBD).
  !     TCLOAD IS THE TOTAL CANOPY FUEL LOAD (LBS/SQFT).

  !  THIS ROUTINE WAS REWRITTEN BY N.CROOKSTON, 04/07/99. I CHANGED
  !  THE RUNNING AVERAGE COMPUTATIONS SO THAT ZEROS ALONG THE BOLE
  !  WHERE NO CROWN EXISTS ON ANY TREES ARE NOT COUNTED IN THE AVERAGE
  !  THIS CAUSES THE CALCULATION OF THE BASE OF THE LIVE CROWN TO BE
  !  MORE RESPONSIVE TO THINNINGS AND PRUNINGS.

  !  JANUARY 2003: HARDWOODS (LSW(I) .FALSE.) DO NOT CONTRIBUTE TO
  !  THE CALCULATIONS

  !     PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !     COMMON INCLUDE FILES.

  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  !     VARIABLE DECLARATIONS.

  ! Variables
  logical :: DEBUG,LBHPP
  integer :: J,I,I1,I2,J1,J2,MXJ,IYR,K,ICALL
  real    :: CRBOT,CRFILL(400),ABOT,ABOTMX
  real    :: ADCRWN,A,ADJ
  real    :: MSDI,MRD,WTRADJ,WEIBB,WEIBC,TSCL,SECINT
  real    :: ADCRN(400),SECBND,WPROP(400),LCR,DCM,CRBIO

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMPOCR',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,ITRN
  7 format(' ENTERING FMPOCR CYCLE = ',I2,' ITRN=',I5)

  do J = 1,400
     CRFILL(J) = 0.0
  end do

  ! SET VARIABLES AND INSERT CODE TO COMPUTE RELATIVE DENSITY FOR NEW
  ! WEIBULL FUNCTION FOR BLACK HILLS PONDEROSA PINE.
  ! SEE KEYSER AND SMITH, FOREST SCIENCE 56(2) 2010
  ! RELATIVE DENSITY IS IN METRIC UNITS.

  MSDI = 0
  MRD = 0
  WEIBB = 0
  WEIBC = 0
  WTRADJ = 0
  TSCL = 0
  SECINT = 0
  do I = 1,ITRN
     DCM = DBH(I)*2.54
     MSDI = MSDI + ((FMPROB(I)*2.47)*(DCM/25.4)**1.6)
  end do
  MRD = MSDI / 1111.97
  if (MRD.gt.1.0) MRD = 1.0

  do I = 1,ITRN

     if ((LSW(ISP(I)) .or. (ICANSP .eq.1)) .and.&
          &FMICR(I) .gt.0.and.&
          &HT(I)    .gt. CANMHT) then

        LBHPP = .false.
        select case (VARACD)
        case ('CR')
           if ((KODFOR .eq.203.or. KODFOR .eq.207) .and.&
                &(ISP(I) .eq.13)) LBHPP = .true.
        case ('IE','EM','KT')
           if (ISP(I) .eq.10) LBHPP = .true.
        end select

        if (.not.(LBHPP)) then

           CRBOT = HT(I) * (1.0 - (FLOAT(FMICR(I)) * 0.01))
           if (CRBOT.lt.0.) CRBOT = 0.0

           ADCRWN = (CROWNW(I,0) + CROWNW(I,1) * 0.5) *&
                &FMPROB(I) / (HT(I) - CRBOT)

           I1 = INT(CRBOT) + 1
           I2 = INT(HT(I)) + 1

           if (DEBUG) write (JOSTND,10) I,ADCRWN,HT(I),FMICR(I),&
                &CRBOT,I1,I2,CROWNW(I,0),CROWNW(I,1)
           10 format (' FMPOCR, I=',I4,' ADCRWN=',F9.4,' HT=',F7.2,&
                &' FMICR=',I3,' CRBOT=',F7.2,' I1,I2=',2I5,&
                &' CROWNW(0&1)=',2F7.2)

           if (I1.gt.400) I1 = 400
           if (I2.gt.400) I2 = 400

           ! ADD THE TREES CANOPY FUELS MATERIAL TO CRFILL.  ADJUST THE TOP AND BOTTOM
           ! INTERVALS SINCE THE CROWN MIGHT NOT FULLY COVER THAT 1-FT INTERVAL.  SAR 3/2015

           if (I1.le.I2.and. ADCRWN .gt.0.0) then
              do J = I1,I2
                 if (J .eq. I1) then
                    ADJ = MAX(0.,MIN(1.,I1 - CRBOT))
                    CRFILL(J) = CRFILL(J) + ADCRWN*ADJ
                 else if (J .eq. I2) then
                    ADJ = MAX(0.,MIN(1.,I2 - HT(I)))
                    CRFILL(J) = CRFILL(J) + ADCRWN*(1-ADJ)
                 else
                    CRFILL(J) = CRFILL(J) + ADCRWN
                 end if
              end do
           end if

        else ! Black Hill PP uses special shape distribution
           LCR = (FLOAT(FMICR(I)) * 0.01)
           CRBIO = (CROWNW(I,0) + CROWNW(I,1) * 0.5) * FMPROB(I)

           ! COMPUTE HEIGHT TO BASE OF CROWN (I1) AND TOP OF CROWN (I2)

           CRBOT = HT(I) * (1.0 - (FLOAT(FMICR(I)) * 0.01))
           !            WRITE(20,*) 'LCR,CRBIO,CRBOT,'
           !            WRITE(20,*) LCR,',',CRBIO,',',CRBOT

           if (CRBOT.lt.0.) CRBOT = 0.0

           I1 = INT(CRBOT) + 1
           I2 = INT(HT(I)) + 1
           if ((I2 - HT(I)) .ge.1) I2 = I2 - 1

           if (I1.gt.400) I1 = 400
           if (I2.gt.400) I2 = 400
           !            WRITE(20,*) 'I1 ,', I1
           !            WRITE(20,*) 'I2 ,', I2

           if (I1.le.I2.and. CRBIO .gt.0.0) then

              ! THE NEW WEIBUL STUFF
              ! ESTIMATE THE WEIBULL PARAMETERS B AND C FOR THE TREE
              ! B = SHAPE PARAMTER AND C = SCALE PARAMETER
              ! MRD = SDI RELATIVE DENSITY AND LCR = LIVE CROWN RATIO

              WEIBB = 7.1386 - (0.0608 * (HT(I) / 3.28))
              WEIBC = 3.3126 - (0.0214 * (HT(I) / 3.28)) -&
                   &(1.1622 * MRD)

              ! DETERMINE THE ADJUSTMENT FOR THE WEIBULL TRUNCATION

              WTRADJ = 1 - EXP(-((10/WEIBB)**WEIBC))
              !              WRITE(20,*) 'WEIBB ,', WEIBB
              !              WRITE(20,*) 'WEIBC ,', WEIBC
              !              WRITE(20,*) 'WTRADJ ,', WTRADJ

              ! DETERMINE THE AMOUNT OF FOLIAGE IN EACH 1 FOOT SECTION OF CROWN.
              ! SCALE FEET TO A CONTIOUS X VARIABLE FROM 1 TO 10 FOR WEIBULL.
              ! DETERMINE THE PROPORTION OF CROWN MASS WITHIN EACH SECTION BY
              ! SUBTRACTION. MULTIPLY THE PROPORTION BY TOTAL CROWN MASS
              !   TSCL = CROWN LENGTH
              !   SECINT = INTERVAL FOR 1 FOOT SECTION BOUNDARIES SCALED TO 10
              !   SECBND = 1 FOOT SECTION BOUNDARIES SCALED TO 10
              !   WPROP = PROPORTION OF TOTAL FUEL MASS IN A 1 FOOT SECTION
              !   K = COUNTER (NOT USED BUT HELPS INTERPRET OUTPUT

              ! SET VARIABLES

              TSCL = I2-I1
              SECINT = 10.0 / (TSCL + 1)
              do J = 1,400
                 WPROP(J) = 0.0
                 ADCRN(J) = 0.0
              end do
              ADCRWN = 0
              SECBND = 0
              K = 1
              !              WRITE(20,*) 'TSCL ,', TSCL
              !              WRITE(20,*) 'SECINT ,', SECINT

              ! COMPUTE BIOMASS FOR EACH FOOT OF TREE CROWN

              do J = I2,I1,-1
                 SECBND = SECBND + SECINT
                 if (J.eq.I2) then
                    WPROP(J) = (1 - EXP(-((SECBND/WEIBB)**WEIBC)))
                 else
                    WPROP(J) = (1 - EXP(-((SECBND/WEIBB)**WEIBC))) -&
                         &(1 - EXP(-(((SECBND-SECINT)/WEIBB)**WEIBC)))
                 end if
                 !                WRITE(20,*) 'K ,', K
                 K = K + 1
                 ADCRN(J) = (((CROWNW(I,0) + CROWNW(I,1) * 0.5) *&
                      &FMPROB(I)) * WPROP(J)) / WTRADJ
                 ADCRWN = ADCRWN + ADCRN(J)
                 !                WRITE(20,*) 'J ,', J
                 !                WRITE(20,*) 'SECBND ,', SECBND
                 !                WRITE(20,*) 'WPROP ,', WPROP(J)
                 !                WRITE(20,*) 'ADCRN ,', ADCRN(J)
              end do
              !                WRITE(20,*) 'ADCRWN,', ADCRWN

              ! ADD TREE CROWN/FOOT TO STAND TOTAL

              if (I1.le.I2.and. ADCRWN .gt.0.0) then
                 do J = I1,I2
                    CRFILL(J) = CRFILL(J) + ADCRN(J)
                 end do
              end if
           end if
        end if
     else
        if (DEBUG) write (JOSTND,11) I,HT(I),FMICR(I)
        11 format (' FMPOCR, I=',I4,' HT=',F7.2,' FMICR=',I3)
     end if
  end do

  !     SUM UP THE TOTAL CANOPY FUEL LOAD

  TCLOAD = 0.0 ! THIS SUM WILL BE IN LBS/ACRE
  do I = 1,400
     TCLOAD = TCLOAD + CRFILL(I)
  end do
  TCLOAD = TCLOAD / 43560.0 ! CONVERT LBS/ACRE TO LBS/SQFT
  if (DEBUG) write (JOSTND,*) 'TCLOAD = ', TCLOAD

  !     PASS CANOPY PROFILE INFORMATION TO DATABASE TABLE, IF REQUESTED.
  !     ONLY PRINT IT FOR POST-ACTIVITY VALUES.

  if (DEBUG) write (JOSTND,*) 'IN FMPOCR: ',&
       &'ICFPB=',ICFPB,' ICALL=',ICALL,' IYR=',IYR

  if (ICFPB .eq.9999) goto 14
  if (ICALL .eq.2) call DBSFMCANPR(IYR, CRFILL, NPLT)
  14 continue

  !     CALCULATE THE 13-FT RUNNING AVERAGE. THE MAX VALUE OF THE MEAN
  !     IS THE CROWN BULK DENSITY. THE LOWEST HEIGHT AT WHICH
  !     MEAN > 30 LBS/ACRE-FT (CBHCUT) IS THE CROWN BASE HEIGHT

  CBD = 0.0
  ACTCBH = -1
  MXJ = -1
  ABOTMX = 0.0

  !     FIRST, FIND THE HEIGHT WERE THE CROWN "STARTS".  THIS IS THE
  !     LOWEST POINT WERE AT LEAST 5 LBS/ACRE-FT EXIST.  WE WILL START
  !     START THE RUNNING AVERAGE AT THAT POINT. ALSO FIND THE POINT WHERE
  !     THE CROWN "ENDS".  THIS IS A COMPLIMENT TO THE STARTING RULE.

  J1 = 0
  do J = 1,400
     if (CRFILL(J) .gt.5.0) then
        J1 = J
        exit
     end if
  end do

  !     IF J1 IS STILL 0, THEN NO 1-FOOT INTERVAL HAS MORE THAN 5 LBS.,
  !     THEREFORE THE BOTTOM OF THE LIVE CROWN CAN NOT BE COMPUTED.

  if (J1.eq.0) goto 80

  !     FIND THE EFFECTIVE TOP OF THE CANOPY.

  J2 = 201
  do J = 400,1,-1
     if (CRFILL(J) .gt.5.0) then
        J2 = J
        exit
     end if
  end do

  !     BOTTOM AND TOP ARE THE SAME PLACE.  SET THE BOTTOM AND CBD.

  if (J1.eq. J2) then
     CBD = CRFILL(J1)
     if (CRFILL(J1) .ge.5.0) ACTCBH = J1
     goto 80
  end if

  !     START THE CODE TO COMPUTE THE RUNNING AVERAGE, 6 ABOVE AND 6 BELOW.

  do J = J1,J2

     I1 = J-6
     if (I1.lt. J1) I1 = J1
     I2 = J+6
     if (I2.gt. J2) I2 = J2

     A = 0.0
     do I = I1,I2
        A = A + CRFILL(I)
     end do
     A = A / FLOAT(I2-I1+1)

     !       FIND THE MAXIMUM CBD.

     if (A .gt. CBD) CBD = A

     !       FIND THE BOTTOM OF THE LIVE CROWN...COMPUTE A 3FT RUNNING
     !       AVERAGE...AND SAVE A POINTER TO THE MAX VALUE.

     I1 = J-1
     if (I1.lt. J1) I1 = J1
     I2 = J+1
     if (I2.gt. J2) I2 = J2

     ABOT = 0.0
     do I = I1,I2
        ABOT = ABOT + CRFILL(I)
     end do
     ABOT = ABOT / FLOAT(I2-I1+1)

     !        DON'T INCREMENT THE CROWN BASE UNLESS THE UPPER RUNNING
     !        AVERAGE IS 'SLIGHTLY' MORE THAN THE LOWER RUNNING AVERAGE.
     !        (THIS KEEPS THE CROWN BASE SET AT THE LOWEST VALUE IN THE
     !         RARE CASE THAT THE CROWN PROFILE IS UNIFORM OVER A RANGE
     !         OF HEIGHTS).

     !         IF (ABOT .GT. ABOTMX) THEN
     if (ABOT .gt. (ABOTMX+0.1)) then
        ABOTMX = ABOT
        MXJ = J
     end if

     if (ABOT .ge. CBHCUT .and. ACTCBH .eq. -1) ACTCBH = J

     if (DEBUG) write (JOSTND,20) J,CRFILL(J),A,CBD,ACTCBH,ABOT,MXJ
     20 format (' FMPOCR, J=',I3,' CRFILL=',F9.3,' A=',F9.3,&
          &' CBD=',F9.3,' ACTCBH=',I4,' ABOT=',F9.3,&
          &' MXJ=',I3)

  end do

  !     IF THE CROWN IS STILL NOT COMPUTED, PICK THE LOCATION OF THE MAX
  !     RUNNING AVERAGE SO LONG AS IT IS OVER 5.

  if (ACTCBH .eq. -1.and. ABOTMX .gt.5.0) ACTCBH = MXJ

  80 continue

  !     CHANGE THE CBD TO KG/M3:
  !     MULTIPLY BY LBTOKG AND DIVIDE BY (ACRETOM2 * FTTOM)

  CBD = CBD * 0.45359237 / (4046.856422 * 0.3048)

  !     CAP CBD AT 0.35 - S. REBAIN - SEPT 2005
  if (CBD .gt.0.35) CBD = 0.35

  if (DEBUG) write (JOSTND,90) CBD,ACTCBH
  90 format (' FMPOCR, CBD=',F8.4,' ACTCBH=',I4)

  return
end
