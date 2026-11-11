!===================!
!   wwpb : bmfint   !
!===================!
! WWPB $Id$
subroutine BMFINT (BYRAM, FLAME, ISTD)

  !     CALLED FROM BMFIRE

  !  **BMFINT--WS  DATE OF LAST REVISION:  JUNE 13, 1994

  !  PURPOSE:
  !    CALCULATION OF BYRAMS INTENSITY FOR USE BY THE MORTALITY MODEL.
  !    THIS SUBROUTINE HAS BEEN COPIED FROM THE SUBROUTINE
  !    FIREMD IN THE MODEL FIRESUM. KEANE ET AL, 1989

  !  CALL LIST DEFINITIONS:
  !     BYRAM:  BYRAMS INTENSITY, BTU/MIN/FT OF FIRELINE LEGTH
  !     FLAME:  FLAME LENGTH IN METERS

  !  LOCAL VARIABLE DEFINITIONS:
  !     I:      FUEL CATEGORY: 1=DEAD, 2=LIVE
  !     J:      SIZE CLASS WITHIN CATEGORY (<100)

  !  COMMON BLOCK VARIABLES AND PARAMETERS:

  !.... PARAMETER INCLUDE FILES.
  ! Common blocks
  include 'PPEPRM.f90'

  !.... COMMON INCLUDE FILES.
  include 'BMFCOM.f90'

  !.... VARIABLE DECLARATIONS.

  ! Variables
  integer :: ISIZE(2,7),IFINES(2),LARGE1,LARGE2
  real    :: LHV,ST,SE
  real    :: AI(2),BSE(2),SIGMA1(2),WO1(2),A(2,7),F(2,7)
  real    :: QIG(2,7),BARNS(2),FX(2),WO(2,7)
  real    :: LHV1,BETA1,SIGMA,GAMMA,XIR,RHOBQIG,PHIS
  real    :: XIO,MCSA(2),MDCSA(2),IR(2)
  real    :: G(2,7),GS(2,7),NOCLAS(2),BULK(2,7)

  !.... DATA STATEMENTS.

  !.... CHECK FOR DEBUG.

  !.... BEGIN ROUTINE

  !.... INITIALIZE ALL VARIABLES AND PARAMETERS

  !     TURN SLOPE INTO A PROPORTION IF NECESSARY
  if (SLP(ISTD) .gt.1.0) then
     TTHETA = SLP(ISTD) / 100.0
  else
     TTHETA = SLP(ISTD)
  end if

  !.... THESE NEXT FOUR PARAMETERS ARE THE SAME FOR ALL FIRE MODELS
  !     IN THIS VERSION
  !     [MAYBE THEY SHOULD BE SET IN THEIR OWN PARAMETER COMMON?]

  RHOP = 32
  LHV = 8000
  ST = .0555
  SE = .01

  LARGE1 = ND
  LARGE2 = NL
  IFINES(1) = 1

  if (NL .gt.0) then
     IFINES(2) = 1
  else
     IFINES(2) = 0
  end if

  NOCLAS(1) = ND
  NOCLAS(2) = NL

  !     ZERO ALL WORKING VARIABLES
  do I = 1,2
     do J = 1,7
        BULK(I, J) = FWG(I, J) / DEPTH
        ISIZE(I, J) = J
        G(I,J) = 0.0
        GS(I,J) = 0.0
        A(I,J) = 0.0
        F(I,J) = 0.0
        WO(I,J) = 0.0
        QIG(I,J) = 0.0
        110 continue
     end do
     AI(I) = 0.0
     MCSA(I) = 0.0
     BSE(I) = 0.0
     SIGMA1(I) = 0.0
     WO1(I) = 0.0
     IR(I) = 0.0
     BARNS(I) = 0.0
     FX(I) = 0.0
     100 continue
  end do
  BYRAM = 0.0
  RATE = 0.0
  GAMMA = 0.0
  SIGMA = 0.0
  AT = 0.0
  LHV1 = 0.0
  FLAG(1) = 0.0
  FLAG(2) = 0.0
  FLAG(3) = 0.0
  SUM1 = 0.0
  SUM2 = 0.0
  SUM3 = 0.0
  SUM4 = 0.0

  !.... SORT FUEL COMPONENTS BY SIZE, FINEST FUELS FIRST

  do I = 1,2
     JMAX = NOCLAS(I)
     if (JMAX .gt.1) then
        JMM = JMAX - 1
        do J = 1,JMM
           KM = JMAX - J
           do K = 1,KM
              IDA = ISIZE(I, K)
              IDB = ISIZE(I, K + 1)
              SIZA = MPS(I, IDA)
              SIZB = MPS(I, IDB)
              if (SIZA .lt. SIZB) then
                 ISIZE(I, K + 1) = IDA
                 ISIZE(I, K) = IDB
              end if
              215 continue
           end do
           210 continue
        end do
     end if
     200 continue
  end do

  !     DELETE LARGE LOGS FROM FIRESPREAD CONSIDERATIONS
  do I = 1,2
     KMAX = NOCLAS(I)
     if (KMAX .lt.1) goto 205
     do K = 1,KMAX
        J = ISIZE(I,K)
        if (MPS(I,J) .ge.16.0) goto 202
        NOCLAS(I) = K-1
        goto 205
        202 continue
     end do
     205 continue
  end do

  !     CALCULATE WEIGHTING FACTORS FIRST FOR DEAD FUELS, THEN FOR LIVE FUELS
  N1 = NOCLAS(1)
  N2 = NOCLAS(2)
  NOCLAS(1) = MIN0(LARGE1,N1)
  NOCLAS(2) = MIN0(LARGE2,N2)

  do I = 1,2
     KMIN = IFINES(I)
     KMAX = NOCLAS(I)
     if ((KMAX .ne.0) .and. (KMIN .le. KMAX)) then
        do K = KMIN,KMAX
           J = ISIZE(I, K)
           GS(I, J) = MPS(I, J) / RHOP
           A(I, J) = FWG(I, J) * GS(I, J)
           GS(I, J) = EXP(-138.0 / (MPS(I, J) + 1E-9))
           AI(I) = AI(I) + A(I, J)
           WO(I, J) = FWG(I, J) * (1 - ST)
           310 continue
        end do
        do K = KMIN,KMAX
           J = ISIZE(I, K)
           if (AI(I) .ne.0) F(I, J) = A(I, J) / AI(I)
           320 continue
        end do
     end if
     300 continue
  end do
  AT = AI(1) + AI(2)
  FX(1) = AI(1) / (AT + 1E-9)
  FX(2) = 1 - FX(1)

  !.... FIND WEIGHT LOADING OF DEAD AND LIVE FINES, MOISTURE EXTINCT
  !     NOTE DEAD AND LIVE FUELS WTD BY EXP(-C/SIGMA) WHERE C=138 OR 500
  FINED = 0.0
  FINEL = 0.0
  WDFMN = 0.0
  FINDM = 0.0
  do I = 1,2
     N = IFINES(I)
     JM = NOCLAS(I)
     if ((JM .gt.0) .and. (N .le. JM)) then
        if (I .eq.1) then
           do J = N,JM
              JJ = ISIZE(I, J)
              SA = MPS(I, JJ)
              if (SA .ne.0) then
                 EP = EXP(-138.0 / SA)
              else
                 EP = 0
              end if
              WTFAC = FWG(I, JJ) * EP
              WMFAC = WTFAC * MOIS(I, JJ)
              FINED = FINED + WTFAC
              WDFMN = WDFMN + WMFAC
              410 continue
           end do
           if (FINED .ne.0) FINDM = WDFMN / FINED
        else if (I .eq.2) then
           do J = N,JM
              JJ = ISIZE(I, J)
              SA = MPS(I, JJ)
              if (SA .ne.0) then
                 EP = EXP(-500.0 / SA)
              else
                 EP = 0.0
              end if
              FINEL = FINEL + FWG(I, JJ) * EP
              420 continue
           end do
        end if
     end if
     400 continue
  end do
  if (FINEL .ne.0.0) then
     FACTOR = FINED / FINEL
     XMOISL = 2.9 * FACTOR * (1 - FINDM / (MEXT(1) + 1E-9)) - 0.226
     if (XMOISL .lt. MEXT(1)) XMOISL = MEXT(1)
  else
     XMOISL = 100
  end if
  MEXT(2) = XMOISL

  !.... INTERMEDIATE COMPUTATIONS FOR EACH CATEGORY OF FUEL (DEAD, LIVE)
  do I = 1,2
     AA1 = 0.0
     AA2 = 0.0
     AA3 = 0.0
     AA4 = 0.0
     AA5 = 0.0
     JM = NOCLAS(I)
     N = IFINES(I)
     if ((JM .ne.0) .and. (N .le. JM)) then
        do K = N,JM
           J = ISIZE(I, K)
           AX = F(I, J)
           SIGM = MPS(I, J)
           if (SIGM .lt.48.0) then
              AA5 = AA5 + A(I, J)
           else if (SIGM .lt.96.0) then
              AA4 = AA4 + A(I, J)
           else if (SIGM .lt.192.0) then
              AA3 = AA3 + A(I, J)
           else if (SIGM .lt.1200.0) then
              AA2 = AA2 + A(I, J)
           else
              AA1 = AA1 + A(I, J)
           end if
           QIG(I, J) = 250.0 + 1116.0 * MOIS(I, J)
           MCSA(I) = MCSA(I) + AX * MOIS(I, J)
           BSE(I) = BSE(I) + AX * SE
           SIGMA1(I) = SIGMA1(I) + AX * MPS(I, J)
           LHV1 = LHV1 + AX * LHV
           SUM4 = SUM4 + BULK(I, J) * FWG(I, J)
           SUM1 = SUM1 + FWG(I, J)
           SUM2 = SUM2 + FWG(I, J) / RHOP
           SUM3 = SUM3 + FX(I) * F(I, J) * QIG(I, J) * GS(I, J)
           510 continue
        end do
        do K = N,JM
           J = ISIZE(I, K)
           SIGM = MPS(I, J)
           if (AI(I) .ne.0.0) AI(I) = AI(I) + 1E-9
           if (SIGM .lt.48.0) then
              G(I, J) = AA5 / AI(I)
           else if (SIGM .lt.96.0) then
              G(I, J) = AA4 / AI(I)
           else if (SIGM .lt.192.0) then
              G(I, J) = AA3 / AI(I)
           else if (SIGM .lt.1200.0) then
              G(I, J) = AA2 / AI(I)
           else
              G(I, J) = AA1 / AI(I)
           end if
           WO1(I) = WO1(I) + G(I, J) * WO(I, J)
           520 continue
        end do
        BETA = MCSA(I) / (MEXT(I) + 1E-9)
        MDCSA(I) = 1 - BETA * (2.59 - BETA&
             &* (5.11 - BETA * 3.52))
        if (MEXT(I) .lt. MCSA(I)) MDCSA(I) = 0
        if (BSE(I) .ne.0.0) BARNS(I) = .174 / (BSE(I) ** .19)
        if (BARNS(I) .gt.1.0) BARNS(I) = 1.0
        SIGMA = SIGMA + FX(I) * SIGMA1(I)
        IR(I) = WO1(I) * LHV1 * MDCSA(I) * BARNS(I)
     end if
     500 continue
  end do

  !.... DEAD FUEL TOO MOIST TO SPREAD FLAME

  if (MDCSA(1) .le.0.0) FLAG(1) = 1

  !.... BEGIN FINAL COMPUTATIONS
  !     ONLY DO FINAL CALCS IF MDCSA(1) > 0

  if (MDCSA(1) .gt.0.0) then

     !....    BULK DENSITY
     RHOP1 = SUM4 / (SUM1 + 1E-9)

     !....    PACKING RATIO
     BETA1 = SUM2 * RHOP1 / (SUM1 + 1E-9)

     !....    OPTIMUM PACKING RATIO
     if (SIGMA .eq.0.0) SIGMA = 1E-9
     BEST = 3.348 / (SIGMA ** 0.8189)
     RAT = BETA1 / (BEST + 1E-9)

     !....    REACTION INTENSITY WEIGHTED BY SURFACE AREA FRACTION
     A1 = 133.0 / (SIGMA ** 0.7913)
     V = SIGMA ** 1.5
     GAMMA = (V * (RAT ** A1) * EXP(A1 * (1 - RAT)))&
          &/ (495.0 + .0594 * V)
     IR(1) = GAMMA * IR(1)
     IR(2) = GAMMA * IR(2)
     XIR = IR(1) + IR(2)

     !....    HEAT SINK TERMS

     RHOBQIG = RHOP1 * SUM3

     !....    PROPAGATING INTENSITY

     B = (0.792 + 0.681 * SQRT(SIGMA)) * (0.1 + BETA1)
     XIO = (XIR * EXP(B)) / (192.0 + 0.2595 * SIGMA)

     !....    SLOPE FACTOR PHIS

     if (BETA1.ne.0.0) PHIS = 5.275 * TTHETA * TTHETA&
          &/ (BETA1 ** 0.3)

     !....    PARAMETERS FOR DETERMINING WIND FACTOR PHIW

     XM1 = 0.02526 * (SIGMA ** 0.54)
     XN1 = 0.715 * EXP(-0.000359 * SIGMA)
     C1 = 7.47 * EXP(-0.133 * (SIGMA ** 0.55))
     if (RAT .ne.0.0) C1 = C1 / (RAT ** XN1)

     !....    A FEW LINES OF MAXES WERE LEFT OUT BECAUSE
     !        NOT NEEDED IN THIS VERSION

     WMAX = 0.9 * XIR
     W = FWIND * 88.0
     PHIW = C1 * (W ** XM1)
     R = XIO * (1.0 + PHIS + PHIW) / (RHOBQIG + 1E-9)
     BYRAM = XIR * R * 384.0 / SIGMA

     !....    SURF/VOL TOO SMALL

     if ((W .ne.0.0) .and. (SIGMA .lt.175.0)) FLAG(3) = 1

     !....    WIND SPEED EXCEEDS RELIABLE EXTRAPOLATION

     if (W .gt. WMAX) FLAG(2) = 1

  end if
  FLAME = 0.45 * (BYRAM / 60.0) ** 0.46

  return
end
