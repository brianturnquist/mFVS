!=================================!
!   Stand visualization: SVGTPL   !
!=================================!
! BASE $Id$
subroutine SVGTPL
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     J.J.MARCHINEK -- RMRS MOSCOW -- JANUARY 1999
  !     D. ROBINSON   -- ESSA        -- MAY 2005

  !     RETURNS THE LOWER LEFT AND UPPER RIGHT POINTS FOR A PLOT,
  !     GIVEN IPTINV PLOTS ARE ON ONE SQUARE ACRE...IN FEET!!!!

  !     OR IF THE PLOT IS CIRCULAR, RETURNS THE TWO DIFFERENT RADII
  !     AND ANGLES BOUNDING THE REGION

  !     NR IS THE NUMBER OF ROWS
  !     NC IS THE NUMBER OF COLUMNS
  !     NCF IS THE NUMBER OF THE FIRST COLUMN THAT CONTAINS AN
  !         EXTRA ROW...0 IF NONE CONTAIN EXTRA ROWS.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'PLOT.F77'
  include 'SVDATA.F77'
  include 'CONTRL.F77'
  include 'ARRAYS.F77'

  ! Variables
  integer :: NPLPRG(105)
  integer :: MPLPRG(105)
  real    :: SQFTAC,SQRTFA,RADPI,YLEN,XLEN,AP,YLEN1,XLEN1,YLEN2,XLEN2,APP,ANG
  real    :: SQFTACI,SQRTFAI
  real    :: SQFTACM,SQRTFAM
  integer :: I,NR,NC,NCF,IC,IR,K,IXN,N,J,NP,IPPC

  data RADPI   /3.14159/
  data SQFTACI /43560./, SQRTFAI /208.7103/
  data SQFTACM /10000./, SQRTFAM /100./

  if (IMETRIC.eq.0) then
     SQFTAC = SQFTACI
     SQRTFA = SQRTFAI
  else
     SQFTAC = SQFTACM
     SQRTFA = SQRTFAM
  end if

  !     THIS CALCULATES THE MAX OF ITRE() OR IPTINV, WHICH EVER IS GREATER
  !     THAT VALUE IS ASSIGNED TO ISVINV

  ISVINV = IPTINV
  if (ITRN.gt.0) then
     do I = 1,ITRN
        if (ITRE(I) .gt. ISVINV) ISVINV = ITRE(I)
     end do
  end if

  !     THIS ROUTINE IS FOR A SQUARE PLOT THAT IS SUBDIVIDED

  if (IPLGEM.eq.1) then
     do I = 1,ISVINV
        NR = IFIX(SQRT(FLOAT(ISVINV)))
        if (NR*NR .eq. ISVINV) then
           NC = NR
           NCF = 0
        else if (NR*(NR+1) .lt. ISVINV) then
           NC = NR+1
           NCF = ISVINV-(NC*NR)
           NCF = NC-NCF+1
        else
           NC = NR
           NCF = ISVINV-(NC*NR)
           NCF = NC-NCF+1
        end if

        !         IR IS THE ROW NUMBER FOR POINT I
        !         IC IS THE COLUMN NUMBER FOR POINT I

        if (NCF.eq.0) then
           IC = I/NR
           if (MOD(I,NR) .gt. 0) IC = IC+1
           IR = I-((IC-1)*NR)
        else if (I .le. NR*(NCF-1)) then
           IC = I/NR
           if (MOD(I,NR) .gt. 0) IC = IC+1
           IR = I-((IC-1)*NR)
        else
           K = I-(NR*(NCF-1))
           IC = K/(NR+1)
           if (MOD(K,(NR+1)) .gt. 0) IC = IC+1
           IR = K-((IC-1)*(NR+1))
           IC = IC+NCF-1
        end if

        if (NCF.eq.0) then
           YLEN = SQRTFA/FLOAT(NR)
           XLEN = SQRTFA/FLOAT(NC)
           X1R1S(I) = XLEN*(IC-1)
           X2R2S(I) = X1R1S(I)+XLEN
           Y1A1S(I) = YLEN*(IR-1)
           Y2A2S(I) = Y1A1S(I)+YLEN
        else
           AP = SQFTAC/ISVINV
           YLEN1 = SQRTFA/NR
           XLEN1 = AP/YLEN1
           if (IC.lt.NCF) then
              X1R1S(I) = XLEN1*(IC-1)
              X2R2S(I) = X1R1S(I)+XLEN1
              Y1A1S(I) = YLEN1*(IR-1)
              Y2A2S(I) = Y1A1S(I)+YLEN1
           else
              YLEN2 = SQRTFA/(NR+1)
              XLEN2 = AP/YLEN2
              X1R1S(I) = ((NCF-1)*XLEN1)+((IC-NCF)*XLEN2)
              X2R2S(I) = X1R1S(I)+XLEN2
              Y1A1S(I) = YLEN2*(IR-1)
              Y2A2S(I) = Y1A1S(I)+YLEN2
           end if
        end if
     end do

     !     THIS IS FOR A CIRCULAR PLOT THAT IS SUBDIVIDED

  else if (IPLGEM.eq.3) then

     if (ISVINV.gt.4) then

        !         FINDS THE NUMBER OF CIRCULAR RINGS IN AREA
        !         IXN IS THE NUMBER OF RINGS FOR THE PLOT

        IXN = IFIX(SQRT(FLOAT(ISVINV)))
        if (IXN*IXN.ne.ISVINV) IXN = IXN+1
        if (MOD(IXN,2).eq.0) then
           IXN = IXN+2
        else
           IXN = IXN+1
        end if
        IXN = IXN/2

        !         FINDS THE MAX NUMBER OF PLOTS PER RING
        !         MPLPRG IS THE ARRAY FOR THE MAX NUMBER OF PLOTS PER RING
        !         NPLPRG IS THE ARRAY FOR THE NUMBER OF PLOTS PER RING

        MPLPRG(1) = 1
        MPLPRG(2) = 8
        N = ISVINV
        do J = 3,IXN
           MPLPRG(J) = 8+MPLPRG(J-1)
        end do

        do J = 1,105
           NPLPRG(J) = 0
        end do

        do J = 1,(IXN-1)
           NPLPRG(J) = J*J
           N = N-NPLPRG(J)
        end do

        10 continue
        NPLPRG(IXN) = NPLPRG(IXN)+1
        N = N-1
        if (N.gt.0) then
           if (NPLPRG(IXN).lt.(NPLPRG(IXN-1)+8)) goto 10
        end if

        20 continue

        do J = 2,IXN
           if (N.gt.0) then
              if (NPLPRG(J).lt.MPLPRG(J)) then
                 NPLPRG(J) = NPLPRG(J)+1
                 N = N-1
              end if
           end if
        end do
        if (N.gt.0) goto 20

     else
        IXN = 1
        NPLPRG(1) = ISVINV
     end if

     !        NP IS THE PLOT NUMBER
     !        APP IS THE AREA PER PLOT
     !        IPPC IS THE PLOTS PER CIRCLE WITHIN THE WHOLE ACRE
     !        ANG IS THE ANGLE DIFFERENCE PER RING

     X1R1S(1) = 0
     Y1A1S(1) = 0
     NP = 1
     APP = SQFTAC/ISVINV
     if (IXN.gt.1) then
        Y2A2S(1) = 2*RADPI
        X2R2S(1) = SQRT(APP/RADPI)
        X1R1S(2) = X2R2S(1)
        Y1A1S(2) = 0
        NP = NP+1

        if (IXN.gt.2) then
           IPPC = 1
           do I = 2,(IXN-1)
              IPPC = IPPC+NPLPRG(I)
              X2R2S(NP) = SQRT((APP*IPPC)/RADPI)
              ANG = ((2*RADPI)/NPLPRG(I))
              Y2A2S(NP) = ANG
              do J = 1,NPLPRG(I)-1
                 X2R2S(NP+1) = X2R2S(NP)
                 X1R1S(NP+1) = X1R1S(NP)
                 Y2A2S(NP+1) = Y2A2S(NP)+ANG
                 Y1A1S(NP+1) = Y2A2S(NP)
                 NP = NP+1
              end do
              NP = IPPC+1
              X1R1S(NP) = X2R2S(NP-1)
              Y1A1S(NP) = 0
           end do
        end if
     end if

     if (IMETRIC.eq.0) then
        X2R2S(NP) = 117.7522
     else
        X2R2S(NP) = 56.42
     end if

     ANG = ((2*RADPI)/NPLPRG(IXN))
     Y2A2S(NP) = ANG
     do J = 1,NPLPRG(IXN)-1
        X2R2S(NP+1) = X2R2S(NP)
        X1R1S(NP+1) = X1R1S(NP)
        Y2A2S(NP+1) = Y2A2S(NP)+ANG
        Y1A1S(NP+1) = Y2A2S(NP)
        NP = NP+1
     end do

     !     THIS IS FOR A NON-SUBDIVIDED SQUARE PLOT

  else if (IPLGEM.eq.0) then
     do I = 1,ISVINV
        X1R1S(I) = 0
        Y1A1S(I) = 0
        if (IMETRIC.eq.0) then
           X2R2S(I) = 208.7103
           Y2A2S(I) = 208.7103
        else
           X2R2S(I) = 100.0
           Y2A2S(I) = 100.0
        end if
     end do

     !     THIS IS FOR A NON-SUBDIVIDED CIRCULAR PLOT

  else
     do I = 1,ISVINV
        X1R1S(I) = 0
        Y1A1S(I) = 0

        if (IMETRIC.eq.0) then
           X2R2S(I) = 117.7522
        else
           X2R2S(I) = 56.42
        end if

        Y2A2S(I) = 2*RADPI
     end do
  end if

  return
end subroutine SVGTPL
