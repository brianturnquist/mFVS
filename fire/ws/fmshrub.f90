!=======================!
!   fire/ws : fmshrub   !
!=======================!
! FIRE-WS $Id$
subroutine FMSHRUB (IYR,FMD)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMCFMD IN CA VARIANTS (WS/NC/IC)

  !     PURPOSE:

  !     A POOR MAN'S SHRUB MODEL:

  !     THIS ROUTINE IS CALLED AFTER THE COMPUTATIONS DONE BY **FMDYN**, AND
  !     DETECTS A SWITCH FROM FM8/9 TO FM5/25 IN A YEAR IN WHICH A MANAGEMENT OR
  !     FIRE DISTURBANCE OCCURS. THE SWITCH TO A BRUSH MODEL IS DELAYED AS FOLLOWS

  !     FM8/9 -> FM5  -- SPREAD OVER 5 YRS
  !     FM8/9 -> FM26 -- SPREAD OVER 15 YRS; GOING TO FM5 AT 5 YRS AND THEN
  !                      TO FM26 AT 15 YRS

  !     RATIO OF 8/9 IS STORED AND USED FOR SHIFTING PROPORTIONALLY WHEN
  !     BOTH ARE PRESENT. THE ACTUAL AMOUNT TO REARRANGE CHANGES WITH THE
  !     PREDICTED AMOUNT OF THE SHRUB MODEL (5 AND/OR 26). SINCE FUEL
  !     MODEL WEIGHT AND NUMBER MAY CHANGE WHEN THIS ROUTINE IS CALLED
  !     FWT(), FMOD() & NFMODS MUST ALL BE RE-COMPUTED. A CUT/PASTE FROM
  !     **FMDYN** IS USED TO DO THIS

  !  CALL LIST DEFINITIONS:
  !     IYR:     CURRENT YEAR
  !     FMD:     FUEL MODEL NUMBER

  !.... PARAMETER INCLUDE FILES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... COMMON INCLUDE FILES.

  include 'FMFCOM.f90'
  include 'FMCOM.f90'

  ! Variables
  logical :: L89,L5,L26,L526
  logical :: FMCHKFWT
  integer :: IYR,FMD
  integer :: I,J,K,JYR
  integer :: I5,I8,I9,I26
  integer :: FMOD2(MXFMOD+3),INDX(MXFMOD+3)
  real    :: R8,R9,FWT5,FWT8,FWT9,X
  real    :: XV1(2),XV2(2),YV(2),FWT2(MXFMOD+3)
  real    :: ALGSLP

  data     XV1 / 0.0, 5.0 /
  data     XV2 / 5.0,15.0 /
  data     YV  / 1.0, 0.0 /

  L89 = .false.
  L5 = .false.
  L26 = .false.
  L526 = .false.

  I5 = 0
  I8 = 0
  I9 = 0
  I26 = 0

  R8 = 0.0
  R9 = 0.0

  do I = 1,MXFMOD+3
     FMOD2(I) = 0
     FWT2(I) = 0.0
  end do

  do I = 1,MXFMOD
     if (FMOD(I) .eq.5) then
        L5 = .true.
        L526 = .true.
        I5 = I
     end if
     if (FMOD(I) .eq.8) then
        L89 = .true.
        I8 = I
     end if
     if (FMOD(I) .eq.9) then
        L89 = .true.
        I9 = I
     end if
     if (FMOD(I) .eq.26) then
        L26 = .true.
        L526 = .true.
        I26 = I
     end if
  end do

  !     SET YEAR IN WHICH FM CHANGE BEGINS: MUST HAVE 5/26 IN
  !     CURRENT YR AND HAVE BEEN FM8/9 WITH NO 5/26 IN PREV YR;
  !     AT LEAST 10% CHANGE IN %CC MUST ALSO HAVE TAKEN PLACE

  if ((CCCHNG .ge. CCCRIT) .and. LPRV89.and. L526) then
     FM89YR = IYR
     LATSHRB = .true.
  end if

  !     RECORD FM8 AND FM9 WEIGHTS

  LPRV89 = L89.and. (.not. L526)
  if (LPRV89) then
     if (I8.gt.0) PRV8 = FWT(I8)
     if (I9.gt.0) PRV9 = FWT(I9)
  end if

  !     SHIFT MODELS IN "ACTIVE SHRUB" PERIOD

  if (LATSHRB) then

     FWT5 = 0.0 ! TEMP STORAGE FOR NEW WTS
     FWT8 = 0.0
     FWT9 = 0.0

     R8 = PRV8/(PRV8+PRV9) ! PROPN IN FM8
     R9 = 1.0 - R8 ! PROPN IN FM9

     JYR = IYR-FM89YR

     if (L5) then
        X = ALGSLP(AMAX0(MIN(JYR, 5),0),XV1,YV,2)
        FWT8 = FWT8 + (FWT(I5) * X * R8)
        FWT9 = FWT9 + (FWT(I5) * X * R9)
        FWT(I5) = FWT(I5) - (FWT(I5) *  X)
     end if
     if (L26) then
        if (JYR .le.5) then
           X = ALGSLP(AMAX0(MIN(JYR, 5),0),XV1,YV,2)
           FWT8 = FWT8 + (FWT(I26) * X * R8)
           FWT9 = FWT9 + (FWT(I26) * X * R9)
           FWT5 = FWT5 + (FWT(I26) * (1.0 - X))
           FWT(I26) = 0.0
        else
           X = ALGSLP(AMAX0(MIN(JYR,15),0),XV2,YV,2)
           FWT5 = FWT5 + (FWT(I26) * X)
           FWT(I26) = FWT(I26) - (FWT(I26) * X)
        end if
     end if

     !       RECONFIGURE THE FMOD AND FWT ARRAYS: SOME SLOTS
     !       PREVIOUSLY OCCUPIED ARE NOW EMPTY, AND V/VERSA. BEGIN BY
     !       MOVING WEIGHT TO ANY MODELS ALREADY IN THE FMOD LIST

     if (I5.ne.0) then
        FWT(I5) = FWT(I5) + FWT5
        FWT5 = 0.0
     end if
     if (I8.ne.0) then
        FWT(I8) = FWT(I8) + FWT8
        FWT8 = 0.0
     end if
     if (I9.ne.0) then
        FWT(I9) = FWT(I9) + FWT9
        FWT9 = 0.0
     end if

     !       MOVE ALL NON-ZERO FWT/FMOD, LEAVING ALL OPEN SPOTS AT THE
     !       BOTTOM OF THE ARRAY

     K = 0
     do I = 1,MXFMOD
        if (FMOD(I) .eq.0.or. FWT(I) .eq.0.0) goto 60
        K = K + 1
        if (I .ne. K .and. FMCHKFWT(K)) then
           FMOD(K) = FMOD(I)
           FWT(K) = FWT(I)
           FMOD(I) = 0
           FWT(I) = 0.0
        end if
        60 continue
     end do

     !       COPY CURRENT MODELS TO TOP OF TMP ARRAY;
     !       THEN NEW MODELS AT END

     do I = 1,MXFMOD
        FMOD2(I) = FMOD(I)
        FWT2(I) = FWT(I)
     end do
     if (FWT5.gt.0.0) then
        I = MXFMOD+1
        FMOD2(I) = 5
        FWT2(I) = FWT5
     end if
     if (FWT8.gt.0.0) then
        I = MXFMOD+2
        FMOD2(I) = 8
        FWT2(I) = FWT8
     end if
     if (FWT9.gt.0.0) then
        I = MXFMOD+3
        FMOD2(I) = 9
        FWT2(I) = FWT9
     end if

     !       SORT BY WEIGHT; LAST 3 ARE DROPPED

     call RDPSRT (MXFMOD+3,FWT2,INDX,.true.)
     do I = 1,MXFMOD
        J = INDX(I)
        FMOD(I) = FMOD2(J)
        FWT(I) = FWT2(J)
     end do

     !       BEGINNING OF COPY FROM **FMDYN**

     !       TRUNCATE NUMBER OF FUEL MODELS TO 4 AND REWEIGHT,
     !       IN THE RARE CASE THAT >4 MODELS HAVE BEEN SELECTED
     !       THIS SOMETIMES HAPPENS IN THE EC, WITH THE 5TH MODEL
     !       WEIGHT AT 2-3% OF THE TOTAL.
     !       (CONVERSATION WITH NICK; 3 DECEMBER 2002)

     X = 0.0
     do I = 1,4
        X = X + FWT(I)
     end do
     if (X .gt.1.0E-6) then
        do I = 1,4
           FWT(I) = FWT(I) / X
        end do
        do I = 5,MXFMOD
           FMOD(I) = 0
           FWT(I) = 0.0
        end do
     end if

     !       SELECT NON-DYNAMIC MODEL BASED ON MOST HEAVILY WEIGHTED MODEL
     !       `FAILSAFE IS TO NAME MODEL 8 AND ISSUE A WARNING

     FMD = -1
     if (FWT(1) .gt.1.0E-6) FMD = FMOD(1)
     if (FMD .lt.0) then
        FMOD(1) = 8
        FWT(1) = 1.0
        NFMODS = 1
        do I = 2,MXFMOD
           FMOD(I) = 0
           FWT(I) = 0.0
        end do
        call RCDSET(2, .true.)
     end if

     !       COMPUTE THE NUMBER OF ACTIVE MODELS

     do I = 1,MXFMOD
        if (FWT(I) .le.1.0E-6) then
           NFMODS = I-1
           exit
        end if
     end do
     NFMODS = MIN(NFMODS,4)

     !       IF STATIC FUEL MODEL IS DESIRED, UNSET ALL OTHER WEIGHTS

     if (.not. LDYNFM) then
        FMOD(1) = FMD
        FWT(1) = 1.0
        NFMODS = 1
        do I = 2, MXFMOD
           FMOD(I) = 0
           FWT(I) = 0.0
        end do
     end if

     !       END OF COPY FROM **FMDYN**

     !       TURN OFF SHRUB MODEL WHEN THERE ARE NO MORE SPECIAL
     !       WEIGHTS TO PROCESS

     if (LATSHRB .and. ((FWT5+FWT8+FWT9) .le.0.0)) then
        LATSHRB = .false.
     end if

  end if

  return
end
