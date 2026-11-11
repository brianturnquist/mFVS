!=======================!
!   fire/base : fmdyn   !
!=======================!
! FIRE-BASE $Id$
subroutine FMDYN (SM,LG,ITYP,XPTS,EQWT,IPTR,ICLSS,LDYN,FMD)
  implicit none

  !     CALLED FROM: FMCFMD

  !  PURPOSE:
  !     THIS SUBROUTINE RETURNS THE SET OF UP TO 4, (TYPICALLY 2) FUEL
  !     MODELS THAT WOULD BE USED IF THE DYNAMIC FUEL MODEL IS SELECTED

  !************DYNAMIC FUEL MODEL CALCULATIONS***************

  !     THIS ROUTINE FINDS A GROUP OF FIRE MODELS MOST APPROPRIATE TO
  !     THE INPUT COMBINATION OF LARGE AND SMALL FUEL COMPONENTS.

  !     THIS ROUTINE TAKES AS INPUT VARIABLES SMALL AND LARGE; A POINT
  !     IN A 2-SPACE, REPRESENTING SMALL AND LARGE FUEL. THE POINT IS
  !     COMPARED TO THE FUEL MODELS, EACH OF WHICH IS REPRESENTED BY A
  !     STRAIGHT LINE ENCODED WITH XPTS(). THE NEAREST NEIGHORING LINES
  !     ARE FOUND AND THE INVERSE OF THE DISTANCE TO THOSE LINES IS
  !     USED AS THE WEIGHT WHICH IS GIVEN FOR THE FUEL MODEL ASSOCIATED
  !     WITH THE LINE.

  !     USUALLY THERE WILL BE TWO NEIGHBORING LINES (ONE TO THE LEFT AND
  !     ONE TO THE RIGHT OF THE POINT). IN CASES WHERE THE POINT IS BEYOND
  !     THE UPPER/LOWER-MOST LINE, THERE WILL BE ONLY ONE NEIGHBOR, AND IN
  !     CASES WHERE HORIZONTAL, VERTICAL AND SLOPING LINES CROSS, THERE
  !     MAY BE AS MANY AS FOUR NEIGHBOURS. THE LAST CASE IS UNUSUAL AND
  !     WILL ONLY OCCUR IF THE MODEL WRITER KNOWINGLY MIXES IN THESE
  !     TYPES OF LINES.

  !     THE VECTORS FWT(5) AND FMOD(5) ARE PRODUCED AS OUTPUT. FOR EACH
  !     NON-ZERO FMOD(I) THERE WILL BE A NON-ZERO WEIGHT GIVEN TO THE
  !     CORRESPONDING FMOD() MODEL. WEIGHTS SUM TO 1.0

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  !     LOCAL VARIABLE DEFINITIONS:

  !     SM:         SMALL FUEL LOAD
  !     LG:         LARGE FUEL LOAD
  !     LOK:        LOGICAL: TRUE IF MODEL IS A CANDIDATE;OTHERWISE FALSE
  !     ITYP:       0 FOR SLANTED LINES; -1 FOR HORIZ; 1 FOR VERTICAL
  !     XPTS(-,1):  FOR EACH FUEL MODEL, X-INTERCEPT (VALUE OF SMALL WHEN LARGE=0)
  !     XPTS(-,2):  FOR EACH FUEL MODEL, Y-INTERCEPT (VALUE OF LARGE WHEN SMALL=0)
  !     EQMOD:      INDEX OF ANY COLLINEAR FUEL MODEL (OTHERWISE 0)
  !     EQWT:       WT OF ANY COLLINEAR FUEL MODEL (OTHERWISE 0)
  !     IPTR:       LABEL/TAG FOR EACH FIRE MODEL CLASS
  !     ICLSS:      LENGTH OF NUMEROUS VECTORS (IPTR,ITYP,EQMOD,EQWT,XPTS)
  !     LDYN:       IS DYNAMIC MODELLING NEEDED OR NOT
  !     FMD:        DOMINANT FUEL MODEL (ASSIGNED ON RETURN)

  ! Variables
  integer :: ICLSS
  integer :: I,II,J,K,K2
  integer :: IPTR,ITYP,FMD
  integer :: FMOD2,NBR(4),INDX(MXFMOD)
  integer :: EQMOD(ICLSS)
  real    :: SM,LG
  real    :: XPTS,XD,YD
  real    :: PT(2),NPT(2),M1,M2,B1,B2,XP,YP,XWT
  real    :: EQWT,FWT2
  real    :: PRV(4),WT(4)
  logical :: DEBUG,LOK,FMCHKFWT,LDYN

  dimension IPTR(ICLSS), ITYP(ICLSS), EQWT(ICLSS)
  dimension XPTS(ICLSS,2), XD(ICLSS), YD(ICLSS), LOK(ICLSS)
  dimension FWT2(MXFMOD), FMOD2(MXFMOD)

  call DBCHK (DEBUG,'FMDYN',5,ICYC)

  !     INITIALIZE WEIGHTS OF DYNAMIC FIRE MODELS

  do I = 1,4
     NBR(I) = 0
  end do

  do I = 1,MXFMOD
     FMOD(I) = 0
     FWT(I) = 0.
     FMOD2(I) = 0
     FWT2(I) = 0.
  end do

  do I = 1,ICLSS
     EQMOD(I) = 0
     LOK(I) = .false.
  end do

  PT(1) = SM
  PT(2) = LG

  !     EMPTY-HANDED IF EITHER SMALL OR LARGE IS NEGATIVE

  if (PT(1) .lt.0.0.or. PT(2) .lt.0.0) goto 999

  !     SET LOK() VECTOR

  do I = 1,ICLSS
     if (EQWT(I) .gt.0.0) LOK(I) = .true.
  end do

  !     UNSET LINES THAT ARE LOK, BUT WITH INVALID COEFFICIENTS

  do I = 1, ICLSS
     if (LOK(I)) then
        J = ITYP(I)
        if ((J .eq.0.and.&
             &(XPTS(I,1) .eq.0..or. XPTS(I,2) .eq.0.0)) .or.&
             &(J .eq. -1.and. XPTS(I,2) .eq.0) .or.&
             &(J .eq.1.and. XPTS(I,1) .eq.0)) then
           LOK(I) = .false.
           EQWT(I) = 0.0
        end if
     end if
  end do

  !     SET EQMOD() VECTOR

  do I = 1,ICLSS
     EQMOD(I) = 0
  end do
  do I = 1,ICLSS
     if (LOK(I)) then
        do J = I,ICLSS
           if (EQMOD(J) .eq.0.and.&
                &LOK(J) .and.&
                &XPTS(I,1) .eq. XPTS(J,1) .and.&
                &XPTS(I,2) .eq. XPTS(J,2)) EQMOD(J) = I
        end do
     end if
  end do

  !     RESCALE SO THAT THE COLINEAR EQWT ELEMENTS SUM TO 1.0

  do I = 1,ICLSS
     if (LOK(I)) then
        XWT = 0.0
        do J = I,ICLSS
           if (LOK(J) .and. EQMOD(J) .eq. I) XWT = XWT+ EQWT(J)
        end do
        if (XWT .gt.1.0E-6) then
           do J = I,ICLSS
              if (LOK(J) .and. EQMOD(J) .eq. I) EQWT(J) = EQWT(J)/XWT
           end do
        end if
     end if
  end do

  !     COMPUTE VECTOR XD() CONTAINING DISTANCE TO THE LEFT (<0) AND RIGHT
  !     (>0) LINES RELATIVE TO THE SAMPLE POINT. DO THE SAME WITH YD() FOR
  !     UPPER (>0) AND LOWER (<0) SAMPLE POINTS. IN WELL-BEHAVED SITUATIONS
  !     THEY WILL BE THE SAME, BUT TO HANDLE THE HORIZONTAL AND VERTICAL
  !     CASES, THE TWO SEARCHES ARE REQUIRED. THERE ARE CASES (WITH ISOLINES
  !     THAT CROSS IN A COMPLICATED WAY) THAT WILL NOT BE DONE PROPERLY:
  !     THESE WOULD REQUIRE A SMARTER NEIGHBOR SEARCH. IN SOME CASES THE
  !     SAMPLE POINT WILL BE THE LARGEST OR SMALLEST; THESE CORRESPOND TO
  !     BEING ABOVE OR BELOW THE GREATEST OR SMALLEST ISOLINE.

  !     XD() HOLDS DISTANCE ON X-AXIS FROM POINT TO WHERE IT CROSSES EACH LINE
  !     IF <0, THEN TO THE LEFT; >0 THEN TO THE RIGHT; HORIZONTAL/VERTICAL CASES
  !     ARE GIVEN AN ARBITRARY LARGE NUMBER

  do I = 1, ICLSS
     if (.not. LOK(I)) goto 20
     J = ITYP(I)
     XD(I) = 0.
     YD(I) = 0.
     if (J .eq.0) then
        M1 = XPTS(I,2) / (-XPTS(I,1))
        B1 = XPTS(I,2)
        XP = (PT(2) - B1 ) / M1
        YP = M1 * PT(1) + B1
        XD(I) = XP - PT(1)
        YD(I) = YP - PT(2)
     else if (J .eq.1) then ! VERTICAL
        XD(I) = XPTS(I,1) - PT(1)
     else if (J .eq. -1) then ! HORIZONTAL
        YD(I) = XPTS(I,2) - PT(2)
     end if
     20 continue
  end do

  !     FIND CLOSEST NEIGHBORS IN LEFT/RIGHT AND UP/DOWN DIRECTION
  !     AND STORE INDICES IN NBR() ARRAY. DUPLICATES ARE OK

  PRV(1) = -9.99E+30
  PRV(2) = 9.99E+30
  PRV(3) = -9.99E+30
  PRV(4) = 9.99E+30

  do I = 1,ICLSS
     if (.not. LOK(I)) goto 22
     J = ITYP(I)
     if (J .eq.0.or. J .eq.1) then ! NORMAL OR VERTICAL
        if (XD(I) .lt.0..and. XD(I) .gt. PRV(1)) then
           PRV(1) = XD(I)
           NBR(1) = I
        else if (XD(I) .ge.0..and. XD(I) .lt. PRV(2)) then
           PRV(2) = XD(I)
           NBR(2) = I
        end if
     end if
     if (J .eq.0.or. J .eq. -1) then ! NORMAL OR HORIZONTAL
        if (YD(I) .lt.0..and. YD(I) .gt. PRV(3)) then
           PRV(3) = YD(I)
           NBR(3) = I
        else if (YD(I) .ge.0..and. YD(I) .lt. PRV(4)) then
           PRV(4) = YD(I)
           NBR(4) = I
        end if
     end if
     22 continue
  end do

  !     FIND DISTANCE FROM TRIAL POINT TO EACH DEFINED LINE;
  !     FIND THE NEAREST TWO LINES IF WITHIN THE DOMAIN; OTHERWISE
  !     FIND THE NEAREST ONE LINE IF OUTSIDE THE DOMAIN

  !     M1 = SLOPE OF ISOLINE
  !     B1 = INTERCEPT OF "ISOLINE"
  !     M2 = SLOPE OF LINE NORMAL ISOLINE
  !     B2 = INTERCEPT OF LINE NORMAL TO "ISOLINE"

  !     DISTANCE IS PLACE IN WT() VECTOR

  do K = 1,4
     I = NBR(K)
     if (I .eq.0) goto 30
     if (.not. LOK(I)) goto 30
     J = ITYP(I)
     if (J .eq.0) then
        M1 = XPTS(I,2) / (-XPTS(I,1))
        B1 = XPTS(I,2)
        M2 = -(1./M1)
        B2 = PT(2) - (M2 * PT(1))
        NPT(1) = (B2 - B1) / (M1 - M2)
        NPT(2) = M2 * NPT(1) + B2
        WT(K) = SQRT((PT(2) - NPT(2))**2 + (PT(1) - NPT(1))**2)
     else if (J .eq.1) then ! VERTICAL
        WT(K) = ABS(XPTS(I,1) - PT(1))
     else if (J .eq. -1) then ! HORIZONTAL
        WT(K) = ABS(XPTS(I,2) - PT(2))
     end if
     30 continue
  end do

  !     MERGE DUPLICATE INDICES; WHEN FINISHED, K IS THE NEXT
  !     OPEN POSITION IN THE FWT/FMOD ARRAYS

  K2 = 0
  do I = 1,4
     if (NBR(I) .eq.0) goto 40
     K2 = K2 + 1
     do J = 1, I
        if (FMOD(J) .eq. NBR(I)) then
           K = J
           goto 42
        end if
        41 continue
     end do
     K = K2
     if (FMCHKFWT(K)) FMOD(K) = NBR(I)
     42 if (FMCHKFWT(K)) FWT(K) = FWT(K) + WT(I)
     40 continue
  end do

  !     REWEIGHT BY INVERSE OF DISTANCE

  XWT = 0.
  do I = 1,MXFMOD
     if (FMOD(I) .eq.0) goto 50
     FWT(I) = 1.0/(FWT(I) + 1.0E-6)
     XWT = XWT + FWT(I)
     50 continue
  end do
  do I = 1, MXFMOD
     if (FMOD(I) .eq.0) goto 55
     FWT(I) = FWT(I) / XWT
     55 continue
  end do

  !     MOVE ALL NON-ZERO FWT/FMOD, LEAVING ALL OPEN SPOTS
  !     AT THE BOTTOM OF THE ARRAY

  K = 0
  do I = 1,MXFMOD
     if (FMOD(I) .eq.0) goto 60
     K = K + 1
     if (I .ne. K .and. FMCHKFWT(K)) then
        FMOD(K) = FMOD(I)
        FWT(K) = FWT(I)
        FMOD(I) = 0
        FWT(I) = 0.0
     end if
     60 continue
  end do

  !     WALK THROUGH LIST OF MODELS AND SAVE THE WEIGHT TO BE SHARED
  !     AMONG THE OVERLAPPING MODELS

  do I = 1,MXFMOD
     FMOD2(I) = 0
     FWT2(I) = 0.
  end do

  K = 1
  do I = 1,MXFMOD
     if (FMOD(I) .ne.0) then
        II = FMOD(I)
        XWT = FWT(I)
        if (EQMOD(II) .eq.0.and. FMCHKFWT(K)) then
           FWT2(K) = FWT(I)
           FMOD2(K) = FMOD(I)
           K = K + 1
        else
           do J = 1,ICLSS
              if (EQMOD(J) .eq. EQMOD(II) .and.&
                   &EQWT(J) .gt.0.0) then
                 if (FMCHKFWT(K)) then
                    FWT2(K) = FWT2(K) + EQWT(J) * XWT
                    FMOD2(K) = J
                    K = K + 1
                 end if
              end if
           end do
        end if
     end if
  end do

  !     MERGE IN ANY DUPLICATES THAT MAY HAVE ARISEN FROM
  !     THE ADDITION OF COLINEAR FUEL MODELS (IMPOSSIBLE UNDER
  !     CURRENT FUEL MODEL DEFINITIONS)

  999 K2 = 0
  do I = 1,MXFMOD
     FMOD(I) = 0
     FWT(I) = 0.
  end do

  do I = 1,MXFMOD
     if (FMOD2(I) .eq.0) goto 70
     K2 = K2 + 1
     do J = 1, I
        if (FMOD(J) .eq. FMOD2(I)) then
           K = J
           goto 72
        end if
     end do
     K = K2
     if (FMCHKFWT(K)) FMOD(K) = FMOD2(I)
     72 if (FMCHKFWT(K)) FWT(K) = FWT(K) + FWT2(I)
     70 continue
  end do

  do I = 1,MXFMOD
     if (FMOD(I) .eq.0) goto 75
     FMOD(I) = IPTR(FMOD(I))
     75 continue
  end do

  !     SORT WEIGHTS SUCH THAT HIGHEST WEIGHT IS ALWAYS FIRST
  !     WHAT ABOUT TIES?

  call RDPSRT (MXFMOD,FWT,INDX,.true.)
  do I = 1,MXFMOD
     J = INDX(I)
     FMOD2(I) = FMOD(J)
     FWT2(I) = FWT(J)
  end do
  do I = 1,MXFMOD
     FMOD(I) = FMOD2(I)
     FWT(I) = FWT2(I)
  end do

  !     TRUNCATE NUMBER OF FUEL MODELS TO 4 AND REWEIGHT,
  !     IN THE RARE CASE THAT >4 MODELS HAVE BEEN SELECTED
  !     THIS SOMETIMES HAPPENS IN THE EC, WITH THE 5TH MODEL
  !     WEIGHT AT 2-3% OF THE TOTAL.
  !     (CONVERSATION WITH NICK; 3 DECEMBER 2002)

  XWT = 0.0
  do I = 1,4
     XWT = XWT + FWT(I)
  end do
  if (XWT .gt.1.0E-6) then
     do I = 1,4
        FWT(I) = FWT(I) / XWT
     end do
     do I = 5,MXFMOD
        FMOD(I) = 0
        FWT(I) = 0.0
     end do
  end if

  !     SELECT NON-DYNAMIC MODEL BASED ON MOST HEAVILY WEIGHTED MODEL
  !     FAILSAFE IS TO NAME MODEL 8 AND ISSUE A WARNING

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

  !     COMPUTE THE NUMBER OF ACTIVE MODELS

  do I = 1,MXFMOD
     if (FWT(I) .le.1.0E-6) then
        NFMODS = I-1
        exit
     end if
  end do
  NFMODS = MIN(NFMODS,4)

  !     IF STATIC FUEL MODEL IS DESIRED, UNSET ALL OTHER WEIGHTS

  if (.not. LDYN) then
     FMOD(1) = FMD
     FWT(1) = 1.0
     NFMODS = 1
     do I = 2, MXFMOD
        FMOD(I) = 0
        FWT(I) = 0.0
     end do
  end if

  if (DEBUG) write (JOSTND,80) FMD, FMOD,FWT
  80 format (' FMDYN, FMD=',I4,' FMOD=',5I4,' FWT=',5F7.2)

  return
end
