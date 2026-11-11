!==================!
!   lpmpb : mpgr   !
!==================!
! LPMPB $Id$
subroutine MPGR
  implicit none

  !     PART OF THE MOUNTAIN PINE BEETLE EXTENSION OF PROGNOSIS SYSTEM.
  !     NICK CROOKSTON; INT-MOSCOW                      NOV 1980.

  !     MPGR COMPUTES THE PERIODIC GROWTH RATIO FOR LODGEPOLE PINE.
  !     ENTRY MPSVDG IS CALLED FROM MPBGO TO SAVE THE PAST DG'S.

  ! Revision History
  !   05/31/00 Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'MPBCOM.f90'

  ! Variables
  integer :: I,I2,J

  real    :: AVRAT,BRATIO,D1,D2,FDG,ODDS,ODG,PRB,SCALE,SUMP,X

  !     NOTE: THIS ROUTINE ASSUMES THAT ENTRY MPSVDG IS CALLED BEFORE
  !           DGDRIV IS CALL DURING THE CURRENT CYCLE AND THEREFORE
  !           DG IS ON A IFINT YEAR BASIS FOR THE PREVIOUS CYCLE OR
  !           AS IT WAS INVENTORIED.

  !           ENTRY MPGR MUST BE CALLED AFTER DGDRIV FOR THE CURRENT
  !           CYCLE AND DG IS THEN ASSUMED TO CONTAIN DG'S SCALED TO
  !           A YR BASIS...(USUALLY 10 YEARS).

  !     ********************     EXECUTION     ********************

  !     IF THERE ARE LE 2 LPP SELECTED; THEN: SET PGR=.9 AND RETURN.

  if (NPGR .gt.2) go to 10
  PGR = .9
  return
  10 continue

  !     INITIALIZE THE SCALARS

  SCALE = YR/NPYR
  SUMP = 0.
  AVRAT = 0.

  !     LOOP THRU SELECTED LPP.

  ILP = ILP-1
  do J = 1,NPGR
     I = IPT(J+ILP)
     PRB = PROB(I)
     SUMP = SUMP+PRB
     FDG = DG(I)
     D2 = DBH(I)*BRATIO(7,DBH(I),HT(I))
     ODG = XPT(I)
     D1 = D2-ODG

     !     COMPUTE OLD DDS ON A YR-PERIOD BASIS.

     ODDS = ODG*(2*D2-ODG)*SCALE

     !     COMPUTE ODG ON A YR-PERIOD BASIS.

     ODG = 0.
     X = D2*D2-ODDS
     if (X .gt..000001) ODG = D2-SQRT(X)

     !     THE FUTURE DG IS CURRENTLY ON A YR-PERIOD BASIS. COMPUTE RATIO.

     AVRAT = AVRAT+(FDG/ODG*PRB)

     if (DEBUIN) write (JOMPB,15) J,I,D1,D2,FDG,ODG,PRB
     15 format ('J=',I4,' I=',I4,' D1=',F5.2,' D2=',F5.2,' FDG=',&
          &F4.2,' ODG=',F4.2,' PRB=',F6.3)
     20 continue
  end do

  !     COMPUTE THE AVERAGE PGR.

  PGR = AVRAT/SUMP

  !     WRITE DEBUG, IF REQUESTED.

  if (DEBUIN) write(JOMPB,50) ICYC,PGR,SUMP
  50 format (/,'IN MPGR - ICYC=',I3,'; PGR=',F8.3,'; SUMP=',F8.3)
  return

  entry MPSVDG
  NPGR = 0

  !     IF THIS IS CYCLE 1 AND IDG GT 1 (THE DG MEASURMENT PERIOD IS
  !     CONCURRENT WITH THE PAST GROWTH PERIOD) THEN WE CAN NOT
  !     ESTIMATE PGR...THEREFORE RETURN.

  if (ICYC.eq.1.and. IDG.gt.1) return

  !     SAVE THE VALUES OF DG WHICH WILL BE NEEDED TO COMPUTE PGR.

  !     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
  !     with new species mapping and new location for LP (RNH Dec98, GEB May2000)

  ILP = ISCT(IDXLP,1)
  I2 = ISCT(IDXLP,2)

  !      ILP=ISCT(7,1)
  !      I2=ISCT(7,2)
  do J = ILP,I2
     I = IND1(J)
     if (PCT(I).lt.PCTCO) goto 100
     IPT(NPGR+ILP) = I
     NPGR = NPGR+1
     XPT(I) = DG(I)
     100 continue
  end do

  !     INSURE NPYR CONTAINS THE MEASUREMENT PERIOD OF THE LAST CYCLE.
  !     IF ICYC=1; THEN: NPYR WAS SET IN MPBIN (ENTRY MPBOPS).
  !     ELSE:

  if (ICYC .gt.1) NPYR = IY(ICYC)-IY(ICYC-1)

  if (DEBUIN) write (JOMPB,150) NPGR,PCTCO,NPYR
  150 format (/,'IN PGR - ',I4,' TREE RECS SELECTED; PCTCO=',F8.3,&
       &' NPYR=',I3)
  return
end
