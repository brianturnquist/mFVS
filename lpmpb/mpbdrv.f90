!====================!
!   lpmpb : mpbdrv   !
!====================!
! LPMPB $Id$
subroutine MPBDRV
  implicit none

  !     INTERFACING PROGRAM TO CALL MPBMOD

  ! Revision History
  !   05/31/00 GEB
  !     Adding variable IDXLP, the index of Lodgepole pine in arrays
  !     Defined in mpblkd.f, which is variant dependent
  !   12/01/98 RNH
  !     Adapated to 24 species (CR variant)
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.
  !   03/29/13 Lance R. David (FMSC)
  !     Some variables defined locally were already defined in a common.
  !   08/22/14 Lance R. David (FMSC)
  !     Function name was used as variable name.
  !     changed variable INT to INCRS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'

  ! Variables
  character(len=4) :: IF60

  integer :: ITWO,MAXCLS,KODE,II,I1,I2,I,J

  real    :: A3(1),A4(1),A5(1),A6(1),A7(1),A8(1),DGI,DDS5,BAI5,X,DEAD,SUMDED

  data MAXCLS/ 30 /,ITWO/ 2 /, IF60/'F6.0'/

  !     BETTER= REALITIVE IMPORTANCE VALUES OF ATTRIBUTES OF INTEREST
  !     CLASS = ATTRIBUTES IN THEIR RESPECTIVE CLASSES
  !     IMPROB= POINTS TO THE POSITION WITHIN CLASS OF PROB
  !     NACLAS= ACTUAL  NUMBER OF CLASSES     1 <= NACLAS <= 30
  !     NCLASS= NUMBER OF CLASSES REQUESTED   2 <= NCLASS <= 30
  !     NATR  = NUMBER OF ATTRIBUTES TO BE CLASSIFIED
  !     DEBUIN= TRUE WHEN 'DEBUG' WAS SPECIFIED IN TREMOD SYSIN
  !     WORKIN= TRUE IF MPBMOD WAS IN THE MID OF AN INFESTATION
  !             WHEN IT LAST RETURNED TO MPBDRV.  CAN ALSO BE
  !             SET TRUE WHEN 'MANUAL' IS SPECIFIED IN TREMOD SYSIN.
  !     LBET  = TRUE IF BETTER HAS BEEN INITIALIZED
  !     RGMORT= FALSE WHEN MPBMOD CAUSES 'PRCENT' OR MORE PERCENT
  !             MORTALITY FOR SPECIES 'ISPC'.  TRUE IF MORTS
  !             IS TO CALCULATE ADDITIONAL MORTALITY
  !     PRCENT= PERCENT MORTALITY CRITERIA FOR SWITCHING 'RGMORT'
  !     DEAD  = TOTAL PROB WHICH IS DEAD UPON RETURN FROM MPBMOD
  !     SURVIV= PERCENT SURVIVAL IN EACH CLASS

  ! Revision History
  !    Last noted revision date.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  !********************        EXECUTION   STARTS        ***************

  !     IF WE ARE NOT IN THE MIDST OF AN OUTBREAK;
  !     COMPUTE PERIODIC GROWTH RATIO.

  if (MPBYR.eq.0) call MPGR

  !     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
  !     with new species mapping and new location for LP (RNH Dec98, GEB May2000)

  I1 = ISCT(IDXLP,1)
  I2 = ISCT(IDXLP,2)

  !      I1 = ISCT(7,1)
  !      I2 = ISCT(7,2)
  ILP = I2-I1+1
  do J = I1,I2
     I = IND1(J)

     !     LOAD ARRAY 'IPT'

     IPT(J) = I

     !     CALCULATE PHLOEM THICKNESS OF EACH TREE RECORD.
     !     STORE VALUES IN XPT FOR COMPRESSION.

     !     CALCULATE DELTA DIAMETER SQUARED FOR A 5 YEAR PERIOD.

     DGI = DG(I)
     DDS5 = ( 2. * DBH(I) * DGI + DGI * DGI ) / 2.0
     BAI5 = DDS5 * 0.7853982

     !     PHLOEM MODEL: (FROM D. M. COLE (1973))
     !     TREES UNDER 4.5 FEET TALL HAVE NO DIAMETER GROWTH...THEREFORE
     !     ONLY ESTIMATE PHLOEM ON TREES WHICH HAVE DIA. GROWTH.

     XPT(I) = 0.
     if (BAI5.gt..0001) XPT(I) = EXP(-3.17152 + .12591*ALOG(BAI5) +&
          &.50932*ALOG(DBH(I)) - .0077*HT(I))

     !     THE FOLLOWING PHLOEM MODEL WAS CALIBRATED FROM DATA COLLECTED
     !     IN THE BLUE MOUNTAINS, EASTERN OR.  AND HAS A STRONGERR
     !     CORRELATION WITH DIA GROWTH THAN COLE'S.

     !*    IF (BAI5.GT..0001) XPT(I) = EXP(-2.90907+0.45506*ALOG(BAI5) -
     !*   >                            0.37328*ALOG(DBH(I))+0.0029*HT(I))

     10 continue
  end do

  !     LOAD ARRAYS 'CLASS' AND 'MPISC'

  if ( DEBUIN ) write (JOMPB,20)
  20 format ('CALLING GARBEL (LP)')

  call GARBEL ( IPT(I1),ILP,KEYMPB,BETTER,6,&
       &NACLAS,.5,CLASS,MPISC,WK3,DBH,XPT,A3,A4,A5,&
       &A6,A7,A8,PROB,KODE,MAXCLS,ITWO)
  if (KODE .eq.0) go to 40
  write  (JOMPB,30)
  30 format (/'********    ERROR - LPMPB: SUBROUTINE GARBEL FAILED')
  40 continue

  !     ADD THE PROPER OFFSET TO SECTOR POINTERS

  do I = 1, NACLAS
     MPISC(I,1) = MPISC(I,1) + I1 - 1
     MPISC(I,2) = MPISC(I,2) + I1 - 1
     50 continue
  end do

  !     STORE TOTAL PROB FOR EACH CLASS.  THIS ALLOWS CALCULATION OF
  !     PROPORTIONATE SURVIVAL IN CLASS(I,IMPROB)

  do I = 1,NACLAS
     SURVIV(I) = CLASS(I,IMPROB)
     60 continue
  end do

  !*    IF ( .NOT. DEBUIN ) GO TO 90

  !     THIS DEBUG OUTPUT IS DEPENTENT UPON A FIXED CLASSIFICATION
  !     SYSTEM. (THE VALUES OF 'KEYMPB' ARE ASSUMED)

  !*    WRITE (JOMPB,62)
  !* 62 FORMAT ('CLASS    MPISC  II    I     DBH',
  !*   >        '     PHLOEM      PROB     P-VECTOR',/)
  !*    DO 80 J=1,NACLAS
  !*    I1= MPISC(J,1)
  !*    I2= MPISC(J,2)
  !*    DO 70 II=I1,I2
  !*    I=IPT(II)
  !*    WRITE (JOMPB,65) II,I,DBH(I),XPT(I),PROB(I),WK3(I)
  !* 65 FORMAT (13X,2I5,F10.2,F10.4,F10.2,F12.5)
  !* 70 CONTINUE
  !*    WRITE (JOMPB,75) J,I1,I2,CLASS(J,2),CLASS(J,3),CLASS(J,1)
  !* 75 FORMAT (/,I3,I6,I5,9X,F10.2,F10.4,F10.2/)
  !* 80 CONTINUE

  !* 90 CONTINUE

  if ( MPBYR .eq.0 ) call MPBHED

  !                         *********   CALL MPBMOD   **********

  call SURFCE
  call MPBMOD

  !     CALCULATE PROPORTIONATE REDUCTION IN CLASS(I,IMPROB)

  do I = 1,NACLAS
     SURVIV (I) = CLASS(I,IMPROB) / SURVIV(I)
     100 continue
  end do

  if ( .not. DEBUIN ) go to 130
  write (JOMPB,110) (SURVIV(I),I = 1,NACLAS)
  110 format (/,'SURVIV . . .',//,3(10E13.3,/))

  130 continue

  !     PUT DEAD TREES IN WK2--CONVERTED TO VOLUMNS IN UPDATE.

  SUMDED = 0.0
  do J = 1,NACLAS
     I1 = MPISC(J,1)
     I2 = MPISC(J,2)
     DEAD = 1.-SURVIV(J)
     SUMDED = SUMDED+TPROB
     do II = I1,I2
        I = IPT(II)
        X = PROB(I)*DEAD
        if (X .gt. WK2(I)) WK2(I) = X
        if (PROB(I)-WK2(I) .lt.1E-6) WK2(I) = PROB(I)-1E-6
        135 continue
     end do
     140 continue
  end do

  !     IF WE JUST RETURNED FORM MPBMOD FOR THE LAST TIME, FOR THIS
  !     STAND AND FOR THIS EPIDEMIC, WE PRINT THE OUTBREAK GRAPH

  if ( DEBUIN ) write (JOMPB,170) MPBYR,MPBGRF
  170 format (//,'MPBYR = ',I3,' MPBGRF = ',L2)
  if ( MPBYR .gt.0.or..not. MPBGRF ) return

  !     ENTRY TO WRITE FINAL PLOT OUTPUT (GRAPH)

  entry MPLOTW

  call MPBHED
  write (JOMPB,180)
  180 format (/,T7,'** ATTACK, EMERGENCE & PIONEER DENSITIES',&
       &' (A,E,D),  SURFACE KILLED (*),  BEETLES PRODUCED',&
       &' (B), AND P **',/,T7,&
       &'    NOTE:  $ SHOWS ACTUAL SURFACE KILLED DATA',/)

  call PTOPT ( IPLTNO, JOMPB, IF60  , INCRS + 1, 1 )

  return
end
