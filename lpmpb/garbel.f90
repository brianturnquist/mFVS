!====================!
!   lpmpb : garbel   !
!====================!
! LPMPB $Id$
subroutine GARBEL (&
     &IPT,NRECS,KEY,WEIGHT,NATR,NCLAS,PN1,CLAS,ISC,P,&
     &A1,A2,A3,A4,A5,A6,A7,A8,PROB,KODE,MAXCLS,ITWO)
  implicit none

  !     N.L. CROOKSTON PROGRAMMER     JAN 1978 MAY 1980 JAN 1981

  !     GENERALIZED CLASSIFICATION ROUTINE

  !     THE GOAL OF THIS ROUTINE IS TO REDUCE THE LENGTH OF THE
  !     TREE ATTRIBUTE VECTORS FROM THEIR ORGINIAL LENGTH (EQUAL TO NRECS)
  !     TO THE LENGTH 'NCLAS'. THE OUTPUT OF THE CLASSIFICATION IS
  !     STORED IN THE ARRAY CLAS; THE CLASSES ARE ACCESSED THRU THE
  !     POINTER ARRAY IPT VIA THE ARRAY ISC.

  !     THERE ARE TWO METHODS USED TO FIND THE CLASSES:

  !     METHOD 1: FIRST, DEFINE A LIST, P (WITH LENGTH NRECS),
  !     OF THE SUM OF EACH TREE ATTRIBUTE (IE, CFV, HT, DBH, ETC.)
  !     MULTIPLIED BY THE APPROPRIATE ATTRIBUTE WEIGHT. THEN,
  !     DEFINE THE CLASS BOUNDARIES AS BEING AT THE MAXIMUM BREAKS
  !     IN P.

  !     METHOD 2: FIND THE CLASS WHICH HAS THE GREATEST NUMBER OF TREE
  !     RECORDS AND SPLIT IT INTO TWO CLASSES. REPEAT UNITL THE
  !     NUMBER OF ADDITIONAL CLASSES DESIRED HAVE BEEN FOUND.

  !     IPT   = POINTER ARRAY. MUST BE PACKED (CONTAIN NO ZEROS)
  !             AND START AT POSITION 1.
  !     NRECS = NUMBER OF RECORDS.
  !     KEY   = POSITION AND TYPE DISCRIPTOR ARRAY.
  !     WEIGHT= WEIGHTS OF ASSIGNED ATTRIBUTES
  !     NATR  = NUMBER OF ATTRIBUTES, NEEDED FOR OBJECT TIME
  !             DIMENSIONING OF CLAS
  !     MAXCLS= MAX. NUMBER OF CLASSES (DIMENSIONING OF ISC)
  !     ITWO  = ALWAYS EQUAL TO '2'    (DIMENSIONING OF ISC)
  !     NCLAS = NUMBER OF REQUESTED CLASSES. MUST BE .GE. NRECS
  !     CLAS  = CLASSIFIED ARRAY
  !     ISC   = SECTOR ARRAY, KEY TO THE IPT ARRAY.
  !     KODE  = 0 EVERYTHING WENT OK
  !             1 CNTR WAS TOO LOW
  !     P     = WORK ARRAY WHICH HOLDS THE WEIGHTED SUMS.
  !     NCL1  = THE NUMBER OF CLASSES FOUND USING METHOD 1.
  !     NCL2  = THE NUMBER OF CLASSES FOUND USING METHOD 2.
  !     PN1   = THE PROPORTION OF THE TOTAL NUMBER OF CLASSES
  !             FOUND USING METHOD 1.
  !     A1    = FIRST ARGUMENT TO BE CLASSIFIED IN POSITION
  !             KEY(1) OF CLAS.
  !     A2 TO A8  ---   LIKE A1   ---
  !     PROB  = TREMOD'S PROB.
  !     IMP   = POSITION IN CLAS OF PROB.  EQUAL TO KEY(9)

  ! Revision History
  !   04/03/02 Lance R. David (FHTET)
  !     Arrays dimensioned "1" changed to "*" so that array size is
  !     inherited from calling routine.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Variables
  integer :: KODE,NCLAS,NRECS,ITWO,MAXCLS,NATR,IPT(*),KEY(9),ISC(MAXCLS,ITWO),IMP,II,I,NCL1,NCL2,J,&
       &NC1,IPT1,IPT2,I1,I2,K,MAX,JJ
  real    :: CNTR,DIFFP,PN1,WEIGHT(8),CLAS(MAXCLS,NATR),PROB(*),P(*),A1(*),A2(*),A3(*),A4(*),A5(*),&
       &A6(*),A7(*),A8(*)

  KODE = 0
  IMP = KEY(9)

  !     STEP1:  INITIALIZE 'P' AND SUM PROB

  CNTR = 0.0
  do II = 1, NRECS
     I = IPT(II)
     P(I) = 0.0
     CNTR = CNTR + PROB(I)
     50 continue
  end do
  if ( CNTR .ge..001) go to 55
  KODE = 1
  return
  55 continue

  !     STEP2:  CALCULATE THE NUMBER OF CLASSES FOUND BY EACH METHOD.

  NCL1 = NCLAS*PN1+.5
  if (NCL1.lt.1) NCL1 = 1
  NCL2 = NCLAS-NCL1

  !     RESTRICTION: THERE HAS TO BE AT LEAST 05 MORE TREE RECORDS THAN
  !     REQUESTED CLASSES BEFORE METHOD 2 WILL BE USED.

  if (NRECS-NCLAS .gt.5) go to 10
  NCL1 = NCLAS
  NCL2 = 0
  10 continue

  !     STEP3:  CALCULATE THE WEIGHTED SCORES FOR EACH TREE RECORD
  !            FOR ALL ATTRIBUTES

  if ( KEY (1) .eq.0 ) go to 62
  call GRPSUM (NRECS,IPT,A1,P,WEIGHT(1))
  62 continue
  if ( KEY (2) .eq.0 ) go to 72
  call GRPSUM (NRECS,IPT,A2,P,WEIGHT(2))
  72 continue
  if ( KEY (3) .eq.0 ) go to 82
  call GRPSUM (NRECS,IPT,A3,P,WEIGHT(3))
  82 continue
  if ( KEY (4) .eq.0 ) go to 92
  call GRPSUM (NRECS,IPT,A4,P,WEIGHT(4))
  92 continue
  if ( KEY (5) .eq.0 ) go to 102
  call GRPSUM (NRECS,IPT,A5,P,WEIGHT(5))
  102 continue
  if ( KEY (6) .eq.0 ) go to 112
  call GRPSUM (NRECS,IPT,A6,P,WEIGHT(6))
  112 continue
  if ( KEY (7) .eq.0 ) go to 122
  call GRPSUM (NRECS,IPT,A7,P,WEIGHT(7))
  122 continue
  if ( KEY (8) .eq.0 ) go to 132
  call GRPSUM (NRECS,IPT,A8,P,WEIGHT(8))
  132 continue

  !     STEP4:  SORT 'IPT' ON 'P' USING RDPSRT

  call RDPSRT (NRECS,P,IPT,.false.)

  !     STEP5:  SET UP CLASES IN ISC

  !     FIND THE MAXIMUM DIFFERENCES IN ARRAY P.  THE CLASS BOUNDS WILL
  !     OCCUR AT THE POINTS OF MAX DIFFERENCES.

  !     (THIS CODE WAS MODIFIED FROM SOME ROELKE AND BURNELL TEXT)

  !     (A) ZERO OUT STORAGE AREA THAT IS USED TO HOLD THE DIFFERENCES.

  do J = 1,NCL1
     CLAS(J,IMP) = 0
     135 continue
  end do

  !     (B) CALCULATE DIFFERENCES

  NC1 = NCL1 - 1
  IPT1 = IPT(1)

  do I = 2,NRECS
     IPT2 = IPT(I)
     DIFFP = P(IPT1) - P(IPT2)

     do J = 1,NC1

        !     (C) SAVE LARGEST DIFFERENCES

        if (DIFFP .le. CLAS(J+1,IMP)) go to 140

        !     (D) BUBBLE SMALLER DIFFERENCES DOWN 1

        CLAS(J,IMP) = CLAS(J+1,IMP)
        ISC(J,1) = ISC(J+1,1)
        138 continue
     end do

     J = NCL1
     140 continue
     CLAS(J,IMP) = DIFFP
     ISC(J,1) = I
     IPT1 = IPT2
     142 continue
  end do

  ISC(1,1) = NRECS + 1

  !     (E) SORT CLASS DIVISION POINTERS INTO ASCENDING ORDER.
  !         NOTE THAT ISC IS TREATED LIKE A VECTOR IN IQRSRT.

  call IQRSRT (ISC,NCL1)

  !     STEP6:  DEFINE NCL2 MORE CLASSES USING METHOD 2.

  !     (A) CALCULATE THE LENGTH (IE, THE NUMBER OF TREE RECORDS)
  !     IN EACH CLASS. ALSO, REDEFINE ISC(J,1) TO POINT TO THE
  !     END OF CLASS J.

  I1 = 1
  do J = 1,NCL1
     I2 = ISC(J,1)-1
     ISC(J,1) = I2
     ISC(J,2) = I2-I1+1
     I1 = I2+1
     143 continue
  end do

  !     (B) IF NCL2 IS ZERO, METHOD 2 IS NOT TO BE USED.  SKIP
  !     TO THE ASSIGNMENT OF THE SECTOR POINTERS.

  if (NCL2.eq.0) go to 147

  !     (C) CREATE NCL2 NEW CLASSES BY DIVIDING THE NCL2 LARGEST
  !     CLASSES INTO TWO CLASSES EACH (INCLUDE THE NEW CLASSES IN THE
  !     SEARCH FOR THE LARGEST CLASS).

  do K = 1,NCL2
     MAX = 0
     do J = 1,NCL1
        if (ISC(J,2) .le. MAX) go to 144
        MAX = ISC(J,2)
        I = J
        144 continue
     end do

     NCL1 = NCL1+1
     MAX = FLOAT(MAX)/2. + .5
     ISC(NCL1,1) = ISC(I,1)
     ISC(I,1) = ISC(I,1)-MAX
     ISC(NCL1,2) = MAX
     ISC(I,2) = ISC(I,2)-MAX
     145 continue
  end do

  !     (D) RESORT THE CLASS DIVISION POINTERS

  call IQRSRT(ISC,NCL1)

  !     (E) ASSIGN THE CLASS SECTOR POINTERS.

  147 continue
  I1 = 1
  do J = 1,NCL1
     I2 = ISC(J,1)
     ISC(J,1) = I1
     ISC(J,2) = I2
     I1 = I2+1
     148 continue
  end do

  !     STEP7:  --- FIND AVERAGES FOR EACH CLAS
  !                        AND TOTAL PROB

  do I = 1, NCLAS
     CLAS(I,IMP) = 0.0
     I1 = ISC(I,1)
     I2 = ISC(I,2)
     do JJ = I1, I2
        J = IPT(JJ)
        CLAS (I,IMP) = CLAS (I,IMP) + PROB (J)
        150 continue
     end do
  end do

  if ( KEY (1) .eq.0 ) go to 162
  call GRCLAS (NRECS,IPT,A1,ISC,NCLAS,NATR,CLAS,PROB,IMP,KEY(1),&
       &MAXCLS,ITWO)
  162 continue
  if ( KEY (2) .eq.0 ) go to 172
  call GRCLAS (NRECS,IPT,A2,ISC,NCLAS,NATR,CLAS,PROB,IMP,KEY(2),&
       &MAXCLS,ITWO)
  172 continue
  if ( KEY (3) .eq.0 ) go to 182
  call GRCLAS (NRECS,IPT,A3,ISC,NCLAS,NATR,CLAS,PROB,IMP,KEY(3),&
       &MAXCLS,ITWO)
  182 continue
  if ( KEY (4) .eq.0 ) go to 192
  call GRCLAS (NRECS,IPT,A4,ISC,NCLAS,NATR,CLAS,PROB,IMP,KEY(4),&
       &MAXCLS,ITWO)
  192 continue
  if ( KEY (5) .eq.0 ) go to 202
  call GRCLAS (NRECS,IPT,A5,ISC,NCLAS,NATR,CLAS,PROB,IMP,KEY(5),&
       &MAXCLS,ITWO)
  202 continue
  if ( KEY (6) .eq.0 ) go to 212
  call GRCLAS (NRECS,IPT,A6,ISC,NCLAS,NATR,CLAS,PROB,IMP,KEY(6),&
       &MAXCLS,ITWO)
  212 continue
  if ( KEY (7) .eq.0 ) go to 222
  call GRCLAS (NRECS,IPT,A7,ISC,NCLAS,NATR,CLAS,PROB,IMP,KEY(7),&
       &MAXCLS,ITWO)
  222 continue
  if ( KEY (8) .eq.0 ) go to 232
  call GRCLAS (NRECS,IPT,A8,ISC,NCLAS,NATR,CLAS,PROB,IMP,KEY(8),&
       &MAXCLS,ITWO)
  232 continue

  return
end
