!====================!
!   lpmpb : colmod   !
!====================!
! LPMPB $Id$
subroutine COLMOD (IFINT,ICYC,IYEAR,NPLT)
  implicit none

  !     BOUSFIELD'S NEW Q VALUE EQUATION IS IN THIS VERSION.

  !     VARIABLES :
  !        DEADCL - ARRAY OF TOTAL INFESTED TREES BY DIAMETER CLASS.
  !        DEAD   - ARRAY OF INFESTED TREES BY DIAMETER CLASS AND YEAR IN
  !                 INFESTATION CYCLE.
  !        GREEN  - ARRAY OF UNINFESTED TREES BY DBH CLASS AND INF. YEAR.
  !        ZINMRT - ARRAY OF INITIAL MORTALITY RATES USED TO
  !                 DETERMINE INITIAL NUMBER OF INFESTED TREES
  !        PRNOIN - ARRAY OF PROBABILITIES OF A TREE  -NOT-  BEING
  !                 INFESTED.  ARRAY IS REFERENCED BY  ????
  !                 (DBH CLASSES FOR THIS ARRAY ARE 6-14'',14-16',
  !                 AND 16+")
  !        TDEAD  - ARRAY BY INFESTATION YEAR OF TOTAL NUMBER OF
  !                 DEAD TREES
  !        TGREEN - ARRAY BY INFESTATION YEAR OF TOTAL NUMBER OF
  !                 LIVE TREES

  ! Revision History
  !   12/23/99; Lance R. David (FHTET-FC)
  !     Updated for expansion of FVS stand id (variable NPLT)
  !     from 8 to 26 characters.
  !   02/06/04  Lance R. David (FHTET)
  !     Changed the minimum number of GREEN trees that must exist in
  !     a DBH class before trees from that class can be killed.
  !     The original value of 1.0E-12 was changed to 0.03.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COLCOM.f90'
  include 'MPBCOM.f90'

  ! Variables
  character(len=26) :: NPLT

  integer :: MDGREN(7),MDDEAD(7),MDDTOT,MDGTOT,IOUT,IYEAR,ICYC,IFINT,IRND,I,J,K,L,NUMYRS

  real    :: PMIN(10),PMAX(10),WAYNE(10),X,DTOTAL,PNEW

  !      Q VALUES FOR MIN AND MAX BOUNDARIES, BOUSFIELD'S NORMAL TPA

  data PMIN/.970,.950,.900,.800,.600,.500,.400,.150,.100,.050/
  data PMAX/1.0,1.0,1.0,1.0,.990,.980,.970,.900,.800,.700/
  data WAYNE/150.,100.,95.,66.,35.5,16.5,5.5,3.,1.5,.80/

  !     STATEMENT FUNCTION IRND (ROUNDING FUNCTION)

  IRND(X) = IFIX(X + 0.5)

  !     PRINT DEBUG IF REQUESTED.

  if (DEBUIN) write(JOMPB,1) IYEAR,NPLT
  1 format ('ENTER COLMOD (MPB), IYEAR=',I4,'; NPLT=',A26)

  !     ADD UP THE STARTING GREEN TREES.

  STOTAL = 0.0
  do I = 1,10
     STOTAL = STOTAL + START(I)
     2 continue
  end do

  if (DEBUIN) write(JOMPB,*) 'IN COLMOD, STOTAL=', STOTAL

  !     FIRST OBTAIN INITIAL INFESTATION VALUES.

  DTOTAL = 0.0

  do I = 1,10
     if (ICYC .eq.1.and. (LCURMR .or. LINVMR)) then
        DTOTAL = DTOTAL + CURRMR(I)
        if (START(I) * ZINMOR(I) .gt. GREINF(I) + CURRMR(I))&
             &GREINF(I) = START(I) * ZINMOR(I) - CURRMR(I)
        if (GREINF(I) .gt. START(I)) GREINF(I) = START(I)
        DEAD(1,I) = GREINF(I) + CURRMR(I)
        GREEN(1,I) = START(I) - GREINF(I)
     else
        DEAD(1,I) = START(I) * ZINMOR(I)
        GREEN(1,I) = START(I) - DEAD(1,I)
     end if

     TDEAD(1) = TDEAD(1) + DEAD(1,I)
     TGREEN(1) = TGREEN(1) + GREEN(1,I)
     10 continue
  end do

  if (DEBUIN) write(JOMPB,*) 'IN COLMOD, TDEAD(1)=', TDEAD(1),&
       &' TGREEN(1)=', TGREEN(1), ' DTOTAL=', DTOTAL

  !     SET NUMYRS TO THE LENGTH OF THE OUTBREAK IN YEARS.

  if (MPMXYR .gt. IFINT) then
     NUMYRS = IFINT
  else
     NUMYRS = MPMXYR
  end if

  if (NUMYRS .gt.10)  NUMYRS = 10

  if (DEBUIN) write(JOMPB,*) 'IN COLMOD, NUMYRS=', NUMYRS,&
       &' IBOUSE=', IBOUSE

  do K = 2,NUMYRS
     if ((TDEAD(K-1) .le.0.0005) .or. (TGREEN(K-1) .le.0.0005))&
          &goto 39

     !        OTHERWISE, AN OUTBREAK IS IN PROGRESS

     do J = 1,10

        !           NEW ADDITIONS TO MODEL 4/13/88 TO ADD EFFECT OF WAYNE
        !           BOUSFIELD'S CHANGES TO THE MODEL.
        !           06-FEB-04 Lance David
        !           Changed 1.E12 to 0.03 in .LT. condition below. In this
        !           case, a size class having less than 3 tree per 100 acres is
        !           of no significance and well outside the min and max values
        !           imposed directly following.

        if (IBOUSE .eq.1) then
           if (GREEN(1,J) .lt.0.03 ) then
              PNEW = 0.0
           else
              if (DEBUIN) write(JOMPB,*) 'IN COLMOD, PRNOIN(',&
                   &J,')=',PRNOIN(J),' WAYNE=',WAYNE(J),' GREEN=',&
                   &GREEN(1,J)
              PNEW = PRNOIN(J)**(WAYNE(J) / GREEN(1,J))
           end if

           if (PNEW .lt. PMIN(J)) PNEW = PMIN(J)
           if (PNEW .gt. PMAX(J)) PNEW = PMAX(J)
        else
           PNEW = PRNOIN(J)
        end if

        if (DEBUIN) write(JOMPB,*) 'IN COLMOD, PNEW=', PNEW,&
             &' GREEN=', GREEN(K-1,J),' DEAD=', DEAD(K-1,J)

        DEAD(K,J) = GREEN(K-1,J) * (1.0 - (PNEW**DEAD(K-1,J)))

        DEADCL(J) = DEADCL(J) + DEAD(K,J)
        TDEAD(K) = TDEAD(K) + DEAD(K,J)
        GREEN(K,J) = GREEN(K-1,J) - DEAD(K,J)
        TGREEN(K) = TGREEN(K) + GREEN(K,J)
        30 continue
     end do
     TDEAD(K) = TDEAD(K) + TDEAD(K-1)
     35 continue
  end do

  !.... Tell the event monitor the number of trees killed by mountain
  !.... pine beetle.

  if (DEBUIN) write(JOMPB,*) 'IN COLMOD, CALL EVSET4, TDEAD(10)=',&
       &TDEAD(10)

  call EVSET4 (4,TDEAD(10))

  !     OUTPUT TREES PER DBH, TOTAL DEAD AND TOTAL LIVE PER TIME

  39 continue
  write(JOMPB,37) IYEAR, NUMYRS
  37 format (//,'MOUNTAIN PINE BEETLE RATE OF LOSS MODEL :  OUTBREAK',&
       &' IN YEAR = ',I4,' FOR ',I2,' YEARS.',/)

  write(JOMPB,40)
  40 format (19X,'TREES/ACRE OF LIVE LODGEPOLE PINE BY DBH CLASS',T85,&
       &' TOTAL    TOTAL')

  write(JOMPB,41)
  41 format ('TIME     1-3    3-5    5-7    7-9    9-11  11-13 ',&
       &' 13-15  15-17  17-19   19+          LIVE     DEAD'/)

  if (JOMPBX .ne.0) write(JOMPBX,*) 'SUMMARY BY YEAR'

  write(JOMPB,42) (START(J),J = 1,10),STOTAL,DTOTAL
  if (JOMPBX .ne.0) write(JOMPBX,42) (START(J),J = 1,10),&
       &STOTAL, DTOTAL

  42 format ('START ',10F7.2,5X,2F9.2)

  do L = 1,NUMYRS
     IOUT = L + IYEAR
     write(JOMPB,50) IOUT,(GREEN(L,J),J = 1,10), TGREEN(L), TDEAD(L)
     if (JOMPBX .ne.0) write(JOMPBX,50) IOUT,(GREEN(L,J),J = 1,10),&
          &TGREEN(L), TDEAD(L)
     50 format (I4,2X,10F7.2,5X,2F9.2)
     100 continue
  end do

  !     IF DEBUG IS REQUESTED, WRITE INFO.

  if (DEBUIN) then

     !     PREPARE OUTPUT THAT HAS THE SAME FORMAT AS GENERATED FROM
     !     THE OTHER STAND ALONE MODEL...(WITH 7 CLASSES).

     do L = 1,NUMYRS
        IOUT = L + IYEAR - 1
        MDDEAD(1) = IRND(DEAD(L,3))
        MDGREN(1) = IRND(GREEN(L,3))

        do J = 4,8
           MDDEAD(J-2) = IRND(DEAD(L,J))
           MDGREN(J-2) = IRND(GREEN(L,J))
           115 continue
        end do

        MDDEAD(7) = IRND(DEAD(L,9) + DEAD(L,10))
        MDGREN(7) = IRND(GREEN(L,9) + GREEN(L,10))
        MDGTOT = 0
        MDDTOT = 0

        do J = 1,7
           MDGTOT = MDGTOT + MDGREN(J)
           MDDTOT = MDDTOT + MDDEAD(J)
           116 continue
        end do

        write(JOMPB,117) IOUT,NPLT,&
             &(MDGREN(J),MDDEAD(J),J = 1,7),&
             &MDGTOT, MDDTOT
        117 format (I4,1X,A26,5X,16I5)
        120 continue
     end do
  end if

  if (DEBUIN) write(JOMPB,*) 'EXIT COLMOD'
  return
end
