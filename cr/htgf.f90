!======================================!
!   Compute large tree height growth   !
!======================================!
! CR $Id$
subroutine HTGF
  implicit none

  !   THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !   INCREMENT FOR EACH CYCLE AND LOADS IT INTO
  !    AN ARRAY, HTG.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'MULTCM.f90'
  include 'HTCAL.f90'
  include 'PDEN.f90'
  include 'VARCOM.f90'
  include 'GGCOM.f90'

  external RANN
  ! Variables
  logical :: DEBUG
  real    :: HTOSI(6,MAXSP),TEMHTG,BACHLO,ZZRAN,ASPFAC,TEMSI,HGU,HGE
  real    :: THTG,H30,TCCF,DFUT,AGEFUT,HTGI,TSITE,AP,DGI,PCCFI,XWT,SCALE
  real    :: HHU2,HHU1,HHE2,HHE1,HNOW,BAUTBA,BRATIO,BARK,D,ADJUST,SSITE
  real    :: MISHGF
  integer :: I3,ICLS,I,ISPC,I1,I2,ITFN

  !  SPECIES ORDER:
  !   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
  !  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
  !  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
  !  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH

  !  SPECIES EXPANSION:
  !  UJ,AJ,RM,OJ,ER USE CR JU
  !  NC,PW USE CR CO
  !  GO,AW,EM,BK,SO USE CR OA
  !  PB USES CR AS
  !  PM,PD,AZ USE CR PI
  !  CI USES CR PP

  ! LOAD HEIGHT-TO-SITE FOR SOUTHWEST MC MODEL TYPE

  data (HTOSI(1,I),I = 1,MAXSP)/&
       &1.25, 1.25, 1.18, 1.00, 1.20, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.20, 1.00, 1.20, 1.20, 1.20, 1.00, 1.20, 1.20, 1.20, 1.00,&
       &1.15, 1.15, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.20, 1.15, 1.15/

  ! LOAD HEIGHT-TO-SITE FOR SOUTHWEST PP MODEL TYPE

  data (HTOSI(2,I),I = 1,MAXSP)/&
       &1.25, 1.25, 1.18, 1.00, 1.20, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.15, 1.00, 1.20, 1.15, 1.20, 1.00, 1.20, 1.20, 1.20, 1.00,&
       &1.15, 1.15, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.20, 1.15, 1.15/

  ! LOAD HEIGHT-TO-SITE FOR BLACK HILL PP MODEL TYPE

  data (HTOSI(3,I),I = 1,MAXSP)/&
       &1.13, 1.13, 1.10, 1.10, 1.13, 1.10, 1.10, 1.10, 1.00, 1.07,&
       &1.13, 1.00, 1.10, 1.10, 1.10, 1.00, 1.15, 1.15, 1.15, 1.00,&
       &1.05, 1.05, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.10, 1.05, 1.15/

  ! LOAD HEIGHT-TO-SITE FOR SPRUCE-FIR MODEL TYPE

  data (HTOSI(4,I),I = 1,MAXSP)/&
       &1.15, 1.15, 1.18, 1.00, 1.15, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.10, 1.00, 1.15, 1.05, 1.15, 1.00, 1.10, 1.10, 1.10, 1.00,&
       &1.05, 1.05, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.15, 1.05, 1.05/

  ! LOAD HEIGHT-TO-SITE FOR LODGEPOLE PINE MODEL TYPE

  data (HTOSI(5,I),I = 1,MAXSP)/&
       &1.25, 1.25, 1.25, 1.10, 1.25, 1.10, 1.10, 1.10, 1.00, 1.07,&
       &1.30, 1.00, 1.25, 1.20, 1.20, 1.00, 1.20, 1.20, 1.20, 1.00,&
       &1.18, 1.18, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.25, 1.15, 1.20/

  ! LOAD HEIGHT-TO-SITE FOR ASPEN MODEL TYPE

  data (HTOSI(6,I),I = 1,MAXSP)/&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00,&
       &1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00, 1.00/

  SCALE = FINT/YR
  ISMALL = 0

  ! CHECK FOR DEBUG.

  call DBCHK (DEBUG,'HTGF',4,ICYC)

  ! GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)
  if (DEBUG)write(JOSTND,9099)XHMULT,SCALE
  9099 format( ' HTGF MULT=',F10.3,'SCALE=',F10.3)

  ! BEGIN SPECIES LOOP

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 400
     I2 = ISCT(ISPC,2)
     SSITE = SITEAR(ISPC)
     ADJUST = HTOSI(IMODTY,ISPC)

     ! BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        HTG(I) = 0.
        if (PROB(I) .le.0.0 ) go to 161

        ! SAVE POINTERS TO SMALL TREES
        ! BYPASS CALCULATION IF DIAMETER IS LESS THAN LOWER LIMIT

        if (DBH(I).lt.0.5.or. HT(I).le.4.5) then
           ISMALL = ISMALL + 1
           IND2(ISMALL) = I
           go to 161
        end if
        D = DBH(I)
        ICLS = IFIX(D + 1.0)
        if (ICLS .gt.41) ICLS = 41
        BARK = BRATIO(ISPC,D,HT(I))
        BAUTBA = BAU(ICLS)/BA
        HNOW = HT(I)
        HHE1 = 0.0
        HHE2 = 0.0
        HHU1 = 0.0
        HHU2 = 0.0
        XWT = 0.0
        PCCFI = PCCF(ITRE(I))
        DGI = DG(I)
        AP = ABIRTH(I)

        !  ADJUST AP FOR BREAST HIGH AGE CURVES IF NECESSARY

        if (IMODTY.eq.1.or. IMODTY.eq.2.or. IMODTY.eq.4) then
           if (ISPC.eq.20.or. ISPC.eq.28.or. ISPC.eq.38&
                &.or. ISPC.eq.14.or. (IMODTY.eq.3.and.&
                &(ISPC.eq.21.or. ISPC.eq.22.or. ISPC.eq.37.or. ISPC.eq.16&
                &.or. (ISPC.ge.29.and. ISPC.le.32)))) then
              AP = AP-(4.5/(0.1+SSITE/50.))
           else if (IMODTY.eq.1) then
              TSITE = SSITE
              if (TSITE.lt.30.)TSITE = 30.
              AP = AP-(4.5/(-0.642+0.02285*TSITE))
           else if (IMODTY.eq.2) then
              AP = AP-(4.5/(0.25+0.00467*SSITE))
           else if (IMODTY.eq.4) then
              TSITE = SSITE
              if (TSITE.lt.20.)TSITE = 20.
              AP = AP-(4.5/(-0.22+0.0155*TSITE))
           end if
           if (AP.lt.1.)AP = 1.
        end if

        HHE1 = 0.
        HHU1 = 0.
        IHTG = 0
        HTGI = 0.
        if (DEBUG)write(JOSTND,*)' IN HTGF 1ST CALL TO GEMHT ARGS= ',&
             &IMODTY,HHE1,HHU1,SSITE,D,HNOW,ISPC,BA,AP,BAUTBA,&
             &PCCFI,DGI,BARK,IHTG,HTGI

        ! GEMHT NOW COMPUTES BOTH EVEN/UNEVEN-AGE REGARDLESS.

        call GEMHT(IMODTY,HHE1,HHU1,SSITE,D,HNOW,ISPC,BA,AP,&
             &BAUTBA,PCCFI,DGI,BARK,IHTG,HTGI)

        if (DEBUG)write(JOSTND,*)' RETURN FROM 1ST GEMHT HHE1,HHU1,IHTG,',&
             &'HTGI= ',HHE1,HHU1,IHTG,HTGI

        !  IF HEIGHT GROWTH IS ALREADY A DIRECT ESTIMATE, SKIP NEXT SECTION.

        if (IHTG .eq.1) then
           HTG(I) = HTGI*ADJUST
           go to 190
        end if

        ! HTG IS A TEN YEAR ESTIMATE AT THIS POINT, BUT IS SCALED TO A
        ! FINT YEAR BASIS BELOW.

        AGEFUT = AP + 10.0
        DFUT = D + DGI/BARK

        if (DEBUG)write(JOSTND,*)' IN HTGF 2ND CALL TO GEMHT ARGS= ',&
             &IMODTY,HHE2,HHU2,SSITE,DFUT,HNOW,ISPC,BA,AGEFUT,BAUTBA,&
             &PCCFI,DGI,BARK,IHTG,HTGI

        call GEMHT(IMODTY,HHE2,HHU2,SSITE,DFUT,HNOW,ISPC,BA,AGEFUT,&
             &BAUTBA,PCCFI,DGI,BARK,IHTG,HTGI)

        if (DEBUG)write(JOSTND,*)' RETURN FROM 2ND GEMHT HHE2,HHU2= ',&
             &HHE2,HHU2

        ! REALIGN HHE1 AND HHE2 FOR BLACK HILLS

        if (IMODTY .eq.3) then
           HHE2 = HHE1
           HHE1 = HT(I)
        end if

        ! IF EVEN-AGED,UNEVEN-AGED AND BA .LT. 70, OR UNEVEN-AGED AND PCT(I)
        ! IS .GE.40 (OVERSTORY) THEN HTG(I) IS EVEN-AGED HT GROWTH ESTIMATE.

        HTG(I) = (HHE2 - HHE1)*ADJUST

        !  NEED TO GET LPP MODEL TYPE EVEN-AGED YOUNG TREES TO HIT CARL'S
        !  SITE FUNCTION EVALUATED AT AGE 30.

        if (IMODTY.eq.5.and. ABIRTH(I).le.31.) then
           TCCF = PCCF(ITRE(I))
           if (TCCF.le.125.)TCCF = 0.
           H30 = 5.25621 + 0.37515*SSITE - 0.00082*TCCF*SSITE
           H30 = H30*ABIRTH(I)/31.
           THTG = H30-HT(I)
           if (THTG .gt. HTG(I)) then
              HTG(I) = THTG
              HHE2 = H30
              HHE1 = HT(I)
           end if
        end if

        ! IF STAND IS UNEVEN-AGED AND BA IS .GE. TO 70 THEN USE A BLEND.
        ! TREES WITH PCT() .LE. 10 (OVERTOPPED TREES) GET UNEVEN-AGED
        ! HEIGHT GROWTH; TREES WITH PCT() .GT. 10 AND .LT. 40 GET A
        ! WEIGHTED AVERAGE OF EVEN/UNEVEN-AGED HEIGHT GROWTH.

        if (AGERNG .gt.40.0.and. BA .ge.70.0) then
           if (PCT(I) .le.10.)HTG(I) = HHU2 - HHU1
           if ((PCT(I) .gt.10.0) .and. (PCT(I) .lt.40.0)) then
              HGE = (HHE2 - HHE1)*ADJUST
              HGU = HHU2 - HHU1
              XWT = ((PCT(I)-10.)*(10./3.))/100.
              HTG(I) = XWT*HGE + (1.0-XWT)*HGU
           end if
        end if

        !  ADJUSTMENT TO HEIGHT GROWTH FOR ASPEN AND PAPER BIRCH

        if (ISPC.eq.20.or. ISPC.eq.28) then
           TEMSI = SITEAR(ISPC)
           if (TEMSI .lt.30.)TEMSI = 30.
           if (TEMSI .gt.90.)TEMSI = 90.
           ASPFAC = 0.6253 + 0.00583*TEMSI
           HTG(I) = HTG(I)*ASPFAC
        end if

        !     ADD RANDOM INCREMENT TO HTG.  GETS AWAY FROM ALL TREES HAVING
        !     THE SAME INCREMENT (ESP UNDER EDMINSTERS EVEN-AGED LOGIC).

        190 continue
        ZZRAN = 0.0
        if (DGSD .gt.0.0) then
           ZZRAN = BACHLO(0.0,1.0,RANN)
           if (ZZRAN .gt. DGSD .or. ZZRAN .lt. (-DGSD)) go to 190
           if (DEBUG)write(JOSTND,9984) I,HTG(I),ZZRAN,SCALE
           9984 format(1H ,'IN HTGF 9984 FORMAT',I5,2X,3(F10.4,2X))
        end if
        HTG(I) = HTG(I) + ZZRAN*0.1

        !  IF STAGNATION EFFECT IS ON FOR THIS SPECIES,
        !  ONLY REDUCE HT GROWTH BY HALF OF DSTAG, NOT DSTAG. DIXON 3-9-93

        if (ISTAGF(ISPC).ne.0)HTG(I) = HTG(I)*(DSTAG+1.0)*.5
        if (HTG(I) .lt.0.1) HTG(I) = 0.1
        if (DEBUG)&
             &write(JOSTND,*)' HTGF I,ISPC,XWT,HHE1,HHE2,HHU1,HHU2,HT,HTG = ',&
             &I,ISPC,XWT,HHE1,HHE2,HHU1,HHU2,HT(I),HTG(I)

        !  CALCULATE HEIGHT GROWTH
        !  NEGATIVE HEIGHT GROWTH IS NOT ALLOWED

        HTG(I) = HTG(I)*EXP(HTCON(ISPC))*SCALE*XHMULT(ISPC)

        !  LIMIT BLACK HILLS WHITE SPRUCE TO 90 FEET.
        !  LIMIT BLACK HILLS ASPEN AND PAPER BIRCH TO 54 FEET.
        !     HFUT=HNOW+HTG(I)
        !     IF(IMODTY.EQ.3 .AND. ISPC.EQ.17.AND. HFUT.GT.90.)HTG(I)=0.1
        !     IF(IMODTY.EQ.3 .AND. ISPC.EQ.18.AND. HFUT.GT.90.)HTG(I)=0.1
        !     IF(IMODTY.EQ.3 .AND. ISPC.EQ.19.AND. HFUT.GT.90.)HTG(I)=0.1
        !     IF(IMODTY.EQ.3 .AND. ISPC.EQ.20.AND. HFUT.GT.54.)HTG(I)=0.1
        !     IF(IMODTY.EQ.3 .AND. ISPC.EQ.28.AND. HFUT.GT.54.)HTG(I)=0.1

        161 continue

        !    APPLY DWARF MISTLETOE HEIGHT GROWTH IMPACT HERE,
        !    INSTEAD OF AT EACH FUNCTION IF SPECIAL CASES EXIST.

        HTG(I) = HTG(I)*MISHGF(I,ISPC)
        TEMHTG = HTG(I)

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (.not. LTRIP) go to 9002
        ITFN = ITRN + 2*I - 1
        HTG(ITFN) = TEMHTG

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(ITFN)+HTG(ITFN)).gt.SIZCAP(ISPC,4)) then
           HTG(ITFN) = SIZCAP(ISPC,4)-HT(ITFN)
           if (HTG(ITFN) .lt.0.1) HTG(ITFN) = 0.1
        end if

        HTG(ITFN+1) = TEMHTG

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(ITFN+1)+HTG(ITFN+1)).gt.SIZCAP(ISPC,4)) then
           HTG(ITFN+1) = SIZCAP(ISPC,4)-HT(ITFN+1)
           if (HTG(ITFN+1) .lt.0.1) HTG(ITFN+1) = 0.1
        end if

        if (DEBUG)write(JOSTND,9001)HTG(ITFN),HTG(ITFN+1),ISPC,I
        9001 format('  LOWER HTG = ',F8.4,'  UPPER HTG = ',F8.4,2I5)
        9002 continue
        300 continue
     end do

     ! END OF SPECIES LOOP

     400 continue
  end do
  return

  entry HTCONS

  !  LOAD HTCON.  IF CORRECTION TERMS ARE TO BE USED, MODIFY ACCORDINGLY

  !  LOAD OVER INTERCEPT FOR EACH SPECIES

  do ISPC = 1,MAXSP
     HTCON(ISPC) = 0.0
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     50 continue
  end do

  return
end
