!======================================!
!   Compute large tree height growth   !
!======================================!
! CS $Id$
subroutine HTGF
  implicit none

  !  THIS SUBROUTINE COMPUTES THE PREDICTED PERIODIC HEIGHT
  !  INCREMENT FOR EACH CYCLE AND LOADS IT INTO THE ARRAY HTG.
  !  HEIGHT INCREMENT IS PREDICTED FROM SPECIES, SITE INDEX,
  !  BASAL AREA/ACRE, AND DBH.  THIS ROUTINE IS CALLED FROM
  !  **TREGRO** DURING REGULAR CYCLING.  ENTRY **HTCONS**
  !  IS CALLED FROM **RCON** TO LOAD SITE DEPENDENT CONSTANTS
  !  THAT NEED ONLY BE RESOLVED ONCE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'COEFFS.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'
  include 'MULTCM.f90'
  include 'PDEN.f90'
  include 'HTCAL.f90'

  ! Variables
  logical :: DEBUG
  real    :: SCALE,XHT,BAL,YRS,H,HTG1,HTMAX,AGET,GMOD,RELHTA,TEMHTG
  integer :: ISPC,I1,I2,I3,I,MODE0,IVAR,ITFN

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'HTGF',4,ICYC)
  if (DEBUG) write(JOSTND,3)ICYC
  3 format(' ENTERING SUBROUTINE HTGF CYCLE =',I5)
  SCALE = FINT/YR

  !  GET THE HEIGHT GROWTH MULTIPLIERS.

  call MULTS (2,IY(ICYC),XHMULT)

  !  BEGIN SPECIES LOOP.

  do ISPC = 1,MAXSP
     I1 = ISCT(ISPC,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(ISPC,2)
     XHT = XHMULT(ISPC)

     !  BEGIN TREE LOOP WITHIN SPECIES LOOP

     do I3 = I1,I2
        I = IND1(I3)
        BAL = (1.0-(PCT(I)/100.))*BA

        !   CALL HTCALC TO CALCULATE AGE BASED ON CURRENT TREE HEIGHT

        MODE0 = 0
        IVAR = 2
        YRS = 10.
        H = HT(I)
        AGET = 0.
        call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX,&
             &HTG1,JOSTND,DEBUG)

        !   CALL HTCALC TO CALCULATE HTG BASED ON AGE GIVEN CURRENT TREE HEIGHT

        MODE0 = 9
        IVAR = 2
        YRS = 10.
        H = HT(I)
        HTG1 = 0.
        call HTCALC (MODE0,IVAR,ISPC,SITEAR(ISPC),YRS,H,AGET,HTMAX,&
             &HTG1,JOSTND,DEBUG)
        HTG(I) = HTG1
        if (DEBUG)write(JOSTND,*) ' I,SI,H,AGET,HTMAX,HTG1= ',&
             &I,SITEAR(ISPC),H,AGET,HTMAX,HTG1

        !  GET BAL MODIFIER BORROWED FROM THE DIAMETER GROWTH EQUATIONS AND
        !  ADJUST THE HEIGHT GROWTH ESTIMATE.
        !  POTENTIALLY ONLY AFFECT HEIGHT GROWTH HALF AS MUCH AS DIA GROWTH
        !  TEMPER THE ADJUSTMENT BY RELATIVE HEIGHT
        !  ADD A RANDOM INCREMENT COMMENSURATE WITH DG RANDOM INCREMENT.

        call BALMOD(ISPC,DBH(I),BAL,BA,GMOD,DEBUG)
        if (DEBUG)write(16,*)' AFTER BALMOD GMOD,HT,AVH= ',GMOD,HT(I),AVH
        !      GMOD = (GMOD+1.0)/2.0
        RELHTA = 0.
        if (AVH .gt.0.)RELHTA = MIN((HT(I)/AVH),1.0)
        GMOD = (1.0-((1.0-GMOD)*(1.0-RELHTA)))*0.8
        HTG(I) = HTG(I)*(1.0+OLDRN(I))*GMOD

        if (DEBUG)write(16,*)' I,HTG,RELHTA,GMOD,OLDRN= ',&
             &I,HTG(I),RELHTA,GMOD,OLDRN(I)

        if (HTG(I) .lt.0.1)HTG(I) = 0.1
        HTG(I) = SCALE*XHT*HTG(I)*EXP(HTCON(ISPC))
        if (DEBUG) write(JOSTND,*) 'I= ',I,'  SCALE= ',SCALE,'  XHT= ',&
             &XHT, '  HTG(I)= ',HTG(I),' HTCON= ',HTCON(ISPC)
        TEMHTG = HTG(I)

        ! CHECK FOR SIZE CAP COMPLIANCE.

        if ((HT(I)+HTG(I)).gt.SIZCAP(ISPC,4)) then
           HTG(I) = SIZCAP(ISPC,4)-HT(I)
           if (HTG(I) .lt.0.1) HTG(I) = 0.1
        end if

        if (.not.LTRIP) go to 30
        ITFN = ITRN+2*I-1
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

        if (DEBUG) write(JOSTND,9001) HTG(ITFN),HTG(ITFN+1)
        9001 format( ' UPPER HTG =',F8.4,'LOWER HTG =',F8.4)

        !  END OF TREE LOOP

        30 continue
     end do

     !  END OF SPECIES LOOP

     40 continue
  end do
  return

  entry HTCONS

  !  ENTRY POINT FOR LOADING HEIGHT INCREMENT MODEL COEFFICIENTS THAT
  !  ARE SITE DEPENDENT AND REQUIRE ONE-TIME RESOLUTION.  HGHC
  !  CONTAINS HABITAT TYPE INTERCEPTS, HGLDD CONTAINS HABITAT DEPENDENT
  !  COEFFICIENTS FOR THE DIAMETER INCREMENT TERM,HGH2 CONTAINS
  !  HABITAT DEPENDENT COEFFICIENTS FOR THE HEIGHT-SQUARED TERM, AND
  !  HGHC CONTAINS SPECIES DEPENDENT INTERCEPTS.  HABITAT TYPE IS
  !  INDEXED BY ITYPE (SEE /PLOT/ COMMON AREA).

  !  LOAD OVERALL INTERCEPT FOR EACH SPECIES

  do ISPC = 1,MAXSP
     HTCON(ISPC) = 0.0
     if (LHCOR2.and. HCOR2(ISPC).gt.0.0) HTCON(ISPC) = &
          &HTCON(ISPC)+ALOG(HCOR2(ISPC))
     60 continue
  end do
  return
end
