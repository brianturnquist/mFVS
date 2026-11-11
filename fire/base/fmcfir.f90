!========================!
!   fire/base : fmcfir   !
!========================!
! FIRE-BASE $Id$
subroutine FMCFIR (IYR,FMOIS,WMULT,SWIND,CFTMP,OINIT1,OACT1,HPA)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMPOFL
  !                  FMBURN

  !  PURPOSE:  CALCULATES THE CROWN FIRE INFORMATION

  !  CALL LIST DEFINITIONS:
  !     FMOIS    FIRE TYPE BEING CALCULATED
  !     WMULT    ACTUAL WIND MODIFIER
  !     SWIND    WINDSPEED BEFORE MODIFIER
  !     CFTMP    LABEL FOR TYPE OF FIRE
  !     OINIT1   TORCHING INDEX
  !     OACT1    CROWNING INDEX

  !  LOCAL VARIABLE DEFINITIONS:

  !  COMMON BLOCK VARIABLES AND PARAMETERS:
  !     ACTCBH   ACTUAL CROWN BASE HEIGHT (FT) (-1 means that there isn't one)
  !     CBD      CROWN BULK DENSITY (METRIC UNITS)

  !     PARAMETER STATEMENTS.

  !     PARAMETER INCLUDE FILES.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !     COMMON INCLUDE FILES.
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'

  !     VARIABLE DECLARATIONS.

  ! Variables
  integer :: IYR,FMOIS
  integer :: SWIND
  integer :: OLDND,OLDNL,IRTNCD
  real    :: OLDFWG(2,7),OLDMEX(3),OLDMPS(2,3),OLDEPT
  real    :: HPA,HPA2,HPA3,B,DIFF,BOUNDL,BOUNDU
  real    :: INIT1,RACT
  real    :: OINIT1(3),OACT1(3)
  character(len=8) :: CFTMP
  logical :: DEBUG
  integer :: I,J
  real    :: WMULT,SAVWND,FLAME,BYRAM,RINIT1,CFB

  !     INITIALIZE VARAIBLES THAT ARE NOT PASSED AS PARAMETERS.

  I = 0
  J = 0
  IRTNCD = 0
  OLDND = 0
  OLDNL = 0
  B = 0.0
  BOUNDL = 0.0
  BOUNDU = 0.0
  BYRAM = 0.0
  CFB = 0.0
  DIFF = 0.0
  HPA2 = 0.0
  HPA3 = 0.0
  INIT1 = 0.0
  FLAME = 0.0
  OLDEPT = 0.0
  RACT = 0.0
  RINIT1 = 0.0
  SAVWND = 0.0

  call DBCHK (DEBUG,'FMCFIR',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,FMOIS,CBD
  7 format(' ENTERING FMCFIR CYCLE=',I3,' FMOIS=',I2,' CBD=',F7.3)

  !     IF THE CROWN IS VERY SPARSE, THEN CAN HAVE NO CROWN FIRE, SO IGNORE
  !     IF (ACTCBH .LE. 0 .OR. CBD .LE. 0.0) THEN

  if (CBD .le.0.0) then
     CFTMP = 'SURFACE'
     CRBURN = 0.0
     OINIT1(FMOIS) = -1.0
     OACT1(FMOIS) = -1.0
     return
  end if

  INIT1 = 0.0
  !      IF (ACTCBH .NE. -1) INIT1 = (4.078 * ACTCBH)**(3.0/2.0)  ! original line
  !      IF (ACTCBH .NE. -1) INIT1 = (4.065 * ACTCBH)**(3.0/2.0)  ! new coefficient
  !      IF (ACTCBH .NE. -1) THEN
  !        INIT1 = (0.3048*(460+25.9*FOLMC))/100*(0.2891)**(2/3)  ! with FMC
  !        INIT1 = (INIT1 * ACTCBH)**(3.0/2.0)
  !      ENDIF
  if (ACTCBH .ne. -1) then
     INIT1 = ((460+25.9*FOLMC))*.001333 ! with FMC simplified
     INIT1 = (INIT1 * ACTCBH)**(3.0/2.0)
  end if

  !     GET THE FUEL MODEL 10 INFORMATION (AND BACKUP THE EXISTING INFO)
  !     (COPIED FROM FMCFMD)

  OLDND = ND
  OLDNL = NL
  OLDEPT = DEPTH
  OLDFWG(1,7) = FWG(1,7)
  OLDFWG(2,7) = FWG(2,7)
  do I = 1,3
     OLDMPS(1,I) = MPS(1,I)
     OLDMPS(2,I) = MPS(2,I)
     OLDFWG(1,I) = FWG(1,I)
     OLDFWG(2,I) = FWG(2,I)
     J = I + 3
     OLDFWG(1,J) = FWG(1,J)
     OLDFWG(2,J) = FWG(2,J)
     OLDMEX(I) = MEXT(I)
  end do
  MPS(1, 1) = 2000
  MPS(1, 2) = 109
  MPS(1, 3) = 30
  MPS(2, 1) = 1500
  ND = 3
  NL = 1
  FWG(1, 1) = .138
  FWG(1, 2) = .092
  FWG(1, 3) = .23
  FWG(2, 1) = .092
  DEPTH = 1.0
  MEXT(1) = .25

  !     CHANGE THE DEFAULT WIND AND MOISTURE CONDITIONS, IF NECESSARY
  !     SAVE THE CANOPY-CORRECTED WIND SPEED SO THAT IT IS THE REPORTED VALUE

  SAVWND = FWIND
  FWIND = SWIND * 0.4

  !     CALL FMFINT TO GET THE INTERMEDIATE VALUES THAT WE NEED FOR THE CALC.

  call FMFINT(IYR, BYRAM, FLAME, 2, HPA2, 1)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  FWIND = SAVWND

  !     DO THE CROWN CALCULATIONS

  B = 0.02526 * (SSIGMA(FMOIS)**0.54)

  !C     C = 7.47 * EXP(-0.133 * (SSIGMA(FMOIS)**0.55))
  !C     E = 0.715 * EXP(-3.59 * (10**(-4.0)) * SSIGMA(FMOIS))

  !     CROWNING INDEX (O'ACTIVE)

  if (SIRXI(2) .lt.00001) then
     OACT1(FMOIS) = -1
  else
     OACT1(FMOIS) = ((2.95 * SRHOBQ(2) / (SIRXI(2) * CBD))&
          &- SPHIS(2) - 1.0) / 0.001612
     if (OACT1(FMOIS) .gt.0.0) then
        OACT1(FMOIS) = (OACT1(FMOIS)**0.7) * 0.01137 / 0.4
     else
        OACT1(FMOIS) = 0.0
     end if
  end if

  !     GET MORE VALUES FOR CALC THE CROWN TYPE VALUES

  RACT = 3.34 * SFRATE(2)
  RINIT1 = 0
  if (HPA .gt.0) RINIT1 = 60.0*INIT1 / HPA

  !     RESET THESE VALUES BACK SINCE THEY WERE RESET FOR USE OF FM 10.

  ND = OLDND
  NL = OLDNL
  DEPTH = OLDEPT
  FWG(1,7) = OLDFWG(1,7)
  FWG(2,7) = OLDFWG(2,7)

  do I = 1,3
     MPS(1,I) = INT(OLDMPS(1,I))
     MPS(2,I) = INT(OLDMPS(2,I))
     FWG(1,I) = OLDFWG(1,I)
     FWG(2,I) = OLDFWG(2,I)
     J = I + 3
     FWG(1,J) = OLDFWG(1,J)
     FWG(2,J) = OLDFWG(2,J)
     MEXT(I) = OLDMEX(I)
  end do

  !     NEW TORCHING INDEX CALCULATION
  !     CALL FMFINT AND ITERATE WITH DIFFERENT WIND SPEEDS UNTIL THE
  !     PREDICTED SPREAD RATE VALUE EQUALS THE CRITICAL VALUE FOR INITIATION.
  !     USE THE OLD CALCULATION AS A STARTING POINT.
  !     THIS FIXES THE PROBLEM WHERE THE FIRE TYPE BASED ON TI IS
  !     INCONSISTENT WITH THE FIRE TYPE BASED ON SPREAD RATES (I.E. THE
  !     NONLINEARITY PROBLEM THAT SOMETIMES CROPPED UP WHEN INTERPOLATING
  !     BETWEEN FUEL MODELS.)

  SAVWND = FWIND

  if ((ACTCBH .ne. -1) .and. (HPA .gt.0)) then
     OINIT1(FMOIS) = ((60.*INIT1*SRHOBQ(FMOIS) / (HPA*SIRXI(FMOIS)))&
          &- SPHIS(FMOIS) - 1.0) / SCBE(FMOIS)
     if (OINIT1(FMOIS) .gt.0.0) then
        OINIT1(FMOIS) = (OINIT1(FMOIS)**(1/B)) * 0.01137 / WMULT
     else
        OINIT1(FMOIS) = 0.0
     end if

     FWIND = 999 * WMULT
     call FMFINT(IYR, BYRAM, FLAME, 2, HPA3, 2)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return

     if (SFRATE(2) .lt. RINIT1) then
        OINIT1(FMOIS) = 999
        goto 205
     end if

     if (OINIT1(FMOIS) .ge.999) OINIT1(FMOIS) = 999

     FWIND = OINIT1(FMOIS) * WMULT
     call FMFINT(IYR, BYRAM, FLAME, 2, HPA3, 2)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return

     DIFF = SFRATE(2) - RINIT1
     if ((DIFF .le..001) .and. (DIFF .ge. -.001)) goto 205

     do I = 1,1000
        if (I .eq.1) then
           if (DIFF .gt..001) then
              BOUNDL = 0
              BOUNDU = OINIT1(FMOIS)
           else
              BOUNDL = OINIT1(FMOIS)
              BOUNDU = 999
           end if
        end if
        OINIT1(FMOIS) = (BOUNDU + BOUNDL)/2
        FWIND = OINIT1(FMOIS) * WMULT
        call FMFINT(IYR, BYRAM, FLAME, 2, HPA3, 2)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return

        DIFF = SFRATE(2) - RINIT1
        if (DEBUG) write(JOSTND,*) 'ITER NUMBER = ', I
        if (DEBUG) write(JOSTND,*) 'BOUNDL = ', BOUNDL
        if (DEBUG) write(JOSTND,*) 'BOUNDU = ', BOUNDU
        if ((DIFF .le..001) .and. (DIFF .ge. -.001)) goto 205
        if (DIFF .gt..001) BOUNDU = OINIT1(FMOIS)
        if (DIFF .lt. -.001) BOUNDL = OINIT1(FMOIS)
        if (BOUNDU .le.0) goto 205
        if ((BOUNDU .gt.0) .and. (BOUNDU .lt..00000000001)) then
           OINIT1(FMOIS) = 0.0
           goto 205
        end if
        200 continue
     end do
     205 continue
     if (DEBUG) write(JOSTND,*) 'ITER NUMBER = ', I
     if (DEBUG) write(JOSTND,*) 'DIFF = ', DIFF
     if (DEBUG) write(JOSTND,*) 'SFRATE(2) = ',SFRATE(2)
     !        SET 999 AS THE MAX TORCHING INDEX
     OINIT1(FMOIS) = MIN(OINIT1(FMOIS), 999.)
  else

     !     THE FIRE DOESN'T REALLY BURN OR THERE WAS TOO LITTLE CROWN, SO SET
     !     A -1 TO INDICATE A NULL VALUE

     OINIT1(FMOIS) = -1.0
  end if

  FWIND = SAVWND

  !  THIS BLOCK OF OLD CODE IS HERE FOR DOCUMENTATION

  !     FIRE TYPE

  !      RACT1 = 9.84249/CBD ! critical spread rate for active crown fire (ft/min)

  !      IF (RACT .GE. RACT1) THEN
  !        IF (SFRATE(FMOIS) .GE. RINIT1) THEN
  !          CFTMP = 'ACTIVE'
  !          CRBURN = 1.0
  !          FIRTYPE = 1
  !          RFINAL = RACT
  !        ELSE
  !          CFTMP = 'COND_CRN'
  !          CRBURN = 1.0
  !          FIRTYPE = 1
  !          RFINAL = RACT
  !        ENDIF
  !      ELSEIF (SFRATE(FMOIS) .GE. RINIT1) THEN
  !        CFTMP = 'PASSIVE'
  !        FIRTYPE = 2
  !      ELSE
  !        CFTMP = 'SURFACE'
  !        CRBURN = 0.0
  !        FIRTYPE = 3
  !        RFINAL = SFRATE(FMOIS)
  !      ENDIF

  !  END BLOCK OF OLD CODE

  if (OINIT1(FMOIS) .gt. SWIND) then
     if (OACT1(FMOIS) .gt. SWIND) then
        CFTMP = 'SURFACE'
        CRBURN = 0.0
        FIRTYPE = 3
        RFINAL = SFRATE(FMOIS)
     else
        CFTMP = 'COND_CRN'
        CRBURN = 1.0
        FIRTYPE = 1
        RFINAL = RACT
     end if
  else if (OACT1(FMOIS) .gt. SWIND) then
     CFTMP = 'PASSIVE'
     FIRTYPE = 2
  else
     CFTMP = 'ACTIVE'
     CRBURN = 1.0
     FIRTYPE = 1
     RFINAL = RACT
  end if
  if ((OINIT1(FMOIS) .eq. -1) .or. (OACT1(FMOIS) .eq. -1)) then
     CFTMP = 'SURFACE'
     CRBURN = 0.0
     FIRTYPE = 3
     RFINAL = SFRATE(FMOIS)
  end if

  !      FOR PASSIVE FIRES, CALCULATE THE CROWN FRACTION BURNED USING
  !      THE STRAIGHT LINE FORMULA, LIKE IN NEXUS
  !      (SCOTT AND REINHARDT 2001 PG 41)

  !     CALL FMFINT WITH THE CROWNING INDEX AS THE WINDSPEED TO GET R'SA

  if (CFTMP .eq. 'PASSIVE') then
     SAVWND = FWIND
     FWIND = OACT1(FMOIS) * WMULT
     call FMFINT(IYR, BYRAM, FLAME, 2, HPA2, 2)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return

     FWIND = SAVWND
     CFB = (SFRATE(FMOIS) - RINIT1)/(SFRATE(2) - RINIT1)
     RFINAL = SFRATE(FMOIS) + CFB * (RACT - SFRATE(FMOIS))
     CRBURN = CFB
     if (CRBURN .gt.1.0) CRBURN = 1.0
  end if

  if (DEBUG) write(JOSTND,*) 'SFRATE(FMOIS) = ',SFRATE(FMOIS)
  if (DEBUG) write(JOSTND,*) 'SFRATE(2) = ',SFRATE(2)
  if (DEBUG) write(JOSTND,*) 'RACT = ',RACT
  if (DEBUG) write(JOSTND,*) 'RINIT1 = ',RINIT1
  if (DEBUG) write(JOSTND,*) 'CRBURN = ',CRBURN

  if (DEBUG) write(JOSTND,8) FMOIS,CFTMP,OINIT1(FMOIS),&
       &OACT1(FMOIS), RFINAL
  8 format(' FMCFIR FMOIS=',I2,' CFTMP=',A,' OINIT1=',F13.3,&
       &' OACT1=',F13.3, ' RFINAL=',F13.3)

  return
end
