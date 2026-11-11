!===================!
!   wwpb : bmlite   !
!===================!
! WWPB $Id$
subroutine BMLITE (ISTD,IYR)

  !     CALLED FROM: BMDRV
  !     CALLS:  BMRANN
  !             SPLAAR
  !             GPGET2

  ! **BMLITE    Date of last modification:  June 8, 1994

  ! This is a landscape level model that takes the density of the lightning
  ! strike and, by comparison with a random number, determines if a strike
  ! will occur. More than one strike can occur if the stand is large or if
  ! the strike density is high. If a strike occurs, a cummulative distribution
  ! is found based on the relative heights of trees in each size class.
  ! If a random number indicates a class, one tree in that size class is struck.
  ! Currently, there is no provision in the model for the fact that not all strikes
  ! are on trees, or for the fact that only 1/100 strikes (approx) make an
  ! attractor tree.

  ! Lightning is always active unless the user sets the strike density to 0

  ! Definitions:
  !     DENS:   from BMCOM; Strike density (strikes/acre)
  !     STRIKE: from BMCOM; Proportion of trees in class struck

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'PPCNTL.f90'
  include 'BMCOM.f90'

  !.... Variable declarations.

  parameter(MAXPRM = 7)

  ! Variables
  logical :: LOK
  integer :: ISIZ,NUMSTR
  integer :: DUM(1)
  real    :: TOTHT,TOTSTR
  real    :: PRMS(MAXPRM)
  real    :: REMAIN

  save

  !.... Check for debug.

  if (LBMDEB) write(JBMBPR,15) IYR, ISTD
  15 format(' Begin BMLITE: Year = ',I5,' Stand = ',I6)

  TOTHT = 0.0
  do ISIZ = 1,NSCL
     STRIKE(ISTD,ISIZ) = 0.0
     10 continue
  end do

  !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

  if (ICNT .ge. BMEND) ICNT = 0
  ICNT = ICNT + 1

  if (ICNT .eq.1) then

     IYR1 = IYR
     NPRMS = 2

     !     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
     !     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
     !     SET OF PARAMETERS.

     call GPGET2 (306, IYR1, MAXPRM, NPRMS, PRMS, 1, I, DUM, LOK)

     if (LOK) then

        DENS = PRMS(2)

        if (LBMDEB) write (JBMBPR,71) MICYC, IYR1, IYR2, PRMS(2)
        71 FORMAT (/' BMLITE: MICYC=', I5, 'IYR1=',I5,'                  &
             &      IYR2=',I5, 'Strikes/acre=', F7.4)

        IYR2 = IYR1 + IFIX(PRMS(1)) - 1
        if (IYR2.ge.MIY(MICYC)) then
           PRMS(1) = IYR2 - MIY(MICYC) + 1
           call GPADD (KODE, MIY(MICYC), 306, NPRMS, PRMS(1), 1, DUM)
           IYR2 = MIY(MICYC) - 1
           if (LBMDEB) write (JBMBPR,81) PRMS(1),IYR2
           81 format (/' IN BMLITE:  Lightning has been SCHEDULED FOR THE ',&
                &'NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,&
                &'  NEW ILTYR2=',I5)
        end if
     end if

     !     IF THE CURRENT PARAMETERS HAVE FINISHED, RETURN
     !     TO THE DEFAULT VALUES.

     !        IF (IYR .EQ. IYR2+1) THEN
     !          DENS= 0.
     !        ENDIF

  end if

  !     IF THE STRIKE DENSITY = 0 THEN LIGHTNING DOES NOT OCCUR.
  if (DENS .le.0.0) return

  !     Choose a random number and find the area of the stand

  call BMRANN(X)
  call SPLAAR(ISTD,SAREA,IRC)

  !     Calculate the total number of strikes that may occur in the stand (TOTSTR)
  !     Then find out how many will always occur (integer portion of TOTSTR), and
  !     use the remainder of TOTSTR to give the probability of another strike occuring.

  TOTSTR = DENS * SAREA
  NUMSTR = INT(TOTSTR)
  REMAIN = TOTSTR - NUMSTR

  if (X .le. REMAIN) NUMSTR = NUMSTR + 1
  if (NUMSTR .eq.0) goto 40

  !     Calculate the sum of the average ht in each size class to use for making
  !     a cummulative distribution.
  do ISIZ = 1,NSCL
     TOTHT = TOTHT + HTS(ISTD,ISIZ,1)
     20 continue
  end do
  if (TOTHT .le.1E-3) goto 40

  do I = 1,NUMSTR
     call BMRANN(X)

     !       scale up the random number to use for picking a size class (so that X
     !       won't be biased towards smaller dbh classes.

     if (I .eq. NUMSTR .and. REMAIN .gt.0.0) X = X / REMAIN
     PSTRIK = 0.0

     do ISIZ = 1, NSCL

        PSTRIK = PSTRIK + HTS(ISTD,ISIZ,1) / TOTHT
        if (X .le. PSTRIK .and. TREE(ISTD,ISIZ,1) .gt.1E-6) then
           !              one tree in the stand in this size class is hit
           !                  (strike is a proportion)
           PROPN = (1.0 / SAREA) / TREE(ISTD,ISIZ,1)
           if (PROPN .gt.1.0) PROPN = 1.0
           if (STRIKE(ISTD,ISIZ) .lt.1.0) then
              STRIKE(ISTD,ISIZ) = STRIKE(ISTD,ISIZ) + PROPN
              goto 35
           end if
        end if
        30 continue
     end do
     35 continue
  end do

  40 continue

  if (LBMDEB) write(JBMBPR,90) IYR, ISTD
  90 format(' End BMLITE: Year = ',I5,' Stand = ',I6)

  return
end
