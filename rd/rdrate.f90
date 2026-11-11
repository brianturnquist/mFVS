!=================!
!   rd : rdrate   !
!=================!
! RD $Id$
subroutine RDRATE (NSIM)
  implicit none

  !  THIS SUBROUTINE CALCULATES SPREAD RATE TO APPLY TO EACH CENTER
  !  USING THE RANGE OF VALUES CALCULATED BY THE SPREAD RATE MODEL.
  !  CENTERS THAT HAD A ZERO SPREAD RATE LAST YEAR ARE MORE LIKELY TO
  !  HAVE A ZERO SPREAD RATE THIS YEAR UNLESS THOSE CENTERS ARE SHRINKING
  !  CENTERS.

  !  CALLED BY :
  !     RDSPRD  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]

  !  DEFINITIONS:
  !     RRA:    CALCULATED SPREAD RATE FOR EACH CENTER
  !     NSIM:   NUMBER OF SIMULATIONS THAT WERE DONE BY THE MONTE CARLO SPREAD MODEL
  !     MCRATE: THE RESULT OF EACH OF THE NSIM SIMULATIONS
  !     GCENTS: THE NUMBER OF NON-SHRINKING CENTERS (POTENTIALLY GROWING CENTERS)

  !  Revision History:
  !    25-APR-00 Lance R. David (FHTET)
  !       Reduced RETURN statements to 1 at the end of routine.
  !       Added Debug code.
  !    06-AUG-01 Lance R. David (FHTET)
  !       Initialization of entire arrays RRA and IPNT instead of just the number
  !       of elements represented equal to the current number of centers. That
  !       method left calculations using uninitialized elements when the number of
  !       disease centers exceeded the number of Monte Carlo simulations.
  !   09/02/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: DEBUG,LZERO

  integer :: GCENTS,I,IPNT(100),J,K,NSIM,NUM

  real    :: DIV,REM,RRA(100),TEMP,TRR

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'RDRATE',6,ICYC)

  if (DEBUG) write (JOSTND,*)&
       &'BEGIN RDRATE:  IRRSP=',IRRSP,' NCENTS=',NCENTS(IRRSP)

  !     BEGIN PROGRAM

  !     INITIALIZE VARIABLES
  do I = 1,100
     RRA(I) = 0.0
     IPNT(I) = I
     50 continue
  end do

  GCENTS = NCENTS(IRRSP) - NSCEN(IRRSP)

  if (DEBUG) write (JOSTND,*)&
       &'IN RDRATE: GCENTS=',GCENTS,' NSIM=',NSIM

  if (GCENTS .le.0) then
     RRRATE(IRRSP) = 0.0
     goto 900 ! RETURN
  end if

  LZERO = .false.
  REM = 0.0
  K = 1
  if (GCENTS .le. NSIM) then

     !       THERE ARE FEWER CENTERS THAN THERE WERE SIMULATIONS SO TAKE THE
     !       AVERAGE OF VARIOUS SIMULATIONS AS A SPREAD RATE TO APPLY

     DIV = FLOAT(NSIM) / (FLOAT(GCENTS) + 1E-9)

     do I = 1, GCENTS
        NUM = INT(DIV + REM)
        REM = DIV + REM - NUM

        do J = 1, NUM
           RRA(I) = RRA(I) + MCRATE(IRRSP,K)
           K = K + 1
           if (K .gt. NSIM) NUM = J
           100 continue
        end do

        RRA(I) = RRA(I) / (FLOAT(NUM) + 1E-9)
        if ((.not. LZERO) .and. (RRA(I) .eq.0.0)) LZERO = .true.
        150 continue
     end do
  else

     !       THERE ARE MORE CENTERS THAN THERE WERE SIMULATIONS SO APPLY EACH
     !       SIMULATED VALUE TO ONE OR MORE CENTERS.

     I = 1
     DIV = FLOAT(GCENTS) / FLOAT(NSIM)
     200 continue

     NUM = INT(DIV + REM)
     REM = DIV + REM - NUM

     do J = 1, NUM
        RRA(I) = MCRATE(IRRSP,K)
        I = I + 1
        if ((.not. LZERO) .and. (RRA(I) .eq.0.0)) LZERO = .true.
        250 continue
     end do

     K = MIN0(NSIM,K + 1)
     if (I .le. GCENTS) goto 200

  end if

  !     NOW SORT RRA ARRAY ENOUGH SO THAT THE ZEROS COME TO THE TOP (OTHER
  !     VALUES MUST REMAIN UNSORTED)

  if (LZERO) then
     do I = 1,GCENTS
        if (RRA(I) .eq.0.0) then
           do J = I,2, -1
              if (RRA(J-1) .gt.0.0) then
                 TEMP = RRA(J)
                 RRA(J) = RRA(J-1)
                 RRA(J-1) = TEMP
              else
                 goto 350
              end if
              300 continue
           end do
        end if
        350 continue
     end do

     !     SET UP POINTERS TO LAST YEAR'S SPREAD RATES TO GET ZEROS AT THE TOP
     !     AND SHRINKING CENTERS AT THE BOTTOM. IGNORE ZEROS FROM RECENTLY CREATED
     !     SPORE CENTERS
     !     (IPNT IS INITIALIZED ABOVE TO BE IPNT(I) = I)

     do I = 1,NCENTS(IRRSP)
        if (SHCENT(IRRSP,I,2) .gt.0.0) then
           do J = I,NCENTS(IRRSP)-1
              IPNT(J) = IPNT(J+1)
              425 continue
           end do
           IPNT(NCENTS(IRRSP)) = I
        else if ((RRATES(IRRSP,I) .eq.0.0) .and.&
             &(ICENSP(IRRSP,I) .eq.0)) then
           do J = I,2, -1
              IPNT(J) = IPNT(J-1)
              400 continue
           end do
           IPNT(1) = I
        end if
        450 continue
     end do
  end if

  if (DEBUG) write (JOSTND,*) 'IN RDRATE: MCRATE=',MCRATE
  if (DEBUG) write (JOSTND,*) 'IN RDRATE: RRA=',RRA

  !     ASSIGN SPREAD RATES TO CENTERS BASED ON THE POINTER ARRAY

  TRR = 0.0
  do I = 1,GCENTS
     RRATES(IRRSP,IPNT(I)) = RRA(I)
     TRR = TRR + RRA(I)
     500 continue
  end do

  if (DEBUG) write (JOSTND,*)&
       &'IN RDRATE: TRR=',TRR,' GCENTS=',GCENTS

  RRRATE(IRRSP) = TRR / (FLOAT(GCENTS) + 1E-9)

  900 continue
  if (DEBUG) write (JOSTND,*) 'EXIT RDRATE: RRRATE=',RRRATE(IRRSP)
  return
end
