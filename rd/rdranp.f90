!=================!
!   rd : rdranp   !
!=================!
! RD $Id$
  ! Variables
real function RDRANP(PROP)
  implicit none

  !  THIS FUNCTION WILL RETURN A PROPORTION BASED ON A RANDOM DRAW GIVEN
  !  A MEAN PROPORTION.  IT IS BASED ON A BINOMIAL PROBABILITY
  !  DENSITY FUNCTION. IT IS THE SAME AS THE RRDPRP FUNCTION USED BY THE
  !  WESTERN ROOT DISEASE MODEL EXCEPT THAT IT CALLS THE ROOT DISEASE RANDOM
  !  NUMBER GENERATOR INSTEAD.

  !  CALLED BY :
  !     RDINF   [ROOT DISEASE]
  !     RDSETP  [ROOT DISEASE]

  !  CALLS :
  !     RDRANN  (FUNCTION)   [ROOT DISEASE]

  !  PARAMETERS :
  !     PROP     - PROPORTION THAT IS TO BE RANDOMIZED

  !  LOCAL VARIABLES :
  !     CDF      - ARRAY THAT HOLDS THE VALUES FROM THE CUMULATIVE
  !                DISTRIBUTION FUNCTION
  !     EXPROP   - THE EXPECTED PROPORTION
  !     INTNUM   - NUMBER OF ITERATIONS TO RUN THE FUNCTION
  !     K        - COUNTER FOR ITERATION CODE
  !     L        - COUNTER FOR EXPECTED PORPORTION LOOP
  !     LREV     - LOGICAL VARIABLE THAT IS SET TO .TRUE. IF PROPIN > 0.5
  !                AND 1-PROPIN IS USED FOR THE CALCULATIONS.
  !     OLDPRP   - PROPIN FROM THE PREVIOUS CALL TO THIS FUNCTION
  !                (OLD PROPIN)
  !     PDF      - TEMPORARY VARIABLE THAT HOLDS THE VALUE FROM THE
  !                PROBABILITY DENSITY FUNCTION
  !     PROPIN   - PROPORTION THAT IS TO BE RANDOMIZED. SET TO PROP AT
  !                THE BEGINNING OF THE ROUTINE
  !     RANNUM   - RANDOM NUMBER RETURNED FROM THE RANDOM NUMBER
  !                GENERATOR

  !  REVISION HISTORY:
  !    09/04/92
  !    01/28/09 - Lance R. David
  !      Common block array CDF was moved to its own common due to compiler
  !      complaint (mixed variable types in common) reported by Don Robinson
  !      (ESSA).
  !   09/09/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !     Changed array CDF to REAL, double precision was not necessary.
  !     Moved local commons PRPDAT and PRPDATD to RDCOM.F77 file.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'RDCOM.f90'

  logical :: LREV
  integer :: INTNUM,K,L
  real    :: EXPROP,PROP,PROPIN,RANNUM,RDRANN
  real    :: PDF

  !     OLDPRP AND CDF WERE PUT IN A COMMON BLOCKS SO THAT THEY WOULD BE
  !     SAVED BETWEEN CALLS TO THIS FUNCTION
  !     The variable declatations (REAL OLDPRP, DOUBLE PRECISSION CDF(1001))
  !     and common block specifications were moved to the RDCOM.F77 file
  !     and then the RDCOM.F77 included in this routine.

  !      COMMON  /PRPDAT/ OLDPRP
  !      COMMON  /PRPDATD/ CDF

  !     INITIALIZE L AND LREV, SET PROPIN TO PROP

  L = 0
  LREV = .false.
  PROPIN = PROP

  !     INTNUM (N) IS  EQUAL TO 5 / PROPIN.

  INTNUM = NINT(5 / PROPIN)

  if (INTNUM .gt.100) INTNUM = 100
  if (INTNUM .lt.10)  INTNUM = 10

  !     ADJUST PROPIN TO ACCOUNT FOR THE SHORTENING OF RANGE FROM
  !     0 <= X <= 1  TO  0 < X <= 1 .

  !     WHEN THE RANGE IS 0 < X <= 1 THE EXPECTED MEAN PROPORTION IS:
  !     E = P / (1 - (1-P)**n)

  100 continue
  EXPROP = PROPIN / (1.0 - (1.0 - PROPIN)**INTNUM)
  if (ABS(PROPIN - EXPROP) .gt. (1.0 / real(INTNUM)) .and.&
       &L .lt.10) then
     PROPIN = PROPIN - (0.5 * (PROPIN - EXPROP))
     L = L + 1
     goto 100
  end if

  !     CHECK TO SEE IF PROPIN > 0.5
  !     IF PROPIN > 0.5 THEN SET PROPIN TO 1-PROPIN AND SET LREV TO .TRUE.

  !     IF THE PROPORTION IS GREATER THAN 0.5 THEN USE THE CURVE FROM 0.0
  !     TO 0.5 AND SET THE PROPORTION BASED ON 1-(THE MEAN PROPORTION)
  !     AFTER THE RANDOM PROPORTION IS CALCULATED THEN THE PROPORTION IS
  !     REVERSED AGAIN PROPIN = 1-(RANDOM PROPORTION CALCULATED)

  if (PROPIN .gt.0.5) then
     PROPIN = 1.0 - PROPIN
     LREV = .true.
  end if

  !     THIS SECTION OF CODE ONLY NEEDS TO BE EXECUTED IF THE PROPORTION
  !     READ IN HAS CHANGED SINCE THE LAST TIME THIS FUNCTION WAS CALLED

  if (PROPIN .ne. OLDPRP) then

     !        SET OLDPRP TO PROPIN TO SAVE PROPIN

     OLDPRP = PROPIN

     !        SET PDF AND CDF FOR FIRST ITERATION

     PDF = (1.0 - PROPIN)**INTNUM
     CDF(1) = PDF
     !ccc         write(*,*) 'cdf 1 = ', pdf

     !        CALCULATE PDF FOR ALL ITERATIONS AND SET CDF

     !        PDF(K|P,N) = PDF(K-1|P,N) * (P/(1-P)) * (N-K+1) / K

     do K = 1,INTNUM

        if (PDF.gt.1E-15) then
           PDF = PDF * (PROPIN / (1.0-PROPIN) *&
                &real(INTNUM - K + 1)) / real(K)
        else
           PDF = 0.
        end if

        !           ADJUST CDF VALUE TO ACCOUNT FOR THE TRUNCATED RANGE
        !           0 < X <= 1

        CDF(K+1) = CDF(K) + (PDF / (1.0 - CDF(1)))
        !ccc         write(*,*) 'cdf x = ', cdf(k+1)
        200 continue
     end do
  end if

  !     LOOP TO GET RANDOM NUMBER THAT IS GREATER THAN ZERO

  300 RANNUM = RDRANN(0)
  if (RANNUM .eq.0.0) then
     goto 300
  end if

  !     INITIALIZE RDRANP AND K

  RDRANP = 0.7 * (1.0 / real(INTNUM))
  K = 1

  !     LOOP THROUGH THE CURVE CDF(K+1) UNTIL THE RANDOM NUMBER FITS
  !     THE CURVE

  400 continue
  if (RANNUM .ge. CDF(K+1) .and. K .le. INTNUM) then
     RDRANP = real(K+1) / real(INTNUM)
     K = K + 1
     goto 400
  end if

  !     IF PROPIN WAS > 0.5 THEN RESET RDRANP TO 1.0-RDRANP
  !     BECAUSE PROPIN WAS SET TO 1.0-PROPIN

  if (LREV) then
     RDRANP = 1.0 - RDRANP
  end if

  !ccc      write (*,*) 'oldprp = ', oldprp
  !ccc      write (*,*) 'cdf = ', cdf

  return
end
