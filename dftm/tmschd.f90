!===================!
!   dftm : tmschd   !
!===================!
! DFTM $Id$
subroutine TMSCHD
  implicit none

  !     THIS ROUTINE IS CALLED ONCE PER SIMULATION FROM DFTMIN AND
  !     SCHEDULES ALL OF THE NECESSARY OUTBREAK BEGINNING CYCLES.
  !     IF 'RANSCHED' OPTION IS SPECIFIED, THIS ROUTINE ALSO SCHEDULES
  !     THE TIMING OF REGIONAL OUTBREAKS.

  !     N.L. CROOKSTON      MAY 1978 & JUNE 1981    INT -- MOSCOW

  !     THE CONCEPT FOR THE 'RANSCHED' OPTION IS FROM A. R.
  !     STAGE. INT -- MOSCOW.

  ! Revision History:
  !   01-APR-2013 Lance R. David (FMSC)
  !      A few variables defined locally were already defined
  !      in a common block. Local declaration removed.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'TMCOM1.f90'

  !     INTERNAL VARIABLES

  !     SEL   = A UNIFORM RANDOM NUMBER (0-1)
  !     NYR   = THE NUMBER OF DRAWS MINUS 1 NECESSARY FOR THE
  !             EVENT TO OCCUR.
  !     NEXT  = THE YEAR OF THE NEXT OUTBREAK.

  !     THE OUTBREAK YEARS ARE ASSIGNED TO A PREVIOUSLY SCHEDULED
  !     CYCLE IF THE RESOLUTION OF THE SYSTEM CAN BE MAINTAINED.

  !     TMRES = A RESOLUTION FACTOR USED TO CALCULATE MOVE
  !     TMBASE= TUSSOCK MOTH MODEL TIME BASE...5 YEARS.
  !     MOVE  = THE MAX. NUMBER OF YEARS AN OUTBREAK WILL BE MOVED.
  !     MOVE  = IFIX ( FLOAT(TMBASE) * TMRES )
  !     IYI   = SUBSCRIPT OF ARRAY IY.
  !     LAST  = THE LAST CYCLE YEAR.
  !     IDIFF = THE DIFFERENCE IN YEARS BETWEEN THE TIME THE NEXT
  !             OUTBREAK SHOULD OCCUR AND THE CLOSEST CYCLE YEAR
  !     MSCH  = COUNTER WHICH KEEPS TRACK OF THE MANUALLY SCHEDULED
  !             OUTBREAKS WHICH HAVE BEEN SPECIFIED BY DATE.
  !     NMSCH = THE NUMBER OF MANUALLY SCHEDULED OUTBREAKS SPECIFIED
  !             BY DATE.
  !     ITMYR = VECTOR OF MANUALLY SCHEDULED TM-YRS (VIA DATE). ITMYR IS
  !             EQUIVALENT TO TMYRS; HOWEVER IN THE IBM VERSION ITMYR
  !             IS HALF-WORD.

  ! Variables
  integer :: I,I1,IBOUND,IDIFF,INVYR,IPAST,ISPOT,ITMYR(41),IYI,IYLAST,KODE,LAST,MOVE,MSCH,NEXT,NMSCH,&
       &NYR,TMBASE

  real    :: PRMS(1),SEL

  equivalence (ITMYR(1), TMYRS(1))

  data TMBASE / 5 /, MOVE / 2 /

  if (ITMSCH .ne.1) goto 10
  MSCH = 0
  NMSCH = ITMYR(41)
  ITMYR(41) = 0
  if (NMSCH .eq.0) return
  call IQRSRT(ITMYR, NMSCH)

  10 continue
  INVYR = IY(1)
  LAST = IY(NCYC + 1)
  I1 = 1
  IPAST = TMPAST

  30 continue
  IYLAST = NCYC + 1

  !     FIND THE DATE OF THE NEXT REGIONAL OUTBREAK

  if (ITMSCH .ne.1) goto 35
  MSCH = MSCH + 1
  if (MSCH .gt. NMSCH) return
  NEXT = ITMYR(MSCH)
  ITMYR(MSCH) = 0
  goto 65

  35 continue
  NYR = 0

  40 continue
  call TMRANN(SEL)
  if (SEL .le. TMEVNT) goto 60
  NYR = NYR + 1
  if (NYR .lt.300) goto 40

  !     THE EVENT PROBABILITY (TMEVNT) MAY BE TOO DAMN SMALL OR
  !     EQUAL TO ZERO.

  write (JOSTND,50) TMEVNT
  50 format (//,'***** WARNING:  TUSSOCK MOTH EVENT PROBABILITY',&
       &' EQUALS:',E13.7,'.  NO OUTBREAKS SCHEDULED.')
  return

  60 continue
  NEXT = IPAST + TMWAIT + NYR

  65 continue
  if (NEXT .ge. LAST - MOVE) return
  if (NEXT .lt. INVYR) NEXT = INVYR
  IPAST = NEXT

  !     FIND THE SUBSCRIPT OF THE IY WITH THE SMALLEST ABSOLUTE
  !     DISTANCE FROM NEXT.

  IYI = 0
  IDIFF = 9999

  do I = I1, IYLAST
     if (IABS(IY(I) - NEXT) .gt. IABS(IDIFF)) goto 70
     IDIFF = NEXT - IY(I)
     IYI = I
     70 continue
  end do

  if (IYI .gt.0) goto 80
  write (JOSTND,75)
  75 format ( //,'***** ERROR IN SUBROUTINE TMSCHD.')
  stop 200

  80 continue
  if (IABS(IDIFF) .le. MOVE) goto 160
  if (IYI .ge.40) return

  !     A CYCLE NEEDS TO BE INSERTED.

  if (IDIFF .lt.0) goto 120
  IBOUND = IDIFF
  if (IABS(IY(IYI + 1) - NEXT - TMBASE) .le. MOVE)&
       &IBOUND = IY(IYI + 1) - TMBASE - IY(IYI)
  go to 150

  120 continue
  IBOUND = IY(IYI) - IY(IYI-1) - TMBASE
  if (-IDIFF .gt. TMBASE+MOVE) IBOUND = IY(IYI) - IY(IYI-1) + IDIFF
  IYI = IYI - 1
  150 continue

  call INSCYC(IYI, IBOUND, ISPOT, .false., .true., JOSTND, TMDEBU)
  if (ISPOT .le.0) return
  IYI = ISPOT

  160 continue

  !     IF TM-YRS WERE MANUALLY SCHEDULED THEY ALREADY EXIST AS
  !     ACTIVITIES.

  if (ITMSCH .eq.1) goto 30
  call OPNEW (KODE, NEXT, 811, 0, PRMS)
  if (KODE .ne.0) return
  I1 = IYI
  goto 30

end
