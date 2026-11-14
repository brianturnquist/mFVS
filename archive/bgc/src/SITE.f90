!============================!
!   archive/bgc/src : SITE   !
!============================!

!  THIS IS A FILE OF THE VARIABLES FOR SITE CHARACTERISTICS
!  ie, the S(n) variables.
!        -- DWC, 10/93.

! Variables
integer :: YRFLAG
integer :: NFLOC,HTYPE
integer :: IHAB1,IHAB2,ILOC
real    :: S(16)
real    :: AA,BB,VOLMAX,SOILWP,VMAX(10)
real    :: SLOPE,ASPECT,ELEV,SWHC
real    :: LAT,LONG
real    :: THINYR,PTREMOVE,PSREMOVE,PGREMOVE
real    :: RADTOP,RADBOT

real    :: B1(13,3),B2(22)
common /BLBBETAS/ B1, B2

common /BLBSITE/ S, AA, BB, VOLMAX, SOILWP, RADTOP, RADBOT,&
     &YRFLAG, VMAX

common /BLBHABIT/ NFLOC, HTYPE, IHAB1, IHAB2, ILOC

common /BLBTOPO/ SLOPE, ASPECT, ELEV, SWHC,&
     &LAT, LONG

common /BLBTHIN/ THINYR, PTREMOVE, PSREMOVE, PGREMOVE
