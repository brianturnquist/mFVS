!=========================!
!   archive/ak : oldsbb   !
!=========================!
! AK $Id: sbb.f 2472 2018-08-20 21:22:34Z gedixon $
subroutine SBB(ISPC,D,H,Z,IFLAG)
  implicit none

  ! THIS ROUTINE LOOKS UP A Z GIVEN SPECIES, DBH, AND HT.
  ! IT ALSO LOOKS UP A HEIGHT GIVEN SPECIES, DBH, AND Z.

  ! Variables
  integer :: IFLAG,ISPC,JSPC

  real    :: D,FBY1,FBY2,H,PSI,XI1,XI2,Y1,Y2,Z

  real    :: COF(9,11)

  !  DATA STATEMENTS:

  !  SPECIES ORDER
  !  1    2    3    4    5    6    7    8    9   10   11  12  13
  ! WS  WRC  PSF   MH   WH  AYC   LP   SS   SAF  RA   CW  OH  OS

  data COF/&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073,&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073,&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073,&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073,&

       ! WESTERN HEMLOCK

       &70.0,     200.0,   1.53035,    .77779,   1.17256,   1.07719,&
       &.87280,   1.67853,    .95008,&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073,&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073,&

       ! SITKA SPRUCE

       &70.0,     220.0,   1.10228,    .72391,    .99652,    .93681,&
       &.86387,   1.27598,    .91893,&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073,&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073,&
       &80.00000, 240.00000,   1.48997,    .97900,    .72187,    .68448,&
       &.94891,   1.88756,   1.00073/

  JSPC = ISPC
  if (ISPC .eq.12) JSPC = 10
  if (ISPC .eq.13) JSPC = 11

  XI2 = 4.5
  XI1 = 0.1
  if (IFLAG .eq.1) then
     if (H.le.4.5.or. D .le..1) return
     if ((XI1 + COF(1,JSPC)) .le. D) go to 180
     if ((XI2 + COF(2,JSPC)) .le. H) go to 180
     go to 190
     180 continue
     Z = 0.0
     return
     190 continue
     Y1 = (D - XI1)/COF(1,JSPC)
     Y2 = (H - XI2)/COF(2,JSPC)
     FBY1 = ALOG(Y1/(1.0 - Y1))
     FBY2 = ALOG(Y2/(1.0 - Y2))
     Z = ( COF(4,JSPC) + COF(6,JSPC)*FBY2 - COF(7,JSPC)*( COF(3,JSPC) +&
          &COF(5,JSPC)*FBY1))*(1.0 - COF(7,JSPC)**2)**(-0.5)
     return
  else
     if (D .le.0.1)return
     if ((XI1 + COF(1,JSPC)) .le. D) then
        H = COF(2,JSPC)
        return
     end if

     !  CALCULATE HEIGHT AFTER 10 YEARS

     PSI = COF(8,JSPC)*((D-XI1)/(XI1+ COF(1,JSPC) - D))**COF(9,JSPC)&
          &* (EXP(Z*((1.0 - COF(7,JSPC)**2  ))**0.5/COF(6,JSPC)))
     H = ((PSI/(1.0 + PSI))* COF(2,JSPC)) + XI2

     return
  end if
end
