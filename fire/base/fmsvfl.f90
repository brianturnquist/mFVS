!========================!
!   fire/base : fmsvfl   !
!========================!
! FIRE-BASE $Id$
subroutine FMSVFL (NOUT)
  implicit none

  !     FIRE MODEL STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- JANUARY 2000
  !     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
  !     D. ROBINSON   -- ESSA        -- MAY 2005

  !     INPUT:
  !     NOUT  =THE OUTPUT FILE REFERENCE NUMBER

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'SVDATA.f90'
  include 'FMSVCM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'METRIC.f90'

  external SVRANN
  ! Variables
  integer :: NOUT,NFLF,NFLAMES,NFLMS,IFLAMES,II,IFLR0T
  real    :: WIDTHMIN,TILTBASE,WMAX,WIDTH,FLMX,FLMY,BACHLO,TEMP,TEMP2,TMPDIST,TMPPCT,TMPVAR,TMPHEIGHT,&
       &FLMHT,FMWDTH,FLMWDTH,X,FLMTILT,XD,XR,XR2

  !     OUTPUT THE FIRE LINE

  write (NOUT,10) '#FIRE_LINE ',FMY2
  10 format (A,100(1X,F6.2))

  !     OUTPUT THE FUEL LOADS BEFORE AND AFTER THE FIRE LINE

  write (NOUT, 10) '#PRE_FIRE_LOAD',TCWD
  write (NOUT, 10) '#POST_FIRE_LOAD',TCWD2

  !     CALCULATE THE GEOMETRY INFORMATION USED BY THE FLAME OUTPUT.

  !     WIDTH REFERS TO THE MAX-X VALUE OF THE STAND.  IT IS SHIFTED
  !     BY MINWIDTH.

  WIDTHMIN = 0.

  !     CALCULATE THE FLAMES TILT ANGLE BASED ON THE WIND.

  if (FWIND.le.1.) then
     TILTBASE = 5.
  else if (FWIND.le.10.) then
     TILTBASE = 15.
  else if (FWIND.le.35.) then
     TILTBASE = 30.
  else
     TILTBASE = 40.
  end if

  !     FOR ONLY PASSIVE (TORCHING) OR SURFACE FIRES, DRAW SOME FLAMES
  !     ON THE GROUND IN A FLAME WALL.

  if (FIRTYPE .eq.1) then
     NFLF = 0
  else
     NFLF = 250
  end if

  if (IMETRIC.eq.0) then
     if (IPLGEM .le.1) then
        WMAX = 208.71
        WIDTH = 208.71
     else
        WMAX = 235.50
     end if
  else
     if (IPLGEM .le.1) then
        WMAX = 100.0
        WIDTH = 100.0
     else
        WMAX = 112.84
     end if
  end if
  FLPART = WMAX / NFLPTS

  !     NFLAMES IS THE NUMBER OF RANDOM FLAME OBJECTS TO DRAW.

  NFLAMES = NFLF
  NFLMS = NFLAMES

  do IFLAMES = 1, NFLAMES

     !       INITIALIZE THE FLAMES

     if (IFLAMES .le. NFLF) then

        !         CALCULATE AN INITIAL X-VALUE.  BRANCH BACK IF THE POINT IS REJECTED

        20 continue
        call SVRANN( FLMX )
        FLMX = FLMX * WMAX

        !         FIGURE OUT WHICH FLAME SECTION THIS VALUE WOULD BE IN.

        II = IFIX( FLMX / FLPART )
        if ( II.gt.NFLPTS ) then
           II = NFLPTS
        else if ( II.lt.1 ) then
           II = 1
        end if

        !         BASE THE INITIAL Y-VALUE ON THE SECTION.

        FLMY = BACHLO( FMY2(II), 2.5, SVRANN )

        !         IF THE Y IS OUTSIDE THE ALLOWED RANGE ,
        !         THEN WE SHOULDN'T DRAW THE FLAME.

        if ( FLMY .lt.0..or. FLMY .gt. WMAX ) goto 30

        !         CHECK TO MAKE SURE THIS VALUE IS WITHIN THE STAND

        if (IPLGEM .gt.1) then

           !           Calculate the width based on FLMY
           !           D=235.50, R=117.75, R^2 = 13865.06250

           if (IMETRIC.eq.0) then
              XD = 235.50
              XR = 117.75
              XR2 = 13865.06
           else
              XD = 112.84
              XR = 56.42
              XR2 = 3183.22
           end if

           if (FLMY .gt. XR) then
              TEMP = FLMY - XR
              TEMP2 = TEMP**2
              WIDTH = 2. * SQRT(XR2 - (FLMY-XR)**2)
           else
              TEMP = XR - FLMY
              TEMP2 = TEMP**2
              WIDTH = 2. * SQRT(XR2 - (XR-FLMY)**2 )
           end if
           WIDTHMIN = (XD - WIDTH) / 2.
        end if

        !         IF THE X-VALUE DOESN'T WORK WITH THE Y-VALUE, REDO THE
        !         CALCULATION.

        if ( FLMX.lt.WIDTHMIN .or.&
             &FLMX.gt.(WIDTHMIN+WIDTH) ) goto 20

        !         CALCULATE THE FLAME HEIGHT.  IN FRONT OF THE MAIN WALL, THE
        !         FLAMES WILL LIKELY BE ABOUT 10% FULL, 100% AT THE MAIN WALL
        !         AND TAILING OFF WITH AN F(X) = X^3 FUNCTION.

        if ( FLMY.gt.(FMY2(II)+1.0) ) then

           !           TMPDIST IS GUARANTEED TO BE GREATER THAN 1.0 AT THIS POINT

           TMPDIST = FLMY - FMY2(II)
           TMPPCT = 1.0 / TMPDIST
           TMPVAR = 1.0 - TMPPCT**2
           TMPHEIGHT = FLAMEHT - ((FLAMEHT * TMPVAR))
           FLMHT = BACHLO(TMPHEIGHT, TMPHEIGHT * 0.05, SVRANN)

        else if (FLMY.ge.(FMY2(II)-1.0)) then
           FLMHT = BACHLO(FLAMEHT, FLAMEHT * 0.1, SVRANN)
        else

           !           TMPDIST IS GUARANTEED TO BE GREATER THAN 1.0 AT THIS POINT

           TMPDIST = FMY2(II) - FLMY
           TMPPCT = 1.0 / TMPDIST
           TMPVAR = 1.0 - TMPPCT**3
           TMPHEIGHT = FLAMEHT - ((FLAMEHT * TMPVAR)/2.0)
           FLMHT = BACHLO(TMPHEIGHT, TMPHEIGHT * 0.05, SVRANN)
        end if
     else

        !       ANY RANDOM FLAMES WOULD BE DRAWN HERE.

     end if

     call SVRANN( FMWDTH )
     FLMWDTH = FMWDTH * FLAMEHT + 1.0
     FLMTILT = BACHLO(TILTBASE, 5.0, SVRANN)
     call SVRANN(X)
     IFLR0T = INT(X*360.)

     !       GO AHEAD AND WRITE OUT THE FIRE INFORMATION TO THE SVS FILE.

     if (IMETRIC.eq.0) then
        write (NOUT,25) IFLAMES+NSVOBJ,&
             &FLMHT,FLMTILT,IFLR0T,FLMWDTH,FLMX,FLMY,0.
     else
        write (NOUT,25) IFLAMES+NSVOBJ,&
             &FLMHT*FTtoM,FLMTILT,IFLR0T,FLMWDTH*FTtoM,FLMX,FLMY,0.
     end if

     25 format ('@flame.eob',T16,I5,' 99 0 1 0',2F6.0,I5,' 0',F6.1,&
          &' 0 0 0 0 0 0 0 1 0',3F8.2)
     30 continue
  end do

  return
end

subroutine FMGETFL(ASIZE, FLINE)

  !     **FMGETFL--FIRE   DATE OF LAST REVISION: 06/19/02

  !     FIRE MODEL GET FIRE LINE INFORMATION
  !     D.L.GAMMEL -- SEM -- JUNE 2002

  !     INPUT:
  !     ASIZE  =THE SIZE OF THE ARRAY PASSED IN
  !     FLINE  =THE ARRAY TO BE FILLED WITH THE FIRE LINE INFO

  ! Common blocks
  include 'PRGPRM.f90'
  include 'SVDATA.f90'
  include 'FMSVCM.f90'

  ! Variables
  real    :: FLINE
  integer :: ASIZE

  dimension FLINE(ASIZE)

  do I = 1,MIN(ASIZE,NFLPTS)
     FLINE(I) = FMY2(I)
  end do

end
