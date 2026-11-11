!=========================!
!   fire/base : fmsvout   !
!=========================!
! FIRE-BASE $Id$
subroutine FMSVOUT(IYEAR,FLMHTIN,IFTYP)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- JANUARY 2000
  !     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
  !     D. ROBINSON   -- ESSA        -- MAY 2005

  !     INPUT:
  !     IYEAR   = THE YEAR PART OF THE TITLE
  !     FLMHTIN = THE HEIGHT OF THE FLAMES TO DRAW, IN FEET.
  !     IFTYP   = FIRE TYPE.  ACTIVE, PASSIVE OR SURFACE.

  !     CALLS SVOUT MULTIPLE TIMES TO DISPLAY MULTIPLE, PROGRESSIVE PICTURES
  !     OF THE FIRE BURNING THROUGH THE STAND.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'SVDATA.f90'
  include 'SVDEAD.f90'
  include 'FMSVCM.f90'
  include 'METRIC.f90'

  external SVRANN

  !     THE NEXT SET OF LOCAL VARIABLES ARE USED FOR CALCULATING HEIGHT AND
  !     VOLUME SETS FOR SNAGS TO BE REDUCED FROM THE COURSE WOODY DEBRIS FIELDS.
  !     VOL - A VECTOR RECORDING THE DIFFERENT VOLUMES AT THE VARIOUS HEIGHTS,
  !     FROM THE TOTAL LENGTH OF THE TREE THROUGH THE LENGTH OF TREE
  !     WITH 3 INCH DIAMETER AND LESS.
  !     VOL(1) - VOLUME FROM GREATHER THAN 12 INCH DIAMETER
  !     VOL(2) - VOLUME FROM 6 TO 12 INCH DIAMETER
  !     VOL(3) - VOLUME FROM 3 TO 6 INCH DIAMETER
  !     VOL(4) - VOLUME FROM 0 TO 3 INCH DIAMETER
  !     REMVOL - THE REMAINING VOLUME.  THIS VALUE IS SET WHEN A TREE IS SMALLER
  !     THAN 12 INCH DIAMETER.  BASICALLY, IT POINTS TO THE LAST TREE
  !     CATEGORY THAT IT FITS INTO.  FOR INSTANCE, IF THE DBH IS 11.5
  !     INCHES, THEN THE REMVOL WILL BE 2.

  ! Variables
  real    :: VOL(4)
  logical :: DEBUG
  character(len=40) :: TITLE
  integer :: IFTYP,IYEAR,I,IFMSVPX,II,ISTLNB,MEDIAN
  real    :: FLMHTIN,TEMPIOBJ,TEMPIS,HT12,HT6,HT3,XYMAX,XJUMP,BACHLO,XSD

  !     WRITE OUT DEBUG FUEL LOADS IF APPLICABLE.

  call DBCHK (DEBUG,'FMSVOUT',7,ICYC)
  IF (DEBUG) WRITE(JOSTND,'('' IN FMSVOUT,NFMSVPX='',I2,            &
       & '' IYEAR,FLAMEHT,IFTYP='',I5,F10.2,I5)')&
       &NFMSVPX,IYEAR,FLAMEHT,IFTYP
  if (DEBUG) write(JOSTND,5) 'Pre',TCWD,TCWD2
  5 format (1X,A,'-Snag-Removal Fuel Loads:  Litter   Duff  ',&
       &'  0-3    3-6    6-12     >12  '/&
       &5X,'Before Fire:',12X,6(F6.2,2X)/&
       &5X,'After Fire:',12X,6(F6.2,2X))

  !     SET THE FIRE TYPE AND FLAME HT IN THE LOCALLY USED COMMON AREA

  IFMTYP = IFTYP
  FLAMEHT = FLMHTIN

  !     IF WE ARE NOT USING SV ROUTINES, OR NO IMAGES ARE DESIRED,
  !     THEN EXIT FMSVOUT AT THIS POINT.

  if (JSVOUT.eq.0.or. NFMSVPX.eq.0) return

  !     SWAP THE IOBJTP AND IOBJTPTMP, AND THE IS2F AND IS2FTMP

  do I = 1,NSVOBJ
     TEMPIOBJ = real(IOBJTPTMP(I))
     IOBJTPTMP(I) = IOBJTP(I)
     IOBJTP(I) = INT(TEMPIOBJ)

     TEMPIS = real(IS2FTMP(I))
     IS2FTMP(I) = IS2F(I)
     IS2F(I) = INT(TEMPIS)

     !        WHILE WE ARE IN THIS LOOP, GO AHEAD AND MODIFY THE CURRENT
     !        WOODY DEBRIS VALUES TO NOT INCLUDE SNAGS THAT WE ARE DRAWING.

     ! temp: make sure this code is skiped:
     if (i.lt.0) then
        !**       IF ( IOBJTP(I).EQ.2 .AND. FALLDIR(IS2F(I)).NE.-1 ) THEN

        !         THIS IS A SIGNIFICANT FALLEN SNAG.  CACULATE THE VOLUMES
        !         FOR EACH OF THE DEBRIS CATEGORIES AND SUBTRACT FROM PRE-FIRE
        !         TCWD VECTOR.

        !         CALCULATE THE TOTAL SNAG VOLUME.

        call FMSVL2(ISNSP(IS2F(I)),ODIA(IS2F(I)),&
             &OLEN(IS2F(I)),OLEN(IS2F(I)),VOL(1), 0,&
             &'D',.false.,.false.,JOSTND)

        !         CALCULATE THE VOLUME UP TO 12 INCH DIAMETER.

        HT12 = OLEN(IS2F(I)) - (( 12.0 * (OLEN(IS2F(I))*12.-36.)/&
             &ODIA(IS2F(I)))/12.)
        call FMSVL2(ISNSP(IS2F(I)),ODIA(IS2F(I)),&
             &OLEN(IS2F(I)), HT12, VOL(2),0,&
             &'D',.false.,.false.,JOSTND)
        HT6 = OLEN(IS2F(I)) - ( (6.0 * (OLEN(IS2F(I))*12.-36.) /&
             &ODIA(IS2F(I)))/12.)
        call FMSVL2(ISNSP(IS2F(I)),ODIA(IS2F(I)),&
             &OLEN(IS2F(I)), HT6, VOL(3), 0,&
             &'D',.false.,.false.,JOSTND)
        HT3 = OLEN(IS2F(I)) - ( (3.0 * (OLEN(IS2F(I))*12.-36.)/&
             &ODIA(IS2F(I)))/12.)
        call FMSVL2(ISNSP(IS2F(I)),ODIA(IS2F(I)),&
             &OLEN(IS2F(I)), HT3, VOL(4), 0,&
             &'D',.false.,.false.,JOSTND)

        if ( HT3.ge.OLEN(IS2F(I)) .or. HT3.lt.0. ) then

           !           THIS SNAG ISN'T EVEN 3 INCHES IN DIAMETER AT ANY POINT.
           !           GO AHEAD AND SET THE VOL(4) TO BE VOL(1) WHICH IS CURRENTLY
           !           THE ENTIRE VOLUME OF THE SNAG.  THEN CLEAR ALL OF THE OTHERS.

           VOL(4) = VOL(1)
           VOL(1) = 0.
           VOL(2) = 0.
           VOL(3) = 0.

        else if ( HT6.ge.OLEN(IS2F(I)) .or. HT6.lt.0. ) then

           !           THIS SNAG IS AT LEAST 3 INCHES AT SOME POINT, BUT NOT MORE
           !           THAN 6 INCHES AT ANY POINT.
           !           SET VOL(3) TO THE ENTIRE VOLUME OF THE SNAG (VOL(1)) AND THEN
           !           REDUCE THAT VALUE BY VOL(4).  VOL(4) SHOULD BE ACCURATE.

           VOL(4) = VOL(1) - VOL(4)
           VOL(3) = VOL(1) - VOL(4)
           VOL(1) = 0.
           VOL(2) = 0.

           !           THIS LOOKS LIKE WE ARE SETTING VOL(3) AND VOL(4) TO THE
           !           SAME VALUES, BUT VOL(4) IS ACTUALLY BEING CHANGED IN THE
           !           FIRST CALCULATION THERE.

        else if ( HT12.ge.OLEN(IS2F(I)) .or. HT12.lt.0. ) then

           !           THIS SNAG IS AT LEAST 6 INCHES AT SOME POINT, BUT NOT MORE
           !           THAN 12 INCHES AT ANY POINT.  REMOVE VOL(4) FROM VOL(3),
           !           VOL(4) AND VOL(3) FROM VOL(2) AND ALL OF THEM FROM VOL(1)
           !           THEREFORE, REMOVE VOL(4) FROM VOL(3) AND THEN THEY ARE BOTH
           !           ACCURATE.

           VOL(4) = VOL(1) - VOL(4)
           VOL(3) = VOL(1) - VOL(3) - VOL(4)
           VOL(2) = VOL(1) - VOL(3) - VOL(4)
           VOL(1) = 0.

        else

           !           THIS SNAG IS BIGGER THAN TWELVE INCHES AT SOME POINT.
           !           THIS MEANS THAT VOL(1) REPRESENTS THE VOLUME OF STUFF
           !           THAT IS GREATER THAN 12 INCHES.

           VOL(4) = VOL(1) - VOL(4)
           VOL(3) = VOL(1) - VOL(3) - VOL(4)
           VOL(2) = VOL(1) - VOL(2) - VOL(3) - VOL(4)
           VOL(1) = VOL(1) - VOL(2) - VOL(3) - VOL(4)
        end if

        !         NOW THAT WE HAVE THE DIFFERENT VOLUMES, CALCULATE THEIR WEIGHT
        !         TO REMOVE FROM THE TCWD BY CALLING V2T.

        VOL(1) = VOL(1) * V2T(ISNSP(IS2F(I)))
        VOL(2) = VOL(2) * V2T(ISNSP(IS2F(I)))
        VOL(3) = VOL(3) * V2T(ISNSP(IS2F(I)))
        VOL(4) = VOL(4) * V2T(ISNSP(IS2F(I)))
        TCWD(6) = TCWD(6) - VOL(1)
        TCWD(5) = TCWD(5) - VOL(2)
        TCWD(4) = TCWD(4) - VOL(3)
        TCWD(3) = TCWD(3) - VOL(4)
     end if

     !       DO THE EXACT SAME THING FOR TEMP OBJECT (POST-FIRE)

     ! temp: make sure this code is skiped:
     if (i.lt.0) then
        !**       IF ( IOBJTPTMP(I).EQ.2  .AND. FALLDIR(IS2F(I)).NE.-1 ) THEN

        !         THIS IS A SIGNIFICANT FALLEN SNAG.  CACULATE THE VOLUMES
        !         FOR EACH OF THE DEBRIS CATEGORIES AND SUBTRACT FROM PRE-FIRE
        !         TCWD VECTOR

        !         CALCULATE THE TOTAL SNAG VOLUME.

        call FMSVL2(ISNSP(IS2FTMP(I)),ODIA(IS2FTMP(I)),&
             &OLEN(IS2FTMP(I)),OLEN(IS2FTMP(I)), VOL(1),0,&
             &'D',.false.,.false.,JOSTND)

        !         CALCULATE THE VOLUME UP TO 12 INCH DIAMETER.

        HT12 = OLEN(IS2FTMP(I)) - ((12.0 *&
             &(OLEN(IS2FTMP(I))*12.-36.)/ODIA(IS2FTMP(I)))/12.)
        call FMSVL2(ISNSP(IS2FTMP(I)),ODIA(IS2FTMP(I)),&
             &OLEN(IS2FTMP(I)), HT12, VOL(2),0,&
             &'D',.false.,.false.,JOSTND)
        HT6 = OLEN(IS2FTMP(I)) - ( (6.0 *&
             &(OLEN(IS2FTMP(I))*12.-36.)/ODIA(IS2FTMP(I)))/12.)
        call FMSVL2(ISNSP(IS2FTMP(I)),ODIA(IS2FTMP(I)),&
             &OLEN(IS2FTMP(I)), HT6, VOL(3), 0,&
             &'D',.false.,.false.,JOSTND)
        HT3 = OLEN(IS2FTMP(I)) - ( (3.0 *&
             &(OLEN(IS2FTMP(I))*12.-36.)/ODIA(IS2FTMP(I)))/12.)
        call FMSVL2(ISNSP(IS2FTMP(I)),ODIA(IS2FTMP(I)),&
             &OLEN(IS2FTMP(I)), HT3, VOL(4), 0,&
             &'D',.false.,.false.,JOSTND)

        if ( HT3.ge.OLEN(IS2FTMP(I)) .or. HT3.lt.0. ) then

           !           THIS SNAG ISN'T EVEN 3 INCHES IN DIAMETER AT ANY POINT.
           !           GO AHEAD AND SET THE VOL(4) TO BE VOL(1) WHICH IS CURRENTLY
           !           THE ENTIRE VOLUME OF THE SNAG.  THEN CLEAR ALL OF THE OTHERS.

           VOL(4) = VOL(1)
           VOL(1) = 0.
           VOL(2) = 0.
           VOL(3) = 0.

        else if ( HT6.ge.OLEN(IS2FTMP(I)) .or. HT6.lt.0. ) then

           !           THIS SNAG IS AT LEAST 3 INCHES AT SOME POINT, BUT NOT MORE
           !           THAN 6 INCHES AT ANY POINT.
           !           SET VOL(3) TO THE ENTIRE VOLUME OF THE SNAG (VOL(1)) AND THEN
           !           REDUCE THAT VALUE BY VOL(4).  VOL(4) SHOULD BE ACCURATE.

           VOL(4) = VOL(1) - VOL(4)
           VOL(3) = VOL(1) - VOL(4)
           VOL(1) = 0.
           VOL(2) = 0.

           !           THIS LOOKS LIKE WE ARE SETTING VOL(3) AND VOL(4) TO THE
           !           SAME VALUES, BUT VOL(4) IS ACTUALLY BEING CHANGED IN THE
           !           FIRST CALCULATION THERE.

        else if ( HT12.ge.OLEN(IS2FTMP(I)) .or. HT12.lt.0. ) then

           !           THIS SNAG IS AT LEAST 6 INCHES AT SOME POINT, BUT NOT MORE
           !           THAN 12 INCHES AT ANY POINT.  REMOVE VOL(4) FROM VOL(3),
           !           VOL(4) AND VOL(3) FROM VOL(2) AND ALL OF THEM FROM VOL(1)
           !           THEREFORE, REMOVE VOL(4) FROM VOL(3) AND THEN THEY ARE BOTH
           !           ACCURATE.

           VOL(4) = VOL(1) - VOL(4)
           VOL(3) = VOL(1) - VOL(3) - VOL(4)
           VOL(2) = VOL(1) - VOL(3) - VOL(4)
           VOL(1) = 0.
        else

           !           THIS SNAG IS BIGGER THAN TWELVE INCHES AT SOME POINT.
           !           THIS MEANS THAT VOL(1) REPRESENTS THE VOLUME OF STUFF
           !           THAT IS GREATER THAN 12 INCHES.

           VOL(4) = VOL(1) - VOL(4)
           VOL(3) = VOL(1) - VOL(3) - VOL(4)
           VOL(2) = VOL(1) - VOL(2) - VOL(3) - VOL(4)
           VOL(1) = VOL(1) - VOL(2) - VOL(3) - VOL(4)

        end if

        !         NOW THAT WE HAVE THE DIFFERENT VOLUMES, CALCULATE THEIR WEIGHT
        !         TO REMOVE FROM THE TCWD BY CALLING V2T.

        VOL(1) = VOL(1) * V2T(ISNSP(IS2FTMP(I)))
        VOL(2) = VOL(2) * V2T(ISNSP(IS2FTMP(I)))
        VOL(3) = VOL(3) * V2T(ISNSP(IS2FTMP(I)))
        VOL(4) = VOL(4) * V2T(ISNSP(IS2FTMP(I)))
        TCWD2(6) = TCWD2(6) - VOL(1)
        TCWD2(5) = TCWD2(5) - VOL(2)
        TCWD2(4) = TCWD2(4) - VOL(3)
        TCWD2(3) = TCWD2(3) - VOL(4)
     end if
  end do
  if (DEBUG) write(JOSTND,5) 'Post',TCWD,TCWD2

  !     SET UP THE LOCATION BOUNDARIES

  if (IMETRIC.eq.0) then
     XSD = 7.5 ! SD (FT)
     if (IPLGEM .le.1) then ! SQUARE STAND
        XYMAX = 208.71
     else ! CIRCULAR
        XYMAX = 235.50
     end if
  else
     XSD = 7.5*FTtoM ! SD (M)
     if (IPLGEM .le.1) then ! SQUARE STAND
        XYMAX = 100.0
     else ! CIRCULAR
        XYMAX = 112.84
     end if
  end if

  XJUMP = (XYMAX/(NFMSVPX-1))/NFLPTS
  FMY2(1) = 0.
  OFFSET(1) = BACHLO( 0., XSD, SVRANN)
  do I = 2, NFLPTS
     FMY2(I) = 0.
     OFFSET(I) = OFFSET(I-1) + BACHLO( 0., XSD, SVRANN )
  end do

  !     FMSVPIX IS THE NUMBER OF PICTURES TO DRAW, AND THEREFORE THE NUMBER
  !     OF DIVISIONS IN THE Y-DIRECTION.  THIS MAKES IT LOOK LIKE THE FIRE
  !     BURNS PROGRESSIVELY MORE OF THE STAND IN EACH FRAME.

  do IFMSVPX = 1,NFMSVPX

     do I = 1,NFLPTS
        FMY1(I) = FMY2(I)
        MEDIAN = INT(((real(IFMSVPX - 0.0))/real((NFMSVPX-1)))*XYMAX)
        CATCHUP(I) = BACHLO(0., XJUMP, SVRANN)
        FMY2(I) = MEDIAN + OFFSET(I) + CATCHUP(I)
     end do

     if ( IFMSVPX .eq. NFMSVPX ) then
        do I = 1, NFLPTS
           FMY2(I) = XYMAX
        end do
     end if

     do I = 1,NSVOBJ

        !         CALCULATE WHICH PART OF THE LINE THIS OBJECT IS BEHIND.
        !         II REPRESENTS THIS "CHUNK" OF THE STAND.

        II = IFIX( XSLOC(I) / (XYMAX/NFLPTS) )

        if (II .lt.1) then
           II = 1
        else if (II .gt. NFLPTS) then
           II = NFLPTS
        end if
        if (YSLOC(I).le.FMY2(II) .and. YSLOC(I).gt.FMY1(II)) then

           !           THIS OBJECT IS IN THE BURNED AREA.

           IOBJTP(I) = IOBJTPTMP(I)
           IS2F(I) = IS2FTMP(I)
        end if
     end do

     if ( IFMSVPX .eq.1 ) then
        write (TITLE,10) 'Beginning of fire', IFMSVPX, NFMSVPX
        10 format (A,' (',I2.2,'/',I2.2,')')
        call SVOUT(IYEAR,4,TITLE(:ISTLNB(TITLE)))
     else if ( IFMSVPX.eq.NFMSVPX ) then
        write (TITLE,10) 'After the fire', IFMSVPX, NFMSVPX
        call SVOUT(IYEAR,5,TITLE(:ISTLNB(TITLE)))
     else
        write (TITLE,10) 'During the fire', IFMSVPX, NFMSVPX
        call SVOUT(IYEAR,4,TITLE(:ISTLNB(TITLE)))
     end if
  end do

  return
end
