!==========================!
!   fire/base : fmsvtree   !
!==========================!
! FIRE-BASE $Id$
subroutine FMSVTREE (NOUT,ISVOBJ)
  implicit none

  !     FIRE MODEL STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- JANUARY 2000
  !     A.H.DALLMANN  -- RMRS MOSCOW -- JANUARY 2000
  !     D.GAMMEL      -- SEM MOSCOW  -- JUNE 2002
  !     D. ROBINSON   -- ESSA        -- MAY 2005

  !     INPUT:
  !     NOUT  =THE OUTPUT FILE REFERENCE NUMBER
  !     ISVOBJ=THE OBJECT BEING PROCESSED

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'SVDATA.f90'
  include 'SVDEAD.f90'
  include 'FMSVCM.f90'
  include 'METRIC.f90'

  external SVRANN
  ! Variables
  integer :: ISVOBJ,NOUT,II,NFLMS,IFLR0T,IMAXFLAMES
  real    :: FLMX,FLMY,X,CALC,BASECRWN,HALFCRWN,Z,FLMZ,TILTBASE,FLMTILT,BACHLO,FLMHT,FMWDTH,FLMWDTH,&
       &MIDCRWN,FLMCALC,FLMCRAWL

  II = IFIX( XSLOC(ISVOBJ) / FLPART ) + 1

  if (YSLOC(ISVOBJ).lt.FMY2(II)) then

     !       PRELIMINARY CALCS

     BASECRWN = ((100. - CRNRTO(IS2F(ISVOBJ))) / 100.)&
          &* OLEN(IS2F(ISVOBJ))
     HALFCRWN = ((CRNRTO(IS2F(ISVOBJ)) / 100.) *&
          &OLEN(IS2F(ISVOBJ))) / 2.

     MIDCRWN = BASECRWN + HALFCRWN
     FLMCRAWL = 0.0
     CALC = 0.0

     if (IFMTYP .eq.1) then

        !         THIS IS A CROWNING FIRE
        !         DO SOME PRELIMINARY CROWN FIRE CALCS

        if (YSLOC(ISVOBJ).gt.(FMY2(II)-150)) then
           if (YSLOC(ISVOBJ).gt.(FMY2(II)-15.)) then
              CALC = 0.75
           else if (YSLOC(ISVOBJ).gt.(FMY2(II)-30.)) then
              CALC = 0.8
           else if (YSLOC(ISVOBJ).gt.(FMY2(II)-45.)) then
              CALC = 0.9
           else if (YSLOC(ISVOBJ).gt.(FMY2(II)-60.)) then
              CALC = 1.0
           else if (YSLOC(ISVOBJ).gt.(FMY2(II)-75.)) then
              CALC = 0.9
           else if (YSLOC(ISVOBJ).gt.(FMY2(II)-90.)) then
              CALC = 0.8
           else if (YSLOC(ISVOBJ).gt.(FMY2(II)-105.)) then
              CALC = 0.75
           else if (YSLOC(ISVOBJ).gt.(FMY2(II)-120.)) then
              CALC = 0.1
           else
              CALC = 0.
           end if

           FLMCRAWL = 3. * (HALFCRWN*2/OLEN(IS2F(ISVOBJ))) + 1.5
        else
           CALC = 0.
        end if
     else if (IFMTYP .eq.2) then

        !         TORCHING FIRE

        !         PRELIMINARY CALCULATIONS

        if (YSLOC(ISVOBJ).gt.(FMY2(II)-15.)) then
           CALC = 0.25
        else if (YSLOC(ISVOBJ).gt.(FMY2(II)-30.)) then
           CALC = 0.50
        else if (YSLOC(ISVOBJ).gt.(FMY2(II)-45.)) then
           CALC = 0.75
        else if (YSLOC(ISVOBJ).gt.(FMY2(II)-60.)) then
           CALC = 1.0
        else if (YSLOC(ISVOBJ).gt.(FMY2(II)-75.)) then
           CALC = 0.75
        else if (YSLOC(ISVOBJ).gt.(FMY2(II)-90.)) then
           CALC = 0.50
        else if (YSLOC(ISVOBJ).gt.(FMY2(II)-105.)) then
           CALC = 0.25
        else if (YSLOC(ISVOBJ).gt.(FMY2(II)-120.)) then
           CALC = 0.1
        else
           CALC = 0.
        end if

        FLMCRAWL = 4. * (HALFCRWN*2/OLEN(IS2F(ISVOBJ))) + 2.

        !         FOR 80% OF THE TREES, DO NOT ALLOW THE FLAMES INTO
        !         THE CROWN OF THE TREE IF THE GROUND FIRE CAN NOT
        !         REACH IT.

        if ((BASECRWN).gt.(FLAMEHT * 1.2)) then
           call SVRANN(X)
           if (X.lt.0.8) then
              CALC = 0.
           end if
        end if
     end if

     !       DETERMINE X,Y,Z FOR FLAME

     call SVRANN(Z)
     FLMZ = (Z * HALFCRWN) + BASECRWN
     FLMX = XSLOC(ISVOBJ)
     FLMY = YSLOC(ISVOBJ)

     !       CREATE THE FLAMES

     IMAXFLAMES = 0
     NFLMS = 0
     do while(IMAXFLAMES .eq.0)
        NFLMS = NFLMS + 1

        FLMZ = FLMZ + FLMCRAWL
        TILTBASE = MAX(MIN(FWIND*.5,40.),5.)
        FLMTILT = MAX(BACHLO(TILTBASE, 5., SVRANN),0.)
        FLMCALC = (OLEN(IS2F(ISVOBJ)) - FLMZ) * 0.65

        if (IMETRIC.eq.0) then
           FLMHT = BACHLO( FLMCALC, 0.5, SVRANN )
        else
           FLMHT = BACHLO( FLMCALC, 0.5*FTtoM, SVRANN )
        end if

        call SVRANN( FMWDTH )
        FLMWDTH = CRNDIA(IS2F(ISVOBJ)) * (1 -&
             &((FLMZ - BASECRWN)/(HALFCRWN*2))) * CALC
        call SVRANN(X)
        IFLR0T = INT(X*90. + 270.)

        if (FLMHT.gt.0..and. FLMWDTH.gt.0) then

           if (IMETRIC.eq.0) then
              write (NOUT,30) NFLMS+NSVOBJ,&
                   &FLMHT,FLMTILT,IFLR0T,FLMWDTH,FLMX,FLMY,FLMZ
           else
              write (NOUT,30) NFLMS+NSVOBJ,&
                   &FLMHT*FTtoM,FLMTILT,IFLR0T,FLMWDTH*FTtoM,&
                   &FLMX,FLMY,FLMZ*FTtoM
           end if

           30 format ('@flame.eob',T16,I5,' 99 0 1 0',2F6.0,I5,&
                &' 0',F6.1,' 0 0 0 0 0 0 0 1 0',3F8.2)
        end if
        if ((FLMHT + FLMZ).ge.(OLEN(IS2F(ISVOBJ))*CALC)) then
           IMAXFLAMES = 1
        end if
     end do
  end if
end
