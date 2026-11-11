!=====================!
!   fire/ls : fmcba   !
!=====================!
! FIRE-LS $Id$
subroutine FMCBA (IYR,ISWTCH)
  implicit none

  !  SINGLE-STAND VERSION
  !  CALLED FROM: FMMAIN

  !  PURPOSE:
  !     FIND THE FOREST TYPE. SET THE INITIAL LIVE AND DEAD FUEL VALUES
  !     THE DEAD FUELS ARE ONLY INITIALIZED IN THE FIRST YEAR, BUT THE
  !     LIVE FUELS MUST BE DONE EACH YEAR.  THE ROUTINE ALSO CALCULATES
  !     PERCOV, WHICH IS NEEDED FOR THE FIRE BEHAVIOR CALCS.

  !  LOCAL VARIABLE DEFINITIONS:
  !     BAMOST:  THE HIGHEST BASAL AREA IN A SINGLE SPECIES
  !     FUINI:   THE INITIAL DEAD FUEL LOADINGS
  !     FULIV:   THE HERB/SHRUB LOADINGS
  !     ISWTCH:  =1 if called by SVSTART
  !              =0 if called by any other subroutine (FMMAIN, FMPPHV)
  !     TOTBA:   THE TOTAL BASAL AREA IN THE STAND

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !  LOCAL VARIABLE DECLARATIONS

  ! Variables
  real    :: FULIV(2,9),FUINI(MXFLCL,26)
  real    :: PRMS(12),TOTBA,STFUEL(MXFLCL,2),PRCL,ADD,BAMOST
  real    :: CAREA,TOTCRA,CWIDTH,FOTOVAL(MXFLCL),FOTOVALS(9)
  integer :: IFFEFT,MYACT(3),IYR,KSP,I,J,ISZ,NPRM,IACTK,ISWTCH,IDC,JYR
  integer :: FTLIVEFU,FTDEADFU
  logical :: DEBUG

  !  CURRENTLY THE HERB AND SHRUB VALUES ARE ESTIMATED FROM FIA FOREST
  !  TYPE AND SIZE CLASS.  ONLY THREE FOREST TYPES ARE CONSIDERED:
  !  JACK PINE, RED/WHITE PINE, AND HARDWOODS.
  !  THE HERB AND SHRUB VALUES COME FROM THE FOLLOWING DOCUMENTS:

  !  Ottmar, Roger D.; Vihnanek, Robert E. 1999. Stero photo series for
  !  quantifying natural fuels.  Volume V: midwest red and white pine, northern
  !  tallgrass prairie, and mixed oak types in the Central and Lake States.
  !  PMS 834.  Boise, ID: National Wildfire Coordinating Group, National
  !  Interagency Fire Center.  99 p.

  !  Ottmar, Roger D.; Vihnanek, Robert E.; Wright, Clinton S. 2002. Stero
  !  photo series for quantifying natural fuels.  Volume Va: jack pine in
  !  the Lake States.  PMS 837.  Boise, ID: National Wildfire Coordinating
  !  Group, National Interagency Fire Center.  49 p.

  !                  herbs, shrubs
  data FULIV /0.12, 0.17,& ! white / red pine, sz cl 1 - use MP06
       &0.08, 0.02,& ! white / red pine, sz cl 2 - use MP10
       &0.06, 0.00,& ! white / red pine, sz cl 3 - use MP09
       &0.06, 0.63,& ! jack pine, sz cl 1 - use photo JP16
       &0.10, 0.04,& ! jack pine, sz cl 2 - use JP15
       &0.14, 0.35,& ! jack pine, sz cl 3 - use JP11
       &0.00, 0.00,& ! hardwoods, sz cl 1 - use MO03
       &0.00, 0.00,& ! hardwoods, sz cl 2 - MO11
       &0.00, 0.01/ ! hardwoods, sz cl 3 - use MO09

  !  CURRENTLY, THE INITIAL DEAD SURFACE FUEL LOADINGS ARE ESTIMATED
  !  FROM FIA FOREST TYPE AND SIZE CLASS.

  !                  <.25 to1  1-3   3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
  data FUINI /0.09,0.51,1.76,0.78,1.89,1.95,0.0,0.0,0.0,1.83,3.87,& ! white / red pine (100), sz cl 1
       &0.07,0.51,0.93,0.06,0.26,0.00,0.0,0.0,0.0,0.54,1.43,& ! white / red pine (100), sz cl 2
       &0.07,0.25,0.18,0.00,0.00,0.00,0.0,0.0,0.0,2.05,9.96,& ! white / red pine (100), sz cl 3
       &0.31,2.22,4.99,0.64,2.11,1.46,0.0,0.0,0.0,1.44,9.05,& ! jack pine (101), sz cl 1
       &0.12,0.44,1.05,0.66,1.26,0.00,0.0,0.0,0.0,0.81,2.97,& ! jack pine (101), sz cl 2
       &0.11,0.89,4.58,4.98,4.76,0.00,0.0,0.0,0.0,0.20,2.09,& ! jack pine (101), sz cl 3
       &0.40,0.95,1.79,1.28,4.47,1.46,0.0,0.0,0.0,0.90,64.29,& ! spruce - fir (120), sz cl 1
       &0.19,0.60,1.22,0.52,1.88,0.57,0.0,0.0,0.0,2.36,80.12,& ! spruce - fir (120), sz cl 2
       &0.07,0.49,2.04,0.98,2.61,0.35,0.0,0.0,0.0,1.20,36.70,& ! spruce - fir (120), sz cl 3
       &0.00,0.00,0.00,0.79,0.00,0.00,0.0,0.0,0.0,0.07,8.28,& ! eastern redcedar (180)
       &0.12,0.57,1.14,0.62,3.71,0.37,0.0,0.0,0.0,0.81,2.14,& ! oak - pine (400), sz cl 1
       &0.11,0.95,1.59,0.17,1.89,0.00,0.0,0.0,0.0,0.29,1.28,& ! oak - pine (400), sz cl 2
       &0.12,0.49,0.60,0.67,0.77,0.00,0.0,0.0,0.0,0.20,4.77,& ! oak - pine (400), sz cl 3
       &0.18,0.53,1.60,0.78,1.65,0.52,0.0,0.0,0.0,0.91,4.45,& ! oak - hickory (500), sz cl 1
       &0.24,0.59,1.41,0.74,1.61,1.30,0.0,0.0,0.0,0.44,3.02,& ! oak - hickory (500), sz cl 2
       &0.09,0.80,0.16,0.48,0.51,0.00,0.0,0.0,0.0,0.49,3.96,& ! oak - hickory (500), sz cl 3
       &0.17,0.87,2.10,0.97,2.17,4.52,0.0,0.0,0.0,1.61,40.33,& ! elm - ash - cottonwood (700), sz cl 1
       &0.22,0.48,1.78,1.01,3.09,0.89,0.0,0.0,0.0,1.00,156.25,& ! elm - ash - cottonwood (700), sz cl 2
       &0.13,0.26,0.74,0.49,1.88,0.12,0.0,0.0,0.0,1.17,21.42,& ! elm - ash - cottonwood (700), sz cl 3
       &0.22,0.62,1.74,0.99,1.77,1.41,0.0,0.0,0.0,1.38,7.82,& ! maple - beech - birch (800), sz cl 1
       &0.29,0.74,2.11,0.96,1.92,3.33,0.0,0.0,0.0,0.90,7.65,& ! maple - beech - birch (800), sz cl 2
       &0.23,0.69,2.96,1.15,2.81,0.28,0.0,0.0,0.0,0.81,2.37,& ! maple - beech - birch (800), sz cl 3
       &0.13,0.86,2.10,0.82,2.76,1.10,0.0,0.0,0.0,0.71,14.75,& ! aspen - birch (900), sz cl 1
       &0.12,0.70,2.51,1.04,2.41,2.70,0.0,0.0,0.0,0.53,6.58,& ! aspen - birch (900), sz cl 2
       &0.13,0.52,1.69,0.77,1.80,0.66,0.0,0.0,0.0,1.04,9.55,& ! aspen - birch (900), sz cl 3
       &0.07,0.32,0.34,0.19,0.63,0.00,0.0,0.0,0.0,0.00,0.36/ ! nonstocked (999)

  data MYACT / 2521, 2548, 2553 /

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCBA',5,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMCBA CYCLE = ',I2)

  !  ZERO OUT THE CUMMULATIVE VARIABLES

  COVTYP = 0
  PERCOV = 0
  TOTBA = 0.0

  !  DETERMINE FFE FOREST TYPE (1 OF 10 CATEGORIES) FROM FIA FOR. TYPE

  call FMLSFT(IFFEFT)

  !  LOAD LIVE FUELS AS A FUNCTION OF FOREST TYPE

  select case (IFFEFT)
  case(1,3,4) ! white and red pine, spruce-fir and redcedar
     if (ISZCL .eq.1) then
        FTLIVEFU = 1
     else if (ISZCL .eq.2) then
        FTLIVEFU = 2
     else
        FTLIVEFU = 3
     end if

  case(2) ! jack pine
     if (ISZCL .eq.1) then
        FTLIVEFU = 4
     else if (ISZCL .eq.2) then
        FTLIVEFU = 5
     else
        FTLIVEFU = 6
     end if

  case default
     if (ISZCL .eq.1) then
        FTLIVEFU = 7
     else if (ISZCL .eq.2) then
        FTLIVEFU = 8
     else
        FTLIVEFU = 9
     end if
  end select

  do I = 1,2
     FLIVE(I) = FULIV(I, FTLIVEFU)
  end do

  if (ITRN.gt.0) then

     !  ZERO OUT THE CUMMULATIVE VARIABLES

     BAMOST = 0.0
     TOTCRA = 0.0
     do KSP = 1,MAXSP
        FMTBA(KSP) = 0.0
     end do

     !  LOOP THROUGH THE TREE LIST

     do I = 1,ITRN
        if (FMPROB(I) .gt.0.0) then
           KSP = ISP(I)
           FMTBA(KSP) = FMTBA(KSP) +&
                &FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

           !  CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
           !  ENCOMPASSED BY ALL TREES

           CWIDTH = CRWDTH(I)

           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           TOTCRA = TOTCRA + CAREA*FMPROB(I)
        end if

        !  USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.

        CURKIL(I) = 0.0
     end do

     !  DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
     !  -> THAT WILL BE THE COVER TYPE

     do KSP = 1,MAXSP
        if (FMTBA(KSP) .gt. BAMOST) then
           BAMOST = FMTBA(KSP)
           COVTYP = KSP
        end if
        TOTBA = TOTBA + FMTBA(KSP)
     end do

     !  USE THE CROWN WIDTH INFORMATION TO DETERMINE THE PERCENT COVER
     !  OF THE STAND. USE THE EQUATION SENT BY NICK WHICH ASSUMES THAT
     !  CROWNS ARE RANDOMLY DISTRUBUTED IN THE STAND:

     !  PERCOV = 100*(1-EXP(-TOTAL CROWN AREAS PER ACRE / SQFT IN AN ACRE))

     PERCOV = 1.0 - EXP(-TOTCRA/43560.)
     PERCOV = PERCOV * 100.0
  end if
  if (DEBUG) write(JOSTND,*) 'PERCOV = ',PERCOV

  !   IF THERE ARE NO TREES (COVTYP=0) IN CYCLE 1,
  !   SET COVTYP TO A DEFAULT RED PINE COVER. AFTER THE
  !   FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
  !   PRESENT.

  if (COVTYP .eq.0) then
     if (IYR .eq. IY(1)) then
        WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',  &
             &       'BASAL AREA',                                              &
             &       /1X,'*** COVER TYPE SET TO RED PINE',/1X)")
        call RCDSET (2,.true.)
        COVTYP = 3
     else
        COVTYP = OLDCOVTYP
     end if
  end if

  OLDCOVTYP = COVTYP

  !  INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION

  if (IYR .eq. IY(1)) then
     !sng      IF (IYR .EQ. IY(1)) THEN
     !ppe      IF (IYR .EQ. MIY(1)) THEN

     !  LOAD DEAD FUELS AS A FUNCTION OF FOREST TYPE.

     select case (IFFEFT)
     case(1) ! white / red pine
        if (ISZCL .eq.1) then
           FTDEADFU = 1
        else if (ISZCL .eq.2) then
           FTDEADFU = 2
        else
           FTDEADFU = 3
        end if
     case(2) ! jack pine
        if (ISZCL .eq.1) then
           FTDEADFU = 4
        else if (ISZCL .eq.2) then
           FTDEADFU = 5
        else
           FTDEADFU = 6
        end if
     case(3) ! spruce - fir
        if (ISZCL .eq.1) then
           FTDEADFU = 7
        else if (ISZCL .eq.2) then
           FTDEADFU = 8
        else
           FTDEADFU = 9
        end if
     case(4) ! eastern redcedar
        FTDEADFU = 10
     case(5) ! oak - pine
        if (ISZCL .eq.1) then
           FTDEADFU = 11
        else if (ISZCL .eq.2) then
           FTDEADFU = 12
        else
           FTDEADFU = 13
        end if
     case(6) ! oak - hickory
        if (ISZCL .eq.1) then
           FTDEADFU = 14
        else if (ISZCL .eq.2) then
           FTDEADFU = 15
        else
           FTDEADFU = 16
        end if
     case(7) ! elm - ash - cottonwood
        if (ISZCL .eq.1) then
           FTDEADFU = 17
        else if (ISZCL .eq.2) then
           FTDEADFU = 18
        else
           FTDEADFU = 19
        end if
     case(8) ! maple - beech - birch
        if (ISZCL .eq.1) then
           FTDEADFU = 20
        else if (ISZCL .eq.2) then
           FTDEADFU = 21
        else
           FTDEADFU = 22
        end if
     case(9) ! aspen - birch
        if (ISZCL .eq.1) then
           FTDEADFU = 23
        else if (ISZCL .eq.2) then
           FTDEADFU = 24
        else
           FTDEADFU = 25
        end if
     case(10) ! nonstocked
        FTDEADFU = 26
     end select

     do ISZ = 1,MXFLCL
        STFUEL(ISZ,2) = FUINI(ISZ,FTDEADFU)
        STFUEL(ISZ,1) = 0
     end do

     !       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

     call OPFIND(1,MYACT(2),J)
     if (J .gt.0) then
        call OPGET(J,2,JYR,IACTK,NPRM,PRMS)
        if ((PRMS(1) .ge.0) .and. (PRMS(2) .ge.0)) then
           call FMPHOTOVAL(NINT(PRMS(1)), NINT(PRMS(2)), FOTOVAL,&
                &FOTOVALS)
           do I = 1, MXFLCL
              if (FOTOVAL(I) .ge.0) STFUEL(I,2) = FOTOVAL(I)
              if (I .le.9) STFUEL(I,1) = FOTOVALS(I)
           end do

           !  IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
           !  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
           !  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

           if (FOTOVAL(1).ge.0.and. ISWTCH.ne.1) call OPDONE(J,IYR)
        else
           WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',    &
                &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ', &
                &      'REQUIRED.',/1X)")
           call RCDSET (2,.true.)
        end if
     end if

     !  CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
     !  FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

     call OPFIND(1,MYACT(1),J)
     if (J .gt.0) then
        call OPGET(J,12,JYR,IACTK,NPRM,PRMS)
        if (PRMS(2) .ge.0) STFUEL(3,2) = PRMS(2)
        if (PRMS(3) .ge.0) STFUEL(4,2) = PRMS(3)
        if (PRMS(4) .ge.0) STFUEL(5,2) = PRMS(4)
        if (PRMS(5) .ge.0) STFUEL(6,2) = PRMS(5)
        if (PRMS(6) .ge.0) STFUEL(10,2) = PRMS(6)
        if (PRMS(7) .ge.0) STFUEL(11,2) = PRMS(7)
        if (PRMS(8) .ge.0) STFUEL(1,2) = PRMS(8)
        if (PRMS(9) .ge.0) STFUEL(2,2) = PRMS(9)
        if (PRMS(1) .ge.0) then
           if ((PRMS(8) .lt.0) .and. (PRMS(9) .lt.0)) then
              STFUEL(1,2) = PRMS(1) * 0.5
              STFUEL(2,2) = PRMS(1) * 0.5
           end if
           if ((PRMS(8) .lt.0) .and. (PRMS(9) .ge.0)) then
              STFUEL(1,2) = MAX(PRMS(1) - PRMS(9),0.)
           end if
           if ((PRMS(8) .ge.0) .and. (PRMS(9) .lt.0)) then
              STFUEL(2,2) = MAX(PRMS(1) - PRMS(8),0.)
           end if
        end if
        if (PRMS(10) .ge.0) STFUEL(7,2) = PRMS(10)
        if (PRMS(11) .ge.0) STFUEL(8,2) = PRMS(11)
        if (PRMS(12) .ge.0) STFUEL(9,2) = PRMS(12)

        !  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

        if ( ISWTCH .ne.1 ) call OPDONE(J,IYR)
     end if

     call OPFIND(1,MYACT(3),J)
     if (J .gt.0) then
        call OPGET(J,9,JYR,IACTK,NPRM,PRMS)
        if (PRMS(1) .ge.0) STFUEL(1,1) = PRMS(1)
        if (PRMS(2) .ge.0) STFUEL(2,1) = PRMS(2)
        if (PRMS(3) .ge.0) STFUEL(3,1) = PRMS(3)
        if (PRMS(4) .ge.0) STFUEL(4,1) = PRMS(4)
        if (PRMS(5) .ge.0) STFUEL(5,1) = PRMS(5)
        if (PRMS(6) .ge.0) STFUEL(6,1) = PRMS(6)
        if (PRMS(7) .ge.0) STFUEL(7,1) = PRMS(7)
        if (PRMS(8) .ge.0) STFUEL(8,1) = PRMS(8)
        if (PRMS(9) .ge.0) STFUEL(9,1) = PRMS(9)

        !         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

        if ( ISWTCH .ne.1 ) call OPDONE(J,IYR)

     end if

     !  DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
     !  OF BASAL AREA IN THE STAND.

     do ISZ = 1,MXFLCL
        if (TOTBA .gt.0.0) then
           do KSP = 1,MAXSP
              if (FMTBA(KSP) .gt.0.0) then
                 do J = 1,2
                    PRCL = FMTBA(KSP) / TOTBA
                    IDC = DKRCLS(KSP)
                    ADD = PRCL * STFUEL(ISZ,J)
                    CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + ADD
                 end do
              end if
           end do
        else
           IDC = DKRCLS(COVTYP)
           do J = 1,2
              CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + STFUEL(ISZ,J)
           end do
        end if
     end do

  end if

  entry SNGCOE

  !  ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
  !  IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

  return
end
