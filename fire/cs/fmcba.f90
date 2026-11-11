!=====================!
!   fire/cs : fmcba   !
!=====================!
! FIRE-CS $Id$
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
  !     FUINI:   THE INITIAL FUEL LOADINGS
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
  real    :: TBA(MAXSP),FULIV(2,4),FUINI(MXFLCL,7)
  real    :: PRMS(12),TOTBA,STFUEL(MXFLCL,2),PRCL,ADD,BAMOST,BA1
  real    :: CAREA,BIGDBH,TOTCRA,CWIDTH,FOTOVAL(MXFLCL),FOTOVALS(9)
  integer :: IFFEFT,MYACT(3),IYR,KSP,I,J,ISZ,NPRM,IACTK,IDC,ISWTCH,JYR
  integer :: FTLIVEFU,FTDEADFU
  logical :: DEBUG

  !  CURRENTLY THE HERB AND SHRUB VALUES ARE ONLY ESTIMATED FROM FOREST
  !  TYPE, DUE TO A LACK OF DATA.  ONLY THREE FOREST TYPES ARE CONSIDERED:
  !  PINE, HARDWOOD, AND REDCEDAR. (Taken from SN-FFE)

  !                  herbs, shrubs
  data FULIV /0.1, 0.25,& ! pines
       &0.01, 0.03,& ! hardwoods
       &1.0,  5.0,& ! redcedar
       &0.02, 0.13/ ! oak savannah

  !  CURRENTLY, THE INITIAL DEAD SURFACE FUEL LOADINGS ARE ESTIMATED
  !  FROM FIA FOREST TYPE.

  !                <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
  data FUINI /0.18,0.93,1.77,0.27,0.75,8.38,0.0,0.0,0.0,4.10,3.82,& !pines 100s
       &0.19,0.86,1.58,0.11,0.31,0.67,0.0,0.0,0.0,4.89,4.40,& !redcedar 181/402
       &0.18,0.75,2.42,0.59,0.67,1.34,0.0,0.0,0.0,5.37,3.07,& !pine-hardwood 400s
       &0.15,0.74,1.70,0.38,0.97,2.68,0.0,0.0,0.0,5.17,4.52,& !oak-hickory 500s
       &0.20,0.92,2.19,0.41,1.46,3.80,0.0,0.0,0.0,2.49,2.80,& !elm-ash-cottonwood 700s
       &0.19,0.88,1.95,0.56,1.62,1.82,0.0,0.0,0.0,3.88,3.41,& !maple-beech-birch 800s
       &0.02,0.21,0.40,0.02,0.33,0.42,0.0,0.0,0.0,3.12,2.05/ !nonstocked 999

  data MYACT / 2521, 2548, 2553 /

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCBA',5,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMCBA CYCLE = ',I2)

  !  ZERO OUT THE CUMMULATIVE VARIABLES

  COVTYP = 0
  PERCOV = 0
  BIGDBH = 0
  TOTBA = 0.0

  !  DETERMINE FFE FOREST TYPE (1 OF 9 CATEGORIES) FROM FIA FOR. TYPE

  call FMCSFT(IFFEFT)

  !  LOAD LIVE FUELS AS A FUNCTION OF FOREST TYPE

  select case (IFFEFT)
  case(3,4,5)
     FTLIVEFU = 1 !pines
  case(7)
     FTLIVEFU = 3 !redcedar
  case(6)
     FTLIVEFU = 4 !oak savannah
  case default
     FTLIVEFU = 2 !hardwoods
  end select

  do I = 1,2
     FLIVE(I) = FULIV(I, FTLIVEFU)
  end do

  if (ITRN.gt.0) then

     !  ZERO OUT THE CUMMULATIVE VARIABLES

     BAMOST = 0.0
     TOTCRA = 0.0
     do KSP = 1,MAXSP
        TBA(KSP) = 0.0
     end do

     !  LOOP THROUGH THE TREE LIST

     do I = 1,ITRN
        if (FMPROB(I) .gt.0.0) then
           KSP = ISP(I)
           BA1 = 3.14159 * (DBH(I) / 24.0) * (DBH(I) / 24.0)
           TBA(KSP) = TBA(KSP) + BA1 * FMPROB(I)

           if (DBH(I) .gt. BIGDBH) BIGDBH = DBH(I)

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
        if (TBA(KSP) .gt. BAMOST) then
           BAMOST = TBA(KSP)
           COVTYP = KSP
        end if
        TOTBA = TOTBA + TBA(KSP)
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
  !   SET COVTYP TO A DEFAULT RED OAK COVER. AFTER THE
  !   FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
  !   PRESENT.

  if (COVTYP .eq.0) then
     if (IYR .eq. IY(1)) then
        WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',  &
             &       'BASAL AREA',                                              &
             &       /1X,'*** COVER TYPE SET TO RED OAK',/1X)")
        call RCDSET (2,.true.)
        COVTYP = 48
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

     select case (IFORTP)
     case(101:168)
        FTDEADFU = 1 !pines
     case(181,402)
        FTDEADFU = 2 !redcedar
     case(401,403:409)
        FTDEADFU = 3 !pine-hardwood
     case(501:520)
        FTDEADFU = 4 !oak-hickory
     case(701:709)
        FTDEADFU = 5 !elm-ash-cottonwood
     case(801:809)
        FTDEADFU = 6 !maple-beech-birch
     case(999)
        FTDEADFU = 7 !nonstocked
     case default
        FTDEADFU = 4 !oak-hickory
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
              if (TBA(KSP) .gt.0.0) then
                 do J = 1,2
                    PRCL = TBA(KSP) / TOTBA
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
