!============================!
!   canada/fire/bc : fmcba   !
!============================!
! CANADA-FIRE-BC $Id$
subroutine FMCBA (IYR,ISWTCH)
  implicit none

  !     SINGLE-STAND VERSION
  !     CALLED FROM: FMMAIN

  !  PURPOSE:
  !     FIND THE DOMINANT SPECIES (BY BASAL AREA). SET THE INITIAL LIVE
  !     AND DEAD FUEL VALUES AS WELL. THE DEAD FUELS ARE ONLY INITIALIZED
  !     IN THE FIRST YEAR, BUT COVTYP AND THE LIVE FUELS MUST BE DONE
  !     EACH YEAR. CALCULATE PERCOV, USED HERE AND IN FIRE BEHAVIOR CALCS.

  !  LOCAL VARIABLE DEFINITIONS:
  !     BAMOST:  THE HIGHEST BASAL AREA IN A SINGLE SPECIES
  !     CAREA:   THE AREA COVERED BY THE CROWN AT ITS WIDEST POINT (SQFT)
  !     COVINI:  THE SERAL COVER TYPE TO BE USED FOR INITIATING FUELS IN
  !              BARE STANDS
  !     CRL:     CROWN LENGTH
  !     CWIDTH:  THE MAXIMUM WIDTH OF THE CROWNS (FT)
  !     FUINIE:  THE INITIAL FUEL LOADINGS FOR ESTABLISHED STANDS (FROM JBROWN)
  !     FUINII:  THE INITIAL FUEL LOADINGS FOR INITIATING STANDS (FROM JBROWN)
  !     FULIVE:  THE HERB/SHRUB/REGEN FOR ESTABLISHED STANDS (FROM JBROWN)
  !     FULIVI:  THE HERB/SHRUB/REGEN FOR INITIATING STANDS (FROM JBROWN)
  !     ISWTCH:  =1 if called by SVSTART
  !              =0 if called by any other subroutine (FMMAIN, FMPPHV)
  !     TOTBA:   THE TOTAL BASAL AREA IN THE STAND (USED IN THE FUELS CALCS)
  !     TOTCRA:  THE SUM OF THE AREA OF THE CROWNS, PER ACRE (SQFT)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  !  LOCAL VARIABLES DECLARATIONS

  ! Variables
  real    :: TBA(MAXSP)
  real    :: BAMOST,BA1,TOTCRA,CWIDTH
  real    :: FULIVE(2,MAXSP),FULIVI(2,MAXSP)
  real    :: FUINIE(MXFLCL,MAXSP),FUINII(MXFLCL,MAXSP)
  real    :: STFUEL(MXFLCL,2),XCOV(2),YLOAD(2),FOTOVAL(MXFLCL)
  real    :: PRMS(12),FOTOVALS(9)
  logical :: DEBUG
  integer :: NDRY
  integer :: COVINI(30)
  parameter (NDRY = 16)
  integer :: MAPDRY(2,NDRY),IHB
  integer :: MYACT(3)
  integer :: IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
  real    :: BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD

  !     CLASSIFICATION OF HABITATS TO BE DRY GRASSY (IDRYB=1),
  !     DRY SHRUBBY (IDRYB=2), OR OTHER (IDRYB=0)
  !     THE FOLLOWING TYPE CAN BE ADDED ONCE WE KNOW THEIR CODES:
  !     ?,1  !  PIPO/ARUV
  !     ?,2  !  PIPO/AMAL
  !     ?,2  !  PIPO/PYMA
  !     ?,2  !  PSME/SYOC

  !     NOTE: BC VARIANT DOESN'T USE HABITAT TYPES. INSTEAD
  !     IT USES BECS. WHEN WE CALIBRATE THIS MODEL, THIS CODE
  !     WILL NEED TO CHANGE. (SARAH MARCH 04)

  data MAPDRY/&
       &130,1,& ! PIPO/AGSP
       &140,1,& ! PIPO/FEID
       &210,1,& ! PSME/AGSP
       &220,1,& ! PSME/FEID
       &230,1,& ! PSME/FESC
       &161,2,& ! PIPO/PUTR
       &170,2,& ! PIPO/SYAL
       &171,2,& ! PIPO/SYAL-SYAL
       &172,2,& ! PIPO/SYAL-BERE
       &180,2,& ! PIPO/PRVI
       &181,2,& ! PIPO/PRVI-PRVI
       &182,2,& ! PIPO/PRVI-SHCA
       &310,2,& ! PSME/SYAL
       &311,2,& ! PSME/SYAL-AGSP
       &312,2,& ! PSME/SYAL-CARU
       &313,2/ ! PSME/SYAL-SYAL

  !                  herbs, shrubs
  data FULIVE /0.15,  0.1,&
       &0.2,   0.2,&
       &0.2,   0.2,&
       &0.15,  0.1,&
       &0.2,   0.2,&
       &0.2,   0.2,&
       &0.2,   0.1,&
       &0.15,  0.2,&
       &0.15,  0.2,&
       &0.2,   0.25,&
       &0.25,  0.25,& ! birch 21 !QA - ottmar and others 2000b
       &0.25,  0.25,& ! aspen 18
       &0.25,  0.25,& ! cotton 19 !QA - ottmar and others 2000b
       &0.2,   0.2,& ! FD 3
       &0.25,  0.25/ ! birch 21

  data FULIVI /0.30, 2.0,&
       &0.4,  2.0,&
       &0.4,  2.0,&
       &0.30, 2.0,&
       &0.4,  2.0,&
       &0.4,  2.0,&
       &0.4,  1.0,&
       &0.30, 2.0,&
       &0.30, 2.0,&
       &0.25, 0.10,&
       &0.18, 1.32,&
       &0.18, 1.32,&
       &0.18, 1.32,&
       &0.4,  2.0,&
       &0.18, 1.32/

  !                  Size Categories: inches
  !                  A = 0.25,    B = <1,     C = 1-3,     D = 3-6
  !                  E = 6-12,    F = 12-20,  G = 20-35,   H = 35-50
  !                  I = >50,     J = Litter, K = Duff

  !                    A    B    C    D    E    F    G   H   I    J    K

  data FUINIE /1.0, 1.0, 1.6,10.0,10.0,10.0, 0.0,0.0,0.0, 0.8,30.0,&
       &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0,0.0,0.0, 0.6,10.0,&
       &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0,0.0,0.0, 0.6,10.0,&
       &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0, 0.6,25.0,&
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0, 1.0,35.0,&
       &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0, 1.0,35.0,&
       &0.9, 0.9, 1.2, 7.0, 8.0, 0.0, 0.0,0.0,0.0, 0.6,15.0,&
       &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0, 0.6,30.0,&
       &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0, 0.6,30.0,&
       &0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0,0.0,0.0, 1.4, 5.0,&
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0, 1.4,16.8,&
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0, 1.4,16.8,&
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0, 1.4,16.8,&
       &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0,0.0,0.0, 0.6,10.0,&
       &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0, 1.4,16.8/

  !                    A    B    C     D    E    F    G   H   I    J    K

  data FUINII /0.6, 0.6, 0.8, 6.0, 6.0, 6.0, 0.0,0.0,0.0, 0.4,12.0,&
       &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0,0.0,0.0, 0.3, 5.0,&
       &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0,0.0,0.0, 0.3, 5.0,&
       &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0, 0.3,12.0,&
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0, 0.5,12.0,&
       &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0, 0.5,12.0,&
       &0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0,0.0,0.0, 0.3, 7.0,&
       &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0, 0.3,12.0,&
       &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0, 0.3,12.0,&
       &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0,0.0,0.0, 0.5, 0.8,&
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0, 0.8, 5.6,&
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0, 0.8, 5.6,&
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0, 0.8, 5.6,&
       &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0,0.0,0.0, 0.3, 5.0,&
       &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0, 0.8, 5.6/

  data COVINI /10,10, 3, 3, 5, 4, 3, 3, 3, 8,&
       &8, 4, 4, 6, 6, 6, 5, 9, 9, 9,&
       &9, 9, 9, 9, 9, 9, 9, 9, 9, 9/

  data MYACT /2521, 2548, 2553 /

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCBA',5,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC
  7 format(' ENTERING FMCBA CYCLE = ',I2)

  !     BEGIN ROUTINE.

  !     ZERO OUT THE CUMMULATIVE VARIABLES

  COVTYP = 0
  PERCOV = 0.0
  BIGDBH = 0.0
  TOTBA = 0.0

  !     LOOP THROUGH THE TREE LIST

  if (ITRN .gt.0) then

     !       ZERO OUT THE CUMMULATIVE VARIABLES

     BAMOST = 0.0
     TOTCRA = 0.0

     do KSP = 1,MAXSP
        TBA(KSP) = 0.0
     end do

     do I = 1,ITRN
        if (FMPROB(I) .gt.0.0) then

           KSP = ISP(I)

           BA1 = 3.14159 * (DBH(I) / 24.0) * (DBH(I) / 24.0)
           TBA(KSP) = TBA(KSP) + BA1 * FMPROB(I)

           if (DBH(I) .gt. BIGDBH) BIGDBH = DBH(I)

           !           CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
           !           ENCOMPASSED BY ALL TREES

           CWIDTH = CRWDTH(I)
           CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
           TOTCRA = TOTCRA + CAREA*FMPROB(I)
        end if

        !         USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.

        CURKIL(I) = 0.0
     end do

     !        DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
     !        -> THAT WILL BE THE COVER TYPE

     do KSP = 1,MAXSP
        if (TBA(KSP) .gt. BAMOST) then
           BAMOST = TBA(KSP)
           COVTYP = KSP
        end if
        TOTBA = TOTBA + TBA(KSP)
     end do

     !       USE THE CROWN WIDTH INFORMATION TO DETERMINE THE PERCENT COVER
     !       OF THE STAND. USE THE EQUATION SENT BY NICK WHICH ASSUMES THAT
     !       CROWNS ARE RANDOMLY DISTRUBUTED IN THE STAND:

     !       PERCOV = 100*(1-EXP(-TOTAL CROWN AREAS PER ACRE / SQFT IN AN ACRE))

     PERCOV = 1.0 - EXP(-TOTCRA/43560.)
     PERCOV = PERCOV * 100.0

  end if

  !     IF THERE ARE NO TREES (COVTYP=0) THEN USE THE HABITAT TYPE
  !     (INDEX=ITYPE) TO DETERMINE A SURROGATE

  if (COVTYP .eq.0) COVTYP = COVINI(ITYPE)

  !     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
  !     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.

  XCOV(1) = 10.
  XCOV(2) = 60.
  do I = 1,2
     YLOAD(1) = FULIVI(I,COVTYP)
     YLOAD(2) = FULIVE(I,COVTYP)
     FLIVE(I) = ALGSLP(PERCOV,XCOV,YLOAD,2)
  end do

  if (DEBUG) write(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
  8 format(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',&
       &2F6.3)

  !     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION

  if (IYR .eq. IY(1)) then

     !sng      IF (IYR .EQ. IY(1)) THEN

     !        ASSUME THE FUELS ARE UNPILED AND HARD. [**THIS WILL CHANGE LATER]

     !        LOAD DEAD FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
     !        STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.

     XCOV(1) = 10.0
     XCOV(2) = 60.0
     do ISZ = 1,MXFLCL
        YLOAD(1) = FUINII(ISZ,COVTYP)
        YLOAD(2) = FUINIE(ISZ,COVTYP)
        STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
        STFUEL(ISZ,1) = 0
     end do

     !       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

     !       ** CURRENTLY UNSUPPORTED IN THE BC VARIANT **
     !       ** BUT PERHAPS COULD BE.                   **

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

           !           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
           !           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
           !           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

           if (FOTOVAL(1).ge.0.and. ISWTCH.ne.1) call OPDONE(J,IYR)
        else
           WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',    &
                &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ', &
                &      'REQUIRED.',/1X)")
           call RCDSET (2,.true.)
        end if
     end if

     !        CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
     !       FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

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

        !          DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !          NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

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

        !          DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
        !          NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

        if ( ISWTCH .ne.1 ) call OPDONE(J,IYR)

     end if

     !        DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
     !        OF BASAL AREA IN THE STAND.

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

     !       SET IDRYB FOR THIS HABITAT TYPE; TO BE USED IN **FMCFMD**
     !       NOTE IDRYB IS USED IN OTHER VARIANTS TO SIGNAL START/END YEARS
     !       FOR DROUGHT; UNUSED IN THIS VARIANT, SO REUSED TO HOLD THIS
     !       CATEGORY

     IDRYB = 0
     do I = 1,NDRY
        if (MAPDRY(1,I) .eq. FMKOD) then
           IDRYB = MAPDRY(2,I)
           exit
        end if
     end do

  end if

  !     IN FIRST YEAR, SET C-REPORTING REGION FOR FOREST CODE 621
  if (IYR.eq.IY(1) .and. KODFOR.eq.621) ICHABT = 2

  return

  !     HOOK TO ALLOW THE HABITAT CATEGORY TO BE READ BY **FMCFMD*

  entry NIFMHAB(IHB)
  IHB = IDRYB
  return

  entry SNGCOE

  !     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
  !     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

  return
end
