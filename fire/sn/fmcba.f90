!=====================!
!   fire/sn : fmcba   !
!=====================!
! FIRE-SN $Id$
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
  !     FUINI:   THE INITIAL FUEL LOADINGS BY FOREST TYPE GROUP
  !     FULIV:   THE HERB/SHRUB LOADINGS
  !     ISWTCH:  =1 if called by SVSTART
  !              =0 if called by any other subroutine (FMMAIN, FMPPHV, FMBURN)
  !     TOTBA:   THE TOTAL BASAL AREA IN THE STAND

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'

  !  LOCAL VARIABLE DECLARATIONS

  ! Variables
  real    :: TBA(MAXSP),FULIV(2,4),FULIV2(8,6),FULIV3(8)
  real    :: FUINI(MXFLCL,9),PRMS(12),TOTBA,STFUEL(MXFLCL,2),PRCL,ADD
  real    :: BAMOST,BA1,Y(8),SHRUBAGE,FOTOVALS(9)
  real    :: CAREA,BIGDBH,TOTCRA,CWIDTH,FOTOVAL(MXFLCL),ALGSLP
  integer :: IFFEFT,MYACT(3),IYR,KSP,I,J,ISZ,NPRM,IACTK,ISWTCH,IDC,JYR
  integer :: FTLIVEFU,FTDEADFU,L
  integer :: IFK
  logical :: DEBUG

  !  FOR SOME ECOLOGICAL UNITS, THE HERB AND SHRUB VALUES ARE ONLY ESTIMATED FROM FOREST
  !  TYPE, DUE TO A LACK OF DATA.

  !                  herbs, shrubs
  data FULIV /0.1, 0.25,& ! pines
       &0.01, 0.03,& ! hardwoods
       &1.0,  5.0,& ! redcedar
       &0.02, 0.13/ ! oak savannah

  !  FOR COASTAL PLAIN, PIEDMONT, AND MOUNTAIN SITES, THE HERB AND SHRUB VALUES
  !  ARE SET BASED ON UNDERSTORY HEIGHT AND THE AGE OF THE ROUGH, USING VALUES FROM
  !  "SOUTHERN FORESTRY SMOKE MANAGEMENT GUIDEBOOK" - GTR-SE-10, PG118, ALSO REPLICATED
  !  IN "WILDLAND FIRE IN ECOSYSTEMS: EFFECTS OF FIRE ON FLORA" - RMRS-GTR-42, VOL2, PG63
  !  SINCE UNDERSTORY HEIGHT IS NOT KNOWN, SITE INDEX IS USED AS A SURROGATE.
  !  COASTAL PLAIN SITE (PALMETTO-GALBERRY) WILL GET THE VALUES BELOW, WHILE PIEDMONT AND
  !  MOUNTAIN PYRIC SITES (MOUNTAIN-LAUREL AND RHODODENDRON) WILL GET 40% OF THE VALUES BELOW.
  !  ALL OF THE BIOMASS WILL BE IN THE SHRUB CATEGORY AND NONE WILL GO INTO HERBS.

  data Y / 1, 2, 3, 5, 7, 10, 15, 20/
  !                  age of rough (1,2,3,5,7,10,15,20 years)
  data FULIV2 /0.4, 0.4, 0.5, 0.6, 0.9, 1.4, 2.6, 4.2,& ! understory ht of 1 ft  / si < 50
       &1.2, 1.3, 1.3, 1.5, 1.7, 2.2, 3.4, 5.1,& ! understory ht of 2 ft  / si 50 - 65
       &2.6, 2.6, 2.7, 2.8, 3.1, 3.5, 4.7, 6.4,& ! understory ht of 3 ft  / si 65 - 80
       &4.5, 4.5, 4.6, 4.7, 5.0, 5.5, 6.6, 8.3,& ! understory ht of 4 ft  / si 80 - 95
       &7.0, 7.0, 7.0, 7.2, 7.4, 7.9, 9.1,10.8,& ! understory ht of 5 ft  / si 95 - 110
       &10.0,10.0,10.0,10.2,10.4,10.9,12.1,13.8/ ! understory ht of 6 ft  / si >= 110

  !  CURRENTLY, THE INITIAL DEAD SURFACE FUEL LOADINGS ARE ESTIMATED
  !  FROM FIA FOREST TYPE.

  !                 <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
  data FUINI /0.10,0.50,1.68,0.55,0.64,0.07,0.0,0.0,0.0,4.02,12.52,& ! eastern white pine (100s)
       &0.10,0.66,0.98,0.12,0.29,0.26,0.0,0.0,0.0,6.38,8.66,& ! longleaf-slash pine (140s)
       &0.14,0.72,1.54,0.25,0.44,0.33,0.0,0.0,0.0,4.90,6.03,& ! loblolly-shortleaf pine (160s)
       &0.24,1.24,2.72,0.36,0.97,0.33,0.0,0.0,0.0,3.82,3.80,& ! eastern redcedar (181 / 402)
       &0.18,0.77,2.17,0.31,0.86,0.78,0.0,0.0,0.0,4.07,6.15,& ! oak-pine (400s)
       &0.13,0.68,1.93,0.43,1.01,1.01,0.0,0.0,0.0,4.28,5.91,& ! oak-hickory (500s)
       &0.13,0.67,1.83,0.18,0.57,0.77,0.0,0.0,0.0,2.49,5.68,& ! oak-gum-cypress (600s)
       &0.22,1.09,2.68,0.26,0.76,0.43,0.0,0.0,0.0,2.33,1.60,& ! elm-ash-cottonwood (700s)
       &0.09,0.64,2.03,0.43,1.18,3.38,0.0,0.0,0.0,3.75,4.10/ ! maple-beech-birch (800s)

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

  !  SET LIVE FUEL LEVELS

  if ((PCOM(1:3) .eq. '232') .or. (PCOM(1:3) .eq. '231') .or.&
       &(PCOM(1:4) .eq. 'M221')) then

     if (SITEAR(ISISP) .lt.50) then
        J = 1
     else if (SITEAR(ISISP) .lt.65) then
        J = 2
     else if (SITEAR(ISISP) .lt.80) then
        J = 3
     else if (SITEAR(ISISP) .lt.95) then
        J = 4
     else if (SITEAR(ISISP) .lt.110) then
        J = 5
     else
        J = 6
     end if

     if (BURNYR .gt.0) then
        SHRUBAGE = IYR - BURNYR
     else
        SHRUBAGE = (IYR - IY(1) + 5) ! so default is 5 years before inventory
     end if
     SHRUBAGE = MIN(20,MAX(1,INT(SHRUBAGE)))

     do L = 1,8
        FULIV3(L) = FULIV2(L,J)
     end do

     FLIVE(1) = 0.0 ! no herbs
     FLIVE(2) = ALGSLP(SHRUBAGE,Y,FULIV3,8)
     if ((PCOM(1:3) .eq. '231') .or. (PCOM(1:4) .eq. 'M221')) then
        FLIVE(2) = FLIVE(2)*0.40
     end if

  else

     !  DETERMINE FFE FOREST TYPE (1 OF 8 CATEGORIES) FROM FIA FOR. TYPE AND LOAD LIVE FUELS

     call FMSNFT(IFFEFT)
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
  end if

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
        COVTYP = 75
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
     case(101:105)
        FTDEADFU = 1 !eastern white pine
     case(141,142)
        FTDEADFU = 2 !longleaf-slash pine
     case(161:168)
        FTDEADFU = 3 !loblolly-shortleaf pine
     case(181,402)
        FTDEADFU = 4 !eastern redcedar
     case(401,403:409)
        FTDEADFU = 5 !oak-pine
     case(501:520)
        FTDEADFU = 6 !oak-hickory
     case(601:608)
        FTDEADFU = 7 !oak-gum-cypress
     case(701:709)
        FTDEADFU = 8 !elm-ash-cottonwood
     case(801:809)
        FTDEADFU = 9 !maple-beech-birch
     case default
        FTDEADFU = 6 !oak-hickory
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

  !     IN FIRST YEAR, SET C-REPORTING REGION FOR FOREST CODE
  !     DEFAULT is ICHABT=1 -> SC; ICHABT=2 -> SE

  if (IYR.eq.IY(1)) then
     IFK = (KODFOR/100) ! drop lower 2 digits
     select case (IFK)
     case (803,805,808,811,812)
        ICHABT = 2
     end select
  end if

  entry SNGCOE

  !  ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
  !  IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

  return
end
