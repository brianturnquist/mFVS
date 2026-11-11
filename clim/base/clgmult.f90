!=========================!
!   clim/base : clgmult   !
!=========================!
! CLIM-BASE $Id$
subroutine CLGMULT(TREEMULT)
  implicit none

  !     CLIMATE EXTENSION -- COMUTES TREE-LEVEL GROWTH MULTIPLIER

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'CLIMATE.f90'

  ! Variables
  integer :: I,I1,I2
  real    :: TREEMULT(MAXTRE),THISYR,ALGSLP,BIRTHYR,GROW_TD0,GROW_TD1,XINVYR,D100_BIRTH,D100_NOW,&
       &D100_INVYR,MTCM_BIRTH,MTCM_NOW,MTCM_INVYR,MTCM_TD,SMI_NOW,SMI_INVYR,SMI_BIRTH,SMI_TD,MMIN_NOW,&
       &MMIN_INVYR,MMIN_BIRTH,MMIN_TD,DD0_NOW,DD0_INVYR,PSITE_NOW,PSITE_INVYR,DD0_BIRTH,MAT_BIRTH,XDF,XWL,&
       &XPP,XRELGR,VSCORE(MAXSP),PS,SPWTS(MAXSP),XWT,XGSITE
  logical :: DEBUG
  integer :: MYACT(1),IDT,IACT,NP,ITODO,NTODO
  real    :: PRMS(2)
  data MYACT/2803/

  call DBCHK (DEBUG,'CLGMULT',7,ICYC)
  if (DEBUG) write (JOSTND,1) LCLIMATE
  1 format ('IN CLGMULT, LCLIMATE=',L2)

  call OPFIND(1,MYACT,NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET(ITODO,2,IDT,IACT,NP,PRMS)
        if (IACT.lt.0.or. NP.ne.2) cycle
        call OPDONE (ITODO,IY(ICYC))
        I = IFIX(PRMS(1))
        if (I.eq.0) then
           CLGROWMULT = PRMS(2)
        else if (I.gt.0.and. I.le.MAXSP) then
           CLGROWMULT(I) = PRMS(2)
        end if
     end do
  end if

  !     INSURE THE MULTIPLIER, AND THE REPORTED SPECIES AVERAGE ARE
  !     INITIALLY 1.0 (NO CLIMATE EFFECT).

  TREEMULT(1:ITRN) = 1.
  SPGMULT = 0.
  SPVIAB = 1.
  SPSITGM = 1.

  if (.not.LCLIMATE) return

  if (DEBUG) write (JOSTND,2) ICYC,IY(ICYC),IXMTCM,IXMAT,&
       &IXDD5,IXGSP,IXD100,IXMMIN,IXDD0,IXPSITE
  2 format ('IN CLGMULT, ICYC,IY(ICYC)=',2I5,' IXMTCM,',&
       &'IXDD5,IXMAT=',3I4,' IXGSP,IXD100,IXMMIN,IXDD0,IXPSITE=',5I4)

  if (IXMTCM*IXDD5*IXMAT*IXGSP*IXD100*IXMMIN*IXDD0.eq.0)&
       &return

  !     LOAD THE CLIMATE DATA FOR THIS YEAR.

  XINVYR = FLOAT(IY(1))

  MTCM_INVYR = ALGSLP (XINVYR, FLOAT(YEARS), ATTRS(1,IXMTCM),NYEARS)
  D100_INVYR = ALGSLP (XINVYR, FLOAT(YEARS), ATTRS(1,IXD100),NYEARS)
  MMIN_INVYR = ALGSLP (XINVYR, FLOAT(YEARS), ATTRS(1,IXMMIN),NYEARS)
  DD0_INVYR = ALGSLP (XINVYR, FLOAT(YEARS), ATTRS(1,IXDD0), NYEARS)
  SMI_INVYR = ALGSLP (XINVYR, FLOAT(YEARS), ATTRS(1,IXDD5), NYEARS)/&
       &ALGSLP (XINVYR, FLOAT(YEARS), ATTRS(1,IXGSP), NYEARS)

  THISYR = FLOAT(IY(ICYC))+(FINT/2)

  MTCM_NOW = ALGSLP (THISYR, FLOAT(YEARS), ATTRS(1,IXMTCM),NYEARS)
  D100_NOW = ALGSLP (THISYR, FLOAT(YEARS), ATTRS(1,IXD100),NYEARS)
  MMIN_NOW = ALGSLP (THISYR, FLOAT(YEARS), ATTRS(1,IXMMIN),NYEARS)
  DD0_NOW = ALGSLP (THISYR, FLOAT(YEARS), ATTRS(1,IXDD0), NYEARS)
  SMI_NOW = ALGSLP (THISYR, FLOAT(YEARS), ATTRS(1,IXDD5), NYEARS)/&
       &ALGSLP (THISYR, FLOAT(YEARS), ATTRS(1,IXGSP), NYEARS)

  if (IXPSITE.eq.0) then
     PSITE_INVYR = 1.0
     PSITE_NOW = 1.0
     XGSITE = 1.0
  else
     PSITE_INVYR = ALGSLP(XINVYR,FLOAT(YEARS),ATTRS(1,IXPSITE),NYEARS)
     PSITE_NOW = ALGSLP (THISYR,FLOAT(YEARS),ATTRS(1,IXPSITE),NYEARS)
     if (PSITE_INVYR.le..001) then
        XGSITE = 2.
     else if (ABS(PSITE_NOW-PSITE_INVYR) .lt..001) then
        XGSITE = 1.0
     else
        XGSITE = PSITE_NOW/PSITE_INVYR
        XGSITE = 1.5819767*(1.-(EXP(-XGSITE)))
        if (ABS(XGSITE-1.0).lt..01) XGSITE = 1.0
     end if
  end if
  if (DEBUG) write (JOSTND,5) MTCM_INVYR,D100_INVYR,MMIN_INVYR,&
       &SMI_INVYR,PSITE_INVYR,PSITE_NOW,XGSITE
  5 format ('IN CLGMULT,MTCM_INVYR,D100_INVYR,MMIN_INVYR,',&
       &' SMI_INVYR=',4F10.4,'PSITE_INVYR,NOW,XGSITE=',3F10.4)

  VSCORE = 1.0
  do I = 1,MAXSP
     I2 = INDXSPECIES(I)
     if (I2.gt.0) then
        VSCORE(I) = ALGSLP (THISYR,FLOAT(YEARS),ATTRS(1,I2),NYEARS)
        SPVIAB(I) = VSCORE(I) ! used in the report generation
        if (VSCORE(I).gt.0.5) then
           VSCORE(I) = 1.
        else
           VSCORE(I) = -.66666667 + VSCORE(I)*3.3333333
        end if
        if (VSCORE(I).lt.0.2) VSCORE(I) = 0.2
        if (DEBUG) write (JOSTND,7) JSP(I),VSCORE(I)
        7 format ('IN CLGMULT, SP=',A2,' VSCORE= ',F13.5)
     end if
  end do

  do I = 1,ITRN

     !       ABIRTH IS AGE, NOT YEAR OF BIRTH.  COMPUTE BIRTH YEAR:

     BIRTHYR = THISYR-ABIRTH(I)

     MTCM_BIRTH = ALGSLP (BIRTHYR,FLOAT(YEARS),ATTRS(1,IXMTCM),&
          &NYEARS)
     MAT_BIRTH = ALGSLP (BIRTHYR,FLOAT(YEARS),ATTRS(1,IXMAT),&
          &NYEARS)
     MMIN_BIRTH = ALGSLP (BIRTHYR,FLOAT(YEARS),ATTRS(1,IXMMIN),&
          &NYEARS)
     DD0_BIRTH = ALGSLP (BIRTHYR,FLOAT(YEARS),ATTRS(1,IXDD0),&
          &NYEARS)
     D100_BIRTH = ALGSLP (BIRTHYR,FLOAT(YEARS),ATTRS(1,IXD100),&
          &NYEARS)
     SMI_BIRTH = ALGSLP (BIRTHYR,FLOAT(YEARS),ATTRS(1,IXDD5),&
          &NYEARS)   /&
          &ALGSLP (BIRTHYR,FLOAT(YEARS),ATTRS(1,IXGSP),&
          &NYEARS)

     !       FROM LEITES ET AL. ECOLOGICAL APPLICATIONS 22(1)-154-165
     !       b0 (intercept)  373.97
     !       b1 (MTCM_TD)      6.799
     !       b2 (MTCM_TD^2)   -3.726
     !       b3 (MTCM)        38.52
     !       b4 (MTCM_TD*MTCM)-3.602

     !       NOTE THAT MTCM_TD IS ACTUALLY THE DIFFERENCE BETWEEN TWO PLACES, HERE
     !       WE SUBSTITUE TIME FOR SPACE. MTCM IS THE TEMPERATURE AT THE SEED
     !       SOURCE...WE USE IT HERE AS "BIRTH YEAR".

     MTCM_TD = MTCM_NOW - MTCM_BIRTH
     GROW_TD0 = 373.97 + 38.52*MTCM_BIRTH
     GROW_TD1 = 373.97 + 6.799*MTCM_TD - 3.726*MTCM_TD**2 +&
          &38.52 * MTCM_BIRTH    - 3.602*MTCM_BIRTH*MTCM_TD
     if (GROW_TD0.lt.0.05) GROW_TD0 = 0.05
     if (GROW_TD1.lt.0.05) GROW_TD1 = 0.05
     XDF = GROW_TD1/GROW_TD0

     !       FROM LEITES FINAL LARCH MODEL
     !       b0 (intercept)     542.20
     !       b1 (mmin.trds)     17.50
     !       b2 (mmin.trds2)    -1.215
     !       b3 (dd0)           -0.1468
     !       b4 (mmin.trds*dd0) -0.0187

     MMIN_TD = MMIN_NOW - MMIN_BIRTH
     GROW_TD0 = 542.20 - 0.1468*DD0_BIRTH
     GROW_TD1 = 542.20 + 17.50*MMIN_TD - 1.215*MMIN_TD**2&
          &- 0.1468*DD0_BIRTH - 0.0187*MMIN_TD*DD0_BIRTH
     if (GROW_TD0.lt.0.05) GROW_TD0 = 0.05
     if (GROW_TD1.lt.0.05) GROW_TD1 = 0.05
     XWL = GROW_TD1/GROW_TD0

     !       FROM LEITES PRELIMINARY PONDEROSA PINE MODEL
     !       (Intercept)     551.20221
     !       smi.trds        -14.88483
     !       I(smi.trds^2)   -0.58027
     !       d100            -2.02135
     !       smi.trds:d100    0.15582

     SMI_TD = SMI_NOW - SMI_BIRTH
     GROW_TD0 = 551.20221 - 2.02135*D100_BIRTH
     GROW_TD1 = 551.20221 -14.88483*SMI_TD -0.58027*SMI_TD**2&
          &-2.02135*D100_BIRTH + 0.15582*SMI_TD*D100_BIRTH
     if (GROW_TD0.lt.0.05) GROW_TD0 = 0.05
     if (GROW_TD1.lt.0.05) GROW_TD1 = 0.05
     XPP = GROW_TD1/GROW_TD0

     if (PLNJSP(ISP(I)).eq.'PSME') then
        XRELGR = XDF
     else if (PLNJSP(ISP(I)).eq.'PICO') then
        XRELGR = XDF
     else if (PLNJSP(ISP(I)).eq.'PIPO') then
        XRELGR = XPP
     else if (PLNJSP(ISP(I)).eq.'LAOC') then
        XRELGR = XWL
     else if (PLNJSP(ISP(I)).eq.'PIMO3') then
        XRELGR = XWL
     else if (PLNJSP(ISP(I)).eq.'PIEN') then
        XRELGR = XWL
     else if (PLNJSP(ISP(I)).eq.'TSHE') then
        XRELGR = XWL
     else
        XRELGR = 1 + ((((XDF+XPP+XWL)/3.) - 1.) *.5)
     end if

     if (ABS(XRELGR-1.0).lt..005) XRELGR = 1.0

     ! If the growth effects are above 1, then apply the one
     ! that results in the most growth. Otherwise apply the one
     ! that results in the least growth.
     PS = MIN(XGSITE,XRELGR,VSCORE(ISP(I)))
     if (PS.gt.0.99) PS = MAX(XGSITE,XRELGR,VSCORE(ISP(I)))
     if (PS .gt.3.) PS = 3.
     TREEMULT(I) = 1.+((PS-1.)*CLGROWMULT(ISP(I)))
     if (TREEMULT(I) .lt.0) TREEMULT(I) = 0.
     if (DEBUG) write (JOSTND,10) I,JSP(ISP(I)),BIRTHYR,&
          &XDF,XPP,XWL,PS,XRELGR,TREEMULT(I)
     10 format ('IN CLGMULT, I=',I5,' SP=',A2,' BIRTHYR=',F7.1,&
          &' XDF=',F8.3,' XPP=',F8.3,' XWL=',F8.3,&
          &' PS=',F8.3,' XRELGR=',F8.3,' TREEMULT=',F8.3)
  end do
  ! Compute SPSITGM for reporting only (this is a vector operation).
  SPSITGM = XGSITE**CLGROWMULT

  !     CREATE THE REPORTED AVERAGE SCORE.

  SPWTS = 0.
  do I = 1,ITRN
     I1 = ISP(I)
     XWT = DBH(I)*DBH(I)*PROB(I) ! ba weighted
     SPWTS(I1) = SPWTS(I1)+XWT
     SPGMULT(I1) = SPGMULT(I1)+(TREEMULT(I)*XWT)
  end do
  do I = 1,MAXSP
     if (SPWTS(I).gt.0.) then
        SPGMULT(I) = SPGMULT(I)/SPWTS(I)
     else
        SPGMULT(I) = 1.
     end if
  end do

  if (DEBUG) then
     do I = 1,MAXSP
        write (JOSTND,20) I,JSP(I),SPVIAB(I),&
             &SPSITGM(I),SPGMULT(I),SPWTS(I)
        20 format ('IN CLGMULT, I=',I5,' SP=',A2,' SPVIAB=',F7.3,&
             &' SPSITGM=',F9.3,' SPGMULT=',F9.3,' SPWTS=',F13.2)
     end do
  end if

  return
end
