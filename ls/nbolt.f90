!================!
!   ls : nbolt   !
!================!
! LS $Id$
subroutine NBOLT(ISPC,H,D,DBHMIN,BFMIND,SINDX,TOPDOB,BFTDOB,&
     &JOSTND,DEBUG,IHT1,IHT2)
  implicit none

  ! THIS ROUTINE CALCULATES THE NUMBER OF 8' BOLTS TO SPECIFIED SAWTIMBER
  ! TOP DIAMETER AND THE NUMBER OF 8' BOLTS TO A SPECIFIED PULPWOOD TOP
  ! DIAMETER.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'VARCOM.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: DEBUG

  real    :: B1(MAXSP),B2(MAXSP),B3(MAXSP),B4(MAXSP),B5(MAXSP),B6(MAXSP),DBHMIN(MAXSP),BFMIND(MAXSP)
  integer :: IHT1,IHT2,JOSTND,ISPC
  real    :: BFTDOB,TOPDOB,SINDX,D,H,FACTOR,ESTTHT,ESTCHT,PULBOL
  real    :: ESTSHT,SAWBOL

  if (DEBUG) write(JOSTND,10)
  10 format(' ENTERING NBOLT ')

  !  LOADING SPECIES SPECIFIC COEFFICIENTS FOR HEIGHT EQUATIONS.

  data B1/&
       &2*16.934,2*36.851,16.281,2*31.957,14.304,20.038,13.62,&
       &8.2079, 5.3117,2*16.934,2*11.291,13.625,6.9572,2*6.86,&
       &3*8.458,7.1852,6.3628,3*5.3416,8.1782,4*9.2078,6.6844,&
       &2*3.8011,3*6.1034,5.5346,2*6.4301, 7.2773,&
       &25*6.4301/
  data B2/&
       &2*.12972,2*.08298,.08621,2*.18511,.19894,.18981,.24255,.19672,&
       &.10357,2*.12972,2*.25250,.28668,.26564,2*.27725,3*.27527,&
       &.28384,.27859,3*.23044,.27316,4*.22208,.19049,2*.39213,&
       &3*.17368,.22637,2*.23545,.22721,25*.23545/
  data B3/&
       &5*1.0,2*1.7020,1.4195,1.2909,1.2885,1.3112,&
       &3*1.0,2*1.5466,1.6124,1.0,2*1.4287,3*1.9602,1.4417,&
       &1.8677,3*1.1529,1.7250,5*1.00,2*2.9053,4*1.00,&
       &2*1.3380,1.0000,25*1.3380/
  data B4/&
       &2*.20854,2*.00001,.16220,2*.0,.23349,.17836,.25831,.33978,&
       &.68454,2*.20854,2*.35711,.30651,.48660,2*.40115,3*.34894,&
       &.38884,.49589,3*.54194,.38694,4*.31723,.43972,2*.55634,&
       &3*.44725,.46918,2*.47370,.41179,25*.47370/
  data B5/&
       &2*.77792,2*.63884,.86833,2*.68967,.76878,.57343,.68128,.76173,&
       &.7141,2*.77792,2*.7506,1.0292,.76954,2*.85299,3*.89213,.82157,&
       &.76169,3*.8344,.75822,4*.8356,.82962,2*.84317,3*1.02370,.72456,&
       &2*.73385,.76498,25*.73385/
  data B6/&
       &2*.12902,2*.18231,.23316,2*.162,.12399,.10159,.10771,.11666,&
       &.00,2*.12902,2*.06859,.07460,.01618,2*.12403,3*.12594,.11411,&
       &.05841,3*.06372,.10847,4*.13465,.10806,2*.09593,3*.1461,.11782,&
       &2*.08228,.11046,25*.08228/

  !  COMPUTE ESTIMATED TOTAL TREE HEIGHT.

  if (D .lt. DBHMIN(ISPC)) then
     IHT1 = 0
     IHT2 = 0
     go to 100
  else
     FACTOR = 0.0
     ESTTHT = 4.5+B1(ISPC)*(1.0-EXP(-1.0*B2(ISPC)*D))&
          &**B3(ISPC)*SINDX**B4(ISPC)*(1.00001-FACTOR)&
          &**B5(ISPC)*BA**B6(ISPC)
     if (DEBUG)write(JOSTND,*)' ESTTHT=',ESTTHT,' ISPC=',ISPC,&
          &' B1=',B1(ISPC),' B2=',B2(ISPC),' B3=',B3(ISPC),' B4=',&
          &B4(ISPC),' B5=',B5(ISPC),' B6=',B6(ISPC),' D=',D,' H=',H,&
          &' SINDX=',SINDX,' TOP/D=',TOPDOB/D,' BA=',BA,&
          &' TOPDOB=',TOPDOB
  end if

  !  COMPUTE THE NUMBER OF 8' BOLTS TO SPECIFIED PULPWOOD TOP DIAMETER.

  if (D .lt. DBHMIN(ISPC)) then
     IHT1 = 0
     IHT2 = 0
     go to 100
  else
     FACTOR = TOPDOB/D
     if (FACTOR .gt.1.0) FACTOR = 1.0
     ESTCHT = 4.5+B1(ISPC)*(1.0-EXP(-1.0*B2(ISPC)*D))&
          &**B3(ISPC)*SINDX**B4(ISPC)*(1.00001-FACTOR)&
          &**B5(ISPC)*BA**B6(ISPC)
     if (DEBUG)write(JOSTND,*)' ESTCHT=',ESTCHT,' ISPC=',ISPC,&
          &' B1=',B1(ISPC),' B2=',B2(ISPC),' B3=',B3(ISPC),' B4=',&
          &B4(ISPC),' B5=',B5(ISPC),' B6=',B6(ISPC),' D=',D,' H=',H,&
          &' SINDX=',SINDX,' TOP/D=',TOPDOB/D,' BA=',BA,&
          &' TOPDOB=',TOPDOB
     PULBOL = ESTCHT*(H/ESTTHT)
     IHT2 = INT(PULBOL/8.333333)
     if (DEBUG)write(JOSTND,*)' No. PULPWOOD BOLTS (IHT2)= ',IHT2
  end if

  !  COMPUTE THE NUMBER OF 8' BOLTS TO A SPECIFIED SAWTIMBER TOP
  !  DIAMETER.

  if (D .lt. BFMIND(ISPC)) then
     IHT1 = 0
     go to 100
  else
     FACTOR = BFTDOB/D
     if (FACTOR .gt.1.0) FACTOR = 1.0
     ESTSHT = 4.5+B1(ISPC)*(1.0-EXP(-1.0*B2(ISPC)*D))&
          &**B3(ISPC)*SINDX**B4(ISPC)*(1.00001-FACTOR)&
          &**B5(ISPC)*BA**B6(ISPC)
     if (DEBUG)write(JOSTND,*)' B1=',B1(ISPC),' B2=',B2(ISPC),&
          &' B3=',B3(ISPC),' B4=',B4(ISPC),' B5=',B5(ISPC),' B6=',&
          &B6(ISPC),' ISPC=',ISPC,' BA=',BA,' SINDX=',SINDX,&
          &' BFT/D=',BFTDOB/D,' D=',D,' H=',H,' ESTSHT=',ESTSHT
     SAWBOL = ESTSHT*(H/ESTTHT)
     IHT1 = INT(SAWBOL/8.333333)
     if (DEBUG)write(JOSTND,*)' No. SAWWOOD BOLTS (IHT1)= ',IHT1
  end if
  100 continue
  return
end
