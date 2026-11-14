!=====================!
!   vcovr : cvmapsp   !
!=====================!
! VCOVR $Id$
subroutine CVMAPSP (MAPISP)
  implicit none

  !  **CVMAPSP** MAPS EACH VARIANT SPECIES TO ONE OF THE ORIGINAL 11 NI
  !  SPECIES WHOSE COEFFICIENTS ARE USED BY THE COVER MODEL. CALLED BY
  !  **CVSHAP** AND **CVCBMS**, WHICH PREVIOUSLY HELD IDENTICAL COPIES OF
  !  THESE TABLES.

  !  MAPISP(49) -- (OUTPUT) ORIGINAL NI SPECIES INDEX (1-11) FOR EACH
  !                VARIANT SPECIES INDEX

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  integer :: MAPISP(49),MAPAK(49),MAPCA(49),MAPBM(49),MAPCI(49),MAPCR(49),MAPEC(49),MAPEM(49),&
       &MAPIE(49),MAPNI(49),MAPSO(49),MAPTT(49),MAPUT(49),MAPWC(49),MAPWS(49)

  !  MAP VARIANT SPECIES = ORIGINAL 11 SPECIES
  !  IF VARIANT SPECIES NUMBER OR ORDER CHANGES
  !  THE FOLLOWING MAPPING SHOULD BE ADJUSTED HERE

  data MAPNI /&
       !     WP=WP,WL=WL,DF=DF,GF=GF,WH=WH,RC=RC,LP=LP,ES=ES,AF=AF,PP=PP
       &1,    2,    3,    4,    5,    6,    7,    8,    9,   10,&
       !     MH=MH RW=RC
       &11,    6, 37*0/

  data MAPAK /&
       !     WS=ES,RC=RC,SF=GF,MH=MH,WH=WH,YC=RC,LP=LP,SS=ES,AF=AF,RA=WL
       &8,    6,    4,   11,    5,    6,    7,    8,    9,    2,&
       !     CW=WL,OH=WL,OS=MH
       &2,    2,   11, 36*0/

  data MAPBM /&
       !     WP=WP,WL=WL,DF=DF,GF=GF,MH=MH,WJ=RC,LP=LP,ES=ES,AF=AF,PP=PP
       &1,    2,    3,    4,   11,    6,    7,    8,    9,   10,&
       !     WB=WP,LM=WP,PY=WH,YC=RC,AS=WL,CW=WL,OS=MH,OH=WL
       &1,    1,    5,    6,    2,    2,   11,    2, 31*0/

  data MAPCA /&
       !     PC=RC,IC=RC,RC=RC,WF=GF,RF=AF,SH=AF,DF=DF,WH=WH,MH=MH,WB=WP
       &6,    6,    6,    4,    9,    9,    3,    5,   11,    1,&
       !     KP=PP,LP=LP,CP=PP,LM=WP,JP=PP,SP=WP,WP=WP,PP=PP,MP=PP,GP=PP
       &10,    7,   10,    1,   10,    1,    1,   10,   10,   10,&
       !     WJ=RC,BR=ES,GS=RC,PY=WH,OS=MH,LO=WL,CY=WL,BL=WL,EO=WL,WO=WL
       &6,    8,    6,    5,   11,    2,    2,    2,    2,    2,&
       !     BO=WL,VO=WL,IO=WL,BM=WL,BU=WL,RA=WL,MA=WL,GC=WL,DG=WL,FL=WL
       &2,    2,    2,    2,    2,    2,    2,    2,    2,    2,&
       !     WN=WL,TO=WL,SY=WL,AS=WL,CW=WL,WI=WL,CN=WL,CL=WL,OH=WL
       &2,    2,    2,    2,    2,    2,    2,    2,    2/

  data MAPCI /&
       !     WP=WP,WL=WL,DF=DF,GF=GF,WH=WH,RC=RC,LP=LP,ES=ES,AF=AF,PP=PP
       &1,    2,    3,    4,    5,    6,    7,    8,    9,   10,&
       !     WB=WP,PY=WH,AS=WL,WJ=RC,MC=WL,LM=WP,CW=WL,OS=MH,OH=WL
       &1,    5,    2,    6,    2,    1,    2,   11,    2, 30*0/

  data MAPCR /&
       !     AF=AF,CB=AF,DF=DF,GF=GF,WF=GF,MH=MH,RC=RC,WL=WL,BC=WP,LM=WP
       &9,    9,    3,    4,    4,   11,    6,    2,    1,    1,&
       !     LP=LP,PI=PP,PP=PP,WB=WP,SW=WP,UJ=RC,BS=ES,ES=ES,WS=ES,AS=WL
       &7,   10,   10,    1,    1,    6,    8,    8,    8,    2,&
       !     NC=WL,PW=WL,GO=WL,AW=WL,EM=WL,BK=WL,SO=WL,PB=WL,AJ=RC,RM=RC
       &2,    2,    2,    2,    2,    2,    2,    2,    6,    6,&
       !     OJ=RC,ER=RC,PM=PP,PD=PP,AZ=PP,CI=LP,OS=MH,OH=WL
       &6,    6,   10,   10,   10,    7,   11,    2, 11*0/

  data MAPEC /&
       !     WP=WP,WL=WL,DF=DF,SF=GF,RC=RC,GF=GF,LP=LP,ES=ES,AF=AF,PP=PP
       &1,    2,    3,    4,    6,    4,    7,    8,    9,   10,&
       !     WH=WH,MH=MH,PY=WH,WB=WP,NF=GF,WF=GF,LL=WL,YC=RC,WJ=RC,BM=WL
       &5,   11,    5,    1,    4,    4,    2,    6,    6,    2,&
       !     VN=WL,RA=WL,PB=WL,GC=WL,DG=WL,AS=WL,CW=WL,WO=WL,PL=WL,WI=WL
       &2,    2,    2,    2,    2,    2,    2,    2,    2,    2,&
       !     OS=MH,OH=WL
       &11,    2, 17*0/

  data MAPEM /&
       !     WB=WP,WL=WL,DF=DF,LM=WP,LL=WL,RM=RC,LP=LP,ES=ES,AF=AF,PP=PP
       &1,    2,    3,    1,    2,    6,    7,    8,    9,   10,&
       !     GA=WL,AS=WL,CW=WL,BA=WL,PW=WL,NC=WL,PB=WL,OS=MH,OH=WL
       &2,    2,    2,    2,    2,    2,    2,   11,    2, 30*0/

  data MAPIE /&
       !     WP=WP,WL=WL,DF=DF,GF=GF,WH=WH,RC=RC,LP=LP,ES=ES,AF=AF,PP=PP
       &1,    2,    3,    4,    5,    6,    7,    8,    9,   10,&
       !     MH=MH,WB=WP,LM=WP,LL=WL,PI=PP,RM=RC,PY=WH,AS=WL,CO=WL,MM=WL
       &11,    1,    1,    2,   10,    6,    5,    2,    2,    2,&
       !     PB=WL,OH=WL,OS=MH
       &2,    2,   11, 26*0/

  data MAPSO /&
       !     WP=WP,SP=WP,DF=DF,WF=GF,MH=MH,IC=RC,LP=LP,ES=ES,SH=AF,PP=PP
       &1,    1,    3,    4,   11,    6,    7,    8,    9,   10,&
       !     WJ=RC,GF=GF,AF=AF,SF=GF,NF=GF,WB=WP,WL=WL,RC=RC,WH=WH,PY=WH
       &6,    4,    9,    4,    4,    1,    2,    6,    5,    5,&
       !     WA=WL,RA=WL,BM=WL,AS=WL,CW=WL,CH=WL,WO=WL,WI=WL,GC=WL,MC=WL
       &2,    2,    2,    2,    2,    2,    2,    2,    2,    2,&
       !     MB=WL,OS=MH,OH=WL
       &2,   11,    2, 16*0/

  data MAPTT /&
       !     WB=WP,LM=WP,DF=DF,PM=PP,BS=ES,AS=WL,LP=LP,ES=ES,AF=AF,PP=PP
       &1,    1,    3,   10,    8,    2,    7,    8,    9,  10,&
       !     UJ=RC,RM=RC,BI=WL,MM=WL,NC=WL,MC=WL,OS=MH,OH=WL
       &6,    6,    2,    2,    2,    2,   11,    2, 31*0/

  data MAPUT /&
       !     WB=WP,LM=WP,DF=DF,WF=GF,BS=ES,AS=WL,LP=LP,ES=ES,AF=AF,PP=PP,
       &1,    1,    3,    4,    8,    2,    7,    8,    9,   10,&
       !     PI=PP,WJ=RC,GO=WL,PM=PP,RM=RC,UJ=RC,GB=WP,NC=WL,FC=WL,MC=WL,
       &10,    6,    2,   10,    6,    6,    1,    2,    2,    2,&
       !    &BI=WL,BE=WL,OS=MH,OH=WL
       &2,    2,   11,    2, 25*0/

  data MAPWC /&
       !     SF=GF,WF=GF,GF=GF,AF=AF,RF=AF,  =MH,NF=GF,YC=RC,IC=RC,ES=ES
       &4,    4,    4,    9,    9,   11,    4,    6,    6,    8,&
       !     LP=LP,JP=PP,SP=WP,WP=WP,PP=PP,DF=DF,RW=RC,RC=RC,WH=WH,MH=MH
       &7,   10,    1,    1,   10,    3,    6,    6,    5,   11,&
       !     BM=WL,RA=WL,WA=WL,PB=WL,GC=WL,AS=WL,CW=WL,WO=WL,WJ=RC,LL=WL
       &2,    2,    2,    2,    2,    2,    2,    2,    6,    2,&
       !     WB=WP,KP=PP,PY=WH,DG=WL,HT=WL,CH=WL,WI=WL,  =MH,OT=MH
       &1,   10,    5,    2,    2,    2,    2,   11,   11, 10*0/

  data MAPWS /&
       !     SP=WP,DF=DF,WF=GF,GS=RC,IC=RC,JP=PP,RF=AF,PP=PP,LP=LP,WB=WP
       &1,    3,    4,    6,    6,   10,    9,   10,    7,    1,&
       !     WP=WP,PM=PP,SF=GF,KP=PP,FP=PP,CP=PP,LM=WP,MP=PP,GP=PP,WE=PP,
       &1,   10,    4,   10,   10,   10,    1,   10,   10,   10,&
       !     GB=WP,BD=DF,RW=RC,MH=MH,WJ=RC,UJ=RC,CJ=RC,LO=WL,CY=WL,BL=WL,
       &1,    3,    6,   11,    6,    6,    6,    2,    2,    2,&
       !     BO=WL,VO=WL,IO=WL,TO=WL,GC=WL,AS=WL,CL=WL,MA=WL,DG=WL,BM=WL,
       &2,    2,    2,    2,    2,    2,    2,    2,    2,    2,&
       !     MC=WL,OS=MH,OH=WL
       &2,   11,    2,  6*0/

  select case (VARACD)
     !  ORIGINAL 11 SPECIES VARIANTS
  case('KT','NC')
     MAPISP = MAPNI
  case('AK')
     MAPISP = MAPAK
  case('BM')
     MAPISP = MAPBM

     !  SPECIES 4=GF IN OC VARIANT SO USING CA MAPPING IS OKAY FOR OC

  case('CA','OC')
     MAPISP = MAPCA
  case('CI')
     MAPISP = MAPCI
  case('CR')
     MAPISP = MAPCR
  case('EC')
     MAPISP = MAPEC
  case('EM')
     MAPISP = MAPEM
  case('IE')
     MAPISP = MAPIE
  case('SO')
     MAPISP = MAPSO
  case('TT')
     MAPISP = MAPTT
  case('UT')
     MAPISP = MAPUT

     !  SPECIES 23=MA, 24=TO, 25=GC IN OP VARIANT; ALL MAP TO WL. SO USING
     !  WC MAPPING IS OKAY FOR OP

  case('WC','PN','OP')
     MAPISP = MAPWC
  case('WS')
     MAPISP = MAPWS
  case default
     MAPISP = 1
  end select
  return
end
