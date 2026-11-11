!==========================!
!   volume : fvshannbare   !
!==========================!
! VOLUME $Id$

subroutine FVSHANNBARE(VN,VM,VMAX,ISPC,D,H,CTKFLG)
  implicit none
  !  This routine calculates volumes for the CR variant using
  !  the Hann and Bare method METHC = 8, R2 forestes use coefficients
  !  from the Cibola NF
  !  called from **FVSVOL

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'GGCOM.f90'
  include 'CONTRL.f90'
  include 'VOLSTD.f90'

  !  DECLARATIONS: NOTE THAT THIS IS A R2 VOLUME CALCULATION METHOD only SO
  !  THE MAXSP ELEMENT IN THE IEQMAP ARRAY IS HARD CODDED TO THe NUMBER
  !  OF SPECIES IN THE CR VARIANT

  ! Variables
  integer :: IEQMAP(11,38),ISPC,IEQN,I
  real    :: VN,VM,VMAX,D,H,X,RIC,RSI,VI,VMR,VU
  real    :: BBFV
  logical :: CTKFLG,BTKFLG
  real    :: BN(10),B0(10),B1(10),B2(10),F0(10),F1(10),F2(10),F3(10),E0(10),E1(10),E2(10),E3(10),&
       &A0(10),A1(10)

  !  LOCAL VARIABLES

  !     IEQMAP -- ARRAY CONTAINING EQUATION NUMBERS FOR R3 H&B VOLUMES
  !               1ST DIMENSION = R3 FOREST, 2ND DIMENSION=SPECIES

  !  VARIABLE IEQMAP IS USED FOR VOLUME METHOD 8 R3 H&B VOLUMES
  !  GLOBAL VALUES ARE SET HERE, SOME ARE MODIFIED IN **SITSET**
  !  DEPENDING ON MODEL TYPE AND FOREST CODE.
  !  COLUMNS IN THE FOLLOWING ARRAY CORRESPOND TO A REGION 3 FOREST.
  !  1=301, 2=302 ... 11=312
  !  ROWS CORRESPOND SPECIES: ROW 1=AF, 2=CB, ... , 38=OH

  data (IEQMAP(I, 1),I = 1,11)/ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
  data (IEQMAP(I, 2),I = 1,11)/ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
  data (IEQMAP(I, 3),I = 1,11)/ 6, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7/
  data (IEQMAP(I, 4),I = 1,11)/ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
  data (IEQMAP(I, 5),I = 1,11)/ 9,10,10,10,10,10,10, 9,10,10,10/
  data (IEQMAP(I, 6),I = 1,11)/ 9,10,10,10,10,10,10, 9,10,10,10/
  data (IEQMAP(I, 7),I = 1,11)/ 9,10,10,10,10,10,10, 9,10,10,10/
  data (IEQMAP(I, 8),I = 1,11)/ 9,10,10,10,10,10,10, 9,10,10,10/
  data (IEQMAP(I, 9),I = 1,11)/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
  data (IEQMAP(I,10),I = 1,11)/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
  data (IEQMAP(I,11),I = 1,11)/ 6, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7/
  data (IEQMAP(I,12),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,13),I = 1,11)/ 4, 5, 4, 4, 4, 4, 4, 4, 4, 5, 4/
  data (IEQMAP(I,14),I = 1,11)/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
  data (IEQMAP(I,15),I = 1,11)/ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1/
  data (IEQMAP(I,16),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,17),I = 1,11)/ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
  data (IEQMAP(I,18),I = 1,11)/ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
  data (IEQMAP(I,19),I = 1,11)/ 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
  data (IEQMAP(I,20),I = 1,11)/ 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8/
  data (IEQMAP(I,21),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,22),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,23),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,24),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,25),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,26),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,27),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,28),I = 1,11)/ 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8/
  data (IEQMAP(I,29),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,30),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,31),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,32),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,33),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,34),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,35),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,36),I = 1,11)/ 4, 5, 4, 4, 4, 4, 4, 4, 4, 5, 4/
  data (IEQMAP(I,37),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/
  data (IEQMAP(I,38),I = 1,11)/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/

  !  DATA FROM R2-CR VARIANT BRUCE AND DEMAR'S EQS.

  data A0/0.16089,0.22547,0.23720,0.08107,0.04831,0.43837,0.34113,&
       &0.03270,0.21090,0.15778/
  data A1/0.0020325,0.0021697,0.0022112,0.0019835,0.0020497,&
       &0.0017564,0.0019180,0.0023112,0.0018400,0.0020091/

  ! GROSS M.C.F. C.F. TABLE 4, PG. 7

  data BN/1.5,1.5,1.0,1.5,1.5,1.0,1.5,1.5,1.0,1.5/
  data B0/-0.21301,-0.26648,0.01855,-0.12535,-0.13397,-0.08315,&
       &-0.18763,-0.23643,-0.18270,-0.18756/
  data B1/0.0049121,0.0061290,0.0007882,0.0036042,0.0065017,&
       &0.0012190,0.0067187,0.0058021,0.0012482,0.0063265/
  data B2/0.0060606,0.0074311,0.0050551,0.0054063,0.0049022,&
       &0.0054174,0.0053645,0.0060804,0.0062448,0.0060413/

  ! GROSS BD.FT. INT. C.F. TABLE 7, PG. 11

  data F0/6.69197,5.98736,7.10051,6.84752,7.58122,6.58735,6.59717,&
       &6.68808,6.24688,5.73645/
  data F1/7.52011,9.84792,7.97922,7.69491,8.51941,0.89272,0.89405,&
       &-1.27685,7.01994,1.72093/
  data F2/216.34837,-300.81281,229.55650,221.37723,245.09754,&
       &243.51491,243.87797,-4.50480,201.95873,74.57379/
  data F3/0.0,2855.34246,0.0,0.0,0.0,0.0,0.0,1423.98524,0.0,0.0/

  ! GROSS BD. FT. SCR. C.F. TABLE 9, PG. 13

  data E0/1.00609,0.87845,0.98210,0.96579,0.99399,1.00090,0.87026,&
       &0.88789,1.0,1.01725/
  data E1/2.38466,0.0,0.92603,0.40579,1.46349,0.0,0.0,0.0,1.88814,&
       &1.87057/
  data E2/0.0,0.0,0.0,0.0,0.0,4.10007,0.0,0.0,0.0,0.0/
  data E3/0.0,15.99846,14.49444,16.93678,12.40585,0.0,19.49594,&
       &17.19374,8.85145,8.51445/

  !  USE CIBOLA COEFFICIENTS FOR R2 FORESTS

  if (IFOR .lt. IGFOR) then
     IEQN = IEQMAP(     3,ISPC)
  else
     IEQN = IEQMAP(IFOR-12,ISPC)
  end if

  !  SET INITIAL VALUES. IF DIAMETER LIMITS NOT MET THEN RETURN.

  VN = 0.
  VM = 0.
  if (D .lt.1.0.or. H .lt.1.0) go to 400
  if (IEQN .lt.1.or. IEQN .gt.10) go to 300

  ! COMPUTE TOTAL STEM GROSS CUBIC FOOT VOLUME - UNFORKED TREES

  VN = A0(IEQN)+(A1(IEQN)*D*D*H)
  if (VN .le.0.0) VN = 0.

  !   COMPUTE MERCH CUBIC FOOT VOLUME. IF DIAMETER LIMITS NOT MET
  !   THEN BYPASS THIS CALCULATION.

  if (D .lt. DBHMIN(ISPC) .or. D .le. TOPD(ISPC)) go to 400

  ! COMPUTE MERCHANTABLE GROSS CUBIC FOOT VOLUME- UNFORKED TREES, TOP
  ! DIAMETER (TOPD) 3 TO 8 IN. I.B.

  VU = B0(IEQN)+(B1(IEQN)*((TOPD(ISPC)**3)*H)/(D**BN(IEQN)))&
       &+(B2(IEQN)*D*D)
  if (VU .gt. VN) go to 400
  VM = VN - VU
  go to 400

  ! PINYON AND JUNIPER EQUATIONS
  ! CHOJNACKY & OTT  RES. NOTE INT-363  NOV 1986
  ! OAK FROM DAVE WILSON (ALSO A CHOJNACKY EQN)
  ! SET MERCH CUBIC = TOTAL SINCE THESE ARE FIREWOOD SPECIES

  300 continue
  X = D*D*H/1000.
  if (ISPC .eq.12) then
     if (X .le.5) then
        VN = -0.07 + 2.51*X + 0.098*X*X
     else
        VN = 7.29 + 2.51*X - 24.53/X
     end if
  else if (ISPC .eq.16) then
     if (X .le.5) then
        VN = -0.05 + 2.48*X + 0.057*X*X
     else
        VN = 4.24 + 2.48*X - 14.29/X
     end if
  else if (ISPC .eq.22) then
     if (X .le.5) then
        VN = -0.068 + 2.4048*X + 0.1383*X*X
     else
        VN = 6.571 + 2.4048*X - 17.704/X
     end if
  end if
  if (VN .lt.0.) VN = 0.
  if (D .lt. DBHMIN(ISPC)) then
     VM = 0.
  else
     VM = VN
  end if

  400 continue
  VMAX = VN
  CTKFLG = .true.
  return

  !  ENTER ANY OTHER BOARD HERE

  entry HANNBAREBF (BBFV,ISPC,D,H,VMAX,BTKFLG)

  !  USE CIBOLA COEFFICIENTS FOR R2 FORESTS

  if (IFOR .lt. IGFOR) then
     IEQN = IEQMAP(     3,ISPC)
  else
     IEQN = IEQMAP(IFOR-12,ISPC)
  end if
  BBFV = 0.
  BTKFLG = .false.

  !  IF NON-COMMERCIAL SPECIES, RETURN 0 BOARD FOOT VOLUME.

  if (IEQN .lt.1) return

  !   IF DIAMETER LIMITS NOT MET THEN RETURN.

  if (D .lt. BFMIND(ISPC) .or. D .lt. BFTOPD(ISPC)) return

  ! CALCULATE M.C.F. FOR DTOPB INCH TOP

  VU = B0(IEQN)+(B1(IEQN)*((BFTOPD(ISPC)**3)*H)/(D**BN(IEQN)))&
       &+(B2(IEQN)*D*D)
  if (VU .gt. VMAX) go to 600
  VMR = VMAX - VU

  ! CALCULATE B.F.I. USING M.C.F. TO DTOPB INCH TOP

  RIC = F0(IEQN)-(F1(IEQN)/D)-(F2(IEQN)/(D**2))-(F3(IEQN)/(D**3))
  if (RIC .lt.0.0) go to 600
  VI = VMR * RIC

  ! CALCULATE B.F.S. USING B.F.I.

  RSI = E0(IEQN)-(E1(IEQN)/D)-(E2(IEQN)/(D**1.177748))-(E3(IEQN)/&
       &(D**2))
  if (RSI .lt.0.0) go to 600
  BBFV = VI * RSI

  600 continue
  BTKFLG = .true.
  return
end
