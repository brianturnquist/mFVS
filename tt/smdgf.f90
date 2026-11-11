!================!
!   tt : smdgf   !
!================!
! TT $Id$
subroutine SMDGF(ISPC,H,CR,RD,SDIAM)
  implicit none

  !  THIS SUBROUTINE COMPUTES THE DIAMETER FOR SMALL TREES.  PREDICTIONS
  !  ARE BASED ON SPECIES, HEIGHT, CROWN RATIO, AND POINT CCF;
  !  CALLED FROM REGENT.

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  integer :: ISPC
  real    :: SDHTCR(MAXSP),SDHPCF(MAXSP),SDCR(MAXSP),SDHL4(MAXSP)
  real    :: SDIAM,RD,CR,H,HLESS4,DLESS3

  ! SPECIES ORDER FOR TETONS VARIANT:

  !  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
  ! 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH

  ! VARIANT EXPANSION:
  ! BS USES ES EQUATIONS FROM TT
  ! PM USES PI (COMMON PINYON) EQUATIONS FROM UT
  ! PP USES PP EQUATIONS FROM CI
  ! UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
  ! BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
  ! MM USES MM EQUATIONS FROM IE
  ! NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
  ! MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
  ! OS USES OT (OTHER SP.) EQUATIONS FROM TT

  !  SDHTCR COEFFICIENTS WERE SCALED TO MAINTAIN CONSISTENCY WITH
  !  **SMHTGF**;  CR IS SCALED 0-100 (WRW--3/26/91).

  data SDHTCR /&
       &0.000231,  0.000231,  -0.28654,        0.,   0.04125,&
       &-0.41227,  -0.41227,   0.04125,  -0.15906,        0.,&
       &0.,        0.,        0.,        0.,        0.,&
       &0.,  0.000231,        0./
  data SDHPCF /&
       &-0.00005,  -0.00005,   0.13469,        0.,   0.17486,&
       &0.16944,   0.16944,   0.17486,   0.15323,        0.,&
       &0.,        0.,        0.,        0.,        0.,&
       &0.,  -0.00005,        0./

  !  CROWN RATIO COEFFICIENTS WERE SCALED TO MAINTAIN CONSISTENCY
  !  WITH **SMHTGF**; CR IS SCALED 0-100 (WRW--3/26/91).

  data SDCR /&
       &0.001711,  0.001711,  0.002736,        0., -0.002371,&
       &0.003191,  0.003191, -0.002371,       0.0,        0.,&
       &0.,        0.,        0.,        0.,        0.,&
       &0.,  0.001711,        0./
  data SDHL4 /&
       &0.17023,   0.17023,   0.00036,        0.,  -0.00070,&
       &-0.00220,  -0.00220,  -0.00070,       0.0,        0.,&
       &0.,        0.,        0.,        0.,        0.,&
       &0.,   0.17023,        0./

  select case (ISPC)

  case(3,5:9)

     ! FOR DF,BS,AS,LP,ES,AF USE ALTERNATE DIAMETER MODEL.
     ! SDHTCR USED FOR CONSTANT COEFFICIENT
     ! SDHPCF USED FOR HEIGHT COEFFICIENT
     ! SDCR USED FOR CROWN RATIO COEFFICIENT
     ! SDHL4 USED FOR RELATIVE DENSITY COEFFICIENT (I.E. PCCF)

     SDIAM = SDHTCR(ISPC) + SDHPCF(ISPC)*H + SDCR(ISPC)*CR&
          &+ SDHL4(ISPC)*RD

     ! ALL OTHER ORIGINAL TT VARIANT SPECIES (WB, LM, OS)

  case default
     HLESS4 = H - 4.5
     DLESS3 = SDHTCR(ISPC)*HLESS4*CR&
          &+ SDHPCF(ISPC) * HLESS4 * RD&
          &+ SDCR(ISPC) * CR&
          &+ SDHL4(ISPC) * HLESS4
     SDIAM = DLESS3 + 0.3

  end select

  return
end
