!=========================!
!   Compute bark ratios   !
!=========================!
! BM $Id$
function BRATIO(IS,D,H)
  implicit none

  ! FUNCTION TO COMPUTE BARK RATIOS AS A FUNCTION OF DIAMETER AND SPECIES.
  ! REPLACES ARRAY BKRAT IN BLKDAT.

  ! Common blocks
  include 'PRGPRM.f90'

  !  SPECIES ORDER:
  !   1=WP,  2=WL,  3=DF,  4=GF,  5=MH,  6=WJ,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=WB, 12=LM, 13=PY, 14=YC, 15=AS, 16=CW,
  !  17=OS, 18=OH

  !  SPECIES EXPANSION
  !  WJ USES SO JU (ORIGINALLY FROM UT VARIANT; REALLY PP FROM CR VARIANT)
  !  WB USES SO WB (ORIGINALLY FROM TT VARIANT)
  !  LM USES UT LM
  !  PY USES SO PY (ORIGINALLY FROM WC VARIANT)
  !  YC USES WC YC
  !  AS USES SO AS (ORIGINALLY FROM UT VARIANT)
  !  CW USES SO CW (ORIGINALLY FROM WC VARIANT)
  !  OS USES BM PP BARK COEFFICIENT
  !  OH USES SO OH (ORIGINALLY FROM WC VARIANT)

  ! Variables
  integer :: IS

  real    :: BRATIO,D,DIB,H,RDANUW,TEMD

  real    :: BARK1(MAXSP),BARK2(MAXSP)

  ! Data statements
  data BARK1/&
       &0.859045,  0.859045,  0.903563,  0.904973,  0.903563,&
       &0.,       0.9,       0.9,  0.904973,  0.809427,&
       &0.969,    0.9625,  0.933290,  0.837291,     0.950,&
       &0.075256,  0.809427,    0.9000/

  data BARK2/&
       &1.,        1.,  0.989388,        1.,  0.989388,&
       &0.,        1.,        1.,        1.,  1.016866,&
       &0.,   -0.1141,        1.,        1.,        0.,&
       &0.949670,  1.016866,        1./

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

  select case (IS)

     !  ORIGINAL BM VARIANT SPECIES
     !     BARK COEFFICIENTS:
     !     202,15,122,116,81 FROM WALTERS ET.AL. RES BULL 50
     !     242,93,108   FROM WYKOFF ET.AL. RES PAPER INT 133

  case(1:5,7:10,17)
     if (D .gt.0) then
        DIB = BARK1(IS)*D**BARK2(IS)
        BRATIO = DIB/D
        if (BRATIO .gt.1.0.or. BRATIO .le.0.0) then
           BRATIO = .999
           go to 10
        end if
     else
        BRATIO = 0.999
        go to 10
     end if

     !  PACIFIC YEW (13=PY)
     !  ALASKA CEDAR (14=YC)
     !  OTHER HARDWOODS (18=OH)

  case(13,14,18)
     if (D .gt.0) then
        DIB = BARK1(IS)*D**BARK2(IS)
        BRATIO = DIB/D
     else
        BRATIO = 0.99
     end if

     !  WESTERN JUNIPER (6 = WJ)

  case(6)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     if (TEMD.gt.19.)TEMD = 19.
     BRATIO = 0.9002 - 0.3089*(1/TEMD)

     !  WHITEBARK PINE (11 = WB)
     !  QUAKING ASPEN (15 = AS)

  case(11,15)
     BRATIO = BARK1(IS)

     !  LIMBER PINE (12 = LM)

  case(12)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     BRATIO = BARK1(IS)+BARK2(IS)*(1.0/TEMD)

     !  BLACK COTTONWOOD (16 = CW)

  case(16)
     if (D .gt.0) then
        DIB = BARK1(IS) + BARK2(IS)*D
        BRATIO = DIB/D
     else
        BRATIO = 0.99
     end if
  end select

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  10 continue
  return
end
