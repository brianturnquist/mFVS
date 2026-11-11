!=========================!
!   Compute bark ratios   !
!=========================!
! SO $Id$
function BRATIO(IS,D,H)
  implicit none

  !  FUNCTION TO COMPUTE BARK RATIOS, OR CONSTANTS.  THIS ROUTINE IS
  !  VARIANT SPECIFIC AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS
  !  PASSED TO IT.

  !  SPECIES LIST FOR SO VARIANT
  !  1=WP,  2=SP,  3=DF,  4=WF,  5=MH,  6=IC,  7=LP,  8=ES,  9=SH,  10=PP,
  ! 11=JU, 12=GF, 13=AF, 14=SF, 15=NF, 16=WB, 17=WL, 18=RC, 19=WH,  20=PY,
  ! 21=WA, 22=RA, 23=BM, 24=AS, 25=CW, 26=CH, 27=WO, 28=WI, 29=GC,  30=MC,
  ! 31=MB, 32=OS, 33=OH

  ! Common blocks
  include 'PRGPRM.f90'

  ! Variables
  real    :: BARKB(3,MAXSP),BRDAT(MAXSP),H,D,BRATIO,DIB,TEMD
  integer :: IS
  real    :: RDANUW

  data BRDAT /&
       &.964, .851, .867, .915, .934, .950, .969, .956, .000, .890,&
       &.000, .915, .937, .903, .000, .969, .851, .950, .000, .000,&
       &.000, .000, .000, .950, .000, .000, .000, .000, .000, .000,&
       &.000, .867, .000/

  data BARKB/&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &-0.1593,    0.8911,        2.,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.904973,  1.000000,  1.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.933710,  1.000000,  1.000000,&
       &0.933290,  1.000000,  1.000000,&
       &0.075256,  0.949670,  2.000000,&
       &0.075256,  0.949670,  2.000000,&
       &0.083600,  0.947820,  2.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.075256,  0.949670,  2.000000,&
       &0.075256,  0.949670,  2.000000,&
       &-0.30722,   0.95956,        2.,&
       &0.075256,  0.949670,  2.000000,&
       &0.075256,  0.949670,  2.000000,&
       &0.900000,  1.000000,  1.000000,&
       &0.900000,  1.000000,  1.000000,&
       &0.000000,  0.000000,  0.000000,&
       &0.900000,  1.000000,  1.000000/

  !  SPECIES FORM WC VARAINT
  !  NOTE: COEFFICIENTS FOR SPECIES 312, 431, & 815 IN PILLSBURY &
  !  KIRKLEY ARE METRIC. INTERCEPT IS DIVIDED BY 2.54 TO CONVERT THESE
  !  EQUATIONS TO ENGLISH.
  !  BARK COEFS
  !  202,15,122,116,81 FROM WALTERS ET.AL. RES BULL 50
  !  312,431,815  FROM PILLSBURY AND KIRKLEY RES NOTE PNW 414
  !  242,93,108   FROM WYKOFF ET.AL. RES PAPER INT 133

  !  EQUATION TYPES
  !  1  DIB = a * DOB ** b
  !  2  DIB = a + bDOB
  !  3  DIB = a*DOB = a * DOB ** b, (eQ.1), WITH b= 1.0
  !  MODEL TYPE 1

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  RDANUW = H

  select case (IS)

     !  SPECIES FROM WC,CA VARIANT

  case(9,15,19:23,25,26,27,28:31,33)

     if (D .gt.0) then
        if (BARKB(3,IS) .eq.1.) then
           DIB = BARKB(1,IS)*D**BARKB(2,IS)
           BRATIO = DIB/D
        else if (BARKB(3,IS) .eq.2.) then
           DIB = BARKB(1,IS) + BARKB(2,IS)*D
           BRATIO = DIB/D
        end if
     else
        BRATIO = 0.99
     end if

     !  FROM UT(12) JU

  case(11)
     TEMD = D
     if (TEMD.lt.1.)TEMD = 1.
     if (TEMD.gt.19.)TEMD = 19.
     BRATIO = 0.9002 - 0.3089*(1/TEMD)

     !  BARK RATIO IS CONSTANT

  case default
     BRATIO = BRDAT(IS)
  end select

  if (BRATIO .gt.0.99) BRATIO = 0.99
  if (BRATIO .lt.0.80) BRATIO = 0.80

  return
end
