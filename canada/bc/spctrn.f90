!========================!
!   canada/bc : spctrn   !
!========================!
! CANADA-BC $Id$
subroutine SPCTRN (SPCIN, ISPC1)
  implicit none

  !     PURPOSE:
  !     TAKES TREELIST FILE UPPER CASE SPECIES CODE AS INPUT AND RETURNS
  !     AN INTEGER INDEX TO THE SPECIES. NORMAL VALUES ARE IN THE 1-15
  !     RANGE; NON-MATCHES ARE ASSIGNED 14. THIS MESSY FORM IS SIMPLEST
  !     GIVEN THE WIDE VARIETY OF INPUT SPECIES CODES.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Variables
  character(len=*) :: SPCIN
  character(len=3) :: SPCOUT
  integer :: ISPC1

  select case (SPCIN(:2))
  case ("PW")
     ISPC1 = 1
     SPCOUT = "PW "
  case ("LW","L ")
     ISPC1 = 2
     SPCOUT = "LW "
  case ("FD","F ")
     ISPC1 = 3
     SPCOUT = "FD "
  case ("BG")
     ISPC1 = 4
     SPCOUT = "BG "
  case ("HW","H ","HM")
     ISPC1 = 5
     SPCOUT = "HW "
  case ("CW","C ")
     ISPC1 = 6
     SPCOUT = "CW "
  case ("PL")
     ISPC1 = 7
     SPCOUT = "PL "
  case ("SE","SX","S ","SW","SB","SS")
     ISPC1 = 8
     SPCOUT = "SE "
  case ("BL","B ")
     ISPC1 = 9
     SPCOUT = "BL "
  case ("PY")
     ISPC1 = 10
     SPCOUT = "PY "
  case ("EP","E ")
     ISPC1 = 11
     SPCOUT = "EP "
  case ("AT")
     ISPC1 = 12
     SPCOUT = "AT "
  case ("AC")
     ISPC1 = 13
     SPCOUT = "AC "
  case ("OC","Y ","YC","YP","BA","J ","JR","LA","LT","P ")
     ISPC1 = 14
     SPCOUT = "OC "
  case ("PJ","PF","PX","PA","T ","TW","X ","XC","Z ","ZC")
     ISPC1 = 14
     SPCOUT = "OC "
  case ("BB","BP","BM","BC","OA","OB","PM","PR","PS","SN")
     ISPC1 = 14
     SPCOUT = "OC "
  case ("OT")
     ISPC1 = 14
     SPCOUT = "OC "
  case ("OH","D ","DR","U ","UP","A ","AX","R ","RA","EA")
     ISPC1 = 15
     SPCOUT = "OH "
  case ("EX","EW","K ","KC","V ","VB","VV","VP","G ","GP")
     ISPC1 = 15
     SPCOUT = "OH "
  case ("M ","MB","MV","Q ","QG","W ","WB","WP","WA","WD")
     ISPC1 = 15
     SPCOUT = "OH "
  case ("WS","WT","XH","ZH","UA","AD","EE","ES","VS","ME")
     ISPC1 = 15
     SPCOUT = "OH "
  case ("MN","MS","OD","OE","OF","OG","QE")
     ISPC1 = 15
     SPCOUT = "OH "
  case default
     ISPC1 = 14
     SPCOUT = "OC "
  end select
  return
end
