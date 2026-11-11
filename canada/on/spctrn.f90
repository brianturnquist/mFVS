!========================!
!   canada/on : spctrn   !
!========================!
! CANADA-ON $Id$
subroutine SPCTRN (SPCIN, ISPC1)
  implicit none

  !     PURPOSE:
  !     TAKES TREELIST FILE UPPER CASE SPECIES CODE AS INPUT AND RETURNS
  !     AN INTEGER INDEX TO THE SPECIES. NORMAL VALUES ARE IN THE 1-72
  !     RANGE; NON-MATCHES ARE ASSIGNED 49.

  !     THIS IS DUMMY CODE, SINCE THE SPECIES SHOWN HERE ARE ALL IDENTICAL
  !     TO THOSE DECLARED AND INITIALIZED IN BLKDAT; THIS COULD BE USED
  !     TO EXPAND THE ALPHA CODES THAT ARE RECOGNIZED

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Variables
  character(len=*) :: SPCIN
  integer :: ISPC1

  select case (SPCIN(:2))
  case ("PJ")
     ISPC1 = 1
  case ("PS")
     ISPC1 = 2
  case ("RN")
     ISPC1 = 3
  case ("RP")
     ISPC1 = 4
  case ("PW")
     ISPC1 = 5
  case ("SW")
     ISPC1 = 6
  case ("SN")
     ISPC1 = 7
  case ("BF")
     ISPC1 = 8

  case ("SB")
     ISPC1 = 9
  case ("TA")
     ISPC1 = 10
  case ("CE")
     ISPC1 = 11
  case ("HE")
     ISPC1 = 12
  case ("SO")
     ISPC1 = 13
  case ("CR")
     ISPC1 = 14
  case ("AB")
     ISPC1 = 15
  case ("AR")
     ISPC1 = 16
  case ("CW")
     ISPC1 = 17
  case ("MV")
     ISPC1 = 18

  case ("MR")
     ISPC1 = 19
  case ("CB")
     ISPC1 = 20
  case ("EW")
     ISPC1 = 21
  case ("ES")
     ISPC1 = 22
  case ("ER")
     ISPC1 = 23
  case ("BY")
     ISPC1 = 24
  case ("BD")
     ISPC1 = 25
  case ("MH")
     ISPC1 = 26
  case ("MB")
     ISPC1 = 27
  case ("BE")
     ISPC1 = 28

  case ("AW")
     ISPC1 = 29
  case ("OW")
     ISPC1 = 30
  case ("OP")
     ISPC1 = 31
  case ("OB")
     ISPC1 = 32
  case ("OC")
     ISPC1 = 33
  case ("OR")
     ISPC1 = 34
  case ("BO")
     ISPC1 = 35
  case ("PN")
     ISPC1 = 36
  case ("HB")
     ISPC1 = 37
  case ("HP")
     ISPC1 = 38

  case ("HU")
     ISPC1 = 39
  case ("PG")
     ISPC1 = 40
  case ("PT")
     ISPC1 = 41
  case ("PB")
     ISPC1 = 42
  case ("BW")
     ISPC1 = 43
  case ("CH")
     ISPC1 = 44
  case ("BT")
     ISPC1 = 45
  case ("WB")
     ISPC1 = 46
  case ("IW")
     ISPC1 = 47
  case ("LB")
     ISPC1 = 48

  case ("NC")
     ISPC1 = 49
  case ("MM")
     ISPC1 = 50
  case ("MS")
     ISPC1 = 51
  case ("MT")
     ISPC1 = 52
  case ("BB")
     ISPC1 = 53
  case ("CA")
     ISPC1 = 54
  case ("BH")
     ISPC1 = 55
  case ("DF")
     ISPC1 = 56
  case ("HT")
     ISPC1 = 57
  case ("ML")
     ISPC1 = 58

  case ("GB")
     ISPC1 = 59
  case ("SY")
     ISPC1 = 60
  case ("CP")
     ISPC1 = 61
  case ("CC")
     ISPC1 = 62
  case ("PL")
     ISPC1 = 63
  case ("WI")
     ISPC1 = 64
     !       CASE ("WI")   ! 3 willow species... so
     !          ISPC1 = 65 ! 65-66 are not really
     !       CASE ("WI")   ! used at all
     !          ISPC1 = 66
  case ("SS")
     ISPC1 = 67
  case ("AM")
     ISPC1 = 68

  case ("JP")
     ISPC1 = 69
  case ("WP")
     ISPC1 = 70
  case ("SP")
     ISPC1 = 71
  case ("BP")
     ISPC1 = 72

  case default ! non-commercial hardwood
     ISPC1 = 49
  end select

  return
end
