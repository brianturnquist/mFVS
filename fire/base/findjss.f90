!==================================!
!   Find structural stage column   !
!==================================!
! FIRE-BASE $Id$
!  Find column in CWHR fuel model table, given structural stage/density string
integer function FINDJSS(STR)

  character(len=3) :: STR
  integer :: II

  II = 0
  select case (STR(1:1))
  case("X")
     II = 1 ! Initiating stand: < 150 tpa & <10% CC
  case("1")
     II = 1
  case("2")
     select case (STR(3:3))
     case("S")
        II = 2
     case("P")
        II = 3
     case("M")
        II = 4
     case("D")
        II = 5
     end select
  case("3")
     select case (STR(3:3))
     case("S")
        II = 6
     case("P")
        II = 7
     case("M")
        II = 8
     case("D")
        II = 9
     end select
  case("4")
     select case (STR(3:3))
     case("S")
        II = 10
     case("P")
        II = 11
     case("M")
        II = 12
     case("D")
        II = 13
     end select
  case("5")
     select case (STR(3:3))
     case("S")
        II = 14
     case("P")
        II = 15
     case("M")
        II = 16
     case("D")
        II = 17
     end select
  case("6")
     II = 18
  end select

  FINDJSS = II

  return
end function FINDJSS
