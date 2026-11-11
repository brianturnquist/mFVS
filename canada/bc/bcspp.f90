!=======================!
!   canada/bc : bcspp   !
!=======================!
! CANADA-BC $Id$
subroutine BCSPP(VARNAME, SPKODE, INDX)
  implicit none

  !     PURPOSE: TAKES TREELIST FILE SPECIES CODE AS INPUT, AND RETURNS
  !              AN INTEGER INDEX TO THE SPECIES. NORMAL VALUES ARE IN
  !              THE 1-10 RANGE; NON-MATCHES ARE ASSIGNED 11. THIS MESSY
  !              FORM IS SIMPLEST GIVEN THE WIDE VARIETY OF INPUT SPECIES
  !              CODES

  ! Variables
  character(len=7) :: VARNAME
  character(len=4) :: SPKODE
  integer :: I,INDX

  do I = 1,2
     call UPCASE(SPKODE(I:I))
  end do

  select case (VARNAME(1:2))

  case("IB ")
     select case (SPKODE(1:2))
     case ("PW")
        INDX = 1
     case ("LW","L ")
        INDX = 2
     case ("FD","F ")
        INDX = 3
     case ("BG")
        INDX = 4
     case ("HW","H ","HM")
        INDX = 5
     case ("CW","C ")
        INDX = 6
     case ("PL")
        INDX = 7
     case ("SE","SX","S ","SW","SB","SS")
        INDX = 8
     case ("BL","B ")
        INDX = 9
     case ("PY")
        INDX = 10
     case ("EP","E ")
        INDX = 11
     case ("AT")
        INDX = 12
     case ("AC")
        INDX = 13
     case ("OC","Y ","YC","YP","BA","J ","JR","LA","LT","P ")
        INDX = 14
     case ("PJ","PF","PX","PA","T ","TW","X ","XC","Z ","ZC")
        INDX = 14
     case ("BB","BP","BM","BC","OA","OB","PM","PR","PS","SN")
        INDX = 14
     case ("OT")
        INDX = 14
     case ("OH","D ","DR","U ","UP","A ","AX","R ","RA","EA")
        INDX = 15
     case ("EX","EW","K ","KC","V ","VB","VV","VP","G ","GP")
        INDX = 15
     case ("M ","MB","MV","Q ","QG","W ","WB","WP","WA","WD")
        INDX = 15
     case ("WS","WT","XH","ZH","UA","AD","EE","ES","VS","ME")
        INDX = 15
     case ("MN","MS","OD","OE","OF","OG","QE")
        INDX = 15
     case default
        INDX = 14
     end select

     !     PN VARIANT CUSTOMIZED FOR BC SPECIES CODES
     !     THIS IS A DRAFT VERSION... MINOR CODES MAY NEED SOME MORE WORK

  case("PN")
     select case (SPKODE(1:2))
     case ("BA")
        INDX = 1
     case ("BC")
        INDX = 2
     case ("BG")
        INDX = 3
     case ("BL")
        INDX = 4
     case ("BM")
        INDX = 5
     case ("SS")
        INDX = 6
     case ("BP")
        INDX = 7
     case ("YC","LW","L ")
        INDX = 8
     case ("OA")
        INDX = 9
     case ("SE","SX","S ","SW","SB")
        INDX = 10
     case ("PL")
        INDX = 11
     case ("JP")
        INDX = 12
     case ("PS")
        INDX = 13
     case ("PW")
        INDX = 14
     case ("PP")
        INDX = 15
     case ("FD","F ")
        INDX = 15
     case ("OC")
        INDX = 17
     case ("CW","C ")
        INDX = 18
     case ("HW","H ")
        INDX = 19
     case ("HM")
        INDX = 20
     case ("MB")
        INDX = 21
     case ("DR","D ")
        INDX = 22
     case ("RA","R ")
        INDX = 23
     case ("EP","E ")
        INDX = 24
     case ("GC")
        INDX = 25
     case ("AT")
        INDX = 26
     case ("AC")
        INDX = 27
     case ("QG","Q ")
        INDX = 28
     case ("J ")
        INDX = 29
     case ("LA")
        INDX = 30
     case ("PA","PX")
        INDX = 31
     case ("KP")
        INDX = 32
     case ("TW","T ")
        INDX = 33
     case ("GP","G ")
        INDX = 34
     case ("HT")
        INDX = 35
     case ("VB","V ")
        INDX = 36
     case ("W ","WB","WP","WA","WD","WS","WT")
        INDX = 37
     case ("OT")
        INDX = 39

        !C - MINOR SPECES - NEED TO CHECK WHAT THESE ARE...

     case ("Y ","YP","JR","LT","P ")
        INDX = 39
     case ("PJ","PF","X ","XC","Z ","ZC")
        INDX = 39
     case ("BB","OB","PM","PR","SN")
        INDX = 39
     case ("OH","U ","UP","A ","AX","EA")
        INDX = 39
     case ("EX","EW","K ","KC","VV","VP")
        INDX = 39
     case ("M ","MV")
        INDX = 39
     case ("XH","ZH","UA","AD","EE","ES","VS","ME")
        INDX = 39
     case ("MN","MS","OD","OE","OF","OG","QE")
        INDX = 39
     case default
        INDX = 39
     end select

  case("WC")
     select case (SPKODE(1:2))
     case ("BA")
        INDX = 1
     case ("BC")
        INDX = 2
     case ("BG")
        INDX = 3
     case ("BL")
        INDX = 4
     case ("BM")
        INDX = 5
     case ("BP")
        INDX = 7
     case ("YC","LW","L ")
        INDX = 8
     case ("OA")
        INDX = 9
     case ("SE","SX","S ","SW","SB","SS")
        INDX = 10
     case ("PL")
        INDX = 11
     case ("JP")
        INDX = 12
     case ("PS")
        INDX = 13
     case ("PW")
        INDX = 14
     case ("PY")
        INDX = 15
     case ("FD","F ")
        INDX = 15
     case ("OC")
        INDX = 17
     case ("CW","C ")
        INDX = 18
     case ("HW","H ")
        INDX = 19
     case ("HM")
        INDX = 20
     case ("MB")
        INDX = 21
     case ("DR","D ")
        INDX = 22
     case ("RA","R ")
        INDX = 23
     case ("EP","E ")
        INDX = 24
     case ("GC")
        INDX = 25
     case ("AT")
        INDX = 26
     case ("AC")
        INDX = 27
     case ("QG","Q ")
        INDX = 28
     case ("J ")
        INDX = 29
     case ("LA")
        INDX = 30
     case ("PA","PX")
        INDX = 31
     case ("KP")
        INDX = 32
     case ("TW","T ")
        INDX = 33
     case ("GP","G ")
        INDX = 34
     case ("HT")
        INDX = 35
     case ("VB","V ")
        INDX = 36
     case ("W ","WB","WP","WA","WD","WS","WT")
        INDX = 37
     case ("OT")
        INDX = 39

        !C - MINOR SPECES - NEED TO CHECK WHAT THESE ARE...

     case ("Y ","YP","JR","LT","P ")
        INDX = 39
     case ("PJ","PF","X ","XC","Z ","ZC")
        INDX = 39
     case ("BB","OB","PM","PR","SN")
        INDX = 39
     case ("OH","U ","UP","A ","AX","EA")
        INDX = 39
     case ("EX","EW","K ","KC","VV","VP")
        INDX = 39
     case ("M ","MV")
        INDX = 39
     case ("XH","ZH","UA","AD","EE","ES","VS","ME")
        INDX = 39
     case ("MN","MS","OD","OE","OF","OG","QE")
        INDX = 39
     case default
        INDX = 39
     end select

  case("AK")
     select case (SPKODE(1:2))
     case ("SE","SX","S ","SW","SB")
        INDX = 1
     case ("CW","C ")
        INDX = 2
     case ("BA")
        INDX = 3
     case ("HM")
        INDX = 4
     case ("HW","H ")
        INDX = 5
     case ("Y ","YC")
        INDX = 6
     case ("PL")
        INDX = 7
     case ("SS")
        INDX = 8
     case ("BL","B ")
        INDX = 9
     case ("D ","DR")
        INDX = 10
     case ("AC")
        INDX = 11
     case ("PW","LW","L ","FD","F ","BG","PY")
        INDX = 13
     case ("OC","YP","J ","JR","LA","LT","P ")
        INDX = 13
     case ("PJ","PF","PX","PA","T ","TW","X ","XC","Z ","ZC")
        INDX = 13
     case ("BB","BP","BM","BC","OA","OB","PM","PR","PS","SN")
        INDX = 13
     case ("OT")
        INDX = 13
     case ("OH","EP","E ","U ","UP","A ","AX","R ","RA","EA")
        INDX = 12
     case ("EX","EW","K ","KC","V ","VB","VV","VP","G ","GP")
        INDX = 12
     case ("M ","MB","MV","Q ","QG","W ","WB","WP","WA","WD")
        INDX = 12
     case ("WS","WT","XH","ZH","UA","AD","EE","ES","VS","ME")
        INDX = 12
     case ("MN","MS","OD","OE","OF","OG","QE","AT")
        INDX = 12
     case default
        INDX = 13
     end select

  case default
     INDX = 0
  end select

  return
end
