!====================!
!   vstrp : essprt   !
!====================!
! VSTRP $Id$
subroutine ESSPRT(VAR,ISPC,PREM,DSTMP)
  implicit none

  !  SUBROUTINE CONTAINING ENTRY POINTS TO HANDLE COMPUTATIONS FOR
  !  VARIOUS STUMP SPROUTING FUNCTIONS WHICH VARY BY VARIANT, SPECIES,
  !  AND PARENT-TREE DIAMETER.

  !  VARIANTS USING THE STRP VERSION: BM, CA, CR, CS, EC, LS, NC, NE,
  !  OC, OP, PN, SN, SO, TT, UT, WC, WS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'SNCOM.f90'

  ! Variables
  character(len=2) :: VAR
  integer :: IAG,INDXAS,ISHAG,ISPC,NMSPRC
  real    :: ASTPAR,ASBAR,HTSPRT,PREM,RSHAG,SI,SPA,TREES,DSTMP

  !  VARIANT AND SPECIES SPECIFIC RULES FOR DETERMINING TPA A SPROUT
  !  RECORD WILL REPRESENT. ASPEN IS HANDLED SEPERATELY IN ENTRY ASSPTN

  select case (VAR)

     !  SN: SOUTHERN
     !     BASED ON KEYSER AND LOFTIS, SHORT-TERM STUMP SPROUT DYNAMICS OF
     !     24 UPLAND HARDWOOD TREE SPECIES FOLLOWING REGENERATION HARVESTS
     !     IN THE SOUTHERN APPALACHIAN MOUNTAINS, USA

     !  ALL OTHER VARIANTS:
     !     ESTIMATES OF SPROUTS/AC REPRESENTED BY EACH SPROUT RECORD COME
     !     FROM VARIOUS LITERATURE SOURCES

     ! BM Variant

  case('BM')
     select case(ISPC)
     case(13)
        PREM = PREM * 0.40
     case(16)
        PREM = PREM * 0.90
     case default
        PREM = PREM * 1.
     end select

     ! CA and OC Variants

  case('CA','OC')
     select case(ISPC)
     case(24)
        PREM = PREM * 0.40
     case(26,27,33)
        PREM = PREM * 0.50
     case(28)
        if (DSTMP.lt.27.1) then
           PREM = PREM * ((70.7857-2.6071 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(29:32,34,37:39,42,45,46,48)
        PREM = PREM * 0.90
     case(35,40,47)
        PREM = PREM * 0.80
     case(36)
        if (DSTMP.lt.25.9) then
           PREM = PREM * ((99.9999-3.8462 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(41)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.8
        else
           PREM = PREM * 0.5
        end if
     case(43)
        PREM = PREM * 0.70
     case(50)
        if (DSTMP.lt.216.7) then
           PREM = PREM * ((93.2669-0.4303 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case default
        PREM = PREM * 1.
     end select

     ! CR Variant

  case('CR')
     select case(ISPC)
     case(24,28)
        PREM = PREM * 0.70
     case(21,23,25,26)
        PREM = PREM * 0.80
     case(22,27)
        PREM = PREM * 0.90
     case(29)
        if (DSTMP.lt.41.4) then
           PREM = PREM * ((97.4603-2.3492 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(36)
        PREM = PREM * 0.30
     case default
        PREM = PREM * 1.
     end select

     ! CS Variant

  case('CS')
     select case(ISPC)
     case(3)
        PREM = PREM * 0.42
     case(8)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.8
        else
           PREM = PREM * 0.5
        end if
     case(9)
        PREM = PREM * 0.3
     case(10,11,12,13,77,83,84,96)
        PREM = PREM * 0.9
     case(14,15,17,19,20,21,22,23)
        if (DSTMP.lt.24.0) then
           PREM = PREM * 0.95
        else
           PREM = PREM * 0.60
        end if
     case(16,18)
        if (DSTMP.lt.24.0) then
           PREM = PREM * 0.75
        else
           PREM = PREM * 0.50
        end if
     case(24)
        PREM = PREM * 0.50
     case(25,26,27,31,32,44,45,46,80)
        if (DSTMP.lt.12.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(28)
        PREM = PREM * 0.40
     case(29)
        if (DSTMP.lt.12.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.60
        end if
     case(30)
        if (DSTMP.lt.15.0) then
           PREM = PREM * 0.60
        else
           PREM = PREM * 0.30
        end if
     case(33,36,37,38,39,40,53,64,72,73,79,81,82,86,89,90,92)
        PREM = PREM * 0.70
     case(34,42,55,60,69,75,87,91,93,94,95)
        PREM = PREM * 0.80
     case(35)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.40
        else
           PREM = PREM * 0.20
        end if
     case(41,74)
        if (DSTMP.lt.25.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(43)
        PREM = PREM * ((89.191-2.611 * DSTMP)/100.)
     case(47)
        PREM = PREM * (EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) * 2.54)))/&
             &(1. + EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) * 2.54)))))
     case(48,51,61,62)
        PREM = PREM * ((57.3-0.0032 * DSTMP**3)/100.)
     case(49,65,66)
        if (DSTMP.lt.10.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(50)
        PREM = PREM * (EXP(6.0065 + (-0.0777 *&
             &((DSTMP/0.7801) *2.54)))/(1. + EXP(6.0065 +&
             &(-0.0777 * ((DSTMP/0.7801) * 2.54)))))
     case(52)
        PREM = PREM * 1. / (1. + EXP(-(2.3656 +&
             &(-0.2781 * (DSTMP/0.7801)))))
     case(54)
        PREM = 0.9 * (PREM * (EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) *2.54)))/&
             &(1. + EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) * 2.54))))))
     case(56,59)
        PREM = PREM * (EXP(6.4205 + (-0.1097 *&
             &(((DSTMP/0.8188)-0.23065) *2.54)))/(1. + EXP(6.4205 +&
             &(-0.1097 * (((DSTMP/0.8188)-0.23065) * 2.54)))))
     case(57,58)
        PREM = PREM * (1. / (1. + EXP(-(-2.8058 +&
             &22.6839 * (1./((DSTMP/0.7788)-0.4403))))))
     case(63,70)
        PREM = PREM * 0.40
     case(67)
        if (DSTMP.lt.10.0) then
           PREM = PREM * 0.60
        else
           PREM = PREM * 0.30
        end if
     case(88)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.70
        else
           PREM = PREM * 0.90
        end if
     case default
        PREM = PREM * 1.
     end select

     ! EC Variant

  case('EC')
     select case(ISPC)
     case(22)
        if (DSTMP.lt.25.9) then
           PREM = PREM * ((99.9999-3.8462 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(23,29)
        PREM = PREM * 0.70
     case(20,21,24,25,27,28,30)
        PREM = PREM * 0.90
     case default
        PREM = PREM * 1.
     end select

     ! LS and ON Variants

  case('LS', 'ON')
     select case(ISPC)
     case(15,16,18,19,20,29)
        if (DSTMP.lt.12.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(17)
        PREM = PREM * 0.40
     case(21,22,23,33,43,52,53,57,60,61,63,68)
        PREM = PREM * 0.70
     case(24,45)
        PREM = PREM * 0.30
     case(25,32,36,40,47,54,62,67)
        PREM = PREM * 0.80
     case(26,27)
        if (DSTMP.lt.34.1) then
           PREM = PREM * ((89.191-2.611 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(28,58)
        PREM = PREM * 0.50
     case(30)
        PREM = PREM * (EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) * 2.54)))/&
             &(1. + EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) * 2.54)))))
     case(31)
        PREM = 0.9 * (PREM * (EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) *2.54)))/&
             &(1. + EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) * 2.54))))))
     case(34)
        PREM = PREM * ((57.3-0.0032 * DSTMP**3)/100.)
     case(35)
        PREM = PREM * (EXP(6.0065 + (-0.0777 *&
             &((DSTMP/0.7801) *2.54)))/(1 + EXP(6.0065 +&
             &(-0.0777 * ((DSTMP/0.7801) * 2.54)))))
     case(37,39)
        if (DSTMP.lt.24.0) then
           PREM = PREM * 0.95
        else
           PREM = PREM * 0.60
        end if
     case(38)
        if (DSTMP.lt.24.0) then
           PREM = PREM * 0.75
        else
           PREM = PREM * 0.50
        end if
     case(42)
        if (DSTMP.lt.25.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(46)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(48,59,64,65,66)
        PREM = PREM * 0.90
     case(50)
        if (DSTMP.lt.15.0) then
           PREM = PREM * 0.60
        else
           PREM = PREM * 0.30
        end if
     case(55)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.40
        else
           PREM = PREM * 0.20
        end if
     case(56)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.70
        else
           PREM = PREM * 0.90
        end if
     case default
        PREM = PREM * 1.
     end select

     ! NC Variant

  case('NC')
     select case(ISPC)
     case(5,7,8)
        PREM = PREM * 0.90
     case(12)
        if (DSTMP.lt.216.7) then
           PREM = PREM * ((93.2669-0.4303 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case default
        PREM = PREM * 1.
     end select

     ! NE Variant

  case('NE')
     select case(ISPC)
     case(26,29,41,42,43,44,45,46,54)
        if (DSTMP.lt.12.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(27,28)
        if (DSTMP.lt.34.1) then
           PREM = PREM * ((89.191-2.611 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(30,78,100)
        PREM = PREM * 0.3
     case(31,32,34,47,48,57,62,76,88,95,96,97,102,103,105,107,108)
        PREM = PREM * 0.70
     case(33,84,85,86,90,91)
        PREM = PREM * 0.90
     case(35,38,39)
        if (DSTMP.lt.24.0) then
           PREM = PREM * 0.95
        else
           PREM = PREM * 0.60
        end if
     case(36,37)
        if (DSTMP.lt.24.0) then
           PREM = PREM * 0.75
        else
           PREM = PREM * 0.50
        end if
     case(40)
        PREM = PREM * 0.93
     case(50)
        if (DSTMP.lt.25.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(51)
        PREM = PREM * 0.40
     case(52,56,63,80,82,87,92,93,94,101,106)
        PREM = PREM * 0.80
     case(53)
        PREM = PREM * 0.40
     case(55)
        PREM = PREM * (EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) * 2.54)))/&
             &(1. + EXP(1.6134 + (-0.0184 *&
             &(((DSTMP/0.7788)-0.4403) * 2.54)))))
     case(58)
        PREM = PREM * (1. / (1. + EXP(-(-2.8058 +&
             &22.6839 * (1./((DSTMP/0.7788)-0.4403))))))
     case(59,60,61,67,70)
        PREM = PREM * ((57.3-0.0032 * DSTMP**3)/100.)
     case(64,66)
        PREM = PREM * (EXP(6.4205 + (-0.1097 *&
             &(((DSTMP/0.8188)-0.23065) *2.54)))/(1. + EXP(6.4205 +&
             &(-0.1097 * (((DSTMP/0.8188)-0.23065) * 2.54)))))
     case(68,89)
        if (DSTMP.lt.10.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(69)
        PREM = PREM * (EXP(6.0065 + (-0.0777 *&
             &((DSTMP/0.7801) *2.54)))/(1. + EXP(6.0065 +&
             &(-0.0777 * ((DSTMP/0.7801) * 2.54)))))
     case(72,73,75)
        PREM = PREM * 0.40
     case(74,77,83)
        PREM = PREM * 0.50
     case(79)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(81)
        PREM = PREM * 1. / (1. + EXP(-(2.7386 + (-0.1076 * DSTMP))))
     case(99)
        if (DSTMP.lt.15.0) then
           PREM = PREM * 0.60
        else
           PREM = PREM * 0.30
        end if
     case(104)
        if (DSTMP.lt.8.0) then
           PREM = PREM * 0.70
        else
           PREM = PREM * 0.90
        end if
     case default
        PREM = PREM * 1.
     end select

     ! OP Variant

  case('OP')
     select case(ISPC)
     case(17)
        if (DSTMP.lt.216.7) then
           PREM = PREM * ((93.2669-0.4303 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(21,23,24,25,27,28,34,36,37)
        PREM = PREM * 0.90
     case(22)
        if (DSTMP.lt.25.9) then
           PREM = PREM * ((99.9999-3.8462 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(35)
        PREM = PREM * 0.70
     case(33)
        PREM = PREM * 0.40
     case default
        PREM = PREM * 1.
     end select

     ! PN and WC Variants

  case('PN','WC')
     select case(ISPC)
     case(17)
        if (DSTMP.lt.216.7) then
           PREM = PREM * ((93.2669-0.4303 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(21,23,25,27,28,34,36,37)
        PREM = PREM * 0.90
     case(22)
        if (DSTMP.lt.25.9) then
           PREM = PREM * ((99.9999-3.8462 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(24,35)
        PREM = PREM * 0.70
     case(33)
        PREM = PREM * 0.40
     case default
        PREM = PREM * 1.
     end select

     ! SN Variant

  case('SN')
     select case(ISPC)
     case(5)
        PREM = PREM * 0.42
     case(18,19,26,30:32,41,51,52,56,82)
        PREM = PREM * 0.94
     case(20)
        PREM = PREM * 1. / (1. + EXP(-(4.1975 + (-0.1821 * DSTMP))))
     case(22)
        PREM = PREM * 0.73
     case(23)
        PREM = PREM * 0.96
     case(24,25)
        PREM = PREM * 1. / (1. + EXP(-(3.3670 + (-0.5159 * DSTMP))))
     case(27)
        PREM = PREM * 0.95
     case(33)
        PREM = PREM * 0.93
     case(45)
        PREM = PREM * 0.79
     case(46)
        PREM = PREM * 0.95
     case(47)
        PREM = PREM * 0.69
     case(54)
        PREM = PREM * 0.72
     case(57)
        PREM = PREM * 0.97
     case(63)
        PREM = PREM * 1. / (1. + EXP(-(2.4608 + (-0.3093 * DSTMP))))
     case(64)
        if (ISEFOR.eq.809.or. ISEFOR.eq.810.or. ISEFOR.eq.905.or.&
             &ISEFOR.eq.908) then
           PREM = PREM * ((57.3-0.0032 * DSTMP**3)/100.)
        else
           PREM = PREM * 1./(1. + EXP(-(3.8897 + (-0.2260 * DSTMP))))
        end if
     case(66)
        if (ISEFOR.eq.809.or. ISEFOR.eq.810.or. ISEFOR.eq.905.or.&
             &ISEFOR.eq.908) then
           PREM = PREM * ((57.3-0.0032 * DSTMP**3)/100.)
        else
           PREM = PREM * 1./(1. + EXP(-(2.7386 + (-0.1076 * DSTMP))))
        end if
     case(70)
        if (ISEFOR.eq.809.or. ISEFOR.eq.810.or. ISEFOR.eq.905.or.&
             &ISEFOR.eq.908) then
           PREM = PREM * 1. / (1. + EXP(-(2.3656 +&
                &(-0.2781 * (DSTMP/0.7801)))))
        else
           PREM = PREM * 1./(1. + EXP(-(2.7386 + (-0.1076 * DSTMP))))
        end if
     case(74)
        PREM = PREM * 0.78
     case(75)
        if (ISEFOR.eq.809.or. ISEFOR.eq.810.or. ISEFOR.eq.905.or.&
             &ISEFOR.eq.908) then
           PREM = PREM * ((57.3-0.0032 * DSTMP**3)/100.)
        else
           PREM = PREM * 1./(1. + EXP(-(3.2586 + (-0.1120 * DSTMP))))
        end if
     case(77)
        if (ISEFOR.eq.809.or. ISEFOR.eq.810.or. ISEFOR.eq.905.or.&
             &ISEFOR.eq.908) then
           PREM = PREM * (1. / (1. + EXP(-(-2.8058 +&
                &22.6839 * (1./((DSTMP/0.7788)-0.4403))))))
        else
           PREM = PREM * 1./(1. + EXP(-(2.7386 + (-0.1076 * DSTMP))))
        end if
     case(78)
        PREM = PREM * 1. / (1. + EXP(-(3.1070 + (-0.2128 * DSTMP))))
     case(80)
        PREM = PREM * 0.86
     case(83)
        PREM = PREM * 0.99
     case default
        PREM = PREM * 1. / (1. + EXP(-(2.7386 + (-0.1076 * DSTMP))))
     end select

     ! SO Variant

  case('SO')
     select case(ISPC)
     case(20)
        PREM = PREM * 0.40
     case(21,22)
        if (DSTMP.lt.25.9) then
           PREM = PREM * ((99.9999-3.8462 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(31)
        PREM = PREM * 0.70
     case(23,25:29)
        PREM = PREM * 0.90
     case default
        PREM = PREM * 1.
     end select

     ! TT Variant

  case('TT')
     select case(ISPC)
     case(13,14)
        PREM = PREM * 0.70
     case(15)
        PREM = PREM * 0.90
     case default
        PREM = PREM * 1.
     end select

     ! UT Variant

  case('UT')
     select case(ISPC)
     case(22)
        PREM = PREM * 0.50
     case(21)
        PREM = PREM * 0.70
     case(13,18,19)
        PREM = PREM * 0.80
     case default
        PREM = PREM * 1.
     end select

     ! WS Variant

  case('WS')
     select case(ISPC)
     case(23)
        if (DSTMP.lt.216.7) then
           PREM = PREM * ((93.2669-0.4303 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(28,29,33)
        PREM = PREM * 0.50
     case(30)
        if (DSTMP.lt.27.1) then
           PREM = PREM * ((70.7857-2.6071 * DSTMP)/100.)
        else
           PREM = 0.
        end if
     case(31,32,34,35,37:40)
        PREM = PREM * 0.90
     case default
        PREM = PREM * 1.
     end select

     ! XX Variant including AC. All species.

  case default
     PREM = PREM * 1.

  end select
  return

  entry ESASID(VAR,INDXAS)

  !  DETERMINE IF QUAKING ASPEN IS IN THIS VARIANT; IF SO, RETURN THE
  !  FVS SPECIES INDEX FOR ASPEN IN THIS VARIANT; OTHERWISE RETURN 9999.

  select case (VAR)

     ! BC Variant

  case('BC')
     INDXAS = 12

     ! BM Variant

  case('BM')
     INDXAS = 15

     ! CA Variant

  case('CA')
     INDXAS = 44

     ! CR Variant

  case('CR')
     INDXAS = 20

     ! CS Variant

  case('CS')
     INDXAS = 76

     ! EC Variant

  case('EC')
     INDXAS = 26

     ! LS and ON Variants

  case('LS', 'ON')
     INDXAS = 41

     ! NE Variant

  case('NE')
     INDXAS = 49

     ! OC Variant

  case('OC')
     INDXAS = 44

     ! OP Variant

  case('OP')
     INDXAS = 26

     ! PN and WC Variants

  case('PN', 'WC')
     INDXAS = 26

     ! SO Variant

  case('SO')
     INDXAS = 24

     ! TT and UT Variants

  case('TT','UT')
     INDXAS = 6

     ! WS Variant

  case('WS')
     INDXAS = 36

     ! XX Variant

  case default
     INDXAS = 9999

  end select
  return

  entry NSPREC (VAR,ISPC,NMSPRC,DSTMP)

  !  DETERMINE NUMBER OF SPROUT RECORDS TO CREATE

  !  SET TO 1 FOR: BM - PACIFIC YEW (13)
  !                CA - PACIFIC YEW (24) AND CALIFORNIA NUTMEG (47)
  !                PN - PACIFIC YEW (33)
  !                SO - PACIFIC YEW (20)
  !                WC - PACIFIC YEW (33)
  !                SN - DEFAULT AND AB & BT & BK & SS < 5",
  !                     AND SP < 7.0"
  !                CS - DEFAULT AND AB < 4", BA & PA & UA & RM & SV &
  !                     YP & BW & SM & RO & CB & QI & SS & BP & BT &
  !                     BK & DW & MV & SD < 5", SP < 7.0", BN & OV < 8",
  !                     AND EC < 25"
  !  0.2*DSTMP     CS - 5" <= BA & PA & UA & SV & SM & RO & CB &
  !                     QI & BP & BK & DW & SD <= 10"
  ! -1.0+0.4*DSTMP CS - 5" <= RM & YP & BW & SS & BT & MV <= 10"
  !  SET TO 2 FOR: CS - BA & PA & UA & SV & SM & RO & CB & QI &
  !                     BP & BK & DW & SD < 10"
  !  SET TO 3 FOR: CS - RM & YP & BW & SS & BT & MV < 10"
  ! -1.0+0.4*DSTMP SN - 5" <= AB & BT & BK & SS <= 10"
  !  SET TO 3 FOR: SN - AB & BT & BK & SS < 10"

  !  SIMILAR LOGIC APPLIES TO SPECIES IN OTHER VARIANTS

  !  OTHERWISE SET TO 2

  select case (VAR)

     ! BM Variant

  case('BM')
     select case (ISPC)
     case(15)
        NMSPRC = 2
     case(16)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! CA and OC Variants

  case('CA','OC')
     select case (ISPC)
     case(26:33,35,39,40)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(34,37,38,42,45,48,50)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case(44)
        NMSPRC = 2
     case default
        NMSPRC = 1
     end select

     ! CR Variant

  case('CR')
     select case (ISPC)
     case(15)
        NMSPRC = 2
     case(23:27)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(21,22)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! CS Variant

  case('CS')
     select case (ISPC)
     case(3)
        if (DSTMP.lt.7.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(9,63,70)
        if (DSTMP.lt.8.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(24)
        if (DSTMP.lt.4.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(25,26,27,31,43,48,61,62,77,88,96)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(28)
        if (DSTMP.lt.25.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(29,41,42,69,74,75,93)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case(76)
        NMSPRC = 2
     case default
        NMSPRC = 1
     end select

     ! EC Variant

  case('EC')
     select case (ISPC)
     case(20,21,24,27)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case(25,28)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(26)
        NMSPRC = 2
     case default
        NMSPRC = 1
     end select

     ! LS Variant

  case('LS')
     select case (ISPC)
     case(17)
        if (DSTMP.lt.25.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(28)
        if (DSTMP.lt.4.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(41)
        NMSPRC = 2
     case(45)
        if (DSTMP.lt.8.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(15,18,19,26,27,34,48,56,68)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(25,40,42,62,67)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! NC Variant

  case('NC')
     select case (ISPC)
     case(7)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(5,8,12)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! NE Variant

  case('NE')
     select case (ISPC)
     case(49)
        NMSPRC = 2
     case(51)
        if (DSTMP.lt.25.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(53)
        if (DSTMP.lt.12.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(72,73,75,78)
        if (DSTMP.lt.8.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(26,27,28,29,43,45,59,60,61,67,68,70,86,90,102,104)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(40,46,50,52,82,87,92,93,94,101)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! OP Variant

  case('OP')
     select case (ISPC)
     case(26)
        NMSPRC = 2
     case(28,34)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(17,21,23,24,25,27,36)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! PN and WC Variants

  case('PN','WC')
     select case (ISPC)
     case(26)
        NMSPRC = 2
     case(28,34)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(17,21,23,25,27,36)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! SN Variant

  case('SN')
     select case (ISPC)
     case(5)
        if (DSTMP.lt.7.0) then
           NMSPRC = 1
        else
           NMSPRC = 0
        end if
     case(33,61,80,82)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! SO Variant

  case('SO')
     select case (ISPC)
     case(24)
        NMSPRC = 2
     case(27)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(23,25,26,29)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! TT Variant

  case('TT')
     select case (ISPC)
     case(6)
        NMSPRC = 2
     case(15)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! UT Variant

  case('UT')
     select case (ISPC)
     case(6)
        NMSPRC = 2
     case(13,22)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(18,19)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     ! WS Variant

  case('WS')
     select case (ISPC)
     case(36)
        NMSPRC = 2
     case(28:33,39)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(0.2 * DSTMP)
        else
           NMSPRC = 2
        end if
     case(23,34,35,37,38,40)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.le.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

     !  ALL OTHER VARIANTS & SPECIES

  case default
     NMSPRC = 2

  end select

  return

  entry ASSPTN (ISHAG,ASBAR,ASTPAR,PREM,TREES)

  !  DETERMINE NUMBER OF QUAKING ASPEN SPROUTS PER ACRE (SPA)
  !  BASED ON POLYNOMIAL FORMULA FROM CROUCH DATA SET, MODIFIED
  !  BY PROPORTION OF ASPEN BA CUT BY CHAD KEYSER (FMSC).

  RSHAG = FLOAT(ISHAG)
  SPA = 40100.45 - 3574.02*RSHAG**2.0 + 554.02*RSHAG**3.0&
       &- 3.5208*RSHAG**5.0 + 0.011797*RSHAG**7.0
  if (SPA .lt.2608.)SPA = 2608.
  if (SPA .gt.30125.)SPA = 30125.
  SPA = SPA * ASBAR/198.
  TREES = (PREM/(ASTPAR*2.))*SPA

  return

  entry SPRTHT (VAR,ISPC,SI,IAG,HTSPRT)

  !  DETERMINE SPROUT HEIGHT BY VARIANT AND SPECIES.

  !  EQUATION IS HT = (0.1 + SI/BASEAGE)*AGE

  !  DEFAULT EQUATION, HTSPRT = 0.5 + 0.5*IAG, IS THE ORIGINAL EQUATION
  !  FROM NI REGEN/ESTAB

  select case (VAR)

     ! BM: BLUE MOUNTAINS
     !     SPROUTING SPECIES: 13=PY, 15=AS, 16=CW

  case('BM')
     select case (ISPC)
     case(13,16)
        HTSPRT = (0.1 + SI/100.)*IAG
     case(15)
        HTSPRT = (0.1 + SI/80.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! CA: INLAND CALIFORNIA / SOUTHERN CASCADES
     ! OC: ORGANON SOUTHWEST
     !     SPROUTING SPECIES:   24=PY, 26=LO, 27=CY, 28=BL, 29=EO, 30=WO,
     !     31=BO, 32=VO, 33=IO, 34=BM, 35=BU, 36=RA, 37=MA, 38=GC, 39=DG,
     !     40=FL, 41=WN, 42=TO, 43=SY, 44=AS, 45=CW, 46=WI, 47=CN, 48=CL

  case('CA','OC')
     select case (ISPC)
     case(24,26:48)
        HTSPRT = (0.1 + SI/50.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! CR: CENTRAL ROCKIES
     !     SPROUTING SPECIES:   20=AS, 21=NC, 22=PW, 23=GO, 24=AW, 25=EM,
     !     26=BK, 27=SO, 28=PB, 29=AJ, 36=CI

  case('CR')
     select case (ISPC)
     case(21:29,36)
        HTSPRT = (0.1 + SI/100.)*IAG
     case(20)
        HTSPRT = (0.1 + SI/80.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! CS: CENTRAL STATES
     ! SPROUTING SPECIES: 3=SP,  8=WN,  9=BN, 10=TL, 11=TS, 12=WT, 13=BG,
     !     14=HS, 15=SH, 16=SL, 17=MH, 18=PH, 19=HI, 20=WH, 21=BH, 22=PE,
     !     23=BI, 24=AB, 25=BA, 26=PA, 27=UA, 28=EC, 29=RM, 30=BE, 31=SV,
     !     32=BC, 33=AE, 34=SG, 35=HK, 36=WE, 37=EL, 38=SI, 39=RL, 40=RE,
     !     41=YP, 42=BW, 43=SM, 44=AS, 45=WA, 46=GA, 47=WO, 48=RO, 49=SK,
     !     50=BO, 51=SO, 52=BJ, 53=CK, 54=SW, 55=BR, 56=SN, 57=PO, 58=DO,
     !     59=CO, 60=PN, 61=CB, 62=QI, 63=OV, 64=WK, 65=NK, 66=WL, 67=QS,
     !     69=SS, 70=OB, 71=CA, 72=PS, 73=HL, 74=BP, 75=BT, 76=QA, 77=BK,
     !     79=SY, 80=BY, 81=RB, 82=SU, 83=WI, 84=BL, 86=AH, 87=RD, 88=DW,
     !     89=HT, 90=KC, 91=OO, 92=CT, 93=MV, 94=MB, 95=HH, 96=SD

  case('CS')
     select case (ISPC)
     case(3,8:67,69:77,79:84,86:96)
        HTSPRT = (0.1 + SI/50.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! EC: EAST CASCADES
     !     SPROUTING SPECIES: NONE (but there will be in the 32 species version)

  case('EC')
     select case (ISPC)
     case(99999)
        HTSPRT = (0.1 + SI/50.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! LS: LAKE STATES
     !     SPROUTING SPECIES:   15=BA, 16=GA, 17=EC, 18=SV, 19=RM, 20=BC,
     !     21=AE, 22=RL, 23=RE, 24=YB, 25=BW, 26=SM, 27=BM, 28=AB, 29=WA,
     !     30=WO, 31=SW, 32=BR, 33=CK, 34=RO, 35=BO, 36=NP, 37=BH, 38=PH,
     !     39=SH, 40=BT, 41=QA, 42=BP, 43=PB, 45=BN, 46=WN, 47=HH, 48=BK,
     !     50=BE, 51=ST, 52=MM, 53=AH, 54=AC, 55=HK, 56=DW, 57=HT, 58=AP,
     !     59=BG, 60=SY, 61=PR, 62=CC, 63=PL, 64=WI, 65=BL, 66=DM, 67=SS,
     !     68=MA

  case('LS','ON')
     select case (ISPC)
     case(15:43,45:48,50:68)
        HTSPRT = (0.1 + SI/50.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! NC: NORTHERN CALIFORNIA
     !     SPROUTING SPECIES:    5=MA,  7=BO,  8=TO

  case('NC')
     select case (ISPC)
     case(5,7,8)
        HTSPRT = (0.1 + SI/50.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! NE: NORTHEAST STATES
     !     SPROUTING SPECIES:   26=RM, 27=SM, 28=BM, 29=SV, 30=YB, 31=SB,
     !     32=RB, 33=PB, 34=GB, 35=HI, 36=PH, 37=SL, 38=SH, 39=MH, 40=AB,
     !     41=AS, 42=WA, 43=BA, 44=GA, 45=PA, 46=YP, 47=SU, 48=CT, 49=QA,
     !     50=BP, 51=EC, 52=BT, 53=PY, 54=BC, 55=WO, 56=BR, 57=CK, 58=PO,
     !     59=OK, 60=SO, 61=QI, 62=WK, 63=PN, 64=CO, 65=SW, 66=SN, 67=RO,
     !     68=SK, 69=BO, 70=CB, 72=BU, 73=YY, 74=WR, 75=HK, 76=PS, 77=HY,
     !     78=BN, 79=WN, 80=OO, 81=MG, 82=MV, 83=AP, 84=WT, 85=BG, 86=SD,
     !     87=PW, 88=SY, 89=WL, 90=BK, 91=BL, 92=SS, 93=BW, 94=WB, 95=EL,
     !     96=AE, 97=RL, 99=BE,100=ST,101=AI,102=SE,103=AH,104=DW,105=HT,
     !    106=HH,107=PL,108=PR

  case('NE')
     select case (ISPC)
     case(26:70,72:97,99:108)
        HTSPRT = (0.1 + SI/50.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! SN: SOUTHERN STATES
     ! SPROUTING SPECIES: 5=SP, 15=BY, 16=PC, 18=FM, 19=BE, 20=RM, 21=SV,
     !     22=SM, 23=BU, 24=BB, 25=SB, 26=AH, 27=HI, 28=CA, 29=HB, 30=RD,
     !     31=DW, 32=PS, 33=AB, 34=AS, 35=WA, 36=BA, 37=GA, 38=HL, 39=LB,
     !     40=HA, 41=HY, 42=BN, 43=WN, 44=SU, 45=YP, 46=MG, 47=CT, 48=MS,
     !     49=MV, 50=ML, 51=AP, 52=MB, 53=WT, 54=BG, 55=TS, 56=HH, 57=SD,
     !     59=SY, 60=CW, 61=BT, 62=BC, 63=WO, 64=SO, 65=SK, 66=CB, 67=TO,
     !     68=LK, 69=OV, 70=BJ, 71=SN, 72=CK, 73=WK, 74=CO, 75=RO, 76=QS,
     !     77=PO, 78=BO, 79=LO, 80=BK, 81=WI, 82=SS, 83=BW, 84=EL, 85=WE,
     !     86=AE, 87=RL

  case('SN')
     select case (ISPC)
     case(5,15,16,18:57,59:87)
        HTSPRT = (0.1 + SI/50.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! SO: SOUTHERN OREGON / NORTHEASTERN CALIFORNIA
     !     SPROUTING SPECIES:   20=PY, 21=WA, 22=RA, 23=BM, 24=AS, 25=CW,
     !     26=CH, 27=WO, 28=WI, 29=GC, 31=MB

  case('SO')
     select case (ISPC)
     case(20,21,23,25,26,28,29,31)
        HTSPRT = (0.1 + SI/100.)*IAG
     case(24)
        HTSPRT = (0.1 + SI/80.)*IAG
     case(27)
        HTSPRT = (0.1 + SI/50.)*IAG
     case(22)
        HTSPRT = (0.1 + SI/20.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! TT: TETONS
     !     SPROUTING SPECIES:    6=AS, 13=BI, 14=MM, 15=NC

  case('TT')
     select case (ISPC)
     case(13,15)
        HTSPRT = (0.1 + SI/100.)*IAG
     case(6,14)
        HTSPRT = (0.1 + SI/80.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! UT: UTAH
     !     SPROUTING SPECIES:    6=AS, 13=GO, 18=NC, 19=FC, 21=BI, 22=BE

  case('UT')
     select case (ISPC)
     case(13,18,19,21,22)
        HTSPRT = (0.1 + SI/100.)*IAG
     case(6)
        HTSPRT = (0.1 + SI/80.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! WC: WEST CASCADES
     ! PN: PACIFIC NORTHWEST
     ! OP: ORGANON PACIFIC NORTHWEST (SP 24 = TO IN THIS VARIANT)
     !     SPROUTING SPECIES:   17=RW, 21=BM, 22=RA, 23=WA, 24=PB, 25=GC,
     !     26=AS, 27=CW, 28=WO, 33=PY, 34=DG, 35=HT, 36=CH, 37=WI

  case('WC','PN', 'OP')
     select case (ISPC)
     case(17,21,23:27,33:37)
        HTSPRT = (0.1 + SI/100.)*IAG
     case(28)
        HTSPRT = (0.1 + SI/300.)*IAG
     case(22)
        HTSPRT = (0.1 + SI/20.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! WS: WESTERN SIERRA NEVADA
     !     SPROUTING SPECIES:   23=RW, 28=LO, 29=CY, 30=BL, 31=BO, 32=VO,
     !     33=IO, 34=TO, 35=GC, 36=AS, 37=CL, 38=MA, 39=DG, 40=BM

  case('WS')
     select case (ISPC)
     case(23,28:40)
        HTSPRT = (0.1 + SI/50.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! XX: LEFTOVERS

  case default
     HTSPRT = 0.5 + 0.5*IAG

  end select

  return

end
