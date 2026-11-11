!====================!
!   vestb : essprt   !
!====================!
! VESTB $Id$
subroutine ESSPRT(VAR,ISPC,NSPT,PREM,DSTMP)
  implicit none

  !  THIS SUBROUTINE CONTAINS ENTRY POINTS TO HANDLE COMPUTATIONS FOR
  !  VARIOUS STUMP SPROUTING FUNCTIONS WHICH VARY BY VARIANT AND
  !  SPECIES.

  !  VARIANTS USING THE ESTB VERSION: AK, CI, EM, IE, KT, NI

  !  NO COMMON BLOCKS ARE CURRENTLY NEEDED.

  !  DEFINITIONS

  !     PREM -- NUMBER OF SPROUTS PER SPROUT RECORD
  !    DSTMP -- DIAMETER OF PARENT TREE

  ! Variables
  character(len=2) :: VAR
  integer :: IAG,INDXAS,ISHAG,ISPC,NMSPRC,NSPT
  real    :: ASTPAR,ASBAR,HTSPRT,PREM,RSHAG,SI,SPA,TREES,DSTMP
  integer :: IDANUW

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = NSPT

  !  VARIANT AND SPECIES SPECIFIC RULES FOR DETERMINING TPA A SPROUT
  !  RECORD WILL REPRESENT. ASPEN IS HANDLED SEPERATELY IN ENTRY ASSPTN
  !  PREM = TPA OF PARENT TREE OF SPROUT RECORD

  select case (VAR)

  case('AK')
     select case(ISPC)
     case(14,15)
        if (DSTMP.lt.25.9) then
           PREM = PREM * ((99.9999-3.8462 * DSTMP)/100)
        else
           PREM = 0.
        end if
     case(16,17)
        if (DSTMP.lt.6.0) then
           PREM = PREM * 0.92
        else if (DSTMP.ge.6.0.and.DSTMP.lt.9.0) then
           PREM = PREM * (-0.13333 * DSTMP + 1.70)
        else
           PREM = PREM * 0.50
        end if
     case(18,20)
        if (DSTMP.lt.6.25) then
           PREM = PREM * 0.39
        else if (DSTMP.ge.6.25.and.DSTMP.lt.8.75) then
           PREM = PREM * 0.32
        else
           PREM = PREM * 0.25
        end if
     case(21,22)
        PREM = PREM * 0.90
     case default
        PREM = PREM * 1.
     end select

  case('IE')
     select case(ISPC)
     case(17)
        PREM = PREM * 0.40
     case(19)
        PREM = PREM * 0.90
     case(20,21)
        PREM = PREM * 0.70
     case default
        PREM = PREM * 1.
     end select

  case('CI')
     select case(ISPC)
     case(12)
        PREM = PREM * 0.40
     case(17)
        PREM = PREM * 0.90
     case default
        PREM = PREM * 1.
     end select

  case('EM')
     select case(ISPC)
     case(11)
        if (DSTMP.le.12.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(13,15)
        PREM = PREM * 0.90
     case(14)
        if (DSTMP.le.25.0) then
           PREM = PREM * 0.80
        else
           PREM = PREM * 0.50
        end if
     case(16)
        PREM = PREM * 0.80
     case(17)
        PREM = PREM * 0.70
     end select

     !  ALL OTHER VARIANTS & SPECIES

  case default
     PREM = PREM * 1.

  end select
  return

  entry ESASID(VAR,INDXAS)

  !  THIS SUBROUTINE DETERMINES IF QUAKING ASPEN IS IN THIS VARIANT;
  !  IF SO, RETURN THE FVS SPECIES INDEX FOR ASPEN IN THIS VARIANT;
  !  OTHERWISE RETURN 9999.

  !  DEFINITIONS

  !   INDXAS -- QUAKING ASPEN SPECIES NUMBER

  select case (VAR)

  case('AK')
     INDXAS = 19

  case('CI')
     INDXAS = 13

  case('EM')
     INDXAS = 12

  case('IE')
     INDXAS = 18

  case default
     INDXAS = 9999

  end select
  return

  entry NSPREC (VAR,ISPC,NMSPRC,DSTMP)

  !  THIS SUBROUTINE DETERMINES THE NUMBER OF SPROUT RECORDS TO CREATE.

  !  DEFINITIONS

  !   NMSPRC -- NUMBER OF SPROUT RECORDS TO CREATE
  !    DSTMP -- DIAMETER OF PARENT TREE

  select case (VAR)

     !  SET TO 1 FOR: CI - PACIFIC YEW (12)
     !                IE - PACIFIC YEW (17)

     !  OTHERWISE SET TO 2

  case('AK')
     select case (ISPC)
     case(18,20)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.lt.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case(19)
        NMSPRC = 2
     case default
        NMSPRC = 1
     end select

  case('CI')
     select case (ISPC)
     case(12)
        NMSPRC = 1
     case(13)
        NMSPRC = 2
     case(17)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.lt.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

  case('EM')
     select case (ISPC)
     case(12)
        NMSPRC = 2
     case(13:16)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.lt.10.0) then
           NMSPRC = NINT(-1.0 + 0.4 * DSTMP)
        else
           NMSPRC = 3
        end if
     case default
        NMSPRC = 1
     end select

  case('IE')
     select case (ISPC)
     case(18)
        NMSPRC = 2
     case(19)
        if (DSTMP.lt.5.0) then
           NMSPRC = 1
        else if (DSTMP.ge.5.0.and.DSTMP.lt.10.0) then
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

  !  THIS SUBROUTINE DETERMINES THE NUMBER OF QUAKING ASPEN SPROUTS
  !  PER ACRE (SPA) BASED ON POLYNOMIAL FORMULA FROM CROUCH DATA SET,
  !  MODIFIED BY PROPORTION OF ASPEN BA CUT BY CHAD KEYSER (FMSC).

  !  DEFINITIONS

  !    RSHAG -- AGE OF SPROUT RECORD
  !      SPA -- POTENTIAL TREES PER ACRE OF ASPEN SPROUTS
  !    ASBAR -- ASPEN BASAL AREA HARVESTED (198 IS AVERAGE FOR STOCKED
  !               STANDS IN INPUT DATASET)
  !   ASTPAR -- TOTAL REMOVED ASPEN TREES PER ACRE
  !    TREES -- TREES PER ACRE REPRESENTED BY SPROUT RECORD

  RSHAG = FLOAT(ISHAG)
  SPA = 40100.45 - 3574.02*RSHAG**2.0 + 554.02*RSHAG**3.0&
       &- 3.5208*RSHAG**5.0 + 0.011797*RSHAG**7.0
  if (SPA .lt.2608.)SPA = 2608.
  if (SPA .gt.30125.)SPA = 30125.
  SPA = SPA * ASBAR/198.
  TREES = (PREM/(ASTPAR*2.))*SPA

  return

  entry SPRTHT (VAR,ISPC,SI,IAG,HTSPRT)

  !  THIS SUBROUTINE DETERMINES SPROUT HEIGHT BY VARIANT AND SPECIES.

  !  EQUATION IS HT = (0.1 + SI/BASEAGE)*AGE

  !  DEFAULT EQUATION, HTSPRT = 0.5 + 0.5*IAG, IS THE ORIGINAL EQUATION
  !  FROM NI REGEN/ESTAB

  !  DEFINITIONS

  !    HTSPRT -- HEIGHT OF SPROUT RECORD

  select case (VAR)

     ! AK: ALASKA
     !     SPROUTING SPECIES: 14=AD, 15=RA, 16=PB, 17=AB, 18=BA, 19=AS,
     !     20=CW, 21=WI, 22=SU

  case('AK')
     select case (ISPC)
     case(14:22)
        HTSPRT = (0.1 + SI/100.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! CI: CENTRAL IDAHO
     !     SPROUTING SPECIES: 12=PY, 13=AS, 17=CW

  case('CI')
     select case (ISPC)
     case(13)
        HTSPRT = (0.1 + SI/80.)*IAG
     case(12,17)
        HTSPRT = (0.1 + SI/100.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! EM: EASTERN MONTANA
     !     SPROUTING SPECIES: 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC, 17=PB

  case('EM')
     select case (ISPC)
     case(12,17)
        HTSPRT = (0.1 + SI/80.)*IAG
     case(11,13:16)
        HTSPRT = (0.1 + SI/100.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! IE: INLAND EMPIRE
     !     SPROUTING SPECIES: 17=PY, 18=AS, 19=CO, 20=MM, 21=PB

  case('IE')
     select case (ISPC)
     case(18)
        HTSPRT = (0.1 + SI/80.)*IAG
     case(17,19:21)
        HTSPRT = (0.1 + SI/100.)*IAG
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! KT: KOOTENAI, KANIKSU, TALLY LAKE RD OF THE FLATHEAD
     ! NI: NORTHERN IDAHO
     !     SPROUTING SPECIES: NONE

  case('KT','NI')
     select case (ISPC)
     case default
        HTSPRT = 0.5 + 0.5*IAG
     end select

     ! XX: LEFTOVERS

  case default
     HTSPRT = 0.5 + 0.5*IAG

  end select

  return

end
