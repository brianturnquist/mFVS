!========================!
!   fire/vbase : fmcrow  !
!========================!
! FIRE-LS, ON, CS, NE $Id$
subroutine FMCROW
  implicit none

  !     CALLED FROM: FMSDIT, FMPRUN
  !     CALLS:

  !  PURPOSE:
  !     THIS SUBROUTINE CALCULATES CROWNW(TREE,SIZE), THE WEIGHT OF
  !     VARIOUS SIZES OF CROWN MATERIAL THAT IS ASSOCIATED WITH EACH TREE
  !     RECORD IN THE CURRENT STAND.

  !  LOCAL VARIABLE DEFINITIONS:
  !     D:        DBH
  !     H:        HEIGHT
  !     IC:       LENGTH OF LIVE CROWN
  !     SP:       SPECIES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'

  !  VARIABLE DECLARATIONS

  ! Variables
  logical :: DEBUG
  integer :: I,J,IC,SPI
  integer :: MAPLS(68),MAPON(72),MAPCS(96),MAPNE(108)
  real    :: D,H,SG,XV(0:5)

  !  INDEX TO THE CROWN EQUATIONS USED.  THE SPECIES NUMBERS ARE THOSE
  !  LISTED IN FMCROWE.F.  THE FIRST 68 ARE THE 68 SPECIES IN THE LS
  !  VARIANT.

  !  RETAIN EACH NATIVE SPECIES MAP. THE TREE LOOP AND CROWN-WEIGHT
  !  CALL ARE SHARED; ONLY CS/NE EMIT THE EXTRA ORIGINAL DIAGNOSTICS.

  !  LS NATIVE MAPPING.

  data MAPLS /1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,&
       &21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,&
       &38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,&
       &55,56,57,58,59,60,61,62,63,64,65,66,67,68/

  !  ON NATIVE MAPPING.

  data MAPON /1, 2, 3, 4, 5, 6, 7, 8, 9,10,&
       &11,12,13,14,15,16,17,18,19,20,&
       &21,22,23,24,25,26,27,28,29,30,&
       &31,32,33,34,35,36,37,38,39,40,&
       &41,42,43,44,45,46,47,48,49,50,&
       &51,52,53,54,55,56,57,58,59,60,&
       &61,62,63,64,65,66,67,68, 1, 5,&
       &6, 9 /

  !  CS NATIVE MAPPING.

  data MAPCS /14,14, 5, 5, 5, 5, 5,46,45,59,&
       &59,59,59,39,39,39,39,38,39,39,&
       &37,39,39,28,15,16,16,17,19,50,&
       &18,20,21,55,55,21,21,21,22,23,&
       &44,25,26,16,29,16,30,34,34,35,&
       &34,34,33,31,32,30,30,30,30,36,&
       &34,34,30,30,34,30,34,44,67,44,&
       &44,44,44,42,40,41,48,60,60,44,&
       &24,44,64,65,56,53,44,56,57,44,&
       &44,44,44,44,47,44/

  !  NE NATIVE MAPPING.

  data MAPNE / 8,10, 6, 9, 7, 9, 9, 3, 5, 3,&
       &3,11,11,14,14,12,12, 3, 3, 3,&
       &3, 3, 3, 3, 3,19,26,27,18,24,&
       &24,24,43,24,39,38,39,39,39,28,&
       &29,29,15,16,16,44,44,44,41,42,&
       &17,40,17,20,30,32,33,30,30,34,&
       &34,34,36,30,30,30,34,34,35,34,&
       &44,44,44,24,55,44,44,45,46,44,&
       &44,44,58,59,59,44,44,60,34,48,&
       &65,67,25,25,21,21,22,49,50,51,&
       &44,44,53,56,57,47,63,61/

  !  CHECK FOR DEBUG

  call DBCHK (DEBUG,'FMCROW',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,ITRN
  7 format(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)

  if (ITRN.eq.0) return

  do I = 1,ITRN

     !  INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
     !  TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
     !  IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.

     if (GROW(I) .lt.1) GROW(I) = GROW(I) + 1
     if (GROW(I) .lt.1) goto 999

     !  ARGUMENTS TO PASS

     select case (VARIANTCD)
     case ('LS')
        SPI = MAPLS(ISP(I))
     case ('ON')
        SPI = MAPON(ISP(I))
     case ('CS')
        SPI = MAPCS(ISP(I))
     case ('NE')
        SPI = MAPNE(ISP(I))
     end select
     D = DBH(I)
     H = HT(I)
     IC = ICR(I)
     SG = V2T(ISP(I))
     if (VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE') then
        if (DEBUG) write(JOSTND,8) ISP(I),SPI
        8 format(' FMCROW ISP(I) = ',I2,' SPI=',I5)
     end if

     !  INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
     !  OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.

     do J = 0,5
        XV(J) = 0.0
     end do

     call FMCROWE(SPI,ISP(I),D,H,IC,SG,XV)

     !  COPY TEMPORARY VALUES TO FFE ARRAY

     do J = 0,5
        CROWNW(I,J) = XV(J)
        if (DEBUG) write(JOSTND,*) 'I=',I,' size=',J,&
             &' CROWNW=',CROWNW(I,J)
     end do

     if (VARIANTCD.eq.'CS'.or.VARIANTCD.eq.'NE') then
        if (DEBUG) write(JOSTND,9) D,CROWNW(I,0),CROWNW(I,1),&
             &CROWNW(I,2),CROWNW(I,3),CROWNW(I,4),CROWNW(I,5)
        9 format(' FMCROW DBH = ',F5.1,' CROWNW=',6F10.4)
        if (DEBUG) write(JOSTND,10) (CROWNW(I,0)+ CROWNW(I,1) +&
             &CROWNW(I,2) + CROWNW(I,3) + CROWNW(I,4) + CROWNW(I,5))
        10 format(' FMCROW SUM OF CROWNW=',F10.4)
     end if

     999 continue
  end do

  return
end
