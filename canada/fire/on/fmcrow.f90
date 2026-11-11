!=============================!
!   canada/fire/on : fmcrow   !
!=============================!
! CANADA-FIRE-ON $Id$
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
  integer :: I,J,IC,ISPMAP(MAXSP),SPI
  real    :: D,H,SG,XV(0:5)

  !  INDEX TO THE CROWN EQUATIONS USED.  THE SPECIES NUMBERS ARE THOSE
  !  LISTED IN FMCROWE.F.  THE FIRST 68 ARE THE 68 SPECIES IN THE LS
  !  VARIANT.

  data ISPMAP /1, 2, 3, 4, 5, 6, 7, 8, 9,10,&
       &11,12,13,14,15,16,17,18,19,20,&
       &21,22,23,24,25,26,27,28,29,30,&
       &31,32,33,34,35,36,37,38,39,40,&
       &41,42,43,44,45,46,47,48,49,50,&
       &51,52,53,54,55,56,57,58,59,60,&
       &61,62,63,64,65,66,67,68, 1, 5,&
       &6, 9 /

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

     SPI = ISPMAP(ISP(I))
     D = DBH(I)
     H = HT(I)
     IC = ICR(I)
     SG = V2T(ISP(I))

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

     999 continue
  end do

  return
end
