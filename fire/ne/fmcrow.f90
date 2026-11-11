!======================!
!   fire/ne : fmcrow   !
!======================!
! FIRE-NE $Id$
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
  !  LISTED IN FMCROWE.F.  MAPPING IS DONE WHERE NECESSARY.  NOTE: MANY
  !  SPECIES ARE MAPPED TO COMMERCIAL HARDWOODS SO THAT THE "MIXED HARDWOOD"
  !  EQUATIONS IN JENKINS ET. AL. ARE USED.

  data ISPMAP / 8,10, 6, 9, 7, 9, 9, 3, 5, 3,&
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

     SPI = ISPMAP(ISP(I))
     D = DBH(I)
     H = HT(I)
     IC = ICR(I)
     SG = V2T(ISP(I))
     if (DEBUG) write(JOSTND,8) ISP(I),SPI
     8 format(' FMCROW ISP(I) = ',I2,' SPI=',I5)

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

     if (DEBUG) write(JOSTND,9) D,CROWNW(I,0),CROWNW(I,1),&
          &CROWNW(I,2),CROWNW(I,3),CROWNW(I,4),CROWNW(I,5)
     9 format(' FMCROW DBH = ',F5.1,' CROWNW=',6F10.4)
     if (DEBUG) write(JOSTND,10) (CROWNW(I,0)+ CROWNW(I,1) +&
          &CROWNW(I,2) + CROWNW(I,3) + CROWNW(I,4) + CROWNW(I,5))
     10 format(' FMCROW SUM OF CROWNW=',F10.4)

     999 continue
  end do

  return
end
