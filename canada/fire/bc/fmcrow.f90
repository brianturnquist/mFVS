!=============================!
!   canada/fire/bc : fmcrow   !
!=============================!
! CANADA-FIRE-BC $Id$
subroutine FMCROW
  implicit none

  !     CALLED FROM: FMSDIT, FMPRUN
  !     CALLS        RDPSRT
  !                  PCTILE
  !  PURPOSE:
  !     THIS SUBROUTINE CALCULATES CROWNW(TREE,SIZE), THE WEIGHT OF
  !     VARIOUS SIZES OF CROWN MATERIAL THAT IS ASSOCIATED WITH EACH TREE
  !     RECORD IN THE CURRENT STAND.  THESE WEIGHTS DEPEND ON TREE
  !     SPECIES, DIAMETER, HEIGHT, AND CROWN RATIO ACCORDING TO THE
  !     RELATIONSHIPS DESCRIBED IN BROWN & JOHNSTON, 1976, 'DEBRIS
  !     PREDICTION SYSTEM', FUEL SCIENCE RWU 2104, WHICH IS ITSELF
  !     BASED PRIMARILY ON RES PAPER INT-197.

  !     NOTE:  THE ALLOCATION BETWEEN CROWN SIZE CLASS 4 AND 5 IS
  !            SOMEWHAT ARBITRARY, WITH CLASS 5 CURRENTLY NOT RECEIVING ANY.

  !  LOCAL VARIABLE DEFINITIONS:
  !     D:        DBH
  !     H:        HEIGHT
  !     HPCT:     HEIGHT PERCENTILE RANKING OF EACH TREE LIST ELEMENT
  !     HPOINT:   HEIGHT POINTER: SPECIFIES WHICH TREE LIST ELEMENT HAS
  !               THE TALLEST TREES, NEXT TALLEST, AND SO ON.
  !     IC:       LENGTH OF LIVE CROWN
  !     SPIE:     EQUAL TO ISPMAP
  !     SPIW:     SPECIES NUMBER IS IE VARIANT
  !     ISPMAP:   SPECIES (NUMBER) IN FMCROWW OR FMCROWE THAT DIFFERENT
  !               SPECIES ARE MAPPED TO

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'

  !  VARIABLE DECLARATIONS

  ! Variables
  logical :: DEBUG
  integer :: I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
  integer :: ITR,SPIW,SPIE
  real    :: D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)

  !     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
  !     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
  !     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
  !     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
  !     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF

  !     I   NAME                     MAPS TO          WEST   EAST

  !     1 = WESTERN WHITE PINE       -                15
  !     2 = WESTERN LARCH            -                 8
  !     3 = DOUGLAS-FIR              -                 3
  !     4 = GRAND FIR                -                 4
  !     5 = WESTERN HEMLOCK          -                 6
  !     6 = WESTERN REDCEDAR         -                 7
  !     7 = LODGEPOLE PINE           -                11
  !     8 = ENGELMANN SPRUCE         -                18
  !     9 = SUBALPINE FIR            -                 1
  !    10 = PONDEROSA PINE           -                13
  !    11 = BIRCH (PB)               aspen            43
  !    12 = ASPEN (AS)                                41
  !    13 = COTTONWOOD (CW)          cottonwood sp    17
  !    14 = OTHER CONIFER (OC)       FD                3
  !    15 = OTHER HARDWOOD (OH)      EP               41

  data ISPMAP /15, 8, 3, 4, 6, 7,11,18, 1,13,&
       &43,41,17, 3,41/

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCROW',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,ITRN
  7 format(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)

  if (ITRN.eq.0) return

  !     YOU'LL NEED TO KNOW PERCENTILE HEIGHT OF EACH TREE.
  !     TO GET THIS, MAKE AN ARRAY THAT LISTS THE TREE LIST ELEMENTS
  !     IN DESCENDING ORDER BY THE HEIGHT OF EACH RECORD:

  call RDPSRT(ITRN,HT,HPOINT,.true.)

  !     NOW CALL PCTILE TO GET THE HEIGHT PERCENTILE OF EACH RECORD.
  !     NOTE THAT PCTILE ONLY WORKS IF YOU PASS IT THE DENSITY OF TREES IN
  !     EACH RECORD RATHER THAN THE HEIGHT OF THE RECORD. NOTE ALSO THAT
  !     ANY RECORDS WITH NO TREES WILL NONETHELESS COME BACK WITH THE
  !     PERCENTILE RANKING IMPLIED BY THEIR HEIGHT. SUCH RECORDS WILL NOT
  !     INFLUENCE THE PERCENTILE RANKING OF TREES IN OTHER RECORDS.

  call PCTILE (ITRN, HPOINT, PROB, HPCT, JUNK)

  do I = 1,ITRN

     !       INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
     !       TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
     !       IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.

     if (GROW(I) .lt.1) GROW(I) = GROW(I) + 1
     if (GROW(I) .lt.1) goto 999

     !       ARGUMENTS TO PASS

     SPIW = ISP(I)
     SPIE = ISPMAP(SPIW)
     D = DBH(I)
     H = HT(I)
     IC = ICR(I)
     ITR = ITRUNC(I)
     HP = HPCT(I)
     SG = V2T(SPIW)

     !       INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
     !       OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.

     do J = 0,5
        XV(J) = 0.0
     end do

     select case (SPIW)
     case (11,12,13,15)
        call FMCROWE(SPIE,SPIW,D,H,IC,SG,XV)
     case default
        call FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
     end select

     !       COPY TEMPORARY VALUES TO FFE ARRAY

     do J = 0,5
        CROWNW(I,J) = XV(J)
        if (DEBUG) write(JOSTND,*) 'I=',I,' size=',J,&
             &' CROWNW=',CROWNW(I,J)
     end do

     999 continue
  end do

  return
end

  !  PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**

subroutine HTDBH(I10,I11,X10,X11,I12)
  implicit none

  ! Variables
  integer :: I10,I11,I12
  real    :: X10,X11

  I10 = 0
  I11 = 0
  I12 = 0

  X10 = 0.0
  X11 = 0.0

  return
end
