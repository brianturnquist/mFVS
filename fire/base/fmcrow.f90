!======================!
!   fire/base : fmcrow !
!======================!
! FIRE-BASE $Id$
subroutine FMCROW
  implicit none

  !     CALLED FROM: FMSDIT, FMPRUN
  !     CALLS        RDPSRT
  !                  PCTILE
  !  Purpose:
  !     This subroutine calculates CROWNW(tree,size), the weight of
  !     various sizes of crown material that is associated with each tree
  !     record in the current stand.  These weights depend on tree
  !     species, diameter, height, and crown ratio according to the
  !     relationships described in Brown & Johnston, 1976, 'Debris
  !     Prediction System', Fuel Science RWU 2104, which is itself
  !     based primarily on Res Paper INT-197.

  !     NOTE:  The allocation between crown size class 4 and 5 is
  !            somewhat arbitrary, with class 5 currently not receiving any.

  !  Local variable definitions:

  !     D;        Dbh
  !     H:        Height
  !     HPCT:     Height PerCenTile ranking of each tree list element
  !     HPOINT:   Height POINTer: specifies which tree list element has
  !               the tallest trees, next tallest, and so on.
  !     IC:       length of live Crown
  !     SP:       SPecies

  !  Common block variables and parameters:

  !.... Parameter include files.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... Common include files.
  include 'FMCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'

  !.... Parameter statements.

  ! Variables
  logical :: DEBUG
  integer :: I,J,IC,ISPMAPKT(11),ISPMAPNC(12),SPI,HPOINT(MAXTRE)
  integer :: ITR
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
  !    11 = OTHER                    mountain hemlock 24

  data ISPMAPKT /15, 8, 3, 4, 6, 7,11,18, 1,13,&
       &24/

  !     NC SPECIES: OTHER CONIFERS, SUGAR PINE, DOUGLAS-FIR, WHITE FIR,
  !     MADRONE, INCENSE CEDAR, CALIFORNIA BLACK OAK, TANOAK, RED FIR,
  !     PONDEROSA PINE, OTHER HARDWOOD, COAST REDWOOD. KEEP THE ORIGINAL
  !     TWELVE VALUES, INCLUDING THE FINAL 19; THE OLD COMMENT SAID 3.

  data ISPMAPNC / 3, 15, 3, 4, 10, 20, 21, 17, 4, 13,&
       &17, 19/

  !     CHECK FOR DEBUG

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

     !       TABLE LENGTHS DIFFER. USE THE COMPILE-TIME VARIANT SO THE
     !       SPECIES MAPPING DOES NOT DEPEND ON GRINIT HAVING RUN.

     if (VARIANTCD .eq. 'NC') then
        SPI = ISPMAPNC(ISP(I))
     else
        SPI = ISPMAPKT(ISP(I))
     end if

     D = DBH(I)
     H = HT(I)
     IC = ICR(I)
     ITR = ITRUNC(I)
     HP = HPCT(I)
     SG = V2T(ISP(I))

     !       INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
     !       OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.

     do J = 0,5
        XV(J) = 0.0
     end do

     call FMCROWW(SPI,D,H,ITR,IC,HP,SG,XV)

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
