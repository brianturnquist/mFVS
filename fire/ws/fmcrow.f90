!======================!
!   fire/ws : fmcrow   !
!======================!
! FIRE-WS $Id$
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
  integer :: I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
  integer :: ITR,SPIW,SPIE
  real    :: D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)

  !     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
  !     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
  !     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
  !     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
  !     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF

  !     I   NAME                     MAPS TO          WEST   EAST

  !     1 = SUGAR PINE               w white pine     15
  !     2 = DOUGLAS-FIR              -                 3
  !     3 = WHITE FIR                grand fir         4
  !     4 = GIANT SEQUOIA            -                19
  !     5 = INCENSE CEDAR            -                20
  !     6 = JEFFREY PINE             w white pine     15
  !     7 = RED FIR                  grand fir         4
  !     8 = PONDEROSA PINE           -                13
  !     9 = LODGEPOLE PINE           -                11
  !    10 = WHITEBARK PINE           -                14
  !    11 = WESTERN WHITE PINE       -                15
  !    12 = SINGLELEAF PINYON        pinyon pine      12
  !    13 = PACIFIC SILVER FIR       grand fir         4
  !    14 = KNOBCONE PINE            lodgepole pine   11
  !    15 = FOXTAIL PINE             lodgepole pine   11
  !    16 = COULTER PINE             lodgepole pine   11
  !    17 = LIMBER PINE              lodgepole pine   11
  !    18 = MONTEREY PINE            ponderosa pine   13
  !    19 = GRAY/CA FOOTHILL PINE    lodgepole pine   11
  !    20 = WASHOE PINE              lodgepole pine   11
  !    21 = GB BRISTLECONE PINE      -                 9
  !    22 = BIGCONE DOUGLAS-FIR      Douglas-fir       3
  !    23 = REDWOOD                  giant sequoia    19
  !    24 = MOUNTAIN HEMLOCK         -                24
  !    25 = WESTERN JUNIPER          juniper          16
  !    26 = UTAH JUNIPER             juniper          16
  !    27 = CALIFORNIA JUNIPER       juniper          16
  !    28 = CALIFORNIA LIVE OAK      tanoak           17
  !    29 = CANYON LIVE OAK          tanoak           17
  !    30 = BLUE OAK                 CA black oak     21
  !    31 = CALIFORNIA BLACK OAK     -                21
  !    32 = CA WHITE/VALLEY OAK      CA black oak     21
  !    33 = INTERIOR LIVE OAK        tanoak           17
  !    34 = TANOAK                   tanoak           17
  !    35 = GIANT CHINKAPIN          tanoak           17
  !    36 = QUAKING ASPEN            -                         41
  !    37 = CALIFORNIA-LAUREL        tanoak           17
  !    38 = PACIFIC MADRONE          -                10
  !    39 = PACIFIC DOGWOOD          flowering dogwood         56
  !    40 = BIGLEAF MAPLE            bigleaf maple     5
  !    41 = CURL-LF. MT. MAHOG.      quaking aspen             41
  !    42 = OTHER SOFTWOODS          lodgepole pine   11
  !    43 = OTHER HARDWOODS          CA black oak     21

  data ISPMAP/&
       &15,  3,  4, 19, 20, 15,  4, 13, 11, 14,&
       &15, 12,  4, 11, 11, 11, 11, 13, 11, 11,&
       &9,  3, 19, 24, 16, 16, 16, 17, 17, 21,&
       &21, 21, 17, 17, 17, 41, 17, 10, 56,  5,&
       &41, 11, 21/

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

     SPIW = ISP(I)
     SPIE = ISPMAP(SPIW)

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

     select case (SPIW)
     case (36,39,41)
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
