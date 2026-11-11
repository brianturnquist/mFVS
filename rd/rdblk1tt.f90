!===================!
!   rd : rdblk1tt   !
!===================!
! RD $Id$
block data RDBLK1
  implicit none

  !  Purpose :
  !     This block data file initializes constants in the Root Disease
  !     extension to FVS Tetons variant.

  !  Revision History:
  !  13-APR-10 Lance David (FMSC)
  !     FVS TT variant expanded to 18 species. IRTSPC array set accordingly.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'METRIC.f90'

  !.... COMMON INCLUDE FILES

  include 'RDCOM.f90'
  include 'RDCRY.f90'
  include 'RDARRY.f90'
  include 'RDADD.f90'

  !.... The array IRTSPC is used to index the species dependent arrays
  !.... HABFAC, PNINF, PKILLS, RRJSP, ISPS, DBIFAC, HTIFAC, PROOT,
  !.... RSLOP, ROWDOM, ROWIBP, RRPSWT, SSSFAC, IDITYP, PCOLO.
  !.... In the root disease model, the defaults for these variables
  !.... are indexed as follows :
  !....
  !.... RD Model species:
  !....   #  Code  Name                   #  Code  Name
  !....   1   WP   WHITE PINE            21   CB   CORKBARK FIR
  !....   2   WL   WESTERN LARCH         22   WB   WHITEBARK PINE
  !....   3   DF   DOUGLAS-FIR           23   LM   LIMBER PINE
  !....   4   GF   GRAND FIR             24   CO   COTTONWOOD
  !....   5   WH   WESTERN HEMLOCK       25   WS   WHITE SPRUCE
  !....   6   RC   W. REDCEDAR           26   JU   JUNIPER
  !....   7   LP   LODGEPOLE PINE        27   OC   OTHER CONIFERS
  !....   8   ES   ENGELMANN SPRUCE      28   GS   GIANT SEQUOIA
  !....   9   AF   SUBALPINE FIR         29   BO   BLACK OAK
  !....  10   PP   PONDEROSA PINE        30   OTH  OTHER
  !....  11   MH   MOUNTAIN HEMLOCK      31   JP   JEFFREY PINE
  !....  12   SP   SUGAR PINE            32   TO   TANOAK/CHINKAPIN
  !....  13   WF   WHITE FIR             33   PI   PINYON PINE
  !....  14   IC   INCENSE CEDAR         34   YC   YELLOW CEDAR
  !....  15   RF   RED FIR               35   RW   REDWOOD
  !....  16   SF   P. SILVER FIR         36   LL   SUBALPINE LARCH
  !....  17   OS   OTHER SOFTWOOD        37   KP   KNOBCONE PINE
  !....  18   OH   OTHER HARDWOOD        38   PY   PACIFIC YEW
  !....  19   AS   ASPEN                 39   NF   NOBLE FIR
  !....  20   BS   BLUE SPRUCE           40   NH   NON-HOST
  !....
  !.... SPECIES LIST FOR TETON VARIANT. ***** 18 species *****
  !....
  !....  ------FVS TT VARIANT-------   WRD MODEL SPECIES    ANNOSUS
  !....   # CD COMMON NAME             OR SURROGATE SP.     TYPE
  !....  -- -- --------------------- ---------------------  --------
  !....   1 WB WHITEBARK PINE          WHITEBARK PINE (22)  P-TYPE
  !....   2 LM LIMBER PINE                LIMBER PINE (23)  P-TYPE
  !....   3 DF DOUGLAS-FIR                DOUGLAS-FIR (3)   S-TYPE
  !....   4 PM SINGLELEAF PINYON          PINYON PINE (33)  P-TYPE
  !....   5 BS BLUE SPRUCE                BLUE SPRUCE (20)  S-TYPE
  !....   6 AS QUAKING ASPEN                    ASPEN (19)  NON-HOST
  !....   7 LP LODGEPOLE PINE          LODGEPOLE PINE (7)   P-TYPE
  !....   8 ES ENGLEMANN SPRUCE      ENGELMANN SPRUCE (8)   S-TYPE
  !....   9 AF SUBALPINE FIR            SUBALPINE FIR (9)   S-TYPE
  !....  10 PP PONDEROSA PINE          PONDEROSA PINE (10)  P-TYPE
  !....  11 UJ UTAH JUNIPER                   JUNIPER (26)  P-TYPE
  !....  12 RM ROCKY MOUNTAIN JUNIPER         JUNIPER (26)  P-TYPE
  !....  13 BI BIGTOOTH MAPLE                NON-HOST (40)  NON-HOST
  !....  14 MM ROCKY MOUNTAIN MAPLE          NON-HOST (40)  NON-HOST
  !....  15 NC NARROWLEAF COTTONWOOD       COTTONWOOD (24)  NON-HOST
  !....  16 MC CURLLEAF MOUNTAIN-            NON-HOST (40)  NON-HOST
  !....        MAHOGANY
  !....  17 OS OTHER SOFTWOODS         OTHER SOFTWOOD (17)  P-TYPE
  !....  18 OH OTHER HARDWOODS         OTHER HARDWOOD (18)  NON-HOST
  !....
  !....
  !.... IRTSPC can be modified for different variants of FVS so
  !.... that species match between FVS and the root disease
  !.... model.
  !....
  !.... The following IRTSPC is for the TT 18 species variant.
  !....              1   2   3   4   5   6   7   8   9   10  -- FVS index
  !....              WB  LM  DF  PM  BS  AS  LP  ES  AF  PP  -- FVS species
  data IRTSPC /22, 23, 3,  33, 20, 19, 7,  8,  9,  10,&
       !....              11  12  13  14  15  16  17  18          -- FVS index
       !....              UJ  RM  BI  MM  NC  MC  OS  OH          -- FVS species
       &26, 26, 40, 40, 24, 40, 17, 18 /

  data DICLAS /0.0, 5.0, 12.0, 24.0/
  data DSFAC  /1.0, 0.75/

  data IOUNIT /22/
  data IRUNIT /18/

end
