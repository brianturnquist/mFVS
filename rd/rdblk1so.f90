!===================!
!   rd : rdblk1so   !
!===================!
! RD $Id$
block data RDBLK1
  implicit none

  !  Purpose :
  !     This block data file initializes constants in the Root Disease
  !     extension to FVS.

  !  Previous revision date 07/09/10

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
  !.... Species #|  1 |  2 |  3 |  4 |  5 |  6 |  7 |  8 |  9 | 10 |
  !.... Species  | WP | WL | DF | GF | WH |  C | LP |  S | AF | PP |
  !....
  !.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
  !.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
  !....
  !.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
  !.... Species  | CB | WB | LM | CW | WS | J  | OC | GS | BO | OTH|
  !....
  !.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
  !.... Species  | JP | TO | P  | YC | RW | LL | KP | PY | NF | NH |
  !....
  !.... IRTSPC can be modified for different variants of FVS so
  !.... that species match between FVS and the root disease
  !.... model.

  !.... The following IRTSPC is for the SO 33 species variant.

  data IRTSPC /&
       !....     1    2    3    4    5    6    7    8    9   10   11   -- SO index
       !....    WP   SP   DF   WF   MH   IC   LP   ES   SH   PP   JU   -- SO specie
       &1,  12,   3,  13,  11,  14,   7,   8,  15,  10,  26,& !-- RD index
       !....    12   13   14   15   16   17   18   19   20   21   22   -- SO index
       !....    GF   AF   SF   NF   WB   WL   RC   WH   PY   WA   RA   -- SO specie
       &4,   9,  16,  39,  22,   2,   6,   5,  38,  40,  40,& !-- RD index
       !....    23   24   25   26   27   28   29   30   31   32   33   -- SO index
       !....    BM   AS   CW   CH   WO   WI   GC   MC   MB   OS   OH   -- SO specie
       &40,  19,  24,  40,  40,  40,  40,  40,  40,  17,  18/ !-- RD index

  data DICLAS /0.0, 5.0, 12.0, 24.0/
  data DSFAC  /1.0, 0.75/

  data IOUNIT /22/
  data IRUNIT /18/

end
