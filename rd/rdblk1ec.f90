!===================!
!   rd : rdblk1ec   !
!===================!
! RD $Id$
block data RDBLK1
  implicit none

  !  Purpose :
  !     This block data file initializes constants in the Root Disease
  !     extension to FVS.

  !  Previous revision date 03/16/12

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

  !.... The following IRTSPC is for the EC (32 species) variant.
  !....              WP  WL  DF  SF  RC  GF  LP  ES  AF  PP  WH
  data IRTSPC / 1,  2,  3, 16,  6,  4,  7,  8,  9, 10,  5,&
       !....              MH  PY  WB  NF  WF  LL  YC  WJ  BM  VN  RA
       &11, 38, 22, 39, 13, 36, 34, 26, 40, 40, 40,&
       !....              PB  GC  DG  AS  CW  WO  PL  WI  OS  OH
       &40, 40, 40, 19, 40, 40, 40, 40, 17, 18 /

  data DICLAS /0.0, 5.0, 12.0, 24.0/
  data DSFAC  /1.0, 0.75/

  data IOUNIT /22/
  data IRUNIT /18/

end
