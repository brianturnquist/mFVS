!=================!
!   rd : rdblk1   !
!=================!
! RD $Id$
block data RDBLK1
  implicit none

  !  Purpose :
  !     This block data file initializes constants in the Root Disease
  !     extension to FVS.

  !  Previous revision date 04/30/09

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
  !.... Species  | WP | WL | DF | GF | WH | RC | LP | ES | AF | PP |
  !....
  !.... Species #| 11 | 12 | 13 | 14 | 15 | 16 | 17 | 18 | 19 | 20 |
  !.... Species  | MH | SP | WF | IC | RF | SF | OS | OH | AS | BS |
  !....
  !.... Species #| 21 | 22 | 23 | 24 | 25 | 26 | 27 | 28 | 29 | 30 |
  !.... Species  | CB | WB | LM | CO | WS | JU | OC | GS | BO | OTH|
  !....
  !.... Species #| 31 | 32 | 33 | 34 | 35 | 36 | 37 | 38 | 39 | 40 |
  !.... Species  | JP | TO | PI | YC | RW | LL | KP | PY | NF | NH |
  !....
  !.... IRTSPC can be modified for different variants of FVS so
  !.... that species match between FVS and the root disease
  !.... model.

  !.... The following IRTSPC is the base root disease model IRTSPC
  !.... It is used for variants : NI, CI, and KT

  data IRTSPC / 1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 30/

  data DICLAS /0.0, 5.0, 12.0, 24.0/
  data DSFAC  /1.0, 0.75/

  data IOUNIT /22/
  data IRUNIT /18/

end
