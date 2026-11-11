!===============================!
!   fire/base/common : FMPARM   !
!===============================!
! FIRE-BASE-COMMON $Id$

!  **FMPARM FIRE

!     PARAMETERS FOR THE FIRE MODEL ARE:

! Variables
integer :: MAXOPT
integer :: MXSNAG
integer :: TFMAX
integer :: MXFLCL
integer :: MXFMOD
integer :: MXDFMD
real    :: P2T

parameter (MAXOPT = 40)
parameter (MXSNAG = 2000)
parameter (P2T = 0.0005)
parameter (MXFLCL = 11)
parameter (MXFMOD = 5)
parameter (MXDFMD = 256)

!     THE FUEL CLASSES (MXFLCL=11) HAVE THESE MEANINGS. THEY ARE USED
!     FOR THE CWD AND RELATED ARRAYS (SEE FMCOM.F77)

!     1 : <0.25"
!     2 :  0.25" -   1"
!     3 :  1"    -   3"
!     4 :  3"    -   6"
!     5 :  6"    -  12"
!     6 :  12"   -  20"
!     7 :  20"   -  35"
!     8 :  35    -  50"
!     9 :          >50"
!     10:  Litter
!     11:  Duff

!     THE CROWN WEIGHT CATEGORIES ARE RELATED, BUT NOT IDENTICAL:
!     THEY ARE USED FOR CROWNW, OLDCRW, TFALL, CWD2B & CWD2B2
!     ARRAYS (DEFINED IN FMCOM.F77)

!     0 :  Foliage
!     1 : <0.25"
!     2 :  0.25" -   1"
!     3 :  1"    -   3"
!     4 :  3"    -   6"
!     5 :  6"    -  12"

parameter (TFMAX = 60)

!     TFMAX - THE MAXIMUM NUMBER OF YEARS TO TRACK DEAD CROWN MATERIAL AS IT
!     FALLS FROM THE CROWN. IF TFMAX IS CHANGED, MAKE SURE NO TFALL() VALUES
!     EXCEED IT.

!     MAXOPT:  THE MAX. NUMBER OF DIFFERENT KEYWORDS LINKED TO OPTION PROCESSOR
!     MXFLCL:  THE MAX. NUMBER OF FUEL CLASSES (NOTE THAT 7=LITTER, 8=DUFF)
!     MXSNAG:  THE MAX. NUMBER OF DIFFERENT SNAG RECORDS THAT WILL BE KEPT
!     MXFMOD:  THE MAX. NUMBER OF FUEL MODELS THAT CAN BE ACTIVE AT ANY TIME
!     MXDFMD:  THE MAX. NUMBER OF FUEL MODELS THAT CAN BE DEFINED IN THE MODEL
!     P2T:     CONVERSION FACTOR FROM POUNDS TO TONS.
