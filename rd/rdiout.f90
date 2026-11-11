!=================!
!   rd : rdiout   !
!=================!
! RD $Id$
subroutine RDIOUT
  implicit none

  !  PRODUCES OPTIONAL OUTPUT (IN A SEPARATE FILE) OF THE RESULTS
  !  OF MONTE CARLO SIMULATIONS OF INFECTION WITHIN A CENTER

  !  CALLED BY :
  !     RDCNTL  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]

  !  PARAMETERS :
  !     NONE

  !  COMMON BLOCK VARIABLES :
  !     ANUINF: ARRAY CONTAINING INDIVIDUAL SIMULATION RESULTS
  !     CORINF: ARRAY CONTAINING THE AVERAGE RESULT AND THE TOTAL UNINF. TREES BEFORE
  !             ANY INFECTION OCCURS

  !  LOCAL VARIABLES :
  !     ISDOUT:  NUMBER OF OUTPUT FILE

  !  REVISION HISTORY:
  !  18-JUN-2001 Lancd R. David (FHTET)
  !    Added Stand ID and Management ID line to report header.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDARRY.f90'
  include 'RDCOM.f90'
  include 'RDCRY.f90'
  include 'RDADD.f90'

  ! Variables
  character(len=1) :: CHTYPE(ITOTRR)
  integer :: ISIM,J,JYR,MAXSIM,N
  real    :: AVG,SD,SUMX,UINUM

  data CHTYPE /'P','S','A','W'/

  if (IROOT .eq.0) return

  !     IF MONTE CARLO OUTPUT WAS NOT REQUESTED THEN RETURN

  if (ISDOUT .le.0) return

  MAXSIM = MIN(NINSIM,10)

  if (ICYC .eq.1) then

     !         WRITE THE TABLE HEADERS

     write (ISDOUT,1100)
     write (ISDOUT,1105)
     write (ISDOUT,1100)
     write (ISDOUT,1110) NPLT, MGMID
     write (ISDOUT,1115)
     write (ISDOUT,1120)
     write (ISDOUT,1155) (N,N = 1,MAXSIM)
     write (ISDOUT,1160)

  end if

  JYR = IY(ISTEP) - 1

  do IRRSP = MINRR,MAXRR

     if (PAREA(IRRSP) .le.0.0) goto 5000

     AVG = CORINF(IRRSP,1)
     UINUM = CORINF(IRRSP,2)

     SUMX = 0.0
     SD = 0.0
     do ISIM = 1,NINSIM
        SUMX = SUMX + (ANUINF(IRRSP,ISIM) - AVG)**2
        100 continue
     end do
     if (NINSIM .gt.1) SD = SQRT(SUMX) / FLOAT(NINSIM - 1)

     write(ISDOUT,2099) JYR,CHTYPE(IRRSP),NINSIM,AVG,SD,UINUM,&
          &(ANUINF(IRRSP,J),J = 1,MAXSIM)
     write(ISDOUT,2199) (INFISD(IRRSP,J),J = 1,MAXSIM)
     !         WRITE(ISDOUT,2200) (SPRQMD(IRRSP,J),J=1,MAXSIM)

     5000 continue
  end do

  write(ISDOUT,1121)

  return

  1100 format (' ',53('* '))
  1105 format (' ',43X,'WESTERN ROOT DISEASE MODEL')
  1110 format (' ',33X,'STAND ID= ',A26,5X,'MANAGEMENT ID= ',A4)
  1115 format (' ',35X,'INSIDE CENTER INFECTION SIMULATION RESULTS')
  1120 format (' ',106('-'))
  1121 format (' ')
  1155 format (1X,'YEAR',2X,'TYPE',2X,'NUM',2X,'AVG',3X,'SD',3X,&
       &'#UNINF',1X,10(4X,'#',I2))
  1160 format (1X,106('-'))

  2099 format (1X,I4,4X,A1,3X,I2,2X,F4.0,2X,F4.0,2X,F6.0,2X,10(3X,F4.0))
  2199 format (37X,10(4X,I2,1X))
  ! 2200 FORMAT (37X,10(3X,F4.1))

end
