!========================!
!   metric/rd : rdsout   !
!========================!
! METRIC-RD $Id$
subroutine RDSOUT

  !  PRODUCES OPTIONAL OUTPUT (IN A SEPARATE FILE) OF THE RESULTS
  !  OF THE MONTE CARLO SIMULATIONS OF SPREAD RATE

  !  CALLED BY :
  !     RDCNTL  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]

  !  PARAMETERS :
  !     NONE

  !  COMMON BLOCK VARIABLES :
  !     MCRATE: ARRAY CONTAINING INDIVIDUAL SIMULATION RESULTS
  !     SDRATE: ARRAY CONTAINING THE STANDARD DEVIATION OF THE RESULTS

  !  LOCAL VARIABLES :
  !     CMC:    NAME OF OUTPUT FILE
  !     JOMCS:  NUMBER OF OUTPUT FILE

  !  REVISION HISTORY:
  !  18-JUN-2001 Lance R. David (FHTET)
  !    Added Stand ID and Management ID line to header.

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
  include 'METRIC.f90'

  !      LOGICAL DEBUG
  ! Variables
  logical :: FIRSTL
  !      CHARACTER*40 CMC
  integer :: JOMCS
  character(len=1) :: CHTYPE(ITOTRR)

  data CHTYPE /'P','S','A','W'/
  data FIRSTL /.false./

  if (IROOT .eq.0) return

  !     IF MONTE CARLO OUTPUT WAS NOT REQUESTED THEN RETURN

  if (IMCOUT .le.0) return
  JOMCS = IMCOUT

  if (ISTEP .eq.2) then

     !         SINCE THIS IS CALLED FROM RDCNTL, ISTEP=2 THE FIRST TIME IT'S CALLED

     !         WRITE THE TABLE HEADERS

     write (JOMCS,1100)
     write (JOMCS,1105)
     write (JOMCS,1100)
     write (JOMCS,1110) NPLT, MGMID
     write (JOMCS,1115)
     write (JOMCS,1120)
     write (JOMCS,1155) (N,N = 1,10)
     write (JOMCS,1160)

     FIRSTL = .true.

  end if

  JYR = IY(ISTEP) - 1

  do IRRSP = MINRR,MAXRR

     if (PAREA(IRRSP) .le.0.0) goto 5000

     SUM = 0.0
     do IJ = 1,NMONT
        SUM = SUM + MCRATE(IRRSP,IJ)
        100 continue
     end do
     AVG = SUM / FLOAT(NMONT)

     SFPROB = 0.0
     IDI = IRRSP
     do I = 1,ITRN
        if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(ISP(I)))
        if (IDI .eq. IRRSP) SFPROB = SFPROB + FFPROB(I,1)
        130 continue
     end do

     if (LMTRIC) then
        write(JOMCS,2099) JYR,CHTYPE(IRRSP),NMONT,&
             &AVG*FTTOM,SDRATE(IRRSP)*FTTOM,&
             &SFPROB/ACTOHA,&
             &(MCRATE(IRRSP,J)*FTTOM,J = 1,10)
        if (FIRSTL) then
           write(JOMCS,2210) (MCTREE(IRRSP,J),J = 1,10)
           write(JOMCS,2220) (SPRQMD(IRRSP,J)*INTOCM,J = 1,10)
           FIRSTL = .false.
        else
           write(JOMCS,2199) (MCTREE(IRRSP,J),J = 1,10)
           write(JOMCS,2200) (SPRQMD(IRRSP,J)*INTOCM,J = 1,10)
        end if
     else
        write(JOMCS,2099) JYR,CHTYPE(IRRSP),NMONT,AVG,SDRATE(IRRSP),&
             &SFPROB,(MCRATE(IRRSP,J),J = 1,10)
        if (FIRSTL) then
           write(JOMCS,2210) (MCTREE(IRRSP,J),J = 1,10)
           write(JOMCS,2220) (SPRQMD(IRRSP,J),J = 1,10)
           FIRSTL = .false.
        else
           write(JOMCS,2199) (MCTREE(IRRSP,J),J = 1,10)
           write(JOMCS,2200) (SPRQMD(IRRSP,J),J = 1,10)
        end if
     end if
     if (ISTEP .ne.1) write(JOMCS,1121)

     5000 continue
  end do

  200 continue
  return

  1100 format (' ',53('* '))
  1105 format (' ',43X,'WESTERN ROOT DISEASE MODEL')
  1110 format (' ',33X,'STAND ID= ',A26,5X,'MANAGEMENT ID= ',A4)
  1115 format (' ',35X,'SPREAD RATE MONTE CARLO SIMULATION RESULTS')
  1120 format (' ',106('-'))
  1121 format (' ')
  1155 format (1X,'YEAR',2X,'TYPE',2X,'NUM',2X,'AVG',3X,'SD',3X,&
       &'DENS',4X,10(4X,'#',I2))
  1160 format (1X,106('-'))

  2099 format (1X,I4,4X,A1,3X,I2,2X,F4.2,2X,F4.2,2X,F6.2,2X,10(3X,F4.2))
  2199 format (37X,10(3X,I2,2X))
  2200 format (37X,10(3X,F4.1))

  2210 format (4X,'NUM TREES IN MC SIM.',13X,10(3X,I2,2X))
  2220 format (4X,'QMD OF TREES IN SIM.',13X,10(3X,F4.1))

end
