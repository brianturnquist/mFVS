!=================!
!   ci : esinit   !
!=================!
! CI $Id$
subroutine ESINIT
  implicit none

  !     CALLED FROM INITRE, ONLY ONCE, TO INITIALIZE REGEN. MODEL.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'
  include 'ESRNCM.f90'
  include 'ESWSBW.f90'

  equivalence (NSTK,SUMPRB)
  ! Variables
  logical :: LTEMP,LKECHO
  character(len=256) :: CNAME
  integer :: NSTK,I,KODE,JSTND,ICOUNT
  real    :: X

  !     SPECIES LIST FOR CENTRAL IDAHO VARIANT.

  !     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
  !     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
  !     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
  !     4 = GRAND FIR (GF)                   ABIES GRANDIS
  !     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
  !     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
  !     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
  !     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
  !     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
  !    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
  !    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
  !    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
  !    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
  !    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
  !    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
  !    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
  !    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
  !    18 = OTHER SOFTWOODS (OS)
  !    19 = OTHER HARDWOODS (OH)

  do I = 1,MAXSP
     HTADJ(I) = 0.0
     XESMLT(I) = 1.0
     10 continue
  end do
  ICOUNT = 0
  ITRNRM = 0
  NSTK = 0
  NBWHST = 0
  STOADJ = 0.0
  CONFID = 5.0
  LINGRW = .false.
  LAUTAL = .false.
  LSPRUT = .true.
  IPRINT = 1
  INADV = 0
  LOAD = 0
  IBLK = 0
  MINREP = 50
  KDTOLD = -99
  IPINFO = 0
  NTALLY = 0
  IDSDAT = -9999
  IYRLRM = -9999
  XTES = 0.0
  THRES1 = 0.10
  THRES2 = 0.30
  call ESRNSD (.false.,X)
  NPTIDS = 0
  inquire (UNIT = JOREGT,OPENED = LTEMP)
  if (.not.LTEMP) then
     CNAME = TRIM(KWDFIL)//'_RegenRpt.txt'
     !        CALL MYOPEN (JOREGT,CNAME,4,133, 0,1,1,0,KODE)
     call MYOPEN (JOREGT,CNAME,1,133, 0,1,1,0,KODE)
     do while (KODE /= 0)
        ICOUNT = ICOUNT + 1
        call SLEEP(100)
        call MYOPEN (JOREGT,CNAME,1,133, 0,1,1,0,KODE)
        if (ICOUNT .ge.5) then
           write(*,'(''OPEN FAILED FOR '',I4)') JOREGT
           exit
        end if
     end do
  end if
  return

  !     CALLED FROM INITRE WHEN NOTREES KEYWORD IS ENTERED.

  entry ESEZCR (JSTND,LKECHO)
  INADV = 1
  if (LKECHO)write(JSTND,30)
  30 format(T12,'(EZCRUISE OPTION IN REGENERATION ESTABLISHMENT ',&
       &'MODEL IS AUTOMATICALLY INVOKED.)'  )

  return
end
