!================================!
!   Stand visualization: SVKEY   !
!================================!
! BASE $Id$
subroutine SVKEY(KEYWRD,LNOTBK,ARRAY)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     D. ROBINSON   -- ESSA        -- MAY 2005
  !     S.SCHAROSCH   -- Abacus      -- MAR 2008

  !     PROCESS VISUALZATION KEYWORD.

  !     ALL ARGUMENTS ARE INPUT, STD FROM INITRE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'SVDATA.f90'

  ! Variables
  logical :: LNOTBK(12),LOPEN
  real    :: ARRAY(12)
  character(len=8) :: KEYWRD
  character(len=10) :: SUFFIX

  if (LNOTBK(1)) IPLGEM = IFIX(ARRAY(1))
  if (IPLGEM.lt.0 .or. IPLGEM.gt.3) IPLGEM = 1

  if (LNOTBK(2)) IGRID = INT(ARRAY(2))
  if (IGRID.gt.256) IGRID = 256
  if (IGRID.lt.0) IGRID = 0

  if (LNOTBK(7)) JSVOUT = -1

  if (LNOTBK(5)) ICOLIDX = INT(ARRAY(5))
  if (ICOLIDX.gt.20) ICOLIDX = 20

  !     Allow negative color index to pass through.
  !     Will be interpreted in SVGRND as a request for
  !     a uniform ground surface of the indicated color.

  !>>>      IF (ICOLIDX.LT.0) ICOLIDX=2

  write (JOSTND,10) KEYWRD,IPLGEM,IGRID
  10 format (/A8,'   PRODUCE SVS-READY DATA'/T12, &
       & 'PLOT GEOMETRY CODE = ',I2, &
       & ' (0=SQUARE, IGNORE POINTS; 1=SUBDIVIDED SQUARE; ', &
       & '2=ROUND, IGNORE POINTS; 3=SUBDIVIDED CIRCLE)'/T12, &
       & 'GROUND FILE GRID RESOLUTION (ZERO IMPLIES NO GROUND FILE)= ', &
       & I3)
  IRPOLES = IFIX(ARRAY(3))
  if (IRPOLES.gt.1) IRPOLES = 1
  if (IRPOLES.lt.0) IRPOLES = 0
  IDPLOTS = IFIX(ARRAY(4))
  if (IDPLOTS.gt.1) IDPLOTS = 1
  if (IDPLOTS.lt.0) IDPLOTS = 0
  IMETRIC = IFIX(ARRAY(6))
  if (IMETRIC.gt.1) IMETRIC = 1
  if (IMETRIC.lt.0) IMETRIC = 0

  if (IRPOLES.eq.0) then
     write (JOSTND,15) 'RANGE POLES ARE NOT DRAWN.'
  else
     write (JOSTND,15) 'RANGE POLES ARE DRAWN.'
  end if
  if (IDPLOTS.eq.0) then
     write (JOSTND,15) 'SUBPLOT BOUNDARIES ARE NOT DRAWN.'
  else
     write (JOSTND,15) 'SUBPLOT BOUNDARIES ARE DRAWN.'
  end if
  if (IMETRIC.eq.0) then
     write (JOSTND,15) 'OUTPUT DATA ARE IMPERIAL.'
  else
     write (JOSTND,15) 'OUTPUT DATA ARE METRIC.'
  end if
  write (JOSTND,16) ICOLIDX
  if (LNOTBK(7)) JSVOUT = -1
  if (JSVOUT .lt. 0) write (JOSTND,15) &
       & 'SVS RUNS BUT NO OUTPUT FILES ARE PRODUCED.'

  15 format (T12,A)
  16 format (T12,'COLOR INDEX= ',I4)

  if (JSVOUT.lt.0) return

  JSVOUT = 90
  inquire(unit = JSVOUT,opened = LOPEN)
  if (.not.LOPEN) then
     SUFFIX = '_index.svs'
     open(unit = JSVOUT,file = trim(KWDFIL)//SUFFIX, &
          & status = "replace",err = 19)
     goto 21
     19 continue
     write (JOSTND,20) trim(KWDFIL)//SUFFIX
     20 format (/T12,'**** FILE OPEN ERROR FOR FILE: ',A)
     call RCDSET (2,.true.)
     JSVOUT = 0
     return
     21 continue
     write (JSVOUT,'(''#TREELISTINDEX'')')
  end if
  return
end subroutine SVKEY
