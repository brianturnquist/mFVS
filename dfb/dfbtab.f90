!==================!
!   dfb : dfbtab   !
!==================!
! DFB $Id$
subroutine DFBTAB(CFTVOL)
  implicit none

  !  PRINTS OUT THE DOUGLAS-FIR BEETLE SUMMARY TABLE.

  !  CALLED BY :
  !     DFBMRT [DFB]

  !  CALLS :
  !     DFBIND  (SUBROUTINE)  [DFB]

  !  PARAMETERS :
  !     CFTVOL - ARRAY THAT HOLDS THE CUBIC FOOT VOLUME LOST DUE TO DFB
  !              FOR EACH DBH CLASS.

  !  LOCAL VARIABLES :
  !     DCLASS - ARRAY THAT HOLDS THE DIAMETER CLASS HEADINGS THAT ARE
  !              TO BE PRINTED IN THE SUMMARY TABLE.
  !     I      - INDEX,  COUNTER.
  !     TDEAD  - HOLDS THE TOTAL NUMBER OF DOUGLAS-FIR KILLED BY THE DFB
  !              (TREES/ACRE).
  !     TLIVE  - HOLDS THE TOTAL NUMBER OF LIVE DOUGLAS-FIR (TREES/ACRE).
  !     DCNDX  - THE DBH CLASS OF THE CURRENT TREE RECORD.
  !     TTOTL  - HOLDS THE TOTAL NUMBER OF LIVE TREES (TREES/ACRE).
  !     TVOL   - HOLDS THE TOTAL CUBIC FOOT VOLUME LOST DUE TO THE DFB.
  !     TOTGRN - ARRAY THAT HOLDS THE TOTAL LIVE TREES/ACRE FOR EACH DBH
  !              CLASS IN THE STAND.

  !  COMMON BLOCK VARIABLES USED :
  !     DBH    - (ARRAYS)  INPUT
  !     DEADDF - (DFBCOM)  INPUT
  !     DFKILL - (DFBCOM)  INPUT
  !     ICYC   - (CONTRL)  INPUT
  !     ITRN   - (CONTRL)  INPUT
  !     IY     - (CONTRL)  INPUT
  !     IYOUT  - (DFBCOM)  INPUT
  !     JODFB  - (DFBCOM)  INPUT
  !     JODFBX - (DFBCOM)  INPUT
  !     LINPRG - (DFBCOM)  INPUT/OUTPUT
  !     LINV   - (DFBCOM)  INPUT/OUTPUT
  !     LIVEDF - (DFBCOM)  INPUT
  !     PREKLL - (DFBCOM)  INPUT
  !     PROB   - (ARRAYS)  INPUT

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'DFBCOM.f90'

  ! Variables
  integer :: I,DCNDX

  real    :: TOTGRN(20),CFTVOL(20),TDEAD,TLIVE,TTOTL,TVOL

  character(len=8) :: DCLASS(20)

  data DCLASS / ' 1 -  3',' 3 -  5',' 5 -  7',' 7 -  9',' 9 - 11',&
       &'11 - 13','13 - 15','15 - 17','17 - 19','19 - 21',&
       &'21 - 23','23 - 25','25 - 27','27 - 29','29 - 31',&
       &'31 - 33','33 - 35','35 - 37','37 - 39','39 - 40+' /

  !     INITIALIZE TOTGRN

  do I = 1,20
     TOTGRN(I) = 0.0
     100 continue
  end do

  !     BREAK PROGNOSIS PROB ARRAY INTO DBH CLASSES.

  do I = 1, ITRN
     call DFBIND (DBH(I), DCNDX)
     TOTGRN(DCNDX) = TOTGRN(DCNDX) + PROB(I)
     250 continue
  end do

  !     PRINT DOUGLAS-FIR BEETLE SUMMARY TABLE HEADERS.

  write (JODFB,500)
  write (JODFB,510)

  if (LINV .and. LINPRG .and. ICYC .eq.1)&
       &write (JODFB,515) INT(PREKLL), IYOUT

  !     TURN OFF OUTBREAK IN PROGRESS.

  LINV = .false.
  LINPRG = .false.

  !     PRINT TABLE HEADERS.

  write (JODFB,520) IY(ICYC)
  write (JODFB,525) DFKILL
  write (JODFB,*)
  write (JODFB,530)

  500 format (//,'             DOUGLAS-FIR BEETLE MORTALITY')
  510 format (1X,'---------------------------------------------------',&
       &'-------')
  515 format (1X,I4,' TREES WERE READ IN AS DEAD AFTER ',I1,' YEAR(S) ',&
       &'OF DFB MORTALITY.')
  520 format (1X,'YEAR OF THE OUTBREAK             : ',I5)
  525 format (1X,'PROJECTED MORTALITY (TREES/ACRE) : ',F5.1)
  530 format (1X,'             TOTAL         DF          DF     ',&
       &'     DF    ',/,&
       &1X,'           TREES/ACRE  TREES/ACRE  TREES/ACRE ',&
       &' CU/FT VOL ',/,&
       &1X,'DBH CLASS    ALIVE       ALIVE       KILLED   ',&
       &'    LOST   ',/,&
       &1X,'---------  ----------  ----------  ---------- ',&
       &' ----------')

  !     IF USER WANTS POST PROCESSOR OUTPUT THEN PRINT THE YEAR TO THE
  !     POST PROCESSOR FILE.

  if (JODFBX .gt.0) write (JODFBX,540) IY(ICYC)
  540 format (I4)

  !     INITIALIZE THE VARIABLES THAT KEEP THE TOTALS.

  TLIVE = 0.0
  TDEAD = 0.0
  TTOTL = 0.0
  TVOL = 0.0

  !     PRINT OUT SUMMARY TABLE INFORMATION AND CALCULATE TOTALS.

  do I = 1,20
     TOTGRN(I) = TOTGRN(I) - DEADDF(I)
     write (JODFB,700) DCLASS(I), TOTGRN(I), LIVEDF(I),&
          &DEADDF(I), CFTVOL(I)

     !        PRINT POST PROCESSOR OUTPUT IF USER USED DFBECHO KEYWORD.

     if (JODFBX .gt.0) write (JODFBX,700) DCLASS(I),&
          &TOTGRN(I), LIVEDF(I), DEADDF(I), CFTVOL(I)
     TLIVE = TLIVE + LIVEDF(I)
     TDEAD = TDEAD + DEADDF(I)
     TTOTL = TTOTL + TOTGRN(I)
     TVOL = TVOL  + CFTVOL(I)
     600 continue
  end do

  !     PRINT OUT THE SUMMARY TABLE TOTALS.

  write (JODFB,710)
  write (JODFB,720) TTOTL, TLIVE, TDEAD, TVOL

  !     PRINT SUMMARY TOTALS TO POST PROCESSOR OUTPUT IF USER USED
  !     DFBECHO KEYWORD.

  if (JODFBX .gt.0) then
     write (JODFBX,710)
     write (JODFBX,720) TTOTL, TLIVE, TDEAD, TVOL
     write (JODFBX,*)
  end if

  700 format (3X,A8,3X,F6.1,6X,F6.1,6X,F6.1,5X,F7.1)
  710 format (2X,'           ----------  ----------  ---------- ',&
       &' ----------')
  720 format (14X,F6.1,6X,F6.1,6X,F6.1,5X,F7.1)

  1000 continue
  return
end
