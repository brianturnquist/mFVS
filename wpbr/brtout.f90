!===================!
!   wpbr : brtout   !
!===================!
! WPBR $Id$
subroutine BRTOUT
  implicit none

  !  Purpose:
  !  BRTOUT writes the treelist summary to output file.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  14-MAY-1999 Lance David
  !     Added Truncated Tree Height and Crown Ratio columns and
  !     reformatted the output.
  !     Added note on FVS crown ratio to header.
  !  03-JUN-1999 Lance David
  !     Implemented logical variable to control main header printing
  !     instead using cycle 0 as key, which did not work for bare ground
  !     runs.
  !  14-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 70) and species temp index variable (I3)
  !     are new.
  !  22-MAR-2001 Lance R. David (FHTET)
  !     Added reserve/escape tree status code 9 handling.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM.
  !  15-MAY-2001 Lance R. David (FHTET)
  !     Added species code (BRSPC) to output record.
  !  21-MAY-2001 Lance R. David (FHTET)
  !     Added stand id line to heading and FVS commons OUTCOM and PLOT.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'

  !.... Local variable declarations.
  !....    CNOCNK - char. rep. of tree canker status: no cankers
  !....    CNONLE - char. rep. of tree canker status: non-lethal
  !....    CPRUNE - char. rep. of tree canker status: prunable
  !....    CEXCIS - char. rep. of tree canker status: excisable
  !....    CNONSA - char. rep. of tree canker status: non-salvable
  !....    CTPKIL - char. rep. of tree canker status: top kill
  !....    CTDEAD - char. rep. of tree canker status: tree kill
  !....    CHSTAT - current tree status in character form

  ! Variables
  integer :: I1,I2,I3,I4,J,K,ISTLNB
  real    :: GGI,HTBCR,TRUNC
  character(len=8) :: CNOCNK
  character(len=8) :: CNONLE
  character(len=8) :: CPRUNE
  character(len=8) :: CEXCIS
  character(len=8) :: CNONSA
  character(len=8) :: CTPKIL
  character(len=8) :: CTDEAD
  character(len=8) :: CHSTAT
  character(len=8) :: CESCAP

  !.... Initializations.

  CNOCNK = 'NO      '
  CNONLE = ' NL     '
  CPRUNE = '  PR    '
  CEXCIS = '   EX   '
  CNONSA = '    NS  '
  CTPKIL = '     TK '
  CTDEAD = '      DD'
  CESCAP = 'ESCAPE  '

  !.... Write treelist data for white pines. If no trees or no host
  !.... trees then return.

  if (ITRN .eq.0) go to 100

  if (BRTHDR) then
     !....    Write tree status key first time only.

     BRTHDR = .false.

     write(IDTOUT,20)
     20 format('Pine Blister Rust Treelist Summary',&
          &//,'Key to tree status codes: ',&
          &' NL = non-lethal,   PR = prunable,  EX = excisable,',&
          &/,26X,' NS = non-salvable, TK = top kill,  DD = tree died,',&
          &/,26X,' NO = no cankers',&
          &//,'Key to stock type codes:   ',&
          &'1 = wild stock,   2 = F1 stock,   3 = F3 stock,   4 ='&
          &' GCOP stock',&
          &//,'Notes: (N/T/A*1000) = Thousands of needles per tree',&
          &' per acre.',/,&
          &'       Crown Ratio is from FVS and may not correlate',&
          &' with Crown Base height if pruning has occurred.',/)
     write (IDTOUT,25) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
     25 format(' STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A,/)
  end if

  !.... Write heading information.

  write(IDTOUT,30) ICYC,IY(ICYC+1)
  30 format('Cycle: ',I3,4X,'Year: ',I4 /)

  write(IDTOUT,40)
  40 format(38X,'DIAM',3X,'TREE',2X,'TRUNC',2X,'CROWN',1X,'CROWN',&
       &5X,'SUM OF',13X,'GROWTH',2X,'EXPECTED',2X,'TOTAL',/,&
       &2X,'TREE',7X,'STK',2X,&
       &'AGE',3X,'DBH',3X,'TREES/',1X,'GROWTH',1X,'HEIGHT',1X,&
       &'HEIGHT',1X,'RATIO',2X,&
       &'BASE',2X,'TARGET AREA',4X,'RUST',4X,'INDEX',3X,'NO. OF',&
       &3X,'NO. OF',3X,'TREE',/,&
       &1X,'NUMBER',2X,'SPC TYP',1X,'(YRS)',&
       &1X,'(INCH)',2X,'ACRE',2X,'(INCH)',1X,'(FEET)',1X,'(FEET)',2X,&
       &'(%)',2X,'(FEET)',1X,'(N/T/A*1000)',2X,'INDEX',3X,'(FEET)',2X,&
       &'CANKERS',2X,'CANKERS',2X,'STATUS')

  write(IDTOUT,45)
  45 format(8('-'),1X,'---',1X,'---',1X,5('-'),1X,6('-'),1X,6('-'),&
       &1X,6('-'),1X,6('-'),1X,6('-'),1X,5('-'),1X,6('-'),1X,12('-'),&
       &1X,7('-'),1X,8('-'),1X,8('-'),1X,7('-'),1X,8('-'))

  !.... Start species loop
  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 70
     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 70
     I2 = ISCT(I3,2)

     !.... Loop through for each tree.

     do J = I1,I2
        K = IND1(J)

        !....    Convert growth index from meters to feet.
        !....    Convert height to base of crown from cm to ft.

        GGI = GI(K)/.3048
        HTBCR = BRHTBC(K)/30.48
        TRUNC = (FLOAT(ITRUNC(K)))/100.0

        !....    Convert current tree canker status to character code.

        if (IBRSTAT(K).eq.1) then
           CHSTAT = CNONLE
        else if (IBRSTAT(K).eq.2) then
           CHSTAT = CPRUNE
        else if (IBRSTAT(K).eq.3) then
           CHSTAT = CEXCIS
        else if (IBRSTAT(K).eq.4) then
           CHSTAT = CNONSA
        else if (IBRSTAT(K).eq.5) then
           CHSTAT = CTPKIL
        else if (IBRSTAT(K).eq.7) then
           CHSTAT = CTDEAD
        else if (IBRSTAT(K).eq.0) then
           CHSTAT = CNOCNK
        else if (IBRSTAT(K).eq.9) then
           CHSTAT = CESCAP
        end if

        !....    Set blister rust species code index
        I4 = BRSPM(ISP(K))

        !....    Write summary statistics.

        !....    for debug
        !         WRITE(JOSTND,*) ' IDTREE=',IDTREE(K),' BRAGE=',BRAGE(K),
        !     &   ' HT=',HT(K),' GGI=',GGI

        write(IDTOUT,50) IDTREE(K),BRSPC(I4),ISTOTY(K),BRAGE(K),&
             &DBH(K),PROB(K),DG(K),HT(K),TRUNC,ICR(K),HTBCR,&
             &TSTARG(K),RI(K),GGI,ESTCAN(K),ITCAN(K),CHSTAT
        50 format(I8,1X,A3,2X,I1,3X,F4.0,2X,F5.1,1X,F6.1,1X,F6.2,&
             &1X,F6.1,1X,F6.1,4X,I2,1X,F6.1,4X,F9.1,1X,F7.5,1X,F8.2,&
             &3X,F6.1,4X,I4,1X,A8)
        60 continue
     end do

     !.... End species loop
     70 continue
  end do

  write(IDTOUT,80)
  80 format(//)

  !.... Common return.

  100 continue
  return
end
