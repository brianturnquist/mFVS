!===================!
!   wpbr : brcout   !
!===================!
! WPBR $Id$
subroutine BRCOUT
  implicit none

  !  Purpose:
  !  BRCOUT write a detailed summary of cankers to output file.
  !  The canker and tree information is printed in  FEET and INCHES.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  17-MAY-1999 Lance David
  !     Reformatted the output for narrower and closer columns.
  !     Added note on FVS crown ratio to header.
  !  03-JUN-1999 Lance David
  !     Implemented logical variable to control main header printing
  !     instead using cycle 0 as key, which did not work for bare ground
  !     runs.
  !  13-SEP-2000 Lance David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 450) and species temp index variable (I3)
  !     are new.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM. Instead of just being and indicator of a
  !     species being a host, BRSPM holds the array index value for that
  !     species and is used to access all species-specific BR arrays.
  !  21-MAY-2001 Lance R. David (FHTET)
  !     Added stand id line to heading and FVS common OUTCOM.F77

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'OUTCOM.f90'

  !.... Local variable declarations.
  !....    LPRALL - logical variable used to control when all columns
  !....             are printed.
  !....    HTBCR  - height to base of crown in feet.
  !....    CANUP  - canker's distance above ground in feet.
  !....    CANOUT - canker's distance out on branch from bole in inches.
  !....    GIRD   - percent of tree's bole girdled by canker.
  !....    CINACT - char. representation of canker status inactive
  !....    CNONLE - char. representation of canker status non-lethal
  !....    CPRUNE - char. representation of canker status prunable
  !....    CEXCIS - char. representation of canker status excisable
  !....    CNONSA - char. representation of canker status non-salvable
  !....    CTPKIL - char. represantation of canker status top kill
  !....    CTDEAD - char. represenataion of canker status tree kill
  !....    CHSTAT - current tree status in character form

  ! Variables
  logical :: LPRALL
  integer :: I1,I2,I3,ISTLNB,J,JCSTAT,K,NCAN,NLCAN
  real    :: HTBCR,CANUP,CANOUT,GIRD
  character(len=8) :: CINACT
  character(len=8) :: CNONLE
  character(len=8) :: CPRUNE
  character(len=8) :: CEXCIS
  character(len=8) :: CNONSA
  character(len=8) :: CTPKIL
  character(len=8) :: CTDEAD
  character(len=8) :: CHSTAT

  !.... Initializations.

  CINACT = 'IN      '
  CNONLE = ' NL     '
  CPRUNE = '  PR    '
  CEXCIS = '   EX   '
  CNONSA = '    NS  '
  CTPKIL = '     TK '
  CTDEAD = '      DD'

  !.... If no trees or no host species in the treelist, then return.

  if (ITRN .eq.0) go to 500

  !.... Write header.

  if (BRCHDR) then

     !....    Write canker status key first time only.

     BRCHDR = .false.

     write(IDCOUT,20)
     20 format('Pine Blister Rust Detailed Canker Summary',&
          &//,'Key to canker status codes: ',&
          &' NL = non-lethal,   PR = prunable,  EX = excisable,',&
          &/,T29,' NS = non-salvable, TK = top kill,  DD = tree died,',&
          &/,T29,' IN = canker inactivated',&
          &//,'Note: Crown Ratio is from FVS and may not correlate',&
          &' with Crown Base height',/,&
          &'      if pruning has occurred.',/)
     write (IDCOUT,25) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
     25 format('STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A,/)
  end if

  write(IDCOUT,30) ICYC,IY(ICYC+1)
  30 format('Cycle: ',I2,4X,'Year: ',I4 /)

  write(IDCOUT,100)
  100 format(19X,'CANK   CANK',19X,'TREE',13X,'CROWN',/,&
       &'  TREE   TREE CANK DIST.  DIST.   PCNT  CANKER   HGT ',&
       &'  DBH  CROWN  BASE',/,&
       &' NUMBER  INDX INDX UP(FT) OUT(IN) GIRD  STATUS   (FT)',&
       &'  (IN) RATIO  (FT)',/,&
       &'-------- ---- ---- ------ ------- ---- -------- -----',&
       &' ----- ----- -----')

  !.... Start species loop

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 450

     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 450
     I2 = ISCT(I3,2)

     do J = I1,I2
        K = IND1(J)
        NLCAN = ILCAN(K)
        if (NLCAN.eq.0) then

           !....       Tree has no cankers. skip it.

           continue
        else
           LPRALL = .true.
           do NCAN = 1,NLCAN

              !....          Convert current canker status to character code.

              JCSTAT = ISTCAN(NCAN,K)
              if (JCSTAT.eq.1) then
                 CHSTAT = CNONLE
              else if (JCSTAT.eq.2) then
                 CHSTAT = CPRUNE
              else if (JCSTAT.eq.3) then
                 CHSTAT = CEXCIS
              else if (JCSTAT.eq.4) then
                 CHSTAT = CNONSA
              else if (JCSTAT.eq.5) then
                 CHSTAT = CTPKIL
              else if (JCSTAT.eq.7) then
                 CHSTAT = CTDEAD
              else if (JCSTAT.eq.-1) then
                 CHSTAT = CINACT
              else
                 go to 300
              end if

              !....          Get other canker information.

              GIRD = GIRDL(NCAN,K)
              CANUP = DUP(NCAN,K)/30.48
              CANOUT = DOUT(NCAN,K)/2.54

              if (LPRALL) then

                 !....             Print all columns of information.
                 !....             When a tree has more than one canker, the last
                 !....             four columns are printed for the first canker only.

                 LPRALL = .false.
                 HTBCR = BRHTBC(K)/30.48
                 write(IDCOUT,200) IDTREE(K),K,NCAN,CANUP,CANOUT,&
                      &GIRD,CHSTAT,HT(K),DBH(K),ICR(K),HTBCR
                 200 format(I8,1X,I4,3X,I2,2X,F5.1,3X,F5.1,1X,&
                      &F4.0,1X,A8,1X,F5.1,1X,F5.1,3X,I3,1X,F5.1)
              else

                 write(IDCOUT,210) IDTREE(K),K,NCAN,CANUP,CANOUT,&
                      &GIRD,CHSTAT
                 210 format(I8,1X,I4,3X,I2,2X,F5.1,3X,F5.1,1X,F4.0,&
                      &1X,A8)
              end if
              300 continue
           end do
        end if
        320 continue
     end do

     !.... End of species loop
     450 continue
  end do

  write(IDCOUT,460)
  460 format(//)

  !.... Common return.

  500 continue
  return
end
