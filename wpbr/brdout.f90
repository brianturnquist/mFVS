!===================!
!   wpbr : brdout   !
!===================!
! WPBR $Id$
subroutine BRDOUT
  implicit none

  !  Purpose:
  !  Writes a Blister Rust Model All-Host DBH Class Statistics table at the
  !  end of the FVS listing.  This is the same information that is in the
  !  WPBR Summary table but it is broken down by 2-inch DBH classes.
  !  This summary includes all host species, they are not separated.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  14-SEP-2000 Lance David (FHTET)
  !     Modified table heading. Changed "White Pine" to "Pine", because
  !     the model now recognizes other pine host species and all are
  !     represented in this table.
  !     Changed version number from 1.0 to 1.1
  !  04-MAY-2001 Lance R. David (FHTET)
  !     Added species dimension to BROUT (only used for year).
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM.
  !  21-MAY-2001 Lance R. David (FHTET)
  !     Added stand id line to heading and FVS commons OUTCOM and PLOT.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'BRCOM.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'

  ! Variables
  character(len=3) :: BRVER
  !.... Local variable declarations.

  integer :: I3,I4,J,K,ISTLNB
  logical :: BRGO

  !.... See if Blister Rust Model is active. If not or no trees then return.

  call BRATV(BRGO)
  if (.not. BRGO .or. ITRN .eq.0) go to 1000
  BRVER = '1.2'

  !.... Write heading information.

  write(JOSTND,100)
  write(JOSTND,102) BRVER
  write(JOSTND,103)
  write (JOSTND,15) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
  15 format('STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A)
  write(JOSTND,104)

  100 format(//)
  101 format(' ')
  102 format(T36,'*  *  *  WHITE PINE BLISTER RUST MODEL:'&
       &' Version ',A3,'  *  *  *')
  103 format(T42,'Stand DBH Class Statistics for Blister Rust Hosts')
  104 format(131('-'))

  105 format('YEAR',60X,'2-INCH DBH CLASS')
  106 format(5('-'),8X,118('-'))
  107 format(F5.0,9X,'0 - 2.9"',4X,'3 - 4.9"',4X,'5 - 6.9"',4X,&
       &'7 - 8.9"',4X,'9 - 10.9"',2X,'11 - 12.9"',2X,'13 - 14.9"',2X,&
       &'15 - 16.9"',2X,'17 - 18.9"',4X,'19"  +')
  108 format(11X,10(2X,10('-')))
  110 format('A.C/T All',2X,10(6X,F6.1))
  111 format('A.C/T Leth',1X,10(6X,F6.1))
  112 format('A.C/T Exp',2X,10(6X,F6.1))
  114 format('P.Inf All',2X,10(8X,F4.2))
  115 format('P.Inf Leth',1X,10(8X,F4.2))
  117 format('T/A Clean',2X,10(6X,F6.1))
  118 format('T/A NonLeth',10(6X,F6.1))
  119 format('T/A Prune',2X,10(6X,F6.1))
  120 format('T/A Excise',1X,10(6X,F6.1))
  121 format('T/A NonSalv',10(6X,F6.1))
  122 format('T/A TopKill',10(6X,F6.1))
  123 format('T/A Dead',3X,10(6X,F6.1))
  124 format('T/A Total',2X,10(6X,F6.1))

  !.... Write 2" DBH class summary data; repeat DBH class headings for
  !.... each year because it contains the year in the heading.
  !....
  !.... Just to access the year from the summary array, set species
  !.... index to the first blister rust host species.
  I3 = 0
  300 I3 = I3 + 1
  if (BRSPM(I3) .eq.0) go to 300
  I4 = BRSPM(I3)

  do K = 1,NCYC+1
     write(JOSTND,105)
     write(JOSTND,106)
     write(JOSTND,107) BROUT(I4,01,K)
     write(JOSTND,108)
     write(JOSTND,110) (BRDBHO(J,01,K),J = 1,10)
     write(JOSTND,111) (BRDBHO(J,02,K),J = 1,10)
     write(JOSTND,112) (BRDBHO(J,03,K),J = 1,10)
     write(JOSTND,101)
     write(JOSTND,114) (BRDBHO(J,04,K),J = 1,10)
     write(JOSTND,115) (BRDBHO(J,05,K),J = 1,10)
     write(JOSTND,101)
     write(JOSTND,117) (BRDBHO(J,06,K),J = 1,10)
     write(JOSTND,118) (BRDBHO(J,07,K),J = 1,10)
     write(JOSTND,119) (BRDBHO(J,08,K),J = 1,10)
     write(JOSTND,120) (BRDBHO(J,09,K),J = 1,10)
     write(JOSTND,121) (BRDBHO(J,10,K),J = 1,10)
     write(JOSTND,122) (BRDBHO(J,11,K),J = 1,10)
     write(JOSTND,123) (BRDBHO(J,12,K),J = 1,10)
     write(JOSTND,124) (BRDBHO(J,13,K),J = 1,10)
     write(JOSTND,101)
     800 continue
  end do

  !.... Common return.

  1000 continue
  return
end
