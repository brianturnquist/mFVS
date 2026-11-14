!=================!
!   rd : rdrout   !
!=================!
! RD $Id$
subroutine RDROUT
  implicit none

  !  ****** NOTICE ********* NOTICE ******* NOTICE ****** NOTICE *******

  !  *** 3/24/2015 Implementation of General Report Writer facility  ***
  !  *** has made this subroutined obsolete. It is no longer needed. ***

  !  WRITES FINAL ROOT DISEASE SUMMARY OUTPUT.  ALSO WRITES DETAILED REPORT IF
  !  THE RRDOUT KEYWORD IS USED.

  !  CALLED BY :
  !     MAIN    [PROGNOSIS]

  !  CALLS     :
  !     NONE

  !  Revision History :
  !   11/06/89 - Last revision date.
  !   09/02/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'CONTRL.f90'
  include 'RDCOM.f90'

  ! Variables
  character(len=132) :: DSTRNG

  if (IROOT .eq.0.or. IRUNIT .eq. JOSTND) return

  !     WRITE END OF FILE MARKER THEN REWIND IRUNIT

  ENDFILE IRUNIT
  rewind IRUNIT

  125 continue
  read (IRUNIT,130,end = 200) DSTRNG
  130 format(A132)
  write (JOSTND,130) DSTRNG
  goto 125

  200 continue
  rewind IRUNIT

  !     WRITE DETAILED OUPUT IF ASKED FOR.

  if (IRDOUT .eq.1) then

     !        WRITE END OF FILE MARKER THEN REWIND IOUNIT.

     ENDFILE IOUNIT
     rewind  IOUNIT

     !        WRITE IOUNIT (DETAILED REPORT) TO END OF JOSTND.

     300 continue
     read (IOUNIT, 130, end = 400) DSTRNG
     write (JOSTND, 130) DSTRNG
     goto 300

     400 continue
     rewind IOUNIT
  end if

  return
end
