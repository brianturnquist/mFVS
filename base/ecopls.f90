!=====================================!
!   Write economic activity records   !
!=====================================!
! BASE $Id$
subroutine ECOPLS (KEYWRD,IDISP,PARMS,J1,J2)
  implicit none

  !  THIS SUBROUTINE IS CALLED ONCE PER PROJECTION.
  !  IT OUTPUTS THE KEYWORD, DATE AND NECESSARY PARAMETERS FOR THE
  !  VARIOUS PROGNOSIS MODEL OPTIONS AND/OR EXTENSIONS WHICH ARE
  !  RECOGNIZED BY THE ECONOMIC ANALYSIS PROGRAM CHEAPO

  !     KEYWRD = CHARACTER*8 KEYWORD.
  !     IDISP  = ACTIVITY DISPOSITION CODE WHERE:
  !              -1 = DELETED
  !               0 = NOT DONE
  !              >0 = THE DATE ACTIVITY WAS DONE.
  !     PARMS  = THE PARAMETER ARRAY.
  !     J1     = IF ZERO THEN THERE ARE NO PARAMETERS FOR THE ACTIVITY.
  !              OTHERWISE THE LOCATION OF THE FIRST PARAMETER
  !     J2     = THE LOCATION OF THE LAST PARAMETER FOR THE ACTIVITY.

  ! Common blocks
  include 'ECON.F77'

  ! Variables
  integer :: J1,J2,IDISP,ITBSZ,I,JTWO,J
  parameter (ITBSZ = 7)
  real    :: PARMS(*)
  character(len=8) :: TABLE(ITBSZ)
  character(len=8) :: KEYWRD

  data  TABLE/'MECHPREP','BURNPREP','PLANT   ','NPV2    ', &
       & 'NPV3    ','CHEMICAL','PRUNE   '/

  if (.not.LECON) return

  !   IF THE KEYWORD IS IN THE TABLE OUTPUT THE NECESSARY INFORMATION

  do I = 1,ITBSZ
     if ( TABLE(I) .ne. KEYWRD ) go to 200
     if ( IDISP .le. 0 ) return

     !   THERE IS ONLY ROOM TO OUTPUT UP TO SIX(6) PARAMETERS.

     if (J1.gt.0 .and. J2.ge.J1) then
        JTWO = J2
        if ( J2-J1 .gt. 5 ) JTWO = J1+5
        write(JOSUME,100) KEYWRD,IDISP,(PARMS(J),J = J1,JTWO)
     else
        write(JOSUME,100) KEYWRD,IDISP
     end if
     100 format (A8,I6,6F10.2)
     return
     200 continue
  end do
  return
end subroutine ECOPLS
