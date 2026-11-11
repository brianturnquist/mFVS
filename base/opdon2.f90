!=======================================!
!   Update activity completion status   !
!=======================================!
! BASE $Id$
subroutine OPDON2(IACTK,IDT,IYR1,IYR2,ISQNUM,KODE)
  implicit none

  !     OPDON2 IS USED TO SET THE STATUS OF AN OPTION FOUND AS
  !     DESCRIBED UNDER OPGET2 AS 'DONE', OR THE VALUE OF IDT.
  !     THE ARGUMENTS:

  !     IACTK, IYR1, IYR2, ISQNUM, AND KODE ARE SAME AS ABOVE.
  !     IDT   = THE YEAR THE ACTIVITY WAS ACCOMPLISHED.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  integer :: KODE,ISQNUM,IYR1,IYR2,IDT,IACTK,IFIND,NTIMES,I2,II,I,ID

  !     PREFORM THE SEARCH FOR THE ACTIVITY IN ASSENDING DATE ORDER.

  KODE = 0
  IFIND = 0
  NTIMES = 0
  I2 = IMGL-1
  do II = 1,I2
     I = IOPSRT(II)
     ID = IDATE(I)
     if (ID.lt.IYR1 .or. ID.gt.IYR2 .or. IACTK.ne.IACT(I,1)) goto 40
     if (IACT(I,4).ne.0) goto 40
     NTIMES = NTIMES+1
     if (ISQNUM.gt.0) goto 30
     IFIND = I
     goto 40
     30 continue
     if (ISQNUM.ne.NTIMES) goto 40
     IFIND = I
     goto 50
     40 continue
  end do
  50 continue
  if (IFIND.gt.0) goto 60
  KODE = 1
  return

  !     THE SEARCH IS COMPLETE - PROCEED WITH THE OPERATION.

  60 continue
  ISEQDN = ISEQDN+1
  ISEQ(IFIND) = ISEQDN
  IACT(IFIND,4) = IDT
  return
end subroutine OPDON2
