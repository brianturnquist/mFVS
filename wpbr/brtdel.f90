!===================!
!   wpbr : brtdel   !
!===================!
! WPBR $Id$
subroutine BRTDEL(IVAC,IREC)
  implicit none

  !  Purpose:
  !  This Blister Rust model subroutine is called from the FVS routine
  !  TREDEL.  It deletes and packs the blister rust tree lists.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: IVAC,IREC,J
  logical :: LGO,DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRTDEL',6,ICYC)
  if (DEBUG) write(JOSTND,15) ICYC
  15 format('Entering subroutine BRTDEL: cycle = ',I2)

  !.... Call BRATV to see if the Blister Rust Model is active for this
  !.... simulation.  If not, then return.

  call BRATV(LGO)
  if (.not.LGO) go to 1000

  !.... Move the Blister Rust records.

  UPMARK(IVAC) = UPMARK(IREC)
  BRHTBC(IVAC) = BRHTBC(IREC)
  GI(IVAC) = GI(IREC)
  RI(IVAC) = RI(IREC)
  BRGD(IVAC) = BRGD(IREC)
  BRPB(IVAC) = BRPB(IREC)
  ESTCAN(IVAC) = ESTCAN(IREC)
  TSTARG(IVAC) = TSTARG(IREC)
  ITCAN(IVAC) = ITCAN(IREC)
  ILCAN(IVAC) = ILCAN(IREC)
  BRAGE(IVAC) = BRAGE(IREC)
  ISTOTY(IVAC) = ISTOTY(IREC)
  IBRTID(IVAC) = IBRTID(IREC)
  ICRED(IVAC) = ICRED(IREC)
  IBRSTAT(IVAC) = IBRSTAT(IREC)

  do J = 1,10
     ISTCAN(J,IVAC) = ISTCAN(J,IREC)
     GIRDL(J,IVAC) = GIRDL(J,IREC)
     DUP(J,IVAC) = DUP(J,IREC)
     DOUT(J,IVAC) = DOUT(J,IREC)
     999 continue
  end do

  !.... Common return.

  1000 continue
  if (DEBUG) write(JOSTND,1010) ICYC
  1010 format('Leaving subroutine BRTDEL: cycle = ',I2)
  return
end
