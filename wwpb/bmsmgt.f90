!===================!
!   wwpb : bmsmgt   !
!===================!
! WWPB $Id$
subroutine BMSMGT (IYR)

  !     CALLED FROM BMDRV

  ! **BMSMGT    Date of last revision:  June 22, 1994

  !  Routine to remove fresh slash (created from FVS-level harvest or BM-
  !  model salvage cut (but not from sanititation cut)). Model removes
  !  user-defined proportion of DOWNED (only) dead wood from host and non-
  !  host pools.

  !  Definitions:
  !     ISC:    Loop counter over size classes
  !     JSC:    Size class < 3 or > 3
  !     PSLREM: Proportion of slash to remove that is < 3 or >3 in
  !                 -given by keyword.

  !  Common block variables and parameters:

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPCNTL.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: ISC,JSC
  integer :: SCNT,MYLST(MXSTND)
  logical :: LOK
  real    :: PRMS(2)
  real    :: PSLREM(2)

  save

  if (LBMDEB) write(JBMBPR,10) IYR
  10 format(' Begin BMSMGT: Year= ',I5)

  !     Initializations

  IYR1 = 0
  NPRMS = 2
  IYR1 = IYR
  call GPGET2 (315,IYR1,7,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
  if (LOK) then
     PSLREM(1) = PRMS(1)
     PSLREM(2) = PRMS(2)

     do I = 1, SCNT

        ISTD = MYLST(I)
        if (.not.STOCK(ISTD) .or. ISTD .le.0) goto 200

        !         Remove some dead wood from downed dead wood, but only for the
        !         current year.
        !         (remember that DWPHOS is all > 3in, but is in 3 size classes and
        !          DDWP is only <3 or > 3in)

        do ISC = 1, MXDWHZ
           JSC = MIN0(ISC,MXDWSZ)

           DWPHOS(ISTD,2,ISC) = DWPHOS(ISTD,2,ISC) * (1 - PSLREM(2))

           if (ISC .le. MXDWSZ)&
                &DDWP(ISTD,ISC,1) = DDWP(ISTD,ISC,1) * (1 - PSLREM(JSC))

           45 continue
        end do

        200 continue
     end do
  end if

  if (LBMDEB) write(JBMBPR,99) IYR
  99 format(' End BMSMGT: Year= ',I5)

  return
end
