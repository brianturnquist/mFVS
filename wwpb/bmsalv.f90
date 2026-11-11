!===================!
!   wwpb : bmsalv   !
!===================!
! WWPB $Id$
subroutine BMSALV (IYR)

  !     CALLED FROM BMDRV
  !     CALLS:  BMSLSH

  ! **BMSALV    Date of last revision:  09/28/05

  !     Modified input data fields to coresspond with SANITISE keyword,
  !     and modified SALVAGE keyword to consider minimum volume to
  !     conduct Salvage operation (RNH May98)

  !  Definitions:
  !     IAG:    Loop counter over age classes
  !     IPC:    Loop counter over pool types (fast, med, slow)
  !     ISC:    Loop counter over size classes
  !     MINSC:  Minimum size class for cut
  !     MAXSC:  Maximum size class for cut
  !     REMOVE: Amount of standing dead volume removed for one age & size
  !     SUM:    Total amount of standing dead volume (all classes)
  !     THRVOL -min. Vol. to perform salvage (RNH MAy98)
  !     VREMOV: Amount of standing dead volume removed over age and size classes

  !  Common block variables and parameters:

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPCNTL.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'
  include 'BMPCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: ISC
  integer :: MINSC,MAXSC,MAXAGE
  integer :: SCNT,MYLST(MXSTND)
  integer :: MINLIV,MAXLIV
  logical :: LOK
  real    :: PRMS(7)
  real    :: REMOVE
  real    :: VREMOV,EFFC,THRVOL

  save

  if (LBMDEB) write(JBMBPR,10) IYR
  10 format(' Begin BMSALV: Year= ',I5)

  !     Initializations

  IYR1 = 0
  !      NPRMS = 4

  !     Added one parameter so new total in 5 parameters (RNH May98)
  NPRMS = 5
  IYR1 = IYR
  MAXPRM = 7
  MAXLIV = -1
  MINLIV = -1
  call GPGET2 (318,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
  if (LOK) then
     MINSC = INT(PRMS(1))
     MAXSC = INT(PRMS(2))
     MAXAGE = INT(PRMS(3))
     THRVOL = PRMS(4) !  Min. Vol. to perform Salvage (RNH May98)
     !        EFFC = PRMS(4)

     !     Efficiency is now t 5th parameter in Field 7 of Keyword (RNH MAy98)
     EFFC = PRMS(5)
     if (EFFC .gt.1.0) EFFC = 1.0
     if (EFFC .lt.0.0) EFFC = 0.0

     !       DON'T BOTHER DOING THE REST OF THE LOOP IF EFFICIENCY = 0
     if (EFFC .eq.0.0) return

     do ISIZ = 1,NSCL
        if (L2D(ISIZ) .eq. MINSC .and. MINLIV .eq. -1) MINLIV = ISIZ
        if (L2D(ISIZ) .eq. MAXSC .and. MAXLIV .eq. -1) MAXLIV = ISIZ
        22 continue
     end do

     !     Beginning of main stand loop

     do I = 1, SCNT

        ISTD = MYLST(I)
        if (.not.STOCK(ISTD).or.ISTD.le.0) goto 200

        !     Check for Min. volume for salvage cut.  This routine was lifted
        !     from an ESSA commented-out r0outine at the end of this module
        !     (RNH May98)

        SUM = 0.0
        !     Total standing dead volume available for cut
        do IPC = 1,MXDWPC
           do ISC = MINSC,MAXSC
              !          DO 20 IAG= 1,MXDWAG   Fixed 10/01 ajm.  Only dead trees
              !                                less than user-defined threshold should be
              !                                added to eligibility pool!  User-entered age
              !                                {PRMS(3)=[MAXAGE]} is set to a class (1-5) in BMPPIN.
              do IAG = 1,MAXAGE
                 SUM = SUM + SDWP(ISTD,IPC,ISC,IAG)
                 20 continue
              end do
              25 continue
           end do
           30 continue
        end do
        SUM = SUM * EFFC

        !     If not enough volume to warrent a salvage cut then return
        if (SUM .lt. THRVOL) go to 200

        !         Remove some standing dead wood from various classes
        do IPC = 1,MXDWPC
           VREMOV = 0.0
           do ISC = MINSC,MAXSC
              do IAG = 1,MAXAGE
                 REMOVE = SDWP(ISTD,IPC,ISC,IAG) * EFFC

                 VREMOV = VREMOV + REMOVE
                 SDWP(ISTD,IPC,ISC,IAG) = SDWP(ISTD,IPC,ISC,IAG) - REMOVE
                 40 continue
              end do
              45 continue
           end do

           !           Calculate amount of slash produced from salvage

           !            CALL BMSLSH (IPC,1,VREMOV,ISTD)
           !           **** Integer argument "1" caused fortran error (arithmetic underflow...
           !                passed 1E-45 into BMSLSH via TTREE). Changed to a real number in line below.
           !                MJO March 1998

           call BMSLSH (IPC,1.0,VREMOV,ISTD)

           VOLREM(ISTD,2) = VOLREM(ISTD,2) + VREMOV
           50 continue
        end do

        !         For bookkeeping purposes, make sure to remove some PBKILL and ALLKILL
        !         since they would have been part of the removals in the youngest age class.

        do ISIZ = MINLIV,MAXLIV
           PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) * (1 - EFFC)
           ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) * (1 - EFFC)
           52 continue
        end do

        200 continue
     end do
  end if

  !      SUM = 0.0
  !     Total standing dead volume available for cut
  !      DO 30 IPC= 1,MXDWPC
  !        DO 25 ISC= MINSC,MAXSC
  !          DO 20 IAG= 1,MXDWAG
  !              SUM = SUM + SDWP(ISTD,IPC,ISC,IAG)
  !    20     CONTINUE
  !   25   CONTINUE
  !   30 CONTINUE
  !      SUM = SUM * EFFC

  !     If not enough volume to warrent a salvage cut then return
  !      IF (SUM .LT. THRVOL) RETURN

  if (LBMDEB) write(JBMBPR,99) IYR
  99 format(' End BMSALV: Year= ',I5)

  return
end
