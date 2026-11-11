!===================!
!   covr : cvcnop   !
!===================!
! COVR $Id$
subroutine CVCNOP (LTHIN)
  implicit none

  !  **CVCNOP** CONTROLS THE CALLS TO THE CROWN WIDTH, CROWN SHAPE,
  !  CROWN FOLIAGE BIOMASS, SUMMARY, AND CLASSIFICATION SUBPROGRAMS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'CVCOM.f90'

  ! Variables
  logical :: LTHIN,DEBUG

  !  CHECK FOR DEBUG.

  call DBCHK(DEBUG,'CVCNOP',6,ICYC)

  if (.not. LCOV) return

  if (LTHIN) LTHIND(ICYC) = .true.

  !  CALL **CVCW** TO COMPUTE CROWN WIDTH.

  if (DEBUG) write (JOSTND,9000) ICYC
  9000 format ('**CALLING CVCW, CYCLE =',I2)
  call CVCW (LTHIN)

  !  CALL **CVSHAP** TO COMPUTE CROWN SHAPE.

  if (.not.LCNOP) go to 10
  if (DEBUG) write (JOSTND,9001) ICYC
  9001 format ('**CALLING CVSHAP, CYCLE =',I2)
  call CVSHAP (LTHIN)

  !  CALL **CVCBMS** TO COMPUTE CROWN BIOMASS.

  if (DEBUG) write (JOSTND,9002) ICYC
  9002 format ('**CALLING CVCBMS, CYCLE =',I2)
  call CVCBMS (LTHIN)

  !  CALL **CVSUM** TO COMPUTE SUMS.

  10 continue
  if (DEBUG) write (JOSTND,9003) ICYC
  9003 format ('**CALLING CVSUM, CYCLE =',I2)
  call CVSUM (LTHIN)

  !  CALL **CVCLAS** TO COMPUTE THE 'STAND SUCCESSIONAL STAGE' CODE.

  if (.not.LBROW) return
  if (DEBUG) write (JOSTND,9004) ICYC
  9004 format ('**CALLING CVCLAS, CYCLE =',I2)
  call CVCLAS(LTHIN)
  return
end
