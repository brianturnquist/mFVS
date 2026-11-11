!==========================!
!   fire/vbase : fmsngdk   !
!==========================!
! FIRE-VBASE $Id$
subroutine FMSNGDK(VAR,KSP,D,DKTIME)
  implicit none

  !     SNAG DECAY PREDICTION

  !   Purpose:
  !     This routine calculates the number of years, since death, for a
  !     snag to become soft, based on species, DBH, and plant association.

  !     The base logic in this routine was extracted from its original
  !     location in FMSNAG, FMSCRO, and SVSNAGE, in order to
  !     structure/generalize the logic for use with both the
  !     FFE snag pools, and the base FVS model snag records.

  !   Called from: FMSNAG to compute decay time for a given FFE snag pool.
  !                FMSCRO to compute decay time for a given FFE snag pool.
  !                SVSNAGE to compute decay time for a given FVS snag record.

  !   Local variable definitions:
  !     D:       Diameter of current snag pool/record.
  !     DKTIME:  Years, since death, for snag to become soft.
  !     KSP:     Species number for current snag pool/record.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'FMCOM.f90'
  include 'PLOT.f90'

  ! Variables
  character(len=2) :: VAR
  integer :: JADJ,JSML,JYRSOFT,KSP
  real    :: D,DKTIME,XMOD

  !  Call SVHABT to get habitat-based multiplier for decay rate.
  !  (NOTE: SVHABT currently returns a constant value of 1.0.
  !         The call to SVHABT is preserved here for potential future use.
  !         The XMOD multiplier is not applied to decay rates computed in
  !         the FMR6SDCY routine, since it specifically addresses plant
  !         association.)

  call SVHABT(XMOD)

  !  Calculate years, since death, for snag to become soft.

  select case (VAR)
  case('LS','ON')
     DKTIME = 0.65 * DECAYX(KSP) * D
     DKTIME = DKTIME * XMOD
  case('PN', 'WC', 'BM', 'EC', 'AK', 'OP')
     call FMR6SDCY(KSP, D, JYRSOFT, JADJ, JSML)
     DKTIME = JYRSOFT * DECAYX(KSP)
  case('SO')
     select case (KODFOR)
     case (601,602,620,799) !OREGON
        call FMR6SDCY(KSP, D, JYRSOFT, JADJ, JSML)
        DKTIME = JYRSOFT * DECAYX(KSP)
     case default !CALIFORNIA
        DKTIME = (1.24 * DECAYX(KSP) * D) + (13.82 * DECAYX(KSP))
        DKTIME = DKTIME * XMOD
     end select
  case default
     DKTIME = (1.24 * DECAYX(KSP) * D) + (13.82 * DECAYX(KSP))
     DKTIME = DKTIME * XMOD
  end select

  return
end
