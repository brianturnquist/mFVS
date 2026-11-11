!=============================!
!   canada/newmist : dmrann   !
!=============================!
! CANADA-NEWMIST $Id$
subroutine DMRANN (SEL)
  implicit none

  ! **DMRANN -- NISI  Date of last revision: April 12 1994

  ! Purpose:
  !   This routine is based on the uniform random number found in
  ! ESRANN, coded by Nicholas Crookston. It is used in many places
  ! to simulate random processes.

  ! Called by:

  !     DMINIT    [ENTRY DMRNSD]
  !     MISIN     [ENTRY DMRNSD]
  !     DMFSHD
  !     DMNDMR
  !     DMSAMP
  !     DMSLOP
  !     DMSLST
  !     DMTLST

  ! Other routines called:

  !     [none]

  ! Argument list (and ENTRY point) definitions:

  !     INTEGER   SEL   (O) Uniform random number.
  !     LOGICAL   LSET  (I) Logical value specifying whether the
  !                          generator is being reseeded (.TRUE.).
  !     REAL      SEED  (I) Value to reseed the random
  !                          sequence.

  ! Local variable definitions:

  !     [none]

  ! Common block variables and parameters:

  !     DMS0      DMCOM
  !     DMS1      DMCOM
  !     DMSS      DMCOM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'DMCOM.f90'

  ! Argument list (including ENTRY point) variables.

  ! Variables
  real    :: SEL
  logical :: LSET
  real    :: SEED

  DMS1 = DMOD(16807D0*DMS0,2147483647D0)
  SEL = real(DMS1/2147483648D0,4)
  DMS0 = DMS1
  return

  ! You may reseed the generator by supplying an real-valued odd
  ! number with LSET = .TRUE. If LSET = .FALSE. a call to DMRNSD will
  ! cause the random number generator to start over.

  entry DMRNSD (LSET, SEED)
  if (LSET) goto 10
  SEED = DMSS
  DMS0 = SEED
  return
  10 continue
  if (AMOD(SEED,2.0).eq.0.) SEED = SEED+1
  DMSS = SEED
  DMS0 = SEED
  return
end
