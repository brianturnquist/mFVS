!====================!
!   lpmpb : mpotpr   !
!====================!
! LPMPB $Id$
subroutine MPOTPR (PROTBK)
  implicit none

  !     PROTBK = THE PROBABILITY OF A MPB OUTBREAK.

  !     ***** WARNING:  DO NOT CALL THIS ROUTINE UNLESS THERE
  !                     IS LP IN THE TREE LIST (SEE MPBGO).

  ! Revision History
  !   11/07/00 Lance David (FHTET)
  !     Changed array reference RELDSP(7) to RELDSP(IDXLP) for relative
  !     density of Lodgepole Pine.
  !   11/09/00 Lance David (FHTET)
  !     Added variable descriptions and code comments.
  !     Condition requiring stand age to be at least 40 was commented out
  !     sometime in the past and I deleted it.
  !   07/02/10 Lance R. David (FMSC)
  !     Added IMPLICIT NONE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'MPBCOM.f90'

  !     VARIABLE DESCRIPTIONS:
  !     ISTDT  - START DATE SPECIFIED IN RANSTART KEYWORD (DEFAULT = 1).
  !     ICYC   - FVS CYCLE NUMBER
  !     IY     - CALENDAY YEAR
  !     A45DBH - AVERAGE DBH OF ALL LODGEPOLE 4.5 INCHES AND LARGER.
  !     IDXLP  - ARRAY INDEX NUMBER FOR LODGEPOLE PINE.
  !     RELDSP - FVS RELATIVE DENSITY OF SPECIES INDEXED BY SPECIES NUMBER.
  !     RELDEN - FVS RELATIVE DENSITY OF STAND, ALL SPECIES INCLUDED.
  !     CNTLP  - SUM OF LODGEPOLE PINE TPA (PROB(I)) FOR STAND
  !     PBALPP - PROPORTION OF BASAL AREA THAT IS LODGEPOLE PINE.

  !     CALCULATE THE PROBABILITY OF AN OUTBREAK OCCURING

  !     PROBABILITY IS EQUAL TO ZERO IF ANY OF THE MINIMUM CONDITIONS
  !     ARE NOT MET.

  ! Variables
  real    :: PROTBK,X

  PROTBK = 0.0

  !.... AT OR BEYOND MPB START CYCLE OR DATE
  if ((ISTDT .le.40.and. ICYC .lt. ISTDT) .or.&
       &(ISTDT .gt.40.and. IY(ICYC) .lt. ISTDT)) goto 10

  !.... AVERAGE DBH OF 4.5+ LP MUST BE AT LEAST 6.0 INCHES, INDICATING
  !.... THAT THERE ARE ADEQUATE LARGER LP IN THE LP POPULATION.
  if ( A45DBH  .lt.6.0 ) go to 10

  !.... 25+% OF THE STAND BASAL AREA IS LP
  if ( PBALPP .lt.0.25 ) go to 10

  !.... 20+% OF THE TREES IN THE STAND ARE LP
  if ( RELDSP(IDXLP) / RELDEN .lt.0.20 ) go to 10

  !.... POPULATION OF LP IS AT LEAST 40 TREES PER ACRE.
  if ( CNTLP .lt.40.0 ) go to 10

  !.... END OF MINIMUM CONDITIONS

  !.... PROPORTION OF BASAL AREA IN LODGEPOLE IS LIMITED TO 0.8
  !.... FOR THE PROBABILITY FUNCTION.
  X = PBALPP
  if ( X .gt.0.8 ) X = 0.8

  PROTBK = 1. / ( 1. + EXP( 9.583&
       &- 0.08967 * ( X *RELDEN)&
       &))

  10 continue
  if ( .not. DEBUIN ) go to 50
  write (JOMPB,40) CNTLP,TLP45,A45DBH,BA,BALPP,PBALPP,&
       &RELDSP(IDXLP),RELDEN,PROTBK
  40 format (/,'ALL LPP/ACRE= ',E13.3,'; >4.5 LPP/ACRE= ',E13.3,&
       &'; AVE >4.5 DBH= ',E13.3,'; TOT BA= ',E13.3,//,&
       &'BALPP= ',E13.3,'; PBALPP= ',E13.3,'; LPP-CCF= ',E13.3,&
       &'; TOT CCF= ',E13.3,'; PROB OUTBREAK= ',E13.3)
  50 continue
  return
end
