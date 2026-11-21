!====================================================!
!   Set site indices from a Dunning site-class code   !
!====================================================!
! CA, NC, OC $Id$
subroutine DUNN (SS)
  use VARCFG_MOD, only: CFG_MAXSP
  implicit none

  ! THIS SUBROUTINE PROCESSES THE DUNNING CODE INFORMATION THAT WAS
  ! ENTERED BY KEYWORD.

  ! WHEN A DUNNING CODE IS ENTERED (IE ANY OF THE SITEAR VALUES BETWEEN
  ! 0 AND 7)  THEN SITE VALUES FOR ALL SPECIES ARE AUTOMATICALLY SET.  IF
  ! ANY SITEAR VALUES ARE BETWEEN 8 AND 10 THIS IS AN ERROR AND THE
  ! DEFAULT VALUE SPECIFIED IN GRINIT IS MAINTAINED.
  ! THIS ROUTINE IS CALLED FROM INITRE.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'CONTRL.f90'

  ! Variables
  real    :: ADJFAC,DUNN50(8),XST2(8),SS,DU50
  real    :: ADJFACCA(50),ADJFACNC(12)
  integer :: IST,ISPC,I

  !  IF THESE ADJUSTMENT FACTORS CHANGE, ALSO CHANGE THEM IN SITSET.

  !  CA AND OC USE THE CA 50-SPECIES BANK; NC RETAINS ITS 12 ROWS.
  !  THE BUILD-TIME VARIANT SELECTS THE FACTOR WITHOUT CHANGING THE
  !  DUNNING LOOKUP OR THE SEPARATE GETDUNN ENTRY.

  data ADJFACCA/&
       &0.90, 0.76, 0.90, 1.00, 1.00, 1.00, 1.00, 0.90, 0.90, 0.90,&
       &0.90, 0.90, 0.90, 0.90, 1.00, 1.00, 0.90, 1.00, 0.90, 0.90,&
       &0.76, 0.76, 1.00, 0.76, 0.90, 0.57, 0.57, 0.57, 0.57, 0.57,&
       &0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57,&
       &0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 0.57, 1.00/

  data ADJFACNC/ 0.90, 0.90, 1.00, 1.00, 0.57, 0.76,&
       &0.57, 0.57, 1.00, 1.00, 0.57, 1.00/

  data DUNN50/106.,90.,75.,56.,49.,39.,31.,23./

  IST = INT(SS+1.0)
  DU50 = DUNN50(IST)

  !   SET SITE INDEX VALUES BASED ON DUNNING VALUES ENTERED.

  do ISPC = 1,CFG_MAXSP
     if (VARIANTCD.eq.'NC') then
        ADJFAC = ADJFACNC(ISPC)
     else
        ADJFAC = ADJFACCA(ISPC)
     end if
     SITEAR(ISPC) = DU50 * ADJFAC
     20 continue
  end do
  return

  !   CA, NC AND OC FIRE MODELS NEED SITE INDICES FOR DUNNING CODES.

  entry GETDUNN(XST2)
  do I = 1,8
     XST2(I) = DUNN50(I)
  end do
  return
end
