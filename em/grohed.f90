!===================================!
!   Write the model output header   !
!===================================!
! EM $Id$
subroutine GROHED (IUNIT)
  use FVSVERSION
  implicit none

  !     WRITES HEADER FOR BASE MODEL PORTION OF PROGNOSIS SYSTEM

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  character(len=10) :: DAT
  character(len=8) :: TIM
  character(len=10) :: REV
  character(len=32) :: SVN
  integer :: IUNIT

  !  SPECIES ORDER:
  !   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
  !   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
  !  17=PB, 18=OS, 19=OH

  !  SPECIES EXPANSION
  !  LM USES IE LM (ORIGINALLY FROM TT VARIANT)
  !  LL USES IE AF (ORIGINALLY FROM NI VARIANT)
  !  RM USES IE JU (ORIGINALLY FROM UT VARIANT)
  !  AS,PB USE IE AS (ORIGINALLY FROM UT VARIANT)
  !  GA,CW,BA,PW,NC,OH USE IE CO (ORIGINALLY FROM CR VARIANT)

  !     CALL REVISE TO GET THE LATEST REVISION DATE FOR THIS VARIANT.

  call REVISE (VARACD,REV)

  !     CALL THE DATE AND TIME ROUTINE FOR THE HEADING.

  call GRDTIM (DAT,TIM)
  SVN = FVSVER_VERSION

  write (IUNIT,40) SVN,REV,DAT,TIM
  40 format (//T6,'FOREST VEGETATION SIMULATOR',&
       &5X,'VERSION ',A,' -- EASTERN MONTANA PROGNOSIS',&
       &T97,'RV:',A,T112,A,2X,A)
  return
end
