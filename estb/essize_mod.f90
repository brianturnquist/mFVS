!=======================!
!   estb : essize_mod   !
!=======================!
module ESSIZE_MOD
  implicit none
  private
  public :: ESSIZE

contains

  subroutine ESSIZE(ISSP,ISHAG,PREM,SMULT,HMULT,DEBUG)
    implicit none

    include 'PRGPRM.f90'
    include 'ARRAYS.f90'
    include 'COEFFS.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'
    include 'VARCOM.f90'

    integer, intent(in) :: ISSP,ISHAG
    real, intent(in) :: PREM,SMULT,HMULT
    logical, intent(in) :: DEBUG
    real :: HTI,RANDEV,AX,BX,BACHLO
    external :: ESRANN

    ! These equations use feet, inches and trees per acre in every variant.
    ! Keep each rejected random draw and the arithmetic order unchanged;
    ! unit conversion belongs to the callers' later reports.
    PROB(ITRN) = PREM*SMULT
    HTI = 0.
    if (DEBUG) then
      if (LMETRICVAR) then
        write(JOSTND,*)' BEFORE SPRTHT VARACD,ISSP,SI,ISHAG= ',&
             &VARACD,ISSP,SITEAR(ISSP),ISHAG
      else
        write(JOSTND,*)'BEFORE SPRTHT VARACD,ISSP,SI,ISHAG= ',&
             &VARACD,ISSP,SITEAR(ISSP),ISHAG
      end if
    end if
    call SPRTHT (VARACD,ISSP,SITEAR(ISSP),ISHAG,HTI)
    if (DEBUG) then
      if (LMETRICVAR) then
        write(JOSTND,*)' AFTER SPRTHT HTI= ',HTI
      else
        write(JOSTND,*)'AFTER SPRTHT HTI= ',HTI
      end if
    end if
    HT(ITRN) = HTI*HMULT
    162 RANDEV = BACHLO(0.,0.5,ESRANN)
    if (RANDEV.lt.-1.0.or. RANDEV.gt.1.0) go to 162
    RANDEV = RANDEV*HT(ITRN)/5.5
    HT(ITRN) = HT(ITRN)+RANDEV

    if (HT(ITRN) .gt.4.5) then
       BX = HT2(ISSP)
       if (IABFLG(ISSP).eq.1) then
          AX = HT1(ISSP)
       else
          AX = AA(ISSP)
       end if
       DBH(ITRN) = (BX/(ALOG(HT(ITRN)-4.5)-AX))-1.0
       if (DBH(ITRN).lt.0.1) DBH(ITRN) = 0.1
    else
       DBH(ITRN) = 0.1
    end if

    ICR(ITRN) = 70
  end subroutine ESSIZE

end module ESSIZE_MOD
