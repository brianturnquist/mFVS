!===========================!
!   mistoe : misclass_mod   !
!===========================!
module MISCLASS_MOD
  implicit none
  private
  public :: MISCLASS

contains

  subroutine MISCLASS(ITREE,ISPC,P,DCTPA,DCTPAX,DCMRT,SDTPA,SDMRT,&
       &DCINF,DCSUM,SDINF,SDSUM,SDIN12,SDIN34,SDIN56,SDIN0)
    implicit none

    include 'PRGPRM.f90'
    include 'ARRAYS.f90'
    include 'MISCOM.f90'
    include 'METRIC.f90'

    integer, intent(in) :: ITREE,ISPC
    real, intent(out) :: P
    real, intent(inout) :: DCTPA(20),DCTPAX(20),DCMRT(20),DCINF(20),DCSUM(20)
    real, intent(inout) :: SDTPA(MAXSP,20),SDMRT(MAXSP,20),SDINF(MAXSP,20),SDSUM(MAXSP,20)
    real, intent(inout) :: SDIN12(MAXSP,20),SDIN34(MAXSP,20),SDIN56(MAXSP,20),SDIN0(MAXSP,20)
    integer :: IVAL,IDBH,IDMR

    ! The caller supplies species selection and retains all summary folding,
    ! report gates and BC spatial writers. Accumulate in the original order.
    ! Keep the legacy five-centimetre remainder formula (bug 27).
    ! Correcting these bins would change model reports, not just structure.
    if (LMETRICVAR) then
       IVAL = IFIX(DBH(ITREE)*INtoCM)
       IDBH = IVAL / 5 + MOD(IVAL,5)
    else
       IVAL = IFIX(DBH(ITREE))
       IDBH = IVAL / 2 + MOD(IVAL,2)
    end if
    IDBH = MAX(1,MIN(20,IDBH))
    P = PROB(ITREE)
    IDMR = IMIST(ITREE)

    DCTPA(IDBH) = DCTPA(IDBH) + P

    if (DBH(ITREE) .ge. DMRMIN) DCTPAX(IDBH) = DCTPAX(IDBH) + P

    DCMRT(IDBH) = DCMRT(IDBH) + DMMTPA(ITREE)
    SDTPA(ISPC,IDBH) = SDTPA(ISPC,IDBH) + P
    SDMRT(ISPC,IDBH) = SDMRT(ISPC,IDBH) + DMMTPA(ITREE)

    if (IDMR .gt.0.and. DBH(ITREE) .ge. DMRMIN) then
       DCINF(IDBH) = DCINF(IDBH) + P
       DCSUM(IDBH) = DCSUM(IDBH) + IDMR * P
       SDINF(ISPC,IDBH) = SDINF(ISPC,IDBH) + P
       SDSUM(ISPC,IDBH) = SDSUM(ISPC,IDBH) + IDMR * P

       if (IDMR .eq.1.or. IDMR .eq.2)&
            &SDIN12(ISPC,IDBH) = SDIN12(ISPC,IDBH) + P
       if (IDMR .eq.3.or. IDMR .eq.4)&
            &SDIN34(ISPC,IDBH) = SDIN34(ISPC,IDBH) + P
       if (IDMR .eq.5.or. IDMR .eq.6)&
            &SDIN56(ISPC,IDBH) = SDIN56(ISPC,IDBH) + P
    else
       SDIN0(ISPC,IDBH) = SDIN0(ISPC,IDBH) + P
    end if

  end subroutine MISCLASS

end module MISCLASS_MOD
