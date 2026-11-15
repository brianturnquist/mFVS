!==========================!
!   mistoe : mistprt_mod   !
!==========================!
module MISTPRT_MOD
  implicit none
  private
  public :: MISTPRT

contains

  subroutine MISTPRT(ARRAY,LNOTBK,LKECHO,KEYWRD)
    implicit none

    include 'PRGPRM.f90'
    include 'CONTRL.f90'
    include 'MISCOM.f90'
    include 'METRIC.f90'

    real, intent(in) :: ARRAY(12)
    logical, intent(in) :: LNOTBK(12),LKECHO
    character(len=8), intent(in) :: KEYWRD

    ! Only the input and echoed threshold use metric units. MISPRT and
    ! both mistletoe models continue to compare DMRMIN with DBH in inches.
    PRTMIS = .true.
    if (LMETRICVAR) then
      if (LNOTBK(1).and.ARRAY(1).ge.0.0) DMRMIN = ARRAY(1)*CMtoIN
      if (LKECHO)write(JOSTND,620) KEYWRD,DMRMIN*INtoCM
    else
      if (LNOTBK(1).and.ARRAY(1).ge.0.0) DMRMIN = ARRAY(1)
      if (LKECHO)write(JOSTND,620) KEYWRD,DMRMIN
    end if
    620 format(/A8,'   Print mistletoe summary output; ',&
         &'DMRs and DMIs calculated for trees with DBH >=',F4.1)
  end subroutine MISTPRT

end module MISTPRT_MOD
