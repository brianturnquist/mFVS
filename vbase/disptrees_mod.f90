!========================================================!
!   Write a removal or residual tree-distribution record   !
!========================================================!
module DISPTREES_MOD
  implicit none
  private
  public :: DISPTREES

contains

  subroutine DISPTREES(JOSTND,AT,ONT,OSP,IOSP)
    implicit none

    include 'PRGPRM.f90'
    include 'METRIC.f90'

    integer, intent(in) :: JOSTND
    character(len=9), intent(in) :: AT
    real, intent(in) :: ONT(7),OSP(4)
    character(len=3), intent(in) :: IOSP(4)
    integer :: I

    ! CUTS supplies six diameter statistics followed by tree density.
    ! Convert only the printed values; DISPLY still owns the report gates
    ! and blank lines, and later calculations need the native-unit arrays.
    if (LMETRICVAR) then
      write(JOSTND,9004) AT,&
           &(ONT(I)*INtoCM,I = 1,6),&
           &ONT(7)/ACRtoHA,&
           &(OSP(I),IOSP(I),I = 1,4)
    else
      write(JOSTND,9004) AT,ONT,(OSP(I),IOSP(I),I = 1,4)
    end if
    9004 format(6X,A9,3X,5F7.1,F8.1,F9.0,' TREES   ',&
         &3(F5.0,'% ',A3,','),F5.0,'% ',A3)
  end subroutine DISPTREES

end module DISPTREES_MOD
