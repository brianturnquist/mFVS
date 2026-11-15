!========================================!
!   Request the all fuels report in FMIN   !
!========================================!
module FMFUELREQ_MOD
  implicit none
  private
  public :: FMFUELREQ

contains

  subroutine FMFUELREQ(ICALL,KEYWRD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    integer, intent(in) :: ICALL
    character(len=8), intent(in) :: KEYWRD
    logical, intent(in) :: LKECHO

    ! Reject landscape requests before allocating an ID or changing state.
    if (ICALL .eq.2) then
      write(JOSTND,1504) KEYWRD
      1504 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
           &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
      return
    end if

    if (IDFLAL .eq.0) call GETID (IDFLAL)

    ! Both readers set year bounds here. Metric FMDOUT still rejects a
    ! positive begin year (bug 24); sharing this request must retain that
    ! behavior until producer and consumer are corrected together.
    IFLALB = IY(1)
    IFLALE = IY(1) + 999

    if (LKECHO)write(JOSTND,1515) KEYWRD
    1515 format(/A8,'   THE ALL FUELS REPORT WILL BE PRINTED.')
  end subroutine FMFUELREQ

end module FMFUELREQ_MOD
