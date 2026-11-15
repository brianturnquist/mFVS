!===============================================!
!   Request the down wood cover report in FMIN   !
!===============================================!
module FMDWCVREQ_MOD
  implicit none
  private
  public :: FMDWCVREQ

contains

  subroutine FMDWCVREQ(ICALL,KEYWRD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    integer, intent(in) :: ICALL
    character(len=8), intent(in) :: KEYWRD
    logical, intent(in) :: LKECHO

    ! Landscape requests must leave any previous ID and window intact.
    ! The rejection is printed even when ordinary keyword echo is off.
    if (ICALL .eq.2) then
      write(JOSTND,5204) KEYWRD
      5204 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
           &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
      return
    end if

    if (IDDWCV .eq.0) call GETID (IDDWCV)

    ! Metric FMDOUT uses a sentinel flag; imperial FMDOUT uses year bounds.
    ! Preserve both contracts until the separate bug-24 model correction.
    if (LMETRICVAR) then
      IDWCVB = -1
      IDWCVE = -1
    else
      IDWCVB = IY(1)
      IDWCVE = IY(1) + 999
    end if

    if (LKECHO)write(JOSTND,5215) KEYWRD
    5215 format(/A8,'   THE DOWN WOOD COVER REPORT WILL BE PRINTED.')
  end subroutine FMDWCVREQ

end module FMDWCVREQ_MOD
