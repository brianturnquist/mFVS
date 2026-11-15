!==============================!
!   fire/base : fmdwdreq_mod   !
!==============================!
module FMDWDREQ_MOD
  implicit none
  private
  public :: FMDWDREQ

contains

  subroutine FMDWDREQ(ICALL,KEYWRD,LKECHO)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'CONTRL.f90'

    integer, intent(in) :: ICALL
    character(len=8), intent(in) :: KEYWRD
    logical, intent(in) :: LKECHO

    ! DWDVLOUT is stand-only; rejected requests must not allocate an ID or
    ! change an earlier reporting window, even when keyword echo is off.
    if (ICALL .eq.2) then
      write(JOSTND,5104) KEYWRD
      5104 format(/A8,'   ***KEYWORD IS A STAND-LEVEL KEYWORD ONLY',&
           &' AND CANNOT BE INCLUDED WITH THE LANDSCAPE-LEVEL KEYWORDS')
      return
    end if

    if (IDDWRP .eq.0) call GETID (IDDWRP)

    ! FMDOUT still interprets metric requests as sentinel flags (bug 24).
    ! Keep producer and consumer paired until a separate model correction.
    if (LMETRICVAR) then
      IDWRPB = -1
      IDWRPE = -1
    else
      IDWRPB = IY(1)
      IDWRPE = IY(1) + 999
    end if

    if (LKECHO)write(JOSTND,5115) KEYWRD
    5115 format(/A8,'   THE DOWN WOOD VOLUME REPORT WILL BE PRINTED.')
  end subroutine FMDWDREQ

end module FMDWDREQ_MOD
