!=================================================!
!   Handle SQLite stand sampling-design fields    !
!=================================================!
module DBSSTANDDESIGN_MOD
  implicit none
  private
  public :: DBSSTANDDESIGN_READ,DBSSTANDDESIGN

contains

  subroutine DBSSTANDDESIGN_READ(VALUE,IFIELD,IPRESENT,NullReal)
    implicit none

    include 'PRGPRM.f90'
    include 'METRIC.f90'

    real, intent(inout) :: VALUE
    integer, intent(in) :: IFIELD
    integer, intent(inout) :: IPRESENT
    real, intent(in) :: NullReal

    ! Presence is determined in input units, before converting even a NULL
    ! sentinel. The reader retains SQL traversal and the overlaid data array.
    if (VALUE .ne. NullReal) IPRESENT = 1
    if (LMETRICVAR) then
      select case (IFIELD)
      case (10)
        if (VALUE .lt.0.0) then
          VALUE = VALUE / HAtoACR
        else
          VALUE = VALUE * M2pHAtoFT2pACR
        end if
      case (11)
        VALUE = VALUE / HAtoACR
      case (12)
        VALUE = VALUE * CMtoIN
      end select
    end if
  end subroutine DBSSTANDDESIGN_READ

  subroutine DBSSTANDDESIGN(VALUES,Basal_LI,PlotArea_LI,BPDBH_LI,&
       &NullInt,JOSTND,LKECHO,BAF,FPA,BRK)
    implicit none

    include 'PRGPRM.f90'
    include 'METRIC.f90'

    real, intent(in) :: VALUES(3)
    integer, intent(in) :: Basal_LI,PlotArea_LI,BPDBH_LI,NullInt,JOSTND
    logical, intent(in) :: LKECHO
    real, intent(inout) :: BAF,FPA,BRK

    if (Basal_LI.ne.NullInt) then
      BAF = VALUES(1)
      ! Preserve the original positive-BAF metric echo error (bug 32).
      if (LMETRICVAR) then
        if (LKECHO)write(JOSTND,'(T12,''BASAL_AREA_FACTOR: '',T34,F6.1)') BAF / ACRtoHA
      else
        if (LKECHO)write(JOSTND,'(T12,''BASAL_AREA_FACTOR: '',T34,F6.1)') BAF
      end if
    end if
    if (PlotArea_LI.ne.NullInt) then
      FPA = VALUES(2)
      if (LMETRICVAR) then
        if (LKECHO)write(JOSTND,'(T12,''INV_PLOT_SIZE: '',T34,F6.0)') FPA / ACRtoHA
      else
        if (LKECHO)write(JOSTND,'(T12,''INV_PLOT_SIZE: '',T34,F6.0)') FPA
      end if
    end if
    if (BPDBH_LI.ne.NullInt) then
      BRK = VALUES(3)
      if (LMETRICVAR) then
        if (LKECHO)write(JOSTND,'(T12,''BRK_DBH: '',T34,F6.1)') BRK * INtoCM
      else
        if (LKECHO)write(JOSTND,'(T12,''BRK_DBH: '',T34,F6.1)') BRK
      end if
    end if
  end subroutine DBSSTANDDESIGN

end module DBSSTANDDESIGN_MOD
