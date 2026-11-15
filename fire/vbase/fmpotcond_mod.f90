!================================!
!   fire/vbase : fmpotcond_mod   !
!================================!
module FMPOTCOND_MOD
  implicit none
  private
  public :: FMPOTCOND

contains

  subroutine FMPOTCOND(IDPL)
    implicit none

    include 'PRGPRM.f90'
    include 'FMPARM.f90'
    include 'CONTRL.f90'
    include 'PLOT.f90'
    include 'FMCOM.f90'
    include 'FMFCOM.f90'
    include 'METRIC.f90'

    integer, intent(in) :: IDPL
    integer :: IDPM,M,IPRES

    ! FMMOIS still runs once for each scenario and leaves MOIS as before.
    ! The metric override uses the caller's prior row index (bug 26), while
    ! the imperial override tests the row currently being written.
    if (ICYC.eq.1) then
       do IDPM = 1,2
          M = IDPM
          if (IDPM .eq.2) M = 3
          call FMMOIS(M, MOIS)
          IPRES = IDPM
          if (LMETRICVAR) IPRES = IDPL
          if (PRESVL(IPRES,1) .eq.1) then
             MOIS(1,1) = PRESVL(IDPM,2)
             MOIS(1,2) = PRESVL(IDPM,3)
             MOIS(1,3) = PRESVL(IDPM,4)
             MOIS(1,4) = PRESVL(IDPM,5)
             MOIS(1,5) = PRESVL(IDPM,6)
             MOIS(2,1) = PRESVL(IDPM,7)
             MOIS(2,2) = PRESVL(IDPM,8)
          end if
          if (LMETRICVAR) then
             call DBSFMPFC(NPLT,PREWND(IDPM)*MItoKM,&
                  &INT(POTEMP(IDPM)*FtoC1+FtoC2),&
                  &(100.0*MOIS(1,1)),(100.0*MOIS(1,2)),(100.0*MOIS(1,3)),&
                  &(100.0*MOIS(1,4)),(100.0*MOIS(1,5)),(100.0*MOIS(2,1)),&
                  &(100.0*MOIS(2,2)),IDPM)
          else
             call DBSFMPFC(NPLT,PREWND(IDPM),INT(POTEMP(IDPM)),&
                  &(100.0*MOIS(1,1)),(100.0*MOIS(1,2)),(100.0*MOIS(1,3)),&
                  &(100.0*MOIS(1,4)),(100.0*MOIS(1,5)),(100.0*MOIS(2,1)),&
                  &(100.0*MOIS(2,2)),IDPM)
          end if
       end do
    end if
  end subroutine FMPOTCOND

end module FMPOTCOND_MOD
