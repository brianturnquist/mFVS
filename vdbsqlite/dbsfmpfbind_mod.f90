!=================================================!
!   Bind potential-fire fuel models and weights   !
!=================================================!
module DBSFMPFBIND_MOD
  implicit none
  private
  public :: DBSFMPFBIND

contains

  subroutine DBSFMPFBIND(IoutDBref,ColNumber,iRet,LEAST,&
       &FUELMOD,BFUELWT,SFUELMOD,BSFUELWT)
    implicit none

    integer, intent(in) :: IoutDBref,FUELMOD(4),SFUELMOD(4)
    integer, intent(inout) :: ColNumber
    integer, intent(out) :: iRet
    logical, intent(in) :: LEAST
    double precision, intent(in) :: BFUELWT(4),BSFUELWT(4)
    integer :: I,fsql3_bind_int,fsql3_bind_double

    ! Callers retain DBLE(INT(weight*100.+0.5)) in default REAL arithmetic.
    ! Their West-only fields determine the incoming column index. INSERT's
    ! named columns require moderate IDs, moderate weights, then severe data.
    do I = 1,4
      ColNumber = ColNumber+1
      iRet = fsql3_bind_int(IoutDBref,ColNumber,FUELMOD(I))
    end do
    do I = 1,4
      ColNumber = ColNumber+1
      iRet = fsql3_bind_double(IoutDBref,ColNumber,BFUELWT(I))
    end do

    ! West never reads BSFUELWT, which its caller does not initialize.
    if (LEAST) then
      do I = 1,4
        ColNumber = ColNumber+1
        iRet = fsql3_bind_int(IoutDBref,ColNumber,SFUELMOD(I))
      end do
      do I = 1,4
        ColNumber = ColNumber+1
        iRet = fsql3_bind_double(IoutDBref,ColNumber,BSFUELWT(I))
      end do
    end if
  end subroutine DBSFMPFBIND

end module DBSFMPFBIND_MOD
