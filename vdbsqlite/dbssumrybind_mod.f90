!=================================================!
!   Bind Year through QMD in ordinary Summary     !
!=================================================!
module DBSSUMRYBIND_MOD
  implicit none
  private
  public :: DBSSUMRYBIND

contains

  subroutine DBSSUMRYBIND(IoutDBref,ColNumber,iRet,&
       &IYEAR,IAGE,ITPA,IBA,ISDI,ICCF,ITOPHT,FQMDB)
    implicit none

    integer, intent(in) :: IoutDBref,IYEAR,IAGE,ITPA,IBA,ISDI,ICCF,ITOPHT
    integer, intent(inout) :: ColNumber
    integer, intent(out) :: iRet
    double precision, intent(in) :: FQMDB
    integer :: fsql3_bind_int,fsql3_bind_double

    ! SUMOUT already supplied the correct units. The caller promotes QMD
    ! to DOUBLE PRECISION and owns both schemas and the statement lifetime.
    ! Leave ColNumber at QMD so the caller advances to its first volume field.
    iRet = fsql3_bind_int(IoutDBref,ColNumber,IYEAR)
    ColNumber = ColNumber+1
    iRet = fsql3_bind_int(IoutDBref,ColNumber,IAGE)
    ColNumber = ColNumber+1
    iRet = fsql3_bind_int(IoutDBref,ColNumber,ITPA)
    ColNumber = ColNumber+1
    iRet = fsql3_bind_int(IoutDBref,ColNumber,IBA)
    ColNumber = ColNumber+1
    iRet = fsql3_bind_int(IoutDBref,ColNumber,ISDI)
    ColNumber = ColNumber+1
    iRet = fsql3_bind_int(IoutDBref,ColNumber,ICCF)
    ColNumber = ColNumber+1
    iRet = fsql3_bind_int(IoutDBref,ColNumber,ITOPHT)
    ColNumber = ColNumber+1
    iRet = fsql3_bind_double(IoutDBref,ColNumber,FQMDB)
  end subroutine DBSSUMRYBIND

end module DBSSUMRYBIND_MOD
