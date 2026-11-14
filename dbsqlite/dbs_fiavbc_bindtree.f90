!====================================!
!   dbsqlite : dbs_fiavbc_bindtree    !
!====================================!
subroutine DBS_FIAVBC_BINDTREE(I,ITRNK,ColNumber,iRet)
  implicit none

  ! Bind the shared DBH-through-carbon-fraction fields in FIA column order.
  ! The writer retains row selection, IDs, probabilities and volume fields.
  ! Keep default REAL calculations before assignment to the REAL*8 bindings.
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'DBSCOM.f90'

  integer :: I,ITRNK,ColNumber,iRet
  integer :: fsql3_bind_double,fsql3_bind_int
  real*8  :: DDBH,DHT,ESTHT,DCULL,DCRBFRC

  DDBH = DBH(I)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DDBH) !DBH

  DHT = HT(I)
  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DHT) !Ht

  if (NORMHT(I) .gt.0) then
     ESTHT = (real(NORMHT(I))+5)/100
  else
     ESTHT = 0
  end if
  ColNumber = ColNumber+1
  iRet = fsql3_bind_double(IoutDBref,ColNumber,ESTHT) !NormHt

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ITRNK) !TruncHt

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,ICR(I)) !PctCr (Percent live crown)

  !           Add FIA Cull, Number WoodlandStems, Decay Code, Carbon fraction
  ColNumber = ColNumber+1
  DCULL = CULL(I)
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DCULL) !Cull

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,WDLDSTEM(I)) !WdldStem

  ColNumber = ColNumber+1
  iRet = fsql3_bind_int(IoutDBref,ColNumber,DECAYCD(I)) !DecayCd

  ColNumber = ColNumber+1
  DCRBFRC = CARB_FRAC(I)
  iRet = fsql3_bind_double(IoutDBref,ColNumber,DCRBFRC) !CarbFrac

  return
end
