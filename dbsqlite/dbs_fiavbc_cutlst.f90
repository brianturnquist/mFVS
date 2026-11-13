!==================================!
!   dbsqlite : dbs_fiavbc_cutlst   !
!==================================!
! DBSQLITE $Id$
subroutine DBS_FIAVBC_CUTLST()

  !     PURPOSE: TO OUTPUT THE FIAVBC CUTS LIST DATA TO THE DATABASE
  !              (TABLE FVS_FIAVBC_CutList). THE TABLE IS WRITTEN BY
  !              DBS_FIAVBC_ATCUT IN dbs_fiavbc_atrtls.f90, WHICH ALSO
  !              WRITES THE FIAVBC AFTER TREATMENT LIST.

  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'

  call DBS_FIAVBC_ATCUT(.true.,WK3)
  return
end
