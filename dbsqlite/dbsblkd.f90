!========================!
!   dbsqlite : dbsblkd   !
!========================!
! DBSQLITE $Id$
block data DBSBLKD

  !     INITIALIZE DATABASE VARIABLES

  ! Common blocks
  include 'DBSCOM.f90'

  data DSNOUT/"FVSOut.db"/
  data DSNIN /"FVS_Data.db"/
  data IoutDBref/-1/
  data IinDBref /-1/
  data IMEMDB   /0/

end
