!===================!
!   dbs : dbsblkd   !
!===================!
! DBS $Id$
block data DBSBLKD

  !     INITIALIZE DATABASE VARIABLES

  ! Common blocks
  include 'DBSCOM.f90'

  data DSNOUT/'FVSOut.mdb'/
  data ConnHndlOut/-1/
  data EnvHndlOut/-1/
  data DSNIN/'FVSIn.mdb'/
  data ConnHndlIn/-1/
  data EnvHndlIn/-1/

end
