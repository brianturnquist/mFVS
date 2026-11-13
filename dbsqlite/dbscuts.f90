!========================!
!   dbsqlite : dbscuts   !
!========================!
! DBSQLITE $Id$
subroutine DBSCUTS(IWHO,KODE)

  !     PURPOSE: TO OUTPUT THE CUTS LIST DATA TO THE DATABASE. THE TABLE IS
  !              WRITTEN BY DBSATCUT IN dbsatrtls.f90, WHICH ALSO WRITES THE
  !              AFTER TREATMENT LIST.

  !     INPUT: IWHO  - THE WHO CALLED ME VALUE WHICH MUST BE 2
  !                     INORDER FOR US TO CONTINUE
  !            KODE  - FOR LETTING CALLING ROUTINE KNOW IF THIS IS A
  !                     REDIRECT OF THE FLAT FILE REPORT OR IN
  !                     ADDITION TO

  implicit none

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'DBSCOM.f90'

  ! Variables
  integer :: IWHO,KODE

  !     IF CUTSOUT IS NOT TURNED ON OR THE IWHO VARIABLE IS NOT 2
  !     THEN JUST RETURN

  if (ICUTLIST.eq.0.or.IWHO.ne.2) return

  !     IS THIS OUTPUT A REDIRECT OF THE REPORT THEN SET KODE TO 0

  if (ICUTLIST.eq.2) KODE = 0

  !     THE THIRD ARGUMENT (TEM) IS ONLY USED FOR THE AFTER TREATMENT LIST.

  call DBSATCUT(.true.,WK3,0.)
  return
end
