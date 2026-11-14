!================================!
!   archive/bgc/src : setleafn   !
!================================!
subroutine SETLEAFN(I)

  !  THIS SUBROUTINE SETS LEAF_ON & FROOT_ON

  ! Variables
  integer :: C
  ! Common blocks
  include 'ENTITY.f90'
  ! Set tree on-off
  ITON = 0
  ITOFF = 365
  ! Set shrub on-off
  ISON = 120
  ISOFF = 260
  ! Set grass on-off
  IGON = 120
  IGOFF = 210
  ! Set LEAF_ON and LEAF_OFF
  if (ID(I).eq.'T' .and. (JD.ge.ITON .and. JD.le.ITOFF)) then
     LEAF_ON(I) = .true.
     FROOT_ON(I) = .true.
  else if (ID(I).eq.'S' .and. (JD.ge.ISON .and. JD.le.ISOFF)) then
     LEAF_ON(I) = .true.
     FROOT_ON(I) = .true.
  else if (ID(I).eq.'G' .and. (JD.ge.IGON .and. JD.le.IGOFF)) then
     LEAF_ON(I) = .true.
     FROOT_ON(I) = .true.
     !!!!shutting down grass based on limited soil water not yet implemented
     !!!!in model -- DWC,10/28/94.
     !       ELSE IF(ID(I).EQ.'G' .AND. VOLCONT.LT.(H2OMAX*.1)) THEN
     !          LEAF_ON(I)=.FALSE.
     !          FROOT_ON(I)=.FALSE.
  else
     LEAF_ON(I) = .false.
     FROOT_ON(I) = .true.
  end if
  !       WRITE(*,*) I,ID(I),JD,LEAF_ON(I)
  ! Count the number of days LEAF_ON=TRUE
  if (LEAF_ON(I)) then
     C = 1
  else
     C = 0
  end if
  TLEAFON(I) = TLEAFON(I) + C
  return
end
