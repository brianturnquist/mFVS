!============================!
!   Find fuel model column   !
!============================!
! FIRE-BASE $Id$
integer function FINDMOD(JMOD,IPTR,ICLSS)

  integer :: JMOD,K,IPTR
  dimension IPTR(ICLSS)

  do K = 1,ICLSS
     if (JMOD .eq. IPTR(K)) then
        goto 11
     end if
  end do
  K = 8
  11 continue
  FINDMOD = K

  return
end function FINDMOD
