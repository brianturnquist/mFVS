!============================!
!   Store activity strings   !
!============================!
! BASE $Id$
subroutine OPCACT (KODE,CSTR)
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JAN 2003 - MOSCOW

  !     OPCACT IS USED TO STORE A STRING ASSOCIATED WITH THE MORE RECENTLY
  !     STORED ACTIVITIY.  THIS ROUTINE MUST BE CALLED AFTER THE CALL THAT
  !     ADDS AN ACTIVITY TO THE ACTIVITY LIST.  THE STRING IS ASSOCITED WITH
  !     THE MOST RECENTLY ADDED ACTIVITY. ONLY CALL THIS ROUTINE IF THE
  !     MOST RECENTLY ADDED ACTIVITY WAS SUCCESSFULLY ADDED.

  !     KODE = THE RETURN CODE WHERE:
  !            0   ALL WENT OK,
  !            1   STRING COULD NOT BE ADDED BECAUSE STORAGE IS FULL.
  !                A WARNING MESSAGE IS ISSUED VIA ERRGRO.
  !     CSTR = THE STRING THAT IS BEING ADDED.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'
  include 'CONTRL.F77'

  ! Variables
  logical :: LDEB
  character(len=*) :: CSTR
  integer :: KODE,NCHAR,I

  !     SEE IF WE NEED WRITE DEBUG.

  call DBCHK (LDEB,'OPCACT',6,ICYC)
  if (LDEB) write (JOSTND,10) ICACT,MXCACT,LOPEVN,IMPL,IEPT, &
       & TRIM(CSTR)
  10 format (' IN OPCACT: ICACT=',I4,' MXCACT=',I6,' LOPEVN=',L2, &
       & ' IMPL=',I4,' IEPT=',I5,' CSTR=',A)

  !     MAKE SURE THERE IS ROOM IN THE ACTIVITY STORAGE AREAS
  !     TO STORE THE ACTIVITY AND ITS PARAMETERS.

  NCHAR = LEN_TRIM(CSTR)
  if (NCHAR+ICACT.gt.MXCACT) then
     KODE = 1
     call ERRGRO (.true.,10)
     write (JOSTND,*) 'ISSUED IN OPCACT CANNOT ADD ACTIVITY STRING'
     return
  else
     KODE = 1

     !        SET THE POINTER TO THE MOST RECENT ACTIVITY AND LOAD THE
     !        STARTING POSITION.

     if (LOPEVN) then
        I = IEPT+1
     else
        I = IMGL-1
     end if
     IACT(I,5) = ICACT

     !        STORE THE CHARACTER STRING.

     do I = 1,NCHAR
        CACT(ICACT) = CSTR(I:I)
        ICACT = ICACT+1
     end do
     CACT(ICACT) = CHAR(0)
     ICACT = ICACT+1
  end if
  return
end subroutine OPCACT
