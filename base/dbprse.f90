!==============================!
!   Parse debugging requests   !
!==============================!
! BASE $Id$
subroutine DBPRSE (IREAD,RECORD,JOUT,ICYC,IRC)
  implicit none

  ! Common blocks
  include 'DBSTK.F77'

  ! Variables
  integer :: IRC,ICYC,JOUT,IREAD,ISTLNB,IS,IE,IP,LP,NC,IQ,II
  character(len=*) :: RECORD
  character(len=1) :: BLANK
  character(len=1) :: AMPER

  data BLANK/' '/,AMPER/'&'/
  IRC = 0
  3030 continue
  read(IREAD,3035,end = 3070) RECORD
  3035 format (A)
  write (JOUT,3037) RECORD(1:ISTLNB(RECORD))
  3037 format (T13,A)
  IS = 0
  IE = LEN(RECORD)+1
  3040 continue
  IE = IE-1
  if ((RECORD(IE:IE).eq.BLANK).and.(IE.gt.1)) goto 3040
  3050 continue
  IS = IS+1
  if ((RECORD(IS:IS).eq.BLANK).and.(IS.lt.IE)) goto 3050
  if (RECORD(IS:IS).eq.AMPER) goto 3030
  if (IE.lt.IS) goto 9999
  IP = INDEX(RECORD(IS:IE),BLANK)
  if (IP.gt.0) then
     LP = IP+IS-2
  else
     LP = IE
  end if
  NC = LP-IS+1
  IQ = LP+1
  if (NC.gt.MAXLEN) then
     LP = IS+MAXLEN-1
     NC = LP-IS+1
     IQ = LP
  end if
  do II = IS,LP
     call UPCASE (RECORD(II:II))
     3065 continue
  end do
  call DBADD (RECORD(IS:LP),NC,ICYC,IRC)
  if (IRC.gt.0) goto 9999
  IS = IQ
  if (IS.lt.IE) goto 3050
  goto 9999
  3070 continue
  IRC = 1
  9999 continue
  return
end subroutine DBPRSE
