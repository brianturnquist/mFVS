!==================!
!   dbs : dbsprs   !
!==================!
! DBS $Id$
subroutine DBSPRS(TOKEN,SOURCE,DELIMSTR)
  implicit none

  !     PURPOSE: TO SIMPLY RETURN THE FIRST TOKEN FROM THE SOURCE PASSED
  !     IN USING THE DELIMSTR AS THE DELIMITER

  !     THE ROUTINE ALSO CHANGES SOURCE, SO THAT SUBSEQUENT CALLS SKIP
  !     THE TOKEN.

  !     OUTPUT: TOKEN    - THE TOKENIZED VALUE
  !             SOURCE   - THE SOURCE STRING WHICH IS TOKENIZED
  !             DELIMSTR - THE DELIMITER THAT THE STRING IS TOKENIZED ON

  ! Variables
  character(LEN = *) :: SOURCE
  character(LEN = *) :: TOKEN
  character(LEN = *) :: DELIMSTR
  character(len=1000) :: SAVESTR
  integer :: ISTART,ILEN,IDLEN,IBEGPOS,IENDPOS

  TOKEN = ' '
  ISTART = 1
  SAVESTR = SOURCE

  IBEGPOS = ISTART
  ILEN = LEN(SAVESTR)
  IDLEN = LEN(DELIMSTR)
  5 continue
  if (IBEGPOS .lt. ILEN) then
     if (INDEX(DELIMSTR,SAVESTR(IBEGPOS:IBEGPOS+IDLEN-1)).ne.0) then
        IBEGPOS = IBEGPOS + 1
        goto 5
     end if
  end if
  IENDPOS = IBEGPOS
  10 continue
  if ((IENDPOS .le. ILEN)) then
     if (INDEX(DELIMSTR,SAVESTR(IENDPOS:IENDPOS+IDLEN-1)).eq.0) then
        IENDPOS = IENDPOS + 1
        goto 10
     end if
  end if
  TOKEN = SAVESTR(IBEGPOS:IENDPOS-1)
  SOURCE = SAVESTR(IENDPOS+IDLEN:)
  ISTART = IENDPOS

end
