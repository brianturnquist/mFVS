!=======================================!
!   Search scheduled activity indices   !
!=======================================!
! BASE $Id$
subroutine OPBISR(N,IA,if,IP)
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JAN 1981 - MOSCOW

  !     BINARY SEARCH ROUTINE:  FINDS THE SUBSCRIPT LOCATION OF IF IN
  !     ARRAY IA.

  !     N  = THE LENGTH OF IA.
  !     IA = A LIST OF INTEGERS SORTED INTO ASCENDING ORDER
  !     IF = AN INTEGER WHICH YOU WISH FIND IN IA.
  !     IP = THE POSITION IN IA WHERE IF WAS FOUND; OR
  !          0 WHEN IF IS NOT A MEMBER OF IA.

  ! Variables
  integer :: IA(*),IP,if,N,IMID,ITOP,IBOT

  IMID = 1
  if (if.le.IA(1)) goto 40
  IMID = N
  if (if.ge.IA(N)) goto 40
  ITOP = 1
  IBOT = N
  20 continue
  IMID = (IBOT+ITOP)/2
  if (if.ge.IA(IMID)) goto 30
  IBOT = IMID-1
  if (if.gt.IA(IBOT)) goto 40
  goto 20
  30 continue
  ITOP = IMID+1
  if (if.lt.IA(ITOP)) goto 40
  goto 20
  40 continue
  IP = IMID
  if (if.ne.IA(IP)) IP = 0
  return
end subroutine OPBISR
