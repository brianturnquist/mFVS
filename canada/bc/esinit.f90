!========================!
!   canada/bc : esinit   !
!========================!
! CANADA-BC $Id$
subroutine ESINIT
  implicit none

  !     CALLED FROM INITRE, ONLY ONCE, TO INITIALIZE REGEN. MODEL.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ESPARM.f90'
  include 'ESCOMN.f90'
  include 'ESHAP.f90'
  include 'ESRNCM.f90'
  include 'ESWSBW.f90'

  ! Variables
  integer :: NSTK,I,KODE,JOSTND
  real    :: SUMPRB,X
  character(len=40) :: CNAME
  equivalence (NSTK,SUMPRB)
  logical :: LTEMP,LKECHO
  do I = 1,MAXSP
     HTADJ(I) = 0.0
     XESMLT(I) = 1.0
     10 continue
  end do
  ITRNRM = 0
  NSTK = 0
  NBWHST = 0
  STOADJ = 0.0
  CONFID = 5.0
  LINGRW = .false.
  LAUTAL = .false.
  LSPRUT = .true.
  IPRINT = 1
  INADV = 0
  LOAD = 0
  IBLK = 0
  MINREP = 50
  KDTOLD = -99
  IPINFO = 0
  NTALLY = 0
  IDSDAT = -9999
  IYRLRM = -9999
  do I = 1,3
     JDST(I) = -9999
  end do
  XTES = 0.0
  THRES1 = 0.10
  THRES2 = 0.30
  call ESRNSD (.false.,X)
  NPTIDS = 0
  inquire (UNIT = JOREGT,OPENED = LTEMP)
  if (.not.LTEMP) then
     CNAME = ' '
     call MYOPEN (JOREGT,CNAME,4,133, 0,1,1,0,KODE)
     if (KODE.gt.0) write(*,'('' OPEN FAILED FOR '',I4)') JOREGT
  end if
  return

  !     CALLED FROM INITRE WHEN NOTREES KEYWORD IS ENTERED.

  entry ESEZCR (JOSTND,LKECHO)
  INADV = 1
  if (LKECHO)write(JOSTND,30)
  30 format(T13,'(EZCRUISE OPTION IN REGENERATION ESTABLISHMENT ',&
       &'MODEL IS AUTOMATICALLY INVOKED.)'  )
  return
end
