!===================!
!   estb : esinit   !
!===================!
! ESTB $Id$
subroutine ESINIT
  implicit none

  !     CALLED FROM INITRE, ONLY ONCE, TO INITIALIZE REGEN. MODEL.

  !     THE FULL (LFULLESTAB) AND PARTIAL REGENERATION MODELS DIFFER IN
  !     THE DEFAULTS SET HERE AND IN HOW THE REGENERATION REPORT FILE IS
  !     OPENED.  THE DIFFERENCES ARE KEPT DELIBERATELY.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ESCOMN.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'
  include 'ESRNCM.f90'
  include 'ESWSBW.f90'

  ! Variables
  integer :: NSTK,I,KODE,JSTND,ICOUNT
  real    :: X

  !     THE PARTIAL MODEL'S REPORT FILE NAME BUFFER WAS ONE CHARACTER
  !     SHORTER (255).
  character(len=merge(256,255,LFULLESTAB)) :: CNAME

  !     NSTK OVERLAYS SUMPRB IN /ESHAP2/.  IN THE PARTIAL MODEL SUMPRB WAS
  !     A LOCAL, SO NSTK = 0 BELOW IS ONLY DONE FOR THE FULL MODEL.
  equivalence (NSTK,SUMPRB)
  logical :: LTEMP,LKECHO
  do I = 1,MAXSP
     HTADJ(I) = 0.0
     XESMLT(I) = 1.0
     10 continue
  end do
  ICOUNT = 0
  ITRNRM = 0
  if (LFULLESTAB) NSTK = 0
  NBWHST = 0
  if (LFULLESTAB) then
     STOADJ = 1.0
  else
     STOADJ = 0.0
  end if
  CONFID = 5.0
  if (LFULLESTAB) then
     LINGRW = .true.
     LAUTAL = .true.
  else
     LINGRW = .false.
     LAUTAL = .false.
  end if
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
  XTES = 0.0
  THRES1 = 0.10
  THRES2 = 0.30
  call ESRNSD (.false.,X)
  NPTIDS = 0
  inquire (UNIT = JOREGT,OPENED = LTEMP)

  !     THE FULL MODEL OPENS THE FILE EVEN IF JOREGT IS ALREADY OPEN.

  if (LFULLESTAB .or. .not.LTEMP) then
     CNAME = TRIM(KWDFIL)//'_RegenRpt.txt'
     !        CALL MYOPEN (JOREGT,CNAME,4,133, 0,1,1,0,KODE)
     call MYOPEN (JOREGT,CNAME,1,133, 0,1,1,0,KODE)
     do while (KODE /= 0)
        ICOUNT = ICOUNT + 1
        if (LFULLESTAB) then
           call SLEEP(100)
        else
           call SLEEP(1)
        end if
        call MYOPEN (JOREGT,CNAME,1,133, 0,1,1,0,KODE)
        if (ICOUNT .ge.5) then
           write(*,'(''OPEN FAILED FOR '',I4)') JOREGT
           exit
        end if
     end do
  end if
  return

  !     CALLED FROM INITRE WHEN NOTREES KEYWORD IS ENTERED.

  entry ESEZCR (JSTND,LKECHO)
  INADV = 1
  if (LKECHO)write(JSTND,30)
  30 format(T12,'(EZCRUISE OPTION IN REGENERATION ESTABLISHMENT ',&
       &'MODEL IS AUTOMATICALLY INVOKED.)'  )
  return
end
