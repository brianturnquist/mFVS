!===================!
!   wwpb : bmdfol   !
!===================!
! WWPB $Id$
subroutine BMDFOL (ISTD,IYR)

  !     CALLED FROM BMDRV

  ! **BMDFOL    Date of last revision:  June 14, 1994

  !  Definitions:
  !     ITYP:   Type of tree: 1=host (PB host), 2=non-host

  !  Common block variables and parameters:

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.
  include 'PPCNTL.f90'
  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  integer :: ISIZ,MINSIZ,MAXSIZ
  integer :: DUM(1)
  logical :: LOK
  real    :: PATTCK
  real    :: PRMS(4)

  save

  if (LBMDEB) write(JBMBPR,10) IYR, ISTD
  10 format(' Begin BMDFOL: Year= ',I5, 'Stand= ', I6)

  !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

  if (ICNT .ge. BMEND) ICNT = 0
  ICNT = ICNT + 1

  !     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
  !     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
  !     SET OF PARAMETERS.

  if (ICNT .eq.1) then

     IYR1 = IYR
     NPRMS = 4

     call GPGET2 (314, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

     if (LOK) then

        IYR2 = IYR1 + IFIX(PRMS(1)) - 1

        PATTCK = PRMS(2)
        MINSIZ = INT(PRMS(3))
        MAXSIZ = INT(PRMS(4))

        if (LBMDEB) write (JBMBPR,101) IYR1,PATTCK,MINSIZ,MAXSIZ
        101 format (/' IN BMDFOL: IYR1=',I5,' PATTCK=',F5.3,&
             &' MINSIZE=',I4,' MAXSIZE=',I4)

        if (IYR2.ge.MIY(MICYC)) then
           PRMS(1) = IYR2 - MIY(MICYC) + 1
           call GPADD (KODE, MIY(MICYC), 314, NPRMS, PRMS, 1, DUM)
           IYR2 = MIY(MICYC) - 1
           if (LBMDEB) write (JBMBPR,103) PRMS(1),IYR2
           103 format (/' IN BMDFOL: DEFOLIATORS ARE SCHEDULED FOR ',&
                &'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,&
                &'  NEW IYR2=',I5)
        end if
     else
        !         Zero out the attact rate to signify that defoliators are not active this year.
        if (IYR .gt. IYR2) PATTCK = 0.0
     end if
  end if

  do ISIZ = 1,NSCL
     RVDFOL(ISTD,ISIZ) = 1.0
     30 continue
  end do

  !     LEAVE ROUTINE IF THERE ARE NO TREES ATTACKED.

  if (PATTCK .le.0.0) return

  !     RV FROM DEFOLIATORS IS SIMPLY 1 - PROP. TREES > 70% ATTACKED (user defined)

  do ISIZ = MINSIZ,MAXSIZ
     RVDFOL(ISTD,ISIZ) = 1.0 - PATTCK
     50 continue
  end do

  if (LBMDEB) write(JBMBPR,99) IYR, ISTD
  99 format(' End BMDFOL: Year= ',I5, 'Stand= ', I6)

  return
end
