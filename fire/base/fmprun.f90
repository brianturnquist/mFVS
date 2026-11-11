!========================!
!   fire/base : fmprun   !
!========================!
! FIRE-BASE $Id$
subroutine FMPRUN(CTCRWN)
  implicit none

  !     PART OF THE FIRE MODEL EXTENSION.
  !     THIS ROUTINE ADDS THE MATERIAL THAT WAS PRUNED IN CUTS.
  !     CALLED FROM -- CUTS

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'FMCOM.f90'
  include 'FMFCOM.f90'

  dimension CTCRWN(MAXTRE)
  ! Variables
  logical :: DEBUG
  integer :: I,IDC,ISZ
  real    :: CTCRWN,X

  call DBCHK (DEBUG,'FMPRUN',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC,LFMON
  7 format(' ENTERING FMPRUN CYCLE = ',I2,' LFMON=',L2)

  if (.not.LFMON) return

  do I = 1,ITRN

     IDC = DKRCLS(ISP(I))
     X = CTCRWN(I)*P2T
     CWD(1,10,2,IDC) = CWD(1,10,2,IDC) + (CROWNW(I,0) * X)
     do ISZ = 1,5
        CWD(1,ISZ,2,IDC) = CWD(1,ISZ,2,IDC) + (CROWNW(I,ISZ) * X)
     end do

     if (DEBUG) write (JOSTND,10) I,CTCRWN(I),DBH(I),ISP(I),IDC,X,&
          &(CROWNW(I,ISZ),ISZ = 0,5)
     10 format (' I=',I4,' CTCRWN=',F5.3,' DBH=',F7.2,' ISP=',I3,&
          &' IDC=',I2,' X=',E10.4,' CROWNW=',6F9.2)

  end do

  !     Update the crown weight material arrays for post-prune values.

  call FMCROW

  return
end
