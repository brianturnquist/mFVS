!===================!
!   wwpb : bmpstc   !
!===================!
! WWPB $Id$
subroutine BMPSTC (IYR)

  !     CALLED FROM: BMDRV

  ! **BMPSTC:   Date of last revision: June 22, 1994

  ! Pesticide spraying is done only in high quality stand in which there
  ! are no beetles already. It is 100% effective at killing beetles that
  ! try to land on the trees, and lasts for 2 years on the trees.
  ! Keyword: SPRAY yrstart, prop. stand sprayed

  ! Definitions
  !     PSPRAY:     prop sprayed host trees in stand (100% effective)
  !     INFEST:     0,1 switch for if beetles are present

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
  integer :: INFEST
  integer :: ISIZ
  integer :: SCNT,MYLST(MXSTND)
  logical :: LOK
  real    :: PSPRAY

  save

  dimension PRMS(1)

  !     Move spray value for second year into this year's slot in case
  !     there is no pesticide applied this year

  do ISTD = 1,BMSTND
     if (STOCK(ISTD)) then
        SPRAY(ISTD,1) = SPRAY(ISTD,2)
        SPRAY(ISTD,2) = 0.0
     end if
     3 continue
  end do

  !.... Check for debug.

  if (LBMDEB) write(JBMBPR,10) IYR
  10 format(' Begin BMPSTC: Year = ',I5)

  !     Check if this is year for pesticide

  IYR1 = 0
  NPRMS = 1
  IYR1 = IYR
  call GPGET2 (304,IYR1,7,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
  if (LOK) then
     PSPRAY = PRMS(1)
     do I = 1, SCNT

        ISTD = MYLST(I)
        if (.not.STOCK(ISTD) .or. ISTD .le.0) goto 200

        INFEST = 0
        do ISIZ = 1,NSCL

           !           if there are any pests in the stand then no spraying occurs
           if (PBKILL(ISTD,ISIZ) .gt.0.0) then
              INFEST = 1

              if (LBMDEB) write(JBMBPR,15) ISTD
              15 format(' Pests present in stand ',I5,&
                   &' so no pesticide was applied')

              goto 200

           end if

           20 continue
        end do

        if (INFEST .eq.0) then

           !           Pesticide is effective for two years so put in both slots of array
           SPRAY(ISTD,1) = PSPRAY
           SPRAY(ISTD,2) = PSPRAY
        end if

        200 continue
     end do
  end if

  if (LBMDEB) write(JBMBPR,90) IYR
  90 format(' End BMPSTC: Year = ',I5)

  return
end
