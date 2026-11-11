!===================!
!   wwpb : bmcspt   !
!===================!
! WWPB $Id$
subroutine BMCSPT(ISTD,ISIZ,IPASS,IYR)

  !     CALLED FROM: BMCNUM

  !  **BMCSPT  Date of last revision:  March 28, 1994

  !  Purpose:

  !  Local variable definitions:
  !     II:     Loop counter over max # special tree types
  !     IJ:     Loop counter over
  !     ISIZ:   Loop counter over dbh size classes
  !     SP:     Holder for prop'n special trees of each type

  !  Common block variables and parameters:
  !     ATRPHE: PROPORTION of trees with attractant pheromone
  !     OTHATT: PROPORTION of trees attacked, but not killed
  !     PBSPEC: Pine Beetle specie(s) being simulated
  !     PITCH:  PROPORTION of trees with pitch-out or strip-kill
  !     SCORCH: PROPORTION of trees in size class severely scorched by fire
  !     SPCLT:  PROPORTION of special trees in each dbh class
  !     STRIKE: PROPORTION of trees in class struck by lightning
  !     TOPKLL: PROPORTION of stems attacked by Ips in each size class

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: TOPFLAG,BOTFLAG
  integer :: II,IJ,IK,JJ,JK
  integer :: SPTCNT
  real    :: SP(MXSPTT)
  real    :: XSPLT
  real    :: MT,MTT,MTTT

  if (LBMDEB) write(JBMBPR,10) IYR, ISTD, ISIZ, IPASS
  10 format(' Begin BMCSPT: Year= ',I5, 'Stand= ', I6,&
       &'Size class= ', I3, 'Pass= ', I2)

  !.... if there are no trees in size class then don't need to calc anything

  XSPLT = 0.0
  if (TREE(ISTD,ISIZ,1) .le.1.0E-9) goto 9999

  !....   calculate prop. special trees

  !....   Mountain Pine Beetle

  if ((PBSPEC .eq.1) .and. (IPASS .eq.1)) then

     SP(1) = PITCH(ISTD,ISIZ)
     SP(2) = STRIKE(ISTD,ISIZ)
     SP(3) = TOPKLL(ISTD,ISIZ) + OTHATT(ISTD,ISIZ)
     SP(4) = SCORCH(ISTD,ISIZ)
     SP(5) = ATRPHE(ISTD)
     !        SP(6)= RROT(ISTD,ISIZ) * TREE(ISTD,ISIZ,1)
     !        IF (SP(6) .GT. 1.0) SP(6) = 1.0

     SPTCNT = 5

     !....      Western Pine Beetle

  else if ((PBSPEC .eq.2) .and. (IPASS .eq.1)) then

     SP(1) = PITCH(ISTD,ISIZ)
     SP(2) = STRIKE(ISTD,ISIZ)
     SP(3) = TOPKLL(ISTD,ISIZ) + OTHATT(ISTD,ISIZ)
     SP(4) = SCORCH(ISTD,ISIZ)
     SP(5) = ATRPHE(ISTD)
     !        SP(6)= RROT(ISTD,ISIZ) * TREE(ISTD,ISIZ,1)
     !        IF (SP(6) .GT. 1.0) SP(6) = 1.0

     SPTCNT = 5

  else if ((PBSPEC .eq.3) .or. (IPASS .eq.2)) then

     !....      Ips
     !          The other things Ips like are not living trees
     !          so they are used in calculations elsewhere

     SP(1) = OTHATT(ISTD,ISIZ)
     SP(2) = ATRPHE(ISTD)
     SPTCNT = 2

  end if

  TOPFLAG = .true.
  do I = 1,SPTCNT
     if (SP(I) .eq.1.0) then
        TOPFLAG = .false.
        goto 21
     end if
     20 continue
  end do
  21 continue

  BOTFLAG = .false.
  do I = 1,SPTCNT
     if (SP(I) .ne.0.0) then
        BOTFLAG = .true.
        goto 31
     end if
     30 continue
  end do
  31 continue

  if (TOPFLAG .and. BOTFLAG) then

     do II = 1,SPTCNT

        !....          special trees if no overlap

        XSPLT = XSPLT + SP(II)

        do IJ = II+1,SPTCNT

           !....             all this multiplication accounts for
           !                 overlap. eg. struck trees that were
           !                 pitchouts and fire scorched etc.

           MT = SP(II) * SP(IJ)
           do IK = IJ + 1,SPTCNT
              MTT = MT * SP(IK)
              do JJ = IK + 1,SPTCNT
                 MTTT = MTT * SP(JJ)
                 do JK = JK + 1,SPTCNT
                    XSPLT = XSPLT - MT - MTT - MTTT - (MTTT * SP(JK))
                    1115 continue
                 end do
                 1112 continue
              end do
              1110 continue
           end do
           1100 continue
        end do
        1000 continue
     end do

     if (XSPLT .gt.1.0) XSPLT = 1.0

  else if (.not.BOTFLAG) then

     !....       if all props= 0 then special= 0

     XSPLT = 0.0

  else if (.not.TOPFLAG) then

     !....    if any one of the props= 1 then special= 1

     XSPLT = 1.0

  end if

  9999 continue

  SPCLT(ISTD,ISIZ,IPASS) = XSPLT

  if (LBMDEB) write(JBMBPR,11) IYR, ISTD
  11 format(' End BMCSPT: Year= ',I5, 'Stand= ', I6)

  return
end
