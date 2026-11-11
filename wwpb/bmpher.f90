!===================!
!   wwpb : bmpher   !
!===================!
! WWPB $Id$
subroutine BMPHER (IYR)

  !     CALLED FROM:  BMDRV
  !     ENTRY POINTS: BMAPH
  !     CALLS:        SPLAAR

  !     **BMPHER     Date of last revision:  June 21, 1994

  ! This subroutine simulates application of attracting or repelling
  ! pheromones. Attracting pheromone makes "special" trees in the stand.
  ! Repelling pheromone reduces the amount of host basal area that the
  ! beetles can sense.
  !      'Keyword: APHERO yrstart, %efficiency     (attracting)
  !      '         RPHERO yrstart, %efficiency     (repelling)

  ! Definitions:
  !     PROPEFF: Proportion of trees which are sprayed with repelling
  !              pheromone
  !     REPPHE:  Multiplier reducing the attractiveness of host BA due to
  !              the repelling pheromone (i.e. if 30% of trees are not
  !              sprayed then only 30% of BA is sensed as attractive).

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPCNTL.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'BMCOM.f90'

  parameter(MAXPRM = 7)

  !.... Variable declarations.

  ! Variables
  logical :: LOK
  real    :: INFEST
  integer :: SCNT
  integer :: IRC
  integer :: ISIZ
  integer :: MYLST(MXSTND)
  real    :: PRMS(MAXPRM)
  real    :: PROPEFF
  real    :: TOTREE

  !     Initializations

  IYR1 = 0

  if (LBMDEB) write(JBMBPR,10) IYR, ISTD
  10 format(' Begin BMPHERO: Year= ',I5, 'Stand= ', I6)

  do ISTD = 1, BMSTND
     if (STOCK(ISTD)) REPPHE(ISTD) = 1.0
     15 continue
  end do

  NPRMS = 1
  IYR1 = IYR
  call GPGET2 (302,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
  if (LOK) then
     !         convert from a percent to a proportion
     PROPEFF = PRMS(1) / 100
     if (PROPEFF .le.0.0) return

     do I = 1, SCNT

        ISTD = MYLST(I)
        if (.not.STOCK(ISTD) .or. ISTD .le.0) goto 33

        INFEST = 0.0
        TOTREE = 0.0
        do ISIZ = 1,NSCL
           INFEST = INFEST + PBKILL(ISTD,ISIZ)
           TOTREE = TOTREE + TREE(ISTD,ISIZ,1)
           20 continue
        end do

        !         Repelling pheromone
        !         only works in large MPB stands that are less than 10% infested
        !         (Note that TOTREE does not include PBKilled trees)

        call SPLAAR(ISTD,SAREA,IRC)

        if (TOTREE .gt.0.0) then
           if ((PBSPEC .eq.1) .and. (SAREA .gt.2.5) .and.&
                &(INFEST / (INFEST + TOTREE) .le.0.1)) then

              !             Reduce host BA attractiveness by prop trees sprayed

              REPPHE(ISTD) = (1.0 - PROPEFF)
           else
              if (LBMDEB) write(JBMBPR,50)
              50 format('  IN BMPHERO: Conditions not right for',&
                   &' repelling pheromone.')
              go to 80
           end if
        end if

        80 continue

        33 continue
     end do
  end if

  if (LBMDEB) write(JBMBPR,99) IYR, ISTD
  99 format(' End BMPHERO: Year= ',I5, 'Stand= ', I6)

  return

  !     Entry for Attracting pheromone

  entry BMAPH (IYR)

  !     Zero the array.

  do ISTD = 1, BMSTND
     if (STOCK(ISTD)) ATRPHE(ISTD) = 0.0
     100 continue
  end do

  !     See if the option needs to be done. If it does, copy the
  !     *proportion* (converted from %) to stand locations in the
  !     ATRPHE array

  IYR1 = 0
  NPRMS = 1
  IYR1 = IYR
  call GPGET2 (303,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
  if (LOK) then
     X = PRMS(1) * 0.01
     do I = 1, SCNT
        ISTD = MYLST(I)
        if (STOCK(ISTD) .and. ISTD .gt.0) ATRPHE(ISTD) = X
        200 continue
     end do
  end if

  if (LBMDEB) write(JBMBPR,900) IYR
  900 format('Did BMAPHER: Year= ',I5)

  return
end
