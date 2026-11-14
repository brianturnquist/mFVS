!===================!
!   wwpb : bmslsh   !
!===================!
! WWPB $Id$
subroutine BMSLSH (ISPC,TTREE,TVL,ISTD)

  !     CALLED FROM: CUTS   [PROGNOSIS]
  !                  BMSANI [BEETLE]
  !                  BMSALV [BEETLE]

  ! **BMSLSH    Date of last revision: Dec 6, 2006 [AJM]
  !             Date of last revision: July 8, 1994

  ! Calculate tons/acre and eqivalent stems/acre slash produced as
  ! a result of various thinning, salvage and sanitation cuts. Even if
  ! not a host type, slash is added to standing and dead wood.

  ! Definitions
  !     VOLCUT  Volume harvested
  !     PDOWN   Prop slash down on ground
  !     PSLSH   Prop of standing and downed in each size class
  !     T2STEM  Tons/ac -> stem/ac conversion factor
  !     VOL2SL  Total volume of harvest -> total weight of resulting slash

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPCNTL.f90'
  include 'BMPRM.f90'

  !.... Common include files.

  include 'BMCOM.f90'

  !.... Variable declarations.

  ! Variables
  logical :: LX
  real    :: NEWSL
  real    :: PDOWN
  real    :: PSLSH(2,MXDWHZ+1)
  real    :: T2STEM(MXDWHZ)
  real    :: VOL2SL
  real    :: Z

  data T2STEM /12,1.4,0.33/
  !     Note: T2STEM is calculated based on 1t=83.3cu.ft and sample trees
  !           with the following characteristics:  sc1: 6.5"dbh, 60'ht;
  !           sc2: 15"dbh, 95'ht; sc3: 25"dbh, 120'ht
  !          and a form factor = 0.5 for each.
  data ((PSLSH(I,J), J = 1,MXDWHZ+1), I = 1,2)&
       &/0.1,0.3,0.6,0.0, 0.7,0.2,0.2,0.0/
  !     Note: PSLSH assumes different proportions for standing & downed wood.
  !           Standing wood is the first 4 values (different size classes),
  !           and downed is last 4
  data VOL2SL /0.005/
  !     Note: VOL2SL assumes that slash equals 40% of the weight of the
  !           total volume

  PDOWN = 0.95
  LX = .false.

  if (ISTD .eq.0) then
     !        Routine was called from CUTS.

     !        If WWPBM not invoked, bail [AJM 12/06]
     if (BMSTND .le.0) goto 100

     !        FIND OUT IF THE CURRENT STAND IS IN THE DISPERSAL GROUP
     call OPBISR (BMSTND,BMSDIX,ISTND,ISTD)

     !        IF IBMSTD IS ZERO, THEN BRANCH TO EXIT.

     if (ISTD .le.0) goto 100

     !        Determine whether tree species is host or non-host
     if (  (((PBSPEC .eq.1) .or. (PBSPEC .eq.4))&
          &.and. (HSPEC(1,ISPC) .eq.1))&
          &.or. (((PBSPEC .eq.2) .or. (PBSPEC .eq.4))&
          &.and. (HSPEC(2,ISPC) .eq.1))&
          &.or. ((PBSPEC .eq.2) .and. (HSPEC(2,ISPC) .eq.1))&
          &.or. ((PBSPEC .eq.3) .and. (HSPEC(3,ISPC) .eq.1)) )&
          &LX = .true.

     !        Determine how much volume was cut
     VOLCUT = TTREE * TVL
     IPC = ISPFLL(ISPC)
  else
     !        Routine called from salvage or sanitation cuts

     !        Determine how much volume was cut
     VOLCUT = TTREE * TVL
     IPC = ISPC
     if (IPC .eq. IQPTYP(ISTD,1)) LX = .true.
  end if

  !     This is the new way of figuring out the amount of slash that is left behind
  !     after a cut.

  NEWSL = VOL2SL * VOLCUT

  Z = 1.0 - PDOWN
  if (LX) then
     do I = 1, MXDWHZ
        !         (need to make this J because class 1 of pslash is < 3in, while class 1 of
        !             dwphos is 3-10)
        J = I + 1
        DWPHOS(ISTD,2,I) = DWPHOS(ISTD,2,I) + NEWSL * PSLSH(2,J) *&
             &PDOWN * T2STEM(I)
        DWPHOS(ISTD,1,I) = DWPHOS(ISTD,1,I) + NEWSL * PSLSH(1,J) *&
             &Z * T2STEM(I)

        40 continue
     end do
  end if
  do I = 1, (MXDWHZ + 1)
     JSC = MIN0(MXDWSZ,I)

     DDWP(ISTD,JSC,1) = DDWP(ISTD,JSC,1) + NEWSL * PSLSH(2,I) * PDOWN

     SDWP(ISTD,IPC,I,1) = SDWP(ISTD,IPC,I,1) + NEWSL * PSLSH(1,I)&
          &* Z / V2T
     50 continue
  end do

  !      IF(LBMDEB) WRITE(JBMBPR,90) IYR, ISTD
  !   90 FORMAT(' End BMSLSH: Year = ',I5,' Stand = ',I6)

  100 continue

  return
end
