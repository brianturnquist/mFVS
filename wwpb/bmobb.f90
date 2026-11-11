!==================!
!   wwpb : bmobb   !
!==================!
! WWPB $Id$
subroutine BMOBB(ISTD,IYR)

  !  BARK BEETLE FUNCTIONAL TYPE 3, EXCEPT THAT THE BEETLES DON'T ACTUALLY KILL TREES; THEY
  !  ONLY SET THEM UP TO BE ATTACKED BY THE MAIN BEETLE SPECIES.  THE MODEL REMAINS ON ONLY
  !  FOR THE DURATION THAT THE USER SPECIFIED.

  !  CALLED BY :
  !     BMDRV

  !  CALLS     :
  !     GPGET2
  !     GPADD

  !  PARAMETERS :
  !     MINSIZ: Minimum size class for killing
  !     THRESH: Threshold for outbreak to occur
  !     ATTKRT: Tree attack rate
  !     GRFMAX: Level above which tree is too healthy (GRF too high)

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
  logical :: LOK
  integer :: ISIZ,MINSIZ
  integer :: DUM(1)
  real    :: GRFMAX
  real    :: ATTKRT
  real    :: THRESH
  real    :: CLSUMI
  dimension PRMS(7)

  save

  CLSUMI = 0.0

  !.... Check for debug.

  if (LBMDEB) write (JBMBPR,3) IYR, ISTD
  3 format (' Begin BMOBB:  Year=',I5, ' Stand= ',I6)

  do ISIZ = 1,NSCL
     OTHATT(ISTD,ISIZ) = 0.0
     117 continue
  end do

  !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

  if (ICNT .ge. BMEND) ICNT = 0
  ICNT = ICNT + 1

  if (ICNT .eq.1) then

     IYR1 = IYR
     NPRMS = 5

     !     FETCH THE MOST RECENTLY SCEHEDULED UNTRIGGERED ACTIVITY. IF THERE
     !     ARE ANY SET FOR THIS YEAR (IYR), THEN THEY TAKE OVER ANY CURRENT
     !     SET OF PARAMETERS.

     call GPGET2 (323, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

     if (LOK) then

        IYR2 = IYR1 + IFIX(PRMS(1)) - 1

        MINSIZ = INT(PRMS(2))
        THRESH = PRMS(3)
        ATTKRT = PRMS(4)
        GRFMAX = PRMS(5)

        if (LBMDEB) write (JBMBPR,101) IYR1,MINSIZ,THRESH,&
             &ATTKRT,GRFMAX
        101 format (/' IN BMOBB: IYR1=',I5,' MINSIZ=',&
             &I5,' THRESHOLD=',F7.4,' ATTACK RATE= ',F6.4,' MAX RV=',F4.2)

        if (IYR2.ge.MIY(MICYC)) then
           PRMS(1) = IYR2 - MIY(MICYC) + 1
           call GPADD (KODE, MIY(MICYC), 323, NPRMS, PRMS, 1, DUM)
           IYR2 = MIY(MICYC) - 1
           if (LBMDEB) write (JBMBPR,103) PRMS(1),IYR2
           103 format (/' IN BMOBB: OTHER BARK BEETLES ARE SCHEDULED FOR ',&
                &'THE NEXT MASTER CYCLE. DURATION WILL BE =',F5.0,&
                &'  NEW IYR2=',I5)
        end if
     else
        !         Zero out the attact rate to signify that bark beetles are not active this year.
        if (IYR .gt. IYR2) ATTKRT = 0.0
     end if
  end if

  !     IF THE ATTACK RATE IS ZERO THEN ASSUME THE MODEL IS NOT ACTIVE THIS YEAR.
  if (ATTKRT .le.0.0) return

  !     IF NO DESIRED SPECIES PRESENT THEN RETURN.

  if (BAH(ISTD,NSCL+1) .le.0.0) goto 777

  !     LOOP OVER TREE LIST AND CALCULATE THE
  !     NUMBER OF TARGET STEMS, DEFINED AS THOSE
  !     HAVING A LOWER GRF THAN A USER SPECIFIED LIMIT
  !     (NONHOSTS HAVE NO GRF SO ALL TREES ARE ELIGIBLE)

  do ISIZ = MINSIZ, NSCL
     if (GRF(ISTD,ISIZ) .gt. GRFMAX) goto 6
     CLSUMI = CLSUMI + TREE(ISTD,ISIZ,1)
     6 continue
  end do

  !     COMPARE DENSITY OF STEMS TO THRESHOLD SPECIFIED BY USER

  if (CLSUMI .lt. THRESH) goto 777

  !     SUFFICIENT STEMS FOR OUTBREAK, NOW APPLY SPECIFIC ATTACK RATE.

  do ISIZ = MINSIZ,NSCL

     if (GRF(ISTD,ISIZ) .gt. GRFMAX) goto 20

     OTHATT(ISTD,ISIZ) = OTHATT(ISTD,ISIZ) + ATTKRT
     OTHATT(ISTD,ISIZ) = AMIN1(OTHATT(ISTD,ISIZ),1.0)

     20 continue
  end do

  777 continue

  if (LBMDEB) write (JBMBPR,999) IYR, ISTD
  999 format (' End BMOBB :  Year=',I5, ' Stand= ',I6)

  return
end
