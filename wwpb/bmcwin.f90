!===================!
!   wwpb : bmcwin   !
!===================!
! WWPB $Id$
subroutine BMCWIN (ISTD,IYR)

  !     CALLED FROM:  BMDRV
  !     CALLS   GPGET2
  !             GPADD
  !             PCTILE
  !             SPLAAR

  !  **BMCWIN  Date of last revision:  June 9, 1994

  !  Purpose:
  !     Windthrow Model adapted from the windthrow model in the
  !     Western Root Disease Model.
  !     Windthrow affects all trees equally, regardless of root infection
  !       or stress.

  !  Call list definitions:

  !  Local variable definitions:
  !     CRASH:  Prop'n of eligible stems to windthrow
  !     IPNT:   Pointer array for height order (for PCTILE)
  !     ISIZ:   Loop counter for size classes
  !     ITYP:   Loop counter over tree types
  !     MWINHT  The minimum height that a tree must be before it can be windthrown
  !     ROCR:   Sum of tree density*crown dominance
  !     ROWIND: Proportion of eligible stems to knock over
  !     SAREA:  Area of current stand
  !     SUM:    Total susceptibility (used to normalize susceptibilities)
  !     STEMS:  Stem frequency by tree type
  !     TBPROB: Total # of trees to windthrow
  !     TEMP:   Temporary array of heights (for passing to PCTILE)
  !     TEMP2:  Temporary pointer (for sorting)
  !     TPCT:   Array holding height percentiles
  !     TSTEMS: Total stem frequency
  !     TT:     Density of trees windthrown
  !     THRESH: Min # stems to windthrow
  !     WDOM:   Min dom at which trees of each type are eliglible for windthrow
  !     WINDN:  Total number of eligible stems to throw
  !     WINDSP: Number of trees to windthrow
  !     WINDW:  Tree density * crown dominance
  !     WSUSC:  Susceptibility of tree types (i) to windthrow
  !             (j=2=normalized susc)
  !     WDOM:   Min dom at which trees of each type are eliglible for windthrow

  !  Common block variables and parameters:
  !     DWPHOS: From BMCOM; Array to hold density of downed host trees for
  !             Ips (stratified by standing/dead and size class)
  !     OAKILL: Array containing the proportion of trees in each size class
  !             and tree type killed by "other agents" (i.e. not pine beetles)

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
  integer :: ISIZ,ITYP,IJ
  integer :: IEND
  integer :: DUM(1)
  integer :: IPNT(NSCL+NSCL)

  real    :: SUM
  real    :: TBPROB
  real    :: TEMP(NSCL+NSCL),TEMP2
  real    :: TT,TVAL
  real    :: TSTEMS
  real    :: TPCT(NSCL+NSCL)
  real    :: WSUSC(2,2)

  real    :: WINDSP(2),ROCR(2),STEMS(2)
  real    :: WINDW(NSCL,2),WDOM(2)

  dimension PRMS(4)
  logical :: LOK

  save

  !.... Initializations

  !     maybe these should be initialized elsewhere?

  !     from root disease manual (.028 is white pine, lodgepole pine;i
  !      0.111 is western & mountain hemlock, red cedar)

  WSUSC(1, 1) = .028
  WSUSC(2, 1) = .111

  !     should be 80-100% (from root disease manual)

  WDOM(1) = 80
  WDOM(2) = 85

  !.... Check for debug.

  if (LBMDEB) write(JBMBPR,10) IYR, ISTD
  10 format(' Begin BMCWIN: Year = ',I5, 'Stand = ', I6)

  !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

  if (ICNT .ge. BMEND) ICNT = 0
  ICNT = ICNT + 1

  !     Check if this is a year for windthrow

  if (ICNT .eq.1) then

     IYR1 = IYR
     NPRMS = 4

     call GPGET2 (305, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

     if (LOK) then

        IYR2 = IYR1 + IFIX(PRMS(1)) - 1
        MWINHT = PRMS(2)
        THRESH = PRMS(3)
        CRASH = PRMS(4)

        if (LBMDEB) write (JBMBPR,71) MICYC,IYR1,IYR2,PRMS(4),PRMS(3)
        71 format (/' BMCWIN: MICYC=', I5,' IYR1=',I5,' IYR2=',I5,&
             &' CRASH=',F7.4,' THRESH=',F7.4)

        if (IYR2.ge.MIY(MICYC)) then
           PRMS(1) = IYR2 - MIY(MICYC) + 1
           call GPADD (KODE, MIY(MICYC), 305, NPRMS, PRMS(1), 1, DUM)
           IYR2 = MIY(MICYC) - 1
           if (LBMDEB) write (JBMBPR,81) PRMS(1),IYR2
           81 format (/' IN BMCWIN:  WINDTHROW HAS BEEN SCHEDULED FOR ',&
                &'THE NEXT MASTER CYCLE. DURATION WILL BE ',F5.0,&
                &' NEW IYR2=',I5)
        end if
     end if
     if (IYR .gt. IYR2) CRASH = 0.0
  end if

  !     If windthrow is not active this year then exit out of routine.

  if (IYR .gt. IYR2.or. CRASH .le.0.0) return

  call SPLAAR(ISTD,SAREA,IRC)

  !.... -------------------- Begin Routine ----------------------------

  !.... normalize relative susceptibility of different species to windthrow

  !.... WSUSC(i,1)=susc of type i to windthrow (type 1=host, 2=nonhost)

  SUM = WSUSC(1, 1) + WSUSC(2, 1)

  !.... WSUSC(i,2) = normalized susc.

  WSUSC(1, 2) = WSUSC(1, 1) / SUM
  WSUSC(2, 2) = WSUSC(2, 1) / SUM

  !.... Sum stems by type (host,nonhost) and build pointer lists
  !     to dominant/codominant classes

  IJ = 0
  do ITYP = 1,2
     do ISIZ = 1,NSCL
        IJ = IJ + 1
        TEMP(IJ) = HTS(ISTD,ISIZ,ITYP)
        IPNT(IJ) = IJ
        1100 continue
     end do
     1000 continue
  end do

  !     Set up pointers to the hts in decending order for use by the PCTILE routine

  IEND = 2 * NSCL - 1
  do IJ = 1,IEND
     do I = 1,IEND
        if (TEMP(IPNT(I)) .lt. TEMP(IPNT(I+1))) then
           TEMP2 = IPNT(I)
           IPNT(I) = IPNT(I+1)
           IPNT(I+1) = TEMP2
        end if
        2100 continue
     end do
     2000 continue
  end do

  call PCTILE(NSCL+NSCL,IPNT,TEMP,TPCT,TVAL)

  IJ = 0
  do ITYP = 1,2

     WINDSP(ITYP) = 0.0
     STEMS(ITYP) = 0.0
     ROCR(ITYP) = 0.0

     do ISIZ = 1,NSCL
        IJ = IJ + 1

        if ((TPCT(IJ) .ge. WDOM(ITYP)) .and.&
             &(HTS(ISTD,ISIZ,ITYP) .ge. MWINHT)) then
           if (TREE(ISTD,ISIZ,ITYP) .gt.0.0) then
              STEMS(ITYP) = STEMS(ITYP) + TREE(ISTD,ISIZ,ITYP)
              WINDW(ISIZ,ITYP) = (CRS(ISTD,ISIZ,ITYP) / 100.0)&
                   &* TREE(ISTD,ISIZ,ITYP)
              ROCR(ITYP) = ROCR(ITYP) + WINDW(ISIZ,ITYP)
           end if
        end if

        3100 continue
     end do
     3000 continue
  end do

  TSTEMS = 0.0
  TBPROB = 0.0
  do ITYP = 1,2
     TSTEMS = TSTEMS + STEMS(ITYP)
     WINDSP(ITYP) = STEMS(ITYP) * WSUSC(ITYP, 2)
     TBPROB = TBPROB + WINDSP(ITYP)
     4000 continue
  end do

  !...  Total number of stems to windthrow

  WINDN = TSTEMS * CRASH
  if (WINDN .gt. TSTEMS) WINDN = TSTEMS

  !.... Allocate windthrow to types, assuming that the density of stems
  !.... is above min

  if (WINDN .ge. THRESH) then

     do ITYP = 1,2
        WINDSP(ITYP) = WINDN * (WINDSP(ITYP) / TBPROB)
        5000 continue
     end do

     !....    Now allocate these proportions into the "other agent kill" list
     !        NB: BLOWDOWN IS NOT A PROPORTION!
     !        I assume that no BLOWDN trees are left standing (pun, ahem!)

     do ISIZ = 1,NSCL
        do ITYP = 1,2
           if ((TREE(ISTD,ISIZ,ITYP) .gt.0.0)&
                &.and. (ROCR(ITYP) .gt.0.0)) then
              TT = WINDSP(ITYP) * WINDW(ISIZ,ITYP) / ROCR(ITYP)
              if (TT .gt. (TREE(ISTD,ISIZ,ITYP) * .95))&
                   &TT = TREE(ISTD,ISIZ,ITYP) * .95

              J = L2D(ISIZ)
              if (ITYP .eq.1.and. J .gt.0)&
                   &DWPHOS(ISTD,2,J) = DWPHOS(ISTD,2,J) + TT

              OAKILL(ISTD,ISIZ,ITYP) = OAKILL(ISTD,ISIZ,ITYP)&
                   &+ TT / TREE(ISTD,ISIZ,ITYP)

              TT = TT * TVOL(ISTD,ISIZ,ITYP) * V2T
              if (J .ge.1) then
                 !                    large downed dead wood
                 DDWP(ISTD,2,1) = DDWP(ISTD,2,1) + TT
              else
                 !                    small downed dead wood
                 DDWP(ISTD,1,1) = DDWP(ISTD,1,1) + TT
              end if
           end if
           6100 continue
        end do
        6000 continue
     end do

  else

     !....  if the windthrow event does not occur then reschedule the event for next year

     do ITYP = 1,2
        WINDSP(ITYP) = 0
        7000 continue
     end do
     WINDN = 0

  end if

  !.... Common Return

  9999 continue

  if (LBMDEB) write(JBMBPR,99) IYR
  99 format(' End BMCWIN: Year = ',I5)

  return
end
