!===================!
!   wwpb : bmsani   !
!===================!
! WWPB $Id$
subroutine BMSANI (IYR)

  !     CALLED FROM BMDRV
  !     CALLS:  BMSLSH

  ! **BMSANI    Date of last revision:  09/28/05

  !             revised 7/28/05: added new var: REMBKP(ISTD)to bookkeep the BKP removed
  !             via sanitation cutting, by stand. In common.  For output.  AJM
  !             REVISED 6/30/05.  TREE, BAH, and AREMS arrays should only be adjusted
  !             when removing unattacked, low-RV trees.  COmmented out the adjustments
  !             happening to these arrays in the block where recently attacked trees
  !             are being sanitized. AJM
  !             Date of last revision 8/99(AJM).  Commented out block
  !             referring to Target Volume removal method of calculating
  !             amount to sanitize.  Target volume no longer a user-input
  !             option.  The referred-to PRMS(6) does not exist.
  ! **BMSANI    Date of last revision:  October 28, 1998 (RNH, OCT98)
  !             >>>>>Remove low RV trees from highst Size to lowest based on<<<<<
  !             linearly varying efficiency
  ! **BMSANI    Date of last revision:  October 16, 1998 (RNH, OCT98)
  !             Added target volume removal method for sanitize management
  ! **BMSANI    Date of last revision:  September 16, 1998 (RNH, May98)
  !             Added target basal area method for sanitize management
  ! **BMSANI    Date of last revision:  August 5, 1998 (RNH, May98)
  !             Passed MYLSTS, MAXSCS and MINSCS to BMKILL to track
  !             Sanitize removals in BMKILL
  ! **BMSANI    Date of last revision:  June 21, 1994
  ! **BMSANI    Date of last revision:  May 6, 1998 (RNH, May98)
  !            Major revision to run Sanitize as stand level activity,
  !            similar to SALVAGE keyword.  Also modified BMPPIN to
  !            pass user specified stand list, from keyword file or
  !            from user specified file, to BMSANI like SALVAGE keyword

  !  Sanitation cuts are a landscape-level action. If conditions are right,
  !  the cut could occur anywhere in the landscape. All beetle-killed trees
  !  and low RV trees within the given size range will be removed.

  !  NOTE THAT WE MAY WANT TO CHANGE THIS TO MAKE IT STAND-ACTIVATED (LIKE PHEROMONES)
  !            Done May98 by RNH

  !  Definitions:
  !     IPC:    Loop counter over pool types (fast, med, slow)
  !     ISIZ:   Loop counter over size classes
  !     MINSC:  Minimum size class for cut
  !     MAXSC:  Maximum size class for cut
  !     RVMAX:  Maximum RV that will be removed (of unattacked trees)
  !     REMOVE: Amount of TPA removed from one size class of recent kills
  !     SUM:    Total amount of standing dead volume (all classes)
  !     VREMOV: Amount of standing dead volume removed over size classes

  !  Common block variables and parameters:

  !.... Parameter statements.

  !.... Parameter include files.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPCNTL.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'
  include 'BMPCOM.f90' !AJM 9/05 FOR AAREMS AND VOLREM
  !.... Variable declarations.

  ! Variables
  integer :: ISIZ,IPC
  integer :: MINSC,MAXSC
  integer :: DUM(1),INDEX(NSCL)

  !     Add required Variables for stand level simulation (RNH May98)

  integer :: MYLST(MXSTND),SCNT

  logical :: LOK
  real    :: REMPB,REMIPS
  real    :: REMOVE,RVMAX
  real    :: SUM
  real    :: VREMOV,TARVR
  real    :: PRMS(7),AA(NSCL)

  real    :: THRVOL,EFFC

  save

  if (LBMDEB) write(JBMBPR,10) IYR, ISTD
  10 format(' Begin BMSANI: Year= ',I5, 'Stand= ', I6)

  !     Initialization

  !      Move to loop 500

  !      IPC = IQPTYP(ISTD,1)
  !      SUM = 0.0
  !      VREMOV = 0.0

  !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

  !      IF (ICNT .GE. BMEND) ICNT = 0
  !      ICNT = ICNT + 1

  !      IF (ICNT .EQ. 1) THEN

  !     Zero out MYLST array from last year (RNH 10Aug98)

  do IJK = 1, MXSTND
     MYLST(IJK) = 0.0
     VOLREM(IJK,1) = 0.0 !NEW AJM 9/05
     do ISIZ = 1, NSCL
        AAREMS(IJK,ISIZ) = 0.0 !NEW AJM 9/05  THIS WILL BE JUST LIKE AREMS ONLY ZEROED ANNUALLY FOR USE IN BMOUT
        12 continue
     end do
     11 continue
  end do

  IYR1 = IYR
  NPRMS = 6
  MAXPRM = 7 !   From SALVAGE (RNH)

  call GPGET2 (308,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
  !       CALL GPGET2 (308, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)

  !     Set LOKS to control Sanitize accounting in BMKILL (RNH Aug98)
  !     LOKS should remain TRUE thoughout the cycle to trigger calculations
  !     in BMKILL.  LOKS1 is set to FALSE in BMKILL to signal end of SANI action

  LOKS = LOK
  if (LOKS) LOKS1 = .true.
  if (LOKS1) LOKS = .true.

  if (LOK) then

     MINSC = INT(PRMS(1))
     MAXSC = INT(PRMS(2))
     RVMAX = PRMS(3)
     THRVOL = PRMS(4)
     EFFC = PRMS(5)
     TARVR = PRMS(6)
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
     !     The following variables are set here to be passed to BMKILL to
     !     track Sanitization removals in the PPE (MAXSCS, MINSCS,
     !     MYLSTS) (RNH Aug98)

     MINSCS = MINSC
     MAXSCS = MAXSC

     !     Append the MYLSTS array with the current years sanitiation
     !     removals from MYLST

     IJ0 = 1
     do IJ = 1, MXSTND
        if (MYLSTS(IJ) .ne.0) go to 19
        MYLSTS(IJ) = MYLST(IJ0)
        IJ0 = IJ0 + 1
        19 continue
     end do
     !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

     if (LBMDEB) write (JBMBPR,71) MICYC, IYR1, MINSC, MAXSC,&
          &RVMAX, THRVOL, EFFC
     71 format (' BMSANI: MICYC=', I5,' IYR1=',I5,' MINSC=',I4,&
          &' MAXSC=',I4,' RVMAX=',F5.2,' THRVOL=',F8.0,' EFFC=',F5.3)

  else
     EFFC = 0.0
  end if
  !      ENDIF

  if (EFFC .le.0.0) return

  !     Main loop to calculate stands +++++++++++++++++++++++++++++

  !     The folling loop over SCNT was suggested from BMSALV

  do I = 1, SCNT

     ISTD = MYLST(I)
     IPC = IQPTYP(ISTD,1)
     if (.not.STOCK(ISTD).or.ISTD.le.0) goto 500

     !     Total recently killed volume or low RV volume available for cut

     VREMOV = 0.0
     SUM = 0.0
     SUMBA = 0.0

     !  Calc. total basal area of stand (SBAT10)

     !      SBAT10 = 0.0
     !      DO 14 ISBA1 = 1, NSCL
     !      BAHPNH = BAH(ISTD,ISBA1) + BANH(ISTD,ISBA1)
     !      SBAT10 = SBAT10 + BAHPNH
     !   14 CONTINUE

     do ISIZ = MINSC,MAXSC
        SUM = SUM + (PBKILL(ISTD,ISIZ) + ALLKLL(ISTD,ISIZ))&
             &* TVOL(ISTD,ISIZ,1)*0.90
        if (GRF(ISTD,ISIZ) .le. RVMAX)&
             &SUM = SUM + TREE(ISTD,ISIZ,1) * TVOL(ISTD,ISIZ,1)*EFFC

        20 continue
     end do

     !     If not enough volume to warrent a sanitation cut then
     if (SUM .lt. THRVOL) go to 500

     !     If the target basal area method of sanitiation is being used
     !     TARBA < 999. then start calculations below.  Else, brance to
     !     statement 35 to begin sanitation operations based on maximum
     !     rating value method TARBA >= 999. (RNH sep98)

     !      IF (TARVR .GE. 9999.) GO TO 35

     !      WRITE (*,*) SUM,THRVOL
     go to 35
     !                                                                                    !
     !...  Above change made 8/99 (AJM)                                                   !
     !                                                                                    !

     !     New variables                                                                  !
     !     NN        = number of size classes to be included in sanitization              !
     !     AA(MAXSC) = scatch array to pass rating values of size classes                 !
     !                 (MINSC TO MAXSC) to sort routine                                   !
     !     TARVR     = stand target removal volume specified by user in input             !
     !     INDEX(NN) = passed fromn sort routine this array holds the subscrips           !
     !                 of AA in order of accending RV                                     !
     !     INDRV     = subscript values from INDEX transformed to range                   !
     !                 MINSC - MAXSC for use in other arrays                              !
     !     SRVOL1    = temporary variable holding removal volumes, used to compare        !
     !                 with TARVR                                                         !
     !                                                                                    !
     !     The following routine removes infected trees first.  Then the                  !
     !     tree size classes are removed based on rating value, with the                  !
     !     lowest rating value trees cut first, until the target removal volume           !
     !     (TARVR) for the stand is reached.                                              !
     !                                                                                    !
     !     First take out all infected trees                                              !
     !                                                                                    !
     REMLRV = 0.0 !
     SRVOL1 = 0.0 !

     do ISIZ = MINSC,MAXSC !
        !                                                                                    !
        REMPB = PBKILL(ISTD,ISIZ) * 0.90 !
        PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - REMPB !
        !                                                                                    !
        REMIPS = ALLKLL(ISTD,ISIZ) * 0.90 !
        ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) - REMIPS !
        !                                                                                    !
        !     Changed removal efficiency of beetle killed trees to 0.90 obove                !
        !     Accounted for these removal in tree record before removing low                 !
        !     RV trees.  calculated reduction in basal area properly (RNH19Aug)              !
        !                                                                                    !
        TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMPB - REMIPS !
        BAH(ISTD,ISIZ) = BAH(ISTD,ISIZ) - BAH(ISTD,ISIZ)*(REMPB + REMIPS) !
        !                                                                                    !
        SRVOL1 = SRVOL1 +(REMPB + REMIPS)*TVOL(ISTD, ISIZ, 1) !

        REMOVE = REMPB + REMIPS + REMLRV !
        VREMOV = VREMOV + REMOVE * TVOL(ISTD,ISIZ,1) !

        !        Also remove some wood from the current standing dead pools because          !
        !        the killed trees were added to these pools last year (so use subscript      !
        !        2 which means age 1). Multiply REMOVE by the decay factor to account        !
        !        for one year's decay. Note that SDWP is measured in volume.                 !

        JSC = L2D(ISIZ) + 1 !
        JJ = MIN0(JSC,MXDWSZ) !
        SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV)& !
             &* TVOL(ISTD,ISIZ,1) * SDECRT(JJ) !

        SREMOV(ISTD) = SREMOV(ISTD) + REMOVE !
        AREMS(ISTD,ISIZ) = AREMS(ISTD,ISIZ) + REMPB + REMIPS !
        !                                                                                    !
        22 continue !
     end do
     !                                                                                    !
     !*************                                                                       !
     !     Take out rating value ranking code and remove trees from largest               !
     !     size class to lowest based on linearly varying efficiencyN (EFFCLN)            !
     !     RNH October 28, 1998                                                           !
     !*************                                                                       !
     !                                                                                    !
     27 continue ! Branch from end of loop if TARBA not met                !
     !                                                                                    !
     do IJK = MAXSC, MINSC, -1 !
        !                                                                                    !
        !     calculate efficiency based on size class.  Want maximum size class             !
        !     cut at input efficiency and size class 3 cut at efficiency= 0.0                !
        !     The efficiency for cutting size classes (EFFCLN) in between                    !
        !     should vary linearly                                                           !
        !                                                                                    !
        if (IJK .gt.3) then !
           EFFCLN = EFFC*(IJK-3)/(MAXSC-3) !
        else !
           EFFCLN = 1.0E-6 !
        end if !
        !      WRITE(29,*) ' SIZCLASS= ',IJK,' EFFCLN= ',EFFCLN,' ISTD= ',ISTD               !
        !      WRITE(29,*)' MINSC= ',MINSC,' MAXSC= ',MAXSC                                  !
        !                                                                                    !
        if (GRF(ISTD,IJK) .le. RVMAX) then !
           !                                                                                    !
           if (SRVOL1.lt. TARVR) then !
              !                                                                                    !
              !     SRVOL2 is temporary storage for SRVOL1 in refiinement of removals              !
              !     to meet TARVR                                                                  !
              !                                                                                    !
              SRVOL2 = SRVOL1 !
              !                                                                                    !
              REMLRV = TREE(ISTD,IJK,1)*EFFCLN !
              !                                                                                    !
              if (REMLRV .le.1.0E-6) REMLRV = 0.0 !
              !                                                                                    !
              !     Accumulate removals in SRVOL1                                                  !
              !                                                                                    !
              SRVOL1 = SRVOL1 + REMLRV*TVOL(ISTD, IJK, 1) !
              !                                                                                    !
              !      If removal accumulates above TARVR then use efficiency to refine              !
              !      removal volume to approximate TARVR                                           !
              !                                                                                    !
              if (SRVOL1.gt. TARVR) then !

                 EFFCLN1 = (TARVR - SRVOL2)/(TREE(ISTD,IJK,1)*TVOL(ISTD,IJK,1)) !
                 !                                                                                    !
                 !       Recalculate REMLRV based on refined efficiency estimate                      !
                 !                                                                                    !
                 REMLRV = TREE(ISTD,IJK,1)*EFFCLN1 !
                 SRVOL1 = SRVOL2 + REMLRV*TVOL(ISTD,IJK,1) !
                 BAH(ISTD,IJK) = BAH(ISTD,IJK)*(1-EFFCLN1) !
                 IJK1 = IJK !
                 !                                                                                    !
                 !                                                                                    !
                 go to 1010 !
                 !                                                                                    !
              end if !
              !                                                                                    !
              if (BAH(ISTD,IJK) .le.1.0E-6) BAH(ISTD,IJK) = 0.0 !
              !                                                                                    !

              BAH(ISTD,IJK) = BAH(ISTD,IJK) * (1 - EFFCLN) !
              !                                                                                    !
              1010 continue !
              !                                                                                    !
              !+++++++++++++                                         ++++++++++++++                !
              !      Accumulate removal data to pass to BMKILL for Sanitize accounting             !
              !      in PPE (RNH Aug98)                                                            !
              !                                                                                    !
              AREMS(ISTD,IJK) = AREMS(ISTD,IJK) + REMLRV !
              !                                                                                    !
              !++++++++++++                                          ++++++++++++++                !
              !                                                                                    !
              TREE(ISTD,IJK,1) = TREE(ISTD,IJK,1) - REMLRV !
              !                                                                                    !
              REMOVE = REMLRV !
              VREMOV = VREMOV + REMOVE * TVOL(ISTD,IJK,1) !
              !                                                                                    !
              !        Also remove some wood from the current standing dead pools because          !
              !        the killed trees were added to these pools last year (so use subscript      !
              !        2 which means age 1). Multiply REMOVE by the decay factor to account        !
              !        for one year's decay. Note that SDWP is measured in volume.                 !
              !                                                                                    !
              !         JSC = L2D(IJK) + 1                                                         !
              !         JJ = MIN0(JSC,MXDWSZ)                                                      !
              !         SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV)            !
              !     &                                  * TVOL(ISTD,IJK,1)*SDECRT(JJ)               !
              !                                                                                    !
              SREMOV(ISTD) = SREMOV(ISTD) + REMOVE !
              !                                                                                    !
           else !
              !                                                                                    !
              !      Met TARVR criterium, finished with sanitize                                   !
              !                                                                                    !
              !      WRITE(29,*) ' SRVOL1= ',SRVOL1,' SREMOV= ', SREMOV(ISTD)                      !
              !      WRITE(29,*) ' VREMOV= ',VREMOV                                                !
              !                                                                                    !
              go to 50 !
           end if !
        end if !
        28 continue !
     end do
     !                                                                                    !
     !     If loop through stand did not meet TARVR criterium then loop through again     !
     !                                                                                    !
     !                                                                                    !
     if (SRVOL1.lt. TARVR) go to 27 !
     !                                                                                    !
     !                                                                                    !
     go to 50 !End of TARVR sanitize method loop                    !
     !                                                                                    !
     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     !
     !                                                                                    !
     !     Branch from TARBA test (ie method of sanitization)                             !
     !C     Target stand removal volume method of Sanitization
     35 continue

     !     Insert section of code from above defining EFFCLN (8/10/99, AJM).

     !      IF (IJK .GT. 3) THEN
     !       EFFCLN= EFFC*(IJK-3)/(MAXSC-3)
     !       ELSE
     !       EFFCLN= 1.0E-6
     !      ENDIF

     !     Maximum rating value mehtod of Sanitation
     !     Remove trees from each appropriate size class based on the
     !     desired efficiency

     do ISIZ = MINSC,MAXSC
        !         REMPB = PBKILL(ISTD,ISIZ) * EFFC
        REMPB = PBKILL(ISTD,ISIZ) * 0.90
        PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - REMPB ! REMOVE ATTACKED TREES

        !         REMIPS = ALLKLL(ISTD,ISIZ) * EFFC
        REMIPS = ALLKLL(ISTD,ISIZ) * 0.90
        ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) - REMIPS !DITTO

        !     New var for bookkeeping bkp removal (FOR OUTPUT). ajm 7/05
        !     Note: this assumes that each dead beetle-killed tree removed via sanitation
        !     (i.e. the decrements from the PBKILL array) is fully occupied with BKP.  \

        REMBKP(ISTD) = REMBKP(ISTD) + REMPB * MSBA(ISIZ) !WHERE IS THIS ZEROED?AJM

        !     Changed removal efficiency of beetle killed trees to 0.90 obove
        !     Accounted for these removal in tree record before removing low
        !     RV trees.  calculated reduction in basal area properly (RNH19Aug)

        ! COMMENTING OUT THE NEXT THREE LINES AJM 6/30/05
        ! TREE AND BAH ARRAYS WERE ALREADY DECREMENTED IN BMISTD IF THE TREES WERE BEETLE-KILLED,
        ! WHICH THIS SECTION IS REMOVING.  LIKEWISE, AREMS IS BEING BOOKEPT *ONLY* FOR
        ! POSTING TO BASE-FVS WK2 ARRAY IN BMKILL.  SINCE THESE TREES REMOVED VIA SANIT
        ! ARE ALREADY DEAD, THEY'VE ALREADY BEEN ADDED TO THE TPBK ARRAY, SO ARE ALREADY
        ! ACCOUNTED FOR.  REMOVALS POSTED TO WK2 ARRAY FROM SANI SHOULD ONLY REFLECT THOSE
        ! LIVE UNATTACKED TREES REMOVED VIA SANIT.  SEE NEXT IF...THEN BLOCK.

        !      TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMPB - REMIPS
        !      BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) - MSBA(ISIZ)*(REMPB + REMIPS)
        !      AREMS(ISTD,ISIZ)= AREMS(ISTD,ISIZ) + REMPB + REMIPS

        if (GRF(ISTD,ISIZ) .le. RVMAX) then !NOW REMOVE LOW RV TREES
           !        REMLRV = TREE(ISTD,ISIZ,1) * EFFCLN
           REMLRV = TREE(ISTD,ISIZ,1) * EFFC

           !... Changed above formula [and commented out block above(ca lines 388-92)]
           !     so that now efficiency is NOT modified by size class, but remains
           !     constant at the user-specified level from field 7 in SANITIZE
           !     keyword; default = 0.95 (set in BMPPIN).  AJM 8/11/99
           !+++++++++++++                                         ++++++++++++++
           !     Accumulate removal data to pass to BMKILL for Sanitize accounting
           !     in PPE (RNH Aug98)

           AREMS(ISTD,ISIZ) = AREMS(ISTD,ISIZ) + REMLRV
           AAREMS(ISTD,ISIZ) = REMLRV ! NEW AJM 9/05 ANNUAL ACCUMULATOR REZEROED ANNUALLY (ABOVE) FOR OUTPUT
           !            BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) * (1 - EFFCLN) ! COMMENT OUT AJM 6/30/05
           !                           ! THIS SHOULD'VE BEEN CHANGED 8/99 WHEN EFFCLN WAS NIXED
           BAH(ISTD,ISIZ) = BAH(ISTD,ISIZ) * (1 - EFFC) ! ADD 6/30/05 AJM

           !      BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) - MSBA(ISIZ)*EFFC
           TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMLRV
        else
           REMLRV = 0.0
        end if

        REMOVE = REMLRV + REMPB + REMIPS
        VREMOV = VREMOV + REMOVE * TVOL(ISTD,ISIZ,1)

        !        Also remove some wood from the current standing dead pools because
        !        the killed trees were added to these pools last year (so use subscript
        !        2 which means age 1). Multiply REMOVE by the decay factor to account
        !        for one year's decay. Note that SDWP is measured in volume.

        JSC = L2D(ISIZ) + 1
        JJ = MIN0(JSC,MXDWSZ)
        SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV)&
             &* TVOL(ISTD,ISIZ,1) * SDECRT(JJ)

        SREMOV(ISTD) = SREMOV(ISTD) + REMOVE

        40 continue
     end do

     !+++++++++++++++++++++++++++++++++++++++++++++++++++++++

     !     Branch from TARBA method

     50 continue

     VOLREM(ISTD,1) = VOLREM(ISTD,1) + VREMOV ! NEW ANNUAL ACCUMULATOR AJM 6/05
     CVOLREM(ISTD,1) = CVOLREM(ISTD,1) + VREMOV

     !     Calculate amount of slash produced from sanitation

     call BMSLSH (IPC,1.0,VREMOV,ISTD)

     !     Re-calculate GRF in case any low RV trees were removed.
     !                                        !note 7/28/05 ajm
     ! this step may not be necessary, because
     TOTAL = 0.0 ! GRFs are recalculated after this sanitation (call to BMCGRF from BMDRV)
     GRFSTD(ISTD) = 0.0 ! but before all of the BKP dynamics that needs the GRFs
     do ICLS = 1,NSCL ! i.e. we don't need to derive this here and now
        X = BAH(ISTD,ICLS)
        TOTAL = TOTAL + X
        GRFSTD(ISTD) = GRFSTD(ISTD) + GRF(ISTD,ICLS) * X
        200 continue
     end do

     if (TOTAL .gt.1.0E-9) then
        GRFSTD(ISTD) = GRFSTD(ISTD) / TOTAL
     else
        GRFSTD(ISTD) = 1.0
     end if

     if (LBMDEB) write(JBMBPR,99) IYR, ISTD
     99 format(' End BMSANI: Year= ',I5, 'Stand= ', I6)

     500 continue
  end do

  !     End of main stand loop +++++++++++++++++++++++++

  return
end
