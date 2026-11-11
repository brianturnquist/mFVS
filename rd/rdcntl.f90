!=================!
!   rd : rdcntl   !
!=================!
! RD $Id$
subroutine RDCNTL
  implicit none

  !  MAIN ROOT DISEASE SUBROUTINE.  CONTROLS SPREAD OF DISEASE
  !  PATCHES, CARRYOVER OF ROOT DISEASE BETWEEN ROTATIONS, INFECTS
  !  NEW TREES IN STAND, SIMULATES INSIDE PATCH DYNAMICS, CALCULATES
  !  MORTALITY DUE TO ROOT DISEASE.

  !  CALLED BY :
  !     RDTREG  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDINUP  (SUBROUTINE)   [ROOT DISEASE]
  !     RDJUMP  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSPRD  (SUBROUTINE)   [ROOT DISEASE]
  !     RDAREA  (SUBROUTINE)   [ROOT DISEASE]
  !     RDINF   (SUBROUTINE)   [ROOT DISEASE]
  !     OPADD   (SUBROUTINE)   [PROGNOSIS]
  !     OPINCR  (SUBROUTINE)   [PROGNOSIS]
  !     OPFIND  (SUBROUTINE)   [PROGNOSIS]
  !     OPDONE  (SUBROUTINE)   [PROGNOSIS]
  !     RDCENT  (SUBROUTINE)   [ROOT DISEASE]
  !     RDINSD  (SUBROUTINE)   [ROOT DISEASE]
  !     RDIOUT  (SUBROUTINE)   [ROOT DISEASE]
  !     RDMORT  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSOUT  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSPOR  (SUBROUTINE)   [ROOT DISEASE]
  !     RDSHRK  (SUBROUTINE)   [ROOT DISEASE]
  !     RDZERO  (SUBROUTINE)   [ROOT DISEASE]

  !  Revision History :
  !    04/03/97 - Matt Thompson (FHTET)
  !      Modified the variables that are tested to check
  !      for a stand entry before the spore model is called.
  !    14-JAN-00 - Lance David (FHTET)
  !      Removed literals from option processor calls and replaced
  !      them with references to MYACT. Expanded array MYACT to
  !      include activity codes 2401 and 2431.
  !    06-AUG-01 Lance R. David (FHTET)
  !      Added declaration and initialization of TOTCEN.
  !    01-JUL-02 Lance R. David (FHTET)
  !      Corrected what looked like a typo with reset of PAREA near line 257.
  !   08/27/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'RDCRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !   NEW CODE ADDED FOR ROOT ROT MODEL (11/6/89). THIS CODE IS USED
  !   SO THAT THE CARRYOVER MODEL CAN BE ADDED TO THE OPTIONS
  !   PROCESSING LOGIC AND SIGNALED COMPLETED IN THE CYCLE WHEN
  !   CARRYOVER IS DONE.

  ! Variables
  integer :: I,I1,I2,IACTK,ICEN,ICLASS,IDI,IDIFF,II,IJ,IP,IRG,ISIZE,IT,J,KDT,KSP,MYACT(3),NPS,NTODO
  real    :: DIFF,MRAT,PRMS(5),PTMOVE,RRADEC,RRSURV,SPCEN,TESTAG,TOTCEN
  logical :: DEBUG

  data MYACT / 2401, 2402, 2431/

  KDT = IY(ICYC)
  TOTCEN = 0.0

  ! Check whether debugging is enabled
  call DBCHK(DEBUG,'RDCNTL',6,ICYC)

  if (DEBUG) write(JOSTND,10) ICYC
  10 format(' IN RDCNTL CYCLE=',I4)

  if (IROOT .eq.0) goto 1000

  !     CALL THE SPORE OPTION PROCESSOR TO SEE IF THERE ARE ANY
  !     CHANGES TO THOSE VARIABLES. THIS CALL WAS ALSO DONE IN RDSPUP
  !     BECAUSE SOME SPORE VARIABLES ARE USED THERE (BUT THAT IS NOT
  !     CALLED EVERY YEAR SO MAIN CALL IS HERE)
  !     (ACTIVITY CODE 2431)

  call OPFIND (1,MYACT(3),NTODO)

  if (NTODO .gt.0) then

     do IJ = 1,NTODO
        call OPGET (IJ,5,KDT,IACTK,NPS,PRMS)

        IRRSP = INT(PRMS(5))
        SPINF(IRRSP) = PRMS(1)
        SPDBH(IRRSP) = PRMS(2)
        SPYTK(IRRSP) = PRMS(3)
        SPTRAN(IRRSP) = PRMS(4)

        call OPDONE (IJ,IY(ICYC))
        23 continue
     end do
  end if

  !        INFECT UNINFECTED TREES INSIDE INFECTION ZONES

  do IRRSP = MINRR,MAXRR

     !        BEFORE DOING ANYTHING ELSE, CHANGE SPORE-CREATED CENTERS THAT WERE
     !        MADE MORE THAN 10 YEARS AGO INTO ORDINARY CENTERS.

     SPCEN = 0.0
     do ICEN = 1,NCENTS(IRRSP)
        if ((ICENSP(IRRSP,ICEN) .gt.0) .and.&
             &(JCENSP(IRRSP,ICEN) .lt. IYEAR)) then
           IDIFF = IYEAR - JCENSP(IRRSP,ICEN)
           if (IDIFF .ge.10) ICENSP(IRRSP,ICEN) = 0
        end if
        if (ICENSP(IRRSP,ICEN) .ne.0) SPCEN = SPCEN +&
             &PCENTS(IRRSP,ICEN,3) ** 2
        TOTCEN = TOTCEN + PCENTS(IRRSP,ICEN,3) ** 2
        27 continue
     end do
     SPPROP(IRRSP) = SPCEN / (TOTCEN + 1E-6)

     if (DEBUG) write(JOSTND,315) IRRSP, PAREA(IRRSP)
     315 format(' RDCNTL BEFORE CALL TO RDINSD: IRRSP PAREA=',I2,E15.7)

     call RDINUP
     if (IRIPTY .ne.0) call RDINSD

     525 continue
  end do
  call RDIOUT

  !.... If there has been a recent stand entry than move spore infected
  !.... stumps to the regular stump arrays if necessary.

  if (LSPFLG(1) .or. LSPFLG(2) .or. LSPFLG(3)) then
     call RDSPOR
  end if

  !     FIND OUT IF THE SPREAD RATE PARAMETERS ARE GOING TO
  !     CHANGE IN THIS TIMESTEP (ANY SPREAD KEYWORD DESIGNATED
  !     PARAMETER COULD CHANGE, INCLUDING TYPE OF SPREAD CALC.)
  !     (ACTIVITY CODE 2401)

  call OPFIND (1,MYACT(1),NTODO)
  if (NTODO .gt.0) then

     call OPGET (NTODO,4,KDT,IACTK,NPS,PRMS)
     IRSPTY = INT(PRMS(1))
     RRRSET(MINRR) = PRMS(2)
     RRRSET(MAXRR) = PRMS(3)
     NMONT = INT(PRMS(4))

     call OPDONE (NTODO,IY(ICYC))
  end if

  do IRRSP = MINRR,MAXRR

     call RDINUP
     call RDJUMP

     !        SPREADING OF INFECTION CENTERS

     !        ESTIMATE SPREADING (AND SHRINKING) RATE OF INFECTION ZONES

     if (LONECT(IRRSP) .eq.1) then
        RRRATE(IRRSP) = 0.0
        goto 75
     end if

     if (IRSPTY .ge.1) goto 50
     RRRATE(IRRSP) = RRRSET(IRRSP)
     goto 75

     50 continue

     call RDSHRK
     call RDSPRD

     75 continue
  end do

  !     PRINT THE CALCULATED SPREAD RATES

  if (IRSPTY .ge.1) call RDSOUT

  if (DEBUG) write(JOSTND,333) INFLAG,RRRATE(1),RRRATE(2),&
       &RRRATE(3),RRRATE(4)
  333 format(' RDCNTL: INFLAG RRRATE=',I3,4F9.2)

  MRAT = RRRATE(1)
  if (RRRATE(2) .lt. MRAT) goto 7296
  MRAT = RRRATE(2)
  7296 continue
  if (((INFLAG.ne.1).and.(INFLAG.ne.3))&
       &.or.(MRAT .lt.0.0)) goto 1003
  AGECUR = 3000.0
  INFLAG = 0

  1003 continue

  !     'GROW' (OR SHRINK) SPREADING CENTERS

  do IRRSP = MINRR,MAXRR

     SPPROP(IRRSP) = 0.0
     if (NCENTS(IRRSP) .eq.0) goto 201

     SPCEN = 0.0
     TOTCEN = 0.0

     do I = 1, NCENTS(IRRSP)
        if (PCENTS(IRRSP,I,3) .le.0) goto 200

        if (IRSPTY .eq.1) then
           PCENTS(IRRSP,I,3) = PCENTS(IRRSP,I,3) +&
                &RRATES(IRRSP,I) * FINT
        else
           PCENTS(IRRSP,I,3) = PCENTS(IRRSP,I,3) +&
                &RRRATE(IRRSP) * FINT
        end if

        if (ICENSP(IRRSP,I) .ne.0) SPCEN = SPCEN +&
             &PCENTS(IRRSP,I,3) ** 2
        TOTCEN = TOTCEN + PCENTS(IRRSP,I,3) ** 2

        200 continue
     end do
     SPPROP(IRRSP) = SPCEN / (TOTCEN + 1E-6)

     201 continue

     !        TOTALLY REMOVE ANY CENTERS WHOSE RADIUS IS LESS THAN 0.01 (.1 INCHES)
     call RDZERO

     !        CALCULATE NEW AREA INFECTED BY OVERLAPPING(?) CENTERS

     if (DEBUG) write(JOSTND,811) IRRSP, PAREA(IRRSP)
     811 format(' RDCNTL BEFORE CALL TO RDAREA TYPE=', I1,&
          &' PAREA=',E15.7)
     call RDAREA
     if (DEBUG) write(JOSTND,812) IRRSP, PAREA(IRRSP)
     812 format(' RDCNTL AFTER CALL TO RDAREA TYPE=', I1, ' PAREA=',&
          &E15.7)

     AREANU(IRRSP) = PAREA(IRRSP) - OOAREA(IRRSP)

     !        looks like this may be a typo error LRD 01-jul-02
     !        IF (PAREA(IRRSP) .LE. 0.0) AREANU(IRRSP) = 0.0
     if (PAREA(IRRSP) .le.0.0) PAREA(IRRSP) = 0.0
     if (AREANU(IRRSP) .le.0.0) AREANU(IRRSP) = 0.0
     OOAREA(IRRSP) = PAREA(IRRSP)

     !        ESTIMATE NUMBER OF NEW TREES INFECTED

     call RDINF

     300 continue
  end do

  !     NEW INFECTION CENTERS (FROM CARRYOVER MODEL)

  if (DEBUG) write(JOSTND,334) INFLAG
  334 format(' RDCNTL : INFLAG=',I4)

  if (INFLAG .eq.0) goto 500
  TESTAG = FLOAT(IY(ICYC))
  if (DEBUG) write(JOSTND,335) TESTAG,AGECUR
  335 format(' RDCNTL : TESTAG AGECUR=',2F9.2)

  if (AGECUR .ne. TESTAG) goto 500
  if (DEBUG) write(JOSTND,336) RRRATE(1), RRRATE(2) ,RRGEN(1,7)
  336 format(' RDCNTL : RRRATE 1&2  RRGEN(1,7) =',3F9.2)

  if (MRAT .lt. RRGEN(1,7)) goto 500

  !     ESTIMATE PROBABILITY OF INITIATION OF NEW INFECTION CENTER
  !     IF IFRRC EQ 1 DYNAMIC CARRYOVER MODEL IS USED. IF IFRRC
  !     EQ 0 STATIC CARRYOVER MODEL IS USED. LOOP 380 IS THE
  !     STATIC CARRYOVER MODEL !!!

  if (IFRRC.eq.1) goto 390

  !     SET PATRAMETERS FOR STATIC CARRYOVER MODEL AND INCORPORATE STATIC
  !     MODEL INTO THE OPTION PROCESSOR.
  !     (ACTIVITY CODE 2402)

  PRMS(1) = PINSET
  PRMS(2) = FLOAT(NNCENT)
  PRMS(3) = FLOAT(IRGEN(1))

  call OPADD (KDT,MYACT(2),KDT,3,PRMS,I)

  do IDI = MINRR,MAXRR
     do I = 1, 2
        do J = 1, 5
           PROBIN(IDI,I,J) = PINSET
           370 continue
        end do
        380 continue
     end do
     385 continue
  end do
  goto 400

  390 continue

  !     DYNAMIC CARRYOVER MODEL. ADD TO THE OPTION PROCESSOR BY CALLING
  !     OPADD.
  !     (ACTIVITY CODE 2402)

  PRMS(1) = RRGEN(1,7)
  PRMS(2) = FLOAT(IRGEN(1))

  call OPADD (KDT,MYACT(2),KDT,2,PRMS,II)

  ! **************** CD CHANGE *** remove dynamic carryover

  !     CALL RRCARY

  ! **************** CD END *************

  400 continue

  !     SET UP NEW INFECTION CENTERS

  do IRRSP = MINRR,MAXRR
     call RDCENT(PROBIN,PCENTS,NCENTS,CRNSTO,SAREA,&
          &IRFLAG,NNCENT,PISIZE,PAREA,IRRSP)

     if (DEBUG) write(JOSTND,813) IRRSP, NCENTS(IRRSP), NNCENT
     813 format(' RDCNTL AFTER CALL TO RDCENT: IRRSP NCENTS NNCENT',&
          &3I5)

     call RDAREA
     if (DEBUG) write(JOSTND,814) IRRSP, PAREA
     814 format(' RDCNTL AFTER CALL TO RDAREA: IRRSP, PAREA=',I2,&
          &E15.7)
     411 continue
  end do

  !     SEE IF WE ARE GOING TO RESCHEDULE CARRYOVER BECAUSE OF A
  !     NESTED CUT.  (INFLAG=3).

  if (INFLAG .ne.3) goto 444
  INFLAG = 1
  IRG = ICYC + IRGEN(1)
  AGECUR = 3000.0
  if (IRG .le. NCYC) AGECUR = IY(IRG)
  goto 447

  444 continue
  AGECUR = 3000.0
  INFLAG = 0

  447 continue

  do IRRSP = MINRR,MAXRR
     RRADEC = OOAREA(IRRSP) - PAREA(IRRSP)
     if (RRADEC .le.0.0) goto 8865

     !        ADJUST TREE LISTS FOR CHANGE IN DISEASE AREA

     if (ITRN .eq.0) goto 482
     if (DEBUG) write(JOSTND,69) IRRSP, RRADEC
     69 format(' RDCNTL : IRRSP  CARRYOVER AREA REDUCTION ',I2, E16.8)

     IDI = IRRSP
     do I = 1, ITRN
        if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(ISP(I)))
        if (IDI .ne. IRRSP) goto 420

        PTMOVE = PROBIU(I) * RRADEC / (OOAREA(IRRSP) + 1.0E-6)
        PROBIU(I) = PROBIU(I) - PTMOVE
        if (DEBUG) write(JOSTND,421) PROBIU(I)
        421 format(' IN RDCNTL : PROBIU',F10.2)
        DIFF = SAREA - PAREA(IRRSP)
        if (DIFF .le.1E-3) FPROB(I) = 0.0
        if (DIFF .le.1E-3) goto 420
        FPROB(I) = (FPROB(I) * (SAREA - OOAREA(IRRSP)) + PTMOVE) /&
             &(SAREA - PAREA(IRRSP) + 1.0E-6)
        420 continue
     end do

     !        REDUCE STUMP LIST WITH CHANGE IN DISEASE AREA

     do IDI = MINRR,MAXRR
        do ITYPE = 1,2
           do ISIZE = 1,5
              do IT = 1,40
                 PROBDA(IDI,ITYPE,ISIZE,IT) = &
                      &PROBDA(IDI,ITYPE,ISIZE,IT) * PAREA(IDI) /&
                      &(OOAREA(IDI) +1.0E-6)
                 460 continue
              end do
              461 continue
           end do
           462 continue
        end do
        463 continue
     end do
     go to 6688

     8865 continue
     call RDINF

     6688 continue
     OOAREA(IRRSP) = PAREA(IRRSP)

     482 continue
  end do

  !         **END CARRYOVER MODEL

  500 continue

  !        INFECT UNINFECTED TREES INSIDE INFECTION ZONES

  !      DO 525 IRRSP=MINRR,MAXRR
  !         IF (DEBUG) WRITE(JOSTND,315) IRRSP, PAREA(IRRSP)
  !  315    FORMAT(' RDCNTL BEFORE CALL TO RDINSD: IRRSP PAREA=',I2,E15.7)
  !         IF (IRIPTY .NE. 0) CALL RDINSD
  !  525 CONTINUE

  !     INFECTION AND DEATH OF LIVE TREES

  call RDMORT

  !     ACCUMULATE SOME INDICATORS

  do J = 1,(ITOTRR+MAXSP)
     PRINF(J) = 0
     PRPTOT(J) = 0.0
     550 continue
  end do

  !     ACCUMULATE STUMPS BY CATEGORY AND TOTALS

  !     ACCUMULATE LIVE TREES IN INFECTED AREA BY CATEGORY AND TOTALS

  if (ITRN .le.0) goto 1000
  do I = 1, ITRN
     KSP = ISP(I) + 2
     RRSURV = PROBIT(I) + PROBIU(I)
     if (RRSURV .le.0) goto 700
     ICLASS = 1

     do J = 1, 3
        ICLASS = ICLASS + 1
        if (DBH(I) .le. DICLAS(J+1)) goto 670
        660 continue
     end do

     670 continue
     700 continue
  end do

  IDI = MAXRR
  do J = 1, 12
     do I = 1, 4
        if (J .gt.2) goto 705
        if (MAXRR .lt.3) IDI = J
        goto 710
        705 continue
        if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(J-2))
        710 continue
        720 continue
     end do
     730 continue
  end do

  !     CALCULATE WEIGHTED AVERAGE PROPORTION OF INFECTED ROOTS
  !        (PRINF & PRPTOT ARE ZEROED ABOVE)

  IDI = MAXRR
  do KSP = 1,MAXSP
     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))

     !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
     !     If non-host species then IDI = 0, and loop should be skipped to
     !     prevent array out of bounds error

     if (IDI .le.0) go to 800

     if (ISCT(KSP,1) .eq.0) goto 800
     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)

     do J = I1, I2
        I = IND1(J)

        do IT = 1, ISTEP
           do IP = 1,2
              if (PROPI(I,IT,IP) .gt.0.0) then
                 PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) + PROBI(I,IT,IP)&
                      &* PROPI(I,IT,IP)
                 PRINF(IDI) = PRINF(IDI) + PROBI(I,IT,IP) *&
                      &PROPI(I,IT,IP)
              end if
              PRPTOT(KSP+ITOTRR) = PRPTOT(KSP+ITOTRR) + PROBI(I,IT,IP)
              PRPTOT(IDI) = PRPTOT(IDI) + PROBI(I,IT,IP)
              742 continue
           end do
           745 continue
        end do
        750 continue
     end do

     PRINF(KSP+ITOTRR) = PRINF(KSP+ITOTRR) /&
          &(PRPTOT(KSP+ITOTRR) + 1E-6)
     800 continue
  end do

  do IDI = MINRR,MAXRR
     PRINF(IDI) = PRINF(IDI) / (PRPTOT(IDI) + 1E-6)
     805 continue
  end do

  1000 continue
  if (DEBUG) write (JOSTND,70) INFLAG, TESTAG, AGECUR
  70 format (' LEAVING RDCNTL : INFLAG, TESTAG, AGECUR = ',I5,2X,2F7.0)

  return
end
