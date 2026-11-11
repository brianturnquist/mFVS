!=================!
!   rd : rddout   !
!=================!
! RD $Id$
subroutine RDDOUT
  implicit none

  !  Purpose :
  !     This subroutine produces detailed output for the root disease
  !     model.  This output will be printed each time step.  It details
  !     the stand attributes and effects of root disease within root
  !     disease patches.  Stand attributes are detailed for each species
  !     and for 6 size classes.

  !  Called by :
  !     RDPR    [ROOT DISEASE]

  !  Calls :
  !     RDPSRT  (SUBROUTINE)   [PROGNOSIS]
  !     PCTILE  (SUBROUTINE)   [PROGNOSIS]
  !     RDDST   (SUBROUTINE)   [ROOT DISEASE]

  !  Local variables :
  !     I,J,L  - INTEGER
  !              Counters.
  !     IK,IU  - INTEGER
  !              Array indexes.
  !     INDXKL - INTEGER
  !              Index array for PVECKL.
  !     INDXU  - INTEGER
  !              Index array for PVECU.
  !     JYR    - INTEGER
  !              Current year in simulation.
  !     KSP    - INTEGER
  !              Counter for species loop.
  !     ODBHKL - REAL
  !              Array that holds the DBH of trees killed in the current
  !              cycle.
  !     ODBHU  - REAL
  !              Array that holds the DBH of trees inside patches but
  !              uninfected by root disease.
  !     ONDTRE - REAL
  !              Holds the sum of the vector PCTKL.
  !     ONLTRE - REAL
  !              Holds the sum of the vector PCTU.
  !     OPROBI - REAL
  !              Array that holds the number of trees infected by root
  !              disease for each species.
  !     OPROBU - REAL
  !              Array that holds the number of trees inside patches
  !              but uninfected by root disease for each species.
  !     ORRKIL - REAL
  !              Array that holds the number of trees killed by
  !              root disease in the current cycle for each species.
  !     PCTKL  - REAL
  !              Array that holds vector percentiles for PVECKL
  !              (trees/acre killed by root disease in the current cycle).
  !     PCTU   - REAL
  !              Array that holds vector percentiles for PVECU
  !              (trees/acre inside patches).
  !     PVECKL - REAL
  !              Array that holds the trees/acre killed by root disease in
  !              the current cycle.
  !     PVECU  - REAL
  !              Array that holds the trees/acre inside patches (infected
  !              and uninfected trees).
  !     TMPI   - REAL
  !              Sum of PROBI for each tree record (total number of trees
  !              in tree record that are infected by root disease).  Used
  !              to calculate OPROBI.
  !     X1     - REAL
  !              The trees/acre killed by root disease in the current
  !              cycle for a species.
  !     X2     - REAL
  !              The trees/acre inside patches but uninfected by root
  !              disease for a species.
  !     X3     - REAL
  !              The trees/acre infected by root disease for a species.
  !     X4     - REAL
  !              Weighted average percentage of root systems of trees
  !              infected by root disease for a species.

  !  Common Block Variables Used :
  !     DBH    - (ARRAYS)  (I)
  !     I1, I2 - (RRCOM)   (O)
  !     IND1   - (ARRAYS)  (I)
  !     IOUNIT - (RRCOM)   (I)
  !     IRRTRE - (RRPARM)  (I)
  !     ISCT   - (CONTRL)  (I)
  !     ISTEP  - (RRCOM)   (I)
  !     ITRN   - (CONTRL)  (I)
  !     IY     - (CONTRL)  (I)
  !     JSP    - (PLTCHR)  (I)
  !     MAXSP  - (PRGPRM)  (I)
  !     MGMID  - (PLTCHR)  (I)
  !     NPLT   - (PLTCHR)  (I)
  !     PAREA  - (RRCOM)   (I)
  !     PRINF  - (RRCOM)   (I)
  !     PROBI  - (RR)      (I)
  !     PROBIU - (RR)      (I)
  !     RDKILL - (RRCOM)   (I)

  !  Revision History :
  !     08-OCT-97 Matthew K. Thompson (FHTET)
  !        Percentile point values were not being reset to 0.0.
  !        (Arrays ONDTRE and ONLTRE are now being reset.
  !     23-DEC-99 Lance R. David (FHTET)
  !        Updated for expansion of FVS stand id (variable NPLT)
  !        from 8 to 26 characters.
  !     18-JUN-01 Lance R. David (FHTET)
  !        Modified header to be more consistant with other output
  !        headers in the model and add Stand ID and Management ID line.
  !     02-AUG-01 Lance R. David (FHTET)
  !        Added initialization for ONDTRE and ONLTRE arrays.
  !     16-AUG-2006 Lance R. David (FHTET)
  !        Change of metric conversion factors variable names to match
  !        variables in new \FVS\COMMON\METRIC.F77. rd\src\metric.f77
  !        will be retired. (mods courtesy of Don Robinson, ESSA)
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/24/15 Lance R. David (FMSC)
  !     Implemented General Report Writer facility.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'
  include 'METRIC.f90'

  !.... Dimension statements for local variables.

  ! Variables
  integer :: I,I1,I2,IDI,IK,INDXKL(IRRTRE),INDXU(IRRTRE),IU,J,JYR,KSP,L

  real    :: ODBHKL(IRRTRE),ODBHU(IRRTRE),ONDTRE(7),ONLTRE(7),OPROBI(MAXSP),OPROBU(MAXSP),&
       &ORRKIL(MAXSP),PCTKL(IRRTRE),PCTU(IRRTRE),PVECKL(IRRTRE),PVECU(IRRTRE),TMPI,X1,X2,X3,X4,DDBHCLS(6),&
       &LDBHCLS(6),LAREA

  character(len=1) :: CHTYPE(ITOTRR)

  data CHTYPE /'P','S','A','W'/

  if (ITRN .eq.0) return
  TMPI = 0.0

  !.... Initialize arrays.

  do J = 1,7
     ONDTRE(J) = 0.0
     ONLTRE(J) = 0.0
     90 continue
  end do

  do J = 1,MAXSP
     ORRKIL(J) = 0.0
     OPROBI(J) = 0.0
     OPROBU(J) = 0.0
     100 continue
  end do

  do J = 1,IRRTRE
     ODBHKL(J) = 0.0
     ODBHU(J) = 0.0
     INDXKL(J) = 0
     INDXU(J) = 0
     PVECKL(J) = 0.0
     PVECU(J) = 0.0
     200 continue
  end do

  !     Get logical unit number and open genrpt file if it has been closed.

  call GETLUN(IOUNIT)

  if (ISTEP .gt.1.or. IRRSP .ne. MINRR) goto 1000
  JYR = IY(ICYC+1)

  !     get report ID and logical unit number.

  call GETID(IDRDOUT(2))

  write (IOUNIT,'(2(/1X,I5))') IDRDOUT(2),IDRDOUT(2)

  !.... Print header for detailed output table.

  write (IOUNIT,1100) IDRDOUT(2),IDRDOUT(2)
  write (IOUNIT,1105) IDRDOUT(2)
  write (IOUNIT,1107) IDRDOUT(2)
  write (IOUNIT,1110) IDRDOUT(2), NPLT, MGMID
  write (IOUNIT,1115) IDRDOUT(2)
  write (IOUNIT,1120) IDRDOUT(2)
  write (IOUNIT,1125) IDRDOUT(2)
  if (.not. LMTRIC) write (IOUNIT,1130) IDRDOUT(2)
  if (LMTRIC) write (IOUNIT,1230) IDRDOUT(2)
  write (IOUNIT,1135) IDRDOUT(2)
  if (.not. LMTRIC) write (IOUNIT,1140) IDRDOUT(2)
  if (LMTRIC) write (IOUNIT,1240)  IDRDOUT(2)
  write (IOUNIT,1145) IDRDOUT(2)
  goto 1010

  1000 continue
  JYR = IY(ICYC+1)

  1010 continue

  if (IRRSP .eq. MINRR) then
     if (LMTRIC) then
        write (IOUNIT,2105) IDRDOUT(2), JYR, CHTYPE(IRRSP),&
             &PAREA(IRRSP)*ACRtoHA
     else
        write (IOUNIT,2105) IDRDOUT(2), JYR,&
             &CHTYPE(IRRSP), PAREA(IRRSP)
     end if
  else
     if (LMTRIC) then
        write (IOUNIT,2106) IDRDOUT(2), CHTYPE(IRRSP),&
             &PAREA(IRRSP)*ACRtoHA
     else
        write (IOUNIT,2106) IDRDOUT(2), CHTYPE(IRRSP), PAREA(IRRSP)
     end if
  end if

  if (PAREA(IRRSP) .eq.0.0) then
     write(IOUNIT,2120) IDRDOUT(2)
     return
  end if

  !.... Find tree species in root disease patches.

  if (ITRN .gt.0) goto 804
  write (IOUNIT,4100) IDRDOUT(2)
  4100 format (1X,I5,1X,'***** NO TREES IN STAND')
  goto 4000

  804 continue
  IDI = IRRSP
  do KSP = 1, MAXSP
     if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(KSP))
     if ((ISCT(KSP,1) .eq.0) .or. (IDI .ne. IRRSP)) goto 800

     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)
     IK = 0
     IU = 0

     !....    Accumulate indicators.

     do J = I1, I2
        I = IND1(J)
        TMPI = 0.0

        do L = 1,ISTEP
           TMPI = TMPI + PROBI(I,L,1)
           TMPI = TMPI + PROBI(I,L,2)
           760 continue
        end do

        if (TMPI .le.0.0) goto 105

        OPROBI(KSP) = OPROBI(KSP) + TMPI
        105 if (PROBIU(I) .le.0.0) goto 115
        IU = IU + 1
        OPROBU(KSP) = OPROBU(KSP) + PROBIU(I)
        PVECU(IU) = (PROBIU(I) + TMPI) / (PAREA(IRRSP) + 1.0E-9)
        ODBHU(IU) = DBH(I)
        INDXU(IU) = IU

        115 if (RDKILL(I) .le.0.0) goto 125
        IK = IK + 1
        ORRKIL(KSP) = ORRKIL(KSP) + RDKILL(I)
        PVECKL(IK) = RDKILL(I) / (PAREA(IRRSP) + 1.0E-9)
        ODBHKL(IK) = DBH(I)
        INDXKL(IK) = IK

        125 continue
        750 continue
     end do

     !....    Find percentile points for each species.

     call RDPSRT(IK,ODBHKL,INDXKL,.false.)
     call RDPSRT(IU,ODBHU,INDXU,.false.)
     call PCTILE(IK,INDXKL,PVECKL,PCTKL,ONDTRE(7))
     call PCTILE(IU,INDXU,PVECU,PCTU,ONLTRE(7))
     call RDDST(IK,ONDTRE,PCTKL,ODBHKL,INDXKL)
     call RDDST(IU,ONLTRE,PCTU,ODBHU,INDXU)

     !....    Write out the indicators.

     X1 = ORRKIL(KSP) / (PAREA(IRRSP) + 1.0E-9)
     X2 = OPROBU(KSP) / (PAREA(IRRSP) + 1.0E-9)
     X3 = OPROBI(KSP) / (PAREA(IRRSP) + 1.0E-9)
     X4 = 100.0 * PRINF(KSP+ITOTRR)
     if (X1.ne.0.0) goto 801

     do I = 1,6
        PCTKL(I) = 0.0
        802 continue
     end do

     801 continue

     !        LOAD DIAMETER CLASS ARRAYS FOR OUTPUT

     if (LMTRIC) then
        do I = 1,6
           DDBHCLS(I) = ONDTRE(I)*INTOCM
           LDBHCLS(I) = ONLTRE(I)*INTOCM
        end do
        X1 = X1/ACRtoHA
        X2 = X2/ACRtoHA
        X3 = X3/ACRtoHA
        LAREA = PAREA(IRRSP)*ACRtoHA

        !X            WRITE(IOUNIT,2110) IDRDOUT(2), JSP(KSP),
        !X     >           (ONDTRE(I)*INTOCM,I=1,6), X1/ACRtoHA,
        !X     >           (ONLTRE(I)*INTOCM,I=1,6),  X2/ACRtoHA, X3/ACRtoHA, X4
        !XC
        !XC           Call DBS for RD Detail output to database
        !XC
        !X            CALL DBSRD2 (JYR,NPLT,CHTYPE(IRRSP),PAREA(IRRSP)*ACRtoHA,
        !X     >        JSP(KSP),
        !X     >        ONDTRE(1)*INTOCM, ONDTRE(2)*INTOCM, ONDTRE(3)*INTOCM,
        !X     >        ONDTRE(4)*INTOCM, ONDTRE(5)*INTOCM, ONDTRE(6)*INTOCM,
        !X     >        X1/ACRtoHA,
        !X     >        ONLTRE(1)*INTOCM, ONLTRE(2)*INTOCM, ONLTRE(3)*INTOCM,
        !X     >        ONLTRE(4)*INTOCM, ONLTRE(5)*INTOCM, ONLTRE(6)*INTOCM,
        !X     >        X2/ACRtoHA, X3/ACRtoHA, X4)

     else
        do I = 1,6
           DDBHCLS(I) = ONDTRE(I)
           LDBHCLS(I) = ONLTRE(I)
        end do
        LAREA = PAREA(IRRSP)
     end if

     write(IOUNIT,2110) IDRDOUT(2), JSP(KSP),&
          &(DDBHCLS(I),I = 1,6), X1,&
          &(LDBHCLS(I),I = 1,6), X2, X3, X4

     !        Call DBS for RD Detail output to database

     call DBSRD2 (JYR,NPLT,CHTYPE(IRRSP),LAREA,&
          &JSP(KSP),DDBHCLS, X1, LDBHCLS, X2, X3, X4)

     !        Zero out arrays.

     do I = 1,IRRTRE
        ODBHKL(I) = 0.0
        ODBHU(I) = 0.0
        INDXKL(I) = 0
        INDXU(I) = 0
        PVECKL(I) = 0.0
        PVECU(I) = 0.0
        780 continue
     end do

     do I = 1,6
        ONDTRE(I) = 0.0
        ONLTRE(I) = 0.0
        DDBHCLS(I) = 0.0
        LDBHCLS(I) = 0.0
        790 continue
     end do

     800 continue
  end do

  if (IRRSP .eq.1) write(IOUNIT,2121)
  if (IRRSP .eq.2) write(IOUNIT,2120)
  return

  !.... No entries in tree list.

  4000 continue
  if (IRRSP .eq.1) write(IOUNIT,2121)
  if (IRRSP .eq.2) write(IOUNIT,2120)
  return

  !.... Format statements for header.

  1100 format (1X,I5,/1X,I5,1X,65('* '))
  1105 format (1X,I5,51X,'WESTERN ROOT DISEASE MODEL')
  1107 format (1X,I5,1X,65('* '))
  1110 format (1X,I5,34X,'STAND ID= ',A26,5X,'MANAGEMENT ID= ',A4)
  1115 format (1X,I5,33X,'DETAILED OUTPUT OF STAND ATTRIBUTES INSIDE ',&
       &'ROOT DISEASE PATCHES')
  1120 format (1X,I5,1X,130('-'))
  1125 format (1X,I5,8X,'ROOT',18X,'KILLED TREES',33X,'ALIVE TREES')
  1130 format (1X,I5,7X,'DISEASE',3X,&
       &2(6X,'%TILE POINTS BY DBH (INCHES)    TOTAL'),&
       &'       TOTAL')
  1135 format (1X,I5,6X,9('-'),8X,28('-'),'    KILLED',&
       &5X,28('-'),'   UNINFECTED  INFECTED',4X,'% ROOTS')
  1140 format (1X,I5,' YEAR TYPE AREA  SP',2(5X,'10   30   50   70',&
       &'   90  100  TREES/ACRE'),' TREES/ACRE',2X,'INFECTED')
  1145 format (1X,I5,1X,'---- ---------  --',2(3X,6(' ----'),&
       &' ----------'),2(' ----------'))

  !.... Format statements for headers specific to metric.

  1230 format (1X,I5,7X,'DISEASE',3X,&
       &2(6X,' %TILE POINTS BY DBH (CM)       TOTAL'),&
       &'       TOTAL')
  1240 format (1X,I5,' YEAR TYPE AREA  SP',2(5X,'10   30   50   70',&
       &'   90  100   TREES/HA '),'  TREES/HA  ',2X,'INFECTED')

  !.... Format statements for data values.

  2105 format(1X,I5,1X,I4,2X,A1,F7.1)
  2106 format(1X,I5,1X,4X,2X,A1,F7.1)
  2110 format(1X,I5,1X,16X,A2,2(3X,6F5.1,2X,F6.1,3X),2X,F6.1,F10.2)
  2120 format(1X,I5,1X,130('-'))
  2121 format(1X,I5,1X,130(' '))

end
