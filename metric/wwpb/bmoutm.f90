!==========================!
!   metric/wwpb : bmoutm   !
!==========================!
! METRIC-WWPB $Id$
subroutine BMOUTM (I,IYEAR)

  !                                    July 1998
  !                                    August 1998
  !     Two output files were created (units 28 and 29) to output
  !     annual stand and landscape data (RN Havis July 1998)

  !     Westwide Pine Beetle model; summary of stand state *after*
  !     beetles have dispersed and *after* beetle induced
  !     mortality has occurred and *before* trees and basal area have
  !     been removed from the tree lists.

  !     This routine also calculates and prints the averages needed for
  !     the detailed beetle model output
  !     A TABLE OF RESULTS IS OUTPUT AT THE CONCLUSION OF EACH CYCLE.

  !     CALLED FROM -- BMDRV

  !     VREM(A,B,C) - a temporary storage array to hold removal volumes
  !                   for Sanitation and Salvage output to BMSPY.OUT
  !                   A - stand, B- Sanitation(1) or Salvage(2)
  !                   C - stand removal has (1) or has not (0) been
  !                       printed for the simulation
  !     If a particular stand is Sanitized or Salvaged
  !     more than once in a cycle, the second action will not be output
  !     to the BMSPY output file

  !     PBKILL - number of trees killed by beetles by size class

  !     SSBATK changed 8/99.  It is now dimensioned and added to common block.
  !     It is dimensioned (MXSTAND) (i.e. now an array containing one value
  !     per stand).  These values will be used in a new calculation of the
  !     numerator of the attractiveness equation (subroutine BMCNUM).
  !     The BA killed this year will be added back in next year.  AJM

  !     CORRECTED THE SETTING OF LSBAK1, (JUST BELOW  310 CONTINUE) AJM 6/05

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PPEPRM.f90'
  include 'PPCNTL.f90'
  include 'BMPRM.f90'
  include 'BMCOM.f90'
  include 'BMPCOM.f90'
  include 'METRIC.f90'

  !     Added real array LBAKL1, LBAH1, LVREM1 to store annual landscape
  !     summaries (RNH July98)

  ! Variables
  real    :: LBAKL1(NSCL+1),LBAH1(NSCL+1),LVREM1(2),RBAKT(MXSTND,NSCL+1),VTEMP(MXSTND,2,2)

  real    :: LBKP1,LRVSTD1,LBAT1
  real    :: LPROPH1,LDDWP1,LSDWP1
  real    :: LTKILL1,LVKILL1
  real    :: LSREM1,LSBAK1,LTBAK1
  real    :: LBKPO1,LBKPI1,LBKPS1

  integer :: RYEAR,RYEAR2,RYEARP

  !      IF (.NOT. LBMDET .AND. .NOT. LBMVOL) RETURN

  !     Test for print flags for annual stand or annual landscape output
  !     (RNH July98)

  if (.not. LBMDET .and..not. LBMVOL&
       &.and..not. LBMSPY .and..not. LBMLPY) return

  !     RYEAR triggers the reporting. It is the last year of a cycle.
  !     The average values are reported for the first year of the
  !     cycle just finished. e.g: for and outbreak from 1993-1994
  !     during the 1990-1994 period, the reporting would be for 1990
  !     and would be the average of 1993 and 1994.

  RYEAR = MIY(MICYC) - 1
  RYEAR2 = MIY(MICYC - 1)

  !     New local variable RYEARP to identify output table values as
  !     end of cycle values (RNH June98)

  RYEARP = MIY(MICYC)

  !     +++++++++

  !     Set the Volume removed print flags to false(0)
  !     for the first year of cycle, and inititalize VTEMP
  !     If first year of cycle and inititialization of VTEMP
  !     has not been carried out then dothe following

  if ((IYEAR .eq. MIY(MICYC-1)) .and. (INITV .ge.1)) then
     go to 5
  else
     INITV = 0
  end if

  if ((IYEAR .eq. MIY(MICYC-1)) .and. (INITV .le.0)) then
     do IV1 = 1, MXSTND
        VTEMP(IV1,1,2) = 0.
        VTEMP(IV1,2,2) = 0.
        VTEMP(IV1,1,1) = 0.
        VTEMP(IV1,2,1) = 0.
        4 continue
     end do
     INITV = 1
  end if

  5 continue

  !     Sanitation and Salvage removals are reported to output BMSPY file
  !     RNH (Aug98). Load the VREMOV array each year in the following

  !     Copy VOLREM array into a temporary array for current year

  if (IYEAR .ne. IYEARV) then
     do IV = 1, MXSTND
        VTEMP(IV,1,1) = VOLREM(IV,1)
        VTEMP(IV,2,1) = VOLREM(IV,2)
        6 continue
     end do
     IYEARV = IYEAR
  end if

  !     +++++++++++

  !     Accumulate sums for each variable

  SRVSTD(I) = SRVSTD(I) + GRFSTD(I)
  SBAT(I) = SBAT(I) + (BANH(I,NSCL+1)+BAH(I,NSCL+1)) *FT2pACRtoM2pHA
  SBAH(I,NSCL+1) = SBAH(I,NSCL+1) + BAH(I,NSCL+1) * FT2pACRtoM2pHA

  SBKP(I) = SBKP(I) + BKPA(I) / ACRtoHA
  SBKPO(I) = SBKPO(I) + BKPOUT(1,I) / ACRtoHA
  SBKPI(I) = SBKPI(I) + BKPIN(1,I) / ACRtoHA
  SBKPS(I) = SBKPS(I) + BKPS(I) / ACRtoHA
  SBKPIS(I) = SBKPIS(I) + BKPIS(I) / ACRtoHA

  do ISIZ = 1,NSCL
     TOTKLL = PBKILL(I,ISIZ) + ALLKLL(I,ISIZ)
     STKILL(I) = STKILL(I) + TOTKLL / ACRtoHA
     SVKILL(I) = SVKILL(I) + TOTKLL * TVOL(I,ISIZ,1) * FT3pACRtoM3pHA

     !     For annual output (RNH July98), calculate total volume killed

     SVKILL1 = SVKILL1 + TOTKLL*TVOL(I,ISIZ,1)*FT3pACRtoM3pHA

     if (TREE(I,ISIZ,1) .gt.1E-6) then
        PBAREM = TOTKLL / TREE(I,ISIZ,1)
     else
        PBAREM = 0.0
     end if
     BAHHA = BAH(I,ISIZ) * FT2pACRtoM2pHA
     SBAKL(I,ISIZ) = SBAKL(I,ISIZ) + BAHHA * PBAREM
     SBAKL(I,NSCL+1) = SBAKL(I,NSCL+1) + BAHHA * PBAREM
     SBAH(I,ISIZ) = SBAH(I,ISIZ) + BAHHA
     10 continue
  end do

  do J = 1,(MXDWHZ+1)
     do K = 1,MXDWAG
        do IPC = 1,MXDWPC
           SSDWP(I) = SSDWP(I) + SDWP(I,IPC,J,K) * FT3pACRtoM3pHA
           322 continue
        end do
        if (J .le. MXDWSZ) then
           SDDWP(I) = SDDWP(I) + DDWP(I,J,K) * FT3pACRtoM3pHA
        end if
        320 continue
     end do
     300 continue
  end do

  !     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !     Calculate RBAKT = ratio of killed to total stand BA for each size class
  !               TVOLT = total volume of trees in stand in size class
  !               TVOLK = total volume of killed trees in size class
  !               SBATKT= Fraction of total stand basal area killed
  !               SSBAT = sum of total stand basal area
  !               SSBATK(I)= sum of basal area killed, by stand

  ! Note that PBKILL was convertied to number of trees at end
  !     of BMISTD.F

  !     (RNH July98, Aug98)

  SSBAT = 0.0
  do IIKK = 1,NSCL
     SSBAT = SSBAT + BAH(I, IIKK) + BANH(I,IIKK)
     312 continue
  end do

  SSBATK(I) = 0.0
  STVOLT = 0.0
  STVOLK = 0.0
  do JSCL1 = 1, NSCL

     !     The following added 10/7/99 to correct output reporting problem.
     !     Currently, RBATK(I,JSCL1) gets reported as previous year's value
     !     if the size class BA gets reduced to zero.

     RBAKT(I,JSCL1) = 0.0

     if ((BAH(I, JSCL1) + BANH(I, JSCL1)) .le.0.0) go to 310

     !     Use the PBAREM parameter that ESSA uses.  Because PBKILL was
     !     transformed to number of trees per acre in BMISTD

     TOTKLL = (PBKILL(I,JSCL1)+ALLKLL(I,JSCL1))

     if (TREE(I,JSCL1,1) .gt.1E-6) then
        PBAREM = TOTKLL / TREE(I,JSCL1,1)
     else
        PBAREM = 0.0
     end if

     !     Calculate percent (RNH 18 Nov98) basal area killed by size class

     RBAKT(I,JSCL1) = BAH(I,JSCL1)*PBAREM/SSBAT*100.

     !     Sum basal area killed
     !     SSBATK changed to a MXSTAND-dimensioned array 8/99 (AJM)

     SSBATK(I) = SSBATK(I) + BAH(I,JSCL1)*PBAREM

     TVOLT = (TVOL(I,JSCL1,1)*TREE(I,JSCL1,1)&
          &+ TVOL(I,JSCL1,2)*TREE(I,JSCL1,2))

     STVOLT = STVOLT + TVOLT

     TVOLK = TVOL(I,JSCL1,1)*TOTKLL

     STVOLK = STVOLK + TVOLK

     if (TVOLT .le.1.0E-6) TVOLT = 1.0E-6

     if (STVOLT .ge.1.0E-6) then
        TOTKT = STVOLK/STVOLT
     else
        TOTKT = 0.0
     end if

     310 continue
  end do

  !     SET landscape BA killed

  !     modified 6/28/05 ajm.  this needs to happen after we calculate SBATKT!
  !     comment out next line, add it to below next IF...ENDIF block.

  !      LSBAK1= SBATKT

  !     Change  SBATKT to percentfor SPY output (RNH 18Nov98)

  !     Added loop to account for the rare occasion when no BA remains in
  !     stand.
  if (SSBAT .gt.0) then
     SBATKT = SSBATK(I)/SSBAT*100.
  else
     SBATKT = 0
  end if

  LSBAK1 = SBATKT

  !     Write annual individual stand output to unit number JBMSPY
  !     (RNH July98)
  !     WRite header for file

  if (LBMSPY .and. IPRHEDS .le.0) then
     write (JBMSPY, 313)
     write (JBMSPY, 314)
     write (JBMSPY, 315)

     !     BASAL AREA OF HOST BEETLE-KILLED AND BAH REMAINING ADDED (BELOW)
     !     TO THE *.SPY (ANNUAL) OUTPUT.  ajm 10/6/99

     313 format(T25,'INDIVIDUAL STAND ANNUAL OUTPUT VARIABLES')
     314 FORMAT(T63,'% BASAL AREA OF SIZE CLASS KILLED / TOTAL STAND BASAL &
          &AREA')
     !     , 23X, 'BASAL AREA OF BEETLE-KILLED HOST TREES BY SIZE CLASS',
     !     2 20X, 'BASAL AREA OF HOST TREES BY SIZE CLASS')

     315 format(1X,'YEAR', ' STAND ID','    BKP','      RV','    BA   ',&
          &'  VOLSAN ','VOLSAL','  TVOLT ','  TVOLK ','   B1/TOT ','B2/TOT ',&
          &'B3/TOT ','B4/TOT ','B5/TOT ','B6/TOT ','B7/TOT ','B8/TOT ',&
          &'B9/TOT',' B10/TOT',' TOT/TOT')
     !     , '  0-3  ', '  3-6  ', '  6-9  ',
     !     3 '  9-12 ', ' 12-15 ', ' 15-18 ', ' 18-21 ', ' 21-25 ', ' 25-30 ',
     !     4 '  >30  ', 1X, '  0-3  ', '  3-6  ', '  6-9  ',
     !     5 '  9-12 ', ' 12-15 ', ' 15-18 ', ' 18-21 ', ' 21-25 ', ' 25-30 ',
     !     6 '  >30  ')

     IPRHEDS = 1

  end if

  !     Output volumes for current year only through checking print flags

  if ((VTEMP(I,1,2) .le.0.) .and. (VTEMP(I,1,1) .gt.0.)) then
     VTEMP1 = VTEMP(I,1,1)
     VTEMP(I,1,2) = 1.
  else
     VTEMP1 = 0.0
  end if

  if ((VTEMP(I,2,2) .le.0.) .and. (VTEMP(I,2,1) .gt.0.)) then
     VTEMP2 = VTEMP(I,2,1)
     VTEMP(I,2,2) = 1.
  else
     VTEMP2 = 0.0
  end if

  !     CHANGE ALL VARIABLES INTO METRIC

  SSBAT = SSBAT * FT2pACRtoM2pHA
  VTEMP1 = VTEMP1 * FT3pACRtoM3pHA
  VTEMP2 = VTEMP2 * FT3pACRtoM3pHA
  STVOLT = STVOLT * FT3pACRtoM3pHA
  STVOLK = STVOLK * FT3pACRtoM3pHA
  SBATKT = SBATKT * FT2pACRtoM2pHA

  if (LBMSPY) then
     write (JBMSPY, 316) IYEAR, BMSTDS(I), BKPA(I)/ACRtoHA,&
          &GRFSTD(I), SSBAT, VTEMP1,VTEMP2,STVOLT,STVOLK,&
          &(RBAKT(I,JSCL1), JSCL1 = 1, NSCL),SBATKT
     !     ,
     !     4           (SBAKL(I,JNSCL), JNSCL=1, NSCL),
     !     5           (BAH(I,JSCL1), JSCL1=1,NSCL)
  end if

  316 format (1X, I4, 1X, A8, 1X, F7.2, 1X, F7.4, 1X, F7.2,&
       &1X, F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,11F7.2)
  !     , 1X,
  !     2       10F7.2, 1X, 10F7.2)

  !     Write annual landscape means to unit number JBMLPY

  !     Count stands and accumulate summary information using method
  !     shamelessly adapted from ESSA code here

  if (ICNT1.ge. BMEND) ICNT1 = 0
  ICNT1 = ICNT1 + 1
  if (ICNT1.eq.1) SACRES1 = 0.0
  call SPLAAR (I, ACRES1, IRC)

  if (ACRES1.le.0.) ACRES1 = 1.0

  SACRES1 = SACRES1 + ACRES1

  !     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !     Accumulate stand data to calculate landscape means
  !     using adapted code from ESSA with variable names changed here

  LBKP1 = LBKP1 + BKPA(I) * ACRES1
  LRVSTD1 = LRVSTD1 + GRFSTD(I) * ACRES1
  LBAT1 = LBAT1 + SSBAT*ACRES1
  LPROPH1 = LPROPH1 + PROPH(I) * ACRES1

  LVKILL1 = LVKILL1 + SVKILL1 * ACRES1

  !     Reinitialize SVKILL1
  SVKILL1 = 0.0

  !     Temporary storage for volumes to be zeroed each year (RNH Jul98)
  !     VOLREM(I,J) is only zeroed on a cycle basis

  VREM1 = VOLREM(I,1)
  VREM2 = VOLREM(I,2)

  LTBAK1 = LTBAK1 + LSBAK1*ACRES1
  LSREM1 = LSREM1 + SREMOV(I) * ACRES1
  LVREM1(1) = LVREM1(1) + VREM1 * ACRES1
  LVREM1(2) = LVREM1(2) + VREM2 * ACRES1

  LBKPO1 = LBKPO1 + BKPOUT(1,I) * ACRES1
  LBKPI1 = LBKPI1 + BKPIN(1,I) * ACRES1
  LBKPS1 = LBKPS1 + BKPS(I) * ACRES1

  !     Generate output when all stands have been counted for year

  if (ICNT1.ge. BMEND) then

     Y1 = 1.0 / SACRES1

     LTBAK1 = LTBAK1*Y1
     LBKP1 = LBKP1 * Y1
     LRVSTD1 = LRVSTD1 * Y1
     LBAT1 = LBAT1 * Y1
     LPROPH1 = LPROPH1 * Y1

     LDDWP1 = LDDWP1 * Y1
     LSDWP1 = LSDWP1 * Y1

     LVKILL1 = LVKILL1 * Y1

     LSREM1 = LSREM1 * Y1
     LVREM1(1) = LVREM1(1) * Y1
     LVREM1(2) = LVREM1(2) * Y1

     LBKPO1 = LBKPO1 * Y1
     LBKPI1 = LBKPI1 * Y1
     LBKPS1 = LBKPS1 * Y1

     do ISIZ = 1,(NSCL+1)
        LBAKL1(ISIZ) = LBAKL1(ISIZ) * Y1
        LBAH1(ISIZ) = LBAH1(ISIZ) * Y1
        73 continue
     end do

     !     Write annual Landscape output to unit number JBMLPY
     !     (RNH July98)

     !     The variable TTVOL is the total volume of the stand from OUTCOM
     !     common and calculated from OCVCUR(7) array (rnh July98)

     if (LBMLPY .and. IPRHEDL .le.0) then

        write (JBMLPY, 301)
        write (JBMLPY, 302)
        301 format(T15,'LANDSCAPE ANNUAL STAND-AREA-WEIGHTED OUTPUT VARIABLES'&
             &/)
        302 format(' YEAR','      L-BKP ','    L-RV  ','  TOT-BA  ',&
             &'T-VOL-KILL',&
             &' T-BA-KILL','  VOL-SANI ',' VOL-SALV ')

        IPRHEDL = 1
     end if

     !   WRITE OUTPUT FILE FOR TEST MONTECARLO SIMULATIONS AJM 7/00
     !      OPEN (92,FILE='VRRAN.OUT',STATUS='OLD',POSITION='APPEND')
     !      WRITE (92,*) DREWTEST,IYEAR,LBKP1,LBAT1

     if (LBMLPY) then
        write (JBMLPY, 303) IYEAR, LBKP1, LRVSTD1, LBAT1,&
             &LVKILL1,LTBAK1,&
             &LVREM1(1), LVREM1(2)
     end if

     !     Reinitialize

     TTVOL = 0.0
     LBKP1 = 0.0
     LRVSTD1 = 0.0
     LBAT1 = 0.0
     LVKILL1 = 0.0
     LTBAK1 = 0.0
     LBKPS1 = 0.0
     LBKPO1 = 0.0
     LBKPI1 = 0.0
     LPROPH1 = 0.0
     LDDWP1 = 0.0
     LSDWP1 = 0.0
     LSREM1 = 0.0
     LVREM1(1) = 0.0
     LVREM1(2) = 0.0
     VREM1 = 0.0
     VREM2 = 0.0
     if (.not. LBMDVO) LRVSTD1 = 0.0

  end if

  !     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  303 format (1X, I4, 1X, 8F10.3)

  !     2           SRVSTD(I), SBAT(I),
  !     3           (SBAKL(I,JNSCL), JNSCL= 1, NSCL+1)

  !            WRITE (JBMVOL,90) RYEARP,'LANDSCAPE',LBKP,LRVSTD,LBAT,
  !     >       LVKILL,LBAKL(NSCL+1),
  !     >       LVREM(1), LVREM(2), TVOL
  !   90       FORMAT (1X, I4, 1X, A9, F8.2, F6.2, F8.2, I4, 1X, 11F10.2)

  !     End of annual landscape calculations
  !     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  !       Print state information averaged within the master cycle.

  if (IYEAR .eq. RYEAR) then
     if (IBMMRT .le.0) goto 85

     SSREM(I) = SREMOV(I)
     SVREM(I,1) = VOLREM(I,1)
     SVREM(I,2) = VOLREM(I,2)

     X = 1.0 / FLOAT(IBMMRT)
     SBKP(I) = SBKP(I) * X
     SRVSTD(I) = SRVSTD(I) * X
     SBAT(I) = SBAT(I) * X
     if (SBAT(I) .gt.1E-6) then
        PROPH(I) = (SBAH(I,NSCL+1) / SBAT(I)) * X
     else
        PROPH(I) = 0.0
     end if

     SDDWP(I) = SDDWP(I) * X
     SSDWP(I) = SSDWP(I) * X

     STKILL(I) = STKILL(I) * X
     SVKILL(I) = SVKILL(I) * X
     SBAKL(I,NSCL+1) = SBAKL(I,NSCL+1) * X

     SSREM(I) = SSREM(I) * X
     SVREM(I,1) = SVREM(I,1) * X
     SVREM(I,2) = SVREM(I,2) * X

     do ISIZ = 1,NSCL
        SBAKL(I,ISIZ) = SBAKL(I,ISIZ) * X
        SBAH(I,ISIZ) = SBAH(I,ISIZ) * X
        31 continue
     end do

     SBKPO(I) = SBKPO(I) * X
     SBKPI(I) = SBKPI(I) * X
     SBKPS(I) = SBKPS(I) * X
     SBKPIS(I) = SBKPIS(I) * X

     !       Write the stand averages to the main beetle output file

     !     Changed RYEAR2 to RYEARP to specify end rather than beginning
     !     of cycle in output table (RNH June98),

     if (LBMVOL) then
        !          WRITE(JBMVOL,60) RYEAR2, BMSTDS(I), SBKP(I),SRVSTD(I),SBAT(I),
        write(JBMVOL,60) RYEARP, BMSTDS(I), SBKP(I),SRVSTD(I),SBAT(I),&
             &INT(PROPH(I)*100),SDDWP(I),SSDWP(I),STKILL(I),SVKILL(I),&
             &SBAKL(I,NSCL+1), SSREM(I), SVREM(I,1), SVREM(I,2)
        60 format (1X, I4, 1X, A8, 1X, F8.2, F6.2, F8.2, I4, 8(F9.2))
     end if

     !       Write the stand averages to the detailed beetle output file (if requested)

     if (LBMDET) then
        !         WRITE(JBMDBH,65) RYEAR2, BMSTDS(I),INT(SBKPS(I)*100),SBKP(I),
        write(JBMDBH,65) RYEARP, BMSTDS(I),INT(SBKPS(I)*100),SBKP(I),&
             &SBKPI(I),SBKPO(I), (SBAKL(I,IZ),IZ = 1,NSCL),&
             &(SBAH(I,IZ),IZ = 1,NSCL), SBKPIS(I)
        65 format (1X,I4,1X,A8,1X,I5,3(F7.2),10(F7.2),10(F7.1),F7.2)
     end if

     !     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.

     if (ICNT .ge. BMEND) ICNT = 0
     ICNT = ICNT + 1

     !       Print landscape averages.

     if (ICNT .eq.1) SACRES = 0.0

     call SPLAAR (I, ACRES, IRC)
     if (ACRES .le.0.0) ACRES = 1.0

     !        CHANGE ACRES TO BE IN HA INSTEAD
     ACRES = ACRES * ACRtoHA

     SACRES = SACRES + ACRES

     LBKP = LBKP + SBKP(I) * ACRES
     LRVSTD = LRVSTD + SRVSTD(I) * ACRES
     LBAT = LBAT + SBAT(I) * ACRES
     LPROPH = LPROPH + PROPH(I) * ACRES

     LDDWP = LDDWP + SDDWP(I) * ACRES
     LSDWP = LSDWP + SSDWP(I) * ACRES

     LTKILL = LTKILL + STKILL(I) * ACRES
     LVKILL = LVKILL + SVKILL(I) * ACRES
     LBAKL(NSCL+1) = LBAKL(NSCL+1) + SBAKL(I,NSCL+1) * ACRES

     LSREM = LSREM + SSREM(I) * ACRES
     LVREM(1) = LVREM(1) + SVREM(I,1) * ACRES
     LVREM(2) = LVREM(2) + SVREM(I,2) * ACRES

     LBKPO = LBKPO + SBKPO(I) * ACRES
     LBKPI = LBKPI + SBKPI(I) * ACRES
     LBKPS = LBKPS + SBKPS(I) * ACRES
     LBKPIS = LBKPIS + SBKPIS(I) * ACRES

     do ISIZ = 1,NSCL
        LBAKL(ISIZ) = LBAKL(ISIZ) + SBAKL(I,ISIZ) * ACRES
        LBAH(ISIZ) = LBAH(ISIZ) + SBAH(I,ISIZ) * ACRES
        81 continue
     end do

     if (ICNT .ge. BMEND) then

        y = 1.0 / SACRES

        LBKP = LBKP * Y
        LRVSTD = LRVSTD * Y
        LBAT = LBAT * Y
        LPROPH = LPROPH * Y

        LDDWP = LDDWP * Y
        LSDWP = LSDWP * Y

        LTKILL = LTKILL * Y
        LVKILL = LVKILL * Y
        LBAKL(NSCL+1) = LBAKL(NSCL+1) * Y

        LSREM = LSREM * Y
        LVREM(1) = LVREM(1) * Y
        LVREM(2) = LVREM(2) * Y

        LBKPO = LBKPO * Y
        LBKPI = LBKPI * Y
        LBKPS = LBKPS * Y
        LBKPIS = LBKPIS * Y

        do ISIZ = 1,NSCL
           LBAKL(ISIZ) = LBAKL(ISIZ) * Y
           LBAH(ISIZ) = LBAH(ISIZ) * y
           82 continue
        end do

        if (LBMVOL) then

           !     Changed RYEAR2 to RYEARP to specify end rather than beginning
           !     of cycle in output table (RNH June98), next 2 write statements

           !            WRITE (JBMVOL,90) RYEAR2,'LANDSCAPE',LBKP,LRVSTD,LBAT,

           write (JBMVOL,90) RYEARP,'LANDSCAPE',LBKP,LRVSTD,LBAT,&
                &INT(LPROPH*100),LDDWP,LSDWP,LTKILL,LVKILL,LBAKL(NSCL+1),&
                &LSREM, LVREM(1), LVREM(2)
           90 format (1X, I4, 1X, A9, F8.2, F6.2, F8.2, I4, 8(F9.2))
        end if

        if (LBMDET) then
           !            WRITE(JBMDBH,95) RYEAR2,'LANDSCAPE',INT(LBKPS*100),LBKP,
           write(JBMDBH,95) RYEARP,'LANDSCAPE',INT(LBKPS*100),LBKP,&
                &LBKPI,LBKPO,(LBAKL(ISIZ),ISIZ = 1,NSCL),&
                &(LBAH(ISIZ),ISIZ = 1,NSCL), LBKPIS
           95 format (1X,I4,1X,A9,I5,3(F7.2),10(F7.2),10(F7.1),F7.2)
        end if

        !     Zero out all summary variables after printing.

        LBKP = 0.0
        LBKPS = 0.0
        LBKPO = 0.0
        LBKPI = 0.0
        LBKPIS = 0.0
        LBAT = 0.0
        LPROPH = 0.0
        LDDWP = 0.0
        LSDWP = 0.0
        LTKILL = 0.0
        LVKILL = 0.0
        LSREM = 0.0
        LVREM(1) = 0.0
        LVREM(2) = 0.0
        if (.not. LBMDVO) LRVSTD = 0.0

        do ISIZ = 1,NSCL+1
           LBAKL(ISIZ) = 0.0
           LBAH(ISIZ) = 0.0
           71 continue
        end do
     end if

     SBKP(I) = 0.0
     SBKPS(I) = 0.0
     SBKPI(I) = 0.0
     SBKPIS(I) = 0.0
     SBKPO(I) = 0.0
     SBAT(I) = 0.0
     PROPH(I) = 0.0
     SDDWP(I) = 0.0
     SSDWP(I) = 0.0
     STKILL(I) = 0.0
     SVKILL(I) = 0.0
     SSREM(I) = 0.0
     SVREM(I,1) = 0.0
     SVREM(I,2) = 0.0
     if (.not. LBMDVO) SRVSTD(I) = 0.0

     do ISIZ = 1,NSCL+1
        SBAH(I,ISIZ) = 0.0
        SBAKL(I,ISIZ) = 0.0
        75 continue
     end do
  end if

  85 continue

  return
end
