!==================================================!
!   Coordinate regeneration and stand projection   !
!==================================================!
! AK $Id$
subroutine ESNUTR
  implicit none

  ! ESTB $Id: esnutr.f 2862 2019-11-19 13:41:38Z ckeyser01 $

  !     INTERFACE ROUTINE TO COUPLE THE REGEN MODEL AND PROGNOSIS.

  !  MODIFIED FOR ALASKA TO MAKE AUTOESTAB KICK IN
  !  EVERY 50 YEARS INSTEAD OF EVERY 20 (ANTOS ET AL,2000)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'OUTCOM.f90'
  include 'PLOT.f90'
  include 'ESHAP.f90'
  include 'ESHAP2.f90'

  ! Variables
  logical :: DEBUG,LONE

  integer :: NCLAS
  parameter (NCLAS = INT(real(MAXTRE) * 0.4))

  integer :: I,IACTK,IDT,ISQ,IST,ITODO,ITRGT,J,KD,KDT,KEEP
  integer :: NP,NP1,NPNATS,NT,NTODO

  integer :: IESTB(3),MYACT2(5),IGRP,IULIM,IG,IGSP

  real    :: XCUF,XTPA

  real    :: PRMS(3)

  !  DATA STATEMENTS:

  data IESTB/427,428,429/,MYACT2/95,440,442,430,431/

  LONE = .false.

  !     SET THE DEBUG OPTION

  call DBCHK (DEBUG,'ESNUTR',6,ICYC)

  !     CALL ESADDT TO ADD TREES (OR OPTIONS) FROM EXTERNAL SOURCES

  call ESADDT(1)

  !     PROCESS SPECMULT, STOCKADJ, AND HTADJ OPTIONS.

  call OPFIND (3,MYACT2,NTODO)
  if (DEBUG) write (JOSTND,5) NTODO
  5 format ('IN ESNUTR: OPTS NTODO=',I2)
  if (NTODO.gt.0) then
     KDT = IY(ICYC)
     do I = 1,NTODO
        call OPGET (I,2,IDT,IACTK,NP,PRMS)
        if (IACTK.lt.0) goto 60
        if (IACTK.eq.95) then
           J = IFIX(PRMS(1))
           if (J .lt. 0) then
              IGRP = -J
              IULIM = ISPGRP(IGRP,1)+1
              do IG = 2,IULIM
                 IGSP = ISPGRP(IGRP,IG)
                 XESMLT(IGSP) = PRMS(2)
                 19 continue
              end do
           else if (J .eq. 0) then
              do J = 1,MAXSP
                 XESMLT(J) = PRMS(2)
                 20 continue
              end do
           else
              XESMLT(J) = PRMS(2)
           end if
        else if (IACTK.eq.440) then
           STOADJ = PRMS(1)
        else if (IACTK.eq.442) then
           J = IFIX(PRMS(1))
           if (J .lt. 0) then
              IGRP = -J
              IULIM = ISPGRP(IGRP,1)+1
              do IG = 2,IULIM
                 IGSP = ISPGRP(IGRP,IG)
                 HTADJ(IGSP) = PRMS(2)
                 21 continue
              end do
           else if (J .eq. 0) then
              do J = 1,MAXSP
                 HTADJ(J) = PRMS(2)
                 30 continue
              end do
           else
              HTADJ(J) = PRMS(2)
           end if
        end if
        call OPDONE (I,KDT)
        60 continue
     end do
  end if

  !     IF THE DATE OF DISTURBANCE IS -9999 THEN IT HAS NEVER BEEN
  !     ESTABLISHED...SET THE DEFAULT VALUE TO 20 YRS PRIOR TO INVENTORY.

  if (IDSDAT.eq.-9999) IDSDAT = IY(1)-20

  !     SET DATE OF LAST REMOVAL.  RULES: IF MORE THAN ZERO TREES
  !     HAVE BEEN CUT THIS CYCLE.

  if (IYRLRM.eq.-9999 ) IYRLRM = IY( 1  )-20
  if (ONTREM(7).gt.0.0) IYRLRM = IY(ICYC)

  !     LOGIC FOR CALLING THE SPROUT SUBROUTINE.

  if (DEBUG) write (JOSTND,70) ITRNRM,LSPRUT
  70 format ('IN ESNUTR: ITRNRM=',I5,'; LSPRUT=',L2)
  if (LSPRUT) then
     if (ITRNRM.ge.1) then
        if (ITRN+ITRNRM.gt.MAXTRE) then
           ITRGT = MAXTRE-ITRNRM
           if (NCLAS.gt.ITRGT) ITRGT = NCLAS
           call ESCPRS (ITRGT,DEBUG)
        end if
        call ESUCKR

        !           SET THE VALUE OF IREC1 TO POINT TO THE LAST LOCATION IN THE
        !           TREELIST. CALL SPESRT TO REESTABLISH THE SPECIES ORDER SORT.

        IREC1 = ITRN
        call SPESRT

        !           REESTABLISH THE DIAMETER SORT (INCLUDE THE NEW TREES).

        if (ITRN .gt. 0) call RDPSRT (ITRN,DBH,IND,.true.)

        !           SET IFST=1 SO THAT THE SAMPLE TREE RECORDS WILL BE REPICKED
        !           NEXT TIME DIST IS CALLED.

        IFST = 1
     end if
  else

     !        INSURE THAT ITRNRM IS ZERO

     ITRNRM = 0
  end if

  !     SET KDT TO THE LAST YEAR OF THE CYCLE.

  KDT = IY(ICYC+1)-1

  !     FIND OUT IF THE REGEN MODEL IS TO BE CALLED THIS CYCLE.
  !     FIRST, FIND ALL TALLYONE'S THAT ARE SCHEDULED FOR THIS CYCLE.

  call OPFIND (1,IESTB(2),NTODO)

  !     IF THERE IS NO TALLYONE, LOOK FOR A TALLYTWO.

  if (NTODO.eq.0) then
     call OPFIND (1,IESTB(3),NTODO)

     !        IF THERE IS NO TALLYTWO, BRANCH TO TALLY PROCESSING.

     if (NTODO.eq.0) goto 100
  end if

  !     GET EITHER TALLY (ONE OR TWO), COMPUTE NTALLY (THE TALLY NUMBER),
  !     USING THE ACTIVITY CODES.

  call OPGET (NTODO,1,IDT,IACTK,NP,PRMS)
  if (IACTK.lt.0) goto 100
  NTALLY = IACTK-427
  IDSDAT = IFIX(PRMS(1))
  LONE = .true.

  !     CONVERT IDSDAT TO YEAR IF REQUIRED

  if ((IDSDAT.lt.1000).and.(IDSDAT.ge.1))IDSDAT = IY(IDSDAT)

  !     IF THE SCHEDULING OF TALLYONE OR TALLYTWO RESULTS IN THEM
  !     BEING ACCOMPLISHED GREATER THAN 20 YEARS FROM THE DATE OF
  !     DISTURBANCE, ISSUE AN ERROR MESSAGE, CANCEL THEM, RETURN.

  if (KDT+1-IDSDAT.gt.20) then
     write (JOSTND,80) IDSDAT,KDT,NTALLY
     80 format(/'REGENERATION MODEL CANNOT PREDICT REGENERATION', &
          & ' TALLIES BEYOND 20 YEARS FROM DATE OF DISTURBANCE',/, &
          & 'DISTURBANCE DATE=',I6,'  TALLY DATE=',I6,'  TALLY=',I3)
     goto 300
  end if

  !     IF THIS IS A TALLYTWO: FIND OUT IF TALLYONE HAS BEEN EXECUTED
  !     WITHIN THE LAST 19 YEARS.  IF ONE HAS NOT BEEN, CHANGE THE
  !     TALLYTWO TO TALLYONE.

  if (NTALLY.eq.2) then
     ISQ = 0
     call OPSTUS (428,IDSDAT,KDT,ISQ,NT,IDT,NP1,IST,KD)
     if (KD.gt.0 .or. IST.le.IDSDAT) then
        write (JOSTND,90) IY(ICYC+1)-1
        90 format (/,'TALLYTWO CHANGED TO TALLYONE. YEAR=',I4)
        NTALLY = 1
     end if
  end if
  goto 200
  100 continue

  !     CHECK FOR THE TALLY KEYWORD.

  call OPFIND (1,IESTB(1),NTODO)

  if (DEBUG) write (JOSTND,105) NTODO,IDSDAT
  105 format ('IN ESNUTR: NTODO=',I4,'; IDSDAT=',I5)

  !     IF A TALLY ACTIVITY IS PRESENT, FIND THE ONE WITH THE 'LATEST'
  !     DATE OF DISTURBANCE, SET NTALLY=1, CALL OPDONE, AND BRANCH TO
  !     ESTAB CALLING SEQUENCE.

  if (NTODO.gt.0) then
     IDSDAT = -1
     do ITODO = 1,NTODO
        call OPGET(ITODO,1,IDT,IACTK,NP,PRMS)
        if (IACTK.lt.0) goto 120
        I = IFIX(PRMS(1))
        if (I.gt.IDSDAT) then
           IDSDAT = I
           ISQ = ITODO
        end if
        110 continue
     end do

     !     CONVERT IDSDAT TO YEAR IF REQUIRED

     if ((IDSDAT.lt.1000).and.(IDSDAT.ge.1))IDSDAT = IY(IDSDAT)

     !        IF THE TALLY GREATER THAN 20 YEARS FROM THE DATE OF
     !        DISTURBANCE, ISSUE AN ERROR MESSAGE, CANCEL THEM, RETURN.

     if (KDT+1-IDSDAT.gt.20) then
        write (JOSTND,80) IDSDAT,KDT,NTALLY
        goto 300
     end if

     NTODO = ISQ
     NTALLY = 1
     goto 200
  end if
  120 continue

  !     SET NUMBER OF ACTIVITIES TO DO TO ZERO...THE FOLLOWING LOGIC
  !     TRIGGERS ESTAB CALLS WITHOUT AN ACTIVITY BEING PRESENT.

  NTODO = 0

  !     CALL FOR AUTAL: AUTOMATIC TALLIES.

  if (LAUTAL) then

     !        AUTAL: IF BETWEEN 'THRES1' TO 'THRES2' PERCENT OF STAND HAS
     !        BEEN REMOVED, THEN, SET UP ONE TALLY 1.
     !        IF OVER 'THRES2' PERCENT OF STAND HAS BEEN REMOVED, SET UP A
     !        NORMAL TALLY SEQUENCE.

     XTPA = 0.0
     if (ONTCUR(7).gt.0.0) XTPA = ONTREM(7)/ONTCUR(7)
     XCUF = 0.0
     if (OCVCUR(7).gt.0.0) XCUF = OCVREM(7)/OCVCUR(7)
     XTES = AMAX1(XTPA,XCUF)
     LONE = (XTES.ge.THRES1.and.XTES.lt.THRES2)
     if (DEBUG) write (JOSTND,130) XTPA,XCUF,LONE
     130 format ('IN ESNUTR: XTPA XCUF=',2F6.3,'; LONE=',L2)
     if (LONE .or. (XTPA.ge.THRES2 .or. XCUF.ge.THRES2)) then
        IDSDAT = IY(ICYC)
        NTALLY = 1
        KDT = IY(ICYC+1)-1

        !           ADD THE TALLY TO THE ACTIVITY SCHEDULE.

        PRMS(1) = FLOAT(IDSDAT)
        call OPADD (KDT,IESTB(1),KDT,1,PRMS,I)
        goto 200
     end if
  end if

  !     CALL ESTAB IF CURRENT DATE IS WITHIN 20-YRS OF A DISTURBANCE
  !     AND IF NTALLY IS GT 0.

  if (DEBUG) write (JOSTND,150) KDT,IDSDAT,NTALLY
  150 format ('IN ESNUTR: KDT=',I6,'; IDSDAT=',I6,'; NTALLY=',I5)

  if (KDT-IDSDAT.le.19 .and. NTALLY.gt.0) then
     PRMS(1) = FLOAT(IDSDAT)
     NTALLY = NTALLY+1
     PRMS(2) = FLOAT(NTALLY)
     NP = 2
     KDT = IY(ICYC+1)-1
     call OPADD (KDT,IESTB(1),KDT,NP,PRMS,I)
     goto 200
  end if

  !     CALL ESTAB FOR INGROWTH...RULES:
  !     1. IF THERE ARE NO TALLYIES SCHEDULED IN THE NEXT CYCLE.
  !     2. IF THE CURRENT YEAR IS GE THAN 39 YEARS PAST THE LAST DATE
  !        OF DISTURBANCE, OR THERE ARE NO TREES IN THE TREE LIST.

  if (LINGRW) then

     !        CHECK TO SEE IF THERE ARE ANY TALLIES IN THE NEXT CYCLE.
     !        CALL OPCSET TO SET POINTERS TO THE NEXT CYCLE AND CALL OPFIND.

     if (IFINT.le.20) then
        call OPCSET (ICYC+1)
        call OPFIND (3,IESTB,KEEP)
        call OPCSET (ICYC)
        if (DEBUG) write (JOSTND, &
             & '(''IN ESNUTR: TALLIES NEXT CYCLE='',I3)') KEEP
        if (KEEP.gt.0) goto 400
     end if

     if (DEBUG) write(JOSTND,160) ITRN,ICYC,IY(ICYC+1),IYRLRM, &
          & IDSDAT
     160 format('IN ESNUTR: ITRN=',I4,'  ICYC=',I3,'  IY(ICYC+1)=', &
          & I5,'  IYRLRM=',I5,'  IDSDAT=',I5)
     if ((ITRN.eq.0 .and. ICYC.eq.1)   .or. &
          & (IY(ICYC+1)-IDSDAT .ge. 40) ) then

        !           SET NTALLY=99 TO SIGNAL ESTAB THAT THIS IS INGROWTH
        !           AND SET THE DATE OF DISTURBANCE EQUAL TO 20 YEARS AGO.

        NTALLY = 99
        IDSDAT = IY(ICYC+1)-20
        LONE = .true.
        IDT = 0
        goto 200
     end if
  end if

  !     CALL ESTAB IF THERE ARE PLANT OR NATURAL KEYWORDS IN THE CASE
  !     WHERE ESTAB WOULD NOT OTHERWISE BE CALLED.

  call OPFIND (2,MYACT2(4),NPNATS)
  if (DEBUG) write (JOSTND,170) NPNATS
  170 format ('IN ESNUTR: OPTS NPNATS=',I2)
  if (NPNATS.gt.0) then

     !        CALL ESTAB WITH NTALLY=99 AND AN OLD DATE OF DISTURBANCE

     NTALLY = 99
     IDSDAT = IY(ICYC+1)-20
     IDT = 0
     goto 200
  end if

  if (DEBUG) write (JOSTND,'(''IN ESNUTR: NO ESTAB.'')')

  goto 400

  !     CALLING SEQUENCE USED REGARDLESS OF THE OPTION FOR TALLIES.

  200 continue

  !     THE DATE OF LAST REMOVAL IS NO OLDER THAN THE DATE OF DISTURB.

  if (IYRLRM.lt.IDSDAT) IYRLRM = IDSDAT

  !     IF ALL OF THE PLOTS ARE NONSTOCKABLE, WRITE A MSG, BRANCH
  !     TO CANCEL THESE TALLIES, AND RETURN.

  if (IPINFO.ge.4) then
     write (JOSTND,210) NPLT,IY(ICYC+1)-1
     210 format (/,'NOTE: NONE OF THE PLOTS ARE STOCKABLE. ', &
          & ' STAND ID: ',A,'; YEAR=',I5)
     goto 300
  end if

  !     SIGNAL THAT THE TALLY IS DONE (ONLY IF NTODO IS GT 0).
  !     THIS IS DONE BEFORE CALL TO ESTAB BECAUSE ESTAB CALLS OPFIND
  !     AND CALLS TO OPFIND CANNOT BE NESTED.

  if (NTODO.gt.0) call OPDONE (NTODO,KDT)

  !     MAKE SURE THERE IS ROOM IN THE TREELIST TO HOLD NEW TREES.

  if (ITRN.gt.MAXTRE*.7 .and. NTODO.gt.0) call ESCPRS (NCLAS,DEBUG)

  !     ESTABLISH NEW TREES

  if (DEBUG) write (JOSTND,220) IDT,IDSDAT,IYRLRM,LONE, &
       & NTALLY,KDT,ITRN,NTODO
  220 format('IN ESNUTR: CALLING ESTAB: IDT=',I5,'; IDSDAT=',I5, &
       & '; IYRLRM=',I5,'; LONE=',L2/T12,'NTALLY=',I3,'; KDT=', &
       & I5,'; ITRN=',I5,'; NTODO=',I2)

  call ESTAB (KDT)

  !     REESTABLISH THE DIAMETER SORT (INCLUDE THE NEW TREES).

  if (ITRN .gt. 0) call RDPSRT (ITRN,DBH,IND,.true.)

  !     SET IFST=1 SO THAT THE SAMPLE TREE RECORDS WILL BE REPICKED
  !     NEXT TIME DIST IS CALLED.

  IFST = 1

  !     IF THE CALL WAS BASED ON A TALLYONE OR TALLYTWO, OR
  !     IF THE CALL WAS TRIGGERED BY A MODERATE REMOVAL, OR INGROWTH
  !     THEN, SET NTALLY EQUAL TO 0.

  if (LONE) NTALLY = 0
  300 continue

  !     FIND ALL OF THE TALLIES THAT ARE SCHEDULED FOR THIS CYCLE, BUT
  !     NOT YET DONE, AND DELETE THEM.

  call OPFIND (3,IESTB,NTODO)
  if (NTODO.gt.0) then
     do I = 1,NTODO
        call OPDEL1(I)
        310 continue
     end do
  end if
  400 continue
  return
end subroutine ESNUTR
