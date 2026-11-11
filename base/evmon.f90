!==================================!
!   Evaluate and schedule events   !
!==================================!
! BASE $Id$
subroutine EVMON (IPH,IPPCL)
  implicit none

  !     CALLED FROM TREGRO.
  !     TASKS:
  !     1) AGES EVENTS.
  !     2) MAINTAINS TEST VARIABLE TABLES.
  !     3) DETERMINES WHICH EVENTS OCCUR.
  !     4) POSTS THE DATE EVENTS OCCUR.
  !     5) SCHEDULES THE ACTIVITIES WHICH ARE TO FOLLOW AN EVENT.

  !     EVENT MONITOR ROUTINE - NL CROOKSTON - AUG 1982 - MOSCOW, ID
  !     MAJOR REWRITE IN JAN 1987 - NL CROOKSTON - MOSCOW.

  !     IPH   = THE PROJECTION PHASE NUMBER...USED TO SET IPHASE.
  !     IPPCL = FLAG CONTROLLING ACTIVITY GROUP PROCESSING.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'OPCOM.f90'
  include 'CONTRL.f90'

  !     MAE   = LENGTH OF MAELNK.
  !     MAELNK= LIST OF ACTIVITY GROUPS THAT ARE ASSOCIATED WITH
  !             AN EVENT THAT HAS MORE THAN ONE ACTIVITY GROUP
  !             ASSOCIATED WITH IT.
  !             (1,.)= POINTS TO THE ENTRY IN IEVACT, AGLSET, & LENAGL
  !             (2,.)= POINTS TO THE ENTRY IN IEVNTS

  !     IEVLST= A LIST OF EVENTS THAT OCCURRED THIS CYCLE.
  !             (1,.)= THE EVENT NUMBER (A ROW IN IEVNTS)
  !             (2,.)= FIRST ACTIVITY GROUP (A ROW IN IEVACT)
  !             (3,.)= NUMBER OF ACTIVITY GROUPS (ROWS IN IEVACT)

  ! Variables
  integer :: IPPCL,IPH,MXPASS,MAELNK(2,MAXEVT),IEVLST(3,MAXEVT)
  integer :: IALNK,NGRPS,I,J1,J2,J,IDEL,INOCC,JEVNT,II,NSCHDS
  integer :: KODE,LENWRK,ISCHDS,NOCCRD,IEN,IRC,MAE,IPASS
  parameter (MXPASS = 50)
  logical :: LDEB,LDEB2

  !     SEE IF WE NEED WRITE DEBUG.

  call DBCHK (LDEB,'EVMON',5,ICYC)

  IPHASE = IPH

  !     WRITE DEBUG OUTPUT, IF DESIRED.

  if (LDEB) write (JOSTND,5) IEVT,IPHASE,ICYC
  5 format (/' IN EVMON, IEVT=',I5,'; IPHASE=',I3,'; ICYC=',I3)
  call DBCHK (LDEB2,'ALGEVL',6,ICYC)

  !     LOAD TEST VARIABLE TABLES

  if (LEVUSE) call EVTSTV (0)
  MAE = 0

  !     IF NO EVENT MONITORING IS REQUESTED; THEN: RETURN.

  if (IEVT.le.1) goto 300

  !     IF THIS IS A PHASE 2 CALL OR IF FIRST CYCLE; THEN: SKIP EVENT
  !     AGEING.

  if (.not.(IPHASE.gt.1 .or. ICYC.eq.1)) call EVAGE (IY(ICYC))

  !     LOOP THRU ALL OF THE EVENTS AND FIND OUT WHICH ONES OCCURRED.
  !     LET NOCCRD BE THE NUMBER OF EVENTS THAT OCCURRED.

  IPASS = 0
  6 continue
  NOCCRD = 0
  do IEN = 1,(IEVT-1)

     !     IF THE EVENT STATUS CODE INDICATES THAT THE EVENT HAS ALREADY
     !     OCCURRED, THEN SKIP THIS EVENT.

     if (IEVNTS(IEN,2).gt.-1) goto 20

     !     EVALUATE THE EXPRESSION.

     call ALGEVL (LREG,MXLREG,XREG,MXXREG,IEVCOD(IEVNTS(IEN,1)), &
          & MAXCOD-IEVNTS(IEN,1)+1,IY(1),IY(ICYC),LDEB2,JOSTND,IRC)

     if (IRC.gt.1) call ERRGRO (.true.,21)
     if (LDEB) write (JOSTND,10) IEN,IEVNTS(IEN,1),LREG(1),IRC
     10 format (' IN EVMON: IEN=',I3,' IEVNTS(IEN,1)=',I5, &
          & ' LREG(1),IRC=',L2,I4)

     !     IF THE EVENT HAPPENED, LREG(1) BE TRUE AND THE RETURN CODE
     !     WILL BE ZERO.

     if (LREG(1) .and. IRC.eq.0) then

        !        CALL EVPOST TO POST THE EVENT AND COUNT THE NUMBER OF
        !        ACTIVITY GROUPS THAT FOLLOW THE EVENT.

        call EVPOST (JOSTND,IEN,IY(ICYC),NGRPS,IALNK)

        !        BUILD AN ENTRY IN IEVLST

        NOCCRD = NOCCRD+1
        IEVLST(1,NOCCRD) = IEN
        IEVLST(2,NOCCRD) = IALNK
        IEVLST(3,NOCCRD) = NGRPS

        !        WRITE DEBUG, IF REQUESTED.

        if (LDEB) write (JOSTND,15) NOCCRD,IEN,IALNK,NGRPS
        15 format (' IN EVMON: NOCCRD,IEN,IALNK,NGRPS=',4I5)
     end if
     20 continue
  end do
  if (LDEB) write (JOSTND,'(/'' IN EVMON: NOCCRD='',I5)') NOCCRD

  !     IF NO EVENTS OCCURRED, THEN BRANCH TO EXIT.

  if (NOCCRD.eq.0) goto 300

  !     COUNT UP THE NUMBER OF ACTIVITY GROUPS THAT FOLLOW EVENTS THAT
  !     HAVE MORE THAN ONE ACTIVITY GROUP ASSOCIATED WITH THEM.

  do I = 1,NOCCRD
     if (IEVLST(3,I).gt.1) then
        J1 = IEVLST(2,I)
        J2 = IEVLST(3,I)+J1-1
        do J = J1,J2
           MAE = MAE+1
           MAELNK(1,MAE) = J
           MAELNK(2,MAE) = IEVLST(1,I)
           30 continue
        end do
     end if
     40 continue
  end do

  !     WRITE SOME DEBUG.

  if (LDEB) write (JOSTND,'(/'' IN EVMON: NOCCRD,MAE= '',2I5)') &
       & NOCCRD,MAE

  !     IF THERE ARE NONE, THEN BRANCH PAST LOGIC THAT ATTEMPTS TO
  !     DELETE THEM.

  if (MAE.le.0) goto 100

  !     WRITE SOME MORE DEBUG.

  if (LDEB) write (JOSTND,50) (I,MAELNK(1,I),MAELNK(2,I),I = 1,MAE)
  50 format (' IN EVMON(50): MAELNK(1&2,',I3,')= ',2I5)

  !     IF LABEL PROCESSING IS BEING USED, TRIM THE LIST USING THE
  !     LABEL LOGIC.

  if (LBSETS) then
     if (LDEB) write (JOSTND,60)  LENSLS,SLSET(1:LENSLS), &
          & (I,MAELNK(1,I),MAELNK(2,I),AGLSET(MAELNK(1,I)) &
          & (1:LENAGL(MAELNK(1,I))),I = 1,MAE)
     60 format (' IN EVMON, BEFORE LBTRIM, LENSLS=',I4,' SLSET= '/T12, &
          & A/((T12,'MAELNK(1:2,',I2,')= ',2I5,' AGLSET='/,T12,A)))
     call LBTRIM (MAE,MAELNK)

     !        IF ALL OF THE ACTIVITY GROUPS HAVE BEEN DELETED, THEN
     !        BRANCH PAST THE OPSAME CHECK.

     if (MAE.le.0) goto 100
     if (LDEB) write (JOSTND,70) (I,MAELNK(1,I), &
          & MAELNK(2,I),I = 1,MAE)
     70 format (' IN EVMON(70): MAELNK(1:2,',I2,')= ',2I5)
  end if

  !     TRIM THE LIST FURTHER BY DELETING DUPLICATE GROUPS OF ACTIVITIES

  call OPSAME (MAE,MAELNK,JOSTND,LDEB)

  100 continue

  !     IF IPPCL IS EQUAL TO 2,
  !     THEN GO THROUGH THE LIST OF EVENTS THAT HAVE MORE THAN
  !     ONE ACTIVITY GROUP.  IF THE EVENT IS NO LONGER LISTED
  !     IN MAELNK, THEN DELETE THE ENTRY IN IEVLST. IF THE EVENT
  !     IS IN MAELNK, SET THE ENTRY IN IEVLST TO THE FIRST
  !     ACTIVITY GROUP AND SET THE NUMBER OF GROUPS TO ONE.

  if (IPPCL.eq.2) then
     IDEL = 0
     do INOCC = 1,NOCCRD
        if (IEVLST(3,INOCC).eq.1) goto 140
        JEVNT = IEVLST(1,INOCC)
        J1 = 0
        if (MAE.eq.0) goto 130
        do II = 1,MAE
           if (MAELNK(2,INOCC).eq.JEVNT) then
              J1 = II
              goto 130
           end if
           120 continue
        end do
        130 continue
        if (J1.eq.0) then
           IDEL = IDEL+1
           IEVLST(1,INOCC) = 0
        else

           !           SET THE ACTIVITY CODE NEGITIVE THEREBY SIGNALING THAT
           !           LABEL PROCESSING, ETC, FOR THIS OPTION IS DONE.

           IEVLST(2,INOCC) = -MAELNK(1,J1)
           IEVLST(3,INOCC) = 1
        end if
        140 continue
     end do

     !        DELETE ENTRIES IN IEVLST, IF NECESSARY

     if (IDEL.gt.0) then
        if (IDEL.eq.NOCCRD) then
           NOCCRD = 0
        else
           IDEL = NOCCRD+1
           do INOCC = 1,NOCCRD
              if (IEVLST(1,INOCC).eq.0) then
                 IDEL = IDEL-1
                 do J1 = 1,3
                    IEVLST(J1,INOCC) = IEVLST(J1,IDEL)
                    150 continue
                 end do
              end if
              160 continue
           end do
           NOCCRD = IDEL
        end if
     end if
  end if

  !     INITIALIZE THE NUMBER OF ACTIVITY GROUPS THAT HAVE BEEN
  !     SCHEDULED AND LOOP THRU ALL OF THE EVENTS THAT OCCURRED.

  NSCHDS = 0
  if (NOCCRD.eq.0) goto 210
  do INOCC = 1,NOCCRD
     if (LDEB) write (JOSTND,170) INOCC,(IEVLST(I,INOCC),I = 1,3)
     170 format (' IN EVMON: IEVLST(1:3,',I3,')= ',3I4)

     !     IF THE ACTIVITY GROUP NUMBER IN IEVLST IS NEG, SET IT POSITIVE
     !     AND BRANCH TO SCHEDULE ACTIVITIES

     if (IEVLST(2,INOCC).lt.0) then
        IEVLST(2,INOCC) = -IEVLST(2,INOCC)
        IALNK = IEVLST(2,INOCC)
        goto 190
     end if
     IALNK = IEVLST(2,INOCC)

     !     IF LABEL PROCESSING IS TURNED ON, THEN PROCESS LABELS TO
     !     DETERMINE IF THE ACTIVITIES WILL BE SCHEDULED.

     if (LBSETS) then
        IF (LDEB) WRITE (JOSTND,'(/'' IN EVMON: LENSLS='',I5,          &
             &                    '' SLSET= '',A)') LENSLS,SLSET(:LENSLS)

        !        IF THE ACTIVITY GROUP LABEL WAS SET (LENAGL>-1), THEN
        !        COMPUTE THE INTERSECTION OF THE ACTIVITY GROUP LABEL AND THE
        !        STAND LABEL.

        if (LENAGL(IALNK).gt.-1) then
           if (LDEB) write (JOSTND,180) IALNK, &
                & AGLSET(IALNK)(1:LENAGL(IALNK))
           180 format (' IN EVMON: IALNK=',I3,' AGLSET=',A)
           call LBINTR (LENAGL(IALNK),AGLSET(IALNK),LENSLS,SLSET, &
                & LENWRK,WKSTR1,KODE)

           !           IF THE RESULT IS THE NULL SET, DO NOT SCHEDULE THE
           !           ACTIVITY GROUP.

           if (LENWRK.eq.0) goto 200
        end if
     end if
     190 continue

     !     SCHEDULE THE ACTIVITY GROUP.

     call OPSCHD (IY(ICYC),IEVACT(IALNK,4),IEVACT(IALNK,5), &
          & ISCHDS,KODE)
     if (LDEB) write (JOSTND,195) ICYC,IY(ICYC),IALNK,IEVACT(IALNK,4), &
          & IEVACT(IALNK,5),ISCHDS,KODE
     195 format (/' IN EVMON: ICYC=',I3,' IY(ICYC)=',I5,' IALNK=',I5, &
          & ' IEVACT(IALNK,4:5)=',2I5,' ISCHDS=',I4,' KODE=',I3)

     if (KODE.ge.2) then

        !        FLAG THE ACTIVITY GROUP THAT AN ATTEMPT TO SCHEDULE HAS
        !        FAILED, AND DELETE THE EVENT.

        IEVACT(IALNK,2) = -1
        IEVNTS(IEVLST(1,INOCC),3) = -1
        call ERRGRO (.true.,18)
        goto 210
     end if

     !     SET THE SCHEDULING CODE FOR THIS ACTIVITY GROUP

     IEVACT(IALNK,2) = 2

     !     COUNT THE NUMBER OF ALTERNATIVES ACTUALLY SCHEDULED.

     NSCHDS = NSCHDS+ISCHDS
     200 continue
  end do
  210 continue
  if (LDEB) write (JOSTND,'(/'' IN EVMON: NSCHDS='',I5)') NSCHDS

  !     IF ONE OR MORE ACTIVITIES WERE SCHEDULED, INCORPORATE THEM INTO
  !     THE ACTIVITY ARRAYS AND RECALL EVTSTV TO RECOMPUTE USER DEFINED
  !     VARIABLES.

  if (NSCHDS.gt.0) then
     call OPINCR (IY,ICYC,NCYC)
     call EVTSTV (1)
     IPASS = IPASS+1
     if (LDEB) write (JOSTND,'('' IN EVMON: IPASS='',I5)') IPASS
     if (IPASS.le.MXPASS) goto 6
  end if

  300 continue

  !     IF CCADJ IS CONDITIONALLY SCHEDULED, CALL SSTAGE FROM HERE

  !     Commented out as this causes replicate records written to
  !     associated DB output table, FVS_StrClass table. LD - 2/21/2023
  !      CCCT=0
  !      DO ACTF=1,MAXACT
  !        IF((IACT(ACTF,1).EQ.444))THEN
  !          CCCT=CCCT+1
  !        ENDIF
  !      END DO
  !      IF(CCCT.GE.1)CALL SSTAGE(1,ICYC,.TRUE.)

  return
end subroutine EVMON
