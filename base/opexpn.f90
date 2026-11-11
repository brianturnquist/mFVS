!===============================!
!   Expand activity schedules   !
!===============================!
! BASE $Id$
subroutine OPEXPN (IOUT,NCYC,IY)
  implicit none

  !     OPTION PROCESSING ROUTINE - NL CROOKSTON - JAN 1981 - MOSCOW
  !     FROM 1991 TO 2003 THERE WERE NO CHANGES TO THIS ROUTINE!
  !     IN MAY 2003 CROOKSTON ADDED PROVISIONS FOR CHARACTER ARGUMENTS

  !     EXPANDS THE ACTIVITY LIST SUCH THAT ACTIVITIES SPECIFIED FOR
  !     ALL CYCLES ARE DUPLICATED ONCE FOR EACH CYCLE.  AN ACTIVITY
  !     SPECIFIED FOR ALL CYCLES WILL NOT BE ADDED TO A CYCLE WHICH
  !     ALREADY CONTAINS THAT ACTIVITY.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'OPCOM.f90'

  ! Variables
  integer :: IY(MAXCY1),NCYC,IOUT,I,IY1,IMG3,IPSYR,ISYR,IC,J,IP,K
  integer :: IY2,IP1,IPS,IPY,L

  !     STEP01: CONVERT ANY CYCLE NUMBERS IN THE DATE ARRAY TO DATES,
  !     THEN SORT IOPSRT ON IDATE AND ISEQ.

  IMG2 = IMGL-1

  !     IF THERE ARE NO ACTIVITIES;
  !     THEN: THE JOB IS DONE--RETURN.

  if (IMG2 .le. 0) return
  do I = 1,IMG2
     IY1 = IDATE(I)
     if (IY1.le.MAXCYC .and. IY1.gt.0) then
        if (IY(IY1).gt.0) IY1 = IY(IY1)
     end if
     IDATE(I) = IY1
     ISEQ(I) = I
     10 continue
  end do
  ISEQDN = IMGL
  call OPSORT(IMG2,IDATE,ISEQ,IOPSRT,.true.)

  !     STEP02: FIND THE STARTING YEAR (ISYR) IN THE
  !     ACTIVITY DATE ARRAY (IDATE), AND:
  !     SET IPSYR=1 IF THERE ARE NO 'ALL-CYCLE' ACTIVITIES.
  !     SET IPSYR=0 IF ALL ACTIVITIES ARE 'ALL-CYCLE' ACTIVITIES.
  !     SET IPSYR>1 TO EXPAND THE 'ALL-CYCLE' ACTIVITIES STORED IN
  !         IACT((IOPSRT(J),J=1,IPSYR-1),*).

  do I = 1,IMG2
     IMG3 = IOPSRT(I)
     if (IDATE(IMG3).le.0) goto 20
     IPSYR = I
     ISYR = IDATE(IMG3)
     goto 30
     20 continue
  end do
  IPSYR = 0
  30 continue

  !     IF IPSYR=1, THERE ARE NO 'ALL-CYCLE' ACTIVITIES;
  !     THEN: PROCESSING IS COMPLETE, RETURN.

  if (IPSYR .eq. 1) return

  !     ELSE:
  !     STEP03: EXPAND THE 'ALL-CYCLE' ACTIVITIES.

  !     IF IPSYR>1 SOME SPICIFICLY DATED AND SOME 'ALL-CYCLE'
  !     ACTIVITIES EXIST. THEN: BRANCH TO SPECIAL CODE.

  if (IPSYR .gt. 1) goto 70

  !     ELSE: ALL ACTIVITIES MUST BE ASSIGNED TO ALL CYCLES.

  !     ASSIGN THE FIRST DATE TO THE SPECIFIED ACTIVITIES.

  ISYR = IY(1)
  do I = 1,IMG2
     IDATE(I) = ISYR
     40 continue
  end do

  !     IF THE NUMBER OF CYCLES = 1;
  !     THEN: WE'ER DONE; RETURN

  if (NCYC .eq. 1) return

  !     ELSE: COPY THE SPECIFIED ACTIVITIES TO THE BOTTOM OF THE
  !     ACTIVITY ARRAY AND ASSIGN THE NEXT CYCLE STARTING DATE
  !     TO THE DATE ARRAY (IDATE). DO THIS FOR ALL CYCLES.

  !     MAKE SURE ALL OF THE ACTIVITIES WILL FIT.

  IC = MAXACT/IMG2

  if (IC.ge.NCYC) goto 50
  write (IOUT,6010) NCYC,IC,IY(IC+1)
  6010 format (/1X,I3,' CYCLES REQUESTED, ',I3,' CYCLES POSSIBLE.  ', &
       & 'PROJECTION WILL END IN ',I4)
  call ERRGRO (.true.,10)
  write (IOUT,*) 'ISSUED IN OPEXPN: IC, NCYC, MAXACT, IMG2 =', &
       & IC, NCYC, MAXACT, IMG2
  NCYC = IC
  50 continue

  !     COPY THE ACTIVITIES.

  do IC = 2,NCYC
     ISYR = IY(IC)
     J = (IC-1)*IMG2
     do IP = 1,IMG2
        IMG3 = J+IP
        IDATE(IMG3) = ISYR
        ISEQ(IMG3) = IMG3
        IOPSRT(IMG3) = IMG3
        do K = 1,5
           IACT(IMG3,K) = IACT(IP,K)
        end do

        !     ONLY YEAR-0 ACTIVITIES REACH THIS LOOP (SEE STEP02).
        !     AN ACTIVITY SCHEDULED FOR YEAR 0 (EVERY CYCLE) IS COPIED INTO
        !     EACH CYCLE.  THE COPIES REUSE THE ORIGINAL'S PARAMETER VALUES IN
        !     PARMS INSTEAD OF EACH STORING THEIR OWN, SO PARMS DOES NOT FILL
        !     UP ON LONG PROJECTIONS.  THIS IS SAFE BECAUSE STORED PARAMETERS
        !     ARE NEVER CHANGED.  COMPUTE (33) IS THE EXCEPTION: IT SAVES EACH
        !     CYCLE'S RESULT IN ITS OWN PARAMETER, SO EACH COPY GETS ITS OWN.

        if (IACT(IMG3,2).le.0 .or. IACT(IMG3,1).ne.33) goto 65
        do I = IACT(IMG3,2),IACT(IMG3,3)
           PARMS(IMPL) = PARMS(I)
           IMPL = IMPL+1
           if (IMPL.gt.ITOPRM) goto 200
           64 continue
        end do
        IACT(IMG3,2) = IMPL-(IACT(IMG3,3)-IACT(IMG3,2)+1)
        IACT(IMG3,3) = IMPL-1
        65 continue
     end do
     67 continue
  end do
  IMG2 = IMG3
  IMGL = IMG2+1

  return
  70 continue

  !     STEP04:  ADD 'ALL-CYCLE' ACTIVITIES.

  IMG1 = IPSYR
  IPSYR = IPSYR-1
  IMG3 = IMG2
  K = IMG1

  !     DO FOR ALL CYCLES.

  do IC = 1,NCYC

     !     FIND THE BEGINNING AND END OF THE CYCLE WITHIN THE ACTIVITY
     !     ARRAY. NOTE THAT NO ACTIVITIES MAY YET EXIST FOR THE CYCLE.

     IY1 = IY(IC)
     IY2 = IY(IC+1)
     IP1 = 0
     do IP = K,IMG2
        IPS = IOPSRT(IP)
        IPY = IDATE(IPS)
        if (IPY .lt. IY1 .or. IPY .ge. IY2) goto 90
        if (IP1 .eq. 0) IP1 = IP
        90 continue
        if (IPY .lt. IY2) goto 100
        J = IP
        goto 105
        100 continue
     end do
     goto 110
     105 continue
     K = J
     110 continue

     !     COPY THE ACTIVITIES.

     ISYR = IY1

     !     DO FOR ALL 'ALL-CYCLE' ACTIVITIES.

     do I = 1,IPSYR
        J = IOPSRT(I)

        !     IF THERE IS NOT ENOUGH ROOM; THEN: BRANCH TO SPECIAL CODE.

        IMG3 = IMG3+1
        if (IMG3 .gt. MAXACT) goto 200

        !     ELSE: ADD THE ACTIVITY

        IDATE(IMG3) = ISYR
        ISEQ(IMG3) = IMG3
        IOPSRT(IMG3) = IMG3
        do L = 1,5
           IACT(IMG3,L) = IACT(J,L)
           150 continue
        end do

        !     YEAR-0 COPIES REUSE THE ORIGINAL'S PARAMETERS, EXCEPT COMPUTE
        !     (SEE STEP03 ABOVE).  BRANCH TO SPECIAL CODE IF PARMS IS FULL.

        if (IACT(IMG3,2).le.0 .or. IACT(IMG3,1).ne.33) goto 160
        do IP = IACT(IMG3,2),IACT(IMG3,3)
           PARMS(IMPL) = PARMS(IP)
           IMPL = IMPL+1
           if (IMPL.gt.ITOPRM) goto 200
           155 continue
        end do
        IACT(IMG3,2) = IMPL-(IACT(IMG3,3)-IACT(IMG3,2)+1)
        IACT(IMG3,3) = IMPL-1
        160 continue
     end do
     170 continue
  end do
  goto 300
  200 continue

  !     SPECIAL CASE: OUT OF ROOM IN THE ACTIVITY ARRAYS.
  !     BACK UP TO THE LAST COMPLETED CYCLE. SET THE NCYC VALUE
  !     EQUAL TO THAT CYCLE, WRITE A MESSAGE AND GO ON.  THIS
  !     SHOULD BE A RATHER RARE EVENT, THEREFORE VERY LITTLE CODE
  !     HAS BEEN WRITTEN TO TAKE MAX ADVANTAGE OF THE SITUATION.
  !     (IF IT HAPPENS OFTEN, THE SOLUTION IS TO INCREASE THE SIZE OF
  !     THE ACTIVITY ARRAYS.)

  IC = IC-1
  IMG3 = IMG3-1
  write (IOUT,6010) NCYC,IC,IY(IC+1)
  call ERRGRO (.true.,10)
  write (IOUT,*) 'ISSUED IN OPEXPN: IMG3 ', IMG3, &
       & ' HAS EXCEEDED MAXACT ', MAXACT, &
       & ' OR IMPL ',IMPL,'HAS EXCEEDED ITOPRM ',ITOPRM
  NCYC = IC
  300 continue

  !     STEP05: MOVE THE ACTIVITIES IN THE BOTTOM OF THE LIST UP TO
  !     THE TOP, OVERWRITTING THE 'ALL-CLCLE' ACTIVITIES.

  IMG2 = IMG3
  ISEQDN = IMG2+1
  do J = 1,IPSYR
     I = IOPSRT(J)
     do K = 1,5
        IACT(I,K) = IACT(IMG2,K)
     end do
     IDATE(I) = IDATE(IMG2)
     ISEQ(I) = ISEQ(IMG2)
     IMG2 = IMG2-1
     330 continue
  end do

  !     STEP06: REESTABLISH THE DATE SORT.

  call OPSORT(IMG2,IDATE,ISEQ,IOPSRT,.false.)
  IMGL = IMG2+1
  return
end subroutine OPEXPN
