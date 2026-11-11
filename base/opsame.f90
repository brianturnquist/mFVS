!============================!
!   Compare activity lists   !
!============================!
! BASE $Id$
subroutine OPSAME (MAE,MAELNK,IOUT,LDEB)
  implicit none

  !     CALLED FROM EVMON, DELETE ACTIVITY GROUPS THAT MIGHT CREATE
  !     BRANCHES THAT HAVE IDENTICAL ACTIVITIES.

  !     EVENT MONITOR ROUTINE..PART OF THE PROGNOSIS SYSTEM
  !     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JAN 1987.

  !     MAE   = LENGTH OF MAELNK.
  !     MAELNK= LIST OF ACTIVITY GROUPS THAT ARE ASSOCIATED WITH
  !             AN EVENT THAT HAS MORE THAN ONE ACTIVITY GROUP
  !             ASSOCIATED WITH IT.
  !             (1,.)= POINTS TO THE ENTRY IN IEVACT, AGLSET, & LENAGL
  !             (2,.)= POINTS TO THE ENTRY IN IEVNTS

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  integer :: IOUT,MAE,IDEL,I1,J1,MF1,MF2,NMF,II,I2,J2,MS1,MS2,NMS
  integer :: IA,IPF1,IPF2,IPS1,IPS2,NPF,NPS,IPF,LNUNIN,I,KODE
  integer :: MAELNK(2,50)
  logical :: LDEB

  !     IF THE LENGTH OF MAELNK IS LE ONE, BRANCH TO RETURN

  if (MAE.le.1) goto 100

  !     DO THROUGH THE ACTIVITY GROUPS, AND FLAG MEMBERS OF MEMLNK
  !     FOR ACTIVITY GROUPS THAT SHOULD BE DROPPED.

  IDEL = 0
  do I1 = 1,(MAE-1)
     J1 = MAELNK(1,I1)
     if (LDEB) write (IOUT,'('' IN OPSAME: I1,J1='',2I4)') I1,J1
     if (J1.eq.0) goto 50

     !     IF THE BRANCH WEIGHTING FACTOR MAY BE CHANGED, DO NOT DELETE.

     if (IEVACT(J1,6).ne.0) goto 50

     !     MF1 POINTS TO THE FIRST ACTIVITY IN THE ACTIVITY GROUP AND MF2
     !     POINTS TO THE SECOND.  NMF IS THE NUMBER OF ACTIVITIES IN THE
     !     GROUP.  MF1 IS LARGER THAN MF2 BECAUSE ACTIVITIES THAT DEPEND ON
     !     EXPRESSIONS ARE STORED IN IACT FROM THE BOTTOM TO THE TOP.

     MF1 = IEVACT(J1,4)
     MF2 = IEVACT(J1,5)
     NMF = MF1-MF2+1
     II = I1+1

     do I2 = II,MAE
        J2 = MAELNK(1,I2)
        if (LDEB) write (IOUT,'('' IN OPSAME: I2,J2='',2I4)') I2,J2
        if (J2.eq.0) goto 40

        !     IF THE BRANCH WEIGHTING FACTOR MAY BE CHANGED, DO NOT DELETE.

        if (IEVACT(J2,6).ne.0) goto 40
        MS1 = IEVACT(J2,4)

        !     IF BOTH GROUPS ARE EMPTY SETS, THEY ARE CONSIDERED EQUAL.

        if (MF1.eq.0 .and. MS1.eq.0) goto 35
        MS2 = IEVACT(J2,5)
        NMS = MS1-MS2+1

        !     IF THE NUMBER OF ACTIVITIES IN THE GROUP DIFFERS, THE GROUP IS
        !     UNIQUE.  BRANCH TO THE NEXT GROUP.

        if (NMF.ne.NMS) goto 40

        !     GO THROUGH THE ACTIVITIES ONE-BY-ONE.  IF AN ACTIVITY CODE
        !     OR A PARAMETER DIFFERS, THE GROUP IS UNIQUE.  IF THE ORDER
        !     OF THE ACTIVITIES DIFFERS, THE GROUP IS ALSO UNIQUE.

        MF1 = MF1+1
        MS1 = MS1+1
        do IA = 1,NMF
           MF1 = MF1-1
           MS1 = MS1-1
           if (IACT(MF1,1).ne.IACT(MS1,1)) goto 40
           IPF1 = IACT(MF1,2)
           if (IPF1.lt.0) goto 40
           IPF2 = IACT(MF1,3)
           IPS1 = IACT(MS1,2)
           if (IPS1.lt.0) goto 40
           IPS2 = IACT(MS1,3)
           NPF = IPF2-IPF1+1
           NPS = IPS2-IPS1+1
           if (NPF.ne.NPS) goto 40
           if (NPF.eq.0) goto 30
           IPS1 = IPS1-1
           do IPF = IPF1,IPF2
              IPS1 = IPS1+1
              if (PARMS(IPF).ne.PARMS(IPS1)) goto 40
              20 continue
           end do
           30 continue
        end do
        35 continue
        if (LDEB) write (IOUT,'('' IN OPSAME: LBSETS='',L2)') LBSETS

        !     ALL OF THE ACTIVITIES MATCH IN ORDER AND PARAMETERS.  THE
        !     ACTIVITY GROUP MAY BE DELETED.  IF LABEL PROCESSING IS BEING
        !     DONE, COMBINE THE ACTIVITY GROUP LABEL SET FOR THE DELETED
        !     GROUP WITH THE SET USED FOR THE KEPT ONE.

        if (LBSETS) then
           if (LDEB) then
              write (IOUT,'('' IN OPSAME: AGLSET(J1)='',A)') AGLSET(J1) &
                   & (1:LENAGL(J1))
              write (IOUT,'('' IN OPSAME: AGLSET(J2)='',A)') AGLSET(J2) &
                   & (1:LENAGL(J2))
           end if
           call LBUNIN (LENAGL(J1),AGLSET(J1),LENAGL(J2),AGLSET(J2), &
                & LNUNIN,WKSTR1,KODE)

           !        IF THE UNION IS TOO LONG, (KODE=1) THEN DO NOT DELETE THE
           !        ACTIVITY GROUP, SIMPLY CARRY IT ALONG TO AVOID AN INCORRECT
           !        ACTIVITY GROUP LABEL.

           if (KODE.gt.0) goto 40

           !        SET THE ACTIVITY GROUP LABEL FOR GROUP J1 EQUAL TO THE UNION

           LENAGL(J1) = LNUNIN
           AGLSET(J1) = WKSTR1
        end if

        !     DELETE ACTIVITY GROUP J2

        MAELNK(1,I2) = 0
        IDEL = IDEL+1
        if (LDEB) write (IOUT,'('' IN OPSAME: AGLSET(J1)='',A)') &
             & AGLSET(J1)(1:LENAGL(J1))
        if (LDEB) write (IOUT,'('' IN OPSAME: KODE,IDEL='',2I4)') &
             & KODE,IDEL
        40 continue
     end do
     50 continue
  end do

  !     IF NO ACTIVITY GROUPS ARE DELETED, RETURN.

  if (IDEL.eq.0) goto 100

  !     IF ALL OF THE ACTIVIY GROUPS HAVE BEEN DELETED, RETURN.

  if (IDEL.eq.MAE) then
     MAE = 0
     goto 100
  end if

  !     IF SOME HAVE BEEN DELETED, COMPRESS THE LIST MAELNK.

  IDEL = 0
  do I = 1,MAE
     if (MAELNK(1,I).gt.0) then
        IDEL = IDEL+1
        MAELNK(1,IDEL) = MAELNK(1,I)
        MAELNK(2,IDEL) = MAELNK(2,I)
     end if
     60 continue
  end do
  MAE = IDEL

  100 continue
  return
end subroutine OPSAME
