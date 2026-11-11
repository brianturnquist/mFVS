!=====================================!
!   Create default stand label sets   !
!=====================================!
! BASE $Id$
subroutine LBDSET (JOSTND,LKECHO)
  implicit none

  !     CREATE DEFAULT A LABEL SET FOR THE STAND AND WRITE IT TO THE
  !     OPTIONS SELECTED BY DEFAULT TABLE.  ASSIGN THE STAND LABEL SET
  !     TO ALL ACTIVITY GROUPS FOR WHICH NO ACTIVITY GROUP LABEL SET
  !     HAS BEEN SPECIFIED.

  !     PART OF THE LABEL PROCESSING COMPONENT OF THE PROGNOSIS SYSTEM
  !     N.L. CROOKSTON -- INTERMOUNTAIN RESEARCH STATION -- JAN 1987

  !     JOSTND= PRINT MESSAGE FILE.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  logical :: LKECHO
  integer :: JOSTND,NGRPS,I,KODE,LWRK,I1,I2,J

  !     IF LABEL PROCESSING HAS NOT BEEN ACTIVIATED, BRANCH TO EXIT.

  if (.not.LBSETS) goto 80

  !     IF NO STAND LABEL SET KEYWORD HAS BEEN READ (LENSLS=-1) THEN
  !     CREATE THE DEFAULT STAND LABEL SET BY TAKING THE UNION OVER
  !     ALL OF THE ACTIVITY GROUP LABELS.

  NGRPS = IEVA-1
  if (LENSLS.eq.-1) then
     if (NGRPS.gt.0) then
        do I = 1,NGRPS
           call LBUNIN (LENSLS,SLSET,LENAGL(I),AGLSET(I),LWRK,WKSTR1, &
                & KODE)
           if (KODE.gt.0) then
              write (JOSTND,10) AGLSET(I)(1:LENAGL(I))
              10 format (/'********   ERROR:  DEFAULT STAND LABEL ', &
                   & 'SET IS TOO LONG.  THE FOLLOWING ACTIVITY GROUP ', &
                   & 'IS NOT INCLUDED.'/'ACTIVITY LABEL:  ',A)
              call RCDSET (2,.true.)
           end if
           SLSET = WKSTR1
           LENSLS = LWRK
           20 continue
        end do

        !           IF THE STAND LABEL SET IS NOW EMPTY, TURN OFF LABEL
        !           PROCESSING.

        if (SLSET.eq.' ') then
           LBSETS = .false.
           LENSLS = -1
           goto 80
        end if
     end if

     !        WRITE THE DEFAULT STAND LABEL SET.

     if (LKECHO)write(JOSTND,30)
     30 format (/'SPLABEL',T12,'STAND POLICY LABEL SET: ')
     I1 = 1
     I2 = 100
     40 continue
     if (I2.gt.LENSLS) I2 = LENSLS
     if (LKECHO)write(JOSTND,'(T12,A)') SLSET(I1:I2)
     if (I2.lt.LENSLS) then
        I1 = I2+1
        I2 = I2+100
        goto 40
     end if
  end if

  !     PASS THROUGH THE ACTIVITIES.  IF ANY ACTIVITY GROUP HAS NO
  !     LABEL, GIVE IT THE STAND LABEL SET AS A DEFAULT.

  J = 0
  do I = 1,NGRPS
     if (LENAGL(I).le.0) then
        J = J+1
        LENAGL(I) = LENSLS
        AGLSET(I) = SLSET
     end if
     50 continue
  end do
  if (J.gt.0) write (JOSTND,60) J
  60 format (/'********   WARNING: ',I2,' ACTIVITY GROUP(S) HAD ', &
       & 'NO LABEL AND WERE ASSIGNED THE STAND POLICY ', &
       & 'LABEL SET.')
  80 continue
  return
end subroutine LBDSET
