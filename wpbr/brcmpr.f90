!===================!
!   wpbr : brcmpr   !
!===================!
! WPBR $Id$
subroutine BRCMPR (NCLAS,PROB2,INDX,INDX1)
  implicit none

  !  Purpose:
  !  This Blister Rust Model subroutine is called from the FVS routine
  !  COMPRS.  It compresses the blister rust model variables.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.

  !.... Common include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BRCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'

  !.... Local variable declarations.

  ! Variables
  integer :: BOLCNT,BOLAVE,BRACNT,BRAAVE,ITRCNT,NCLAS,INDX(MAXTRE),INDX1(MAXTRE),I,I1,I2,I3,I4,ICL,IREC,J,K
  real    :: TXP2,TXP3,TXP4,TXP5,TXP6,TXP7,TXP8,TXP9,TXP10,TXP12(10),TXP13(10),TXP14(10),TXP15(10),&
       &TXP16,PROB2(MAXTRE),GIRD,OUT,TXP,UP,XP
  logical :: LGO,DEBUG

  !.... See if we need to do some debug.

  call DBCHK(DEBUG,'BRCMPR',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format('Entering subroutine BRCMPR: cycle = ', I2)

  !.... If Blister Rust not active, exit subroutine.

  call BRATV (LGO)
  if (.not.LGO) go to 550

  !.... Compress the blister rust tree records.
  !.... Set flag, BRCMP, so statuses for the compressed cankers will
  !.... be assigned by calling subroutine BRCSTA from subroutine BRPR.

  BRCMP = .true.
  I1 = 1

  do ICL = 1,NCLAS

     !....    Set the pointers to the trees in the class.

     I2 = INDX1(ICL)

     !....    If there is only one record in clase ICL, then skip the class.

     IREC1 = INDX(I1)
     if (I1.eq.I2) go to 300

     !....    Create a cumulative PROB2 for all the tree in the class.
     !....    Set tree record, bole canker, and branch canker counters for
     !....    variables not compressed by a weighted average.

     ITRCNT = 1
     I3 = 0
     I4 = 0
     BRACNT = 0
     BOLCNT = 0
     XP = PROB2(IREC1)
     TXP = XP
     K = I1+1
     TXP2 = UPMARK(IREC1)
     TXP3 = BRHTBC(IREC1)
     TXP4 = GI(IREC1)*XP
     TXP5 = RI(IREC1)*XP
     TXP6 = BRGD(IREC1)*XP
     TXP7 = ESTCAN(IREC1)*XP
     TXP8 = TSTARG(IREC1)*XP
     TXP9 = BRAGE(IREC1)
     TXP10 = BRPB(IREC1)
     TXP16 = ITCAN(IREC1)*XP

     !....    Initialize arrays used to accumulate the values for bole
     !....    and branch cankers.  Inactivated cankers are not included.
     !....    Bole and branch cankers are accumulated seperately
     !....    according to their order of occurance in the canker arrays.
     !....    The first bole canker values for each tree are accumulated in
     !....    TXP12(1) and TXP13(1).  The first branch canker values for
     !....    each tree are accumulated in TXP14(1) and TXP(1).

     do J = 1,10
        TXP12(J) = 0.0
        TXP13(J) = 0.0
        TXP14(J) = 0.0
        TXP15(J) = 0.0
        UP = DUP(J,IREC1)
        OUT = DOUT(J,IREC1)
        GIRD = GIRDL(J,IREC1)

        if (ISTCAN(J,IREC1).eq.-1.or.J.gt.ILCAN(IREC1)) then
           continue
        else if (OUT.eq.0.0.and.UP.gt.0.0) then
           BOLCNT = BOLCNT+1
           I3 = I3+1
           TXP12(I3) = UP
           TXP13(I3) = GIRD
        else if (OUT.gt.0.0) then
           BRACNT = BRACNT+1
           I4 = I4+1
           TXP14(I4) = UP
           TXP15(I4) = OUT
        end if
        50 continue
     end do

     !....    Add in the other trees which form the class.

     do I = K,I2
        I3 = 0
        I4 = 0
        ITRCNT = ITRCNT+1
        IREC = INDX(I)
        XP = PROB2(IREC)
        TXP = TXP+XP
        TXP2 = TXP2+UPMARK(IREC)
        TXP3 = TXP3+BRHTBC(IREC)
        TXP4 = TXP4+GI(IREC)*XP
        TXP5 = TXP5+RI(IREC)*XP
        TXP6 = TXP6+BRGD(IREC)*XP
        TXP7 = TXP7+ESTCAN(IREC)*XP
        TXP8 = TXP8+TSTARG(IREC)*XP
        TXP9 = TXP9+BRAGE(IREC)
        TXP10 = TXP10+BRPB(IREC)
        TXP16 = TXP16+ITCAN(IREC)*XP

        do J = 1,ILCAN(IREC)
           UP = DUP(J,IREC)
           OUT = DOUT(J,IREC)
           GIRD = GIRDL(J,IREC)

           if (ISTCAN(J,IREC).eq.-1) then
              continue
           else if (OUT.eq.0.0.and.UP.gt.0.0) then
              BOLCNT = BOLCNT+1
              I3 = I3+1
              TXP12(I3) = TXP12(I3)+UP
              TXP13(I3) = TXP13(I3)+GIRD
           else if (OUT.gt.0.0) then
              BRACNT = BRACNT+1
              I4 = I4+1
              TXP14(I4) = TXP14(I4)+UP
              TXP15(I4) = TXP15(I4)+OUT
           end if
           80 continue
        end do
        90 continue
     end do

     !....    Divide by the total PROB for weighted average values.
     !....    Divide by the tree record count for average values.
     !....    Move the values into the IREC1 position in the arrays.

     UPMARK(IREC1) = TXP2/ITRCNT
     BRHTBC(IREC1) = TXP3/ITRCNT
     GI(IREC1) = TXP4/TXP
     RI(IREC1) = TXP5/TXP
     BRGD(IREC1) = TXP6/TXP
     ESTCAN(IREC1) = TXP7/TXP
     TSTARG(IREC1) = TXP8/TXP
     BRAGE(IREC1) = TXP9/ITRCNT
     BRPB(IREC1) = TXP10
     ITCAN(IREC1) = INT(TXP16/ITRCNT)
     ISTOTY(IREC1) = 5
     ICRED(IREC1) = 0

     !....    Load the cankers for the compressed tree record.
     !....    The average (not weighted average) number of cankers (ILCAN)
     !....    is carried to the compression record.
     !....    Bole cankers, if any, are stored in the first positions of the
     !....    canker arrays and branch cankers will be stored in the
     !....    following positions.

     !....    Tree's status code is set to 1 if it has any lethal cankers,
     !....    just to indicate that it is not clean.  A more accurate
     !....    status code will be assigned during the regular cycling process

     BOLAVE = INT(BOLCNT/ITRCNT)
     BRAAVE = INT(BRACNT/ITRCNT)
     ILCAN(IREC1) = BOLAVE+BRAAVE

     if (ILCAN(IREC1).gt.0) then
        IBRSTAT(IREC1) = 1
     else
        IBRSTAT(IREC1) = 0
     end if

     if (BOLAVE.gt.0) then
        do I = 1,BOLAVE
           DUP(I,IREC1) = TXP12(I)/BOLCNT
           DOUT(I,IREC1) = 0.0
           GIRDL(I,IREC1) = TXP13(I)/BOLCNT
           ISTCAN(I,IREC1) = 0
           110 continue
        end do
     end if

     if (BRAAVE.gt.0) then
        do I = 1,BRAAVE
           I4 = BOLAVE+I
           DUP(I4,IREC1) = TXP14(I)/BRACNT
           DOUT(I4,IREC1) = TXP15(I)/BRACNT
           GIRDL(I4,IREC1) = 0.0
           ISTCAN(I4,IREC1) = 0
           120 continue
        end do
     end if

     !....    End of compression loop.

     300 continue

     !....    Re-define I1 such that it points to the first
     !....    index in the next class.

     I1 = I2+1
     500 continue
  end do

  !.... Common return.

  550 continue
  if (DEBUG) write(JOSTND, 600) ICYC
  600 format('Leaving subroutine BRCMPR: cycle = ', I2)
  return
end
