!===================!
!   wwpb : bmrrsr   !
!===================!
! WWPB $Id$
subroutine BMRRSS

  !  **BMRRSS--WWPB   DATE OF LAST REVISION:  07/10/94
  !     A quick model that will provide some sensitivity to root
  !     disease and stem rust in the inventory. These should be
  !     replaced as soon as possible by a proper landscape level
  !     model for these pests.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BMRRCM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: L,LNOTBK,LTR,LTEE
  character(len=8) :: KEYWRD
  character(len=8) :: NORR
  integer*4 :: IDD
  integer :: IND,IND1,IRECN,IX1,IX2
  real    :: XP,TXP,TXP1
  real    :: X1
  dimension   ARRAY(7),LNOTBK(7),ICODES(6)
  dimension   RX1(MAXTRE)
  dimension   PROB(MAXTRE), IND(MAXTRE), IND1(MAXTRE)

  data NORR/'*NO RROT'/

  !     Keyword control is not available with this model.
  entry RRIN (KEYWRD,ARRAY,LNOTBK)
  call ERRGRO (.true.,11)
  return

  !     The model *is* running and is initialized from *inventory*.
  entry RRATV (L,LTEE)
  L = .true.
  LTEE = .true.
  return

  !     No records are required for root diseased trees in this model.
  entry RRTRES (ITN1,ITN2)
  ITN2 = MAXTP1
  return

  !     Called from INITRE. Normally initialization would happen here,
  !     for each stand. Because there is only landscape control, this
  !     is kludged in BMPPIN.

  entry RRINIT
  do I = 1, MAXTRE
     BMRR(I) = 0.0
     BMSR(I) = 0.0
     90 continue
  end do
  return

  !     Called from DAMCDS. This picks up all root disease codes.
  !     Many parameters are ignored here. Must recognize 61,62,64
  !     for root disease, and 35-39 for stem rust. Subratings are
  !     not used.

  entry RRDAM (II,IITH,IDD,ICODES,PROBB,HHT,DDBH,IISPI,&
       &ITREII,TTHT)

  if (IITH .lt.6.or. IITH .gt.9) then
     if (LRRON) then
        do J = 1, 5, 2
           IC = ICODES(J)
           if (IC.eq.61.or.IC.eq.62.or.IC.eq.64) BMRR(II) = 0.01
           100 continue
        end do
     end if
     if (LSRON) then
        do J = 1, 5, 2
           IC = ICODES(J)
           if (IC.ge.35.and.IC.le.39) BMSR(II) = 0.001
           101 continue
        end do
     end if

  end if

  return

  !     Not relevant to this model.
  entry RRTRP (LTR)
  return

  !     Not relevant to this model.
  entry RRROUT
  return

  !     Not relevant to this model.
  entry RRPRIN (IX1)
  return

  !     Not relevant to this model.
  entry RRESIN
  return

  !     Not relevant to this model.
  entry RRTOUT
  return

  !     A very complicated model. Only inventoried infections increase.
  !     Note that both RR (Root Rot) and SR (Stem Rust) are done here.
  !     Option 1: 10% of existing infection. The following lines would be
  !     used:
  !       X1= EXP(FINT * LOG(1.0 + BMRRSR) / 10.)
  !       BMRR(I) = AMIN1(1.0, BMRR(I) * X1)
  !     Similar changes would be needed for BMSR() calculation.
  !     Option 2: 10% increase. This requires testing if the value is > 0,
  !     or everything will get infected. This is what is used in the code.

  entry RRTREG
  if (LRRON) then
     X1 = BMRRSR * FINT / 10.
     do I = 1, ITRN
        if (BMRR(I) .gt.0.0) BMRR(I) = AMIN1(1.0, BMRR(I) + X1)
        201 continue
     end do
  end if

  if (LSRON) then
     X1 = BMSRSR * FINT / 10.
     do I = 1, ITRN
        if (BMSR(I) .gt.0.0) BMSR(I) = AMIN1(1.0, BMSR(I) + X1)
        202 continue
     end do
  end if

  return

  !     Compress BMRR and BMSR if they are in use.
  entry RRCMPR (NCLAS,PROB,IND,IND1)

  if (LRRON) then
     I1 = 1
     do ICL = 1,NCLAS

        I2 = IND1(ICL)
        IREC1 = IND(I1)
        if (I1.eq. I2) goto 480

        XP = PROB(IREC1)
        TXP = XP
        K = I1 + 1
        TXP1 = BMRR(IREC1) * XP

        do I = K,I2
           IREC = IND(I)
           XP = PROB(IREC)
           TXP = TXP + XP
           TXP1 = TXP1 + BMRR(IREC) * XP
           215 continue
        end do

        !         DIVIDE BY THE TOTAL PROB AND MOVE THE VALUES INTO
        !         THE 'IREC1' POSITION IN THE ARRAYS.

        if (TXP .le.0.0) then
           BMRR(IREC1) = 0.0
        else
           BMRR(IREC1) = TXP1 / TXP
        end if

        480 continue
        I1 = I2+1
        500 continue
     end do
  end if

  if (LSRON) then
     I1 = 1
     do ICL = 1,NCLAS

        I2 = IND1(ICL)
        IREC1 = IND(I1)
        if (I1.eq. I2) goto 481

        XP = PROB(IREC1)
        TXP = XP
        K = I1 + 1
        TXP1 = BMSR(IREC1) * XP

        do I = K,I2
           IREC = IND(I)
           XP = PROB(IREC)
           TXP = TXP + XP
           TXP1 = TXP1 + BMSR(IREC) * XP
           216 continue
        end do

        !         DIVIDE BY THE TOTAL PROB AND MOVE THE VALUES INTO
        !         THE 'IREC1' POSITION IN THE ARRAYS.

        if (TXP .le.0.0) then
           BMSR(IREC1) = 0.0
        else
           BMSR(IREC1) = TXP1 / TXP
        end if

        481 continue
        I1 = I2+1
        501 continue
     end do
  end if

  return

  !     Not relevant to this model.
  entry RRSTR (II1,RR1,RR2)
  return

  !     Not relevant to this model.
  entry RRESCP (II1,II2)
  II2 = II1
  return

  !     Not relevant to this model.
  entry RRESTB (IX1,RX1)
  return

  !     Not relevant to this model.
  entry RRMN1 (II)
  return

  !     Not relevant to this model.
  entry RRMN2 (OLD)
  return

  !     Not relevant to this model.
  entry RRPR
  return

  !     Since measure is *proportion*, weight is not required.
  entry RRTRIP (IX1,IX2,RR1)
  BMRR(IX1) = BMRR(IX2)
  BMSR(IX1) = BMRR(IX2)
  return

  !     Setting to 0.0 may not be necessary.
  entry RRTDEL (IVAC,IRECN,IX1)
  BMRR(IVAC) = BMRR(IRECN)
  BMSR(IVAC) = BMSR(IRECN)
  BMRR(IRECN) = 0.0
  BMSR(IRECN) = 0.0
  return

  entry RRKEY (KEY,KEYWRD)
  KEYWRD = NORR
  return

end
