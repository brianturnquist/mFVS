!===================!
!   strp : esfltr   !
!===================!
! STRP $Id$
subroutine ESFLTR
  implicit none

  !      CALLED AFTER CRATET TO COLLECT INVENTORY INFORMATION FOR
  !      UNDERSTORY/OVERSTORY DENSITIES AND TO FLAG BEST TREES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'ESPARM.f90'
  include 'ESTREE.f90'
  include 'ESHAP.f90'
  include 'PDEN.f90'
  include 'CALDEN.f90'

  ! Variables
  integer :: IDENWK(MAXTRE),NOTE(30),ICHOIS(30),NULL(MAXSP),ITALL(MAXSP),IPLOT(MAXTRE)
  real    :: TALL(MAXSP)
  integer :: I,N,KODE,NPS,IDT,IFLAG,IBGIN,NNN,III,IS,KT,IHOLD
  real    :: HEIGHT,CHEK,ZPROB,D,PIX
  integer :: NUM,J,IDENT
  equivalence (WK5,IDENWK),(WK6,IPLOT)
  do I = 1,MAXPLT
     BAAINV(I) = 0.0
     10 continue
  end do
  TPACRE = 0.0
  if (ITRN.lt.1) go to 201

  !     PIX IS NUMBER OF STOCKABLE PLOTS

  PIX = PI-FLOAT(NONSTK)
  do I = 1,ITRN
     IPLOT(I) = ITRE(I)
     D = DBH(I)
     N = ITRE(I)
     ZPROB = PROB(I)
     if (D.lt.REGNBK) go to 20
     BAAINV(N) = BAAINV(N) +0.005454154*D*D*ZPROB*PIX
     go to 30
     20 continue
     TPACRE = TPACRE +ZPROB
     30 continue
  end do

  !     FLAG INCOMING BEST TREE RECORDS. IGNORE PROB. ASSUMPTIONS:
  !     1. SPECIES CODES ARE 1 TO MAXSP.  2. HEIGHTS, DIAMETERS, ETC. ARE
  !     SET.  3. BEST TREE RECORDS ARE WITHIN THE FIRST 30 RECORDS OF TREES
  !     TALLIED ON A PLOT.  4. IESTAT IS ZEROED OUT (DONE IN INTREE).
  !     5. PLOT NUMBERS ARE INTEGERS BETWEEN 1 AND 200.

  call OPGET2 (428,IDT,IY(1)-20,IY(1)+20,1,1,NPS,WK4,KODE)
  if (KODE.ne.0) go to 201
  IDSDAT = IFIX(WK4(1))
  call IAPSRT(ITRN,IPLOT,IDENWK,.true.)
  IFLAG = 0
  IBGIN = ITRE(IDENWK(1))
  N = 0
  NNN = 0
  do I = 1,30
     ICHOIS(I) = 0
     60 continue
  end do
  do I = 1,MAXSP
     NULL(I) = 0
     ITALL(I) = 0
     TALL(I) = 0.001
     61 continue
  end do

  !     BEGIN WORKING WITH THE TREE LIST. FLAG BEST TREES BY PLOT.

  do III = 1,ITRN
     NUM = ITRE(IDENWK(III))
     if (NUM.eq.IBGIN) go to 190
     62 continue
     if (N.gt.0) go to 63
     IBGIN = ITRE(IDENWK(III))
     go to 190
     63 continue

     !     STEP 1:  PICK 2 TALLEST RECORDS REGARDLESS OF SPECIES.

     do J = 1,2
        if (NNN.ge.N) go to 180
        CHEK = 0.001
        do I = 1,N
           if (ICHOIS(I).eq.1) go to 64
           IDENT = NOTE(I)
           IS = ISP(IDENT)
           HEIGHT = HT(IDENT)
           if (HEIGHT.lt.CHEK) go to 64
           CHEK = HEIGHT
           KT = IS
           IHOLD = I
           64 continue
        end do
        NULL(KT) = 1
        ICHOIS(IHOLD) = 1
        NNN = NNN+1
        65 continue
     end do

     !     STEP 2:  PICK TALLEST TREE OF EACH ADDITIONAL SPECIES.

     do J = 1,MAXSP
        if (NULL(J).eq.1) go to 71
        do I = 1,N
           IDENT = NOTE(I)
           IS = ISP(IDENT)
           if (IS.ne.J) go to 70
           if (ICHOIS(I).eq.1) go to 70
           HEIGHT = HT(IDENT)
           if (HEIGHT.lt.TALL(J)) go to 70
           TALL(J) = HEIGHT
           ITALL(J) = I
           70 continue
        end do
        71 continue
     end do
     do J = 1,MAXSP
        if (NULL(J).eq.1) go to 80
        KT = ITALL(J)
        if (KT.eq.0) go to 80
        ICHOIS(KT) = 1
        NNN = NNN+1
        80 continue
     end do
     if (NNN.ge.N) go to 180
     if (NNN.ge.4) go to 180

     !     STEP 3:  PICK 4 TREES BY DESCENDING HEIGHTS, IF POSSIBLE.

     90 continue
     CHEK = 0.001
     do I = 1,N
        if (ICHOIS(I).eq.1) go to 100
        IDENT = NOTE(I)
        HEIGHT = HT(IDENT)
        if (HEIGHT.lt.CHEK) go to 100
        CHEK = HEIGHT
        IHOLD = I
        100 continue
     end do
     ICHOIS(IHOLD) = 1
     NNN = NNN+1
     if (NNN.ge.N) go to 180
     if (NNN.lt.4) go to 90
     180 continue
     do I = 1,N
        IDENT = NOTE(I)
        IESTAT(IDENT) = ICHOIS(I)* (IDSDAT+20)
        181 continue
     end do

     !     RE-INITIALIZE

     if (IFLAG.eq.1) go to 200
     IBGIN = ITRE(IDENWK(III))
     N = 0
     NNN = 0
     do I = 1,30
        ICHOIS(I) = 0
        185 continue
     end do
     do I = 1,MAXSP
        NULL(I) = 0
        ITALL(I) = 0
        TALL(I) = 0.001
        186 continue
     end do
     190 continue
     if (N.gt.29) go to 195
     D = DBH(IDENWK(III))
     if (D.ge.REGNBK) go to 200
     N = N+1
     NOTE(N) = IDENWK(III)
     195 continue
     if (III.eq.ITRN) IFLAG = 1
     if (IFLAG.eq.1) go to 62
     200 continue
  end do
  201 continue
  return
end
