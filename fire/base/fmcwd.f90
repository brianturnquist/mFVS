!=======================!
!   fire/base : fmcwd   !
!=======================!
! FIRE-BASE $Id$
subroutine FMCWD(IYR)
  implicit none

  !     CALLED FROM: FMSNAG
  !                  FMMAIN
  !     CALLS:       FMSVL2
  !                  FMSVOL

  !     This routine contains CWD1, CWD2, and CWD3

  !  Purpose:
  !     This subroutine calculates annual changes in each coarse woody
  !     debris pool,

  !  Local variable definitions:

  !  Common block variables and parameters:

  !.... Parameter statements.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... Common include files.

  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'

  !.... Variable declarations.

  !  SEE LINES 368-377
  !     LOGICAL LMERCH

  ! Variables
  logical :: LCUTS,DEBUG
  integer :: I,J,K,L
  integer :: SP,IDCL,IYR
  integer :: ISNG,KSP
  real    :: D,DIAM,HTD,AMT
  real    :: DISIN,HTH,X,Y,ADD
  real    :: TVOLI,R1,R1SQ,R2SQ,P1,P2,SDIFF,S2
  real    :: HIHT(2),LOHT(2),DIS,DIH,OLDHTH,OLDHTS
  real    :: VHI(2),VLO(2),RHRAT,DIF,HICUT,LOCUT
  real    :: BP(0:9),BPH(0:9),SCNV(2),TOSOFT,DUMHT
  integer :: IDANUW,CRWNRTO

  !     Conventional breakpoints for fuel size categories.

  data BP   /0.0, 0.25, 1., 3., 6., 12., 20, 35, 50, 9999. /

  !     Assume that soft material is 80% of the density of hard
  !     Totally data-free

  data SCNV / 0.80, 1.00 /

  !  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION

  IDANUW = IYR

  !  CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMCWD',5,ICYC)
  if (DEBUG) write(JOSTND,7) 'FMCWD',ICYC
  7 format(' ENTERING ',A,' CYCLE = ',I2)

  !     Apply respiration decay losses and duff-conversion transfer
  !     to the CWD array. These losses are not tracked anywhere yet;
  !     they simply disappear. SOFT material is assumed to disappear
  !     10% faster than hard. All duff goes into the hard pool

  do I = 1, 2
     do L = 1, 4

        !         First decay duff so can add stuff to it later

        CWD(I,11,1,L) = CWD(I,11,1,L) * (1.0 - (DKR(11,L)*1.1))**NYRS
        CWD(I,11,2,L) = CWD(I,11,2,L) * (1.0 - DKR(11,L))**NYRS
        if (CWD(I,11,1,L) .lt.0.0) CWD(I,11,1,L) = 0.0
        if (CWD(I,11,2,L) .lt.0.0) CWD(I,11,2,L) = 0.0

        do J = 1, 10
           !           To work on cycle boundaries, we need to figure out how much
           !           material gets decayed, and then take a proportion of that and
           !           move it to duff (i.e., use PRDUFF directly, instead of TODUFF
           !           which combines PRDUFF and DKR together). (April 2013)

           !           Determine amount by the pools are reduced, and take a proportion
           !           to turn material into duff
           AMT = CWD(I,J,1,L) -&
                &CWD(I,J,1,L) * (1.0-(DKR(J,L)*1.1))**NYRS
           if (AMT .lt.1.0E-9) AMT = 0.0
           CWD(I,11,2,L) = CWD(I,11,2,L) + AMT * PRDUFF(J,L)
           AMT = CWD(I,J,2,L) - CWD(I,J,2,L) * (1.0-(DKR(J,L)))**NYRS
           if (AMT .lt.1.0E-9) AMT = 0.0
           CWD(I,11,2,L) = CWD(I,11,2,L) + AMT * PRDUFF(J,L)

           !            CWD(I,11,2,L) = CWD(I,11,2,L) +
           !     &                      CWD(I,J,1,L) * (1.1 * TODUFF(J,L))
           !            CWD(I,11,2,L) = CWD(I,11,2,L) +
           !     &                             CWD(I,J,2,L) * TODUFF(J,L)

           !           Now actually decrease the pools
           CWD(I,J,1,L) = CWD(I,J,1,L) * (1.0 - (DKR(J,L) * 1.1))**NYRS
           CWD(I,J,2,L) = CWD(I,J,2,L) * (1.0 -  DKR(J,L))**NYRS
           if (CWD(I,J,1,L) .lt.1.0E-9) CWD(I,J,1,L) = 0.0
           if (CWD(I,J,2,L) .lt.1.0E-9) CWD(I,J,2,L) = 0.0

           !           Now move material from hard to soft
           !           This is new as of Nov 2010.  Assuming a log is soft when it is at
           !           64% of its original density (Kim Mellen-McLean), we estimate the
           !           years until soft and then take the inverse to determine what
           !           proportion of the hard pool moves to the soft pool.

           if (J .lt.10) then
              TOSOFT = NYRS*(LOG(1-DKR(J,L)))/(LOG(0.64))
              if (TOSOFT .lt.0) TOSOFT = 0
              if (TOSOFT .gt.1) TOSOFT = 1
              TOSOFT = TOSOFT * CWD(I,J,2,L)
              CWD(I,J,1,L) = CWD(I,J,1,L) + TOSOFT
              CWD(I,J,2,L) = CWD(I,J,2,L) - TOSOFT
              if (CWD(I,J,1,L) .lt.1.0E-9) CWD(I,J,1,L) = 0.0
              if (CWD(I,J,2,L) .lt.1.0E-9) CWD(I,J,2,L) = 0.0
           end if

           6 continue
        end do
        8 continue
     end do
     5 continue
  end do

  !     NOTE: FuelMove logic was moved to new routine: FMFMOV
  return

  !     CWD1:

  !     CALLED FROM FMSNAG

  !     ENTRY POINT FOR ADDITION OF NEW CWD VIA FALLING OF SNAGS.

  !     ISNG  = INDEX IN SNAG LIST
  !     KSP   = SPECIES OF SNAG
  !     D     = DBH OF SNAG
  !     DIH   = DENSITY (#/AC) OF INITIALLY-HARD SNAGS FALLEN
  !     DIS   = DENSITY (#/AC) OF INITIALLY-SOFT SNAGS FALLEN

  entry CWD1(ISNG, DIH, DISIN)

  call DBCHK (DEBUG,'FMCWD',5,ICYC)

  if (DEBUG) write(JOSTND,7) 'FM-CWD1',ICYC

  if (DEBUG) write (JOSTND,*) 'ISNG=',ISNG,' DIH=',DIH,&
       &' DISIN=',DISIN

  if ((DIH+DISIN) .le.0.0) return
  I = ISNG
  DIS = DISIN

  !     These snags are not created by **CUTS**

  LCUTS = .false.

  !     Height of top and bottom limits for volume integration;
  !     1=soft and 2=hard

  HIHT(1) = HTIS(I)
  HIHT(2) = HTIH(I)

  LOHT(1) = 1.0
  !c1      LOHT(2) = 1.0
  LOHT(2) = 0.10

  DIAM = DBHS(I)
  HTD = HTDEAD(I)
  SP = SPS(I)

  !     GET A TOTAL VOLUME FOR THIS SNAG

  TVOLI = 0.
  DUMHT = -1.0
  call FMSVL2(SP,DIAM,HTD,DUMHT,TVOLI,0,&
       &'D',.false.,.false.,JOSTND)
  !      CALL FMSVOL(I,HTD,TVOLI,.false.,JOSTND)
  if (DEBUG) write (JOSTND,*) 'I(CWD1)=',I,' HTD=',HTD,&
       &' TVOLI=',TVOLI

  goto 1000

  !     CWD2:

  !     CALLED FROM FMSNAG

  !     ISNG    = INDEX IN SNAG LIST
  !     DIH     = DENSITY (#/AC) OF INITIALLY-HARD SNAGS BROKEN
  !     DIS     = DENSITY (#/AC) OF INITIALLY-SOFT SNAGS BROKEN
  !     OLDHTS  = HEIGHT (FT) BEFORE BREAKAGE OF INITIALLY-SOFT SNAGS
  !     OLDHTH  = HEIGHT (FT) BEFORE BREAKAGE OF INITIALLY-HARD SNAGS

  entry CWD2(ISNG, DIH, DISIN, OLDHTH, OLDHTS)

  call DBCHK (DEBUG,'FMCWD',5,ICYC)

  if (DEBUG) write(JOSTND,7) 'FM-CWD2',ICYC

  if (DEBUG) write (JOSTND,*) 'ISNG=',ISNG,' DIH=',DIH,&
       &' DISIN=',DISIN

  if ((DIH+DISIN) .le.0.0) return
  I = ISNG
  DIS = DISIN

  !     These snags are not created by **CUTS**

  LCUTS = .false.

  !     Height of top and bottom limits for volume integration;
  !     soft and hard

  HIHT(1) = OLDHTS
  HIHT(2) = OLDHTH

  LOHT(1) = HTIS(I)
  LOHT(2) = HTIH(I)

  DIAM = DBHS(I)
  HTD = HTDEAD(I)
  SP = SPS(I)

  !     GET A TOTAL VOLUME FOR THIS SNAG

  TVOLI = 0.
  DUMHT = -1.0
  call FMSVL2(SP,DIAM,HTD,DUMHT,TVOLI,0,&
       &'D',.false.,.false.,JOSTND)
  !      CALL FMSVOL(I,HTD,TVOLI,.false.,JOSTND)

  goto 1000

  !     CWD3:

  !     CALLED FROM FMSCUT

  !     ISP     = SPECIES INDEX
  !     D       = DBH
  !     DIH     = DENSITY (#/AC) OF DOWNED (HARD) SNAGS FROM **CUTS**
  !     HT      = HEIGHT (FT) OF TREE JUST DOWNED

  entry CWD3(KSP, D, DIH, HTH, CRWNRTO)

  DEBUG = .false.

  if (DIH .le.0.0) return
  !     This snag-material is created by **CUTS**. It is all hard.

  DIS = 0.
  LCUTS = .true.

  !     Height of top and bottom limits for volume integration;
  !     soft are ignored and set to zero

  HIHT(1) = 0.
  HIHT(2) = HTH

  LOHT(1) = 0.
  !c1      LOHT(2) = 1.
  LOHT(2) = .1

  DIAM = D
  HTD = HTH
  SP = KSP

  !     GET A TOTAL VOLUME FOR THIS TREE (created by cuts)

  TVOLI = -1
  call FMSVL2(SP,DIAM,HTD,TVOLI,TVOLI,CRWNRTO,&
       &'D',.false.,.false.,JOSTND)

  1000 continue

  !     Use a conical taper to find the largest possible diameter of the debris
  !     54 is the height for DBH measurements, HT0 is the height of the original
  !     tree (uncorrected for topkill, so far). X is the diameter of the large end
  !     of the fragment or the base of the tipped snag, depending on context. Use
  !     that value to find the size class.

  !      X = (HIHTH - LOHTH) * 12. * DBHS(I) / ((LOHTH * 12.) - 54.)

  !     RADIUS/HEIGHT RATIO FOR TRIANGLE (CONE MODEL OF TREE)

  if (DIAM .le.0.1) DIAM = 0.1
  !cccccc sdiff and s2 are only for debugging new code
  SDIFF = 0.
  S2 = 0.

  RHRAT = ((HTD * 12.) - 54.) / (0.5 * DIAM)

  IDCL = DKRCLS(SP)

  !     FIND HEIGHTS AT WHICH THE BREAKPOINTS WILL LIE. A -1 MARKS
  !     A BREAKPOINT OUTSIDE THE HEIGHT OF THE SNAG. THE BOTTOM OF THE
  !     SNAG IS AT 1 FOOT.

  do J = 0, 9

     X = (0.5 * BP(J) * RHRAT) / 12.0
     Y = HTD - X

     !c1       BPH(J) = MAX(1.0, Y)
     BPH(J) = MAX(0.10, Y)

     10 continue
  end do
  !c      WRITE (JOSTND,*) 'BPH=',BPH

  !     WALK THROUGH ALL THE BREAKPOINTS. IF ANY INTERVAL CONTAINS
  !     A PIECE OF THE TREE OF INTEREST, DO SOME CALCS.

  do K = 1, 2
     VHI(K) = 0.
     VLO(K) = 0.

     !       skip the loop if there are no snag pieces of K-type initial hardness

     if (K .eq.1) then
        if (DIS .le.0.0) goto 20
     else
        if (DIH .le.0.0) goto 20
     end if

     !c1        LOHT(K) = MAX(1.0, LOHT(K))
     LOHT(K) = MAX(0.10, LOHT(K))

     R1 = DIAM*0.0416666667 ! (= 1/12 * .5)
     if (HTD .gt.4.5) then ! compute r1 at base of stem.
        R1 = R1 + (LOHT(K) * ((R1*HTD)/(HTD-4.5)))
     end if
     R1SQ = R1*R1

     do J = 1, 9

        !         skip loop if none of the broken snag falls in current size category

        if (HIHT(K) .le. BPH(J))    goto 21
        if (LOHT(K) .gt. BPH(J-1))  goto 21

        !         set HICUT to top of the broken snag or top of current size category,
        !         whichever is less

        HICUT = HIHT(K)
        if (HIHT(K) .gt. BPH(J-1))  HICUT = BPH(J-1)

        !         set LOCUT to bottom of the broken snag or bottom of current size
        !         category, whichever is greater

        LOCUT = LOHT(K)
        if (LOHT(K) .le. BPH(J)) LOCUT = BPH(J)

        !         if the low and high points are the same, then we have reached the end and
        !         do not need to calculate volumes
        if (LOCUT .eq. HICUT) goto 21

        !         get the TOTAL volume-per-snag up to HICUT and up to LOCUT.  Set DIF to the
        !         volume between them - i.e., the vol. in the current size category -
        !         and convert it to volume-per-acre.

        !cccccccccc This block can be deleted if the new code is used
        !          IF (LCUTS) THEN
        !            LMERCH = .FALSE.
        !            CALL FMSVL2(SP,DIAM,HTD,HICUT,VHI(K),LMERCH,.false.,JOSTND)
        !            CALL FMSVL2(SP,DIAM,HTD,LOCUT,VLO(K),LMERCH,.false.,JOSTND)
        !          ELSE
        !            CALL FMSVOL(I,HICUT,VHI(K),.false.,JOSTND)
        !            CALL FMSVOL(I,LOCUT,VLO(K),.false.,JOSTND)
        !          ENDIF
        !cccccccccc above .........

        R2SQ = R1 * (1. - (HICUT/HTD))
        R2SQ = R2SQ * R2SQ
        P1 = (R2SQ*(HTD-HICUT))/(R1SQ*HTD)
        R2SQ = R1 * (1. - (LOCUT/HTD))
        R2SQ = R2SQ * R2SQ
        P2 = (R2SQ*(HTD-LOCUT))/(R1SQ*HTD)
        DIF = MAX(0.,(P2-P1)) * TVOLI
        SDIFF = SDIFF+DIF
        S2 = S2 + (VHI(K) - VLO(K))
        if (DEBUG) write (JOSTND,*) 'R2=',R2SQ**.5,'P1=',&
             &P1,'P2=',P2,' DIF=',DIF,&
             &' OLDDIF=',VHI(K) - VLO(K),' SDIFF=',SDIFF,' S2=',S2

        !***uncomment to use old DIF:          DIF = VHI(K) - VLO(K)

        if (K .eq.1) then
           DIF = DIF * DIS
        else
           DIF = DIF * DIH
        end if

        !         allocate the volume DIF of material to the various CWD categories.

        ADD = 0.
        if (DIF .gt.1.E-6) then
           ADD = DIF * V2T(SP) * SCNV(K)
           CWD(1,J,K,IDCL) = CWD(1,J,K,IDCL) + ADD

           CWDNEW(2,J) = CWDNEW(2,J) + ADD
        end if

        if (DEBUG) write (JOSTND,16) I,K,LOCUT,&
             &HICUT, VHI(K), VLO(K),DIF,ADD,TVOLI
        16 format(' I=',I4,' K=',I2,' LOCUT=',F7.3,' HICUT=',&
             &F7.3,' VHI=',F7.3,' VLO=',F7.3,' DIF=',F8.5,&
             &' ADD=',F10.6,' TVOLI=',F10.6)

        21 continue
     end do
     20 continue
  end do

  return
end
