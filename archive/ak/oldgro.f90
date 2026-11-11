!=========================!
!   archive/ak : oldgro   !
!=========================!
! AK $Id$
subroutine OLDGRO(ISPC,VN,D,H,BBFV)
  implicit none

  !   OLDGRO COMPUTES VOLUMES OF OLD GROWTH TREES WITH
  !   (D GT 35  OR H GT 190).   32-foot log rule.

  !   FROM DON DEMARS --- PNW, JUNEAU AK

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'

  ! Variables
  logical :: DEBUG

  integer :: I,ICK,ISPC,JJ,K,KB,KBD,N,NM,NN,NNN

  real    :: BBFV,BK,BRATIO,CL,CV4,D,DD2MI,DIFICK,DXL,FF,FK,FV,H,HEST
  real    :: NOL16,NOL32,R,RXL,SCFT,ST,TAPER,VN,XINTT

  real    :: DIB(20),DS(20),DSHI(2),DSLO(2),DST(20),HH(20),HMM(20)
  real    :: SCF(20),SH(20),VS(20),XINT(20),XLL(20)

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'OLDGRO',6,ICYC)

  111 continue
  NOL32 = 0
  NOL16 = 0
  do I = 1,20
     VS(I) = 0.0
     DS(I) = 0.0
     DIB(I) = 0.0
     SCF(I) = 0.0
     XINT(I) = 0.0
     XLL(I) = 0.0
     15 continue
  end do

  ST = 1.0
  if (D.gt.36)ST = D/36.
  if (NOL32.eq.0.and.NOL16.eq.0)BK = BRATIO(ISPC,D,H)
  if (NOL32.eq.0.and.NOL16.eq.0)FV = .005454154*D**2*(H-4.5)*BK
  if (NOL32.gt.0)NOL16 = NOL32*2
  if (NOL16.gt.0)HH(1) = ST+NOL16*16.3
  if (NOL16.gt.0)H = 0.0
  do I = 1,2
     XLL(I) = 1.-(2./3.)*(TOPD(ISPC)/D)
     if (H.lt.0.01)HEST = HH(1)/XLL(I)
     if (HEST.gt.0.01)BK = BRATIO(ISPC,D,HEST)
     DST(I) = (TOPD(ISPC)**2)/(BK*D**2)
     DSLO(I) = DST(I)-.00001
     DSHI(I) = DST(I)+.00001
     if (H.gt.0.01)go to 428
     R = (HEST-HH(1))/(HEST-4.5)
     call PROF1(R,D,HEST,DD2MI)
     DS(I) = DD2MI
     RXL = .9*R
     call PROF1(RXL,D,HEST,DD2MI)
     DXL = DD2MI
     TAPER = (DS(I)-DXL)/(.1*R)
     go to 429

     428 continue
     HH(I) = H*XLL(I)
     R = (H-HH(I))/(H-4.5)
     call PROF1(R,D,H,DD2MI)
     DS(I) = DD2MI

     !      ESTIMATE TAPER NEAR R

     RXL = .9*R
     call PROF1(RXL,D,H,DD2MI)
     DXL = DD2MI
     TAPER = (DS(I)-DXL)/(.1*R)
     429 continue
     do K = 1,10
        if (DS(I).gt.DSLO(I).and.DS(I).lt.DSHI(I))go to 40
        RXL = R+(DST(I)-DS(I))/TAPER
        R = RXL
        if (H.gt.0.01)go to 246
        HEST = (HH(I)-4.5*R)/(1.-R)
        call PROF1(R,D,HEST,DD2MI)
        DS(I) = DD2MI
        go to 245
        246 continue
        call PROF1(R,D,H,DD2MI)
        DS(I) = DD2MI
        245 continue
     end do
     40 continue
     if (NOL16.gt.0.and.H.lt.0.01)go to 27
     HH(I) = H-(R*(H-4.5))
     27 continue
     if (H.lt.0.01)H = HEST

     !     FIND NUMBER (NN) OF LOGS AND CHUNKS AND FIND CHUNK LENGTH
     !     FOR LIMD(1) ONLY (I.E., DIB TOP=6.0 INCHES).

     2 continue
  end do
  do N = 1,15
     HMM(1) = 16.3*N+ST+.0001
     NN = N
     if (HMM(1).ge.HH(1))go to 60
     50 continue
  end do
  60 continue
  KBD = NN-1

  !     CALCULATE CHUNK LENGTH

  CL = HH(1)-16.3*KBD-ST

  !     WE NOW KNOW (OR AT LEAST WE THINK WE KNOW) DBH, HEIGHT, DIAMETER
  !       OUTSIDE BARK AT 6-INCH TOP, MERCH. HEIGHT TO 6-INCH TOP,
  !       DIAMETER OUTSIDE BARK AT 4-INCH TOP, MERCH. HEIGHT TO A 4-INCH
  !       TOP, NUMBER OF WHOLE LOGS IN THE TREE TO A 6-INCH TOP, AND
  !       THE CHUNK LENGTH.

  !     NEXT, FIND SCALING DIAMETERS AND VOLUMES FOR ALL LOG HEIGHTS TO
  !       TIP OF TREE.  START WITH SCALING HEIGHT LIST.

  SH(1) = 0.
  SH(2) = ST
  do KB = 1,KBD
     SH(KB+2) = ST+KB*16.3
     70 continue
  end do
  SH(KBD+3) = HH(1)
  SH(KBD+4) = HH(2)
  SH(KBD+5) = H
  NNN = NN+3
  BK = BRATIO(ISPC,D,H)
  FV = .005454154*D**2*(H-4.5)*BK
  do JJ = 1,NNN
     R = (H-SH(JJ))/(H-4.5)
     call PROF1(R,D,H,DD2MI)
     DIB(JJ) = DD2MI
     if (DEBUG)write(JOSTND,800)R,D,H,DD2MI,BK
     800 format(' IN OLDGRO 800 FMT R,D,H,DD2MI,BK= ',5F10.4)
     DIB(JJ) = (DIB(JJ)*BK)**.5*D
     call PROF2(R,D,H,FF)
     VS(JJ) = FF*FV

     78 continue
  end do

  !     NEXT, GET LOG AND TREE VOLUMES

  CV4 = VS(2)-VS(NNN)

  !     PRINT OUT EVERYTHING

  XINTT = 0.0

  !     NOW ADD SCRIBNER FORMULA AND INTERNATIONAL 1/4-INCH SCALE.  START
  !     WITH DIB(3) AND GO TO DIB(NNN-1).  DIB(NNN-1) IS 6.0 INCH TOP.

  SCFT = 0.0
  NM = NNN-1
  do K = 3,NM

     ! SCRIBNER 32 FOOT LOG RULE

     FK = real(K) * 0.5
     ICK = INT(FK)
     DIFICK = FK - real(ICK) + 0.0001
     if (DIFICK .gt.0.1.and. K .ne. NM) go to 1500
     if (DIFICK .gt.0.1.and. K .eq. NM)&
          &SCF(K) = CL / 32.6 * (1.58 * DIB(K)*DIB(K) - 4.0 * DIB(K)-8.)
     if (DIFICK .le.0.1.and. K .ne. NM)&
          &SCF(K) = (1.58 * DIB(K)*DIB(K) - 4. * DIB(K) - 8.)
     if (DIFICK .le.0.1.and. K .eq. NM)&
          &SCF(K) = (CL + 16.3) / 32.6 *&
          &(1.58 * DIB(K)*DIB(K) - 4.0 * DIB(K) - 8.)
     go to 1600
     1500 continue
     SCF(K) = 0.0
     1600 continue
     SCFT = SCFT + SCF(K)

     ! INTERNATIONAL 1/4 INCH LOG RULE

     XINT(K) = (.796*DIB(K)**2-1.374*DIB(K)-1.23)
     if (K.eq.NM)go to 91
     go to 92
     91 continue
     if (CL.gt.0.0.and.CL.lt.4.075)XINT(K) = CL/4.075*(0.199*DIB(K)**2-0.6&
          &42*DIB(K))
     if (CL.ge.4.075.and.CL.lt.8.15)XINT(K) = CL/8.15*(0.398*DIB(K)**2-1.0&
          &86*DIB(K)-.271)
     if (CL.ge.8.15.and.CL.lt.12.225)XINT(K) = CL/12.225*(0.597*DIB(K)**2-&
          &1.330*DIB(K)-.715)
     if (CL.ge.12.225.and.CL.lt.16.3)XINT(K) = CL/16.3*(.796*DIB(K)**2-1.3&
          &75*DIB(K)-1.230)
     92 continue
     XINTT = XINTT+XINT(K)
     90 continue
  end do
  VN = CV4
  BBFV = SCFT
  900 return
end
