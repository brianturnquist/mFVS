!=========================!
!   fire/base : fmevmon   !
!=========================!
! FIRE-BASE $Id$
subroutine FMEVMON
  implicit none

  !     CALLED FROM: EVLDX

  !  Purpose:
  !     This subroutine brings into one place the procedures to summarize
  !     information for the Event Monitor. The first entry point is for
  !     coarse woody debris; the second is for snags.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... Common include files.

  include 'FMCOM.f90'
  include 'FMFCOM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'

  !.... Variable declarations.

  ! Variables
  integer :: I,J,K,M,IX,JX,KX,ILO,IHI,II,III
  real    :: RVAL,XLDBH,XHDBH,XLHT,XHHT
  real    :: XS,XH,X,X1
  integer :: IRC,IGRP,IULIM,IG
  real    :: TPA,H,VT,D,HS
  real    :: XD,XM1,HTD

  integer :: ITYP,IPART,ISTAND
  integer :: ISZ,IDC,ITM,ISPC,ISPS
  real    :: SNGSTM,SNGCRN
  real    :: TOTBA,TOTSBA,SNBAIH,SNBAIS,SNVIH,SNVIS
  real    :: LIVSRM,LIVSLV,LIVCRN,LIVFOL,TOTFOL,LIVCRM
  real    :: SNGSRM,LIVSRMTEM
  real    :: TOTDEAD,LVCRNRMFOL
  logical :: LINCL,DEBUG,LMERCH
  real    :: SNGVOL1,SNGSALVOL
  real    :: CRREM(MAXTRE),DSNG1(MAXTRE),SSNG1(MAXTRE)
  integer :: IJ

  !     EVENT MONITOR POTFLEN FUNCTION.

  entry FMEVFLM(RVAL, II, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     select case (II)
     case (1)
        RVAL = PFLAM(1) ! Total severe flame length
     case (2)
        RVAL = PFLAM(3) ! Total moderate flame length
     case (3)
        RVAL = PFLAM(2) ! Surface severe flame length
     case (4)
        RVAL = PFLAM(4) ! Surface moderate flame length
     end select
  end if
  return

  !     EVENT MONITOR POTFMORT FUNCTION.

  entry FMEVMRT(RVAL, II, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     select case (II)
     case (1)
        RVAL = POTKIL(1)*100 ! potential severe fire mortality (%BA)
     case (2)
        RVAL = POTKIL(3)*100 ! pot. moderate fire mortality (%BA)
     case (3)
        RVAL = POTVOL(1) ! potential severe fire mortality (tot cuft)
     case (4)
        RVAL = POTVOL(2) ! pot. moderate fire mortality (tot cuft)
     end select
  end if
  return

  !     EVENT MONITOR FUELMODS FUNCTION.

  entry FMEVFMD(RVAL, II, III, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     if (III .eq.1) then
        RVAL = 0
        select case (II)
        case (1)
           if (FWT(1) .gt.0) RVAL = FMOD(1) ! first fuel model
        case (2)
           if (FWT(2) .gt.0) RVAL = FMOD(2) ! second fuel model
        case (3)
           if (FWT(3) .gt.0) RVAL = FMOD(3) ! third fuel model
        case (4)
           if (FWT(4) .gt.0) RVAL = FMOD(4) ! fourth fuel model
        end select
     else if (III .eq.2) then
        select case (II)
        case (1)
           RVAL = FWT(1) ! first fuel model weight
        case (2)
           RVAL = FWT(2) ! second fuel model weight
        case (3)
           RVAL = FWT(3) ! third fuel model weight
        case (4)
           RVAL = FWT(4) ! fourth fuel model weight
        end select
     end if
  end if
  return

  !     EVENT MONITOR FUELLOAD FUNCTION. ILO AND IHI MARK THE LOWER AND
  !     UPPER INDICES FOR THE 2ND ELEMENT OF THE CWD ARRAY. SUMMATION
  !     IS CARRIED OUT OVER ALL THE OTHER DIMENSIONS.

  entry FMEVCWD(RVAL, ILO, IHI, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     RVAL = 0.0
     do I = 1, 2
        do J = ILO, IHI
           do K = 1, 2
              do M = 1, 4
                 RVAL = RVAL + CWD(I,J,K,M)
              end do
           end do
        end do
     end do
  end if

  return

  !     EVENT MONITOR SNAGS FUNCTION.

  !     RVAL  VALUE RETURNED AFTER EVALUATING THE SNAG LIST
  !     IX    INDEX FOR KIND OF RESULT: 1=TPA; 2=BA; 3=VOL
  !     JX    SPECIES INDEX; <0=SPECIES GROUP; 0=ALL; 1...MAXSP
  !     KX    HARD INDEX:    0=ALL; 1=HARD; 2=SOFT

  entry FMEVSNG(RVAL, IX, JX, KX, XLDBH, XHDBH, XLHT, XHHT, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     XH = 0.
     XS = 0.
     RVAL = 0.
     if (NSNAG.lt.1) return

     do I = 1, NSNAG

        ISPS = SPS(I)
        D = DBHS(I)

        LINCL = .false.
        if (JX.eq.0.or. JX.eq.ISPS) then
           LINCL = .true.
        else if (JX.lt.0) then
           IGRP = -JX
           IULIM = ISPGRP(IGRP,1)+1
           do IG = 2,IULIM
              if (ISPS .eq. ISPGRP(IGRP,IG)) then
                 LINCL = .true.
                 go to 91
              end if
              90 continue
           end do
        end if
        91 continue

        if (LINCL .and.&
             &(D.ge.XLDBH .and. D.lt.XHDBH)) then

           !  PASS OVER THE INITIALLY-HARD SNAGS

           HS = HTIH(I)
           TPA = DENIH(I)
           if (TPA .gt.0..and. HS.ge.XLHT .and. HS.lt.XHHT) then
              goto (211,212,213), IX
              211 continue
              X = TPA
              goto 120
              212 continue
              X = TPA * D * D * 0.005454154
              goto 120
              213 continue
              call FMSVOL(I,HS,X1,.false.,0)
              X = TPA * X1
              goto 120
              120 continue

              if (HARD(I)) then
                 XH = XH + X
              else
                 XS = XS + X
              end if
           end if

           !     PASS OVER THE INITIALLY-SOFT SNAGS

           HS = HTIS(I)
           TPA = DENIS(I)
           if (TPA .gt.0..and. HS.ge.XLHT .and. HS.lt.XHHT) then
              goto (311,312,313), IX
              311 continue
              X = TPA
              goto 220
              312 continue
              X = TPA * D * D * 0.005454154
              goto 220
              313 continue
              call FMSVOL(I,HS,X1,.false.,0)
              X = TPA * X1
              goto 220
              220 continue

              XS = XS + X
           end if
        end if

        500 continue
     end do

     ! TAKE HARD-COMPONENT, SOFT-COMPONENT, OR BOTH

     if (KX .eq.1) then
        RVAL = XH
     else if (KX .eq.2) then
        RVAL = XS
     else
        RVAL = XH + XS
     end if
  end if

  return

  !     EVENT MONITOR SALVVOL FUNCTION.
  !     JX    SPECIES INDEX; <0=SPECIES GROUP; 0=ALL; 1...MAXSP
  !     XLDBH THE LOWEST DBH VALUE
  !     XHDBH THE HIGHEST DBH VALUE

  entry FMEVSAL(RVAL, JX, XLDBH, XHDBH, IRC)
  IRC = 0
  XH = 0.
  XS = 0.
  RVAL = 0.
  if (NSNAGSALV.lt.1) return

  do I = 1, NSNAGSALV

     ISPS = SPSSALV(I)
     XD = DBHSSALV(I)
     HTD = HTDEADSALV(I)

     LINCL = .false.
     if (JX.eq.0.or. JX.eq.ISPS) then
        LINCL = .true.
     else if (JX.lt.0) then
        IGRP = -JX
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           if (ISPS .eq. ISPGRP(IGRP,IG)) then
              LINCL = .true.
              go to 93
           end if
           92 continue
        end do
     end if
     93 continue

     if (LINCL .and.&
          &(XD.ge.XLDBH .and. XD.lt.XHDBH)) then

        !  PASS OVER THE INITIALLY-HARD SNAGS

        HS = HTIHSALV(I)
        TPA = SALVSPA(I,1)
        if (TPA .gt.0.) then
           call FMSVL2(ISPS,XD,HTD,HS,X1,0,&
                &'D',.false.,DEBUG,JOSTND)
           X = TPA * X1
           if (HARDSALV(I)) then
              XH = XH + X
           else
              XS = XS + X
           end if
        end if

        !     PASS OVER THE INITIALLY-SOFT SNAGS

        HS = HTISSALV(I)
        TPA = SALVSPA(I,2)
        if (TPA .gt.0. ) then
           call FMSVL2(ISPS,XD,HTD,HS,X1,0,&
                &'D',.false.,DEBUG,JOSTND)
           X = TPA * X1
           XS = XS + X
        end if
     end if

     600 continue
  end do

  RVAL = XH + XS

  return

  !     EVENT MONITOR POTFTYPE FUNCTION.

  entry FMEVTYP(RVAL, II, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     select case (II)
     case (1)
        RVAL = POTTYP(1) ! potential fire type for severe fires
     case (2)
        RVAL = POTTYP(2) ! potential fire type for moderate fires
     end select
  end if
  return

  !     EVENT MONITOR TREEBIO FUNCTION.

  !     RVAL   VALUE RETURNED AFTER EVALUATING THE SNAG DATA
  !     ISTAND TRREE STATUS -  STANDING(<0), REMOVED(0),OR BOTH(>0)
  !     ITYP   TREE TYPE -  DEAD(<0), LIVE(0),OR BOTH(>0)
  !     IPART  TREE PART -  STEM(<0), CROWN(0),OR BOTH-WHOLE TREE(>0)
  !     JX     SPECIES INDEX; <0=SPECIES GROUP; 0=ALL; 1...MAXSP
  !     XLDBH  LOWER RANGE DBH
  !     XHDBH  UPPER RANGE DBH
  !     XLHT   LOWER RANGE HEIGHT
  !     XHHT   UPPER RANGE HEIGHT

  !     LOCAL VARIABLES
  !     SNGSTM   - SNAG STEM WEIGHT (TONS)
  !     SNGCRN   - SNAG CROWN WEIGHT (TONS)

  entry FMTREM(DSNG1,SSNG1,CRREM)

  !     CHECK TO SEE IF THE FIRE MODEL IS ACTIVE

  if (.not. LFMON) return
  ITRNL = ITRN
  do J = 1,MAXTRE
     PREMST(J) = 0.
     PREMCR(J) = 0.
     DBHC(J) = 0.
     HTC(J) = 0.
     ISPCC(J) = 0
     do IJ = 0,5
        CROWNWC(J,IJ) = 0.
     end do
  end do

  !  STORES THE FRACTION OF THE TREE RECORD REMOVED FROM THE STAND
  !  CALLED FROM **CUTS**

  LREMT = .true.
  ICYCRM = ICYC

  do I = 1,ITRNL
     PREMST(I) = AMAX1(0.,WK3(I)-SSNG1(I)-DSNG1(I))

     !  LIVE REMOVED CROWN MATERIAL IS STORED IN PREMCR ARRAY
     !  ASSUME THAT ALL DEAD CROWN MATERIAL IS LEFT IN STAND

     PREMCR(I) = CRREM(I)

     ISPCC(I) = ISP(I)
     DBHC(I) = DBH(I)
     HTC(I) = HT(I)

     do J = 0,5
        CROWNWC(I,J) = CROWNW(I,J)
     end do

     if (PREMCR(I).lt.0.00001)PREMCR(I) = 0.
     if (PREMST(I).lt.0.00001)PREMST(I) = 0.

     !      IF(DEBUG)WRITE(JOSTND,*)' ENTERING FMTREM,CYCLE=  ICYC,ITRNL,',
     !     &'I,ISPC,DBH,DSNG1,SSNG1,CRREM,FMPROB(I),PREMST(I),',
     !     &'PREMCR(I)= ',ICYC,ITRNL,I,ISP(I),DBH(I),DSNG1(I),
     !     &SSNG1(I),CRREM(I),FMPROB(I),PREMST(I),PREMCR(I)

  end do

  return

  !  CALCULATES THE TREE BIOMASS EVENT MONITOR VARIABLE

  entry FMEVTBM(RVAL,ISTAND,ITYP,IPART,JX,XLDBH,XHDBH,XLHT,XHHT,IRC)

  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     RVAL = 0.0

     !  IF THIS IS NOT A THINING CYCLE, SET LREMT TO FALSE SO REMOVALS ARE
     !  NOT ACCOUNTED FOR UNTIL NEXT THINING, AND ZERO OUT PREMSTAND
     !  PREMCR ARRAYS.

     if ((ICYCRM.ne.ICYC).or.(.not.LREMT)) then
        do I = 1,MAXTRE
           PREMST(I) = 0.
           PREMCR(I) = 0.
        end do
     end if

     call DBCHK (DEBUG,'FMEVMON',7,ICYC)
     if (DEBUG) write(JOSTND,7) ICYC,LFMON,NSNAG,ICYCRM
     7 format(' ENTERING FMEVMON CYC = ',I2,' LFMON= ',L2,&
          &' NSNAG=',I4,' ICYCRM= ',I2)
     if (DEBUG)write(JOSTND,*)' RVAL,ISTAND,ITYP,IPART,JX,XLDBH,',&
          &'XHDBH,XLHT,XHHT,IRC= ',RVAL,ISTAND,ITYP,IPART,JX,XLDBH,&
          &XHDBH,XLHT,XHHT,IRC

     !  INITALIZE VARIABLES

     SNGSTM = 0. !WEIGHT OF SELECTED SNAG STEMS (TONS)
     SNGCRN = 0. !WEIGHT OF SELECTED SNAG CROWN MATERIAL (TONS)
     SNGSRM = 0. !WEIGHT OF REMOVED SNAG STEMS
     TOTBA = 0. !TOTAL BASAL AREA OF ALL SNAGS
     TOTSBA = 0. !TOTAL BASAL AREA OF SELECTED SNAGS
     SNBAIH = 0. !BASAL AREA OF SELECTED INITIALLY HARD SNAGS
     SNBAIS = 0. !BASAL AREA OF SELECTED INITIALLY SOFT SNAGS
     XH = 0.
     XS = 0.
     SNGVOL1 = 0. !DEBUG VARIABLE
     SNGSALVOL = 0. !DEBUG VARIABLE
     XM1 = -1.

     !  SNAGS

     if (NSNAG.lt.1)goto 100
     do I = 1, NSNAG

        ISPS = SPS(I)
        D = DBHS(I)
        LINCL = .false.

        if (JX.eq.0.or. JX.eq.ISPS) then
           LINCL = .true.
        else if (JX.lt.0) then
           IGRP = -JX
           IULIM = ISPGRP(IGRP,1)+1
           do IG = 2,IULIM
              if (ISPS .eq. ISPGRP(IGRP,IG)) then
                 LINCL = .true.
                 exit
              end if
           end do
        end if

        !  ESTIMATE TOTAL BASAL AREA OF SNAGS

        TOTBA = TOTBA+0.005454154*D*D*(DENIH(I)+DENIS(I))

        if (LINCL) then
           if ((DENIS(I)+DENIH(I)).gt.0.) then
              SNVIH = 0.0
              SNVIS = 0.0

              !  SNAG STEMS
              !  CONSTRAIN TO DBH RANGE

              if ((D.ge.XLDBH).and.(D.lt.XHDBH)) then

                 !  CONSTRAIN TO HEIGHT RANGE

                 if ((HTIH(I).ge.XLHT).and.(HTIH(I).lt.XHHT)) then
                    if (DENIH(I) .gt.0.0) then
                       SNBAIH = SNBAIH+0.005454154*D*D*DENIH(I)
                       call FMSVOL (I, HTIH(I), SNVIH, .false., JOSTND)
                       SNVIH = SNVIH*DENIH(I)
                    end if
                 end if

                 if ((HTIS(I).ge.XLHT).and.(HTIS(I).lt.XHHT)) then
                    if (DENIS(I) .gt.0.0) then
                       SNBAIS = SNBAIS+0.005454154*D*D*DENIS(I)
                       call FMSVOL (I, HTIS(I), SNVIS,  .false., JOSTND)
                       SNVIS = SNVIS*DENIS(I)
                    end if
                 end if

                 SNGVOL1 = SNGVOL1+SNVIS+SNVIH

                 !  SUM SNAG STEM VOLUME AND CONVERT TO TONS

                 SNGSTM = SNGSTM + (SNVIS+SNVIH) * V2T(ISPS)

              end if !DBH
           end if !DENIS(I)+DENIH(I))
        end if !SPECIES (LINCL)
     end do !SNAG LOOP
     100 continue
     TOTSBA = SNBAIS + SNBAIH

     !  ACCUMULATE DEAD CROWN MATERIAL
     !  ADD SNAG CROWNS TO THE APPROPRIATE SIZE CLASS
     !  INCLUDE LITTER IN THE 0-3 CLASS, JUST SO THAT IT IS REPORTED

     do ISZ = 0,5
        do IDC = 1,4
           do ITM = 1,TFMAX
              SNGCRN = SNGCRN + P2T *&
                   &(CWD2B(IDC,ISZ,ITM) + CWD2B2(IDC,ISZ,ITM))
           end do
        end do
     end do

     !  ADJUST TOTAL SNAG CROWN WEIGHT BY THE PROPORTION OF SNAG BASAL AREA
     !  SELECTED BY USER

     if (TOTBA.gt.0.)SNGCRN = SNGCRN*TOTSBA/TOTBA
     TOTDEAD = SNGSTM+SNGCRN
     if (DEBUG)write(JOSTND,*)' ICYC,TOTDEAD,SNGCRN, TOTSBA,TOTBA=  ',&
          &ICYC,TOTDEAD,SNGCRN, TOTSBA,TOTBA

     !  CALCUALTE THE SALVAGE VOLUME, SNAG LIST FOR SALVBAGE CALCULATIONS
     !  IS PASSED FROM FMSALV

     !  PASS OVER THE INITIALLY-HARD SNAGS

     if (DEBUG)write(JOSTND,*)' NSNAGSALV= ',NSNAGSALV

     if ((ISTAND.ge.0).and.(ITYP.ne.0).and.(NSNAGSALV.gt.0)) then !SALVAGE
        do I = 1,NSNAGSALV
           XS = 0.
           XH = 0.
           ISPS = SPSSALV(I)
           XD = DBHSSALV(I)
           HTD = HTDEADSALV(I)
           if (DEBUG)write(JOSTND,*)' I,fmevmon-XD,HTD,HTDEAD,HTIHSALV,=',&
                &I,XD,HTDEADSALV(I),HTDEAD(I),HTIHSALV(I)

           LINCL = .false.

           if (JX.eq.0.or. JX.eq.ISPS) then
              LINCL = .true.
           else if (JX.lt.0) then
              IGRP = -JX
              IULIM = ISPGRP(IGRP,1)+1
              do IG = 2,IULIM
                 if (ISPS .eq. ISPGRP(IGRP,IG)) then
                    LINCL = .true.
                    exit
                 end if
              end do
           end if

           !  INITIALLY HARD SNAGS

           if (LINCL.and.&
                &(XD.ge.XLDBH .and. XD.lt.XHDBH).and.&
                &((HTIHSALV(I).ge.XLHT).and.&
                &(HTIHSALV(I).lt.XHHT))) then
              if ((SALVSPA(I,1)+SALVSPA(I,2)).gt.0.) then
                 HS = HTIHSALV(I)
                 TPA = SALVSPA(I,1)
                 if (TPA .gt.0.) then
                    call FMSVL2(ISPS,XD,HTD,HS,X1,0,&
                         &'D',.false.,DEBUG,JOSTND)
                    XH = TPA * X1
                 end if

                 !     PASS OVER THE INITIALLY-SOFT SNAGS

                 HS = HTISSALV(I)
                 TPA = SALVSPA(I,2)
                 if (TPA .gt.0. ) then
                    call FMSVL2(ISPS,XD,HTD,HS,X1,0,&
                         &'D',.false.,DEBUG,JOSTND)
                    XS = TPA * X1
                 end if

                 SNGSRM = SNGSRM+(XS+XH)*V2T(ISPS) !SNAG STEMS SALVAGED
                 !ASSUME THAT CROWNS
                 !STAY IN STAND
                 SNGSALVOL = SNGSALVOL+XS+XH !DEBUG VARIABLE

                 if (DEBUG) write(JOSTND,50) I,DENIH(I),HTIH(I),XD,XS,&
                      &DENIS(I),HTIS(I),XH,ISPS,V2T(ISPS),SNGSRM,SALVSPA(I,1),&
                      &SALVSPA(I,2)
                 50 format(' FMEVMON-SALVAGE:I=',I5,'DENIH,HTIH,XD,XS=',&
                      &4F10.3,' DENIS,HTIS,XH=',3F10.3/T18,'V2T=',&
                      &F6.3,' SNGSRM=',F10.4,' SALVSPA(I,1,2)= ',2F10.3)

              end if !TPA>0
           end if !LINCL - SPECIES
        end do !SALVAGE SNAG LOOP

     end if !SALVAGE

     if (DEBUG)write(JOSTND,*)'SNGSTM,SNGSRM,SNGVOL1,SNGSALVOL= ',&
          &SNGSTM,SNGSRM,SNGVOL1,SNGSALVOL

     !  ACCUMULATE LIVE TREE INFORMATION
     !  INITITIALIZE LOCAL VARIABLES

     LIVSRM = 0. !USER SELECED LIVE STEM WEIGHT REMOVED(TONS)
     LIVSRMTEM = 0. !DEBUG VARIABLE
     LIVSLV = 0. !USER SELECED LIVE STEM WEIGHT LEFT
     !IN STAND(TONS)
     LIVCRM = 0. !USER SELECED LIVE CROWN WEIGHT REMOVED(TONS)
     LIVCRN = 0. !USER SELECED LIVE CROWN WEIGHT (TONS)
     LIVFOL = 0. !USER SELECED FOLIAGE WEIGHT (TONS)
     TOTFOL = 0. !TOTAL FOLIAGE WEIGHT (TONS)
     LVCRNRMFOL = 0.

     !  LIVE REMOVALS

     if ((ICYCRM.eq.ICYC).and.(ISTAND.ge.0).and.(ITYP.ge.0)) then
        do I = 1,ITRNL

           ISPC = ISPCC(I)
           D = DBHC(I)
           H = HTC(I)

           LINCL = .false.
           if (JX.eq.0.or. JX.eq.ISPC) then
              LINCL = .true.
           else if (JX.lt.0) then
              IGRP = -JX
              IULIM = ISPGRP(IGRP,1)+1
              do IG = 2,IULIM
                 if (ISPC .eq. ISPGRP(IGRP,IG)) then
                    LINCL = .true.
                    exit
                 end if
              end do
           end if

           !  CONSTRAIN SPECIES

           if (LINCL) then
              XM1 = -1.

              !  LIVE TREES
              !  CONSTRAIN TO DBH RANGE

              if ((D.ge.XLDBH).and.(D.lt.XHDBH)) then

                 !  CONSTRAIN TO HEIGHT RANGE

                 if ((H.ge.XLHT).and.(H.lt.XHHT)) then
                    LMERCH = .false.
                    call FMSVL2(ISPC,D,H,XM1,VT,0,&
                         &'L',LMERCH,.false.,JOSTND)

                    if (DEBUG) write(JOSTND,60) I,FMPROB(I),PROB(I),&
                         &ISP(I),D,H,VT
                    60 format(' FMEVMON(LIVE): I=',I5,' FMPROB=',F10.3,&
                         &' PROB=',F10.3,' ISP=',I3,' D,H,VT=',3F10.3)

                    !  LIVE STEM

                    LIVSRMTEM = PREMST(I) * VT * V2T(ISPC)
                    LIVSRM = LIVSRM+LIVSRMTEM

                    if (DEBUG)write(JOSTND,*)' I,PREMST(I),FMPROB(I),',&
                         &'PROB(I)= ',PREMST(I),FMPROB(I),PROB(I)
                    if (DEBUG)write(JOSTND,*)' I,ISPC,LREMT,LIVSRM,LIVSLV= ',&
                         &I,ISPC,LREMT,LIVSRM,LIVSLV

                    !  LIVE CROWN

                    do J = 0,5
                       LIVCRM = LIVCRM+(CROWNWC(I,J))*P2T*PREMCR(I)
                    end do

                    if (DEBUG)write(JOSTND,*)' I,ISPC,LREMT,LIVCRM,LIVCRN= ',&
                         &I,ISPC,LREMT,LIVCRM,LIVCRN

                 end if !HEIGHT
              end if !DBH
           end if !SPECIES
        end do
     end if !LIVE REMOVALS

     !  STANDING LIVE

     if ((ISTAND.ne.0).and.(ITYP.ge.0)) then

        do I = 1,ITRN

           ISPC = ISP(I)
           D = DBH(I)
           H = HT(I)
           XM1 = -1.

           LINCL = .false.
           if (JX.eq.0.or. JX.eq.ISPC) then
              LINCL = .true.
           else if (JX.lt.0) then
              IGRP = -JX
              IULIM = ISPGRP(IGRP,1)+1
              do IG = 2,IULIM
                 if (ISPC .eq. ISPGRP(IGRP,IG)) then
                    LINCL = .true.
                    exit
                 end if
              end do
           end if

           !  CONSTRAIN SPECIES

           if (LINCL) then

              !  CONSTRAIN TO DBH RANGE

              if ((D.ge.XLDBH).and.(D.lt.XHDBH)) then

                 !  CONSTRAIN TO HEIGHT RANGE

                 if ((H.ge.XLHT).and.(H.lt.XHHT)) then
                    LMERCH = .false.

                    call FMSVL2(ISPC,D,H,XM1,VT,0,&
                         &'L',LMERCH,.false.,JOSTND)

                    if (DEBUG) write(JOSTND,61) I,FMPROB(I),PROB(I),&
                         &ISP(I),D,H,VT
                    61 format(' FMEVMON(LIVE): I=',I5,' FMPROB=',F10.3,&
                         &' PROB=',F10.3,' ISP=',I3,' D,H,VT=',3F10.3)

                    !  LIVE STEM

                    LIVSLV = LIVSLV+FMPROB(I) * VT * V2T(ISPC)

                    if (DEBUG)write(JOSTND,*)' I,PREMST(I),FMPROB(I),',&
                         &'PROB(I)= ',PREMST(I),FMPROB(I),PROB(I)
                    if (DEBUG)write(JOSTND,*)' I,ISPC,LREMT,LIVSRM,LIVSLV= ',&
                         &I,ISPC,LREMT,LIVSRM,LIVSLV

                    !  LIVE CROWN

                    do J = 0,5
                       LIVCRN = LIVCRN+(CROWNW(I,J))*P2T*FMPROB(I)
                    end do

                    !  LIVE FOLIAGE

                    TOTFOL = TOTFOL+CROWNW(I,0)*P2T*FMPROB(I)

                    if (DEBUG)write(JOSTND,*)' I,ISPC,LREMT,LIVCRM,LIVCRN,',&
                         &'TOTFOL= ',I,ISPC,LREMT,LIVCRM,LIVCRN,TOTFOL

                 end if !HEIGHT
              end if !DBH
           end if !SPECIES
        end do
     end if !LIVE STANDING

     if (DEBUG)write(JOSTND,*)' LIVSRM, LIVSLV, LIVCRN LVCRNRMFOL= ',&
          &LIVSRM,LIVSLV, LIVCRN,LVCRNRMFOL

     !  CALCULATE RETURN VARIABLES

     !  ISTAND TRREE STATUS -  STANDING(<0), REMOVED(0),OR BOTH(>0)
     !  ITYP  TREE TYPE -  DEAD(<0), LIVE(0),OR BOTH(>0)
     !  IPART  TREE PART -  STEM(<0), CROWN(0),OR BOTH-WHOLE TREE(>0)

     if (ISTAND.lt.0) then !STANDING
        if (ITYP.lt.0) then !DEAD ONLY
           if (IPART.lt.0)RVAL = SNGSTM !STEM ONLY
           if (IPART.eq.0)RVAL = SNGCRN !CROWN ONLY
           if (IPART.gt.0)RVAL = SNGSTM+SNGCRN !BOTH STEM AND CROWN
        else if (ITYP.eq.0) then !LIVE ONLY
           if (IPART.lt.0)RVAL = LIVSLV !STEM ONLY
           if (IPART.eq.0)RVAL = LIVCRN !CROWN ONLY
           if (IPART.gt.0)RVAL = LIVSLV+LIVCRN !BOTH STEM AND CROWN
           if (IPART.eq.2)RVAL = TOTFOL !FOLIAGE
        else if (ITYP.gt.0) then !BOTH LIVE AND DEAD
           if (IPART.lt.0)RVAL = LIVSLV+SNGSTM !STEM ONLY
           if (IPART.eq.0)RVAL = LIVCRN+SNGCRN !CROWN ONLY
           if (IPART.gt.0)RVAL = LIVSLV+LIVCRN+&
                &SNGSTM+SNGCRN !BOTH STEM AND CROWN
        end if !ITYP

     else if (ISTAND.eq.0) then !REMOVED
        if (ITYP.lt.0) then !DEAD ONLY
           if (IPART.lt.0)RVAL = SNGSRM !STEM ONLY
           if (IPART.eq.0)RVAL = 0. !+ SNGCRN ASSUME LEAVE SNG CR
           if (IPART.gt.0)RVAL = SNGSRM !+SNGCRN  ASSUME LEAVE SNG CR
        else if (ITYP.eq.0) then !LIVE ONLY
           if (IPART.lt.0)RVAL = LIVSRM !STEM ONLY
           if (IPART.eq.0)RVAL = LIVCRM !CROWN ONLY
           if (IPART.gt.0)RVAL = LIVSRM+LIVCRM !BOTH STEM AND CROWN
        else if (ITYP.gt.0) then !BOTH LIVE AND DEAD
           if (IPART.lt.0)RVAL = LIVSRM+SNGSRM !STEM ONLY
           if (IPART.eq.0)RVAL = LIVCRM !+SNGCRN -ASSUME LEAVE SNG CROWN
           if (IPART.gt.0)RVAL = LIVSRM+LIVCRM+& !BOTH STEM AND CROWN
                &SNGSRM !+SNGCRN ASSUME LEAVE SNG CROWN
        end if !ITYP

     else if (ISTAND.gt.0) then !BOTH STANDING & REMOVED
        if (ITYP.lt.0) then !DEAD ONLY
           if (IPART.lt.0)RVAL = SNGSTM+SNGSRM !STEM ONLY
           if (IPART.eq.0)RVAL = SNGCRN !CROWN ONLY
           if (IPART.gt.0)RVAL = SNGSTM+SNGCRN+SNGSRM !BOTH STEM AND CROWN
        else if (ITYP.eq.0) then !LIVE ONLY
           if (IPART.lt.0)RVAL = LIVSLV+LIVSRM !STEM ONLY
           if (IPART.eq.0)RVAL = LIVCRN+LIVCRM !CROWN ONLY
           if (IPART.gt.0)RVAL = LIVSLV+LIVSRM+& !BOTH STEM AND CROWN
                &LIVCRN+LIVCRM
        else if (ITYP.gt.0) then !BOTH LIVE AND DEAD
           if (IPART.lt.0)RVAL = LIVSLV+LIVSRM+& !STEM ONLY
                &SNGSTM+SNGSRM
           if (IPART.eq.0)RVAL = LIVCRN+SNGCRN+& !CROWN ONLY
                &LIVCRM
           if (IPART.gt.0)RVAL = LIVSLV+LIVSRM+& !BOTH STEM AND CROWN
                &LIVCRN+LIVCRM+&
                &SNGSTM+SNGSRM+SNGCRN
        end if !ITYP
     end if !ISTAND

     !  SET RETURN VARIABLE=0, IF F1=>0 OR F2=/0 AND F3=2

     if (((ISTAND.ge.0).or.(ITYP.ne.0)).and.(IPART.eq.2))RVAL = 0.

     if (DEBUG)write(JOSTND,*)' RVAL,ISTAND,ITYP,IPART,JX,XLDBH,',&
          &'XHDBH,XLHT,XHHT,IRC= ',RVAL,ISTAND,ITYP,IPART,JX,XLDBH,&
          &XHDBH,XLHT,XHHT,IRC
     if (DEBUG)write(JOSTND,*)' ITRN,SNGSRM,SNGCRN,LIVSRM,LIVCRM= ',&
          &ITRN,SNGSRM,SNGCRN,LIVSRM,LIVCRM

  end if ! IFMYR1

  return

  !     EVENT MONITOR POTSRATE FUNCTION.

  entry FMEVSRT(RVAL, II, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     RVAL = POTFSR(II) ! potential spread rate
  end if
  return

  !     EVENT MONITOR POTREINT FUNCTION.

  entry FMEVRIN(RVAL, II, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     RVAL = POTRINT(II) ! potential fire reaction intensity
  end if
  return

  !     EVENT MONITOR CARBSTAT FUNCTION.

  entry FMEVCARB(RVAL, II, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     RVAL = 0
     if (II .ge.1.and. II .le.17) RVAL = CARBVAL(II)
  end if
  return

  !     EVENT MONITOR DWDVAL FUNCTION.

  !     RVAL  VALUE RETURNED AFTER EVALUATING DOWN WOOD
  !     IX    INDEX FOR KIND OF RESULT: 1=VOLUME (CUFT/ACRE), 2=COVER (%)
  !     JX    HARD/SOFT INDEX: 0=BOTH, 1=HARD, 2=SOFT
  !     ILO   SIZE INDEX:    1=0-3", 2=3-6:, 3=6-12", 4=12-20", 5=20-35",
  !                          6=35-50", 7=50"+
  !     IHI   SIZE INDEX:    1=0-3", 2=3-6:, 3=6-12", 4=12-20", 5=20-35",
  !                           6=35-50", 7=50"+

  entry FMDWD(RVAL, IX, JX, ILO, IHI, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     RVAL = 0
     IRC = 0

     !     ADJUST THE VALUES BECAUSE THE INDEX IS 1 - 7, BUT THE ARRAY SPOTS
     !     ARE 1 - 9, WITH 0-3" COVERING 3 CATEGORIES.

     if (ILO .gt.1) ILO = ILO + 2
     if (IHI .ge.1) IHI = IHI + 2

     select case (IX)
     case (1) ! volume
        do I = ILO, IHI
           select case (JX)
           case(0) ! both
              RVAL = RVAL + CWDVOL(3,I,1,5) + CWDVOL(3,I,2,5)
           case(1) ! hard
              RVAL = RVAL + CWDVOL(3,I,2,5)
           case(2) ! soft
              RVAL = RVAL + CWDVOL(3,I,1,5)
           end select
        end do

     case (2) ! cover
        do I = ILO, IHI
           select case (JX)
           case(0) ! both
              RVAL = RVAL + CWDCOV(3,I,1,5) + CWDCOV(3,I,2,5)
           case(1) ! hard
              RVAL = RVAL + CWDCOV(3,I,2,5)
           case(2) ! soft
              RVAL = RVAL + CWDCOV(3,I,1,5)
           end select
        end do
     end select
  end if
  return

  !     EVENT MONITOR MAX CONIFER SNAG FUNCTION -- USED IN SUBROUTINE
  !     EVPRED TO COMPUTE THE FISHER HABITAT INDEX.

  entry FMEVMSN(RVAL)
  RVAL = 0.
  do I = 1,NSNAG
     if (NSNAG .lt.1) go to 110
     ISPC = SPS(I)
     if (LSW(ISPC)) then
        if (DBHS(I) .gt. RVAL) RVAL = DBHS(I)
     end if
     105 continue
  end do
  110 continue
  return

  !     EVENT MONITOR HERBSHRB FUNCTION (I.E. LIVE SURFACE FUELS)

  entry FMEVLSF(RVAL, II, IRC)
  if (IFMYR1.eq.-1) then
     IRC = 1
  else
     IRC = 0
     select case (II)
     case (1)
        RVAL = FLIVE(1) ! live herbs in tons/acre
     case (2)
        RVAL = FLIVE(2) ! live shrubs in tons/acre
     case (3)
        RVAL = FLIVE(1) + FLIVE(2) ! total herbs and shrubs in tons/acre
     end select
  end if
  return

end
