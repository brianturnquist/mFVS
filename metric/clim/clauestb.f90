!============================!
!   metric/clim : clauestb   !
!============================!
! METRIC-CLIM $Id$
subroutine CLAUESTB
  implicit none

  !     CLIMATE EXTENSION--AUTOMATIC ESTABLISHMENT ROUTINE

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'CLIMATE.f90'
  include 'METRIC.f90'

  ! Variables
  real    :: ALGSLP,TMAXTRS,XX,XMAX,PRMS(6),THISYR,PTREES,TTOADD,SPBA(MAXSP),SPTPA(MAXSP),SPIMP(MAXSP)
  integer :: I,I1,I2,ISPINDX(MAXSP),NSPEC,KODE,JOUT
  logical :: DEBUG

  integer :: MYACT(1),IDT,IACT,NP,ITODO,NTODO
  data MYACT/2802/

  call DBCHK (DEBUG,'CLAUESTB',8,ICYC)

  POTESTAB = 0.

  !     IF THE OPTION IS TURNED OFF, THEN JUST RETURN.

  if (.not.LCLIMATE) return

  call OPFIND(1,MYACT,NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET(ITODO,3,IDT,IACT,NP,PRMS)
        if (IACT.lt.0.or. NP.ne.3) cycle
        call OPDONE (ITODO,IY(ICYC))
        AESTOCK = PRMS(1)
        AESNTREES = PRMS(2)
        NESPECIES = IFIX(PRMS(3))
        LAESTB = .true.
     end do
  end if

  if (DEBUG) write (JOSTND,10) LCLIMATE,LAESTB,ICYC
  10 format ('IN CLAUESTB, LCLIMATE,LAESTB=',2L2,' ICYC=',I3)

  !     ARE WE BELOW AESTOCK PERCENT OF FULL STOCKING?

  call SDICAL (0,XMAX)
  XX = MAX(5.0,RMSQD)
  TMAXTRS = (XMAX/0.02483133)*(XX**(-1.605)) ! 0.02483133 = 10**(-1.605)

  !     PTREES IS THE PROPORTION OF TREES TO ESTABLISH AS A FUNCTION
  !     OF HOW LOW THE STOCKING IS (ANOTHER ADJUSTMENT IS BELOW).

  if (TMAXTRS.gt.1) then
     PTREES = 2. - (4.*(TPROB/TMAXTRS))
     if (PTREES.lt.0.) PTREES = 0.
     if (PTREES.gt.1.) PTREES = 1.
  else
     PTREES = 1.
  end if

  if (DEBUG) write (JOSTND,16) TPROB,PTREES,TMAXTRS,AESNTREES,&
       &NESPECIES
  16 format ('IN CLAUESTB, TPROB,PTREES,TMAXTRS,AESNTREES=',4F12.5,&
       &' NESPECIES=',I3)

  NSPEC = 0
  THISYR = FLOAT(IY(ICYC))+FINT/2

  if (PTREES*AESNTREES.gt.0.) then

     do I = 1,MAXSP
        if (INDXSPECIES(I).eq.0) then
           POTESTAB(I) = 0.
        else
           I2 = INDXSPECIES(I)
           POTESTAB(I) = ALGSLP(THISYR, FLOAT(YEARS),ATTRS(1,I2),NYEARS)
        end if
        if (DEBUG) write (JOSTND,30) I,JSP(I),INDXSPECIES(I),&
             &POTESTAB(I)
        30 format ('IN CLAUESTB, SP,INDXSPECIES,POTESTAB(I)=',&
             &I4,1X,A2,I3,F10.3)
     end do
     call RDPSRT(MAXSP,POTESTAB,ISPINDX,.true.)
     NSPEC = 0
     do I = 1,MAXSP
        if (POTESTAB(ISPINDX(I)).lt..40) exit
        NSPEC = NSPEC+1
     end do
  end if

  if (DEBUG) write (JOSTND,40) NSPEC
  40 format ('IN CLAUESTB, NSPEC=',I3)

  if (NSPEC.gt.0) then
     if (NSPEC.gt.NESPECIES) NSPEC = NESPECIES

     !       SCALE THE TOP NESPECIES POTESTAB SCORES

     do I = 1,NSPEC
        XX = -1. + 2.5*POTESTAB(ISPINDX(I))
        if (XX.lt.0.) XX = 0.
        if (XX.gt.1.) XX = 1.
        POTESTAB(ISPINDX(I)) = XX
     end do
     TTOADD = POTESTAB(ISPINDX(1))
     if (POTESTAB(ISPINDX(1)).gt.0.8) then
        TTOADD = AESNTREES
     else
        TTOADD = AESNTREES * POTESTAB(ISPINDX(1))
     end if
     if (DEBUG) write (JOSTND,41) POTESTAB(ISPINDX(1)),&
          &JSP(ISPINDX(1)),TTOADD,NSPEC,NESPECIES
     41 format ('IN CLAUESTB, MAX POTESTAB=',F7.3,' SP=',A2,&
          &' TTOADD=',F10.3,' NSPEC=',I2,' NESPECIES=',I2)

     XX = 0.
     do I = 1,NSPEC
        XX = XX+POTESTAB(ISPINDX(I))
     end do
     if (XX .gt.0.001) then
        do I = 1,NSPEC
           POTESTAB(ISPINDX(I)) = POTESTAB(ISPINDX(I))/XX
        end do
        do I = 1,MAXSP
           if (I.gt.NSPEC) then
              POTESTAB(ISPINDX(I)) = 0
           else
              XX = PTREES*TTOADD*POTESTAB(ISPINDX(I))
              if (XX.le.1.) XX = 0.
              POTESTAB(ISPINDX(I)) = XX
           end if
        end do
     else
        NSPEC = 0
     end if
  end if
  if (NSPEC.le.0) then
     POTESTAB = 0.
     ISPINDX = 1
  end if

  !     CURRENT STOCKING MUST BE BELOW THE THRESHOLD, IF NOT RETURN.

  if (DEBUG) write (JOSTND,42) TPROB,TMAXTRS,AESTOCK,&
       &TPROB .gt. TMAXTRS*AESTOCK*.01,JCLREF
  42 format ('IN CLAUESTB: TPROB,TMAXTRS,AESTOCK=',3F12.5,&
       &' WILL RETURN=',L2,' JCLREF=',I3)

  !     WRITE THE REPORT, SKIP IF JCLREF IS -1 (A FLAG SUPPRESSING THE OUTPUT).

  JOUT = 0
  if (JCLREF.ge.0) then
     call GETLUN(JOUT)
     if (JCLREF.eq.0) then
        call GETID (JCLREF)
        write (JOUT,'(1X,I5," $#*%")') JCLREF
        write (JOUT,43)
        43 format(/T8,'CLIMATE-FVS VERSION 2.0 VIABILITY',&
             &' AND EFFECTS REPORT'/,69('-')/T11,'SP VIA-',&
             &14X,'VIAB. dCLIM GROWTH SITE MXDEN AUTOESTB'/&
             &'YEAR SPEC BILITY  BA/HA   TPH   MORT ',&
             &' MORT MULT   MULT  MULT   TPH'/&
             &'---- ---- ------ ------ ------ ----- ',&
             &'----- ----- ----- ----- --------'/'$#*%')
     else
        write (JOUT,44) JCLREF
        44 format (I5,' ')
     end if
  end if

  SPBA = 0.
  SPTPA = 0.
  do I = 1,ITRN
     I1 = ISP(I)
     SPBA(I1) = SPBA(I1)+(DBH(I)*DBH(I)*PROB(I)*0.005454154)
     SPTPA(I1) = SPTPA(I1)+PROB(I)
  end do
  SPIMP = 0.
  XX = SUM(SPBA)
  if (XX.gt.0) SPIMP = SPBA/XX
  XX = SUM(SPTPA)
  if (XX.gt.0) SPIMP = SPIMP+(SPTPA/XX)
  XX = SUM(SPIMP)
  if (XX.gt.0) SPIMP = SPIMP/XX
  I2 = 0
  do I = 1,MAXSP
     if ((SPIMP(I).gt.0.05.or. SPVIAB(I).gt..4) .and.&
          &INDXSPECIES(I).gt.0) then
        I2 = 1
        if (JOUT.gt.0) then
           write (JOUT,45) JCLREF,IY(ICYC),JSP(I),SPVIAB(I),&
                &SPBA(I)*FT2pACRtoM2pHA,&
                &SPTPA(I)/ACRtoHA,&
                &SPMORT1(I),SPMORT2(I),SPGMULT(I),&
                &SPSITGM(I),MXDENMLT,POTESTAB(I)
           45 format (1X,I5,1X,I4,3X,A2,F7.3,F7.2,F7.1,5F6.3,F9.1)
        end if
        call DBSCLSUM(NPLT,IY(ICYC),JSP(I),SPVIAB(I),&
             &SPBA(I)*FT2pACRtoM2pHA,&
             &SPTPA(I)/ACRtoHA,&
             &SPMORT1(I),SPMORT2(I),SPGMULT(I),&
             &SPSITGM(I),MXDENMLT,POTESTAB(I))
     end if
  end do
  if (I2.eq.0) then
     if (JOUT.gt.0) then
        write (JOUT,45) JCLREF,IY(ICYC),"--",&
             &0.,0.,0.,0.,0.,0.,0.,MXDENMLT,0.
     end if
     call DBSCLSUM(NPLT,IY(ICYC),"--",&
          &0.,0.,0.,0.,0.,0.,0.,MXDENMLT,0.)
  end if

  if (.not.LAESTB) return
  if (TPROB .gt. TMAXTRS*AESTOCK*.01 ) return

  !     ADD PLANT ACTIVITIES, CODE IS  430

  I2 = 0
  do I = 1,NSPEC
     if (POTESTAB(ISPINDX(I)) .gt.0) then
        PRMS(1) = ISPINDX(I) ! species
        PRMS(2) = POTESTAB(ISPINDX(I)) ! trees/acre
        PRMS(3) = 100. ! % survival
        PRMS(4) = 0. ! age
        PRMS(5) = 0. ! height
        PRMS(6) = 0. ! shade code
        call OPADD (IY(ICYC+1)-1,431,0,6,PRMS,KODE)
        I2 = I2+1
        if (DEBUG) write (JOSTND,50) I,KODE,PRMS
        50 format ('IN CLAUESTB, I=',I2,' KODE=',I2,' PRMS=',6F8.3)
     else
        if (DEBUG) write (JOSTND,60) I,ISPINDX(I)
        60 format ('IN CLAUESTB, TOO FEW: I=',I2,'ISPINDX(I)=',I2)
     end if
  end do
  if (I2.gt.0) call OPINCR (IY,ICYC,NCYC)
  return
end

subroutine CLSPVIAB(ISP,VIA,IRC)
  implicit none

  !     CLIMATE EXTENSION--FETCH THE VIABILITY SCORE FOR SPECIES ISP

  !     ISP is the FVS species number, VIA will be the viability score
  !     for that species. If ISP is out of range, or if there is no
  !     score for the species, then IRC (the return code) is set to 1;
  !     otherwise it is set to 0.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'CLIMATE.f90'
  include 'PLOT.f90'

  ! Variables
  real    :: ALGSLP,THISYR,VIA
  integer :: ISP,IRC

  if (ISP.le.0.or. ISP.gt.MAXSP) then
     IRC = 1
     return
  end if
  if (INDXSPECIES(ISP).eq.0) then
     IRC = 1
     return
  end if
  THISYR = FLOAT(IY(ICYC))+(FINT/2)
  VIA = ALGSLP(THISYR, FLOAT(YEARS),&
       &ATTRS(1,INDXSPECIES(ISP)),NYEARS)
  IRC = 0
  return
end
