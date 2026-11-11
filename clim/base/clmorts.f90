!=========================!
!   clim/base : clmorts   !
!=========================!
! CLIM-BASE $Id$
subroutine CLMORTS
  implicit none

  !     CLIMATE EXTENSION - COMPUTES CLIMATE-CAUSED MORTALITY

  ! Common blocks
  include 'PRGPRM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'CLIMATE.f90'

  ! Variables
  integer :: I,I2
  real    :: THISYR,ALGSLP,X,XV,FYRMORT(MAXSP),CTHISYR(6),CBIRTH(6),DTV(6),DMORT,BIRTHYR,SPWTS(MAXSP),&
       &VS(2),SR(2)
  logical :: DEBUG,LDMORT
  data VS/.2,.5/,SR/0.,1./
  integer :: MYACT(1),IDT,IACT,NP,ITODO,NTODO
  real    :: PRMS(3)
  data MYACT/2801/

  call DBCHK (DEBUG,'CLMORTS',7,ICYC)

  if (DEBUG) write (JOSTND,1) LCLIMATE
  1 format ('IN CLMORTS, LCLIMATE=',L2,' ICYC=',I3)

  if (.not.LCLIMATE) return

  call OPFIND(1,MYACT,NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET(ITODO,3,IDT,IACT,NP,PRMS)
        if (IACT.lt.0.or. NP.ne.3) cycle
        call OPDONE (ITODO,IY(ICYC))
        I = IFIX(PRMS(1))
        if (I .eq.0) then
           CLMRTMLT1 = PRMS(2)
           CLMRTMLT2 = PRMS(3)
        else if (I.gt.0.and. I.le.MAXSP) then
           CLMRTMLT1(I) = PRMS(2)
           CLMRTMLT2(I) = PRMS(3)
        end if
     end do
  end if

  !     IF THIS IS THE FIRST CYCLE, THEN SET THE SPECIES PRESENCE CALIBRATION.

  if (ICYC.eq.1) then
     THISYR = FLOAT(IY(ICYC))
     do I = 1,MAXSP
        if (INDXSPECIES(I).eq.0) cycle
        I2 = INDXSPECIES(I)
        if (ISCT(I,1) .gt.0) then ! THE SPECIES IS PRESENT (THIS IS A FACT).
           SPCALIB(I) = &
                &ALGSLP (THISYR,FLOAT(YEARS),ATTRS(1,I2),NYEARS)*.9
        else
           SPCALIB(I) = -1. ! SIGNAL SPECIES WAS NOT PRESENT.
        end if
        if (DEBUG) write (JOSTND,5) I,JSP(I)(1:2),&
             &SPCALIB(I)
        5 format ('IN MORTS, I=',I3,' SP=',A2,' SPCALIB=',F10.4)
     end do
  end if

  SPMORT1 = 0.
  FYRMORT = 0.
  THISYR = FLOAT(IY(ICYC))+(FINT/2)
  do I = 1,MAXSP
     if (INDXSPECIES(I).eq.0) cycle
     I2 = INDXSPECIES(I)

     !       X IS THE SPECIES VIABILITY SCORE.

     XV = ALGSLP (THISYR,FLOAT(YEARS),ATTRS(1,I2),NYEARS)

     !       CONVERT XV TO A 10-YR SURVIVAL RATE. IF THE SPCALIB VALUE
     !       IS -1 (SPECIES WAS NOT IN ORIGINAL DATA), OR IF THE VALUE
     !       IS > .5 (PRESENT AND CONSISTENT WITH VIABLILITY SCORES), THEN
     !       SIMPLY USE THE BASIC SURVIVAL FUNCTION.

     if (SPCALIB(I).eq.-1..or. SPCALIB(I) .gt..5) then
        X = ALGSLP (XV,VS,SR,2)
     else
        X = SPCALIB(I)
        if (X.lt..1) X = .1
        X = ALGSLP (XV,VS * X * 2.,SR,2)
     end if

     !       CONVERT TO A MORTALITY RATE AND APPLY MULTIPLIER.
     !       SAVE THIS VERSION FOR REPORTING (10 YR).

     SPMORT1(I) = (1.-X)*CLMRTMLT1(I)

     !       CONVERT TO A FINT-YR SURVIVAL RATE.

     if (X.gt.1E-5) then ! VERY LOW SURVIVAL SET TO 0
        X = EXP(LOG(X)/10.)**FINT
        if (X.lt.0) then
           X = 0.
        else if (X.gt.1.0) then
           X = 1.0
        end if
     else
        X = 0.
     end if

     !       CONVERT TO A MORTALITY RATE AND APPLY MULTIPLIER.

     FYRMORT(I) = (1.-X)*CLMRTMLT1(I)

     if (DEBUG) write (JOSTND,10) IFIX(THISYR),I,JSP(I)(1:2),XV,X,&
          &CLMRTMLT1(I),CLMRTMLT2(I),SPMORT1(I),FYRMORT(I)
     10 format ('IN CLMORTS, THISYR=',I5,' I=',I3,1X,A3,' XVia=',&
          &2F10.4,' CLMRTMLT1&2=',2F10.4,' MORT=',2F10.4)
  end do

  !     COMPUTE MORTALITY BASED ON CLIMATE TRANSFER DISTANCE. FIRST
  !     COMPUTE THE CLIMATE METRICS FOR "THIS" YEAR. THE SECOND
  !     PART IS FOR THE BIRTH YEAR, AND IT MUST BE DONE WITHIN THE
  !     TREE LOOP.

  LDMORT = IDEmtwm.gt.0.and. IDEmtcm.gt.0.and.&
       &IDEdd5.gt.0.and. IDEsdi.gt.0.and.&
       &IDEdd0.gt.0.and. IDEmapdd5.gt.0.and.&
       &IXMTWM.gt.0.and. IXMTCM.gt.0.and.&
       &IXDD5.gt.0.and. IXDD0.gt.0.and.&
       &IXMAP.gt.0.and. IXGSP.gt.0.and. IXGSDD5.gt.0

  if (DEBUG) write (JOSTND,11) LDMORT,IDEmtwm,IDEmtcm,&
       &IDEdd5,IDEsdi,IDEdd0,IDEmapdd5,&
       &IXMTWM,IXMTCM,IXDD5,IXDD0,IXMAP,IXGSP,IXGSDD5
  11 format ('IN CLMORTS, LDMORT=',L2,13I3)

  if (LDMORT) then
     CTHISYR(1) = ALGSLP(THISYR,FLOAT(YEARS),ATTRS(1,IXMTWM),&
          &NYEARS)
     CTHISYR(2) = ALGSLP(THISYR,FLOAT(YEARS),ATTRS(1,IXMTCM),&
          &NYEARS)
     CTHISYR(3) = ALGSLP(THISYR,FLOAT(YEARS),ATTRS(1,IXDD5),&
          &NYEARS)
     !       Definition: sdi=sqrt(gsdd5)/gsp
     CTHISYR(4) = SQRT(ALGSLP(THISYR,FLOAT(YEARS),&
          &ATTRS(1,IXGSDD5),NYEARS))/&
          &ALGSLP(THISYR,FLOAT(YEARS),&
          &ATTRS(1,IXGSP),NYEARS)
     CTHISYR(5) = ALGSLP(THISYR,FLOAT(YEARS),ATTRS(1,IXDD0),&
          &NYEARS)
     !       Definition: mapdd5 = map*dd5/1000
     CTHISYR(6) = ALGSLP(THISYR,FLOAT(YEARS),ATTRS(1,IXMAP),&
          &NYEARS)*CTHISYR(3)/1000
     if (DEBUG) write (JOSTND,12) CTHISYR
     12 format ('IN CLMORTS, CTHISYR=',6F13.5)
  end if
  SPMORT2 = 0.
  SPWTS = 0.
  do I = 1,ITRN
     DMORT = 0.
     if (LDMORT) then
        BIRTHYR = THISYR-ABIRTH(I)
        CBIRTH(1) = ALGSLP(BIRTHYR,FLOAT(YEARS),ATTRS(1,IXMTWM),&
             &NYEARS)
        CBIRTH(2) = ALGSLP(BIRTHYR,FLOAT(YEARS),ATTRS(1,IXMTCM),&
             &NYEARS)
        CBIRTH(3) = ALGSLP(BIRTHYR,FLOAT(YEARS),ATTRS(1,IXDD5),&
             &NYEARS)
        !         Definition: sdi=sqrt(gsdd5)/gsp
        CBIRTH(4) = SQRT(ALGSLP(BIRTHYR,FLOAT(YEARS),&
             &ATTRS(1,IXGSDD5),NYEARS))/&
             &ALGSLP(BIRTHYR,FLOAT(YEARS),&
             &ATTRS(1,IXGSP),NYEARS)
        CBIRTH(5) = ALGSLP(BIRTHYR,FLOAT(YEARS),ATTRS(1,IXDD0),&
             &NYEARS)
        !         Definition: mapdd5 = map*dd5/1000
        CBIRTH(6) = ALGSLP(BIRTHYR,FLOAT(YEARS),ATTRS(1,IXMAP),&
             &NYEARS)*CBIRTH(3)/1000
        DTV = CTHISYR - CBIRTH
        if (ATTRS(1,IDEmtwm)  >0) then
           DTV(1) = DTV(1) / ATTRS(1,IDEmtwm)
        else
           DTV(1) = 1.
        end if
        if (ATTRS(1,IDEmtcm)  >0) then
           DTV(2) = DTV(2) / ATTRS(1,IDEmtcm)
        else
           DTV(2) = 1.
        end if
        if (ATTRS(1,IDEdd5)   >0) then
           DTV(3) = DTV(3) / ATTRS(1,IDEdd5)
        else
           DTV(3) = 1.
        end if
        if (ATTRS(1,IDEsdi)   >0) then
           DTV(4) = DTV(4) / ATTRS(1,IDEsdi)
        else
           DTV(4) = 1.
        end if
        if (ATTRS(1,IDEdd0)   >0) then
           DTV(5) = DTV(5) / ATTRS(1,IDEdd0)
        else
           DTV(5) = 1.
        end if
        if (ATTRS(1,IDEmapdd5)>0) then
           DTV(6) = DTV(6) / ATTRS(1,IDEmapdd5)
        else
           DTV(6) = 1.
        end if
        DMORT = (SUM(DTV)/6.)-1.1 ! COMPUTE THE AVERAGE, THEN TRANSLATE
        if (DMORT .lt.0)   DMORT = 0
        if (DMORT .gt.5.9) DMORT = 5.9
        DMORT = .9*(1-EXP(-(DMORT)**2.5))
        X = (DBH(I)*DBH(I)*PROB(I))
        SPMORT2(ISP(I)) = SPMORT2(ISP(I))+(DMORT*CLMRTMLT2(ISP(I))*X)
        SPWTS(ISP(I)) = SPWTS(ISP(I))+X
        DMORT = 1.-DMORT !CONVERT TO A SURVIVAL RATE
        if (DMORT.gt.1E-5) then ! VERY LOW SURVIVAL WILL BE 0
           DMORT = EXP(LOG(DMORT)/10.)**FINT
           if (DMORT.lt.0.) then
              DMORT = 0.
           else if (DMORT.gt.1.0) then
              DMORT = 1.0
           end if
        else
           DMORT = 0.
        end if
        ! CONVERT BACK TO A MORTALITY RATE AND APPLY SPECIES MULT.
        DMORT = (1.-DMORT)*CLMRTMLT2(ISP(I))
     end if

     ! X IS THE FVS PERIODIC MORTALITY RATE (FINT-YEARS)...
     if (PROB(I)-WK2(I) .le.1E-10) then
        X = 1.
     else if (PROB(I).lt.1E-10) then
        X = 1.
     else
        X = WK2(I)/PROB(I)
     end if
     if (DEBUG) write (JOSTND,15) I,JSP(ISP(I))(1:2),BIRTHYR,X,&
          &FYRMORT(ISP(I)),DMORT,LDMORT,WK2(I),PROB(I)
     15 format ('IN CLMORTS, I=',I4,' SP=',A3,' BIRTHYR=',F6.0,&
          &' FVSmort=',F10.4,' FYRMORT=',F10.4,' DMORT=',F10.4,&
          &' LDMORT=',L2,' WK2=',F10.5,' PROB=',F10.5)
     if (LDMORT.and.DEBUG) write (JOSTND,16) CBIRTH,CTHISYR,DTV
     16 format ('IN CLMORTS, CBIRTH =',6F13.5/&
          &'IN CLMORTS, CTHISYR=',6F13.5/&
          &'IN CLMORTS, DTV    =',6F13.5)
     ! USE THE MAX OF THE TWO RATES.
     if (FYRMORT(ISP(I)).gt.DMORT) DMORT = FYRMORT(ISP(I))
     if (DMORT.gt.X) WK2(I) = PROB(I)*DMORT
  end do

  !     COMPUTE THE WEIGHTED AVERAGE MORTALITY RATE FOR CAUSE 2

  do I = 1,MAXSP
     if (SPWTS(I).gt.0.0001) then
        SPMORT2(I) = SPMORT2(I)/SPWTS(I)
     else
        SPMORT2(I) = 0.
     end if
  end do

  return
end
