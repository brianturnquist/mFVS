!===================!
!   wsbwe : bwep3   !
!===================!
! WSBWE $Id$
subroutine BWEP3
  implicit none

  !  PRINT ANNUAL OUTPUT RE: BUDWORM MORTALITY & FEEDING
  !    ALSO PRINTS TABLE 5 - ANNUAL SUMMARY

  !  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR

  !   CALLED FROM: BWELIT

  !   SUBROUTINES AND FUNCTIONS CALLED:    NONE

  !   PARAMETERS:

  !   ITITLB - TITLE FOR BW MODEL
  !   IYRCUR - CURRENT YEAR
  !   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
  !   OUT3(IC,IH,I), FOR I: 1= INITIAL NO. OF EGGS; 2-7= % OF TOTAL
  !     (GENERATIONAL) MORTALITY DURING EGGS TO ESTABLISHED L2S (2),
  !     L2S TO MID-L4S (STARVATION =3, PREDATORS + OTHER =4),
  !     MID-L4S TO L6 (STARV.+DISPERSAL = 5, PRED.S + OTHER = 6),
  !     PUPAL PREDS. = 7); % DEFOLIATION OF NEW (=8) AND TOTAL (=9) FOLIAGE;
  !     AMT. OF NEW (=10) AND TOTAL (=11) FOLIAGE; AMOUNT OF NEW
  !     EGGS PRODUCED BY THIS GENERATION (=12); FINAL NO. OF ADULTS
  !     (=13); NO. OF FEMALES (=14); FEMALE WT. (=15);
  !     AND NO. EGGS PER FEMALE (=16);  % MORT. DUE TO SPRAY (=17);
  !     AND PARASITES (L2-4 =18, L4-6=19, P=20).

  !  Revision History:
  !    09-JUN-00 Lance David (FHTET)
  !      .Minor adjustment to annual summary (JOBWP5) header spacing.
  !    07-APR-2004 Lance David (FHTET)
  !      Corrected two misspellings of variable SUMEL2.
  !   02-JUN-2009 Lance R. David (FMSC)
  !      Added Stand ID and comma delimiter to output tables, some header
  !      and column labels modified.
  !   14-JUL-2010 Lance R. David (FMSC)
  !      Added IMPLICIT NONE and declared variables as needed.
  !   11-APR-2013 Lance R. David (FMSC)
  !      REAL variable DIFF was used in loop control at label 85 in error.
  !      Changed to variable IDIFF and removed REAL declaration of DIFF.
  !   19-SEP-2014 Lance R. David (FMSC)
  !      Changed variable named INDEX to INDX.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'BWECM2.f90'
  include 'BWEBOX.f90'
  include 'BWECOM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'

  ! Variables
  logical :: HEADER
  character(len=3) :: TRESP(6)
  character(len=3) :: sumt1
  character(len=3) :: sumt2
  integer :: IAPPB,IAPPC,IAPPD,IAPPE,IAPPF,IAPPG,IAPPH,IAPPI,IAPPJ,IC,ICROWN,IDIFF,IEGGS,IH,INDX,&
       &ISEL2,ISIZE,ISPAR,ISPNUM,ISPRE,ISSPR,ISSTV,IYEARK,K,KCROWN(18),LH,LSIZE,NEWDEF
  integer :: I,IYEAR,IYR,LASTYR
  real    :: APPB,APPC,APPD,APPE,APPF,APPG,APPH,APPI,APPJ,DEAD,DEFNEW,DEFOLD,EGGD,FECUND,GENMOR,&
       &OVWINT,RINC,RINC10,SPRING,SUMALE,SURV,TEMPH(20),TEMPO(20),TEMPS(20),X
  real    :: EGMASS,SNEWEG,SUMACT,SUMADT,SUMEGG,SUMEL2,SUMFEM,SUMFOL,SUMPAL,SUMPAP,SUMPAR,SUMPAS,&
       &SUMPOT,SUMPRE,SUMPRL,SUMPRP,SUMPRS,SUMSPR,SUMSTL,SUMSTS,SUMSTV

  data HEADER/.true./
  data TRESP/' WF',' DF',' GF','SAF',' ES',' WL'/
  data EGMASS/41.7/

  ! SOURCES: GPERM2 - SHEEHAN ET AL (1989) TABLE 9A (MEAN OF WF & DF,
  !   ALL THREE CROWN THIRDS WT'D EQUALLY); EGMASS - SHEEHAN ET AL
  !   (1989) TABLE 2

  do I = 1,20
     TEMPH(I) = 0.0
     TEMPO(I) = 0.0
     10 TEMPS(I) = 0.0
  end do

  SUMACT = 0.0
  SUMPOT = 0.0
  SUMFOL = 0.0
  SUMEGG = 0.0
  SUMEL2 = 0.0
  SUMSTV = 0.0
  SUMPRE = 0.0
  SUMPAR = 0.0
  SUMSPR = 0.0
  SNEWEG = 0.0
  SUMADT = 0.0
  SUMPRS = 0.0
  SUMPRL = 0.0
  SUMPRP = 0.0
  SUMPAS = 0.0
  SUMPAL = 0.0
  SUMPAP = 0.0
  SUMSTS = 0.0
  SUMSTL = 0.0
  SUMFEM = 0.0

  ! IF THIS IS THE FIRST CALL, PRINT THE HEADER - FIRST FOR
  !    TABLE 5, THEN FOR G_CROWN (LP2)

  if (HEADER .and. LP5) then
     write (JOBWP5,15) MGMIDB,ITITLB
     15 format (A4,' -- ',A72/)
     write (JOBWP5,40)
     40 format (51X,'% of total mortality',&
          &/,40X,41('-'),'  total'/,&
          &40X,'eggs   L2 to L4      L4 to L6',7X,'pupae  gener. ',&
          &' init     % defol.   rate    no.of ',/,40X,&
          &' to   ----------  ----------------  -----    %  ',&
          &'   egg     ----------   of     init',/,'STAND ID,',&
          &19X,'year, tree, ',&
          &' L2,  strv,  NE,  strv,  NE,  spry,  NE,    mort, ',&
          &' dens.,  new,  totl, incr.,   eggs,',/,&
          &'--------------------------, ----, ----, ',&
          &'----, ----, ----, ----, ----, ----, -----, -----, ------,',&
          &'  ----, ----, -----, --------,')
  else if (LP5) then
     write (JOBWP5,45)
     45 format (1X)
  end if

  if (HEADER .and. LP6) then
     IYR = IYRCUR-1
     write (JOBWP6,50) NPLT,IYR,IYR
     50 format ('STAND ID,',19X,&
          &' YEAR,  %_DEFO,  EGG_DN,  V_POOR,  V_GOOD, ',&
          &' OTHER,   YEAR,   EM_L2,   STARV,   PREDS,   PARAS,',&
          &'   SPRAY,   TOTAL,',/,&
          &A26,', ',I5,',',7X,'0,',7X,'0,',3(5X,'   ,'),1X,I5,',',&
          &6(7X,'0,'))
  end if
  if (HEADER .and. LP7) then
     IYR = IYRCUR-1
     write (JOBWP7,55) NPLT,IYR
     55 format ('STAND ID,',18X,&
          &' YEAR,   A_MALE,   P_PARA,   P_PRED,   L_PARA,',&
          &'   L_PRED,   L_STRV,   L_SPRA,   S_PRED,   S_STRV,   SPRING,',&
          &'   OVWINT, INIT_EGG,   R_INCR,   EGGS/F,',/,&
          &A26,',',I5,',',14('          '))
     LASTYR = IYR
  end if

  if (HEADER .and. LP2) then
     SUMT1 = ' DF'
     SUMT2 = ' GF'
     write (JOBWP2,60) SUMT1,SUMT2

     60 format (26X,',',2(A3,',  SMALL,',3(5X,','),&
          &'MEDIUM,    ,',2(5X,','),'LARGE,',2(5X,',')),/,&
          &'Stand ID,',18X,6(' Year,  Top,  Mid,  Bot,'))
     IYEAR = 0
     if (IYRCUR.gt.IY(1)) then
        IDIFF = IYRCUR-IY(1)
        IYR = IY(1)-1
        do I = 1,IDIFF
           IYEAR = IYEAR+1
           IYR = IYR+1
           write (JOBWP2,75) NPLT,IYR,IYR,IYR,IYR,IYR,IYR
           75 format (A26,',',6(1X,I4,',',3('    0,')))
           80 continue
        end do
     end if
  end if

  HEADER = .false.

  !  PRINT OUT THE WHOLE SHEBANG!

  LH = 0
  LSIZE = 0
  if (LP2) then
     IYEAR = IYEAR+1
     IYR = IYR+1
     if (IYR.lt.IYRCUR) then
        IDIFF = IYRCUR-IYR
        do I = 1,IDIFF
           write (JOBWP2,75) NPLT,IYR,IYR,IYR,IYR,IYR,IYR
           IYEAR = IYEAR+1
           IYR = IYR+1
           85 continue
        end do
     end if
  end if
  do K = 1,18
     KCROWN(K) = 0
     87 continue
  end do
  do IH = 1,6
     ISPNUM = 0
     if (TRESP(IH).eq.SUMT1) ISPNUM = 1
     if (TRESP(IH).eq.SUMT2) ISPNUM = 2

     !  TRANSLATE CROWN LEVEL INTO TREE SIZE AND CROWN THIRD

     do IC = 1,9
        if (IC.le.3) then
           ISIZE = 1
        else if (IC.le.6) then
           ISIZE = 2
        else
           ISIZE = 3
        end if
        ICROWN = MOD(IC,3)
        if (ICROWN.eq.0) ICROWN = 3
        if (OUT3(IC,IH,1).lt.1.0) goto 180
        if (LH.ne.IH.or.LSIZE.ne.ISIZE) then
           LH = IH
           LSIZE = ISIZE
        end if

        !  CALC. % BW MORTALITY AND % DEFOLIATION

        !  SUM FOR TABLE 5

        if (LP5.or. LP6.or. LP7) then
           TEMPH(1) = TEMPH(1)+OUT3(IC,IH,1)
           TEMPH(2) = TEMPH(2)+OUT3(IC,IH,2)
           TEMPH(3) = TEMPH(3)+OUT3(IC,IH,3)
           TEMPH(4) = TEMPH(4)+OUT3(IC,IH,4)
           TEMPH(5) = TEMPH(5)+OUT3(IC,IH,5)
           TEMPH(6) = TEMPH(6)+OUT3(IC,IH,6)
           TEMPH(7) = TEMPH(7)+OUT3(IC,IH,7)
           TEMPH(8) = TEMPH(8)+OUT3(IC,IH,8)
           TEMPH(9) = TEMPH(9)+OUT3(IC,IH,9)
           TEMPH(10) = TEMPH(10)+OUT3(IC,IH,10)
           TEMPH(11) = TEMPH(11)+OUT3(IC,IH,11)
           TEMPH(12) = TEMPH(12)+OUT3(IC,IH,12)
           TEMPH(13) = TEMPH(13)+OUT3(IC,IH,13)
           TEMPH(14) = TEMPH(14)+OUT3(IC,IH,14)
           TEMPH(15) = TEMPH(15)+(OUT3(IC,IH,15)*OUT3(IC,IH,14))
           TEMPH(16) = TEMPH(16)+(OUT3(IC,IH,16)*OUT3(IC,IH,14))
           TEMPH(17) = TEMPH(17)+OUT3(IC,IH,17)
           TEMPH(18) = TEMPH(18)+OUT3(IC,IH,18)
           TEMPH(19) = TEMPH(19)+OUT3(IC,IH,19)
           TEMPH(20) = TEMPH(20)+OUT3(IC,IH,20)
        end if

        !  PRINT NEW TABLE 2 (G_CROWN)

        if (LP2.and. ISPNUM .ne.0) then
           INDX = ((ISPNUM-1)*9)+IC
           DEFNEW = 0.0
           if (OUT3(IC,IH,10).gt.0.0) DEFNEW = 100.0*(1.0-(OUT3(IC,IH,8)&
                &/OUT3(IC,IH,10)))
           if (DEFNEW.gt.0.0) KCROWN(INDX) = NINT(DEFNEW)
        end if

        !  RESET THE ARRAY TO ZERO

        180 do I = 1,20
           OUT3(IC,IH,I) = 0.0
           190 continue
        end do
        200 continue
     end do

     !  TABLE 5: PRINT RESULTS FOR THIS HOST, THEN RESET SUMMARY VAR.S

     if (LP5.or. LP6.or. LP7) then
        if (TEMPH(1).lt.0.0) then
           write (JOBWP5,320) NPLT,IYRCUR
           320 format (' GAAACK! URP!! STAND ID= ',A26,' YEAR= ',I4)
        else if (TEMPH(1).gt.0.0) then
           SUMACT = SUMACT+TEMPH(8)
           SUMPOT = SUMPOT+TEMPH(10)
           SUMFOL = SUMFOL+TEMPH(11)
           SUMEGG = SUMEGG+TEMPH(1)
           SUMEL2 = SUMEL2+TEMPH(2)
           SUMSTV = SUMSTV+TEMPH(3)+TEMPH(5)
           SUMPRE = SUMPRE+TEMPH(4)+TEMPH(6)+TEMPH(7)
           SUMPAR = SUMPAR+TEMPH(18)+TEMPH(19)+TEMPH(20)
           SUMSPR = SUMSPR+TEMPH(17)
           SNEWEG = SNEWEG+TEMPH(12)
           SUMADT = SUMADT+TEMPH(13)
           SUMPRS = SUMPRS+TEMPH(4)
           SUMPRL = SUMPRL+TEMPH(6)
           SUMPRP = SUMPRP+TEMPH(7)
           SUMPAS = SUMPAS+TEMPH(18)
           SUMPAL = SUMPAL+TEMPH(19)
           SUMPAP = SUMPAP+TEMPH(20)
           SUMSTS = SUMSTS+TEMPH(3)
           SUMSTL = SUMSTL+TEMPH(5)
           SUMFEM = SUMFEM+TEMPH(14)

           if (LP5) then
              DEAD = TEMPH(1)-TEMPH(13)

              if (DEAD.gt.0.0) then
                 TEMPH(2) = 100.0*TEMPH(2)/DEAD
                 TEMPH(3) = 100.0*TEMPH(3)/DEAD
                 TEMPH(4) = 100.0*(TEMPH(4)+TEMPH(18))/DEAD
                 TEMPH(5) = 100.0*TEMPH(5)/DEAD
                 TEMPH(6) = 100.0*(TEMPH(6)+TEMPH(19))/DEAD
                 TEMPH(7) = 100.0*(TEMPH(7)+TEMPH(20))/DEAD
                 TEMPH(17) = 100.0*TEMPH(17)/DEAD
              else
                 do K = 2,7
                    TEMPH(K) = -0.0
                    350 continue
                 end do
                 TEMPH(17) = -0.0
              end if
              GENMOR = 100.0*DEAD/TEMPH(1)
              if (TEMPH(14).gt.0.0) then
                 TEMPH(15) = TEMPH(15)/TEMPH(14)
                 TEMPH(16) = TEMPH(16)/TEMPH(14)
              else
                 TEMPH(15) = 0.0
                 TEMPH(16) = 0.0
              end if
              DEFNEW = -1.0
              if (TEMPH(10).gt.0.0) DEFNEW = 100.0*(1.0-(TEMPH(8)&
                   &/TEMPH(10)))
              DEFOLD = -1.0
              if (TEMPH(11).gt.0.0) DEFOLD = 100.0*(1.0-(TEMPH(9)&
                   &/TEMPH(11)))
              RINC = TEMPH(12)/TEMPH(1)
              EGGD = (TEMPH(1)*GPERM2)/(EGMASS*TEMPH(11))
              write (JOBWP5,380) NPLT,IYRCUR,TRESP(IH),(TEMPH(K),K = 2,6),&
                   &TEMPH(17),TEMPH(7),GENMOR,EGGD,DEFNEW,DEFOLD,RINC,&
                   &TEMPH(1)
              380 format    (A26,',',1X,I4,',',1X,A3,',',1X,3(F5.1,','),&
                   &3(F5.1,','),F5.1,',',2X,F5.1,',',2X,&
                   &F5.1,',',1X,F5.0,',',F5.0,',',F6.2,',',F9.0,',')
           end if
           do I = 1,20
              390 TEMPH(I) = 0.0
           end do
        end if
     end if

     300 continue
  end do

  !  PRINT STRIPPED OUTPUT TABLE FOR GRAPHICS DISPLAY

  if (LP6) then

     !        APPARENT SURV. CALCS

     SURV = SUMEGG
     if (SURV.gt.0.0) then
        APPB = 100.0-(SUMEL2*100.0/SURV)
        IAPPB = NINT(APPB)
        RINC = SNEWEG/SUMEGG
        if (SUMFEM.gt.0.0) then
           FECUND = SNEWEG/SUMFEM
        else
           FECUND = 0.0
        end if
     else
        IAPPB = 0
        RINC = 0.0
     end if
     SURV = SURV-SUMEL2
     if (SURV.gt.0.0) then
        APPC = 100.0-(SUMSTS*100.0/SURV)
        IAPPC = NINT(APPC)
        APPD = 100.0-(SUMPRS*100.0/SURV)
        IAPPD = NINT(APPD)
     else
        IAPPC = 0
        IAPPD = 0
     end if
     SURV = SURV-SUMSTS-SUMPRS
     if (SURV.gt.0.0) then
        APPE = 100.0-(SUMSTL*100.0/SURV)
        IAPPE = NINT(APPE)
        APPF = 100.0-(SUMPRL*100.0/SURV)
        IAPPF = NINT(APPF)
        APPG = 100.0-(SUMPAL*100.0/SURV)
        IAPPG = NINT(APPG)
        APPH = 100.0-(SUMSPR*100.0/SURV)
        IAPPH = NINT(APPH)
     else
        IAPPE = 0
        IAPPF = 0
        IAPPG = 0
        IAPPH = 0
     end if
     SURV = SURV-SUMSTL-SUMPRL-SUMPAL-SUMSPR
     if (SURV.gt.0.0) then
        APPI = 100.0-(SUMPRP*100.0/SURV)
        IAPPI = NINT(APPI)
        APPJ = 100.0-(SUMPAP*100.0/SURV)
        IAPPJ = NINT(APPJ)
     else
        IAPPI = 0
        IAPPJ = 0
     end if

     !        actual mortality calculations

     IEGGS = NINT(SUMEGG)
     if (SUMPOT.gt.0.0) then
        X = 100*(1.0-(SUMACT/SUMPOT))
     else
        X = 0.0
     end if
     NEWDEF = NINT(X)
     if (EGMASS*SUMFOL.gt.0.0) then
        EGGD = (SUMEGG*GPERM2)/(EGMASS*SUMFOL)
     else
        EGGD = 0.0
     end if
     DEAD = SUMEGG-SUMADT
     if (DEAD.gt.0.0) then
        SUMEL2 = 100.0*SUMEL2/DEAD
        SUMSTV = 100.0*SUMSTV/DEAD
        SUMPRE = 100.0*SUMPRE/DEAD
        SUMPAR = 100.0*SUMPAR/DEAD
        SUMSPR = 100.0*SUMSPR/DEAD
     else
        SUMEL2 = -0.0
        SUMSTV = -0.0
        SUMPRE = -0.0
        SUMPAR = -0.0
        SUMSPR = -0.0
     end if
     RINC10 = -0.0
     if (SUMEGG.gt.0.0) RINC10 = 10.0*SNEWEG/SUMEGG
     ISEL2 = NINT(SUMEL2)
     ISSTV = NINT(SUMSTV)
     ISPRE = NINT(SUMPRE)
     ISPAR = NINT(SUMPAR)
     ISSPR = NINT(SUMSPR)

     write (JOBWP6,600) NPLT,IYRCUR,NEWDEF,EGGD,(IOUT6A(I),I = 1,3),&
          &IYRCUR,ISEL2,ISSTV,ISPRE,ISPAR,ISSPR,RINC10
     600 format (A26,', 'I5,',',2X,I6,',',2X,F6.1,',',3(5X,A3,','),&
          &1X,I5,',',5(5X,I3,','),2X,F6.2,',')
  end if

  !  PRINT MORTALITY/SURVIVAL SUMMARY FILE (FOR EXPORT)

  if (LP7) then
     if (LASTYR+1.eq.IYRCUR) then
        LASTYR = IYRCUR
     else
        IDIFF = IYRCUR-LASTYR-1
        do I = 1,IDIFF
           IYEARK = LASTYR+I
           write (JOBWP7,610) NPLT,IYEARK
           610 format (A26,',',I5,',',14('          '))
           620 continue
        end do
        LASTYR = IYRCUR
     end if
     SUMALE = SUMADT-SUMFEM
     X = FWSURV*WCOLDW
     OVWINT = SUMEGG*(1.0-X)
     SPRING = SUMEL2-OVWINT
     RINC = 0.0
     if (SUMEGG.gt.0.0) RINC = SNEWEG/SUMEGG
     FECUND = 0.0
     if (SUMFEM.gt.0.0) FECUND = SNEWEG/SUMFEM
     write (JOBWP7,640) NPLT,IYRCUR,SUMALE,SUMPAP,SUMPRP,SUMPAL,&
          &SUMPRL,SUMSTL,SUMSPR,SUMPRS,SUMSTS,SPRING,OVWINT,SUMEGG,&
          &RINC,FECUND
     640 format (A26,',',I5,',',14(F9.1,','))
  end if

  !  PRINT NEW TABLE 2 (G_CROWN) FOR THIS YEAR

  if (LP2) write (JOBWP2,650) NPLT,IYRCUR,(KCROWN(K),K = 1,3),&
       &IYRCUR,(KCROWN(K),K = 4,6),IYRCUR,(KCROWN(K),K = 7,9),IYRCUR,&
       &(KCROWN(K),K = 10,12),IYRCUR,(KCROWN(K),K = 13,15),IYRCUR,&
       &(KCROWN(K),K = 16,18)
  650 format (A26,',',6(1X,I4,',',3(2X,I3,',')))

  return
end
