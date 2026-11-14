!===================!
!   wpbr : brtreg   !
!===================!
! WPBR $Id$
subroutine BRTREG
  implicit none

  !  Purpose:
  !  BRTREG drives the Blister Rust Model in the cycling process.

  !  Revision History:

  !  dd-MMM-YYYY programmer_name
  !     description of change or update.
  !  20-APR-1999 Lance R. David (FHTET)
  !     Syntax changes to eliminate warnings from Lahey FORTRAN 90 (LF90)
  !     compiler.
  !  13-MAY-1999 Lance R. David (FHTET)
  !     The height to base of crown (BRHTBC) calculation corrected to use
  !     FVS normal height and crown ratio for all trees, even topkilled.
  !     So, actual crown length is based on normal height and crown ratio
  !     less any topkill.
  !  25-MAY-1999 Lance R. David (FHTET)
  !     Added debug to activity processing.
  !     The check to ensure that the new height to base of crown calculated
  !     from FVS crown ratio is higher than the current height maintained
  !     within the model (BRHTBC) before resetting had been removed.
  !     I do not know why it was removed, but it eliminated the effects
  !     of pruning. The height to base of crown is only allowed to increase
  !     and since this model does not actually change the FVS crown ratio
  !     variable, this check must be in place.
  !  15-SEP-2000 Lance R. David (FHTET)
  !     Transfered Glen Brink's July, 2000 modifications from older version
  !     of blister rust source code:
  !     Modified to allow blister rust on other species using ISPBR array.
  !     Species loop (label 40) and species temp index variable (I3)
  !     are new.
  !  14-DEC-2000 Lance R. David (FHTET)
  !     Relocated call to BRIBA from within species loop to before loop.
  !     Added call to BGRI subroutine to update tree growth index- GI(x).
  !  06-MAR-2001 Lance R. David (FHTET)
  !     Added processing of pathological pruning option to PRUNE activity.
  !  15-MAR-2001 Lance R. David (FHTET)
  !     Added DEVFACT activity (code 1010) processing.
  !  22-MAR-2001 Lance R. David (FHTET)
  !     Changed processing structure so that all trees are processed each
  !     year of the cycle instead of processing a tree for all years of
  !     the cycle and then going to next tree. This enables annual
  !     monitoring of the stands infection level. Maximum stand infection
  !     level PIMAX calculation was added and PIMAX was also added to
  !     BRECAN parameter list. PIMAX is used for assigning escape (status
  !     code 9) trees and truncating stand infection level.
  !  24-APR-2001 Lance R. David (FHTET)
  !     Added species dimension to PRPSTK and RESIST arrays in Stock
  !     activity processing.
  !  02-MAY-2001 Lance R. David (FHTET)
  !     Added species dimension to calculations using BRNTRECS and
  !     category variables.
  !  09-MAY-2001 Lance R. David (FHTET)
  !     Changed ISPBR to BRSPM. Instead of just being and indicator of a
  !     species being a host, BRSPM holds the array index value for that
  !     species and is used to access all species-specific BR arrays.
  !  10-MAY-2001 Lance R. David (FHTET)
  !     Calculations of AVGRI and AVGGI were missing the cycle length IFINT
  !     in the denominator, it was added.
  !  15-MAY-2001 Lance R. David (FHTET)
  !     Added last year of cycle mortality accounting at stmt lable 36
  !  06-NOV-2002 Lance R. David
  !     Added call to BRICAL, new routine for calculating Rust Index.
  !  07-MAY-2013 Lance R. David (FMSC)
  !     Added statement label 410 for exit from routine when model
  !     is inactive.
  !  14-MAY-2014 Lance R. David (FMSC)
  !     BRECAN call modified by removal of RI(J) call parameter.

  !.... Common include files

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ARRAYS.f90'
  include 'PLOT.f90'
  include 'BRCOM.f90'

  !.... Local variable declarations

  ! Variables
  logical :: BRGO,DEBUG,LREDF
  real    :: CRATIO,HTBC,PRMS(6),RISUM(NBRSP),GISUM(NBRSP),BRDG,BRHIN,BRHNU,BRHT,BRHTOL,CANEXP,DNEW,&
       &PIMAX,GIBR,HNEW,PROP,REDFAC,STAR,STHT,TBSUM
  integer :: I1,I2,I3,I4,IACTK,ICLS,II,IIAG,J,K,KDT,L,MYACTS(10),NLCAN,NP,NTODO

  !.... Activities and related codes are as follows:
  !....
  !.... prune----1001   exspecs-1004   brtlst-1007   devfact-1010
  !.... prnspecs-1002   ribes---1005   brclst-1008
  !...  excise---1003   inact---1006   stock--1009

  data MYACTS/&
       &1001,1002,1003,1004,1005,1006,1007,1008,1009,1010/

  !.... See if Blister Rust Model is being used in this sumulation.
  !.... If not or there are no trees, return.

  call BRATV(BRGO)
  if (.not. BRGO .or. ITRN .eq.0) go to 410

  !.... Is debug requested?

  call DBCHK(DEBUG,'BRTREG',6,ICYC)
  if (DEBUG) write(JOSTND,10) ICYC
  10 format('Entering subroutine BRTREG: cycle = ',I2)

  !.... Initialize variables which control execution of activities.
  !.... Find out if any of the following activities have been scheduled
  !.... for this cycle:
  !.... PRUNE, PRSPEC, EXCISE, EXSPEC, RIBES, INACT, BRTLST, BRCLST,
  !.... STOCK, DEVFACT

  LPRGO = .false.
  LEXGO = .false.
  LPATPR = .false.
  LPRUN = .false.
  LCLEN = .false.
  BRTL = .false.
  BRCL = .false.
  LREDF = .false.

  !.... Find activities to be performed this cycle, if any.

  call OPFIND(10,MYACTS,NTODO)
  if (DEBUG) write(JOSTND,*) ' ACTIVITIES NTODO=',NTODO
  if (NTODO.gt.0) then
     do K = 1,NTODO

        !....       Get activity

        call OPGET(K,6,KDT,IACTK,NP,PRMS)

        if (DEBUG) write(JOSTND,*)' IACTK=',IACTK,' PRMS=',PRMS

        if (IACTK.eq.1002) go to 110
        if (IACTK.eq.1003) go to 115
        if (IACTK.eq.1004) go to 120
        if (IACTK.eq.1005) go to 125
        if (IACTK.eq.1006) go to 130
        if (IACTK.eq.1007) go to 135
        if (IACTK.eq.1008) go to 140
        if (IACTK.eq.1009) go to 145
        if (IACTK.eq.1010) go to 150

        !....       PRUNE keyword

        if (DEBUG) write(JOSTND,*) ' PRUNE: PRMS=',PRMS
        SRATE(1) = PRMS(1)
        LPRGO = .true.
        if (INT(PRMS(2)).eq.1) LPRUN = .true.
        if (INT(PRMS(3)).eq.1) LCLEN = .true.
        if (INT(PRMS(4)).eq.1) LPATPR = .true.
        go to 192

        !....       PRNSPECS keyword

        110 continue
        if (DEBUG) write(JOSTND,*) ' PRNSPECS: PRMS=',PRMS
        HTPRPR = PRMS(1)
        HTMAX(1) = PRMS(2)*30.48
        OUTDST = PRMS(3)*2.54
        OUTNLD = PRMS(4)*2.54
        go to 192

        !....       EXCISE keyword

        115 continue
        if (DEBUG) write(JOSTND,*) ' EXCISE: PRMS=',PRMS
        SRATE(2) = PRMS(1)
        LEXGO = .true.
        go to 192

        !....       EXSPECS keyword

        120 continue
        if (DEBUG) write(JOSTND,*) ' EXSPECS: PRMS=',PRMS
        EXDMIN = PRMS(1)
        HTMAX(2) = PRMS(2)*30.48
        GIRMAX = PRMS(3)
        GIRMRT = PRMS(4)
        HTMIN = PRMS(5)*2.54
        go to 192

        !....       RIBES keyword

        125 continue
        if (DEBUG) write(JOSTND,*) ' RIBES: PRMS=',PRMS
        RIBUS(1,1) = PRMS(1)
        RIBUS(2,1) = PRMS(2)
        RIBUS(1,2) = PRMS(3)
        RIBUS(2,2) = PRMS(4)
        RIBUS(1,3) = PRMS(5)
        RIBUS(2,3) = PRMS(6)

        !....       Call BRIBES to calculate the rust index reduction factor
        !....       based on change in number of ribes bushes or a new stand
        !....       rust index depending on whether a value for old number of
        !....       bushes/acre has been provided.  Either the new rust index or
        !....       reduction factor should remain in effect until changed by a
        !....       subsequent RIBES keyword.

        call BRIBES(REDFAC,LREDF)
        go to 192

        !....       INACT keyword

        130 continue
        if (DEBUG) write(JOSTND,*) ' INACT: PRMS=',PRMS
        RATINV(1) = PRMS(1)
        RATINV(2) = PRMS(2)
        go to 192

        !....       BRTLST keyword

        135 continue
        if (DEBUG) write(JOSTND,*) ' BRTLST'
        BRTL = .true.
        go to 192

        !....       BRCLST keyword

        140 continue
        if (DEBUG) write(JOSTND,*) ' BRCLST'
        BRCL = .true.
        go to 192

        !....       STOCK keyword
        !....             1-species, 2-stock type, 3-proportion, 4-resistance factor

        145 continue
        if (DEBUG) write(JOSTND,*) ' STOCK: PRMS=',PRMS
        I3 = PRMS(1)
        I4 = BRSPM(I3)
        ICLS = PRMS(2)
        PRPSTK(I4,ICLS) = PRMS(3)
        RESIST(I4,ICLS) = PRMS(4)
        go to 192

        !....       DEVFACT keyword

        150 continue
        I3 = PRMS(1)
        I4 = BRSPM(I3)
        DFACT(I4,1) = PRMS(2)
        DFACT(I4,2) = PRMS(3)
        DFACT(I4,3) = PRMS(4)
        DFACT(I4,4) = PRMS(5)

        !....       Done with that particular activity.

        192 continue
        call OPDONE(K,IY(ICYC))
        195 continue
     end do
  end if

  !.... If no tree records, then exit.

  if (ITRN.eq.0) go to 300

  !.... Initialize tree category variables to be used in BRTSTA.
  !.... Initialize other summing variables.

  do I3 = 1, MAXSP
     if (BRSPM(I3) .eq.0) go to 196
     I4 = BRSPM(I3)
     TBRHST(I4) = 0.0
     TBRCLN(I4) = 0.0
     TBRNOL(I4) = 0.0
     TBRPRN(I4) = 0.0
     TBREXC(I4) = 0.0
     TBRNOS(I4) = 0.0
     TBRGIR(I4) = 0.0
     TBRMRT(I4) = 0.0
     STSUM(I4) = 0.0
     RISUM(I4) = 0.0
     GISUM(I4) = 0.0
     196 continue
  end do

  do II = 1,10
     D2CLN(II) = 0.0
     D2WP(II) = 0.0
     D2NOL(II) = 0.0
     D2PRN(II) = 0.0
     D2EXC(II) = 0.0
     D2NOS(II) = 0.0
     D2GIR(II) = 0.0
     D2DED(II) = 0.0
     200 continue
  end do

  !.... Call BRCREM to remove cankers in this cycle.

  call BRCREM

  !.... Call BRSTYP to specify mix of stock types.

  call BRSTYP

  !.... Calculate Rust Index (RIDEF) using Basal Area.
  !.... The calculation of new rust index value is controlled by the
  !.... rust index assignment method (RIMETH) chosen for the run using
  !.... the RUSTINDX keyword.  RIMETH = 0 or 1 requests that RI be
  !.... set at initialization only. RIMETH = 2 requests that RI be
  !.... calculated every cycle based on the new stand BA.

  if (RIMETH.eq.2) call BRIBA

  !.... 06-NOV-2002
  !.... Calculate Rust Index based on exposure time (stand age).

  if (RIMETH .ge.3) call BRICAL

  !.... Calculated the infection level maximum. When the proportion
  !.... of infected trees reaches this point, clean trees will be
  !.... tagged as reserve/escape trees.

  PIMAX = 1.0-(EXP(-(100/(1+100*DFACT(1,1)))))

  if (DEBUG) then
     write(JOSTND,*) ' DFACT(1,1)=',DFACT(1,1),' PIMAX=',PIMAX
     write(JOSTND,*)&
          &' PITCA=',PITCA,' TRETN=',TRETN,' THPROB=',THPROB
  end if

  !.... Process host pines in the treelist.  If none, return.
  !.... Set number of host trees counter.
  !     BRNTRECS = 0

  !.... Start species loop

  do I3 = 1, MAXSP

     if (BRSPM(I3) .eq.0) go to 40

     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 40
     I2 = ISCT(I3,2)

     !.... Set blister rust species index
     I4 = BRSPM(I3)

     BRNTRECS(I4) = I2-I1+1

     !.... Process trees in the treelist on a yearly basis over the
     !.... cycle length period IFINT.

     !.... Start of year loop.
     do K = 1,IFINT

        !.... Start of tree loop.
        do L = I1,I2
           J = IND1(L)
           if (DEBUG) write(JOSTND,*) ' J=',J,' L=',L,' SPC=',BRSPC(I4)
           !....    Increment tree age.
           BRAGE(J) = BRAGE(J)+1.0
           NLCAN = ILCAN(J)
           BRDG = DG(J)*2.54

           !....    Number of expected cankers is initialized first year.
           if (K.eq.1) ESTCAN(J) = 0.0

           if (IBRSTAT(J).eq.7) then

              !....       Tree is dead and will not be included in the calculation
              !....       of averages for this cycle. Number of trees (divisor) is
              !....       reduced by 1.
              !....       Status code is temporarily set to 77 so that reduction to
              !....       counter does not occur again in later years.

              IBRSTAT(J) = 77
              BRNTRECS(I4) = BRNTRECS(I4)-1
              go to 35
           else if (IBRSTAT(J).eq.77) then
              go to 35
           end if

           !....    Assign rust index value to the tree including the resistance
           !....    factor for the stock type and the rust index adjustment factor
           !....    from initial tree conditions.

           ICLS = ISTOTY(J)
           RI(J) = RIDEF*RESIST(I4,ICLS)*RIAF(I4)

           if (DEBUG) write(JOSTND,*) ' RI=',RI(J),' RIDEF=',RIDEF,&
                &' RESIST=',RESIST(I4,ICLS),' RIAF=',RIAF(I4)

           !....    If a reduction factor has been calculated, apply it.

           if (LREDF) RI(J) = RI(J)*REDFAC

           GISUM(I4) = GISUM(I4)+GI(J)
           RISUM(I4) = RISUM(I4)+RI(J)

           if (DEBUG) write(JOSTND,*) ' LREDF=',LREDF,' REDFAC=',REDFAC,&
                &' GISUM=',GISUM(I4),' RISUM=',RISUM(I4)

           !....       Units of measure in following calculations are:
           !....       feet for HT, HTG, HNEW; inches for DBH, DG, DNEW;
           !....       meters for BRHT, BRHIN, BRHNU, BRHTOL, STHT;
           !....       Top killed trees have their heights calculated based
           !....       on the height to truncation.
           !....       ITRUNC is stored in hundreds of feet.

           PROP = FLOAT(K)/IFINT
           HNEW = HT(J)+(HTG(J)*PROP)
           DNEW = DBH(J)+(DG(J)*PROP)

           if (ITRUNC(J).eq.0) then
              BRHT = HT(J)*.3048
              BRHIN = HTG(J)*.3048
              BRHNU = BRHT+BRHIN
              BRHTOL = BRHT
              STHT = BRHTOL+(BRHIN*PROP)
           else
              BRHT = (FLOAT(ITRUNC(J))/100.0)*.3048
              BRHIN = 0.0
              BRHNU = BRHT
              BRHTOL = BRHT
              STHT = BRHT
           end if

           !....       Calculate height to base of crown from FVS crown ratio.
           !....       BRHTBC is in centimeters.
           !....       Crown ratio and length is based on normal tree height in FVS,
           !....       not truncated (topkill) height.
           !....       To facilitate the activity of pruning, a new height to crown base
           !....       is only applied when the new value calculated is greater than
           !....       the current value. It was decided by the developing group that
           !....       to actually change the crown ratio in FVS may have greater affect
           !....       than expected; therefore, BRHTBC is maintained in this manner.

           CRATIO = (FLOAT(ICR(J)))/100.0
           HTBC = (HNEW*30.48)*(1.0-CRATIO)
           if (BRHTBC(J) .lt. HTBC) BRHTBC(J) = HTBC

           !....       Call BRGI to calculate tree growth index valuethis year using the
           !....       tree's actual height and age at this point in time.
           !....       Only the GI value is utilized from this call.

           IIAG = BRAGE(J)
           call BRGI(IIAG,STHT,GIBR,TBSUM)
           GI(J) = GIBR

           !....       Call BRSTAR to calculate tree target for this year using the
           !....       tree's actual height at this point in time.
           !....       Accumulate individual tree targets and stand target.

           call BRSTAR(STHT,STAR)

           TSTARG(J) = TSTARG(J)+STAR
           STSUM(I4) = STSUM(I4)+TSTARG(J)

           !....       No more processing for escape/reserve trees.
           if (IBRSTAT(J).eq.9) go to 36

           !....       Call BRECAN to calculate # new cankers expected for tree
           !....       this year and accumulate for cycle.

           call BRECAN(J,BRHNU,STAR,STHT,PROP,PIMAX,CANEXP)
           ESTCAN(J) = ESTCAN(J)+CANEXP

           !....       Call BRCGRO to grow cankers this year.
           !....       Cankers added this cycle are not grown until next cycle;
           !....       i.e. BRCGRO will grow NLCAN cankers where NLCAN has been set
           !....       to what ILCAN was last cycle even though BRECAN has already
           !....       updated ILCAN for this cycle with the expected new cankers -
           !....       those will be grown next cycle, see???

           if (NLCAN.gt.0)&
                &call BRCGRO(J,PROP,BRHT,BRHIN,BRDG,NLCAN,HNEW,DNEW)

           !....       During the last year of the cycle, host trees that were
           !....       not killed by BR this cycle need to have the mortality
           !....       imposed by FVS or other model captured so that the total
           !....       historic mortality of the host species is accurate.
           !....       Also, the number of trees variable needs reduced for
           !....       trees killed during the last year of the cycle, the historical
           !....       mortality will have already been addressed in this case.

           36 if (K .eq. IFINT) then
              if (IBRSTAT(J) .eq.7) then
                 BRNTRECS(I4) = BRNTRECS(I4) - 1
              else
                 TBRHMR(I4) = TBRHMR(I4) + WK2(J)
                 if (DEBUG) write(JOSTND,*)&
                      &'TBRHMR=',TBRHMR(I4),' WK2=',WK2(J)
              end if
           end if

           !....    End of tree loop.
           35 continue
        end do

        !.... End of year loop.

        34 continue
     end do

     !.... End of species loop
     40 continue
  end do

  !.... The reset dead tree status codes (IBRSTAT).

  do I3 = 1, MAXSP
     if (BRSPM(I3) .eq.0) go to 60
     I1 = ISCT(I3,1)
     if (I1.eq.0) go to 60
     I2 = ISCT(I3,2)
     do L = I1,I2
        J = IND1(L)
        if (IBRSTAT(J) .eq.77) IBRSTAT(J) = 7
        55 continue
     end do
     60 continue
  end do

  !.... If a rust index reduction factor has been calculated, apply it
  !.... to the stand rust index.

  if (LREDF) RIDEF = RIDEF*REDFAC

  !.... Call BRCRED to reduce crowns on trees girdled by Blister Rust.

  call BRCRED

  !.... If all host pine have been killed, set values to 0.  Otherwise,
  !.... calculate average rust index, growth index, and stand target area.

  do I3 = 1, MAXSP
     if (BRSPM(I3).eq.0) go to 80
     !....    Set blister rust species index
     I4 = BRSPM(I3)
     if (BRNTRECS(I4).eq.0) then
        AVGRI(I4) = 0.0
        AVGGI(I4) = 0.0
        AVGSTS(I4) = 0.0
     else
        AVGRI(I4) = RISUM(I4)/FLOAT(BRNTRECS(I4)*IFINT)
        AVGGI(I4) = GISUM(I4)/FLOAT(BRNTRECS(I4)*IFINT)
        AVGSTS(I4) = STSUM(I4)/FLOAT(BRNTRECS(I4)*IFINT)
     end if
     80 continue
  end do

  !.... Call BRUPDT to update blister rust variables.

  call BRUPDT

  !.... Common return.

  300 continue
  if (DEBUG) write(JOSTND,400) ICYC
  400 format ('Leaving subroutine BRTREG: cycle = ',I2)
  410 return
end
