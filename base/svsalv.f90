!=================================!
!   Stand visualization: SVSALV   !
!=================================!
! BASE $Id$
subroutine SVSALV(IYR,MINDBH,MAXDBH,MAXAGE,OKSOFT,PROP,PROPLV)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     S.N.SCHAROSCH -- Abacus -- NOV 2007

  !   Purpose:
  !     This routine removes snags from the base FVS snag list, in
  !     response to a salvage operation done by the FFE, as specified
  !     via SALVAGE/SALVSP keywords.

  !   Called from: FMSALV, once at beginning of each cycle.

  !   Local variable definitions:
  !     ISALFG:  Array of salvage flags for base FVS snag records:
  !                 ISALFG(i)=0: snag not available for salvage
  !                 ISALFG(i)=1: snag available for salvage
  !                 ISAlFG(i)=2: snag felled and salvaged
  !                 ISAlFG(i)=3: snag felled and culled
  !     IYR:     Current year
  !     LINCL:   True if current snag record meets current species selection
  !              criteria for salvage.
  !     MAXDBH:  MAXimum DBH of snags to be salvaged
  !     MAXAGE:  MAXimum AGE (years since death) of snags to be salvaged
  !     MINDBH:  MINimum DBH of snags to be salvaged
  !     OKSOFT:  Whether it's OK to salvage SOFT snags.
  !              (Note: OKSOFT: 0=all snags, 1=hard only, 2=soft only)
  !     PROP:    PROPortion of the eligible snags to actually salvage
  !     PROPLV:  PROPortion of the eligible snags to LeaVe in the stand
  !              as down material.

  !   Common variable definitions:
  !     ISALVS:  the sp/spgroup for which a specific cut/leave switch
  !              was set via SALVSP keyword.
  !     ISALVC   the cut/leave switch which was set for a specific
  !              sp/spgroup, via the SALVSP keyword:
  !              0 = cut this sp/spgroup in a salvage cut;
  !              1 = leave this sp/spgroup in a salvage cut

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'CONTRL.f90'
  include 'FMCOM.f90'
  include 'SVDATA.f90'
  include 'SVDEAD.f90'

  ! Variables
  integer :: B4SNAGS,B4SVSNG,EL_SNAGS,I,IG,IGRP,ISALFG(MXDEAD),ISREF(MXDEAD),IT,IULIM,IYR,J,NOTFELD, &
       & NWSVSNG,OKSOFT
  logical :: DEBUG,LINCL
  real    :: CULLDIF,CULLPCT,CULLTPA,EL_CULL,EL_FELL,FELLDIF,FELLPCT,FELLTPA,MAXDBH,MAXAGE,MINDBH, &
       & OVRPCT,PROP,PROPLV,UNDPCT,X

  !  Check for debug:

  call DBCHK (DEBUG,'SVSALV',6,ICYC)

  if (DEBUG) then
     write (JOSTND,1000) ICYC, &
          & ISALVS, ISALVC, MINDBH, &
          & MAXDBH, MAXAGE, OKSOFT, &
          & PROP, PROPLV, NDEAD, &
          & NSVOBJ
     1000 format (' ','IN SVSALV, ICYC=',I5,':', / , &
          & ' ',T5,'ISALVS=',I2,', ISALVC=',I2,', MINDBH=',F6.2, &
          & ', MAXDBH=',F6.2,', MAXAGE=',F6.2,', OKSOFT=',I1, &
          & ', PROP=',F4.2,', PROPLV=',F4.2,', NDEAD=',I4, &
          & ', NSVOBJ=',I5, // , &
          & ' ',T5,'SNAG LIST BEFORE SALVAGE:', // , &
          & ' ',T5,'  I  SPP SNGDIA OLEN IYRCOD STATUS FALLDIR',/, &
          & ' ',T5,'---- --- ------ ---- ------ ------ -------' )
     !                       XXXX XXX XXX.XX XXX. XXXXXX XXXXXX XXXXXX.
     do I = 1,NDEAD
        write(JOSTND,1010) I, ISNSP(I), SNGDIA(I), OLEN(I), &
             & IYRCOD(I), ISTATUS(I), FALLDIR(I)
        1010 format(' ',T5,I4,1X,I3,1X,F6.2,1X,F4.0,1X, &
             & I6,1X,I6,1X,F7.0)
        20 continue
     end do
  end if

  if (NDEAD.eq.0) return

  !  Compute:
  !    1) EL_SNAGS: number of eligible snags for salvaging, according to
  !                 selection criteria from SALVAGE keyword.
  !    2) EL_FELL: number of eligible snags to cut, according to PROP
  !                field of SALVAGE keyword.
  !    3) EL_CULL: number of felled snags to leave in stand, according to
  !                PROPLV field of SALVAGE keyword.
  !  For tallying, snag must meet following selection criteria:
  !    1) Species has not been excluded from salvage via a SALVSP keyword.
  !    2) DBH is within specified SALVAGE range.
  !    3) Snag age is less than specified maximum SALVAGE age.
  !    4) Snag condition (hard/soft) meets specified SALVAGE criteria.
  !    5) Snag is still standing.
  !  Snag records represent whole trees, so compute the eligible snag
  !  counts as the desired percentage times the number of applicable
  !  snag records.

  EL_SNAGS = 0
  do I = 1,NDEAD
     ISALFG(I) = 0
     LINCL = .false.
     if (ISALVS.eq.0 .or. ISALVS.eq.ISNSP(I)) then
        LINCL = .true.
     else if (ISALVS.lt.0) then
        IGRP = -ISALVS
        IULIM = ISPGRP(IGRP,1)+1
        do IG = 2,IULIM
           if (ISNSP(I) .eq. ISPGRP(IGRP,IG)) then
              LINCL = .true.
              go to 70
           end if
           60 continue
        end do
     end if
     70 continue

     if (ISALVC.eq.0 .and. .not.LINCL) go to 100
     if (ISALVC.eq.1 .and. LINCL) go to 100
     if (SNGDIA(I).lt.MINDBH .or. SNGDIA(I).gt.MAXDBH) go to 100
     if ((IYR-IYRCOD(I)).gt.MAXAGE) go to 100
     if (OKSOFT.eq.1 .and. ISTATUS(I).eq.4) go to 100
     if (OKSOFT.eq.2 .and. ISTATUS(I).ne.4) go to 100
     if (FALLDIR(I).ne.-1) go to 100

     ISALFG(I) = 1
     EL_SNAGS = EL_SNAGS + 1
     100 continue
  end do

  EL_FELL = EL_SNAGS * PROP
  EL_CULL = EL_FELL * PROPLV
  B4SNAGS = NDEAD
  B4SVSNG = 0
  do I = 1,NSVOBJ
     if (IOBJTP(I).eq.2) B4SVSNG = B4SVSNG+1
     110 continue
  end do

  if (DEBUG) then
     write (JOSTND,1020) EL_SNAGS, EL_FELL, EL_CULL
     1020 format (/,' ',T5,'EL_SNAGS=',I4,', EL_FELL=',F5.0, &
          & ', EL_CULL=',F5.0)
  end if

  if (EL_SNAGS.eq.0.0) return

  !  Loop back through the base FVS snag list, and process snags that match
  !  FFE SALVAGE operation.
  !  Since salvage operations target merchantable volume, process snags
  !  in order of decreasing DBH (no preference given to hard vs soft).

  call RDPSRT(NDEAD,SNGDIA,ISREF,.true.)
  FELLTPA = 0.0
  120 continue

  do I = 1,NDEAD
     IT = ISREF(I)
     if (ISALFG(IT).ne.1) go to 200

     !  Snag record meets selection criteria.
     !  Snag records each represent one tree/ac; but SALVAGE keyword allows
     !  for the simulation of partial snag removal. Therefore, we need to
     !  stochastically fell snag records until we hit the desired residual
     !  snag density.

     call SVRANN(X)
     if (X.le.PROP) then
        FELLTPA = FELLTPA + 1

        !  If removing this snag record puts us over the desired salvage
        !  proportion, test to see if we're better off leaving it.

        if ((FELLTPA/FLOAT(EL_SNAGS)).gt.PROP) then
           OVRPCT = (FELLTPA/FLOAT(EL_SNAGS)) - PROP
           UNDPCT = ((FELLTPA-1)/FLOAT(EL_SNAGS)) - PROP
           if (DEBUG) then
              write (JOSTND,1030) OVRPCT, UNDPCT
              1030 format (' ',T5,'OVRPCT=',F8.4,', UNDPCT=',F8.4)
           end if
           if (OVRPCT.gt.ABS(UNDPCT)) then
              FELLTPA = FELLTPA - 1
              go to 210
           end if
        end if

        !  Snag record selected for felling.

        ISALFG(IT) = 2
     end if
     FELLDIF = (FELLTPA/FLOAT(EL_SNAGS)) - PROP
     if (FELLDIF.gt.0.0 .or. ABS(FELLDIF).le.0.005) go to 210
     200 continue
  end do

  !  If, by random chance, we did not select enough snags to meet
  !  desired salvage proportion, go back through the snag list again.

  if ((FELLTPA/FLOAT(EL_SNAGS)).lt.PROP) then
     if (DEBUG) then
        write (JOSTND,1070) FELLDIF
        1070 format (' ',T5,'INSUFFICIENT SNAG REMOVAL.', &
             & 'FELLDIF=',F6.2,'. TRYING AGAIN...')
     end if
     go to 120
  end if

  210 continue

  !  Go back through the snags that have been selected for felling,
  !  and stochastically determine which get salvaged, and which get
  !  culled.

  CULLTPA = 0.0
  220 continue

  do I = 1,NDEAD
     IT = ISREF(I)
     if (ISALFG(IT).ne.2) go to 300
     call SVRANN(X)
     if (X.le.PROPLV) then
        CULLTPA = CULLTPA + 1

        !  If culling this snag record puts us over the desired cull
        !  proportion, test to see if we're better off leaving it.

        if ((CULLTPA/FELLTPA).gt.PROPLV) then
           OVRPCT = (CULLTPA/FELLTPA) - PROPLV
           UNDPCT = ((CULLTPA-1)/FELLTPA) - PROPLV
           if (DEBUG) then
              write (JOSTND,1030) OVRPCT, UNDPCT
           end if
           if (OVRPCT.gt.ABS(UNDPCT)) then
              CULLTPA = CULLTPA - 1
              go to 310
           end if
        end if
        ISALFG(IT) = 3
        if (DEBUG) then
           write (JOSTND,1080) IT, SNGDIA(IT), CULLTPA
           1080 format (' ',T5,'SELECTED TO FELL & LEAVE, SNAG:',I4, &
                & ', SNGDIA(IT):',F5.2,', CULLTPA:',F6.0)
        end if
        call SVRANN(X)
        FALLDIR(IT) = IFIX((360.0*X) + 0.5)
     end if
     CULLDIF = (CULLTPA/FELLTPA) - PROPLV
     if (CULLDIF.gt.0.0 .or. ABS(CULLDIF).le.0.005) go to 310
     300 continue
  end do

  !  If, by random chance, we did not select enough snags to meet
  !  desired cull proportion, go back through the snag list again.

  if ((CULLTPA/FELLTPA).lt.PROPLV) then
     if (DEBUG) then
        write (JOSTND,1090)
        1090 format (' ',T5,'INSUFFICIENT SNAG CULLING, TRYING AGAIN...')
     end if
     go to 220
  end if

  310 continue

  !  For snags being felled and salvaged, flag the record for
  !  removal from the base FVS snag arrays, and the SVS snag objects.
  !  Don't actually remove the snags yet, since they need to be
  !  displayed as downed snags in the post thin/salvage SVS display.
  !  Set IOBJTP to 5 to signal the salvaged snags will be removed
  !  after the next call to SVOUT.

  do I = 1,NDEAD
     if (ISALFG(I).eq.2) then
        ISTATUS(I) = ISTATUS(I) * (-1)
        do J = 1,NSVOBJ
           if (IOBJTP(J).eq.2 .and. IS2F(J).eq.I) then
              IOBJTP(J) = 5
              exit
           end if
           350 continue
        end do
     end if
     400 continue
  end do

  if (DEBUG) then
     write (JOSTND,1100) NDEAD, NSVOBJ
     1100 format (/, &
          & ' ',T5,'LEAVING SVSALV. NDEAD=',I4,', NSVOBJ=',I5, // , &
          & ' ',T5,'SNAG LIST AFTER SALVAGE:', // , &
          & ' ',T5,'  I  SPP SNGDIA OLEN IYRCOD STATUS FALLDIR',/, &
          & ' ',T5,'---- --- ------ ---- ------ ------ -------' )
     !                       XXXX XXX XXX.XX XXX. XXXXXX XXXXXX XXXXXX.
     do I = 1,NDEAD
        write(JOSTND,1010) I, ISNSP(I), SNGDIA(I), OLEN(I), &
             & IYRCOD(I), ISTATUS(I), FALLDIR(I)
        520 continue
     end do

     NOTFELD = INT(real(EL_SNAGS) - FELLTPA)
     FELLPCT = (FELLTPA/EL_SNAGS)*100.0
     CULLPCT = (CULLTPA/FELLTPA)*100.0
     NWSVSNG = 0
     do I = 1,NSVOBJ
        if (IOBJTP(I).eq.2) NWSVSNG = NWSVSNG+1
        530 continue
     end do

     write (JOSTND,1110) B4SNAGS, B4SVSNG, EL_SNAGS, &
          & NDEAD, NWSVSNG, NOTFELD, &
          & FELLPCT, CULLPCT
     1110 format (/, &
          & ' ',T5,16X,' Total      Total    Eligible ', / , &
          & ' ',T5,16X,'base FVS   SVS snag  standing ', / , &
          & ' ',T5,16X,'snag recs  objects   snag recs', / , &
          & ' ',T5,16X,'---------  --------  ---------', / , &
          & ' ',T5,'Before salvage:',3X,I4,6X,I5,6X,I4, / , &
          & ' ',T5,'After salvage: ',3X,I4,6X,I5,6X,I4, / , &
          & ' ',T5,'Snag fell pct: ',23X,F5.1, / , &
          & ' ',T5,'Snag cull pct: ',23X,F5.1, / )
  end if

  return
end subroutine SVSALV
