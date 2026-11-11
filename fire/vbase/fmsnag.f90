!=========================!
!   fire/vbase : fmsnag   !
!=========================!
! FIRE-VBASE $Id$
subroutine FMSNAG (IYR, YR1)
  implicit none

  !     CALLED FROM: FMMAIN
  !     CALLS   FMSSEE
  !             FMSADD
  !             FMSFALL
  !             CWD1 (entry in FMCWD)
  !             FMSNGHT
  !             CWD2 (entry in FMCWD)
  !             FMSNGDK

  !  Purpose:
  !     THIS SUBROUTINE CALCULATES ANNUAL CHANGES IN EACH SNAG RECORD,
  !     INCLUDING THE LOSS OF SNAGS DUE TO FALLDOWN, THE TRANSITION FROM
  !     'HARD' TO 'SOFT' DECAY STATES, AND HEIGHT LOSS DUE TO TOP BREAKAGE.
  !     NI - This subroutine calculates annual changes in each snag record,
  !          including the loss of snags due to falldown, the transition from
  !          'hard' to 'soft' decay states, and height loss due to top breakage.
  !          The calculations are based on discussions with Richard Teck and
  !          Nick Crookston, and data from Bruce Marcot's Snag Recruitment
  !          Simulator spreadsheet (Eastside version) and Cline et al. 1980.
  !          'Base' rate-of-fall is calculated from DBH assuming that Marcot's
  !          observed rate for <18" snags applies on average to 9" snags, and
  !          his rate for >18" snags applies to 28" snags.
  !     SN - MULTIPLIERS TO TWEAK THE EQUATIONS BY SPECIES WERE DEVELOPED FROM
  !          OZARK-OUACHITA WORKSHOP INPUT
  !     LS - MULTIPLIERS AND EQUATIONS WERE DEVELOPED FROM LAKE STATES WORKSHOP INPUT
  !     NE - FALL RATES WERE DETERMINED FROM DISCUSSIONS WITH COELI HOOVER AND LINDA
  !          HEATH AND A PAPER BY YAMASAKI AND LEAK (IN PRESS, NJAF)

  !  Local variable definitions:
  !     DFIS:   density of initially-soft snags to fall / fallen.
  !     DFIH:   density of initially-hard snags to fall / fallen.
  !     DFALLN: target density of snags to fall under normal conditions
  !             (where hard and soft snags fall at the same rate).
  !     DZERO:  Density (#/acre) of snags considered equal to ZERO
  !     RSOFT:  Rate of snag fall implied by PBSOFT and PBTIME
  !     RSMAL:  Rate of snag fall implied by PBSMAL and PBTIME
  !     YR1:    First year in the master cycle

  !  Common block variables and parameters:

  !.... Parameter statements.

  !.... Parameter include files.
  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'

  !.... Common include files.
  include 'FMCOM.f90'
  include 'CONTRL.f90'

  !.... Variable declarations.

  ! Variables
  integer :: I,JSP,YR1,YEAR
  integer :: IYR,NTODO,JDO,NPRM,IACTK,JYR
  real    :: DENTTL,DFIS,DFIH,DFALLN,DZERO,HTSNEW
  real    :: OLDHTH,OLDHTS,XS,XH
  real    :: D,DKTIME,HTD,AGE,SNUM,RSOFT,RSMAL
  logical :: DEBUG,LASCO

  integer :: MYACT(1)
  real    :: PRMS(6)
  double precision :: SAVESO

  data MYACT/2522/

  !     CHECK FOR DEBUG.

  call DBCHK (DEBUG,'FMSNAG',6,ICYC)
  if (DEBUG) write(JOSTND,7) ICYC, IYR, YR1, NSNAG
  7 format(' FMSNAG CYCLE=',I2,' IYR=',I5,' YR1=',I5,' NSNAG=',I5)

  !.... Begin routine.

  !     Before we do anything else, in the first year check to see if the user
  !     initialized some snags.  These need to be added to the snag lists.

  if (IYR .eq. YR1) then
     call OPFIND(1,MYACT,NTODO)
     do JDO = 1,NTODO
        call OPGET(JDO,6,JYR,IACTK,NPRM,PRMS)
        JSP = INT(PRMS(1))
        D = PRMS(2)
        HTD = PRMS(3)
        AGE = PRMS(5)
        YEAR = INT(real(IYR) - AGE)
        SNUM = PRMS(6)
        if (DEBUG) write(JOSTND,*)' IN FMSNAG JDO=',JDO,' JSP=',JSP
        call FMSSEE (1,JSP,D,HTD,SNUM,4,DEBUG,JOSTND)
        call FMSADD (YEAR,-JDO)
        call OPDONE(JDO,IYR)
        5 continue
     end do
  end if

  !     Initialize variables.

  DZERO = NZERO / 50.0

  !     Loop over all snag records that are in use.

  if (NSNAG.le.0) return

  select case (VARACD)

  case ('BM','EC','OP','PN','SO','WC')
     call RANNGET(SAVESO)

  case default

  end select

  do I = 1, NSNAG

     !       Skip this record if there are no snags in it.

     if (DEBUG) write(JOSTND,*)'IN FMSNAG I=',I,' DENIS=',DENIS(I),&
          &' DENIH=',DENIH(I)

     if ((DENIS(I) + DENIH(I)) .le.0.0) goto 100

     JSP = SPS(I)

     !       SEE IF SPECIES IS ASPEN OR COTTONWOOD OR PAPER BIRCH -
     !       SPECIAL RULES IN UT/TT/CR

     LASCO = .false.

     select case (VARACD)

     case ('UT')
        if (JSP.eq.6.or. JSP.eq.18.or. JSP.eq.19) LASCO = .true.

     case ('TT')
        if (JSP.eq.6.or. JSP.eq.15)   LASCO = .true.

     case ('CR')
        if (JSP.eq.20.or. JSP.eq.21.or. JSP.eq.22.or. JSP.eq.28)&
             &LASCO = .true.

     case ('BC')
        if (JSP.eq.11.or. JSP.eq.12.or. JSP.eq.13.or. JSP.eq.15)&
             &LASCO = .true.

     case default

     end select

     !       Call FMSFALL to:
     !         1) Compute special fall rates if this after a fire.
     !         2) Calculate the density of snags in this record that would fall
     !            ANNUALLY under normal conditions.  This depends on species, dbh and
     !            whether 5% are left.

     JSP = SPS(I)
     DENTTL = DENIH(I) + DENIS(I)
     call FMSFALL(IYR,JSP,DBHS(I),DEND(I),DENTTL,1,&
          &RSOFT,RSMAL,DFALLN)

     !* some of these values can be undefined at this point.
     !*        IF (DEBUG) WRITE(JOSTND,*)' IN FMSNAG RSOFT=',RSOFT,
     !*     &                            ' RSMAL=',RSMAL,' PBSOFT=',pbsoft,
     !*     &                            ' PBSMAL=',PBSMAL

     !     Now set PBFRIH and PBFRIS. Different rates apply to small snags and
     !     snags that are soft AT TIME OF FIRE (whether initially hard or soft):
     !     if both apply, use whichever rate is greater.  Fires do not affect
     !     the fall rate of large snags that are hard at the time of the fire.

     if ((IYR - BURNYR) .le.1) then
        PBFRIS(I) = RSOFT
        PBFRIH(I) = 0.0

        if (DBHS(I) .lt. PBSIZE) then
           PBFRIH(I) = RSMAL
           if ((PBFRIH(I) .lt. PBFRIS(I)) .and. (.not. HARD(I)))&
                &PBFRIH(I) = PBFRIS(I)
           if (PBFRIH(I) .gt. PBFRIS(I))&
                &PBFRIS(I) = PBFRIH(I)
        end if
     end if

     !     Set DFIS and DFIH according to whichever density is largest - the
     !     density falling under normal conditions, or the density to fall under
     !     post-burn conditions if these are applicable.

     !       ** Special handling for aspen & cottonwood (LASCO .eq. .TRUE.) **

     DFIS = DENIS(I) * DFALLN / (DENIS(I)+DENIH(I))
     DFIH = DENIH(I) * DFALLN / (DENIS(I)+DENIH(I))

     if (BURNYR .gt.0.and.&
          &YRDEAD(I) .le. BURNYR) then
        if (LASCO .and. (IYR-BURNYR) .le.10) then
           DFIS = DFIS * 0.5
           DFIH = DFIH * 0.5
           XS = PBFRIS(I) * DENIS(I)
           XH = PBFRIH(I) * DENIH(I)
           if (DFIS .lt. XS) DFIS = XS
           if (DFIH .lt. XH) DFIH = XH
        else if ((IYR-BURNYR) .le. PBTIME) then
           XS = PBFRIS(I) * DENIS(I)
           XH = PBFRIH(I) * DENIH(I)
           if (DFIS .lt. XS) DFIS = XS
           if (DFIH .lt. XH) DFIH = XH
        end if
     end if

     !     Now actually remove the snags.  If less than DZERO will be left,
     !     remove them all.

     if (DFIS .gt. (DENIS(I)-DZERO)) DFIS = DENIS(I)
     if (DFIH .gt. (DENIH(I)-DZERO)) DFIH = DENIH(I)
     DENIS(I) = DENIS(I) - DFIS
     DENIH(I) = DENIH(I) - DFIH

     !     Add the fallen snags to down debris pools.

     call CWD1(I, DFIH, DFIS)

     !     Skip the rest of the loop if no snags are left in the current record.

     if ((DENIS(I) + DENIH(I)) .le. DZERO) then
        DENIS(I) = 0.0
        DENIH(I) = 0.0
        goto 100
     end if

     !       Call FMSNGHT to predict snag height loss due to top breakage.
     !       OLDHT here is just a temporary record of the height that the snags
     !       had before adjustment for breakage during the preceding timestep.

     OLDHTH = -1.0
     OLDHTS = -1.0

     if (DENIH(I) .gt.0.0) then
        OLDHTH = HTIH(I)
        call FMSNGHT(VARACD,JSP,HTDEAD(I),HTIH(I),1,HTSNEW)
        HTIH(I) = HTSNEW
     end if
     if (DENIS(I) .gt.0.0) then
        OLDHTS = HTIS(I)
        call FMSNGHT(VARACD,JSP,HTDEAD(I),HTIS(I),0,HTSNEW)
        HTIS(I) = HTSNEW
     end if

     call CWD2(I, DENIH(I), DENIS(I), OLDHTH, OLDHTS)

     !     If all the init-hard or init-soft snags in this record have zero
     !     height, set their density to zero.  Skip the rest of the loop if
     !     this means there are no snags of either type left in the record.

     select case (VARACD)

     case ('CR','TT','UT')
        if ((DENIH(I) .gt.0.0) .and. (HTIH(I) .lt.1.5))&
             &DENIH(I) = 0.0
        if ((DENIS(I) .gt.0.0) .and. (HTIS(I) .lt.1.5))&
             &DENIS(I) = 0.0

     case default
        if ((DENIH(I) .gt.0.0) .and. (HTIH(I) .lt.1.0))&
             &DENIH(I) = 0.0
        if ((DENIS(I) .gt.0.0) .and. (HTIS(I) .lt.1.0))&
             &DENIS(I) = 0.0

     end select

     if ((DENIS(I) + DENIH(I)) .le.0.0) goto 100

     !       Call FMSNGDK to predict years, since death, for snag to become
     !       soft. Update hard/soft status accordingly.

     if ((DENIH(I) .gt.0.0) .and. (HARD(I))) then
        call FMSNGDK(VARACD,JSP,DBHS(I),DKTIME)
        if ((IYR - YRDEAD(I)) .ge. DKTIME) HARD(I) = .false.
     end if

     100 continue
  end do

  select case (VARACD)

  case ('BM','EC','OP','PN','SO','WC')
     call RANNPUT(SAVESO)

  case default

  end select

  return
end
