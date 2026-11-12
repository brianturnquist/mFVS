!=======================================!
!   Compute tree growth and mortality   !
!=======================================!
! BASE $Id$
subroutine GRINCR (DEBUG,IPMODI,LTMGO,LMPBGO,LDFBGO, &
     & LBWEGO,LCVATV)
  implicit none

  !     COMPUTES GROWTH AND MORTALITY ON EACH TREE RECORD.

  !     CALLED FROM: TREGRO.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'PLOT.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PDEN.f90'
  include 'OUTCOM.f90'
  include 'STDSTK.f90'
  include 'ESHAP.f90'
  include 'VARCOM.f90'
  include 'OPCOM.f90'

  ! Variables
  real    :: PRM(6)
  integer :: MYACTS(4)
  integer :: IPMODI,NTODO,ITODO,NP,IACTK,IDT,I,IS,IGRP,IULIM,IG,IGSP
  integer :: IC,ICDF,IBDF,ISPCC,ITFN,J,JJ,K
  real    :: STAGEA,STAGEB
  logical :: LTMGO,LMPBGO,LDFBGO,LBWEGO,LCVATV,DEBUG,LINCL
  integer :: IBA,ISTOPRES,IRTNCD
  !     KARD IS 20 CHARACTERS BECAUSE THE BC HABTYP ARGUMENT IS 20 LONG;
  !     OTHER VARIANTS' HABTYP ROUTINES USE THE FIRST 10 CHARACTERS.
  character(len=20) :: KARD

  !     ACTIVITY CODES FOR OPTIONS: FIXDG, FIXHTG, SETSITE, PERMFRST

  data MYACTS/98,99,120,445/
  data KARD/'          '/

  !     SET THE CYCLE LENGTH AND OPTION POINTERS FOR THE CYCLE.

  if (ICYC .ge. 2) then
     OLDFNT = IY(ICYC)-IY(ICYC-1)
  else
     OLDFNT = FINT
  end if
  IFINT = IY(ICYC+1)-IY(ICYC)
  FINT = IFINT

  !     SET UP THE OPTION POINTERS FOR THE CYCLE.

  call OPCSET (ICYC)

  !     SET CYCLE DEPENDENT SWITCHES

  LTRIP = (ICYC.le.ICL4.and.ITRN.le.(MAXTRE/3).and..not.NOTRIP)

  !     GET THE RESTART CODE AND BRANCH ACCORDINGLY

  call fvsGetRestartCode (ISTOPRES)
  if (DEBUG) write (JOSTND,5) ICYC,ISTOPRES,NPLT
  5 format (/' IN GRINCR, ICYC=',I3,'; ISTOPRES=',I3,'; NPLT=',A)
  if (VARACD .eq. 'BC') then
     goto (1,16,17,71,72), ISTOPRES+1
  else
     goto (1,16,17,71,72,1), ISTOPRES+1
  end if

  !     STOP HAS NOT OCCURED

  1 continue

  ! PROCESS THE SETSITE OPTIONS.

  call OPFIND (1,MYACTS(3),NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET (ITODO,6,IDT,IACTK,NP,PRM)
        if (IACTK.lt.0) goto 9
        call OPDONE(ITODO,IY(ICYC))

        if (PRM(1) .gt. 0.) then
           KODTYP = IFIX(PRM(1))
           KARD = '         '
           CPVREF = '          '
           call HABTYP (KARD,PRM(1))
           if (KODTYP .gt. 0)ICL5 = KODTYP
           do I = 1,MAXPLT
              IPHAB(I) = ITYPE
              4 continue
           end do
           if (DEBUG)write(JOSTND,*)' IN GRINCR PROCESSING SETSITE HABITA' &
                & ,'T TYPE: KODTYP,ICL5,ITYPE= ',KODTYP,ICL5,ITYPE
        end if

        if (PRM(2) .gt. 0) then
           BAMAX = PRM(2)
           LBAMAX = .true.
           if (DEBUG)write(JOSTND,*)' IN GRINCR PROCESSING SETSITE', &
                & ' BASAL AREA MAXIMUM = ',BAMAX,'  LBAMAX = ',LBAMAX
        end if

        IS = IFIX(PRM(3))

        !     IF (IS<0) CHANGE ALL SPECIES IN THE GROUP
        !     IF SITE SPECIES HAS NOT BEEN SET, SET IT TO THE FIRST SPECIES
        !     IN THE GROUP.

        if (IS .lt. 0) then
           IGRP = -IS
           IULIM = ISPGRP(IGRP,1)+1
           if (ISISP .eq. 0) ISISP = ISPGRP(IGRP,2)
           LSITE = .true.
           do IG = 2,IULIM
              IGSP = ISPGRP(IGRP,IG)
              if (PRM(5) .eq. 0) then
                 if (PRM(4).gt.0.)SITEAR(IGSP) = PRM(4)
              else
                 if (PRM(4).ne.0.)SITEAR(IGSP) = SITEAR(IGSP)+SITEAR(IGSP)* &
                      & PRM(4)/100.
              end if
              if (SITEAR(IGSP).lt.1.)SITEAR(IGSP) = 1.
              if (PRM(6) .gt. 0.) then
                 SDIDEF(IGSP) = PRM(6)
                 MAXSDI(IGSP) = 1
              end if
              6 continue
           end do

           !     IF (IS=0) ALL SPECIES WILL BE CHANGED IN FOLLOWING CODE:

        else if (IS .eq. 0) then
           do I = 1,MAXSP
              if (PRM(5) .eq. 0) then
                 if (PRM(4).gt.0.)SITEAR(I) = PRM(4)
              else
                 if (PRM(4).ne.0.)SITEAR(I) = SITEAR(I)+SITEAR(I)*PRM(4)/100.
              end if
              if (SITEAR(I).lt.1.)SITEAR(I) = 1.
              if (PRM(6) .gt. 0.) then
                 SDIDEF(I) = PRM(6)
                 MAXSDI(I) = 1
              end if
              7 continue
           end do

           !     SPECIES CODE IS SPECIFIED (IS>0).

        else
           if (PRM(5) .eq. 0) then
              if (PRM(4).gt.0.)SITEAR(IS) = PRM(4)
           else
              if (PRM(4).ne.0.)SITEAR(IS) = SITEAR(IS)+SITEAR(IS)*PRM(4)/100.
           end if
           if (SITEAR(IS).lt.1.)SITEAR(IS) = 1.
           if (PRM(6) .gt. 0.) then
              SDIDEF(IS) = PRM(6)
              MAXSDI(IS) = 1
           end if
        end if

        if (DEBUG .and. (PRM(4).gt.0. .or. PRM(6).gt.0.)) then
           write(JOSTND,*)' IN GRINCR PROCESSING SETSITE SITE INDEX ', &
                & 'AND/OR SDIMAX:'

           do I = 1,15
              J = (I-1)*10 + 1
              JJ = J+9
              if (JJ.gt.MAXSP)JJ = MAXSP
              write(JOSTND,990)(NSP(K,1)(1:2),K = J,JJ)
              990 format(/'    SPECIES ',5X,10(A2,6X))
              if (VARACD .eq. 'BC') then
                 write(JOSTND,9994)(SITEAR(K),K = J,JJ )
                 9994 format(' SITE INDEX ',   10F8.0)
                 write(JOSTND,9991)(SDIDEF(K),K = J,JJ )
                 9991 format('    SDI MAX ',   10F8.0)
              else
                 write(JOSTND,994)(NINT(SITEAR(K)),K = J,JJ )
                 994 format(' SITE INDEX ',   10I8.0)
                 write(JOSTND,991)(NINT(SDIDEF(K)),K = J,JJ )
                 991 format('    SDI MAX ',   10I8.0)
              end if
              if (JJ .eq. MAXSP)go to 993
              992 continue
           end do
           993 continue
        end if
        9 continue
     end do
     if (ISISP.gt.0 .and. ISISP.le.MAXSP) then
        STNDSI = SITEAR(ISISP)
     else
        STNDSI = 0.
     end if
     call RCON
  end if

  ! PROCESS THE PERMFRST OPTION WHICH IS ONLY VALID FOR AK VARIANT.

  call OPFIND (1,MYACTS(4),NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET (ITODO,1,IDT,IACTK,NP,PRM)
        if (IACTK.ge.0) then
           call OPDONE(ITODO,IY(ICYC))
           if (PRM(1) .eq. 0.) then
              LPERM = .false.
           else if (PRM(1) .eq. 1.) then
              LPERM = .true.
           end if
           if (DEBUG) write(JOSTND,*) &
                & ' IN GRINCR PROCESSED PERMFRST - LPERM: ',LPERM
        end if
     end do
  end if

  !     WESTERN ROOT DISEASE VER. 3.0 MODEL PROJECTION SETUP.

  call RDMN2 (OLDFNT)
  call RDTRP (LTRIP)

  !     FIRE MODEL PROJECTION SETUP

  call FMSDIT

  !     CALL SILFTY (ENTRY IN SDICAL) TO COMPUTE SILVAH FOREST TYPE
  !     BEFORE THINNING

  call SILFTY

  if (DEBUG) write(JOSTND,10)BTSDIX,SDIBC,SDIBC2,ICYC
  10 format(' BEFORE SDICAL, BTSDIX, SDIBC, SDIBC2, CYCLE=', &
       & F9.1,2F9.1,I2)

  !     CALL SDICAL TO GET CYCLE 0 BEFORE CUT SDI MAX.

  call SDICAL(0,BTSDIX)
  call SDICLS(0,0.,999.,1,SDIBC,SDIBC2,STAGEA,STAGEB,0)

  if (DEBUG) write(JOSTND,15) BTSDIX, SDIBC, SDIBC2, ICYC
  15 format(' AFTER SDICAL, BTSDIX, SDIBC, SDIBC2, CYCLE=', &
       & F9.1,2F9.1,I2)

  IBA = 1
  call SSTAGE (IBA,ICYC,.false.)

  !     REFRESH EVENT MONITOR TIME VARIABLES SO THEY ARE CURRENT AT STOP 1.
  !     SAME AS EVTSTV; EVMON HAS NOT RUN YET.

  TSTV1(1) = IY(ICYC)
  TSTV1(2) = IAGE+IY(ICYC)-IY(1)
  TSTV1(13) = ICYC

  !     IS THIS A STOPPING POINT?

  call fvsStopPoint (1,ISTOPRES)
  if (ISTOPRES.ne.0) return
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  16 continue

  !     IF PROCESSING A PHASE 1 SKIP CALL TO EVENT MONITOR.

  call EVMON (1,1)

  !     IS THIS A STOPPING POINT?

  call fvsStopPoint (2,ISTOPRES)
  if (ISTOPRES.ne.0) return
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  17 continue

  !     EVALUATE ECON EXTENSTION STATUS
  !     0 MEANS THIS CALL TO ECSTATUS.F IS BEFORE THE CALL TO CUTS.F

  call ECSTATUS(ICYC, NCYC, IY, 0)

  !     PRINT THE VISULIZATION FOR BEGINING CYCLE. SKIP IN FIRST CYCLE.

  if (ICYC.gt.1) call SVOUT(IY(ICYC),1,'Beginning of cycle')

  !     SAVE LAST CYCLE DENSITY STATS.

  OLDTPA = TPROB
  OLDAVH = AVH
  OLDBA = BA
  RELDM1 = RELDEN
  ORMSQD = RMSQD
  if (VARACD .ne. 'BC') then
     ODR016 = DR016
  end if

  !     CALL **CUTS** TO INVOKE THINNING.

  if (DEBUG) write(JOSTND,20) ICYC
  20 format(' CALLING CUTS, CYCLE=',I2)
  call CUTS

  !     STORE THE REMOVED TPA IN WK4 FOR USE IN THE SECOND CALL TO THE
  !     EVENT MONITOR (SPMCDBH INVOLVING CUT TREES).

  do IC = 1,MAXTRE
     WK4(IC) = WK3(IC)
     21 continue
  end do

  !     CALL **CVGO** TO DETERMINE IF THE COVER EXTENSION IS ACTIVE.

  call CVGO (LCVATV)

  !     CALL **DENSE** TO UPDATE STAND DENSITY STATISTICS IF THINNING
  !     OCCURRED.

  if (ONTREM(7).gt.0.0) then
     if (DEBUG) write(JOSTND,40) ICYC
     40 format (' CALLING DENSE (POST THIN), CYCLE=',I3)
     call DENSE
  end if

  !     SAVE POST-THIN DENSITY.

  ATAVD = RMSQD
  if (VARACD .ne. 'BC') then
     ATDR016 = DR016
  end if
  ATAVH = AVH
  ATBA = BA
  ATCCF = RELDEN
  ATTPA = TPROB
  call SDICAL(0,ATSDIX)
  call SDICLS(0,0.,999.,1,SDIAC,SDIAC2,STAGEA,STAGEB,0)

  !     CALL SILFTY (ENTRY IN SDICAL) TO COMPUTE SILVAH FOREST TYPE
  !     AFTER THINNING

  call SILFTY

  !     CALL CVBROW & CVCNOP TO COMPUTE POST-THIN STAND SHRUB AND COVER
  !     STATISTICS.

  if (ONTREM(7).gt.0.0 .and. LCVATV) then
     if (DEBUG) write(JOSTND,60) ICYC
     60 format (' CALLING CVBROW, CYCLE =',I2)
     call CVBROW (.true.)
     if (DEBUG) write(JOSTND,70) ICYC
     70 format (' CALLING CVCNOP, CYCLE =',I2)
     call CVCNOP (.true.)
  end if

  !     COMPUTE THE STRUCTURAL STAGE OF THE STAND AFTER CALL TO CUTS.

  IBA = 2
  call SSTAGE (IBA,ICYC,.false.)

  !     IS THIS A STOPPING POINT?

  call fvsStopPoint (3,ISTOPRES)
  if (ISTOPRES.ne.0) return
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  71 continue

  !     CALL EVENT MONITOR FOR PHASE II CHECKING.

  call EVMON (2,1)

  !     IS THIS A STOPPING POINT?

  call fvsStopPoint (4,ISTOPRES)
  if (ISTOPRES.ne.0) return
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return
  72 continue

  !     EVALUATE ECON EXTENSTION STATUS
  !     1 MEANS THIS CALL TO ECSTATUS.F IS AFTER THE CALL TO CUTS.F

  call ECSTATUS(ICYC, NCYC, IY, 1)

  !  SET PAST VOLUME VARIABLES FOR USE NEXT CYCLE

  do I = 1,ITRN
     PTOCFV(I) = CFV(I)
     if (VARACD .eq. 'BC') then
        PMRCFV(I) = WK1(I)
     else
        PMRCFV(I) = MCFV(I)
        PSCFV(I) = SCFV(I)
     end if
     PMRBFV(I) = BFV(I)
     PDBH(I) = DBH(I)
     PHT(I) = HT(I)
     ICDF = (DEFECT(I)-((DEFECT(I)/10000)*10000))/100
     IBDF = DEFECT(I)-((DEFECT(I)/100)*100)
     NCFDEF(I) = ICDF
     NBFDEF(I) = IBDF
     215 continue
  end do

  !     CALL COMCUP TO COMPRESS THE TREE LIST.

  if (DEBUG) write (JOSTND,30) ICYC
  30 format (' CALLING COMCUP, CYCLE=',I3)
  call COMCUP

  !     IF IPMODI IS EQUAL TO 2, SKIP THESE BUG MODELS

  if (IPMODI.eq.2) goto 120

  !     CALL DFTMGO, MPBGO, AND DFBGO, TO DETERMINE IF INSECT
  !     OUTBREAK WILL OCCUR THIS CYCLE.

  if (DEBUG) write(JOSTND,80) ICYC
  80 format(' CALLING DFTMGO, CYCLE=',I2)
  call DFTMGO(LTMGO)
  if (DEBUG) write(JOSTND,90) ICYC
  90 format(' CALLING MPBGO, CYCLE=',I2)
  call MPBGO(LMPBGO)
  if (DEBUG) write(JOSTND,95) ICYC
  95 format(' CALLING DFBGO, CYCLE=',I2)
  call DFBGO(LDFBGO)

  !     ADAPTATION FOR BUDLITE MODEL

  if (DEBUG) write(JOSTND,105) ICYC
  105 format(' CALLING BWEGO, CYCLE=',I2)
  call BWEGO(LBWEGO)

  !     BRANCH TO STATEMENT 40 IF NO TUSSOCK MOTH OUTBREAK THIS CYCLE.

  if (LTMGO) then

     !        THERE IS A TUSSOCK MOTH OUTBREAK; COMPUTE BIOMASS.

     if (DEBUG) write(JOSTND,110) ICYC
     110 format(' CALLING TMBMAS, CYCLE=',I2)
     call TMBMAS
  end if

  !     BRANCH TO THIS LOCATION IF PROCESSING A BRANCH WHICH WAS CREATED
  !     BY A PHASE II EVENT.

  120 continue

  !     CALL **DGDRIV** TO COMPUTE TREE DIAMETER INCREMENT.  THE ARRAY DG
  !     WILL RETURN WITH DIAMETER GROWTHS SCALED TO YR-YEAR BASIS.

  if (DEBUG) write(JOSTND,130) ICYC
  130 format(' CALLING DGDRIV, CYCLE=',I2)
  call DGDRIV

  !     CALL **HTGF** TO COMPUTE TREE HEIGHT INCREMENTS.

  if (DEBUG) write(JOSTND,140) ICYC
  140 format(' CALLING HTGF, CYCLE=',I2)
  call HTGF

  !     CALL **REGENT** TO COMPUTE HEIGHT AND DIAMETER INCREMENT.

  if (DEBUG) write(JOSTND,150) ICYC
  150 format(' CALLING REGENT, CYCLE=',I2)
  call REGENT(.false.,1)

  ! PROCESS THE FIXDG AND FIXHTG OPTIONS.

  call OPFIND (1,MYACTS(1),NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET (ITODO,4,IDT,IACTK,NP,PRM)
        if (IACTK.lt.0) goto 300
        call OPDONE(ITODO,IY(ICYC))
        ISPCC = IFIX(PRM(1))
        if (PRM(2) .lt. 0.) PRM(2) = 0.
        if (PRM(3) .lt. 0.) PRM(3) = 0.
        if (PRM(4) .le. 0.) PRM(4) = 999.
        if (ITRN.gt.0) then
           do I = 1,ITRN
              LINCL = .false.
              if (ISPCC.eq.0 .or. ISPCC.eq.ISP(I)) then
                 LINCL = .true.
              else if (ISPCC.lt.0) then
                 IGRP = -ISPCC
                 IULIM = ISPGRP(IGRP,1)+1
                 do IG = 2,IULIM
                    if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                       LINCL = .true.
                       go to 401
                    end if
                    400 continue
                 end do
              end if
              401 continue
              if (LINCL .and. &
                   & (PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                 DG(I) = DG(I)*PRM(2)
                 if (LTRIP) then
                    ITFN = ITRN+2*I-1
                    DG(ITFN) = DG(ITFN)*PRM(2)
                    DG(ITFN+1) = DG(ITFN+1)*PRM(2)
                 end if
              end if
              290 continue
           end do
        end if
        300 continue
     end do
  end if
  call OPFIND (1,MYACTS(2),NTODO)
  if (NTODO.gt.0) then
     do ITODO = 1,NTODO
        call OPGET (ITODO,4,IDT,IACTK,NP,PRM)
        if (IACTK.lt.0) goto 302
        call OPDONE(ITODO,IY(ICYC))
        ISPCC = IFIX(PRM(1))
        if (PRM(2) .lt. 0.)  PRM(2) = 0.
        !CCC     IF (PRM(2) .GT. 1.0) PRM(2)=1.0
        if (ITRN.gt.0) then
           do I = 1,ITRN
              LINCL = .false.
              if (ISPCC.eq.0 .or. ISPCC.eq.ISP(I)) then
                 LINCL = .true.
              else if (ISPCC.lt.0) then
                 IGRP = -ISPCC
                 IULIM = ISPGRP(IGRP,1)+1
                 do IG = 2,IULIM
                    if (ISP(I) .eq. ISPGRP(IGRP,IG)) then
                       LINCL = .true.
                       go to 403
                    end if
                    402 continue
                 end do
              end if
              403 continue
              if (LINCL .and. &
                   & (PRM(3).le.DBH(I) .and. DBH(I).lt.PRM(4))) then
                 HTG(I) = HTG(I)*PRM(2)
                 if (LTRIP) then
                    ITFN = ITRN+2*I-1
                    HTG(ITFN) = HTG(ITFN)*PRM(2)
                    HTG(ITFN+1) = HTG(ITFN+1)*PRM(2)
                 end if
              end if
              301 continue
           end do
        end if
        302 continue
     end do
  end if

  !     CALL **MORTS** TO COMPUTE TREE MORTALITY.

  if (DEBUG) write(JOSTND,160) ICYC
  160 format(' CALLING MORTS, CYCLE=',I2)
  call MORTS

  !     NOW TRIPLE RECORDS AND REALLIGN POINTERS IF TRIPLING OPTION IS
  !     SET.

  if (LTRIP.and.ITRN.gt.0) then
     if (DEBUG) write(JOSTND,170) ICYC
     170 format(' CALLING TRIPLE, CYCLE=',I2)
     call TRIPLE

     !        INFLATE THE TREE RECORD COUNT.

     ITRN = ITRN*3

     !        REALLIGN POINTER VECTORS.

     if (DEBUG) write(JOSTND,180) ICYC
     180 format(' CALLING REASS, CYCLE=',I2)
     call REASS

     !        INFLATE IREC1.

     IREC1 = IREC1*3
  end if

  !     CALL **FFERT** TO COMPUTE THE EFFECTS OF APPLYING FERTILIZER.

  if (DEBUG) write(JOSTND,190) ICYC
  190 format(' CALLING FFERT, CYCLE=',I2)
  call FFERT

  return
end subroutine GRINCR
