!=================!
!   rd : rdmort   !
!=================!
! RD $Id$
subroutine RDMORT
  implicit none

  !  THIS SUBROUTINE CALCULATES THE INFECTION OF THE ROOT SYSTEMS OF
  !  LIVE TREES THROUGH TIME AND ESTIMATES WHEN THEY DIE DEPENDING
  !  ON THE PROPORTION INFECTED AND THE SPECIES.

  !  CALLED BY :
  !     RDCNTL  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDSLP   (FUNCTION)     [ROOT DISEASE]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]

  !  Revision History:
  !    17-JUN-2002  Lance R. David
  !      Previous revision date note was "MARCH 7, 1995".
  !      Added debug code.
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,I1,I2,IDI,IP,ISL,IT,J,KSP
  real    :: HABSP,TAREA,RDSLP

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'RDMORT',6,ICYC)

  if (DEBUG) then
     write(JOSTND,*) 'ENTER RDMORT: ICYC=',ICYC,' ISTEP=',ISTEP
     write(JOSTND,*)&
          &'             I,PROBI(I,1,1),PROBIU(I),PROBIT(I),FPROB(I)'
     do I = 1,ITRN
        write(JOSTND,618)&
             &I,PROBI(I,1,1),PROBIU(I),PROBIT(I),FPROB(I)
        618 format(' IN RDMORT: BEF',I4,4F8.2)
        333 continue
     end do
  end if

  !     ZERO OUT ACCUMULATORS.

  do KSP = 1,MAXSP
     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)
     if (ISCT(KSP,1) .eq.0) goto 100

     do J = I1,I2
        I = IND1(J)
        RDKILL(I) = 0.0
        150 continue
     end do
     100 continue
  end do

  !     LOOP OVER TREE LIST IF THERE ARE ANY INFECTION CENTERS.

  TAREA = 0.0
  do IDI = MINRR,MAXRR
     TAREA = TAREA + PAREA(IDI)
     RRGEN(IDI,3) = 0.0
     410 continue
  end do
  if (TAREA .le.0.0) goto 600
  if (ITRN .eq.0) return

  IDI = MAXRR
  do KSP = 1, MAXSP
     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))

     !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
     !     If non-host species then IDI = 0, and loop should be skipped to
     !     prevent array out of bounds error

     if (IDI .le.0) go to 500

     if (DEBUG) write (JOSTND,*)&
          &'IN RDMORT: KSP, IRTSPC, IDI, IRHAB, HABFAC: ',&
          &KSP, IRTSPC(KSP), IDI, IRHAB, HABFAC(IRTSPC(KSP),IDI,IRHAB)

     HABSP = HABFAC(IRTSPC(KSP),IDI,IRHAB)

     !        MODIFY THE TIME TO DEATH MULTIPLIER (HABSP) BASED ON THE
     !        PROPORTION OF CENTERS THAT ARE SPORE INITIATED (SPPROP).

     if (DEBUG) write (JOSTND,*)&
          &'IN RDMORT: SPPROP, SPYTK: ', SPPROP(IDI),SPYTK(IDI)

     HABSP = HABSP * ((SPPROP(IDI) * SPYTK(IDI)) +&
          &(1 - SPPROP(IDI)))
     if (ISCT(KSP,1) .eq.0) goto 500

     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)

     do J = I1, I2
        I = IND1(J)

        do IT = 1, ISTEP
           do IP = 1,2

              if (PROBI(I,IT,IP) .le.0) goto 300

              if (DEBUG) write (JOSTND,*)&
                   &'IN RDMORT: I, DBH, XXINF, YYINF, NNINF: ',&
                   &I, DBH(I), XXINF, YYINF, NNINF

              YTKILL = RDSLP(DBH(I),XXINF,YYINF,NNINF)

              if (DEBUG) write (JOSTND,*)&
                   &'IN RDMORT: IDI, YTKILL, XMINKL HABSP RRPSWT: ',&
                   &IDI, YTKILL, XMINKL(IDI), HABSP, RRPSWT(IRTSPC(KSP))

              if (YTKILL .le. XMINKL(IDI)) goto 250

              YTKILL = (YTKILL - XMINKL(IDI)) *&
                   &HABSP *&
                   &RRPSWT(IRTSPC(KSP)) + XMINKL(IDI)
              250 continue

              if (DEBUG) write (JOSTND,*)&
                   &'IN RDMORT: IDI, YTKILL, PROPI, PKILLS: ',&
                   &IDI, YTKILL,PROPI(I,IT,IP),PKILLS(IRTSPC(KSP),IDI)

              CURAGE = PROPI(I,IT,IP) * YTKILL/PKILLS(IRTSPC(KSP),IDI)

              if (DEBUG) write (JOSTND,*)&
                   &'IN RDMORT: CURAGE: ',CURAGE

              PROPI(I,IT,IP) = (CURAGE + FINT) *&
                   &PKILLS(IRTSPC(KSP),IDI) / YTKILL
              if (DEBUG) write (JOSTND,*)&
                   &'IN RDMORT: I, DBH, CURAGE, PROPI: ',&
                   &I, DBH(I), CURAGE, PROPI(I,IT,IP)
              if (PROPI(I,IT,IP) .lt. PKILLS(IRTSPC(KSP),IDI)) goto 300

              RRKILL(I) = RRKILL(I) + PROBI(I,IT,IP)
              RDKILL(I) = RDKILL(I) + PROBI(I,IT,IP)
              RRGEN(IDI,3) = RRGEN(IDI,3) + PROBI(I,IT,IP) /&
                   &(PAREA(IDI) + 1E-9)

              if (DEBUG) write(JOSTND,260)&
                   &I, IT, CURAGE, DBH(I), PROB(I), PROBI(I,IT,IP)
              260 format(' IN RDMORT:  I, IT, CURAGE, DBH, PROB, PROBI ',&
                   &I4,I4,4F10.3)

              PROBI(I,IT,IP) = 0.0
              300 continue
           end do
           350 continue
        end do

        !        UPDATE THE STUMP LIST WITH DEAD INFECTED TREES

        if (RDKILL(I) .gt.0.0) then
           call RDSSIZ(KSP,DBH(I),STCUT,ISL,ISPS,IRTSPC)
           call RDSTP (ISL,KSP,RDKILL(I),DBH(I),ROOTL(I))
        end if

        400 continue
     end do
     500 continue
  end do

  600 continue

  !     RECALCULATE PROBIT (SUM OF PROBI'S)

  call RDSUM(ITRN,PROBIT,PROBI,ISTEP)

  if (DEBUG) then
     write (JOSTND,*)&
          &'             I,DBH(I),PROBIU(I),PROBIT(I),FPROB(I)'
     do I = 1,ITRN
        write (JOSTND,619) I,DBH(I),PROBIU(I),PROBIT(I),FPROB(I)
        619 format(' IN RDMORT: AFT',I4,4F8.2)
        444 continue
     end do
  end if

  if (DEBUG) write (JOSTND,*) 'EXIT RDMORT:'
  return
end
