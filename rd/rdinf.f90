!================!
!   rd : rdinf   !
!================!
! RD $Id$
subroutine RDINF
  implicit none

  !  Purpose :
  !     This subroutine takes a newly infected area and converts it
  !     into the equivalent number of infected trees of each tree
  !     class in the tree list.

  !  Called By :
  !     RDCNTL  [ROOT DISEASE]
  !     RDJUMP  [ROOT DISEASE]

  !  Calls :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]

  !  Revision History :
  !     06/12/96 - Matthew K. Thompson
  !                Moved the declaration of DSO, DSII, and DSIU to the
  !                parameter include file RDPARM.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: DEBUG

  integer :: I,I1,I2,IDI,J,KSP
  real    :: ADDINF,NUINSD,OAMOVE(3),PNSP,PROPN,RDRANP

  !.... See if we need to do some debug.

  call DBCHK (DEBUG,'RDINF',5,ICYC)

  !.... Don't bother doing anything if there is no new area.

  if (AREANU(IRRSP) .le.0.0) return

  IDI = MAXRR
  do KSP = 1,MAXSP
     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(KSP))

     !     Modification (RNH, MAR98).  If MAXRR < 3 then annosus disease.
     !     If non-host species then IDI = 0, and loop should be skipped to
     !     prevent array out of bounds error

     if (IDI .le.0) go to 500

     PNSP = PNINF(IRTSPC(KSP),IDI)

     !....    First, modify probability of infection to account for the
     !....    number of years in a cycle.

     PNSP = 1 - ((1 - PNSP) ** (FINT/PINT))

     !....    Modify probability of infection given root to root contact
     !....    based on the proportion of centers that are spore initiated
     !....    (SPPROP).

     PNSP = PNSP * ((SPPROP(IDI) * SPTRAN(IDI)) + (1 - SPPROP(IDI)))

     if ((ISCT(KSP,1) .eq.0) .or. (IDI .ne. IRRSP)) goto 500

     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)

     do J = I1, I2
        I = IND1(J)

        if (FPROB(I) .le.0.0) goto 400

        NUINSD = AREANU(IRRSP) * FPROB(I)
        ADDINF = NUINSD * PNSP

        !           IF (ICYC .GT. 1) THEN
        !              PROPI(I,ISTEP) = .001
        !           ENDIF

        !....       Get a random proportion of roots infected based around a
        !....       mean proportion of roots infected for a new infection
        !....       mean = 0.001 (Annosus), 0.05 (Armillaria, Phellinus)

        PROPI(I,ISTEP,2) = RDRANP(RRNEW(IDI))
        PROBI(I,ISTEP,2) = PROBI(I,ISTEP,2) + ADDINF
        PROBIU(I) = PROBIU(I) + NUINSD * (1 - PNSP)

        !....       Add the new infection resulting from the center area
        !....       expanding.
        !....       (,1)=# trees infected, (,2)=total # new trees in area.

        EXPINF(IDI,1) = EXPINF(IDI,1) + ADDINF
        EXPINF(IDI,2) = EXPINF(IDI,2) + NUINSD

        !....       Make sure that OAKL continues to contain the right number
        !....       of killed trees. Assume that some proportion of those trees
        !....       that were outside killed are now inside killed (either
        !....       infected or uninfected) (eg. If 10% of the outside trees
        !....       were killed from BB or windthrow then 10% of those trees
        !....       just becoming inside trees were killed from BB or windthrow)
        !....
        !....       NUINSD -- Number of new trees inside the center.
        !....       OAMOVE -- Amount to move from one OAKL slot to another.

        if (OAKL(DSO,I) .gt.0.0) then
           PROPN = OAKL(DSO,I) / (FPROB(I) * (SAREA - PAREA(IRRSP) +&
                &AREANU(IRRSP)))

           OAMOVE(DSO) = PROPN * NUINSD
           OAMOVE(DSII) = PROPN * ADDINF
           OAMOVE(DSIU) = PROPN * (NUINSD - ADDINF)

           call RDMREC (2,I,KSP,OAMOVE)

        end if

        if (DEBUG) write(JOSTND,401) PROBI(I,ISTEP,2), PROBIU(I)
        401 format(' IN RDINF PROBI PROBIU',2F10.2)
        400 continue
     end do
     500 continue
  end do

  !.... Call RDSUM to calculate new value of PROBIT.

  call RDSUM(ITRN,PROBIT,PROBI,ISTEP)

  return
end
