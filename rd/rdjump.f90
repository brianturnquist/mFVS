!=================!
!   rd : rdjump   !
!=================!
! RD $Id$
subroutine RDJUMP
  implicit none

  !  Purpose :
  !    This subroutine expands the area in root disease when the
  !    stand is clearcut.

  !  Called By :
  !    RDCNTL  [ROOT DISEASE]

  !  Calls :
  !    RDAREA  (SUBROUTINE)   [ROOT DISEASE]
  !    RDINF   (SUBROUTINE)   [ROOT DISEASE]

  !  Local Variables :
  !    <incomplete>

  !  Common Block Variables Used :
  !    <incomplete>

  !  Revision History
  !    06/10/96 - Matthew K. Thompson
  !               Changed the summing of trees in tree records.
  !               Changed from an INTEGER summation to a REAL summation.
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'RDADD.f90'

  !.... Local variable declarations

  ! Variables
  integer :: I,I1,I2,IDI,J,KSP
  real    :: SPCEN,TOTCEN,TRENUM,XTRENU,XJPINC

  if (PAREA(IRRSP) .eq.0.0) return

  !.... Jump out if a cut occurred

  if (INFLAG .ne.1) goto 650

  SPPROP(IRRSP) = 0.0
  if (NCENTS(IRRSP) .eq.0) goto 650
  if (TNJUMP(IRRSP) .le.0.0) goto 650

  SPCEN = 0.0
  TOTCEN = 0.0
  do I = 1, NCENTS(IRRSP)
     if (PCENTS(IRRSP,I,3) .le.0) goto 700

     !....    Expand centers by the diameter of the uninfected root systems
     !....    present at the time of the cut (given by 2*RRJINC)

     PCENTS(IRRSP,I,3) = PCENTS(IRRSP,I,3) +&
          &2 * RRJINC(IRRSP) * TNJUMP(IRRSP)
     if (ICENSP(IRRSP,I) .ne.0) SPCEN = SPCEN +&
          &PCENTS(IRRSP,I,3)**2
     TOTCEN = TOTCEN + PCENTS(IRRSP,I,3) ** 2
     700 continue
  end do
  SPPROP(IRRSP) = SPCEN / (TOTCEN + 1E-6)

  call RDAREA

  AREANU(IRRSP) = PAREA(IRRSP) - OOAREA(IRRSP)
  if (PAREA(IRRSP) .le.0.0) AREANU(IRRSP) = 0.0
  if (AREANU(IRRSP) .lt.0.0) AREANU(IRRSP) = 0.0
  OOAREA(IRRSP) = PAREA(IRRSP)

  !.... Infect new trees due to the expansion of the centers

  call RDINF

  !.... Loop over tree list if there are any infection centers.
  !.... Calculate the mean jump increment to use next year.

  !     TRENUM - Number of trees outside patch area represented by the
  !              record (so will weight roots by number of trees).
  !     XTRENU - Sum of number of trees outside patch area
  !     XJPINC - Sum of uninfected root radii in appropriate records
  !     RRJINC - Mean uninfected root radii (used to increment the
  !              radii of centers next year)

  650 continue

  RRJINC(IRRSP) = 0
  XJPINC = 0.0
  XTRENU = 0.0
  TRENUM = 0.0

  if (ITRN .eq.0) return

  IDI = IRRSP
  do KSP = 1, MAXSP
     if (ISCT(KSP,1) .eq.0) goto 500
     if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(KSP))
     if (IDI .ne. IRRSP) goto 500

     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)

     do J = I1, I2
        I = IND1(J)
        TRENUM = FPROB(I) * (SAREA - PAREA(IDI))
        XJPINC = XJPINC + ROOTL(I) * TRENUM
        XTRENU = XTRENU + TRENUM
        400 continue
     end do
     500 continue
  end do

  RRJINC(IRRSP) = XJPINC / (XTRENU + 1E-6)

  return
end
