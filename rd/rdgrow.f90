!=================!
!   rd : rdgrow   !
!=================!
! RD $Id$
subroutine RDGROW
  implicit none

  !  SUBROUTINE FOR CALCULATING GROWTH INCREMENT REDUCTION
  !  FROM ROOT DISEASE ROOT DISEASE INFECTION

  !  CALLED BY :
  !     RDTREG  [ROOT DISEASE]

  !  CALLS     :
  !     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
  !     RDSLP   (FUNCTION)     [ROOT DISEASE]

  !  Revision History :
  !   03/07/95 - Last revision date.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'RDADD.f90'

  ! Variables
  integer :: I,IDI,IP,IS,J
  real    :: BOTTOM,DGIMP,DIFF,OUTNUM,PRADI,RDSLP,TPAREA

  TPAREA = 0.0
  do IDI = MINRR,MAXRR
     TPAREA = TPAREA + PAREA(IDI)
     150 continue
  end do

  if (ITRN .le.0.or. TPAREA .eq.0.0) return

  call RDSUM(ITRN,PROBIT,PROBI,ISTEP)

  IDI = MAXRR
  do I = 1,ITRN

     if (MAXRR .lt.3) IDI = IDITYP(IRTSPC(ISP(I)))

     !     Skip operations if non-host species (RNH May98)

     if (IDI .le.0) go to 1000

     if (PAREA(IDI) .le.0.0) goto 1000

     IS = ISP(I)
     HTTOT = 0.0
     DGTOT = 0.0

     do J = 1,ISTEP
        do IP = 1,2

           if (PROBI(I,J,IP) .le.1E-3) goto 500

           !....          If infection has not yet reached the center, tree is not
           !....          affected by the infection (temporarily set radius of
           !....          infection to zero).

           PRADI = PROPI(I,J,IP)
           if (PRADI .lt.0.0) PRADI = 0.0

           HTIMP = RDSLP(PRADI,XHT,YHT,2) * HTIFAC(IRTSPC(IS))
           DGIMP = RDSLP(PRADI,XDBH,YDBH,2) * DBIFAC(IRTSPC(IS))
           HTTOT = HTTOT + HTIMP * PROBI(I,J,IP) /&
                &(PROBIT(I) + 1E-6)
           DGTOT = DGTOT + DGIMP * PROBI(I,J,IP) /&
                &(PROBIT(I) + 1E-6)
           500 continue
        end do
        550 continue
     end do

     DIFF = SAREA - PAREA(IDI)
     if (DIFF .le.1E-3.and. PROBIU(I) .le.1E-3.and.&
          &PROBIT(I) .le.1E-3) goto 999

     OUTNUM = FPROB(I) * (SAREA - PAREA(IDI))
     BOTTOM = OUTNUM + PROBIU(I) + PROBIT(I)

     if (BOTTOM .le.1E-6) then
        DG(I) = 0.0001
        HTG(I) = 0.0001
     else
        DG(I) = (DG(I) * (OUTNUM + PROBIU(I)) +&
             &DG(I) * DGTOT * PROBIT(I)) / BOTTOM
        HTG(I) = (HTG(I) * (OUTNUM + PROBIU(I)) +&
             &HTG(I) * HTTOT * PROBIT(I)) / BOTTOM
     end if

     999 continue
     if (DG(I) .lt.0.0001) DG(I) = 0.0001
     if (HTG(I) .lt.0.0001) HTG(I) = 0.0001

     1000 continue
  end do

  return
end
