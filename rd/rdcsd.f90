!================!
!   rd : rdcsd   !
!================!
! RD $Id$
subroutine RDCSD(ISPI,DBHLIM,THRESH,CRIT)
  implicit none

  !  Purpose :
  !    CALCULATES THE CRITICAL STAND DENSITY THAT DETERMINES WHETHER OR
  !    NOT AN OUTBREAK OF BARK BEETLE 1 OCCURS.
  !    Depending on the arguments supplied to the keyword DNSCALC, the user
  !    can select from the following to define the critical density:

  !    METHOD: STEM or SDI
  !    SPACE:  ALL or OUTSDE
  !    DTYPE:  LIVE or ALL

  !    where:

  !    STEM    is stems/acre greater than the DBHLIM supplied
  !    SDI     is Reinecke's Stand Density Index.  This method sums host +
  !            non-host trees if SPACE ALL is selected.  If SPACE OUTSDE is
  !            selected, only BB host trees outside root rot centers are
  !            included.  In neither case is DBHLIM used to screen out trees.
  !    ALL     for SPACE, includes the whole stand area, both inside and
  !             outside of root rot infection centers.
  !            for DTYPE, includes all live trees (whether infected or not),
  !             AND all trees that died during the last two years (whether
  !             infected or not, and without regard to the cause of death).
  !    OUTSDE  includes only areas outside root rot-infected areas
  !    LIVE    includes only living trees (both infected and not infected).

  !    The meaning of THRESH depends upon the METHOD; it can be either
  !    stems/acre or the SDI value.

  !  CALLED BY :
  !     RDBB1  [ROOT DISEASE]

  !  CALLS     :
  !     DBCHK  (SUBROUTINE)   [PROGNOSIS]

  !  PARAMETERS :
  !     ISPI - the species targetted by the beetle      (INTEGER)
  !     DBHLIM - the dbh limit when the METHOD is STEM  (REAL)
  !     THRESH - the threshold stems/acre or SDI value  (REAL)
  !     CRIT - exceeds critical value for outbreak      (LOGICAL)

  !  VARIABLES
  !     TOTDEN = the TOTal DENsity over the whole stand of trees that
  !              meet the user's criteria
  !     NSUM   = Number SUM - a sum of numbers of trees that meet the
  !              user's criteria
  !     DOSUM  = Density Outside SUM - a sum of the density outside centers
  !              of trees that meet the user's criteria
  !     BASUM  = Basal Area SUM - the total basal area in the stand of trees
  !              that meet the user's criteria
  !     PIBY4  = PI divided BY 4, used to save work in iterative computation
  !     PIABY4 = PI * stand Area divided BY 4, used to save work

  !  Revision History :
  !     06/12/96  Matthew K. Thompson
  !               Moved the declaration of DSO, DSII, and DSIU to the
  !               parameter include file RDPARM.
  !     21-MAR-00 Lance David (FHTET)
  !               Changed local variable "SLOPE" to "REISLP" because SLOPE
  !               is a FVS variable in common PLOT.F77.
  !     15-JUL-02 Lance David (FHTET)
  !               Added debug.
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
  include 'PLOT.f90'

  ! Variables
  integer :: ALLX,DTYPE,I,I1,I2,ISPI,J,LIVE,METHOD,OUTSDE,RRTYPE,SDI,SPACE,STEM

  real    :: ASD,BASUM,DBHLIM,DOSUM,NSUM,PIBY4,PIABY4,REISLP,RRFREE,SDIVAL,TEMP,THRESH,TOTDEN

  logical :: CRIT,DEBUG

  !    Index conventions for arrays and logic

  data ALLX     /0 /
  data OUTSDE   /1/

  data LIVE     /1/

  data STEM     /1/
  data SDI      /2/
  !.... SEE IF WE NEED TO DO SOME DEBUG.

  call DBCHK (DEBUG,'RDCSD',5,ICYC)
  if (DEBUG) write (JOSTND,*) 'ENTER RDCSD: ISPI=',ISPI,&
       &' DBHLIM=', DBHLIM,'THRESH=',THRESH,' CRIT=',CRIT

  ! REINEK(1) triggers the METHOD. If REINEK(1) is zero, then the METHOD
  ! is STEMS; otherwise it is SDI. REINEK(2) triggers SPACE. If
  ! REINEK(2) is zero, SPACE is ALL; otherwise it is OUTSDE. REINEK(3)
  ! triggers the DTYPE. If REINEK(2) is zero, DTYPE is LIVE and does
  ! not include recently dead trees in the density calculation; otherwise, it
  ! is ALL and trees that died in the last cycle are included.
  ! REINEK(4) optionally triggers a change in the SLOPE (Error check in
  ! subroutine 'rrin').

  ! Default options for critical density calculations.

  METHOD = STEM
  SPACE = ALLX
  DTYPE = LIVE

  if (REINEK(1) .eq.0.0) goto 4
  METHOD = SDI

  4 continue
  if (REINEK(2) .eq.0.0) goto 5
  SPACE = OUTSDE

  5 continue
  if (REINEK(3) .eq.0.0) goto 6
  DTYPE = ALLX

  6 continue
  REISLP = REINEK(4)

  CRIT = .false.
  if (DEBUG) write (JOSTND,*) 'IN RDCSD: METHOD=',METHOD,' SPACE=',&
       &SPACE,' DTYPE=',DTYPE,' REISLP=',REISLP

  ! Find out what kind of fungus infects the host species
  RRTYPE = MAXRR
  if (MAXRR .lt.3) RRTYPE = IDITYP(IRTSPC(ISPI))

  if (DEBUG) write (JOSTND,*) 'IN RDCSD: RRTYPE=',RRTYPE,&
       &' PAREA=',PAREA(RRTYPE),' SAREA=',SAREA

  if ((SPACE .eq. OUTSDE) .and. (PAREA(RRTYPE) .ge. SAREA)) goto 778

  ! Branch to 200 if the Reineke Stand Density Index is being used.

  if (METHOD .eq. SDI) goto 200

  !  METHOD 1:
  !  Use the "Eligible Stems/Acre" method for determining if the outbreak
  !  threshold density has been passed.

  if (DEBUG) write (JOSTND,*) 'IN RDCSD: CALC METHOD 1 - STEMS'

  ! Find the first and last tree records of the host species.
  I1 = ISCT(ISPI,1)
  I2 = ISCT(ISPI,2)

  ! Look at every record of the species, and, for those that exceed
  ! the dbh limit, count the portions of the record that meet the user's
  ! criteria.

  NSUM = 0.0
  DOSUM = 0.0

  do J = I1,I2
     I = IND1(J)
     if (DBH(I) .lt. DBHLIM) goto 100

     !        count live stems outside
     DOSUM = DOSUM + FPROB(I)

     !        add recent dead stems outside
     if (DTYPE .eq. ALLX) NSUM = NSUM + ((2/FINT) * PROAKL(DSO,I))

     if (SPACE .eq. ALLX) then
        !           add live stems inside
        NSUM = NSUM + PROBIT(I) + PROBIU(I)

        !           add recent dead stems inside
        if (DTYPE .eq. ALLX) NSUM = NSUM + ((2/FINT)&
             &* (PROAKL(DSII,I) + PROAKL(DSIU,I) + PRANKL(I)))
     end if
     100 continue
  end do

  ! Calculate TOTDEN, compare it to the user's threshold, and use CRIT to signal
  ! whether the threshold was surpassed.
  if (SPACE .eq. ALLX) then
     TOTDEN = (NSUM + DOSUM*(SAREA - PAREA(RRTYPE))) / SAREA
  else
     TOTDEN = DOSUM + NSUM / (SAREA - PAREA(RRTYPE))
  end if

  if (TOTDEN .ge. THRESH) CRIT = .true.

  if (DEBUG) write (JOSTND,*) 'IN RDCSD: DOSUM=',DOSUM,&
       &' NSUM=',NSUM,' TOTDEN=',TOTDEN,' CRIT=',CRIT

  goto 778

  200 continue

  !  METHOD 2:
  !  Calculate threshold densities by Reinecke's SDI method.

  ! The model needs the Average Stand Diameter (ASD) of all trees that
  ! meet the user's criteria (live vs. dead, host-outside-centers vs.
  ! all-species-everywhere). The first step is to find the total basal
  ! area of these trees in the stand.
  ! Basal Area of each tree = pi * (DBH/2)^2.
  ! PIBY4 and PIABY4 are used to save work in the iterative computations.
  ! NOTE:  THERE IS SOME ERROR involved in using DBH(I) as the dbh of both
  ! live and dead trees in the record.

  if (DEBUG) write (JOSTND,*) 'IN RDCSD: CALC METHOD 2 - SDI'

  NSUM = 0.0
  BASUM = 0.0
  PIBY4 = 0.785398163
  PIABY4 = PIBY4 * SAREA

  if (SPACE .eq. OUTSDE) goto 301

  ! If SPACE = ALL, consider all tree species everywhere:

  if (DEBUG) write (JOSTND,*) 'IN RDCSD: SPACE = ALL'

  do I = 1,ITRN

     !        start with live stems
     NSUM = NSUM + PROBL(I) * SAREA
     BASUM = BASUM + PROBL(I) * PIABY4 * DBH(I)**2

     !        add recent dead stems
     if (DTYPE .eq. ALLX) then
        TEMP = (2/FINT) *&
             &(PROAKL(DSO,I)+PROAKL(DSII,I)+PROAKL(DSIU,I)+PRANKL(I))
        NSUM = NSUM + TEMP
        BASUM = BASUM + TEMP * PIBY4 * DBH(I)**2
     end if
     300 continue
  end do
  TOTDEN = NSUM / SAREA
  goto 401

  ! If SPACE = OUTSDE, consider only trees outside the disease centers of either root
  ! rot type.  This procedure assumes that there is no overlap in the two types of disease
  ! centers, but this assumption does not create any error since the end result is a
  ! density of trees (not a number).  Note that the model assumes that root rot centers do
  ! not affect the density of non-host trees.

  301 continue

  if (DEBUG) write (JOSTND,*) 'IN RDCSD: SPACE = OUTSIDE'

  RRFREE = SAREA
  do RRTYPE = MINRR,MAXRR
     RRFREE = RRFREE - PAREA(RRTYPE)
     340 continue
  end do

  do I = 1,ITRN

     !        start with live stems outside
     TEMP = FPROB(I) * RRFREE
     NSUM = NSUM + TEMP
     BASUM = BASUM + (TEMP * PIBY4 * DBH(I)**2)

     !        add recent dead stems outside
     if (DTYPE .eq. ALLX) then
        RRTYPE = MAXRR
        if (MAXRR .lt.3) RRTYPE = IDITYP(IRTSPC(ISP(I)))
        TEMP = (2/FINT) * PROAKL(DSO,I)&
             &* RRFREE / (SAREA - PAREA(RRTYPE))
        NSUM = NSUM + TEMP
        BASUM = BASUM + TEMP * PIBY4 * DBH(I)**2
     end if
     400 continue
  end do
  TOTDEN = NSUM / RRFREE

  401 continue

  ! Calculate the ASD of trees that met the user's criteria from the total basal area
  ! of these trees:  total BA = #trees * pi * (ASD/2)^2.  Use ASD and TOTDEN to find
  ! the SDIVALue of the stand from a standard equation.  Compare SDIVAL to the threshold
  ! and use CRIT to signal whether the conditions for an outbreak have been met.

  if (NSUM .gt.0.0) then
     ASD = SQRT(BASUM / (NSUM * PIBY4))
     SDIVAL = 10.0**(LOG10(TOTDEN) - (REISLP * LOG10(ASD)) + REISLP)

     if (SDIVAL .ge. THRESH) CRIT = .true.
  end if

  if (DEBUG) write (JOSTND,*) 'IN RDCSD: NSUM=',NSUM,' ASD=',ASD,&
       &' SDIVAL=',SDIVAL,' CRIT=',CRIT

  778 continue
  if (DEBUG) write (JOSTND,*) 'EXIT: RDCSD'

  return
end
