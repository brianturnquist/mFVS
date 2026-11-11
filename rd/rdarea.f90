!=================!
!   rd : rdarea   !
!=================!
! RD $Id$
subroutine RDAREA
  implicit none

  !  Purpose :
  !     THIS ROUTINE COMPUTES THE AREA IN PATCHES USING A GRID
  !     SAMPLING APPROACH.

  !  Called By :
  !     RDCNTL  [ROOT DISEASE]
  !     RDJUMP  [ROOT DISEASE]
  !     RDSETP  [ROOT DISEASE]
  !     RDSPOR  [ROOT DISEASE]
  !     RDBB4   [ROOT DISEASE]

  !  Calls :
  !     DBCHK   (SUBROUTINE)   [FVS]

  !  Local Variables :
  !     LMEM   - Logical
  !              A logical array that works as a grid map of the stand.
  !              If a point in the grid is in a patch then that point is
  !              set to .TRUE.  Used to calculate patch area.
  !     SCL    - Real
  !              a value calculated to translate the X and Y coordinates
  !              from a coordinate system in feet to an integer
  !              coordinate system that is a 75 by 75 grid system.
  !     <incomplete>

  !  Common Block Variables Used :
  !     PAREA    - (RDCOM)   (I/O)
  !     SAREA    - (RDCOM)   (I)
  !     PCENTS   - (RDCOM)   (I)
  !     NCENTS   - (RDCOM)   (I)
  !     <incomplete>

  !  Revision History :
  !   06/10/96 - Matthew K. Thompson
  !     Renamed variables in inline functions.
  !   08/26/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'
  include 'CONTRL.f90'
  include 'RDCOM.f90'

  ! Variables
  logical :: LMEM(75,75),DEBUG

  integer :: ITRN0,ITRN1,IDUM1

  integer :: IGRID,IPAT,I,IN,IX,IX1,IX2,IX3,IXC,IXX,IY1,IY2,IYC,IYR,IYS,IYY,J,K,NTRY
  real    :: SCL,RTRNS,RDUM1,RDUM2,SDUM1,SDUM2,SDUM3

  real    :: ABSDEL,ABSDIF,ARCAL,AREAI,ARINC,ARTAR,ARTEM,DELTA,PERDIF,RNURAD,TOLER,X,Y2

  data IGRID /75/, TOLER/0.01/

  !.... The following are inline functions that define the coordinate
  !.... translation functions.
  !.... ITRN1 -
  !.... ITRN2 -
  !.... RTRNS -

  ITRN1(RDUM1,SDUM1) = IFIX((RDUM1 * SDUM1) + 0.5)
  ITRN0(RDUM2,SDUM2) = IFIX((RDUM2 * SDUM2) - 0.3)
  RTRNS(IDUM1,SDUM3) = FLOAT(IDUM1) / SDUM3

  !.... SEE IF WE NEED TO DO SOME DEBUG.

  call DBCHK (DEBUG,'RDAREA',6,ICYC)

  !.... AREAI is input disease area specified by the user, or if not
  !.... specified it is the default input disease area.

  NTRY = 0
  AREAI = PAREA(IRRSP)
  ARTEM = AREAI

  if (DEBUG) write(JOSTND,111) AREAI,ARTEM
  111 format (' AREAI ARTEM=',2F9.2)

  !.... If there are no patches, set the patch area to zero and return.

  if (NCENTS(IRRSP) .gt.0) goto 5
  PAREA(IRRSP) = 0.0

  if (DEBUG) write(JOSTND,69)
  69 format (' NO PATCHES IN RDAREA ')

  return

  5 continue
  if (DEBUG) write (JOSTND,169) NCENTS(IRRSP),&
       &((PCENTS (IRRSP,I,J),J = 1,3),I = 1,NCENTS(IRRSP))
  169 format (' IN RDAREA: PATCH INFORMATION '/I5,(3E15.7))

  6 continue

  !.... Re-enter area calculation here if last area calculation was
  !.... not within the tolerance limits of the input patch area.

  !.... Initialize LMEM

  do IYY = 1,IGRID
     do IX = 1,IGRID
        LMEM(IX,IYY) = .false.
        10 continue
     end do
     15 continue
  end do

  !.... Compute a scale value to translate the X and Y coordinates
  !.... from feet to a integer coordinate system that is (75,75)

  SCL = FLOAT(IGRID) / SQRT(SAREA * 43560.0)

  !.... Loop over patches.

  do IPAT = 1,NCENTS(IRRSP)

     !....    Compute the location of the center of the patch in the integer
     !....    coordinate system.  Also compute the extent of the patch along
     !....    the X and Y axis.

     IXC = ITRN1(PCENTS(IRRSP,IPAT,1),SCL)
     IYC = ITRN1(PCENTS(IRRSP,IPAT,2),SCL)
     IX1 = ITRN0(PCENTS(IRRSP,IPAT,1) - PCENTS(IRRSP,IPAT,3),SCL)
     IX2 = ITRN1(PCENTS(IRRSP,IPAT,1) + PCENTS(IRRSP,IPAT,3),SCL)
     IY1 = ITRN0(PCENTS(IRRSP,IPAT,2) - PCENTS(IRRSP,IPAT,3),SCL)
     IY2 = ITRN1(PCENTS(IRRSP,IPAT,2) + PCENTS(IRRSP,IPAT,3),SCL)

     !....    Treat the points that fall on the X, and Y axis as special cases.
     !....
     !....    If the Y axis falls out of the stand, none of the points along
     !....    X axis at IYC are in the stand.

     if (IYC .le.0.or. IYC .gt. IGRID) goto 30

     do IX = IX1,IX2

        !....       If the IX's are in the stand, then set the corresponding
        !....       values to .TRUE. in the "MAP" (LMEM).

        if (IX .gt.0.and. IX .le. IGRID) LMEM(IX,IYC) = .true.
        20 continue
     end do

     30 continue

     !....    Do the same thing along the Y axis.

     if (IXC .le.0.or. IXC .gt. IGRID) goto 50

     do IYY = IY1,IY2
        if (IYY .gt.0.and. IYY .le. IGRID) LMEM(IXC,IYY) = .true.
        40 continue
     end do

     50 continue

     !....    Now, process the points in the circle that are not
     !....    on the X, Y axes.

     IX1 = IXC + 1
     IX3 = IX2 - 1

     !....    If there are no other points, then skip this section.

     if (IX1.gt. IX2) goto 80

     do IX = IX1,IX3
        IXX = IXC + IXC - IX
        X = RTRNS(IX-IXC,SCL)
        if (X .gt. PCENTS(IRRSP,IPAT,3)) goto 70

        Y2 = SQRT((PCENTS(IRRSP,IPAT,3) * PCENTS(IRRSP,IPAT,3)) -&
             &(X * X))
        IY2 = IYC + ITRN0(Y2,SCL)
        IY1 = IYC - ITRN1(Y2,SCL)
        if (IY1.le.0) IY1 = 1
        if (IY1.gt. IY2) goto 70

        do IYR = IY1,IY2
           if (IYR .gt. IGRID) goto 60
           if (IX .ge.1.and. IX .le. IGRID) LMEM(IX,IYR) = .true.
           if (IXX .ge.1.and. IXX .le. IGRID)&
                &LMEM(IXX,IYR) = .true.
           60 continue
        end do
        70 continue
     end do

     80 continue
     200 continue
  end do

  !.... Count the points that are in and compute the area.

  IN = 0
  do IYS = 1,IGRID
     do IX = 1,IGRID
        if (LMEM(IX,IYS)) IN = IN + 1
        210 continue
     end do
     215 continue
  end do

  ARCAL = SAREA * (FLOAT(IN) / FLOAT(IGRID*IGRID))
  if (.not. LSTART) goto 220

  !.... If PAREA = -1 then centers were input by user and there is no
  !.... area target to be met: area should be exactly what is calculated.

  if (PAREA(IRRSP) .eq. -1.0) goto 220

  !.... If the calculated acerage is within the allowable tolerance of the
  !.... input acreage specified (user supplied or default) then exit.  If
  !.... not then begin increasing the radii of the patches until the
  !.... tolerance is met. The whole process of matching calculated area
  !.... with input area is only done in the initialization phase of the
  !.... simulation.

  PERDIF = (AREAI - ARCAL) / AREAI
  ABSDIF = ABS(PERDIF)
  DELTA = ARTEM - ARCAL
  ABSDEL = ABS(DELTA)
  if (ABSDIF .le. TOLER .or. ABSDEL .lt. TOLER) goto 220

  !.... Calculate estimate of area increase needed to hit the input area
  !.... target.  This increase is based on a percentage of the patch area
  !.... calculated to the input area specified.  Calculate new radius
  !.... for the circle required to give the target area.

  ARINC = ARTEM * PERDIF
  ARTAR = ARTEM + ARINC
  ARTEM = ARTAR
  ARTAR = ARTAR * 43560.0 / FLOAT(NCENTS(IRRSP))

  if (DEBUG) write(JOSTND,222) ARCAL,ARINC,ARTAR,ARTEM
  222 format(' ARCAL ARINC ARTAR ARTEM=',2(2X,F9.2),2(2X,F12.2))

  RNURAD = SQRT(ARTAR / 3.14159)

  !.... Load PCENTS with the new radius of the circle.

  do K = 1,NCENTS(IRRSP)
     PCENTS(IRRSP,K,3) = RNURAD
     90 continue
  end do

  !.... If we have tried and tried to reach the target area and have been
  !.... unsuccessful up to this point (25 times through the loop) then
  !.... assume the geometry does not allow us to reach it. Take what you
  !.... have and exit.

  NTRY = NTRY + 1
  if (NTRY .gt.25) goto 220

  !.... Go to top of area calculation loop.

  goto 6

  220 continue
  PAREA(IRRSP) = ARCAL
  if (LSTART) OOAREA(IRRSP) = PAREA(IRRSP)

  if (DEBUG) write(JOSTND,223) IRRSP, PAREA(IRRSP)
  223 format(' READY TO LEAVE - DTYPE=', I1, '  PAREA=',F9.2)

  return
end
