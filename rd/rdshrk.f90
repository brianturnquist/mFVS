!=================!
!   rd : rdshrk   !
!=================!
! RD $Id$
subroutine RDSHRK
  implicit none

  !  Purpose :
  !     Reduces the radius of centers.  Once centers have started to
  !     decay they are likely to continue decaying, unless there are
  !     a significant number of new infections.  Centers decay in the
  !     same manner as the biggest stump present so the center radius
  !     decays for a short time and then "sits" at some size for a
  !     while and then disappears.  Centers should therefor disappear
  !     when there is no inculum left in the stand.

  !  Called By :
  !     RDCNTL  (ROUTINE)  [ROOT DISEASE]

  !  Calls     :
  !     RDSLP   (FUNCTION)   [ROOT DISEASE]

  !  Common block variables Used :
  !     SHCENT(,,1) = Root radius to decay immediately.
  !     SHCENT(,,2) = Annual rate to decay that amount of root.
  !     SHCENT(,,3) = Time to decay remaining amount of roots

  !  Local Variables :

  !     DEBUG:  Logical flag to turn debug on or off.

  ! Revision History:
  !   24-FEB-2004 Lance R. David (FHTET)
  !      At DO loop 700 a GOTO loop existed within it which lead to
  !      an array out of bounds error.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'PLOT.f90'
  include 'RDADD.f90'

  !.... Local variables.

  ! Variables
  logical :: DEBUG
  integer :: I,I1,I2,ICEN,IDI,IJ,IPNT(100),IRSP,ISHR,ITMP,J,KNT,KSP,NDIF
  real    :: DBHM,REDAMT,ROOTM,RSFAC,TCLAS,TMINLF,TSTU,YREM

  !.... SEE IF WE NEED TO DO SOME DEBUG.

  call DBCHK (DEBUG,'RDSHRK',6,ICYC)

  if (DEBUG) write (JOSTND,999) ICYC
  999 format (' Begin RDSHRK, Cycle = ', I5)

  !.... SEE IF THERE IS ANY REASON TO DECREASE THE SIZE OF CENTERS
  !.... FIRST CHECK THE NUMBER OF LIVE (INFECTED AND UNINFECTED) INSIDE CENTERS

  IDI = IRRSP

  TCLAS = 0.0
  IRSP = IDI

  do KSP = 1,MAXSP
     if (IDI .lt.3) IRSP = IDITYP(IRTSPC(KSP))
     if ((ISCT(KSP,1) .eq.0) .or. (IRSP .ne. IDI)) goto 800

     I1 = ISCT(KSP,1)
     I2 = ISCT(KSP,2)

     do J = I1,I2
        I = IND1(J)
        TCLAS = TCLAS + PROBIU(I) + PROBIT(I)
        750 continue
     end do
     800 continue
  end do

  !.... IF THERE ARE MORE TREES THAN CENTERS THEN NO NEED TO SHRINK CENTERS
  !.... AND ANY CENTERS THAT WERE SHRINKING STOP

  if (TCLAS .ge. NCENTS(IDI)) then
     if (NSCEN(IDI) .gt.0.0) then

        do ICEN = 1,NCENTS(IDI)
           if (SHCENT(IDI,ICEN,2) .gt.0.0) then
              SHCENT(IDI,ICEN,1) = 0.0
              SHCENT(IDI,ICEN,2) = 0.0
              SHCENT(IDI,ICEN,3) = 0.0
              RRATES(IDI,ICEN) = 0.0
           end if
           50 continue
        end do

        NSCEN(IDI) = 0
     end if

     goto 9000
  end if

  !.... Number of centers to shrink.

  ISHR = NCENTS(IDI) - INT(TCLAS)

  !.... Determine which centers to shrink

  if (ISHR .gt. NSCEN(IDI)) then

     !....    There are too few shrinking centers so start picking smallest
     !....    non-shrinking ones to start shrinking.  Set up a list of
     !....    pointers which references these indices.

     do ICEN = 1, NCENTS(IDI)
        IPNT(ICEN) = ICEN
        1000 continue
     end do

     if (ISHR .lt. NCENTS(IDI)) then

        !....       Set up pointers to the size of centers in ascending order if
        !....       not all centers will start shrinking.

        do IJ = 1,NCENTS(IDI) - 1
           do I = 1,NCENTS(IDI) - 1
              if (PCENTS(IDI,IPNT(I), 3) .gt.&
                   &PCENTS(IDI,IPNT(I+1),3)) then
                 ITMP = IPNT(I)
                 IPNT(I) = IPNT(I+1)
                 IPNT(I+1) = ITMP
              end if
              2000 continue
           end do
           2100 continue
        end do
     end if

     !....    Find the parameters to use to shrink centers (They'll be
     !....    the same for all centers). Choose the biggest stump present,
     !....    choosing from resinous first (since they take longer to decay)

     DBHM = 0.0
     ROOTM = 0.0
     TSTU = 0.0
     RSFAC = 1.0

     do J = 5, 1, -1
        do I = 1, 2
           if (PROBD(IDI,I,J) .ne.0.0) then
              DBHM = DBHM + DBHD(IDI,I,J) * PROBD(IDI,I,J)
              ROOTM = ROOTM + ROOTD(IDI,I,J) * PROBD(IDI,I,J)
              RSFAC = DSFAC(I)
              TSTU = TSTU + PROBD(IDI,I,J)
              if (TSTU .gt.0.0) goto 550
           end if
           400 continue
        end do
        500 continue
     end do

     550 continue

     DBHM = DBHM / (TSTU + 1E-6)
     ROOTM = ROOTM / (TSTU + 1E-6)

     ROTSIT = RSITFN(IDI,1) * DBHM + RSITFN(IDI,2)

     if (DBHM .le.12) then
        JRSIT = INT(YRSITF(IDI,1,1) * DBHM + YRSITF(IDI,2,1))
     else
        JRSIT = INT(YRSITF(IDI,1,2) * DBHM + YRSITF(IDI,2,2))
     end if

     KNT = 1

     do IJ = 1,(ISHR-NSCEN(IDI))

        350 continue

        ICEN = IPNT(KNT)
        KNT = KNT + 1

        if (SHCENT(IDI,ICEN,2) .eq.0.0) then

           !....          This center has not yet started shrinking so assign
           !....          values.

           SHCENT(IDI,ICEN,1) = ROOTM - ROTSIT
           SHCENT(IDI,ICEN,3) = JRSIT
           if (DBHM .le.12) then
              SHCENT(IDI,ICEN,2) = (DECFN(IDI,1,1) * ROOTM +&
                   &DECFN(IDI,2,1)) / RSFAC
           else
              SHCENT(IDI,ICEN,2) = (DECFN(IDI,1,2) * ROOTM +&
                   &DECFN(IDI,2,2)) / RSFAC
           end if

           if (SHCENT(IDI,ICEN,1) .lt.0.0) SHCENT(IDI,ICEN,1) = 0.0

           !....          Modify the shrink rate to account for the minimum lifespan
           !....          of inoculum.

           TMINLF = JRSIT + SHCENT(IDI,ICEN,1) / SHCENT(IDI,ICEN,2)

           if (TMINLF .lt. XMINLF(IDI)) then
              SHCENT(IDI,ICEN,2) = SHCENT(IDI,ICEN,1) /&
                   &(XMINLF(IDI) - JRSIT)
           end if

        else
           goto 350
        end if

        300 continue
     end do

     NSCEN(IDI) = ISHR

  else if (ISHR .lt. NSCEN(IDI)) then

     !....    There are too many shrinking centers so need to stop shrinking some.
     !....    Choose biggest centers to stop shrinking, again, set up a list of pointers
     !....    to the biggest centsrs.

     do ICEN = 1,NCENTS(IDI)
        IPNT(ICEN) = ICEN
        4000 continue
     end do

     if (ISHR .gt.0) then

        !....       Set up pointers to the size of centers in decending order if
        !....       only some centers will stop shrinking (since if all centers stop
        !....       shrinking, it doesn't matter what order we stop them in)

        do IJ = 1, NCENTS(IDI) - 1
           do I = 1, NCENTS(IDI) - 1
              if (PCENTS(IDI,IPNT(I),3) .lt.&
                   &PCENTS(IDI,IPNT(I+1),3)) then
                 ITMP = IPNT(I)
                 IPNT(I) = IPNT(I+1)
                 IPNT(I+1) = ITMP
              end if
              5000 continue
           end do
           5100 continue
        end do
     end if

     !....    Loop through the centers and zero out the SHCENT arrays for those
     !....    that will no longer shrink.  Also, set the spread rate to zero.

     KNT = 1
     NDIF = NSCEN(IDI) - ISHR

     if (DEBUG) write (JOSTND,*) 'IN RDSHRK: ISHR=',ISHR,&
          &' NDIF=',NDIF,' NSCEN(IDI)=',NSCEN(IDI),' (AT 700 LOOP)'

     do I = 1,NDIF
        !C  650       CONTINUE
        ICEN = IPNT(KNT)

        if (DEBUG) write (JOSTND,*) 'IN RDSHRK: I=',I,' KNT=',KNT,&
             &' ICEN=',ICEN

        KNT = KNT + 1

        if (SHCENT(IDI,ICEN,2) .gt.0.0) then
           SHCENT(IDI,ICEN,1) = 0.0
           SHCENT(IDI,ICEN,2) = 0.0
           SHCENT(IDI,ICEN,3) = 0.0
           RRATES(IDI,ICEN) = 0.0
           NSCEN(IDI) = NSCEN(IDI) - 1
           !C            ELSE
           !C               GOTO 650
        end if
        700 continue
     end do
  end if

  if (DEBUG) write (JOSTND,*) 'IN RDSHRK: NCENTS(IDI)=',&
       &NCENTS(IDI),' (AT 900 LOOP)'

  do I = 1, NCENTS(IDI)
     if (SHCENT(IDI,I,2) .eq.0.0) goto 900

     if (SHCENT(IDI,I,1) .gt.0.0) then
        REDAMT = SHCENT(IDI,I,2) * FINT

        if (SHCENT(IDI,I,1) .lt. REDAMT) then

           !....          Only reduce this first amount in the initial timestep.

           REDAMT = SHCENT(IDI,I,1)

           !....          But since it didn't take all the timesteps to reduce the
           !....          initial amount,  reduce the time remaining (INDX 3) by
           !....          the rest of the timestep.

           YREM = FINT - SHCENT(IDI,I,1) / SHCENT(IDI,I,2)
           SHCENT(IDI,I,3) = SHCENT(IDI,I,3) - YREM
        end if

        !....       Reduce the initial radius parameter here.

        SHCENT(IDI,I,1) = SHCENT(IDI,I,1) - REDAMT
        SHCENT(IDI,I,1) = AMAX1(SHCENT(IDI,I,1),0.0)

        !....       Set a negative rate for use later in RDCNTL.

        RRATES(IDI,I) = -REDAMT / FINT
     else

        !....       If the initial amount is gone, no shrinkage occurs.

        RRATES(IDI,I) = 0.0

        !....       But the time remaining to the center gets reduced and
        !....       If no time left then set the rate so that the center
        !....       disappears.

        SHCENT(IDI,I,3) = SHCENT(IDI,I,3) - FINT
        if (SHCENT(IDI,I,3) .le.0.0)&
             &RRATES(IDI,I) = -PCENTS(IDI,I,3) / FINT
     end if
     900 continue
  end do

  9000 continue

  if (DEBUG) write (JOSTND,9100) ICYC
  9100 format (' End RDSHRK, Cycle = ', I5)

  return
end
