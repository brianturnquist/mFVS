!=================!
!   rd : rdinoc   !
!=================!
! RD $Id$
subroutine RDINOC(LICALL)
  implicit none

  !  Purpose :
  !     Decomposes infected root systems in the dead tree/stump list.

  !  Called By :
  !     RDSETP  [ROOT DISEASE]
  !     RDTREG  [ROOT DISEASE]

  !  Calls :
  !     RDSLP   (FUNCTION)   [ROOT DISEASE]

  !  Common block variables used :
  !     DECRAT - Decay rate of roots.
  !     JRSIT  - Number of years to 'sit' without decreasing root
  !              radius
  !     ROTSIT - Root radius at which to begin 'sitting' without
  !              decreasing (feet)

  !  Local variables :

  !     DEBUG  - Logical flag to turn debug on or off.
  !     JINT   - Interval over which to decay roots.
  !     LICALL - .TRUE. = Called during initialization, .FALSE. = Called
  !              elsewhere.
  !     RTODEC - Root radius to decay this cycle.
  !     RTREM  - Root radius remaining after decay.

  ! Revision History:
  !   26-JUN-2002 Lance David (FHTET)
  !     (last revision date noted was 3/25/97)
  !     Added debug code to track down devide by zero error that has
  !     occurred.
  !   30-JUL-2002 Lance David (FHTET)
  !     Added check and reset of root radius when current radius is
  !     less than the value calculated as the radius at which the
  !     root system is suppose to sit for x years.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'RDADD.f90'
  include 'RDCOM.f90'
  include 'PLOT.f90'

  !.... Local variables.

  ! Variables
  integer :: I,IDI,J,JINT,K
  real    :: DROOTS,TMINLF,RTODEC,RTREM

  logical :: DEBUG,LICALL

  !.... See if we need to do some debug.

  call DBCHK (DEBUG,'RDINOC',6,ICYC)

  if (DEBUG) then
     write (JOSTND,900) ICYC,LICALL
     900 format (' Begin RDINOC, Cycle = ', I5,' LICALL=',L)
  end if

  JINT = INT(FINT)

  do IDI = MINRR,MAXRR
     do I = 1, 2
        do J = 1, 5
           do K = 1, ISTEP
              if (DEBUG) write (JOSTND,*)&
                   &'IN RDINOC: IDI=',IDI,' I=',I,' J=',J,' K=',K

              if (PROBDA(IDI,I,J,K) .ne.0.0.and.&
                   &DBHDA(IDI,I,J,K) .ne.0.0) then

                 if (LICALL) then
                    JINT = ABS(JRAGED(IDI,I,J,K))
                    JRAGED(IDI,I,J,K) = 0
                 end if

                 ROTSIT = RSITFN(IDI,1) * DBHDA(IDI,I,J,K) +&
                      &RSITFN(IDI,2)

                 !....                If the current root radius ROOTDA stored for the class
                 !....                is less than that calculated as the radius at which
                 !....                the root systems sits ROTSIT for x years, set the
                 !....                ROOTDA to ROTSIT. So that the decay process does
                 !....                not try to grow the root system. The weighted averaging
                 !....                of individuals into the class may have caused this
                 !....                backward condition to occur. LRD 30JUL02

                 if (ROOTDA(IDI,I,J,K) .lt. ROTSIT) then
                    ROOTDA(IDI,I,J,K) = ROTSIT
                 end if

                 !....                Armillaria & Phellinus change functions at
                 !....                12in DBH.

                 if (DBHDA(IDI,I,J,K) .le.12.0) then
                    JRSIT = INT(YRSITF(IDI,1,1) *&
                         &DBHDA(IDI,I,J,K) +&
                         &YRSITF(IDI,2,1))
                 else
                    JRSIT = INT(YRSITF(IDI,1,2) *&
                         &DBHDA(IDI,I,J,K) +&
                         &YRSITF(IDI,2,2))
                 end if

                 if (DEBUG) write (JOSTND,*)&
                      &'IN RDINOC: DBHDA=',DBHDA(IDI,I,J,K),' JINT=',JINT,&
                      &' JRAGED=',JRAGED(IDI,I,J,K),' ROTSIT=',ROTSIT,&
                      &' JRSIT=',JRSIT,'PROBDA=',PROBDA(IDI,I,J,K),&
                      &' RSITFN1=',RSITFN(IDI,1),' RSITFN2=',RSITFN(IDI,2)

                 if (JRAGED(IDI,I,J,K) .le.0) then

                    if (DECRAT(IDI,I,J,K) .le.0.0) then

                       !....                      If stump has not yet started decaying
                       !....                      then find the decay rate.  The decay
                       !....                      rate is a function of root radius only.

                       if (DBHDA(IDI,I,J,K) .le.12.0) then
                          DECRAT(IDI,I,J,K) = (DECFN(IDI,1,1) *&
                               &ROOTDA(IDI,I,J,K) +&
                               &DECFN(IDI,2,1)) /&
                               &DSFAC(I)
                       else
                          DECRAT(IDI,I,J,K) = (DECFN(IDI,1,2) *&
                               &ROOTDA(IDI,I,J,K) +&
                               &DECFN(IDI,2,2)) /&
                               &DSFAC(I)
                       end if

                       !....                      Modify the decay rate to account for the
                       !....                      minimum lifespan of inoculum (default is 0
                       !....                      but user may change)

                       DROOTS = ROOTDA(IDI,I,J,K) - ROTSIT

                       TMINLF = JRSIT + DROOTS / DECRAT(IDI,I,J,K)

                       if (DEBUG) write (JOSTND,*)&
                            &'IN RDINOC: DECRAT=',DECRAT(IDI,I,J,K),&
                            &' ROOTDA=',ROOTDA(IDI,I,J,K),&
                            &' DROOTS=',DROOTS,' TMINLF=',TMINLF,&
                            &' XMINLF=',XMINLF(IDI)

                       if (TMINLF .lt. XMINLF(IDI)) then
                          DECRAT(IDI,I,J,K) = DROOTS /&
                               &(XMINLF(IDI) - JRSIT)
                       end if
                    end if

                    !....                   If stump decay has not yet reached the 'core'
                    !....                   part then keep decreasing the root radius.

                    RTODEC = DECRAT(IDI,I,J,K) * JINT
                    RTREM = ROOTDA(IDI,I,J,K) - RTODEC

                    if (RTREM .lt. ROTSIT) then
                       RTODEC = ROOTDA(IDI,I,J,K) - ROTSIT
                       if (RTODEC .lt.0.0) RTODEC = 0.0
                       JRAGED(IDI,I,J,K) = JINT - INT(RTODEC /&
                            &DECRAT(IDI,I,J,K))
                       RTREM = ROTSIT
                    end if

                    ROOTDA(IDI,I,J,K) = RTREM
                 else

                    JRAGED(IDI,I,J,K) = JRAGED(IDI,I,J,K) + JINT
                 end if

                 if (JRAGED(IDI,I,J,K) .gt. JRSIT) then

                    !....                   If stump has sat with no apparent decay for
                    !....                   enough time then it disappears.

                    PROBDA(IDI,I,J,K) = 0.0
                    DBHDA(IDI,I,J,K) = 0.0
                    JRAGED(IDI,I,J,K) = 0
                    ROOTDA(IDI,I,J,K) = 0.0
                 end if
              end if

              300 continue
           end do
           400 continue
        end do
        500 continue
     end do
     600 continue
  end do

  if (DEBUG) then
     write (JOSTND,910) ICYC
  end if

  910 format (' End RDINOC, Cycle = ', I5)

  return
end
