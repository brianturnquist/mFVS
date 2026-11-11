!=================!
!   rd : rdspor   !
!=================!
! RD $Id$
subroutine RDSPOR
  implicit none

  !  Purpose :
  !     Updates stump arrays and creates new disease centers from
  !     stumps infected by spores in previous time step. No crossover
  !     occurs and only stumps that are big enough are added to the stump list.

  !  Called By :
  !     RDCNTL  [ROOT DISEASE]

  !  Calls :
  !     DBCHK   [FVS]
  !     RDAREA  [ROOT DISEASE]
  !     RDINUP  [ROOT DISEASE]
  !     RDSPL1  [ROOT DISEASE]
  !     RDSPL2  [ROOT DISEASE]

  !  Local Variables :

  !  Common Block Variables Used :

  !  Revision History :
  !     03/21/97 - Matt Thompson (FHTET)
  !                Deleted commented out code and cleaned up code.
  !     04/03/97 - Matt Thompson (FHTET)
  !                Modified the code to better handle moving spore
  !                infected stumps to the stump lists and the
  !                new centers lists.
  !   09/03/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDADD.f90'

  ! Variables
  logical :: DEBUG
  integer :: I,IDI,ISPINT,IST,J,JJ
  real    :: DEN,DIAM,RCEN,RTD,SPCEN,STNEW,TOTCEN,TST

  call DBCHK(DEBUG,'RDSPOR',6,ICYC)

  IST = INT(AMAX0(1,ISTEP))

  !.... Calculate averages of infected root characteristics for each
  !.... root disease type.

  do IRRSP = MINRR, MAXRR
     call RDINUP
     50 continue
  end do

  do ISPINT = 3, 2, -1

     if (LSPFLG(ISPINT) .and.&
          &(IY(ICYC) - ISDATE(ISPINT)) .gt.5) then

        !....       Clear the flag that says a stand entry occurred.

        LSPFLG(ISPINT) = .false.

        !....       Stand entry occurred greater then 5 years ago, so transfer
        !....       spore infected stumps to stump list or start new centers.

        do IDI = 1,2
           do I = 1,2
              do J = 1,5

                 !....                Spore infected stumps inside of same disease type
                 !....                are added to inside center stump list (If stumps
                 !....                are of a large enough diameter.)

                 DEN = STUIN(IDI,I,J,ISPINT)
                 DIAM = DBHUIN(IDI,I,J,ISPINT)

                 if (DIAM .lt. SPDBH(IDI)) goto 650
                 if (DEN .le.1E-4) goto 650

                 if (DEBUG) then
                    write (JOSTND,*) 'RDSPOR : SAME TYPE, ',&
                         &'INS STUMPS'
                    write (JOSTND,*) 'IDI I J DEN', IDI,I,J,DEN
                 end if

                 TST = PROBDA(IDI,I,J,IST) + DEN
                 DBHDA(IDI,I,J,IST) = (DBHDA(IDI,I,J,IST) *&
                      &PROBDA(IDI,I,J,IST) + DIAM * DEN) / TST

                 !....                Assume only 50% of root radius is infected and add
                 !....                only that much to the root list (to be consistent
                 !....                with the way infected tree roots are added to the
                 !....                list)

                 ROOTDA(IDI,I,J,IST) = (ROOTDA(IDI,I,J,IST) *&
                      &PROBDA(IDI,I,J,IST) + 0.5 *&
                      &RTUIN(IDI,I,J,ISPINT) *&
                      &DEN) / TST

                 PROBDA(IDI,I,J,IST) = PROBDA(IDI,I,J,IST) + DEN

                 650 continue

                 !....                Stumps that are infected outside of centers
                 !....                create new centers if they are large enough.

                 RCEN = STOUT(IDI,I,J,ISPINT)
                 if (RCEN .le.1E-4) goto 770

                 DIAM = DBHOUT(IDI,I,J,ISPINT)
                 RTD = RTOUT(IDI,I,J,ISPINT)

                 if (DEBUG) then
                    write (JOSTND,*) 'RDSPOR :  BEFORE RRSPL2'
                    write (JOSTND,*) 'IDI I J', IDI,I,J
                    write (JOSTND,*) 'DIAM RTD RCEN NCENTS(IDI)',&
                         &DIAM,RTD,RCEN,NCENTS(IDI)
                 end if

                 call RDSPL2(RCEN,IDI,DIAM,RTD)

                 if (DEBUG) then
                    write (JOSTND,*) 'RDSPOR :  AFTER RRSPL2'
                    write (JOSTND,*) 'NCENTS(IDI)', NCENTS(IDI)
                    write (JOSTND,*) 'PCENTS 1,2,3 ICENSP'

                    do JJ = 1,NCENTS(IDI)
                       write(JOSTND,*) PCENTS(IDI,JJ,1),&
                            &PCENTS(IDI,JJ,2),&
                            &PCENTS(IDI,JJ,3),&
                            &ICENSP(IDI,JJ)
                       756 continue
                    end do
                 end if

                 !....                Add all stumps that are large enough to create
                 !....                centers to stump list.

                 if (DIAM .ge. SPDBH(IDI)) STNEW = RCEN
                 if (STNEW .lt.1) goto 770

                 TST = PROBDA(IDI,I,J,IST) + STNEW
                 DBHDA(IDI,I,J,IST) = (DBHDA(IDI,I,J,IST) *&
                      &PROBDA(IDI,I,J,IST) + DBHOUT(IDI,I,J,ISPINT) *&
                      &STNEW) / TST

                 !....                Assume 50% of root radius is infected so set
                 !....                infection level to that amount.

                 ROOTDA(IDI,I,J,IST) = (ROOTDA(IDI,I,J,IST) *&
                      &PROBDA(IDI,I,J,IST) + 0.5 *&
                      &RTOUT(IDI,I,J,ISPINT) *&
                      &STNEW) / TST
                 PROBDA(IDI,I,J,IST) = PROBDA(IDI,I,J,IST) + STNEW

                 770 continue

                 !....                Zero arrays

                 STOUT(IDI,I,J,ISPINT) = 0.0
                 DBHOUT(IDI,I,J,ISPINT) = 0.0
                 RTOUT(IDI,I,J,ISPINT) = 0.0
                 STUIN(IDI,I,J,ISPINT) = 0.0
                 DBHUIN(IDI,I,J,ISPINT) = 0.0
                 RTUIN(IDI,I,J,ISPINT) = 0.0

                 800 continue
              end do
              850 continue
           end do
           900 continue
        end do

        do IRRSP = 1,2
           if (NCENTS(IRRSP) .eq.0) goto 960

           SPCEN = 0.0
           TOTCEN = 0.0

           do I = 1,NCENTS(IRRSP)
              if (ICENSP(IRRSP,I) .ne.0) SPCEN = SPCEN +&
                   &PCENTS(IRRSP,I,3) ** 2
              TOTCEN = TOTCEN + PCENTS(IRRSP,I,3) ** 2
              970 continue
           end do

           SPPROP(IRRSP) = 0.0

           if (SPCEN .gt.1E-4.and. TOTCEN .gt.1E-4) then
              SPPROP(IRRSP) = SPCEN / TOTCEN
           end if

           960 continue

           if (DEBUG) write(JOSTND,*)&
                &'RDSPOR :  BEF PAREA', IRRSP, PAREA(IRRSP)

           call RDAREA

           if (DEBUG) write(JOSTND,*)&
                &'RDSPOR :  AFT PAREA', IRRSP, PAREA(IRRSP)

           !....          If new infection type in stand, and user did not specify
           !....          to model as one center then model stand as multiple centers.

           if (LONECT(IRRSP) .eq.0.and. PAREA(IRRSP) .gt.0)&
                &LONECT(IRRSP) = 2

           950 continue
        end do

     end if

     1000 continue
  end do

  do ISPINT = 2, 1, -1

     if (LSPFLG(ISPINT)) then

        !....       A stand entry occurred in a previous timestep (if the cycle
        !....       length was less than 5) and/or this time, so shift spore
        !....       infected stumps within their arrays.

        do IDI = 1,2
           do I = 1,2
              do J = 1,5
                 STOUT(IDI,I,J,ISPINT+1) = STOUT(IDI,I,J,ISPINT)
                 STOUT(IDI,I,J,ISPINT) = 0.0

                 DBHOUT(IDI,I,J,ISPINT+1) = DBHOUT(IDI,I,J,ISPINT)
                 DBHOUT(IDI,I,J,ISPINT) = 0.0

                 RTOUT(IDI,I,J,ISPINT+1) = RTOUT(IDI,I,J,ISPINT)
                 RTOUT(IDI,I,J,ISPINT) = 0.0

                 STUIN(IDI,I,J,ISPINT+1) = STUIN(IDI,I,J,ISPINT)
                 STUIN(IDI,I,J,ISPINT) = 0.0

                 DBHUIN(IDI,I,J,ISPINT+1) = DBHUIN(IDI,I,J,ISPINT)
                 DBHUIN(IDI,I,J,ISPINT) = 0.0

                 RTUIN(IDI,I,J,ISPINT+1) = RTUIN(IDI,I,J,ISPINT)
                 RTUIN(IDI,I,J,ISPINT) = 0.0

                 ISDATE(ISPINT+1) = ISDATE(ISPINT)
                 ISDATE(ISPINT) = 0

                 LSPFLG(ISPINT+1) = LSPFLG(ISPINT)
                 1100 continue
              end do
              1200 continue
           end do
           1300 continue
        end do

        LSPFLG(ISPINT) = .false.
     end if
     1400 continue
  end do

  return
end
