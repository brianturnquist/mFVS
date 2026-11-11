!=================!
!   rd : rdbout   !
!=================!
! RD $Id$
subroutine RDBOUT
  implicit none

  !  Purpose :
  !     Produces optional output (in a separate file) of mortality due to
  !     bark beetles.

  !  Called By :
  !     RDOAGM  [ROOT DISEASE]

  !  Calls :
  !     GETID   (SUBROUTINE)   [FVS]
  !     GETLUN  (SUBROUTINE)   [FVS]
  !     DBSRD3  (SUBROUTINE)   [FVS]

  !  Arguments :
  !     none

  !  Common Block Variables Used :
  !     ISTEP
  !     ICYC
  !     DBH
  !     ISP
  !     IY     - translates ICYC and ISTEP into calendar years
  !     BBKILL -  number of trees killed by bark beetles, for
  !              DSII/DSIU/DSO, for each record.
  !     RROBNK - total number of trees killed by bark beetles, for each
  !              tree species.

  !  Local Variables :
  !     CLKILL - REAL
  !              Total number of trees killed in each type, by 5 inch
  !              DBH size classes.
  !     CMC    - CHARACTER
  !              Name of output file.
  !     IBBOUT - INTEGER
  !              Unit number of output file.
  !     NOKILL - LOGICAL
  !              FLAG - .TRUE. if there were no beetles active this year.
  !     ROWSUM - REAL
  !              Total number of trees killed in each type (inside,
  !              I-uninf, outsied)
  !     SUMLIV - REAL
  !              Total number of trees alive in each type, before bark
  !              beetles attacked.
  !     TOLDYR - LOGICAL
  !              FLAG - .TRUE. if other species this year are already
  !              printed.
  !     TOTSPC - REAL
  !              The total number of trees killed for each species.
  !              This values is weighted by proportion of diseased (inside)
  !              and undiseased (outside) stand area.

  !  Revision History :
  !     10/07/97 - Matthew K. Thompson (FHTET)
  !        Fixed loop to calculate size class totals.
  !     18-JUN-2001 Lance R. David (FHTET)
  !        Added Stand ID and Management ID line to header.
  !        Moved return condition check to after writing of headers.
  !     16-AUG-2006 Lance R. David (FHTET)
  !        Change of metric conversion factors variable names to match
  !        variables in new \FVS\COMMON\METRIC.F77. rd\src\metric.f77
  !        will be retired. (mods courtesy of Don Robinson, ESSA)
  !   08/26/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   02/26/2015 Lance R. David (FMSC)
  !     TOTSPC value was not properly weight based on diseased and
  !     undiseased proportional area of stand.
  !     Implemented General Report Writer facility.
  !     Auxiliary flat file from BBOUT keyword no longer available. This
  !     report now available in output DB.

  !.... Parameter include files.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... Common include files.

  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDCOM.f90'
  include 'RDADD.f90'
  include 'RDARRY.f90'
  include 'ARRAYS.f90'
  include 'METRIC.f90'

  !.... Local variable declarations.

  ! Variables
  logical :: TOLDYR,NOKILL
  integer :: CLASS,I,ICL,IDI,J,JYR,K,SP
  real    :: ROWSUM(3,MAXSP),CLKILL(3,MAXSP,7),SUMLIV(3,MAXSP)
  real    :: TOTSPC(MAXSP)
  real    :: MRTIN(7),MRTUN(7),MRTOUT(7)

  !.... ISTEP=2 (set in RDCNTL?) is the first time it's called.  Write the
  !.... table headers then.
  !.... Since the report has been requested, need to get report ID and
  !.... logical unit number from general report facility and write headers
  !.... whether there are any trees in the simulation at this time or not.

  !     Get logical unit number and open genrpt file if it has been closed.

  call GETLUN(IBBOUT)

  if (ISTEP .eq.2) then

     !     get report ID and logical unit number.

     call GETID(IDRDOUT(3))

     write (IBBOUT,'(2(/1X,I5))') IDRDOUT(3),IDRDOUT(3)

     write (IBBOUT,1100) IDRDOUT(3)
     write (IBBOUT,1105) IDRDOUT(3)
     write (IBBOUT,1100) IDRDOUT(3)
     write (IBBOUT,1110) IDRDOUT(3), NPLT, MGMID

     if (.not. LMTRIC) write (IBBOUT,1115) IDRDOUT(3)
     if (LMTRIC) write (IBBOUT,1215) IDRDOUT(3)

     write (IBBOUT,1111) IDRDOUT(3)
     write (IBBOUT,1120) IDRDOUT(3)

     if (.not. LMTRIC) write (IBBOUT,1125) IDRDOUT(3)
     if (LMTRIC) write (IBBOUT,1225) IDRDOUT(3)

     write (IBBOUT,1130) IDRDOUT(3)

     if (.not. LMTRIC) write (IBBOUT,1135) IDRDOUT(3)
     if (LMTRIC) write (IBBOUT,1235) IDRDOUT(3)

     write (IBBOUT,1120) IDRDOUT(3)
  end if

  if (IROOT .eq.0) goto 1000

  JYR = IY(ICYC+1)
  NOKILL = .true.
  TOLDYR = .false.

  do I = 1,3
     do J = 1,MAXSP
        ROWSUM(I,J) = 0.0
        SUMLIV(I,J) = 0.0
        TOTSPC(J) = 0.0

        do K = 1,7
           CLKILL(I,J,K) = 0.0
           10 continue
        end do
        11 continue
     end do
     22 continue
  end do

  IDI = MAXRR

  do I = 1,ITRN
     SP = ISP(I)
     !         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(SP)) ! changed 11/18/2015

     !....    If both P and S type Annosus is being used, set disease area
     !....    to that of the disease type of the host tree species; otherwise,
     !....    use the disease area of the one disease being simulated even though
     !....    current tree may not be specified as host species.

     if (MAXRR .lt.3.and. MINRR .ne. MAXRR) then
        IDI = IDITYP(IRTSPC(SP))
     end if

     CLASS = INT(DBH(I)/5.0) + 1
     if (CLASS .gt.7) CLASS = 7

     do J = 1,3
        ROWSUM(J,SP) = ROWSUM(J,SP) + BBKILL(J,I)
        CLKILL(J,SP,CLASS) = CLKILL(J,SP,CLASS) + BBKILL(J,I)
        33 continue
     end do

     !      IF (SP .EQ. 10)                                       ! Debug
     !     & WRITE (*,*) 'IN RDBOUT: I, SP, BBKILL(x,I) ',I, SP,  ! Debug
     !     & BBKILL(1,I),BBKILL(2,I),BBKILL(3,I)                  ! Debug

     SUMLIV(2,SP) = SUMLIV(2,SP) + PROBIT(I) + BBKILL(2,I)
     SUMLIV(3,SP) = SUMLIV(3,SP) + PROBIU(I)
     SUMLIV(1,SP) = SUMLIV(1,SP) + FPROB(I) * (SAREA - PAREA(IDI))
     66 continue
  end do

  IDI = MAXRR

  do SP = 1,MAXSP
     if (RROBNK(SP) .le.0.0) goto 99
     NOKILL = .false.

     !....    First change everything into per acre.

     !         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(SP)) ! changed 11/18/2015

     !....    If both P and S type Annosus is being used, set disease area
     !....    to that of the disease type of the host tree species; otherwise,
     !....    use the disease area of the one disease being simulated even though
     !....    current tree may not be specified as host species.

     if (MAXRR .lt.3.and. MINRR .ne. MAXRR) then
        IDI = IDITYP(IRTSPC(SP))
     end if

     do J = 2,3
        SUMLIV(J,SP) = SUMLIV(J,SP) / PAREA(IDI)
        ROWSUM(J,SP) = ROWSUM(J,SP) / PAREA(IDI)

        !           Value added to total for species must be weighted by
        !           proportion of stand area that is diseased (inside).
        !            TOTSPC(SP) = TOTSPC(SP) + ROWSUM(J,SP)
        TOTSPC(SP) = TOTSPC(SP)&
             &+ (ROWSUM(J,SP) * (PAREA(IDI) / SAREA))
        77 continue
     end do

     !         WRITE (*,*) 'IN RDBOUT: SP, IDI, PAREA, SAREA',  ! Debug
     !     &                           SP,IDI,PAREA(IDI),SAREA  ! Debug

     if (PAREA(IDI) .lt. SAREA) then
        SUMLIV(1,SP) = SUMLIV(1,SP) / (SAREA-PAREA(IDI))
        ROWSUM(1,SP) = ROWSUM(1,SP) / (SAREA-PAREA(IDI))

        !           Value added to total for species must be weighted by
        !           proportion of stand area that is not diseased (outside).
        !            TOTSPC(SP) = TOTSPC(SP) + ROWSUM(1,SP)
        TOTSPC(SP) = TOTSPC(SP)&
             &+ (ROWSUM(1,SP) * ((SAREA - PAREA(IDI)) / SAREA))
     else
        SUMLIV(1,SP) = 0.0
        ROWSUM(1,SP) = 0.0
     end if

     do ICL = 1,7
        CLKILL(2,SP,ICL) = CLKILL(2,SP,ICL) / PAREA(IDI)
        CLKILL(3,SP,ICL) = CLKILL(3,SP,ICL) / PAREA(IDI)
        if (PAREA(IDI) .lt. SAREA)&
             &CLKILL(1,SP,ICL) = CLKILL(1,SP,ICL)/(SAREA-PAREA(IDI))
        88 continue
     end do

     if (LMTRIC) then
        if (TOLDYR) then
           write(IBBOUT,2011) IDRDOUT(3), JSP(SP),&
                &(CLKILL(2,SP,I)/ACRtoHA,I = 1,7),&
                &ROWSUM(2,SP)/ACRtoHA,&
                &SUMLIV(2,SP)/ACRtoHA
        else
           write(IBBOUT,2001) IDRDOUT(3), JYR, JSP(SP),&
                &(CLKILL(2,SP,I)/ACRtoHA,I = 1,7),&
                &ROWSUM(2,SP)/ACRtoHA,&
                &SUMLIV(2,SP)/ACRtoHA
           TOLDYR = .true.
        end if

        write(IBBOUT,2002) IDRDOUT(3),&
             &(CLKILL(3,SP,I)/ACRtoHA,I = 1,7),&
             &ROWSUM(3,SP)/ACRtoHA,&
             &SUMLIV(3,SP)/ACRtoHA
        write(IBBOUT,2004) IDRDOUT(3),&
             &(CLKILL(1,SP,I)/ACRtoHA,I = 1,7),&
             &ROWSUM(1,SP)/ACRtoHA,&
             &TOTSPC(SP)/ACRtoHA,&
             &SUMLIV(1,SP)/ACRtoHA
        write(IBBOUT,1111) IDRDOUT(3)
     else
        if (TOLDYR) then
           write(IBBOUT,2011) IDRDOUT(3),&
                &JSP(SP), (CLKILL(2,SP,I),I = 1,7),&
                &ROWSUM(2,SP), SUMLIV(2,SP)
        else
           write(IBBOUT,2001) IDRDOUT(3),&
                &JYR, JSP(SP), (CLKILL(2,SP,I),I = 1,7),&
                &ROWSUM(2,SP), SUMLIV(2,SP)
           TOLDYR = .true.
        end if

        write(IBBOUT,2002) IDRDOUT(3),&
             &(CLKILL(3,SP,I),I = 1,7), ROWSUM(3,SP),&
             &SUMLIV(3,SP)
        write(IBBOUT,2004) IDRDOUT(3),&
             &(CLKILL(1,SP,I),I = 1,7), ROWSUM(1,SP),&
             &TOTSPC(SP), SUMLIV(1,SP)
        write(IBBOUT,1111) IDRDOUT(3)

        !           Call DBS for RD Bark Beetle output to database
        !           Need to load 1-dimensional arrays first.

        do I = 1,7
           MRTIN(I) = CLKILL(2,SP,I)
           MRTUN(I) = CLKILL(3,SP,I)
           MRTOUT(I) = CLKILL(1,SP,I)
        end do

        call DBSRD3 (JYR,NPLT,JSP(SP),&
             &MRTIN,ROWSUM(2,SP), SUMLIV(2,SP),&
             &MRTUN,ROWSUM(3,SP), SUMLIV(3,SP),&
             &MRTOUT,ROWSUM(1,SP), SUMLIV(1,SP),&
             &TOTSPC(SP))

     end if
     99 continue
  end do

  if (NOKILL) write(IBBOUT,2000) IDRDOUT(3), JYR
  write(IBBOUT,1111) IDRDOUT(3)

  1000 return

  1100 format (1X,I5,1X,63('* '))
  1105 format (1X,I5,51X,'WESTERN ROOT DISEASE MODEL')
  1110 format (1X,I5,34X,'STAND ID= ',A26,5X,'MANAGEMENT ID= ',A4)
  1111 format (1X,I5)
  1115 format (1X,I5,36X,&
       &'TREES PER ACRE KILLED BY BARK BEETLES IN EACH TIME PERIOD')
  1120 format (1X,I5,1X,126('-'))
  1125 format (1X,I5,1X,'Period   Tree    General ',23X,&
       &'DBH Class (in inches)',27X,&
       &'     Row     Species   Total')
  1130 format (1X,I5,1X,'Start   Species  Status   ',69('-'),&
       &'     Total     Total    Before')
  1135 format (1X,I5,1X,'(year)',20X,'     0-<5     5-<10 ',&
       &'   10-<15    15-<20    20-<25    25-<30       30+',&
       &'                        Attack')

  2000 format (1X,I5,2X,I4,21X,'no beetle kill')
  2001 format (1X,I5,2X,I4,5X,A3,'    in-inf   ',7(F9.2,1X),1X,F9.2,10X,&
       &1X,F9.2)
  2011 format (1X,I5,11X,A3,'    in-inf   ',7(F9.2,1X),1X,F9.2,10X,1X,&
       &F9.2)
  2002 format (1X,I5,18X,'in-uninf ',7(F9.2,1X),1X,F9.2,10X,1X,F9.2)
  2004 format (1X,I5,18X,'outside  ',7(F9.2,1X),3(1X,F9.2))

  !.... Headers used when running the metric version.

  1215 format (1X,I5,37X,&
       &'TREES PER HA KILLED BY BARK BEETLES IN EACH TIME PERIOD')
  1225 format (1X,I5,1X,'Period   Tree    General ',25X,&
       &'DBH Class (in cm)  ',27X,&
       &'     Row     Species   Total')
  1235 format (1X,I5,1X,'(year)',20X,'    0-<13    13-<25 ',&
       &'   25-<38    38-<51    51-<64    64-<76       76+',&
       &'                        Attack')

end
