!===================!
!   wsbwe : bwep4   !
!===================!
! WSBWE $Id$
subroutine BWEP4(ICALL)
  implicit none

  !  PRINT PARAMETER SUMMARY AND SPECIAL EVENTS TABLE

  !  K.A. SHEEHAN  USDA-FS, R6-NATURAL RESOURCES, PORTLAND, OR

  !   CALLED FROM: BWELIT
  !                BWEIN

  !   SUBROUTINES AND FUNCTIONS CALLED: NONE

  !   PARAMETERS:

  !   EVENT  - STORES CODES THAT ARE REPORTED IN THIS TABLE
  !   IEVENT(250,4) - BW SPECIAL EVENTS SUMMARY ARRAY
  !   ITITLB - TITLE FOR BW MODEL
  !   IYRCUR - CURRENT YEAR
  !   MGMIDB - MANAGEMENT ID TAG FOR BW MODEL [BWEBOX]
  !   NEVENT - NUMBER OF BW SPECIAL EVENTS TO DATE

  ! Revision History:
  !   04-MAY-00 Lance David (FHTET)
  !     .Added FVS common files for keyword file name and stand id in
  !      summary heading.
  !     .Corrected write statements and formats to use weater and outbreak
  !      random number seed variables WSEEDR and OBSEER.
  !   16-MAY-00 Lance David (FHTET)
  !     .Replaced temp variable TEMPS1(1) with variable ILOBYR.
  !   06-DEC-00 Lance David (FHTET)
  !     .Condition .OR.ISPRAY.EQ.3 removed from ISPRAY.EQ.1 and ISPRAY.EQ.2
  !      statements because there is no third spray option.
  !   28-DEC-2005 Lance R. David (FHTET)
  !     .Increased length of weather file name.
  !   10-AUG-2006 Lance R. David
  !     .Removed variables MOPT and OPTION from printing of summary output
  !      because there is now only one source of options and that is the
  !      FVS keyword file.
  !   30-AUG-2006 Lance R. David (FHTET)
  !      Changed array orientation of IEVENT from (4,250) to (250,4).
  !   02-JUN-2009 Lance R. David (FMSC)
  !      Added Stand ID and comma delimiter to output tables, some header
  !      and column labels modified.
  !   14-JUL-2010 Lance R. David (FMSC)
  !      Added IMPLICIT NONE and declared variables as needed.
  !   28-AUG-2013 Lance R. David (FMSC)
  !      Added weather year to special events table.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'BWECM2.f90'
  include 'BWEBOX.f90'
  ! Variables
  character(len=15) :: VARY
  character(len=6) :: STAGE(3)
  character(len=5) :: TRESIZ(4)
  character(len=3) :: TRESP(7)
  character(len=3) :: CROWN(4)
  integer :: I,ICALL,ICHECK,ICROWN,IS,ISIZE,J,KSP,N

  data TRESP/' WF',' DF',' GF','SAF',' ES',' WL','all'/,&
       &TRESIZ/'small','med. ','large',' all '/,&
       &CROWN/'top','mid','bot','all'/,&
       &STAGE/'fourth','fifth','sixth'/

  ! IF THIS IS THE FIRST CALL, PRINT THE SUMMARY INFORMATION

  if (ICALL.eq.1) then
     write (JOBWP4,15) MGMIDB,ITITLB
     15 format (A4,' -- ',A72/)
     write (JOBWP4,30) KWDFIL(1:40),NPLT
     30 format('FVS keyword file: ',A40,/,' Stand ID: ',A26,/)
     IS = ITEMP(4)
     ISTN = ITEMP(5)
     VARY = 'variable'
     if (IWOPT .eq.2) VARY = 'constant'
     if (IWSRC.eq.1) then
        write (JOBWP4,50) STATES(IS,1),WSLOOK(ISTN,IS),&
             &VARY, WSEEDR
        50 format ('Budworm Defoliation Model Weather Options: ',&
             &' state= ',A10,';  station= ',A16,';',/,&
             &18X,'between years, parameters are: ',A15,/,&
             &18X,'random number seed for weather: ',F10.0,/)
     else if (IWSRC.eq.2) then
        write (JOBWP4,55) WFNAME,VARY,WSEEDR
        55 format ('Budworm Defoliation Model Weather Options: ',/,&
             &' summary file = ',A,/,&
             &18X,'between years, parameters are: ',A15,/,&
             &18X,'random number seed for weather: ',F10.0,/)
     else
        write (JOBWP4,60)&
             &WFNAME, IYRCNT, INT(BWPRMS(11,1)), BWPRMS(11,IYRCNT)
        60 format ('Budworm Defoliation Model Weather Options:',/,&
             &'    RAWS daily data processed from file = ',A,/,&
             &'    with ',I2,' complete years ',I4,' through ',F5.0,/,&
             &'    No variation applied.',/)
     end if

     VARY = 'was not'
     if (LTEMP1(1)) VARY = '* was *'
     IS = ITEMP(2)
     write (JOBWP4,70) ILOBYR,VARY,TEMPS2(IS),OBSEER
     70 format ('Outbreak Status: year that last outbreak began= ',&
          &I4,/,18X,'outbreak ',A7,' active at the',&
          &' start of the simulation',&
          &/,18X,'freq. / duration source: ',A20,/,&
          &18X, 'random number seed for outbreaks:',F10.0,/)
     if (IOBOPT.eq.3) then
        write (JOBWP4,73)
        73 format ('Outbreak Scheduling Option:  user has selected',&
             &' starting and ending years')
        do N = 1,NOBSCH
           write (JOBWP4,75) N,IOBSCH(N,1),IOBSCH(N,2)
           75 format ('   outbreak no. ',I1,':  start= ',I4,&
                &', end= ',I4)
           78 continue
        end do
     end if
     if (IOBOPT.eq.2) write (JOBWP4,72)
     72 format ('Outbreak Scheduling Option:  both starting and ',&
          &'ending dates generated',/,10x,'from frequency and durat',&
          &'ion statistics')
     if (IOBOPT.eq.1) write (JOBWP4,71)
     71 format ('Outbreak Scheduling Option:  starting dates gen',&
          &'erated from frequency',/,10x,'statistics; outbreaks ',&
          &'end when % new defoliation is < 10% for',/,10x,&
          &'3 consecutive years (minimum duration = 5 years)')
     VARY = ' increase  '
     if (.not.LTEMP1(2)) VARY = 'are constant'
     ICHECK = 0
     do I = 1,4
        do J = 1,3
           if (NEMULT(I,J).ne.1.0) ICHECK = 1
           80 continue
        end do
     end do
     if (ICHECK.eq.0) then
        write (JOBWP4,90) VARY
        90 format (/'Natural Enemy Options: parasitism rates ',A12,&
             &' during the outbreak'/24X,'all multipliers set to 1.0'/)
     else
        write (JOBWP4,95) VARY,((NEMULT(I,J),J = 1,3),I = 1,4)
        95 format ('Natural Enemy Options: parasitism rates ',A12,&
             &' during the outbreak'/8X,'multipliers: birds= ',3F6.3,&
             &';   ants= ',3F6.3/21X,'paras= ',3F6.3,';  other= ',&
             &3F6.3/)
     end if
     VARY = ' increase  '
     if (.not.LTEMP1(3)) VARY = 'are constant'
     write (JOBWP4,110) VARY
     110 format ('Foliage Quality Effects: larval development times ',&
          &A12,' during the outbreak')
     VARY = ' decrease  '
     if (.not.LTEMP1(4)) VARY = 'are constant'
     write (JOBWP4,120) VARY
     120 format (26X,'pupal weights ',A12,' during the outbreak'/)

     if (ISPRAY.eq.1) then
        do KSP = 1,NSPRAY
           write (JOBWP4,130) ISPYR(KSP),STAGE(INSTSP),SPEFF
           130 format('Insecticide applied in year ',I4,/&
                &'   at peak ',A6,&
                &' instar, resulting in ',F5.1,' % mortality.'/)
           135 continue
        end do
     else if (ISPRAY.eq.2) then
        write (JOBWP4,140) ISPVAR,STAGE(INSTSP),SPEFF
        140 format('When last year''s % defoliation is >',I4,'%, an ',&
             &'insecticide is applied',/,'   at peak ',A6,&
             &' instar, resulting in ',F5.1,' % mortality.')
        if (LIMITS.eq.1) then
           write (JOBWP4,145)
           145 format ('  Only one application is allowed per',&
                &' outbreak.')
        else
           write (JOBWP4,147)
           147 format ('  No limit on the number of applications ',&
                &'per outbreak.')
        end if
     end if

     write (JOBWP4,150)
     150 format (//,&
          &10X,'**********',5X,'Special Events Table',5X,'**********',&
          &//8X,    ',',18X,' Sim , Wthr, Tree,  Tree, Crwn,       ,',/&
          &'Stand ID,',18X,' Year, Year, Spec,  Size, Third, Event,',/&
          &26('-'),',',' ----, ----, ----, -----, -----, ',50('-'),',')

     !  OTHERWISE, PRINT THE SPECIAL EVENTS TABLE

  else
     if (NEVENT.le.0) return
     do N = 1,NEVENT

        ! FIGURE OUT THE TREE SIZE CLASS AND CROWN THIRD

        if (IEVENT(N,3).eq.0) then
           ISIZE = 4
           ICROWN = 4
        else
           if (IEVENT(N,3).le.3) then
              ISIZE = 1
           else if (IEVENT(N,3).le.6) then
              ISIZE = 2
           else
              ISIZE = 3
           end if
           ICROWN = MOD(IEVENT(N,3),3)
           if (ICROWN.eq.0) ICROWN = 3
        end if

        write (JOBWP4,250)&
             &NPLT,IEVENT(N,1),IEVENT(N,5),TRESP(IEVENT(N,2)),&
             &TRESIZ(ISIZE),CROWN(ICROWN),EVENT(IEVENT(N,4))
        250 format&
             &(A26,', ',I4,', ',I4,', ',A3,',  ',A5,',  ',A3,',  ',A50,',')

        300 continue
     end do

  end if

  return
end
