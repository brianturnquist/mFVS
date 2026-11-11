!=================!
!   rd : rdtout   !
!=================!
! RD $Id$
subroutine RDTOUT
  implicit none

  !  PRINTS A SUMMARY OF THE DISEASE INFORMATION WHICH WAS READ IN
  !  FROM THE TREELIST.

  !  CALLED BY :
  !     INITRE  [PROGNOSIS] -NOT ANY MORE
  !     RDMN1   [ROOT DISEASE]

  !  CALLS     :
  !     NONE

  !  PARAMETERS :
  !     NONE

  !  COMMON BLOCK VARIABLES :
  !     STCUT:   From ANCOM;

  !  LOCAL VARIABLES :

  !  Revision History:
  !   06/12/13 Lance R. David (FMSC)
  !     Modified for removal of "print control" column 1.
  !   09/04/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'RDCOM.f90'
  include 'RDARRY.f90'
  include 'RDADD.f90'
  include 'METRIC.f90'

  ! Variables
  integer :: IDI,J
  character(len=1) :: DITYPE(4)

  !.... DATA statements

  data DITYPE/'P','S','A','W'/

  !     WRITE HEADING FOR OUTPUT

  write (JOSTND,1000)
  1000 format (/,130('-'))
  write (JOSTND,1010)
  1010 format (/,42X,'SUMMARY OF DISEASE INFORMATION READ IN ON',&
       &' INPUT')
  write (JOSTND,1000)

  do IDI = MINRR,MAXRR
     write (JOSTND,1020) DITYPE(IDI)
     1020 format (/,11X,'DISEASE TYPE: ',A1)

     if (RRTINV) then
        write (JOSTND,1030) RINNF(IDI), RINUF(IDI), RISTU(IDI)
        1030 format (11X, 'NUMBER OF INFECTED TREE RECORDS IN ',&
             &'DISEASE PATCHES=  ', F6.0/&
             &11X, 'NUMBER OF UNINFECTED TREE RECORDS IN ',&
             &'DISEASE PATCHES=', F6.0/&
             &11X, 'NUMBER OF INFECTED STUMP RECORDS IN ',&
             &'DISEASE PATCHES= ', F6.0)
     end if
     write (JOSTND,1040)
     write (JOSTND,1050)
     write (JOSTND,1060)

     1040 format (/,11X, '        NUMBER OF INFECTED STUMPS         ')
     1050 format (/,11X, 'SIZE CLASS      RESINOUS      NON-RESINOUS')
     1060 format (11X,   '------------------------------------------')

     do J = 1,5
        if (LMTRIC) then
           if (J.lt.5) then
              write (JOSTND,1071)&
                   &NINT(STCUT(J)*INTOCM), NINT(STCUT(J+1)*INTOCM),&
                   &PROBDA(IDI,1,J,1), PROBDA(IDI,2,J,1)
           else
              write (JOSTND,1081) NINT(STCUT(J)*INTOCM),&
                   &PROBDA(IDI,1,J,1), PROBDA(IDI,2,J,1)
           end if
        else
           if (J.lt.5) then
              write (JOSTND,1070)&
                   &NINT(STCUT(J)), NINT(STCUT(J+1)),&
                   &PROBDA(IDI,1,J,1), PROBDA(IDI,2,J,1)
           else
              write (JOSTND,1080) NINT(STCUT(J)),&
                   &PROBDA(IDI,1,J,1), PROBDA(IDI,2,J,1)
           end if
        end if
        1070 format (12X,I2,'-',I3,'"',6X,F5.0,10X,F5.0)
        1080 format (12X,'  >',I3,'"',6X,F5.0,10X,F5.0)
        1071 format (12X,I3,'-',I3,' CM',5X,F5.0,10X,F5.0)
        1081 format (12X,'   >',I3,' CM',5X,F5.0,10X,F5.0)
        50 continue
     end do

     90 continue
  end do

  !     PRINT DELIMITER FOR END OF OUTPUT

  write (JOSTND,1000)

  return
end
