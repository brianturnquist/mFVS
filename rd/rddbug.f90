!=================!
!   rd : rddbug   !
!=================!
! RD $Id$
subroutine RDDBUG (ICODE)
  implicit none

  !  PRINT DEBUG INFORMATION BASED ON PARAMETER ICODE.

  !  CALLED BY :
  !     VARIOUS [ROOT DISEASE]

  !  CALLS :
  !     NONE

  !  PARAMETERS :
  !     ICODE  - (I ) which debug information to print; if 0, nothing
  !                   is printed.

  !  COMMON BLOCK VARIABLES :
  !     ICYC:   From CONTRL; cycle index number.
  !     IRUNIT: From ANCOM; unit number of annosus output.

  !  LOCAL VARIABLES:
  !     IDI:    Disease type (1, P type; 2, S type).
  !     IS:     Tree type (1, heartwood; 2, non-heartwood).
  !     ISL:    Stump class (1-5).
  !     IST:    Time step (1-40).
  !     PRSNT:  Array (IDI,IS): true if stumps of that type are
  !             present in this cycle; controls printing.
  !     HEADR:  Array (IDI,IS): headers for output tables.

  !  Revision History :
  !   03/18/93 - Last revision date.
  !   08/28/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'CONTRL.f90'
  include 'RDCOM.f90'

  !.... Local variables

  ! Variables
  integer :: ICODE,I,IDI,IS,ISL,IST,J
  logical :: PRSNT(2,2)
  character(len=20) :: HEADR(2,2)

  !.... DATA statements

  data ((HEADR(I,J),J = 1,2),I = 1,1)&
       &/'P-type Heartwood',&
       &'P-type Non-Heartwood'/
  data ((HEADR(I,J),J = 1,2),I = 2,2)&
       &/'S-type Heartwood',&
       &'S-type Non-Heartwood'/

  !.... Clear, then set PRSNT, based on current cycle PROBDA

  do IDI = 1,2
     do IS = 1,2
        PRSNT(IDI,IS) = .false.
        do ISL = 1,5
           if (PROBDA(IDI,IS,ISL,1).gt.0.0)&
                &PRSNT(IDI,IS) = .true.
        end do
     end do
  end do

  !.... Check ICODE

  if (ICODE.eq.0) goto 9000
  if (ICODE.eq.1) then
     write (IRUNIT,800)

     write (IRUNIT,810)
     do IDI = 1,2
        do IS = 1,2
           if (PRSNT(IDI,IS)) then
              write (IRUNIT,850) HEADR(IDI,IS)
              write (IRUNIT,860)(ISL,ISL = 1,5)
              do IST = 1,ICYC
                 write (IRUNIT,870) IST,&
                      &(PROBDA(IDI,IS,ISL,IST),ISL = 1,5)
              end do
           end if
        end do
     end do

     write (IRUNIT,820)
     do  IDI = 1,2
        do IS = 1,2
           if (PRSNT(IDI,IS)) then
              write (IRUNIT,850) HEADR(IDI,IS)
              write (IRUNIT,860)(ISL,ISL = 1,5)
              do IST = 1,ICYC
                 write (IRUNIT,870) IST,&
                      &(DBHDA(IDI,IS,ISL,IST),ISL = 1,5)
              end do
           end if
        end do
     end do

     write (IRUNIT,830)
     do IDI = 1,2
        do IS = 1,2
           if (PRSNT(IDI,IS)) then
              write (IRUNIT,850) HEADR(IDI,IS)
              write (IRUNIT,860)(ISL,ISL = 1,5)
              do IST = 1,ICYC
                 write (IRUNIT,870) IST,&
                      &(ROOTDA(IDI,IS,ISL,IST),ISL = 1,5)
              end do
           end if
        end do
     end do

     write (IRUNIT,840)
     do IDI = 1,2
        do IS = 1,2
           if (PRSNT(IDI,IS)) then
              write (IRUNIT,850) HEADR(IDI,IS)
              write (IRUNIT,860)(ISL,ISL = 1,5)
              do IST = 1,ICYC
                 write (IRUNIT,880) IST,&
                      &(JRAGED(IDI,IS,ISL,IST),ISL = 1,5)
              end do
           end if
        end do
     end do

     write (IRUNIT,890)

     800 format (' RDDBUG output:')
     810 format (/' PROBDA array:')
     820 format (/' DBHDA array:')
     830 format (/' ROOTDA array:')
     840 format (/' JRAGED array:')
     850 format (1X, A20)
     860 format (' Cycle/Stump ', 5I10)
     870 format (10X, I5, 5F10.4)
     880 format (10X, I5, 5I10)
     890 format (/' RDDBUG end.'/)

  end if
  goto 9000

  !.... Common exit

  9000 continue
  return
end
