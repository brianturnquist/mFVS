!================================!
!   Read activity group labels   !
!================================!
! BASE $Id$
subroutine LBAGLR (KEYWRD,JOSTND,IREAD,IRECNT,RECORD,KODE)
  implicit none

  !     READ AN ACTIVITY GROUP LABEL FOR THE CURRENT ACTIVITY GROUP.

  !     PART OF THE LABEL PROCESSING COMPONENT OF THE PROGNOSIS SYSTEM
  !     N.L. CROOKSTON -- INTERMOUNTAIN RESEARCH STATION -- JAN 1987

  !     KEYWRD= KEYWORD CHARACTER STRING.
  !     JOSTND= PRINT MESSAGE FILE.
  !     IREAD = READER FILE
  !     IRECNT= NUMBER OF RECORDS READ ON IRECNT.
  !     KODE  = 0: OK, 1: END-OF-DATA FOUND ON IREAD.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  character(len=8) :: KEYWRD
  character(len=*) :: RECORD
  integer :: KODE,IRECNT,IREAD,JOSTND,LWRK,I1,I2

  !     READ THE ACTIVITY GROUP LABEL, STORE IT IN A WORK STRING.

  call LBSTRD (IREAD,LWRK,WKSTR1,IRECNT,RECORD,KODE,WKSTR2,WKSTR3)

  !     IF THE RETURN CODE IS GT ONE, END-OF-DATA WAS FOUND

  if (KODE.gt.1) then
     KODE = 1
     goto 50
  end if

  !     WRITE THE ACTIVITY GROUP LABEL.

  write (JOSTND,10) KEYWRD
  10 format (/1X,A8,'   ACTIVITY GROUP LABEL SET: ')
  I1 = 1
  I2 = 100
  20 continue
  if (I2.gt.LWRK) I2 = LWRK
  write (JOSTND,'(T13,A)') WKSTR1(I1:I2)
  if (I2.lt.LWRK) then
     I1 = I2+1
     I2 = I2+100
     goto 20
  end if
  if (KODE.eq.1) then
     KODE = 0
     write (JOSTND,30)
     30 format (/' ********   WARNING: THIS LABEL SET IS SHORTER ', &
          & 'THAN THE ONE YOU SPECIFIED.')
     call RCDSET (1,.true.)
  end if

  !     IF LOPENV IS TRUE, THEN STORE THE LABEL SET IN THE CORRECT STRING
  !     AND SET LBSETS TRUE INDICATING THAT AT LEAST SOME ACTIVITY GROUPS
  !     HAVE LABELS.

  if (LOPEVN) then
     LBSETS = .true.
     AGLSET(IEVA) = WKSTR1
     LENAGL(IEVA) = LWRK
  else

     !        AN ACTIVITY GROUP LABEL CAN NOT BE ENTERED BECAUSE THE
     !        AGPLABEL KEYWORD MUST BE ENTERED IN THE CONTEXT OF AN
     !        ACTIVITY GROUP.

     write (JOSTND,40)
     40 format (/' ********   ERROR: THIS ACTIVITY GROUP LABEL IS NOT', &
          & ' PART OF AN ACTIVITY GROUP AND WILL BE IGNORED.')
     call RCDSET (1,.true.)
  end if
  50 continue
  return
end subroutine LBAGLR
