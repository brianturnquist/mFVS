!==============================!
!   Read stand policy labels   !
!==============================!
! BASE $Id$
subroutine LBSPLR (KEYWRD,JOSTND,IREAD,IRECNT,RECORD,KODE,LKECHO)
  implicit none

  !     READ A STAND POLICY LABEL FROM IREAD.

  !     PART OF THE LABEL PROCESSING COMPONENT OF THE PROGNOSIS SYSTEM
  !     N.L. CROOKSTON -- INTERMOUNTAIN RESEARCH STATION -- JAN 1987

  !     KEYWRD= KEYWORD STRING.
  !     JOSTND= PRINT MESSAGE FILE.
  !     IREAD = READER FILE
  !     IRECNT= NUMBER OF RECORDS READ ON IRECNT.
  !     RECORD= INPUT RECORD
  !     KODE  = 0: OK, 1: END-OF-DATA FOUND ON IREAD.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'OPCOM.F77'

  ! Variables
  character(len=8) :: KEYWRD
  character(len=*) :: RECORD
  logical :: LKECHO
  integer :: KODE,IRECNT,IREAD,JOSTND,I1,I2

  !     READ THE STAND LABEL SET

  call LBSTRD (IREAD,LENSLS,SLSET,IRECNT,RECORD,KODE,WKSTR1,WKSTR2)
  if (KODE.gt.1) then
     KODE = 1
     goto 40
  end if

  !     SIGNAL THAT LABEL PROCESSING HAS BEEN ACTIVATED.

  LBSETS = .true.

  !     ECHO THE KEYWORD AND THE SLS.

  if (LKECHO)write(JOSTND,10) KEYWRD
  10 format (/A8,'   STAND POLICY LABEL SET:')
  I1 = 1
  I2 = 100
  20 continue
  if (I2.gt.LENSLS) I2 = LENSLS
  if (LKECHO)write(JOSTND,'(T12,A)') SLSET(I1:I2)
  if (I2.lt.LENSLS) then
     I1 = I2+1
     I2 = I2+100
     goto 20
  end if
  if (KODE.eq.1) then
     KODE = 0
     write (JOSTND,30)
     30 format (/'********   WARNING: THIS LABEL SET IS SHORTER ', &
          & 'THAN THE ONE YOU SPECIFIED.')
     call RCDSET (1,.true.)
  end if
  40 continue
  return
end subroutine LBSPLR
