!=====================!
!   pg : putgetsubs   !
!=====================!
! PG $Id$
subroutine BFREAD (BUFFER,IPNT,ILIMIT,VARBLE,ILEN,IBEGIN)
  implicit none

  !     READ A VARIABLE FROM A BUFFER.

  !     PART OF THE PARALLEL PROCESSING EXTENSION OF PROGNOSIS SYSTEM.

  !        BUFFER - A SINGLE DIMENSIONAL WORK ARRAY USED AS A PRINT
  !                 BUFFER.
  !        IPNT   - POINTER TO THE CURRENT WORD IN THE BUFFER.
  !                 WHEN READING THE FIRST RECORD IN A SERIES,
  !                 IPNT IS SET TO THE END OF THE BUFFER IN ORDER
  !                 BE RESET TO ZERO ON THE FIRST READ.
  !        ILIMIT - END OF BUFFER.  MAX NUMBER OF WORDS WHICH CAN
  !                 BE STORED IN BUFFER.
  !        VARBLE - THE ARRAY WHICH IS READ FROM BUFFER.
  !        ILEN   - THE NUMBER OF WORDS IN VARBLE.
  !        IBEGIN - SIGNAL DESCRIBING WHETHER WE ARE BEGINNING A
  !                 SERIES OF CALLS TO BFREAD, IN THE MIDDLE OF A
  !                 SERIES OF CALLS, OR AT THE END OF A SERIES OF CALLS.
  !                 1 - THIS IS THE FIRST CALL IN A GROUP TO BFREAD.
  !                     THE BUFFER COUNTER IS SET FULL, SO WE WILL
  !                     BEGIN READING FROM A NEW RECORD.
  !                 2 - THIS IS A MIDDLE CALL TO BFREAD.
  !                 3 - THIS IS THE LAST CALL TO BFREAD.

  ! Variables
  real    :: BUFFER (*),VARBLE (*)
  integer :: IBEGIN,ILEN,ILIMIT,IPNT,IWORD,I
  if (IBEGIN.eq.1) IPNT = ILIMIT
  if (ILEN.lt.1) goto 1000
  do IWORD = 1,ILEN
     if (IPNT.lt.ILIMIT) goto 500
     call DSTASH (BUFFER,IPNT)
     call fvsGetRtnCode(I)
     if (I.ne.0) return
     IPNT = 0
     500 continue
     IPNT = IPNT+1
     VARBLE (IWORD) = BUFFER (IPNT)
     900 continue
  end do
  1000 continue
  return
end

subroutine BFWRIT (BUFFER,IPNT,ILIMIT,VARBLE,ILEN,IBEGIN)
  implicit none

  !  **BFWRIT--PPBASE    DATE OF LAST REVISION:  07/31/08

  !     STORE A VARIABLE IN A BUFFER.

  !     CALLED FROM:

  !       PUTSTD - STORE A STAND ON DIRECT ACCESS FILE IDAIO.
  !       MSTRPT - STORE THE MASTER SCHEDULED OPTION LIST.
  !       PPAUSE - PAUSE THE PARALLEL PROCESSINT EXTENSION BY
  !                WRITING ITS DATA TO DIRECT ACCESS FILE IDAIO.

  !     SUBROUTINES CALLED:

  !       STASH  - STORE A RECORD IN THE DIRECT ACCESS FILE IDAIO.

  !     PARAMETERS:

  !        BUFFER - A SINGLE DIMENSIONAL WORK ARRAY USED AS A PRINT
  !                 BUFFER.
  !        IPNT   - POINTER TO THE CURRENT WORD IN THE BUFFER.
  !                 WHEN READING THE FIRST RECORD IN A SERIES,
  !                 IPNT IS SET TO THE END OF THE BUFFER IN ORDER
  !                 BE RESET TO ZERO ON THE FIRST READ.
  !        ILIMIT - END OF BUFFER.  MAX NUMBER OF WORDS WHICH CAN
  !                 BE STORED IN BUFFER.
  !        VARBLE - THE ARRAY WHICH IS WRITTEN TO THE BUFFER.
  !        ILEN   - THE NUMBER OF WORDS IN VARBLE.
  !        IBEGIN - SIGNAL DESCRIBING WHETHER WE ARE BEGINNING A
  !                 SERIES OF CALLS TO BFWRIT, IN THE MIDDLE OF A
  !                 SERIES OF CALLS, OR AT THE END OF A SERIES OF CALLS.

  !                 1 - THIS IS THE FIRST CALL IN A GROUP TO BFWRIT.
  !                     THE BUFFER IS SET EMPTY, SO WE WILL
  !                     BEGIN WRITING TO A NEW RECORD.
  !                 2 - THIS IS A MIDDLE CALL TO BFWRIT.
  !                 3 - THIS IS THE LAST CALL TO BFWRIT, WHICH
  !                     DUMPS THE CONTENTS OF THE BUFFER, AND
  !                     FINALIZES THE WRITE.

  ! Variables
  real    :: BUFFER (*),VARBLE (*)
  integer :: IBEGIN,ILEN,ILIMIT,IPNT,IWORD
  if (IBEGIN.eq.1) IPNT = 0
  if (ILEN.lt.1) goto 1000
  do IWORD = 1,ILEN
     if (IPNT.lt.ILIMIT) goto 500
     call STASH (BUFFER,ILIMIT)
     IPNT = 0
     500 continue
     IPNT = IPNT+1
     BUFFER (IPNT) = VARBLE (IWORD)
     900 continue
  end do
  1000 continue
  if (IBEGIN.eq.3) call STASH (BUFFER,ILIMIT)
  return
end

subroutine CHREAD (CBUFF,IPNT,LNCBUF,CVARBL,IBEGIN)
  implicit none

  !  **CHREAD  DATE OF LAST REVISION:  07/31/08

  ! Variables
  integer :: IBEGIN,IPNT,LNCBUF,I
  character(len=1) :: CBUFF(LNCBUF)
  character(len=1) :: CVARBL
  if (IBEGIN.eq.1) IPNT = LNCBUF
  if (IPNT.lt.LNCBUF) goto 500
  call CHDSTH (CBUFF,IPNT)
  call fvsGetRtnCode(I)
  if (I.ne.0) return
  IPNT = 0
  500 continue
  IPNT = IPNT+1
  CVARBL = CBUFF(IPNT)
  return
end

subroutine CHWRIT (CBUFF,IPNT,LNCBUF,CVARBL,IBEGIN)
  implicit none

  !  **CHWRIT  DATE OF LAST REVISION:  07/31/08

  ! Variables
  integer :: IBEGIN,IPNT,LNCBUF,I
  character(len=1) :: CBUFF(LNCBUF)
  character(len=1) :: CVARBL
  if (IBEGIN.eq.1) IPNT = 0
  if (IPNT.ge.LNCBUF) then
     call CHSTSH (CBUFF,LNCBUF)
     IPNT = 0
  end if
  IPNT = IPNT+1
  CBUFF(IPNT) = CVARBL
  if (IBEGIN.eq.3) then
     if (IPNT.lt.LNCBUF) then
        do I = IPNT+1,LNCBUF
           CBUFF(I) = ' '
        end do
     end if
     call CHSTSH (CBUFF,LNCBUF)
  end if
  return
end

subroutine LFREAD(BUFFER,IPNT,ILIMIT,LVAR,ILEN,IBEGIN)
  implicit none

  !  **LFREAD--PPBASE    DATE OF LAST REVISION:  07/31/08

  ! Variables
  logical :: LVAR(*),LSTOR
  real    :: BUFFER(*),RSTOR
  integer :: IBEGIN,ILEN,ILIMIT,IPNT,IWORD,I
  equivalence (LSTOR,RSTOR)
  if (IBEGIN.eq.1) IPNT = ILIMIT
  if (ILEN.lt.1) goto 1000
  do IWORD = 1,ILEN
     if (IPNT.lt.ILIMIT) goto 500
     call DSTASH (BUFFER,IPNT)
     call fvsGetRtnCode(I)
     if (I.ne.0) return
     IPNT = 0
     500 continue
     IPNT = IPNT+1
     RSTOR = BUFFER(IPNT)
     LVAR(IWORD) = LSTOR
     900 continue
  end do
  1000 continue
  return
end

subroutine LFWRIT (BUFFER,IPNT,ILIMIT,LVAR,ILEN,IBEGIN)
  implicit none

  !  **LFWRIT--PPBASE    DATE OF LAST REVISION:  07/31/08

  ! Variables
  logical :: LVAR(*),LSTOR
  real    :: BUFFER(*),RSTOR
  integer :: IBEGIN,ILEN,ILIMIT,IPNT,IWORD
  equivalence (RSTOR,LSTOR)
  if (IBEGIN.eq.1) IPNT = 0
  if (ILEN.lt.1) goto 1000
  do IWORD = 1,ILEN
     if (IPNT.lt.ILIMIT) goto 500
     call STASH (BUFFER,ILIMIT)
     IPNT = 0
     500 continue
     LSTOR = LVAR(IWORD)
     IPNT = IPNT+1
     BUFFER(IPNT) = RSTOR
     900 continue
  end do
  1000 continue
  if (IBEGIN.eq.3) call STASH (BUFFER,ILIMIT)
  return
end

subroutine IFREAD(BUFFER,IPNT,ILIMIT,IVAR,ILEN,IBEGIN)
  implicit none

  !  **IFREAD--PPBASE   DATE OF LAST REVISION:  07/31/08

  ! Variables
  real    :: BUFFER(*)
  integer :: IVAR(*)
  integer :: IBEGIN,ILEN,ILIMIT,IPNT,IWORD,I
  real    :: RSTOR
  integer*4 :: ISTOR
  equivalence (RSTOR,ISTOR)
  if (IBEGIN.eq.1) IPNT = ILIMIT
  if (ILEN.lt.1) goto 1000
  do IWORD = 1,ILEN
     if (IPNT.lt.ILIMIT) goto 500
     call DSTASH (BUFFER,IPNT)
     call fvsGetRtnCode(I)
     if (I.ne.0) return
     IPNT = 0
     500 continue
     IPNT = IPNT+1
     RSTOR = BUFFER(IPNT)
     IVAR(IWORD) = ISTOR
     900 continue
  end do
  1000 continue
  return
end

subroutine IFWRIT (BUFFER,IPNT,ILIMIT,IVAR,ILEN,IBEGIN)
  implicit none

  !  **IFWRIT--PPBASE    DATE OF LAST REVISION:  07/31/08

  ! Variables
  real    :: BUFFER(*),RSTOR
  integer :: IVAR(*),IBEGIN,ILEN,ILIMIT,IPNT,IWORD
  integer*4 :: ISTOR
  equivalence (RSTOR,ISTOR)
  if (IBEGIN.eq.1) IPNT = 0
  if (ILEN.lt.1) goto 1000
  do IWORD = 1,ILEN
     if (IPNT.lt.ILIMIT) goto 500
     call STASH (BUFFER,ILIMIT)
     IPNT = 0
     500 continue
     ISTOR = IVAR(IWORD)
     IPNT = IPNT+1
     BUFFER(IPNT) = RSTOR
     900 continue
  end do
  1000 continue
  if (IBEGIN.eq.3) call STASH (BUFFER,ILIMIT)
  return
end
