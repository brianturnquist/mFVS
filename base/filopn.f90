!=======================================!
!   Open model input and output files   !
!=======================================!
! BASE $Id$
subroutine FILOPN
  implicit none

  ! BASE

  !  THIS ROUTINE OPENS THE FILES FOR THE PROGNOSIS MODEL.
  !  TO PROMPT FOR FILE NAMES, SET LPT TRUE,
  !  IF PROMPTS ARE NOT WANTED, SET LPT FALSE.

  ! Common blocks
  include 'PRGPRM.F77'
  include 'CONTRL.F77'
  include 'FMPARM.F77'
  include 'FMFCOM.F77'
  include 'ECON.F77'
  include 'SVDATA.F77'
  include 'FVSSTDCM.F77'

  ! Variables
  integer :: LENKEY,KODE,I,LENNAM,ISTLNB,IRTNCD
  character(len=250) :: KEYFIL
  character(len=250) :: CNAME
  character(len=10) :: REV
  logical :: LOPEN
  logical :: LPT
  data LPT/.true./

  !  KEYWORD and OUTPUT FILES.

  KWDFIL = ' '
  call fvsGetKeywordFileName(KWDFIL,len(KWDFIL),I)
  KWDFIL = ADJUSTL(TRIM(KWDFIL))
  if (KWDFIL.ne.' ') then
     inquire(unit = iread,opened = LOPEN)
     if (LOPEN) close(unit = iread)

     call fvsGetRestartCode (i)
     if (i.eq.0) open(unit = IREAD,file = KWDFIL,status = "old",err = 101)
     lenkey = index(KWDFIL,".k")
     if (lenkey == 0) lenkey = index(KWDFIL,".K")
     if (lenkey > 0) KWDFIL = KWDFIL(:lenkey-1)
     lenkey = len_trim(KWDFIL)
     cname = KWDFIL(:lenkey)//".out"

     inquire(unit = JOSTND,opened = LOPEN)
     if (LOPEN) close(unit = JOSTND)

     if (i.eq.0) then
        open(unit = JOSTND,file = trim(cname),status = "replace", &
             & err = 102)
     else
        open(unit = JOSTND,file = trim(cname),status = "unknown", &
             & position = "append",err = 102)
     end if

      ! Clear pre-existing TRL/FST files if NOT a restart, These
      ! files will be opened as required in PRTRLS/FVSSTD/FMSOUT
      ! via OpenIfClosed().TRL uses default JOLIST; FST uses default KOLIST;
      ! SNG uses JOLIST (but only for a moment).
      ! These outputs will NOT be identical to output files created
      ! with nonstop runs: they are blocked into multi-stand sets
      ! based on the stop point(s).

     if (i.eq.0) then
        cname = KWDFIL(:lenkey)//".trl"
        inquire(unit = JOLIST,opened = LOPEN)
        if (.not.LOPEN) then
           open(unit = JOLIST,file = trim(cname),err = 102)
           close(unit = JOLIST, STATUS = 'delete')
        end if
        cname = KWDFIL(:lenkey)//".fst"
        inquire(unit = KOLIST,opened = LOPEN)
        if (.not.LOPEN) then
           open(unit = KOLIST,file = trim(cname),err = 102)
           close(unit = KOLIST, STATUS = 'delete')
        end if
        cname = KWDFIL(:lenkey)//".sng"
        inquire(unit = JOLIST,opened = LOPEN)
        if (.not.LOPEN) then
           open(unit = JOLIST,file = trim(cname),err = 102)
           close(unit = JOLIST, STATUS = 'delete')
        end if
     end if

     call KEYFN(KWDFIL)
     call DBSVKFN(KWDFIL)

     !       open the scratch file (should be removed sometime)
     open(unit = JOTREE,status = "scratch",form = "unformatted")

     return
     101 continue
     print *,"File open error on: ",trim(KWDFIL)
     call fvsSetRtnCode(1)
     return
     102 continue
     print *,"File open error on: ",trim(cname)
     call fvsSetRtnCode(1)
     return
  end if

  if (LPT) then

     !  GET VARIANT NAME AND REVISION DATE.
     !  NOTE: CR VARIANT MODEL TYPE WILL ALWAYS BE SM (SOUTHWEST MIXED CONIFERS
     !  (DEFAULT)) AT THIS POINT BECAUSE KEYWORDS HAVE NOT BEEN READ.

     call REVISE (VARACD,REV)
     write(*,2) VARACD,REV
     2 format(/T20,A2,' FVS VARIANT -- RV:',A10/)

     WRITE (*,'('' ENTER KEYWORD FILE NAME ('',I2.2,                   &
          &        ''): '')') IREAD

  end if

  read (*,'(A)',end = 100) KWDFIL
  call UNBLNK(KWDFIL,LENKEY)
  if (LENKEY.le.0) then
     write (*,'('' A KEYWORD FILE NAME IS REQUIRED'')')
     call RCDSET (3,.false.)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     return
  end if
  call MYOPEN (IREAD,KWDFIL,3,150,0,1,1,0,KODE)
  if (KODE.gt.0) then
     write (*,'('' OPEN FAILED FOR '',A)') &
          & KWDFIL(1:LENKEY)
     write (*,'('' A KEYWORD FILE IS REQUIRED'')')
     call RCDSET (3,.false.)
     call fvsGetRtnCode(IRTNCD)
     if (IRTNCD.ne.0) return
     return
  end if

  !     DBS EXTENSION NEEDS THIS FILENAME WITH EXTENSION FOR CASES TABLE

  call DBSVKFN(KWDFIL)

  !  MAIN OUTPUT FILE NEEDS KEYFILE NAME WITH EXTENSION. KEYFN ENTRY
  !  IS IN KEYRDR ROUTINE

  call KEYFN(KWDFIL)

  !     STORE THE KEYWORD FILENAME WITH EXTENSION IN GLBCNTL COMMON. MAY
  !     BE USED LATER TO CREATE FILES FOR TREELIST AND SNAG REPORT

  call fvsGetKeywordFileName(KWDFIL,250,251)

  !  FIND THE LAST PERIOD IN THE FILENAME AND SET THE REST OF THE
  !  KEYWORD FILE NAME TO BLANKS

  do I = LENKEY, 1, -1
     if (KWDFIL(I:I) .eq. '.') then
        KEYFIL = KWDFIL
        KWDFIL(I:) = ' '
        go to 10
     end if
  end do
  10 continue

  !  TREE DATA FILE.

  if (LPT) then
     WRITE (*,'('' ENTER TREE DATA FILE NAME ('',I2.2,              &
          &                 ''): '')') ISTDAT
  end if
  read (*,'(A)',end = 100) CNAME
  call UNBLNK(CNAME,LENNAM)
  if (LENNAM.gt.0) then
     call MYOPEN (ISTDAT,CNAME,1,150,0,1,1,0,KODE)
     if (KODE.gt.0) write (*,'('' OPEN FAILED FOR '',A)') CNAME
  end if

  !  PRINT FILE.

  if (LPT) then
     WRITE (*,'('' ENTER MAIN OUTPUT FILE NAME ('',I2.2,            &
          &                  ''): '')') JOSTND
  end if
  read (*,'(A)',end = 100) CNAME
  call UNBLNK(CNAME,LENNAM)
  if (LENNAM.le.0) CNAME = KWDFIL(:ISTLNB(KWDFIL))//'.out'
  call MYOPEN (JOSTND,CNAME,5,133,0,1,1,1,KODE)
  if (KODE.gt.0) then
     write (*,'('' OPEN FAILED FOR '',A)') CNAME
     write (*,'('' ALL OUTPUT IS SENT TO STANDARD OUT'')')
     JOSTND = 6
  end if

  !  TREELIST OUTPUT.

  if (LPT) then
     WRITE (*,'('' ENTER TREELIST OUTPUT FILE NAME ('',             &
          &        I2.2,''):  '')') JOLIST
  end if
  read (*,'(A)',end = 100) CNAME
  call UNBLNK(CNAME,LENNAM)
  if (LENNAM.le.0) CNAME = KWDFIL(:ISTLNB(KWDFIL))//'.trl'
  call UNBLNK(CNAME,LENNAM)
  call MYOPEN (JOLIST,CNAME,5,133,0,1,1,1,KODE)
  if (KODE.gt.0) write (*,'('' OPEN FAILED FOR '',A)') CNAME

  !  SUMMARY OUTPUT FILE.

  if (LPT) then
     WRITE (*,'('' ENTER SUMMARY OUTPUT FILE NAME ('',              &
          &                  I2.2,''): '')') JOSUM
  end if
  read (*,'(A)',end = 100) CNAME
  call UNBLNK(CNAME,LENNAM)
  if (LENNAM.le.0) CNAME = KWDFIL(:ISTLNB(KWDFIL))//'.sum'
  call UNBLNK(CNAME,LENNAM)
  call MYOPEN (JOSUM,CNAME,5,133,0,1,1,0,KODE)
  if (KODE.gt.0) write (*,'('' OPEN FAILED FOR '',A)') CNAME

  !  AUXILIARY FILE (CHEAPOII) FILE

  if (LPT) then
     WRITE (*,'('' ENTER CHEAPOII/CALBSTAT '',                      &
          &        ''OUTPUT FILE NAME ('',I2.2,''): '')') JOSUME
  end if
  read (*,'(A)',end = 100) CNAME
  call UNBLNK(CNAME,LENNAM)
  if (LENNAM.le.0) CNAME = KWDFIL(:ISTLNB(KWDFIL))//'.chp'
  call MYOPEN (JOSUME,CNAME,5,91,0,1,1,0,KODE)
  if (KODE.gt.0) write (*,'('' OPEN FAILED FOR '',A)') CNAME

  !  FFE SNAG OUTPUT FILE

  CNAME = KWDFIL(:ISTLNB(KWDFIL))//'.sng'
  call MYOPEN (JSNOUT,CNAME,5,91,0,1,1,0,KODE)
  if (KODE.gt.0) then
     write (*,'('' OPEN FAILED FOR '',A)') CNAME
  else
     close(UNIT = JSNOUT, STATUS = 'DELETE')
  end if

  !  OPEN THE SAMPLE TREE SCRATCH FILE.

  CNAME = ' '
  call MYOPEN (JOTREE,CNAME,4,512, 0,2,1,0,KODE)
  if (KODE.gt.0) write (*,'('' OPEN FAILED FOR '',I4)') JOTREE
  100 continue

  return

  entry FILClose
  call DBSCLOSE(.true.,.true.)
  inquire(unit = iread,opened = LOPEN)
  if (LOPEN) close(unit = iread)
  if (JOSTND.ne.6) then
     inquire(unit = JOSTND,opened = LOPEN)
     if (LOPEN) close(unit = JOSTND)
  end if
  inquire(unit = ISTDAT,opened = LOPEN)
  if (LOPEN) close(unit = ISTDAT)
  inquire(unit = JOTREE,opened = LOPEN)
  if (LOPEN) close(unit = JOTREE)
  inquire(unit = JOSUM,opened = LOPEN)
  if (LOPEN) close(unit = JOSUM)
  inquire(unit = JOLIST,opened = LOPEN)
  if (LOPEN) close(unit = JOLIST)
  inquire(unit = JOSUME,opened = LOPEN)
  if (LOPEN) close(unit = JOSUME)
  if (JSVOUT.gt.0) then
     inquire(unit = JSVOUT,opened = LOPEN)
     if (LOPEN) close(unit = JSVOUT)
  end if

  return
end subroutine FILOPN

subroutine openIfClosed (ifileref,sufx,lok)
  implicit none
  ! Variables
  integer :: ifileref,I
  character(len=*) :: sufx
  logical :: lok
  character(len=256) :: keywrdfn
  logical :: lconn

  lok = .true.
  inquire(UNIT = ifileref,opened = lconn)

  if (.not.lconn) then
     call fvsGetKeywordFileName(keywrdfn,len(keywrdfn),I)

     if (keywrdfn.ne.' ') then
        I = index(keywrdfn,".k")
        if (I == 0) I = index(keywrdfn,".K")
        if (I == 0) I = len_trim(keywrdfn)
        keywrdfn = TRIM(keywrdfn(:I-1))//"."//trim(sufx)

        open(UNIT = ifileref,FILE = TRIM(keywrdfn), &
             & POSITION = 'append', STATUS = 'unknown',err = 10)
     end if
  end if
  return
  10 lok = .false.
  return
end subroutine openIfClosed
