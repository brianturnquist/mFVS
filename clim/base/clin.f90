!======================!
!   clim/base : clin   !
!======================!
! CLIM-BASE $Id$
subroutine CLIN (DEBUG,LKECHO)
  implicit none

  !     CLIMATE EXTENSION

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'CLIMATE.f90'

  ! Variables
  integer :: KWCNT
  parameter (KWCNT = 8)

  character(len=8) :: TABLE(KWCNT)
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=10) :: KARD(12)
  character(len=2048) :: CHTMP
  character(len=2048) :: FMT
  character(len=40) :: CSTDID
  character(len=30) :: CCLIM
  character(len=30) :: CYR
  logical :: LNOTBK(12),LKECHO,DEBUG
  real    :: ARRAY(12),TMPATTRS(MXCLATTRS),PRMS(8)
  integer :: KODE,IPRMPT,NUMBER,I,I1,I2,IYRTMP,IUTMP,IRTNCD,IDT,IPAS

  data TABLE /&
       &'END','CLIMDATA','MORTMULT','AUTOESTB','GROWMULT',&
       &'MXDENMLT','CLIMREPT','SETATTR'/

  !     **********          EXECUTION BEGINS          **********

  !     SIGNAL THAT THE CLIMATE MODEL IS NOW ACTIVE.

  10 continue

  call KEYRDR (IREAD,JOSTND,DEBUG,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,LFLAG,LKECHO)

  !  RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK OR ANOTHER ERROR,2=EOF
  !               LESS THAN ZERO...USE OF PARMS STATEMENT IS PRESENT.

  if (KODE.lt.0) then
     IPRMPT = -KODE
  else
     IPRMPT = 0
  end if
  if (KODE .le.0) go to 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  call ERRGRO (.true.,6)
  goto 10
  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.false.,JOSTND)

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

  if (KODE .eq.0) goto 90
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if
  goto 90

  !     SPECIAL END-OF-FILE TARGET

  80 continue
  call ERRGRO(.false.,2)
  call fvsGetRtnCode(IRTNCD)
  if (IRTNCD.ne.0) return

  90 continue

  !     PROCESS OPTIONS

  go to( 100, 200, 300, 400, 500, 600, 700, 800), NUMBER

  100 continue
  !                        OPTION NUMBER 1 -- END

  LCLIMATE = NATTRS .gt.0.and. NYEARS.gt.0

  if (LKECHO .and. LCLIMATE) write(JOSTND,105) KEYWRD
  105 format (/A8,'   CLIMATE EXTENSION ACTIVE, END OF OPTIONS.')
  if (LKECHO .and..not. LCLIMATE) write(JOSTND,106) KEYWRD
  106 format (/A8,'   CLIMATE EXTENSION MISSING REQUIRED DATA ',&
       &'AND IS NOT ACTIVE. END OF OPTIONS.')
  return

  200 continue
  !                        OPTION NUMBER 2 -- CLIMDATA
  NATTRS = 0
  NYEARS = 0
  INDXSPECIES = 0
  read (IREAD,'(A)',end = 80) CLIMATE_NAME
  IRECNT = IRECNT+1
  CLIMATE_NAME = ADJUSTL(CLIMATE_NAME)
  read (IREAD,'(A)',end = 80) CHTMP !GET THE FILE NAME
  IRECNT = IRECNT+1
  CHTMP = ADJUSTL(CHTMP)
  if (LKECHO)write (JOSTND,210) KEYWRD,TRIM(CLIMATE_NAME),&
       &TRIM(NPLT),TRIM(CHTMP)
  210 format (/A8,T12,'READING ATTRS SCORES FOR CLIMATE=',A,&
       &' AT STAND=',A,' FROM FILE=',A)
  if (CHTMP.eq."*") then
     IUTMP = IREAD
  else
     open (NEWUNIT = IUTMP,FILE = TRIM(CHTMP),STATUS = 'OLD',ERR = 298)
  end if
  I2 = 0
  read (IUTMP,'(A)',end = 296) CHTMP
  I2 = I2+1
  if (CHTMP.eq."-999") goto 296
  read (CHTMP,*,end = 211) CSTDID,CCLIM,CYR,ATTR_LABELS ! toss the first three
  211 continue
  CHTMP(1:5) = CSTDID(1:5)
  do I = 1,5
     call UPCASE(CHTMP(I:I))
  end do
  if (CHTMP(1:5) .ne. "STAND") write (JOSTND,212) TRIM(CSTDID)
  212 format (T12,'ATTRS FILE CREATION TAG=',A)
  do I = 1,MXCLATTRS
     if (ICHAR(ATTR_LABELS(I)(1:1)).eq.0.or.&
          &ATTR_LABELS(I)(1:1).eq.' ') exit
     NATTRS = NATTRS+1
  end do
  215 continue
  read (IUTMP,'(A)',end = 290) CHTMP
  I2 = I2+1
  if (CHTMP.eq."-999") goto 290
  read (CHTMP,*) CSTDID,CCLIM
  if (TRIM(CSTDID).ne.TRIM(NPLT)) goto 215
  if (TRIM(CCLIM) .ne.TRIM(CLIMATE_NAME)) goto 215
  read (CHTMP,*) CSTDID,CCLIM,IYRTMP,(TMPATTRS(I),I = 1,NATTRS)
  if (NYEARS.eq.0) then
     NYEARS = 1
     YEARS(1) = IYRTMP
     I1 = 1
  else
     I1 = 0
     do I = 1,NYEARS
        if (YEARS(I).ne.IYRTMP) cycle
        I1 = I
        exit
     end do
     if (I1.eq.0) then
        if (NYEARS.eq.MXCLYEARS) then
           write (JOSTND,220) I2
           220 format (/,'********   CLIMATE ERROR: ',&
                &'TOO MANY YEARS IN CLIMATE DATA.',/&
                &T27,'RECORDS READ = ', I4)
           NATTRS = 0
           NYEARS = 0
           INDXSPECIES = 0
           if (IUTMP.eq.IREAD) then
              IRECNT = IRECNT+I2
              !             READ TO THE END OF DATA MARK (-999)
              do
                 read (IUTMP,'(A)',end = 80) CHTMP
                 IRECNT = IRECNT+1
                 if (CHTMP(1:4).eq.'-999') exit
              end do
           else
              close (UNIT = IUTMP)
           end if
           call RCDSET (2,.true.)
           goto 10
        end if
        NYEARS = NYEARS+1
        I1 = NYEARS
        YEARS(NYEARS) = IYRTMP
     end if
  end if
  ATTRS(I1,1:NATTRS) = TMPATTRS(1:NATTRS)
  goto 215
  290 continue
  if (LKECHO)write (JOSTND,291) I2,NATTRS,NYEARS
  291 format (T12,'RECORDS READ=',I4,'; NUMBER OF ATTRIBUTES=',&
       &I4,'; NUMBER OF YEARS=',I4)
  if (NYEARS*NATTRS.eq.0) then
     write (JOSTND,2915)
     2915 format (/,'********   CLIMATE ERROR: ',&
          &'NO CLIMATE DATA FOR THIS STAND.')
     NATTRS = 0
     NYEARS = 0
     INDXSPECIES = 0
     if (IUTMP.eq.IREAD) then
        IRECNT = IRECNT+I2
     else
        close (UNIT = IUTMP)
     end if
     call RCDSET (2,.true.)
     goto 10
  end if
  if (IUTMP.eq.IREAD) then
     IRECNT = IRECNT+I2
  else
     close (UNIT = IUTMP)
  end if
  do I1 = 1,NATTRS
     if ('dd5'  .eq.TRIM(ATTR_LABELS(I1))) IXDD5 = I1
     if ('mat'  .eq.TRIM(ATTR_LABELS(I1))) IXMAT = I1
     if ('map'  .eq.TRIM(ATTR_LABELS(I1))) IXMAP = I1
     if ('mtcm' .eq.TRIM(ATTR_LABELS(I1))) IXMTCM = I1
     if ('mtwm' .eq.TRIM(ATTR_LABELS(I1))) IXMTWM = I1
     if ('gsp'  .eq.TRIM(ATTR_LABELS(I1))) IXGSP = I1
     if ('d100' .eq.TRIM(ATTR_LABELS(I1))) IXD100 = I1
     if ('mmin' .eq.TRIM(ATTR_LABELS(I1))) IXMMIN = I1
     if ('dd0'  .eq.TRIM(ATTR_LABELS(I1))) IXDD0 = I1
     if ('gsdd5'.eq.TRIM(ATTR_LABELS(I1))) IXGSDD5 = I1
     if ('pSite'.eq.TRIM(ATTR_LABELS(I1))) IXPSITE = I1
     if ('DEmtwm'  .eq.TRIM(ATTR_LABELS(I1))) IDEmtwm = I1
     if ('DEmtcm'  .eq.TRIM(ATTR_LABELS(I1))) IDEmtcm = I1
     if ('DEdd5'   .eq.TRIM(ATTR_LABELS(I1))) IDEdd5 = I1
     if ('DEsdi'   .eq.TRIM(ATTR_LABELS(I1))) IDEsdi = I1
     if ('DEdd0'   .eq.TRIM(ATTR_LABELS(I1))) IDEdd0 = I1
     if ('DEpdd5'  .eq.TRIM(ATTR_LABELS(I1))) IDEmapdd5 = I1
     if (      IXMTCM.gt.0.and. IXMAT  .gt.0&
          &.and. IXGSP .gt.0.and. IXD100.gt.0&
          &.and. IXMMIN.gt.0.and. IXDD0.gt.0&
          &.and. IXDD5.gt.0.and. IXPSITE.gt.0&
          &.and. IXGSDD5.gt.0&
          &.and. IDEmtwm   .gt.0&
          &.and. IDEmtcm   .gt.0&
          &.and. IDEdd5.gt.0&
          &.and. IDEsdi    .gt.0&
          &.and. IDEdd0.gt.0&
          &.and. IDEmapdd5.gt.0) exit
  end do

  do I = 1,MAXSP
     do I1 = 1,NATTRS
        if (TRIM(PLNJSP(I)).eq.TRIM(ATTR_LABELS(I1))) then
           INDXSPECIES(I) = I1
           exit
        end if
     end do
  end do

  if (LKECHO) then
     I2 = 1
     do I = 1,NATTRS
        CHTMP(I2:) = ''
        if (IXMTCM.eq.I .or. IXDD5.eq.I   .or. IXMAT  .eq.I .or.&
             &IXPSITE.eq.I .or. IXGSP.eq.I   .or. IXD100.eq.I .or.&
             &IXGSDD5.eq.I .or. IXMMIN.eq.I  .or. IXDD0.eq.I .or.&
             &IXMTWM .eq.I .or. IXMAP .eq.I  .or.&
             &IDEmtwm.eq.I .or. IDEmtcm.eq.I .or. IDEdd5.eq.I  .or.&
             &IDEdd0.eq.I .or. IDEmapdd5.eq.I.or.IDEsdi.eq.I )&
             &CHTMP(I2:) = ' *USED*'
        do I1 = 1,MAXSP
           if (TRIM(PLNJSP(I1)).eq.TRIM(ATTR_LABELS(I))) then
              write (CHTMP(I2:),'(I3,"=",A)') I1,JSP(I1)
              exit
           end if
        end do
        I2 = I2+8
     end do
     I2 = 0
     do I = 1,NATTRS,14 ! write them 14 at a time
        I2 = I2+14
        if (I2.gt.NATTRS) I2 = NATTRS
        write (JOSTND,292) TRIM(CHTMP(((I-1)*8)+1:(I2*8)))
        292 format (/T12,'CODES: ',A)
        write (JOSTND,293) ADJUSTR(ATTR_LABELS(I:I2))
        293 format ( T12,'YEAR ',14A8)
        FMT = ' T12,I4,1X'
        do I1 = I,I2
           FMT(1:1) = ATTR_LABELS(I1)(1:1)
           call UPCASE(FMT(1:1))
           if (ATTR_LABELS(I1)(1:1).eq.FMT(1:1)) then
              if (ATTR_LABELS(I1)(1:2).eq.'DE') then
                 FMT = TRIM(FMT)//',F8.2'
              else
                 FMT = TRIM(FMT)//',F8.3'
              end if
           else
              FMT = TRIM(FMT)//',F8.1'
           end if
        end do
        FMT(1:1) = '('
        FMT = TRIM(FMT)//')'
        do I1 = 1,NYEARS
           write (JOSTND,FMT) YEARS(I1),ATTRS(I1,I:I2)
        end do
     end do
     I2 = 0
     do I = 1,MAXSP
        if (INDXSPECIES(I).eq.0) I2 = I2+1
     end do
     if (I2.gt.0) then
        write (JOSTND,2935)
        2935 format (/,'********   CLIMATE WARNING: ',&
             &'SPECIES WITHOUT ATTRIBUTES:')
        do I = 1,MAXSP
           if (INDXSPECIES(I).eq.0) write (JOSTND,294)&
                &I,JSP(I),PLNJSP(I)
           294 format (T12,'FVS INDEX=',I3,', ALPHA CODE=',A3,&
                &', PLANT CODE=',A)
        end do
     end if
  end if

  !     TURN ON CLIMATE ESTAB AND TURN OFF BASE ESTAB.

  !      LAESTB=.TRUE.
  !      CALL ESNOAU ('********',LKECHO)

  goto 10
  296 continue
  write (JOSTND,297)
  297 format (/,'********   CLIMATE ERROR: ',&
       &'FILE READ ERROR, DATA NOT STORED.')
  NATTRS = 0
  NYEARS = 0
  INDXSPECIES = 0
  if (IUTMP.ne.IREAD) then
     close (UNIT = IUTMP)
     call RCDSET (2,.true.)
     goto 10
  else
     goto 80
  end if
  298 continue
  write (JOSTND,299)
  299 format (/,'********   CLIMATE ERROR: ',&
       &'FILE OPEN ERROR, FILE NOT READ.')
  call RCDSET (2,.true.)
  goto 10
  300 continue
  !                        OPTION NUMBER 3 -- MORTMULT
  !     ACTIVITY CODE 2801
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,2801,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else
     call SPDECD (2,I,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)
     if (.not.LNOTBK(1)) then ! YEAR/CYCLE NOT SPECIFIED
        if (I.gt.0.and. I.le.MAXSP) then
           if (LNOTBK(3)) CLMRTMLT1(I) = ARRAY(3)
           if (LNOTBK(4)) CLMRTMLT2(I) = ARRAY(4)
           if (LKECHO)write(JOSTND,310) KEYWRD,I,JSP(I),CLMRTMLT1(I),&
                &CLMRTMLT2(I)
           310 format (/A8,'   CLIMATE-CAUSED MORTALITY MULTIPLIERS',&
                &' FOR SPECIES ',I3,'=',A,' ARE ',2F12.4)
        else if (I.eq.0) then
           if (LNOTBK(3)) CLMRTMLT1 = ARRAY(3)
           if (LNOTBK(4)) CLMRTMLT2 = ARRAY(4)
           if (LKECHO)write(JOSTND,315) KEYWRD,CLMRTMLT1(1),&
                &CLMRTMLT2(1)
           315 format (/A8,'   CLIMATE-CAUSED MORTALITY MULTIPLIERS',&
                &' FOR ALL SPECIES ARE',2F12.4)
        end if
     else
        if (I.gt.0.and. I.le.MAXSP) then
           PRMS(1) = I
        else
           PRMS(1) = 0
        end if
        PRMS(2) = ARRAY(3)
        PRMS(3) = ARRAY(4)
        call OPNEW (KODE,IDT,2801,3,PRMS)
        if (KODE .gt.0) goto 10
        if (LKECHO)write(JOSTND,325) KEYWRD,IDT,IFIX(PRMS(1)),&
             &KARD(2)(1:3),PRMS(2),PRMS(3)
        325 format (/A8,'   DATE/CYCLE=',I5,' CLIMATE-CAUSED MORTALITY ',&
             &' MULTIPLIERS FOR SPECIES ',I4,'=',A,'    ARE ',2F12.4)
     end if
  end if
  goto 10
  400 continue
  !                        OPTION NUMBER 4 -- AUTOESTB
  !     ACTIVITY CODE 2802
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))
  if (.not.LNOTBK(1)) then
     if (LNOTBK(2)) AESTOCK = ARRAY(2)
     if (LNOTBK(3)) AESNTREES = ARRAY(3)
     if (LNOTBK(4)) NESPECIES = IFIX(ARRAY(4))
     if (NESPECIES.gt.MAXSP) NESPECIES = MAXSP
     LAESTB = .true.
     if (LKECHO)write(JOSTND,410) KEYWRD,AESTOCK,AESNTREES,NESPECIES
     410 format (/A8,'   TURN ON AUTOMATIC',&
          &' ESTABLISHMENT IN THE CLIMATE MODEL. STOCKING THRESHOLD= ',&
          &F6.2,' PERCENT; BASE NUMBER OF TREES/ACRE TO ADD ',F6.2/T12,&
          &'MAXIMUM NUMBER OF SPECIES TO ADD =',I3)
  else
     if (IPRMPT.gt.0) then
        if (IPRMPT.ne.2) then
           call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
           call ERRGRO (.true.,25)
        else
           call OPNEWC (KODE,JOSTND,IREAD,IDT,2802,KEYWRD,KARD,&
                &IPRMPT,IRECNT,ICYC)
           call fvsGetRtnCode(IRTNCD)
           if (IRTNCD.ne.0) return
        end if
     else
        PRMS(1) = AESTOCK
        PRMS(2) = AESNTREES
        PRMS(3) = NESPECIES
        if (LNOTBK(2)) PRMS(1) = ARRAY(2)
        if (LNOTBK(3)) PRMS(2) = ARRAY(3)
        if (LNOTBK(4)) PRMS(3) = IFIX(ARRAY(4))
        call OPNEW (KODE,IDT,2802,3,PRMS)
        if (KODE .gt.0) goto 10
        if (LKECHO)write(JOSTND,425) KEYWRD,IDT,PRMS(1),PRMS(2),&
             &IFIX(PRMS(3))
        425 format (/A8,'   DATE/CYCLE=',I5,' TURN ON AUTOMATIC',&
             &' ESTABLISHMENT IN THE CLIMATE MODEL. STOCKING THRESHOLD= ',&
             &F6.2,' PERCENT'/T12,'BASE NUMBER OF TREES/ACRE TO ADD ',&
             &F6.2,'MAXIMUM NUMBER OF SPECIES TO ADD =',I3)
     end if
  end if
  call ESNOAU (KEYWRD,LKECHO)
  goto 10
  500 continue
  !                        OPTION NUMBER 5 -- GROWMULT
  !     ACTIVITY CODE 2803
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,2803,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else
     call SPDECD (2,I,NSP(1,1),JOSTND,IRECNT,KEYWRD,&
          &ARRAY,KARD)
     if (.not.LNOTBK(1)) then
        if (I.gt.0.and. I.le.MAXSP) then
           if (LNOTBK(3)) CLGROWMULT(I) = ARRAY(3)
           if (LKECHO)write(JOSTND,510) KEYWRD,I,JSP(I),CLGROWMULT(I)
           510 format (/A8,'   CLIMATE-CAUSED GROWTH MULTIPLIER',&
                &' FOR SPECIES ',I3,'=',A,' IS ',F12.4)
        else if (I.eq.0) then
           if (LNOTBK(3)) CLGROWMULT = ARRAY(3)
           if (LKECHO)write(JOSTND,515) KEYWRD,CLGROWMULT(1)
           515 format (/A8,'   CLIMATE-CAUSED GROWTH MULTIPLIER',&
                &' FOR ALL SPECIES IS',F12.4)
        end if
     else
        if (I.gt.0.and. I.le.MAXSP) then
           PRMS(1) = I
        else
           PRMS(1) = 0
        end if
        PRMS(2) = ARRAY(3)
        call OPNEW (KODE,IDT,2803,2,PRMS)
        if (KODE .gt.0) goto 10
        if (LKECHO)write(JOSTND,525) KEYWRD,IDT,IFIX(PRMS(1)),&
             &KARD(2)(1:3),PRMS(2)
        525 format (/A8,'   DATE/CYCLE=',I5,' CLIMATE-CAUSED GROWTH ',&
             &' MULTIPLIER FOR SPECIES ',I4,'=',A,'    IS ',F10.4)
     end if
  end if
  goto 10
  600 continue
  !                        OPTION NUMBER 6 -- MXDENMLT
  !     ACTIVITY CODE 2804
  IDT = 1
  if (LNOTBK(1)) IDT = IFIX(ARRAY(1))

  !     IF THE KEYWORD RECORD HAS THE 'PARMS' OPTION, CALL OPNEWC
  !     TO PROCESS IT.

  if (IPRMPT.gt.0) then
     if (IPRMPT.ne.2) then
        call KEYDMP (JOSTND,IRECNT,KEYWRD,ARRAY,KARD)
        call ERRGRO (.true.,25)
     else
        call OPNEWC (KODE,JOSTND,IREAD,IDT,2804,KEYWRD,KARD,&
             &IPRMPT,IRECNT,ICYC)
        call fvsGetRtnCode(IRTNCD)
        if (IRTNCD.ne.0) return
     end if
  else
     if (.not.LNOTBK(1)) then
        if (LNOTBK(2)) CLMXDENMULT = ARRAY(2)
        if (LKECHO) write(JOSTND,610) KEYWRD,CLMXDENMULT
        610 format (/A8,'   CLIMATE-CAUSED MAXIMUM DENSITY ADJUSTMENT',&
             &' MULTIPLIER IS ',F10.4)
     else
        PRMS(1) = CLMXDENMULT
        if (LNOTBK(2)) PRMS(1) = ARRAY(2)
        call OPNEW (KODE,IDT,2804,1,PRMS)
        if (KODE .gt.0) goto 10
        if (LKECHO)write(JOSTND,620) KEYWRD,IDT,PRMS(1)
        620 format (/A8,'   DATE/CYCLE=',I5,&
             &' CLIMATE-CAUSED MAXIMUM DENSITY ADJUSTMENT',&
             &' MULTIPLIER IS ',F10.4)
     end if
  end if
  goto 10
  700 continue
  !                        OPTION NUMBER 6 -- CLIMREPT
  JCLREF = 0
  if (LKECHO) write(JOSTND,710) KEYWRD
  710 format (/A8,'   GENERATE FVS-CLIMATE REPORT.')
  goto 10
  !                        OPTION NUMBER 7 -- SETATTR
  800 continue
  if (NATTRS .le.0) then
     call ERRGRO (.true.,30)
     write (JOSTND,810)
     810 format (T12,'SETATTR KEYWORD MUST FOLLOW CLIMDATA KEYWORD.')
     goto 10
  end if
  KARD(1) = ADJUSTL(KARD(1))
  if (KARD(1) .eq. " ") goto 825
  do I = 1,NATTRS
     if (TRIM(KARD(1)).eq.TRIM(ATTR_LABELS(I))) then
        if (LKECHO) write(JOSTND,820) KEYWRD,TRIM(KARD(1)),&
             &ATTRS(1:4,I),ARRAY(2:5)
        820 format (/A8,'   ATTRIBUTE "',A,'" VALUES CHANGED FROM: ',&
             &4F10.4,' TO: ',4F10.4)
        ATTRS(1:4,I) = ARRAY(2:5)
        goto 10
     end if
  end do
  825 continue
  call ERRGRO (.true.,4)
  write (JOSTND,830)
  830 format (T12,'SETATTR ATTRIBUTE STRING WAS NOT MATCHED.')
  goto 10
  return

  entry CLKEY(IPAS,PASKEY)
  PASKEY = TABLE(IPAS)
  return
end
