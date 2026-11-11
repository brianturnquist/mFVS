!=============================!
!   archive/bgc/src : bgcin   !
!=============================!
subroutine BGCIN (KEYWRD,ARRAY,LNOTBK,LKECHO)

  !  **BGCIN  BGC--DATE OF LAST REVISION: 05/21/13 AJM. ADDED 10TH ARG TO KEYRDR CALL
  !                10/15/99
  !                Revised 11/12/02.  Removing index ISTND, and removing PPE
  !                                   common "includes" (PPEPRM, PPCNTL, &
  !                                   PRGPRM).  AJM
  !                These changes--also made in BGCFVS, BGCGROW, BGCINT, BGCGO,
  !                BINITIAL, and BGCCOM.f77--remove all PPE funtionality.
  !                The FVS-BGC code is now, once again, a single stand model.

  !     OPTION PROCESSOR FOR THE BGC EXTENSION

  !     CALLED FROM: INITRE

  ! Common blocks
  include 'BGCCOM.f90'
  include 'ENTITY.f90'
  include 'PRGPRM.f90'
  !      INCLUDE 'PPCNTL.F77'  ! removed 11/02 ajm
  include 'CONTRL.f90'

  parameter (KWCNT = 12)
  ! Variables
  character(len=8) :: TABLE
  character(len=8) :: KEYWRD
  character(len=8) :: PASKEY
  character(len=10) :: KARD(7)
  logical :: LNOTBK,LKECHO
  dimension PRMS(8),KNDX1(5),KNDX2(33)
  dimension ARRAY(7),TABLE(KWCNT),LNOTBK(7)
  character(len=1) :: ATTRIB1
  character(len=2) :: ATTRIB2
  character(len=7) :: ENDFLAG
  data TABLE /&
       &'END     ','UNDERVEG','VEGOPT  ','BGCGROW ','        ',&
       &'        ','        ','        ','        ','        ',&
       &'        ','        '/

  !     **********          EXECUTION BEGINS          **********

  10 continue
  call KEYRDR (IREAD,JOSTND,.false.,KEYWRD,LNOTBK,&
       &ARRAY,IRECNT,KODE,KARD,.false.,LKECHO)
  print *,'in bgcin, after Keyrdr, keywrd= ', keywrd
  print *,'in bgcin, after Keyrdr, Kode= ', kode

  !     RETURN KODES 0=NO ERROR,1=COLUMN 1 BLANK,2=EOF

  if (KODE .eq.0) goto 30
  if (KODE .eq.2) call ERRGRO(.false.,2)
  call ERRGRO (.true.,6)
  goto 10
  30 continue
  call FNDKEY (NUMBER,KEYWRD,TABLE,KWCNT,KODE,.false.,JOSTND)
  print *,'in bgcin, after FNDKEY, Number= ', number
  print *,'in bgcin, after FNDKEY, keywrd= ', keywrd

  !     RETURN KODES 0=NO ERROR,1=KEYWORD NOT FOUND,2=MISSPELLING.

  if (KODE .eq.0) goto 90
  if (KODE .eq.1) then
     call ERRGRO (.true.,1)
     goto 10
  end if
  goto 90

  !     SPECIAL END-OF-FILE TARGET

  80 continue
  call ERRGRO (.false.,2)
  90 continue

  !     SIGNAL THAT THE BGC EXTENSION IS NOW ACTIVE.

  print *,'in bgcin, lbgcon=',lbgcon
  !      IF (.NOT.LBGCON(ISTND)) THEN ! removed 11/02 ajm
  !         LBGCON(ISTND) = .TRUE.    ! ditto
  if (.not.LBGCON) then
     LBGCON = .true.
     call BGCINITALL
  end if

  !     PROCESS OPTIONS

  go to( 100, 200, 300, 400, 500, 600, 700, 800, 900,1000,&
       &1100,1200), NUMBER

  100 continue
  print *,'in bgcin, option=1, keyword= ', keywrd
  !========== OPTION NUMBER 1: END ==========================
  if (LKECHO)write(JOSTND,110) KEYWRD
  110 format (/A8,'   END OF BGC OPTIONS.')
  return
  200 continue
  print *,'in bgcin, option=2, keyword= ', keywrd
  !========== OPTION NUMBER 2: UNDERVEG =====================
  if (LKECHO)write(JOSTND,205) KEYWRD
  205 format (/A8,'   STAND-BGC UNDERSTORY VEGETATION INPUT')
  206 read(IREAD,*)ENDFLAG
  do I = 1,7
     call UPCASE(ENDFLAG(I:I))
     210 continue
  end do
  if (ENDFLAG.ne.'ENDENT') then
     backspace IREAD
     read(IREAD,*) ATTRIB1, ATTRIB2, ATTRIB3, ATTRIB4
     NVE = NVE+1
     VHT(NVE) = ATTRIB3
     VCOV(NVE) = ATTRIB4
     VSP(NVE) = ATTRIB2
     VID(NVE) = ATTRIB1
     goto 206
  end if
  print *,'in bgcin, option=2, end of veg data'
  print *,'in bgcin, option=2, no. of veg entities= ', NVE
  goto 10
  300 continue
  print *,'in bgcin, option=3, keyword= ', keywrd
  !=================OPTION NUMBER 3: VEGOPT =============
  ISOURCE = IFIX(ARRAY(1))
  print *,'in bgcin, option=3, isource= ', ISOURCE
  if (LKECHO)write(JOSTND,305) KEYWRD,ISOURCE
  305 format (/A8,'   SOURCE OF VEG DATA= ', I1)
  goto 10
  400 continue
  !=================OPTION NUMBER 4: BGCGROW ============
  !      IBGC(ISTND)=1  ! removed 11/02 ajm
  IBGC = 1
  print *,'in bgcin, option=4'
  if (LKECHO)write(JOSTND,405) KEYWRD
  405 format (/A8,'   INCREMENTS WILL BE FROM STAND-BGC')
  goto 10
  500 continue
  !=================OPTION NUMBER 5: BGCIN ==============
  goto 10
  600 continue
  !                        OPTION NUMBER 6 --
  goto 10
  700 continue
  !                        OPTION NUMBER 7 --
  goto 10
  800 continue
  !                        OPTION NUMBER 8 --
  goto 10
  900 continue
  !                        OPTION NUMBER 9 --
  goto 10
  1000 continue
  !                        OPTION NUMBER 10 --
  goto 10
  1100 continue
  !                        OPTION NUMBER 11 --
  goto 10
  1200 continue
  !                        OPTION NUMBER 12 --
  goto 10

  !     Special entry to retrieve keywords.

  entry BGCKEY (KEY,PASKEY)
  PASKEY = TABLE(KEY)
  return
end
