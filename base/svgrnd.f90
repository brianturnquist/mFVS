!=================================!
!   Stand visualization: SVGRND   !
!=================================!
! BASE $Id$
subroutine SVGRND (NOUT,KYLAST,KYFRST,IFIREFLG)
  implicit none

  !     SVS BASIC GROUND DEFINITION GENERATION
  !     D.L.GAMMEL    -- SEM         -- JUNE 2002
  !     D. ROBINSON   -- ESSA        -- MAY 2005
  !     S.SCHAROSCH   -- Abacus      -- MAR 2008

  !     INPUT:
  !     NOUT - THE OUTPUT FILE REFERENCE NUMBER
  !     KYFRST - THE INDEX FOR THE FIRST CHAR NEEDED FROM
  !              FROM THE KEYWORD FOR GENERATING THE FILE NAME
  !     KYLAST - THE INDEX OF THE LAST CHAR NEEDED FOR THE
  !              FILE NAME
  !     IFIREFLG- THE IDENTIFIER OF THE CALLING CONTEXT
  !               0=INVENTORY TIME
  !               1=START OF CYCLE
  !               2=AFTER CUTS
  !               3=END OF PROJECTION (CALLED FROM MAIN)
  !               4=CALLED FROM FIRE MODEL, INCLUDES FLAMES
  !               5=CALLED FROM FIRE MODEL, NO FLAMES

  !     BASIC GROUND COLOR CODES (SET VIA SVS KEYWORD FIELD 5):
  !     ICOLIDX  COLOR SCHEME      COLOR CODES
  !              BLACK               0
  !              BLUE                1
  !              GREEN               2
  !              CYAN                3
  !              RED                 4
  !              MAGENTA             5
  !              BROWN               6
  !              GRAY                7
  !              DARK GRAY           8
  !              LIGHT BLUE          9
  !              LIGHT GREEN        10
  !              LIGHT CYAN         11
  !              LIGHT RED          12
  !              LIGHT MAGENTA      13
  !              YELLOW             14
  !              WHITE              15
  !        1     REDS/ORANGES       16 to 27
  !        2     TANS               28 to 39
  !        3     REDDISH BROWNS     40 to 51
  !        4     LT GREYS           52 to 63
  !        5     LIME GREENS        64 to 75
  !        6     DARK GREENS        76 to 87
  !        7     MEDIUM BLUEGREENS  88 to 99
  !        8     DARK GREENS       100 to 111
  !        9     DARK GREYS        112 to 123
  !       10     LIGHT BROWNS      124 to 135
  !       11     KHAKI/BROWNS      136 to 147
  !       12     LIGHT GREENS      148 to 159
  !       13     DARK BLUEGREENS   160 to 171
  !       14     CHOCOLATE BROWNS  172 to 183
  !       15     DARK TANS         184 to 195
  !       16     YELLOWS           196 to 207
  !       17     MEDIUM GREYS      208 to 219
  !       18     BROWNS            220 to 231
  !       19     GREENS            232 to 243
  !       20     DARK BROWNS       244 to 255

  ! Common blocks
  include 'PRGPRM.F77'
  include 'SVDATA.F77'
  include 'CONTRL.F77'

  ! Variables
  character(len=256) :: GRIDLINE
  character(len=100) :: FILENAME
  character(len=30) :: COLORCODE
  integer :: ICOLOR,GRNDOUT,NOUT,I,J,X,Y,INTCOLOR,KYFRST,KYLAST,KODE,IFIREFLG
  logical :: LMONO
  real    :: K,FLINE,FLNY

  dimension FLINE(256)
  dimension FLNY(20)

  ICOLOR = ICOLIDX*12+4
  GRNDOUT = 222
  COLORCODE = "ABCDEFGHIJ0123456789QRSTUVWXYZ"

  !     If the base color set was input as a negative value,
  !     interpret it as a request to generate a monochrome ground
  !     surface of the indicated color (so CWD objects are more visible)

  if ( ICOLIDX .lt. 0 ) then
     LMONO = .true.
     ICOLIDX = (-1) * ICOLIDX
     ICOLOR = ICOLIDX*12+14
  else
     LMONO = .false.
  end if

  !     CREATE THE GROUND FILE NAME

  if (IFIREFLG.gt.3) then
     if (NIMAGE.lt.1000) then
        write(FILENAME,10)KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX,NIMAGE
        10 format(A,'_g',I3.3,'_c',I3.3,'_',I3.3,'.grd')
     else
        write(FILENAME,20)KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX,NIMAGE
        20 format(A,'_g',I3.3,'_c',I3.3,'_',I6.6,'.grd')
     end if
  else
     write(FILENAME,'(A,''_g'',I3.3,''_c'',I3.3,''.grd'')') &
          & KWDFIL(KYFRST:KYLAST),IGRID,ICOLIDX
  end if

  !     ADD GROUNDFILE LINE TO SVS FILE

  write(NOUT, '(''#GROUNDFILE '',A/)') TRIM(FILENAME)

  !     IF THIS IS NOT A FIRE FILE OR THE BEGINNING OF THE INVENTORY
  !     THEN WE ARE FINISHED

  if (IFIREFLG.ge.1.and.IFIREFLG.le.3) return

  !     TRY OPENING FILE WITH PATH NAME INCLUDED

  call MYOPEN(GRNDOUT,TRIM(KWDFIL(:KYLAST)//'/'//FILENAME), &
       & 5,120,0,1,1,0,KODE)

  !     IF OPEN FAILS THEN TRY OPENING FILE W/OUT THE PATH

  if (KODE.gt.0) then
     call MYOPEN(GRNDOUT,TRIM(FILENAME),5,120,0,1,1,0,KODE)
     if (KODE.gt.0) then
        IGRID = 0
        return
     end if
  end if

  !     PRINT OUT THE BASE COLOR SET
  !     The base color set is comprised of:
  !       1) User-specified color scheme from the SVS keyword
  !       2) Colors 16-25 (red/orange): used for red fire line
  !       3) Colors 113-122 (dark greys): used for black fire line

  write(GRNDOUT, '(2I5)') IGRID, IGRID
  if ( LMONO ) then
     do I = 1, 10
        write(GRNDOUT,30) COLORCODE(I:I),(ICOLOR)
     end do
  else
     do I = 1, 10
        write(GRNDOUT,30) COLORCODE(I:I),(ICOLOR+I-0)
     end do
     30 format(A1,I3)
  end if

  do I = 11, 20
     write(GRNDOUT,30) COLORCODE(I:I),(I+5)
  end do
  do I = 21, 30
     write(GRNDOUT,30) COLORCODE(I:I),(I+92)
  end do
  write(GRNDOUT, '(''* 7'')')

  !     IF THERE IS A FIRE THEN GRAB THE FIRE LINE

  if (IFIREFLG.eq.4) then
     call FMGETFL(20, FLNY)
     do I = 1,IGRID
        J = INT(I*20/IGRID+1)
        if (J.gt.20) J = 20

        if (IMETRIC.eq.0) then
           FLINE(I) = IGRID - INT((FLNY(J) / 208.7 * IGRID) + 0.5)
        else
           FLINE(I) = IGRID - INT((FLNY(J) / 100.0 * IGRID) + 0.5)
        end if

     end do
  end if

  !     RANDOMLY GENERATE THE GROUND FILE

  do Y = 1, IGRID
     do X = 1, IGRID
        call SVRANN(K)

        !         IF THERE IS A FIRE THEN GENERATE THE FIRE EFFECTS

        if (IFIREFLG.eq.4) then
           !           CREATE RED LINE
           if (Y.gt.FLINE(X).and.Y.le.(FLINE(X)+(IGRID/10))) then
              INTCOLOR = INT(K * 10) + 11
              !           CREATE RED OR BLACK LINE
           else if (Y.gt.(FLINE(X)+IGRID/10).and. &
                & Y.le.(FLINE(X)+IGRID/5)) then
              INTCOLOR = INT(K * 20) + 11
              !           CREATE BLACK LINE
           else if (Y.gt.(FLINE(X)+IGRID/5)) then
              INTCOLOR = INT(K * 10) + 21
           else
              INTCOLOR = INT(K * 10) + 1
           end if

           !         NO FIRE

        else if (IFIREFLG.eq.5) then
           !           AFTER A FIRE
           INTCOLOR = INT(K * 10) + 21
        else
           !           NO FIRE EFFECTS
           INTCOLOR = INT(K * 10) + 1
        end if
        GRIDLINE(X:X) = COLORCODE(INTCOLOR:INTCOLOR)
     end do
     write(GRNDOUT, '(A)') GRIDLINE(:IGRID)
  end do
  close(GRNDOUT)
end subroutine SVGRND
