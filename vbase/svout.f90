!===================!
!   vbase : svout   !
!===================!
! VBASE $Id$
subroutine SVOUT(IYEAR,IFMCLFG,AMSG)
  implicit none

  !     STAND VISUALIZATION GENERATION
  !     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
  !     D. ROBINSON   -- ESSA        -- MAY 2005
  !     L.R. DAVID    -- FHTET FORT COLLINS -- 07/20/05
  !     S.SCHAROSCH   -- ABACUS      -- MAR 2008
  !     C. KEYSER     -- FS FMSC     -- AUG 2009

  !     INPUT:
  !     IYEAR = THE YEAR PART OF THE TITLE
  !     IFMCLFG= THE IDENTIFIER OF THE CALLING CONTEXT
  !               0=INVENTORY TIME
  !               1=START OF CYCLE
  !               2=AFTER CUTS
  !               3=END OF PROJECTION (CALLED FROM MAIN)
  !               4=CALLED FROM FIRE MODEL, INCLUDES FLAMES
  !               5=CALLED FROM FIRE MODEL, NO FLAMES
  !             CODES 0,1,2 HAVE EQ BEHAVIOR.
  !     AMSG  = THE MESSAGE, USUALLY THE CALLING CONTEXT, OUTPUT ON
  !             THE TITLE.

  !     OUTPUT THE CURRENT VISUALIZATION DATA

  !     Called from: GRINCR
  !                  MAIN
  !                  SVCUTS: to display post-thin SVS diagram.
  !                  SVSTART: to display SVS diagram for initial inventory.
  !                  FMSVOUT: to display fire burning through stand.

  !     CURRENT TREE CLASSES:
  !      0 - GREEN TREE (STANDING OR RECENTLY CUT TREE)
  !     90 - NEW WESTWIDE PINE BEETLE KILL, OFF-GREEN
  !     91 - 1 YEAR OLD WWPB KILL, RED TREE
  !     92 - 2 YEAR OLD WWPB KILL, FADING TREE
  !     94 - SNAG
  !     95 - RED AND GREEN TREE (BURNT 99) NOT USED YET
  !     96 - GREY SNAG (BURNT 94) - OLDER BURNED TREE
  !     97 - GREY TREE (BURNT 98 OR 99) - RECENTLY BURNED TREE
  !     98 - RED TREE (RECENTLY DEAD STANDING OR DOWN TREE)
  !     99 - WILD CARD, WE DON'T USE THIS CODE...IT IS A GREEN
  !          TREE (STANDING OR RECENTLY CUT TREE)

  ! Common blocks
  include 'PRGPRM.f90'
  include 'FMPARM.f90'
  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'FMCOM.f90'
  include 'PLOT.f90'
  include 'SVDATA.f90'
  include 'SVDEAD.f90'
  include 'METRIC.f90'

  ! Variables
  character(*),parameter :: SUFFIX = '_index.svs'
  character(*),parameter :: STDTAG = '"Stand='
  integer :: IFMCLFG,IYEAR,KYLAST,ISTLNB,I,J,K,KYFRST,NOUT,ISVOBJ,IPS,IDIR,ITC,IPUT,IX,ISNAG
  real    :: X,CW,CRAD,XICR,SNDI,SNHT,SNCRTO,SNCRDI,RAD,X1,Y1,X2,Y2,XM1,XM2,XM3
  real    :: EXPSNGS,NFFESNGS,NSTNDNG,XPROB
  character(len=*) :: AMSG
  character(len=2) :: SPCD
  character(len=4) :: SFILE
  character(len=100) :: CBUFF
  character(len=23) :: PLTGEO
  logical :: DEBUG,LOPEN
  call DBCHK (DEBUG,'SVOUT',5,ICYC)
  if (DEBUG) write (JOSTND,5) IYEAR, AMSG, JSVOUT, NSVOBJ,&
       &JSVPIC, NIMAGE, IFMCLFG
  5 format (/' IN SVOUT: IYEAR=',I5,' AMSG=',A,' JSVOUT=',I3,&
       &' NSVOBJ=',I6,' JSVPIC=',I4,' NIMAGE=',I5,' IFMCLFG=',I5)

  if (DEBUG) then
     write (JOSTND,1000) ICYC, IYEAR, ILYEAR,&
          &NDEAD, NSVOBJ
     1000 format (' ','ENTERING SVOUT, ICYC=',I2,&
          &', IYEAR=',I4,', ILYEAR=',I4,':', / ,&
          &' ',T5,'NDEAD=',I4,', NSVOBJ=',I6,//,&
          &' ',T5,'SNAG LIST:',//,&
          &' ',T5,'  I   IDTREE  SPP ODIA OLEN IYRCOD STATUS ',&
          &'FALLDIR OPROB STDNG',/,&
          &' ',T5,'---- -------- --- ---- ---- ------ ------ ',&
          &'------- ----- -----' )
     !                       XXXX XXXXXXXX XXX XX.X XXX. XXXXXX XXXXXX
     !                       XXXXXX. XXXX. XXXX.
     do I = 1,NDEAD
        write(JOSTND,1010) I, OIDTRE(I), ISNSP(I), ODIA(I), OLEN(I),&
             &IYRCOD(I), ISTATUS(I),&
             &FALLDIR(I), SPROBS(I,1), SPROBS(I,2)
        1010 format(' ',T5,I4,1X,I8,1X,I3,1X,F4.1,1X,F4.0,1X,&
             &I6,1X,I6,1X,F7.0,1X,F5.0,1X,F5.0)
     end do

     write(JOSTND,1020)
     1020 format(' ','SNAG SVS OBJECTS CURRENTLY DEFINED:')
     do ISVOBJ = 1,NSVOBJ
        if (IOBJTP(ISVOBJ) .eq.2) then
           write(JOSTND,1030) ISVOBJ, IOBJTP(ISVOBJ), IS2F(ISVOBJ)
           1030 format(' ',T5,'ISVOBJ=',I6,', IOBJTP=',I1,', IS2F=',I6)
        end if
     end do
  end if

  if (JSVOUT.eq.0) return
  if (JSVOUT.lt.0) goto 26 ! PROCESSING IMAGE, BUT NOT OUTPUTING

  !     Make sure that the index file is opened (could be closed if a
  !     restart is being done.

  inquire(unit = JSVOUT,opened = LOPEN)

  if (.not.LOPEN) then
     open(unit = JSVOUT,file = trim(KWDFIL)//SUFFIX,&
          &status = "old",err = 7)

     !       find out the last used value of NIMAGE.

     do
        read(jsvout,'(a)',end = 2) CBUFF
        if (CBUFF(:7).eq.STDTAG) NIMAGE = NIMAGE+1
     end do
     2 continue
     close(unit = JSVOUT)

     open(unit = JSVOUT,file = trim(KWDFIL)//SUFFIX,&
          &position = "append",err = 7)
     goto 9
     7 continue
     write (JOSTND,8) trim(KWDFIL)//SUFFIX
     8 format (/'**** FILE OPEN ERROR FOR FILE: ',A)
     call RCDSET (2,.true.)
     JSVOUT = 0
     return
     9 continue
  end if

  if (IMETRIC.eq.0) then
     if (IPLGEM.lt.2) then
        PLTGEO = '#PLOTSIZE 208.71 208.71'
     else
        PLTGEO = '#ROUNDPLOT 235.5'
     end if
  else
     if (IPLGEM.lt.2) then
        PLTGEO = '#PLOTSIZE 100.00 100.00'
     else
        PLTGEO = '#ROUNDPLOT 112.84'
     end if
  end if

  !     INCREMENT NIMAGE REGARDLESS OF THE FILE OUTPUT STRUCTURE...
  !     THIS IS DONE TO INSURE THAT MULTIPLE RUNS ARE PROCESSED.

  NIMAGE = NIMAGE+1
  if (JSVPIC.gt.0) then

     !       FIND THE FIRST AND LAST CHAR OF THE KEYWORD NAME
     !       WATCH FOR DIRECTORY LEVELS...WE DON'T WANT THEM.

     KYLAST = len_trim(KWDFIL)
     do I = KYLAST,1,-1
        if (KWDFIL(I:I).eq.'/' .or. KWDFIL(I:I).eq.'\') exit
        KYFRST = I
     end do
     if (DEBUG) write (JOSTND,*) 'KYFRST=',KYFRST,&
          &' KYLAST=',KYLAST,' KWDFIL=',KWDFIL(KYFRST:KYLAST)
     if (NIMAGE.lt.1000) then
        write (CBUFF,'(A,''_'',I3.3,''.svs'')')&
             &KWDFIL(KYFRST:KYLAST),NIMAGE
     else
        write (CBUFF,'(A,''_'',I6.6,''.svs'')')&
             &KWDFIL(KYFRST:KYLAST),NIMAGE
     end if
     if (DEBUG) write (JOSTND,*) 'FILE OPEN=',&
          &TRIM(KWDFIL(:KYLAST)//'/'//CBUFF)

     !       TRY TO OPEN A FILE WITH THE DIRECTORY NAME INCLUDED.

     open (UNIT = JSVPIC,FILE = TRIM(KWDFIL(:KYLAST)//'/'//CBUFF),&
          &STATUS = "REPLACE",ERR = 12)

     write (JSVOUT,10) STDTAG,NPLT(1:MAX(1,ISTLNB(NPLT))),IYEAR,&
          &AMSG,KWDFIL(:KYLAST)//'/'//TRIM(CBUFF)
     10 format (A,A,' Year=',I4.4,' ',A,'" "',A,'"')
     goto 20

     !       IF THE OPEN FAILS, THEN OPEN ONE WITHOUT THE DIR NAME INCLUDED.

     12 continue

     open (UNIT = JSVPIC,FILE = TRIM(CBUFF),STATUS = "REPLACE",ERR = 14)
     write (JSVOUT,10) STDTAG,NPLT(1:MAX(1,ISTLNB(NPLT))),IYEAR,&
          &AMSG,TRIM(CBUFF)
     goto 20
     14 continue

     !       IF THIS OPEN FAILS, THEN BAG SVS OUTPUT.

     write (JOSTND,18) TRIM(CBUFF)
     18 format (/T13,'**** FILE OPEN ERROR FOR FILE: ',A)
     call RCDSET (2,.true.)

     !       SETTING JSVOUT TO ZERO TURNS OFF SVS...WE'RE DONE.

     JSVOUT = 0
     return

     20 continue
     NOUT = JSVPIC
  else
     NOUT = JSVOUT
  end if

  select case (VARACD)
  case ('CS','LS','NE','SN')
     SFILE = 'EAST'
  case ('BC')
     SFILE = '  BC'
  case ('ON')
     SFILE = '  ON'
  case default
     SFILE = 'WEST'
  end select

  write (NOUT,21) NPLT(1:MAX(1,ISTLNB(NPLT))),IYEAR,AMSG,&
       &SFILE,PLTGEO(1:ISTLNB(PLTGEO))
  21 format ('#TITLE Stand=',A,' Year=',I4.4,' ',A/&
       &'#TREEFORM ',A,'.TRF'/&
       &'#FORMAT 2'/A)

  if (IMETRIC.eq.0) then
     write(NOUT,"('#UNITS ENGLISH')")
  else
     write(NOUT,"('#UNITS METRIC')")
  end if

  WRITE(NOUT,"(';                  trcl  stus             fang'/    &
       &  ';species        tr#  |crcl|   dbh   ht lang |edia crd',        &
       &  '  cr    crd  cr    crd  cr    crd  cr ex mk  xloc    yloc',    &
       &  '  z')")

  !     OUTPUT SUBPLOT BOUNDARIES, IF DESIRED.

  if (IDPLOTS.eq.1) then

     !       WRITE THE OUTSIDE BOUNDARY.

     if (IMETRIC.eq.0) then
        if (IPLGEM.lt.2) then
           write (NOUT,'(''#RECTANGLE 0 0 208.71 208.71'')')
        else
           write (NOUT,'(''#CIRCLE 117.75 117.75 117.75'')')
        end if
     else
        if (IPLGEM.lt.2) then
           write (NOUT,'(''#RECTANGLE 0 0 100.00 100.00'')')
        else
           write (NOUT,'(''#CIRCLE 56.42 56.42 56.42'')')
        end if
     end if
     if (ISVINV.gt.1) then
        if (IPLGEM.eq.1) then
           do I = 1,ISVINV
              X1 = X1R1S(I)
              Y2 = Y2A2S(I)

              if (IMETRIC.eq.0) then
                 XM1 = 208.7103
              else
                 XM1 = 100.0
              end if

              if (X1.gt.0.and. X1.lt. XM1) then
                 Y1 = Y1A1S(I)
                 write (NOUT,23) X1,Y1,X1,Y2
                 23 format ('#LINE ',4F8.2)
              end if
              if (Y2.gt.0.and. Y2.lt. XM1) then
                 X2 = X2R2S(I)
                 write (NOUT,23) X1,Y2,X2,Y2
              end if
           end do
        else if (IPLGEM.eq.3) then
           RAD = 0.
           IX = 0
           do I = 1,ISVINV
              if (X2R2S(I).gt.RAD) then
                 if (IX.gt.1) write (NOUT,23) X1,Y1,X2,Y2
                 IX = 1
                 RAD = X2R2S(I)

                 if (IMETRIC.eq.0) then
                    XM1 = 117.75
                    if (RAD.lt.XM1) write (NOUT,24) RAD
                    24 format ('#CIRCLE 117.75 117.75',F8.2)
                 else
                    XM1 = 56.42
                    if (RAD.lt.XM1) write (NOUT,241) RAD
                    241 format ('#CIRCLE 56.42 56.42',F8.2)
                 end if

              else
                 IX = IX+1
              end if
              if (IX.gt.1) write (NOUT,23) X1,Y1,X2,Y2
              X1 = COS(Y1A1S(I))*X1R1S(I) + XM1
              Y1 = SIN(Y1A1S(I))*X1R1S(I) + XM1
              X2 = COS(Y1A1S(I))*X2R2S(I) + XM1
              Y2 = SIN(Y1A1S(I))*X2R2S(I) + XM1
           end do
           if (IX.gt.1) write (NOUT,23) X1,Y1,X2,Y2
        end if
     end if
  end if

  !     OUTPUT THE GROUND FILE

  if (IGRID.gt.0) call SVGRND(NOUT,KYLAST,KYFRST,IFMCLFG)

  !     OUTPUT THE FIRE LINE DATA

  if (IFMCLFG.eq.4) call FMSVFL (NOUT)

  !     OUTPUT RANGE POLES, IF DESIRED.

  if (IRPOLES.eq.1) then

     if (IMETRIC.eq.0) then
        I = 100
        J = 30 ! range pole 30 ft
        XM1 = 208.71
        XM2 = 117.75
        XM3 = 235.50
     else
        I = 50
        J = 5 ! range pole 5 m
        XM1 = 100.0
        XM2 = 56.42
        XM3 = 112.84
     end if

     if (IPLGEM.lt.2) then

        !         SQUARE PLOTS

        write (NOUT,25) J,I,0.00,0.00
        25 format ('RANGEPOLE',T16,&
             &'0 0 0 1 ',I3,I4,' 0 0 0 0 0 0 0 0 0 0 0 1 0 ',&
             &2F8.2,' 0')
        write (NOUT,25) J, I, 0.00,  XM1
        write (NOUT,25) J, I,  XM1, 0.00
        write (NOUT,25) J, I,  XM1,  XM1
     else

        !         ROUND PLOTS

        write (NOUT,25) J, I,  XM2,  XM3
        write (NOUT,25) J, I,  XM2, 0.00
        write (NOUT,25) J, I,  XM3,  XM2
        write (NOUT,25) J, I, 0.00,  XM2
     end if
  end if

  26 continue ! BRANCH HERE WHEN SKIPPING OUTPUT.

  !     IF THE FFE IS ACTIVE, CALL SVCWD TO GENERATE CWD OBJECTS

  if ( LFMON ) call SVCWD(IYEAR)

  if (NSVOBJ.eq.0) goto 50

  do ISVOBJ = 1,NSVOBJ
     I = IS2F(ISVOBJ)
     if (I.gt.0) then
        IPS = 1
        IDIR = 0
        ITC = 0
        if (IOBJTP(ISVOBJ).eq.1.or. IOBJTP(ISVOBJ).eq.3) then

           !           TO HANDLE IOBJTP=3 OBJECTS, OR OBJECTS WHICH WILL BE
           !           REMOVED AT THE END OF THIS CYCLE

           if (IOBJTP(ISVOBJ) .eq.3) then
              IPS = 0
              call SVRANN(X)
              IDIR = IFIX(360. * X +.5)
           end if

           !              GET CROWN WIDTH.

           CW = CRWDTH(I)
           CRAD = CW/2.

           !           LOAD THE SPECIES CODE

           SPCD = ' '
           if (NSP(ISP(I),1)(1:1).eq.' ') then
              SPCD(2:2) = ' '
              SPCD(1:1) = NSP(ISP(I),1)(2:2)
           else
              SPCD(1:2) = NSP(ISP(I),1)(1:2)
           end if

           !           ICR CAN BE NEGATIVE WHEN SVOUT IS CALLED DURING A
           !           FIRE OR BUG MODEL CALL.

           XICR = ABS(ICR(I))*.01

           if (JSVOUT.lt.0) goto 31 ! PROCESSING IMAGE, BUT NOT OUTPUTING
           if (IMETRIC.eq.0) then
              write (NOUT,30) SPCD,I,ITC,0,IPS,DBH(I),HT(I),0,IDIR,&
                   &0,CRAD,XICR,CRAD,XICR,CRAD,XICR,CRAD,XICR,&
                   &1,0,XSLOC(ISVOBJ),YSLOC(ISVOBJ),0
           else
              write (NOUT,30) SPCD,I,ITC,0,IPS,&
                   &DBH(I)*INtoCM,HT(I)*FTtoM,0,IDIR,&
                   &0,CRAD*FTtoM,XICR,CRAD*FTtoM,XICR,CRAD*FTtoM,&
                   &XICR,CRAD*FTtoM,XICR,&
                   &1,0, XSLOC(ISVOBJ),YSLOC(ISVOBJ),0
           end if
           30 format (A,T16,I5,I3,2I2,F6.1,F6.0,I2,I4,I2,&
                &4(F6.1,1X,F4.2),2I2,2F8.2,I2)
           31 continue

        else if (IOBJTP(ISVOBJ).eq.2.or. IOBJTP(ISVOBJ).eq.5) then

           !           THE OBJECT IS A SNAG

           SPCD = ' '
           if (NSP(ISNSP(I),1)(1:1).eq.' ') then
              SPCD(2:2) = ' '
              SPCD(1:1) = NSP(ISNSP(I),1)(2:2)
           else
              SPCD(1:2) = NSP(ISNSP(I),1)(1:2)
           end if

           !           If snag was flagged for salvage in SVSALV (IOBJTP=5):
           !              1) Fell it now for display in the post thin/salvage display.
           !              2) Update "remaining snag" count for cohort snags.
           !              3) Snag will be removed at the bottom of SVOUT.

           if (IOBJTP(ISVOBJ) .eq.5) then
              call SVRANN(X)
              FALLDIR(IS2F(ISVOBJ)) = IFIX(360. * X +.5)
              XPROB = 0.0
              do K = 1,MXDEAD
                 if ( ISTATUS(K) .ne.0 ) then
                    if ( OIDTRE(K)  .eq. OIDTRE(IS2F(ISVOBJ)) .and.&
                         &IYRCOD(K)  .eq. IYRCOD(IS2F(ISVOBJ)) .and.&
                         &FALLDIR(K) .eq. -1 ) XPROB = XPROB+1
                 end if
              end do
              do K = 1,MXDEAD
                 if ( ISTATUS(K) .ne.0 ) then
                    if ( OIDTRE(K) .eq. OIDTRE(IS2F(ISVOBJ)) .and.&
                         &IYRCOD(K) .eq. IYRCOD(IS2F(ISVOBJ)) ) then
                       SPROBS(K,2) = XPROB
                    end if
                 end if
              end do
           end if

           call SVSNAGE(IYEAR,IS2F(ISVOBJ),SNCRDI,SNCRTO,&
                &SNHT,SNDI)

           !           Keep snags with diameter less than 1", for better
           !           agreement with FFE logic.

           if ( SNHT .le.0.) then
              if (ISTATUS(IS2F(ISVOBJ)).gt.0) NDEAD = NDEAD-1
              if (DEBUG) then
                 write (JOSTND,1050) IS2F(ISVOBJ), SNHT, NDEAD
                 1050 format (' ',T5,'Dropping snag:',I5,' with SNHT=',F4.1,&
                      &'. NDEAD=',I5)
              end if
              ISTATUS(IS2F(ISVOBJ)) = 0
              IOBJTP(ISVOBJ) = 0
              goto 40
           end if
           CRAD = SNCRDI/2
           XICR = SNCRTO*.01

           if (FALLDIR(IS2F(ISVOBJ)).eq.-1) then

              !             STANDING DEAD TREES....

              if (ISTATUS(IS2F(ISVOBJ)) .eq.1) then

                 !               THE PROGRAM SHOULD NEVER BE IN THIS CASE
                 !               IT IS USED AS A CHECK

                 ITC = 0
                 IDIR = 0
                 IPS = 1
              else if (ISTATUS(IS2F(ISVOBJ)) .eq.2) then
                 ITC = 98
                 IDIR = 0
                 IPS = 1
              else if (ISTATUS(IS2F(ISVOBJ)) .eq.3) then
                 ITC = 94
                 IDIR = 0
                 IPS = 1
              else if (ISTATUS(IS2F(ISVOBJ)) .eq.4) then
                 ITC = 94
                 IDIR = 0
                 IPS = 1
              else if (ISTATUS(IS2F(ISVOBJ)) .eq.5) then
                 ITC = 97
                 IDIR = 0
                 IPS = 1
              else if (ISTATUS(IS2F(ISVOBJ)) .eq.6) then
                 ITC = 96
                 IDIR = 0
                 IPS = 1
              else if (ISTATUS(IS2F(ISVOBJ)) .eq.90) then
                 ITC = 90
                 IDIR = 0
                 IPS = 1
              else if (ISTATUS(IS2F(ISVOBJ)) .eq.91) then
                 ITC = 91
                 IDIR = 0
                 IPS = 1
              else if (ISTATUS(IS2F(ISVOBJ)) .eq.92) then
                 ITC = 92
                 IDIR = 0
                 IPS = 1
              end if
           else

              !             Fallen/salvaged snags.
              !             Test absolute value of status code, since salvaged snags
              !             have had their status code multiplied by -1.

              if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.1) then
                 ITC = 0
                 IPS = 0
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              else if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.2) then
                 ITC = 98
                 IPS = 0
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              else if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.3) then
                 ITC = 94
                 IPS = 0
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              else if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.4) then
                 ITC = 94
                 IPS = 3
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              else if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.5) then
                 ITC = 97
                 IPS = 0
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              else if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.6) then
                 ITC = 96
                 IPS = 0
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              else if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.90) then
                 ITC = 90
                 IPS = 0
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              else if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.91) then
                 ITC = 91
                 IPS = 0
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              else if (ABS(ISTATUS(IS2F(ISVOBJ))) .eq.92) then
                 ITC = 92
                 IPS = 0
                 IDIR = IFIX(FALLDIR(IS2F(ISVOBJ)))
              end if
           end if

           if ( JSVOUT.lt.0) goto 32 ! PROCESSING IMAGE, BUT NOT OUTPUTING

           if (IMETRIC.eq.0) then
              write (NOUT,30) SPCD,I,ITC,0,IPS,SNDI,SNHT,0,IDIR,&
                   &0,CRAD,XICR,CRAD,XICR,CRAD,XICR,CRAD,XICR,&
                   &1,0,XSLOC(ISVOBJ),YSLOC(ISVOBJ),0
           else
              write (NOUT,30) SPCD,I,ITC,0,IPS, SNDI*INtoCM,&
                   &SNHT*FTtoM,0,IDIR,0,CRAD*FTtoM,XICR,CRAD*FTtoM,&
                   &XICR,CRAD*FTtoM,XICR,CRAD*FTtoM,XICR,1,0,&
                   &XSLOC(ISVOBJ),YSLOC(ISVOBJ),0
           end if
           if (ISTATUS(IS2F(ISVOBJ)) .eq.0) IOBJTP(ISVOBJ) = 0

           !              DRAW FLAME OBJECTS FOR THIS TREE

           if (IFMCLFG.eq.4.and. ITC.eq.97.and. IDIR.eq.0)&
                &call FMSVTREE (NOUT,ISVOBJ)

           !              TO HANDLE IOBJTP=3 OBJECTS, OR OBJECTS WHICH WILL B
           !              REMOVED AT THE END OF THIS CYCLE

           32 continue ! BRANCH HERE IF NOT OUTPUTTING

        else if (IOBJTP(ISVOBJ) .eq.4) then

           !  The object is CWD:

           !  For now, use the species code SNAG2. This will cause the CWD
           !  to be tallied in the SVS tree/snag counts.
           !  Special objects have Plant Type=15 in the SVS tree definition file,
           !  so that they don't contribute to tree counts:
           !     CAR, CRANEBOOM, CRANETOWER, CUBE, MARKER, RANGEPOLE, ROCK,
           !     TETRAHEDRON, TRUCK
           !  But special objects can't be depicted in a felled orientation.

           ITC = 0
           IPS = 3
           SNDI = CWDDIA(IS2F(ISVOBJ))
           SNHT = CWDLEN(IS2F(ISVOBJ))
           IDIR = IFIX(CWDDIR(IS2F(ISVOBJ)))
           CRAD = 0
           XICR = 0

           !  Generate SVS records for the CWD objects.
           !  SVS output vars are:
           !     SPCD: species code
           !     I:    plant ID
           !     ITC:  tree class/growth form
           !     0:    crown class
           !     IPS:  plant status:
           !       0 or 10: plant is cut, has branches, is lying on the ground
           !       1 or 11: plant is standing and has branches
           !       2 or 12: plant is no longer present and only a stump remains
           !       3 or 13: plant is cut, has no branches, is lying on the ground
           !       Plants with status codes of 0, 1, 2, or 3 can have their status
           !       codes modified using the marking and treatment features of SVS.
           !       Plants with status codes of 10, 11, 12, or 13 cannot have their
           !       status codes modified within SVS.
           !     SNDI: dbh
           !     SNHT: ht
           !     0:    lean angle
           !     IDIR: felling angle (degrees)
           !     0:    small end diam
           !     CRAD: crown radius 1 (at 0 degrees)
           !     XICR: crown ratio 1
           !     CRAD: crown radius 2 (at 90 degrees)
           !     XICR: crown ratio 2
           !     CRAD: crown radius 3 (at 180 degrees)
           !     XICR: crown ratio 3
           !     CRAD: crown radius 4 (at 270 degrees)
           !     XICR: crown ratio 4
           !     1:    expansion factor
           !     0:    marking status A
           !     XSLOC(ISVOBJ): x location
           !     YSLOC(ISVOBJ): y location
           !     0: elevation

           if ( JSVOUT.lt.0) goto 33 ! PROCESSING IMAGE, BUT NOT OUTPUTING

           if (IMETRIC.eq.0) then
              write (NOUT,1060) I, ITC, 0, IPS, SNDI, SNHT,&
                   &0, IDIR, 0,&
                   &CRAD, XICR, CRAD, XICR,&
                   &CRAD, XICR, CRAD, XICR,&
                   &1, 0, XSLOC(ISVOBJ), YSLOC(ISVOBJ), 0
           else
              SNDI = SNDI * INtoCM
              SNHT = SNHT * FTtoM
              CRAD = CRAD * FTtoM
              write (NOUT,1060) I, ITC, 0, IPS, SNDI, SNHT,&
                   &0, IDIR, 0,&
                   &CRAD, XICR, CRAD, XICR,&
                   &CRAD, XICR, CRAD, XICR,&
                   &1, 0, XSLOC(ISVOBJ), YSLOC(ISVOBJ), 0
              1060 format ('SNAG2',T16,I5,I3,2I2,F6.1,F6.0,&
                   &I2,I4,I2,&
                   &4(F6.1,1X,F4.2),&
                   &2I2,2F8.2,I2)
           end if
           33 continue
        end if
     end if
     40 continue
  end do
  ILYEAR = IYEAR

  !     CLEAN UP THE SVS DATA AFTER A DISPLAY.

  IPUT = 0
  do ISVOBJ = 1,NSVOBJ
     if (IOBJTP(ISVOBJ).le.0.or. IS2F(ISVOBJ).eq.0&
          &.or. IOBJTP(ISVOBJ).eq.3.or. IOBJTP(ISVOBJ).eq.5) then
        if (IPUT.eq.0) IPUT = ISVOBJ
     else
        if (IPUT.gt.0.and. IPUT.lt.ISVOBJ) then
           IS2F(IPUT) = IS2F(ISVOBJ)
           XSLOC(IPUT) = XSLOC(ISVOBJ)
           YSLOC(IPUT) = YSLOC(ISVOBJ)
           IOBJTP(IPUT) = IOBJTP(ISVOBJ)
           IOBJTP(ISVOBJ) = 0
           IPUT = IPUT+1
        end if
     end if
  end do
  if (IPUT.gt.0) NSVOBJ = IPUT-1

  !  Compress the base FVS snag arrays for:
  !     1) Salvaged snags (ISTATUS is negative)
  !     2) Snags whose dia or ht have dropped below the minimum (ISTATUS=0).
  !  Need to update any associated SVS object pointers at the same time.

  if (DEBUG) then
     write (JOSTND,1070) NDEAD
     1070 format (' ',T5,'BEFORE COMPRESS, NDEAD=',I4)
  end if

  IPUT = 0
  do ISNAG = 1,MXDEAD
     if (ISTATUS(ISNAG).le.0) then
        if (IPUT.eq.0) IPUT = ISNAG
     else
        if (IPUT.gt.0.and. IPUT.lt.ISNAG) then
           ISTATUS(IPUT) = ISTATUS(ISNAG)
           ISNSP(IPUT) = ISNSP(ISNAG)
           CRNDIA(IPUT) = CRNDIA(ISNAG)
           CRNRTO(IPUT) = CRNRTO(ISNAG)
           OLEN(IPUT) = OLEN(ISNAG)
           ODIA(IPUT) = ODIA(ISNAG)
           FALLDIR(IPUT) = FALLDIR(ISNAG)
           IYRCOD(IPUT) = IYRCOD(ISNAG)
           OIDTRE(IPUT) = OIDTRE(ISNAG)
           PBFALL(IPUT) = PBFALL(ISNAG)
           SPROBS(IPUT,1) = SPROBS(ISNAG,1)
           SPROBS(IPUT,2) = SPROBS(ISNAG,2)
           SPROBS(IPUT,3) = SPROBS(ISNAG,3)
           SNGDIA(IPUT) = SNGDIA(ISNAG)
           SNGLEN(IPUT) = SNGLEN(ISNAG)
           SNGCNWT(IPUT,0:3) = SNGCNWT(ISNAG,0:3)
           ISTATUS(ISNAG) = 0
           SPROBS(ISNAG,1) = 0
           SPROBS(ISNAG,2) = 0
           SPROBS(ISNAG,3) = 0
           do J = 1,NSVOBJ
              if (IOBJTP(J).eq.2.and. IS2F(J).eq.ISNAG) then
                 IS2F(J) = IPUT
                 exit
              end if
           end do
           IPUT = IPUT+1
        end if
     end if
  end do
  if (IPUT.gt.0) NDEAD = IPUT-1

  if (DEBUG) then
     write (JOSTND,1075) NDEAD
     1075 format (' ',T5,'AFTER COMPRESS, NDEAD=',I4)
  end if

  if (DEBUG) then
     write (JOSTND,1080) ICYC, IYEAR, ILYEAR,&
          &NDEAD, NSVOBJ
     1080 format (' ','Leaving SVOUT, ICYC=',I2,&
          &', IYEAR=',I4,', ILYEAR=',I4,':', / ,&
          &' ',T5,'NDEAD=',I4,', NSVOBJ=',I6,//,&
          &' ',T5,'SNAG LIST:',//,&
          &' ',T5,'  I   IDTREE  SPP ODIA OLEN IYRCOD STATUS ',&
          &'FALLDIR OPROB STDNG',/,&
          &' ',T5,'---- -------- --- ---- ---- ------ ------ ',&
          &'------- ----- -----' )
     !                       XXXX XXXXXXXX XXX XX.X XXX. XXXXXX XXXXXX
     !                       XXXXXX. XXXX. XXXX.
     do I = 1,NDEAD
        write(JOSTND,1010) I, OIDTRE(I), ISNSP(I), ODIA(I), OLEN(I),&
             &IYRCOD(I), ISTATUS(I),&
             &FALLDIR(I), SPROBS(I,1), SPROBS(I,2)
     end do

     NSTNDNG = 0.0
     EXPSNGS = 0.0
     do I = 1,NDEAD
        if (ISTATUS(I).gt.0.and. IYRCOD(I).le.IYEAR ) then
           if (FALLDIR(I).eq.-1) NSTNDNG = NSTNDNG+1
           EXPSNGS = EXPSNGS + SPROBS(I,3)
        end if
     end do

     NFFESNGS = 0.0
     do I = 1,NSNAG
        NFFESNGS = NFFESNGS + DENIH(I) + DENIS(I)
     end do
     write (JOSTND,1090) ICYC, IYEAR,&
          &NDEAD, NSTNDNG, NFFESNGS, EXPSNGS
     1090 format (' ','IN SVOUT, ICYC=',I2,', IYEAR=',I4,':', / ,&
          &' ',T5,'NDEAD=',I4,', NSTNDNG=',F5.0,', NFFESNGS=',F5.0,&
          &', EXPSNGS=',F5.0)
  end if

  IX = 0
  do ISVOBJ = 1,NSVOBJ
     if (IOBJTP(ISVOBJ).eq.2) IX = IX+1
  end do
  if (DEBUG) write (JOSTND,*) 'SVOUTNDEAD=',NDEAD,&
       &'DEADOBJECTS=',IX

  50 continue

  !     CLOSE THE SVS TREE DATA FILE FOR THIS IMAGE (NO CLOSE
  !     IS DONE IF THE FILE IS A SINGLE LONG FILE).

  if (NOUT.eq.JSVPIC) close (JSVPIC)

  if (DEBUG) write (JOSTND,60) NSVOBJ,JSVOUT,JSVPIC
  60 format (' IN SVOUT: END. NSVOBJ=',I6,' JSVOUT=',I4,&
       &' JSVPIC=',I4)

  !     QUICK CHECKS/DEBUG

  if (DEBUG .and. IFMCLFG.eq.3) call SVCDBH(WK3,0)

  !     END OF QUICK CHECK

  return
end
