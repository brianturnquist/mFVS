!====================!
!   sn : sitsetOld   !
!====================!
! SN $Id$
subroutine SITSET
  implicit none

  !  THIS SUBROUTINE LOADS THE SITEAR ARRAY WITH A SITE INDEX FOR EACH
  !  SPECIES WHICH WAS NOT ASSIGNED A SITE INDEX BY KEYWORD. LOGIC
  !  DEVELOPED BY DONNELLY, FMSC. WRITES FIA SPECIES CODE/FVS SPECIES
  !  CODE TRANSLATION TABLE
  !  THIS SUBROUTINE FOLLOWS SE-TWIGS SUBROUTINES BLOCK1, CONV1, AND
  !  CONV2.

  ! Note: This version of sitset.f has been replaced Q2 2023 to update
  !       Merchantability specifications for National Forests in NC,
  !       per FSH 2409.18 Ch 50 Supplement R8_NC_2409.18-2022-1
  !       ** DELETE THIS FILE ONCE ALL TESTING HAS BEEN COMPLETE **

  ! Common blocks
  include 'PRGPRM.f90'
  include 'COEFFS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'VARCOM.f90'
  include 'VOLSTD.f90'
  include 'SNCOM.f90'

  ! Variables
  integer :: IFIASP,ERRFLAG
  integer :: I,IMAPSP,IGRP,IMGSP,IXTMP
  integer :: IREGN,IFORST,INTDIST,ISPC,J
  real    :: RSISP,A,B,C,D,XTMP
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=11) :: VOLEQ
  logical :: DEBUG
  real    :: SIMAX(MAXSP),SIMIN(MAXSP),SDICON(MAXSP)
  integer :: ISNSIS(43),ISNGRP(43),MAPSI(MAXSP),MGSISP(9)
  real    :: MGRSI(9),MGSI,MGSPIX,MGSION

  !  LOAD SDI MAXIMUM VALUES

  data SDICON /&
       &655., 354., 412., 499., 490., 385., 490., 332., 398., 398.,&
       &310., 529., 480., 499., 692., 623., 518., 371., 344., 421.,&
       &590., 371., 371., 400., 350., 375., 276., 492., 420., 422.,&
       &257., 147., 364., 414., 408., 423., 414., 338., 492., 430.,&
       &155., 283., 283., 430., 478., 492., 415., 492., 492., 492.,&
       &422., 277., 726., 430., 704., 304., 164., 492., 499., 648.,&
       &520., 384., 361., 315., 342., 405., 326., 387., 384., 326.,&
       &417., 336., 365., 417., 414., 342., 311., 370., 410., 343.,&
       &447., 492., 526., 282., 263., 282., 227., 354., 492., 421./

  !  THIS ARRAY CONTAINS OFFICIAL SITE SPECIES USED IN FIA DATA AND
  !  IS CONTAINED IN THE SN SPECIES LIST

  data ISNSIS/&
       &5,   6,  11,  17,  64,  35,  47,  75,  78,  15,&
       &16,  44,  59,  76,  45,   8,  12,  13,   2,   1,&
       &3,   7,   4,  14,  34,  61,  65,  87,  74,  10,&
       &20,  22,  24,  25,  33,  60,  62,  63,  66,  69,&
       &71,  73,  83/

  !  THIS ARRAY CONTAINS A MAP FROM THE OFFICIAL SITE SPECIES TO AN
  !  INDEX GROUP.

  data ISNGRP/&
       &1,   1,   1,   1,   2,   2,   2,   2,   2,   3,&
       &3,   3,   3,   3,   3,   4,   4,   4,   5,   5,&
       &5,   5,   5,   5,   6,   6,   6,   6,   7,   8,&
       &9,   9,   9,   9,   9,   9,   9,   9,   9,   9,&
       &9,   9,   9/

  !  THIS ARRAY CONTAINS A MAP TO ESTIMATE INDIVIDUAL SITE INDICIES.

  data MAPSI/&
       &5,   5,   5,   5,   1,   1,   5,   4,   9,   8,&
       &1,   4,   4,   5,   3,   3,   1,   9,   9,   9,&
       &9,   9,   9,   9,   9,   9,   9,   9,   9,   9,&
       &9,   9,   9,   6,   2,   9,   9,   9,   9,   9,&
       &9,   9,   9,   3,   3,   9,   2,   9,   9,   9,&
       &9,   9,   9,   9,   9,   9,   9,   9,   3,   9,&
       &6,   9,   9,   2,   6,   9,   9,   9,   9,   9,&
       &9,   9,   9,   7,   2,   3,   9,   2,   9,   9,&
       &9,   9,   9,   9,   9,   9,   6,   5,   9,   9/

  !  ARRAY TO IDENTIFY THE "MASTER GROUP" SPECIES WHICH HAVE PUBLISHED
  !  REFERENCES TO ALLOW DIRECT TRANSFORMATION OF SITE INDEX BETWEEN
  !  SPECIES (DOOLITTLE 1958).  SITE INDEX COMPARISONS...SOUTHERN
  !  APPALACHIANS, SOIL SCIENCE OF AMERICA PROC. 22(5):455-458;
  !  USDA FS(R8) 1992.  SILVICULTURAL...PRESCRIPTION FIELD BOOK, P.19)

  !--MASTER GROUP NO.1   2   3   4   5   6   7   8   9
  data MGSISP /5, 64, 45, 12, 14, 65, 74, 10, 63/

  !  ARRAYS CONTAINING MAX AND MIN SITE INDEX BY SPECIES AND
  !  MODIFIED FOR USE IN HEIGHT GROWTH SITE INDEX SCALING.
  !  THIS PROCESS IS ESPECIALLY IMPORTANT FOR THE SHORTER TREE SPECIES.

  data SIMIN/&
       &15,   15,   15,   35,   35,   35,   45,   45,   35,   25,&
       &35,   40,   40,   35,   30,   30,   35,   35,   35,   35,&
       &30,   35,   25,   35,   35,   15,   25,   30,   15,   15,&
       &15,   15,   35,   35,   35,   35,   35,   25,   15,   15,&
       &35,   35,   35,   30,   30,   35,   25,   35,   15,   35,&
       &15,   15,   30,   35,   35,   15,   15,   15,   30,   40,&
       &30,   35,   25,   25,   25,   30,   25,   25,   35,   25,&
       &35,   35,   30,   25,   25,   15,   25,   25,   30,   25,&
       &15,   15,   35,   35,   35,   35,   35,   15,   15,   15/

  data SIMAX/&
       &100,   70,   80,  100,  105,  105,   90,  125,   70,   95,&
       &105,  135,  125,   95,  120,  120,   90,   70,   70,   85,&
       &105,  100,   90,   85,   70,   40,   85,   90,   90,   40,&
       &45,   70,   85,  105,   95,   85,  105,  120,   50,   65,&
       &70,   85,   85,  125,  135,  125,  115,  125,   75,  125,&
       &40,   55,  105,  105,   95,   40,   70,   60,  120,  125,&
       &90,  105,  115,  115,  115,  125,   65,   65,   95,   65,&
       &95,   75,  115,  115,  115,  125,   85,  115,   65,   95,&
       &110,   80,   90,   90,   90,   90,   90,   55,   55,   55/

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'SITSET',6,ICYC)

  !  SET DEFAULT SITE INDEX.
  !  IF SITE INDEX SPECIES IS BLANK, USE WHITE OAK.  IF NOT BLANK, CHECK
  !  TO SEE IF IT IS A RECOGNIZED SN SITE SPECIES.  IF NOT, SET TO WHITE OAK.
  !  FVS SEQUENCE No. 63 AND SITE INDEX OF 70

  if (ISISP .le.0) ISISP = 63
  do I = 1,43
     if (ISISP .eq. ISNSIS(I)) then
        IMAPSP = I
        go to 101
     end if
     100 continue
  end do
  ISISP = 63
  IMAPSP = 38
  101 continue
  if (SITEAR(ISISP) .le.0.0) SITEAR(ISISP) = 70.

  !  BOUND INPUT SITE INDEX HERE
  !  (VALUES FROM DONNELLY, 02/2000)

  if (SITEAR(ISISP) .le. SIMIN(ISISP)) then
     SITEAR(ISISP) = SIMIN(ISISP)
     write (JOSTND,110) JSP(ISISP), SITEAR(ISISP)
  end if
  if (SITEAR(ISISP) .ge. SIMAX(ISISP)) then
     SITEAR(ISISP) = SIMAX(ISISP)
     write (JOSTND,110) JSP(ISISP), SITEAR(ISISP)
  end if
  110 FORMAT('***** WARNING - THE SITE SPECIES (',A4,') SITE INDEX VALU &
       &E WAS OUTSIDE OF THE ALLOWABLE RANGE, THE VALUE USED WAS ',F5.1)

  !  COMPUTE RELATIVE SITE INDEX FOR SITE SPECIES

  RSISP = (SITEAR(ISISP)-SIMIN(ISISP))/(SIMAX(ISISP)-SIMIN(ISISP))

  !  FOR MASTER GROUP SPECIES, FIND INDEX EQUATION COEFFICIENTS FOR
  !  X-AXIS VALUE CORRESPONDING TO SITE SPECIES SI.  FIRST, FIND
  !  MASTER GROUP NUMBER OF SITE INDEX SPECIES.  THEN, DETERMINE
  !  COEFFICIENTS TO BE USED IN EQUATION TO COMPUTE TRANSFORMATION
  !  INDEX.

  IGRP = ISNGRP(IMAPSP)
  if (IGRP .eq.1) then
     if (PCOM(1:1) .eq. 'M') then

        !   MOUNTAIN

        A = -7.1837
        B = 0.1633
     else

        !   OTHER

        A = -10.0
        B = 0.2
     end if
  else if (IGRP.eq.2) then
     if (PCOM(1:1) .eq. 'M') then
        A = -8.6809
        B = 0.1702
     else
        A = -12.0
        B = 0.2
        if (ISISP .eq.78) then
           A = -16.0
           B = 0.2667
        end if
     end if
  else if (IGRP.eq.3) then
     A = -4.0
     B = 0.1
  else if (IGRP.eq.4) then
     A = -9.4118
     B = 0.1569
  else if (IGRP.eq.5) then
     A = -9.3913
     B = 0.1739
  else if (IGRP.eq.6) then
     A = -10.0
     B = 0.2
  else if (IGRP .eq.7) then
     A = -8.6809
     B = 0.1702
  else if (IGRP.eq.8) then
     A = -7.1837
     B = 0.1633
  else if (IGRP.eq.9) then
     if (PCOM(1:1) .eq. 'M') then
        A = -8.7442
        B = 0.186
     else
        A = -10.0
        B = 0.2
     end if
  end if

  !  USING A, B COEFFS FROM ABOVE, COMPUTE SITE TRANSFORM INDEX:
  !  FIRST, FIND MASTER GROUP SPECIES FROM MASTER GROUP SITE INDEX ARRAY,
  !         GIVEN THE GROUP NUMBER OF THE ACTUAL SITE INDEX SPECIES
  !  SECOND, COMPUTE MASTER GROUP SPECIES OWN SITE INDEX
  !  THIRD, GIVEN MASTER GROUP SPECIES OWN SITE INDEX, COMPUTE MASTER
  !         GROUP SITE SPECIES TRANSFORM INDEX

  IMGSP = MGSISP(IGRP)
  MGSION = RSISP * ( SIMAX(IMGSP) - SIMIN(IMGSP) ) + SIMIN(IMGSP)
  MGSPIX = A + B*MGSION

  !    FIRST, FIND THE COEFFICIENTS NEEDED TO ESTIMATE THE SITE INDEX FOR
  !    EACH OF THE MASTER GROUP SPECIES.  THEN, USING THE COEFFS C AND D,
  !    COMPUTE THE SITE INDEX FOR EACH MASTER GROUP SPECIES (MGSI).
  !    THEN, COMPUTE THE RELATIVE SITE INDEX FOR EACH MASTER GROUP
  !    SPECIES (MGRSI) BASED ON ITS SITE INDEX POSITION IN ITS RANGE
  !    FROM ITS SIMIN TO ITS SIMAX.

  do I = 1, 9
     if (I.eq.1) then
        if (PCOM(1:1) .eq. 'M') then
           C = 44.
           D = 6.13
        else
           C = 50.
           D = 5.
        end if
     else if (I.eq.2) then
        if (PCOM(1:1) .eq. 'M') then
           C = 51.
           D = 5.88
        else
           C = 60.
           D = 5.
           if (ISISP .eq.78) then
              C = 60.
              D = 3.75
           end if
        end if
     else if (I.eq.3) then
        C = 40.
        D = 10.
     else if (I.eq.4) then
        C = 60.
        D = 6.38
     else if (I.eq.5) then
        C = 54.
        D = 5.75
     else if (I.eq.6) then
        C = 50.
        D = 5.
     else if (I.eq.7) then
        C = 51.
        D = 5.88
     else if (I .eq.8) then
        C = 44.
        D = 6.13
     else if ( I .eq.9.) then
        if (PCOM(1:1) .eq. 'M') then
           C = 47.
           D = 5.38
        else
           C = 50.
           D = 5.
        end if
     end if
     MGSI = C + D * MGSPIX
     MGRSI(I) = ( MGSI - SIMIN(MGSISP(I))) /&
          &( SIMAX(MGSISP(I)) - SIMIN(MGSISP(I)))
     130 continue
  end do

  !    GIVEN THE TRANSFORMED RELATIVE SITE INDEX VALUES FOR EACH OF THE
  !    MASTER GROUP SPECIES, COMPUTE AN ESTIMATED SITE INDEX VALUE FOR
  !    EACH SPECIES POSSIBLE IN THE VARIANT.  EACH SPECIES IS
  !    ASSOCIATED WITH ONE OF THE MASTER GROUP SPECIES.  THE RELATIVE
  !    SITE INDEX THEN SCALES THE POTENTIAL RANGE OF SITE INDEX VALUES
  !    USED IN THE HEIGHT GROWTH EQUATION (CARMEAN, NC-128) ASSIGNED
  !    TO EACH SPECIES.  ALSO, EACH SITE INDEX VALUE IS CHECKED TO BE
  !    SURE IT IS WITHIN MIN AND MAX BOUNDS.

  do I = 1, MAXSP
     if ( SITEAR(I) .eq.0. ) then
        SITEAR(I) = MGRSI(MAPSI(I))*(SIMAX(I) - SIMIN(I)) + SIMIN(I)
        if (DEBUG) write(JOSTND,*)'I, SITEAR= ',I,'  ',SITEAR(I)
        if (SITEAR(I) .le. SIMIN(I)) then
           if (DEBUG) write(JOSTND,155) JSP(I), SITEAR(I),SIMIN(I)
           SITEAR(I) = SIMIN(I)
        end if
        if (SITEAR(I) .ge. SIMAX(I)) then
           if (DEBUG) write(JOSTND,155) JSP(I), SITEAR(I),SIMAX(I)
           SITEAR(I) = SIMAX(I)
        end if
     end if
     155 FORMAT('*** WARNING - THE SITE SPECIES (',A4,f5.1,')SITE INDEX  &
          & WAS OUTSIDE OF THE ALLOWABLE RANGE, THE VALUE USED WAS ',F5.1)
     150 continue
  end do

  !  END OF SITE DUBBING LOGIC

  !  SET SDIDEF VALUES IF NOT SET BY KEYWORD.

  do I = 1,MAXSP
     if (SDIDEF(I) .le.0.) then
        if (BAMAX .gt.0.) then
           SDIDEF(I) = BAMAX/(0.5454154*(PMSDIU/100.))
        else
           SDIDEF(I) = SDICON(I)
        end if
     end if
     15 continue
  end do
  !  START DIGESTION OF FOREST TYPE VARIABLE

  if (IFORTP .gt.999) then

     !  THE LAST 3 CHARACTERS INDICATE THE FOREST TYPE AND THE FIRST
     !  CHARACTER INDICATE THAT THE USER SETS THE FOREST TYPE TO BE
     !  CONSTANT FOR ALL CYCLES.  THE FIELD 3 INPUT IS DECODED
     !  AND LFLAGV IS SET TO TRUE TO INDICATE CONSTANT FOREST TYPE

     XTMP = FLOAT(IFORTP)
     XTMP = XTMP/1000. + 0.00001
     IXTMP = INT(XTMP)
     IFORTP = INT((XTMP-real(IXTMP))*1000.)
     LFLAGV = .true.
  end if

  !  LOAD VOLUME DEFAULT MERCH. SPECS.

  if (ISEFOR.ne.0) then
     IREGN = KODFOR/10000
     IFORST = KODFOR/100-IREGN*100
     write(FORST,'(I2)')IFORST
     if (IFORST.lt.10)FORST(1:1) = '0'
     INTDIST = KODFOR - (KODFOR/100)*100
     write(DIST,'(I2)')INTDIST
     if (INTDIST.lt.10)DIST(1:1) = '0'
  else
     if (KODFOR .gt.1000) then
        IFORST = KODFOR/100-900
     else
        IFORST = KODFOR-900
     end if
     DIST = ' '
     IREGN = 9
     VOLEQ(1:7) = '900CLKE'
     write(FORST,'(I2)')IFORST
     if (IFORST.lt.10)FORST(1:1) = '0'
  end if

  do ISPC = 1,MAXSP
     if (IREGN.eq.8) then
        if (DBHMIN(ISPC).le.0.) then !SET **DBHMIN** DEFAULT
           if ((ISPC.le.17).or.(ISPC.eq.88)) then !SOFTWOODS
              if ((ISPC.eq.7).or.(ISPC.eq.13)) then
                 DBHMIN(ISPC) = 6.
              else
                 DBHMIN(ISPC) = 4.
              end if
           else !HARDWOODS
              if ((ISPC.eq.39).or.(ISPC.eq.43).or.(ISPC.eq.44).or.&
                   &(ISPC.eq.52).or.(ISPC.eq.53).or.(ISPC.eq.55).or.&
                   &(ISPC.eq.63)) then
                 DBHMIN(ISPC) = 6.
              else
                 DBHMIN(ISPC) = 4.
              end if
           end if
        end if
        if (TOPD(ISPC).le.0.)TOPD(ISPC) = 4. !SET **TOPD** DEFAULT
        if (BFMIND(ISPC).le.0.) then !SET **BFMIND** DEFAULT
           if ((ISPC.le.17).or.(ISPC.eq.88)) then !SOFTWOODS
              select case(IFOR)
              case(10)
                 if (ISPC.eq.2) then
                    BFMIND(ISPC) = 9.
                 else
                    BFMIND(ISPC) = 10.
                 end if
              case default
                 BFMIND(ISPC) = 10.
              end select
           else !HARDWOODS
              BFMIND(ISPC) = 12.
           end if
        end if
        if (BFTOPD(ISPC).le.0.) then !SET **BFTOPD** DEFAULT
           if ((ISPC.le.17).or.(JSP(ISPC)(1:2).eq.'OS')) then
              BFTOPD(ISPC) = 7.
           else
              BFTOPD(ISPC) = 9.
           end if
        end if
     else !REGION 9
        if (DBHMIN(ISPC).le.0.) then !SET **DBHMIN** DEFAULT
           if ((ISPC.le.17).or.(ISPC.eq.88)) then !SOFTWOODS
              DBHMIN(ISPC) = 5.
           else !HARDWOODS
              select case(IFOR)
              case(5)
                 DBHMIN(ISPC) = 5.
              case default
                 DBHMIN(ISPC) = 6.
              end select
           end if
        end if
        if (TOPD(ISPC).le.0.) then !SET **TOPD** DEFAULT
           if (ISPC.le.17) then !SOFTWOODS
              TOPD(ISPC) = 4.
           else !HARDWOODS
              select case (IFOR)
              case(8)
                 TOPD(ISPC) = 5.
              case default
                 TOPD(ISPC) = 4.
              end select
           end if
        end if
        if (BFMIND(ISPC).le.0.) then !SET **BFMIND** DEFAULT
           if (ISPC.le.17) then !SOFTWOODS
              select case(IFOR)
              case(5)
                 BFMIND(ISPC) = 9.
                 if (ISPC.eq.2)BFMIND(ISPC) = 6.
              case default
                 BFMIND(ISPC) = 9.
              end select
           else !HARDWOODS
              select case(IFOR)
              case(5)
                 BFMIND(ISPC) = 9.
              case default
                 BFMIND(ISPC) = 11.
              end select
           end if
        end if
        if (BFTOPD(ISPC).le.0.) then !SET **BFTOPD** DEFAULT
           if (ISPC.le.17) then !SOFTWOOD
              select case(IFOR)
              case(5)
                 BFTOPD(ISPC) = 7.6
                 if (ISPC.eq.2)BFTOPD(ISPC) = 5.
              case default
                 BFTOPD(ISPC) = 7.6
              end select
           else !HARDWOODS
              select case(IFOR)
              case(5)
                 BFTOPD(ISPC) = 7.6
              case default
                 BFTOPD(ISPC) = 9.6
              end select
           end if
        end if
     end if
  end do

  !  LOAD VOLUME EQUATION ARRAYS FOR ALL SPECIES

  PROD = '  '
  VAR = 'SN'

  do ISPC = 1,MAXSP
     read(FIAJSP(ISPC),'(I4)')IFIASP
     if (((METHC(ISPC).eq.6).or.(METHC(ISPC).eq.9)).and.&
          &(VEQNNC(ISPC).eq.'           ')) then

        PROD = '02'
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNC(ISPC) = VOLEQ

        if (DEBUG)write(16,*)'VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,',&
             &'ERRFLAG= ',VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG
     end if
     if (((METHB(ISPC).eq.6).or.(METHB(ISPC).eq.9)).and.&
          &(VEQNNB(ISPC).eq.'           ')) then
        PROD = '01'
        call VOLEQDEF(VAR,IREGN,FORST,DIST,IFIASP,PROD,VOLEQ,ERRFLAG)
        VEQNNB(ISPC) = VOLEQ
     end if
  end do

  !  HT-DBH COEFFICIENTS

  HT1(1) = 4.5084
  HT1(2) = 4.0374
  HT1(3) = 4.5084
  HT1(4) = 4.2899
  HT1(5) = 4.6271
  HT1(6) = 4.6561
  HT1(7) = 4.7258
  HT1(8) = 4.5991
  HT1(9) = 4.2139
  HT1(10) = 4.3898
  HT1(11) = 4.5457
  HT1(12) = 4.6090
  HT1(13) = 4.6897
  HT1(14) = 4.4718
  HT1(15) = 4.6171
  HT1(16) = 4.4603
  HT1(17) = 4.5084
  HT1(18) = 4.3164
  HT1(19) = 4.2378
  HT1(20) = 4.3379
  HT1(21) = 4.5991
  HT1(22) = 4.4834
  HT1(23) = 4.5697
  HT1(24) = 4.4388
  HT1(25) = 4.4522
  HT1(26) = 3.8550
  HT1(27) = 4.5128
  HT1(28) = 4.9396
  HT1(29) = 4.4207
  HT1(30) = 3.7512
  HT1(31) = 3.7301
  HT1(32) = 4.4091
  HT1(33) = 4.4772
  HT1(34) = 4.4819
  HT1(35) = 4.5959
  HT1(36) = 4.6155
  HT1(37) = 4.6155
  HT1(38) = 4.3734
  HT1(39) = 4.4009
  HT1(40) = 4.4931
  HT1(41) = 4.0151
  HT1(42) = 4.5018
  HT1(43) = 4.5018
  HT1(44) = 4.5920
  HT1(45) = 4.6892
  HT1(46) = 4.4004
  HT1(47) = 4.6067
  HT1(48) = 4.4004
  HT1(49) = 4.3609
  HT1(50) = 4.4004
  HT1(51) = 3.9678
  HT1(52) = 3.9613
  HT1(53) = 4.4330
  HT1(54) = 4.3802
  HT1(55) = 4.4334
  HT1(56) = 4.0322
  HT1(57) = 4.1352
  HT1(58) = 4.0965
  HT1(59) = 4.6355
  HT1(60) = 4.9396
  HT1(61) = 4.9396
  HT1(62) = 4.3286
  HT1(63) = 4.5463
  HT1(64) = 4.5225
  HT1(65) = 4.5142
  HT1(66) = 4.7342
  HT1(67) = 3.9365
  HT1(68) = 4.4375
  HT1(69) = 4.5710
  HT1(70) = 3.9191
  HT1(71) = 4.6135
  HT1(72) = 4.3420
  HT1(73) = 4.5577
  HT1(74) = 4.4618
  HT1(75) = 4.5202
  HT1(76) = 4.6106
  HT1(77) = 4.2496
  HT1(78) = 4.4747
  HT1(79) = 4.2959
  HT1(80) = 4.4299
  HT1(81) = 4.4911
  HT1(82) = 4.3383
  HT1(83) = 4.5820
  HT1(84) = 4.3744
  HT1(85) = 4.5992
  HT1(86) = 4.6008
  HT1(87) = 4.6238
  HT1(88) = 4.3898
  HT1(89) = 3.9392
  HT1(90) = 3.9089

  HT2(1) = -6.0116
  HT2(2) = -4.2964
  HT2(3) = -6.0116
  HT2(4) = -4.1019
  HT2(5) = -6.4095
  HT2(6) = -6.2258
  HT2(7) = -6.7703
  HT2(8) = -5.9111
  HT2(9) = -4.5419
  HT2(10) = -5.7183
  HT2(11) = -6.8000
  HT2(12) = -6.1896
  HT2(13) = -6.8801
  HT2(14) = -5.0078
  HT2(15) = -6.2684
  HT2(16) = -5.0577
  HT2(17) = -6.0116
  HT2(18) = -4.0582
  HT2(19) = -4.1080
  HT2(20) = -3.8214
  HT2(21) = -6.6706
  HT2(22) = -4.5431
  HT2(23) = -5.7172
  HT2(24) = -4.0872
  HT2(25) = -4.5758
  HT2(26) = -2.6623
  HT2(27) = -4.9918
  HT2(28) = -8.1838
  HT2(29) = -5.1435
  HT2(30) = -2.5539
  HT2(31) = -2.7758
  HT2(32) = -4.8464
  HT2(33) = -4.7206
  HT2(34) = -4.5314
  HT2(35) = -6.4497
  HT2(36) = -6.2945
  HT2(37) = -6.2945
  HT2(38) = -5.3135
  HT2(39) = -5.0560
  HT2(40) = -4.6501
  HT2(41) = -4.3314
  HT2(42) = -5.6123
  HT2(43) = -5.6123
  HT2(44) = -5.1719
  HT2(45) = -4.9605
  HT2(46) = -4.7519
  HT2(47) = -5.2030
  HT2(48) = -4.7519
  HT2(49) = -4.1423
  HT2(50) = -4.7519
  HT2(51) = -3.2510
  HT2(52) = -3.1993
  HT2(53) = -4.5383
  HT2(54) = -4.7903
  HT2(55) = -4.5709
  HT2(56) = -3.0833
  HT2(57) = -3.7450
  HT2(58) = -3.9250
  HT2(59) = -5.2776
  HT2(60) = -8.1838
  HT2(61) = -8.1838
  HT2(62) = -4.0922
  HT2(63) = -5.2287
  HT2(64) = -4.9401
  HT2(65) = -5.2205
  HT2(66) = -6.2674
  HT2(67) = -4.4599
  HT2(68) = -4.6654
  HT2(69) = -6.0922
  HT2(70) = -4.3503
  HT2(71) = -5.7613
  HT2(72) = -5.1193
  HT2(73) = -4.9595
  HT2(74) = -4.8786
  HT2(75) = -4.8896
  HT2(76) = -5.4380
  HT2(77) = -4.8061
  HT2(78) = -4.8698
  HT2(79) = -5.3332
  HT2(80) = -4.9920
  HT2(81) = -5.7928
  HT2(82) = -4.5018
  HT2(83) = -5.0903
  HT2(84) = -4.5257
  HT2(85) = -7.7428
  HT2(86) = -7.2732
  HT2(87) = -7.4847
  HT2(88) = -5.7183
  HT2(89) = -3.4279
  HT2(90) = -3.0149

  !  LOAD FORT BRAGG HT-DBH CONSTANTS

  if (IFOR .eq.20) then
     ITYPE = 176
     PCOM = '232BQ'
     HT1(5) = 4.705
     HT2(5) = -7.904
     HT1(6) = 4.787
     HT2(6) = -8.015
     HT1(8) = 4.562
     HT2(8) = -7.314
     HT1(11) = 4.806
     HT2(11) = -9.573
     HT1(13) = 4.79
     HT2(13) = -8.5
     write(JOSTND,310)
     310 format(/,T12,'ECOLOGICAL UNIT CODE CHANGED TO 232BQ FOR ',&
          &'FURTHER PROCESSING OF FORT BRAGG LOCATION.')
  end if

  !  IF FIA CODES WERE IN INPUT DATA, WRITE TRANSLATION TABLE

  if (LFIA) then
     call FIAHEAD(JOSTND)
     write(JOSTND,311) (NSP(I,1)(1:2),FIAJSP(I),I = 1,MAXSP)
     311 format ((T12,8(A3,'=',A6,:,'; '),A,'=',A6))
  end if

  !  WRITE VOLUME EQUATION NUMBER TABLE

  call VOLEQHEAD(JOSTND)
  write(JOSTND,230)(NSP(J,1)(1:2),VEQNNC(J),VEQNNB(J),J = 1,MAXSP)
  230 format(4(2X,A2,4X,A11,1X,A10,1X))

  return
end
