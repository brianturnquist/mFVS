!=======================!
!   volume : voleqdef   !
!=======================!
  !== last modified  09-15-2016 reconciled Vol. Eq. No. output from FVS with Cruise software-RNH
  ! 01/18/2013 added FIAVOLEQDEF, R5_PNWEQN and R6_PNWEQN for PNE FIA equations.
  ! 03/25/2014 changed default equation for Region 3 (R3_EQN) Ponderosa pine in the forest Apache Sitgreaves, Coconino, Kaibab and Tonto to 300FW2W122.
  ! 09/09/2016 Modified R3_EQN default equation for PP, DF, WF, and WP in Sante Fe NF
  ! 09/15/2016 Corrected R4 default equation for other species to DVE equation
  ! 04/12/2017 removed 532WO2W*** equation from R5_EQN per the email from Craig Bodenhausen (4/12/2017)
  ! 07/19/2021 Changed R8_CEQN to use the R8 new Clark equation 8*1CLKE***. Added 1 to TOPCODE array.
subroutine VOLEQDEF (VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)

  !    SUBROUTINE WILL RETURN THE DEFAULT VOLUME EQUATION NUMBER
  !        SPEC = 3 DIGIT FIA SPECIES CODE
      !DEC$ ATTRIBUTES DLLEXPORT::VOLEQDEF

  ! Variables
  character(len=2) :: FORST
  character(len=2) :: PROD
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=10) :: VOLEQ
  integer :: ERRFLAG,REGN,SPEC

  !  The GETVARIANT routine is required by the NVEL system to find the
  !  FVS variant. When VOLEQDEF is used by FVS, the call to VOLEQDEF always
  !  carries the variant 2 character symbol so the GETVARIANT routine
  !  is not used by FVS.

  if (VAR .eq. '  ') then
     call GETVARIANT(REGN,FORST,DIST,VAR)
  end if
  if (REGN .eq.1) then
     call R1_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  else if (REGN.eq.2) then
     call R2_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
  else if (REGN.eq.3) then
     call R3_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
  else if (REGN.eq.4) then
     call R4_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
  else if (REGN.eq.5) then
     call R5_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  else if (REGN.eq.6) then
     call R6_EQN(VAR,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  else if (REGN.eq.7) then
     call R7_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
     !      ELSE IF((REGN.EQ.8).AND.(PROD.EQ.'02'))THEN
     !         CALL R8_CEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
     !      ELSE IF((REGN.EQ.8).AND.(PROD.EQ.'01'))THEN
     !         CALL R8_BEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
  else if (REGN.eq.8) then
     call R8_CEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
  else if (REGN.eq.9) then
     call R9_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  else if (REGN.eq.10) then
     call R10_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
  end if

  return
end

  !//////////////////////////////////////////////////////////////////
subroutine GETVARIANT(REGN,FORST,DIST,VAR)
  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=7) :: VVER
  integer :: REGN,FORNUM,DISTNUM

  if (FORST(2:2) .lt. '0') then
     FORST(2:2) = FORST(1:1)
     FORST(1:1) = '0'
     if (FORST(2:2) .lt. '0') FORST(2:2) = '0'
  end if
  if (DIST(2:2) .lt. '0') then
     DIST(2:2) = DIST(1:1)
     DIST(1:1) = '0'
     if (DIST(2:2) .lt. '0') DIST(2:2) = '0'
  end if
  read(FORST,'(I2)')FORNUM
  read(DIST,'(I2)')DISTNUM

  if (REGN .eq.8) then
     VAR = 'SN'

  else if (REGN.eq.1) then
     if (FORNUM.eq.4.or. FORNUM.eq.5.or. FORNUM.eq.17.or.&
          &FORNUM.eq.3.or. FORNUM.eq.14.or. FORNUM.eq.16) then
        VAR = 'IE'
     else
        VAR = 'EM'
     end if

  else if (REGN.eq.5) then
     !  DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
     !  INLAND CALIFORNIA
     if (FORNUM.eq.5.or. FORNUM.eq.6.or. FORNUM.eq.8.or.&
          &FORNUM.eq.11.or. FORNUM.eq.14) then
        VAR = 'CA'
        !  SOUTHERN OREGON
     else if (FORNUM.eq.9) then
        VAR = 'SO'
        !  WESTERN SIERRA
     else if (FORNUM.eq.17.or. FORNUM.eq.16.or. FORNUM.eq.15.or.&
          &FORNUM.eq.13.or. FORNUM.eq.3) then
        VAR = 'WS'
        !  KLAMATH/NORTHERN CALIFORNIA
     else if (FORNUM.eq.5) then
        VAR = 'NC'
     end if

  else if (REGN.eq.6) then
     !  DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
     !  BLUE MTN VARIANT
     if (FORNUM.eq.4.or. FORNUM.eq.7.or. FORNUM.eq.14.or.&
          &FORNUM.eq.16) then
        VAR = 'BM'
        !  EASTERN CASCADES
     else if (FORNUM.eq.17.or. FORNUM.eq.8.or. (FORNUM.eq.3.and.&
          &DISTNUM .eq.3) .or. (FORNUM.eq.6.and.&
          &(DISTNUM.eq.1.or.DISTNUM.eq.2.or.DISTNUM.eq.6))) then
        !  MOUNT HOOD Barlow RD
        VAR = 'EC'
        !  SOUTHERN OREGON
     else if (FORNUM.eq.1.or. FORNUM.eq.2.or. FORNUM.eq.20) then
        VAR = 'SO'
        !  WESTERN CASCADES
     else if (FORNUM.eq.5.or. FORNUM.eq.15.or. FORNUM.eq.18.or.&
          &FORNUM.eq.10.or. FORNUM.eq.3.or. FORNUM.eq.6) then
        VAR = 'WC'
        !  PACFIC NORTHWEST
     else if (FORNUM.eq.9.or. FORNUM.eq.12) then
        VAR = 'PN'
        !  NORTHERN CALIFORNIA
     else if (FORNUM.eq.11) then
        VAR = 'NC'
     else if (FORNUM.eq.21) then
        VAR = 'IE'
     end if

  else if (REGN.eq.7) then
     !  DETERMINE VARIENT BY FOREST AND DISTRICT NUMBER
     if (FORNUM.eq.2) then
        VAR = 'WC'
     else if (FORNUM.eq.3) then
        VAR = 'NC'
     else
        VAR = 'SO'
     end if

  else if (REGN .eq.9) then
     if (FORNUM.eq.13.or. FORNUM.eq.10.or. FORNUM.eq.3.or.&
          &FORNUM.eq.9.or. FORNUM.eq.4.or. FORNUM.eq.7.or.&
          &FORNUM.eq.2.or. FORNUM.eq.6) then
        VAR = 'LS'
     else if (FORNUM.eq.12.or. FORNUM.eq.8.or. FORNUM.eq.5) then
        VAR = 'CS'
     else if (FORNUM.eq.21.or. FORNUM.eq.20.or. FORNUM.eq.19.or.&
          &FORNUM.eq.14.or. FORNUM.eq.22) then
        VAR = 'NE'
     end if

  end if

  return
end

  !//////////////////////////////////////////////////////////////////
subroutine R1_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  !      SEARCH BY FIA SPECIES CODE
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  character(len=2) :: VAR
  integer :: SPEC,ERRFLAG
  character(len=10) :: EQNUM(81)
  integer :: FIA(38),FIRST,HALF,LAST,DONE,FORNUM,I
  ! SPECIES
  ! Balsam fir,           White fir,           Grand fir,       Subalpine fir,   Western juniper,
  ! Rocky Mt Juniper,     Subalpine larch,     Western larch,   Engelmann spruce, White spruce,
  ! Blue spruce,          Whitebark pine,      Bristlecone pine,Pinyon Pine,      Lodgepole pine,
  ! Limber pine,          Western white pine,  Ponderosa pine,  Singleleaf pinyon,border pinyon,
  ! Douglas fir,          Pacific yew,         Western redcedar,Western hemlock,  Mountain hemlock,
  ! Other Softwoods,      Rocky Mountain maple,Paper birch,     curlleaf mtn-mahog,green ash,
  ! Cottonwood,           balsam poplar,       plains cottonwood,Quaking aspen,  black cottonwood,
  ! Narrowleaf cottonwood,Other hardwood,      Unknown

  data (FIA(I),I = 1,38)/ 12,   15,   17,   19,   64,&
       &66,   72,   73,   93,   94,&
       &96,  101,  102,  106,  108,&
       &113,  119,  122,  133,  134,&
       &202,  231,  242,  263,  264,&
       &299,  321,  375,  475,  544,&
       &740,  741,  745,  746,  747,&
       &749,  998,  999/

  data (EQNUM(I),I = 1,81)/&
       &'I00FW2W012','102DVEW017','I00FW2W017','I00FW2W019','102DVEW060',&
       &'102DVEW106','I00FW2W019','I00FW2W073','I00FW2W093','102DVEW090',&
       &'102DVEW090','I00FW2W119','102DVEW108','102DVEW106','I00FW2W108',&
       &'I00FW2W073','I00FW2W119','I00FW2W122','102DVEW106','102DVEW106',&
       &'I00FW2W202','616BEHW231','I00FW2W242','I00FW2W260','I00FW2W260',&
       &'I00FW2W260','200DVEW746','101DVEW375','400DVEW475','101DVEW740',&
       &'102DVEW740','101DVEW740','102DVEW740','102DVEW746','102DVEW740',&
       &'102DVEW740','200DVEW746','I00FW2W260','203FW2W122','102DVEW740',&
       &'101DVEW108','100JB2W202','101DVEW202','102DVEW202','100FW2W202',&
       &'103DVEW202','101DVEW017','100JB2W017','101DVEW060','100JB2W108',&
       &'104DVEW108','103DVEW108','102DVEW260','100JB2W122','104DVEW122',&
       &'102DVEW122','105DVEW122','106DVEW122','101DVEW122','102DVEW019',&
       &'101DVEW019','100JB2W073','102DVEW073','101DVEW073','101DVEW999',&
       &'101DVEW260','102DVEW101','I12FW2W012','I11FW2W012','I13FW2W012',&
       &'I14FW2W012','I21FW2W012','I22FW2W012','102DVEW070','101DVEW240',&
       &'102DVEW240','102DVEW119','101DVEW119','103DVEW122','100JB2W019',&
       &'101DVEW090'/

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     do I = 1,81
        if (VOLEQ.eq.EQNUM(I)) then

           !  FOUND VALID EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do
     if (VOLEQ(1:7).eq.'616BEHW') then

        !  FOUND VALID WESTSIDE EQUATION NUMBER

        SPEC = 8888
        return
     end if

     if (VOLEQ(1:7).eq.'632BEHW') then

        !  FOUND VALID EASTSIDE EQUATION NUMBER

        SPEC = 8888
        return
     end if

     if (VOLEQ(1:7).eq.'B00BEHW') then

        !  FOUND VALID BLM/BIA/INDUSTRY LANDS EQUATION NUMBER

        SPEC = 8888
        return
     end if
     return
  end if

  LAST = 38
  read(FORST,'(I2)')FORNUM
  DONE = 0

  if (SPEC.eq.122.and. FORNUM.eq.8) then
     VOLEQ = EQNUM(39)
  else if (SPEC.eq.101) then
     if ((VAR.eq.'EM').or.(VAR.eq.'em').or.&
          &(VAR.eq.'IE').or.(VAR.eq.'ie').or.&
          &(VAR.eq.'CI').or.(VAR.eq.'ci')) then
        VOLEQ = EQNUM(1)
     end if
  else if ((VAR.eq.'EM'.or.VAR.eq.'em').and.((SPEC.eq.745).or.&
       &(SPEC.eq.747).or.(SPEC.eq.749))) then
     VOLEQ = EQNUM(40)
  else
     FIRST = 1

     do 5, while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (FIA(HALF) .eq. SPEC) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        ERRFLAG = 1
        DONE = -1
     else if (FIA(HALF) .lt. SPEC) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
     5 continue

     if (DONE .lt.0) then
        if (SPEC.lt.300) then
           !              Other Softwood
           VOLEQ = EQNUM(26)
        else
           !              Other Hardwood
           VOLEQ = EQNUM(36)
        end if
     else
        VOLEQ = EQNUM(DONE)
     end if
  end if

  return
end
  !//////////////////////////////////////////////////////////////////
subroutine R2_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  integer :: SPEC,ERRFLAG
  character(len=10) :: EQNUM(67)
  integer :: FIA(42),FIRST,HALF,LAST,DONE,FORNUM,I
  !     SPECIES
  !     White fir,        Grand fir,         Corkbark fir,      Subalpine fir,        Juniper,
  !     Alligator juniper,Utah Juniper,      Rocky Mtn. Juniper,Eastern redcedar,     Oneseed Juniper,
  !     Western Larch,    Engelmann's spruce,White spruce,      Blue spruce,          Whitebark pine,
  !     Bristlecode pine, Pinyon Pine,       Lodgepole pine,    Limber pine,          Southwestern white pine,
  !     Chihuahua pine,   Ponderosa pine,    Singleleaf pinyon, Border pinyon,        Arizona pinyon,
  !     Douglas fir,      Western Redcedar,  Mountain Hemlock,  Other softwoods,      Paper birch,
  !     Mountain Mahogany,Cottonwoods,       Plains cottonwood, Quaking aspen,        Narrowleaf cottonwood,
  !     Oak,              Arizona white oak, Emory Oak,         Gambel Oak,           Bur Oak,
  !     Silverleaf oak,   Other Hardwoods

  data (FIA(I),I = 1,42)/ 15,  17,  18,  19,  57,&
       &63,  65,  66,  68,  69,&
       &73,  93,  94,  96, 101,&
       &102, 106, 108, 113, 114,&
       &118, 122, 133, 134, 143,&
       &202, 242, 264, 299, 375,&
       &475, 740, 745, 746, 749,&
       &800, 803, 810, 814, 823,&
       &843, 998/

  data (EQNUM(I),I = 1,67)/&
       &'200FW2W015','I00FW2W019','I00FW2W019','I00FW2W019','300DVEW060',&
       &'300DVEW060','200DVEW065','300DVEW060','300DVEW060','200DVEW069',&
       &'407FW2W093','407FW2W093','407FW2W093','407FW2W093','200FW2W122',&
       &'200FW2W122','200DVEW106','200FW2W108','200FW2W122','200FW2W122',&
       &'200FW2W122','200FW2W122','300DVEW106','300DVEW106','300DVEW106',&
       &'200FW2W202','407FW2W093','407FW2W093','300DVEW060','300DVEW999',&
       &'200DVEW475','300DVEW999','300DVEW999','200FW2W746','300DVEW999',&
       &'300DVEW800','300DVEW800','300DVEW800','300DVEW800','200DVEW823',&
       &'300DVEW800','200DVEW998','203FW2W122','213FW2W122','202FW2W108',&
       &'200CZ2W202','200DVEW093','200CZ2W093','210DVEW093','200DVEW814',&
       &'200DVEW108','210DVEW108','200CZ2W108','212DVEW122','213DVEW122',&
       &'200CZ2W122','203CZ2W122','210DVEW122','200DVEW122','203DVEW122',&
       &'200CZ2W746','200DVEW746','210DVEW746','200DVEW066','200CZ2W019',&
       &'200CZ2W015','200DVEW015'/

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     do I = 1,67
        if (VOLEQ.eq.EQNUM(I)) then

           !  FOUND VALID EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do
     return
  end if

  read(FORST,'(I2)')FORNUM
  DONE = 0

  LAST = 42

  if (SPEC.eq.122.and. FORNUM.eq.3) then
     VOLEQ = EQNUM(43)
  else if (SPEC.eq.122.and. FORNUM.eq.13) then
     VOLEQ = EQNUM(44)
  else if (SPEC.eq.108.and. ((FORNUM.eq.2).or.(FORNUM.eq.14))) then
     VOLEQ = EQNUM(45)
  else
     FIRST = 1

     do 5, while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (FIA(HALF) .eq. SPEC) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        ERRFLAG = 1
        DONE = -1
     else if (FIA(HALF) .lt. SPEC) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
     5 continue
     if (DONE .lt.0) then
        !           Other Hardwood
        VOLEQ = EQNUM(42)
     else
        VOLEQ = EQNUM(DONE)
     end if
  end if

  return
end

  !//////////////////////////////////////////////////////////////////
subroutine R3_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  integer :: SPEC,ERRFLAG
  character(len=10) :: EQNUM(52)
  integer :: FIA(45),FIRST,HALF,LAST,DONE,FORNUM,I

  !     SPECIES
  !     White fir,              Grand fir,        Corkbark fir,         Subalpine fir,     Juniper,
  !     Juniper,                Alligator juniper,Utah juniper,         Rocky Mtn. Juniper,Eastern redcedar
  !     Oneseed juniper,        Western Larch,    Engelmann's spruce,   White spruce,      Blue spruce,
  !     Whitebark pine,         Bristlecode pine, Pinyon Pine,          Lodgepole pine,    Limber pine,
  !     Southwestern white pine,Chihuahua pine,   Ponderosa pine,       Singleleaf pinyon, Border pinyon,
  !     Arizona pinyon,         Douglas fir,      Western Redcedar,     Mountain Hemlock,  Other softwoods,
  !     Maple,                  Black maple,      Paper Birch,          Mountain Mahogany, Cottonwoods,
  !     Plains cottonwood,      Quaking aspen,    Narrowleaf cottonwood,Oak,               Arizona white oak,
  !     Emory oak,              Gambel oak,       Bur oak,              Silverleaf oak,    Other Hardwoods

  data (FIA(I),I = 1,45)/ 15,  17,  18,  19,  57,&
       &60,  63,  65,  66,  68,&
       &69,  73,  93,  94,  96,&
       &101, 102, 106, 108, 113,&
       &114, 118, 122, 133, 134,&
       &143, 202, 242, 264, 299,&
       &310, 314, 375, 475, 740,&
       &745, 746, 749, 800, 803,&
       &810, 814, 823, 843, 998/

  data (EQNUM(I),I = 1,52)/&
       &'301DVEW015','300DVEW093','300DVEW093','300DVEW093','300DVEW060',&
       &'300DVEW060','300DVEW060','300DVEW060','300DVEW060','300DVEW060',&
       &'300DVEW060','301DVEW015','300DVEW093','300DVEW093','300DVEW093',&
       &'300DVEW113','300DVEW113','300DVEW106','301DVEW202','300DVEW113',&
       &'300DVEW113','300DVEW122','300FW2W122','300DVEW106','300DVEW106',&
       &'300DVEW106','301DVEW202','301DVEW015','301DVEW015','300DVEW060',&
       &'300DVEW310','300DVEW314','300DVEW999','300DVEW999','300DVEW999',&
       &'300DVEW999','300DVEW746','300DVEW999','300DVEW800','300DVEW800',&
       &'300DVEW800','300DVEW800','300DVEW800','300DVEW800','300DVEW999',&
       &'302DVEW202','302DVEW202','302DVEW015','301FW2W122','301FW2W202',&
       &'301FW2W015','301FW2W108'/

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     do I = 1,52
        if (VOLEQ.eq.EQNUM(I)) then

           !  FOUND VALID EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do
     return
  end if

  read(FORST,'(I2)')FORNUM
  DONE = 0

  LAST = 45

  if (SPEC.eq.202.and.(FORNUM.eq.2.or.FORNUM.eq.3.or.&
       &FORNUM.eq.7)) then
     DONE = 46
  else if (SPEC.eq.15.and.(FORNUM.eq.2.or.FORNUM.eq.3.or.&
       &FORNUM.eq.7)) then
     DONE = 48
     !    using Fleweling profile model for Ponderosa pine in following forest:
     !    Apache Sitgreaves, Coconino, Kaibab, Tonto. The 300FW2W122 equation
     !    is default for PP, so tht other forests 2,3,5,6,8,9,10 and 11 are
     !    set to the DVE equation blow.
  else if (SPEC.eq.122.and.(FORNUM.eq.2.or.FORNUM.eq.3.or.&
       &FORNUM.eq.5.or.FORNUM.eq.6.or.&
       &FORNUM.eq.8.or.FORNUM.eq.9.or.&
       &FORNUM.eq.11)) then
     DONE = 22
     !    Added profile model for DF, PP, WF and WP to Santa Fe NF
  else if (SPEC.eq.122.and.FORNUM.eq.10) then
     DONE = 49
  else if (SPEC.eq.202.and.FORNUM.eq.10) then
     DONE = 50
  else if (SPEC.eq.15.and.FORNUM.eq.10) then
     DONE = 51
  else if (SPEC.eq.114.and.FORNUM.eq.10) then
     DONE = 52
  else
     FIRST = 1

     do 5, while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (FIA(HALF) .eq. SPEC) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        ERRFLAG = 1
        DONE = -1
     else if (FIA(HALF) .lt. SPEC) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
     5 continue
  end if
  if (DONE .lt.0) then
     !        Unknown
     VOLEQ = EQNUM(45)
  else
     VOLEQ = EQNUM(DONE)
  end if

  return
end

  !//////////////////////////////////////////////////////////////////
subroutine R4_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  integer :: SPEC,ERRFLAG
  character(len=10) :: EQNUM(57)
  integer :: FIA(27),FIRST,HALF,LAST,DONE,FORNUM,I

  ! SPECIES
  ! California red fir,    Juniper,          Western juniper,      Rocky Mtn juniper,    Western larch,
  ! Incense-cedar,         Common pinyon,    Sugar pine,           Western white pine,   Singleleaf pinyon,
  ! Pacific yew,           Western redcedar, Western hemlock,      Mountain hemlock,     Other softwoods,
  ! Box elder,             Rocky Mtn maple,  Bigtooth maple,       Curlleaf Mtn Mahogany,Quaking aspen,
  ! Black cottonwood,      Fremont cottonwod,Narrowleaf cottonwood,Oak (sp.)             Gambel oak,
  ! Other hardwoods,       Other

  data (FIA(I),I = 1,27)/   20,   60,   64,   66,   73,&
       &81,  106,  117,  119,  133,&
       &231,  242,  263,  264,  299,&
       &313,  321,  322,  475,  746,&
       &747,  748,  749,  800,  814,&
       &998,  999/

  data (EQNUM(I),I = 1,57)/&
       &'400MATW020','300DVEW060','300DVEW060','400DVEW066','400MATW073',&
       &'400MATW081','300DVEW106','400MATW117','400MATW117','400DVEW133',&
       &'400DVEW998','400MATW081','400MATW015','401MATW015','400MATW108',&
       &'400MATW108','400MATW108','400MATW108','400DVEW475','400MATW746',&
       &'400DVEW998','400MATW108','400MATW108','300DVEW800','300DVEW800',&
       &'400DVEW998','400DVEW998','I15FW2W017','401MATW015','400MATW015',&
       &'I15FW2W017','405MATW019','400MATW019','401DVEW065','400DVEW065',&
       &'I15FW2W093','407FW2W093','400MATW093','401MATW108','400MATW108',&
       &'I15FW2W122','401MATW122','402MATW122','403MATW122','400MATW122',&
       &'I15FW2W202','405MATW202','400MATW202','400DVEW064','400DVEW106',&
       &'407MATW093','401MATW202','407MATW093','401MATW202','401MATW202',&
       &'407MATW093','407MATW093'/

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     do I = 1,57
        if (VOLEQ.eq.EQNUM(I)) then

           !  FOUND VALID EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do
     return
  end if

  read(FORST,'(I2)')FORNUM
  DONE = 0

  LAST = 27

  !     White fir
  if (SPEC.eq.15) then
     if (FORNUM.eq.2.or.FORNUM.eq.12.or.FORNUM.eq.13.or.&
          &FORNUM.eq.6) then
        VOLEQ = EQNUM(28)
     else if (FORNUM.eq.9.or.FORNUM.eq.17) then
        VOLEQ = EQNUM(29)
     else
        VOLEQ = EQNUM(30)
     end if
     !     Grand fir
  else if (SPEC.eq.17) then
     if (FORNUM.eq.2.or.FORNUM.eq.12.or.FORNUM.eq.13) then
        VOLEQ = EQNUM(31)
     else
        VOLEQ = EQNUM(30)
     end if
     !     Subalpine fir
  else if (SPEC.eq.19) then
     if (FORNUM.eq.5) then
        VOLEQ = EQNUM(32)
     else
        VOLEQ = EQNUM(33)
     end if
     !     Utah Juniper
  else if (SPEC.eq.64) then
     if ((FORNUM.eq.1).or.(FORNUM.eq.4).or.(FORNUM.eq.7).or.&
          &(FORNUM.eq.8).or.(FORNUM.eq.9).or.(FORNUM.eq.10).or.&
          &(FORNUM.eq.17).or.(FORNUM.eq.18).or.(FORNUM.eq.19)) then
        VOLEQ = EQNUM(49)
     else if ((FORNUM.eq.2).or.(FORNUM.eq.6).or.(FORNUM.eq.12).or.&
          &(FORNUM.eq.13).or.(FORNUM.eq.14)) then
        VOLEQ = EQNUM(49)
     end if
  else if (SPEC.eq.65) then
     if (FORNUM.eq.3.or.FORNUM.eq.5.or.&
          &FORNUM.eq.15.or.FORNUM.eq.16) then
        VOLEQ = EQNUM(34)
     else
        VOLEQ = EQNUM(35)
     end if
     !     Engelmann spruce
     !     Blue spruce
  else if (SPEC.eq.93.or.SPEC.eq.96) then
     if (FORNUM.eq.2.or.FORNUM.eq.12.or.FORNUM.eq.13) then
        VOLEQ = EQNUM(36)
     else if (FORNUM.eq.7.or.FORNUM.eq.8) then
        VOLEQ = EQNUM(37)
     else
        VOLEQ = EQNUM(38)
     end if
     !     Whitebark pine
     !     Lodgepole pine
     !     Limber pine
     !     Bristlecone pine
  else if (SPEC.eq.101.or.SPEC.eq.108.or.SPEC.eq.113.or.&
       &SPEC.eq.142) then
     if (FORNUM.eq.9.or.FORNUM.eq.17) then
        VOLEQ = EQNUM(39)
     else
        VOLEQ = EQNUM(40)
     end if
     !     pinyon pine
  else if (SPEC.eq.106) then
     if ((FORNUM.eq.1).or.(FORNUM.eq.4).or.(FORNUM.eq.7).or.&
          &(FORNUM.eq.8).or.(FORNUM.eq.9).or.(FORNUM.eq.10).or.&
          &(FORNUM.eq.17).or.(FORNUM.eq.18).or.(FORNUM.eq.19)) then
        VOLEQ = EQNUM(50)
     end if
     !     Ponderosa pine
  else if (SPEC.eq.122) then
     if (FORNUM.eq.2.or.FORNUM.eq.12.or.FORNUM.eq.13) then
        VOLEQ = EQNUM(41)
     else if (FORNUM.eq.01) then
        VOLEQ = EQNUM(42)
     else if (FORNUM.eq.7.or.FORNUM.eq.8.or.FORNUM.eq.10.or.&
          &FORNUM.eq.18.or.FORNUM.eq.19) then
        VOLEQ = EQNUM(43)
     else if (FORNUM.eq.9.or.FORNUM.eq.17) then
        VOLEQ = EQNUM(44)
     else
        VOLEQ = EQNUM(45)
     end if
     !     Douglas fir
  else if (SPEC.eq.202) then
     if (FORNUM.eq.2.or.FORNUM.eq.12.or.FORNUM.eq.13) then
        VOLEQ = EQNUM(46)
     else if (FORNUM.eq.05) then
        VOLEQ = EQNUM(47)
     else
        VOLEQ = EQNUM(48)
     end if
     !     Other hardwoods
  else if (SPEC.eq.998) then
     if ((FORNUM.eq.2).or.(FORNUM.eq.6).or.(FORNUM.eq.12).or.&
          &(FORNUM.eq.13).or.(FORNUM.eq.14)) then
        VOLEQ = EQNUM(17)
     else
        VOLEQ = EQNUM(21)
     end if
  else
     FIRST = 1

     do 5, while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (FIA(HALF) .eq. SPEC) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        ERRFLAG = 1
        DONE = -1
     else if (FIA(HALF) .lt. SPEC) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
     5 continue

     if (DONE .lt.0) then
        !           Other Hardwood
        VOLEQ = EQNUM(26)
     else
        VOLEQ = EQNUM(DONE)
     end if
  end if

  return
end

  !//////////////////////////////////////////////////////////////////
subroutine R5_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  character(len=2) :: VAR
  integer :: SPEC,ERRFLAG
  character(len=10) :: EQNUM(102)
  integer :: FIA(71),FIRST,HALF,LAST,DONE,FORNUM,I

  !     SPECIES
  !     Pacific silver fir,    White fir,             Grand fir,           Subalpine fir,       California red fir,
  !     Shasta red fir,        Noble fir,             Port Orford cedar,   California juniper,  Juniper,
  !     Utah juniper,          Western Larch,         Incense cedar,       Brewer spruce,       Engelmann spruce,
  !     White bark pine,       Knobcone pine,         Foxtail pine,        Lodgepole pine,      Coulter pine,
  !     Limber pine,           Jeffrey pine,          Sugar pine,          Western white pine,  Ponderosa pine,
  !     Monterey pine,         Grey pine,             Singleleaf pinyon,   Washoe pine,         Great Basin bristlecone pine
  !     Bigcone Douglas-fir,   Douglas-fir,           Redwood,             Giant sequoia,       Pacific yew,
  !     Western red cedar,     California nutmeg,     Western hemlock,     Mountain hemlock,    Other softwoods,
  !     Bigleaf maple,         California buckeye,    Red alder,           White alder          Pacific madrone,
  !     Golden chinkapin,      Curl-leaf Mtn Mahogany,Birchleaf Mtn Mahogany,Pacific dogwood,   Oregon Ash,
  !     Walnut,                Tanoak,                California sycamore, Quacking aspen,      Black cottonwood,
  !     Bitter Cherry,         California live oak,   Canyon live oak,     Blue oak,            Engelmann's oak,
  !     Oregon white oak,      California black oak,  California white oak,Interior live oak,   Willow,
  !     California laurel,     Other hardwoods,       unkown

  !     Sharpnack equations removed as well as references to koa, eucalyptus, Ohia

  data (FIA(I),I = 1,68)/   11,   15,   17,   19,   20,&
       &21,   22,   41,   62,   64,&
       &65,   73,   81,   92,   93,&
       &101,  103,  104,  108,  109,& ! 20
       &113,  116,  117,  119,  122,&
       &124,  127,  133,  137,  142,&
       &201,  202,  211,  212,  231,&
       &242,  251,  263,  264,  299,& ! 40
       &312,  333,  351,  352,  361,&
       &431,  475,  478,  492,  542,&
       &600,  631,  730,  746,  747,&
       &768,  801,  805,  807,  811,& ! 60
       &815,  818,  821,  839,  920,&
       &981,  998,  999/

  data (EQNUM(I),I = 1,102)/&
       &'500WO2W015','500WO2W015','500WO2W015','500WO2W020','500WO2W020',&
       &'500WO2W020','500WO2W020','500WO2W081','500DVEW060','500DVEW060',&
       &'500DVEW060','500WO2W202','500WO2W081','500WO2W015','500WO2W015',&
       &'500WO2W108','500WO2W108','500WO2W108','500WO2W108','500WO2W108',& ! 20
       &'500WO2W108','500WO2W116','500WO2W117','500WO2W117','500WO2W122',&
       &'500WO2W108','500WO2W108','500WO2W116','500WO2W117','500WO2W108',&
       &'500WO2W202','500WO2W202','500WO2W211','500DVEW212','500WO2W108',&
       &'500WO2W081','500DVEW807','500WO2W015','500WO2W015','500WO2W108',& ! 40
       &'500DVEW312','500DVEW807','500DVEW351','500DVEW351','500DVEW361',&
       &'500DVEW431','500DVEW801','500DVEW801','500DVEW807','500DVEW807',&
       &'500DVEW818','500DVEW631','500DVEW818','500DVEW818','500DVEW818',&
       &'500DVEW801','500DVEW801','500DVEW805','500DVEW807','500DVEW811',& ! 60
       &'500DVEW815','500DVEW818','500DVEW821','500DVEW839','500DVEW807',&
       &'500DVEW981','500DVEW981','500DVEW631','532WO2W015','532WO2W020',&
       &'532WO2W081','532WO2W108','532WO2W117','532WO2W122','532WO2W202',&
       &'532WO2W020','532WO2W202','532WO2W081','532WO2W116','616BEHW000',& ! 80
       &'532WO2W108','532WO2W122','532WO2W211','532WO2W117','532WO2W015',&
       &'516TRFW015','532TRFW015','516TRFW021','532TRFW021','516TRFW060',&
       &'532TRFW060','516TRFW081','532TRFW081','516TRFW108','532TRFW108',&
       &'516TRFW117','532TRFW117','516TRFW122','532TRFW122','516TRFW202',& ! 100
       &'616TRFW211','632TRFW211'/

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     do I = 1,102
        if (VOLEQ.eq.EQNUM(I)) then

           !  FOUND VALID EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do
     return
  end if

  read(FORST,'(I2)')FORNUM
  DONE = 0

  !     other softwoods
  if (SPEC.eq.299.or. SPEC.eq.290) then
     if (VAR.eq.'SO' .or. VAR.eq.'so') then
        DONE = 30
     else if (VAR.eq.'NC' .or. VAR.eq.'nc') then
        DONE = 30
     else
        DONE = 40
     end if
     !     other hardwoods
  else if (SPEC.eq.998.) then
     if (VAR.eq.'SO' .or. VAR.eq.'so') then
        DONE = 67
     else if (VAR.eq.'WS' .or. VAR.eq.'ws') then
        DONE = 64
     else if (VAR.eq.'NC' .or. VAR.eq.'nc') then
        DONE = 67
     else
        DONE = 57
     end if
  end if

  LAST = 68

  FIRST = 1

  do 5, while (DONE.eq.0)
  HALF = (LAST - FIRST +1)/2 + FIRST
  if (FIA(HALF) .eq. SPEC) then
     DONE = HALF
  else if (FIRST .eq. LAST) then
     ERRFLAG = 1
     DONE = -1
  else if (FIA(HALF) .lt. SPEC) then
     FIRST = HALF
  else
     LAST = HALF - 1
  end if
  5 continue

  if (DONE .lt.0) then
     VOLEQ = EQNUM(71)
  else
     VOLEQ = EQNUM(DONE)
  end if
  !      ENDIF

  return
end

  !//////////////////////////////////////////////////////////////////
subroutine R6_EQN(VAR,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  character(len=2) :: VAR
  character(len=2) :: DIST
  character(len=3) :: ASPEC
  integer :: SPEC,ERRFLAG,FORNUM,I,FIA(53),DISTNUM
  character(len=10) :: EQNUMI(87)
  character(len=10) :: EQNUM(44)
  character(len=10) :: EQNUMC(39)
  character(len=10) :: EQNUMF(41)
  character(len=10) :: EQNUMD(15)
  integer :: FIRST,HALF,LAST,DONEI,DONEF

  !     All species 32' logs, All species 16' logs,
  !     Balsam fir,      Grand Fir,  Alpine Fir,       Western Larch, Engelmann spruce
  !     Lodgepole pine,  White Pine, Ponderosa pine,   Douglas Fir,   Western red cedar,
  !     Mountain Hemlock,Douglas fir,Western red cedar,Mountain Hemlock

  data (FIA(I),I = 1,53)/  011, 015, 017, 019, 020,&
       &021, 022, 042, 064, 066,&
       &072, 073, 081, 093, 098,&
       &101, 103, 106, 108, 113,&
       &116, 117, 119, 122, 202,&
       &211, 231, 242, 263, 264,&
       &290, 299, 312, 321, 351,&
       &352, 361, 375, 431, 475,&
       &478, 492, 500, 631, 740,&
       &746, 747, 768, 815, 818,&
       &920, 998, 999/

  data (EQNUMI(I),I = 1,87)/&
       &'I00FW2W012','I00FW2W017','I00FW2W019','I00FW2W073','I00FW2W093',&
       &'I00FW2W108','I00FW2W119','I00FW2W122','I00FW2W202','I00FW2W242',&
       &'I00FW2W260','I00FW2W263','I11FW2W012','I11FW2W017','I11FW2W019',&
       &'I11FW2W073','I11FW2W093','I11FW2W108','I11FW2W119','I11FW2W122',&
       &'I11FW2W202','I11FW2W242','I11FW2W260','I11FW2W263','I12FW2W012',&
       &'I12FW2W017','I12FW2W019','I12FW2W073','I12FW2W093','I12FW2W108',&
       &'I12FW2W119','I12FW2W122','I12FW2W202','I12FW2W242','I12FW2W260',&
       &'I12FW2W263','I13FW2W012','I13FW2W017','I13FW2W019','I13FW2W073',&
       &'I13FW2W093','I13FW2W108','I13FW2W119','I13FW2W122','I13FW2W202',&
       &'I13FW2W242','I13FW2W260','I13FW2W263','I14FW2W012','I14FW2W017',&
       &'I14FW2W019','I14FW2W073','I14FW2W093','I14FW2W108','I14FW2W119',&
       &'I14FW2W122','I14FW2W202','I14FW2W242','I14FW2W260','I14FW2W263',&
       &'I00FW2W012','I00FW2W202','I00FW2W093','I00FW2W017','I00FW2W108',&
       &'I00FW2W122','I00FW2W019','I00FW2W073','I00FW2W119','I00FW2W242',&
       &'I00FW2W260','I13FW2W202','I11FW2W202','I12FW2W202','I14FW2W017',&
       &'I13FW2W017','I14FW2W122','I12FW2W122','I13FW2W122','I11FW2W122',&
       &'I11FW2W017','I11FW2W073','I11FW2W242','I12FW2W017','I12FW2W073',&
       &'I13FW2W073','I14FW2W073'/

  data (EQNUMF(I),I = 1,41)/&
       &'F00FW2W202','F00FW2W242','F00FW2W263','F01FW2W202','F01FW2W242',&
       &'F01FW2W263','F02FW2W202','F02FW2W242','F02FW2W263','F03FW2W202',&
       &'F03FW2W242','F03FW2W263','F04FW2W202','F04FW2W242','F04FW2W263',&
       &'F05FW2W202','F05FW2W242','F05FW2W263','F06FW2W202','F06FW2W242',&
       &'F06FW2W263','F07FW2W202','F07FW2W242','F07FW2W263','F08FW2W202',&
       &'F08FW2W242','F08FW2W263','F03FW2W202','F01FW2W202','F02FW2W202',&
       &'F00FW2W202','F04FW2W202','F08FW2W202','F07FW2W202','F06FW2W202',&
       &'F05FW2W202','F00FW2W242','F01FW2W242','F02FW2W242','F03FW2W242',&
       &'F04FW2W242'/

  data (EQNUMC(I),I = 1,39)/&
       &'I21FW2W012','I21FW2W017','I21FW2W019','I21FW2W073','I21FW2W093',&
       &'I21FW2W108','I21FW2W119','I21FW2W122','I21FW2W202','I21FW2W242',&
       &'I21FW2W260','I22FW2W012','I22FW2W017','I22FW2W019','I22FW2W073',&
       &'I22FW2W093','I22FW2W108','I22FW2W119','I22FW2W122','I22FW2W202',&
       &'I22FW2W242','I22FW2W260','I23FW2W012','I23FW2W017','I23FW2W019',&
       &'I23FW2W073','I23FW2W093','I23FW2W108','I23FW2W119','I23FW2W122',&
       &'I23FW2W202','I23FW2W242','I23FW2W260','I22FW2W122','I21FW2W122',&
       &'I23FW2W122','I21FW2W073','I22FW2W073','I23FW2W073'/

  data (EQNUMD(I),I = 1,15)/&
       &'601DVEW205','601DVEW263','601DVEW015','602DVEW122','602DVEW204',&
       &'602DVEW015','602DVEW108','602DVEW122','601DVEW205','602DVEW204',&
       &'602DVEW108','600DVEW122','601DVEW015','601DVEW263','602DVEW015'/

  data (EQNUM(I),I = 1,44)/&
       &'B00BEHW011','B00BEHW015','B00BEHW017','B00BEHW015','B00BEHW021',&
       &'B00BEHW021','B00BEHW022','B00BEHW041','B00BEHW042','B00BEHW242',&
       &'B00BEHW073','B00BEHW073','B00BEHW081','B00BEHW093','B00BEHW098',&
       &'B00BEHW119','B00BEHW108','B00BEHW108','B00BEHW116','B00BEHW117',&
       &'B00BEHW119','B00BEHW122','B01BEHW202','B00BEHW211','B00BEHW231',&
       &'B00BEHW242','B00BEHW260','B00BEHW260','B00BEHW260','B00BEHW312',&
       &'B00BEHW351','B00BEHW361','B00BEHW361','B00BEHW431','B00BEHW542',&
       &'B00BEHW631','B00BEHW747','B00BEHW800','B00BEHW800','B00BEHW998',&
       &'B00BEHW999','B02BEHW202','B03BEHW202','B01BEHW202'/

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     if (VOLEQ(1:7).eq.'616BEHW') then

        !  FOUND VALID WESTSIDE EQUATION NUMBER

        SPEC = 8888
        return
     end if

     if (VOLEQ(1:7).eq.'628BEHW') then
        SPEC = 8888
        return
     end if

     if (VOLEQ(1:7).eq.'632BEHW') then

        !  FOUND VALID EASTSIDE EQUATION NUMBER

        SPEC = 8888
        return
     end if

     !  CHECK FOR VALID PNW TARRIF EQUATION

     if (VOLEQ(1:7).eq.'616TRFW') then
        SPEC = 8888
        return
     end if

     if (VOLEQ(1:7).eq.'632TRFW') then
        SPEC = 8888
        return
     end if

     do I = 1,44
        if (VOLEQ.eq.EQNUM(I)) then

           !  FOUND VALID BLM/BIA/INDUSTRY LANDS EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do

     do I = 1,87
        if (VOLEQ.eq.EQNUMI(I)) then

           !  FOUND VALID INGY EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do

     do I = 1,41
        if (VOLEQ.eq.EQNUMF(I)) then

           !  FOUND VALID WESTSIDE FLEWELLING EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do

     do I = 1,39
        if (VOLEQ.eq.EQNUMC(I)) then

           !  FOUND VALID INGY EQUATION NUMBER - CANADIAN MODEL

           SPEC = 8888
           return
        end if
     end do

     do I = 1,15
        if (VOLEQ.eq.EQNUMD(I)) then

           !  FOUND VALID DIRECT VOLUME ESTIMATORS

           SPEC = 8888
           return
        end if
     end do
     return
  end if

  read(FORST,'(I2)')FORNUM
  read(DIST,'(I2)')DISTNUM
  DONEI = 0
  DONEF = 0

  !      WRITE(ASPEC,'(I3)')SPEC
  !      IF(ASPEC(1:1).EQ.' ')ASPEC(1:1) = '0'
  !     Westside Variants
  if (VAR.eq.'PN' .or. VAR.eq.'WC' .or. VAR.eq.'NC' .or.&
       &VAR.eq.'CA' .or. VAR.eq.'OC' .or. VAR.eq.'OP') then

     !        Gifford Pinchot
     if (FORNUM.eq.3) then
        if (SPEC.eq.11) then
           DONEI = 26
        else if (SPEC.eq.19) then
           DONEI = 6
        else if (SPEC.eq.263.or. SPEC.eq.260) then
           DONEF = 3
           if (DISTNUM.eq.1) then
              DONEF = 21
           else if (DISTNUM.eq.5) then
              DONEF = 15
           end if
        else if (SPEC.eq.202) then
           DONEF = 10
           if (DISTNUM.eq.1) then
              DONEF = 22
           else if (DISTNUM.eq.5) then
              DONEF = 10
           end if
        end if
        !        Mt Hood
     else if (FORNUM.eq.6) then
        if (SPEC.eq.11) then
           DONEI = 26
        else if (SPEC.eq.17) then
           DONEI = 38
        else if (SPEC.eq.93) then
           DONEI = 17
        else if (SPEC.eq.108) then
           DONEI = 18
        else if (SPEC.eq.122) then
           DONEI = 32
        else if (SPEC.eq.263.or. SPEC.eq.260) then
           DONEI = 23
        else if (SPEC.eq.22) then
           DONEI = 38
        else if (SPEC.eq.202) then
           DONEF = 10
           if (DISTNUM.eq.5.or. DISTNUM.eq.9) DONEF = 10
        end if
        !        Mt Baker - Snoqualmie
     else if (FORNUM.eq.5) then
        if (SPEC.eq.263.or. SPEC.eq.260) then
           DONEF = 12
        else if (SPEC.eq.202) then
           DONEF = 25
        end if
        !        Rogue River/Siskiyo
     else if (FORNUM.eq.10.or.FORNUM.eq.11) then
        if (SPEC.eq.15) then
           if ((VAR.ne.'NC').or.(VAR.ne.'nc')) then
              DONEI = 5
           end if
        else if (SPEC.eq.122) then
           if ((VAR.ne.'NC').or.(VAR.ne.'nc')) then
              DONEI = 4
           end if
        else if (SPEC.eq.202) then
           DONEF = 19
        end if
        !        Siuslaw
     else if (FORNUM.eq.12) then
        if (SPEC.eq.202) then
           DONEF = 1
        else if (SPEC.eq.263) then
           DONEF = 12
        end if
        !        Olympic
     else if (FORNUM.eq.9) then
        if (SPEC.eq.202) then
           DONEF = 10
        else if (SPEC.eq.98) then
           DONEF = 12
        else if (SPEC.eq.263) then
           DONEF = 3
        end if
        !        Umpqua
     else if (FORNUM.eq.15) then
        if (SPEC.eq.15) then
           DONEI = 2
        else if (SPEC.eq.20) then
           DONEI = 1
        else if (SPEC.eq.81) then
           DONEI = 4
        else if (SPEC.eq.93) then
           DONEI = 5
        else if (SPEC.eq.108) then
           DONEI = 6
        else if (SPEC.eq.122) then
           DONEI = 4
        else if (SPEC.eq.202) then
           DONEF = 1
        else if (SPEC.eq.242) then
           DONEI = 1
        else if (SPEC.eq.263) then
           DONEI = 23
        else if (SPEC.eq.264) then
           DONEI = 10
        end if
        !         Willamette
     else if (FORNUM.eq.18) then
        if (SPEC.eq.22) then
           DONEI = 6
        else if (SPEC.eq.202) then
           DONEF = 16
        else if (SPEC.eq.263) then
           DONEF = 12
        end if
     end if

     if (DONEI.gt.0) then
        VOLEQ = EQNUMI(DONEI)
     else if (DONEF.gt.0) then
        VOLEQ = EQNUMF(DONEF)
     else
        !           No INGY, find Behre's hyperbola model
        LAST = 53

        FIRST = 1

        do 5, while (DONEF.eq.0)
        HALF = (LAST - FIRST +1)/2 + FIRST
        if (FIA(HALF) .eq. SPEC) then
           DONEF = HALF
        else if (FIRST .eq. LAST) then
           ERRFLAG = 1
           DONEF = -1
        else if (FIA(HALF) .lt. SPEC) then
           FIRST = HALF
        else
           LAST = HALF - 1
        end if
        5 continue

        if (DONEF .lt.0) then
           VOLEQ = '616BEHW000'
        else
           if (FIA(DONEF) .lt.10) then
              write(ASPEC,'(I1)')FIA(DONEF)
              VOLEQ(1:9) = '616BEHW00'
              VOLEQ(10:10) = ASPEC
           else if (FIA(DONEF) .lt.100) then
              write(ASPEC,'(I2)')FIA(DONEF)
              VOLEQ(1:8) = '616BEHW0'
              VOLEQ(9:10) = ASPEC
           else
              write(ASPEC,'(I3)')FIA(DONEF)
              VOLEQ(1:7) = '616BEHW'
              VOLEQ(8:10) = ASPEC
           end if
        end if
     end if

     !     Eastside Variants
  else
     !        first check for INGY equations by forest and species
     !        Deschutes
     if (FORNUM.eq.1) then
        if (SPEC.eq.11.or. SPEC.eq.15.or. SPEC.eq.17&
             &.or. SPEC.eq.21) then
           if ((VAR.ne.'SO').or.(VAR.ne.'so'))DONEI = 2
        else if (SPEC.eq.73) then
           DONEI = 16
        else if (SPEC.eq.108) then
           DONEI = 18
        else if (SPEC.eq.122) then
           DONEI = 20
        else if (SPEC.eq.202 ) then
           DONEI = 21
        else if (SPEC.eq.81) then
           DONEI = 22
        end if
        !        Fremont
     else if (FORNUM.eq.2.or. FORNUM.eq.20) then
        if (SPEC.eq.15.or. SPEC.eq.17) then
           DONEI = 14
        else if (SPEC.eq.81) then
           DONEI = 9
        else if (SPEC.eq.108) then
           DONEI = 6
        else if (SPEC.eq.122) then
           DONEI = 8
        else if (SPEC.eq.202) then
           DONEI = 2
        end if
        !        Gifford Pinchot
     else if (FORNUM.eq.3) then
        if (SPEC.eq.11) then
           DONEI = 26
        else if (SPEC.eq.263.or. SPEC.eq.260) then
           if (DISTNUM.eq.3) DONEF = 3
        else if (SPEC.eq.202) then
           if (DISTNUM.eq.3) DONEI = 10
        end if
        !        Malheur
     else if (FORNUM.eq.4) then
        if (SPEC.eq.108) then
           DONEI = 30
        else if (SPEC.eq.17.or. SPEC.eq.15) then
           DONEI = 26
        else if (SPEC.eq.122) then
           DONEI = 32
        else if (SPEC.eq.202) then
           DONEI = 33
        end if
        !        Mt Hood
     else if (FORNUM.eq.6) then
        if (SPEC.eq.11) then
           DONEI = 26
        else if (SPEC.eq.17) then
           DONEI = 38
        else if (SPEC.eq.93) then
           DONEI = 17
        else if (SPEC.eq.108) then
           DONEI = 18
        else if (SPEC.eq.122) then
           DONEI = 32
        else if (SPEC.eq.263.or. SPEC.eq.260) then
           DONEI = 23
        else if (SPEC.eq.22) then
           DONEI = 38
        else if (SPEC.eq.202) then
           DONEF = 16
           if (DISTNUM.eq.1.or. DISTNUM.eq.6)DONEF = 22
        end if
        !        Ochoco
     else if (FORNUM.eq.7) then
        if (SPEC.eq.17.or. SPEC.eq.15) then
           DONEI = 26
        else if (SPEC.eq.73) then
           DONEI = 28
        else if (SPEC.eq.122) then
           DONEI = 32
        else if (SPEC.eq.202) then
           DONEI = 33
        else if (SPEC.eq.108) then
           DONEI = 30
        end if
        !        Umatilla
     else if (FORNUM.eq.14) then
        if (SPEC.eq.17.or. SPEC.eq.15) then
           DONEI = 38
        else if (SPEC.eq.19) then
           DONEI = 3
        else if (SPEC.eq.73) then
           DONEI = 45
        else if (SPEC.eq.93) then
           DONEI = 5
        else if (SPEC.eq.108) then
           DONEI = 6
        else if (SPEC.eq.122) then
           DONEI = 44
        else if (SPEC.eq.202) then
           DONEI = 38
        end if
        !        Wallowa-Whitman
     else if (FORNUM.eq.16) then
        if (SPEC.eq.17.or. SPEC.eq.15) then
           DONEI = 14
        else if (SPEC.eq.73) then
           DONEI = 40
        else if (SPEC.eq.93) then
           DONEI = 17
        else if (SPEC.eq.108) then
           DONEI = 6
        else if (SPEC.eq.122) then
           DONEI = 20
        else if (SPEC.eq.202) then
           DONEI = 21
        end if
        !        Okanogan - Wenatchee
     else if (FORNUM.eq.8.or. FORNUM.eq.17) then
        if (SPEC.eq.17) then
           DONEI = 14
           if (DISTNUM.eq.2.or. DISTNUM.eq.3.or. DISTNUM.eq.5&
                &.or. DISTNUM.eq.7 ) DONEI = 14
        else if (SPEC.eq.202) then
           DONEI = 33
           if (DISTNUM.eq.2.or. DISTNUM.eq.3.or. DISTNUM.eq.4&
                &.or. DISTNUM.eq.5.or. DISTNUM.eq.7.or.&
                &DISTNUM.eq.9) DONEI = 33
        else if (SPEC.eq.108) then
           DONEI = 30
           if (DISTNUM.eq.4) DONEI = 30
        else if (SPEC.eq.93) then
           DONEI = 17
           if (DISTNUM.eq.9) DONEI = 17
        else if (SPEC.eq.122.or.SPEC.eq.73) then
           DONEI = 32
           if (DISTNUM.eq.4) then
              DONEI = 32
           else if (DISTNUM.eq.2.or. DISTNUM.eq.3.or.&
                &DISTNUM.eq.5.or. DISTNUM.eq.7) then
              DONEI = 20
           end if
        end if
        !        Colville
     else if (FORNUM.eq.21) then
        if (SPEC.eq.17) then
           DONEI = 14
        else if (SPEC.eq.19) then
           DONEI = 21
        else if (SPEC.eq.73) then
           DONEI = 16
        else if (SPEC.eq.93) then
           DONEI = 41
        else if (SPEC.eq.108) then
           DONEI = 18
        else if (SPEC.eq.119) then
           DONEI = 7
        else if (SPEC.eq.122) then
           DONEi = 32
        else if (SPEC.eq.202) then
           DONEI = 21
        else if (SPEC.eq.242) then
           DONEI = 22
        else if (SPEC.eq.263.or. SPEC.eq.264) then
           DONEI = 14
        end if

     end if

     if (DONEI.gt.0) then
        VOLEQ = EQNUMI(DONEI)
     else if (DONEF.gt.0) then
        VOLEQ = EQNUMF(DONEF)
     else
        !           No INGY, find Behre's hyperbola model
        LAST = 53

        FIRST = 1

        do 7, while (DONEF.eq.0)
        HALF = (LAST - FIRST +1)/2 + FIRST
        if (FIA(HALF) .eq. SPEC) then
           DONEF = HALF
        else if (FIRST .eq. LAST) then
           ERRFLAG = 1
           DONEF = -1
        else if (FIA(HALF) .lt. SPEC) then
           FIRST = HALF
        else
           LAST = HALF - 1
        end if
        7 continue

        if (DONEF .lt.0) then
           VOLEQ = '616BEHW000'
        else
           if (FIA(DONEF) .lt.10) then
              write(ASPEC,'(I1)')FIA(DONEF)
              VOLEQ(1:9) = '616BEHW00'
              VOLEQ(10:10) = ASPEC
           else if (FIA(DONEF) .lt.100) then
              write(ASPEC,'(I2)')FIA(DONEF)
              VOLEQ(1:8) = '616BEHW0'
              VOLEQ(9:10) = ASPEC
           else
              write(ASPEC,'(I3)')FIA(DONEF)
              VOLEQ(1:7) = '616BEHW'
              VOLEQ(8:10) = ASPEC
           end if
        end if
     end if

  end if
  return
end

  !//////////////////////////////////////////////////////////////////
subroutine R7_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: VAR
  character(len=2) :: FORST
  integer :: SPEC,ERRFLAG,FORNUM
  character(len=10) :: EQNUM(45)
  character(len=10) :: EQNUMI(60)
  character(len=10) :: EQNUMC(33)
  character(len=10) :: EQNUMF(27)
  character(len=10) :: EQNUMD(8)
  integer :: FIA(41),FIRST,HALF,LAST,DONE,I

  !     SPECIES
  !     Pacific. silver fir,White fir,       Grand fir,        Subalpine fir,    California red fir,
  !     Shasta red fir,     Noble fir,       Port Orford cedar,Alaska cedar,     Western juniper,
  !     Subalpine larch,    Western larch,   Incense cedar,    Engelmann spruce, Sitka spruce,
  !     Whitebark pine,     Knobcone pine,   Lodgepole pine,   Jeffery pine,     Sugar pine,
  !     Western white pine, Ponderosa pine,  Douglas fir,      Redwood,          Pacific yew,
  !     Western red cedar,  Hemlock,         Western Hemlock,  Mountain Hemlock, Big leaf maple,
  !     Red alder,          White alder,     Pacific madrone,  Golden chinquapin,Oregon ash,
  !     Tanoak,             Black cottonwood,Oak species,      Oregon White Oak, Oregon myrtle,
  !     Other species,

  data (FIA(I),I = 1,41)/  11,   15,   17,   19,   20,&
       &21,   22,   41,   42,   64,&
       &72,   73,   81,   93,   98,&
       &101,  103,  108,  116,  117,&
       &119,  122,  202,  211,  231,&
       &242,  260,  263,  264,  312,&
       &351,  352,  361,  431,  542,&
       &631,  747,  800,  815,  981,&
       &999/

  data (EQNUM(I),I = 1,45)/&
       &'B00BEHW011','B00BEHW015','B00BEHW017','B00BEHW015','B00BEHW021',&
       &'B00BEHW021','B00BEHW022','B00BEHW041','B00BEHW042','B00BEHW242',&
       &'B00BEHW073','B00BEHW073','B00BEHW081','B00BEHW093','B00BEHW098',&
       &'B00BEHW119','B00BEHW108','B00BEHW108','B00BEHW116','B00BEHW117',&
       &'B00BEHW119','B00BEHW122','B01BEHW202','B00BEHW211','B00BEHW231',&
       &'B00BEHW242','B00BEHW260','B00BEHW260','B00BEHW260','B00BEHW312',&
       &'B00BEHW351','B00BEHW361','B00BEHW361','B00BEHW431','B00BEHW542',&
       &'B00BEHW631','B00BEHW747','B00BEHW800','B00BEHW800','B00BEHW998',&
       &'B00BEHW999',&
       &'B02BEHW202','B03BEHW202','B01BEHW202','B00BEHW263'/

  !  REGION 6 EQUATIONS

  data (EQNUMI(I),I = 1,60)/&
       &'I00FW2W012','I00FW2W017','I00FW2W019','I00FW2W073','I00FW2W093',&
       &'I00FW2W108','I00FW2W119','I00FW2W122','I00FW2W202','I00FW2W242',&
       &'I00FW2W260','I00FW2W263','I11FW2W012','I11FW2W017','I11FW2W019',&
       &'I11FW2W073','I11FW2W093','I11FW2W108','I11FW2W119','I11FW2W122',&
       &'I11FW2W202','I11FW2W242','I11FW2W260','I11FW2W263','I12FW2W012',&
       &'I12FW2W017','I12FW2W019','I12FW2W073','I12FW2W093','I12FW2W108',&
       &'I12FW2W119','I12FW2W122','I12FW2W202','I12FW2W242','I12FW2W260',&
       &'I12FW2W263','I13FW2W012','I13FW2W017','I13FW2W019','I13FW2W073',&
       &'I13FW2W093','I13FW2W108','I13FW2W119','I13FW2W122','I13FW2W202',&
       &'I13FW2W242','I13FW2W260','I13FW2W263','I14FW2W012','I14FW2W017',&
       &'I14FW2W019','I14FW2W073','I14FW2W093','I14FW2W108','I14FW2W119',&
       &'I14FW2W122','I14FW2W202','I14FW2W242','I14FW2W260','I14FW2W263'/

  data (EQNUMF(I),I = 1,27)/&
       &'F00FW2W202','F00FW2W242','F00FW2W263','F01FW2W202','F01FW2W242',&
       &'F01FW2W263','F02FW2W202','F02FW2W242','F02FW2W263','F03FW2W202',&
       &'F03FW2W242','F03FW2W263','F04FW2W202','F04FW2W242','F04FW2W263',&
       &'F05FW2W202','F05FW2W242','F05FW2W263','F06FW2W202','F06FW2W242',&
       &'F06FW2W263','F07FW2W202','F07FW2W242','F07FW2W263','F08FW2W202',&
       &'F08FW2W242','F08FW2W263'/

  data (EQNUMC(I),I = 1,33)/&
       &'I21FW2W012','I21FW2W017','I21FW2W019','I21FW2W073','I21FW2W093',&
       &'I21FW2W108','I21FW2W119','I21FW2W122','I21FW2W202','I21FW2W242',&
       &'I21FW2W260','I22FW2W012','I22FW2W017','I22FW2W019','I22FW2W073',&
       &'I22FW2W093','I22FW2W108','I22FW2W119','I22FW2W122','I22FW2W202',&
       &'I22FW2W242','I22FW2W260','I23FW2W012','I23FW2W017','I23FW2W019',&
       &'I23FW2W073','I23FW2W093','I23FW2W108','I23FW2W119','I23FW2W122',&
       &'I23FW2W202','I23FW2W242','I23FW2W260'/

  data (EQNUMD(I),I = 1,8)/&
       &'601DVEW205','601DVEW263','601DVEW015','602DVEW122','602DVEW204',&
       &'602DVEW015','602DVEW108','602DVEW122'/

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     if (VOLEQ(1:7).eq.'616BEHW') then

        !  FOUND VALID WESTSIDE EQUATION NUMBER

        SPEC = 8888
        return
     end if

     if (VOLEQ(1:7).eq.'632BEHW') then

        !  FOUND VALID EASTSIDE EQUATION NUMBER

        SPEC = 8888
        return
     end if
     do I = 1,44
        if (VOLEQ.eq.EQNUM(I)) then

           !  FOUND VALID EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do
     do I = 1,60
        if (VOLEQ.eq.EQNUMI(I)) then

           !  FOUND VALID INGY EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do

     do I = 1,27
        if (VOLEQ.eq.EQNUMF(I)) then

           !  FOUND VALID WESTSIDE FLEWELLING EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do

     do I = 1,33
        if (VOLEQ.eq.EQNUMC(I)) then

           !  FOUND VALID INGY EQUATION NUMBER - CANADIAN MODEL

           SPEC = 8888
           return
        end if
     end do

     do I = 1,8
        if (VOLEQ.eq.EQNUMD(I)) then

           !  FOUND VALID DIRECT VOLUME ESTIMATORS

           SPEC = 8888
           return
        end if
     end do
     return
  end if

  read(FORST,'(I2)')FORNUM
  DONE = 0

  LAST = 41

  FIRST = 1
  if (SPEC.eq.202) then
     if (VAR.eq.'WC' .or. VAR.eq.'wc') then
        DONE = 44
     else if ((VAR.eq.'PN') .or. (VAR.eq.'pn') .or.&
          &(VAR.eq.'NC') .or. (VAR.eq.'nc') .or.&
          &(VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc') .or.&
          &(VAR.eq.'OP') .or. (VAR.eq.'op')) then
        DONE = 44
        if (FORNUM.eq.12)DONE = 42
     else
        DONE = 44
     end if
  else if (SPEC.eq.41) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc')) then
        DONE = 13
     end if
  else if (SPEC.eq.263) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'PN') .or. (VAR.eq.'pn') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc') .or.&
          &(VAR.eq.'OP') .or. (VAR.eq.'op')) then
        DONE = 45
     end if
  else if ((SPEC.eq.109).or.(SPEC.eq.113).or.(SPEC.eq.124).or.&
       &(SPEC.eq.127)) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc')) then
        DONE = 18
     end if
  else if (SPEC.eq.92) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc')) then
        DONE = 14
     end if
  else if (SPEC.eq.212) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc')) then
        DONE = 24
     end if
  else if ((SPEC.eq.801).or.(SPEC.eq.805).or.(SPEC.eq.807).or.&
       &(SPEC.eq.811).or.(SPEC.eq.818).or.(SPEC.eq.821).or.&
       &(SPEC.eq.839).or.(SPEC.eq.333).or.(SPEC.eq.730)) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc')) then
        DONE = 38
     else if ((VAR.eq.'NC').or.(VAR.eq.'nc')) then
        if (SPEC.eq.818)DONE = 38
     end if
  else if (SPEC.eq.542) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc')) then
        DONE = 30
     end if
  else if (SPEC.eq.251) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc')) then
        DONE = 25
     end if
  else if (SPEC.eq.981) then
     if ((VAR.eq.'CA') .or. (VAR.eq.'ca') .or.&
          &(VAR.eq.'OC') .or. (VAR.eq.'oc')) then
        DONE = 36
     end if
  end if

  !  FINDS SPECIES INDEX TO FIA ARRAY

  do 5, while (DONE.eq.0)
  HALF = (LAST - FIRST +1)/2 + FIRST
  if (FIA(HALF) .eq. SPEC) then
     DONE = HALF
  else if (FIRST .eq. LAST) then
     ERRFLAG = 1
     DONE = -1
  else if (FIA(HALF) .lt. SPEC) then
     FIRST = HALF
  else
     LAST = HALF - 1
  end if
  5 continue

  if (DONE .lt.0) then
     VOLEQ = 'B00BEHW999'
  else
     VOLEQ = EQNUM(DONE)
  end if
  return
end
  !//////////////////////////////////////////////////////////////////
subroutine R8_BEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
  ! Variables
  character(len=2) :: GEOAREA
  character(len=2) :: GEOCODES(33)
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=10) :: VOLEQ
  character(len=10) :: VEQTEM
  integer :: SPEC,ERRFLAG,FORNUM,DISTNUM,FIRST,HALF,LAST,DONE,I,J
  character(len=3) :: SNSP(92)
  integer :: SNFIA(92)

  !     match species to valid species equation code
  data (SNFIA(I),I = 1,92)/&
       &10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,&
       &129, 130, 131, 132, 221, 222, 260, 299, 311, 313,&
       &316, 317, 318, 330, 370, 372, 391, 400, 450, 460,&
       &471, 491, 521, 531, 540, 541, 543, 544, 552, 555,&
       &580, 591, 601, 602, 611, 621, 650, 651, 652, 653,&
       &654, 660, 680, 691, 693, 694, 701, 711, 721, 731,&
       &740, 743, 762, 802, 806, 812, 813, 819, 820, 822,&
       &824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838,&
       &901, 920, 931, 950, 970, 971, 972, 975, 998, 999/

  data (SNSP(I), I = 1,92)/&
       &'261','100','115','132','110','111','115','121','126','126',&
       &'128',&
       &'129','132','131','132','221','222','261','132','500','500',&
       &'316','300','500','330','370','370','370','400','300','460',&
       &'300','300','500','531','541','541','300','544','500','300',&
       &'300','300','500','500','611','621','652','300','652','653',&
       &'300','300','300','300','693','694','500','300','300','731',&
       &'300','300','300','802','806','812','813','800','800','822',&
       &'800','800','800','827','827','832','833','800','835','800',&
       &'835',&
       &'901','300','300','300','970','970','970','970','300','300'/

  data (GEOCODES(I), I = 1,33)/&
       &'01','02','03','04','05','06','07','08','09','10',&
       &'11','12','13','14','15','16','17','18','19','20',&
       &'21','22','23','24','25','26','27','28','29','30',&
       &'31','32','33'/

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     VEQTEM(1:1) = '8'
     VEQTEM(4:7) = 'DVEE'
     do I = 1,33
        VEQTEM(2:3) = GEOCODES(I)
        !  SN
        do J = 1,92
           VEQTEM(8:10) = SNSP(J)
           if (VOLEQ.eq.VEQTEM) then
              SPEC = 8888
              return
           end if
        end do
     end do
     return
  end if

  read(FORST,'(I2)')FORNUM
  read(DIST,'(I2)')DISTNUM

  if (FORNUM.eq.1) then
     !              // alabama
     if (DISTNUM.eq.1) then
        GEOAREA = '13';
     else if (DISTNUM.eq.3) then
        GEOAREA = '15';
     else if (DISTNUM.eq.4) then
        GEOAREA = '16';
     else if (DISTNUM.eq.5.or. DISTNUM.eq.6) then
        GEOAREA = '14';
     else
        GEOAREA = '17';
     end if

  else if (FORNUM.eq.2) then
     !                     // daniel boone
     GEOAREA = '10';

  else if (FORNUM.eq.3) then
     !                     // Chattahoochee/Oconee
     GEOAREA = '01';
     if (DISTNUM .eq.8) GEOAREA = '06';

  else if (FORNUM.eq.4.or. FORNUM .eq.60) then
     !                     // Cherokee
     !                      // land between the lakes
     GEOAREA = '01';

  else if (FORNUM.eq.5) then
     !                     // Florida
     GEOAREA = '03';

  else if (FORNUM.eq.6) then
     !                     // Kisatchie
     GEOAREA = '02';
     if (DISTNUM .eq.6) GEOAREA = '09';

  else if (FORNUM .eq.7) then
     !                     // Mississippi

     if (DISTNUM.eq.1) then
        GEOAREA = '19';
     else if (DISTNUM.eq.2) then
        GEOAREA = '20';
     else if (DISTNUM.eq.4) then
        GEOAREA = '21';
     else if (DISTNUM.eq.5) then
        GEOAREA = '22';
     else if (DISTNUM.eq.6) then
        GEOAREA = '18';
     else
        GEOAREA = '23';
     end if

  else if (FORNUM .eq.8) then
     !                     // GW/Jeff
     GEOAREA = '11';

     if (DISTNUM.eq.11.or. DISTNUM.eq.12.or. DISTNUM.eq.13.or.&
          &DISTNUM.eq.14.or. DISTNUM.eq.15.or. DISTNUM.eq.16) then
        GEOAREA = '12';
     end if

  else if (FORNUM .eq.9) then
     !                     // Ouachita
     GEOAREA = '31';
     if (DISTNUM.eq.1.or. DISTNUM.eq.6) then
        GEOAREA = '30';
     else if (DISTNUM.eq.12) then
        GEOAREA = '32';
     end if

  else if (FORNUM .eq.10) then
     !                     // Ozark/St Francis
     GEOAREA = '04';
     if (DISTNUM .eq.7) GEOAREA = '05';

  else if (FORNUM .eq.11) then
     !                     // North Carolina
     GEOAREA = '01';
     if (DISTNUM .eq.3) then
        GEOAREA = '07';
     else if (DISTNUM .eq.10) then
        GEOAREA = '08';
     end if

  else if (FORNUM .eq.12) then
     !                     // Francis Marion/Sumpter
     GEOAREA = '24';
     if (DISTNUM .eq.2) then
        GEOAREA = '01';
     else if (DISTNUM .eq.5) then
        GEOAREA = '25';
     end if

  else if (FORNUM.eq.13) then
     !                      // Texas
     if (DISTNUM .eq.1) then
        GEOAREA = '26';
     else if (DISTNUM .eq.3) then
        GEOAREA = '27';
     else if (DISTNUM .eq.4) then
        GEOAREA = '29';
     else
        GEOAREA = '28';
     end if

  else if (FORNUM.eq.36) then
     !            // savannah river
     GEOAREA = '25';

  end if

  !     CREATE THE VOLUME EQUATION NUMBER

  VOLEQ(1:1) = '8'
  VOLEQ(2:3) = GEOAREA
  VOLEQ(4:7) = 'DVEE'

  !     FIND CORRECT SPECIES
  DONE = 0
  FIRST = 1
  LAST = 92
  do 5, while (DONE.eq.0)
  HALF = (LAST - FIRST +1)/2 + FIRST
  if (SNFIA(HALF) .eq. SPEC) then
     DONE = HALF
  else if (FIRST .eq. LAST) then
     ERRFLAG = 1
     DONE = -1
  else if (SNFIA(HALF) .lt. SPEC) then
     FIRST = HALF
  else
     LAST = HALF - 1
  end if
  5 continue
  if (DONE .lt.0) then
     if (SPEC.lt.300) then
        ! USE OTHER SOFTWOOD 299
        DONE = 19
     else
        DONE = 92
     end if
  end if

  VOLEQ(8:10) = SNSP(DONE)
  return
end
  !//////////////////////////////////////////////////////////////////
subroutine R8_CEQN(FORST,DIST,SPEC,PROD,VAR,VOLEQ,ERRFLAG)
  ! Variables
  character(len=1) :: GEOAREA
  character(len=1) :: TOPCODE(5)
  character(len=1) :: ICHAR
  character(len=2) :: PROD
  character(len=2) :: VAR
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=10) :: VOLEQ
  character(len=10) :: VEQTEM
  character(len=3) :: SNSP(92)
  integer :: SNFIA(92)
  integer :: SPEC,ERRFLAG,FORNUM,DISTNUM,FIRST,HALF,LAST,DONE,I,J,K

  !     match species to valid species equation code
  data (SNFIA(I),I = 1,92)/&
       &10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,&
       &129, 130, 131, 132, 221, 222, 260, 299, 311, 313,&
       &316, 317, 318, 330, 370, 372, 391, 400, 450, 460,&
       &471, 491, 521, 531, 540, 541, 543, 544, 552, 555,&
       &580, 591, 601, 602, 611, 621, 650, 651, 652, 653,&
       &654, 660, 680, 691, 693, 694, 701, 711, 721, 731,&
       &740, 743, 762, 802, 806, 812, 813, 819, 820, 822,&
       &824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838,&
       &901, 920, 931, 950, 970, 971, 972, 975, 998, 999/

  data (SNSP(I), I = 1,92)/&
       &'261','100','115','132','110','111','115','121','126','126',&
       &'128',&
       &'129','132','131','132','221','222','261','132','500','500',&
       &'316','300','500','330','370','370','370','400','300','460',&
       &'300','300','500','531','541','541','300','544','500','300',&
       &'300','300','500','500','611','621','652','300','652','653',&
       &'300','300','300','300','693','694','500','300','300','731',&
       &'300','300','300','802','806','812','813','800','800','822',&
       &'800','800','800','827','827','832','833','800','835','800',&
       &'835',&
       &'901','300','300','300','970','970','970','970','300','300'/

  data TOPCODE / '1','4','7','8','9' /

  !  SEARCH FOR VALID EQUATION NUMBER

  if (SPEC.eq.9999) then
     VEQTEM(1:1) = '8'
     VEQTEM(4:7) = 'CLKE'
     do I = 1,7
        write(ICHAR,'(I1)')I
        VEQTEM(2:2) = ICHAR
        do J = 1,5
           VEQTEM(3:3) = TOPCODE(J)
           !  SN
           do K = 1,92
              VEQTEM(8:10) = SNSP(K)
              if (VOLEQ.eq.VEQTEM) then
                 SPEC = 8888
                 return
              end if
           end do
        end do
     end do
     return
  end if

  !     FIND CORRECT FOREST
  read(FORST,'(I2)')FORNUM
  read(DIST,'(I2)')DISTNUM

  !     SET GEOCODE
  !     SET DIAMETER BY PRODUCT

  if (FORNUM.eq.1) then
     !             // alabama
     GEOAREA = '4';
     if (DISTNUM.eq.3) GEOAREA = '1'

  else if (FORNUM.eq.2.or. FORNUM.eq.4.or. FORNUM .eq.8.or.&
       &FORNUM .eq.60) then
     !                     // daniel boone
     !                     // Cherokee
     !                     // GW/Jeff
     !                 // land between the lakes
     GEOAREA = '3';

  else if (FORNUM.eq.3) then
     !                     // Chattahoochee/Oconee
     GEOAREA = '3';
     if (DISTNUM .eq.8) GEOAREA = '2';

  else if (FORNUM.eq.5.or. FORNUM.eq.36) then
     !                     // Florida
     !           // savannah river
     GEOAREA = '1';

  else if (FORNUM.eq.6.or. FORNUM.eq.13) then
     !                     // Kisatchie
     !                     // Texas
     GEOAREA = '5';

  else if (FORNUM .eq.7) then
     !                     // Mississippi
     GEOAREA = '5';

     if (DISTNUM .eq.6) then
        GEOAREA = '7';
     else if (DISTNUM.eq.7.or. DISTNUM.eq.17) then
        GEOAREA = '4';
     end if
  else if (FORNUM .eq.9) then
     !                     // Ouachita
     GEOAREA = '6';
  else if (FORNUM .eq.10) then
     !                     // Ozark/St Francis
     GEOAREA = '6';
     if (DISTNUM .eq.7) GEOAREA = '7';

  else if (FORNUM .eq.11) then
     !              // North Carolina
     GEOAREA = '3';
     if (DISTNUM .eq.3) then
        GEOAREA = '1';
     else if (DISTNUM .eq.10) then
        GEOAREA = '2';
     end if

  else if (FORNUM .eq.12) then
     !                     // Francis Marion/Sumpter
     GEOAREA = '2';
     if (DISTNUM .eq.2) then
        GEOAREA = '3';
     else if (DISTNUM .eq.5) then
        GEOAREA = '1';
     end if
  end if

  !     CREATE THE VOLUME EQUATION NUMBER
  VOLEQ(1:1) = '8'
  VOLEQ(2:2) = GEOAREA
  !     Changed to use New Clark equation 8*1CLKE*** (YW 20210719)
  !      IF(PROD.EQ.'01')THEN
  !         IF(SPEC.LT.300) THEN
  !           7 INCH TOP
  !            VOLEQ(3:3) = '7'
  !          ELSE
  !           9 INCH TOP
  !            VOLEQ(3:3) = '9'
  !          ENDIF
  !       ELSEIF (PROD.EQ.'08') THEN
  !           USE PRODUCT 08 LOGIC
  !          VOLEQ(3:3) = '8'
  !       ELSE
  !           4 INCH TOP
  !         VOLEQ(3:3) = '4'
  !       ENDIF
  if (PROD.eq.'08') then
     !           USE PRODUCT 08 LOGIC
     VOLEQ(3:3) = '8'
  else
     VOLEQ(3:3) = '1'
  end if
  VOLEQ(4:7) = 'CLKE'

  !     FIND CORRECT SPECIES
  DONE = 0
  FIRST = 1
  LAST = 92
  do 5, while (DONE.eq.0)
  HALF = (LAST - FIRST +1)/2 + FIRST
  if (SNFIA(HALF) .eq. SPEC) then
     DONE = HALF
  else if (FIRST .eq. LAST) then
     ERRFLAG = 1
     DONE = -1
  else if (SNFIA(HALF) .lt. SPEC) then
     FIRST = HALF
  else
     LAST = HALF - 1
  end if
  5 continue
  if (DONE .lt.0) then
     if (SPEC.lt.300) then
        ! USE OTHER SOFTWOOD 299
        DONE = 19
     else
        DONE = 92
     end if
  end if

  VOLEQ(8:10) = SNSP(DONE)

  return
end

  !//////////////////////////////////////////////////////////////////
subroutine R9_EQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=10) :: VEQTEM
  character(len=2) :: GEOAREA
  character(len=2) :: VAR
  character(len=2) :: FORST
  character(len=2) :: GEOCODES(6)
  character(len=3) :: LSSP(69)
  character(len=3) :: CSSP(97)
  character(len=3) :: NESP(108)
  character(len=3) :: SNSP(92)
  character(len=3) :: ASPEC
  integer :: SPEC,ERRFLAG,FORNUM,LSFIA(69),CSFIA(97),NEFIA(108),SNFIA(92)
  integer :: FIRST,HALF,LAST,DONE,I,J

  data (LSSP(I), I = 1,69)/&
       &'012','068','071','091','094','095','105','125','125','129',&
       &'130','241','261','001','313','314','315','316','317','318',&
       &'319','371','375','391','402','403','407','421','462','491',&
       &'500','531','541','543','544','601','602','660','693','701',&
       &'731','741','742','743','746','766','761','762','763','766',&
       &'802','804','809','823','826','833','837','901','920','922',&
       &'923','931','935','951','972','975','977','300','999'/

  data (CSSP(I), I = 1,97)/&
       &'068','068','110','129','131','132','221','001','316','316',&
       &'316','318','331','373','391','400','401','402','403','404',&
       &'405','407','408','409','400','450','460','460','471','490',&
       &'500','521','531','541','541','543','541','543','543','552',&
       &'571','601','602','611','621','641','651','653','680','690',&
       &'691','693','694','701','711','731','742','741','742','746',&
       &'746','762','802','823','806','812','813','813','813','823',&
       &'824','823','823','816','813','830','813','832','833','813',&
       &'835','835','837','901','920','920','931','951','970','970',&
       &'970','970','970','970','999','999','999'/

  data (NESP(I), I = 1,108)/&
       &'012','068','068','068','071','097','097','094','095','097',&
       &'105','105','110','105','125','105','105','129','105','131',&
       &'125','068','261','261','105','313','318','315','316','318',&
       &'318','330','332','341','355','371','371','373','374','375',&
       &'375','391','400','400','400','400','400','462','490','500',&
       &'521','531','541','541','543','541','541','591','601','602',&
       &'611','621','641','650','651','653','660','691','693','701',&
       &'711','712','731','741','742','746','742','746','760','761',&
       &'762','802','802','832','806','833','813','806','823','832',&
       &'802','806','830','831','832','833','835','837','901','920',&
       &'931','951','951','970','970','970','999','004'/

  data (SNSP(I), I = 1,92)/&
       &'261','100','115','132','110','111','115','121','126','126',&
       &'128',&
       &'129','132','131','132','221','222','261','132','500','500',&
       &'316','300','500','330','370','370','370','400','300','460',&
       &'300','300','500','531','541','541','300','544','500','300',&
       &'300','300','500','500','611','621','652','300','652','653',&
       &'300','300','300','300','693','694','500','300','300','731',&
       &'300','300','300','802','806','812','813','800','800','822',&
       &'800','800','800','827','827','832','833','800','835','800',&
       &'835',&
       &'901','300','300','300','970','970','970','970','300','300'/

  data (LSFIA(I), I = 1,69)/&
       &12,  68,  71,   91,  94,  95, 105, 125, 125, 129,&
       &130, 241, 261, 299, 313, 314, 315, 316, 317, 318,&
       &319, 371, 375, 391, 402, 403, 407, 421, 462, 491,&
       &500, 531, 541, 543, 544, 601, 602, 660, 693, 701,&
       &731, 741, 742, 743, 746, 760, 761, 762, 763, 766,&
       &802, 804, 809, 823, 826, 833, 837, 901, 920, 922,&
       &923, 931, 935, 951, 972, 975, 977, 993, 994/

  data (CSFIA(I), I = 1,97)/&
       &57,  68, 110, 129, 131, 132, 221, 299, 313, 316,&
       &317, 318, 331, 373, 391, 400, 401, 402, 403, 404,&
       &405, 407, 408, 409, 410, 450, 461, 462, 471, 491,&
       &500, 521, 531, 540, 541, 543, 544, 545, 546, 552,&
       &571, 601, 602, 611, 621, 641, 651, 653, 680, 690,&
       &691, 693, 694, 701, 711, 731, 740, 741, 742, 743,&
       &746, 762, 802, 804, 806, 812, 813, 817, 822, 823,&
       &824, 825, 826, 827, 828, 830, 831, 832, 833, 834,&
       &835, 836, 837, 901, 920, 922, 931, 951, 970, 971,&
       &972, 974, 975, 977, 991, 992, 994/

  data (NEFIA(I), I = 1,108)/&
       &12,  43,  57,  68,  71,  90,  91,  94,  95,  97,&
       &100, 105, 110, 123, 125, 126, 128, 129, 130, 131,&
       &132, 241, 260, 261, 299, 313, 314, 315, 316, 317,&
       &318, 330, 332, 341, 356, 371, 372, 373, 374, 375,&
       &379, 391, 400, 403, 405, 407, 409, 462, 491, 500,&
       &521, 531, 540, 541, 543, 544, 545, 591, 601, 602,&
       &611, 621, 641, 650, 651, 653, 660, 691, 693, 701,&
       &711, 712, 731, 741, 742, 743, 744, 746, 760, 761,&
       &762, 800, 802, 804, 806, 812, 813, 817, 823, 825,&
       &826, 827, 830, 831, 832, 833, 835, 837, 901, 922,&
       &931, 951, 952, 970, 972, 975, 994, 998/

  data (SNFIA(I),I = 1,92)/&
       &10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,&
       &129, 130, 131, 132, 221, 222, 260, 299, 311, 313,&
       &316, 317, 318, 330, 370, 372, 391, 400, 450, 460,&
       &471, 491, 521, 531, 540, 541, 543, 544, 552, 555,&
       &580, 591, 601, 602, 611, 621, 650, 651, 652, 653,&
       &654, 660, 680, 691, 693, 694, 701, 711, 721, 731,&
       &740, 743, 762, 802, 806, 812, 813, 819, 820, 822,&
       &824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838,&
       &901, 920, 931, 950, 970, 971, 972, 975, 998, 999/

  !  SEARCH FOR VALID EQUATION NUMBER
  !  FIRST, SEARCH FOR CLKE OR DVEE

  if (SPEC.eq.9999) then
     if ((VOLEQ(1:7).eq.'900CLKE').or.(VOLEQ(1:7).eq.'900DVEE')) then
        !  LS
        do J = 1,69
           if (VOLEQ(8:10).eq.LSSP(J)) then
              SPEC = 8888
              return
           end if
        end do
        !  CS
        do J = 1,97
           if (VOLEQ(8:10).eq.CSSP(J)) then
              SPEC = 8888
              return
           end if
        end do
        !  NE
        do J = 1,108
           if (VOLEQ(8:10).eq.NESP(J)) then
              SPEC = 8888
              return
           end if
        end do
        !  SN
        do J = 1,92
           if (VOLEQ(8:10).eq.SNSP(J)) then
              SPEC = 8888
              return
           end if
        end do
        return
     else
        !     NOT A VALID REGION 9 EQUATION
        return
     end if
  end if

  if (VOLEQ(1:7).eq.'900CLKE') then
     !     NEW CLARK'S PROFILE MODEL VOLUME EQUATION NUMBERS
     !     MAKE SURE SPEC IS A 3 CHARACTER FIELD.

     write(ASPEC,'(I3)')SPEC

     if (SPEC .lt.10) then
        ASPEC(1:2) = '00'
     else if (SPEC.lt.100) then
        ASPEC(1:1) = '0'
     end if

     VOLEQ(8:10) = ASPEC

     return
  else if (VOLEQ(1:7).eq.'900DVEE') then

     !     DIRECT VOLUME ESTIMATORS
     !     FIND CORRECT SPECIES
     FIRST = 1
     DONE = 0
     if (VAR.eq.'LS' .or. VAR.eq.'ls') then
        LAST = 69
        do 5, while (DONE.eq.0)
        HALF = (LAST - FIRST +1)/2 + FIRST
        if (LSFIA(HALF) .eq. SPEC) then
           DONE = HALF
        else if (FIRST .eq. LAST) then
           ERRFLAG = 1
           DONE = -1
        else if (LSFIA(HALF) .lt. SPEC) then
           FIRST = HALF
        else
           LAST = HALF - 1
        end if
        5 continue
        if (DONE .lt.0) DONE = 69

        VOLEQ(8:10) = LSSP(DONE)
     else if (VAR.eq.'CS' .or. VAR.eq.'cs') then
        LAST = 97
        do 15, while (DONE.eq.0)
        HALF = (LAST - FIRST +1)/2 + FIRST
        if (CSFIA(HALF) .eq. SPEC) then
           DONE = HALF
        else if (FIRST .eq. LAST) then
           ERRFLAG = 1
           DONE = -1
        else if (CSFIA(HALF) .lt. SPEC) then
           FIRST = HALF
        else
           LAST = HALF - 1
        end if
        15 continue
        if (DONE .lt.0) DONE = 97
        VOLEQ(8:10) = CSSP(DONE)
     else if (VAR.eq.'NE' .or. VAR.eq.'ne') then
        LAST = 108
        do 25, while (DONE.eq.0)
        HALF = (LAST - FIRST +1)/2 + FIRST
        if (NEFIA(HALF) .eq. SPEC) then
           DONE = HALF
        else if (FIRST .eq. LAST) then
           ERRFLAG = 1
           DONE = -1
        else if (NEFIA(HALF) .lt. SPEC) then
           FIRST = HALF
        else
           LAST = HALF - 1
        end if
        25 continue
        if (DONE .lt.0) DONE = 108
        VOLEQ(8:10) = NESP(DONE)
     else

        !  SOUTHERN VARIANT RUNNING REGION 9 FORESTS AND GEVORKIANTZ METH=5

        LAST = 92
        do 35, while (DONE.eq.0)
        HALF = (LAST - FIRST +1)/2 + FIRST
        if (SNFIA(HALF) .eq. SPEC) then
           DONE = HALF
        else if (FIRST .eq. LAST) then
           ERRFLAG = 1
           DONE = -1
        else if (SNFIA(HALF) .lt. SPEC) then
           FIRST = HALF
        else
           LAST = HALF - 1
        end if
        35 continue
        if (DONE .lt.0) DONE = 92
        VOLEQ(8:10) = SNSP(DONE)
     end if
     return
  end if
  !     End FVS check equation
  !     NEW CLARK'S PROFILE MODEL VOLUME EQUATION NUMBERS
  !     MAKE SURE SPEC IS A 3 CHARACTER FIELD.

  VOLEQ(1:7) = '900CLKE'
  write(ASPEC,'(I3)')SPEC

  if (SPEC .lt.10) then
     ASPEC(1:2) = '00'
  else if (SPEC.lt.100) then
     ASPEC(1:1) = '0'
  end if

  VOLEQ(8:10) = ASPEC

  return
end
  !//////////////////////////////////////////////////////////////////
subroutine R10_EQN(FORST,SPEC,VOLEQ,ERRFLAG)
  ! Variables
  character(len=10) :: VOLEQ
  character(len=2) :: FORST
  integer :: SPEC,ERRFLAG,FORNUM
  character(len=10) :: TONEQN(23)
  character(len=10) :: CHUEQN(23)
  character(len=10) :: OTHEREQN(27)
  integer :: FIA(23),FIRST,HALF,LAST,DONE,I

  !     AK SPECIES LIST
  !     Pacific silver fir  Subalpine fir     Alaska yellow Cedar  Tamarack
  !     White spruce        Lutz's spruce     Black spruce         Sitka spurce
  !     Lodgepole pine      Western redcedar  Western hemlock      Mountain Hemlock
  !     Other Softwood      Alder species     Red alder            Paper birch
  !     Alaska birch        Balsam poplar     Quaking aspen        Black cottonwood
  !     Willow species      Scouler's willow  Other hardwood

  !     LUTZ SPRUCE IS TREATED AS FIA CODE 94 (WHITE SPRUCE)
  data FIA / 011, 019, 042, 071, 094,&
       &094, 095, 098, 108, 242,&
       &263, 264, 299, 350, 351,&
       &375, 376, 741, 746, 747,&
       &920, 928, 998/

  !     DEFAULT VOLUME EQUATIONS FOR LOCATION CODES: 703, 1002, 1003, 1005,
  !     8134, 8135, 8112
  data TONEQN /&
       &'A00F32W260','A00F32W260','A00F32W042','A00DVEW094','A00DVEW094',&
       &'A00DVEW094','A00DVEW094','A00F32W098','A00F32W260','A00F32W242',&
       &'A00F32W260','A00F32W260','A00DVEW094','A00DVEW747','A32CURW351',&
       &'A00DVEW375','A00DVEW375','A00DVEW747','A00DVEW375','A00DVEW747',&
       &'A00DVEW747','A00DVEW747','A00DVEW747'/

  !     DEFAULT VOLUME EQUATIONS FOR LOCATION CODES: 1004, 713, 720, 7400, 7401,
  !     7402, 7403, 7404, 7405, 7406, 7407, 7408
  data CHUEQN /&
       &'A01DEMW000','A01DEMW000','A00DVEW094','A00DVEW094','A00DVEW094',&
       &'A00DVEW094','A00DVEW094','A00F32W098','A01DEMW000','A01DEMW000',&
       &'A00F32W260','A01DEMW000','A00DVEW094','A00DVEW747','A32CURW351',&
       &'A00DVEW375','A00DVEW375','A00DVEW747','A00DVEW375','A00DVEW747',&
       &'A00DVEW747','A00DVEW747','A00DVEW747'/

  !     OTHER VALID EQUATIONS THAT CAN BE USED -- THESE ARE NOT DEFAULT
  !     EQUATION NUMBERS.
  data OTHEREQN /&
       &'A00FW2W042','A16DEMW042','A61DEMW042','A32DEMW042','A01DVEW094',&
       &'A02DVEW094','A16DEMW098','A00FW2W098','A02F32W098','A02FW2W098',&
       &'A02DEMW000','A32CURW000','A32DEMW098','A61DEMW098','A00FW2W242',&
       &'A61DEMW242','A16DEMW242','A32DEMW242','A00FW2W260','A02F32W260',&
       &'A02FW2W260','A16CURW260','A01DVEW375','A01DVEW747','A00DVEW108',&
       &'A00DVEW310','A00DVEW351'/

  !     SEARCH FOR VALID EQUATION NUMBER IN TONEQN AND CHUEQN
  if (SPEC.eq.9999) then
     do I = 1,23
        if ((VOLEQ.eq.TONEQN(I)).or.(VOLEQ.eq.CHUEQN(I))) then

           !     FOUND VALID EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do

     !     SEARCH FOR VALID EQUATION NUMBER IN OTHEREQN
     do I = 1,27
        if (VOLEQ.eq.OTHEREQN(I)) then

           !     FOUND VALID EQUATION NUMBER

           SPEC = 8888
           return
        end if
     end do
     return
  end if

  read(FORST,'(I2)')FORNUM
  LAST = 23
  FIRST = 1
  DONE = 0

  !     CHECK FOR VALID SPECIES
  do while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (FIA(HALF) .eq. SPEC) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        ERRFLAG = 1
        DONE = -1
     else if (FIA(HALF) .lt. SPEC) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
  end do

  !     IF SPECIES NOT FOUND, USE OTHER SOFTWOOD
  !     LUTZ SPRUCE WILL NEVER BE FOUND IN THE ABOVE LOOP SINCE
  !     IT DOES NOT HAVE A FIA CODE. AS SUCH IT WILL BE ASSIGNED
  !     THE EQUATION NUMBER FOR OTHER SOFTWOOD (INDEX 13).
  if (DONE .lt.0) DONE = 13

  !     DETERMINE DEFAULT EQUATION NUMBER TO USE BASED ON INDEX (DONE)
  !     AND FOREST NUMBER
  if (FORNUM .eq.4) then
     VOLEQ = CHUEQN(DONE)
  else
     VOLEQ = TONEQN(DONE)
  end if

  return
end

subroutine FIAVOLEQDEF(VAR,REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)

  !    SUBROUTINE WILL RETURN THE DEFAULT FIA VOLUME EQUATION NUMBER
  !        SPEC = 3 DIGIT FIA SPECIES CODE

  ! FIA species default equation
  ! species     common name           WOR     WWA    EOR    EWA     CA

  ! 11          Pacific silver fir     11     11     10     10     --
  ! 14          Bristlecone fir        --     --     --     --     18
  ! 15          White fir              23     --     10     --     23
  ! 17          Grand fir              11     11     10     10     23
  ! 19          Subalpine fir          11     11     10     10     18
  ! 20          California red fir     --     --     --     --     18
  ! 21          Shasta red fir         18     18     18     18     18
  ! 22          Noble fir              11     11     10     10     18
  ! 41          Port-Orford-cedar      19     19     19     19      8
  ! 42          Alaska-cedar            9      9      8      8      8
  ! 50          Cypress                --     --     --     --     19
  ! 51          Arizona cypress        --     --     --     --     19
  ! 56          McNabb cypress         --     --     --     --     19
  ! 62          California juniper     --     --     --     --     21
  ! 64          Western juniper        21     21     21     21     21
  ! 65          Utah juniper           --     --     --     --     21
  ! 72          Subalpine larch        --     22     --     22     --
  ! 73          Western larch          --     22     22     22     --
  ! 81          Incense cedar          19     19     19     19     19
  ! 92          Brewer spruce          13     --     13     --     12
  ! 93          Engelmann spruce       13     13     12     12     12
  ! 98          Sitka spruce           13     13     --     --     12
  ! 101          Whitebark pine        15     15     15     15     20
  ! 102          Bristlecone pine      --     --     --     --     16
  ! 103          Knobcone pine         15     --     15     --     16
  ! 104          Foxtail pine          --     --     --     --     16
  ! 108          Lodgepole pine        15     15     15     15     16
  ! 109          Coulter pine          --     --     --     --     5
  ! 113          Limber pine           15     --     15     --     16
  ! 116          Jeffrey pine           5     --      4     --     5
  ! 117          Sugar pine            20     20     20     20     20
  ! 119          Western white pine    15     15     15     15     20
  ! 120          Bishop pine           --     --     --     --     16
  ! 122          Ponderosa pine(>=5dbh) 5      4      4      4      5
  ! 122          Ponderosa pine(<5�dbh) 5      5      5      5      5
  ! 124          Monterey pine         --     --     --     --     16
  ! 127          Gray pine             --     --     --     --     5
  ! 130          Scotch pine           17     17     17     17     17
  ! 133          Singleleaf pinyon     --     --     --     --     21
  ! 137          Washoe pine           --     --     --     --     5
  ! 201          Bigcone Douglas-fir   --     --     --     --     3
  ! 202          Douglas-fir            1      1      2      2     3
  ! 211          Redwood               24     --     --     --     24
  ! 212          Giant Sequoia         24     --     --     --     24
  ! 231          Pacific yew            9      9      8      8     8
  ! 242          Western redcedar       9      9      8      8     8
  ! 251          California nutmeg     --     --     --     --     8
  ! 263          Western hemlock        6      6      6      6     6
  ! 264          Mountain hemlock      17     17     17     17     17
  ! 299          Unknown Conifer       17     17     17     17     17
  ! 312          Bigleaf maple         37     26     37     26     37
  ! 313          Boxelder              --     --     --     --     38
  ! 321          Rocky Mountain maple  --     --     --     --     --
  ! 322          Bigtooth maple        --     --     --     --     --
  ! 330          California buckeye    --     --     --     --     43
  ! 341          Tree of heaven        --     --     --     --     26
  ! 351          Red alder             26     25     26     25     26
  ! 352          White alder           26     --     26     --     26
  ! 361          Pacific madrone       40     26     40     26     40
  ! 374          Water birch           --     --     --     --     26
  ! 375          Paper birch           --     --     --     --     --
  ! 376          Western paper birch   --     26     --     26     --
  ! 431          Golden chinkapin      32     26     --     26     32
  ! 475 Curlleaf mountain-mahogany     --     --     45     --     45
  ! 492          Pacific dogwood       --     26     --     26     26
  ! 500          Hawthorn              --     --     --     --     --
  ! 510          Eucalyptus            26     --     --     --     31
  ! 542          Oregon ash            38     26     38     26     38
  ! 590          Holly                 26     26     26     26     26
  ! 600          Walnut                26     26     26     --     38
  ! 631          Tanoak                34     --     --     --     34
  ! 660          Apple                 26     26     26     26     42
  ! 730          California sycamore   26     26     26     26     42
  ! 740          Cottonwood and poplar --     --     --     --     --
  ! 741          Balsam poplar         --     --     --     --     --
  ! 742          Eastern cottonwood    --     --     --     --     --
  ! 745          Plains cottonwood     --     --     --     --     --
  ! 746          Quaking aspen         26     26     26     26     28
  ! 747          Black cottonwood      26     26     26     26     27
  ! 748          Fremont poplar        --     --     --     --     27
  ! 755          Mesquite              --     --     --     --     --
  ! 760          Cherry                26     26     26     26     26
  ! 800          Oak-deciduous         --     --     --     --     43
  ! 801          California live oak   --     --     --     --     43
  ! 805          Canyon live oak       42     --     --     --     42
  ! 807          Blue oak              --     --     --     --     39
  ! 810          Emory oak             --     --     --     --     --
  ! 811          Englemann oak         --     --     --     --     36
  ! 815          Oregon white oak      41     26     41     26     41
  ! 818          California black oak  38     --     38     26     38
  ! 821          California white oak  --     --     --     --     35
  ! 839          Interior live oak     --     --     --     --     44
  ! 901          Black locust          --     --     --     --     41
  ! 920          Willow                26     26     26     26     40
  ! 981          California-laurel     33     --     --     --     33
  ! 998          Unknown hardwood      26     26     26     26     41
  ! 999          Unknown Tree          26     26     26     26     41

  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=10) :: VOLEQ
  integer :: ERRFLAG,REGN,SPEC

  if (VAR .eq. '  ') then
     call GETVARIANT(REGN,FORST,DIST,VAR)
  end if
  if (REGN .eq.5) then
     call R5_PNWEQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  else if (REGN.eq.6) then
     call R6_PNWEQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  else
     ERRFLAG = 1
  end if

  return
end

  ! FIA DEFAULT VOLUME EQUATION FOR CA
subroutine R5_PNWEQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=10) :: VOLEQ
  character(len=10) :: EQNUM(91)
  integer :: ERRFLAG,SPEC,FIA(91)
  integer :: DONE,HALF,FIRST,LAST

  data (FIA(I),I = 1,91)/&
       &11, 14, 15, 17, 19, 20, 21, 22, 41, 42,&
       &50, 51, 56, 62, 64, 65, 72, 73, 81, 92,&
       &93, 98,101,102,103,104,108,109,113,116,&
       &117,119,120,122,124,127,130,133,137,142,&
       &201,202,211,212,231,242,251,263,264,299,&
       &301,312,313,330,333,341,351,352,361,374,&
       &431,475,478,492,510,542,590,600,631,660,&
       &671,730,746,747,748,760,768,800,801,805,&
       &807,811,815,818,821,839,901,920,981,998,&
       &999/
  data (EQNUM(I),I = 1,91)/&
       !        11,14, 15, 17, 19,
       &'616TRFW264','516TRFW021','516TRFW015','516TRFW015','516TRFW021',&
       !        20,21, 22, 41, 42,
       &'516TRFW021','516TRFW021','516TRFW021','616TRFW242','616TRFW242',&
       !        50,51, 56, 62, 64,
       &'516TRFW081','516TRFW081','516TRFW081','516TRFW060','516TRFW060',&
       !        65,72, 73,81, 92,
       &'516TRFW060','616TRFW073','616TRFW073','516TRFW081','616TRFW094',&
       !        93,98,101,102,103,
       &'616TRFW094','616TRFW094','516TRFW117','516TRFW108','516TRFW108',&
       !       104,108,109,113,116,
       &'516TRFW108','516TRFW108','516TRFW122','516TRFW108','516TRFW122',&
       !       117,119,120,122,124,
       &'516TRFW117','516TRFW117','516TRFW108','516TRFW122','516TRFW108',&
       !       127,130,133,137,142,
       &'516TRFW122','616TRFW264','516TRFW060','516TRFW122','616TRFW264',&
       !       201,202,211,212,231,
       &'516TRFW202','516TRFW202','616TRFW211','616TRFW211','616TRFW242',&
       !       242,251,263,264,299,
       &'616TRFW242','616TRFW242','616TRFW263','616TRFW264','616TRFW264',&
       !       301,312,313,330,333,
       &'500DVEW815','500DVEW312','500DVEW818','500DVEW801','500DVEW815',&
       !       341,351,352,361,374,
       &'500DVEW351','500DVEW351','500DVEW351','500DVEW361','500DVEW351',&
       !       431,475,478,492,510,
       &'500DVEW431','400DVEW475','500DVEW815','500DVEW351','616TRFW998',&
       !       542,590,600,631,660,
       &'500DVEW818','500DVEW351','500DVEW818','500DVEW631','500DVEW805',&
       !       671,730,746,747,748,
       &'500DVEW815','500DVEW805','616TRFW746','616TRFW747','616TRFW747',&
       !       760,768,800,801,805,
       &'500DVEW351','500DVEW815','500DVEW801','500DVEW801','500DVEW805',&
       !       807,811,815,818,821,
       &'500DVEW807','500DVEW811','500DVEW815','500DVEW818','500DVEW821',&
       !       839,901,920,981,998,
       &'500DVEW839','500DVEW815','500DVEW361','500DVEW981','500DVEW815',&
       !       999
       &'500DVEW815'/

  !     GET EQUATION FROM EQNUME ARRAY
  DONE = 0
  LAST = 91
  FIRST = 1
  do 55, while (DONE.eq.0)
  HALF = (LAST - FIRST +1)/2 + FIRST
  if (FIA(HALF) .eq. SPEC) then
     DONE = HALF
  else if (FIRST .eq. LAST) then
     ERRFLAG = 1
     DONE = -1
  else if (FIA(HALF) .lt. SPEC) then
     FIRST = HALF
  else
     LAST = HALF - 1
  end if
  55 continue

  if (DONE .lt.0) then
     VOLEQ = EQNUM(LAST)
  else
     VOLEQ = EQNUM(DONE)
  end if

  return
  end subroutine R5_PNWEQN

  ! FIA DEFAULT VOLUME EQUATION FOR WA AND OR
subroutine R6_PNWEQN(FORST,SPEC,VAR,VOLEQ,ERRFLAG)
  ! Variables
  character(len=2) :: FORST
  character(len=2) :: DIST
  character(len=2) :: VAR
  character(len=10) :: VOLEQ
  character(len=10) :: EQNUMW(66)
  character(len=10) :: EQNUME(66)
  integer :: ERRFLAG,SPEC,FIA(66)
  integer :: DONE,HALF,FIRST,LAST
  data (FIA(I),I = 1,66)/&
       &11, 15, 17, 19, 20, 21, 22, 41, 42, 64,&
       &66, 72, 73, 81, 92, 93, 98,101,103,106,&
       &108,113,116,117,119,122,130,202,211,212,&
       &231,242,263,264,299,312,321,351,352,361,&
       &375,376,431,475,478,492,500,510,542,590,&
       &600,631,660,730,740,746,747,760,768,805,&
       &815,818,920,981,998,999/
  data (EQNUMW(I),I = 1,66)/&
       !         11, 15, 17, 19, 20,
       &'632TRFW011','532TRFW015','632TRFW011','632TRFW011','532TRFW021',&
       !         21, 22, 41, 42, 64,
       &'532TRFW021','632TRFW011','532TRFW081','632TRFW242','532TRFW060',&
       !         66, 72, 73, 81, 92,
       &'532TRFW060','632TRFW073','632TRFW073','532TRFW081','632TRFW098',&
       !         93, 98,101,103,106,
       &'632TRFW098','632TRFW098','632TRFW108','632TRFW108','632TRFW264',&
       !        108,113,116,117,119,
       &'632TRFW108','632TRFW108','532TRFW122','532TRFW117','632TRFW108',&
       !        122,130,202,211,212,
       &'532TRFW122','632TRFW264','632TRFW202','632TRFW211','632TRFW211',&
       !        231,242,263,264,299,
       &'632TRFW242','632TRFW242','632TRFW263','632TRFW264','632TRFW264',&
       !        312,321,351,352,361,
       &'500DVEW312','500DVEW351','500DVEW351','500DVEW351','500DVEW361',&
       !        375,376,431,475,478,
       &'500DVEW351','500DVEW351','500DVEW431','400DVEW475','400DVEW475',&
       !        492,500,510,542,590,
       &'500DVEW351','500DVEW351','500DVEW351','500DVEW818','500DVEW351',&
       !        600,631,660,730,740,
       &'500DVEW351','500DVEW631','500DVEW351','500DVEW351','500DVEW351',&
       !        746,747,760,768,805,
       &'500DVEW351','500DVEW351','500DVEW351','500DVEW351','500DVEW805',&
       !        815,818,920,981,998,
       &'500DVEW815','500DVEW818','500DVEW351','500DVEW981','500DVEW351',&
       !       999
       &'500DVEW351'/
  data (EQNUME(I),I = 1,66)/&
       !         11, 15, 17, 19, 20,21,
       &'616TRFW019','616TRFW019','616TRFW019','616TRFW019','516TRFW021',&
       !         21, 22, 41, 42, 64,
       &'516TRFW021','616TRFW019','516TRFW081','616TRFW242','516TRFW060',&
       !         66, 72, 73, 81, 92,
       &'516TRFW060','616TRFW073','616TRFW073','516TRFW081','616TRFW098',&
       !         93, 98,101,103,106,
       &'616TRFW094','616TRFW094','616TRFW108','616TRFW108','616TRFW264',&
       !        108,113,116,117,119,
       &'616TRFW108','616TRFW108','616TRFW122','516TRFW117','616TRFW108',&
       !        122,130,202,211,212,
       &'616TRFW122','616TRFW264','616TRFW202','616TRFW211','616TRFW211',&
       !        231,242,263,264,299,
       &'616TRFW242','616TRFW242','616TRFW263','616TRFW264','616TRFW264',&
       !        312,321,351,352,361,
       &'500DVEW312','500DVEW351','500DVEW351','500DVEW351','500DVEW361',&
       !        375,376,431,475,478,
       &'500DVEW351','500DVEW351','500DVEW351','400DVEW475','400DVEW475',&
       !        492,500,510,542,590,
       &'500DVEW351','500DVEW351','500DVEW351','500DVEW818','500DVEW351',&
       !        600,631,660,730,740,
       &'500DVEW351','500DVEW631','500DVEW351','500DVEW351','500DVEW351',&
       !        746,747,760,768,805,
       &'500DVEW351','500DVEW351','500DVEW351','500DVEW351','500DVEW805',&
       !        815,818,920,981,998,
       &'500DVEW815','500DVEW818','500DVEW351','500DVEW981','500DVEW351',&
       !       999
       &'500DVEW351'/

  DONE = 0
  LAST = 66
  FIRST = 1
  !     Westside Variants
  if (VAR.eq.'PN' .or. VAR.eq.'WC' .or. VAR.eq.'NC' .or.&
       &VAR.eq.'CA') then
     !     GET EQUATION FROM EQNUMW ARRAY
     do 65, while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (FIA(HALF) .eq. SPEC) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        ERRFLAG = 1
        DONE = -1
     else if (FIA(HALF) .lt. SPEC) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
     65 continue

     if (DONE .lt.0) then
        VOLEQ = EQNUMW(LAST)
     else
        VOLEQ = EQNUMW(DONE)
     end if

     !     FORESTS IN WA
     !     NEED TO FIND FOREST NUMBER FOR WA!!!
     if (FORST.eq.'03' .or. FORST.eq.'05' .or. FORST.eq.'09'&
          &.or. FORST.eq.'13' .or. FORST.eq.'17' .or. FORST.eq.'21') then
        if (SPEC.eq.72.or. SPEC.eq.73) then
           VOLEQ = '632TRFW073'
        else if (SPEC.eq.122) then
           VOLEQ = '632TRFW122'
        else if (SPEC.eq.312.or. SPEC.eq.361.or. SPEC.eq.376&
             &.or. SPEC.eq.431.or. SPEC.eq.492&
             &.or. SPEC.eq.542.or. SPEC.eq.815) then
           VOLEQ = '500DVEW351'
        else if (SPEC.eq.351) then
           VOLEQ = '616TRFW351'
        end if
     end if
     !     EAST VARIANTS
  else
     !     GET EQUATION FROM EQNUME ARRAY
     do 75, while (DONE.eq.0)
     HALF = (LAST - FIRST +1)/2 + FIRST
     if (FIA(HALF) .eq. SPEC) then
        DONE = HALF
     else if (FIRST .eq. LAST) then
        ERRFLAG = 1
        DONE = -1
     else if (FIA(HALF) .lt. SPEC) then
        FIRST = HALF
     else
        LAST = HALF - 1
     end if
     75 continue

     if (DONE .lt.0) then
        VOLEQ = EQNUME(LAST)
     else
        VOLEQ = EQNUME(DONE)
     end if

     !     FORESTS IN WA
     if (FORST.eq.'03' .or. FORST.eq.'05' .or. FORST.eq.'09'&
          &.or. FORST.eq.'13' .or. FORST.eq.'17' .or. FORST.eq.'21') then
        if (SPEC.eq.351) then
           VOLEQ = '616TRFW351'
        else if (SPEC.eq.312.or. SPEC.eq.361.or. SPEC.eq.542&
             &.or. SPEC.eq.815.or. SPEC.eq.818) then
           VOLEQ = '500DVEW351'
        end if
     end if
  end if

  return
  end subroutine R6_PNWEQN
