!===============!
!   rd : rdpr   !
!===============!
! RD $Id$
subroutine RDPR
  implicit none

  !  PRODUCES SUMMARY OUTPUT IN TABLE FORM FROM THE ROOT
  !  DISEASE EXTENSION TO THE PROGNOSIS MODEL. THE SUMMARY OUTPUT
  !  DETAILS INDICATORS OF THE STATE OF THE TREES IN THE ROOT DISEASE
  !  AREA.  CURRENTLY, THIS OUTPUT IS *ALWAYS* PRINTED (I.E. IT IS NOT
  !  KEYWORD CONTROLLED).

  !  CALLED BY :
  !     MAIN    [PROGNOSIS]

  !  CALLS     :
  !     DBCHK   (SUBROUTINE)   [PROGNOSIS]
  !     RDDOUT  (SUBROUTINE)   [ROOT DISEASE]

  !  PARAMETERS :
  !     NONE

  !  COMMON BLOCK VARIABLES :
  !     xxxxx:   From ANCOM;

  !  LOCAL VARIABLES :
  !     FIRSTL:  Is first line of output yet to be printed?
  !     LYAREA:  LAST YEAR'S DISEASED AREA

  !  REVISION HISTORY:
  !    18-JUN-2001 Lance R. David (FHTET)
  !       Added Stand ID and Management ID line to header.
  !    06-AUG-2001 Lance R. David (FHTET)
  !       Initialization of LYAREA at cycle 0.
  !    16-AUG-2006 Lance R. David (FHTET)
  !       Change of metric conversion factors variable names to match
  !       variables in new \FVS\COMMON\METRIC.F77. rd\src\metric.f77
  !       will be retired. (mods courtesy of Don Robinson, ESSA)
  !   08/29/14 Lance R. David (FMSC)
  !     Added implicit none and declared variables.
  !   03/24/15 Lance R. David (FMSC)
  !     Implemented General Report Writer facility.

  !.... PARAMETER INCLUDE FILES

  ! Common blocks
  include 'PRGPRM.f90'
  include 'RDPARM.f90'

  !.... COMMON INCLUDE FILES

  include 'ARRAYS.f90'
  include 'CONTRL.f90'
  include 'PLOT.f90'
  include 'RDARRY.f90'
  include 'RDCOM.f90'
  include 'RDCRY.f90'
  include 'RDADD.f90'
  include 'METRIC.f90'

  ! Variables
  logical :: DEBUG,FIRSTL
  integer :: I,I1,I2,IDI,IEND,IOAGE,IOUT,J,JYR,M,K,KSP,L
  real    :: ALLVOL,BAPA,BASTPA,CFVPA,CORE,DAREA,EXPAND,LYAREA(ITOTSP),TCLAS,TDIE,TDVOL,TIN,TOTINF,&
       &TPRBIN,TPRINF,TSTMPS,TUN
  character(len=1) :: CHTYPE(ITOTRR)

  data CHTYPE /'P','S','A','W'/

  if (ICYC .eq.0) then
     do I = 1,ITOTRR
        LYAREA(I) = 0.0
        10 continue
     end do
  end if

  if (ITRN .eq.0) return

  FIRSTL = .true.

  ! Check whether debugging is enabled
  call DBCHK (DEBUG,'RDPR',4,ICYC)

  if (IROOT .eq.0) return

  do IRRSP = MINRR,MAXRR
     TDIE = 0.0
     TDVOL = 0.0
     TSTMPS = 0.0
     BASTPA = 0.0
     BAPA = 0.0
     TPRINF = 0.0
     CFVPA = 0.0
     TIN = 0.0
     TUN = 0.0
     TPRBIN = 0.0
     CORE = 0.0
     EXPAND = 0.0
     TOTINF = 0.0
     ALLVOL = 0.0

     if (IRDOUT .ne.0) call RDDOUT

     if (DEBUG) write(JOSTND,101) ISTEP
     101 format ('IN RDPR ISTEP=',I5)

     !        Get logical unit number and open genrpt file if it has been closed.

     call GETLUN(IRUNIT)

     if (ISTEP .eq.1.and. IRRSP .eq. MINRR) then
        JYR = IY(ICYC+1)
        IOAGE = IAGE + IY(ICYC+1) - IY(1)

        !           get report ID.

        call GETID(IDRDOUT(1))

        !           WRITE THE TABLE HEADERS

        write (IRUNIT,'(2(/1X,I5))') IDRDOUT(1),IDRDOUT(1)
        write (IRUNIT,1100) IDRDOUT(1),IDRDOUT(1)
        write (IRUNIT,1105) IDRDOUT(1)

        !     Write warning message RE. use of multi pest models (RNH June 98)

        if (LXNOTE) then
           write (IRUNIT,1106) IDRDOUT(1)
           LXNOTE = .false.
        end if

        write (IRUNIT,1107) IDRDOUT(1)
        write (IRUNIT,1110) IDRDOUT(1), NPLT, MGMID
        write (IRUNIT,1115) IDRDOUT(1)
        write (IRUNIT,1120) IDRDOUT(1)
        write (IRUNIT,1125) IDRDOUT(1)
        write (IRUNIT,1130) IDRDOUT(1)
        write (IRUNIT,1135) IDRDOUT(1)
        write (IRUNIT,1140) IDRDOUT(1)
        write (IRUNIT,1143) IDRDOUT(1)
        write (IRUNIT,1145) IDRDOUT(1)
        if (LMTRIC) then
           write (IRUNIT,1250) IDRDOUT(1)
           write (IRUNIT,1255) IDRDOUT(1)
        else
           write (IRUNIT,1150) IDRDOUT(1)
           write (IRUNIT,1155) IDRDOUT(1)
        end if
        write (IRUNIT,1120) IDRDOUT(1)
     else
        JYR = IY(ICYC+1)
        IOAGE = IAGE + IY(ICYC+1) - IY(1)
        if (DEBUG) write (JOSTND,102) ISTEP, ICYC, JYR
        102 format ('IN RDPR:ISTEP ICYC JYR=',3I5)
     end if

     if (PAREA(IRRSP) .le.0.0.and. LYAREA(IRRSP) .le.0.0)&
          &goto 7300
     if (PAREA(IRRSP) .le.0.0.and. LYAREA(IRRSP) .gt.0.0)&
          &goto 6000

     IEND = MAX(1,ISTEP)

     do M = 1,IEND
        do K = 1,5
           do L = 1,2
              TSTMPS = TSTMPS + PROBDA(IRRSP,L,K,M) /&
                   &(PAREA(IRRSP) + 1.0E-9)
              BASTPA = BASTPA + PROBDA(IRRSP,L,K,M) * (3.141593 *&
                   &(DBHDA(IRRSP,L,K,M) / 24.0)**2) /&
                   &(PAREA(IRRSP) + 1.0E-9)
              700 continue
           end do
           701 continue
        end do
        702 continue
     end do

     !        FIND TREES IN TREE LIST

     if (ITRN .le.0) goto 805
     IDI = IRRSP
     do KSP = 1,MAXSP
        if (IRRSP .lt.3) IDI = IDITYP(IRTSPC(KSP))
        if ((ISCT(KSP,1) .eq.0) .or. (IDI .ne. IRRSP)) goto 800

        I1 = ISCT(KSP,1)
        I2 = ISCT(KSP,2)

        do J = I1,I2
           I = IND1(J)

           !              ACCUMULATE INDICATORS

           if (DEBUG) write (JOSTND,749) I, PAREA(IRRSP), PROBIU(I),&
                &PROBIT(I)
           749 format ('IN RDPR :  I PAREA PROBIU PROBIT',I4,3F9.2)

           TCLAS = PROBIU(I) + PROBIT(I)
           TUN = TUN + PROBIU(I) / (PAREA(IRRSP) + 1.0E-9)
           TIN = TIN + PROBIT(I) / (PAREA(IRRSP) + 1.0E-9)

           TDIE = TDIE + RDKILL(I) / (PAREA(IRRSP) + 1.0E-9)
           TDVOL = TDVOL + RDKILL(I) * CFV(I) /&
                &(PAREA(IRRSP) + 1.0E-9)
           ALLVOL = ALLVOL + WK2(I) * CFV(I) /&
                &(PAREA(IRRSP) + 1.0E-9)
           BAPA = BAPA + TCLAS * (3.14159 * (DBH(I) / 24.0)**2) /&
                &(PAREA(IRRSP) + 1.0E-9)
           CFVPA = CFVPA + TCLAS * WK1(I) / (PAREA(IRRSP) + 1.0E-9)

           750 continue
        end do
        800 continue
     end do

     !        CHANGE NEWLY INFECTED TREES TO A PROPORTION

     if (CORINF(IRRSP,2) .gt.0.0)&
          &CORE = CORINF(IRRSP,1) / CORINF(IRRSP,2)
     if (EXPINF(IRRSP,2) .gt.0.0)&
          &EXPAND = EXPINF(IRRSP,1) / EXPINF(IRRSP,2)
     TOTINF = EXPINF(IRRSP,2) + CORINF(IRRSP,2)
     if (TOTINF .gt.0.0)&
          &TOTINF = (EXPINF(IRRSP,1) + CORINF(IRRSP,1)) / TOTINF

     !        ACCUMULATE WEIGHTED AVE PROBABILITY OF NEW CENTER INITIATION

     do I = 1,2
        do J = 1,5
           if (PROBIN(IRRSP,I,J) .ge.0.0.and.&
                &PAREA(IRRSP) .gt.0.0)&
                &TPRBIN = TPRBIN + PROBIN(IRRSP,I,J) * PROBD(IRRSP,I,J)
           900 continue
        end do
        950 continue
     end do

     TPRBIN = TPRBIN / (TSTMPS + 1.0E-9)

     805 continue
     TPRINF = PRINF(IRRSP) * 100.0

     if (LMTRIC) then
        write(IRUNIT,2098)&
             &IDRDOUT(1),JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),&
             &PAREA(IRRSP)*ACRtoHA,RRRATE(IRRSP)*FTTOM,TSTMPS/ACRtoHA,&
             &BASTPA*FT2pACRtoM2pHA,TDIE/ACRtoHA,TDVOL*FT3pACRtoM3pHA,&
             &TUN/ACRtoHA,TIN/ACRtoHA,TPRINF,CFVPA*FT3pACRtoM3pHA,&
             &BAPA*FT2pACRtoM2pHA,CORE,EXPAND,TOTINF

        !              Call DBS for RD Summary output to database

        call DBSRD1 (&
             &JYR,NPLT,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),&
             &PAREA(IRRSP)*ACRtoHA,RRRATE(IRRSP)*FTTOM,TSTMPS/ACRtoHA,&
             &BASTPA*FT2pACRtoM2pHA,TDIE/ACRtoHA,TDVOL*FT3pACRtoM3pHA,&
             &TUN/ACRtoHA,TIN/ACRtoHA,TPRINF,CFVPA*FT3pACRtoM3pHA,&
             &BAPA*FT2pACRtoM2pHA,CORE,EXPAND,TOTINF)
     else
        write(IRUNIT,2098)&
             &IDRDOUT(1),JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),&
             &PAREA(IRRSP),RRRATE(IRRSP),TSTMPS,BASTPA,TDIE,TDVOL,&
             &TUN,TIN,TPRINF,CFVPA,BAPA,CORE,EXPAND,TOTINF

        !              Call DBS for RD Summary output to database

        call DBSRD1 (&
             &JYR,NPLT,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),&
             &PAREA(IRRSP),RRRATE(IRRSP),TSTMPS,BASTPA,TDIE,TDVOL,&
             &TUN,TIN,TPRINF,CFVPA,BAPA,CORE,EXPAND,TOTINF)
     end if
     FIRSTL = .false.
     goto 7300

     6000 continue

     !         OUTPUT TO PRINT IF THIS WAS THE CYCLE AFTER CENTERS DISAPPEARED

     if (LMTRIC) then
        write(IRUNIT,2199)&
             &IDRDOUT(1),JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),&
             &PAREA(IRRSP)*ACRtoHA,RRRATE(IRRSP)*FTTOM
     else
        write(IRUNIT,2199)&
             &IDRDOUT(1),JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),&
             &PAREA(IRRSP),RRRATE(IRRSP)
     end if
     RRRATE(IRRSP) = 0.0

     7300 continue

     !        PRINT THE MACHINE READABLE FORM (I.E. NO TABLE HEADERS) OF THE
     !        OUTPUT ON UNIT IOUT. IRGEN(7) STATES WHETHER TO PRINT THIS OUTPUT.
     !        NOTE THAT THE INFORMATION IS SLIGHTLY DIFFERENT IN THIS FILE.

     DAREA = PAREA(IRRSP) - LYAREA(IRRSP)
     IOUT = IRGEN(2)
     if (IOUT .gt.0.and. IRGEN(7) .eq.1) then
        if (LMTRIC) then
           write(IOUT,4099) JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),&
                &PAREA(IRRSP)*ACRtoHA,RRRATE(IRRSP)*FTTOM,TSTMPS/ACRtoHA,&
                &BASTPA*FT2pACRtoM2pHA,TDIE/ACRtoHA,TDVOL*FT3pACRtoM3pHA,&
                &TUN/ACRtoHA,TIN/ACRtoHA,TPRINF,&
                &DAREA*ACRtoHA,CORE,EXPAND,TOTINF
        else
           write(IOUT,4099) JYR,IOAGE,CHTYPE(IRRSP),NCENTS(IRRSP),&
                &PAREA(IRRSP),RRRATE(IRRSP),TSTMPS,BASTPA,TDIE,TDVOL,&
                &TUN,TIN,TPRINF,DAREA,CORE,EXPAND,TOTINF
        end if
     end if

     LYAREA(IRRSP) = PAREA(IRRSP)

     !       NOW THAT WE'VE USED THE VARIABLES, ZERO THEM FOR FUTURE USE

     CORINF(IRRSP,1) = 0.0
     EXPINF(IRRSP,1) = 0.0
     CORINF(IRRSP,2) = 0.0
     EXPINF(IRRSP,2) = 0.0
     5000 continue
  end do

  if (.not. FIRSTL .and. MINRR .ne. MAXRR)&
       &write(IRUNIT,1120) IDRDOUT(1)
  return

  1100 format (1X,I5,/1X,I5,1X,65('* '))
  1107 format (1X,I5,1X,65('* '))
  1105 format (1X,I5,51X,'WESTERN ROOT DISEASE MODEL')
  1110 format (1X,I5,34X,'STAND ID= ',A26,5X,'MANAGEMENT ID= ',A4)

  !     Warning message RE. multi-pest models (RNH June 1998)

  1106 format (/1X,I5,1X,&
       &'*=================================================',&
       &'===========================*',/&
       &'*---> Note:  The combined insect and pathogen models (in ',&
       &'one executable) <---*',/&
       &'*---> should NOT be used without close consultation with ',&
       &'the forest''s    <---*',/&
       &'*---> pathologist and entomologist.  Because of the ',&
       &'potential for more   <---*',/&
       &'*---> than one insect and/or pathogen acting on the same ',&
       &'tree species,   <---*',/&
       &'*---> the interpretation of the results of the combined ',&
       &'model can be     <---*',/&
       &'*---> inaccurate without appropriate model knowledge and/or',&
       &' experience.  <---*',/&
       &'*==========================================================',&
       &'==================*',/)

  1115 format (1X,I5,26X,'SUMMARY STATISTICS FOR ROOT DISEASE AREAS',&
       &' (PER ACRE BASED ON DISEASED AREA ONLY)')
  1120 format (1X,I5,1X,130('-'))
  1125 format (1X,I5,17X,'DISEASE STATISTICS',&
       &9X,'DEAD TREE CHARACTERISTICS',&
       &8X,'LIVE TREE CHARACTERISTICS')
  1130 format (1X,I5,43X,'STUMPS',6X,'LOSSES FROM DISEASE')

  1135 format (1X,I5,12X,26('-'),3X,12('-'),2X,19('-'),1X,35('-'))
  1140 format (1X,I5,41X,'   ',31X,'UNINF',4X,'INFECTED',9X,'OVERALL')
  1143 format (1X,I5,56X,'NO OF',3X,'VOLUME')
  2098 format (1x,I5,1X,I4,I6,4X,A1,I4,F8.2,F8.2,4(F8.1),3X,F8.1,F8.1,&
       &F8.1,F8.0,F7.0,1X,F5.3,3X,F5.3,3X,F5.3)
  4099 format (I4,I6,4X,A1,I4,F8.2,F8.2,4(F8.1),3X,F8.1,F8.1,&
       &F8.1,F8.2,1X,F5.3,3X,F5.3,3X,F5.3)
  1145 format (1X,I5,18X,'NO',3X,'DISEASE',2X,'SPREAD',10X,'BA/',&
       &4X,'TREES',3X,'LOSSES',5X,'NO OF',3X,'NO OF',&
       &4X,'AVE',3X,'MERCH',4X,'BA/',3X,'NEWLY INFECTED')

  1150 format (1X,I5,18X,'OF',4X,'AREA',5X,'RATE',3X,'TOTAL',&
       &3X,'ACRE',4X,'KILLED',2X,'CU FT',6X,'TREES',3X,'TREES',2X,&
       &'%ROOTS',2X,'CU FT',2X,'ACRE',5X,'PROPORTION')

  1155 format (1X,I5,' YEAR',3X,'AGE',1X,'TYPE',1X,'CENTS',2X,'ACRES',&
       &3X,'FT/YR',&
       &3X,'/ACRE',2X,'SQFT',4X,'/ACRE',3X,'/ACRE',3X,&
       &2(3X,'/ACRE'),1X,'INFECTED',1X,'/ACRE',3X,'SQFT',1X,&
       &'INSIDE',2X,'EXPAND',2X,'TOTAL')
  2199 format (1X,I5,1X,I4,I6,4X,A1,I4,F8.2,F8.2)

  !     SPECIFIC METRIC HEADERS

  1250 format (1X,I5,18X,'OF',4X,'AREA',5X,'RATE',3X,'TOTAL',&
       &3X,'HA  ',4X,'KILLED',2X,'CU M ',6X,'TREES',3X,'TREES',2X,&
       &'%ROOTS',2X,'CU M ',2X,' HA ',5X,'PROPORTION')

  1255 format (1X,I5,' YEAR',3X,'AGE',1X,'TYPE',1X,'CENTS',2X,' HA  ',&
       &3X,' M/YR',&
       &3X,' /HA ',2X,'SQ M',4X,' /HA ',3X,' /HA ',3X,&
       &2(3X,' /HA '),1X,'INFECTED',1X,' /HA ',3X,'SQ M',1X,&
       &'INSIDE',2X,'EXPAND',2X,'TOTAL')

end
