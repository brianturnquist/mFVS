!===================!
!   base : mcnvrt   !
!===================!
! BASE $Id$
subroutine MCNVRT(IKEY,JD,P,X)
  implicit none

  !     PURPOSE: RETURNS A VECTOR 'X(NP)' OF CONVERSION FACTORS TO BE USED
  !              IN CONJUNCTION WITH THE OPTION PROCESSOR'S PARAMETERS.
  !              THE ACTIVITY
  !              CODE 'KEY' LINKS THE ACTIVITY TO THE REQUIRED
  !              DEATILS OF THE CONVERSION

  !     CODED BY: DON ROBINSON, ESSA TECHNOLOGIES

  !     THIS IS A **PRELIMINARY** IMPLEMENTATION. IT COULD EVENTUALLY REPLACE
  !     ALL THE CONVERSION MESSINESS FOUND IN INITRE, RDIN, ESTAB, AND OPLIST.

  !     JD = DIRECTION OF CONVERSION

  ! Common blocks
  include     'METRIC.f90'

  !     CHARACTER*8 KEYWRD
  ! Variables
  integer :: IKEY,JD
  integer :: KEY
  integer :: I,IARY,LOC
  real    :: P(*)
  real    :: X(N_METRIC_PRMS)

  KEY = IKEY

  !     SET DEFAULT VECTOR TO 1

  do I = 1,N_METRIC_PRMS
     X(I) = 1.0
  end do

  !     This works ONLY because none of the entries in the ITRSL2 array
  !     in the calling subroutine OPLIST produces a zero-remainder
  !     when divided by 100 for the options needing metric conversion

  call OPKEY(KEY) ! where is this and what does it do?
  if (KEY.le.0) return
  LOC = (KEY/100)+1

  goto (42,42,43,44,45,46,47,48,49,52,53,54,55,56,57,58),LOC

  !     DETERMINE THE PARAMETER MULTIPLIERS FOR THE BASE ACTIVITIES

  42 KEY = MOD(KEY,1000)
  goto (1000,1000,1003,1000,1000,1000,1000,1000,1000,1000,&
       &1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,&
       &1000,1000,1000,1024,1025,1026,1027,1000,1029,1000,&
       &1000,1000,1000,1000,1035,1000,1037,1038,1000,1000,&
       &1041,1000,1043,1044,1000,1000,1000,1048,1000,1000,&
       &1000,1000,1000,1000,1000,1000,1000,1000,1059,1000,&
       &1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,&
       &1000,1000,1000,1000,1000,1000,1000,1000,1000,1000,&
       &1000,1000,1000,1000,1000,1000,1000,1088,1000,1000,&
       &1000,1000,1000,1000,1000,1096,1000,1000,1000,1000,&
       &1000,1000,1000,1000,1000,1000,1000,1108,1000,1110,&
       &1111,1112,1000,1000,1115) KEY

  1000 continue
  go to 61

  1003 continue ! FIXCW 90
  X(3) = INtoCM
  X(4) = INtoCM
  go to 61

  !     IT SEEMS ODD THAT BOTH THINBTA AND THINRDEN WOULD HAVE IDENTICAL
  !     MAPPINGS.

  1024 continue
  if (IKEY .eq.223) then ! THINBTA 223
     X(1) = 1.0/ACRtoHA
     X(3) = INtoCM
     X(4) = INtoCM
     X(5) = FTtoM
     X(6) = FTtoM
  else if (IKEY .eq.234) then ! THINRDEN 234
     X(1) = FT2pACRtoM2pHA
     X(4) = INtoCM
     X(5) = INtoCM
  end if
  go to 61

  1025 continue ! THINATA 224
  X(1) = 1.0/ACRtoHA
  X(3) = INtoCM
  X(4) = INtoCM
  X(5) = FTtoM
  X(6) = FTtoM
  go to 61

  1026 continue ! THINBBA 225
  X(1) = FT2pACRtoM2pHA
  X(3) = INtoCM
  X(4) = INtoCM
  X(5) = FTtoM
  X(6) = FTtoM
  go to 61

  1027 continue ! THINABA 226
  X(1) = FT2pACRtoM2pHA
  X(3) = INtoCM
  X(4) = INtoCM
  X(5) = FTtoM
  X(6) = FTtoM
  go to 61

  1029 continue ! THINDBH 228
  if (IKEY .eq.228) then
     X(1) = INtoCM
     X(2) = INtoCM
     X(5) = 1.0/ACRtoHA
     X(6) = FT2pACRtoM2pHA
  else if (IKEY .eq.235) then ! THINPT 235
     X(3) = INtoCM
     X(4) = INtoCM
  end if
  go to 61

  1035 continue ! THINHT 232
  X(1) = INtoCM
  X(2) = INtoCM
  X(5) = 1.0/ACRtoHA
  X(6) = FT2pACRtoM2pHA
  go to 61

  1037 continue ! TOPKILL 111
  X(2) = FTtoM
  X(3) = FTtoM
  go to 61

  1038 continue ! HTGSTOP 110
  X(2) = FTtoM
  X(3) = FTtoM
  go to 61

  1041 continue ! THINQFA 237 ... need to know the units from PRMS(7)
  X(1) = INtoCM
  X(2) = INtoCM
  X(5) = INtoCM
  X(6) = 1.
  if (P(7).le.0) X(6) = FT2pACRtoM2pHA !untested
  if (P(7).le.1) X(6) = HAtoACR
  if (P(7).gt.1) X(6) = ACRtoHA
  go to 61

  1043 continue ! VOLUME 217
  X(2) = INtoCM
  X(3) = INtoCM
  X(4) = FTtoCM
  go to 61

  1044 continue ! SPLEAVE 206
  if (IKEY .eq.206) then
     X(2) = INtoCM
     X(3) = INtoCM
     X(4) = FTtoCM
  else if (IKEY .eq.218) then ! BFVOLUME 218
     X(2) = INtoCM
     X(3) = INtoCM
     X(4) = FTtoCM
  end if
  go to 61

  1048 continue ! MINHARV 200
  X(1) = FT3pACRtoM3pHA
  X(3) = FT2pACRtoM2pHA
  X(4) = FT3pACRtoM3pHA
  go to 61

  1059 continue ! MORTMULT 94
  X(3) = INtoCM
  X(4) = INtoCM
  go to 61

  1088 continue ! FIXMORT 97
  X(3) = INtoCM
  X(4) = INtoCM
  go to 61

  1096 continue ! CRNMULT 81
  X(3) = INtoCM
  X(4) = INtoCM
  go to 61

  1108 continue ! PRUNE 249
  X(2) = FTtoM
  X(5) = INtoCM
  X(6) = INtoCM
  go to 61

  1110 continue ! FIXDG 98
  X(3) = INtoCM
  X(4) = INtoCM
  go to 61

  1111 continue ! FIXHTG 99
  X(3) = INtoCM
  X(4) = INtoCM
  go to 61

  1112 continue ! THINSDI 230
  X(3) = INtoCM
  X(4) = INtoCM
  go to 61

  1115 continue ! THINCC 231
  X(3) = INtoCM
  X(4) = INtoCM
  go to 61

  !     ESTABLISHMENT MODEL ACTIVITY CODES.

  43 KEY = MOD(KEY,100)
  goto (2000,2020,2030,2000,2000,2000,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2150,2000,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2000,2260,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2000,2000,2000,2000,2000,2000,&
       &2000,2000,2000,2000,2000,2000,2000,2000,2000,2000) KEY

  2000 continue
  go to 61
  2020 continue ! PLANT 430
  X(2) = 1.0/ACRtoHA
  X(5) = FTtoM
  go to 61
  2030 continue ! NATURAL 431
  X(2) = 1.0/ACRtoHA
  X(5) = FTtoM
  go to 61
  2150 continue ! HTADJ 442
  X(2) = FTtoM
  go to 61
  2260 continue ! SPROUT 450
  X(4) = INtoCM
  X(5) = INtoCM
  go to 61

  !     PLACEHOLDERS FOR CONVERSION OF OTHER KEYWORDS

  44 continue
  !      CALL TMKEY(KEY,KEYWRD)
  go to 61
  45 continue
  !      CALL MPKEY(KEY,KEYWRD)
  go to 61
  46 continue
  !      CALL CVKEY(KEY,KEYWRD)
  go to 61
  47 continue
  !      CALL BWKEY(KEY,KEYWRD)
  go to 61
  48 continue
  !  ** free spot **
  go to 61
  49 continue
  !      CALL RRKEY(KEY,KEYWRD)
  go to 61
  52 continue
  !      CALL BRKEY(KEY,KEYWRD)
  go to 61
  53 continue
  !      CALL MISKEY(KEY,KEYWRD)
  go to 61
  54 continue
  !      CALL ANKEY(KEY,KEYWRD)
  go to 61

  !     DFB ACTIVITY CODES (NOT TESTED)

  55 KEY = MOD(KEY,1000)
  goto (3000,3000,3000,3000,3000,3000,3000,3000,3000,3010,&
       &3000,3000,3000,3000,3000,3000,3000,3000,3000,3000) KEY
  3000 continue
  go to 61
  3010 continue ! WINDTHR 302
  X(2) = 1.0/ACRtoHA
  go to 61

  56 continue
  !      CALL BMKEY(KEY,KEYWRD)
  go to 61

  !  ROOT DISESASE ACTIVITY CODES

  !  PRODUCE METRIC VALUES FOR GENERAL ROOT DISEASE MODEL. NOTE
  !  THAT THE X() INDICES ARE +1 COMPARED TO THE PRMS() OR
  !  PARMS() ARRAY THAT THEY ARE USED WITH. THIS IS BECAUSE OF
  !  THE DIFFERENT WAY THAT THESE ARE STORED, COMPARED TO THE
  !  BASE MODEL KEYWORDS. SO IF PRMS(2) IS DBH THAT SHOULD BE
  !  CONVERTED FROM INCHES TO CM, X(3) IS LOADED WITH THE
  !  CONVERSION FACTOR 'IN2CM'

  57 KEY = MOD(KEY,100)
  goto (5701,5700,5703,5700,5700,5700,5700,5700,5700,5700,&
       &5700,5700,5700,5714,5715,5716,5717,5700,5700,5700,&
       &5700,5700,5700,5700,5700,5700,5700,5700,5700,5730,&
       &5731,5732,5700,5700,5700,5700,5700,5700,5700,5700,&
       &5700,5700) KEY

  5700 go to 61

  5701 continue ! spread
  X(2) = FTtoM
  X(3) = FTtoM
  go to 61

  5703 continue ! pstump
  X(2) = INtoCM
  go to 61

  5714 continue ! windthr  << may not be metric in RDIN
  X(3) = HAtoACR
  go to 61

  5715 continue ! bbtype1
  X(2) = INtoCM
  X(3) = HAtoACR
  go to 61

  5716 continue ! bbtype2
  X(2) = INtoCM
  X(3) = HAtoACR
  go to 61

  5717 continue ! bbtype3
  X(2) = INtoCM
  X(3) = HAtoACR
  go to 61

  5730 continue ! borate
  X(2) = INtoCM
  go to 61

  5731 continue ! spore
  X(2) = INtoCM
  go to 61

  5732 continue ! bbtype4
  X(2) = INtoCM
  X(3) = HAtoACR
  go to 61

  !     FIRE MODEL OPTION CALLS.

  58 KEY = MOD(KEY,100)
  go to(5800,5800,5800,5800,5800,5806,5807,5800,5800,5800,&
       &5800,5800,5800,5800,5800,5800,5800,5800,5800,5820,&
       &5821,5822,5800,5800,5800,5800,5800,5800,5800,5800,&
       &5800,5800,5800,5834,5800,5800,5800,5800,5839,5800,&
       &5800,5800,5800,5800,5800,5800,5800,5800,5849,5800,&
       &5800,5800,5853,5854,5800,5800), KEY

  5800 go to 61

  ! After implement metrification of DEFULMOD and FIRECALC keywords (EBt#1630), I
  ! noted that the indexes for ??ALL?? the metric FFE keywords seem to be
  ! off-by-one. This is probably because some extensions (e.g. ESTAB) pass ARRAY(*)
  ! to OPLIST, but others pass PRMS(*). When building metrification in MCNVRT using
  ! FMIN/ESIN as a guide to indexes, ARRAY(i) values should be indexed using (i+1)
  ! and PRM(i) values should be indexed using (i).

  ! I have changed **and not yet tested** these keywords:

  !   SIMFIRE, FLAMEADJ, SALVAGE, SNAGINIT, FUELMOVE

  ! Remove each after testing confirmed. - DR 19 Jan 2009

  5806 continue ! SIMFIRE 2506
  X(1) = MItoKM
  !                     NOTE FtoC1 ISN'T QUITE ACCURATE BECAUSE ACTUAL TRANSLATION IS:
  !                     val*FtoC1+FtoC2 BUT THIS WILL HAVE TO BE CLOSE ENOUGH
  X(3) = FtoC1
  go to 61

  5807 continue ! FLAMEADJ 2507
  X(2) = FTtoM
  X(4) = FTtoM
  go to 61

  5820 continue ! SALVAGE 2520
  X(1) = INtoCM
  X(2) = INtoCM
  go to 61

  5821 continue ! FUELINIT 2521
  do IARY = 1,12
     X(IARY) = TItoTM / ACRtoHA
  end do
  go to 61

  5822 continue ! SNAGINIT 2522
  X(2) = INtoCM
  X(3) = FTtoM
  X(4) = FTtoM
  X(6) = 1.0 / ACRtoHA
  go to 61

  5834 continue ! FUELMOVE 2530
  X(3) = TItoTM / ACRtoHA
  X(5) = TItoTM / ACRtoHA
  X(6) = TItoTM / ACRtoHA
  go to 61

  5839 continue ! DEFULMOD 2539
  X(2) = MtoFT
  X(3) = MtoFT
  X(4) = MtoFT
  X(5) = MtoFT
  X(6) = M2toFT2/KGtoLB
  go to 61

  5849 continue ! FIRECALC 2549
  X(3) = FTtoM
  X(4) = FTtoM
  X(5) = FTtoM
  X(6) = LBtoKG/FT3toM3
  X(7) = LBtoKG/FT3toM3
  X(8) = BTUtoKJ/LBtoKG
  go to 61

  5853 continue ! FUELSOFT 2553
  do IARY = 1,9
     X(IARY) = TItoTM / ACRtoHA
  end do
  go to 61

  5854 continue ! FMORTMULT 2554
  X(3) = INtoCM
  X(4) = INtoCM

  61 continue

  !     FOR TYPE 1 CONVERSIONS, THE STORED (OR COMPUTED) PARAMETERS ARE METRIC
  !     AND MUST BE CONVERTED TO IMPERIAL FOR THE CORE MODEL. SINCE THE FACTORS
  !     STORED HERE ARE THE IMPERIAL->METRIC CONVERSIONS, WE USE THE INVERSE.
  !     OTHERWISE A TYPE 2 CONVERSION (JD .EQ. 2) AND THE FACTORS ARE UNINVERTED

  do I = 1,N_METRIC_PRMS
     if (JD .eq.1) then
        if (X(I) .gt.0.0) X(I) = 1.0 / X(I)
     end if
  end do

  return
end
