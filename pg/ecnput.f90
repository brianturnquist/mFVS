!=================!
!   pg : ecnput   !
!=================!
! PG $Id$
subroutine ECNPUT (WK3,IPNT,ILIMIT)
  implicit none

  !     STORE THE ECONOMIC DATA FOR A GIVEN STAND.

  !     PART OF THE PARALLEL PROCESSING EXTENSION TO PROGNOSIS.

  ! Common blocks
  include 'PRGPRM.f90'
  include 'CONTRL.f90'
  include 'ECNCOM.f90'
  include 'ECNCOMSAVES.f90'

  ! Variables
  integer :: MXL,MXI,MXR

  parameter (MXL = 6,MXI = 20,MXR = 14)
  integer :: ILIMIT,IPNT,I
  logical :: LOGICS(MXL)
  real    :: WK3 (MAXTRE)
  integer :: INTS (MXI)
  real    :: REALS (MXR)

  !  PUT THE INTEGER SCALARS.

  INTS( 1) = annCostCnt
  INTS( 2) = annRevCnt
  INTS( 3) = econStartYear
  INTS( 4) = fixHrvCnt
  INTS( 5) = fixPctCnt
  INTS( 6) = pctMinUnits
  INTS( 7) = plntCostCnt
  INTS( 8) = varHrvCnt
  INTS( 9) = varPctCnt
  INTS(10) = burnCnt
  INTS(11) = hrvCstCnt
  INTS(12) = hrvRvnCnt
  INTS(13) = logTableId
  INTS(14) = mechCnt
  INTS(15) = startYear
  INTS(16) = specCstCnt
  INTS(17) = specRvnCnt
  INTS(18) = sumTableId
  INTS(19) = pretendStartYear
  INTS(20) = pretendEndYear
  call IFWRIT (WK3, IPNT, ILIMIT, INTS, MXI, 2)

  !  PUT THE INTEGER ARRAYS
  !  ONE DIMENSIONAL

  call IFWRIT (WK3,IPNT,ILIMIT, varHrvUnits, MAX_KEYWORDS,        2)
  call IFWRIT (WK3,IPNT,ILIMIT, varPctUnits, MAX_KEYWORDS,        2)
  call IFWRIT (WK3,IPNT,ILIMIT, plntCostUnits, MAX_PLANT_COSTS,   2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvRevCnt,MAXSP*MAX_REV_UNITS,    2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvRevDiaIndx,MAXSP*&
       &MAX_REV_UNITS*MAX_KEYWORDS,                        2)
  call IFWRIT (WK3,IPNT,ILIMIT, burnCostDur, MAX_RATES,           2)
  call IFWRIT (WK3,IPNT,ILIMIT, mechCostDur, MAX_RATES,           2)
  call IFWRIT (WK3,IPNT,ILIMIT, annCostDur,&
       &MAX_KEYWORDS*MAX_RATES,                            2)
  call IFWRIT (WK3,IPNT,ILIMIT, annRevDur,&
       &MAX_KEYWORDS*MAX_RATES,                            2)
  call IFWRIT (WK3,IPNT,ILIMIT, fixHrvDur,&
       &MAX_KEYWORDS*MAX_RATES,                            2)
  call IFWRIT (WK3,IPNT,ILIMIT, fixPctDur,&
       &MAX_KEYWORDS*MAX_RATES,                            2)
  call IFWRIT (WK3,IPNT,ILIMIT, varHrvDur,&
       &MAX_KEYWORDS*MAX_RATES,                            2)
  call IFWRIT (WK3,IPNT,ILIMIT, varPctDur,&
       &MAX_KEYWORDS*MAX_RATES,                            2)
  call IFWRIT (WK3,IPNT,ILIMIT, plntCostDur,&
       &MAX_PLANT_COSTS*MAX_RATES,                         2)
  call IFWRIT (WK3,IPNT,ILIMIT,hrvRevDur,MAXSP*&
       &MAX_REV_UNITS*MAX_KEYWORDS*MAX_RATES,              2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvCstKeywd, MAX_COSTS,           2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvCstTime,  MAX_COSTS,           2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvCstTyp,   MAX_COSTS,           2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvRvnKeywd, MAX_HARVESTS,        2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvRvnSp,    MAX_HARVESTS,        2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvRvnTime,  MAX_HARVESTS,        2)
  call IFWRIT (WK3,IPNT,ILIMIT, hrvRvnUnits, MAX_HARVESTS,        2)

  !  PUT THE LOGICAL SCALARS.

  LOGICS ( 1) = noLogStockTable
  LOGICS ( 2) = noOutputTables
  LOGICS ( 3) = isFirstEcon
  LOGICS ( 4) = doSev
  LOGICS ( 5) = isEconToBe
  LOGICS ( 6) = isPretendActive
  call LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)

  !  LOGICAL ARRAYS

  call LFWRIT (WK3,IPNT,ILIMIT,hasRevAmt,MAXSP*MAX_REV_UNITS,     2)

  !  PUT THE REAL SCALARS.

  REALS ( 1) = burnCostAmt
  REALS ( 2) = dbhSq
  REALS ( 3) = discountRate
  REALS ( 4) = mechCostAmt
  REALS ( 5) = pctMinDbh
  REALS ( 6) = pctMinVolume
  REALS ( 7) = sevInput
  REALS ( 8) = costDisc
  REALS ( 9) = costUndisc
  REALS (10) = rate
  REALS (11) = revDisc
  REALS (12) = revUndisc
  REALS (13) = sevAnnCst
  REALS (14) = sevAnnRvn
  call BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR,                     2)

  !  REAL ARRYS

  call BFWRIT (WK3,IPNT,ILIMIT, hrvCostBf, MAX_KEYWORDS,          2)
  call BFWRIT (WK3,IPNT,ILIMIT, hrvCostFt3, MAX_KEYWORDS,         2)
  call BFWRIT (WK3,IPNT,ILIMIT, hrvCostTpa, MAX_KEYWORDS,         2)
  call BFWRIT (WK3,IPNT,ILIMIT,lbsFt3Amt,MAXSP,                   2)
  do I = TPA,FT3_100
     call BFWRIT (WK3,IPNT,ILIMIT,harvest(I),1,                    2)
  end do
  call BFWRIT (WK3,IPNT,ILIMIT,annCostAmt,MAX_KEYWORDS,           2)
  call BFWRIT (WK3,IPNT,ILIMIT,annRevAmt,MAX_KEYWORDS,            2)
  call BFWRIT (WK3,IPNT,ILIMIT,fixHrvAmt,MAX_KEYWORDS,            2)
  call BFWRIT (WK3,IPNT,ILIMIT,fixPctAmt,MAX_KEYWORDS,            2)
  call BFWRIT (WK3,IPNT,ILIMIT,pctBf,MAX_KEYWORDS,                2)
  call BFWRIT (WK3,IPNT,ILIMIT,pctFt3,MAX_KEYWORDS,               2)
  call BFWRIT (WK3,IPNT,ILIMIT,pctTpa,MAX_KEYWORDS,               2)
  call BFWRIT (WK3,IPNT,ILIMIT,varHrvAmt,MAX_KEYWORDS,            2)
  call BFWRIT (WK3,IPNT,ILIMIT,varHrvDbhLo,MAX_KEYWORDS,          2)
  call BFWRIT (WK3,IPNT,ILIMIT,varHrvDbhHi,MAX_KEYWORDS,          2)
  call BFWRIT (WK3,IPNT,ILIMIT,varPctAmt,MAX_KEYWORDS,            2)
  call BFWRIT (WK3,IPNT,ILIMIT,varPctDbhLo,MAX_KEYWORDS,          2)
  call BFWRIT (WK3,IPNT,ILIMIT,varPctDbhHi,MAX_KEYWORDS,          2)

  call BFWRIT (WK3,IPNT,ILIMIT,plntCostAmt,MAX_PLANT_COSTS,       2)

  call BFWRIT (WK3,IPNT,ILIMIT,burnCostRate,MAX_RATES,            2)
  call BFWRIT (WK3,IPNT,ILIMIT,mechCostRate,MAX_RATES,            2)

  call BFWRIT (WK3,IPNT,ILIMIT,annCostRate,MAX_KEYWORDS&
       &*MAX_RATES, 2)
  call BFWRIT (WK3,IPNT,ILIMIT,annRevRate,MAX_KEYWORDS*MAX_RATES, 2)
  call BFWRIT (WK3,IPNT,ILIMIT,fixHrvRate,MAX_KEYWORDS*MAX_RATES, 2)
  call BFWRIT (WK3,IPNT,ILIMIT,fixPctRate,MAX_KEYWORDS*MAX_RATES, 2)
  call BFWRIT (WK3,IPNT,ILIMIT,varHrvRate,MAX_KEYWORDS*MAX_RATES, 2)
  call BFWRIT (WK3,IPNT,ILIMIT,varPctRate,MAX_KEYWORDS*MAX_RATES, 2)
  call BFWRIT (WK3,IPNT,ILIMIT,plntCostRate,MAX_PLANT_COSTS*&
       &MAX_RATES, 2)

  call BFWRIT (WK3,IPNT,ILIMIT,hrvRevPrice,MAXSP*MAX_REV_UNITS*&
       &MAX_KEYWORDS,                                      2)
  call BFWRIT (WK3,IPNT,ILIMIT,hrvRevDia,MAXSP*MAX_REV_UNITS*&
       &MAX_KEYWORDS,                                      2)
  call BFWRIT (WK3,IPNT,ILIMIT,revVolume,MAXSP*MAX_REV_UNITS*&
       &MAX_KEYWORDS,                                      2)

  call BFWRIT (WK3,IPNT,ILIMIT,hrvRevRate,MAXSP*MAX_REV_UNITS*&
       &MAX_KEYWORDS*MAX_RATES,                            2)

  call BFWRIT (WK3,IPNT,ILIMIT,logBfVol,ITRN*MAX_LOGS,          2)
  call BFWRIT (WK3,IPNT,ILIMIT,logDibBf, ITRN*MAX_LOGS,         2)
  call BFWRIT (WK3,IPNT,ILIMIT,logFt3Vol,ITRN*MAX_LOGS,         2)
  call BFWRIT (WK3,IPNT,ILIMIT,logDibFt3,ITRN*MAX_LOGS,         2)
  call BFWRIT (WK3,IPNT,ILIMIT,hrvCstAmt,    MAX_COSTS,         2)
  call BFWRIT (WK3,IPNT,ILIMIT,hrvRvnAmt,    MAX_HARVESTS,      2)
  call BFWRIT (WK3,IPNT,ILIMIT,undiscCost,   MAX_YEARS,         2)
  call BFWRIT (WK3,IPNT,ILIMIT,undiscRev,    MAX_YEARS,         2)
  return
end
