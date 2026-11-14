!============================!
!   metric/ppbase : cmprt2   !
!============================!
! METRIC-PPBASE $Id$
subroutine CMPRT2(IOSUM,HRVSUM,IPT,JPRT,JCOP,LEN,MGMID)
  implicit none

  !     CALLED FROM:
  !     CMPRT (ENTRY IN CMADDS) -- COMPUTES AVERAGES FOR WEIGHTED SUMS.

  !     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
  !              1: YEAR
  !              2: AGE
  !              3: TREES/ACRE
  !              4: TOTAL CU FT
  !              5: MERCH CU FT
  !              6: MERCH BD FT
  !              7: REMOVED TREES/ACRE
  !              8: REMOVED TOTAL CU FT
  !              9: REMOVED MERCH CU FT
  !             10: REMOVED MERCH BD FT
  !             11: BASAL AREA/ACRE
  !             12: CCF
  !             13: AVERAGE DOMINATE HEIGHT
  !             14: PERIOD LENGTH (YEARS)
  !             15: ACCRETION (ANNUAL IN CU FT/ACRE)
  !             16: MORTALITY  (ANNUAL IN CU FT/ACRE)
  !             17: TOTAL SAMPLE WEIGHT

  !     HRVSUM= FRACTION OF TOTAL SAMPLE WEIGHT WITH REMOVALS.
  !     IPT   = POINTER ARRAY USED TO ACCESS IOSUM IN CRONOLOGICAL
  !             ORDER. IF IPT(1)=0, IOSUM IS ASSUMED TO BE IN
  !             CRONOLOGICAL ORDER.
  !     JPRT  = DATA SET REFERENCE NUMBER FOR 'PRINTED' COPY (WITH
  !             HEADINGS AND CARRAGE CONTROL BYTE).  IF JPRT=0, NO
  !             DATA WILL BE WRITTEN.
  !     JCOP  = DATA SET REFERENCE NUMBER FOR 'NON-PRINTED' COPY (WITH
  !             OUT HEADINGS, NO CARRAGE CONTROL BYTE). IF JCOP=0,
  !             NO DATA WILL BE WRITTEN.
  !     LEN   = NUMBER OF ROWS (ENTRIES) IN IOSUM.
  !     MGMID = MANAGEMENT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.

  ! Common blocks
  include  'METRIC.f90'

  ! Variables
  integer :: IPT(LEN),JCOP,JPRT,LEN,J,I,K
  character(len=4) :: MGMID
  integer :: IOSUM(17,LEN)
  real    :: HRVSUM(LEN)
  logical :: LPRT,LDSK

  !     STEP1: SET SWITCHES.

  LPRT = JPRT .gt.0
  LDSK = JCOP  .gt.0
  if (.not. (LPRT.or.LDSK)) return

  !     STEP2: WRITE HEADING; SKIP A FEW LINES, DO NOT START A NEW PAGE.

  if (LPRT) write (JPRT,10)
  10 format (//T40,'COMPOSITE YIELD STATISTICS'/1X,102('-')/&
       &T17,'VOLUME PER HA     REMOVALS/TREATED HA  ',T68,&
       &'AVE   GROWTH'/T17,17('-'),1X,23('-'),'  BA/     DOM ',&
       &11('-'),' TOTAL  FRACTION'/T11,&
       &2('TREES TOTAL MERCH MERCH '),' HA      HT  PRD ACC MOR ',&
       &'SAMPLE OF AREA  MGMT'/&
       &' YEAR AGE ',2('/HA   CU M  CU M  BD FT '),&
       &'SQ M CCF M   YRS CU M/YR WEIGHT TREATED  ID'/&
       &' ---- --- ',8('----- '),'---- ',5('--- '),'------ ',&
       &'-------- ----')

  !     STEP3: LOOP THRU ALL ROWS IN IOSUM...WRITE OUTPUT.

  do J = 1,LEN
     I = IPT(J)
     if (I.le.0) goto 50
     if (LPRT) write (JPRT,20)&
          &IOSUM( 1,I),&
          &IOSUM( 2,I),&
          &NINT(IOSUM( 3,I)/ACRtoHA),&
          &NINT(IOSUM( 4,I)*FT3pACRtoM3pHA),&
          &NINT(IOSUM( 5,I)*FT3pACRtoM3pHA),&
          &IOSUM( 6,I),& ! BOARD FEET: NOT METRIFIED
          &NINT(IOSUM( 7,I)/ACRtoHA),&
          &NINT(IOSUM( 8,I)*FT3pACRtoM3pHA),&
          &NINT(IOSUM( 9,I)*FT3pACRtoM3pHA),&
          &IOSUM(10,I),& ! BOARD FEET: NOT METRIFIED
          &NINT(IOSUM(11,I)*FT2pACRtoM2pHA),&
          &IOSUM(12,I),&
          &NINT(IOSUM(13,I)*FTtoM),&
          &IOSUM(14,I),&
          &NINT(IOSUM(15,I)*FT3pACRtoM3pHA),&
          &NINT(IOSUM(16,I)*FT3pACRtoM3pHA),&
          &IOSUM(17,I),&
          &HRVSUM(I),MGMID
     if (LDSK) write (JCOP,30)&
          &IOSUM( 1,I),&
          &IOSUM( 2,I),&
          &NINT(IOSUM( 3,I)/ACRtoHA),&
          &NINT(IOSUM( 4,I)*FT3pACRtoM3pHA),&
          &NINT(IOSUM( 5,I)*FT3pACRtoM3pHA),&
          &IOSUM( 6,I),& ! BOARD FEET: NOT METRIFIED
          &NINT(IOSUM( 7,I)/ACRtoHA),&
          &NINT(IOSUM( 8,I)*FT3pACRtoM3pHA),&
          &NINT(IOSUM( 9,I)*FT3pACRtoM3pHA),&
          &IOSUM(10,I),& ! BOARD FEET: NOT METRIFIED
          &NINT(IOSUM(11,I)*FT2pACRtoM2pHA),&
          &IOSUM(12,I),&
          &NINT(IOSUM(13,I)*FTtoM),&
          &IOSUM(14,I),&
          &NINT(IOSUM(15,I)*FT3pACRtoM3pHA),&
          &NINT(IOSUM(16,I)*FT3pACRtoM3pHA),&
          &IOSUM(17,I),&
          &HRVSUM(I),MGMID
     20 format (1X,2I4,8I6,I5,5I4,I7,F9.6,1X,A4)
     30 format    (2I4,8I6,I5,5I4,I7,F9.6,1X,A4)

     50 continue
  end do
  return
end
